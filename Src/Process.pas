(***************************************************************************

    Copyright 1998-2010, Christian Aymon (cyamon software, www.cyamon.com)

    This file is part of ''Discover''.

    ''Discover'' is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    ''Discover'' is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Discover.  If not, see <http://www.gnu.org/licenses/>.

 
***************************************************************************)
unit Process;

interface
uses
  Classes, Windows, DataBase, Objects, Messages;

const
  XM_COVEREDPOINT = WM_USER + 1;
  XM_DEBUGSTRING = WM_USER + 2;
  XM_RUNNING = WM_USER + 3;
  XM_TERMINATED = WM_USER + 4;
  XM_PROCESSNOTCREATED = WM_USER + 5;
  XM_ERRORCODE = WM_USER + 6;

type
  TThreadInfo = class(TObject)
    Id, Handle : integer;
  end;
  TThreadInfos = class(TSortedCollection)
    function Compare(Key1, Key2 : Pointer) : integer; override;
    function KeyOf(Item : pointer) : pointer; override;
  end;
  TPointCoveredEvent = procedure(Point : TCoveragePoint) of object;
  TProcessEvent = procedure of object;
  TDebugStringEvent = procedure (const s : string) of object;


  TProcess = class(TComponent)
  private
    Handle : THandle;
    FRunParameters : string;
    FStartupDirectory : string;
    FRunMaximized : boolean;
    FRunning, FCreated : boolean;
    FExeName : string;
    procedure GetCoveragePoint(Address : integer; var
      CoveragePoint : TCoveragePoint; var Index : integer);
    procedure SetInitialBreakPoints(aProcess : THandle);
    function SetOneBreakPoint(aProcess : THandle; aPoint : TCoveragePoint) : integer;
  public
    procedure Run;
    procedure Reset;
    property Running : boolean read FRunning;
    property Created : boolean read FCreated;
    property RunParameters : string read FRunParameters write FRunParameters;
    property StartupDirectory : string read FStartupDirectory write FStartupDirectory;
    property RunMaximized : boolean read FRunMaximized write FRunMaximized;
    property ExeName : string read FExeName write FExeName;
  end;

implementation
  uses
    Forms, SysUtils, Globals, Dialogs, Exceptions;
{$Q-} {$R-}

type
  TDebugThread = class (TThread)
  private
    Process : TProcess;
  public
    constructor Create(aProcess : TProcess);
    procedure Execute; override;
  end;
  
  const
    ProgrammOffset_ = $1000;

{~t}
(***********************)
(* TDebugThread.Create *)
(***********************)

constructor TDebugThread.Create(aProcess : TProcess);
begin
  Process := aProcess;
  FreeOnTerminate := true;
  inherited Create(false);
end {TDebugThread.Create};


(************************)
(* TDebugThread.Execute *)
(************************)

procedure TDebugThread.Execute;
  const
    DBG_CONTINUE = $00010002;
    DBG_EXCEPTION_NOT_HANDLED = $80010001;
  var
    DebugEvent: TDebugEvent;
    CurrentCoveragePoint : TCoveragePoint;
    ContinueStatus : integer;
    ProcessHandle, CurrentThreadHandle : THandle;
    ThreadInfos : TThreadInfos;

  procedure HandleBreakPoint(Address : pointer; ThreadId : THandle);
    var
      Context : TContext;
      i, Idx : integer;
      Count : DWORD;
  begin
    // Locate the BP address
    Process.GetCoveragePoint(integer(Address),CurrentCoveragePoint,i);
    if i >= 0 then begin
      if ThreadInfos.Search(pointer(ThreadId), Idx) then begin
        CurrentThreadHandle := TThreadInfo(ThreadInfos.At(Idx)).Handle;
        Context.ContextFlags := CONTEXT_CONTROL;
        if GetThreadContext(CurrentThreadHandle, Context) then begin
          with Context do begin
            dec(Eip);
            WriteProcessMemory(ProcessHandle, Pointer(Eip),
              @CurrentCoveragePoint.OpCode, 1, Count);
            CurrentCoveragePoint.IsBreakPointSet := false;
            // EFlags := EFlags or $100;  // do not single step, it takes too long!
            SetThreadContext(CurrentThreadHandle, Context);
            SendMessage(Application.Handle, XM_COVEREDPOINT, 0, integer(CurrentCoveragePoint));
          end {with};
        end else
          inc(LogInfos_.R.NotAccessibleContexts)
      end else
        inc(LogInfos_.R.NotFoundThreads);
    end else
      inc(LogInfos_.R.NotFoundPoint);
  end {HandleBreakPoint};

  procedure HandleDebugString(DebugInfo:TOutputDebugStringInfo);
    var
      s : array [0..255] of char;
      Dmy : DWORD;
  begin
    // Get the debug string
    if DebugInfo.nDebugStringLength > SizeOf(s) then begin
      ReadProcessMemory(ProcessHandle, DebugInfo.lpDebugStringData, @s,
        255, Dmy);
      s[255] := #0;
    end else
      ReadProcessMemory(ProcessHandle, DebugInfo.lpDebugStringData, @s,
        DebugInfo.nDebugStringLength, Dmy);
    SendMessage(Application.Handle, XM_DEBUGSTRING, 0, integer(@s));
  end {HandleDebugString};

(*
  procedure HandleSingleStep(Address : pointer; ThreadId : THandle);
    var
      Context : TContext;
      i, Idx, a : integer;
      Count : integer;
      b : byte;
      C : TCoveragePoint;
  begin
    if ThreadInfos.Search(pointer(ThreadId), Idx) then begin
      CurrentThreadHandle := TThreadInfo(ThreadInfos.At(Idx)).Handle;
      Context.ContextFlags := CONTEXT_CONTROL;
      if GetThreadContext(CurrentThreadHandle, Context) then begin
        with Context do begin
          EFlags := EFlags and not($100);
        end {with};
        SetThreadContext(CurrentThreadHandle, Context);
      end {if};
    end {if};
    if not CurrentCoveragePoint.IsBreakPointSet then
     SetOneBreakPoint(ProcessHandle, CurrentCoveragePoint);
  end {HandleSingleStep};
*)

  function HandleDebugEvent : boolean;
    var
      Idx : integer;
      ThreadInfo : TThreadInfo;
  begin
    Result := false;
    ContinueStatus := DBG_CONTINUE;
    case DebugEvent.dwDebugEventCode of
      EXCEPTION_DEBUG_EVENT: begin
        // Process the exception code. When handling
        // exceptions, remember to set the continuation
        // status parameter (dwContinueStatus). This value
        // is used by the ContinueDebugEvent function.
        case (DebugEvent.Exception.ExceptionRecord.ExceptionCode) of
          EXCEPTION_BREAKPOINT: begin
            // First chance: Display the current
            // instruction and register values.
            HandleBreakPoint(DebugEvent.Exception.ExceptionRecord.ExceptionAddress,
              DebugEvent.dwThreadId);
          end;

          EXCEPTION_SINGLE_STEP: begin
            // First chance: Update the display of the
            // current instruction and register values.
(*
            HandleSingleStep(DebugEvent.Exception.ExceptionRecord.ExceptionAddress,
              DebugEvent.dwThreadId);
*)
          end;

        else
          // Handle other exceptions.
          ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
        end {case};
      end;

      CREATE_THREAD_DEBUG_EVENT: begin
        // As needed, examine or change the thread's registers
        // with the GetThreadContext and SetThreadContext functions;
        // and suspend and resume thread execution with the
        // SuspendThread and ResumeThread functions.
        ThreadInfo := TThreadInfo.Create;
        with ThreadInfo do begin
          Id := DebugEvent.dwThreadId;
          Handle := DebugEvent.CreateThread.hThread;
        end {with};
        ThreadInfos.Insert(ThreadInfo);
      end;

      CREATE_PROCESS_DEBUG_EVENT: begin
        // As needed, examine or change the registers of the
        // process's initial thread with the GetThreadContext and
        // SetThreadContext functions; read from and write to the
        // process's virtual memory with the ReadProcessMemory and
        // WriteProcessMemory functions; and suspend and resume
        // thread execution with the SuspendThread and ResumeThread
        // functions.
        ThreadInfo := TThreadInfo.Create;
        with ThreadInfo do begin
          Id := DebugEvent.dwThreadId;
          Handle := DebugEvent.CreateProcessInfo.hThread;
        end {with};
        ThreadInfos.Insert(ThreadInfo);
        ProcessHandle := DebugEvent.CreateProcessInfo.hProcess;
        Process.Handle := ProcessHandle;
        // Close the image handle so that it is not blocked when the process exits
        CloseHandle(DebugEvent.CreateProcessInfo.hFile);
        Process.SetInitialBreakPoints(DebugEvent.CreateProcessInfo.hProcess);
      end;

      EXIT_THREAD_DEBUG_EVENT: begin
        if ThreadInfos.Search(pointer(DebugEvent.dwThreadId),Idx) then
          ThreadInfos.AtFree(Idx);
      end;

      EXIT_PROCESS_DEBUG_EVENT: begin
        Result := true;
        Process.FCreated := false;
      end;

      OUTPUT_DEBUG_STRING_EVENT: begin
        HandleDebugString(DebugEvent.DebugString);
      end;

    end {case};

  // Resume executing the thread that reported the debugging event.

  ContinueDebugEvent(DebugEvent.dwProcessId,
      DebugEvent.dwThreadId, ContinueStatus);

  end {HandleDebugEvent};


  function CreateProcess : boolean;
    var
      StartupInfo : TStartupInfo;
      ProcessInformation : TProcessInformation;
      RunParams, StartupDir : PChar;
  begin
    FillChar(StartupInfo, SizeOf(StartupInfo), #0);
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    if Process.FRunMaximized then
      StartupInfo.wShowWindow := SW_MAXIMIZE
    else
      StartupInfo.wShowWindow := SW_SHOWDEFAULT;

    if Process.FRunParameters <> '' then
      RunParams := PChar(Process.FRunParameters)
    else
      RunParams := nil;

    if Process.FStartupDirectory <> '' then
      StartupDir := PChar(Process.FStartupDirectory)
    else
      StartupDir := nil;

    Result := Windows.CreateProcess(PChar(Process.ExeName),  RunParams,
      nil, nil, false, {DEBUG_PROCESS or }DEBUG_ONLY_THIS_PROCESS, nil,
      StartupDir, StartupInfo, ProcessInformation);
  end {CreateProcess};

  var
    Done : boolean;
    Timeout : integer;
begin
  Process.FCreated := false;
  if not CreateProcess then begin
    SendMessage(Application.Handle, XM_PROCESSNOTCREATED, 0, Windows.GetLastError);
    exit;
  end {if};
  Process.FCreated := true;

  ThreadInfos := TThreadInfos.Create;
  Done := false;
  Timeout := 200;
  FillChar(LogInfos_, SizeOf(LogInfos_), #0);
  try
    repeat
      if WaitForDebugEvent(DebugEvent, Timeout) then begin
        Done := HandleDebugEvent;
      end else begin
        if not Process.FRunning then begin
          if WaitForInputIdle(Handle,1) <> WAIT_TIMEOUT then begin
            SendMessage(Application.Handle, XM_RUNNING, 0, 0);
            Process.FRunning := true;
          end {if};
        end else
          TimeOut := INFINITE;
      end {if};
    until Done;
  finally
    LogInfos_.R.MaxThreads := ThreadInfos.Count;
    ThreadInfos.Free;
    Process.FRunning := false;
    Process.FCreated := false;
    SendMessage(Application.Handle, XM_TERMINATED, 0, 0);
  end {try};
end {TDebugThread.Execute};


(*****************************)
(* TProcess.GetCoveragePoint *)
(*****************************)

procedure TProcess.GetCoveragePoint(Address : integer; var CoveragePoint : TCoveragePoint;
  var Index : integer);
begin
  with ProjectDataBase_ do
    if CoveragePoints.Search(pointer(Address-ImageBase-ProgrammOffset_),Index) then begin
      CoveragePoint := CoveragePoints.At(Index);
    end else
      Index := -1;
end {TProcess.GetCoveragePoint};


(******************)
(* TProcess.Reset *)
(******************)

procedure TProcess.Reset;
begin
  TerminateProcess(Handle,0);
end {TProcess.Reset};


(****************)
(* TProcess.Run *)
(****************)

procedure TProcess.Run;
  var
    DebugThread : TDebugThread;
begin
  DebugThread := TDebugThread.Create(Self);
end {TProcess.Run};


(**********************************)
(* TProcess.SetInitialBreakPoints *)
(**********************************)

procedure TProcess.SetInitialBreakPoints(aProcess : THandle);
  var
    C : TCoveragePoint;
    R : TRoutine;
    U : TUnit;
    CIdx, ErrorCode : integer;
begin
  ErrorCode := 0;
  CIdx := 0;
  while (CIdx < ProjectDataBase_.CoveragePoints.Count) do begin
    C := ProjectDataBase_.CoveragePoints.At(CIdx);
    R := ProjectDataBase_.Routines.At(C.RoutineIndex);
    U := ProjectDataBase_.Units.At(R.UnitIndex);
    if (not U.Disabled) and (not R.Disabled) and (not C.Disabled) and
      (C.Counter = 0) then begin
      ErrorCode := SetOneBreakPoint(aProcess, C);
    end {if};
    if (ErrorCode > 0) and (LogInfos_.R.AccessMemFailures < MaxAccessMemFailures) then begin
      SendMessage(Application.Handle, XM_ERRORCODE, ErrorCode, C.Address+
        ProjectDataBase_.ImageBase+ProgrammOffset_);
    end {if};
    inc(CIdx);
  end {while};
end {TProcess.SetInitialBreakPoints};


(*****************************)
(* TProcess.SetOneBreakPoint *)
(*****************************)

function TProcess.SetOneBreakPoint(aProcess : THandle; aPoint : TCoveragePoint) : integer;
  var
    b : byte;
    Dmy : DWORD;
begin
  Result := 0;
  if aPoint.Valid then begin
    inc(LogInfos_.R.WantedBreakPoints);
    b := $CC;
    with ProjectDataBase_ do begin
      if not ReadProcessMemory(aProcess, Pointer(aPoint.Address+ImageBase+ProgrammOffset_), @aPoint.OpCode, 1, Dmy) then begin
        Result := GetLastError();
        inc(LogInfos_.R.AccessMemFailures);
      end else if not WriteProcessMemory(aProcess, Pointer(aPoint.Address+ImageBase+ProgrammOffset_), @b, 1, Dmy) then begin
        Result := GetLastError();
        inc(LogInfos_.R.AccessMemFailures);
        inc(LogInfos_.R.UnSettedBreakPoints);
      end else
        aPoint.IsBreakPointSet := true;
    end {with};
  end {if};
end {TProcess.SetOneBreakPoint};


(************************)
(* TThreadInfos.Compare *)
(************************)

function TThreadInfos.Compare;
begin
  if integer(Key1) < integer(Key2) then
    Result := -1
  else if integer(Key1) > integer(Key2) then
    Result := 1
  else
    Result := 0;
end {TThreadInfos.Compare};


(**********************)
(* TThreadInfos.KeyOf *)
(**********************)

function TThreadInfos.KeyOf;
begin
  Result := pointer(TThreadInfo(Item).Id);
end {TThreadInfos.KeyOf};


{~b}
end.
