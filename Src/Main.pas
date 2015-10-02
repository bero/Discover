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
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, DataBase, ComCtrls, Objects,
  Process, Buttons, ImgList, ProjectInfo;

type
  THeaderTrackingInfo = record
    TrackingRect : TRect;
    TrackingRectDrawn : boolean;
  end {record};
  PHeaderTrackingInfo = ^THeaderTrackingInfo;

  // The very first version of Discover was able to record and play journaling events...
  // However, this featunre has never been completely implemented but several (non huring)
  // declarations still exist here ant there...
  TJournalEvent = (jeWMCancelJournal, jeApplRestored, jeCtrlBreak,
    jeProcessJustRunning, jeTimerElapsed, jeProcessJustTerminated, jePlayStream,
    jePlayStreamsToPlay, jeRecord, jePlayNodesToPlay, jeRecordContinue,
    jeDebugString);
  TStateMachineState = (stIdle, stPlaying, stRecording, stWaitForProcessNotRunning,
    stWaitForProcessRunning, stWaitForProcessReadyForPlaying);

  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MMProject: TMenuItem;
    MMProjectNew: TMenuItem;
    OpenDelphiProjectDialog: TOpenDialog;
    StatusBar: TStatusBar;
    PNLMain: TPanel;
    PNLLeft: TPanel;
    Splitter: TSplitter;
    PNLRight: TPanel;
    PCLeft: TPageControl;
    TSRoutines: TTabSheet;
    TSOverView: TTabSheet;
    Panel6: TPanel;
    HCRoutines: THeaderControl;
    LBRoutines: TListBox;
    TSUnits: TTabSheet;
    LBUnits: TListBox;
    HCUnits: THeaderControl;
    PURoutines: TPopupMenu;
    PURoutinesDisable: TMenuItem;
    PCRight: TPageControl;
    TSCode: TTabSheet;
    LBFile: TListBox;
    PUSource: TPopupMenu;
    PUSourceNextGreen: TMenuItem;
    PUSourceNextRed: TMenuItem;
    OpenStateDialog: TOpenDialog;
    TSSummary: TTabSheet;
    MEMOSummary: TMemo;
    PUUnits: TPopupMenu;
    PUUnitsEnable: TMenuItem;
    N3: TMenuItem;
    PUUnitsSelectAll: TMenuItem;
    OpenLibDialog: TOpenDialog;
    SaveLibDialog: TSaveDialog;
    MMApplication: TMenuItem;
    MMApplicationRun: TMenuItem;
    MMApplicationTerminate: TMenuItem;
    MMOptions: TMenuItem;
    MMHelp: TMenuItem;
    PBOverView: TPaintBox;
    ILLibImages: TImageList;
    MMHelpAbout: TMenuItem;
    PUUnitsDisable: TMenuItem;
    MMProjectSave: TMenuItem;
    MMProjectReload: TMenuItem;
    PURoutinesEnable: TMenuItem;
    N4: TMenuItem;
    MMProjectExit: TMenuItem;
    PUUnitsSelectGroup: TMenuItem;
    MMHelpHelp: TMenuItem;
    SaveStateDialog: TSaveDialog;
    MMProjectMerge: TMenuItem;
    Timer1: TTimer;
    N5: TMenuItem;
    MMExportData: TMenuItem;
    N1: TMenuItem;
    MMProjectSettings: TMenuItem;
    N2: TMenuItem;
    MMProjectClearState: TMenuItem;
    PBLegend: TPaintBox;
    TIMERResize: TTimer;
    procedure MMProjectNewClick(Sender: TObject);
    procedure PBOverViewPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBFileDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure HCSectionTrack(HeaderControl: THeaderControl;
      Section: THeaderSection; Width: Integer; State: TSectionTrackState);
    procedure LBRoutinesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure HCRoutinesSectionClick(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure LBRoutinesClick(Sender: TObject);
    procedure HCRoutinesResize(Sender: TObject);
    procedure LBUnitsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure LBFileClick(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HCUnitsSectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure HCUnitsSectionClick(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure TimerTimer(Sender: TObject);
    procedure PCLeftChange(Sender: TObject);
    procedure PUUnitsEnableDisableClick(Sender: TObject);
    procedure PUUnitsPopup(Sender: TObject);
    procedure PUUnitsSelectAllClick(Sender: TObject);
    procedure MMProjectClick(Sender: TObject);
    procedure MMApplicationClick(Sender: TObject);
    procedure MMApplicationRunClick(Sender: TObject);
    procedure MMApplicationTerminateClick(Sender: TObject);
    procedure MMMacroRecordNewClick(Sender: TObject);
    procedure MMOptionsClick(Sender: TObject);
    procedure MMHelpAboutClick(Sender: TObject);
    procedure PURoutinesPopup(Sender: TObject);
    procedure PURoutinesEnableDisableClick(Sender: TObject);
    procedure MMProjectSaveClick(Sender: TObject);
    procedure MMProjectReloadClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LBRoutinesKeyPress(Sender: TObject; var Key: Char);
    procedure LBUnitsKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MMProjectExitClick(Sender: TObject);
    procedure PUUnitsSelectGroupClick(Sender: TObject);
    procedure PCRightChanging(Sender: TObject; var AllowChange: Boolean);
    procedure MMHelpHelpClick(Sender: TObject);
    procedure MMHelpClick(Sender: TObject);
    procedure MMProjectMergeClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MMExportDataClick(Sender: TObject);
    procedure LBUnitsClick(Sender: TObject);
    procedure LBRoutinesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBFileKeyPress(Sender: TObject; var Key: Char);
    procedure PUSourceNextGreenClick(Sender: TObject);
    procedure PUSourceNextRedClick(Sender: TObject);
    procedure LBFileMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PBOverViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MMProjectSettingsClick(Sender: TObject);
    procedure MMProjectClearStateClick(Sender: TObject);
    procedure PBLegendPaint(Sender: TObject);
    procedure TIMERResizeTimer(Sender: TObject);
  private
    { Private declarations }
    SquaresPerLine : integer;
    CurrentUnit : TUnit;
    CurrentRoutine : TRoutine;
    SortedRoutines : TSortedRoutines;
    SortedUnits : TSortedUnits;
    CurrentSourceFileName, LoadedStatesStr : string;
    CoveredBitMap, UnCoveredBitMap, EnabledBitMap, DisabledBitMap,
      LockShutBitmap : TBitMap;
    HeaderRoutinesInfo, HeaderUnitsInfo : THeaderTrackingInfo;
    Process : TProcess;
    PrevPerCent : integer;
    FStateMachineState : TStateMachineState;
    DeltaCovered : integer;
    AppearanceArray : array [0..14] of integer;
    LastChangedCount : integer;
    CaptureBitmapStream : TMemoryStream;
    InitialScreenDone : boolean;
    ProjectInfo : TProjectInfo;
    StateFileName, InfoFileName, ExeFileName : string;
    Resizing : boolean;
    procedure LoadProject(const DelphiProjectFileName : string);
    procedure LoadState(const StateFileName : string);
    procedure FillLBRoutines;
    procedure FillLBUnits;
    procedure FillSummary;
    procedure UpdateOverView(Routine : TRoutine);
    procedure UpdateFileLine(Point : TCoveragePoint);
    procedure UpdateCoverageOnStatusBar;
    procedure UpdateOnActivate;
    procedure FillSortedRoutineList;
    procedure FillSortedUnitList;
    procedure DisplayStatusFilename;
    procedure AdjustStatusbar;
    procedure DrawStringInColumn(Canvas : TCanvas; R : TRect; const s : string;
      RightJustify : boolean);
    function MainHook(var Message: TMessage): Boolean;

    procedure HandlePointCovered (Point : TCoveragePoint);
    procedure HandleProcessRunning;
    procedure HandleProcessTerminated;
    procedure HandleProgress(Percent : integer; const ActionString : string);
    procedure HandleMessage(var Msg: TMsg; var Handled: Boolean);
    procedure HandleApplActivate(Sender : TObject);
    procedure HandleApplDeActivate(Sender : TObject);
    procedure HandleProcessDebugString(DebugString : PChar);
    procedure HandleIdleApplication (Sender: TObject; var Done: Boolean);
    procedure HandleErrorCode(ErrorCode, Address : integer);
    procedure HandleExceptions(Sender : TObject; E : Exception);
    procedure GotoNextCoveragePoint(RedGreen : boolean);
    procedure PositionRoutineViewToPoint(C : TCoveragePoint);
    procedure DoCommandLineInitialActions;
    procedure DoCommandLineFinalActions;
    procedure MergeWithStateFile(const FileName : string);
    procedure LoadInformationFile;
    procedure SaveInformationFile;
    procedure RunApplication;
    procedure SaveStateFile(const FileName : string);
    procedure MakeReport;


    function AllowsNewState : boolean;
    procedure SaveDataBase(const FileName : string);
    procedure InitAfterLoadingDatabase;
    procedure HandleJournalEvent(Event:TJournalEvent);
    procedure SetStateMachineState(aState : TStateMachineState);
    property StateMachineState : TStateMachineState read FStateMachineState
      write SetStatemachineState;
  public
    { Public declarations }
    procedure LoadLastSavedState;
    procedure AdjustStayOnTop;
    procedure Sleep(Delay : integer);
  end;

var
  FormMain: TFormMain;

implementation
  uses
    CodeParser, Config, Exceptions, Globals, IniFiles, MapFile,
    Util, F_Options, F_About,  F_Edit, CRC32, ShellAPI,
    FileCtrl, F_Export, F_ProjectInfo;

{$R *.DFM}
const
  StateMachineStrings : array [TStateMachineState] of string = (
    'Idle', 'Playing', 'Recording', 'WaitForProcessNotRunning',
    'WaitForProcessRunning', 'WaitForProcessReadyForPlaying');
  JournalEventString : array [TJournalEvent] of string = (
    'WMCancelJournal', 'ApplRestored',  'CtrlBreak',
    'ProcessJustRunning', 'TimerElapsed', 'ProcessJustTerminated',
    'PlayStream', 'PlayStreamsToPlay', 'Record', 'PlayNodesToPlay',
    'RecordContinue', 'DebugString'
  );
  pProgress = 4;
  pFilePos = 4;
  pValidEnabledCoveredPoints = 0;
  pDeltaCovered = 1;
  pProcessState = 2;
var
  OverviewPointSquareSide : integer = 1;

{~t}
(*****************************)
(* TFormmain.AdjustStatusbar *)
(*****************************)

procedure TFormmain.AdjustStatusbar;
  var
    w, i : integer;
begin
  w := 0;
  with StatusBar, Panels do begin
    Panels[pProcessState].Width := Canvas.TextWidth('HHHHHHH');
    for i := 0 to Count - 2 do
      inc(w, Panels[i].Width);
    Panels[pred(Count)].Width := Width - w - GetSystemMetrics(SM_CXHSCROLL);
  end {with};
end {TFormmain.AdjustStatusbar};


(*****************************)
(* TFormMain.AdjustStayOnTop *)
(*****************************)

procedure TFormMain.AdjustStayOnTop;
begin
  if FormOptions.CHKStayOnTop.Checked then
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE)
    // FormStyle := fsStayOnTop
  else
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE)
    // FormStyle := fsNormal;
end {TFormMain.AdjustStayOnTop};


(****************************)
(* TFormMain.AllowsNewState *)
(****************************)

function TFormMain.AllowsNewState;
begin
  Result := true;
  if (ProjectDatabase_ <> nil) and (ProjectDataBase_.ChangedCount <> 0) then
    case MessageDlg('Do you want to save the current state?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: begin
        if ExecDialog(SaveStateDialog, State_Key) then begin
          Refresh;
          SaveStateFile(SaveStateDialog.FileName);
          Result := true;
        end else
          Result := false;
      end;
      mrNo:
        Result := true;
      mrCancel:
        Result := false;
    end {case};
end {TFormMain.AllowsNewState};


(***********************************)
(* TFormMain.DisplayStatusFilename *)
(***********************************)

procedure TFormMain.DisplayStatusFilename;
  var
    s,t : string;
begin
  if CurrentSourceFileName <> '' then
    with StatusBar.Panels[pFilePos] do begin
      t := ' ('+IntToStr(succ(LBFile.ItemIndex))+')';
      s := MinimizeName(CurrentSourceFileName, Canvas, Width-Canvas.TextWidth(t)) + t;
      Text := s;
    end {with};
end {TFormMain.DisplayStatusFilename};


(***************************************)
(* TFormMain.DoCommandLineFinalActions *)
(***************************************)

procedure TFormMain.DoCommandLineFinalActions;
begin
  if CommandLineParams_.SaveStateOnAppTerminate then
    SaveStateFile(StateFileName);

  if CommandLineParams_.ReportWhenAppTerminated then
    MakeReport;

  if CommandLineParams_.CloseWhenAppTerminated then
    Close
  else begin
    CommandLineParams_.Action := caNoAction; // Command line ignored
    Application.Restore;
  end {if};
end {TFormMain.DoCommandLineFinalActions};


(*****************************************)
(* TFormMain.DoCommandLineInitialActions *)
(*****************************************)

procedure TFormMain.DoCommandLineInitialActions;
begin
  if CommandLineParams_.Action = caDPR then begin
    // DPR started
    // Load the project
    LoadProject(CommandLineParams_.FileName);

    ExeFileName := ProjectDataBase_.ExecutableFileName;
    StateFileName := ChangeFileExt(ProjectDataBase_.ExecutableFileName,ProjectStateExtension);
    InfoFileName := ChangeFileExt(StateFileName, ProjectInformationExtension);

    // Check if an information file exists and if yes take its infos
    LoadInformationFile;

    if CommandLineParams_.Merge then begin
      if FileExists(StateFileName) then
        MergeWithStateFile(StateFileName);
    end {if};

    // Run the application
    RunApplication;

  end else begin
    // DPS started
    LoadState(CommandLineParams_.FileName);

    StateFileName := CommandLineParams_.FileName;
    InfoFileName := ChangeFileExt(StateFileName, ProjectInformationExtension);
    ExeFileName := ChangeFileExt(StateFileName, ExecutableExtension);;

    // Check if an information file exists and if yes take its infos
    LoadInformationFile;

    RunApplication;

  end {if};
end {TFormMain.DoCommandLineInitialActions};


(********************************)
(* TFormmain.DrawStringInColumn *)
(********************************)

procedure TFormmain.DrawStringInColumn(Canvas : TCanvas; R : TRect; const s : string;
  RightJustify : boolean);
  var
    t : string;
    StripCount : integer;
begin
  with Canvas do begin
    t := s;
    StripCount := 1;
    if RightJustify then
      dec(R.Right, 5);
    while (StripCount < Length(s)) and (TextWidth(t) > (R.Right - R.Left)) do begin
      t := Copy(s,1,Length(s)-StripCount)+'...';
      inc(StripCount);
    end {while};
    if RightJustify then
      TextOut(R.Right-TextWidth(t), R.Top, t)
    else
      TextOut(R.Left, R.Top, t);
  end {with};
end {TFormmain.DrawStringInColumn};


(****************************)
(* TFormMain.FillLBRoutines *)
(****************************)

procedure TFormMain.FillLBRoutines;
  var
    i : integer;
    R : TRoutine;
begin
  LBRoutines.Items.BeginUpdate;
  LBRoutines.Items.Clear;
  with SortedRoutines do
    for i := 0 to pred(Count) do begin
      R := At(i);
      LBRoutines.Items.AddObject('',R);
    end {for};
  LBRoutines.Items.EndUpdate;
end {TFormMain.FillLBRoutines};


(*************************)
(* TFormMain.FillLBUnits *)
(*************************)

procedure TFormMain.FillLBUnits;
  var
    U : TUnit;
    i : integer;
begin
  LBUnits.Items.BeginUpdate;
  LBUnits.Items.Clear;
  with SortedUnits do
    for i := 0 to pred(Count) do begin
      U := At(i);
      LBUnits.Items.AddObject('',U);
    end {for};
  LBUnits.Items.EndUpdate;
end {TFormMain.FillLBUnits};


(***********************************)
(* TFormMain.FillSortedRoutineList *)
(***********************************)

procedure TFormMain.FillSortedRoutineList;
  var
    i : integer;
    R : TRoutine;
    U : TUnit;
begin
  SortedRoutines.DeleteAll;
  if ProjectDatabase_ <> nil then begin
    ProjectDataBase_.UpDateStatistics;
    for i := 0 to pred(ProjectDataBase_.Routines.Count) do begin
      R := ProjectDataBase_.Routines.At(i);
      U := ProjectDataBase_.Units.At(R.UnitIndex);
      if (R.FirstPointIndex >= 0) and U.IsSourceAvailable then
        SortedRoutines.Insert(R);
    end {for};
  end {if};
end {TFormMain.FillSortedRoutineList};


(********************************)
(* TFormMain.FillSortedUnitList *)
(********************************)

procedure TFormMain.FillSortedUnitList;
  var
    i : integer;
    U : TUnit;
begin
  // Update Statistical info
  if ProjectDatabase_ <> nil then begin
    ProjectDataBase_.UpDateStatistics;
    SortedUnits.DeleteAll;
    with ProjectDataBase_.Units do
      for i := 0 to pred(Count) do begin
        U := At(i);
        if U.IsSourceAvailable or not FormOptions.CHKNoDisplaySourceLessUnits.Checked then
          SortedUnits.Insert(U);
      end {for};
  end {if};
end {TFormMain.FillSortedUnitList};


(*************************)
(* TFormMain.FillSummary *)
(*************************)

procedure TFormMain.FillSummary;
  const
    R100s = 'R.100%';
    R0s = 'R.0%';
  var
    R100, R0 : integer;
begin
  if ProjectDataBase_ <> nil then
    with ProjectDataBase_ do begin
      GetR100R0(R100, R0);
      MEMOSummary.Lines.Clear;
      MEMOSummary.Lines.Add(ProjectDataBase_.ExecutableFileName);
      MEMOSummary.Lines.Add(LoadedStatesStr);
      MEMOSummary.Lines.Add('UNITS');
      MEMOSummary.Lines.Add(Format('  Total:      %5d',[Units.Count]));
      MEMOSummary.Lines.Add(Format('  Source:     %5d',[UnitsWithSource]));
      MEMOSummary.Lines.Add(Format('  Foreground: %5d', [EnabledUnitsQty]));
      MEMOSummary.Lines.Add('');

      MEMOSummary.Lines.Add('ROUTINES');
      MEMOSummary.Lines.Add(Format('  Total:      %5d',[Routines.Count]));
      MEMOSummary.Lines.Add(Format('  Source:     %5d', [RoutinesWithSource]));
      MEMOSummary.Lines.Add(Format('  Foreground: %5d', [EnabledRoutinesQty]));
      MEMOSummary.Lines.Add(Format('  %s:     %5d', [R100s, R100]));
      MEMOSummary.Lines.Add(Format('  %s:       %5d', [R0s, R0]));
      MEMOSummary.Lines.Add('');

      MEMOSummary.Lines.Add('POINTS');
      MEMOSummary.Lines.Add(Format('  Source:     %5d',[CoveragePoints.ValidPointsQty]));
      MEMOSummary.Lines.Add(Format('  Foreground: %5d', [CoveragePoints.ValidEnabledPointsQty]));
      MEMOSummary.Lines.Add('');

      MEMOSummary.Lines.Add('COVERAGE');
      MEMOSummary.Lines.Add(Format('  Source:     %5.1f',[TotalCoverage])+'%');
      MEMOSummary.Lines.Add(Format('  Foreground: %5.1f', [EnabledCoverage])+'%');
    end {with};
end {TFormMain.FillSummary};


(***********************)
(* TFormMain.FormClose *)
(***********************)

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
  var
    I : TIniFile;
begin
  if ProjectDataBase_ <> nil then begin
    if (ProjectDataBase_.ChangedCount <> 0) then begin
      if FormOptions.CHKSaveState.Checked and (CommandLineParams_.Action = caNoAction)then begin
        SaveStateFile(StateFileName);
        I := TIniFile.Create(PrivateProfileFileName_);
        try
          I.WriteString(ProfileSectionStrings[psLastFiles],DefaultState_Key,StateFileName);
        finally
          I.Free;
        end {try};
      end {if};
    end {if};
    SaveInformationFile;
  end {if};
end {TFormMain.FormClose};


(****************************)
(* TFormMain.FormCloseQuery *)
(****************************)

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Process.Running then begin
    MessageDlg('Please, close the application before closing Discover.' ,mtError, [mbOk], 0);
    CanClose := false
  end else
    CanClose := True;
end {TFormMain.FormCloseQuery};


(************************)
(* TFormMain.FormCreate *)
(************************)

procedure TFormMain.FormCreate(Sender: TObject);
  var
    i, j : integer;

begin
  SortedRoutines := TSortedRoutines.Create;
  SortedRoutines.Duplicates := true;
  SortedUnits := TSortedUnits.Create;
  CoveredBitMap := TBitMap.Create;
  CoveredBitMap.Handle := LoadBitMap(hInstance, 'B_PT_COVERED');

  UnCoveredBitMap := TBitMap.Create;
  UnCoveredBitMap.Handle := LoadBitMap(hInstance, 'B_PT_UNCOVERED');

  EnabledBitMap := TBitMap.Create;
  EnabledBitMap.Handle := LoadBitMap(hInstance, 'B_ENABLED');

  DisabledBitMap := TBitmap.Create;
  DisabledBitMap.Handle := LoadBitMap(hInstance, 'B_DISABLED');

  LockShutBitMap := TBitMap.Create;
  LockShutBitmap.Handle := LoadBitMap(hInstance, 'B_LOCKSHUT');

  SendMessage(LBFile.Handle, LB_SETHORIZONTALEXTENT, Screen.Width, 0);
  Application.Title := ApplicationName;
  Application.ShowHint := true;
  Application.HookMainWindow(MainHook);
  Application.OnMessage := HandleMessage;
  Application.OnActivate := HandleApplActivate;
  Application.OnDeactivate := HandleApplDeActivate;
  Application.OnIdle := HandleIdleApplication;
  Application.OnException := HandleExceptions;
  MainWindowHandle_ := Handle;
  Process := TProcess.Create(Self);
  PCLeft.ActivePage := TSSummary;

  PCRight.ActivePage := TSCode;

  Config.GetIntegerArray(Appearence_Key, AppearanceArray);
  if AppearanceArray[0] <> -MaxInt then
    SetBounds(AppearanceArray[0], AppearanceArray[1], AppearanceArray[2], AppearanceArray[3]);
  if AppearanceArray[4] <> -MaxInt then
    PNLLeft.Width := AppearanceArray[4];
  j := 5;
  with HCRoutines.Sections do
    for i := 0 to pred(Count) do begin
      if AppearanceArray[j] <> -MaxInt then
        Items[i].Width := AppearanceArray[j];
      inc(j);
    end {for};
  with HCUnits.Sections do
    for i := 0 to pred(Count) do begin
      if AppearanceArray[j] <> -MaxInt then
        Items[i].Width := AppearanceArray[j];
      inc(j);
    end {for};
  CaptureBitmapStream := TMemoryStream.Create;
  OpenLibDialog.Filter := '*'+LibraryExtension+'|*'+LibraryExtension;
  SaveLibDialog.Filter := '*'+LibraryExtension+'|*'+LibraryExtension;
  OpenStateDialog.Filter := '*'+ProjectStateExtension+'|*'+ProjectStateExtension;
  ProjectInfo := TProjectInfo.Create;
end {TFormMain.FormCreate};


(*************************)
(* TFormMain.FormDestroy *)
(*************************)

procedure TFormMain.FormDestroy(Sender: TObject);
  var
    i, j : integer;
begin
  SortedRoutines.DeleteAll;
  SortedRoutines.Free;
  SortedUnits.DeleteAll;
  SortedUnits.Free;
  CoveredBitMap.Free;
  UnCoveredBitMap.Free;
  EnabledBitMap.Free;
  DisabledBitMap.Free;
  LockShutBitMap.Free;
  AppearanceArray[0] := Left;
  AppearanceArray[1] := Top;
  AppearanceArray[2] := Width;
  AppearanceArray[3] := Height;
  AppearanceArray[4] := PNLLeft.Width;
  j := 5;
  with HCRoutines.Sections do
    for i := 0 to pred(Count) do begin
      AppearanceArray[j] := Items[i].Width;
      inc(j);
    end {for};
  with HCUnits.Sections do
    for i := 0 to pred(Count) do begin
      AppearanceArray[j] := Items[i].Width;
      inc(j);
    end {for};
  Config.SetIntegerArray(Appearence_Key, AppearanceArray);
  CaptureBitmapStream.Free;
  ProjectInfo.Free;
end {TFormMain.FormDestroy};


(************************)
(* TFormMain.FormResize *)
(************************)

procedure TFormMain.FormResize(Sender: TObject);
begin
  AdjustStatusbar;
  Resizing := true;
  if not TIMERResize.Enabled then
    TIMERResize.Enabled := true;
end {TFormMain.FormResize};


(***********************************)
(* TFormMain.GotoNextCoveragePoint *)
(***********************************)

procedure TFormMain.GotoNextCoveragePoint(RedGreen: boolean);
  var
    n : integer;
    C : TCoveragePoint;

  function NextGreenCoveragePoint : TCoveragePoint;
  begin
    Result := nil;
    while (n < LBFile.Items.Count) do begin
      Result := TCoveragePoint(LBFile.Items.Objects[n]);
      if (Result <> nil) and (Result.Valid) and
        (((Result.Counter = 0) and (RedGreen) ) or ((Result.Counter <> 0) and (not RedGreen))) then
        break;
      inc(n);
    end {while};
  end {NextGreenCoveragePoint};

begin
  n := LBFile.ItemIndex;
  if n >= 0 then begin
    inc(n);
    C := NextGreenCoveragePoint;
    if C <> nil then begin
      LBFile.ItemIndex := n;
      LBFile.TopIndex := n - 10;
      DisplayStatusFilename;
      PositionRoutineViewToPoint(C);
    end {if};
  end {if};
end {TFormMain.GotoNextCoveragePoint};


(********************************)
(* TFormMain.HandleApplActivate *)
(********************************)

procedure TFormMain.HandleApplActivate;
begin
  if (ProjectDataBase_ <> nil) and (ProjectDataBase_.ChangedCount <> LastChangedCount) then
    UpdateOnActivate;
end {TFormMain.HandleApplActivate};


(**********************************)
(* TFormMain.HandleApplDeActivate *)
(**********************************)

procedure TFormMain.HandleApplDeActivate;
begin
  if (ProjectDataBase_ <> nil) then begin
    LastChangedCount := ProjectDataBase_.ChangedCount;
    DeltaCovered := 0;
    UpdateCoverageOnStatusBar;
  end {if};
end {TFormMain.HandleApplDeActivate};


(*****************************)
(* TFormMain.HandleErrorCode *)
(*****************************)

procedure TFormMain.HandleErrorCode(ErrorCode, Address : integer);
  var
    s : string;
begin
  s := Format('Cannot access process at %s. GetLastError=%d, "%s"' ,
   [IntToHex(Address,8), ErrorCode, SysErrorMessage(ErrorCode)]);;
  if LogFileEnabled_ then
    Writeln(LogFile_, s);
end {TFormMain.HandleErrorCode};


(******************************)
(* TFormMain.HandleExceptions *)
(******************************)

procedure TFormMain.HandleExceptions(Sender: TObject; E: Exception);
begin
  Application.Restore;
  MessageDlg(E.Message, mtError, [mbOk], 0); 
end {TFormMain.HandleExceptions};


(***********************************)
(* TFormMain.HandleIdleApplication *)
(***********************************)

procedure TFormMain.HandleIdleApplication;
begin
  if not InitialScreenDone then begin
    InitialScreenDone := true;
  end {if};

  // Do we have an autorun
  if CommandLineActionEnabled_ then begin
    CommandLineActionEnabled_ := false;
    DoCommandLineInitialActions;
  end {if};
end {TFormMain.HandleIdleApplication};


(********************************)
(* TFormMain.HandleJournalEvent *)
(********************************)

procedure TFormMain.HandleJournalEvent;

  procedure DefaultHandling;
  begin
    case Event of
      jeTimerElapsed:;

      jeProcessJustTerminated: begin
        if (WindowState <> wsMinimized) or not CommandLineParams_.RunMinimized then
          Application.Restore;
        StateMachineState := stIdle;
      end;

      jeApplRestored: begin
        Application.Restore;
        CommandLineParams_.Action := caNoAction;
        StateMachineState := stIdle;
      end;

      jeWMCancelJournal, {jeApplRestored,} jeCtrlBreak,
        {jeProcessJustTerminated,} jeDebugString: begin
        StateMachineState := stIdle;
        Application.Restore;
      end;
    end {case}
  end {DefaultHandling};

  procedure StateIdle;
  begin
    case Event of

      jePlayStream: begin
      end;

      jePlayStreamsToPlay: begin
      end;

      jePlayNodesToPlay: begin
      end;

      jeRecord, jeRecordContinue: begin
      end;
    else {case}
      DefaultHandling;
    end {case};
  end {StateIdle};

  procedure StateWaitForProcessRunning;
  begin
    case Event of

      jeProcessJustRunning: begin
        // We wait again for the process to be really ready to accept input
        StateMachineState := stWaitForProcessReadyForPlaying;
      end;

      jeTimerElapsed: begin
        // The process doesn't run, complain
        StateMachineState := stIdle;
        raise Exception.Create('The application will not run')
      end;

      jeApplrestored:
        // ignore it...

    else
      DefaultHandling;
    end {case};
  end;

  procedure StateWaitForProcessReadyForPlaying;
  begin
    case Event of
      jeTimerElapsed: begin
        StateMachineState := stIdle;
        HandleJournalEvent(jePlayStream);
      end;

      jeApplRestored:
        // Just ignore it
    else
      DefaultHandling;
    end {case};
  end;

begin
  if LogFileEnabled_ then begin
    WriteLn(LogFile_, Format('%d Event=%s, in state=%s',
     [GetTickCount, JournalEventString[Event],
     StateMachineStrings[StateMachineState]
     ]));
  end {if};
  case StateMachineState of
    stIdle:
      StateIdle;
    stWaitForProcessRunning:
      StateWaitForProcessRunning;
  end {case};
end {TFormMain.HandleJournalEvent};


(***************************)
(* TFormMain.HandleMessage *)
(***************************)

procedure TFormMain.HandleMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if Msg.Message = WM_CANCELJOURNAL then
    HandleJournalEvent(jeWMCancelJournal);
end {TFormMain.HandleMessage};


(********************************)
(* TFormMain.HandlePointCovered *)
(********************************)

procedure TFormMain.HandlePointCovered(Point : TCoveragePoint);
  var
    R : TRoutine;
begin
  with ProjectDataBase_ do begin
    SetCovered(Point);
    R := Routines.At(Point.RoutineIndex);
    UpdateOverView(R);
    UpdateFileLine(Point);
    inc(DeltaCovered);
    UpdateCoverageOnStatusBar;
  end {with};
 end {TFormMain.HandlePointCovered};


(**************************************)
(* TFormMain.HandleProcessDebugString *)
(**************************************)

procedure TFormMain.HandleProcessDebugString;
begin
  HandleJournalEvent(jeWMCancelJournal);
  if LogFileEnabled_ then
    Writeln(LogFile_, Format('Debug string received: "%s"',[DebugString]));
  // MessageDlg(Format('Debug string received: "%s"',[DebugString]), mtWarning, [mbOk], 0);
end {TFormMain.HandleProcessDebugString};


(**********************************)
(* TFormMain.HandleProcessRunning *)
(**********************************)

procedure TFormMain.HandleProcessRunning;
begin
  HandleJournalEvent(jeProcessJustRunning);
  StatusBar.Panels[pProcessState].Text := 'Running';
end {TFormMain.HandleProcessRunning};


(*************************************)
(* TFormMain.HandleProcessTerminated *)
(*************************************)

procedure TFormMain.HandleProcessTerminated;
begin
  HandleJournalEvent(jeProcessJustTerminated);
  StatusBar.Panels[pProcessState].Text := '';
  MMApplicationTerminate.Enabled := false;
  UpdateOnActivate;
  Timer1.Enabled := true;
end {TFormMain.HandleProcessTerminated};


(****************************)
(* TFormMain.HandleProgress *)
(****************************)

procedure TFormMain.HandleProgress(Percent : integer; const ActionString : string);
  var
    s : string;
begin
  if PerCent <> PrevPerCent then begin
    if ActionString <> '' then
      s := Format('%s (%d%s)',[ActionString, PerCent,'%'])
    else
      s := '';
    Statusbar.Panels[pProgress].Text := s;
    Statusbar.Refresh;
    PRevPerCent := PerCent;
  end {if};
end {TFormMain.HandleProgress};


(******************************)
(* TFormMain.HCRoutinesResize *)
(******************************)

procedure TFormMain.HCRoutinesResize(Sender: TObject);
begin
  LBRoutines.Refresh;
end {TFormMain.HCRoutinesResize};


(************************************)
(* TFormMain.HCRoutinesSectionClick *)
(************************************)

procedure TFormMain.HCRoutinesSectionClick(
  HeaderControl: THeaderControl; Section: THeaderSection);
begin
  case Section.Index of
    0: begin
      if SortedRoutines.Mode = srByName then
        SortedRoutines.Inverted := not SortedRoutines.Inverted
      else begin
        SortedRoutines.Mode := srByName;
        SortedRoutines.Inverted := false;
      end {if};
    end;

    1: begin
      if SortedRoutines.Mode = srByUnit then
        SortedRoutines.Inverted := not SortedRoutines.Inverted
      else begin
        SortedRoutines.Mode := srByUnit;
        SortedRoutines.Inverted := false;
      end {if};
    end;

    2: begin
      if SortedRoutines.Mode = srByPoints then
        SortedRoutines.Inverted := not SortedRoutines.Inverted
      else begin
        SortedRoutines.Mode := srByPoints;
        SortedRoutines.Inverted := false;
      end {if};
    end;

    3: begin
      if SortedRoutines.Mode = srByCoverage then
        SortedRoutines.Inverted := not SortedRoutines.Inverted
      else begin
        SortedRoutines.Mode := srByCoverage;
        SortedRoutines.Inverted := false;
      end {if};
    end;
  end {case};
  Screen.Cursor := crHourGlass;
  try
    FillSortedRoutineList;
    FillLBRoutines;
    PBOverViewPaint(PBOverView);
  finally
    Screen.Cursor := crDefault;
  end {try};
end {TFormMain.HCRoutinesSectionClick};


(****************************)
(* TFormMain.HCSectionTrack *)
(****************************)

procedure TFormMain.HCSectionTrack(
  HeaderControl: THeaderControl; Section: THeaderSection; Width: Integer;
  State: TSectionTrackState);

  var
    H : PHeaderTrackingInfo;
    LB : TListBox;

  procedure DrawTrackingRect(Draw : boolean);
    var
      P : TPoint;
    begin
      if Draw then begin
        with LB, Canvas, H^, TrackingRect do begin
          GetCursorPos(P);
          P := ScreenToClient(P);
          Left := P.X;
          Right := succ(Left);
          Top := 0;
          Bottom := ClientRect.Bottom;
          InvertRect(Handle, TrackingRect);
        end {with};
      end else
        InvertRect(LB.Canvas.Handle, H.TrackingRect);
      H.TrackingRectDrawn := Draw;
  end {DrawTrackingRect};

begin
  if HeaderControl = HCRoutines then begin
    H := @HeaderRoutinesInfo;
    LB := LBRoutines;
  end else begin
    H := @HeaderUnitsInfo;
    LB := LBUnits;
  end {if};
  case State of
    tsTrackBegin:
      DrawTrackingRect(true);
    tsTrackEnd: begin
      DrawTrackingRect(false);
      InvalidateRect(LB.Handle, nil, true);
    end;
    tsTrackMove: begin
      DrawTrackingRect(false);
      DrawTrackingRect(true);
    end;
  end {case};
end {TFormMain.HCSectionTrack};


(*********************************)
(* TFormMain.HCUnitsSectionClick *)
(*********************************)

procedure TFormMain.HCUnitsSectionClick(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
  case Section.Index of
    0: begin
      if SortedUnits.Mode = suByName then
        SortedUnits.Inverted := not SortedUnits.Inverted
      else begin
        SortedUnits.Mode := suByName;
        SortedUnits.Inverted := false;
      end {if};
    end;

    1: begin
      if SortedUnits.Mode = suBySize then
        SortedUnits.Inverted := not SortedUnits.Inverted
      else begin
        SortedUnits.Mode := suBySize;
        SortedUnits.Inverted := false;
      end {if};
    end;

    2: begin
      if SortedUnits.Mode = suByRQty then
        SortedUnits.Inverted := not SortedUnits.Inverted
      else begin
        SortedUnits.Mode := suByRQty;
        SortedUnits.Inverted := false;
      end {if};
    end;

    3: begin
      if SortedUnits.Mode = suByR0Pc then
        SortedUnits.Inverted := not SortedUnits.Inverted
      else begin
        SortedUnits.Mode := suByR0Pc;
        SortedUnits.Inverted := false;
      end {if};
    end;

    4: begin
      if SortedUnits.Mode = suByR100Pc then
        SortedUnits.Inverted := not SortedUnits.Inverted
      else begin
        SortedUnits.Mode := suByR100Pc;
        SortedUnits.Inverted := false;
      end {if};
    end;

    5: begin
      if SortedUnits.Mode = suByCoverage then
        SortedUnits.Inverted := not SortedUnits.Inverted
      else begin
        SortedUnits.Mode := suByCoverage;
        SortedUnits.Inverted := false;
      end {if};
    end;
  end {case};
  Screen.Cursor := crHourGlass;
  try
    FillSortedUnitList;
    FillLBUnits;
  finally
    Screen.Cursor := crDefault;
  end {try};
end {TFormMain.HCUnitsSectionClick};


(**********************************)
(* TFormMain.HCUnitsSectionResize *)
(**********************************)

procedure TFormMain.HCUnitsSectionResize(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
  LBUnits.Refresh;
end {TFormMain.HCUnitsSectionResize};


(**************************************)
(* TFormMain.InitAfterLoadingDatabase *)
(**************************************)

procedure TFormMain.InitAfterLoadingDatabase;
begin
  StatusBar.Panels[pFilePos].Text := 'Initializing data';
  try
    ProjectDatabase_.InitStatistics;
    SortedRoutines.Mode := srByName;
    FillSortedRoutineList;
    SortedUnits.Mode := suByName;
    CurrentSourceFileName := '';
    FillSortedUnitList;

    FillLBRoutines;
    FillLBUnits;
    LBFile.Clear;

    // Set the active pages
    PCLeft.ActivePage := TSSummary;
    // PCRight.ActivePage := TSCode;

    PBOverViewPaint(PBOverView);

    if ProjectDataBase_.ExecutableFileName <> '' then begin
      Caption := Format('%s - %s',[ApplicationName, ProjectDataBase_.ExecutableFileName]);
    end else
      Caption := ApplicationName;
    Application.Title := Caption;
    FillSummary;
    DeltaCovered := 0;
    UpdateCoverageOnStatusBar;
    if ProjectDataBase_.Routines.Count < 1000 then
      OverviewPointSquareSide := 8
    else
      OverviewPointSquareSide := 5;
  finally
    StatusBar.Panels[pFilePos].Text := '';
  end {try};
end {TFormMain.InitAfterLoadingDatabase};


(*************************)
(* TFormMain.LBFileClick *)
(*************************)

procedure TFormMain.LBFileClick(Sender: TObject);
  var
    C : TCoveragePoint;
begin
  // Align the routine list with the selected line
  with LBFile do
    if LBRoutines.Visible and (ItemIndex >= 0) then begin
      C := TCoveragePoint(Items.Objects[ItemIndex]);
      PositionRoutineViewToPoint(C);
    end {if};
  DisplayStatusFilename;
end {TFormMain.LBFileClick};


(****************************)
(* TFormMain.LBFileDrawItem *)
(****************************)

procedure TFormMain.LBFileDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
  const
    HOffset = 20;
    MarkSide = 16;
  var
    C : TCoveragePoint;
    R1, R2 : TRect;

  function ExpandTabs(const s : string) : string;
    var
      i,j : integer;
  begin
    Setlength(Result,length(s));
    Result := '';
    i := 1;
    while i <= Length(s) do begin
      if s[i] = TAB then begin
        j := 8*succ(length(Result) div 8);
        while Length(Result) < j do
          Result := Result + ' ';
      end else
        Result := Result + s[i];
      inc(i);
    end {while};
  end {Expandtabs};

begin
  with Control as TListBox, Canvas do begin
    if odFocused in State then
      DrawFocusRect(Rect);
    if odSelected in State then
      Brush.Color := clHighLight
    else
      Brush.Color := clWindow;
    FillRect(Rect);
    C :=  TCoveragePoint(Items.Objects[Index]);
    if (C <> nil) and C.Valid then begin
      with R1 do begin
        Left := (HOffset - MarkSide) div 2;
        Right := Left + MarkSide;
        Top := Rect.Top + (ItemHeight - MarkSide) div 2;
        Bottom := Top + MarkSide;
      end {with};
      with R2 do begin
        Top := 0;
        Left := 0;
        Right := MarkSide;
        Bottom := MarkSide;
      end {with};
      if C.Counter > 0 then
        BrushCopy(R1, CoveredBitMap, R2, clWhite)
      else
        BrushCopy(R1, UncoveredBitMap, R2, clWhite);

    end {if};
    TextOut(Rect.Left+HOffset, Rect.Top, ExpandTabs(Items[Index]));
  end {with};
end {TFormMain.LBFileDrawItem};


(****************************)
(* TFormMain.LBFileKeyPress *)
(****************************)

procedure TFormMain.LBFileKeyPress(Sender: TObject; var Key: Char);

begin
  case Key of
    #7:
      GotoNextCoveragePoint(false);
    #18:
      GotoNextCoveragePoint(true);
  else
  end {case};
end {TFormMain.LBFileKeyPress};


(*****************************)
(* TFormMain.LBFileMouseDown *)
(*****************************)

procedure TFormMain.LBFileMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then begin;
    if LBFile.ItemIndex = - 1 then
      LBFile.ItemIndex := LBFile.ItemAtPos(Point(X, Y), True);
  end {if};
end {TFormMain.LBFileMouseDown};


(*****************************)
(* TFormMain.LBRoutinesClick *)
(*****************************)

procedure TFormMain.LBRoutinesClick(Sender: TObject);
  var
    C : TCoveragePoint;
    S : TStringList;
    R : TRoutine;
    i, LineNr, PointIndex, RoutineIndex : integer;
begin
  if LBRoutines.ItemIndex >= 0 then
    CurrentRoutine := SortedRoutines.At(LBRoutines.ItemIndex);
    CurrentUnit := ProjectDataBase_.Units.At(CurrentRoutine.UnitIndex);
    // Update LBFile
    if (CurrentRoutine.FileIndex >= 0) and (CurrentSourceFileName <> CurrentUnit.FileNames[CurrentRoutine.FileIndex]) then begin
      CurrentSourceFileName := CurrentUnit.FileNames[CurrentRoutine.FileIndex];
      S := TStringList.Create;
      Screen.Cursor := crHourGlass;
      try
        S.LoadFromFile(CurrentSourceFileName);
        RoutineIndex := CurrentUnit.FirstRoutineIndex;
        R := ProjectDataBase_.Routines.At(RoutineIndex);
        // Move until the first routine in this file
        while (R.FileIndex <> CurrentRoutine.FileIndex) and
          (RoutineIndex < ProjectDataBase_.Routines.Count) do begin
          inc(RoutineIndex);
          if RoutineIndex < ProjectDataBase_.Routines.Count then
            R := ProjectDataBase_.Routines.At(RoutineIndex);
        end {while};
        PointIndex := R.FirstPointIndex;
        if PointIndex >= 0 then
          C := ProjectDataBase_.CoveragePoints.At(PointIndex)
        else
          // Must be a finalization routine without explicit code
          C := nil;
        LBFile.Items.BeginUpdate;
        LBFile.Clear;
        // Scan all file lines
        with S do
          for i := 0 to pred(Count) do begin
            LineNr := succ(i);
            if (C <> nil) and (LineNr = C.LineNumber) then begin
              LBFile.Items.AddObject(S[i], C);
              C := nil;
              inc(PointIndex);
              if PointIndex < ProjectDataBase_.CoveragePoints.Count then
                C := ProjectDataBase_.CoveragePoints.At(PointIndex);
            end else
              LBFile.Items.AddObject(S[i],nil)
          end {for};
        LBFile.Items.EndUpdate;
      finally
        S.Free;
        Screen.Cursor := crDefault;
      end {try};
    end {if};
    // Scroll into position
    if CurrentRoutine.FileIndex >= 0 then begin
      C := ProjectDataBase_.CoveragePoints.At(CurrentRoutine.FirstPointIndex);
      SendMessage(LBFile.Handle, LB_SETTOPINDEX, C.LineNumber-10, 0);
      LBFile.ItemIndex := pred(C.LineNumber);
      DisplayStatusFilename;
      // Set LBUnit item index
      LBUnits.TopIndex := SortedUnits.IndexOf(CurrentUnit);
    end {if};
end {TFormMain.LBRoutinesClick};


(********************************)
(* TFormMain.LBRoutinesDrawItem *)
(********************************)

procedure TFormMain.LBRoutinesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
    Routine, PreviousRoutine : TRoutine;
    R : TRect;
    U : TUnit;
    x : single;
    s : string;
    p, q : integer;
begin
  with (Control as TListBox), Canvas do begin
    Routine := SortedRoutines.At(Index);
    if Index > 0 then
      PreviousRoutine := SortedRoutines.At(pred(Index))
    else
      PreviousRoutine := nil;
    U := ProjectDataBase_.Units.At(Routine.UnitIndex);
    if odSelected in State then begin
      Brush.Color := clHighLight;
      Font.Color := clHighLightText;
    end else begin
      Brush.Color := clWindow;
      Font.Color := clWindowText;
    end {if};
    FillRect(Rect);
    if Routine.Disabled then
      Font.Color := clGrayText;
    with R do begin
      Top := Rect.Top;
      Bottom := Rect.Bottom;
      Left := HCRoutines.Sections[0].Left;
      Right := HCRoutines.Sections[0].Right;
    end {with};
    s := Routine.Name;
    if PreviousRoutine <> nil then begin
      // Do we have the same class name
      p := Pos('.', PreviousRoutine.Name);
      q := Pos('.', Routine.Name);
      if (p <> 0) and (q <> 0) and (p = q) and
        (Copy(PreviousRoutine.Name, 1,p)=Copy(Routine.Name,1,p)) then
          // same class name, strip it
          s := '     '+Copy(Routine.Name,q, Length(Routine.Name))
    end {if};
    DrawStringInColumn(Canvas, R, s, false);

    with R do begin
      Left := HCRoutines.Sections[1].Left;
      Right := HCRoutines.Sections[1].Right;
    end {with};
    DrawStringInColumn(Canvas, R, U.Name, false);

    with R do begin
      Left := HCRoutines.Sections[2].Left;
      Right := HCRoutines.Sections[2].Right;
    end {with};
    DrawStringInColumn(Canvas, R, IntToStr(Routine.ValidPointsQty), true);

    with R do begin
      Left := HCRoutines.Sections[3].Left;
      Right := HCRoutines.Sections[3].Right;
    end {with};
    if Routine.ValidPointsQty = 0 then
      s := '?'
    else begin
      x := Routine.CoveredPointsQty;
      x := 100.0*(x / Routine.ValidPointsQty);
      s := Format('%3.0f',[x])+'%';
    end {if};
    DrawStringInColumn(Canvas, R, s, true);
  end {with};
end {TFormMain.LBRoutinesDrawItem};


(********************************)
(* TFormMain.LBRoutinesKeyPress *)
(********************************)

procedure TFormMain.LBRoutinesKeyPress(Sender: TObject; var Key: Char);
  var
    R : TRoutine;
    i : integer;
    FormEdit : TFormEdit;
begin
  if not (Key in ['a'..'z', 'A'..'Z','_']) then begin
    case Key of
      #2: // Ctrl-B
        PURoutinesEnableDisableClick(PURoutinesDisable);
      #6: // Ctrl-F
        PURoutinesEnableDisableClick(PURoutinesEnable);
    else
    end {case};
  end else begin
    FormEdit := TFormEdit.Create(Self);
    try
      // Position the list box to the required routines
      if Key in ['a'..'z', 'A'..'Z','_'] then
        FormEdit.Edit1.Text := Key
      else
        FormEdit.Edit1.Text := '';
      if FormEdit.ShowModal = mrOk then begin
        R := TRoutine.Create;
        try
          R.Name := FormEdit.Edit1.Text;
          SortedRoutines.Search(R,i);
          LBRoutines.ItemIndex := i;
        finally
          R.Free;
        end {try};
      end {if};
    finally
      FormEdit.Free;
    end {try};
  end {if};
end {TFormMain.LBRoutinesKeyPress};


(*********************************)
(* TFormMain.LBRoutinesMouseDown *)
(*********************************)

procedure TFormMain.LBRoutinesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    i : integer;
begin
  if Button = mbRight then begin
    i := LBRoutines.ItemAtPos(Point(x,y), true);
    if i >= 0 then begin
      LBRoutines.ItemIndex := i;
      LBRoutinesClick(LBRoutines);
    end {if};
  end {if};
end {TFormMain.LBRoutinesMouseDown};


(**************************)
(* TFormMain.LBUnitsClick *)
(**************************)

procedure TFormMain.LBUnitsClick(Sender: TObject);
  var
    S : TStringList;
    RoutineIndex, PointIndex, i, LineNr : integer;
    R : TRoutine;
    C : TCoveragePoint;
begin
  if LBUnits.ItemIndex >= 0 then
    CurrentUnit := LBUnits.Items.Objects[LBUnits.ItemIndex] as TUnit;
    // Update LBFile
    if (CurrentUnit.FileNames.Count > 0) then begin
      CurrentSourceFileName := CurrentUnit.FileNames[0];
      S := TStringList.Create;
      Screen.Cursor := crHourGlass;
      try
        S.LoadFromFile(CurrentSourceFileName);
        RoutineIndex := CurrentUnit.FirstRoutineIndex;
        R := ProjectDataBase_.Routines.At(RoutineIndex);
        PointIndex := R.FirstPointIndex;
        if PointIndex >= 0 then
          C := ProjectDataBase_.CoveragePoints.At(PointIndex)
        else
          // Must be a finalization routine without explicit code
          C := nil;
        LBFile.Items.BeginUpdate;
        LBFile.Clear;
        // Scan all file lines
        with S do
          for i := 0 to pred(Count) do begin
            LineNr := succ(i);
            if (C <> nil) and (LineNr = C.LineNumber) then begin
              LBFile.Items.AddObject(S[i], C);
              C := nil;
              inc(PointIndex);
              if PointIndex < ProjectDataBase_.CoveragePoints.Count then
                C := ProjectDataBase_.CoveragePoints.At(PointIndex);
            end else
              LBFile.Items.AddObject(S[i],nil)
          end {for};
        LBFile.Items.EndUpdate;
      finally
        S.Free;
        Screen.Cursor := crDefault;
        DisplayStatusFilename;
      end {try};
    end {if};
end {TFormMain.LBUnitsClick};


(*****************************)
(* TFormMain.LBUnitsDrawItem *)
(*****************************)

procedure TFormMain.LBUnitsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
  var
    U : TUnit;
    R : TRect;
    s : string;
    x : single;

  procedure DrawCheckBox(const R : TRect; Canvas : TCanvas);
    var
      CheckBoxRect, BitMapRect : TRect;
      B : TBitmap;
  begin
    with Canvas do begin
      if not U.IsSourceAvailable then
        B := LockShutBitMap
      else if U.Disabled then
        B := DisabledBitMap
      else
        B := EnabledBitMap;
      with CheckBoxRect do begin
        Left := R.Left + 2;
        Top := R.Top;
        Right := Left + B.Width;
        Bottom := Top + B.Height;
      end {with};
      with BitMapRect do begin
        Left := 0;
        Top := 0;
        Right := B.Width;
        Bottom := B.Height;
      end {with};
      BrushCopy(CheckBoxRect, B, BitMapRect, clOlive);
    end {with};
  end {DrawCheckBox};

begin
  with (Control as TListBox), Canvas do begin
    Brush.Style := bsSolid;
    if odSelected in State then
      Brush.Color := clHighLight
    else
      Brush.Color := clWindow;
    FillRect(Rect);
    U := SortedUnits.At(Index);
    with R do begin
      Top := Rect.Top;
      Bottom := Rect.Bottom;
      Left := HCUnits.Sections[0].Left;
      Right := HCUnits.Sections[0].Right;
    end {with};
    DrawCheckBox(R, Canvas);
    inc(R.Left, EnabledBitMap.Width+4);
    if U.Disabled then
      Pen.Color := clGrayText;
    DrawStringInColumn(Canvas, R, U.Name, false);

    with R do begin
      Left := HCUnits.Sections[1].Left;
      Right := HCUnits.Sections[1].Right;
    end {with};
    DrawStringInColumn(Canvas, R, IntToStr(U.Size), true);

    with R do begin
      Left := HCUnits.Sections[2].Left;
      Right := HCUnits.Sections[2].Right;
    end {with};
    DrawStringInColumn(Canvas, R, IntToStr(U.RoutinesQty), true);

    if U.IsSourceAvailable then begin
      with R do begin
        Left := HCUnits.Sections[3].Left;
        Right := HCUnits.Sections[3].Right;
      end {with};
      DrawStringInColumn(Canvas, R, IntToStr(U.R0Pc), true);

      with R do begin
        Left := HCUnits.Sections[4].Left;
        Right := HCUnits.Sections[4].Right;
      end {with};
      DrawStringInColumn(Canvas, R, IntToStr(U.R100Pc), true);

      with R do begin
        Left := HCUnits.Sections[5].Left;
        Right := HCUnits.Sections[5].Right;
      end {with};
      if U.ValidPointsQty = 0 then
        s := '?'
      else begin
        x := U.CoveredPointsQty;
        x := 100.0*(x / U.ValidPointsQty);
        s := Format('%3.0f',[x])+'%';
      end {if};
      DrawStringInColumn(Canvas, R, s, true);
    end {if};
  end {with};
end {TFormMain.LBUnitsDrawItem};


(*****************************)
(* TFormMain.LBUnitsKeyPress *)
(*****************************)

procedure TFormMain.LBUnitsKeyPress(Sender: TObject; var Key: Char);
  var
    U : TUnit;
    i : integer;
    FormEdit : TFormEdit;
begin
  if not (Key in ['a'..'z', 'A'..'Z','_']) then begin
    case Key of
      #1: // Ctrl-A
        PUUnitsSelectAllClick(Sender);
      #2: // Ctrl-B
        PUUnitsEnableDisableClick(PUUnitsDisable);
      #6: // Ctrl-F
        PUUnitsEnableDisableClick(PUUnitsEnable);
      #4: // Ctrl-f
        PUUnitsSelectGroupClick(LBUnits);
    else
    end {case};
  end else begin
    FormEdit := TFormEdit.Create(Self);
    try
      // Position the list box to the required routines
      if Key in ['a'..'z', 'A'..'Z','_'] then
        FormEdit.Edit1.Text := Key
      else
        FormEdit.Edit1.Text := '';
      if FormEdit.ShowModal = mrOk then begin
        U := TUnit.Create;
        try
          U.Name := FormEdit.Edit1.Text;
          SortedUNits.Search(U,i);
          LBUnits.TopIndex := i;
          if i >= 0 then
            LBUnits.Selected[i] := true;
        finally
          U.Free;
        end {try};
      end {if};
    finally
      FormEdit.Free;
    end {if};
  end {if};
end {TFormMain.LBUnitsKeyPress};


(*********************************)
(* TFormMain.LoadInformationFile *)
(*********************************)

procedure TFormMain.LoadInformationFile;
  var
    U : TUnit;
    R : TRoutine;
    i : integer;
begin
  if not FileExists(InfoFileName) then
    exit;
  ProjectInfo.LoadFromFile(InfoFileName);
  FormProjectInfo.EDITRunParameters.Text := ProjectInfo.RunParameters;
  FormProjectInfo.EDITStartupDirectory.Text := ProjectInfo.StartupDirectory;
  FormProjectInfo.CHKRunMaximized.Checked := ProjectInfo.RunMaximized;
  for i := 0 to pred(ProjectInfo.BackGndUnits.Count) do begin
    U := ProjectDataBase_.Units.AtName(ProjectInfo.BackGndUnits[i]);
    if U <> nil then
      ProjectDataBase_.EnableDisableUnit(U, false);
  end {for};
  for i := 0 to pred(ProjectInfo.BackGndRoutines.Count) do begin
    R := ProjectDataBase_.Routines.AtName(ProjectInfo.BackGndRoutines[i]);
    if R <> nil then
      ProjectDataBase_.EnableDisableRoutine(R, false);
  end {for};
end {TFormMain.LoadInformationFile};


(********************************)
(* TFormMain.LoadLastSavedState *)
(********************************)

procedure TFormMain.LoadLastSavedState;
  var
    I : TIniFile;
    FileName : string;
begin
  I := TIniFile.Create(PrivateProfileFileName_);
  try
    FileName := I.ReadString(ProfileSectionStrings[psLastFiles],DefaultState_Key,'');
    if (FileName <> '') and FileExists(FileName) then begin
      LoadState(FileName);
      StateFileName := FileName;
      InfoFileName := ChangeFileExt(StateFileName, ProjectInformationExtension);;
      ExeFileName := ProjectDataBase_.ExecutableFileName;
      LoadInformationFile;
    end {if};
  finally
    I.Free;
  end {try};
end {TFormMain.LoadLastSavedState};


(*************************)
(* TFormMain.LoadProject *)
(*************************)

procedure TFormMain.LoadProject(const DelphiProjectFileName: string);
  var
    DelphiProjectPath : string;
    MapFileName: string;
    IsBDS : boolean;
    s1, s2, s3 : string;
    NotFoundFiles : TStringList;

    // Information taken either in the 'dof' or the 'bdsproj' or the dproj file
    ProjectInfos : record
      OutputDir : string;
      Conditionals : string;
      SearchPath : string;
      ImageBase : integer;
    end;

  procedure BuildSearchPath(s : string);
    var
      p : integer;

    procedure AddPath(Path : string);
      const
        MacroName = '$(DELPHI)';
      var
        p : integer;
        s : string;
    begin
      // Do we have a macro in the path
(*
      p := Pos(MacroName, Path);
      if p > 0 then begin
        Delete(Path, p, Length(MacroName));
        Insert('', Path, p);
      end {if};
*)
      s := ExpandFileName(Path);
      SearchPath_.Add(s);
    end {AddPath};

  begin
    SearchPath_.Clear;
    SearchPath_.Add(Copy(DelphiProjectPath, 1, pred(Length(DelphiProjectPath))));
    while s <> '' do begin
      p := Pos(';',s);
      if p > 0 then begin
        AddPath(Copy(s,1, pred(p)));
        s := Copy(s,succ(p),Length(s));
      end else begin
        AddPath(s);
        s := '';
      end {if};
    end {while};
  end {BuildSearchPath};

  procedure BuildConditionnals(s : string);
    var
      p : integer;
  begin
    GlobalDefinedConditionnals_.Clear;
    while s <> '' do begin
      p := Pos(';',s);
      if p > 0 then begin
        GlobalDefinedConditionnals_.Add(Copy(s,1,pred(p)));
        s := Copy(s,succ(p),Length(s));
      end else begin
        GlobalDefinedConditionnals_.Add(s);
        s := '';
      end {if};
    end {while};
  end {BuildConditionnals};

  procedure FilterOutCovereagePoints;
    var
      Parser : TCodeParser;
  begin
    Parser := TCodeParser.Create;
    Parser.OnProgressEvent := HandleProgress;
    try
      Parser.Parse;
    finally
      Parser.Free;
    end {try};
  end {FilterOutCovereagePoints};

  procedure LogDataBase;
  begin
    with TIniFile.Create(PrivateProfileFileName_) do
      try
        if ReadBool('Misc', 'LogDb', false) then begin
          ProjectDataBase_.Units.Print(LogFile_, 0);
          ProjectDataBase_.Routines.Print(LogFile_, 0);
        end {if};
      finally
        Free;
      end {try};
  end {LogDataBase};

  procedure ShowNotFoundSrcFiles;
   const
     MaxMissingFiles = 5;
    var
      s : string;
      i : integer;
  begin
    if NotFoundFiles.Count > 0 then begin
      s := '';
      i := 0;
      while (i < NotFoundFiles.Count) and (i <= MaxMissingFiles) do begin
       if Length(s) > 0 then
         s := s +CR+LF;
       s := s + NotFoundFiles.Strings[i];
       inc(i);
      end {while};

      if NotFoundFiles.Count > MaxMissingFiles then
       s := s + CR + LF +'And more...';

      MessageDlg(Format('The %d file(s):'+CR+LF+'%s'+CR+LF+
       'could not be found on the Delphi project search path.', [NotFoundFiles.Count, s]),
       mtWarning, [mbOk], 0);
    end {if};
  end {ShowNotFoundSrcFiles};

  procedure ExtractFromDof(const FileName : string);
    var
      ProjectOptions : TIniFile;
  begin
    if LogFileEnabled_ then
      Writeln(LogFile_, Format('Option file: %s', [FileName]));
    ProjectOptions := TIniFile.Create(FileName);
    try
      ProjectInfos.OutputDir := ProjectOptions.ReadString('Directories', 'OutputDir', '');
      if ProjectInfos.OutputDir <> '' then
        ProjectInfos.OutputDir := ProjectInfos.OutputDir + '/';
      ProjectInfos.SearchPath := ProjectOptions.ReadString('Directories', 'SearchPath', '');
      ProjectInfos.Conditionals := ProjectOptions.ReadString('Directories', 'Conditionals', '');
      ProjectInfos.ImageBase := ProjectOptions.ReadInteger('Linker', 'ImageBase',0);
    finally
      ProjectOptions.Free;
    end {try};
    IsBDS := false;
  end {ExtractFormDof};

  procedure ExtractFromBdsProj(const FileName : string);
    var
      F : TFileStream;
      s, Dp : string;
      p : integer;

    procedure LogError(const s : string);
    begin
      if LogFileEnabled_ then
        Writeln(LogFile_,s);
      raise Exception.Create(s);
    end {LogError};

    function PosEnd(const Substr, Str : string) : integer;
    begin
      Result := Pos(SubStr, Str);
      if Result > 0 then
        inc(Result, length(SubStr));
    end {PosEnd};

    function ExtractElement(const From, Element : string) : string;
      var
        b, e : integer;
        s : string;
    begin
      s := Format('<%s>', [Element]);
      b := Pos(s, From);
      if b = 0 then
        LogError(Format('%s not found', [s]));
      Result := Copy(From, b+length(s), Length(From));
      s := Format('</%s>', [Element]);
      e := Pos(s, Result);
      if e = 0 then
        LogError(Format('%s not found', [s]));
      Result := Copy(Result, 1, pred(e));
    end {ExtractElement};

    procedure SetDirElem(const ElemName : string; var Result : string);
      var
        p : integer;
        s : string;
    begin
      Result := '';
      p := PosEnd(Format('<Directories Name="%s">', [ElemName]), Dp);
      if p = 0 then
        exit;

      s := Copy(Dp, p, length(Dp));
      p := Pos('</Directories>', s);
      if p = 0 then
        exit;

      Result := Copy(s, 1, p-1);
    end {SetElem};

  begin
    if LogFileEnabled_ then
      Writeln(LogFile_, Format('Option file: %s', [FileName]));
    F := TFileStream.Create(Filename, fmOpenRead);
    try
      SetLength(s, F.Size);
      F.Read(s[1], F.Size);
      Dp := ExtractElement(s, 'Delphi.Personality');

      p := PosEnd('<Linker Name="ImageBase">', Dp);
      if p = 0 then
        LogError('ImageBase');
      s := Copy(Dp, p, length(Dp));
      p := Pos('</Linker>', s);
      if p = 0 then
        LogError('ImageBase');
      ProjectInfos.ImageBase := StrToInt(Copy(s, 1, p-1));

      SetDirElem('OutputDir', ProjectInfos.OutputDir);
      if ProjectInfos.OutputDir <> '' then
        ProjectInfos.OutputDir := ProjectInfos.OutputDir + '/';
      SetDirElem('SearchPath', ProjectInfos.SearchPath);
      SetDirElem('Conditionals', ProjectInfos.Conditionals);

    finally
      F.Free;
    end {try};
    IsBDS := true; // Bds map file format
  end {ExtractFromBdsProj};

  procedure ExtractFromDProj(const FileName : string);
    var
      F : TextFile;
      s, t : string;
      p : integer;
      Done : boolean;
  begin
    if LogFileEnabled_ then
      Writeln(LogFile_, Format('Option file: %s', [FileName]));
    FillChar(ProjectInfos, SizeOf(ProjectInfos), 0);
    ProjectInfos.ImageBase := $400000; // Assume it
    AssignFile(F, FileName);
    Reset(F);
    try
(**)
        // Locate the ''PropertyGroup Condition'' block with Debug
        while not Eof(F) do begin
          Readln(F, s);
          if (Pos('PropertyGroup Condition', s) > 0) and (Pos('Debug', s) > 0) then
            break;
        end {while};

(**)
      // Locate one of the required info
      Done := false;
      while (not Eof(F)) and not Done do begin
        Readln(F, s);
        repeat
          // Output Dir
          t := '<DCC_ExeOutput>';
          p := Pos(t, s);
          if p > 0 then begin
            s := Copy(s, p + length(t), length(s));
            p := Pos('<', s);
            ProjectInfos.OutputDir := Copy(s, 1, pred(p));
            break;
          end {if};

          // SearchPath
          t := '<DCC_IncludePath>';
          p := Pos(t, s);
          if p > 0 then begin
            s := Copy(s, p + length(t), length(s));
            p := Pos('<', s);
            ProjectInfos.SearchPath := Copy(s, 1, pred(p));
            break;
          end {if};

          // Conditionnals
          t := '<DCC_Define>';
          p := Pos(t, s);
          if p > 0 then begin
            s := Copy(s, p + length(t), length(s));
            p := Pos('<', s);
            ProjectInfos.Conditionals := Copy(s, 1, pred(p));
            break;
          end {if};

          // Imagebase, if defined
          t := '<DCC_ImageBase>';
          p := Pos(t, s);
          if p > 0 then begin
            s := Copy(s, p + length(t), length(s));
            p := Pos('<', s);
            ProjectInfos.ImageBase := StrToInt('$' + Copy(s, 1, pred(p)));
            break;
          end {if};

          // Is that the end of the group
(**)
            Done := Pos('</PropertyGroup>', s) > 0;

(**)
        until true;

      end {while};

      if ProjectInfos.OutputDir <> '' then
        ProjectInfos.OutputDir := ProjectInfos.OutputDir + '/';

    finally
      CloseFile(F);
    end {try};
    IsBDS := true; // BDS map file format
  end {ExtractFromDProj};

  procedure LogWindowsVersion;
    var
      VersionInfo : TOsVersionInfo;
  begin
    VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
    if Windows.GetVersionEx(VersionInfo) then begin
      Writeln(LogFile_, Format('Windows version Maj=%d Min=%d, BNr=%d, PId=%d %s ',
        [VersionInfo.dwMajorVersion, VersionInfo.dwMinorVersion,
        VersionInfo.dwBuildNumber, VersionInfo.dwPlatformId, StrPas(VersionInfo.szCSDVersion)]));
    end {if};

  end {LogWindowsVersion};

  begin {LoadProject}
    if LogFileEnabled_ then begin
      Writeln(LogFile_,'-');
      LogWindowsVersion;
      Writeln(LogFile_, Format('Opening project: %s', [DelphiProjectFileName]));
    end {if};
    DelphiProjectPath := ExtractFilePath(DelphiProjectFileName);
    if DelphiProjectPath <> '' then begin
      ChDir(DelphiProjectPath);
    end {if};

    // Locate the option file
    s1 := ExpandFileName(ChangeFileExt(DelphiProjectFileName, '.bdsproj'));
    s2 := ExpandFileName(ChangeFileExt(DelphiProjectFileName, '.dof'));
    s3 := ExpandFileName(ChangeFileExt(DelphiProjectFileName, '.dproj'));

    // Discover takes the dproj file, then the bdsproj file and finally the dof file
    if FileExists(s3) then begin
      ExtractFromDProj(s3)
    end else if FileExists(s1) then begin
      ExtractFromBdsProj(s1);
    end else if FileExists(s2) then begin
      ExtractFromdof(s2)
    end else
      raise Exception.Create(Format('File "%s" or "%s" or "%s" not found.', [s3, s1, s2]));

    if LogFileEnabled_ then begin
      Writeln(LogFile_, Format('  OutputDir=%s', [ProjectInfos.OutputDir]));
      Writeln(LogFile_, Format('  Conditionals=%s', [ProjectInfos.Conditionals]));
      Writeln(LogFile_, Format('  SearchPath=%s', [ProjectInfos.SearchPath]));
      Writeln(LogFile_, Format('  ImageBase=$%s', [IntToHex(ProjectInfos.ImageBase, 8)]));
    end {if};

    try
      // Reset the DataBase
      ProjectDataBase_.Free;
      ProjectDataBase_ := TProjectDataBase.Create;
      InitAfterLoadingDatabase;

      ProjectDataBase_.ExecutableFileName := ExpandFileName(ProjectInfos.OutputDir +
        ChangeFileExt(ExtractFileName(DelphiProjectFileName), '.exe'));
      ProjectDataBase_.ExecutableFileCRC := CRC32.FileCRC32(ProjectDataBase_.ExecutableFileName);

      MapFileName := ExpandFileName(ProjectInfos.OutputDir +
        ChangeFileExt(ExtractFileName(DelphiProjectFileName), '.map'));
      if LogFileEnabled_ then
        Writeln(LogFile_, Format('Map file: %s', [MapFileName]));

      GlobalDefinedConditionnals_.Clear;

      BuildSearchPath(ProjectInfos.SearchPath);

      if  FileExists(MapFileName) then begin
        StatusBar.Panels[pFilePos].Text := 'Processing map-file';
        NotFoundFiles := TStringList.Create;
        try
          HandleMapFile(MapFileName, HandleProgress, NotFoundFiles, IsBDS);

(* Don't show these files anymore!

          ShowNotFoundSrcFiles;
*)
        finally
          NotFoundFiles.Free;
        end {try};
      end else begin
        FreeAndNil(ProjectDataBase_);
        raise Exception.Create(Format('Map file "%s" not found.', [MapFileName]));
      end {if};


      // Extract the predefined conditionnals
      if ProjectInfos.Conditionals <> '' then
        BuildConditionnals(ProjectInfos.Conditionals);

      // Get image base
      ProjectDataBase_.ImageBase := ProjectInfos.ImageBase;

      LogDataBase;

      // Filter out the coverage points
      FilterOutCovereagePoints;
      InitAfterLoadingDataBase;

      inc(ProjectDataBase_.ChangedCount); // force dirty

    finally
    end {try};
end {TFormMain.LoadProject};


(***********************)
(* TFormMain.LoadState *)
(***********************)

procedure TFormMain.LoadState(const StateFileName: string);
  var
    F : TFileStream;
begin
  F := TFileStream.Create(StateFileName, fmOpenRead);
  Screen.Cursor := crHourGlass;
  try

    if LogFileEnabled_ then
      WriteLn(LogFile_, 'Loading state: '+StateFileName);
    StatusBar.Panels[pFilePos].Text := 'Loading state';

    ProjectDataBase_.Free;
    ProjectDataBase_ := TProjectDataBase.Load(F);

    LoadedStatesStr := ExtractFileName(StateFileName);
    InitAfterLoadingDataBase;
  finally
    F.Free;
    Screen.Cursor := crDefault;
  end {try};
end {TFormMain.LoadState};


(**********************)
(* TFormMain.MainHook *)
(**********************)

function TFormMain.MainHook;
begin
  Result := false;
  case Message.Msg of
    WM_ACTIVATE:
      with TWMACTIVATE(Message) do
        if (Active = WA_ACTIVE) and Minimized and (FStateMachineState<>stPlaying) then
          HandleJournalEvent(jeApplRestored);
    XM_COVEREDPOINT:
      HandlePointCovered(TCoveragePoint(Message.lParam));
    XM_DEBUGSTRING:
      // HandleProcessDebugString(PChar(Message.lParam))
      ;
    XM_RUNNING:
      HandleProcessRunning;
    XM_TERMINATED:
      HandleProcessTerminated;
    XM_PROCESSNOTCREATED: begin
      if LogFileEnabled_ then
        Writeln(LogFile_, Format('Process not created. Errorcode: 0x%s(%s)',
          [IntToHex(Message.lParam, 8), IntToStr(Message.lParam)]));
      raise Exception.Create(
        Format('Cannot create process, check if file "%s" exists or if the (options) startup directory "%s" exists.',
        [Process.ExeName, Process.StartupDirectory]));
      end;
    XM_ERRORCODE:
      HandleErrorCode(Message.WParam, Message.lParam);
  end {case};
end {TFormMain.MainHook};


(************************)
(* TFormMain.MakeReport *)
(************************)

procedure TFormMain.MakeReport;
  const
    Section = 'FormExport';
  var
    F : TextFile;
    i : integer;
    U : TUnit;
    R : TRoutine;
    UnitFileName, s, t : string;
    x : single;
    IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(PrivateProfileFileName_);
  try
    if IniFile.ReadBool(Section, 'CHKExportUnits', false) then begin
      t := IniFile.ReadString(Section, 'EDITUnitFileName', 'UnitsReport.txt');
      AssignFile(F, t);
      Rewrite(F);
      try
        with SortedUnits do
          for i := 0 to pred(Count) do begin
            U := At(i);
            if U.FileNames.Count > 0 then
              UnitFileName := U.FileNames.Strings[0]
            else
              UnitFileName := '?';
            if IniFile.ReadBool(Section, 'CHKStripPath', false) then
              UnitFileName := ExtractFileName(UnitFileName);
            if U.ValidPointsQty = 0 then
              s := '?'
            else begin
              x := U.CoveredPointsQty;
              x := 100.0*(x / U.ValidPointsQty);
              s := Format('%3.0f',[x])+'%';
            end {if};
            while s [1] = ' ' do s := Copy(s, 2, length(s));
            t := IniFile.ReadString(Section, 'EDITUnitFormatString','%0:s;%1:s;%2:s;%3:s;%4:s;%5:s;%6:s');
            Writeln(F, Format(t,
              [UnitFileName, U.Name, IntToStr(U.Size), IntToStr(U.RoutinesQty),
                IntToStr(U.R0Pc), IntToStr(U.R100Pc), s]));
          end {for};
      finally
        CloseFile(F);
      end {try};
    end {if};

    if IniFile.ReadBool(Section, 'CHKExportRoutines', false) then begin
      t := IniFile.ReadString(Section, 'EDITRoutineFileName', 'RoutinesReport.txt');
      AssignFile(F, t);
      Rewrite(F);
      try
        with SortedRoutines do
          for i := 0 to pred(Count) do begin
            R := At(i);
            U := ProjectDataBase_.Units.At(R.UnitIndex);
            if R.ValidPointsQty = 0 then
              s := '?'
            else begin
              x := R.CoveredPointsQty;
              x := 100.0*(x / R.ValidPointsQty);
              s := Format('%3.0f',[x])+'%';
            end {if};
            while s [1] = ' ' do s := Copy(s, 2, length(s));
            t := IniFile.ReadString(Section, 'EDITRoutineFormatString','%0:s;%1:s;%2:s;%3:s');
            Writeln(F, Format(t,
              [R.Name, U.Name, IntToStr(R.ValidPointsQty), s]));
          end {for};
      finally
        CloseFile(F);
      end {try};
    end {if};
  finally
    IniFile.Free;
  end {try};
end {TFormMain.MakeReport};


(********************************)
(* TFormMain.MergeWithStateFile *)
(********************************)

procedure TFormMain.MergeWithStateFile(const FileName : string);
  var
    StateStream : TFileStream;
    OldDataBase : TProjectDataBase;
begin
  StatusBar.Panels[pFilePos].Text := 'Merging states';
  StatusBar.Update;
  StateStream := TFileStream.Create(FileName, fmOpenRead);
  OldDataBase := TProjectDataBase.Load(StateStream);
  try
    ProjectDataBase_.MergeCoverage(OldDataBase);
    //ProjectDataBase_.Update(OldDataBase);
  finally
    OldDataBase.Free;
    StateStream.Free;
    InitAfterLoadingDataBase;
  end {try};
end {TFormMain.MergeWithStateFile};


(********************************)
(* TFormMain.MMApplicationClick *)
(********************************)

procedure TFormMain.MMApplicationClick(Sender: TObject);
begin
  MMApplicationRun.Enabled := (ProjectDataBase_ <> nil) and not
    Process.Created;
  MMApplicationTerminate.Enabled := Process.Running;
end {TFormMain.MMApplicationClick};


(***********************************)
(* TFormMain.MMApplicationRunClick *)
(***********************************)

procedure TFormMain.MMApplicationRunClick(Sender: TObject);
begin
  RunApplication;
end {TFormMain.MMApplicationRunClick};


(*****************************************)
(* TFormMain.MMApplicationTerminateClick *)
(*****************************************)

procedure TFormMain.MMApplicationTerminateClick(Sender: TObject);
begin
  if MessageDlg('Terminate is used to unconditionally cause the application' + #$D + #$A +
                'process to exit. Use it only in extreme circumstances. The' + #$D + #$A +
                'state of global data maintained by dynamic-link libraries' + #$D + #$A +
                'may be compromised if Terminate is used rather than closing' + #$D + #$A +
                'the application.' + #$D + #$A +
                'Do you want to terminate the application ?'
                , mtWarning, [mbYes, mbNo], 0) = mrYes then
    Process.Reset;
end {TFormMain.MMApplicationTerminateClick};


(*******************************)
(* TFormMain.MMExportDataClick *)
(*******************************)

procedure TFormMain.MMExportDataClick(Sender: TObject);
begin
  if FormExport.ShowModal = mrOk then begin
    Screen.Cursor := crHourGlass;
    try
      MakeReport;
    finally
      Screen.Cursor := crDefault;
    end {try};
  end {if};
end {TFormMain.MMExportDataClick};


(******************************)
(* TFormMain.MMHelpAboutClick *)
(******************************)

procedure TFormMain.MMHelpAboutClick(Sender: TObject);
begin
  FillSummary;
  FormAbout_ := TFormAbout.Create(Self);
  FormAbout_.ShowModal;
  FormAbout_.Free;
end {TFormMain.MMHelpAboutClick};


(*************************)
(* TFormMain.MMHelpClick *)
(*************************)

procedure TFormMain.MMHelpClick(Sender: TObject);
begin
end {TFormMain.MMHelpClick};


(*****************************)
(* TFormMain.MMHelpHelpClick *)
(*****************************)

procedure TFormMain.MMHelpHelpClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open',
    PChar(ChangeFileExt(Application.ExeName, '.rtf')), nil, nil, SW_SHOWNORMAL) ;
end {TFormMain.MMHelpHelpClick};


(***********************************)
(* TFormMain.MMMacroRecordNewClick *)
(***********************************)

procedure TFormMain.MMMacroRecordNewClick(Sender: TObject);
begin
  HandleJournalEvent(jeRecord);
end {TFormMain.MMMacroRecordNewClick};


(****************************)
(* TFormMain.MMOptionsClick *)
(****************************)

procedure TFormMain.MMOptionsClick(Sender: TObject);
begin
  if FormOptions.ShowModal = mrOk then begin
    AdjustStayOnTop;
    FillSortedUnitList;
    FillLBUnits;
  end {if};
end {TFormMain.MMOptionsClick};


(**************************************)
(* TFormMain.MMProjectClearStateClick *)
(**************************************)

procedure TFormMain.MMProjectClearStateClick(Sender: TObject);
begin
  if MessageDlg('Do you want to clear the current state?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    Screen.Cursor := crHourGlass;
    try
      ProjectDataBase_.Clear;
      InitAfterLoadingDatabase;
    finally
      Screen.Cursor := crDefault;
    end {try};
  end {if};
end {TFormMain.MMProjectClearStateClick};


(****************************)
(* TFormMain.MMProjectClick *)
(****************************)

procedure TFormMain.MMProjectClick(Sender: TObject);
begin
  MMProjectNew.Enabled := not Process.Running;
  MMProjectReload.Enabled := not Process.Running;
  MMProjectSave.Enabled := ProjectDatabase_ <> nil;
  MMProjectMerge.Enabled := (ProjectDatabase_ <> nil) and not Process.Running;
  MMProjectClearState.Enabled := (ProjectDatabase_ <> nil) and not Process.Running;
end {TFormMain.MMProjectClick};


(********************************)
(* TFormMain.MMProjectExitClick *)
(********************************)

procedure TFormMain.MMProjectExitClick(Sender: TObject);
begin
  Close;
end {TFormMain.MMProjectExitClick};


(*********************************)
(* TFormMain.MMProjectMergeClick *)
(*********************************)

procedure TFormMain.MMProjectMergeClick(Sender: TObject);
  var
    OldDataBase : TProjectDataBase;
    S : TFileStream;
begin
  if Config.ExecDialog(OpenStateDialog,State_Key) then begin
    Refresh;
    Screen.Cursor := crHourGlass;
    S := TFileStream.Create(OpenStateDialog.FileName, fmOpenRead);
    try
      StatusBar.Panels[pFilePos].Text := 'Merging state';
      OldDataBase := TProjectDataBase.Load(S);
      try
        if LogFileEnabled_ then
          WriteLn(LogFile_, 'Merging with state: '+OpenStateDialog.FileName);
        ProjectDataBase_.MergeCoverage(OldDataBase);
        if LoadedStatesStr <> '' then
          LoadedStatesStr := LoadedStatesStr + ' ';
        LoadedStatesStr := LoadedStatesStr + '+' + ExtractFileName(OpenStateDialog.FileName);
        InitAfterLoadingDataBase;
        inc(ProjectDataBase_.ChangedCount); // force dirty
      finally
        OldDataBase.Free;
      end {try};
    finally
      S.Free;
      Screen.Cursor := crDefault;
    end {Try};
  end {if};
end {TFormMain.MMProjectMergeClick};


(*******************************)
(* TFormMain.MMProjectNewClick *)
(*******************************)

procedure TFormMain.MMProjectNewClick(Sender: TObject);
begin
  if AllowsNewState and Config.ExecDialog(OpenDelphiProjectDialog, DelphiProject_Key) then begin
    Refresh;
    Screen.Cursor := crHourGlass;
    try
      LoadedStatesStr := '';
      LoadProject(OpenDelphiProjectDialog.FileName);

      ExeFileName := ProjectDataBase_.ExecutableFileName;
      StateFileName := ChangeFileExt(ProjectDataBase_.ExecutableFileName,ProjectStateExtension);
      InfoFileName := ChangeFileExt(StateFileName, ProjectInformationExtension);

      if FileExists(StateFileName) then begin
        if MessageDlg('Do you want to include (merge) the existing saved state?',
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
          Refresh;
          Screen.Cursor := crHourGlass;
          try
            MergeWithStateFile(StateFileName);
          finally
            Screen.Cursor := crDefault;
          end {try};
        end {if};
      end {if};
      LoadInformationFile;
    finally
      Screen.Cursor := crDefault;
      HandleProgress(-1, '');
    end {try};
  end {if};
end {TFormMain.MMProjectNewClick};


(**********************************)
(* TFormMain.MMProjectReloadClick *)
(**********************************)

procedure TFormMain.MMProjectReloadClick(Sender: TObject);
begin
  if AllowsNewState then begin
    if Config.ExecDialog(OpenStateDialog, State_Key) then begin
      Refresh;
      LoadState(OpenStateDialog.FileName);

      StateFileName := OpenStateDialog.FileName;
      InfoFileName := ChangeFileExt(StateFileName, ProjectInformationExtension);;
      ExeFileName := ProjectDataBase_.ExecutableFileName;
      LoadInformationFile;
    end {if};
  end {if};
end {TFormMain.MMProjectReloadClick};


(********************************)
(* TFormMain.MMProjectSaveClick *)
(********************************)

procedure TFormMain.MMProjectSaveClick(Sender: TObject);
begin
  if (ProjectDataBase_ <> nil) and
    Config.ExecDialog(SaveStateDialog, State_Key) then begin
    SaveStateFile(ChangeFileExt(SaveStateDialog.FileName,ProjectStateExtension));
  end {if};
end {TFormMain.MMProjectSaveClick};


(************************************)
(* TFormMain.MMProjectSettingsClick *)
(************************************)

procedure TFormMain.MMProjectSettingsClick(Sender: TObject);
begin
  if FormProjectInfo.ShowModal = mrOk then begin
    ProjectInfo.RunParameters := FormProjectInfo.EDITRunParameters.Text;
    ProjectInfo.StartupDirectory := FormProjectInfo.EDITStartupDirectory.Text;
    ProjectInfo.RunMaximized := FormProjectInfo.CHKRunMaximized.Checked;
  end {if};
end {TFormMain.MMProjectSettingsClick};


(***************************)
(* TFormMain.PBLegendPaint *)
(***************************)

procedure TFormMain.PBLegendPaint(Sender: TObject);
  const
    Offset = 5;
    MaxColors = 7;
    Colors : array [1..MaxColors] of TColor =
      (clBlack, clMaroon, clBlue, clAqua, clLime, clYellow, clWhite);
    Strings : array [1..MaxColors] of string =
      ('0%', '<=20%', '<=40%', '<=60%', '<=80%', '<100%', '100%');
  var
    w, h, i, x, dx, y, dy : integer;
    P : TPaintBox;
    R : TRect;
    t : string;
begin
  P := Sender as TPaintBox;
  w := P.Width;
  h := P.Height;
  dx := (w - 2*Offset) div MaxColors;
  x := Offset;
  dy := 24;
  y := (h - dy) div 2;
  for i := 1 to MaxColors do begin
    y := Offset;
    P.Canvas.Brush.Color := Colors[i];
    R := Rect(x, y, x+dx, y+dy);
    P.Canvas.FillRect(R);
    P.Canvas.Font.Color := (not Colors[i]) and $FFFFFF;
    t := Strings[i];
    w := P.Canvas.TextWidth(t);
    h := P.Canvas.TextHeight(t);
    P.Canvas.TextOut(x + ((dx-w) div 2), y + ((dy-h) div 2), t);
    inc(x, dx);
  end {for};
end {TFormMain.PBLegendPaint};


(*******************************)
(* TFormMain.PBOverViewMouseUp *)
(*******************************)

procedure TFormMain.PBOverViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    LineNr, ColumnNr, RoutineIndex : integer;
begin
  // Display the source of the selected routine
  LineNr := Y div OverviewPointSquareSide;
  ColumnNr := X div OverviewPointSquareSide;
  RoutineIndex := LineNr * SquaresPerLine + ColumnNr;
  if RoutineIndex < LBRoutines.Items.Count then begin
    LBRoutines.ItemIndex := RoutineIndex;
    LBRoutinesClick(LBRoutines);
  end {if};
end {TFormMain.PBOverViewMouseUp};


(*****************************)
(* TFormMain.PBOverViewPaint *)
(*****************************)

procedure TFormMain.PBOverViewPaint(Sender: TObject);
  var
    i : integer;
begin
  with Sender as TPaintBox do begin
    SquaresPerLine := PBOverView.Width div OverviewPointSquareSide;
    if not Resizing then begin
      if ProjectDataBase_ <> nil then begin
        with SortedRoutines do
          for i := 0 to pred(Count) do
            UpdateOverView(At(i))
      end else begin
        Canvas.FillRect(ClientRect);
      end {if};
    end {if};
  end {with};
end {TFormMain.PBOverViewPaint};


(**************************)
(* TFormMain.PCLeftChange *)
(**************************)

procedure TFormMain.PCLeftChange(Sender: TObject);
begin
  if PCLeft.ActivePage = TSSummary then
    FillSummary;
end {TFormMain.PCLeftChange};


(*****************************)
(* TFormMain.PCRightChanging *)
(*****************************)

procedure TFormMain.PCRightChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
{$IfDef NOMacros}
  AllowChange := false;
{$EndIf}
end {TFormMain.PCRightChanging};


(****************************************)
(* TFormMain.PositionRoutineViewToPoint *)
(****************************************)

procedure TFormMain.PositionRoutineViewToPoint(C: TCoveragePoint);
  var
    Routine : TRoutine;
    RoutineIndex : integer;
begin
  if C <> nil then begin
    // In which routine is this point?
    Routine := ProjectDataBase_.Routines.At(C.RoutineIndex);
    // Which is the index of this routine?
    RoutineIndex := SortedRoutines.IndexOf(Routine);
    // Is it another routine
    if (RoutineIndex <> LBRoutines.ItemIndex) and (RoutineIndex >= 0) and
     (RoutineIndex < LBRoutines.Items.Count) then begin
      // Is this item displayed?
      LBRoutines.ItemIndex := RoutineIndex;
      LBRoutines.Refresh;
      LBRoutinesDrawItem(LBRoutines, RoutineIndex, LBRoutines.ItemRect(RoutineIndex),
        [odSelected]);
    end {if};
  end {if};
end {TFormMain.PositionRoutineViewToPoint};


(******************************************)
(* TFormMain.PURoutinesEnableDisableClick *)
(******************************************)

procedure TFormMain.PURoutinesEnableDisableClick(Sender: TObject);
  var
    Routine : TRoutine;
    i, j : integer;
begin
  if LBRoutines.ItemIndex >=0 then begin

    j := LBRoutines.ItemIndex;
    LBRoutines.Items.BeginUpdate;
    try
      for i := 0 to pred(LBRoutines.Items.Count) do
        if LBRoutines.Selected[i] then
          with SortedRoutines do begin
            Routine := SortedRoutines.At(i);
            // Enable only items that have a source file
            if Routine.FileIndex >= 0 then
              ProjectDataBase_.EnableDisableRoutine(Routine,
                Sender = PURoutinesEnable);
          end {with};
    finally
      LBRoutines.Items.EndUpdate;
    end {try};

    Screen.Cursor := crHourGlass;
    try
      FillSortedRoutineList;
      FillLBRoutines;
    finally
      Screen.Cursor := crDefault;
    end {try};
    
    LBRoutines.ItemIndex := j;
    LBRoutinesClick(LBRoutines);
    UpdateCoverageOnStatusBar;
  end {if};
end {TFormMain.PURoutinesEnableDisableClick};


(*****************************)
(* TFormMain.PURoutinesPopup *)
(*****************************)

procedure TFormMain.PURoutinesPopup(Sender: TObject);
begin
  with LBRoutines do begin
    PURoutinesEnable.Enabled := (ItemIndex >= 0) and
      (TRoutine(SortedRoutines.At(ItemIndex)).Disabled);
    PURoutinesDisable.Enabled := (ItemIndex >= 0) and
      not (TRoutine(SortedRoutines.At(ItemIndex)).Disabled);
  end {with};
end {TFormMain.PURoutinesPopup};


(************************************)
(* TFormMain.PUSourceNextGreenClick *)
(************************************)

procedure TFormMain.PUSourceNextGreenClick(Sender: TObject);
begin
  GotoNextCoveragePoint(false);
end {TFormMain.PUSourceNextGreenClick};


(**********************************)
(* TFormMain.PUSourceNextRedClick *)
(**********************************)

procedure TFormMain.PUSourceNextRedClick(Sender: TObject);
begin
  GotoNextCoveragePoint(true)
end {TFormMain.PUSourceNextRedClick};


(***************************************)
(* TFormMain.PUUnitsEnableDisableClick *)
(***************************************)

procedure TFormMain.PUUnitsEnableDisableClick(Sender: TObject);
  var
    U : TUnit;
    R : TRect;
    i : integer;
begin
  with LBUnits do
    if SelCount > 0 then begin
      Screen.Cursor := crHourGlass;
      try
        for i := 0 to pred(Items.Count) do
          if Selected[i] then begin
            U := SortedUnits.At(i);
            if U.IsSourceAvailable then begin
              ProjectDataBase_.EnableDisableUnit(U, Sender = PUUnitsEnable);
              R := ItemRect(i);
              InvalidateRect(Handle, @R, true);
            end {if};
          end {if};
        FillSortedRoutineList;
        FillLBRoutines;
        FillSummary;
        PBOverviewPaint(PBOverView);
      finally
        Screen.Cursor := crDefault;
      end {try};
    end {if};
    UpdateCoverageOnStatusBar;
end {TFormMain.PUUnitsEnableDisableClick};


(**************************)
(* TFormMain.PUUnitsPopup *)
(**************************)

procedure TFormMain.PUUnitsPopup(Sender: TObject);
begin
  PUUnitsSelectAll.Enabled := (ProjectDatabase_ <> nil) and
    (Projectdatabase_.Units.Count > 0);
  PUUnitsEnable.Enabled := LBUnits.SelCount > 0;
  PUUnitsDisable.Enabled := LBUnits.SelCount > 0;
  PUUnitsSelectGroup.Enabled := LBUnits.SelCount = 1;
end {TFormMain.PUUnitsPopup};


(***********************************)
(* TFormMain.PUUnitsSelectAllClick *)
(***********************************)

procedure TFormMain.PUUnitsSelectAllClick(Sender: TObject);
  var
    i, n : integer;
begin
  with LBUnits do begin
    if Items.Count > 0 then begin
      n := TopIndex;
      Items.BeginUpdate;
      for i := 0 to Pred(Items.Count) do
        Selected[i] := true;
      TopIndex := n;
      Items.EndUpdate;
    end {if};
  end {with};
end {TFormMain.PUUnitsSelectAllClick};


(*************************************)
(* TFormMain.PUUnitsSelectGroupClick *)
(*************************************)

procedure TFormMain.PUUnitsSelectGroupClick(Sender: TObject);
  var
    RefU, U : TUnit;
    i,n : integer;
begin
  with LBUnits do
    if SelCount = 1 then begin
      Screen.Cursor := crHourGlass;
      n := TopIndex;
      Items.Beginupdate;
      try
        RefU := nil;
        for i := 0 to pred(Items.Count) do
          if Selected[i] then begin
            RefU := SortedUnits.At(i);
            break;
          end {if};
        if (RefU <> nil) and (RefU.FileNames.Count > 0) then begin
          with SortedUnits do
            for i := 0 to pred(Count) do begin
              U := At(i);
              if (U.FileNames.Count >0) and
                (ExtractFilePath(UpperCase(RefU.FileNames[0])) =
                 ExtractFilePath(UpperCase(U.FileNames[0]))) then
                Selected[i] := true;
           end {for};
        end {if};
      finally
        Screen.Cursor := crDefault;
        TopIndex := n;
        Items.EndUpdate;
      end {try};
    end {if};
    UpdateCoverageOnStatusBar;
end {TFormMain.PUUnitsSelectGroupClick};


(****************************)
(* TFormMain.RunApplication *)
(****************************)

procedure TFormMain.RunApplication;
begin
  // Check that the loaded state and exe are uptodate
  DeltaCovered := 0;
  UpdateCoverageOnStatusBar;
  if not FileExists(ExeFileName) then
    raise Exception.Create(Format('File "%s" does not exist.', [ExeFileName]));

  if CRC32.FileCRC32(ExeFileName) <> ProjectDatabase_.ExecutableFileCRC then
    raise Exception.Create('The loaded state doesn''t match with the actual map and exe files. '+
      'The application has probably been recompiled. '+
      'You must load your project again.');

  Process.RunParameters := ProjectInfo.RunParameters;
  Process.StartupDirectory := ProjectInfo.StartupDirectory;
  Process.RunMaximized := ProjectInfo.RunMaximized;
  Process.ExeName := ExeFileName;

  Process.Run;
end {TFormMain.RunApplication};


(**************************)
(* TFormMain.SaveDataBase *)
(**************************)

procedure TFormMain.SaveDataBase(const FileName : string);
  var
    M : TFileStream;
begin
  M := TFileStream.Create(FileName, fmCreate);
  Screen.Cursor := crHourGlass;
  StatusBar.Panels[pFilePos].Text := 'Saving state';
  StatusBar.Refresh;
  try
    if LogFileEnabled_ then
      WriteLn(LogFile_, 'Saving state: '+FileName);
    ProjectDataBase_.Save(M);
  finally
    M.Free;
    Screen.Cursor := crDefault;
    StatusBar.Panels[pFilePos].Text := '';
  end {try};
end {TFormMain.SaveDataBase};


(*********************************)
(* TFormMain.SaveInformationFile *)
(*********************************)

procedure TFormMain.SaveInformationFile;
  var
    U : TUnit;
    R : TRoutine;
    i : integer;
begin
  ProjectInfo.BackGndUnits.Clear;
  for i := 0 to pred(ProjectDataBase_.Units.Count) do begin
    U := ProjectDataBase_.Units.At(i);
    if (U.IsSourceAvailable) and U.Disabled then
      ProjectInfo.BackGndUnits.Add(U.Name);
  end {for};
  ProjectInfo.BackGndRoutines.Clear;
  for i := 0 to pred(ProjectDataBase_.Routines.Count) do begin
    R := ProjectDataBase_.Routines.At(i);
    U := ProjectDataBase_.Units.At(R.UnitIndex);
    if (U.IsSourceAvailable) and (not U.Disabled) and R.Disabled then
      ProjectInfo.BackGndRoutines.Add(R.Name);
  end {for};
  if InfoFileName <> '' then
    ProjectInfo.SaveToFile(InfoFileName);
end {TFormMain.SaveInformationFile};


(***************************)
(* TFormMain.SaveStateFile *)
(***************************)

procedure TFormMain.SaveStateFile(const FileName : string);
begin
  SaveDataBase(FileName);
end {TFormMain.SaveStateFile};


(**********************************)
(* TFormMain.SetStateMachineState *)
(**********************************)

procedure TFormMain.SetStateMachineState;
begin
  FStateMachineState := aState;
  if LogFileEnabled_ then
    Writeln(LogFile_, Format('%d %s',[GetTickCount,
    StateMachineStrings[StateMachineState]]));
end {TFormMain.SetStateMachineState};


(*******************)
(* TFormMain.Sleep *)
(*******************)

procedure TFormMain.Sleep(Delay: integer);
begin
  Windows.Sleep(Delay);
end {TFormMain.Sleep};


(*****************************)
(* TFormMain.StatusBarResize *)
(*****************************)

procedure TFormMain.StatusBarResize(Sender: TObject);
begin
  AdjustStatusbar;
  DisplayStatusFilename;
end {TFormMain.StatusBarResize};


(*************************)
(* TFormMain.Timer1Timer *)
(*************************)

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  // Come here when the application is terminated
  Timer1.Enabled := false;
  UpdateOnActivate;
  if PCLeft.ActivePage = TSOverView then
    PBOverViewPaint(PBOverView);
  if Globals.CommandLineParams_.Action <> caNoAction then
    DoCommandLineFinalActions;
end {TFormMain.Timer1Timer};


(******************************)
(* TFormMain.TIMERResizeTimer *)
(******************************)

procedure TFormMain.TIMERResizeTimer(Sender: TObject);
begin
  if not Resizing then begin
    TIMERResize.Enabled := false;
    PBOverViewPaint(PBOverView);
  end else
    Resizing := false;
end {TFormMain.TIMERResizeTimer};


(************************)
(* TFormMain.TimerTimer *)
(************************)

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  HandleJournalEvent(jeTimerElapsed);
end {TFormMain.TimerTimer};


(***************************************)
(* TFormMain.UpdateCoverageOnStatusBar *)
(***************************************)

procedure TFormMain.UpdateCoverageOnStatusBar;
begin
  if ProjectDataBase_ <> nil then begin
    with ProjectDataBase_.CoveragePoints do
      StatusBar.Panels[pValidEnabledCoveredPoints].Text :=
        IntToStr(ValidEnabledCoveredPointsQty);
    StatusBar.Panels[pDeltaCovered].Text := '+'+IntToStr(DeltaCovered);
  end {if};
end {TFormMain.UpdateCoverageOnStatusBar};


(****************************)
(* TFormMain.UpdateFileLine *)
(****************************)

procedure TFormMain.UpdateFileLine(Point : TCoveragePoint);
  var
    U : TUnit;
    Routine : TRoutine;
    TopLineNr, BottomLineNr : integer;
    Index : integer;
    R : TRect;
begin
  Routine := ProjectDataBase_.Routines.At(Point.RoutineIndex);
  U := ProjectDataBase_.Units.At(Routine.UnitIndex);
  if (Routine.FileIndex >= 0) and
    (CompareText(CurrentSourceFileName, U.FileNames[Routine.FileIndex]) = 0) then begin
    // Is the line corresponding to the coverage point displayed?
    TopLineNr := succ(SendMessage(LBFile.Handle, LB_GETTOPINDEX, 0, 0));
    BottomLineNr := (LBFile.Height div LBFile.ItemHeight) + TopLineNr;
    if (Point.LineNumber >= TopLineNr) and
      (Point.LineNumber <= BottomLineNr) then begin
      Index := pred(Point.LineNumber);
      SendMessage(LBFile.Handle, LB_GETITEMRECT, Index, integer(@R));
      InvalidateRect(LBFile.handle, @R, true);
    end {if};
  end {if};
end {TFormMain.UpdateFileLine};


(******************************)
(* TFormMain.UpdateOnActivate *)
(******************************)

procedure TFormMain.UpdateOnActivate;
begin
  if ProjectDataBase_ <> nil then begin
    Screen.Cursor := crHourGlass;
    try
      UpdateCoverageOnStatusBar;
      FillSummary;
      FillSortedRoutineList;
      FillLBRoutines;
      FillSortedUnitList;
      FillLBUnits;
    finally
      Screen.Cursor := crDefault;
    end {try};
  end {if};
end {TFormMain.UpdateOnActivate};


(****************************)
(* TFormMain.UpdateOverView *)
(****************************)

procedure TFormMain.UpdateOverView(Routine : TRoutine);
  var
    RoutineIndex, LineNr, RowNr, x : integer;
    R : TRect;
begin
  RoutineIndex := SortedRoutines.IndexOf(Routine);
  if RoutineIndex >= 0 then begin
    LineNr := RoutineIndex div SquaresPerLine;
    RowNr := RoutineIndex mod SquaresPerLine;
    with R do begin
      Left := RowNr*OverviewPointSquareSide;
      Top := LineNr*OverviewPointSquareSide;
      Bottom := Top + OverviewPointSquareSide;
      Right := Left + OverviewPointSquareSide;
    end {with};
    if not Routine.Disabled then begin
      if Routine.ValidPointsQty = 0 then
        x := 0
      else
        x := (100*Routine.CoveredPointsQty) div Routine.ValidPointsQty;
      if x = 100 then
        PBOverView.Canvas.Brush.Color := clWhite
      else if x = 0 then
        PBOverView.Canvas.Brush.Color := clBlack
      else if x > 80 then
        PBOverView.Canvas.Brush.Color := clYellow
      else if x > 60 then
        PBOverView.Canvas.Brush.Color := clLime
      else if x > 40 then
        PBOverView.Canvas.Brush.Color := clAqua
      else if x > 20 then
        PBOverView.Canvas.Brush.Color := clBlue
      else
        PBOverView.Canvas.Brush.Color := clMaroon;
    end else
      PBOverView.Canvas.Brush.Color := clGray;

    PBOverView.Canvas.Pen.Color := clGray;
    with R do
      PBOverView.Canvas.Rectangle(pred(R.Left), pred(R.Top), R.Right, R.Bottom);
  end {if};
end {TFormMain.UpdateOverView};


{~b}

end.

