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
unit CodeParser;

interface
  uses
    Classes, ObjectPascalTokenizer, SysUtils, DataBase, Objects, Globals;
type
  TUnitUsedNotification = procedure (const UnitName, UnitFileName : string) of object;
  TLineNumbersToPoints = class(TSortedCollection)
    function Compare(Key1, Key2 : pointer) : integer; override;
    function KeyOf(Item : pointer) : pointer; override;
  end;
  TCodeParser = class(TObject)
  private
    T : TObjectPascalTokenizer;
    CurrentUnit : TUnit;
    CurrentRoutine : TRoutine;
    CurrentRoutineIndex : integer;
    CurrentRoutineLineNumbersToPoints : TLineNumbersToPoints;
    procedure Expect(aToken : TToken);
    function ExpectAndGet(aToken : TToken) : string;
    function ExpectOneOf(aTokenSet : TTokenSet) : TToken;
    procedure SkipUntil(aTokenSet : TTokenSet);
    function OnIfDirective(Directive : TDirective; const Param : string) : boolean;
    procedure OnDefUndefDirective(Directive : TDirective; const Param : string);
    procedure OnIncludeDirective (const Param : string; var FileName : string);
    procedure CompoundStatement(NoBegin : boolean; var LineNumber : integer);
    procedure Statement(var LineNumber : integer);
    procedure IfStatement;
    procedure CaseStatement;
    procedure RepeatStatement(var LineNumber : integer);
    procedure WhileStatement;
    procedure ForStatement;
    procedure WithStatement(var LineNumber : integer);
    procedure TryStatement;
    procedure StatementList(var LineNumber : integer);
    procedure ValidateCoveragePointAt(LineNumber : integer);
  public
    OnUnit : TUnitUsedNotification;
    OnProgressEvent : TProgressEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Parse;
  end;

implementation
  uses
    Exceptions, Util;
{~t}
(*****************************)
(* TCodeParser.CaseStatement *)
(*****************************)

procedure TCodeParser.CaseStatement;
  var
    Done : boolean;
    LineNumber : integer;

  procedure OneCase;
  begin
    SkipUntil([toColon, toEndOfBuffer]);
    Expect(toColon);
    Statement(LineNumber);
    if LineNumber > 0 then
      ValidateCoveragePointAt(LineNumber);
  end {OneCase};

begin
  Expect(toCase);
  SkipUntil([toOf, toEndOfBuffer]);
  Expect(toOf);
  Done := false;
  repeat
    OneCase;
    if T.Token  = toSemicolon then begin
      T.Next;
      Done := (T.Token = toEnd) or (T.Token = toElse);
    end else
      Done := true
  until Done;
  if T.Token = toElse then begin
    T.Next;
    StatementList(LineNumber);
    if LineNumber > 0 then
      ValidateCoveragePointAt(LineNumber);
  end {if};
  if T.Token = toSemiColon then
    T.Next;
  Expect(toEnd);
end {TCodeParser.CaseStatement};


(*********************************)
(* TCodeParser.CompoundStatement *)
(*********************************)

procedure TCodeParser.CompoundStatement(NoBegin : boolean; var LineNumber : integer);
  var
    Done, First : boolean;
    Dmy : integer;
begin
  if not NoBegin then
    Expect(toBegin);
  Done := false;
  First := true;
  repeat
    if First then
      Statement(LineNumber)
    else
      Statement(Dmy);
    First := false;
    if T.Token = toSemiColon then
      T.Next
    else
      Done := true;
  until Done;
  Expect(toEnd);
end {TCodeParser.CompoundStatement};


(**********************)
(* TCodeParser.Create *)
(**********************)

constructor TCodeParser.Create;
begin
  inherited Create;
  T := TObjectPascalTokenizer.Create;
  T.OnIfDirective := OnIfDirective;
  T.OnDefUndefDirective := OnDefUndefDirective;
  T.OnIncludeDirective := OnIncludeDirective;
  CurrentRoutineLineNumbersToPoints := TLineNumbersToPoints.Create;
end {TCodeParser.Create};


(***********************)
(* TCodeParser.Destroy *)
(***********************)

destructor TCodeParser.Destroy;
begin
  T.Free;
  CurrentRoutineLineNumbersToPoints.DeleteAll;
  CurrentRoutineLineNumbersToPoints.Free;
  inherited Destroy;
end {TCodeParser.Destroy};


(**********************)
(* TCodeParser.Expect *)
(**********************)

procedure TCodeParser.Expect(aToken : TToken);
begin
  if T.Token <> aToken then
    raise EParserExpect.Create([aToken], T.FileName, T.LineNumber, T.OffsetInLine)
  else
    T.Next;
end {TCodeParser.Expect};


(****************************)
(* TCodeParser.ExpectAndGet *)
(****************************)

function TCodeParser.ExpectAndGet(aToken : TToken) : string;
begin
  if T.Token <> aToken then
    raise EParserExpect.Create([aToken], T.FileName, T.LineNumber, T.OffsetInLine)
  else begin
    Result := T.GetIdentifier;
    T.Next;
  end {if};
end {TCodeParser.ExpectAndGet};


(***************************)
(* TCodeParser.ExpectOneOf *)
(***************************)

function TCodeParser.ExpectOneOf(aTokenSet : TTokenSet) : TToken;
begin
  if not (T.Token in aTokenSet) then
    raise EParserExpect.Create(aTokenSet, T.FileName, T.LineNumber, T.OffsetInLine)
  else
    T.Next;
end {TCodeParser.ExpectOneOf};


(****************************)
(* TCodeParser.ForStatement *)
(****************************)

procedure TCodeParser.ForStatement;
  var
    LineNumber : integer;
begin
  Expect(toFor);
  SkipUntil([toDo, toEndOfBuffer]);
  Expect(toDo);
  Statement(LineNumber);
  if LineNumber > 0 then
    ValidateCoveragePointAt(LineNumber);
end {TCodeParser.ForStatement};


(***************************)
(* TCodeParser.IfStatement *)
(***************************)

procedure TCodeParser.IfStatement;
  var
    LineNumber : integer;
begin
  Expect(toIf);
  SkipUntil([toThen, toEndOfBuffer]);
  Expect(toThen);
  Statement(LineNumber);
  if LineNumber > 0 then
    ValidateCoveragePointAt(LineNumber);
  if T.Token = toElse then begin
    T.Next;
    Statement(LineNumber);
    if LineNumber > 0 then
      ValidateCoveragePointAt(LineNumber);
  end {if};
end {TCodeParser.IfStatement};


(***********************************)
(* TCodeParser.OnDefUndefDirective *)
(***********************************)

procedure TCodeParser.OnDefUndefDirective(Directive : TDirective; const Param : string);
  var
    s :string;
    i : integer;
begin
  s := Uppercase(Param);
  i := CurrentUnit.DefinedConditionnals.IndexOf(s);
  case Directive of
    dirDefine:
      if i < 0 then
        CurrentUnit.DefinedConditionnals.Add(s);
    dirUndef:
      if i >= 0 then
        CurrentUnit.DefinedConditionnals.Delete(i);
  else
    Assert(false);
  end {case};
end {TCodeParser.OnDefUndefDirective};


(*****************************)
(* TCodeParser.OnIfDirective *)
(*****************************)

function TCodeParser.OnIfDirective(Directive : TDirective; const Param : string) : boolean;
  var
    s :string;
begin
  s := Uppercase(Param);
  case Directive of
    dirIFDef:
      Result := (CurrentUnit.DefinedConditionnals.IndexOf(s) >= 0) or
        (GlobalDefinedConditionnals_.IndexOf(s) >= 0);
    dirIFNdef:
      Result := (CurrentUnit.DefinedConditionnals.IndexOf(s) < 0) and
        (GlobalDefinedConditionnals_.IndexOf(s) < 0);
  else
    Assert(false);
  end {case};
end {TCodeParser.OnIfDirective};


(**********************************)
(* TCodeParser.OnIncludeDirective *)
(**********************************)

procedure TCodeParser.OnIncludeDirective (const Param : string; var FileName : string);
  var
    s : string;
    i : integer;
begin
  if ExtractFileExt(Param) = '' then
    s := ChangeFileExt(Param, '.pas')
  else
    s := Param;
  with SearchPath_ do
    for i := 0 to pred(Count) do begin
      FileName := SearchPath_[i] + '\' + s;
      if FileExists(FileName) then
        break;
    end {for};
end {TCodeParser.OnIncludeDirective};


(*********************)
(* TCodeParser.Parse *)
(*********************)

procedure TCodeParser.Parse;
  var
    Done : boolean;

  function FindNextRoutine : boolean;
    var
      Done, SyncLost : boolean;
      RoutineFileName : string;
      C : TCoveragePoint;
  begin
    Result := false;
    Done := false;
    SyncLost := false;
    repeat
      inc(CurrentRoutineIndex);
      if CurrentRoutineIndex < ProjectDataBase_.Routines.Count then begin
        CurrentRoutine := ProjectDataBase_.Routines.At(CurrentRoutineIndex);
        if CurrentRoutine.FileIndex >= 0 then begin
          CurrentUnit := ProjectDataBase_.Units.At(CurrentRoutine.UnitIndex);
          RoutineFileName := CurrentUnit.FileNames[CurrentRoutine.FileIndex];
          if RoutineFileName <> T.FileName then begin
            T.FileName := RoutineFileName;
            SyncLost := false;
            if LogFileEnabled_ then
              Writeln(LogFile_, Format('Opening src-file: %s', [RoutineFileName]));
          end {if};
          // skip to the routine line
          C := ProjectDataBase_.CoveragePoints.At(CurrentRoutine.FirstPointIndex);
          // WARNING: In certain files (ex:windows.pas in Delphi 5) several line
          // are terminated by CR alone instead of CR/LF. In these cases
          // SkipToLine get lost and an returns false.
          if not SyncLost then begin
            if not T.SkipToLine(C.LineNumber) then begin
              // We just lose the sync
             if LogFileEnabled_ then
                Writeln(LogFile_, Format('*** ERROR *** Sync lost at line %d.',
                  [C.LineNumber]));
             SyncLost := true;
            end {if};
          end {if};
          if not SyncLost then begin
            Result := true;
            Done := true;
          end {if};
        end {if};
      end else
        Done := true;
    until Done;
  end {FindNextRoutine};

  procedure ParseRoutine;
    var
      Dmy,i : integer;
      C : TCoveragePoint;
      Context : TTokenizerContext;
      s : string;
  begin
    if LogFileEnabled_ then
      Writeln(LogFile_, Format('   %s (%d)', [CurrentRoutine.Name, CurrentRoutine.FirstPointIndex]));
    T.GetContext(Context);
    T.Next;
    if not (T.Token in [toEnd, toEndOfBuffer, toInitialization,
      toFinalization, toImplementation]) then begin
      try
        C := ProjectDataBase_.CoveragePoints.At(CurrentRoutine.FirstPointIndex);
        C.Valid := true;
        with Projectdatabase_.CoveragePoints do begin
          inc(ValidPointsQty); inc(ValidEnabledPointsQty);
        end {with};
        CurrentRoutine.ValidPointsQty := 1;
        CurrentRoutineLineNumbersToPoints.DeleteAll;
        i := CurrentRoutine.FirstPointIndex;
        while (C.RoutineIndex = CurrentRoutineIndex) and
          (i < ProjectDataBase_.CoveragePoints.Count) do begin
          CurrentRoutineLineNumbersToPoints.Insert(C);
          inc(i);
          if i < ProjectDataBase_.CoveragePoints.Count then
            C := ProjectDataBase_.CoveragePoints.At(i);
        end {while};
        case T.Token of
          toFunction, toProcedure:;
            // assume a one line routine
          toBegin:
            CompoundStatement(false,Dmy);
          toAsm:;
          //toEnd, toEndOfBuffer, toInitialization, toFinalization, toImplementation:

        else
          T.SetContext(Context);
          s := T.PreviousIdentifier;
          T.SetContext(Context);
          T.Next;
          if s = 'BEGIN' then begin
            CompoundStatement(true, Dmy)
          end else if s = 'ASM' then
            // We assume this is an assembler routine and we do nothing...
          else if (s = 'PROCEDURE') or (s = 'FUNCTION') then
            // One line routine, we do nothing
          else
            CompoundStatement(true, Dmy);
        end {case};
      except
        on E:EParserExpect do
          // Silence...
          if LogFileEnabled_ then
            Writeln(LogFile_, Format('"%s" in %s, at %d',[E.Message, E.FileName, E.LineNr]));
        else
          raise
      end {try};
    end else begin
      // Completely ignore init and exitcode
    end {if};
  end {ParseRoutine};

begin
  CurrentRoutineIndex := -1;
  Done := false;
  repeat
    if FindNextRoutine then
      ParseRoutine
    else
      Done := true;
    if Assigned(OnProgressEvent) then
      OnProgressEvent(100*CurrentRoutineIndex div
        ProjectDataBase_.Routines.Count,'Processing source code');
  until Done;
end {TCodeParser.Parse};


(*******************************)
(* TCodeParser.RepeatStatement *)
(*******************************)

procedure TCodeParser.RepeatStatement(var LineNumber : integer);
begin
  Expect(toRepeat);
  StatementList(LineNumber);
  Expect(toUntil);
  SkipUntil([toEnd, toElse, toSemiColon, toEndOfBuffer, toUntil, toFinally, toExcept]);
end {TCodeParser.RepeatStatement};


(*************************)
(* TCodeParser.SkipUntil *)
(*************************)

procedure TCodeParser.SkipUntil;
begin
  while not (T.Token in aTokenSet) do
    T.Next;
end {TCodeParser.SkipUntil};


(*************************)
(* TCodeParser.Statement *)
(*************************)

procedure TCodeParser.Statement(var LineNumber : integer);
  var
    C : TTokenizerContext;
begin
  T.GetContext(C);
  if T.Token = toIdentifier then begin
    T.Next;
    if T.Token = toColon then begin
      T.Next;
      // Label
    end else
      T.SetContext(C);
  end {if};
  LineNumber := T.LineNumber;
  case T.Token of
    toIdentifier, toRaise, toAt, toInherited, toParOpen:
      // assignement statement, procedure statement
      SkipUntil([toSemicolon, toEnd, toElse, toEndOfBuffer, toUntil, toFinally, toExcept]);
    toGoto: begin
      // Goto statement
      T.Next;
      Expect(toIdentifier);
    end;
    toBegin:
      // CompoundStatement
      CompoundStatement(false, LineNumber);
    toIf:
      IfStatement;
    toCase:
      CaseStatement;
    toRepeat:
      RepeatStatement(LineNumber);
    toWhile:
      WhileStatement;
    toFor:
      ForStatement;
    toWith:
      WithStatement(LineNumber);
    toTry:
      TryStatement;
    toAsm: begin
      T.EnableAsmIdentifiers := true;
      SkipUntil([toEnd, toEndOfBuffer]);
      T.EnableAsmIdentifiers := false;
      Expect(toEnd);
    end;
    else {case}
      LineNumber := -1;
  end {case};
end {TCodeParser.Statement};


(*****************************)
(* TCodeParser.StatementList *)
(*****************************)

procedure TCodeParser.StatementList(var LineNumber: integer);
  var
    Done, First : boolean;
    Dmy : integer;
begin
  Done := false;
  First := true;
  repeat
    if First then
      Statement(LineNumber)
    else
      Statement(Dmy);
    First := false;
    if T.Token <> toSemiColon then
      Done := true
    else
      T.Next;
  until Done;
end {TCodeParser.StatementList};


(****************************)
(* TCodeParser.TryStatement *)
(****************************)

procedure TCodeParser.TryStatement;
  var
    LineNumber : integer;
    IsFinally : boolean;
begin
  Expect(toTry);
  StatementList(LineNumber);
  IsFinally := T.Token = toFinally;
  ExpectOneOf([toExcept, toFinally]);
  repeat
    if T.Token = toOn then begin
      // Exception handler
      SkipUntil([toDo, toEndOfBuffer]);
      Expect(toDo);
      Statement(LineNumber);
      if LineNumber > 0 then
        ValidateCoveragePointAt(LineNumber);
    end else if T.Token = toElse then begin
      T.Next;
      StatementList(LineNumber);
      if LineNumber > 0 then
        ValidateCoveragePointAt(LineNumber);
    end else if T.Token = toExcept then begin
      // This case occurs if there are conditionals in the source that
      // generate a try statement with several except clauses, in such cases
      // we just skip the ''except'' tokens
      T.Next;
    end else begin
      StatementList(LineNumber);
      if (LineNumber > 0) and not IsFinally then
        ValidateCoveragePointAt(LineNumber);
    end {if};
  until T.Token = toEnd;
  Expect(toEnd);
end {TCodeParser.TryStatement};


(***************************************)
(* TCodeParser.ValidateCoveragePointAt *)
(***************************************)

procedure TCodeParser.ValidateCoveragePointAt(LineNumber : integer);
  var
    C : TCoveragePoint;
    i : integer;
begin
  if CurrentRoutineLineNumbersToPoints.Search(pointer(LineNumber), i) then begin
    C := CurrentRoutineLineNumbersToPoints.At(i);
    if C.LineNumber = LineNumber then
      if not C.Valid then begin
        inc(CurrentRoutine.ValidPointsQty);
        with Projectdatabase_.CoveragePoints do begin
          inc(ValidPointsQty); inc(ValidEnabledPointsQty);
        end {with};
        C.Valid := true;
      end {if};
  end {if};
end {TCodeParser.ValidateCoveragePointAt};


(******************************)
(* TCodeParser.WhileStatement *)
(******************************)

procedure TCodeParser.WhileStatement;
  var
    LineNumber : integer;
begin
  Expect(toWhile);
  SkipUntil([toDo, toEndOfBuffer]);
  Expect(toDo);
  Statement(LineNumber);
  if LineNumber > 0 then
    ValidateCoveragePointAt(LineNumber);
end {TCodeParser.WhileStatement};


(*****************************)
(* TCodeParser.WithStatement *)
(*****************************)

procedure TCodeParser.WithStatement(var LineNumber : integer);
begin
  Expect(toWith);
  SkipUntil([toDo, toEndOfBuffer]);
  Expect(toDo);
  Statement(LineNumber);
end {TCodeParser.WithStatement};


(********************************)
(* TLineNumbersToPoints.Compare *)
(********************************)

function TLineNumbersToPoints.Compare;
begin
  if integer(Key1) < integer(Key2) then
    Result := -1
  else if Key1 = Key2 then
    Result := 0
  else
    Result := 1;
end {TLineNumbersToPoints.Compare};


(******************************)
(* TLineNumbersToPoints.KeyOf *)
(******************************)

function TLineNumbersToPoints.KeyOf;
begin
  Result := pointer(TCoveragePoint(Item).LineNumber);
end {TLineNumbersToPoints.KeyOf};


{~b}
end.
