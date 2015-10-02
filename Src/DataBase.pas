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
unit DataBase;

interface
  uses
    Objects, Classes;

  type
    TAddress = integer;

    TAddressable = class(TSavableObject)
      Address : TAddress;
      procedure Save(aStream : TStream); override;
      constructor Load(aStream : TStream);
    end;

    TListOfAddressable = class(TSortedCollection)
    public
      function Compare(Key1, Key2 : pointer) : integer; override;
      function KeyOf(Item : pointer) : pointer; override;
      constructor Create;
      //procedure Print(aStream : TStream; Indent : integer); abstract;
    end;

    TCoveragePoint = class(TAddressable)
      RoutineIndex : integer;
      LineNumber : integer;
      Counter : integer;
      Disabled : boolean;
      IsBreakPointSet : boolean;
      OpCode : byte;
      Valid : boolean;
      procedure Print(aStream : TStream; Indent : integer);
      procedure Save(aStream : TStream); override;
      constructor Load(aStream : TStream);
    end;

    TNamedAddressable = class(TAddressable)
      Name : string;
    end;

    TRoutine = class(TNamedAddressable)
      UnitIndex : integer;
      FileIndex : integer;
      FirstPointIndex : integer;
      ValidPointsQty, CoveredPointsQty : integer;
      Disabled : boolean;
      procedure Print(var aFile : TextFile; Indent : integer);
      constructor Create;
      destructor Destroy; override;
      procedure Save(aStream : TStream); override;
      constructor Load(aStream : TStream);
    end;

    TUnit = class(TNamedAddressable)
      Size : integer;
      FileNames : TStringList;
      FirstRoutineIndex : integer;
      Disabled : boolean;
      RoutinesQty, R0Pc, R100Pc, ValidPointsQty, CoveredPointsQty : integer;
      DefinedConditionnals : TStringList;
      function IsSourceAvailable : boolean;
      constructor Create;
      destructor Destroy; override;
      procedure Print(var aFile : TextFile; Indent : integer);
      procedure Save(aStream : TStream); override;
      constructor Load(aStream : TStream);
    end;

    TUnits = class(TListOfAddressable) // of TUnit
    public
      procedure Print(var aFile : TextFile; Indent : integer);
      function AtName(const aName : string) : TUnit;
      function AtNameIndex(const aName : string) : integer;
      procedure Save(aStream : TStream); override;
      constructor Load(aStream : TStream);
    end;

    TRoutines = class(TListOfAddressable)
      function AtName(const aName : string) : TRoutine;
      function AtNameIndex(const aName : string) : integer;
      procedure Save(aStream : TStream); override;
      procedure Print(var aFile : TextFile; Indent : integer);
      constructor Load(aStream : TStream);
    end;

    TCoveragePoints = class(TListOfAddressable)
      ValidPointsQty, ValidCoveredPointsQty, ValidEnabledPointsQty, ValidEnabledCoveredPointsQty : integer;
      procedure Save(aStream : TStream); override;
      constructor Load(aStream : TStream);
    end;

    TProjectDataBase = class(TSavableObject)
      ExecutableFileName : string;
      ExecutableFileCRC : integer;
      ImageBase : integer;
      Units : TUnits;
      Routines : TRoutines;
      CoveragePoints : TCoveragePoints;
      ChangedCount : integer;
      Coverage : single;
      UnitsWithSource, RoutinesWithSource : integer;

      constructor Create;
      destructor Destroy; override;
      procedure Save(aStream : TStream); override;
      constructor Load(aStream : TStream);
      procedure InitStatistics;
      procedure UpdateStatistics;
      procedure EnableDisableUnit(U : TUnit; Enable : boolean);
      procedure EnableDisableRoutine(R : TRoutine; Enable : boolean);
      procedure EnablePoint(C : TCoveragePoint);
      procedure DisablePoint(C : TCoveragePoint);
      procedure SetCovered(C : TCoveragePoint);
      function TotalCoverage : single;
      function EnabledCoverage : single;
      function EnabledRoutinesQty : integer;
      function EnabledUnitsQty : integer;
      procedure Update(OldDataBase : TProjectDataBase);
      procedure MergeCoverage(OldDataBase : TProjectDataBase);
      procedure GetR100R0(var R100Pc, R0Pc : integer);
      procedure Clear;

    end;

    TSortedRoutineMode = (srByName, srByUnit, srByPoints, srByCoverage);
    TSortedRoutines = class(TSortedCollection)
      Mode : TSortedRoutineMode;
      Inverted : boolean;
      function Compare(Key1, Key2 : pointer) : integer; override;
    end;
    TSortedUnitMode = (suByName, suBySize, suByRQty, suByR0Pc, suByR100Pc, suByCoverage);
    TSortedUnits = class(TSortedCollection)
      Mode : TSortedUnitMode;
      Inverted : boolean;
      function Compare(Key1, Key2 : pointer) : integer; override;
    end;

    TNameIndex = class(TObject)
      Name : string;
      Index : integer;
    end;

    TSortedNameIndex = class(TSortedCollection)
      function Compare(Item1, Item2 : pointer) : integer; override;
      function KeyOf(Item : pointer) : pointer; override;
    end;

  var
    ProjectDataBase_ : TProjectDataBase;

implementation
  uses
    SysUtils;
  const
    DataBaseMagicNumber = 20090701;

  function ReadStringFromStream(aStream : TStream) : string; forward;
  procedure WriteStringToStream(aStream : TStream; const S : string); forward;

{~t}
(************)
(* BlankStr *)
(************)

function BlankStr(Size : integer) : string;
begin
  Result := '';
  while length(Result) < Size do
    Result := Result + ' ';
end {BlankStr};


(************)
(* DoIndent *)
(************)

procedure DoIndent(aStream : TStream; Qty : integer);
  var
    i : integer;
    c : char;
begin
  c := ' ';
  for i := 1 to Qty do
    aStream.Write(c, SizeOf(char));
end {DoIndent};


(*************)
(* DoNewLine *)
(*************)

procedure DoNewLine(aStream : TStream);
  var
    s : string;
begin
  s := #$D + #$A;
  aStream.Write(s[1], 2*SizeOf(char));
end {DoNewLine};


(************************)
(* ReadStringFromStream *)
(************************)

function ReadStringFromStream(aStream : TStream) : string;
  var
    n : integer;
begin
  aStream.Read(n, SizeOf(n));
  SetLength(Result, n);
  aStream.Read(Result[1], n*SizeOf(char));
end {ReadStringFromStream};


(***********************)
(* WriteStringToStream *)
(***********************)

procedure WriteStringToStream(aStream : TStream; const S : string);
  var
    n : integer;
begin
  n := Length(s);
  aStream.Write(n, SizeOf(n));
  aStream.Write(s[1], n*SizeOf(char));
end {WriteStringToStream};


(*********************)
(* TAddressable.Load *)
(*********************)

constructor TAddressable.Load(aStream : TStream);
begin
  aStream.Read(Address, SizeOf(Address));
end {TAddressable.Load};


(*********************)
(* TAddressable.Save *)
(*********************)

procedure TAddressable.Save(aStream : TStream);
begin
  aStream.Write(Address, SizeOf(Address));
end {TAddressable.Save};


(***********************)
(* TCoveragePoint.Load *)
(***********************)

constructor TCoveragePoint.Load(aStream : TStream);
begin
  inherited Load(aStream);
  aStream.Read(RoutineIndex, SizeOf(RoutineIndex));
  aStream.Read(LineNumber, SizeOf(LineNumber));
  aStream.Read(Counter, SizeOf(Counter));
  aStream.Read(Disabled, SizeOf(Disabled));
  aStream.Read(Valid, SizeOf(Valid));
end {TCoveragePoint.Load};


(************************)
(* TCoveragePoint.Print *)
(************************)

procedure TCoveragePoint.Print(aStream : TStream; Indent : integer);
  var
    s : string;
begin
  DoIndent(aStream, Indent);
  s := IntToStr(LineNumber) + ' ' + IntToHex(Address,8);
  aStream.Write(s[1], Length(s));
  DoNewLine(aStream);
end {TCoveragePoint.Print};


(***********************)
(* TCoveragePoint.Save *)
(***********************)

procedure TCoveragePoint.Save(aStream : TStream);
begin
  inherited Save(aStream);
  aStream.Write(RoutineIndex, SizeOf(RoutineIndex));
  aStream.Write(LineNumber, SizeOf(LineNumber));
  aStream.Write(Counter, SizeOf(Counter));
  aStream.Write(Disabled, SizeOf(Disabled));
  aStream.Write(Valid, SizeOf(Valid));
end {TCoveragePoint.Save};


(************************)
(* TCoveragePoints.Load *)
(************************)

constructor TCoveragePoints.Load(aStream : TStream);
  var
    n : integer;
    C : TCoveragePoint;
begin
  inherited Create;
  aStream.Read(n, SizeOf(n));
  while n > 0 do begin
    C := TCoveragePoint.Load(aStream);
    Insert(C);
    if C.Valid then begin
      inc(ValidPointsQty);
      if C.Counter > 0 then
        inc(ValidCoveredPointsQty);
      if not C.Disabled then begin
        inc(ValidEnabledPointsQty);
        if C.Counter > 0 then
         inc(ValidEnabledCoveredPointsQty);
      end {if};
    end {if};
    dec(n);
  end {while};
end {TCoveragePoints.Load};


(************************)
(* TCoveragePoints.Save *)
(************************)

procedure TCoveragePoints.Save(aStream : TStream);
  var
    n : integer;
begin
  n := Count;
  aStream.Write(n, SizeOf(n));
  for n := 0 to pred(Count) do
    TCoveragePoint(At(n)).Save(aStream);
end {TCoveragePoints.Save};


(******************************)
(* TListOfAddressable.Compare *)
(******************************)

function TListOfAddressable.Compare;
begin
  if integer(Key1) < integer(Key2) then
    Result := -1
  else if integer(Key1) > integer(Key2) then
    Result := 1
  else
    Result := 0;
end {TListOfAddressable.Compare};


(*****************************)
(* TListOfAddressable.Create *)
(*****************************)

constructor TListOfAddressable.Create;
begin
  inherited Create;
  Duplicates := true;
end {TListOfAddressable.Create};


(****************************)
(* TListOfAddressable.KeyOf *)
(****************************)

function TListOfAddressable.KeyOf(Item : Pointer) : Pointer;
begin
  integer(Result) := TAddressable(Item).Address;
end {TListOfAddressable.KeyOf};


(**************************)
(* TProjectDataBase.Clear *)
(**************************)

procedure TProjectDataBase.Clear;
  var
    i : integer;
    C : TCoveragePoint;
    R : TRoutine;
    U : TUnit;
begin
  with Units do
    for i := 0 to pred(Count) do begin
      U := Units.At(i);
      U.R0pc := 0;
      U.R100Pc := 0;
      U.ValidPointsQty := 0;
      U.CoveredPointsQty := 0;
    end {for};

  with Routines do
    for i := 0 to pred(Count) do begin
      R := At(i);
      R.CoveredPointsQty := 0;
    end {for};

  with CoveragePoints do begin
    ValidCoveredPointsQty := 0;
    ValidEnabledCoveredPointsQty := 0;
    for i := 0 to pred(CoveragePoints.Count) do begin
      C := CoveragePoints.At(i);
      C.Counter := 0;
    end {for};
  end {with};
  InitStatistics;
end {TProjectDataBase.Clear};


(***************************)
(* TProjectDataBase.Create *)
(***************************)

constructor TProjectDataBase.Create;
begin
  Units := TUnits.Create;
  Routines := TRoutines.Create;
  CoveragePoints := TCoveragePoints.Create;
end {TProjectDataBase.Create};


(****************************)
(* TProjectDataBase.Destroy *)
(****************************)

destructor TProjectDataBase.Destroy;
begin
  Units.Free;
  CoveragePoints.Free;
  Routines.Free;
  inherited Destroy;
end {TProjectDataBase.Destroy};


(*********************************)
(* TProjectDataBase.DisablePoint *)
(*********************************)

procedure TProjectDataBase.DisablePoint(C : TCoveragePoint);
begin
  if not C.Disabled then begin
    C.Disabled := true;
    dec(CoveragePoints.ValidEnabledPointsQty);
    if C.Counter > 0 then
      dec(CoveragePoints.ValidEnabledCoveredPointsQty);
    inc(ChangedCount);
  end {if};
end {TProjectDataBase.DisablePoint};


(************************************)
(* TProjectDataBase.EnabledCoverage *)
(************************************)

function TProjectDataBase.EnabledCoverage : single;
begin
  with CoveragePoints do
    if ValidEnabledPointsQty > 0 then begin
      Result := ValidEnabledCoveredPointsQty;
      Result := 100*Result / ValidEnabledPointsQty;
    end else
      Result := 0;
end {TProjectDataBase.EnabledCoverage};


(*****************************************)
(* TProjectDataBase.EnableDisableRoutine *)
(*****************************************)

procedure TProjectDataBase.EnableDisableRoutine(R : TRoutine; Enable : boolean);
  var
    i, RIdx : integer;
    C : TCoveragePoint;
begin
  RIdx := Routines.IndexOf(R);
  with CoveragePoints do begin
    i := R.FirstPointIndex;
    if i >= 0 then begin
      C := CoveragePoints.At(i);
      while (i < Count) and (C.RoutineIndex = RIdx) do begin
        if C.Valid then begin
          if Enable then
            EnablePoint(C)
          else
            DisablePoint(C);
        end {if};
        inc(i);
        if i < Count then
          C := CoveragePoints.At(i);
      end {while};
    end {if};
  end {with};
  R.Disabled := not Enable;
  inc(ChangedCount);
end {TProjectDataBase.EnableDisableRoutine};


(**************************************)
(* TProjectDataBase.EnableDisableUnit *)
(**************************************)

procedure TProjectDataBase.EnableDisableUnit(U : TUnit; Enable : boolean);
  var
    i, UIdx : integer;
    R : TRoutine;
begin
  UIdx := Units.IndexOf(U);
  with Routines do
    for i := 0 to pred(Count) do begin
      R := At(i);
      if R.UnitIndex = UIdx then
        EnableDisableRoutine(R, Enable);
    end {for};
  U.Disabled := not Enable;
  inc(ChangedCount);
end {TProjectDataBase.EnableDisableUnit};


(***************************************)
(* TProjectDataBase.EnabledRoutinesQty *)
(***************************************)

function TProjectDataBase.EnabledRoutinesQty : integer;
  var
    i : integer;
begin
  Result := 0;
  with Routines do
    for i := 0 to pred(Count) do
      if not TRoutine(At(i)).Disabled then
        inc(Result);
end {TProjectDataBase.EnabledRoutinesQty};


(************************************)
(* TProjectDataBase.EnabledUnitsQty *)
(************************************)

function TProjectDataBase.EnabledUnitsQty : integer;
  var
    i : integer;
begin
  Result := 0;
  with Units do
    for i := 0 to pred(Count) do
      if not TUnit(At(i)).Disabled then
        inc(Result);
end {TProjectDataBase.EnabledUnitsQty};


(********************************)
(* TProjectDataBase.EnablePoint *)
(********************************)

procedure TProjectDataBase.EnablePoint(C : TCoveragePoint);
begin
  if C.Disabled then begin
    C.Disabled := false;
    inc(CoveragePoints.ValidEnabledPointsQty);
    if C.Counter > 0 then
      inc(CoveragePoints.ValidEnabledCoveredPointsQty);
    inc(ChangedCount);
  end {if};
end {TProjectDataBase.EnablePoint};


(******************************)
(* TProjectDataBase.GetR100R0 *)
(******************************)

procedure TProjectDataBase.GetR100R0(var R100Pc, R0Pc : integer);
  var
    i : integer;
    U : TUnit;
begin
  R100Pc := 0;
  R0Pc := 0;
  with Units do
    for i := 0 to pred(Count) do begin
      U := At(i);
      if U.IsSourceAvailable then begin
        inc(R100Pc, U.R100Pc);
        inc(R0Pc, U.R0Pc);
      end {if};
    end {for};
end {TProjectDataBase.GetR100R0};


(***********************************)
(* TProjectDataBase.InitStatistics *)
(***********************************)

procedure TProjectDataBase.InitStatistics;
  var
    i : integer;
    U : TUnit;
    R : TRoutine;
begin
  RoutinesWithSource := 0;
  UnitsWithSource := 0;
  with Routines do
    for i := 0 to pred(Count) do begin
      R := At(i);
      U := Units.At(R.UnitIndex);
      U.Disabled := U.Disabled or not U.IsSourceAvailable;
      if U.IsSourceAvailable then
        inc(RoutinesWithSource);
    end {for};
  with Units do
    for i := 0 to pred(Count) do begin
      U := At(i);
      if not U.IsSourceAvailable then
        EnableDisableUnit(U,false)
      else
        inc(UnitsWithSource);
    end {for};
  ChangedCount := 0;
end {TProjectDataBase.InitStatistics};


(*************************)
(* TProjectDataBase.Load *)
(*************************)

constructor TProjectDataBase.Load(aStream : TStream);
  var
    n : integer;
begin
  aStream.Read(n, SizeOf(n));
  if n <> DataBaseMagicNumber then
    raise Exception.Create('State file cannot be read because the format is illegal or outdated');

  ExecutableFileName := ReadStringFromStream(aStream);
  aStream.Read(ExecutableFileCRC, SizeOf(ExecutableFileCRC));
  aStream.Read(ImageBase, SizeOf(ImageBase));
  Units := TUnits.Load(aStream);
  Routines := TRoutines.Load(aStream);
  CoveragePoints := TCoveragePoints.Load(aStream);
  ChangedCount := 0;
end {TProjectDataBase.Load};


(**********************************)
(* TProjectDataBase.MergeCoverage *)
(**********************************)

procedure TProjectDataBase.MergeCoverage(OldDataBase : TProjectDataBase);
  var
    NewU, OldU : TUnit;
    NewUIndex, OldUIndex : integer;

  procedure DoUnit;
    var
      NewRIndex, OldRIndex : integer;
      NewR, OldR : TRoutine;
      RoutineFound : boolean;

    function RoutineMatch : boolean;
      var
        OldPointIndex, NewPointIndex : integer;
        OldPoint, NewPoint : TCoveragePoint;
    begin
      OldPointIndex := OldR.FirstPointIndex;
      OldPoint := OldDataBase.CoveragePoints.At(OldPointIndex);
      NewPointIndex := NewR.FirstPointIndex;
      NewPoint := CoveragePoints.At(NewPointIndex);
      Result := true;
      while Result and (NewPointIndex < CoveragePoints.Count) and
        (NewPoint.RoutineIndex = NewRIndex) do begin
        Result := (NewPoint.Address - NewR.Address) =
          (OldPoint.Address - OldR.Address);
        inc(NewPointIndex);
        inc(OldPointIndex);
        Result := (NewPointIndex < CoveragePoints.Count) =
          (OldPointIndex < OldDataBase.CoveragePoints.Count);
        if Result and (NewPointIndex < CoveragePoints.Count) then begin
          NewPoint := CoveragePoints.At(NewPointIndex);
          OldPoint := OldDataBase.CoveragePoints.At(OldPointIndex);
          Result := (NewPoint.RoutineIndex = NewRIndex) =
            (OldPoint.RoutineIndex = OldRIndex);
        end {if};
      end {while};
    end {RoutineMatch};

    procedure TransferCoverage;
      var
        OldPointIndex, NewPointIndex : integer;
        OldPoint, NewPoint : TCoveragePoint;
    begin
      OldPointIndex := OldR.FirstPointIndex;
      OldPoint := OldDataBase.CoveragePoints.At(OldPointIndex);
      NewPointIndex := NewR.FirstPointIndex;
      NewPoint := CoveragePoints.At(NewPointIndex);
      while (NewPointIndex < CoveragePoints.Count) and
        (NewPoint.RoutineIndex = NewRIndex) do begin

        if (OldPoint.Counter > 0) and (NewPoint.Counter = 0) then
          SetCovered(NewPoint);

        inc(NewPointIndex);
        inc(OldPointIndex);
        if (NewPointIndex < CoveragePoints.Count) then begin
          NewPoint := CoveragePoints.At(NewPointIndex);
          OldPoint := OldDataBase.CoveragePoints.At(OldPointIndex);
        end {if};
      end {while};
    end {TransferCoverage};

  begin
    NewRIndex := NewU.FirstRoutineIndex;
    if NewRIndex >= 0 then begin
      NewR := Routines.At(NewRIndex);
      while (NewRIndex < Routines.Count) and (NewR.UnitIndex = NewUIndex) do begin
        if (NewR.FirstPointIndex >= 0) then begin
          // Process NewR
          OldRIndex := OldU.FirstRoutineIndex;
          OldR := OldDataBase.Routines.At(OldRIndex);
          // Search for a matching routine
          RoutineFound := false;
          while (not RoutineFound) and (OldRIndex < OldDataBase.Routines.Count) and (OldR.UnitIndex = OldUIndex) do begin
            RoutineFound := (CompareText(OldR.Name,NewR.Name) = 0) and
              (OldR.FirstPointIndex >=0) and RoutineMatch;
            inc(OldRIndex);
            if (not RoutineFound) and (OldRIndex < OldDataBase.Routines.Count) then
              OldR := OldDataBase.Routines.At(OldRIndex);
          end {while};
          if RoutineFound then
            TransferCoverage
          else
//            if LogFileEnabled_ then
//              Writeln(LogFile_, Format('New or changed: Unit=%s, R=%s',[NewU.Name, NewR.Name]));
        end {if};

        inc(NewRIndex);
        if NewRIndex < Routines.Count then
          NewR := Routines.At(NewRIndex);
      end {while};
    end {if};
  end {DoUnit};

begin
  with Units do
    for NewUIndex := 0 to pred(Count) do begin
      NewU := At(NewUIndex);
      OldUIndex := OldDataBase.Units.AtNameIndex(NewU.Name);
      if OldUIndex >= 0 then begin
        OldU := OldDataBase.Units.At(OldUIndex);
        DoUnit
      end else
        // Unit not found in OldDataBase
    end {for};
end {TProjectDataBase.MergeCoverage};


(*************************)
(* TProjectDataBase.Save *)
(*************************)

procedure TProjectDataBase.Save(aStream : TStream);
  var
    n : integer;
begin
  n := DataBaseMagicNumber;
  aStream.Write(n, SizeOf(n));
  WriteStringToStream(aStream, ExecutableFileName);
  aStream.Write(ExecutableFileCRC, SizeOf(ExecutableFileCRC));
  aStream.Write(ImageBase, SizeOf(ImageBase));
  Units.Save(aStream);
  Routines.Save(aStream);
  CoveragePoints.Save(aStream);
  Units.Save(aStream);
  Routines.Save(aStream);
  CoveragePoints.Save(aStream);
  ChangedCount := 0;
end {TProjectDataBase.Save};


(*******************************)
(* TProjectDataBase.SetCovered *)
(*******************************)

procedure TProjectDataBase.SetCovered(C : TCoveragePoint);
  var
    R : TRoutine;
begin
  if C.Counter = 0 then begin
    inc(C.Counter);
    inc(ChangedCount);
    R := Routines.At(C.RoutineIndex);
    inc(R.CoveredPointsQty);
    if C.Valid then begin
      inc(CoveragePoints.ValidCoveredPointsQty);
      if not C.Disabled then
        inc(CoveragePoints.ValidEnabledCoveredPointsQty);
    end {if};
  end {if};
end {TProjectDataBase.SetCovered};


(**********************************)
(* TProjectDataBase.TotalCoverage *)
(**********************************)

function TProjectDataBase.TotalCoverage : single;
begin
  with CoveragePoints do
    if ValidPointsQty > 0 then begin
      Result := ValidCoveredPointsQty;
      Result := 100*Result / ValidPointsQty;
    end else
      Result := 0;
end {TProjectDataBase.TotalCoverage};


(***************************)
(* TProjectDataBase.Update *)
(***************************)

procedure TProjectDataBase.Update(OldDataBase : TProjectDataBase);
  // Translate the forgnd/backgnd status from OldDataBase to self
  var
    OldU, NewU : TUnit;
    OldR, NewR : TRoutine;
    i, Index : integer;
    S : TSortedNameIndex;
    N : TNameIndex;
begin
  // Sort all routines according to their unit.routine name
  S := TSortedNameIndex.Create;
  S.Duplicates := true;
  for i := 0 to pred(Routines.Count) do begin
    NewR := Routines.At(i);
    NewU := Units.At(NewR.UnitIndex);
    N := TNameIndex.Create;
    N.Name := NewU.Name+NewR.Name;
    N.Index := i;
    S.Insert(N);
  end {for};

  try
    for i := 0 to pred(OldDataBase.Routines.Count) do begin
      OldR := OldDataBase.Routines.At(i);
      if OldR.Disabled then begin
        OldU := OldDataBase.Units.At(OldR.UnitIndex);
        if S.Search(PChar(OldU.Name+OldR.Name), Index) then begin
          N := S.At(Index);
          NewR := Routines.At(N.Index);
          Self.EnableDisableRoutine(NewR, false);
        end {if};
      end {if};
    end {for};
  finally
    S.Free;
  end {try};

  S := TSortedNameIndex.Create;
  for i := 0 to pred(Units.Count) do begin
    NewU := Units.At(i);
    N := TNameIndex.Create;
    N.Name := NewU.Name;
    N.Index := i;
    S.Insert(N);
  end {for};
  try
    for i := 0 to pred(OldDataBase.Units.Count) do begin
      OldU := OldDataBase.Units.At(i);
      if OldU.Disabled then begin
        if S.Search(PChar(OldU.Name), Index) then begin
          N := S.At(Index);
          NewU := Units.At(N.Index);
          NewU.Disabled := true;
        end {if};
      end {if};
    end {for};
  finally
    S.Free;
  end {try};
end {TProjectDataBase.Update};


(*************************************)
(* TProjectDataBase.UpdateStatistics *)
(*************************************)

procedure TProjectDataBase.UpdateStatistics;
  var
    i : integer;
    U : TUnit;
    R : TRoutine;
begin
  with Units do
    for i := 0 to pred(Count) do begin
      U := Units.At(i);
      U.R0pc := 0;
      U.R100Pc := 0;
      U.ValidPointsQty := 0;
      U.CoveredPointsQty := 0;
    end {for};

  with Routines do
    for i := 0 to pred(Count) do begin
      R := At(i);
      U := Units.At(R.UnitIndex);
      if U.IsSourceAvailable then begin
        inc(U.ValidPointsQty, R.ValidPointsQty);
        inc(U.CoveredPointsQty, R.CoveredPointsQty);
        if R.ValidPointsQty > 0 then begin
          if R.CoveredPointsQty = 0 then
            inc(U.R0Pc)
          else if R.ValidPointsQty = R.CoveredPointsQty then
            inc(U.R100pc);
        end else
          U := U;  // For BP
      end {if};
    end {for};
end {TProjectDataBase.UpdateStatistics};


(*******************)
(* TRoutine.Create *)
(*******************)

constructor TRoutine.Create;
begin
  inherited Create;
  FileIndex := -1;
  FirstPointIndex := -1;
end {TRoutine.Create};


(********************)
(* TRoutine.Destroy *)
(********************)

destructor TRoutine.Destroy;
begin
  inherited Destroy;
end {TRoutine.Destroy};


(*****************)
(* TRoutine.Load *)
(*****************)

constructor TRoutine.Load(aStream : TStream);
begin
  inherited Load(aStream);
  Name := ReadStringFromStream(aStream);
  aStream.Read(UnitIndex, SizeOf(UnitIndex));
  aStream.Read(FileIndex, SizeOf(FileIndex));
  aStream.Read(FirstPointIndex, SizeOf(FirstPointIndex));
  aStream.Read(ValidPointsQty, SizeOf(ValidPointsQty));
  aStream.Read(CoveredPointsQty, SizeOf(CoveredPointsQty));
  aStream.Read(Disabled, SizeOf(Disabled));
end {TRoutine.Load};


(******************)
(* TRoutine.Print *)
(******************)

procedure TRoutine.Print;
begin
  Writeln(aFile, Format('%sUIndex=%d, FIndex=%d, FirstPIndex=%d',[BlankStr(Indent), UnitIndex, FileIndex, FirstPointIndex]));
end {TRoutine.Print};


(*****************)
(* TRoutine.Save *)
(*****************)

procedure TRoutine.Save(aStream : TStream);
begin
  inherited Save(aStream);
  WriteStringToStream(aStream, Name);
  aStream.Write(UnitIndex, SizeOf(UnitIndex));
  aStream.Write(FileIndex, SizeOf(FileIndex));
  aStream.Write(FirstPointIndex, SizeOf(FirstPointIndex));
  aStream.Write(ValidPointsQty, SizeOf(ValidPointsQty));
  aStream.Write(CoveredPointsQty, SizeOf(CoveredPointsQty));
  aStream.Write(Disabled, SizeOf(Disabled));
end {TRoutine.Save};


(********************)
(* TRoutines.AtName *)
(********************)

function TRoutines.AtName(const aName : string) : TRoutine;
  var
    i : integer;
begin
  for i := 0 to pred(Count) do begin
    Result := At(i);
    if Comparetext(Result.Name, aName) = 0 then
      exit;
  end {for};
  Result := nil;
end {TRoutines.AtName};


(*************************)
(* TRoutines.AtNameIndex *)
(*************************)

function TRoutines.AtNameIndex(const aName : string) : integer;
  var
    i : integer;
    R : TRoutine;
begin
  for i := 0 to pred(Count) do begin
    Result := i;
    R := At(i);
    if Comparetext(R.Name, aName) = 0 then
      exit;
  end {for};
  Result := -1;
end {TRoutines.AtNameIndex};


(******************)
(* TRoutines.Load *)
(******************)

constructor TRoutines.Load(aStream : TStream);
  var
    n : integer;
begin
  inherited Create;
  aStream.Read(n, SizeOf(n));
  while n > 0 do begin
    Insert(TRoutine.Load(aStream));
    dec(n);
  end {while};
end {TRoutines.Load};


(*******************)
(* TRoutines.Print *)
(*******************)

procedure TRoutines.Print(var aFile: TextFile; Indent: integer);
  var
    i : integer;
    R : TRoutine;
begin
  for i := 0 to pred(Count) do begin
    R := At(i);
    Writeln(aFile, Format('%sRoutine[%d]=%s', [BlankStr(Indent), i, R.Name]));
    R.Print(aFile, Indent+2);
  end {for};
end {TRoutines.Print};


(******************)
(* TRoutines.Save *)
(******************)

procedure TRoutines.Save(aStream : TStream);
  var
    n : integer;
begin
  n := Count;
  aStream.Write(n, SizeOf(n));
  for n := 0 to pred(Count) do
    TRoutine(At(n)).Save(aStream);
end {TRoutines.Save};


(****************************)
(* TSortedNameIndex.Compare *)
(****************************)

function TSortedNameIndex.Compare(Item1, Item2: pointer): integer;
begin
  Result := StrComp(PChar(Item1), PChar(Item2))
end {TSortedNameIndex.Compare};


(**************************)
(* TSortedNameIndex.KeyOf *)
(**************************)

function TSortedNameIndex.KeyOf(Item: pointer): pointer;
begin
  Result := PChar(TNameIndex(Item).Name);
end {TSortedNameIndex.KeyOf};


(***************************)
(* TSortedRoutines.Compare *)
(***************************)

function TSortedRoutines.Compare(Key1, Key2 : Pointer) : integer;
  var
    U1, U2 : TUnit;
    R1, R2 : TRoutine;
    x1, x2 : single;
begin
  R1 := TRoutine(Key1);
  R2 := TRoutine(Key2);
(*
  if R1.Disabled and not R2.Disabled then
    Result := 1
  else if (not R1.Disabled) and R2.Disabled then
    Result := -1
  else begin
*)
    case Mode of
      srByName:
        Result := CompareText(R1.Name, R2.Name);

      srByUnit: begin
        U1 := ProjectDataBase_.Units.At(R1.UnitIndex);
        U2 := ProjectDataBase_.Units.At(R2.UnitIndex);
        Result := CompareText(U1.Name+R1.Name, U2.Name+R2.Name);
      end;

      srByPoints: begin
        if R1.ValidPointsQty < R2.ValidPointsQty then
          Result := 1
        else if R1.ValidPointsQty > R2.ValidPointsQty then
          Result := -1
        else
          Result := CompareText(R1.Name, R2.Name);
      end;

      srByCoverage : begin
        if R1.ValidPointsQty > 0 then begin
          x1 := R1.CoveredPointsQty;
          x1 := x1 / R1.ValidPointsQty
        end else
          x1 := 0;
        if R2.ValidPointsQty > 0 then begin
          x2 := R2.CoveredPointsQty;
          x2 := x2 / R2.ValidPointsQty
        end else
          x2 := 0;
        if x1 < x2 then
          Result := 1
        else if x1 > x2 then
          Result := -1
        else
          Result := CompareText(R1.Name, R2.Name);
      end;
    end {case};
    if Inverted then
      Result := -Result;
(*
  end {if};
*)  
end {TSortedRoutines.Compare};


(************************)
(* TSortedUnits.Compare *)
(************************)

function TSortedUnits.Compare(Key1, Key2 : Pointer) : integer;
  var
    U1, U2 : TUnit;
    x1, x2 : single;
begin
  U1 := TUnit(Key1);
  U2 := TUnit(Key2);

  case Mode of
    suByName:
      Result := CompareText(U1.Name, U2.Name);

    suBySize: begin
      if U1.Size < U2.Size then
        Result := 1
      else if U1.Size > U2.Size then
        Result := -1
      else
        Result := CompareText(U1.name, U2.Name);
    end;

    suByRQty: begin
      if U1.RoutinesQty < U2.RoutinesQty then
        Result := 1
      else if U1.RoutinesQty > U2.RoutinesQty then
        Result := -1
      else
        Result := CompareText(U1.name, U2.Name);
    end;

    suByR0Pc: begin
      if U1.R0Pc < U2.R0Pc then
        Result := 1
      else if U1.R0Pc > U2.R0Pc then
        Result := -1
      else
        Result := CompareText(U1.name, U2.Name);
    end;

    suByR100Pc: begin
      if U1.R100Pc < U2.R100Pc then
        Result := 1
      else if U1.R100Pc > U2.R100Pc then
        Result := -1
      else
        Result := CompareText(U1.name, U2.Name);
    end;

    suByCoverage : begin
      if U1.ValidPointsQty > 0 then begin
        x1 := U1.CoveredPointsQty;
        x1 := x1 / U1.ValidPointsQty
      end else
        x1 := 0;
      if U2.ValidPointsQty > 0 then begin
        x2 := U2.CoveredPointsQty;
        x2 := x2 / U2.ValidPointsQty
      end else
        x2 := 0;
      if x1 < x2 then
        Result := 1
      else if x1 > x2 then
        Result := -1
      else
        Result := CompareText(U1.Name, U2.Name);
    end;
  end {case};
  if Inverted then
    Result := -Result;

end {TSortedUnits.Compare};


(****************)
(* TUnit.Create *)
(****************)

constructor TUnit.Create;
begin
  inherited Create;
  DefinedConditionnals := TStringList.Create;
  FileNames := TStringList.Create;
  FirstRoutineIndex := -1;
end {TUnit.Create};


(*****************)
(* TUnit.Destroy *)
(*****************)

destructor TUnit.Destroy;
begin
  DefinedConditionnals.Free;
  FileNames.Free;
  inherited Destroy;
end {TUnit.Destroy};


(***************************)
(* TUnit.IsSourceAvailable *)
(***************************)

function TUnit.IsSourceAvailable : boolean;
begin
  Result := FileNames.Count > 0;
end {TUnit.IsSourceAvailable};


(**************)
(* TUnit.Load *)
(**************)

constructor TUnit.Load(aStream : TStream);
  var
    n : integer;
begin
  inherited Load(aStream);
  Name := ReadStringFromStream(aStream);
  aStream.Read(Size, SizeOf(Size));
  FileNames := TStringList.Create;
  aStream.Read(n, SizeOf(n));
  while n > 0 do begin
    FileNames.Add(ReadStringFromStream(aStream));
    dec(n);
  end {while};
  aStream.Read(FirstRoutineIndex, SizeOf(FirstRoutineIndex));
  aStream.Read(Disabled, SizeOf(Disabled));
  aStream.Read(RoutinesQty, SizeOf(RoutinesQty));
  aStream.Read(ValidPointsQty, SizeOf(ValidPointsQty));
  aStream.Read(CoveredPointsQty, SizeOf(CoveredPointsQty));
end {TUnit.Load};


(***************)
(* TUnit.Print *)
(***************)

procedure TUnit.Print;
  var
    i : integer;
    IndentStr : string;
begin
  IndentStr := BlankStr(Indent);
  with FileNames do
    for i := 0 to pred(Count) do begin
      Writeln(aFile, Format('%sFileName[%d]=%s', [IndentStr, i, Strings[i]]));
    end {for};
  Writeln(aFile, Format('%sFirstRIndex=%d, RQty=%d',[IndentStr, FirstRoutineIndex, RoutinesQty]));
end {TUnit.Print};


(**************)
(* TUnit.Save *)
(**************)

procedure TUnit.Save(aStream : TStream);
  var
    n : integer;
begin
  inherited Save(aStream);
  WriteStringToStream(aStream, Name);
  aStream.Write(Size, SizeOf(Size));
  n := FIleNames.Count;
  aStream.Write(n, SizeOf(n));
  with FileNames do
    for n := 0 to pred(Count) do
      WriteStringToStream(aStream, Strings[n]);
  aStream.Write(FirstRoutineIndex, SizeOf(FirstRoutineIndex));
  aStream.Write(Disabled, SizeOf(Disabled));
  aStream.Write(RoutinesQty, SizeOf(RoutinesQty));
  aStream.Write(ValidPointsQty, SizeOf(ValidPointsQty));
  aStream.Write(CoveredPointsQty, SizeOf(CoveredPointsQty));
end {TUnit.Save};


(*****************)
(* TUnits.AtName *)
(*****************)

function TUnits.AtName(const aName : string) : TUnit;
  var
    i : integer;
begin
  for i := 0 to pred(Count) do begin
    Result := At(i);
    if Comparetext(Result.Name, aName) = 0 then
      exit;
  end {for};
  Result := nil;
end {TUnits.AtName};


(**********************)
(* TUnits.AtNameIndex *)
(**********************)

function TUnits.AtNameIndex(const aName : string) : integer;
  var
    i : integer;
    U : TUnit;
begin
  for i := 0 to pred(Count) do begin
    Result := i;
    U := At(i);
    if CompareText(U.Name, aName) = 0 then
      exit;
  end {for};
  Result := -1;
end {TUnits.AtNameIndex};


(***************)
(* TUnits.Load *)
(***************)

constructor TUnits.Load(aStream : TStream);
  var
    n : integer;
begin
  inherited Create;
  aStream.Read(n, SizeOf(n));
  while n > 0 do begin
    Insert(TUnit.Load(aStream));
    dec(n);
  end {while};
end {TUnits.Load};


(****************)
(* TUnits.Print *)
(****************)

procedure TUnits.Print;
  var
    i : integer;
    U : TUnit;
begin
  for i := 0 to pred(Count) do begin
    U := At(i);
    Writeln(aFile, Format('%sUnit[%d]=%s', [BlankStr(Indent), i, U.Name]));
    U.Print(aFile, Indent+2);
  end {for};
end {TUnits.Print};


(***************)
(* TUnits.Save *)
(***************)

procedure TUnits.Save(aStream : TStream);
  var
    n : integer;
begin
  n := Count;
  aStream.Write(n, SizeOf(n));
  for n := 0 to pred(Count) do
    TUnit(At(n)).Save(aStream);
end {TUnits.Save};


{~b}

initialization
  ProjectDataBase_ := nil;
finalization
  ProjectDataBase_.Free;
end.
