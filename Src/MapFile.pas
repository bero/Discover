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
unit MapFile;

interface
  uses
    Classes, Globals;

  procedure HandleMapFile (const FileName : string;
    ProgressEvent : TProgressEvent; NotFoundFiles : TStringList;
    IsBDS : boolean);

implementation
  uses
    ObjectPascalTokenizer, DataBase, SysUtils, Util;
{~t}
(*****************)
(* HandleMapFile *)
(*****************)

  procedure HandleMapFile (const FileName : string;
    ProgressEvent : TProgressEvent; NotFoundFiles : TStringList;
    IsBDS : boolean);
    const
      ActionString = 'Processing map file';
    var
      MapFile : TextFile;

    function Hex8(const s : string; Index : integer) : integer;
      var
        i : integer;
        c : char;
    begin
      Result := 0;
      for i := 1 to 8 do begin
        Result := Result * 16;
        c := UpCase(s[Index]);
        if (c >= '0') and (c <= '9') then
          Result := Result + ord(c) - ord('0')
        else if (c >= 'A') and (c <= 'F') then
          Result := Result + ord(c) - ord('A') + 10
        else
          raise Exception.Create('Hex8 : Illegal character');
        inc(Index);
      end {for};
    end {Hex8};

    function Identifier(const s : string; Index : integer) : string;
      var
        n : integer;
    begin
      Result := '';
      n := Index;
      while (Index <= length(s)) and (s[Index] <> ' ') do
        inc(Index);
      Result := Copy(s,n,Index-n);
    end;

    procedure DetailedMapOfSegments;
      var
        s : string;
        U : TUnit;
    begin
      // Go to it
      while not Eof(MapFile) do begin
        Readln(MapFile, s);
        if Pos('Detailed map of segments',s) > 0 then break;
      end {while};
      if not Eof(MapFile) then begin
        Readln(MapFile);
        Readln(MapFile, s);
        while (not Eof(MapFile)) and (s[5] = '1') do begin
          U := TUnit.Create;
          U.Address := Hex8(s,7);
          U.Size := Hex8(s,16);
          U.Name := Identifier(s, 60);
          ProjectDataBase_.Units.Insert(U);
          Readln(MapFile, s);
        end {while};
      end {if};
    end {DetailedMapOfSegments};

    procedure PublicsByValue;
      var
        s : string;
        UnitIndex, RoutineAddress, p : integer;
        LastU, U : TUnit;
        RoutineName : string;
        R : TRoutine;
    begin
      while not Eof(MapFile) do begin
        Readln(MapFile, s);
        if Pos('Publics by Value',s) > 0 then
          break;
      end {while};
      if not Eof(MapFile) then begin
        Readln(MapFile);
        Readln(MapFile);
        Readln(MapFile, s);
        UnitIndex := 0;
        U := ProjectDataBase_.Units.At(UnitIndex);
        LastU := U;
        while (not Eof(MapFile)) and (length(s) >= 5) and
          ((s[5] = '1') or (s[5] = '2')) do begin
          // We only look in segment .text (1)
          // In BDS or D2007 there is a .itext segment (2) that we ignore
          // Note that the initialization code is in .itext and the finalization
          // in .text
          if s[5] = '1' then begin
            RoutineAddress := Hex8(s,7);
            // Locate the unit containing the routine
            while not((RoutineAddress >= U.Address) and
              (RoutineAddress < U.Address+U.Size))do begin
              inc(UnitIndex);
              U := ProjectDataBase_.Units.At(UnitIndex);
(* Why this ?
              if U <> LastU then begin
                // We just changed unit, remove the last routine of the prev unit
                if LastU.RoutinesQty > 1 then begin
                  R := ProjectDatabase_.Routines.At(pred(ProjectDataBase_.Routines.Count));
                  ProjectDataBase_.Routines.AtFree(pred(ProjectDataBase_.Routines.Count));
                  dec(LastU.RoutinesQty);
                end {if};
                LastU := U;
              end {if};
*)
            end {while};
            RoutineName := Identifier(s, 22);

            // BDS and D2007
            if IsBDS then begin
              // The routine name is prefixed by "UniName."
              p := Pos(U.Name, RoutineName);
              if p <> 1 then
                raise Exception.Create(Format('Unexpected routine name format %s', [RoutineName]))
              else
                // Remove the unit name and the separating dot
                Delete(RoutineName, 1, length(U.Name)+1);
            end {if};
            
            if (RoutineName[1] <> '@') and  (RoutineName[1] <> '.') and
              (UpperCase(RoutineName) <> 'FINALIZATION') then begin
              R := TRoutine.Create;
              R.Name := RoutineName;
              R.Address := RoutineAddress;
              R.UnitIndex := UnitIndex;
              if U.FirstRoutineIndex < 0 then
                U.FirstRoutineIndex := ProjectDataBase_.Routines.Count;
              ProjectDataBase_.Routines.Insert(R);
              inc(U.RoutinesQty);
            end {if};
          end {if};
          ReadLn(MapFile, s);
        end {while};
      end {if};
    end {PublicsByValue};

    procedure LineNumbers;
      var
        s : string;
        CurrentUnitIndex, LastCPAddress : integer;

      procedure ProcessBlock;
        var
          UnitName, FileName : string;
          LastCPLineNumber, p, CurrentRoutineIndex, CurrentFileIndex,
            LastRoutineAddress : integer;
          U : TUnit;
          CurrentRoutine : TRoutine;
          FileDone : boolean;

        procedure NextRoutine;
        begin
          inc(CurrentRoutineIndex);
          with ProjectDataBase_.Routines do
            if CurrentRoutineIndex < Count then begin
              CurrentRoutine := At(CurrentRoutineIndex);
              if CurrentRoutineIndex < pred(Count) then
                LastRoutineAddress :=
                  pred(TRoutine(At(succ(CurrentRoutineIndex))).Address)
              else
                LastRoutineAddress := $7fffffff;
            end else
              CurrentRoutine := nil;
          end {NextRoutine};

        procedure EnterPoint(LineNumber, Address : integer);
          var
            FileFullPath : string;
            C : TCoveragePoint;
        begin
          if (CurrentRoutine <> nil) and (Address >= CurrentRoutine.Address) then begin

            // Sanity check: in D2006, the address of the last point given under
            // ''line number for'' can be out of the corresponding unit! We ignore
            // these points
            if not (Address < U.Address + U.Size) then begin
              p := p; // for BP
              exit;
            end {if};

            while (CurrentRoutine <> nil) and
              not ((Address >= CurrentRoutine.Address) and (Address <= LastRoutineAddress)) do
              NextRoutine;
            if CurrentRoutine <> nil  then begin
              if not FileDone then begin
                FileFullPath := GetFullFilePath(FileName);
                CurrentFileIndex := U.FileNames.IndexOf(FileFullPath);
                if CurrentFileIndex < 0 then begin
                  if not FileExists(FileFullPath) then
                    NotFoundFiles.Add(FileName)
                  else begin
                    U.FileNames.Add(FileFullPath);
                    CurrentFileIndex := pred(U.FileNames.Count);
                    if LogFileEnabled_ then
                      Writeln(LogFile_, '  File found:'+ FileFullPath);
                  end {if};
                end {if};
                FileDone := true;
              end {if};
              // Create the new point
              C := TCoveragePoint.Create;
              C.LineNumber := LineNumber;
              C.Address := Address;
              C.RoutineIndex := CurrentRoutineIndex;
              if CurrentRoutine.FirstPointIndex < 0 then begin
                CurrentRoutine.FirstPointIndex := ProjectDataBase_.CoveragePoints.Count;
                CurrentRoutine.FileIndex := CurrentFileIndex;
              end {if};
              ProjectDataBase_.CoveragePoints.Insert(C);
            end {if};
            LastCPAddress := Address;
            LastCPLineNumber := LineNumber;
          end {if};
        end {EnterPoint};

        procedure ProcessLine;
          var
            LineNumber, n, Address : integer;

          procedure SkipBlanks;
          begin
            while s[n] = ' ' do
              inc(n);
          end {SkipBlanks};

          function Int : integer;
          begin
            Result := 0;
            while (s[n] >= '0') and (s[n] <= '9') do begin
              Result := Result * 10 + ord(s[n]) - ord('0');
              inc(n);
            end {while};
          end {Int};

        begin
          n := 1;
          while n <= Length(s) do begin
            SkipBlanks;
            LineNumber := Int;
            inc(n,6);
            Address := Hex8(s,n);
            inc(n,8);
            if (Address > LastCPAddress) and
              (LineNumber > LastCPLineNumber) then
              EnterPoint(LineNumber, Address);
          end {while};
        end {ProcessLine};

      begin
        if LogFileEnabled_ then
          Writeln(LogFile_, 'Processing:'+ s);
        p := Pos('(',s);
        UnitName := Copy(s,18,p-18);
        FileName := Copy(s, succ(p), Pos(')', s)-succ(p));
        // Locate the Unit
        U := ProjectDataBase_.Units.AtName(UnitName);
        if (U <> nil) and (U.FirstRoutineIndex >= 0) then begin
          CurrentUnitIndex := ProjectDataBase_.Units.IndexOf(U);
          ReadLn(MapFile);
          ReadLn(MapFile, s);
          LastCPLineNumber := -1;
          CurrentRoutineIndex := pred(U.FirstRoutineIndex);
          NextRoutine;
          FileDone := false;
          while (not Eof(MapFile)) and (Length(s) > 0) do begin
            ProcessLine;
            Readln(MapFile,s);
          end {while};
        end else
          ReadLn(MapFile, s);
      end {ProcessBlock};

      procedure GotoLineNumbers;
      begin
        // Look for a line with ''Line number for'' and segment ''.text'', therefore, ignore
        // the segment ''.itext'' which are new for D2006 and D2007
        while not Eof(MapFile) and ((Pos('Line numbers for',s) = 0) or (Pos('.text',s) = 0)) do
          Readln(MapFile, s);
      end {GotoLineNumbers};

    begin
      GotoLineNumbers;
      if not Eof(MapFile) then begin
        CurrentUnitIndex := -1;
        LastCPAddress := -1;
        while not Eof(MapFile) do begin
          ProcessBlock;
          GotoLineNumbers;
        end {while};
      end else
        raise Exception.Create('A DETAILED map file is required.');
    end {LineNumbers};

   begin {HandleMapFile}
     Assign(MapFile, FileName);
     Reset(MapFile);
     try
       DetailedMapOfSegments;
       PublicsByValue;
       LineNumbers;
     finally
       CloseFile(MapFile);
     end {try};
   end {HandleMapFile};


{~b}
end.
