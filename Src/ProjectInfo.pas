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
unit ProjectInfo;

interface
  uses
    Classes, IniFiles;
  type
    TProjectInfo = class(TObject)
    private
      FStartupDirectory : string;
      FFRunParameters : string;
      FRunMaximized : boolean;
      FBackGndUnits : TStringList;
      FBackGndRoutines : TStringList;
      procedure SaveStringList(F : TIniFile; L : TStringList; const Section : string);
      procedure LoadStringList(F : TIniFile; L : TStringList; const Section : string);
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadFromFile(const FileName : string);
      procedure SaveToFile(const FileName : string);
      property StartupDirectory : string read FStartupDirectory write FStartupDirectory;
      property RunParameters : string read FFRunParameters write FFRunParameters;
      property RunMaximized : boolean read FRunMaximized write FRunMaximized;
      property BackGndUnits : TStringList read FBackGndUnits;
      property BackGndRoutines : TStringList read FBackGndRoutines;
    end;

implementation
  uses
    SysUtils;

  type
    TSectionKey = (iSection, iKey);

  const SRunParam : array [TSectionKey] of string = ('Start', 'RunParams');
  const SStartupDir : array [TSectionKey] of string = ('Start', 'StartupDir');
  const SRunMaximized : array [TSectionKey] of string = ('Start', 'RunMaximized');
{~t}
(***********************)
(* TProjectInfo.Create *)
(***********************)

constructor TProjectInfo.Create;
begin
  FBackGndUnits := TStringList.Create;
  FBackGndRoutines := TStringList.Create;
  inherited;
end {TProjectInfo.Create};


(************************)
(* TProjectInfo.Destroy *)
(************************)

destructor TProjectInfo.Destroy;
begin
  FBackGndRoutines.Free;
  FBackGndUnits.Free;
  inherited;
end {TProjectInfo.Destroy};


(*****************************)
(* TProjectInfo.LoadFromFile *)
(*****************************)

procedure TProjectInfo.LoadFromFile(const FileName: string);
  var
    IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
  try
    FFRunParameters := IniFile.ReadString(SRunParam[iSection], SRunParam[iKey], '');
    FStartupDirectory := IniFile.ReadString(SStartupDir[iSection], SStartupDir[iKey], '');
    FRunMaximized := IniFile.ReadBool(SRunMaximized[iSection], SRunMaximized[iKey], false);
    LoadStringList(IniFile, FBackGndUnits, 'BkndU');
    LoadStringList(IniFile, FBackGndRoutines, 'BkndR');
  finally
    IniFile.Free;
  end {try};
end {TProjectInfo.LoadFromFile};


(*******************************)
(* TProjectInfo.LoadStringList *)
(*******************************)

procedure TProjectInfo.LoadStringList(F : TIniFile; L: TStringList;
  const Section: string);
  var
    i,n : integer;
    s : string;
    T : TStringList;
begin
  T := TStringList.Create;
  try
    i := F.ReadInteger(Section, '0', 0);
    for n := 1 to i do begin
      T.Clear;
      s := F.ReadString(Section, IntToStr(n), '');
      T.CommaText := s;
      L.AddStrings(T);
    end {for};
  finally
    T.Free;
  end {try};
end {TProjectInfo.LoadStringList};


(*******************************)
(* TProjectInfo.SaveStringList *)
(*******************************)

procedure TProjectInfo.SaveStringList(F : TIniFile; L: TStringList;
  const Section: string);
  var
    i, n : integer;
    s : string;
begin
  s := '';
  n := 0;
  for i := 0 to pred(L.Count) do begin
    if s <> '' then
      s := s + ',';
    s := s + L[i];
    if i mod 6 = 5 then begin
      inc(n);
      F.WriteString(Section, IntToStr(n), s);
      s := '';
    end {if};
  end {for};
  if s <> '' then begin
    inc(n);
    F.WriteString(Section, IntToStr(n), s);
  end {if};
  F.WriteInteger(Section, '0', n);
end {TProjectInfo.SaveStringList};


(***************************)
(* TProjectInfo.SaveToFile *)
(***************************)

procedure TProjectInfo.SaveToFile(const FileName: string);
  var
    IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
  try
    IniFile.WriteString(SRunParam[iSection], SRunParam[iKey], FFRunParameters);
    IniFile.WriteString(SStartupDir[iSection], SStartupDir[iKey], FStartupDirectory);
    IniFile.WriteBool(SRunMaximized[iSection], SRunMaximized[iKey], FRunMaximized);
    SaveStringList(IniFile, FBackGndUnits, 'BkndU');
    SaveStringList(IniFile, FBackGndRoutines, 'BkndR');
  finally
    IniFile.Free;
  end {try};
end {TProjectInfo.SaveToFile};


{~b}
end.
