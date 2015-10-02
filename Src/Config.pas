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
unit
  Config;

interface
  uses
    Dialogs;

  type
    TProfileSection = (psOldPath, psLastFiles);
    TProfileSectionStrings = array [TProfileSection] of string;

  const
    DelphiProject_Key = '1';
    State_Key = '2';
    Lib_Key = '3';
    Appearence_Key = '4';
    DefaultState_Key = '5';
    Export_Key = '6';
     
  const
    ProfileSectionStrings : TProfileSectionStrings = (
      'OldPath',
      'LastFiles');

  function ExecDialog(D : TOpenDialog; const Key : string) : boolean;
  function LastFileName(const Key : string) : string;
  procedure SetLastFileName(const Key, Name : string);
  procedure GetIntegerArray(const Key : string; var A : array of integer);
  procedure SetIntegerArray(const Key : string; const A : array of integer);

implementation

  uses
    IniFiles, Globals, SysUtils;

  const
    MaxSelMacLines = 10;
    DefaultPrUnits : array [1..4] of integer = (1,2,3,5);
{~t}
(**************)
(* ExecDialog *)
(**************)

  function ExecDialog(D : TOpenDialog; const Key : string) : boolean;
    var
      IniFile : TIniFile;
  begin
    IniFile := TIniFile.Create(PrivateProfileFileName_);
    try
      if D.FileName = '' then
        D.FileName := IniFile.ReadString(ProfileSectionStrings[psLastFiles],Key,'');
      Result := D.Execute;
      if Result then
        IniFile.WriteString(ProfileSectionStrings[psLastFiles],Key,D.FileName);
    finally
      IniFile.Free;
    end {try};
  end {ExecDialog};


(*******************)
(* GetIntegerArray *)
(*******************)

  procedure GetIntegerArray(const Key : string; var A : array of integer);
    var
      IniFile : TIniFile;
      i : integer;
  begin
    IniFile := TIniFile.Create(PrivateProfileFileName_);
    try
      for i := 0 to High(A) do
        A[i] := IniFile.ReadInteger(Key, IntToStr(i),-MaxInt);
    finally
      IniFile.Free;
    end {try};
  end {GetIntegerArray};


(****************)
(* LastFileName *)
(****************)

  function LastFileName;
    var
      IniFile : TIniFile;
  begin
    IniFile := TIniFile.Create(PrivateProfileFileName_);
    try
      Result := IniFile.ReadString(ProfileSectionStrings[psLastFiles],Key,'');
    finally
      IniFile.Free;
    end {try};
  end {LastFileName};


(*******************)
(* SetIntegerArray *)
(*******************)

  procedure SetIntegerArray(const Key : string; const A : array of integer);
    var
      IniFile : TIniFile;
      i : integer;
  begin
    IniFile := TIniFile.Create(PrivateProfileFileName_);
    try
      for i := 0 to High(A) do
        IniFile.WriteInteger(Key, IntToStr(i), A[i]);
    finally
      IniFile.Free;
    end {try};
  end {SetIntegerArray};


(*******************)
(* SetLastFileName *)
(*******************)

  procedure SetLastFileName;
    var
      IniFile : TIniFile;
  begin
    IniFile := TIniFile.Create(PrivateProfileFileName_);
    try
      IniFile.WriteString(ProfileSectionStrings[psLastFiles],Key,Name);
    finally
      IniFile.Free;
    end {try};
  end {SetLastFileName};


{~b}
end.
