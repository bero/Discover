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
  Globals;
interface
  uses
    Windows, Objects, SysUtils, Classes, Graphics;

  const
    ApplicationName = 'Discover';
    MaxAccessMemFailures = 20;
    MaxLogInt = 10;
    LibraryExtension = '.dml';
    ProjectStateExtension = '.dps';
    ProjectInformationExtension = '.dpi';
    ExecutableExtension = '.exe';
    Copyright = #0169'1998-2010 cyamon software ';

    TAB = #9;
    CR = #13;
    LF = #10;

  type
    TLogInfos = record
      case Boolean of
        true : (A : array [1..MaxLogInt] of integer);
        false : ( R:
          record
            MaxThreads : integer;
            NotFoundThreads : integer;
            NotAccessibleContexts : integer;
            NotFoundPoint : integer;
            WantedBreakPoints : integer;
            UnSettedBreakPoints : integer;
            AccessMemFailures : integer;
            Crc : cardinal;
          end
        );
    end;

    TCommandLineAction = (caNoAction, caDPS, caDPR);
    TProgressEvent = procedure(PerCent : integer; const ActionString : string) of object;

  var
    ApplicationDir_ : array [0..255] of char;
    ApplicationHelpFileName_ : string;
    ApplicationRegFileName_ : string;
    PrivateProfileFileName_ : string;
    GlobalDefinedConditionnals_ : TStringList;
    SearchPath_ : TStringList;
    MainWindowHandle_ : THandle;
    LogFile_ : TextFile;
    LogFileEnabled_ : boolean;
    LogInfos_ : TLogInfos;
    CommandLineActionEnabled_ : boolean;
    CommandLineParams_ : record
      Action : TCommandLineAction;
      CloseWhenAppTerminated : boolean;
      ReportWhenAppTerminated : boolean;
      SaveStateOnAppTerminate : boolean;
      RunMinimized : boolean;
      Merge : boolean;
      FileName : string;
    end {record};


implementation
  uses
    Version;

(************)
(* InitUnit *)
(************)

procedure InitUnit;
  begin
    {Défaut: répertoire d'où le programme à été démarré}
    GetModuleFileName(hInstance,ApplicationDir_,SizeOf(ApplicationDir_));
    StrPCopy(ApplicationDir_,ExtractFilePath(StrPas(ApplicationDir_)));
    PrivateProfileFileName_ := StrPas(ApplicationDir_)+ApplicationName+'.cfg';
    ApplicationHelpFileName_ := StrPas(ApplicationDir_)+ApplicationName+'.chm';
    if not FileExists(ApplicationHelpFileName_) then
      ApplicationHelpFileName_ := '';
    GlobalDefinedConditionnals_ := TStringList.Create;
    SearchPath_ := TStringList.Create;
    LogFileEnabled_ := true;
    if LogFileEnabled_ then begin
      AssignFile(LogFile_, StrPas(ApplicationDir_)+ApplicationName+'.log');
      try
        Rewrite(LogFile_);
        Writeln(LogFile_,Format('%s Version %s',[ApplicationName,
          VersionStr]));
      except
        LogFileEnabled_ := false;
      end {try};
    end {if};
    FillChar(LogInfos_, SizeOf(LogInfos_), 0);
    FillChar(CommandLineParams_, SizeOf(CommandLineParams_), 0);
    CommandLineActionEnabled_ := false;
end {InitUnit};
{$R-}
{$O-}
initialization
  InitUnit;
finalization
  SearchPath_.Free;
  GlobalDefinedConditionnals_.Free;
end.
