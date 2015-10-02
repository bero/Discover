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
unit CommandLineHandling;

interface

function CheckCommandLineParams : boolean;

implementation
  uses
    Windows, SysUtils, Globals;

function CheckCommandLineParams : boolean;

  procedure Error(const s : string);
  begin
    MessageBox(HWnd(0), PChar(s), nil, MB_OK or MB_ICONERROR);
    Result := false;
  end {Error};

  function StripQuotes(const s : string) : string;
  begin
    if (s[1] = '"') and (s[length(s)] = '"') then
      Result := Copy(s, 2, length(s)-2)
    else
      Result := s;
  end {StripQuotes};

  procedure HandleOptionWithFileName(const Param : string; var s : string);
  begin
    if (length(Param) = 2) then
      exit;
    if (length(Param) < 4) or (Param[3] <> '=') then
      Error(Format('Illegal command line param: %s', [Param]))
    else begin
      s := StripQuotes(Copy(Param, 4, length(Param)));
      if (s <> '') and not FileExists(s) then
        Error(Format('File "%s" does not exist.',[s]));
    end {if};
  end {HandleOptionWithFileName};

  var
    i : integer;
    Param : string;
begin
  if (ParamCount > 0) and (LogFileEnabled_) then
    Writeln(LogFile_, GetCommandLine);
  Result := true;
  i := 1;
  while (i <= ParamCount) and Result do begin
    Param := ParamStr(i);
    

    if Param[1] = '-' then begin
      // This is a flag
      case Param[2] of
        'c', 'C' :
          CommandLineParams_.CloseWhenAppTerminated := true;
        's', 'S':
          CommandLineParams_.SaveStateOnAppTerminate := true;
        'm', 'M' :
          CommandLineParams_.RunMinimized := true;
        'x', 'X':
          CommandLineParams_.Merge := true;
        'r', 'R':
          CommandLineParams_.ReportWhenAppTerminated := true;

      else {case}
        Error(Format('Illegal command line switch %s', [Param]));
      end {case};
    end else begin
      // Assme this is a file name
      Param := StripQuotes(Param);
      if not FileExists(Param) then begin
        Error(Format('File "%s" not found.', [Param]));
      end else begin
        CommandLineParams_.FileName := Param;
        if ExtractFileExt(Param) = ProjectStateExtension then
          CommandLineParams_.Action := caDPS
        else
          CommandLineParams_.Action := caDPR;
      end {if};
    end {if};
    inc(i);
  end {while};
end {CheckCommandLineParams};

end.
