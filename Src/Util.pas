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
unit Util;

interface

  function GetFullFilePath(const FileName : string) : string;
  procedure Fatal(ErrorNr : integer);

implementation
  uses
    SysUtils, Globals;

{~t}
(*********)
(* Fatal *)
(*********)

procedure Fatal(ErrorNr : integer);
begin
  raise Exception.Create('Fatal '+IntToStr(ErrorNr));
end {Fatal};


(*******************)
(* GetFullFilePath *)
(*******************)

function GetFullFilePath(const FileName : string) : string;
  var
    t : string;
    i : integer;
begin
  if not FileExists(FileName) then begin
    with SearchPath_ do
      for i := 0 to pred(Count) do begin
        t := SearchPath_[i] + '\' + FileName;
        if FileExists(t) then begin
          Result := t;
          exit;
        end {if};
      end {for};
      Result := '';
  end else
    Result := ExpandFileName(FileName)
end {GetFullFilePath};


{~b}
end.
