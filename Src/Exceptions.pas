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
unit Exceptions;

interface
  uses
    SysUtils, ObjectPascalTokenizer;

type
  EParserExpect = class(Exception)
    LineNr, OffsetInLine : integer;
    FileName : string;
    constructor Create(const Tokens : TTokenSet; const aFileName : string;
      aLineNr, aOffsetInLine : integer);
  end;

implementation

(************************)
(* EParserExpect.Create *)
(************************)

constructor EParserExpect.Create;
  var
    TokenQty : integer;
    s : string;
    t : TToken;
begin
  TokenQty := 0;
  s := '';
  for t := Low(TToken) to High(TToken) do
    if t in Tokens then begin
      if TokenQty > 0 then
        s := s + ', ';
      s := s + TokenString(t);
      inc(TokenQty);
    end {if};
  inherited Create(s+' expected');
  LineNr := aLineNr;
  OffsetInLine := aOffsetInLine;
  FileName := aFileName;
end {EParserExpect.Create};


{~b}
end.
