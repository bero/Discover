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
unit F_About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TFormAbout = class(TForm)
    Button1: TButton;
    LBLText: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout_: TFormAbout;

implementation
  uses
    Version, Globals;

{$R *.DFM}
{~t}
(*************************)
(* TFormAbout.FormCreate *)
(*************************)

procedure TFormAbout.FormCreate(Sender: TObject);
  var
    S : string;
begin
  Caption := 'About ' + Globals.ApplicationName;
  s := Globals.ApplicationName + CR + LF + 'Version ' + VersionStr + CR + LF +
    Copyright + CR + LF;
  LBLText.Caption := s;
end {TFormAbout.FormCreate};


{~b}
end.
