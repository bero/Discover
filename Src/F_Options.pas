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
unit F_Options;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PersistentForm, StdCtrls, Spin, ExtCtrls;

type
  TFormOptions = class(TPersistentForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    CHKStayOnTop: TCheckBox;
    CHKNoDisplaySourceLessUnits: TCheckBox;
    CHKLoadState: TCheckBox;
    CHKSaveState: TCheckBox;
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOptions: TFormOptions;

implementation

{$R *.DFM}
{~t}
(***************************)
(* TFormOptions.FormCreate *)
(***************************)

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  ReadFromProfile;
end {TFormOptions.FormCreate};


(*************************)
(* TFormOptions.FormHide *)
(*************************)

procedure TFormOptions.FormHide(Sender: TObject);
begin
  WriteToProfile;
end {TFormOptions.FormHide};


{~b}
end.
