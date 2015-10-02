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
unit F_Splash;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls;

type
  TFormSplash = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  
const
  SplashScreenDelay_ = 3000;
var
  FormSplash_: TFormSplash;

implementation
  uses
    Version, Globals;

{$R *.DFM}

(**************************)
(* TFormSplash.FormCreate *)
(**************************)

procedure TFormSplash.FormCreate(Sender: TObject);
  const
    Z = 80;
  var
    Rgn : hRgn;
    s : string;
begin
  Width := Image1.Picture.Bitmap.Width;
  Height := Image1.Picture.Bitmap.Height;
  Top := (Screen.Height - Height) div 2;
  Left := (Screen.Width - Width) div 2;
  Rgn := CreateRoundRectRgn(0,0,Width,Height,Z,Z);
  SetWindowRgn(Handle, Rgn, True);
  s := Copyright;
  with Image1.Canvas do begin
    Font.Color := clLime;
    Font.Height := 16;
    Font.Name := 'Arial';
    Font.Style := [];
    SetBkMode(Handle, Transparent);
    TextOut(20, Height-TextHeight('8')-10, s);
    Pen.Width := 3;
    Pen.Color := clLime;
    Brush.Style := bsClear;
    RoundRect(0,0,ClientWidth-1, ClientHeight-1,Z,Z)
  end {with};

end {TFormSplash.FormCreate};


end.
