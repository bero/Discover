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
unit F_Edit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormEdit = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FirstEditFocus : boolean;
    procedure CheckOnScreen;
    procedure WMCommand(var Msg : TWMCommand);
      message WM_Command;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}
{~t}
(***************************)
(* TFormEdit.CheckOnScreen *)
(***************************)

procedure TFormEdit.CheckOnScreen;
  {Fait en sorte que le dialogue soit complètement sur l'écran}
var
  TargetRect, WorkRect, R : TRect;
  d : integer;
begin
  TargetRect := BoundsRect;
  SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkRect, 0);
  IntersectRect(R, TargetRect, WorkRect);
  d := (TargetRect.Bottom-TargetRect.Top) - (R.Bottom - R.Top);
  if d > 0 then begin
    dec(TargetRect.Top, d);
    dec(TargetRect.Bottom, d);
  end {if};

  d := (TargetRect.Right-TargetRect.Left) - (R.Right - R.Left);
  if d > 0 then begin
    dec(TargetRect.Left, d);
    dec(TargetRect.Right, d);
  end {if};
  BoundsRect := TargetRect;
end {TFormEdit.CheckOnScreen};


(**********************)
(* TFormEdit.FormShow *)
(**********************)

procedure TFormEdit.FormShow(Sender: TObject);
  var
    P : TPoint;
begin
  GetCursorPos(P);
  Left := P.X;
  Top := P.Y;
  CheckOnScreen;
  FirstEditFocus := true;
end {TFormEdit.FormShow};


(***********************)
(* TFormEdit.WMCommand *)
(***********************)

procedure TFormEdit.WMCommand;
  begin
    if (Msg.Ctl = Edit1.Handle) and (Msg.NotifyCode = en_SetFocus) and FirstEditFocus then begin
      FirstEditFocus := false;
      Edit1.SelStart := Length(Edit1.Text);
      Edit1.SelLength := 0;
    end {if};
    Inherited;
end {TFormEdit.WMCommand};


{~b}
end.
