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
unit F_Export;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, PersistentForm;

type
  TFormExport = class(TPersistentForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BTNOk: TButton;
    Button4: TButton;
    Panel3: TPanel;
    CHKExportUnits: TCheckBox;
    GroupBox4: TGroupBox;
    EDITUnitFileName: TEdit;
    BTNUnitBrowse: TButton;
    GroupBox5: TGroupBox;
    EDITUnitFormatString: TEdit;
    BTNUnitDefault: TButton;
    Bevel1: TBevel;
    Panel4: TPanel;
    CHKExportRoutines: TCheckBox;
    GroupBox1: TGroupBox;
    BTNRoutineBrowse: TButton;
    GroupBox2: TGroupBox;
    EDITRoutineFormatString: TEdit;
    BTNRoutineDefault: TButton;
    EDITRoutineFileName: TEdit;
    ExportFileNameDialog: TSaveDialog;
    CHKStripPath: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure BTNBrowseClick(Sender: TObject);
    procedure BTNUnitDefaultClick(Sender: TObject);
    procedure BTNRoutineDefaultClick(Sender: TObject);
    procedure BTNOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormExport: TFormExport;

implementation
  uses
    Config;

{$R *.DFM}
{~t}
(******************************)
(* TFormExport.BTNBrowseClick *)
(******************************)

procedure TFormExport.BTNBrowseClick(Sender: TObject);
begin
  if ExecDialog(ExportFileNameDialog, Export_Key) then begin
    if Sender = BTNUnitBrowse then
      EDITUnitFileName.TExt := ExportFileNameDialog.FileName
    else
      EDITRoutineFileName.Text := ExportFileNameDialog.FileName
  end {if};

end {TFormExport.BTNBrowseClick};


(**************************)
(* TFormExport.BTNOkClick *)
(**************************)

procedure TFormExport.BTNOkClick(Sender: TObject);
begin
  if CHKExportUnits.Checked and (EDITUnitFileName.Text = '') then
    raise Exception.Create('No unit file name specified');

  if CHKExportRoutines.Checked and (EDITRoutineFileName.Text = '') then
    raise Exception.Create('No routine file name specified');
  ModalResult := mrOk;
end {TFormExport.BTNOkClick};


(**************************************)
(* TFormExport.BTNRoutineDefaultClick *)
(**************************************)

procedure TFormExport.BTNRoutineDefaultClick(Sender: TObject);
begin
  EDITRoutineFormatString.Text := '%0:s;%1:s;%2:s;%3:s';
end {TFormExport.BTNRoutineDefaultClick};


(***********************************)
(* TFormExport.BTNUnitDefaultClick *)
(***********************************)

procedure TFormExport.BTNUnitDefaultClick(Sender: TObject);
begin
  EDITUnitFormatString.Text := '%0:s;%1:s;%2:s;%3:s;%4:s;%5:s;%6:s';
end {TFormExport.BTNUnitDefaultClick};


(**************************)
(* TFormExport.FormCreate *)
(**************************)

procedure TFormExport.FormCreate(Sender: TObject);
begin
  ReadFromProfile;
  if EDITUnitFormatString.Text = '' then
    BTNUnitDefaultClick(nil);
  if EDITRoutineFormatString.Text = '' then
    BTNRoutineDefaultClick(nil);
end {TFormExport.FormCreate};


(************************)
(* TFormExport.FormHide *)
(************************)

procedure TFormExport.FormHide(Sender: TObject);
begin
  WriteToProfile;
end {TFormExport.FormHide};


{~b}
end.
