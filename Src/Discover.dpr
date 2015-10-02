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
program Discover;

{$R *.dres}

uses
  Forms,
  Main in 'Main.pas' {FormMain},
  CodeParser in 'CodeParser.pas',
  ObjectPascalTokenizer in 'ObjectPascalTokenizer.pas',
  DataBase in 'DataBase.pas',
  Exceptions in 'Exceptions.pas',
  Util in 'Util.pas',
  MapFile in 'MapFile.pas',
  Process in 'Process.pas',
  Version in 'Version.pas',
  PersistentForm in 'PersistentForm.pas',
  F_Options in 'F_Options.pas' {FormOptions},
  F_Splash in 'F_Splash.pas' {FormSplash},
  Globals in 'Globals.pas',
  F_Edit in 'F_Edit.pas' {FormEdit},
  F_Export in 'F_Export.pas' {FormExport},
  ProjectInfo in 'ProjectInfo.pas',
  F_ProjectInfo in 'F_ProjectInfo.pas' {FormProjectInfo},
  CommandLineHandling in 'CommandLineHandling.pas';

{$R *.RES}
begin
  if not CheckCommandLineParams then
    exit;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormOptions, FormOptions);
  Application.CreateForm(TFormExport, FormExport);
  Application.CreateForm(TFormProjectInfo, FormProjectInfo);
  if CommandLineParams_.Action = caNoAction then begin
    // Splash and load previous state
    FormSplash_ := TFormSplash.Create(Application);
    try
      FormSplash_.Show;
      FormSplash_.Update;
      // Make sure that we have seen the splash screen
      // Do other initializations
      if (FormOptions.CHKLoadState.Checked) then
        try
          FormMain.LoadLastSavedState;
        except
        end {try};
      FormMain.AdjustStayOnTop;
      if ParamCount = 0 then
        FormMain.Sleep(2000)
      else
        FormMain.Sleep(1000);
      FormSplash_.Close;
    finally
      FormSplash_.Free;
    end;
  end else begin
    if CommandLineParams_.RunMinimized then
      FormMain.WindowState := wsMinimized;
  end {if};

  if CommandLineParams_.Action <> caNoAction then begin
    CommandLineActionEnabled_ := true;
  end {if};
  Application.Run;
end.
