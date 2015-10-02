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
unit PersistentForm;

interface
  uses
    Forms;

  type
    TPersistentForm = class(TForm)
    protected
      procedure ReadFromProfile;
      procedure WriteToProfile;
    end;

implementation
  uses
    IniFiles, Controls, Classes, SysUtils, StdCtrls, Spin, ExtCtrls, ComCtrls,
    Util, Globals;
{~t}
(***********************************)
(* TPersistentForm.ReadFromProfile *)
(***********************************)

procedure TPersistentForm.ReadFromProfile;
  var
    IniFile : TIniFile;

  procedure DoOneControl(aControl : TControl);

    procedure ReadItems(const Section : string; Items : TStrings);
      var
        i,Count : integer;
      begin
        Count := IniFile.ReadInteger(Section,'c',0);
        i := 0;
        while i < Count do begin
          Items.Add(Inifile.ReadString(Section, IntToStr(i), ''));
          inc(i);
        end {while};
    end {};

    begin
      if aControl is TEdit then
        with aControl as TEdit do begin
          Text := IniFile.ReadString(Self.Name, Name, Text);
        end {with}
      else if aControl is TSpinEdit then
        with aControl as TSpinEdit do begin
          Text := IniFile.ReadString(Self.Name, Name, Text);
        end {with}
      else if aControl is TCheckBox then
        with aControl as TCheckBox do begin
          Checked := IniFile.ReadBool(Self.Name, Name, Checked)
        end {with}
      else if aControl is TRadioButton then
        with aControl as TRadioButton do begin
          Checked := IniFile.ReadBool(Self.Name, Name, Checked)
        end {with}
      else if aControl is TListBox then
        with aControl as TListBox do begin
          ReadItems(Self.Name+'.'+Name, Items);
        end {with}
      else if aControl is TComboBox then
        with aControl as TComboBox do begin
          Text := IniFile.ReadString(Self.Name, Name, '');
        end {with}
      else if aControl is TRadioGroup then
        with aControl as TRadioGroup do begin
          ItemIndex := IniFile.ReadInteger(Self.Name, Name, 0);
        end {with}
      else if (aControl is TLabel) or (aControl is TButton) or
        (aControl is TTabSheet) or (aControl is TGroupBox) or
        (aControl is TPageControl) or (aControl is TNoteBook) or
        (aControl is TPaintBox) or (aControl is TPanel) or
        (aControl is TBevel)
        then
        {Do nothing}
      else
        Fatal(1329);
  end {DoOneControl};

  var
    i : integer;
  begin
    IniFile := TIniFile.Create(PrivateProfileFileName_);
    for i := 0 to pred(ComponentCount) do
      if (Components[i] is TControl) then
        DoOneControl(Components[i] as TControl);
    IniFile.Free;
end {TPersistentForm.ReadFromProfile};


(**********************************)
(* TPersistentForm.WriteToProfile *)
(**********************************)

procedure TPersistentForm.WriteToProfile;
  var
    IniFile : TIniFile;

  procedure DoOneControl(aControl : TControl);

    procedure WriteItems(const Section : string; Items : TStrings);
      var
        i : integer;
      begin
        i := 0;
        IniFile.WriteInteger(Section,'c',Items.Count);
        while i < Items.Count do begin
          Inifile.WriteString(Section, IntToStr(i), Items[i]);
          inc(i);
        end {while};
    end {};

    begin
      if aControl is TEdit then
        with aControl as TEdit do begin
          IniFile.WriteString(Self.Name, Name, Text);
        end {with}
      else if aControl is TSpinEdit then
        with aControl as TSpinEdit do begin
          IniFile.WriteString(Self.Name, Name, Text);
        end {with}
      else if aControl is TCheckBox then
        with aControl as TCheckBox do begin
          IniFile.WriteBool(Self.Name, Name, Checked)
        end {with}
      else if aControl is TRadioButton then
        with aControl as TRadioButton do begin
          IniFile.WriteBool(Self.Name, Name, Checked)
        end {with}
      else if aControl is TListBox then
        with aControl as TListBox do begin
           IniFile.EraseSection(Self.Name+'.'+Name);
           WriteItems(Self.Name+'.'+Name, Items);
        end {with}
      else if aControl is TComboBox then
        with aControl as TComboBox do begin
          IniFile.WriteString(Self.Name, Name, Text);
        end {with}
      else if aControl is TRadioGroup then
        with aControl as TRadioGroup do begin
          IniFile.WriteInteger(Self.Name, Name, ItemIndex);
        end {with}
      else if (aControl is TLabel) or (aControl is TButton) or
        (aControl is TTabSheet) or (aControl is TPageControl) or
        (aControl is TGroupBox) or (aControl is TNoteBook) or
        (aControl is TPaintBox) or (aControl is TPanel) or
        (aControl is TBevel)
      then
        {Do nothing}
      else
        Fatal(1329);
  end {DoOneControl};

  var
    i : integer;
  begin
    IniFile := TIniFile.Create(PrivateProfileFileName_);
    for i := 0 to pred(ComponentCount) do
      if (Components[i] is TControl) then
        DoOneControl(Components[i] as TControl);
    IniFile.Free;
end {TPersistentForm.WriteToProfile};


{~b}
end.
 
