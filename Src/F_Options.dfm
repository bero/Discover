object FormOptions: TFormOptions
  Left = 552
  Top = 276
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 154
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 121
    Width = 394
    Height = 33
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 96
      Top = 6
      Width = 60
      Height = 20
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 238
      Top = 6
      Width = 60
      Height = 20
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object CHKStayOnTop: TCheckBox
    Left = 6
    Top = 90
    Width = 161
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Stay on Top'
    TabOrder = 1
  end
  object CHKNoDisplaySourceLessUnits: TCheckBox
    Left = 6
    Top = 64
    Width = 325
    Height = 14
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 
      'Don'#39't display UNITS without accessible source or without routine' +
      's'
    TabOrder = 2
  end
  object CHKLoadState: TCheckBox
    Left = 6
    Top = 38
    Width = 232
    Height = 14
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'At startup, reload the last saved state'
    TabOrder = 3
  end
  object CHKSaveState: TCheckBox
    Left = 6
    Top = 13
    Width = 283
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'At shutdown, save then current state'
    TabOrder = 4
  end
end
