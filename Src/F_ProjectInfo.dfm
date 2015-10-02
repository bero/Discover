object FormProjectInfo: TFormProjectInfo
  Left = 497
  Top = 318
  BorderStyle = bsDialog
  Caption = 'Project settings'
  ClientHeight = 237
  ClientWidth = 458
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 120
  TextHeight = 16
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 458
    Height = 73
    Align = alTop
    Caption = 'Application run parameters'
    TabOrder = 0
    object EDITRunParameters: TEdit
      Left = 8
      Top = 32
      Width = 441
      Height = 24
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 73
    Width = 458
    Height = 56
    Align = alTop
    Caption = 'Application startup directory'
    TabOrder = 1
    object BTNBrowse: TButton
      Left = 376
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Browse...'
      TabOrder = 0
      OnClick = BTNBrowseClick
    end
    object EDITStartupDirectory: TEdit
      Left = 8
      Top = 24
      Width = 361
      Height = 24
      TabOrder = 1
    end
  end
  object CHKRunMaximized: TCheckBox
    Left = 16
    Top = 152
    Width = 209
    Height = 17
    Caption = 'Run application maximized'
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 196
    Width = 458
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object Button2: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button3: TButton
      Left = 296
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
