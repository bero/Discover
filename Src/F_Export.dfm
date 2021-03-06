object FormExport: TFormExport
  Left = 276
  Top = 139
  BorderStyle = bsDialog
  Caption = 'Export data'
  ClientHeight = 317
  ClientWidth = 485
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
    Top = 284
    Width = 485
    Height = 33
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object BTNOk: TButton
      Left = 107
      Top = 6
      Width = 60
      Height = 20
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '&OK'
      Default = True
      TabOrder = 0
      OnClick = BTNOkClick
    end
    object Button4: TButton
      Left = 318
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
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 485
    Height = 284
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Bevel1: TBevel
      Left = 0
      Top = 157
      Width = 485
      Height = 3
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      Shape = bsTopLine
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 485
      Height = 33
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object CHKExportUnits: TCheckBox
        Left = 6
        Top = 13
        Width = 283
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Export units'#39' data'
        TabOrder = 0
      end
    end
    object GroupBox4: TGroupBox
      Left = 0
      Top = 33
      Width = 485
      Height = 52
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      Caption = 'File name'
      TabOrder = 1
      object EDITUnitFileName: TEdit
        Left = 6
        Top = 16
        Width = 392
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        TabOrder = 0
      end
      object BTNUnitBrowse: TButton
        Left = 403
        Top = 16
        Width = 60
        Height = 20
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Browse...'
        TabOrder = 1
        OnClick = BTNBrowseClick
      end
    end
    object GroupBox5: TGroupBox
      Left = 0
      Top = 85
      Width = 485
      Height = 72
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      Caption = 'Format string'
      TabOrder = 2
      object EDITUnitFormatString: TEdit
        Left = 6
        Top = 16
        Width = 392
        Height = 26
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object BTNUnitDefault: TButton
        Left = 403
        Top = 16
        Width = 60
        Height = 20
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Set default'
        TabOrder = 1
        OnClick = BTNUnitDefaultClick
      end
      object CHKStripPath: TCheckBox
        Left = 6
        Top = 45
        Width = 392
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Strip path'
        TabOrder = 2
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 160
      Width = 485
      Height = 33
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      object CHKExportRoutines: TCheckBox
        Left = 6
        Top = 13
        Width = 283
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Export routines'#39' data'
        TabOrder = 0
      end
    end
    object GroupBox1: TGroupBox
      Left = 0
      Top = 193
      Width = 485
      Height = 52
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      Caption = 'File name'
      TabOrder = 4
      object BTNRoutineBrowse: TButton
        Left = 403
        Top = 16
        Width = 60
        Height = 20
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Browse...'
        TabOrder = 0
        OnClick = BTNBrowseClick
      end
      object EDITRoutineFileName: TEdit
        Left = 6
        Top = 16
        Width = 392
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        TabOrder = 1
      end
    end
    object GroupBox2: TGroupBox
      Left = 0
      Top = 245
      Width = 485
      Height = 52
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      Caption = 'Format string'
      TabOrder = 5
      object EDITRoutineFormatString: TEdit
        Left = 6
        Top = 16
        Width = 392
        Height = 26
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object BTNRoutineDefault: TButton
        Left = 403
        Top = 16
        Width = 60
        Height = 20
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Set default'
        TabOrder = 1
        OnClick = BTNRoutineDefaultClick
      end
    end
  end
  object ExportFileNameDialog: TSaveDialog
    Filter = '*.txt|*.txt|*.*|*.*'
    Left = 32
    Top = 355
  end
end
