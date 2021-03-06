object FormMain: TFormMain
  Left = 394
  Top = 242
  Caption = 'Discover'
  ClientHeight = 387
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 368
    Width = 490
    Height = 19
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
    SizeGrip = False
    OnResize = StatusBarResize
  end
  object PNLMain: TPanel
    Left = 0
    Top = 0
    Width = 490
    Height = 368
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter: TSplitter
      Left = 218
      Top = 0
      Height = 368
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Color = clBtnFace
      ParentColor = False
      ExplicitHeight = 372
    end
    object PNLLeft: TPanel
      Left = 0
      Top = 0
      Width = 218
      Height = 368
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object PCLeft: TPageControl
        Left = 0
        Top = 0
        Width = 218
        Height = 368
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        ActivePage = TSOverView
        Align = alClient
        TabOrder = 0
        OnChange = PCLeftChange
        object TSSummary: TTabSheet
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Summary'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object MEMOSummary: TMemo
            Left = 0
            Top = 0
            Width = 212
            Height = 347
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
        object TSUnits: TTabSheet
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Units'
          object LBUnits: TListBox
            Left = 0
            Top = 14
            Width = 210
            Height = 326
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Style = lbOwnerDrawFixed
            Align = alClient
            Ctl3D = False
            MultiSelect = True
            ParentCtl3D = False
            PopupMenu = PUUnits
            TabOrder = 0
            OnClick = LBUnitsClick
            OnDrawItem = LBUnitsDrawItem
            OnKeyPress = LBUnitsKeyPress
          end
          object HCUnits: THeaderControl
            Left = 0
            Top = 0
            Width = 210
            Height = 14
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Sections = <
              item
                ImageIndex = -1
                Text = 'Units'
                Width = 40
              end
              item
                ImageIndex = -1
                Text = 'Size (bytes)'
                Width = 40
              end
              item
                ImageIndex = -1
                Text = 'R.Qty'
                Width = 40
              end
              item
                ImageIndex = -1
                Text = 'R.0%'
                Width = 40
              end
              item
                ImageIndex = -1
                Text = 'R.100%'
                Width = 40
              end
              item
                ImageIndex = -1
                Text = 'Coverage'
                Width = 40
              end>
            OnSectionClick = HCUnitsSectionClick
            OnSectionResize = HCUnitsSectionResize
            OnSectionTrack = HCSectionTrack
            ExplicitWidth = 212
          end
        end
        object TSRoutines: TTabSheet
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Routines'
          object HCRoutines: THeaderControl
            Left = 0
            Top = 0
            Width = 210
            Height = 14
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Sections = <
              item
                ImageIndex = -1
                Text = 'Name'
                Width = 40
              end
              item
                ImageIndex = -1
                Text = 'Unit'
                Width = 40
              end
              item
                ImageIndex = -1
                Text = 'Points'
                Width = 40
              end
              item
                ImageIndex = -1
                Text = 'Coverage'
                Width = 40
              end>
            OnSectionClick = HCRoutinesSectionClick
            OnSectionTrack = HCSectionTrack
            OnResize = HCRoutinesResize
            ExplicitWidth = 212
          end
          object LBRoutines: TListBox
            Left = 0
            Top = 14
            Width = 210
            Height = 326
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Style = lbOwnerDrawFixed
            Align = alClient
            Ctl3D = False
            MultiSelect = True
            ParentCtl3D = False
            PopupMenu = PURoutines
            TabOrder = 1
            OnClick = LBRoutinesClick
            OnDrawItem = LBRoutinesDrawItem
            OnKeyPress = LBRoutinesKeyPress
            OnMouseDown = LBRoutinesMouseDown
          end
        end
        object TSOverView: TTabSheet
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Caption = 'Overview'
          object PBOverView: TPaintBox
            Left = 0
            Top = 0
            Width = 210
            Height = 316
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alClient
            ParentShowHint = False
            ShowHint = True
            OnMouseUp = PBOverViewMouseUp
            OnPaint = PBOverViewPaint
            ExplicitWidth = 212
            ExplicitHeight = 323
          end
          object PBLegend: TPaintBox
            Left = 0
            Top = 316
            Width = 210
            Height = 24
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alBottom
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -8
            Font.Name = 'Small Fonts'
            Font.Style = []
            ParentFont = False
            OnPaint = PBLegendPaint
            ExplicitTop = 323
            ExplicitWidth = 212
          end
        end
      end
    end
    object PNLRight: TPanel
      Left = 221
      Top = 0
      Width = 269
      Height = 368
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 269
        Height = 368
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel6'
        TabOrder = 0
        object PCRight: TPageControl
          Left = 0
          Top = 0
          Width = 269
          Height = 368
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          ActivePage = TSCode
          Align = alClient
          TabOrder = 0
          OnChanging = PCRightChanging
          object TSCode: TTabSheet
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Source code'
            object LBFile: TListBox
              Left = 0
              Top = 0
              Width = 261
              Height = 340
              Margins.Left = 2
              Margins.Top = 2
              Margins.Right = 2
              Margins.Bottom = 2
              Style = lbOwnerDrawFixed
              Align = alClient
              Ctl3D = True
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Courier New'
              Font.Style = []
              ItemHeight = 17
              ParentCtl3D = False
              ParentFont = False
              PopupMenu = PUSource
              TabOrder = 0
              OnClick = LBFileClick
              OnDrawItem = LBFileDrawItem
              OnKeyPress = LBFileKeyPress
              OnMouseDown = LBFileMouseDown
            end
          end
        end
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 464
    object MMProject: TMenuItem
      Caption = '&Project'
      OnClick = MMProjectClick
      object MMProjectNew: TMenuItem
        Caption = '&Load...'
        OnClick = MMProjectNewClick
      end
      object MMProjectSettings: TMenuItem
        Caption = '&Settings...'
        OnClick = MMProjectSettingsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MMProjectSave: TMenuItem
        Caption = '&Save State...'
        ShortCut = 16467
        OnClick = MMProjectSaveClick
      end
      object MMProjectReload: TMenuItem
        Caption = '&Reload State...'
        ShortCut = 16460
        OnClick = MMProjectReloadClick
      end
      object MMProjectMerge: TMenuItem
        Caption = '&Merge State...'
        ShortCut = 16461
        OnClick = MMProjectMergeClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MMProjectClearState: TMenuItem
        Caption = 'Clear State...'
        OnClick = MMProjectClearStateClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MMExportData: TMenuItem
        Caption = '&Export Data...'
        OnClick = MMExportDataClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MMProjectExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MMProjectExitClick
      end
    end
    object MMApplication: TMenuItem
      Caption = '&Application'
      OnClick = MMApplicationClick
      object MMApplicationRun: TMenuItem
        Caption = '&Run'
        ShortCut = 120
        OnClick = MMApplicationRunClick
      end
      object MMApplicationTerminate: TMenuItem
        Caption = '&Terminate...'
        OnClick = MMApplicationTerminateClick
      end
    end
    object MMOptions: TMenuItem
      Caption = '&Options'
      OnClick = MMOptionsClick
    end
    object MMHelp: TMenuItem
      Caption = '&Help'
      OnClick = MMHelpClick
      object MMHelpHelp: TMenuItem
        Caption = '&Help'
        OnClick = MMHelpHelpClick
      end
      object MMHelpAbout: TMenuItem
        Caption = '&About...'
        OnClick = MMHelpAboutClick
      end
    end
  end
  object OpenDelphiProjectDialog: TOpenDialog
    Filter = '*.dpr|*.dpr|*.bdsproj|*.bdsproj'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Left = 488
    Top = 56
  end
  object PURoutines: TPopupMenu
    OnPopup = PURoutinesPopup
    Left = 384
    Top = 56
    object PURoutinesEnable: TMenuItem
      Caption = '&Foreground'
      ShortCut = 16454
      OnClick = PURoutinesEnableDisableClick
    end
    object PURoutinesDisable: TMenuItem
      Caption = '&Background'
      ShortCut = 16450
      OnClick = PURoutinesEnableDisableClick
    end
  end
  object PUSource: TPopupMenu
    Left = 536
    Top = 56
    object PUSourceNextGreen: TMenuItem
      Caption = 'Goto Next Green'
      ShortCut = 16455
      OnClick = PUSourceNextGreenClick
    end
    object PUSourceNextRed: TMenuItem
      Caption = 'Goto Next Red'
      ShortCut = 16466
      OnClick = PUSourceNextRedClick
    end
  end
  object OpenStateDialog: TOpenDialog
    Filter = '*.xxx|*.xxx'
    Left = 448
    Top = 56
  end
  object PUUnits: TPopupMenu
    OnPopup = PUUnitsPopup
    Left = 416
    Top = 56
    object PUUnitsEnable: TMenuItem
      Caption = '&Foreground'
      ShortCut = 16454
      OnClick = PUUnitsEnableDisableClick
    end
    object PUUnitsDisable: TMenuItem
      Caption = '&Background'
      ShortCut = 16450
      OnClick = PUUnitsEnableDisableClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object PUUnitsSelectAll: TMenuItem
      Caption = 'Select &all'
      ShortCut = 16449
      OnClick = PUUnitsSelectAllClick
    end
    object PUUnitsSelectGroup: TMenuItem
      Caption = 'Select &directory'
      ShortCut = 16452
      OnClick = PUUnitsSelectGroupClick
    end
  end
  object OpenLibDialog: TOpenDialog
    Filter = '*.yyy|*.yyy'
    Left = 376
    Top = 112
  end
  object SaveLibDialog: TSaveDialog
    Filter = '*.yyy|*.yyy'
    Options = [ofOverwritePrompt, ofHideReadOnly]
    Left = 416
    Top = 112
  end
  object ILLibImages: TImageList
    Left = 336
  end
  object SaveStateDialog: TSaveDialog
    Filter = '*.dps|*.dps'
    Options = [ofOverwritePrompt, ofHideReadOnly]
    Left = 289
    Top = 36
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 289
    Top = 68
  end
  object TIMERResize: TTimer
    Enabled = False
    Interval = 150
    OnTimer = TIMERResizeTimer
    Left = 288
    Top = 99
  end
end
