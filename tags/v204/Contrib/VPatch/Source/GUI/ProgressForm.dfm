object frmProg: TfrmProg
  Left = 328
  Top = 266
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Progress...'
  ClientHeight = 193
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 8
    Top = 8
    Width = 369
    Height = 17
    AutoSize = False
    Transparent = True
  end
  object lblSize: TLabel
    Left = 152
    Top = 8
    Width = 225
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Transparent = True
  end
  object lblFile: TLabel
    Left = 8
    Top = 48
    Width = 369
    Height = 17
    AutoSize = False
    Caption = '(filename)'
  end
  object lblNewFile: TLabel
    Left = 8
    Top = 96
    Width = 369
    Height = 17
    AutoSize = False
    Caption = '(filename)'
  end
  object lblTotal: TLabel
    Left = 8
    Top = 144
    Width = 369
    Height = 17
    AutoSize = False
    Caption = 'Total progress'
  end
  object shpFull: TShape
    Left = 8
    Top = 24
    Width = 369
    Height = 17
    Brush.Color = clGray
  end
  object shpLeft: TShape
    Left = 8
    Top = 24
    Width = 369
    Height = 17
    Brush.Color = clRed
  end
  object prgFile: TProgressBar
    Left = 8
    Top = 64
    Width = 369
    Height = 25
    Min = 0
    Max = 100
    TabOrder = 0
  end
  object prgNewFile: TProgressBar
    Left = 8
    Top = 112
    Width = 369
    Height = 25
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object prgAll: TProgressBar
    Left = 8
    Top = 160
    Width = 369
    Height = 25
    Min = 0
    Max = 100
    TabOrder = 2
  end
end
