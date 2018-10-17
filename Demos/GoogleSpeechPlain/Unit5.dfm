object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'Form5'
  ClientHeight = 316
  ClientWidth = 649
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    649
    316)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 137
    Height = 25
    Caption = 'Translate'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 5
    Width = 633
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'audio.raw'
    TextHint = '<Audio file>'
  end
  object Memo1: TMemo
    Left = 8
    Top = 63
    Width = 633
    Height = 245
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
end
