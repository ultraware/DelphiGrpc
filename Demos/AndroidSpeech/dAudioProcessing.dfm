object dmAudioProcessing: TdmAudioProcessing
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 410
  Width = 672
  object Timer1: TTimer
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 320
    Top = 88
  end
end
