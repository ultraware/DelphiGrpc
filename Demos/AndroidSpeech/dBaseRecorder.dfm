object dmBaseRecorder: TdmBaseRecorder
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object tmrFetch: TTimer
    Enabled = False
    Interval = 50
    OnTimer = tmrFetchTimer
    Left = 104
    Top = 48
  end
end
