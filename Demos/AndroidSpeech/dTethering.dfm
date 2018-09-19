object dmTethering: TdmTethering
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 242
  Width = 336
  object TetheringAppProfile1: TTetheringAppProfile
    Manager = TetheringManager1
    Text = 'TetheringAppProfile1'
    Group = 'demo'
    Actions = <>
    Resources = <
      item
        Name = 'test'
        IsPublic = True
      end>
    Left = 184
    Top = 58
  end
  object TetheringManager1: TTetheringManager
    OnEndManagersDiscovery = TetheringManager1EndManagersDiscovery
    OnEndProfilesDiscovery = TetheringManager1EndProfilesDiscovery
    OnNewManager = TetheringManager1NewManager
    Password = 'test'
    Text = 'TetheringManagerDemo'
    Enabled = False
    AllowedAdapters = 'Network'
    Left = 64
    Top = 58
  end
end
