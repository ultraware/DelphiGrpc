unit dTethering;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, IPPeerServer, System.Tether.Manager, System.Tether.AppProfile,
  System.Types,
  dBaseRecorder;

type
  TdmTethering = class(TDataModule)
    TetheringAppProfile1: TTetheringAppProfile;
    TetheringManager1: TTetheringManager;
    procedure TetheringManager1EndManagersDiscovery(const Sender: TObject;
      const ARemoteManagers: TTetheringManagerInfoList);
    procedure TetheringManager1EndProfilesDiscovery(const Sender: TObject;
      const ARemoteProfiles: TTetheringProfileInfoList);
    procedure TetheringManager1NewManager(const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    FServerIp: string;
    FOnServerIpChanged: TNotifyEvent;
  private
    FOnLog: TLogCallback;
    procedure Log(const aType: TLogType; const aData: string);
    procedure SetServerIp(const Value: string);
  public
    procedure EnableServer;
    procedure EnableClient;

    function ServerIps: TStringDynArray;

    property ServerIp: string read FServerIp write SetServerIp;
    property OnServerIpChanged: TNotifyEvent read FOnServerIpChanged write FOnServerIpChanged;

    property OnLog: TLogCallback read FOnLog write FOnLog;
  end;

var
  dmTethering: TdmTethering;

implementation

uses
  {$IFDEF WIN32}
  IdStack,
  IpHlpApi, IpTypes, Winapi.Windows, //fDragonControls3,
  {$ENDIF}
  FMX.DialogService.Async;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{$IFDEF WIN32}
procedure RetrieveLocalAdapterInformation(strings: TStrings; aOnlyConnected: Boolean; out aIpAdress: string);
var
  pAdapterInfo{, pTempAdapterInfo}: PIP_ADAPTER_INFO;
//  AdapterInfo: IP_ADAPTER_INFO;
  BufLen: Cardinal;
  Status: Cardinal;
  strMAC: String;
  i: Integer;
begin
  strings.Clear;

  BufLen:= sizeof(IP_ADAPTER_INFO);
//  pAdapterInfo:= @AdapterInfo;

//  Status:= GetAdaptersInfo(nil, BufLen);
  pAdapterInfo:= AllocMem(BufLen);
  try
    Status:= GetAdaptersInfo(pAdapterInfo, BufLen);

    if (Status <> ERROR_SUCCESS) then
      begin
        case Status of
          ERROR_NOT_SUPPORTED:
            strings.Add('GetAdaptersInfo is not supported by the operating ' +
                        'system running on the local computer.');
          ERROR_NO_DATA:
            strings.Add('No network adapter on the local computer.');
        else
            strings.Add('GetAdaptersInfo failed with error #' + IntToStr(Status));
        end;
        Dispose(pAdapterInfo);
        Exit;
      end;

    while (pAdapterInfo <> nil) do
      begin
        if aOnlyConnected then
        if (pAdapterInfo^.DhcpServer.IpAddress.S = '') or (pAdapterInfo^.DhcpServer.IpAddress.S = '0.0.0.0') then
        begin
          pAdapterInfo:= pAdapterInfo^.Next;
          Continue;
        end;

        strings.Add('Description: ' + pAdapterInfo^.Description);
        strings.Add('Name: ' + pAdapterInfo^.AdapterName);

        strMAC := '';
        for I := 0 to pAdapterInfo^.AddressLength - 1 do
            strMAC := strMAC + '-' + IntToHex(pAdapterInfo^.Address[I], 2);

        Delete(strMAC, 1, 1);
        strings.Add('MAC address: ' + strMAC);
        strings.Add('IP address: ' + pAdapterInfo^.IpAddressList.IpAddress.S);
        strings.Add('IP subnet mask: ' + pAdapterInfo^.IpAddressList.IpMask.S);
        strings.Add('Gateway: ' + pAdapterInfo^.GatewayList.IpAddress.S);
        strings.Add('DHCP enabled: ' + IntTOStr(pAdapterInfo^.DhcpEnabled));
        strings.Add('DHCP: ' + pAdapterInfo^.DhcpServer.IpAddress.S);
        strings.Add('Have WINS: ' + BoolToStr(pAdapterInfo^.HaveWins,True));
        strings.Add('Primary WINS: ' + pAdapterInfo^.PrimaryWinsServer.IpAddress.S);
        strings.Add('Secondary WINS: ' + pAdapterInfo^.SecondaryWinsServer.IpAddress.S);

        aIpAdress := string(pAdapterInfo^.IpAddressList.IpAddress.S);

//        pTempAdapterInfo := pAdapterInfo;
        pAdapterInfo:= pAdapterInfo^.Next;
//      if assigned(pAdapterInfo) then Dispose(pTempAdapterInfo);
    end;
  finally
    Dispose(pAdapterInfo);
  end;
end;
{$ENDIF}

procedure TdmTethering.DataModuleCreate(Sender: TObject);
begin
  TetheringManager1.Enabled := False;
  TetheringAppProfile1.Enabled := False;
end;

procedure TdmTethering.DataModuleDestroy(Sender: TObject);
begin
  TetheringManager1.CancelDiscoverManagers;
  TetheringManager1.Enabled := False;
  TetheringAppProfile1.Enabled := False;
end;

procedure TdmTethering.EnableClient;
begin
  {$IFDEF WIN32}
  FServerIp := 'localhost';
  {$ENDIF}

  TetheringManager1.Enabled := True;
  TetheringManager1.DiscoverManagers();
end;

procedure TdmTethering.EnableServer;
begin
  TetheringManager1.Enabled := False;
  TetheringManager1.Text    := 'TetheringManagerDemo' + '@' + FServerIp;
  Log(ltDebug, TetheringManager1.Text);
  TetheringManager1.Enabled := True;
end;

procedure TdmTethering.Log(const aType: TLogType; const aData: string);
begin
  if Assigned(FOnLog) then
    FOnLog(aType, aData)
end;

function TdmTethering.ServerIps: TStringDynArray;
var
  str: tstrings;
  s: string;
begin
  Result := nil;
  str := TStringlist.Create;
  try
    {$IFDEF WIN32}
    GStack.AddLocalAddressesToList(str);
    {$ENDIF}
    for s in str do
      Result := Result + [s];
  finally
    str.Free;
  end;
end;

procedure TdmTethering.SetServerIp(const Value: string);
begin
  FServerIp := Value;

  if TetheringManager1.Enabled then
  begin
    EnableServer;
  end;
end;

procedure TdmTethering.TetheringManager1EndManagersDiscovery(const Sender: TObject;
  const ARemoteManagers: TTetheringManagerInfoList);
var manager: TTetheringManagerInfo;
begin
  if ARemoteManagers = nil then Exit;

  for manager in ARemoteManagers do
  begin
    if manager.ManagerText.Contains('Demo') then
    begin
      Log(ltInfo, Format('Managers: %s, %s, %s, %s, %s, %d', [manager.ManagerIdentifier, manager.ManagerName, manager.ManagerText, manager.ConnectionString, manager.ManagerAdapters, manager.Version]));

      if manager.ManagerText.Contains('@') then
      begin
        FServerIp := manager.ManagerText.Split(['@'])[1];
        Log(ltInfo, 'Server ip = ' + FServerIp);

        if Assigned(OnServerIpChanged) then
          OnServerIpChanged(Self);
      end;
    end;
  end;
end;

procedure TdmTethering.TetheringManager1EndProfilesDiscovery(const Sender: TObject;
  const ARemoteProfiles: TTetheringProfileInfoList);
var prof: TTetheringProfileInfo;
begin
  if ARemoteProfiles = nil then Exit;

  for prof in ARemoteProfiles do
  begin
    Log(ltDebug, Format('Profiles: %s, %s, %s, %s, %s, %d', [prof.ManagerIdentifier, prof.ProfileIdentifier, prof.ProfileText, prof.ProfileGroup, prof.ProfileType, prof.ProfileVersion]));
    if prof.ProfileGroup = Self.TetheringAppProfile1.Group then
      TetheringAppProfile1.Connect(prof);
  end;
end;

procedure TdmTethering.TetheringManager1NewManager(const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
begin
  Log(ltDebug, format('New manager: %s, %s, %s, %s, %s', [AManagerInfo.ManagerIdentifier, AManagerInfo.ManagerName, AManagerInfo.ManagerText, AManagerInfo.ConnectionString, AManagerInfo.ManagerAdapters]));
end;

end.
