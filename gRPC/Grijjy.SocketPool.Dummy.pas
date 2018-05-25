unit Grijjy.SocketPool.Dummy;

{ Dummy socketpool implementation for Mobile clients }

interface

uses
  System.SysUtils;

type
  TgoSocketConnection = class;
  TgoClientSocketManager = class;

  { Callback events }
  TgoSocketNotifyEvent = procedure of object;
  TgoSocketDataEvent = procedure(const ABuffer: Pointer; const ASize: Integer)
    of object;
  TgoSocketConnectionEvent = procedure(aConnection: TgoSocketConnection) of object;

  { Internal connection state }
  TgoConnectionState = (Disconnected, Disconnecting, Connected);

  TgoSocketConnection = class
  private
    FOnConnected: TgoSocketNotifyEvent;
    FOnDisconnected: TgoSocketNotifyEvent;
    FOnRecv: TgoSocketDataEvent;
    FALPN: Boolean;
    FSSL: Boolean;
    FPrivateKey: TBytes;
    FCertificate: TBytes;
    FState: TgoConnectionState;
    FOnAccept: TgoSocketConnectionEvent;
    FPort: Word;
    FHostname: String;
    function GetShutdown: Boolean;
  public
    constructor Create(const AOwner: TgoClientSocketManager; const AHostname: String; const APort: Word);

    function  Connect(const AUseNagle: Boolean = True): Boolean;
    function  Accept(const AUseNagle: Boolean = True): Boolean;
    procedure Disconnect;
    procedure CloseAccept;

    function Send(const ABuffer: Pointer; const ASize: Integer): Boolean; overload;
    function Send(const ABytes: TBytes): Boolean; overload;

    property SSL: Boolean read FSSL write FSSL;
    property ALPN: Boolean read FALPN write FALPN;
    property Certificate: TBytes read FCertificate write FCertificate;
    property PrivateKey: TBytes read FPrivateKey write FPrivateKey;

    property Hostname: String read FHostname;
    property Port: Word read FPort;
    property State: TgoConnectionState read FState write FState;
    property Shutdown: Boolean read GetShutdown;

    property OnConnected: TgoSocketNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TgoSocketNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnRecv: TgoSocketDataEvent read FOnRecv write FOnRecv;
    property OnAccept: TgoSocketConnectionEvent read FOnAccept write FOnAccept;
  end;

  TgoClientSocketManager = class
  public
    function Request(const AHostname: String; const APort: Word): TgoSocketConnection;
    procedure Release(const AConnection: TgoSocketConnection);
  end;

implementation

{ TgoClientSocketManager }

procedure TgoClientSocketManager.Release(const AConnection: TgoSocketConnection);
begin
  Assert(False);
end;

function TgoClientSocketManager.Request(const AHostname: String; const APort: Word): TgoSocketConnection;
begin
  Assert(False);
  Result := nil;
end;

{ TgoSocketConnection }

function TgoSocketConnection.Accept(const AUseNagle: Boolean): Boolean;
begin
  Assert(False);
  Result := False;
end;

procedure TgoSocketConnection.CloseAccept;
begin
  Assert(False);
end;

function TgoSocketConnection.Connect(const AUseNagle: Boolean): Boolean;
begin
  Assert(False);
  Result := False;
end;

constructor TgoSocketConnection.Create(const AOwner: TgoClientSocketManager; const AHostname: String;
  const APort: Word);
begin
  Assert(False);
end;

procedure TgoSocketConnection.Disconnect;
begin
  Assert(False);
end;

function TgoSocketConnection.GetShutdown: Boolean;
begin
  Assert(False);
  Result := False;
end;

function TgoSocketConnection.Send(const ABuffer: Pointer; const ASize: Integer): Boolean;
begin
  Assert(False);
  Result := False;
end;

function TgoSocketConnection.Send(const ABytes: TBytes): Boolean;
begin
  Assert(False);
  Result := False;
end;

end.
