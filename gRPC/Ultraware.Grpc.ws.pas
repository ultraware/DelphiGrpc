unit Ultraware.Grpc.Ws;

interface

//{$DEFINE LOGGING}

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Grijjy.ProtocolBuffers, Grijjy.Http,
  Ultraware.Grpc, Grijjy.Http2,
  sgcWebSocket_Classes, sgcWebSocket_Client, sgcWebSocket_Server;

type
  {$WARN UNSUPPORTED_CONSTRUCT ERROR}

  TWSRequest = record
  public
    [Serialize(1)] id: uint64;
    [Serialize(2)] path: string;
    [Serialize(3)] close: Boolean;
    [Serialize(4)] size: uint64;
    [Serialize(5)] grpc: TBytes;
  public
    function  Serialize: TBytes;
    procedure Deserialize(const aStream: TBytes);
  end;

  TWSResponse = record
  public
    [Serialize(1)] id: uint64;
    [Serialize(2)] close: Boolean;
    [Serialize(3)] size: uint64;
    [Serialize(4)] grpc: TBytes;
  public
    function  Serialize: TBytes;
    procedure Deserialize(const aStream: TBytes);
  end;

  TGrpcWsClient = class(TInterfacedObject, IGrpcClient)
  protected
    FRequestCounter: Integer;
    FConnected: TBoolean;
    FRequests: TDictionary<Integer, TGrpcCallback>;
    procedure Connect(const aHost: string; aPort: Integer);
    procedure ProcessResponse(const aResponse: TWSResponse);
  protected
    FWSClient: TsgcWSClient;
    FHost: string;
    FPort: Integer;
    FConnectTimeout: Integer;
    FReceiveTimeout: Integer;
    procedure OnWSConnected(aConnection: TsgcWSConnection);
    procedure OnWSDisConnected(aConnection: TsgcWSConnection; aCode: Integer);
    procedure OnWSBinary(aConnection: TsgcWSConnection; const aStream: TMemoryStream);
  protected
    {IGrpcClient}
    function DoRequest(const aSendData: TBytes; const aGrpcPath: string; out aReceivedData: TBytes): Boolean; overload;
    function DoRequest(const aSendData: TBytes; const aGrpcPath: string; const aReceiveCallback: TGrpcCallback): IGrpcStream; overload;
  public
    procedure   AfterConstruction; override;
    constructor Create(const aHost: string; aPort: Integer; aSSL: Boolean = False; aConnectTimeout: Integer = TIMEOUT_CONNECT; aReceiveTimeout: Integer = TIMEOUT_RECV);
    destructor  Destroy; override;

    function IsActive: Boolean;
    property Host: string  read FHost;
    property Port: Integer read FPort;
  end;

  TGrpcWsStream = class(TInterfacedObject, IGrpcStream)
  protected
    FWSClient: TsgcWSClient;
    FRequestId: Integer;
    FRequestClosed: TBoolean;
    FResponseData: TBytes;
    FResponseClosed: TBoolean;
    FPath: string;
    FReceiveCallback: TGrpcCallback;
  protected
    {IGrpcStream}
    procedure SendData(const aStream: TBytes; aClose: Boolean = False);
    function  CloseAndRecvData(out aResult: TBytes): Boolean;
    function  Recv(out aResult: TBytes; aWaitTimeout: Integer = TIMEOUT_RECV): TGrpcWaitResult;
    procedure DoCloseSend();

    function IsResponseClosed: Boolean;
    function IsRequestClosed: Boolean;
  public
    constructor Create(const aClient: TGrpcWsClient; const aPath: string; aRequestId: Integer; const aReceiveCallback: TGrpcCallback);
    destructor  Destroy; override;
  end;

  TGrpcWsServer = class
  protected
    FWSServer: TsgcWSServer;
    FPort: Integer;
    class var FRegisteredImpl: TDictionary<string, TBaseGrpcImplementationClass>;
    class var FActiveCalls: TDictionary<string, IRequestThread>;
    procedure Start(const aHost: string; aPort: Integer);
    procedure OnWSBinary(aConnection: TsgcWSConnection; const aStream: TMemoryStream);
    procedure WSServerConnect(Connection: TsgcWSConnection);
    procedure WSServerDisconnect(Connection: TsgcWSConnection; Code: Integer);
    procedure WSServerError(Connection: TsgcWSConnection; const Error: string);
    procedure WSServerMessage(Connection: TsgcWSConnection; const Text: string);
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterImplementation(const aPath: string; aImplementation: TBaseGrpcImplementationClass);

    procedure   AfterConstruction; override;
    constructor Create(const aHost: string; aPort: Integer; aSSL: Boolean = False; aConnectTimeout: Integer = TIMEOUT_CONNECT; aReceiveTimeout: Integer = TIMEOUT_RECV);
    destructor  Destroy; override;

    procedure StartListen;
    procedure Stop;
  end;

implementation

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  System.SyncObjs, System.DateUtils, FMX.Forms;

function BytesToStringRaw(const AValue: TBytes; aSize: Integer = -1): string;
var
  i: Integer;
begin
  for i := 0 to High(AValue) do
  begin
    if (AValue[i] = 0) and (aSize < 0) then Exit;

    if (AValue[i] < 33) or
       ( (AValue[i] > 126) and
         (AValue[i] < 161) )
    then
      Result := Result + '#' + IntToStr(AValue[i])
    else
      Result := Result + Char(AValue[i]);

    if (aSize > 0) and (i > aSize) then Break;
  end;
end;

{ TWSRequest }

procedure TWSRequest.Deserialize(const aStream: TBytes);
begin
  TgoProtocolBuffer.Deserialize(Self, aStream);
  Assert(Self.size = Length(Self.grpc));
end;

function TWSRequest.Serialize: TBytes;
begin
  Self.size := Length(Self.grpc);
  Result := TgoProtocolBuffer.Serialize(Self);
end;

{ TWSResponse }

procedure TWSResponse.Deserialize(const aStream: TBytes);
begin
  TgoProtocolBuffer.Deserialize(Self, aStream);
  Assert(Self.size = Length(Self.grpc));
end;

function TWSResponse.Serialize: TBytes;
begin
  Self.size := Length(Self.grpc);
  Result := TgoProtocolBuffer.Serialize(Self);
end;

{ TBaseGrpcWsService }

procedure TGrpcWsClient.AfterConstruction;
begin
  inherited;
  FWSClient  := TsgcWSClient.Create(nil);
  FConnected := TBoolean.Create;
  FRequests  := TDictionary<Integer, TGrpcCallback>.Create;
end;

constructor TGrpcWsClient.Create(const aHost: string; aPort: Integer; aSSL: Boolean; aConnectTimeout,
  aReceiveTimeout: Integer);
begin
  FHost := aHost;
  FPort := aPort;
  FReceiveTimeout := aReceiveTimeout;
  FConnectTimeout := aConnectTimeout;
end;

destructor TGrpcWsClient.Destroy;
begin
  FRequests.Free;
  FConnected.Free;
  FWSClient.Free;
end;

function TGrpcWsClient.DoRequest(const aSendData: TBytes;
  const aGrpcPath: string;
  const aReceiveCallback: TGrpcCallback): IGrpcStream;
var
  packet: TGrpcPacket;
  strm: TMemoryStream;
  wsreq: TWSRequest;
  b: TBytes;
begin
  if not FConnected.Value then
  begin
    Connect(Host, Port);
    FConnected.Wait(FConnectTimeout, True {WS needs synchronize});
  end;
  if not FConnected.Value then
    Exit;

  strm := TMemoryStream.Create;
  try
    wsreq.id   := TInterlocked.Increment(FRequestCounter);
    wsreq.path := aGrpcPath;
    packet.Create(aSendData);
    wsreq.grpc := packet.Serialize();
    Result := TGrpcWsStream.Create(Self, aGrpcPath, wsreq.id, aReceiveCallback);

    b := wsreq.Serialize();
    strm.Write(b, Length(b));
    strm.Position := 0;
    FWSClient.WriteData(strm);
  finally
    strm.Free;
  end;
end;

function TGrpcWsClient.IsActive: Boolean;
begin
  Result := Self.FWSClient.Active;
end;

procedure TGrpcWsClient.OnWSBinary(aConnection: TsgcWSConnection; const aStream: TMemoryStream);
var
  wsresp: TWSResponse;
  b: TBytes;
begin
  aStream.Position := 0;
  SetLength(b, aStream.Size);
  aStream.Read(b, aStream.Size);
  wsresp.Deserialize(b);
  ProcessResponse(wsresp);
end;

procedure TGrpcWsClient.OnWSConnected(aConnection: TsgcWSConnection);
begin
  FConnected.Value := True;
end;

procedure TGrpcWsClient.OnWSDisConnected(aConnection: TsgcWSConnection; aCode: Integer);
begin
  FConnected.Value := False;
  FWSClient.Active := False;
  FRequests.Clear;
end;

procedure TGrpcWsClient.Connect(const aHost: string; aPort: Integer);
begin
  FWSClient.UseNagle := False;
  FWSClient.OnConnect := OnWSConnected;
  FWSClient.OnDisconnect := OnWSDisConnected;
  FWSClient.OnBinary := OnWSBinary;

  FWSClient.Host := aHost;
  FWSClient.Port := aPort;
  FWSClient.Active := True;
end;

function TGrpcWsClient.DoRequest(const aSendData: TBytes; const aGrpcPath: string;
  out aReceivedData: TBytes): Boolean;
var
  packet: TGrpcPacket;
  strm: TMemoryStream;
  wsreq: TWSRequest;
  b: TBytes;
  wait: TBoolean;
begin
  aReceivedData := nil;
  Result := False;
  if not FConnected.Value then
  begin
    Connect(Host, Port);
    FConnected.Wait(FConnectTimeout, True {ws uses synchronize :( });
  end;
  if not FConnected.Value then
    Exit;
  Assert(FWSClient.Active);

  strm := TMemoryStream.Create;
  wait := TBoolean.Create;
  try
    wsreq.id   := TInterlocked.Increment(FRequestCounter);
    wsreq.path := aGrpcPath;
    packet.Create(aSendData);
    wsreq.grpc := packet.Serialize();

    FRequests.Add(wsreq.id,
      procedure(const aStream: TBytes; aIsStreamClosed: Boolean)
      begin
        wait.Value := True;
        b := aStream;
      end);

    b := wsreq.Serialize();
    strm.Write(b, Length(b));
    strm.Position := 0;
    FWSClient.WriteData(strm);
    b := nil;

    if wait.Wait(FReceiveTimeout, True {ws uses synchronize :( }) then
    begin
      aReceivedData := b;
      Result := True;
    end
    else
      raise Exception.CreateFmt('No connection or response from server', []);
  finally
    FRequests.Remove(wsreq.id);
    wait.Free;
    strm.Free;
  end;
end;

procedure TGrpcWsClient.ProcessResponse(const aResponse: TWSResponse);
var
  packet: TGrpcPacket;
  callback: TGrpcCallback;
begin
  if packet.TryDeserialize(aResponse.grpc) then
  begin
    if FRequests.TryGetValue(aResponse.id, callback) then
    begin
      callback(packet.Data, aResponse.close);
    end
    else
      Assert(False);
  end
  else
    Assert(False);
end;

{ TBaseGrpcWsServer }

procedure TGrpcWsServer.AfterConstruction;
begin
  inherited;
  FWSServer := TsgcWSServer.Create(nil);
end;

constructor TGrpcWsServer.Create(const aHost: string; aPort: Integer;
  aSSL: Boolean; aConnectTimeout, aReceiveTimeout: Integer);
begin
  FPort := aPort;
end;

destructor TGrpcWsServer.Destroy;
begin
  FWSServer.Free;
end;

procedure TGrpcWsServer.OnWSBinary(aConnection: TsgcWSConnection; const aStream: TMemoryStream);
var
  wsreq: TWSRequest;
  wsresp: TWSResponse;
  b: TBytes;
  t: IRequestThread;
  st: TGrpcStreamCallThread;
  impl: TBaseGrpcImplementationClass;
  packet: TGrpcPacket;
  guid: string;
begin
  aStream.Position := 0;
  SetLength(b, aStream.Size);
  aStream.Read(b, aStream.Size);
  {$IFDEF WIN32}
  {$IFDEF LOGGING}
  OutputDebugString(PChar('>> ' + BytesToStringRaw(b)));
  {$ENDIF}
  {$ENDIF}
  wsreq.Deserialize(b);
  if wsreq.path <> '' then
  begin
    {$IFDEF LOGGING}
    Writeln(Format('>> %s: %d bytes', [wsreq.path, Length(wsreq.grpc)]));
    {$ENDIF}

    //more data for previous/running stream?
    if TGrpcWsServer.FActiveCalls.TryGetValue(wsreq.path + '_' + aConnection.Guid + '_' + wsreq.id.ToString, t) then //todo: make threadsafe
    begin
      st := t as TGrpcStreamCallThread;
      if packet.TryDeserialize(wsreq.grpc) then
        st.AddReceivedData(packet.Data);
      if wsreq.close then
        st.Close;
      Exit;
    end;

    if packet.TryDeserialize(wsreq.grpc) then
    for impl in TGrpcWsServer.FRegisteredImpl.Values do
    begin
      guid := aConnection.Guid;

      if impl.HandleGRPC(wsreq.path, packet.data,
        procedure(const aData: TBytes; aIsStreamClosed: Boolean)
        var
          packet: TGrpcPacket;
          strm: TMemoryStream;
        begin
          {$IFDEF LOGGING}
          Writeln(Format('<< %s: %d bytes', [wsreq.path, Length(aData)]));
          {$ENDIF}

          wsresp.id := wsreq.id;
          wsresp.close := aIsStreamClosed;
          packet.Create(aData);
          wsresp.grpc := packet.Serialize;

          strm := TMemoryStream.Create;
          try
            strm.Size := 0;
            b := wsresp.Serialize;
            {$IFDEF WIN32}
            {$IFDEF LOGGING}
            OutputDebugString(PChar('<< ' + BytesToStringRaw(b)));
            {$ENDIF}
            {$ENDIF}
            strm.Write(b, Length(b));
            strm.Position := 0;
            FWSServer.WriteData(guid, strm);
          finally
            strm.Free;
          end;
        end,
        procedure(const aError: Exception)
        begin
          {$IFDEF LOGGING}
          Writeln(Format('E! %s', [aError.Message]));
          {$ENDIF}
          aConnection.Close;
        end,
        t)
      then
        Break;
    end;

    if t <> nil then
      TGrpcWsServer.FActiveCalls.AddOrSetValue(wsreq.path + '_' + aConnection.Guid + '_' + wsreq.id.ToString, t);  //todo: make threadsafe
  end;
end;

class constructor TGrpcWsServer.Create;
begin
  FRegisteredImpl := TDictionary<string, TBaseGrpcImplementationClass>.Create;
  FActiveCalls    := TDictionary<string, IRequestThread>.Create;
end;

class destructor TGrpcWsServer.Destroy;
begin
  FRegisteredImpl.Free;
  FActiveCalls.Free;
end;

class procedure TGrpcWsServer.RegisterImplementation(const aPath: string;
  aImplementation: TBaseGrpcImplementationClass);
begin
  FRegisteredImpl.AddOrSetValue(aPath, aImplementation);
end;

procedure TGrpcWsServer.Start(const aHost: string; aPort: Integer);
begin
  FWSServer.UseNagle := False;
  FWSServer.OnConnect := Self.WSServerConnect;
  FWSServer.OnDisconnect := WSServerDisconnect;
  FWSServer.OnMessage := WSServerMessage;
  FWSServer.OnError := WSServerError;
  FWSServer.OnBinary := OnWSBinary;

  FWSServer.ThreadPool := True;

  FWSServer.Port := aPort;
  FWSServer.Active := True;
end;

procedure TGrpcWsServer.StartListen;
begin
  Start('', FPort);
end;

procedure TGrpcWsServer.Stop;
begin
  FWSServer.Active := False;
end;

procedure TGrpcWsServer.WSServerConnect(Connection: TsgcWSConnection);
begin
  //
end;

procedure TGrpcWsServer.WSServerDisconnect(Connection: TsgcWSConnection; Code: Integer);
begin
  //
end;

procedure TGrpcWsServer.WSServerError(Connection: TsgcWSConnection; const Error: string);
begin
  //
end;

procedure TGrpcWsServer.WSServerMessage(Connection: TsgcWSConnection; const Text: string);
begin
  //
end;

{ TBaseGrpcWsStream }

constructor TGrpcWsStream.Create(const aClient: TGrpcWsClient; const aPath: string; aRequestId: Integer; const aReceiveCallback: TGrpcCallback);
var ref: IGrpcStream;
begin
  FWSClient := aClient.FWSClient;
  FRequestId := aRequestId;
  FResponseClosed := TBoolean.Create;
  FRequestClosed  := TBoolean.Create;
  FPath := aPath;
  FReceiveCallback := aReceiveCallback;
  ref := Self;

  aClient.FRequests.Add(aRequestId,
    procedure(const aStream: TBytes; aIsStreamClosed: Boolean)
    var callbackref: IGrpcStream;
    begin
      callbackref   := ref;
      FResponseData := aStream;
      FResponseClosed.Value := aIsStreamClosed;

      if Assigned(FReceiveCallback) then
        FReceiveCallback(aStream, aIsStreamClosed);
    end);
end;

destructor TGrpcWsStream.Destroy;
begin
  FResponseClosed.Free;
  FRequestClosed.Free;
  inherited;
end;

function TGrpcWsStream.CloseAndRecvData(out aResult: TBytes): Boolean;
begin
  Result := False;
  SendData(nil, True {close});

  if not FResponseClosed.Wait(TIMEOUT_RECV, True {ws needs synchronize}) then
    Exit;

  aResult := FResponseData;
  Result  := FResponseData <> nil;
end;

procedure TGrpcWsStream.DoCloseSend;
begin
  if not IsRequestClosed then
    SendData(nil, True);
end;

function TGrpcWsStream.IsResponseClosed: Boolean;
begin
  Result := FResponseClosed.Value;
end;

function TGrpcWsStream.IsRequestClosed: Boolean;
begin
  Result := FRequestClosed.Value;
end;

function TGrpcWsStream.Recv(out aResult: TBytes;
  aWaitTimeout: Integer): TGrpcWaitResult;
begin
  Result := TGrpcWaitResult.wrTimeout;
  Assert(false);
end;

procedure TGrpcWsStream.SendData(const aStream: TBytes; aClose: Boolean);
var
  packet: TGrpcPacket;
  wsreq: TWSRequest;
  b: TBytes;
  strm: TMemoryStream;
begin
  if IsRequestClosed then
    Assert(false, 'Cannot send: request already closed');
  if not FWSClient.Active then
    Assert(false, 'Cannot send: connection lost');

  wsreq.id := FRequestId;
  wsreq.path := FPath;
  wsreq.close := aClose;
  packet.Create(aStream);
  wsreq.grpc := packet.Serialize;
  FRequestClosed.Value := aClose;

  strm := TMemoryStream.Create;
  try
    strm.Size := 0;
    b := wsreq.Serialize;
    strm.Write(b, Length(b));
    strm.Position := 0;
    FWSClient.WriteData(strm);
  finally
    strm.Free;
  end;
end;

end.
