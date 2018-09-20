unit Ultraware.Grpc;

interface

//{$DEFINE LOGGING}

uses
  System.SysUtils, System.Types, System.Classes, Generics.Collections,
  Grijjy.Http, Grijjy.Http2;

type
  TGrpcWaitResult = (wrTimeout, wrNoData, wrData, wrClosed);

  TGrpcHeader = packed record
    Compression: Byte;
    Size: Int32;
    function TryDeserialize(const aData: TBytes): Boolean;
    function Serialize: TBytes;
  end;

  TGrpcPacket = record
    Header: TGrpcHeader;
    Data: TBytes;
    constructor Create(const aData: TBytes);
    function TryDeserialize(const aBuffer: TThreadSafeBuffer): Boolean; overload;
    function TryDeserialize(const aData: TBytes): Boolean; overload;
    function Serialize: TBytes;
  end;

  TGrpcUtil = class
    class procedure CheckGrpcResponse(const aHeaders: TgoHttpHeaders);
  end;

  IGrpcMemStream = interface
    ['{81B40D47-D61E-4284-A855-050EA6B34B7C}']
    procedure AddReceivedData(const aData: TBytes);
    procedure Close;
  end;

  TBaseGrpcMemStream = class(TInterfacedObject, IGrpcMemStream)
  protected
    FClosed: TBoolean;
    FRecvBuffer: TThreadSafeBuffer;
    function  Recv(out aResult: TBytes; aWaitTimeout: Integer = TIMEOUT_RECV): TGrpcWaitResult;
  protected
    {IGrpcMemStream}
    procedure AddReceivedData(const aData: TBytes);
    procedure Close;
  public
    constructor Create();
    destructor  Destroy; override;
  protected
  end;

  IGrpcStream = interface
    ['{B16B08D9-E5E6-4812-A6E2-09E8D1377B96}']
    procedure SendData(const aData: TBytes; aClose: Boolean = False);
    function  CloseAndRecvData(out aResult: TBytes): Boolean;
    function  Recv(out aResult: TBytes; aWaitTimeout: Integer = TIMEOUT_RECV): TGrpcWaitResult;
    procedure DoCloseSend();

    function IsResponseClosed: Boolean;
    function IsRequestClosed: Boolean;
  end;

  TGrpcHttp2Stream = class(TInterfacedObject, IGrpcStream)
  protected
    FRequest: TStreamRequest;
    procedure StreamRequestDestroyed(Sender: TObject);
  protected
    {IGrpcStream}
    function  CloseAndRecvData(out aResult: TBytes): Boolean;
    function  Recv(out aResult: TBytes; aWaitTimeout: Integer = TIMEOUT_RECV): TGrpcWaitResult;
    procedure DoCloseSend();
    procedure SendData(const aData: TBytes; aClose: Boolean = False);

    function  IsResponseClosed: Boolean;
    function  IsRequestClosed: Boolean;
  public
    constructor Create(aRequest: TStreamRequest);
    destructor  Destroy; override;
  end;

  IGrpcCallbackStream = interface
    ['{59BAE41C-3DF9-45D0-900F-8558893CDE62}']
    procedure Close;
    procedure RaiseErrror(aException: Exception);
  end;

  TGrpcStream = class(TInterfacedObject, IGrpcStream)
  protected
    FStream: IGrpcStream;
  public
    constructor Create(const aStream: IGrpcStream);
    destructor  Destroy; override;

    property  Stream: IGrpcStream read FStream implements IGrpcStream;
    procedure CloseSend;
  end;

  TGrpcCallback = reference to procedure(const aData: TBytes; aIsStreamClosed: Boolean);
  TGrpcErrorCallback = reference to procedure(const aError: Exception);

  TBaseGrpcCallbackStream = class(TInterfacedObject, IGrpcCallbackStream)
  protected
    FWriteCallback: TGrpcCallback;
    FErrorCallback: TGrpcErrorCallback;
    procedure SendData(const aData: TBytes);
  public
    constructor Create(const aWriteCallback: TGrpcCallback; const aErrorCallback: TGrpcErrorCallback);
    destructor  Destroy; override;

    procedure Close;
    procedure RaiseErrror(aException: Exception);
  end;

  IGrpcClient = interface
    ['{A6132D0A-1EB5-41B2-8F5F-21E2C28CD250}']
    function DoRequest(const aSendData: TBytes; const aGrpcPath: string; out aReceivedData: TBytes): Boolean; overload;
    function DoRequest(const aSendData: TBytes; const aGrpcPath: string; const aReceiveCallback: TGrpcCallback): IGrpcStream; overload;
  end;

  TGrpcHttp2Client = class(TInterfacedObject, IGrpcClient)
  protected
    FHttp2: TgoHttp2Client;
    FHost: string;
    FPort: Integer;
    FConnectTimeout: Integer;
    FReceiveTimeout: Integer;
  protected
    {IGrpcClient}
    function DoRequest(const aSendData: TBytes; const aGrpcPath: string; out aReceivedData: TBytes): Boolean; overload;
    function DoRequest(const aSendData: TBytes; const aGrpcPath: string; const aReceiveCallback: TGrpcCallback): IGrpcStream; overload;
  public
    procedure   AfterConstruction; override;
    constructor Create(const aHost: string; aPort: Integer; aSSL: Boolean = False; aConnectTimeout: Integer = TIMEOUT_CONNECT; aReceiveTimeout: Integer = TIMEOUT_RECV);
    destructor  Destroy; override;

    property Http2Client: TgoHttp2Client read FHttp2;
  end;

  TGrpcClientHandler = class(TInterfacedObject, IGrpcClient)
  protected
    FClient: IGrpcClient;
    property Client: IGrpcClient read FClient implements IGrpcClient;
  public
    constructor Create(const aGrpcClient: IGrpcClient);
  end;

  //------------------------------------------

  TGrpcCallThread = class;

  TBaseGrpcImplementation = class(TInterfacedObject)
  public
    constructor Create;
    class function HandleGRPC(const aPath: string; const aIn: TBytes; const aOut: TGrpcCallback; const aErrorCallback: TGrpcErrorCallback; out aCallThread: IRequestThread): Boolean; virtual; abstract;
  end;
  TBaseGrpcImplementationClass = class of TBaseGrpcImplementation;

  TGrpcServer = class
  protected
    FServer: TgoHttp2Server;
    FRegisteredImpl: TDictionary<string, TBaseGrpcImplementationClass>;
    procedure DoStreamFrameReceived(const aStream: TStreamRequest; out aHandled: Boolean);
    function  GetImplementationForPath(const aPath: string): TBaseGrpcImplementationClass;
  public
    procedure   AfterConstruction; override;
    constructor Create(const aBindAddress: string = ''{any}; aBindPort: Integer = 80 {http}; aSSL: Boolean = False);
    destructor  Destroy; override;

    procedure RegisterImplementation(const aPath: string; aImplementation: TBaseGrpcImplementationClass);

    procedure StartListen;
    procedure Stop;
  end;

  TGrpcCallThread = class(TRequestThread)
  protected
    FCallStarted: Boolean;
    FCall: TThreadProcedure;
    procedure Execute; override;
  protected
    {IRequestThread}
    procedure Close; override;
  public
    constructor Create(const aExecution: TThreadProcedure);
    destructor  Destroy; override;
  end;

  TGrpcStreamCallThread = class(TGrpcCallThread)
  protected
    FStream: IGrpcMemStream;
  public
    constructor Create(const aStream: IGrpcMemStream; aExecution: TThreadProcedure);
    destructor  Destroy; override;

    procedure AddReceivedData(const aData: TBytes);
    procedure Close; override;
  end;

  function  SwapEndian(aValue: Integer): Integer; overload;
  procedure Writeln(const aText: string);

implementation

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  DateUtils;

procedure Writeln(const aText: string);
begin
  {$IFDEF CONSOLE}
  System.Writeln(aText);
  {$ENDIF}
  {$IFDEF WIN32}
  OutputDebugString(PChar(aText));
  {$ENDIF}
end;

function SwapEndian(aValue: Integer): Integer; overload;
var Temp, i: Integer;
begin
  Temp := aValue;
  aValue := 0;
  for i := 0 to 3 do
  begin
    aValue := (aValue shl 8) or (Temp and $FF);
    Temp := Temp shr 8;
  end;
  Result := aValue;
end;

class procedure TGrpcUtil.CheckGrpcResponse(const aHeaders: TgoHttpHeaders);
begin
//https://github.com/grpc/grpc-go/blob/master/codes/codes.go
//		[2]	('grpc-status', '8', 'grpc-status', '8')
//		[3]	('grpc-message', 'grpc: received message larger than max (3033307270 vs. 4194304)', 'grpc-message', 'grpc: received message larger than max (3033307270 vs. 4194304)')

  if (aHeaders.Value('grpc-status') <> '') and
     (aHeaders.Value('grpc-status') <> '0')
  then
    raise Exception.CreateFmt('Server error "%s" with message: %s', [
        aHeaders.Value('grpc-status'),
        aHeaders.Value('grpc-message')]);

  if (aHeaders.Value(':status') <> '') and
     (aHeaders.Value(':status') <> '200')
  then
    raise Exception.CreateFmt('Server error "%s": %s', [aHeaders.Value(':status'), aHeaders.AsString]);
end;

{ TGrpcPacket }

constructor TGrpcPacket.Create(const aData: TBytes);
begin
  Self.Header.Compression := 0;
  Self.Header.Size        := SwapEndian(Length(aData));  //bigendian
  Self.Data               := aData;
end;

function TGrpcPacket.TryDeserialize(const aBuffer: TThreadSafeBuffer): Boolean;
var
  hdr, buffer: TBytes;
  iLength: Integer;
begin
  Self.Data := nil;

  //try load header first to determine packet size
  if not aBuffer.Peek(hdr, SizeOf(TGrpcHeader)) or
     not Self.Header.TryDeserialize(hdr)
  then
    Exit(False);

  iLength := SizeOf(TGrpcHeader) + Self.Header.Size;
  if (aBuffer.Size < iLength) or          //not all data yet?
     not aBuffer.Read(buffer, iLength)
  then
    Exit(False);

  //copy data part after header
  Self.Data := Copy(buffer, SizeOf(TGrpcHeader), iLength);
  Result := True;
end;

function TGrpcPacket.Serialize: TBytes;
begin
  Result := Self.Header.Serialize + Self.Data;
end;

function TGrpcPacket.TryDeserialize(const aData: TBytes): Boolean;
var
  iLength: Integer;
begin
  //try load header first to determine packet size
  if not Self.Header.TryDeserialize(aData) then
    Exit(False);

  iLength := SizeOf(TGrpcHeader) + Self.Header.Size;
  if (Length(aData) < iLength) then          //not all data yet?
    Exit(False);

  //copy data part after header
  Self.Data := Copy(aData, SizeOf(TGrpcHeader), iLength);
  Result := True;
end;

{ TGrpcHeader }

function TGrpcHeader.TryDeserialize(const aData: TBytes): Boolean;
begin
  Result := (Length(aData) >= SizeOf(Self));
  if not Result then
    Exit;

  Move(aData[0], Self, SizeOf(Self));
  Self.Size := SwapEndian(Self.Size);  //bigendian to little endian
end;

function TGrpcHeader.Serialize: TBytes;
begin
  SetLength(Result, SizeOf(Self));
  Move(Self, Result[0], SizeOf(Self));
end;

{ TBaseGrpcHttp2Stream }

function TGrpcHttp2Stream.CloseAndRecvData(out aResult: TBytes): Boolean;
var
  packet: TGrpcPacket;
begin
  Result := False;
  if (FRequest = nil) then
    Exit;

  FRequest.SendRequestData(nil, True {close});

  if not FRequest.WaitForRecvClosed(TIMEOUT_RECV) then
    Exit;

  TGrpcUtil.CheckGrpcResponse(FRequest.ResponseHeaders);
  if packet.TryDeserialize(FRequest.ResponseBuffer) then
  begin
    aResult := packet.Data;
    Result := True;
  end;
end;

procedure TGrpcHttp2Stream.DoCloseSend;
begin
  if (FRequest <> nil) then
    FRequest.SendRequestData(nil, True {close});
end;

constructor TGrpcHttp2Stream.Create(aRequest: TStreamRequest);
begin
  FRequest := aRequest;
  if (FRequest <> nil) then
    FRequest.OnDestroy := StreamRequestDestroyed;
end;

destructor TGrpcHttp2Stream.Destroy;
begin
  if (FRequest <> nil) then
  begin
    if not IsRequestClosed then
    begin
      FRequest.SendRequestData(nil, True {close});
      FRequest.CloseRequest;
    end;
    FRequest.OnFrameReceived := nil;
    FRequest.OnDestroy := nil;
  end;
  inherited;
end;

function TGrpcHttp2Stream.IsResponseClosed: Boolean;
begin
  Result := (FRequest = nil) or FRequest.IsResponseClosed;
end;

function TGrpcHttp2Stream.IsRequestClosed: Boolean;
begin
  Result := (FRequest = nil) or FRequest.IsRequestClosed;
end;

function TGrpcHttp2Stream.Recv(out aResult: TBytes; aWaitTimeout: Integer): TGrpcWaitResult;
var
  packet: TGrpcPacket;
  tstart: TDateTime;
begin
  Result := TGrpcWaitResult.wrTimeout;
  tstart := Now;
  repeat
    TGrpcUtil.CheckGrpcResponse(FRequest.ResponseHeaders);
    if packet.TryDeserialize(FRequest.ResponseBuffer) then
    begin
      aResult := packet.Data;
      if aResult <> nil then
        Exit(TGrpcWaitResult.wrData)
      else if FRequest.IsResponseClosed then  //stream has "close" flag using a zero message
        Exit(wrClosed)
      else
        Exit(TGrpcWaitResult.wrNoData);
    end;
    Sleep(10);
  until MilliSecondsBetween(Now, tstart) > aWaitTimeout;
end;

procedure TGrpcHttp2Stream.SendData(const aData: TBytes; aClose: Boolean);
var
  packet: TGrpcPacket;
begin
  if IsRequestClosed then
    Assert(false, 'Cannot send: request already closed');
  if IsResponseClosed then
    Assert(false, 'Cannot send: response already closed');

  try
    TGrpcUtil.CheckGrpcResponse(FRequest.ResponseHeaders);

    packet.Create(aData);
    FRequest.SendRequestData(packet.Serialize, aClose);

    TGrpcUtil.CheckGrpcResponse(FRequest.ResponseHeaders);
  except
    FRequest.CloseRequest;
    raise;
  end;
end;

procedure TGrpcHttp2Stream.StreamRequestDestroyed(Sender: TObject);
begin
  FRequest := nil;
end;

{ TBaseGrpcService }

procedure TGrpcHttp2Client.AfterConstruction;
begin
  inherited;
end;

constructor TGrpcHttp2Client.Create(const aHost: string; aPort: Integer; aSSL: Boolean; aConnectTimeout, aReceiveTimeout: Integer);
begin
  FHost  := aHost;
  FPort  := aPort;
  FHttp2 := TgoHttp2Client.Create(aHost, aPort, aSSL {http or https});

  FConnectTimeout := aConnectTimeout;
  FReceiveTimeout := aReceiveTimeout;
end;

destructor TGrpcHttp2Client.Destroy;
begin
  FHttp2.Free;
  inherited;
end;

function TGrpcHttp2Client.DoRequest(const aSendData: TBytes; const aGrpcPath: string;
  const aReceiveCallback: TGrpcCallback): IGrpcStream;
var
  packet: TGrpcPacket;
  session: TSessionContext;
  request: TStreamRequest;
begin
  session := FHttp2.Connect();
  packet.Create(aSendData);
  request := session.GetOrCreateStream(-1);

  if Assigned(aReceiveCallback) then
  begin
    request.OnFrameReceived := procedure(const aStream: TStreamRequest; out aHandled: Boolean)
      var packet: TGrpcPacket;
      begin
        TGrpcUtil.CheckGrpcResponse(aStream.ResponseHeaders);
        //process each packet
        while packet.TryDeserialize(request.ResponseBuffer) or aStream.IsResponseClosed do
        begin
          aReceiveCallback(packet.Data, aStream.IsResponseClosed);
          aHandled := True;
          if packet.Data = nil then
            Break;
        end;
      end;
  end;
  request.RequestHeaders.AddOrSet('content-type', 'application/grpc');
  request.RequestHeaders.AddOrSet('user-agent', 'grpc-delphi/0.1.0-dev');

  if request.DoRequest('POST', aGrpcPath, packet.Serialize, FConnectTimeout, FReceiveTimeout, False{wait}) then
  begin
    TGrpcUtil.CheckGrpcResponse(request.ResponseHeaders);
    Result := TGrpcHttp2Stream.Create(request);
  end
  else
  begin
    //todo: recreate session and reconnect?

    TGrpcUtil.CheckGrpcResponse(request.ResponseHeaders);
    raise Exception.CreateFmt('No connection or response from server: %s', [request.ResponseHeaders.AsString]);
  end;
end;

function TGrpcHttp2Client.DoRequest(const aSendData: TBytes; const aGrpcPath: string; out aReceivedData: TBytes): Boolean;
var
  packet: TGrpcPacket;
  session: TSessionContext;
  request: TStreamRequest;
begin
  session := FHttp2.Connect();
  request := session.GetOrCreateStream(-1);
  packet.Create(aSendData);

  request.RequestHeaders.AddOrSet('content-type', 'application/grpc');
  request.RequestHeaders.AddOrSet('user-agent', 'grpc-delphi/0.1.0-dev');

  if request.DoRequest('POST', aGrpcPath, packet.Serialize, FConnectTimeout, FReceiveTimeout, True {close}) and
     request.WaitForRecvClosed(FReceiveTimeout) then
  begin
    Result := True;
    TGrpcUtil.CheckGrpcResponse(request.ResponseHeaders);
    if packet.TryDeserialize(request.ResponseBuffer) then
      aReceivedData := packet.Data;
  end
  else
  begin
    TGrpcUtil.CheckGrpcResponse(request.ResponseHeaders);
    raise Exception.CreateFmt('No connection or response from server: %s', [request.ResponseHeaders.AsString]);
  end;
end;

{ TBaseGrpcImplementation }

constructor TBaseGrpcImplementation.Create;
begin
end;

{ TBaseGrpcMemStream }

procedure TBaseGrpcMemStream.AddReceivedData(const aData: TBytes);
begin
  FRecvBuffer.Write(TGrpcPacket.Create(aData).Serialize);
end;

procedure TBaseGrpcMemStream.Close;
begin
  FClosed.Value := True;
end;

constructor TBaseGrpcMemStream.Create;
begin
  FRecvBuffer := TThreadSafeBuffer.Create;
  FClosed := TBoolean.Create;
end;

destructor TBaseGrpcMemStream.Destroy;
begin
  FRecvBuffer.Free;
  FClosed.Free;
  inherited;
end;

function TBaseGrpcMemStream.Recv(out aResult: TBytes;
  aWaitTimeout: Integer): TGrpcWaitResult;
var
  tstart: TDatetime;
  packet: TGrpcPacket;
begin
  Result := wrTimeout;
  aResult := nil;
  tstart := Now;
  repeat
    if packet.TryDeserialize(FRecvBuffer) then
    begin
      aResult := packet.Data;
      if aResult <> nil then
        Exit(wrData)
      else if FClosed.Value then  //stream has "close" flag using a zero message
        Exit(wrClosed)
      else
        Exit(wrNoData)
    end;
    if FClosed.Value then
      Exit(wrClosed);
    Sleep(10);
  until MilliSecondsBetween(Now, tstart) > aWaitTimeout;
end;

{ TGrpcCallThread }

procedure TGrpcCallThread.Close;
begin
  while not FCallStarted do
    Sleep(1);
  inherited;
end;

constructor TGrpcCallThread.Create(const aExecution: TThreadProcedure);
begin
  FCall := aExecution;
  inherited Create(False{direct run});
end;

destructor TGrpcCallThread.Destroy;
begin
  FCall := nil;
  inherited;
end;

procedure TGrpcCallThread.Execute;
begin
  TThread.NameThreadForDebugging(Self.ClassName);
  if not Terminated then
  begin
    FCallStarted := True;
    FCall();
  end;
  FCall := nil;
end;

{ TGrpcStreamCallThread }

procedure TGrpcStreamCallThread.Close;
begin
  inherited Close;
  FStream.Close;
end;

constructor TGrpcStreamCallThread.Create(const aStream: IGrpcMemStream;
  aExecution: TThreadProcedure);
begin
  FStream := aStream;
  inherited Create(aExecution);
end;

destructor TGrpcStreamCallThread.Destroy;
begin
  inherited;
end;

procedure TGrpcStreamCallThread.AddReceivedData(const aData: TBytes);
begin
  if aData <> nil then
    FStream.AddReceivedData(aData);
end;

{ TBaseGrpcCallbackStream }

procedure TBaseGrpcCallbackStream.Close;
begin
  if Assigned(FWriteCallback) then
    FWriteCallback(nil, True{close});
  FWriteCallback := nil;
end;

constructor TBaseGrpcCallbackStream.Create(const aWriteCallback: TGrpcCallback; const aErrorCallback: TGrpcErrorCallback);
begin
  FWriteCallback := aWriteCallback;
  FErrorCallback := aErrorCallback;
end;

destructor TBaseGrpcCallbackStream.Destroy;
begin
  Close;
  inherited;
end;

procedure TBaseGrpcCallbackStream.RaiseErrror(aException: Exception);
begin
  if Assigned(FErrorCallback) then
    FErrorCallback(aException);
end;

procedure TBaseGrpcCallbackStream.SendData(const aData: TBytes);
begin
  if Assigned(FWriteCallback) then
    FWriteCallback(aData, False{no close});
end;

{ TGrpcServer }

procedure TGrpcServer.AfterConstruction;
begin
  inherited;
  FRegisteredImpl := TDictionary<string, TBaseGrpcImplementationClass>.Create;
end;

constructor TGrpcServer.Create(const aBindAddress: string; aBindPort: Integer;
  aSSL: Boolean);
begin
  FServer := TgoHttp2Server.Create(aBindAddress, aBindPort, aSSL);
  FServer.OnStreamFrameReceived := DoStreamFrameReceived;
end;

destructor TGrpcServer.Destroy;
begin
  FServer.Free;
  FRegisteredImpl.Free;
  inherited;
end;

procedure TGrpcServer.DoStreamFrameReceived(const aStream: TStreamRequest;
  out aHandled: Boolean);
var
  recv: TBytes;
  packet: TGrpcPacket;
  thread: IRequestThread;
  streamthread: TGrpcStreamCallThread;
  impl: TBaseGrpcImplementationClass;
begin
  aHandled := False;
  {$IFDEF LOGGING}
  Writeln(Format('>> %s: %d bytes', [aStream.RequestPath, aStream.RequestBuffer.Size]));
  {$ENDIF}

  //more data for previous/running stream?
  if (aStream.RequestThread <> nil) and
     (aStream.RequestThread is TGrpcStreamCallThread) then
  begin
    streamthread := aStream.RequestThread as TGrpcStreamCallThread;

    while packet.TryDeserialize(aStream.RequestBuffer) do
      streamthread.AddReceivedData(packet.Data);
    if aStream.IsRequestClosed then
    begin
      streamthread.Close;
      //aStream.RequestThread := nil;   thread must not freed here already, let it run till request object is finished
    end;

    aHandled := True;
    Exit;
  end;

  if (aStream.RequestHeaders.Value('content-type') <> '') and
     (aStream.RequestHeaders.Value('content-type') <> 'application/grpc') then
    Exit;
  if aStream.RequestHeaders.Value(':method') <> 'POST' then
    Exit;

  impl := GetImplementationForPath(aStream.RequestPath);   //e.g. '/routeguide.RouteGuide/GetFeature'
  if impl = nil then
  begin
    aHandled := False;
    Exit;
  end;

  //no or not enough data yet?
  if not packet.TryDeserialize(aStream.RequestBuffer) then
  begin
    aHandled := True;
    Exit;
  end;
  recv := packet.Data;
  aStream.ResponseHeaders.AddOrSet(':status', '200');  //must be the first?
  aStream.ResponseHeaders.AddOrSet('content-type', 'application/grpc');

  thread := nil;
  //try to handle new request
  if impl.HandleGRPC(aStream.RequestPath, recv,
    procedure(const aData: TBytes; aIsStreamClosed: Boolean)
    var packet: TGrpcPacket;
    begin
      {$IFDEF LOGGING}
      Writeln(Format('<< %s: %d bytes', [aStream.RequestPath, Length(aData)]));
      {$ENDIF}
      packet.Create(aData);
      aStream.SendResponseData(packet.Serialize, aIsStreamClosed);
    end,
    procedure(const aError: Exception)
    begin
      {$IFDEF LOGGING}
      Writeln(Format('E! %s', [aError.Message]));
      {$ENDIF}
      aStream.SendErrorResponse(aError);
    end,
    thread) then
  begin
    aHandled := True;
    if thread <> nil then
      aStream.RequestThread := thread;
    Exit;
  end;
end;

function TGrpcServer.GetImplementationForPath(
  const aPath: string): TBaseGrpcImplementationClass;
var
  parts: TArray<string>;
begin
  Result := nil;
  parts := aPath.Split(['/']);  //e.g. '/routeguide.RouteGuide/GetFeature'
  if Length(parts) < 2 then
    Exit;

  if FRegisteredImpl.TryGetValue(parts[1], Result) then ;
end;

procedure TGrpcServer.RegisterImplementation(const aPath: string;
  aImplementation: TBaseGrpcImplementationClass);
begin
  // '/routeguide.RouteGuide/  -> routeguide.RouteGuide
  FRegisteredImpl.AddOrSetValue(aPath.Replace('/', ''), aImplementation);
end;

procedure TGrpcServer.StartListen;
begin
  FServer.StartListen;
end;

procedure TGrpcServer.Stop;
begin
  FServer.Stop;
end;

{ TGrpcClientHandler }

constructor TGrpcClientHandler.Create(const aGrpcClient: IGrpcClient);
begin
  FClient := aGrpcClient;
end;

{ TGrpcStream }

procedure TGrpcStream.CloseSend;
begin
  Stream.DoCloseSend;
end;

constructor TGrpcStream.Create(const aStream: IGrpcStream);
begin
  FStream := aStream;
end;

destructor TGrpcStream.Destroy;
begin
  FStream := nil;
  inherited;
end;

end.
