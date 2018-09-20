unit Ultraware.Grpc.Client;

interface

uses System.SysUtils, Grijjy.Http, Grijjy.Http2, Ultraware.Grpc;

type
  TServerErrorEvent = procedure(const aStatus, aMessage: string) of object;
  TProtoCallback<T> = reference to procedure(const aInput: T; const aHasData, aClosed: Boolean);

  TgoHttpHeaders2 = Grijjy.Http2.TgoHttpHeaders2;

  IGrpcStream = interface
    ['{B16B08D9-E5E6-4812-A6E2-09E8D1377B96}']
    procedure SendData(const aData: TBytes; aClose: Boolean = False);
    function CloseAndRecvData(out aResult: TBytes): Boolean;
    function Recv(out aResult: TBytes; aWaitTimeout: Integer = TIMEOUT_RECV): TGrpcWaitResult;
    procedure DoCloseSend();
  end;

  TGrpcStream_Send = class(TInterfacedObject, IGrpcStream)
  protected
    FStream: IGrpcStream;
    procedure CloseSend;
  public
    constructor Create(const aStream: IGrpcStream);
    property Stream: IGrpcStream read FStream implements IGrpcStream;
  end;

  TGrpcStreamClass = class of TGrpcStream_Send;

  TGrpcStream_Send<TInput: record > = class(TGrpcStream_Send)
    procedure Send(const aInput: TInput);
  end;

  TGrpcStream_Send<TInput, TOutput: record > = class(TGrpcStream_Send<TInput>)
    function CloseAndRecv(out aOutput: TOutput): Boolean;
  end;

  TGrpcStream_SendNoOut<TInput: record > = class(TGrpcStream_Send<TInput>)
    function CloseAndRecv(): Boolean;
  end;

  TGrpcHttp2Stream = class(TInterfacedObject, IGrpcStream)
  protected
    FRequest: TStreamRequest;
  protected
    { IGrpcStream }
    // procedure SendData(const aData: TBytes; aClose: Boolean = False);
    function CloseAndRecvData(out aResult: TBytes): Boolean;
    function Recv(out aResult: TBytes; aWaitTimeout: Integer = TIMEOUT_RECV): TGrpcWaitResult;
    procedure DoCloseSend();
  public
    constructor Create(aRequest: TStreamRequest);
    destructor Destroy; override;

    procedure SendData(const aData: TBytes; aClose: Boolean = False);
    function IsReponseClosed: Boolean;
  end;

  TGrpcUtil = class
    class function CheckGrpcResponse(const aHeaders: TgoHttpHeaders; const aHandleError: TServerErrorEvent = nil): Boolean;
  end;

  TBaseGrpcClient = class(TInterfacedObject)
  private
    FHttp2: TgoHttp2Client;
    FHost: string;
    FPort: Integer;
    FConnectTimeout: Integer;
    FReceiveTimeout: Integer;
    FOnException: TServerErrorEvent;
  private
    function ProtoCallbackToInternalCallback<TOutput: record >(const aReceiveCallback: TProtoCallback<TOutput>): TGrpcCallback;
    function InternalStreamToInputStream<TInputStream: TGrpcStream_Send>(const aStream: IGrpcStream): TInputStream;
    function DoRequest(const aSendData: TBytes; const aGrpcPath: string; out aReceivedData: TBytes): Boolean; overload;
    function DoRequest(const aSendData: TBytes; const aGrpcPath: string; const aReceiveCallback: TGrpcCallback): IGrpcStream; overload;
    function DoRequest<TOutput: record >(const aSendData: TBytes; const aGrpcPath: string): TOutput; overload;
  protected
    function DoRequest<TInput, TOutput: record >(const aInput: TInput; const aGrpcPath: string): TOutput; overload;
    function DoRequestNoInput<TOutput: record >(const aGrpcPath: string): TOutput;
    function DoInputStreamRequest<TInputStream: TGrpcStream_Send>(const aGrpcPath: string): TInputStream;
    function DoOuputStreamRequest<TInput, TOutput: record >(const aInput: TInput; const aGrpcPath: string; const aReceiveCallback: TProtoCallback<TOutput>): IGrpcStream; overload;
    procedure DoOuputStreamRequest<TOutput: record >(const aGrpcPath: string; const aReceiveCallback: TProtoCallback<TOutput>); overload;
    function DoInAndOutputStreamRequest<TInputStream: TGrpcStream_Send; TOutput: record >(const aGrpcPath: string; const aReceiveCallback: TProtoCallback<TOutput>): TInputStream;
  protected
    procedure AddRequestHeaders(Headers: TgoHttpHeaders2); virtual;
    function BasePath: string; virtual; abstract;
  public
    constructor Create(const aHost: string; aPort: Integer; const aSSL: Boolean = False;
      const aConnectTimeout: Integer = TIMEOUT_CONNECT; const aReceiveTimeout: Integer = TIMEOUT_RECV); overload;
    destructor Destroy; override;

    property Http2Client: TgoHttp2Client read FHttp2;
    property OnException: TServerErrorEvent read FOnException write FOnException;
  end;

implementation

uses System.DateUtils, Ultraware.Grpc.Logging;

{ TGrpcStream_Send }

procedure TGrpcStream_Send.CloseSend;
begin
  Stream.DoCloseSend();
end;

constructor TGrpcStream_Send.Create(const aStream: IGrpcStream);
begin
  inherited Create;
  FStream := aStream;
end;

{ TGrpcStream_Send<TInput> }

procedure TGrpcStream_Send<TInput>.Send(const aInput: TInput);
begin
  TGrpcLogging.LogInput<TInput>(aInput);
  Stream.SendData(Serialize<TInput>(aInput));
end;

{ TGrpcStream_Send<TInput, TOutput> }

function TGrpcStream_Send<TInput, TOutput>.CloseAndRecv(out aOutput: TOutput): Boolean;
var
  Data: TBytes;
begin
  Result := Stream.CloseAndRecvData(Data);
  if Result then
  begin
    aOutput := Deserialize<TOutput>(Data);
    TGrpcLogging.LogOutput<TOutput>(aOutput);
  end;
end;

{ TGrpcStream_SendNoOut<TInput> }

function TGrpcStream_SendNoOut<TInput>.CloseAndRecv: Boolean;
var
  Data: TBytes;
begin
  Result := Stream.CloseAndRecvData(Data);
end;

{ TBaseGrpcHttp2Stream }

function TGrpcHttp2Stream.CloseAndRecvData(out aResult: TBytes): Boolean;
var
  Packet: TGrpcPacket;
begin
  Result := False;
  FRequest.SendRequestData(nil, True { close } );

  if not FRequest.WaitForRecvClosed(TIMEOUT_RECV) then
    Exit;

  TGrpcUtil.CheckGrpcResponse(FRequest.ResponseHeaders);
  if Packet.TryDeserialize(FRequest.ResponseBuffer) then
  begin
    aResult := Packet.Data;
    Result := True;
  end;
end;

procedure TGrpcHttp2Stream.DoCloseSend;
begin
  // FRequest.SendRequestData([0, 0, 0, 0], True {close});
  FRequest.SendRequestData(nil, True { close } );
end;

constructor TGrpcHttp2Stream.Create(aRequest: TStreamRequest);
begin
  inherited Create;
  FRequest := aRequest;
end;

destructor TGrpcHttp2Stream.Destroy;
begin
  if not IsReponseClosed and not FRequest.IsRequestClosed then
  begin
    FRequest.SendRequestData(nil, True { close } );
    FRequest.CloseRequest;
  end;
  inherited;
end;

function TGrpcHttp2Stream.IsReponseClosed: Boolean;
begin
  Result := FRequest.IsResponseClosed;
end;

function TGrpcHttp2Stream.Recv(out aResult: TBytes; aWaitTimeout: Integer): TGrpcWaitResult;
var
  Packet: TGrpcPacket;
  tstart: TDateTime;
begin
  Result := TGrpcWaitResult.wrTimeout;
  tstart := Now;
  repeat
    TGrpcUtil.CheckGrpcResponse(FRequest.ResponseHeaders);
    if Packet.TryDeserialize(FRequest.ResponseBuffer) then
    begin
      aResult := Packet.Data;
      if aResult <> nil then
        Exit(TGrpcWaitResult.wrData)
      else if FRequest.IsResponseClosed then // stream has "close" flag using a zero message
        Exit(wrClosed)
      else
        Exit(TGrpcWaitResult.wrNoData);
    end;
    Sleep(10);
  until MilliSecondsBetween(Now, tstart) > aWaitTimeout;
end;

procedure TGrpcHttp2Stream.SendData(const aData: TBytes; aClose: Boolean);
var
  Packet: TGrpcPacket;
begin
  Packet.Create(aData);
  FRequest.SendRequestData(Packet.Serialize, aClose);

  TGrpcUtil.CheckGrpcResponse(FRequest.ResponseHeaders);
end;

class function TGrpcUtil.CheckGrpcResponse(const aHeaders: TgoHttpHeaders; const aHandleError: TServerErrorEvent = nil): Boolean;
var
  GrpcStatus: string;
begin
  // https://github.com/grpc/grpc-go/blob/master/codes/codes.go
  // [2]	('grpc-status', '8', 'grpc-status', '8')
  // [3]	('grpc-message', 'grpc: received message larger than max (3033307270 vs. 4194304)', 'grpc-message', 'grpc: received message larger than max (3033307270 vs. 4194304)')
  Result := False;
  GrpcStatus := aHeaders.Value('grpc-status');
  if (GrpcStatus <> '') and (GrpcStatus <> '0') then
  begin
    if Assigned(aHandleError) then
    begin
      aHandleError(GrpcStatus, aHeaders.Value('grpc-message'));
      Exit;
    end
    else
      raise Exception.CreateFmt('Server error "%s" with message: %s', [
        aHeaders.Value('grpc-status'),
        aHeaders.Value('grpc-message')]);
  end;

  if (aHeaders.Value(':status') <> '') and
    (aHeaders.Value(':status') <> '200') then
    raise Exception.CreateFmt('Server error "%s": %s', [aHeaders.Value(':status'), aHeaders.AsString]);
  Result := True;
end;

{ TBaseGrpcClient }

procedure TBaseGrpcClient.AddRequestHeaders(Headers: TgoHttpHeaders2);
begin
  // Virtual
end;

constructor TBaseGrpcClient.Create(const aHost: string; aPort: Integer; const aSSL: Boolean; const aConnectTimeout, aReceiveTimeout: Integer);
begin
  inherited Create;
  FHost := aHost;
  FPort := aPort;
  FHttp2 := TgoHttp2Client.Create(aHost, aPort, aSSL { http or https } );

  FConnectTimeout := aConnectTimeout;
  FReceiveTimeout := aReceiveTimeout;

  // FHttp2.ContentType := 'application/grpc';
  // FHttp2.UserAgent   := 'delphi/grijjy/ultraware';
  // FHttp2.RequestHeaders.AddOrSet('te', 'trailers');
end;

destructor TBaseGrpcClient.Destroy;
begin
  FHttp2.Free;
  inherited;
end;

function TBaseGrpcClient.DoRequest(const aSendData: TBytes; const aGrpcPath: string; const aReceiveCallback: TGrpcCallback): IGrpcStream;
var
  Packet: TGrpcPacket;
  Session: TSessionContext;
  Request: TStreamRequest;
begin
  Result := nil;
  Session := FHttp2.Connect();
  Packet.Create(aSendData);
  Request := Session.GetOrCreateStream(-1);

  if Assigned(aReceiveCallback) then
  begin
    Request.OnFrameReceived := procedure(const aStream: TStreamRequest; out aHandled: Boolean)
      var
        Packet: TGrpcPacket;
      begin
        if not TGrpcUtil.CheckGrpcResponse(aStream.ResponseHeaders, OnException) then
          Exit;
        // process each packet
        while Packet.TryDeserialize(Request.ResponseBuffer) or aStream.IsResponseClosed do
        begin
          aReceiveCallback(Packet.Data, aStream.IsResponseClosed);
          aHandled := True;
          if Packet.Data = nil then
            Break;
        end;
      end;

  end;

{$MESSAGE 'AddRequestHeaders functie?'}
  Request.RequestHeaders.AddOrSet('content-type', 'application/grpc');
  Request.RequestHeaders.AddOrSet('user-agent', 'grpc-delphi/0.1.0-dev');

  if Request.DoRequest('POST', BasePath + aGrpcPath, Packet.Serialize, FConnectTimeout, FReceiveTimeout, False { wait } ) then
  begin
    if TGrpcUtil.CheckGrpcResponse(Request.ResponseHeaders, OnException) then
      Result := TGrpcHttp2Stream.Create(Request);
  end
  else
  begin
    // todo: recreate session and reconnect?
    if TGrpcUtil.CheckGrpcResponse(Request.ResponseHeaders, OnException) then
      raise Exception.CreateFmt('No connection or response from server: %s', [Request.ResponseHeaders.AsString]);
  end;
end;

function TBaseGrpcClient.DoRequest(const aSendData: TBytes; const aGrpcPath: string; out aReceivedData: TBytes): Boolean;
{$MESSAGE 'Samenvoegen met bovenstaande functie -> redelijk veel zelfde code'}
var
  Packet: TGrpcPacket;
  Session: TSessionContext;
  Request: TStreamRequest;
begin
  Result := False;
  Session := FHttp2.Connect();
  Request := Session.GetOrCreateStream(-1);
  Packet.Create(aSendData);

{$MESSAGE 'AddRequestHeaders functie?'}
  Request.RequestHeaders.AddOrSet('content-type', 'application/grpc');
  Request.RequestHeaders.AddOrSet('user-agent', 'grpc-delphi/0.1.0-dev');
  // request.RequestHeaders.AddOrSet('te', 'trailers');
  AddRequestHeaders(Request.RequestHeaders);

  if Request.DoRequest('POST', BasePath + aGrpcPath, Packet.Serialize, FConnectTimeout, FReceiveTimeout, True { close } ) and
    Request.WaitForRecvClosed(FReceiveTimeout) then
  begin
    Result := True;
    if TGrpcUtil.CheckGrpcResponse(Request.ResponseHeaders, OnException) and Packet.TryDeserialize(Request.ResponseBuffer) then
      aReceivedData := Packet.Data
  end
  else
  begin
    if TGrpcUtil.CheckGrpcResponse(Request.ResponseHeaders, OnException) then
      raise Exception.CreateFmt('No connection or response from server: %s', [Request.ResponseHeaders.AsString]);
  end;
end;

function TBaseGrpcClient.ProtoCallbackToInternalCallback<TOutput>(const aReceiveCallback: TProtoCallback<TOutput>): TGrpcCallback;
begin
  if Assigned(aReceiveCallback) then
  begin
    Result :=
        procedure(const aData: TBytes; aIsStreamClosed: Boolean)
      var
        Output: TOutput;
      begin
        if aData <> nil then
        begin
          Output := Deserialize<TOutput>(aData);
          TGrpcLogging.LogOutput<TOutput>(Output);
        end;
        aReceiveCallback(Output, aData <> nil, aIsStreamClosed);
      end
  end
  else
    Result := nil;
end;

function TBaseGrpcClient.InternalStreamToInputStream<TInputStream>(const aStream: IGrpcStream): TInputStream;
begin
  if aStream = nil then
    Result := nil
  else
    Result := TGrpcStreamClass(TInputStream).Create(aStream) as TInputStream;
end;

function TBaseGrpcClient.DoRequest<TOutput>(const aSendData: TBytes; const aGrpcPath: string): TOutput;
var
  Bytes: TBytes;
begin
  if DoRequest(aSendData, aGrpcPath, Bytes) then
  begin
    Result := Deserialize<TOutput>(Bytes);
    TGrpcLogging.LogOutput<TOutput>(Result);
  end;
end;

function TBaseGrpcClient.DoRequest<TInput, TOutput>(const aInput: TInput; const aGrpcPath: string): TOutput;
begin
  TGrpcLogging.LogInput<TInput>(aInput);
  Result := DoRequest<TOutput>(Serialize(aInput), aGrpcPath);
end;

function TBaseGrpcClient.DoRequestNoInput<TOutput>(const aGrpcPath: string): TOutput;
begin
  Result := DoRequest<TOutput>(nil, aGrpcPath);
end;

function TBaseGrpcClient.DoInputStreamRequest<TInputStream>(const aGrpcPath: string): TInputStream;
begin
  Result := InternalStreamToInputStream<TInputStream>(DoRequest(nil, aGrpcPath, nil));
end;

function TBaseGrpcClient.DoOuputStreamRequest<TInput, TOutput>(const aInput: TInput; const aGrpcPath: string; const aReceiveCallback: TProtoCallback<TOutput>): IGrpcStream;
begin
  TGrpcLogging.LogInput<TInput>(aInput);
  Result := DoRequest(Serialize(aInput), aGrpcPath, ProtoCallbackToInternalCallback<TOutput>(aReceiveCallback));
end;

procedure TBaseGrpcClient.DoOuputStreamRequest<TOutput>(const aGrpcPath: string; const aReceiveCallback: TProtoCallback<TOutput>);
begin
  DoRequest(nil, aGrpcPath, ProtoCallbackToInternalCallback<TOutput>(aReceiveCallback))
end;

function TBaseGrpcClient.DoInAndOutputStreamRequest<TInputStream, TOutput>(const aGrpcPath: string; const aReceiveCallback: TProtoCallback<TOutput>): TInputStream;
begin
  Result := InternalStreamToInputStream<TInputStream>(DoRequest(nil, aGrpcPath, ProtoCallbackToInternalCallback<TOutput>(aReceiveCallback)));
end;

end.
