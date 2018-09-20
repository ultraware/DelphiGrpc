unit Ultraware.Grpc.Server;

interface

uses System.Generics.Collections, System.SysUtils, System.Classes,
  Grijjy.Http, Grijjy.Http2, Ultraware.Grpc;

type
  TBaseGrpcImplementation = class;
  TExecuteProc = reference to procedure;
  TSimpleFuncExecuteProc<TInput, TOutput> = function(const aInput: TInput): TOutput of object;
  TSimpleFuncExecuteProcNoInput<TOutput> = function: TOutput of object;
  TStreamInputFuncExecuteProc<TCallbackStream, TOutput> = function(const aInputStream: TCallbackStream): TOutput of object;
  TStreamOutputFuncExecuteProc<TInput, TCallbackStream> = procedure(const aInput: TInput; const aOutputStream: TCallbackStream) of object;
  TStreamOutputFuncExecuteProcNoInput<TCallbackStream> = procedure(const aOutputStream: TCallbackStream) of object;
  TStreamFuncExecuteProc<TInputCallbackStream, TOutputCallbackStream> = procedure(const aInputStream: TInputCallbackStream; const aOutputStream: TOutputCallbackStream) of object;
  TGrpcWaitResult = Ultraware.Grpc.TGrpcWaitResult;
  TGrpcCall = reference to procedure(const aIn: TBytes; const aOut: TGrpcCallback; out aCallThread: IRequestThread);

  ICallbackStream = interface
    ['{B0D44923-69BE-4E67-9F5B-089964D06DD4}']
    procedure CloseSend;
  end;

  IGrpcMemStream = interface
    ['{81B40D47-D61E-4284-A855-050EA6B34B7C}']
    procedure AddReceivedData(const aData: TBytes);
    procedure Close;
    function IsClosed: Boolean;
  end;

  TBaseGrpcMemStream = class(TInterfacedObject, IGrpcMemStream)
  protected
    FClosed: TBoolean;
    FRecvBuffer: TThreadSafeBuffer;
    function Recv(out aResult: TBytes; const aWaitTimeout: Integer = TIMEOUT_RECV): TGrpcWaitResult;
  protected
    { IGrpcMemStream }
    procedure AddReceivedData(const aData: TBytes);
    procedure Close;
    function IsClosed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TBaseGrpcMemStreamClass = class of TBaseGrpcMemStream;

  TBaseGrpcMemStream<TInput: record > = class(TBaseGrpcMemStream)
    function Recv(out aInput: TInput; const aWaitTimeout: Integer): TGrpcWaitResult;
  end;

  TServerCallbackStream = class(TBaseGrpcCallbackStream);
  TServerCallbackStreamClass = class of TServerCallbackStream;

  TServerCallbackStream<TOutput: record > = class(TServerCallbackStream, ICallbackStream)
  protected
    procedure Send(const aOutput: TOutput);
    procedure ICallbackStream.CloseSend = Close;
  end;

  TRegisteredFunctions = TDictionary<string, TGrpcCall>;
  TStreamRequest = Grijjy.Http2.TStreamRequest;

  TBaseGrpcImplementation = class(TInterfacedObject)
  private
    type
    TDummyRecord = record
    end;
  protected
    const
    cDefaultErrorMessageString = 'Internal server error';
    class function GetErrorMessage(E: Exception): string; virtual;
  private
    FRegisteredFunctions: TRegisteredFunctions;
    property RegisteredFunctions: TRegisteredFunctions read FRegisteredFunctions;

    procedure InternalRegisterCall(const aCallName: string; const aCall: TGrpcCall); overload;
  protected
    procedure RegisterCall<TInput, TOutput: record >(const aCallName: string; const aCall: TSimpleFuncExecuteProc<TInput, TOutput>);
    procedure RegisterCallNoInput<TOutput: record >(const aCallName: string; const aCall: TSimpleFuncExecuteProcNoInput<TOutput>);
    procedure RegisterInputStreamCall<StreamClass: TBaseGrpcMemStream; StreamIntf: IGrpcMemStream; TOutput: record >(const aCallName: string; const aCall: TStreamInputFuncExecuteProc<StreamIntf, TOutput>);
    procedure RegisterOutputStreamCall<TInput: record; StreamClass: TServerCallbackStream; StreamIntf: ICallbackStream>(const aCallName: string; const aCall: TStreamOutputFuncExecuteProc<TInput, StreamIntf>);
    procedure RegisterOutputStreamCallNoInput<StreamClass: TServerCallbackStream; StreamIntf: ICallbackStream>(const aCallName: string; const aCall: TStreamOutputFuncExecuteProcNoInput<StreamIntf>);
    procedure RegisterStreamCall<InputStreamClass: TBaseGrpcMemStream; InputStreamIntf: IGrpcMemStream; OutputStreamClass: TServerCallbackStream; OutputStreamIntf: ICallbackStream>(const aCallName: string;
      const aCall: TStreamFuncExecuteProc<InputStreamIntf, OutputStreamIntf>);

    procedure DoRegisterCalls; virtual; abstract;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class function BasePath: string; virtual; abstract;
  private
    class var FDefaultImpl: TBaseGrpcImplementation;
  private
    class function HandleGRPC(const aRequest: TStreamRequest; const aIn: TBytes; const aOut: TGrpcCallback; out aCallThread: IRequestThread): Boolean;
  protected
    class function GetImplementation(const aRequest: TStreamRequest): TBaseGrpcImplementation; virtual;
  end;

  TBaseGrpcImplementationClass = class of TBaseGrpcImplementation;

  TGrpcServer = class
  protected
    FServer: TgoHttp2Server;
    FRegisteredImpl: TDictionary<string, TBaseGrpcImplementationClass>;
    procedure DoStreamFrameReceived(const aStream: TStreamRequest; out aHandled: Boolean);
    function GetImplementationForPath(const aPath: string): TBaseGrpcImplementationClass;
  public
    procedure AfterConstruction; override;
    constructor Create(const aBindAddress: string = '' { any }; aBindPort: Integer = 80 { http }; aSSL: Boolean = False);
    destructor Destroy; override;

    procedure RegisterImplementation(const aPath: string; aImplementation: TBaseGrpcImplementationClass);

    procedure StartListen;
    procedure Stop;
  end;

  TGrpcCallThread = class(TRequestThread)
  protected
    FCall: TThreadProcedure;
    procedure Execute; override;
  public
    constructor Create(const aExecution: TThreadProcedure);
    destructor Destroy; override;
  end;

  TGrpcStreamCallThread = class(TGrpcCallThread)
  protected
    FStream: IGrpcMemStream;
  public
    constructor Create(const aStream: IGrpcMemStream; aExecution: TThreadProcedure);

    procedure AddReceivedData(const aData: TBytes);
    procedure Close; override;
  end;

implementation

uses System.DateUtils, UltraGUID, Ultraware.Grpc.Logging;

{ TBaseGrpcImplementation }

procedure TBaseGrpcImplementation.AfterConstruction;
begin
  inherited;
  FRegisteredFunctions := TRegisteredFunctions.Create;
  DoRegisterCalls();
end;

destructor TBaseGrpcImplementation.Destroy;
begin
  FRegisteredFunctions.Free;
  inherited;
end;

class function TBaseGrpcImplementation.GetErrorMessage(E: Exception): string;
begin
  Result := cDefaultErrorMessageString;
end;

class function TBaseGrpcImplementation.GetImplementation(const aRequest: TStreamRequest): TBaseGrpcImplementation;
begin
  if FDefaultImpl = nil then
    FDefaultImpl := TBaseGrpcImplementationClass(Self).Create;
  Result := FDefaultImpl;
end;

class function TBaseGrpcImplementation.HandleGRPC(const aRequest: TStreamRequest; const aIn: TBytes; const aOut: TGrpcCallback; out aCallThread: IRequestThread): Boolean;
var
  Impl: TBaseGrpcImplementation;
  Call: TGrpcCall;
begin
  Impl := GetImplementation(aRequest);
  Result := Impl.RegisteredFunctions.TryGetValue(aRequest.RequestPath, Call);
  if Result then
    Call(aIn, aOut, aCallThread);
end;

procedure TBaseGrpcImplementation.InternalRegisterCall(const aCallName: string; const aCall: TGrpcCall);
begin
  RegisteredFunctions.Add(BasePath + aCallName, aCall);
end;

procedure TBaseGrpcImplementation.RegisterCall<TInput, TOutput>(const aCallName: string; const aCall: TSimpleFuncExecuteProc<TInput, TOutput>);
begin
  InternalRegisterCall(aCallName,
    procedure(const aIn: TBytes; const aOut: TGrpcCallback; out aCallThread: IRequestThread)
    var
      Input: TInput;
      Output: TOutput;
    begin
      Input := Deserialize<TInput>(aIn);
      TGrpcLogging.LogInput<TInput>(Input);
      Output := aCall(Input);
      TGrpcLogging.LogOutput<TOutput>(Output);
      aOut(Serialize<TOutput>(Output), True);
    end);
end;

procedure TBaseGrpcImplementation.RegisterCallNoInput<TOutput>(const aCallName: string; const aCall: TSimpleFuncExecuteProcNoInput<TOutput>);
var
  SubCall: TSimpleFuncExecuteProc<TDummyRecord, TOutput>;
begin
  InternalRegisterCall(aCallName,
    procedure(const aIn: TBytes; const aOut: TGrpcCallback; out aCallThread: IRequestThread)
    var
      Output: TOutput;
    begin
      Output := aCall();
      TGrpcLogging.LogOutput<TOutput>(Output);
      aOut(Serialize<TOutput>(Output), True);
    end);
end;

procedure TBaseGrpcImplementation.RegisterInputStreamCall<StreamClass, StreamIntf, TOutput>(const aCallName: string; const aCall: TStreamInputFuncExecuteProc<StreamIntf, TOutput>);
begin
  InternalRegisterCall(aCallName,
    procedure(const aIn: TBytes; const aOut: TGrpcCallback; out aCallThread: IRequestThread)
    var
      Stream: StreamIntf;
      StreamObj: TBaseGrpcMemStream;
    begin
      StreamObj := TBaseGrpcMemStreamClass(StreamClass).Create();
      Assert(Supports(StreamObj, TGuidInterceptor.GetGuid<StreamIntf>, Stream));
      aCallThread := TGrpcStreamCallThread.Create(Stream,
        procedure
        var
          Output: TOutput;
        begin
          Output := aCall(Stream);
          TGrpcLogging.LogOutput<TOutput>(Output);
          aOut(Serialize<TOutput>(Output), True);
        end);
    end);
end;

procedure TBaseGrpcImplementation.RegisterOutputStreamCall<TInput, StreamClass, StreamIntf>(const aCallName: string; const aCall: TStreamOutputFuncExecuteProc<TInput, StreamIntf>);
begin
  InternalRegisterCall(aCallName,
    procedure(const aIn: TBytes; const aOut: TGrpcCallback; out aCallThread: IRequestThread)
    var
      Input: TInput;
    begin
      Input := Deserialize<TInput>(aIn);
      TGrpcLogging.LogInput<TInput>(Input);

      aCallThread := TGrpcCallThread.Create(
        procedure
        var
          Stream: StreamIntf;
          StreamObj: TServerCallbackStream;
        begin
          StreamObj := TServerCallbackStreamClass(StreamClass).Create(aOut, nil {todo});
          Assert(Supports(StreamObj, TGuidInterceptor.GetGuid<StreamIntf>, Stream));
          aCall(Input, Stream);
          Stream := nil;
        end);
    end);
end;

procedure TBaseGrpcImplementation.RegisterOutputStreamCallNoInput<StreamClass, StreamIntf>(const aCallName: string; const aCall: TStreamOutputFuncExecuteProcNoInput<StreamIntf>);
begin
  InternalRegisterCall(aCallName,
    procedure(const aIn: TBytes; const aOut: TGrpcCallback; out aCallThread: IRequestThread)
    begin
      aCallThread := TGrpcCallThread.Create(
        procedure
        var
          Stream: StreamIntf;
          StreamObj: TServerCallbackStream;
        begin
          StreamObj := TServerCallbackStreamClass(StreamClass).Create(aOut, nil {todo});
          Assert(Supports(StreamObj, TGuidInterceptor.GetGuid<StreamIntf>, Stream));
          aCall(Stream);
          Stream := nil;
        end);
    end);
end;

procedure TBaseGrpcImplementation.RegisterStreamCall<InputStreamClass, InputStreamIntf, OutputStreamClass, OutputStreamIntf>(const aCallName: string;
const aCall: TStreamFuncExecuteProc<InputStreamIntf, OutputStreamIntf>);
begin
  InternalRegisterCall(aCallName,
    procedure(const aIn: TBytes; const aOut: TGrpcCallback; out aCallThread: IRequestThread)
    var
      InputStream: InputStreamIntf;
      InputStreamObj: TBaseGrpcMemStream;
    begin
      InputStreamObj := TBaseGrpcMemStreamClass(InputStreamClass).Create();
      Assert(Supports(InputStreamObj, TGuidInterceptor.GetGuid<InputStreamIntf>, InputStream));
      aCallThread := TGrpcStreamCallThread.Create(InputStream,
        procedure
        var
          OutputStream: OutputStreamIntf;
          OutputStreamObj: TServerCallbackStream;
        begin
          OutputStreamObj := TServerCallbackStreamClass(OutputStreamClass).Create(aOut, nil {todo});
          Assert(Supports(OutputStreamObj, TGuidInterceptor.GetGuid<OutputStreamIntf>, OutputStream));
          aCall(InputStream, OutputStream);
          OutputStream := nil;
        end);
    end);
end;

{ TGrpcServer }

procedure TGrpcServer.AfterConstruction;
begin
  inherited;
  FRegisteredImpl := TDictionary<string, TBaseGrpcImplementationClass>.Create;
end;

constructor TGrpcServer.Create(const aBindAddress: string; aBindPort: Integer; aSSL: Boolean);
begin
  inherited Create;
  FServer := TgoHttp2Server.Create(aBindAddress, aBindPort, aSSL); // (True {http2}, False {async});
  FServer.OnStreamFrameReceived := DoStreamFrameReceived;
end;

destructor TGrpcServer.Destroy;
begin
  FServer.Free;
  FRegisteredImpl.Free;
  inherited;
end;

procedure TGrpcServer.DoStreamFrameReceived(const aStream: TStreamRequest; out aHandled: Boolean);
var
  Bytes: TBytes;
  Packet: TGrpcPacket;
  Thread: IRequestThread;
  StreamThread: TGrpcStreamCallThread;
  ErrorMessage: string;
  Impl: TBaseGrpcImplementationClass;
begin
  aHandled := False;

  // more data for previous/running stream?
  if (aStream.RequestThread <> nil) and
    (aStream.RequestThread is TGrpcStreamCallThread) then
  begin
    StreamThread := aStream.RequestThread as TGrpcStreamCallThread;

    while Packet.TryDeserialize(aStream.RequestBuffer) do
      StreamThread.AddReceivedData(Packet.Data);
    if aStream.IsRequestClosed then
      StreamThread.Close;
    // aStream.RequestThread := nil;   thread must not freed here already, let it run till request object is finished

    aHandled := True;
    Exit;
  end;

  if (aStream.RequestHeaders.Value('content-type') <> '') and
    (aStream.RequestHeaders.Value('content-type') <> 'application/grpc') then
    Exit;
  if aStream.RequestHeaders.Value(':method') <> 'POST' then
    Exit;

  // '/routeguide.RouteGuide/GetFeature' then
  Impl := GetImplementationForPath(aStream.RequestPath);

  if Impl = nil then
  begin
    aHandled := False;
    Exit;
  end;
  if aStream.RequestBuffer.Size > 0 then
    TGrpcLogging.Log(Format('>> %s: %d bytes', [aStream.RequestPath, aStream.RequestBuffer.Size]));

  // no or not enough data yet?
  if not Packet.TryDeserialize(aStream.RequestBuffer) then
  begin
    aHandled := True;
    Exit;
  end;
  Bytes := Packet.Data;

  Thread := nil;
  aStream.ResponseHeaders.AddOrSet(':status', '200'); // must be the first?
  aStream.ResponseHeaders.AddOrSet('content-type', 'application/grpc');
  // try to handle new request
  try
    if Impl.HandleGRPC(aStream, Bytes,
      procedure(const aData: TBytes; aIsStreamClosed: Boolean)
      var
        Packet: TGrpcPacket;
      begin
        TGrpcLogging.Log(Format('<< %s: %d bytes', [aStream.RequestPath, Length(aData)]));
        Packet.Create(aData);
        aStream.SendResponseData(Packet.Serialize, aIsStreamClosed);
      end,
      Thread) then
    begin
      aHandled := True;
      if Thread <> nil then
        aStream.RequestThread := Thread;
      Exit;
    end;
  except
    on E: Exception do
    begin
      TGrpcLogging.LogError(E.Message);
      if Impl <> nil then
        Impl.GetErrorMessage(E)
      else
        ErrorMessage := 'Internal error';
      aStream.ResponseHeaders.AddOrSet('grpc-status', '500');
      aStream.ResponseHeaders.AddOrSet('grpc-message', ErrorMessage);
      aStream.SendResponseData(nil, True);
      aHandled := True;
    end;
  end;
end;

function TGrpcServer.GetImplementationForPath(const aPath: string): TBaseGrpcImplementationClass;
var
  Parts: TArray<string>;
begin
  Result := nil;
  // '/routeguide.RouteGuide/GetFeature' then
  Parts := aPath.Split(['/']);
  if Length(Parts) < 2 then
    Exit;

  FRegisteredImpl.TryGetValue(Parts[1], Result);
end;

procedure TGrpcServer.RegisterImplementation(const aPath: string; aImplementation: TBaseGrpcImplementationClass);
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

{ TGrpcCallThread }

constructor TGrpcCallThread.Create(const aExecution: TThreadProcedure);
begin
  FCall := aExecution;
  inherited Create(False { direct run } );
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
    FCall();
  FCall := nil;
end;

{ TGrpcStreamCallThread }

procedure TGrpcStreamCallThread.Close;
begin
  inherited Close;
  FStream.Close;
end;

constructor TGrpcStreamCallThread.Create(const aStream: IGrpcMemStream; aExecution: TThreadProcedure);
begin
  FStream := aStream;
  inherited Create(aExecution);
end;

procedure TGrpcStreamCallThread.AddReceivedData(const aData: TBytes);
begin
  if aData <> nil then
    FStream.AddReceivedData(aData);
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
  inherited Create;
  FRecvBuffer := TThreadSafeBuffer.Create;
  FClosed := TBoolean.Create;
end;

destructor TBaseGrpcMemStream.Destroy;
begin
  FRecvBuffer.Free;
  FClosed.Free;
  inherited;
end;

function TBaseGrpcMemStream.IsClosed: Boolean;
begin
  Result := FClosed.Value;
end;

function TBaseGrpcMemStream.Recv(out aResult: TBytes; const aWaitTimeout: Integer = TIMEOUT_RECV): TGrpcWaitResult;
var
  StartTime: TDatetime;
  Packet: TGrpcPacket;
begin
  Result := wrTimeout;
  aResult := nil;
  StartTime := Now;
  try
    repeat
      if Packet.TryDeserialize(FRecvBuffer) then
      begin
        aResult := Packet.Data;
        if aResult <> nil then
          Exit(wrData)
        else if FClosed.Value then // stream has "close" flag using a zero message
          Exit(wrClosed)
        else
          Exit(wrNoData)
      end;
      if FClosed.Value then
        Exit(wrClosed);
      Sleep(10);
    until MilliSecondsBetween(Now, StartTime) > aWaitTimeout;
  finally
    if Result = wrClosed then
      FClosed.Value := True;
  end;
end;

{ TBaseGrpcMemStream<TInput> }

function TBaseGrpcMemStream<TInput>.Recv(out aInput: TInput; const aWaitTimeout: Integer): TGrpcWaitResult;
var
  Bytes: TBytes;
begin
  Result := inherited Recv(Bytes, aWaitTimeout);
  if Result = wrData then
  begin
    aInput := Deserialize<TInput>(Bytes);
    TGrpcLogging.LogInput<TInput>(aInput);
  end
end;

{ TServerCallbackStream<TOutput> }

procedure TServerCallbackStream<TOutput>.Send(const aOutput: TOutput);
begin
  TGrpcLogging.LogOutput<TOutput>(aOutput);
  SendData(Serialize<TOutput>(aOutput));
end;

end.
