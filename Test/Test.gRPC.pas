unit Test.gRPC;

interface

//{$DEFINE WebSockets}

uses
  TestFramework,
  TestService.grpc, TestService.proto, TestService.client, TestService.impl,
  {$IFDEF WebSockets}
  Ultraware.Grpc.Ws,
  {$ENDIF}
  Ultraware.Grpc;

type
  TestTTestService_Client = class(TTestCase)
  protected
    class var FGrpcServer: TGrpcServer;
    {$IFDEF WebSockets}
    class var FWsServer: TGrpcWsServer;
    {$ENDIF}
  protected
    FUseWebsocket: Boolean;
    FClient: ITestService_Client;
    FLog: string;
    procedure Log(const aText: string);
    function Client: ITestService_Client;

    procedure InternalTestSync;
    procedure InternalTestReceiveStream;
    procedure InternalTestSendStream;
    procedure InternalTestSendReceiveStreams;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    class destructor Destroy;
  end;

  TestTTestService_Http2_Client = class(TestTTestService_Client)
  published
    procedure TestSync;
    procedure TestReceiveStream;
    procedure TestSendStream;
    procedure TestSendReceiveStreams;
  end;

  {$IFDEF WebSockets}
  TestTTestService_WS_Client = class(TestTTestService_Client)
  strict private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWSSync;
    procedure TestWSReceiveStream;
    procedure TestWSSendStream;
    procedure TestWSSendReceiveStreams;
  end;
  {$ENDIF}

implementation

uses
  System.DateUtils, System.SysUtils, Grijjy.Http2;

function TestTTestService_Client.Client: ITestService_Client;
begin
  if FClient = nil then
  begin
    {$IFDEF WebSockets}
    if FUseWebsocket then
      FClient := TTestService_Client.Create(TGrpcWsClient.Create('localhost', 1001))
    else
    {$ENDIF}
      FClient := TTestService_Client.Create(TGrpcHttp2Client.Create('localhost', 1000));
  end;

  Result := FClient;
end;

class destructor TestTTestService_Client.Destroy;
begin
  FGrpcServer.Stop;
  //FWsServer.Stop;

  FGrpcServer.Free;
  {$IFDEF WebSockets}
  FWsServer.Free;
  {$ENDIF}
end;

procedure TestTTestService_Client.Log(const aText: string);
begin
  FLog := FLog + aText + #13#10;
end;

procedure TestTTestService_Client.SetUp;
begin
  FUseWebsocket := False;

  if FGrpcServer = nil then
  begin
    FGrpcServer := TGrpcServer.Create(''{any}, 1000);
    FGrpcServer.RegisterImplementation(C_TestService_Path, TTestService_Impl);
    FGrpcServer.StartListen;
  end;

  {$IFDEF WebSockets}
  if FWsServer = nil then
  begin
    FWsServer := TGrpcWsServer.Create('', 1001);
    FWsServer.RegisterImplementation(C_TestService_Path, TTestService_Impl);
    FWsServer.StartListen;
  end;
  {$ENDIF}

  Sleep(10);
end;

procedure TestTTestService_Client.TearDown;
begin
  FClient := nil;
end;

procedure TestTTestService_Client.InternalTestSync;
var
  t: TTime;
  tStart: TDatetime;
begin
  t.sec  := 0;
  t.msec := 1;
  t := Client.Sleep(t);

  tStart := Now;
  t.msec := 100;
  t := Client.Sleep(t);
  CheckTrue( (MilliSecondsBetween(Now, tStart) >= 100) and (MilliSecondsBetween(Now, tStart) < 300) );
end;

procedure TestTTestService_Client.InternalTestSendStream;
var
  strm: ICalcSumStream;
  t: TTime;
begin
  strm := Client.CalcSum;
  t.sec := 1;
  strm.Send(t);
  t.sec := 2;
  strm.Send(t);
  Check( strm.CloseAndRecv(t) );
  CheckEquals(1+2, t.sec);
end;

procedure TestTTestService_Client.InternalTestReceiveStream;
var
  t: TTime;
  tStart: TDatetime;
  lock: TBoolean;
begin
  FLog   := '';
  tStart := Now;
  t.sec  := 0;
  t.msec := 30;
  lock   := TBoolean.Create;
  try
    lock.Value := False;

    Client.CountDown(t,
      procedure(const aTime: TTime; aHasData, aClosed: Boolean)
      begin
        if aHasData then
          Log(Format('%d:%s', [aTime.msec, aTime.msg]));
        if aClosed then
        begin
          Log('DONE');
          lock.Value := True;
        end;
      end);

    lock.Wait(10 * 1000, True{needed for WS!});
    CheckEquals('3:counting down...'#13#10'2:counting down...'#13#10'1:counting down...'#13#10'0:counting DONE'#13#10'DONE'#13#10, Flog);
    CheckTrue( (MilliSecondsBetween(Now, tStart) >= 30) and (MilliSecondsBetween(Now, tStart) < 500) );
  finally
    lock.Free;
  end;
end;

procedure TestTTestService_Client.InternalTestSendReceiveStreams;
var
  strm: IUpdateableCountDown_Send;
  t: TTime;
  tStart: TDatetime;
  lock: TBoolean;
  spart: string;
const
  C_first_part = '10:counting down...'#$D#$A'9:counting down...'#$D#$A'8:counting down...'#$D#$A'7:counting down...'#$D#$A'6:counting down...'#$D#$A'5:counting down...';
  C_second_part = '10:counting down...'#$D#$A'9:counting down...'#$D#$A'8:counting down...'#$D#$A'7:counting down...'#$D#$A'6:counting down...'#$D#$A'5:counting down...'#$D#$A'4:counting down...'#$D#$A'3:counting down...'#$D#$A'2:counting down...'#$D#$A'1:counting down...'#$D#$A'0:counting DONE'#$D#$A'DONE'#$D#$A;
begin
  FLog   := '';
  tStart := Now;
  t.sec  := 0;
  t.msec := 100;
  lock   := TBoolean.Create;
  try
    lock.Value := False;

    strm := Client.UpdateableCountDown(
        procedure(const aTime: TTime; aHasData, aClosed: Boolean)
        begin
          if aHasData then
            Log(Format('%d:%s', [aTime.msec, aTime.msg]));
          if (aTime.msec <= 5) and
             (t.msec > 0) then
          begin
            strm.Send(t);
            t.msec := 0;
          end;

          if aClosed then
          begin
            Log('DONE');
            lock.Value := True;
          end;
        end);

    t.msec := 100;
    strm.Send(t);
    lock.Wait(40 * 1000, True{needed for WS!});

    spart := FLog.Substring(0, Length(C_first_part));
    CheckEquals(C_first_part, spart);
    spart := FLog.Substring(Length(FLog) - Length(C_second_part), Length(C_second_part));
    CheckEquals(C_second_part, spart);
    CheckTrue( (MilliSecondsBetween(Now, tStart) >= 100 + 50) and (MilliSecondsBetween(Now, tStart) < 800) );
  finally
    lock.Free;
  end;
end;

{$IFDEF WebSockets}

{ TestTTestService_WS_Client }

procedure TestTTestService_WS_Client.SetUp;
begin
  inherited SetUp;
end;

procedure TestTTestService_WS_Client.TearDown;
begin
  inherited;
end;

procedure TestTTestService_WS_Client.TestWSReceiveStream;
begin
  FUseWebsocket := True;
  inherited InternalTestReceiveStream;
end;

procedure TestTTestService_WS_Client.TestWSSendReceiveStreams;
begin
  FUseWebsocket := True;
  inherited InternalTestSendReceiveStreams;
end;

procedure TestTTestService_WS_Client.TestWSSendStream;
begin
  FUseWebsocket := True;
  inherited InternalTestSendStream;
end;

procedure TestTTestService_WS_Client.TestWSSync;
begin
  FUseWebsocket := True;
  inherited InternalTestSync;
end;

{$ENDIF}

{ TestTTestService_Http2_Client }

procedure TestTTestService_Http2_Client.TestReceiveStream;
begin
  FUseWebsocket := False;
  inherited InternalTestReceiveStream;
end;

procedure TestTTestService_Http2_Client.TestSendReceiveStreams;
begin
  FUseWebsocket := False;
  inherited InternalTestSendReceiveStreams;
end;

procedure TestTTestService_Http2_Client.TestSendStream;
begin
  FUseWebsocket := False;
  inherited InternalTestSendStream;
end;

procedure TestTTestService_Http2_Client.TestSync;
begin
  FUseWebsocket := False;
  inherited InternalTestSync;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTTestService_Http2_Client.Suite);
  {$IFDEF WebSockets}
  RegisterTest(TestTTestService_WS_Client.Suite);
  {$ENDIF}
end.

