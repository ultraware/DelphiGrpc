unit TestService.impl;

interface

uses
  Sysutils,
  Ultraware.Grpc,
  TestService.grpc, TestService.proto, Grijjy.Http2;

type
  TTestService_Impl = class(TBaseGrpcImplementation, ITestService_Server)
  protected
    {ITestService_Server}
	  function  Sleep(const aTime: TTime): TTime;
	  procedure CountDown(const aTime: TTime; const aOutputStream: ICountDown_Send);
  	function  CalcSum(const aInputStream: ICalcSum_Recv): TTime;
    procedure UpdateableCountDown(const aInputStream: IUpdateableCountDown_Recv; const aOutputStream: IUpdateableCountDown_Send);
  public
    class function HandleGRPC(const aPath: string; const aIn: TBytes; const aOut: TGrpcCallback; const aErrorCallback: TGrpcErrorCallback; out aCallThread: IRequestThread): Boolean; override;
  end;

implementation

{ TTestService_Impl }

class function TTestService_Impl.HandleGRPC(const aPath: string; const aIn: TBytes; const aOut: TGrpcCallback; const aErrorCallback: TGrpcErrorCallback;
  out aCallThread: IRequestThread): Boolean;
var
  t: TTime;
  impl: ITestService_Server;
  strm: ICalcSum_Recv;
  updateableCountDown_Recv: IUpdateableCountDown_Recv;
  updateableCountDown_Send: IUpdateableCountDown_Send;
begin
  Result := False;

  if aPath = C_TestService_Path + 'Sleep' then
  begin
    Result := True;
    t.Deserialize(aIn);

    impl := TTestService_Impl.Create;
    t := impl.Sleep(t);
    aOut(t.Serialize(), True);
  end
  else if aPath = C_TestService_Path + 'CountDown' then
  begin
    Result := True;
    t.Deserialize(aIn);
    aCallThread := TGrpcCallThread.Create(
      procedure
      var
        impl: ITestService_Server;
        send: ICountDown_Send;
      begin
        impl := TTestService_Impl.Create;
        send := TCountDown_Send.Create(aOut, aErrorCallback);
        impl.CountDown(t, send);
        impl := nil;
        send := nil;
      end);
  end
  else if aPath = C_TestService_Path + 'CalcSum' then
  begin
    Result := True;
    strm := TCalcSum_Recv.Create();
    strm.AddReceivedData(aIn);
    aCallThread := TGrpcStreamCallThread.Create(strm,
      procedure
      var
        impl: ITestService_Server;
        t: TTime;
      begin
        impl := TTestService_Impl.Create;
        t := impl.CalcSum(strm);
        aOut(t.Serialize(), True);
      end);
  end
  else if aPath = C_TestService_Path + 'UpdateableCountDown' then
  begin
    Result := True;
    updateableCountDown_Recv := TUpdateableCountDown_Recv.Create;
    updateableCountDown_Recv.AddReceivedData(aIn);
    updateableCountDown_Send := TupdateableCountDown_Send.Create(aOut, aErrorCallback);
    aCallThread := TGrpcStreamCallThread.Create(updateableCountDown_Recv,
      procedure
      var impl: ITestService_Server;
      begin
        impl := TTestService_Impl.Create;
        impl.UpdateableCountDown(updateableCountDown_Recv, updateableCountDown_Send);
      end);
  end
  else
    Assert(False, 'Unknown path: ' + aPath);
end;

function TTestService_Impl.Sleep(const aTime: TTime): TTime;
begin
  SysUtils.Sleep( (aTime.sec * 1000) + aTime.msec );
  Result.sec  := aTime.sec;
  Result.msec := aTime.msec;
  Result.msg  := 'Slept';
end;

procedure TTestService_Impl.CountDown(const aTime: TTime; const aOutputStream: ICountDown_Send);
var
  i: Integer;
  t: TTime;
begin
  if aTime.sec > 0 then
  for i := aTime.sec downto 0 do
  begin
    t.sec  := i;
    t.msec := 0;
		if i > 0 then
      t.msg := 'counting down...'
    else
      t.msg := 'counting DONE';
  	aOutputStream.Send(t);
		SysUtils.Sleep(1 * 1000)
  end;

  if aTime.msec > 0 then
  for i := aTime.msec div 10 downto 0 do
  begin
    t.sec  := 0;
    t.msec := i;
		if i > 0 then
      t.msg := 'counting down...'
    else
      t.msg := 'counting DONE';
  	aOutputStream.Send(t);
		SysUtils.Sleep(10);
  end;

  aOutputStream.CloseSend;
end;

function TTestService_Impl.CalcSum(const aInputStream: ICalcSum_Recv): TTime;
var t: TTime;
begin
  Result.sec := 0;
  Result.msec := 0;
  repeat
    case aInputStream.Recv(t, 1 * 1000) of
      wrTimeout, wrNoData:  Continue;
      wrData:               Inc(Result.sec, t.sec);
      wrClosed:             Break;
    end;
  until False;
  Result.msg := 'Total';
end;

procedure TTestService_Impl.UpdateableCountDown(const aInputStream: IUpdateableCountDown_Recv;
  const aOutputStream: IUpdateableCountDown_Send);
var
  i: Integer;
  t: TTime;
begin
  //wait for first data
  repeat
    case aInputStream.Recv(t, 1000) of
      wrData:               Break;
      wrTimeout, wrNoData:  Continue;
      wrClosed:             Exit;
    end;
  until False;

  try
    repeat
      if t.sec > 0 then
      for i := t.sec downto 0 do
      begin
        t.sec := i;
        if i > 0 then
          t.msg := 'counting down...'
        else
          t.msg := 'counting DONE';
        aOutputStream.Send(t);
        SysUtils.Sleep(1 * 1000);

        case aInputStream.Recv(t, 1) of
          wrData:               Break;      //reset
          //wrTimeout, wrNoData:  Continue;
          wrClosed:             Exit;
        end;
        if i = 0 then
          Exit;
      end;

      if t.msec > 0 then
      for i := t.msec div 10 downto 0 do
      begin
        t.sec  := 0;
        t.msec := i;
        if i > 0 then
          t.msg := 'counting down...'
        else
          t.msg := 'counting DONE';
        aOutputStream.Send(t);
        SysUtils.Sleep(10);

        case aInputStream.Recv(t, 1) of
          wrData:               Break;      //reset
          //wrTimeout, wrNoData:  Continue;
          wrClosed:             Exit;
        end;
        if i = 0 then
          Exit;
      end;

    until False;
  finally
    aOutputStream.CloseSend;
  end;
end;

end.
