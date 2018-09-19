unit TestService.client;

interface

uses
  Ultraware.Grpc,
  TestService.proto, TestService.grpc;

type
  TTestService_Client = class(TGrpcClientHandler, ITestService_Client)
  protected
    {ITestService_Client}
	  function  Sleep(const aTime: TTime): TTime;
	  procedure CountDown(const aTime: TTime; const aResponseCallback: TTimeCallback);
  	function  CalcSum: ICalcSumStream;
    function  UpdateableCountDown(const aResponseCallback: TTimeCallback): IUpdateableCountDown_Send;
  end;

implementation

uses
  SysUtils, Grijjy.Http2;

{ TTestService_Client }

function TTestService_Client.Sleep(const aTime: TTime): TTime;
var
  recv: TBytes;
begin
  if Client.DoRequest(aTime.Serialize(), C_TestService_Path + 'Sleep', recv) then
    Result.Deserialize(recv);
end;

function TTestService_Client.UpdateableCountDown(const aResponseCallback: TTimeCallback): IUpdateableCountDown_Send;
var
  request: IGrpcStream;
  callback: TGrpcCallback;
begin
  if Assigned(aResponseCallback) then
    callback :=
      procedure(const aData: TBytes; aIsStreamClosed: Boolean)
      var
        t: TTime;
      begin
        if aData <> nil then
          t.Deserialize(aData);
        aResponseCallback(t, aData <> nil, aIsStreamClosed);
      end
  else
    callback := nil;

  request := Client.DoRequest(nil, C_TestService_Path + 'UpdateableCountDown', callback);
  Result  := TUpdateableCountDown_Http2Send.Create(request);
end;

procedure TTestService_Client.CountDown(const aTime: TTime;
  const aResponseCallback: TTimeCallback);
var
  callback: TGrpcCallback;
  stream: IGrpcStream;
begin
  if Assigned(aResponseCallback) then
    callback :=
      procedure(const aData: TBytes; aIsStreamClosed: Boolean)
      var
        t: TTime;
      begin
        if aData <> nil then
          t.Deserialize(aData);
        aResponseCallback(t, aData <> nil, aIsStreamClosed);
        if aIsStreamClosed then
          stream := nil;
      end
  else
    callback := nil;

  stream := Client.DoRequest( aTime.Serialize(), C_TestService_Path + 'CountDown', callback);
end;

function TTestService_Client.CalcSum: ICalcSumStream;
var
  request: IGrpcStream;
begin
  request := Client.DoRequest(nil, C_TestService_Path + 'CalcSum', nil);
  if request = nil then
    Exit(nil);
  Result  := TCalcSumStream.Create(request);
end;

end.
