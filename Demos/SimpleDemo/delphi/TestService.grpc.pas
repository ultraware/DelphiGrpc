unit TestService.grpc;

interface

uses
  Grijjy.Http2, Ultraware.Grpc, TestService.proto;

const
  C_TestService_Path = '/testservice.TestService/';

type
  TTimeCallback = reference to procedure(const aTime: TTime; aHasData, aClosed: Boolean);

  ICalcSumStream = interface
    ['{8CC4B307-C17B-4023-A4C4-A5D860C207B2}']
    procedure Send(const aTime: TTime);
    function  CloseAndRecv(out aResult: TTime): Boolean;
  end;

  IUpdateableCountDown_Recv = interface(IGrpcMemStream)
    ['{2189D418-EE2B-4659-A283-01B589341911}']
    function  Recv(out aTime: TTime; aWaitTimeout: Integer): TGrpcWaitResult;
  end;
  IUpdateableCountDown_Send = interface
    ['{9CA82679-0414-413C-B7A1-7C99C23CAC7C}']
    procedure Send(const aTime: TTime);
    procedure CloseSend;
  end;

  ITestService_Client = interface
    ['{0237EED8-A595-470F-99C3-1C051743760A}']
	  function  Sleep(const aTime: TTime): TTime;
	  procedure CountDown(const aTime: TTime; const aResponseCallback: TTimeCallback);
  	function  CalcSum: ICalcSumStream;
    function  UpdateableCountDown(const aResponseCallback: TTimeCallback): IUpdateableCountDown_Send;
  end;

  //------------------------------------------

  ICountDown_Send = interface
    ['{FBB9887B-0C15-419D-9D5D-849373E20687}']
    procedure Send(const aTime: TTime);
    procedure CloseSend;
  end;

  ICalcSum_Recv = interface(IGrpcMemStream)
    ['{B46FD75E-965C-4EE2-857C-A65DA4BF803C}']
    function  Recv(out aTime: TTime; aWaitTimeout: Integer): TGrpcWaitResult;
  end;

  ITestService_Server = interface
    ['{029D569D-274F-4667-AC19-ED8C3ACBCD33}']
	  function  Sleep(const aTime: TTime): TTime;
	  procedure CountDown(const aTime: TTime; const aOutputStream: ICountDown_Send);
  	function  CalcSum(const aInputStream: ICalcSum_Recv): TTime;
    procedure UpdateableCountDown(const aInputStream: IUpdateableCountDown_Recv; const aOutputStream: IUpdateableCountDown_Send);
  end;

  //==========================================

  //stream in, one result back
  TCalcSumStream = class(TGrpcStream, ICalcSumStream)
  protected
    {ICalcSumStream}
    procedure Send(const aTime: TTime);
    function  CloseAndRecv(out aResult: TTime): Boolean;
  end;

  TCountDown_Send = class(TBaseGrpcCallbackStream, ICountDown_Send)
  protected
    {ICountDown_Send}
    procedure Send(const aTime: TTime);
    procedure CloseSend;
  end;

  TCalcSum_Recv = class(TBaseGrpcMemStream, ICalcSum_Recv)
  protected
    {ICalcSum_Recv}
    function  Recv(out aTime: TTime; aWaitTimeout: Integer): TGrpcWaitResult;
  end;

  TUpdateableCountDown_Send = class(TBaseGrpcCallbackStream, IUpdateableCountDown_Send)
  protected
    {IUpdateableCountDown_Send}
    procedure Send(const aTime: TTime);
    procedure CloseSend;
  end;

  TUpdateableCountDown_Http2Send = class(TGrpcStream, IUpdateableCountDown_Send)
  protected
    {IUpdateableCountDown_Send}
    procedure Send(const aTime: TTime);
    procedure CloseSend;
  end;

  TUpdateableCountDown_Recv = class(TBaseGrpcMemStream, IUpdateableCountDown_Recv)
  protected
    {IUpdateableCountDown_Recv}
    function  Recv(out aTime: TTime; aWaitTimeout: Integer): TGrpcWaitResult;
  end;

implementation

uses
  SysUtils;

{ TCalcSumStream }

function TCalcSumStream.CloseAndRecv(out aResult: TTime): Boolean;
var data: TBytes;
begin
  Result := Stream.CloseAndRecvData(data);
  if Result then
    aResult.Deserialize(data);
end;

procedure TCalcSumStream.Send(const aTime: TTime);
begin
  Stream.SendData( aTime.Serialize() );
end;

{ TCountDown_Send }

procedure TCountDown_Send.CloseSend;
begin
  inherited Close;
end;

procedure TCountDown_Send.Send(const aTime: TTime);
begin
  inherited SendData(aTime.Serialize);
end;

{ TCalcSum_Recv }

function TCalcSum_Recv.Recv(out aTime: TTime; aWaitTimeout: Integer): TGrpcWaitResult;
var b: TBytes;
begin
  Result := inherited Recv(b, aWaitTimeout);
  if Result = wrData then
    aTime.Deserialize(b);
end;

{ TUpdateableCountDown_Send }

procedure TUpdateableCountDown_Send.CloseSend;
begin
  inherited Close;
end;

procedure TUpdateableCountDown_Send.Send(const aTime: TTime);
begin
  inherited SendData(aTime.Serialize);
end;

{ TUpdateableCountDown_Recv }

function TUpdateableCountDown_Recv.Recv(out aTime: TTime; aWaitTimeout: Integer): TGrpcWaitResult;
var b: TBytes;
begin
  Result := inherited Recv(b, aWaitTimeout);
  if Result = wrData then
    aTime.Deserialize(b);
end;

{ TUpdateableCountDown_Http2Send }

procedure TUpdateableCountDown_Http2Send.CloseSend;
begin
  Stream.DoCloseSend;
end;

procedure TUpdateableCountDown_Http2Send.Send(const aTime: TTime);
begin
  Stream.SendData(aTime.Serialize);
end;

end.
