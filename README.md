DelphiGrpc
=========

DelphiGrpc is a Delphi implementation of the gRPC protocol (http://grpc.io).

This protocol is very efficient and fast (realtime), because it is based on:
- protobuffers: compact and binary message format (https://developers.google.com/protocol-buffers/docs/encoding)
- http/2: binary and compressed headers, multiplexing ("unlimited" requests over a single connection so no connection overhead), bi-directional and push, etc (https://developers.google.com/web/fundamentals/performance/http2/) 

Another important benefit of gRPC is that it uses a readable contract format (https://grpc.io/docs/guides/concepts.html#service-definition).

# Features
The following features are implemented: 
- both client and server (!) support
- full http/2 support, but also a simple (custom) implementation is made using websockets (for Delphi Mobile clients, and it's easy to add browser support too :) ) 
- full support for bi-directional streaming
- default up to 100 simultaneous requests over a single http/2 connection
- only Windows (http/2) and Android (websocket) platforms are implemented and tested, Linux client works with websockets
- Basic set of gRPC specification

# Missing
- SSL support (in place but not tested yet)
- No .proto importer (code generator)
- Full gRPC specification implementation (timeout and ping handling, compression, metadata, bandwidth throttling, etc.)
- Full Linux support 
- Stress and monkey tests (random connection loss handling not tested yet)

# Implementation
The following libraries are used:
- build on top of ngHttp2.dll (https://nghttp2.org/) for full http/2 communication
- using sgcWebsockets library (https://www.esegece.com/websockets) for (custom) websocket communications (no need for http/2 dll)
- using GrijjyFoundation library (https://github.com/grijjy/GrijjyFoundation) for native Delphi protobuffers support
- using DelphiScalableClientSockets library (https://github.com/grijjy/DelphiScalableClientSockets) for a Delphi wrapper around ngHttp2.dll and a scalable connection implementation (IOCP and pooling)

# Status
Although it has been used in an important research project, it is not production ready yet. 
A demo is available to show it works with the gRPC endpoint of Google Speech.
Another demo shows it works with the official Golang implementation of gRPC too.
See [Examples](#Examples) for an explanation of the Delphi client and server code. 

# gRPC over Websockets
Unfortunately, we can only use the ngHttp2.dll on Windows (and ngHttp2.so on Linux) but not on any mobile device (Android or iOS). There are some gRPC to REST proxies but none of them were suitable for our needs. 
Because gRPC uses http/2 frames, there isn't that big difference with WebSocket messages if you think about it! So our WebSocket wrapper puts every gRPC frame in a WebSocket message (using protobuffers of course), et voila! 

```
message WSRequest {
  int64 id    = 1;  //unique request id
  string path = 2;  //gRPC path, e.g. "/routeguide.RouteGuide/RouteChat"
  bool close  = 3;  //is request/stream closed?
  int64 size  = 4;  //grpc size
  bytes grpc  = 5;  //gRPC frame
}

message WSResponse {
  int64 id   = 1;  //request id 
  bool close = 2;  //has client closed the request/stream?
  int64 size = 3;  //grpc size
  bytes grpc = 4;  //gRPC frame
}
```

The WebSocket server keeps track of the request id's and when a response is received with the same id, the normal gRPC handling processes the response data for the original request (streaming or callback). 

# Examples
Some examples to show how to use it.
### Normal request/response (synchronous) call
Next is an example of a service definition with one "Sleep" call:
```
syntax = "proto3";
package testservice;

service TestService {
  rpc Sleep(Time) returns (Time) {}
}

message Time {
  int32 sec = 1;
}
```
The following Delphi interface client and server code can be made from this definition:
``` 
const
  C_TestService_Path = '/testservice.TestService/';
type
  TTime = record
  public
    [Serialize(1)] sec: UInt32;       
    
    function  Serialize: TBytes;
    procedure Deserialize(const aData: TBytes);
  end;

  ITestService = interface
	  function  Sleep(const aTime: TTime): TTime;
  end;
```
This interface can be used in Delphi client: 

```
FClient: ITestService;
function TestService: ITestService;
begin
  if FClient = nil then
    FClient := TTestService_Client.Create( TGrpcHttp2Client.Create('127.0.0.1', 1000) );
  Result := FClient;
end;

procedure Sleep(aSec: Integer);
var t: TTime;
begin
  t.sec := aSec;
  t := TestService().Sleep(t);
end;
```
The server side implementation is straight forward:
```
type
  TTestService_Impl = class(TBaseGrpcImplementation, ITestService_Server)
  protected
    function  Sleep(const aTime: TTime): TTime;
  end;
  
function TTestService_Impl.Sleep(const aTime: TTime): TTime;
begin
  SysUtils.Sleep( (aTime.sec * 1000) );
  Result.sec  := aTime.sec;
end;  
```
And we start both a http/2 and a WebSocket server for this implementation:
```
FGrpcServer: TGrpcServer;
FWsServer: TGrpcWsServer;

  FGrpcServer := TGrpcServer.Create(''{any}, 1000);
  FGrpcServer.RegisterImplementation(C_TestService_Path, TTestService_Impl);
  FGrpcServer.StartListen;
  Writeln('gRPC server ready on *:1000');

  FWsServer := TGrpcWsServer.Create('', 1001);
  FWsServer.RegisterImplementation(C_TestService_Path, TTestService_Impl);
  FWsServer.StartListen;
  Writeln('gRPC WS server ready on *:1001');
```
### Single request, streaming response (asynchronous) call
A more complex (but powerful!) call is for example this CountDown call: you start this server side function with one call and it will stream back the elapsed time each second:
```
service TestService {
  rpc CountDown(Time) returns (stream Time) {}
}
```
The Delphi interface is a bit more complicated, with a separate client and server interface:
```
type
  TTimeCallback = reference to procedure(const aTime: TTime; aHasData, aClosed: Boolean);
  
  ITestService_Client = interface
    procedure CountDown(const aTime: TTime; const aResponseCallback: TTimeCallback);
  end;

  ICountDown_Send = interface
    procedure Send(const aTime: TTime);
    procedure CloseSend;
  end;
  
  ITestService_Server = interface
	  procedure CountDown(const aTime: TTime; const aOutputStream: ICountDown_Send);
  end;
```
The Delphi client should supply a callback:
```
var
  t: TTime;
begin
  Log('Counting down...');
  t.sec := 5;
  TestService().CountDown(t,
    procedure(const aTime: TTime; aHasData, aClosed: Boolean)
    begin
      if aHasData then
        Log(Format('Count down %ds',[aTime.sec]));
      if aClosed then
        Log('Countdown DONE');
    end);
```
The server has a for loop (which will run in a separate thread), streams back the time each second and closes the request:
```
type
  TTestService_Impl = class(TBaseGrpcImplementation, ITestService_Server)
    procedure CountDown(const aTime: TTime; const aOutputStream: ICountDown_Send); 
  end;
  
procedure TTestService_Impl.CountDown(const aTime: TTime; const aOutputStream: ICountDown_Send);
var
  i: Integer;
  t: TTime;
begin
  for i := aTime.sec downto 0 do
  begin
    t.sec  := i;
  	aOutputStream.Send(t);
	SysUtils.Sleep(1 * 1000)
  end;
  aOutputStream.CloseSend;
end;  
```
### Streaming request, single response (asynchronous) call
The other way around is also possible: stream separate data parts to the server and get a single response back:
```
service TestService {
  rpc CalcSum(stream Time) returns (Time) {}
}
```
Again, the Delphi client and server interfaces are different:
```
  ICalcSumStream = interface
    procedure Send(const aTime: TTime);
    function  CloseAndRecv(out aResult: TTime): Boolean;
  end;
  
  ITestService_Client = interface
  	function  CalcSum: ICalcSumStream;
  end;

  ICalcSum_Recv = interface(IGrpcMemStream)
    function  Recv(out aTime: TTime; aWaitTimeout: Integer): TGrpcWaitResult;
  end;

  ITestService_Server = interface
  	function  CalcSum(const aInputStream: ICalcSum_Recv): TTime;
  end;
```
The client can send more data asynchronously, the server will respond when the client closes the request:
```
var
  s: string;
  strm: ICalcSumStream;
  t: TTime;
begin
  strm := TestService().CalcSum();
  while InputQuery('Amount', 'Amount', s) do
  begin
    t.sec := s.ToInteger();
    strm.Send(t);
  end;
  if strm.CloseAndRecv(t) then
    Log('Sum = ' t.sec.ToString() );
end;
```
The server will directly process each input, till the request is closed:
```
function TTestService_Impl.CalcSum(const aInputStream: ICalcSum_Recv): TTime;
var t: TTime;
begin
  Result.sec := 0;
  repeat
    case aInputStream.Recv(t, 1 * 1000) of
      wrTimeout, wrNoData:  Continue;
      wrData:               Inc(Result.sec, t.sec);
      wrClosed:             Break;
    end;
  until False;
end;
```
### Streaming request, streaming response (asynchronous) call
The most powerful and complex call is the two way streaming call, which will run untill one of parties closes the call: 
```
service TestService {
  rpc UpdateableCountDown(stream Time) returns (stream Time) {}
}
```
The interface code has multiple types to support this:
```
  TTimeCallback = reference to procedure(const aTime: TTime; aHasData, aClosed: Boolean);

  IUpdateableCountDown_Send = interface
    procedure Send(const aTime: TTime);
    procedure CloseSend;
  end;

  ITestService_Client = interface
    function  UpdateableCountDown(const aResponseCallback: TTimeCallback): IUpdateableCountDown_Send;
  end;
  
  IUpdateableCountDown_Recv = interface(IGrpcMemStream)
    function  Recv(out aTime: TTime; aWaitTimeout: Integer): TGrpcWaitResult;
  end;
  
  ITestService_Server = interface
    procedure UpdateableCountDown(const aInputStream: IUpdateableCountDown_Recv; const aOutputStream: IUpdateableCountDown_Send);
  end;  
```
The client registers a callback first and starts the call when the first amount is send:
```
var
  s: string;
  strm: IUpdateableCountDown_Send;
  t: TTime;
begin
  strm := TestService().UpdateableCountDown(
    procedure(const aTime: TTime; aHasData, aClosed: Boolean)
    begin
      if aHasData then
        Log(Format('Count down %ds',[aTime.sec]));
      if aClosed then
        Log('Countdown DONE');
    end);

  while InputQuery('Amount', 'Amount', s) do
  begin
    t.sec := s.ToInteger();
    Log('Updating: ' + s);
    strm.Send(t);
  end;
  strm.CloseSend();
end;
```
The server needs to wait for the first data and can process the next data in a wait loop:
```
procedure TTestService_Impl.UpdateableCountDown(const aInputStream: IUpdateableCountDown_Recv;
  const aOutputStream: IUpdateableCountDown_Send);
var
  i, iCount: Integer;
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
      for i := t.sec downto 0 do
      begin
        t.sec := i;
        aOutputStream.Send(t);
        SysUtils.Sleep(1 * 1000);

        case aInputStream.Recv(t, 1) of
          wrData:    Break;      //restart repeat loop
          wrClosed:  Exit;
        end;
        if i = 0 then
          Exit;
      end;
    until False;
  finally
    aOutputStream.CloseSend;
  end;
end;
```