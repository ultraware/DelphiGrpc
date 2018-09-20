program RealtimeMoveServer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  System.SysUtils,
  demoservice.Server in 'demoservice.Server.pas',
  Ultraware.Grpc.Server in '..\..\Ultraware.Grpc.Server.pas',
  demoservice.Impl in 'demoservice.Impl.pas',
  demoservice.Proto in 'demoservice.Proto.pas';

var
 _GrpcServer: TGrpcServer;
// _WsServer: TGrpcWsServer;
 str: TStrings; s: string;
begin
  try
    _GrpcServer := TGrpcServer.Create(''{any}, 1000);
    _GrpcServer.RegisterImplementation(TMoveService_Server_Impl.BasePath, TMoveService_Server_Impl);
    _GrpcServer.StartListen;
    Writeln('gRPC server ready on *:1000');

//    _WsServer := TGrpcWsServer.Create('', 1001);
//    _WsServer.RegisterImplementation(C_TestService_Path, TTestService_Impl);
//    _WsServer.StartListen;
//    Writeln('gRPC WS server ready on *:1001');

    str := TStringlist.Create;
    for s in str do
      Writeln('Server IP: ' + s);

    //WS needs mainthread?
    repeat
      CheckSynchronize(10);
    until False;
    //Readln(Input);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
