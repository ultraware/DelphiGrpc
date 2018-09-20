program server;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  IdStack, // in 'c:\program files (x86)\embarcadero\studio\18.0\source\Indy10\System\IdStack.pas';
  TestService.impl in 'TestService.impl.pas',
  TestService.grpc in 'TestService.grpc.pas',
  Ultraware.Grpc in '..\..\..\gRPC\Ultraware.Grpc.pas',
  Ultraware.Grpc.ws in '..\..\gRPC\Ultraware.Grpc.ws.pas';

var
 _GrpcServer: TGrpcServer;
 _WsServer: TGrpcWsServer;
 str: TStrings; s: string;
begin
  try
    _GrpcServer := TGrpcServer.Create(''{any}, 1000);
    _GrpcServer.RegisterImplementation(C_TestService_Path, TTestService_Impl);
    _GrpcServer.StartListen;
    Writeln('gRPC server ready on *:1000');

    _WsServer := TGrpcWsServer.Create('', 1001);
    _WsServer.RegisterImplementation(C_TestService_Path, TTestService_Impl);
    _WsServer.StartListen;
    Writeln('gRPC WS server ready on *:1001');

    str := TStringlist.Create;
    TIdStack.IncUsage;
    GStack.AddLocalAddressesToList(str);
    for s in str do
      Writeln('Server IP: ' + s);

    //WS needs mainthread?
    repeat
      CheckSynchronize(10);
    until False;
    //Readln(Input);
  except
    on E: Exception do
      Writeln(E.ClassName + ': ' + E.Message);
  end;
end.
