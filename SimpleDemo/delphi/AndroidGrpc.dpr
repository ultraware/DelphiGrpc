program AndroidGrpc;

uses
  System.StartUpCopy,
  FMX.Forms,
  Mobile in 'Mobile.pas' {Form4},
  Ultraware.Grpc.ws in '..\..\Ultraware.Grpc.ws.pas',
  Grijjy.ProtocolBuffers in '..\..\GrijjyFoundation\Grijjy.ProtocolBuffers.pas',
  Grijjy.SysUtils in '..\..\GrijjyFoundation\Grijjy.SysUtils.pas',
  Grijjy.Collections in '..\..\GrijjyFoundation\Grijjy.Collections.pas',
  Nghttp2 in '..\..\ngHttp2\Nghttp2.pas',
  TestService.client in 'TestService.client.pas',
  TestService.grpc in 'TestService.grpc.pas',
  TestService.proto in 'TestService.proto.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
