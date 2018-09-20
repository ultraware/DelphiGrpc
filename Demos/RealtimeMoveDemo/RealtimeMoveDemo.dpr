program RealtimeMoveDemo;

uses
  Vcl.Forms,
  fClient in 'fClient.pas' {Form5},
  demoservice.Client in 'demoservice.Client.pas',
  Ultraware.Grpc.Client in '..\..\gRPC\Ultraware.Grpc.Client.pas',
  demoservice.Proto in 'demoservice.Proto.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
