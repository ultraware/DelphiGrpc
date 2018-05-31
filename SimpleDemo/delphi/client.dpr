program client;

uses
  System.StartUpCopy,
  FMX.Forms,
  fClient in 'fClient.pas' {frmClient},
  TestService.grpc in 'TestService.grpc.pas',
  TestService.proto in 'TestService.proto.pas',
  TestService.client in 'TestService.client.pas',
  Superobject in 'Superobject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClient, frmClient);
  Application.Run;
end.
