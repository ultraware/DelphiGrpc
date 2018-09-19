program AndroidSpeech;

uses
  System.StartUpCopy,
  FMX.Forms,
  fAndroidRecord in 'fAndroidRecord.pas' {Form4},
  AndroidSpeech.Client in 'AndroidSpeech.Client.pas',
  AndroidSpeech.Grpc in 'AndroidSpeech.Grpc.pas',
  sgcWebSocket in '..\..\..\Componenten\sgcWebSockets\Source\sgcWebSocket.pas',
  dStyle in 'dStyle.pas' {dmStyle: TDataModule},
  dBaseRecorder in 'dBaseRecorder.pas' {dmBaseRecorder: TDataModule},
  dTethering in 'dTethering.pas' {dmTethering: TDataModule};
//  Wave in 'Wave.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmStyle, dmStyle);
  dmTethering := TdmTethering.Create(Application);
  dmTethering.EnableClient;
  
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
