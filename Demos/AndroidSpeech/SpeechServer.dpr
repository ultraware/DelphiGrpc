program SpeechServer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Ultraware.Grpc.ws in '..\..\gRPC\Ultraware.Grpc.ws.pas',
  System.Classes,
  IdStack,
  AndroidSpeech.Grpc in 'AndroidSpeech.Grpc.pas',
  AndroidSpeech.Impl in 'AndroidSpeech.Impl.pas',
  dAudioProcessing in 'dAudioProcessing.pas' {dmAudioProcessing: TDataModule},
  google.cloud.speech.v1.Speech in 'google.cloud.speech.v1.Speech.pas',
  Speech.Google in 'Speech.Google.pas',
  Speech.General in 'Speech.General.pas',
  VCL.Forms,
  dTethering in 'dTethering.pas' {dmTethering: TDataModule};

var
 _WsServer: TGrpcWsServer;
 str: TStrings; s: string;
begin
  try
    dmTethering := TdmTethering.Create(nil);

    _WsServer := TGrpcWsServer.Create('', 1001);
    _WsServer.RegisterImplementation(C_TestService_Path, TSpeechService_Impl);
    _WsServer.StartListen;
    Writeln('gRPC WS server ready on *:1001');

    str := TStringlist.Create;
    GStack.AddLocalAddressesToList(str);
    for s in str do
      Writeln('Server IP: ' + s);

    if str.Count > 0 then
      dmTethering.ServerIp := str[str.Count-1];
    dmTethering.EnableServer;

    TSpeechService_Impl.SpeechAPI := TGoogleSpeechAPI.Create;
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
