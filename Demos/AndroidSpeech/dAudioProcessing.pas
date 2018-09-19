unit dAudioProcessing;

interface

uses
  System.SysUtils, System.Classes,
  google.cloud.speech.v1.Speech, Google.API, Speech.General,
  Vcl.ExtCtrls, Grijjy.Http2;

type
  TdmAudioProcessing = class(TDataModule)
    Timer1: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSampleRate: Integer;
    FGoogleSpeechAPI: ISpeechAPI;
    FSpeechSession: ISpeechSession;
    FResult: string;
    FDone: TBoolean;
    function  GoogleSpeechAPI: ISpeechAPI;
    procedure SendAudioBuffer(const aData: TBytes);
  protected
  public
    function TranslateData(const aData: TBytes): string;
  end;

var
  dmAudioProcessing: TdmAudioProcessing;

implementation

uses
  Ultraware.Grpc, System.IOUtils, superobject, Speech.Google;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TdmAudioProcessing.DataModuleCreate(Sender: TObject);
begin
  FDone := TBoolean.Create;
  FSampleRate := 16000;
end;

function TdmAudioProcessing.GoogleSpeechAPI: ISpeechAPI;
begin
  if FGoogleSpeechAPI = nil then
    FGoogleSpeechAPI := TGoogleSpeechAPI.Create;
  Result := FGoogleSpeechAPI;
end;

procedure TdmAudioProcessing.SendAudioBuffer(const aData: TBytes);
            begin
  FSpeechSession.SendRawAudio(aData);
end;

procedure TdmAudioProcessing.Timer1Timer(Sender: TObject);
begin
  if FSpeechSession <> nil then
    FSpeechSession.CloseSend;
end;

function TdmAudioProcessing.TranslateData(const aData: TBytes): string;
var
  b: TBytes;
begin
  FDone.Value := False;

  if FSpeechSession = nil then
    FSpeechSession := GoogleSpeechAPI.CreateSpeechSession(
      procedure(const aResult: string; aEndOfUtterance: boolean)
      begin
        FResult := aResult;
        FDone.Value := True;
      end,
      FSampleRate, False {single result});

  if (aData <> nil) then
    Exit;

  if (FSpeechSession <> nil) then
  begin
    FSpeechSession.CloseSend;
  end;

  FDone.Wait(5 * 1000);
  Result := FResult;
  FSpeechSession := nil;
end;

end.
