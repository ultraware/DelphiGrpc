unit Speech.Google;

interface

uses
  Google.API, google.cloud.Speech.v1.Speech, Grijjy.Http2, System.SysUtils, Speech.General;

type
  TGoogleSpeechAPI = class(TInterfacedObject, ISpeechAPI)
  protected
    g: TgoGoogle;
    function Token: string;
  protected
    FSpeech: ISpeech;
    function SpeechAPI: ISpeech;
  protected
    {ISpeechAPI}
    function CreateSpeechSession(const aCallback: TSpeechResultCallback; aSampleRate: Integer = 16000; aInterimResults: Boolean = false): ISpeechSession;
  end;

  TSpeechSession = class(TInterfacedObject, ISpeechSession)
  protected
    FSpeech: ISpeech;
    FCallback: TSpeechResultCallback;
    FStream: IStreamingRecognizeRequest_Send;
    FSampleRate: Integer;
    FInterimResults: Boolean;
    //FResult: string;
    //FDone: TBoolean;
    function  SpeechStream: IStreamingRecognizeRequest_Send;
  protected
    {ISpeechSession}
    procedure SendRawAudio(const aData: TBytes);
    procedure CloseSend;
  public
    constructor Create(const aSpeechAPI: ISpeech; const aCallback: TSpeechResultCallback; aSampleRate: Integer; aInterimResults: Boolean);
    destructor  Destroy; override;
  end;

implementation

uses
  superobject, System.IOUtils, Ultraware.Grpc;

{ TGoogleSpeechAPI }

function TGoogleSpeechAPI.CreateSpeechSession(const aCallback: TSpeechResultCallback; aSampleRate: Integer; aInterimResults: Boolean): ISpeechSession;
begin
  Result := TSpeechSession.Create(SpeechAPI, aCallback, aSampleRate, aInterimResults);
end;

function TGoogleSpeechAPI.SpeechAPI: ISpeech;
var
  http2client: TGrpcHttp2Client;
begin
  if FSpeech = nil then
  begin
    http2client := TGrpcHttp2Client.Create('speech.googleapis.com', 443, True);
    http2client.Http2Client.Authority := 'speech.googleapis.com:443';
    http2client.Http2Client.Authorization := Token;
    FSpeech := TSpeech_Client.Create(http2client);
  end;
  Result := FSpeech;
end;

function TGoogleSpeechAPI.Token: string;
var serviceaccount: ISuperObject;
begin
  //https://github.com/grijjy/DelphiGoogleAPI

  if g = nil then
  begin
    //note: you need to download the OAuth private key, see https://developers.google.com/identity/protocols/OAuth2ServiceAccount
    serviceaccount := SO( TFile.ReadAllText('My First Project.json') );

    g := TgoGoogle.Create;
    g.ServiceAccount := serviceaccount.S['client_email'];
    g.PrivateKey := serviceaccount.S['private_key'];
    g.OAuthScope := 'https://www.googleapis.com/auth/cloud-platform';
  end;

  if g.AccessToken = '' then
    raise Exception.Create('Could not create access token');

  Result := 'Bearer ' + g.AccessToken;
end;

{ TSpeechSession }

procedure TSpeechSession.CloseSend;
begin
  if FStream <> nil then
    FStream.CloseSend;
end;

constructor TSpeechSession.Create(const aSpeechAPI: ISpeech; const aCallback: TSpeechResultCallback; aSampleRate: Integer; aInterimResults: Boolean);
begin
  FSpeech := aSpeechAPI;
  FCallback := aCallback;
  FSampleRate := aSampleRate;
  FInterimResults := aInterimResults;
end;

destructor TSpeechSession.Destroy;
begin
  FSpeech := nil;
  FCallback := nil;
  inherited;
end;

procedure TSpeechSession.SendRawAudio(const aData: TBytes);
var
  req2: TStreamingRecognizeRequest2;
  b: TBytes;
begin
  if SpeechStream.IsRequestClosed then
    Assert(False);

  b := aData;
  while b <> nil do
  begin
    req2.audio_content := Copy(b, 0, 1024);
    b := Copy(b, 1024, Length(b));

    (SpeechStream as TGrpcStream).Stream.SendData(req2.Serialize);
  end;
end;

function TSpeechSession.SpeechStream: IStreamingRecognizeRequest_Send;
var
  config: TStreamingRecognitionConfig;
  req: TStreamingRecognizeRequest;
  lastResult: string;
begin
  if FStream = nil then
  begin
    config.single_utterance := True;
    config.interim_results := FInterimResults;
    config.config.encoding := TAudioEncoding.LINEAR16;
    config.config.max_alternatives := 1;
    config.config.sample_rate_hertz := FSampleRate; //16000;
    config.config.language_code := 'nl'; //'en-US';
    config.config.profanity_filter := False;
    config.config.enable_word_time_offsets := False;
    req.streaming_config := config;

    FStream := FSpeech.StreamingRecognize(
      req,
      procedure(const aStreamingRecognizeResponse: TStreamingRecognizeResponse; aHasData, aClosed: Boolean)
      var
        resp: TStreamingRecognizeResponse;
        r: TStreamingRecognitionResult; a: TSpeechRecognitionAlternative;
        sResult: string;
        bFinal: Boolean;
      begin
        resp := aStreamingRecognizeResponse;
        case resp.speech_event_type of
          SPEECH_EVENT_UNSPECIFIED: Log('SPEECH_EVENT_UNSPECIFIED');
          END_OF_SINGLE_UTTERANCE : Log('END_OF_SINGLE_UTTERANCE');
        else
          Assert(False);
        end;

        if (resp.error.code <> 0) and
           (resp.error.message <> '')
        then
          Log(Format('%d: %s', [resp.error.code, resp.error.message]));

        bFinal := False;
        if (resp.results <> nil) then
        begin
          sResult := resp.results[0].alternatives[0].transcript;
          lastResult := sResult;

          for r in resp.results do
          begin
            for a in r.alternatives do
            begin
              Log(Format('Final: %s = %s (%2.1f%%)', [BoolToStr(r.is_final, True{full}), a.transcript, a.confidence * 100]));
              bFinal := r.is_final;
              Break;
            end;
            Break;
          end;
        end
        else
          sResult := lastResult;

        FCallback(sResult, {(resp.speech_event_type = END_OF_SINGLE_UTTERANCE)}bFinal or aClosed);
      end);
  end;
  Result := FStream;
end;

initialization
  //test if token can be made
  with TGoogleSpeechAPI.Create do
  try
    Token();
  finally
    Free;
  end;

end.
