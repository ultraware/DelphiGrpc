unit Unit5;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Speech.Google, Speech.General;
  //Grijjy.Http, Google.API;

type
  TForm5 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
//    g: TgoGoogle;
//    function Client: TgoHttpClient;
//    function Token: string;
    FSpeechAPI: ISpeechAPI;
    function SpeechAPI: ISpeechAPI;
  public
  end;

var
  Form5: TForm5;

implementation

uses
  SuperObject, System.IOUtils, System.SyncObjs,
  google.cloud.speech.v1.Speech;

{$R *.dfm}

procedure TForm5.FormCreate(Sender: TObject);
begin
  RegisterLogger(
    procedure(const aData: string)
    begin
      //not threadsafe, so queue to mainthread
      TThread.Queue(nil,
        procedure
        begin
          Memo1.Lines.Add(aData);
        end);
    end);
end;

procedure TForm5.Button1Click(Sender: TObject);
var
  speechSession: ISpeechSession;
  wait: TEvent;
  sResult: string;
begin
  wait := TEvent.Create;
  try
    //note: for a streaming demo, see this function
    // https://github.com/ultraware/DelphiGrpc/blob/master/Demos/AndroidSpeech/AndroidSpeech.Impl.pas#L104

    //create session with inline callback
    speechSession := SpeechAPI.CreateSpeechSession(
      procedure(const aResult: string; aEndOfUtterance: boolean)
      begin
        wait.SetEvent;
        sResult := aResult;
      end,
      16000, False {single result});

    //upload file
    speechSession.SendRawAudio( TFile.ReadAllBytes(Edit1.Text) );   //16bit mono 16khz raw audio (PCM S16 LE (s16l))
    speechSession.CloseSend;

    //wait for callback
    wait.WaitFor(5 * 1000);
  finally
    speechSession := nil;
    wait.Free;
  end;

  ShowMessage(sResult);
end;

function TForm5.SpeechAPI: ISpeechAPI;
begin
  if FSpeechAPI = nil then
    FSpeechAPI := TGoogleSpeechAPI.Create;
  Result := FSpeechAPI;
end;

end.
