unit Speech.General;

interface

uses
  System.SysUtils;

type
  TSpeechResultCallback = reference to procedure(const aResult: string; aEndOfUtterance: boolean);

  ISpeechSession = interface;
  ISpeechAPI = interface
    ['{B6DAE071-75D2-48F0-9D5B-907709175347}']
    function CreateSpeechSession(const aCallback: TSpeechResultCallback; aSampleRate: Integer = 16000; aInterimResults: Boolean = false): ISpeechSession;
  end;

  ISpeechSession = interface
    ['{0624A369-DBAA-4CD6-96EA-5B10EB6CE3F7}']
    procedure SendRawAudio(const aData: TBytes);
    procedure CloseSend;
  end;

  TLogNotify = reference to procedure(const aData: string);

  procedure Log(const aData: string);
  procedure RegisterLogger(const aLogger: TLogNotify);

implementation

uses
  System.Classes;

var
  FLogger: TLogNotify;

procedure RegisterLogger(const aLogger: TLogNotify);
begin
  FLogger := aLogger;
end;

procedure Log(const aData: string);
begin
  if Assigned(FLogger) then
    FLogger(aData)
  else
    TThread.Queue(nil,
      procedure
      begin
        Writeln(aData);
      end);
end;

end.
