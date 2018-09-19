unit AndroidSpeech.Impl;

interface

uses
  Sysutils, System.IOUtils,
  Ultraware.Grpc,
  AndroidSpeech.grpc, Grijjy.Http2, Speech.General;

type
  TSpeechService_Impl = class(TBaseGrpcImplementation, ISpeechService_Server)
  protected
    class var FSpeechAPI: ISpeechAPI;
    class function GetSpeechAPI: ISpeechAPI; static;
  protected
    {ISpeechService_Client}
    function  UploadFile(const aFile: TFile): TText;
    function  Ping(const aTime: TTime): TTime;
    procedure StreamFile(const aInputStream: IStreamFile_Server_Recv; const aOutputStream: IStreamFile_Result_Send);
  public
    class function HandleGRPC(const aPath: string; const aIn: TBytes; const aOut: TGrpcCallback; const aErrorCallback: TGrpcErrorCallback; out aCallThread: IRequestThread): Boolean; override;
    class property SpeechAPI: ISpeechAPI read GetSpeechAPI write FSpeechAPI;
  end;

implementation

uses
  System.Types, System.Math, Classes,
  //Wave,
  System.Diagnostics;

{ TSpeechService_Impl }

class function TSpeechService_Impl.HandleGRPC(const aPath: string; const aIn: TBytes; const aOut: TGrpcCallback; const aErrorCallback: TGrpcErrorCallback;
  out aCallThread: IRequestThread): Boolean;
var
  f: TFile;
  t: TText;
  t2: TTime;
  impl: ISpeechService_Server;
  StreamFile_Server_Recv: IStreamFile_Server_Recv;
  StreamFile_Send: IStreamFile_Result_Send;
begin
  Result := False;

  if aPath = C_TestService_Path + 'UploadFile' then
  begin
    Result := True;
    f.Deserialize(aIn);

    impl := TSpeechService_Impl.Create;
    t := impl.UploadFile(f);
    aOut(t.Serialize, True);
  end
  else if aPath = C_TestService_Path + 'Ping' then
  begin
    Result := True;
    t2.Deserialize(aIn);
    impl := TSpeechService_Impl.Create;
    t2 := impl.Ping(t2);
    aOut(t2.Serialize, True);
  end
  else if aPath = C_TestService_Path + 'StreamFile' then
  begin
    Result := True;
    StreamFile_Server_Recv := TStreamFile_Server_Recv.Create;
    StreamFile_Server_Recv.AddReceivedData(aIn);
    StreamFile_Send := TStreamFile_Server_Send.Create(aOut, aErrorCallback);
    aCallThread := TGrpcStreamCallThread.Create(StreamFile_Server_Recv,
      procedure
      var impl: ISpeechService_Server;
      begin
        impl := TSpeechService_Impl.Create;
        impl.StreamFile(StreamFile_Server_Recv, StreamFile_Send);
      end);
  end;
end;

function TSpeechService_Impl.Ping(const aTime: TTime): TTime;
var watch: TStopwatch;
begin
  watch := TStopwatch.Create;
  watch.Start;
  Sleep(aTime.msec);
  watch.Stop;
  Result.hour := 0;
  Result.sec  := 0;
  Result.msec := watch.ElapsedMilliseconds;
end;

function ExtractFileName(const aFile: string): string;
var parts: TArray<string>;
begin
  parts := aFile.Split(['/']);
  Result := parts[Length(parts)-1];
end;

class function TSpeechService_Impl.GetSpeechAPI: ISpeechAPI;
begin
  Assert(FSpeechAPI <> nil);
  Result := FSpeechAPI;
end;

procedure TSpeechService_Impl.StreamFile(const aInputStream: IStreamFile_Server_Recv; const aOutputStream: IStreamFile_Result_Send);

  function _WaitForData(out aFile: TFile): Boolean;
  begin
    Result := False;
    repeat
      case aInputStream.Recv(aFile, 10) of
        wrData:               Exit(True);
        TGrpcWaitResult.wrTimeout, wrNoData:  Continue;
        wrClosed:             Exit;
      end;
    until False;
  end;

var
  f: TFile;
  t: TText;
  speechSession: ISpeechSession;
  wait: TBoolean;
  strm: TMemoryStream;
//  w: TWave;
  b: TBytes;
begin
  //wait for first data
  if not _WaitForData(f) then
    Exit;

  wait := TBoolean.Create;
  strm := TMemoryStream.Create;
  try
    wait.Value := False;
    if f.sampleRate <= 0 then
      f.sampleRate := 16000;

    speechSession := SpeechAPI.CreateSpeechSession(
      procedure(const aResult: string; aEndOfUtterance: boolean)
      var t: TText;
      begin
        t.Text := aResult;
        aOutputStream.Send(t);

        if aEndOfUtterance then
        begin
          aOutputStream.CloseSend;
          wait.Value := True;
        end;
      end,
      f.SampleRate, True{interim result});

    repeat
      strm.WriteData(f.data, Length(f.data));
      speechSession.SendRawAudio(f.data);
      if not _WaitForData(f) then
        break;
    until False;

    speechSession.CloseSend;
//    {$MESSAGE WARN 'TODO: disable raw audio logging?'}
//    strm.Position := 0;
//    w := TWave.Create(1, f.sampleRate);
//    SetLength(b, strm.Size);
//    strm.Read(b, strm.Size);
//    w.SaveWaveFile(FormatDateTime('yyyymmdd_hhmmss', now) + '.wav', b);
//    strm.SaveToFile(FormatDateTime('yyyymmdd_hhmmss', now) + '.raw');

    wait.Wait(5*1000, True);
  finally
    aOutputStream.CloseSend;
    wait.Free;
    strm.Free;
  end;
end;

function TSpeechService_Impl.UploadFile(const aFile: TFile): TText;
var
  speechSession: ISpeechSession;
  wait: TBoolean;
  sResult: string;
begin
  Writeln(aFile.filename);
  System.IOUtils.TFile.WriteAllBytes( ExtractFileName(aFile.filename), aFile.data);

  wait := TBoolean.Create;
  try
    wait.Value := False;
    speechSession := SpeechAPI.CreateSpeechSession(
      procedure(const aResult: string; aEndOfUtterance: boolean)
      begin
        wait.Value := True;
        sResult := aResult;
      end,
      IfThen(aFile.SampleRate <= 0, 16000, aFile.SampleRate), False {single result});
    speechSession.SendRawAudio(aFile.data);
    speechSession.CloseSend;
    wait.Wait(15 * 1000, True);
  finally
    speechSession := nil;
    wait.Free;
  end;

  Result.Text := sResult;
  //Writeln(sResult);
end;

end.
