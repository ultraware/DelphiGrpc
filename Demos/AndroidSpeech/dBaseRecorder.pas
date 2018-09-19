unit dBaseRecorder;

interface

uses
  System.SysUtils, System.Classes, AndroidSpeech.Grpc, FMX.Types;

type
  TLogType = (ltDebug, ltInfo);
  TLogCallback = procedure(const aType: TLogType; const aData: string);

  TdmBaseRecorder = class(TDataModule)
    tmrFetch: TTimer;
    procedure tmrFetchTimer(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  protected
    FOnLog: TLogCallback;
    procedure Log(const aType: TLogType; const aData: string);
  public
    property OnLog: TLogCallback read FOnLog write FOnLog;
  protected
    FSampleRate: Integer;
    FPrevData: Tbytes;
    FStream: IStreamFile_Send;
    procedure CreateStream(const aCallback: TTextCallback);
  protected
    FVUValue: Integer;
    FVUMax: Integer;
    FOnVUChanged: TNotifyEvent;
    procedure ProcessVU(const aRaw: TBytes);
    procedure SetOnVUChanged(const Value: TNotifyEvent);
  protected
    FOwnUtteranceDetected: Boolean;
    FOwnUtteranceEndDetected: Boolean;
    FSpeechStart,
    FSilenceStart: TDateTime;
    procedure ProcessAudio(const aRaw: TBytes);
  private
    FOnStateChange: TNotifyEvent;
  protected
    FClient: ISpeechService_Client;
    function Client: ISpeechService_Client;
  public
    function UploadFile(const aFile: TFile): TText;
    function Ping(const aTime: TTime): TTime;
    procedure Stream(const aFile: TFile);
    procedure StopStream;
  public
    procedure Start(const aCallback: TTextCallback); virtual;
    procedure Stop; virtual;

    property  SampleRate: Integer read FSampleRate write FSampleRate;

    function  IsRecording: Boolean;
    function  StartOfSpeech: TDateTime;
    property  OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;

    property VUValue: Integer read FVUValue write FVUValue;
    property VUMax: Integer read FVUMax write FVUMax;
    property OnVUChanged: TNotifyEvent read FOnVUChanged write SetOnVUChanged;
  end;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses System.DateUtils, AndroidSpeech.Client, Ultraware.Grpc.Ws, dTethering;

{$R *.dfm}

{ TdmBaseRecorder }

function TdmBaseRecorder.Client: ISpeechService_Client;
begin
  if FClient = nil then
  begin
    FClient := TTestService_Client.Create( TGrpcWsClient.Create(dmTethering.ServerIp, 1001) );
  end;
  Result := FClient;
end;

procedure TdmBaseRecorder.CreateStream(const aCallback: TTextCallback);
begin
  FStream := Client.StreamFile(
    procedure(const aText: TText; aHasData, aClosed: Boolean)
    begin
      if Assigned(aCallback) then
        aCallback(aText, aHasData, aClosed);
    end);

  if FStream = nil then
    raise Exception.Create('No connection');
end;

procedure TdmBaseRecorder.DataModuleCreate(Sender: TObject);
begin
   SampleRate:= 16000;
end;

function TdmBaseRecorder.IsRecording: Boolean;
begin
  Result := tmrFetch.Enabled;
end;

procedure TdmBaseRecorder.Log(const aType: TLogType; const aData: string);
begin
  if Assigned(FOnLog) then
    FOnLog(aType, aData);
end;

function TdmBaseRecorder.Ping(const aTime: TTime): TTime;
begin
  Result := Client.Ping(aTime);
end;

procedure TdmBaseRecorder.ProcessAudio(const aRaw: TBytes);
var
  bThreshold: boolean;
  f: AndroidSpeech.Grpc.TFile;
begin
  ProcessVU(aRaw);

  f.sampleRate := sampleRate;
  f.data := aRaw;

  //silence detection
  bThreshold := FVUValue > FVUMax div 2;
  if not FOwnUtteranceDetected then
  begin
    if bThreshold then
    begin
      FOwnUtteranceDetected := True;
      FSilenceStart := 0;
      FSpeechStart  := Now;
      f.data := FPrevData + f.data;
      Log(ltDebug, 'new utterance');
    end;
  end
  else
  begin
    if not bThreshold then
      bThreshold := FVUValue > FVUMax div 4;  //lower threshold for stopping
    if not bThreshold then
    begin
      if FSilenceStart <= 0 then
        FSilenceStart := Now;
      if not FOwnUtteranceEndDetected and
         //( (MilliSecondsBetween(Now, FSpeechStart) > 1500) and (MilliSecondsBetween(Now, FSilenceStart) > 2000{1500})) or     //sentence: longer silence allowed
         //( (MilliSecondsBetween(Now, FSpeechStart) < 1500) and (MilliSecondsBetween(Now, FSilenceStart) > 1000{200})) then    //short command: 200ms silence
         (MilliSecondsBetween(Now, FSilenceStart) > 1500) then   //tijdelijk langere tijd ivm veld test demo
      begin
        FOwnUtteranceEndDetected := True;
        Log(ltDebug, 'end of utterance');
      end;
    end
    else
      FSilenceStart := 0;
  end;

  FPrevData := aRaw;

  {$MESSAGE WARN 'TODO: send in background thread, so UI does not hang, and store in memstream for offline working'}
  {$MESSAGE WARN 'TODO: split in seperate part in case of offline'}
  if FStream <> nil then
  begin
    if (FStream.IsResponseClosed or FStream.IsRequestClosed or FOwnUtteranceEndDetected) and tmrFetch.Enabled then
      Self.Stop;
    if FStream <> nil then
      if not FStream.IsRequestClosed and (aRaw <> nil) then
        if FOwnUtteranceDetected then
          FStream.Send(f);
  end
  else if tmrFetch.Enabled then   //recursion check
    Self.Stop;
end;

procedure TdmBaseRecorder.ProcessVU(const aRaw: TBytes);
var
  bRaw: TBytes;
  bAudio: TArray<Int16> absolute bRaw;
  i, iAvg: Integer;
  iSum: Int64;
begin
  bRaw := aRaw;

  //simple VU meter calc
  iSum := 0;
  if bRaw <> nil then
  begin
    for i := 0 to Length(bRaw) div 2 do           //audio is 16bit = 2 bytes
      iSum := iSum + Abs(bAudio[i]);              //negative wave
    iAvg := Round(iSum / (Length(bRaw)div 2));    //int16 max = 32767
  end
  else
    iAvg := 0;
  if (iAvg > FVUMax) then
    FVUMax := iAvg;
  FVUValue := iAvg;

  //fire event
  if Assigned(OnVUChanged) then
    OnVUChanged(Self);
end;

procedure TdmBaseRecorder.SetOnVUChanged(const Value: TNotifyEvent);
begin
  FOnVUChanged := Value;
end;

procedure TdmBaseRecorder.Start(const aCallback: TTextCallback);
begin
  Log(ltDebug, 'starting...');
  CreateStream(aCallback);
  FPrevData := nil;
  FOwnUtteranceDetected := False;
  FOwnUtteranceEndDetected := False;
  FSpeechStart  := 0;
  FSilenceStart := 0;
  FVUMax   := High(Int16) div 512;           //32767
  FVUValue := 0;
  tmrFetch.Enabled := True;
  Log(ltDebug, 'Recording...');
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

function TdmBaseRecorder.StartOfSpeech: TDateTime;
begin
  Result := FSpeechStart;
end;

procedure TdmBaseRecorder.Stop;
begin
  if Fstream <> nil then
    Log(ltDebug, 'stopping...');

  tmrFetch.Enabled := False;
  tmrFetchTimer(nil);

  if Fstream <> nil then
  begin
    FStream.CloseSend;
    Fstream := nil;
    Log(ltDebug, 'stopped');
  end;

  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

procedure TdmBaseRecorder.StopStream;
begin
   FStream.CloseSend;
   FStream := nil;
end;

procedure TdmBaseRecorder.Stream(const aFile: TFile);
begin
   Assert(Fstream <> nil,'Stream not created. use "Start"');
   FStream.Send(aFile);
end;

procedure TdmBaseRecorder.tmrFetchTimer(Sender: TObject);
begin
  //
end;

function TdmBaseRecorder.UploadFile(const aFile: TFile): TText;
begin
   Result := Client.UploadFile(aFile);
end;

end.
