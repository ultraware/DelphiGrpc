unit dAndroidRecorder;

interface

uses
  Androidapi.JNI.Net,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Media,
  Androidapi.Helpers, Androidapi.JNI.App, BroadcastReceiver,

  System.SysUtils, System.Classes, dBaseRecorder, FMX.Types, AndroidSpeech.Grpc;

type
  TdmAndroidRecorder = class(TdmBaseRecorder)
    procedure tmrFetchTimer(Sender: TObject);
  private
    Audio: JAudioManager;
    AudioRecorder: JAudioRecord;
    AudioStr: TJavaArray<Byte>;

    FBlueToothDevice: JAudioDeviceInfo;
    BroadcastReceiver2: TBroadcastReceiver;
    FBluetoothOn: Boolean;

    channelConfig: Integer;
    audioFormat: Integer;
    minBufSize: Integer;
    procedure BroadcastReceiver2Receive(Context: JContext; Intent: JIntent);
  public
    procedure InitAndroid;

    procedure Start(const aCallback: TTextCallback); override;
    procedure Stop; override;

    function IsBluetoothOn: Boolean;
  end;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  Androidapi.Jni;

{$R *.dfm}

procedure TdmAndroidRecorder.BroadcastReceiver2Receive(Context: JContext; Intent: JIntent);
var state: Integer;
begin
  try
    state := intent.getIntExtra(TJAudioManager.JavaClass.EXTRA_SCO_AUDIO_STATE, -1);
    if (state = TJAudioManager.JavaClass.SCO_AUDIO_STATE_CONNECTED) then
    begin
       FBluetoothOn := True;
       Log(TLogType.ltDebug, 'BT Recording is Ready');
       if Assigned(OnStateChange) then
         OnStateChange(Self);

       if AudioRecorder <> nil then
         if not AudioRecorder.setPreferredDevice(FBlueToothDevice) then
           Log(ltInfo, 'Could not set: ' + JStringToString(FBlueToothDevice.getProductName.toString));
    end
    else if (state = TJAudioManager.JavaClass.SCO_AUDIO_STATE_CONNECTING) then
       Log(ltDebug, 'BT connecting...')
    else if (state = TJAudioManager.JavaClass.SCO_AUDIO_STATE_ERROR) then
       Log(ltDebug, 'BT error!')
    else if (state = TJAudioManager.JavaClass.SCO_AUDIO_STATE_DISCONNECTED) then
    begin
       FBluetoothOn := False;
       Log(ltDebug, 'BT Recording Disabled');
       if Assigned(OnStateChange) then
         OnStateChange(Self);
      Audio.setBluetoothScoOn(True);
      Audio.startBluetoothSco();
    end;
  except
    on E:Exception do
      Log(ltInfo, e.Message);
  end;
end;

procedure TdmAndroidRecorder.InitAndroid;
var
  AudioObj: JObject;
  devices: TJavaObjectArray<JAudioDeviceInfo>;
  device: JAudioDeviceInfo;
  i: Integer;
  AUDIO_SERVICE: JString;
  sDevice: string;
begin
  //https://stackoverflow.com/questions/29626247/how-to-record-audio-via-bluetooth-mic
  AUDIO_SERVICE := TJContext.JavaClass.AUDIO_SERVICE;
  AudioObj:= TAndroidHelper.Context.getSystemService(AUDIO_SERVICE);
  Audio := TJAudioManager.Wrap((AudioObj as ILocalObject).GetObjectID);

  BroadcastReceiver2 := TBroadcastReceiver.Create(Self);
  BroadcastReceiver2.onReceive := BroadcastReceiver2Receive;
  BroadcastReceiver2.RegisterReceive;
  BroadcastReceiver2.Add(JStringToString(TJAudioManager.JavaClass.ACTION_SCO_AUDIO_STATE_UPDATED));

  if not Audio.isBluetoothScoAvailableOffCall then
    Log(ltInfo, 'No BT recording available');

  //if not Audio.isBluetoothScoOn then
  begin
    FBluetoothOn := False;
    Audio.setBluetoothScoOn(True);
    //https://developer.android.com/reference/android/media/AudioManager.html#startBluetoothSco()
    Audio.startBluetoothSco();
  end;

  channelConfig:= TJAudioFormat.JavaClass.CHANNEL_IN_MONO;
  audioFormat:= TJAudioFormat.JavaClass.ENCODING_PCM_16BIT;
  // minBufSize = 1024 Bytes
  minBufSize:= TJAudioRecord.JavaClass.getMinBufferSize(sampleRate, channelConfig, audioFormat);
  // AudioRecover = 1024*4 = 4096
  AudioStr:= TJavaArray<Byte>.Create(minBufSize*4);

  devices := Audio.getDevices(TJAudioManager.JavaClass.GET_DEVICES_INPUTS);
  for i := 0 to devices.Length - 1 do
  begin
    device  := devices.Items[i];
    sDevice := JStringToString(device.getProductName.toString);
    Log(ltDebug, sDevice);
    if sDevice.Contains('B350') then
      FBlueToothDevice := device;
  end;
end;

function TdmAndroidRecorder.IsBluetoothOn: Boolean;
begin
  Result := FBluetoothOn;
end;

procedure TdmAndroidRecorder.Start(const aCallback: TTextCallback);
begin
  if FBluetoothOn then
  begin
    Audio.setMode(TJAudioManager.JavaClass.MODE_IN_CALL);
    AudioRecorder:= TJAudioRecord.JavaClass.init(TJMediaRecorder_AudioSource.JavaClass.VOICE_COMMUNICATION,
                                                      sampleRate,
                                                      channelConfig,
                                                      audioFormat,
                                                      minBufSize*4);
    if FBlueToothDevice <> nil then
      if not AudioRecorder.setPreferredDevice(FBlueToothDevice) then
        raise Exception.Create('Could not set: ' + JStringToString(FBlueToothDevice.getProductName.toString));
  end
  else
    AudioRecorder:= TJAudioRecord.JavaClass.init(TJMediaRecorder_AudioSource.JavaClass.VOICE_RECOGNITION,
                                                      sampleRate,
                                                      channelConfig,
                                                      audioFormat,
                                                      minBufSize*4);

  audioRecorder.startRecording();
  inherited;
end;

procedure TdmAndroidRecorder.Stop;
begin
  audioRecorder.stop();
  inherited;
end;

procedure TdmAndroidRecorder.tmrFetchTimer(Sender: TObject);
var
  NewCount: Integer;
  bRaw: TBytes;
begin
  inherited;

  repeat
    if FStream = nil then
      Exit;

    // Read from the AudioRecover
    NewCount:= (AudioRecorder as JAudioRecord).read(AudioStr, 0, AudioStr.Length);
    // The read command does not read 4096, just 2048
    SetLength(bRaw, NewCount);
    Move(AudioStr.Data^, bRaw[0], NewCount);

    ProcessAudio(bRaw);
  until (NewCount <= 0) or tmrFetch.Enabled;
end;

end.
