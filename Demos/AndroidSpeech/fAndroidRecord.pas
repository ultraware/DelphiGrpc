unit fAndroidRecord;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Media,
  AndroidSpeech.Client, AndroidSpeech.Grpc, FMX.ScrollBox, FMX.Memo, FMX.Layouts, dBaseRecorder;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    btnParts: TButton;
    btnRaw: TButton;
    Panel2: TPanel;
    GridPanelLayout1: TGridPanelLayout;
    btnStart: TButton;
    btnStop: TButton;
    ProgressBar1: TProgressBar;
    btnPing: TButton;
    chkDebug: TCheckBox;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRawClick(Sender: TObject);
    procedure btnPartsClick(Sender: TObject);
    procedure btnPingClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkDebugChange(Sender: TObject);
  private
    procedure GetTextCallback(const aText: TText; aHasData, aClosed: Boolean);
  protected
    FRecorder: TdmBaseRecorder;
    function  SampleRate: Integer;
    procedure VUChanged(Sender: TObject);

    procedure Start;
    procedure Stop;
  end;

  procedure Log(const aType: TLogType; const aData: string);

var
  Form4: TForm4;

implementation

uses
  {$IFDEF ANDROID}
  dAndroidRecorder,
  {$ENDIF}
  System.IOUtils, Ultraware.Grpc.Ws, dStyle, System.Diagnostics, System.DateUtils, dTethering,
  FMX.DialogService.Async;

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure Log(const aType: TLogType; const aData: string);
begin
  if ((aType in [ltDebug]) and Form4.Chkdebug.IsChecked) or
     (aType in [ltInfo]) then
  begin
    Form4.Memo1.Lines.Add(aData);
    Form4.Memo1.GoToTextEnd();
  end;
end;

procedure TForm4.btnStartClick(Sender: TObject);
begin
  if dmTethering.ServerIp = '' then
  begin
    dmTethering.EnableClient;
    fmx.DialogService.Async.TDialogServiceAsync.ShowMessage('Searching...');
  end
  else
    Start;
end;

procedure TForm4.btnStopClick(Sender: TObject);
begin
  Stop;
end;

procedure TForm4.chkDebugChange(Sender: TObject);
begin
  btnRaw.Visible   := chkDebug.IsChecked;
  btnParts.Visible := chkDebug.IsChecked;
  btnPing.Visible  := chkDebug.IsChecked;
end;

procedure TForm4.GetTextCallback(const aText: TText; aHasData, aClosed: Boolean);
begin
  if aHasData then
     Log(ltInfo, aText.Text);
  if aClosed then
  begin
    Stop;
  end;
end;

procedure TForm4.btnPingClick(Sender: TObject);
var
  t, t2: TTime;
  watch: TStopwatch;
begin
  t.hour := 0;
  t.sec  := 0;
  t.msec := 100;
  watch := TStopwatch.Create;
  watch.Start;
  t2 := FRecorder.Ping(t);
  watch.Stop;
  Log(ltDebug, Format('Ping-pong, client = %d, server = %d, total = %dms',[100, t2.msec, watch.ElapsedMilliseconds]));
end;

procedure TForm4.btnRawClick(Sender: TObject);
var
  f: AndroidSpeech.Grpc.TFile;
  t: TText;
begin
  f.filename := 'test.raw';
  f.data := TFile.ReadAllBytes(f.filename);
  f.sampleRate := sampleRate;

  if btnStop.Enabled then
    btnStopClick(nil);

  t := FRecorder.UploadFile(f);
  Memo1.Lines.Add(t.Text);
end;

procedure TForm4.btnPartsClick(Sender: TObject);
var b: TBytes;
    f: AndroidSpeech.Grpc.TFile;
begin
  f.filename := 'test.raw';
  f.sampleRate := sampleRate;
  b := TFile.ReadAllBytes(f.filename);
  while b <> nil do
  begin
    f.data := Copy(b, 0, 1024);
    FRecorder.Stream(f);
    b := Copy(b, 1024, Length(b));    //  512 / 16000 * 1000 = 32ms
    Sleep(10);
  end;
  FRecorder.StopStream;
end;

procedure TForm4.VUChanged(Sender: TObject);
begin
  ProgressBar1.Max   := FRecorder.VUMax;
  ProgressBar1.Value := FRecorder.VUValue;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  ProgressBar1.Value := 0;
  chkDebugChange(nil);
end;

procedure TForm4.FormShow(Sender: TObject);
begin
  {$IFDEF ANDROID}
  FRecorder := TdmAndroidRecorder.Create(Self);
  (FRecorder as TdmAndroidRecorder).InitAndroid;
  {$ELSE}
  FRecorder := TdmDummyRecorder.Create(Self);
  {$ENDIF}
  FRecorder.SampleRate  := sampleRate;
  FRecorder.OnLog       := Log;
  FRecorder.SampleRate  := sampleRate;
  FRecorder.OnVUChanged := VUChanged;
end;

function TForm4.SampleRate: Integer;
begin
  Result := FRecorder.SampleRate;
end;

procedure TForm4.Start;
begin
  application.ProcessMessages;

  try
    ProgressBar1.Value := 0;
    ProgressBar1.Max   := FRecorder.VUMax;;

    FRecorder.Start(GetTextCallback);
    btnStart.Enabled := False;
    btnStop.Enabled := True;
  except
    on E:Exception do
    begin
      Log(ltInfo, e.Message);
      raise;
    end;
  end;
end;

procedure TForm4.Stop;
begin
  FRecorder.Stop;
  btnStart.Enabled := True;
  btnStop.Enabled := False;
end;

end.
