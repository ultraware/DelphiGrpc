unit Mobile;

{$INCLUDE 'Grijjy.inc'}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Edit, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  Ultraware.Grpc.Ws,
  TestService.client, TestService.grpc, TestService.proto;

type
  TForm4 = class(TForm)
    Button1: TButton;
    edtHost: TEdit;
    Memo1: TMemo;
    Button2: TButton;
    Label1: TLabel;
    Button3: TButton;
    EditButton1: TEditButton;
    Button4: TButton;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    FClient: ITestService_Client;
    function Client: ITestService_Client;
    procedure Log(const aText: string);
  public
  end;

var
  Form4: TForm4;

implementation

uses
  {$IF DEFINED(ANDROID) OR DEFINED(IOS) OR DEFINED(OSX)}
      {$DEFINE USE_ASYNC}
      FMX.DialogService.Async
  {$ELSE}
      FMX.DialogService.Sync
  {$ENDIF}
  ;

{$R *.fmx}

procedure TForm4.Log(const aText: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      Memo1.Lines.Add(aText);
      Application.ProcessMessages;
    end);
end;

procedure TForm4.Button1Click(Sender: TObject);
var t: TTime;
begin
  Log('Sleeping 2s...');

  t.sec := 2;
  t.msec := 0;
  t := Client.Sleep(t);
  Log(t.ToString);
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  t: TTime;
begin
  Log('Counting down...');
  t.sec := 5;
  t.msec := 0;
  Client.CountDown(t,
    procedure(const aTime: TTime; aHasData, aClosed: Boolean)
    begin
      if aHasData then
        Log(aTime.ToString);
      if aClosed then
        Log('Countdown DONE');
    end);
  Log('<waiting>...');
end;

procedure CalcSum(const aStream: ICalcSumStream; const values: array of string);
var
  s: string;
  t: TTime;
begin
  for s in values do
  if s <> '' then
  begin
    t.sec := s.ToInteger();
    Form4.Log('Adding: ' + s);
    aStream.Send(t);
  end;
  if aStream.CloseAndRecv(t) then
    Form4.Log('Sum = ' + t.ToString);
end;

procedure TForm4.Button3Click(Sender: TObject);
var
  strm: ICalcSumStream;
  temp, values: array of string;
begin
  Log('Calcing sum...');
  strm := Client.CalcSum();

  {$IFDEF USE_ASYNC}
  SetLength(values, 3);
  FMX.DialogService.Async.TDialogServiceAsync.InputQuery('Amount', ['Amount1', 'Amount2', 'Amount3'], values,
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      CalcSum(strm, AValues);
    end);
  {$ELSE}
  SetLength(values, 0);
  temp := [''];
  while FMX.DialogService.Sync.TDialogServiceSync.InputQuery('Amount', ['Amount'], temp) do
  begin
    values := values + temp;
    temp := [''];
  end;
  CalcSum(strm, values);
  {$ENDIF}
end;

function ResetCountDownValue(const strm: IUpdateableCountDown_Send; const aValues: array of string): Boolean;
var
  s: string;
  t: TTime;
begin
  Result := False;
  t.sec := 0;
  t.msec := 0;

  for s in AValues do
  if s <> '' then
  begin
    t.sec := s.ToInteger();
    t.msec := 0;
    Form4.Log('Updating: ' + s);
    strm.Send(t);
  end;

  if (t.sec <= 0) then
    strm.CloseSend()
  else if t.sec > 0 then
    Result := True;
end;

procedure ResetCountDown(const strm: IUpdateableCountDown_Send);
var
  values: array of string;
begin
  {$IFDEF USE_ASYNC}
  values := ['10'];
  FMX.DialogService.Async.TDialogServiceAsync.InputQuery('Amount', ['Amount'], values,
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if (AResult = mrOk) and ResetCountDownValue(strm, AValues) then
        ResetCountDown(strm);
    end);
  {$ELSE}
  values := [''];
  if FMX.DialogService.Sync.TDialogServiceSync.InputQuery('Amount', ['Amount'], values) then
    if ResetCountDownValue(strm, values) then
      ResetCountDown(strm);
  {$ENDIF}
end;

procedure TForm4.Button4Click(Sender: TObject);
var
  t: TTime;
  strm: IUpdateableCountDown_Send;
begin
  Log('Countdown (updateable)...');
  strm := Client.UpdateableCountDown(
    procedure(const aTime: TTime; aHasData, aClosed: Boolean)
    begin
      if aHasData then
        Log(aTime.ToString);
      if aClosed then
        Log('Countdown DONE');
    end);
  //start timer
  t.sec := 10;
  t.msec := 0;
  strm.Send(t);

  ResetCountDown(strm);
end;

function TForm4.Client: ITestService_Client;
begin
  if FClient = nil then
    FClient := TTestService_Client.Create( TGrpcWsClient.Create(edtHost.Text, 1001) );
  Result := FClient;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  {$IFDEF ANDROID}
    {$IFDEF EMU}
    edtHost.Text := '10.0.2.2';    //https://developer.android.com/studio/run/emulator-networking.html
    {$ELSE}
    edtHost.Text := '192.168.178.43';
    {$ENDIF}
  {$ELSE IFDEF LINUX}
  edtHost.Text := '192.168.36.1';
  {$ELSE}
  edtHost.Text := 'localhost';
  {$ENDIF}
end;

end.
