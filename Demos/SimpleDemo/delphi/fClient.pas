unit fClient;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  TestService.client, TestService.grpc, TestService.proto;

type
  TfrmClient = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    FClient: ITestService_Client;
    procedure Log(const aText: string);
  end;

var
  frmClient: TfrmClient;

implementation

uses
  System.IOUtils, IdURI, System.Net.HttpClient,
  SuperObject,
  Ultraware.Grpc,
  Grijjy.Http;

{$R *.fmx}

procedure TfrmClient.FormCreate(Sender: TObject);
begin
  FClient := TTestService_Client.Create( TGrpcHttp2Client.Create('127.0.0.1', 1000) );
end;

procedure TfrmClient.Log(const aText: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      Memo1.Lines.Add(aText);
      Application.ProcessMessages;
    end);
end;

procedure TfrmClient.Button1Click(Sender: TObject);
var
  t: TTime;
begin
  Log('Sleeping...');
  t.msec := 0;
  t.sec := 3;

  t := FClient.Sleep(t);
  Log(
    TSuperRttiContext.Create.AsJson<TTime>(t).AsJSon()
  );
  Log('Sleep DONE');
end;

procedure TfrmClient.Button2Click(Sender: TObject);
var
  t: TTime;
begin
  Log('Counting down...');
  t.msec := 0;
  t.sec := 5;
  FClient.CountDown(t,
    procedure(const aTime: TTime; aHasData, aClosed: Boolean)
    begin
      if aHasData then
        Log(
          TSuperRttiContext.Create.AsJson<TTime>(aTime).AsJSon()
        );
      if aClosed then
        Log('Countdown DONE');
    end);
  Log('<waiting>...');
end;

procedure TfrmClient.Button3Click(Sender: TObject);
var
  s: string;
  strm: ICalcSumStream;
  t: TTime;
begin
  Log('Calcing sum...');
  strm := FClient.CalcSum();
  while InputQuery('Amount', 'Amount', s) do
  begin
    t.sec := s.ToInteger();
    Log('Adding: ' + s);
    strm.Send(t);
  end;
  if strm.CloseAndRecv(t) then
    Log('Sum = ' + TSuperRttiContext.Create.AsJson<TTime>(t).AsJSon());
end;

procedure TfrmClient.Button4Click(Sender: TObject);
var
  s: string;
  strm: IUpdateableCountDown_Send;
  t: TTime;
begin
  Log('Countdown (updateable)...');
  strm := FClient.UpdateableCountDown(
    procedure(const aTime: TTime; aHasData, aClosed: Boolean)
    begin
      if aHasData then
        Log(
          TSuperRttiContext.Create.AsJson<TTime>(aTime).AsJSon()
        );
      if aClosed then
        Log('Countdown DONE');
    end);

  while InputQuery('Amount', 'Amount', s) do
  begin
    t.sec := s.ToInteger();
    Log('Updating: ' + s);
    strm.Send(t);
  end;
  strm.CloseSend();
end;

end.
