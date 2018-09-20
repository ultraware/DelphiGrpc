unit fClient;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, demoservice.Client, Ultraware.Grpc.Client,
  Vcl.Imaging.pngimage;

type
  TForm5 = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
  private
    waitStream: IWaitForMoveStream_Send;
    contineousStream: IContinuousMoveStream_Send;
    FClient: IMoveService_Client;
    procedure WaitForMove;
    procedure CloseWait;
  public
  end;

var
  Form5: TForm5;

implementation

uses
  demoservice.Proto;

{$R *.dfm}

var
  startX, startY: Integer;
  startLeft, startTop: Integer;

procedure TForm5.FormDestroy(Sender: TObject);
var m: TMove;
begin
  CloseWait;

  if contineousStream <> nil then
    contineousStream.CloseAndRecv(m);
  contineousStream := nil;

  FClient := nil;
end;

procedure TForm5.CloseWait;
begin
  if waitStream <> nil then
  begin
    waitStream.CloseSend;
    waitStream := nil;
  end;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  FClient := TMoveService_Client.Create('127.0.0.1', 1000);
  WaitForMove;
end;

procedure TForm5.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
var m: TMove;
begin
  CloseWait;

  contineousStream := FClient.ContinuousMove();
  m.name := Panel1.Name;
  m.X    := Panel1.Left;
  m.Y    := Panel1.Top;
  contineousStream.Send(m);

  Panel1.Tag := 1;
  startX := Mouse.CursorPos.X;
  startY := Mouse.CursorPos.Y;
  startLeft := Panel1.Left;
  startTop  := Panel1.Top;
end;

procedure TForm5.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y:
    Integer);
var m: TMove;
begin
  if Panel1.Tag = 1 then
  begin
    Panel1.Left := startLeft + (Mouse.CursorPos.X - startX);
    Panel1.Top  := startTop  + (Mouse.CursorPos.Y - startY);
    Panel1.Update;

    m.name := Panel1.Name;
    m.X    := Panel1.Left;
    m.Y    := Panel1.Top;
    contineousStream.Send(m);
  end;
end;

procedure TForm5.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
var m: TMove;
begin
  Panel1.Tag := 0;
  contineousStream.CloseAndRecv(m);
  contineousStream := nil;
  WaitForMove;
end;

procedure TForm5.WaitForMove;
var
  m: TMove;
begin
  m.name := Panel1.Name;

  waitStream := FClient.WaitForMove(
    procedure(const aInput: TMove; const aHasData, aClosed: Boolean)
    var m: TMove;
    begin
      if waitStream = nil then
        Exit;

      m := aInput;
      TThread.Queue(nil, procedure
        begin
          Panel1.Left := m.X;
          Panel1.Top  := m.Y;
        end);
    end);

  waitStream.Send(m);
end;

end.
