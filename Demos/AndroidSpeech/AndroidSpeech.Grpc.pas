unit AndroidSpeech.Grpc;

interface

uses
  System.Types, System.SysUtils, Ultraware.Grpc,
  Grijjy.ProtocolBuffers;

const
  C_TestService_Path = '/speechproject.SpeechService/';

type
  TFile = record
  public
//    [Serialize(1)] sec: UInt32;       //note: int32 will give double results in golang?
//    [Serialize(2)] msg: string;
//    [Serialize(3)] msec: UInt32;      //note: int32 will give double results in golang?
    [Serialize(1)] filename: string;
    [Serialize(2)] data: TBytes;
    [Serialize(3)] sampleRate: UInt32;
  public
    function  Serialize: TBytes;
    procedure Deserialize(const aData: TBytes);
//    function ToString: string;
  end;

  TTime = record
    [Serialize(1)] hour: UInt32;
    [Serialize(2)] sec: UInt32;
    [Serialize(3)] msec: UInt32;
  public
    function  Serialize: TBytes;
    procedure Deserialize(const aData: TBytes);
  end;

  TText = record
    [Serialize(1)] Text: string;
  public
    function  Serialize: TBytes;
    procedure Deserialize(const aData: TBytes);
  end;

  TTextCallback = reference to procedure(const aText: TText; aHasData, aClosed: Boolean);

  IStreamFile_Send = interface(IGrpcStream)
    ['{9CA82679-0414-413C-B7A1-7C99C23CAC7C}']
    procedure Send(const aFile: TFile);
    procedure CloseSend;
  end;

  IStreamFile_Result_Send = interface
    procedure Send(const aText: TText);
    procedure CloseSend;
  end;

  // Client
  ISpeechService_Client = interface
    ['{2332ACB3-8163-4CCE-A611-B5374C8860C3}']
    function UploadFile(const aFile: TFile): TText;
    function Ping(const aTime: TTime): TTime;
    function StreamFile(const aResponseCallback: TTextCallback): IStreamFile_Send;
  end;

  TStreamFile_Client_Send = class(TGrpcStream, IStreamFile_Send)
  protected
    {IStreamFile_Send}
    procedure Send(const aFile: TFile);
  end;

  // Server
  IStreamFile_Server_Recv = interface(IGrpcMemStream)
    ['{2189D418-EE2B-4659-A283-01B589341911}']
    function  Recv(out aFile: TFile; aWaitTimeout: Integer): TGrpcWaitResult;
  end;

  ISpeechService_Server = interface
    ['{029D569D-274F-4667-AC19-ED8C3ACBCD33}']
   	function  UploadFile(const aFile: TFile): TText;
    function  Ping(const aTime: TTime): TTime;
    procedure StreamFile(const aInputStream: IStreamFile_Server_Recv; const aOutputStream: IStreamFile_Result_Send);
  end;

  TStreamFile_Server_Send = class(TBaseGrpcCallbackStream, IStreamFile_Result_Send)
  protected
    {IStreamFile_Result_Send}
    procedure Send(const aText: TText);
    procedure CloseSend;
  end;

  TStreamFile_Server_Recv = class(TBaseGrpcMemStream, IStreamFile_Server_Recv)
  protected
    {IStreamFile_Server_Recv}
    function  Recv(out aFile: TFile; aWaitTimeout: Integer): TGrpcWaitResult;
  end;

implementation

{ TFile }

procedure TFile.Deserialize(const aData: TBytes);
begin
  TgoProtocolBuffer.Deserialize(Self, aData);
end;

function TFile.Serialize: TBytes;
begin
  Result := TgoProtocolBuffer.Serialize(Self);
end;

//function TFile.ToString: string;
//begin
//  Result := Format('%ds, %dms, %s', [sec, msec, msg]);
//end;

{ TText }

procedure TText.Deserialize(const aData: TBytes);
begin
  TgoProtocolBuffer.Deserialize(Self, aData);
end;

function TText.Serialize: TBytes;
begin
  Result := TgoProtocolBuffer.Serialize(Self);
end;

{ TStreamFile_Client_Send }

procedure TStreamFile_Client_Send.Send(const aFile: TFile);
begin
  Stream.SendData(aFile.Serialize);
end;

{ TStreamFile_Server_Send }

procedure TStreamFile_Server_Send.CloseSend;
begin
  inherited Close;
end;

procedure TStreamFile_Server_Send.Send(const aText: TText);
begin
  inherited SendData(aText.Serialize);
end;

{ TStreamFile_Server_Recv }

function TStreamFile_Server_Recv.Recv(out aFile: TFile; aWaitTimeout: Integer): TGrpcWaitResult;
var b: TBytes;
begin
  Result := inherited Recv(b, aWaitTimeout);
  if Result = wrData then
    aFile.Deserialize(b);
end;

{ TTime }

procedure TTime.Deserialize(const aData: TBytes);
begin
  TgoProtocolBuffer.Deserialize(Self, aData);
end;

function TTime.Serialize: TBytes;
begin
  Result := TgoProtocolBuffer.Serialize(Self);
end;

end.
