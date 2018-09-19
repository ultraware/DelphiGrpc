unit AndroidSpeech.Client;

interface

uses
  Ultraware.Grpc,
  AndroidSpeech.grpc;

type
  TTestService_Client = class(TGrpcClientHandler, ISpeechService_Client)
  protected
    {ISpeechService_Client}
    function UploadFile(const aFile: TFile): TText;
    function Ping(const aTime: TTime): TTime;
    function StreamFile(const aResponseCallback: TTextCallback): IStreamFile_Send;
  end;

implementation

uses
  System.SysUtils;

{ TTestService_Client }

function TTestService_Client.Ping(const aTime: TTime): TTime;
var
  recv: TBytes;
begin
  if Client.DoRequest(aTime.Serialize(), C_TestService_Path + 'Ping', recv) then
    Result.Deserialize(recv);
end;

function TTestService_Client.StreamFile(const aResponseCallback: TTextCallback): IStreamFile_Send;
var
  request: IGrpcStream;
  callback: TGrpcCallback;
begin
  if Assigned(aResponseCallback) then
    callback :=
      procedure(const aData: TBytes; aIsStreamClosed: Boolean)
      var
        t: TText;
      begin
        if aData <> nil then
          t.Deserialize(aData);
        aResponseCallback(t, aData <> nil, aIsStreamClosed);
      end
  else
    callback := nil;

  request := Client.DoRequest(nil, C_TestService_Path + 'StreamFile', callback);
  if request <> nil then
    Result  := TStreamFile_Client_Send.Create(request);
end;

function TTestService_Client.UploadFile(const aFile: TFile): TText;
var
  recv: TBytes;
begin
  if Client.DoRequest(aFile.Serialize(), C_TestService_Path + 'UploadFile', recv) then
    Result.Deserialize(recv);
end;

end.
