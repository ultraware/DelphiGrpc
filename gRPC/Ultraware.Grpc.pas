unit Ultraware.Grpc;

interface

uses System.SysUtils, Grijjy.Http;

type
  TGrpcWaitResult = (wrTimeout, wrNoData, wrData, wrClosed);

  TGrpcHeader = packed record
    Compression: Byte;
    Size: Int32;
    function TryDeserialize(const aData: TBytes): Boolean;
    function Serialize: TBytes;
  end;

  TGrpcPacket = record
    Header: TGrpcHeader;
    Data: TBytes;
    constructor Create(const aData: TBytes);
    function TryDeserialize(const aBuffer: TThreadSafeBuffer): Boolean; overload;
    function TryDeserialize(const aData: TBytes): Boolean; overload;
    function Serialize: TBytes;
  end;

  TGrpcCallback = reference to procedure(const aData: TBytes; aIsStreamClosed: Boolean);
  TGrpcErrorCallback = reference to procedure(const aError: Exception);

  TBaseGrpcCallbackStream = class(TInterfacedObject)
  protected
    FClosed: Boolean;
    FWriteCallback: TGrpcCallback;
    FErrorCallback: TGrpcErrorCallback;
    procedure SendData(const aData: TBytes);
  public
    constructor Create(const aWriteCallback: TGrpcCallback; const aErrorCallback: TGrpcErrorCallback);
    destructor Destroy; override;

    procedure Close;
    function  IsClosed: Boolean;
    procedure RaiseErrror(aException: Exception);
  end;

  TBaseGrpcCallbackStreamClass = class of TBaseGrpcCallbackStream;

  TSerializeObject = class helper for TObject
    class function Serialize<T: record >(const ARecord: T): TBytes; static;
    class function Deserialize<T: record >(const aData: TBytes): T; static;
  end;

function SwapEndian(aValue: Integer): Integer; overload;

implementation

uses Grijjy.ProtocolBuffers;

function SwapEndian(aValue: Integer): Integer; overload;
var
  Temp, i: Integer;
begin
  Temp := aValue;
  aValue := 0;
  for i := 0 to 3 do
  begin
    aValue := (aValue shl 8) or (Temp and $FF);
    Temp := Temp shr 8;
  end;
  Result := aValue;
end;

{ TGrpcPacket }

constructor TGrpcPacket.Create(const aData: TBytes);
begin
  Header.Compression := 0;
  Header.Size := SwapEndian(Length(aData)); // bigendian
  Data := aData;
end;

function TGrpcPacket.TryDeserialize(const aBuffer: TThreadSafeBuffer): Boolean;
var
  hdr, buffer: TBytes;
  iLength: Integer;
begin
  Data := nil;

  // try load header first to determine packet size
  if not aBuffer.Peek(hdr, SizeOf(TGrpcHeader)) or
    not Self.Header.TryDeserialize(hdr) then
    Exit(False);

  iLength := SizeOf(TGrpcHeader) + Self.Header.Size;
  if (aBuffer.Size < iLength) or // not all data yet?
    not aBuffer.Read(buffer, iLength) then
    Exit(False);

  // copy data part after header
  Data := Copy(buffer, SizeOf(TGrpcHeader), iLength);
  Result := True;
end;

function TGrpcPacket.Serialize: TBytes;
begin
  Result := Self.Header.Serialize + Self.Data;
end;

function TGrpcPacket.TryDeserialize(const aData: TBytes): Boolean;
var
  iLength: Integer;
begin
  // try load header first to determine packet size
  if not Self.Header.TryDeserialize(aData) then
    Exit(False);

  iLength := SizeOf(TGrpcHeader) + Self.Header.Size;
  if (Length(aData) < iLength) then // not all data yet?
    Exit(False);

  // copy data part after header
  Self.Data := Copy(aData, SizeOf(TGrpcHeader), iLength);
  Result := True;
end;

{ TGrpcHeader }

function TGrpcHeader.TryDeserialize(const aData: TBytes): Boolean;
begin
  Result := (Length(aData) >= SizeOf(Self));
  if not Result then
    Exit;

  Move(aData[0], Self, SizeOf(Self));
  Self.Size := SwapEndian(Self.Size); // bigendian to little endian
end;

function TGrpcHeader.Serialize: TBytes;
begin
  SetLength(Result, SizeOf(Self));
  Move(Self, Result[0], SizeOf(Self));
end;

{ TBaseGrpcCallbackStream }

procedure TBaseGrpcCallbackStream.Close;
begin
  FClosed := True;
  if Assigned(FWriteCallback) then
    FWriteCallback(nil, True { close } );
  FWriteCallback := nil;
end;

constructor TBaseGrpcCallbackStream.Create(const aWriteCallback: TGrpcCallback; const aErrorCallback: TGrpcErrorCallback);
begin
  inherited Create;
  FWriteCallback := aWriteCallback;
  FErrorCallback := aErrorCallback;
end;

destructor TBaseGrpcCallbackStream.Destroy;
begin
  Close;
  inherited;
end;

procedure TBaseGrpcCallbackStream.RaiseErrror(aException: Exception);
begin
  if Assigned(FErrorCallback) then
    FErrorCallback(aException);
end;

function TBaseGrpcCallbackStream.IsClosed: Boolean;
begin
  Result := FClosed;
end;

procedure TBaseGrpcCallbackStream.SendData(const aData: TBytes);
begin
  if Assigned(FWriteCallback) then
    FWriteCallback(aData, False { no close } );
end;

{ TSerializeObject }

class function TSerializeObject.Deserialize<T>(const aData: TBytes): T;
begin
  TgoProtocolBuffer.Deserialize<T>(Result, aData);
end;

class function TSerializeObject.Serialize<T>(const ARecord: T): TBytes;
begin
  Result := TgoProtocolBuffer.Serialize(ARecord);
end;

end.
