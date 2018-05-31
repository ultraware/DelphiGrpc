unit TestService.proto;

interface

uses
  System.Types, System.SysUtils,
  Grijjy.ProtocolBuffers;

type
  TTime = record
  public
    [Serialize(1)] sec: UInt32;       //note: int32 will give double results in golang?
    [Serialize(2)] msg: string;
    [Serialize(3)] msec: UInt32;      //note: int32 will give double results in golang?
  public
    constructor Create(aSec, aMsec: UInt32);

    function  Serialize: TBytes;
    procedure Deserialize(const aData: TBytes);

    function ToString: string;
  end;

implementation

{ TTime }

constructor TTime.Create(aSec, aMsec: UInt32);
begin
  sec := aSec;
  msec := aMsec;
end;

procedure TTime.Deserialize(const aData: TBytes);
begin
  TgoProtocolBuffer.Deserialize(Self, aData);
end;

function TTime.Serialize: TBytes;
begin
  Result := TgoProtocolBuffer.Serialize(Self);
end;

function TTime.ToString: string;
begin
  Result := Format('%ds, %dms, %s', [sec, msec, msg]);
end;

end.

