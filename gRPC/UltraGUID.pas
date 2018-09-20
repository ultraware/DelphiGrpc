unit UltraGUID;

interface

uses // Delphi
  Generics.Collections, TypInfo
  // Shared
    ;

type
  TGuidInterceptor = class
  private
    type
    TGUIDs = TDictionary<PTypeData, TGUID>;
  private
    class var GUIDs: TGUIDs;
  public
    class constructor Create;
    class destructor Destroy;
    class function GetGuid<Intf: IInterface>: TGUID;
  end;

implementation

class constructor TGuidInterceptor.Create;
begin
  GUIDs := TGUIDs.Create();
end;

class destructor TGuidInterceptor.Destroy;
begin
  GUIDs.Free;
end;

class function TGuidInterceptor.GetGuid<Intf>: TGUID;
var
  TypeData: PTypeData;
begin
  TypeData := GetTypeData(TypeInfo(Intf));
  if not GUIDs.TryGetValue(TypeData, Result) then
  begin
    Result := TypeData^.Guid;
    Assert(TGUID.Empty <> Result, 'Empty GUID! Will generate ''random'' behaviour! Provide interface with a GUID');
    GUIDs.Add(TypeData, Result);
  end;
end;

end.
