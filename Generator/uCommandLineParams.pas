unit uCommandLineParams;

interface

uses System.Generics.Collections;

type
  TParam = record
    ParamKey: string;
    ParamValue: string;
    procedure Clear;
    function IsEmpty: Boolean;
  end;

  TParamList = class(TList<TParam>)
  public
    procedure Fill;

    function KeyExists(const KeyAliases: TArray<string>): Boolean;
    function FindParam(const KeyAliases: TArray<string>; const FromPos: integer = 0): integer;
  end;

  function CreateAndFillParamList: TParamList;

implementation

uses System.SysUtils;

function CreateAndFillParamList: TParamList;
begin
   Result := TParamList.Create;
   Result.Fill;
end;

{ TParam }

procedure TParam.Clear;
begin
  ParamKey := '';
  ParamValue := '';
end;

function TParam.IsEmpty: Boolean;
begin
  Result := ParamKey = '';
end;

{ TParamList }

procedure TParamList.Fill;
var
  i: integer;
  Param: TParam;
  s: string;
begin
  Clear;
  Param.Clear;
  for i := 1 to ParamCount do
    begin
      s := ParamStr(i);
      if s.StartsWith('/') or s.StartsWith('-') then
        begin
          if not Param.IsEmpty then
            Add(Param);
          Param.Clear;
          Param.ParamKey := s.Substring(1); // string helpers use zero-based strings
        end
      else
        Param.ParamValue := Param.ParamValue + s + ';';
    end;
  if not Param.IsEmpty then
    Add(Param);
end;

function TParamList.FindParam(const KeyAliases: TArray<string>; const FromPos: integer): integer;
var
  i: integer;
  j: integer;
begin
  Result := -1;
  for i := FromPos to Count - 1 do
    for j := 0 to Length(KeyAliases) do
      if SameText(Items[i].ParamKey, KeyAliases[j]) then
        begin
          Result := i;
          Break;
        end;
end;

function TParamList.KeyExists(const KeyAliases: TArray<string>): Boolean;
begin
  Result := FindParam(KeyAliases) <> -1;
end;

end.
