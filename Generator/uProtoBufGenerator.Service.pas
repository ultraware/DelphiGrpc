unit uProtoBufGenerator.Service;

interface

uses System.Classes, uProtoBufGenerator, uProtoBufParserClasses;

type
   TProtoServiceGenerator = class(TProtoBaseServiceGenerator)
   protected
      class function IsClient: Boolean; virtual; abstract;

      class function ServicePath(const aFile: TProtoFile; const aService: TProtoBufService): string;
      class procedure AddInterfaceGUID(var Text: TStringList);
      class function RpcMessageType(const aMessage: TProtoBufMessage): string;
      class function RpcMessageName(const aMessage: TProtoBufMessage): string;
      class function StripFirstChar(const aString: string): string;
      class function GenerateRpcFunction(const aRPC: TProtoBufRPC; const aClassNameStr: string): string;
      class function RpcOutputStreamInput(const aRpc: TProtoBufRPC): string;
      class function RpcInputStreamInput(const aRpc: TProtoBufRPC): string;
      class function RpcOutputStreamInputName(const aRpc: TProtoBufRPC): string;
      class function RpcOutputStreamInputType(const aRpc: TProtoBufRPC): string;
      class function RpcOutputString(const aRpc: TProtoBufRPC): string;
      class function RpcServerOutputString(const aRpc: TProtoBufRPC): string;
      class function GenerateInputString(const aName, aType: string): string; overload;
      class function GenerateInputString(const aType: string): string; overload;
      class function ConvertToParamName(const aType: string): string;
      class function RpcIsFunction(const aRpc: TProtoBufRPC): Boolean;
      class function RpcArgumentInputType(const aRpc: TProtoBufRPC): string;
      class function RpcResultType(const aRpc: TProtoBufRPC): string;
      class function RpcArgumentInputName(const aRpc: TProtoBufRPC): string;
      class function RpcFunctionInput(const aRpc: TProtoBufRPC): string;
      class function RpcFunctionInputType(const aRpc: TProtoBufRPC): string;
      class function RpcFunctionInputName(const aRpc: TProtoBufRPC): string;
      class function RpcFunctionOutput(const aRpc: TProtoBufRPC): string;
      class function RpcFunctionOutputType(const aRpc: TProtoBufRPC): string;

      class function GenerateRpcDeclaration(const aRPC: TProtoBufRPC): string;
   end;

implementation

uses System.SysUtils, System.StrUtils, uProtoBufGenerator.Helper;

{ TProtoServiceGenerator }

class function TProtoServiceGenerator.GenerateRpcFunction(const aRPC: TProtoBufRPC; const aClassNameStr: string): string;
begin
   Result := Format('%s %s%s(%s)%s;',[
      IfThen(RpcIsFunction(aRPC), 'function', 'procedure'),
      aClassNameStr,
      aRpc.Name,
      RpcFunctionInput(aRPC),
      RpcFunctionOutput(aRPC)
   ]);
end;

class function TProtoServiceGenerator.GenerateRpcDeclaration(const aRPC: TProtoBufRPC): string;
begin
   Result := GenerateRpcFunction(aRPC, '');
end;

class procedure TProtoServiceGenerator.AddInterfaceGUID(var Text: TStringList);
var GUID: TGUID;
begin
   {$IFDEF DUnit}
   Exit; // Random -> lastig met unit testen
   {$ENDIF}
   CreateGUID(GUID);
   Text.Add('   ['''+GUID.ToString+''']');
end;

class function TProtoServiceGenerator.GenerateInputString(const aType: string): string;
begin
   Result := GenerateInputString(ConvertToParamName(aType), aType);
end;

class function TProtoServiceGenerator.ConvertToParamName(const aType: string): string;
begin
   Result := 'a'+StripFirstChar(aType);
end;

class function TProtoServiceGenerator.GenerateInputString(const aName, aType: string): string;
begin
   Result := Format('const %s: %s',[aName, aType]);
end;

class function TProtoServiceGenerator.RpcOutputStreamInput(const aRpc: TProtoBufRPC): string;
begin
   Result := GenerateInputString(RpcOutputStreamInputName(aRpc), RpcOutputStreamInputType(aRpc));
end;

class function TProtoServiceGenerator.RpcInputStreamInput(const aRpc: TProtoBufRPC): string;
begin
   Result := GenerateInputString(IfThen(IsClient,aRpc.InputStreamInterfaceType,aRpc.ServerInputStreamInterfaceType));
end;

class function TProtoServiceGenerator.RpcOutputStreamInputName(const aRpc: TProtoBufRPC): string;
begin
   Result := ConvertToParamName(RpcOutputStreamInputType(aRpc));
end;

class function TProtoServiceGenerator.RpcOutputStreamInputType(const aRpc: TProtoBufRPC): string;
begin
   if IsClient then
      Result := aRpc.OutputCallbackType
   else
      Result := aRpc.ServerOutputInterfaceType;
end;

class function TProtoServiceGenerator.RpcServerOutputString(const aRpc: TProtoBufRPC): string;
begin
   Result := GenerateInputString(RpcMessageName(aRpc.RPCResult), RpcResultType(aRpc));
end;

class function TProtoServiceGenerator.RpcOutputString(const aRpc: TProtoBufRPC): string;
begin
   if aRpc.HasOutput then
      Result := StringReplace(RpcServerOutputString(aRpc), 'const ','out ',[])
   else
      Result := '';
end;

class function TProtoServiceGenerator.RpcArgumentInputName(const aRpc: TProtoBufRPC): string;
begin
   Result := RpcMessageName(aRPC.RPCArgument);
end;

class function TProtoServiceGenerator.RpcMessageName(const aMessage: TProtoBufMessage): string;
begin
   if aMessage.Has1Param then
      Result := aMessage[0].Name
   else
      Result := StripFirstChar(aMessage.DelphiClassname);
   Result := 'a' + Result;
end;

class function TProtoServiceGenerator.RpcArgumentInputType(const aRpc: TProtoBufRPC): string;
begin
   Result := RpcMessageType(aRpc.RPCArgument);
end;

class function TProtoServiceGenerator.RpcResultType(const aRpc: TProtoBufRPC): string;
begin
   Result := RpcMessageType(aRpc.RPCResult);
end;

class function TProtoServiceGenerator.RpcMessageType(const aMessage: TProtoBufMessage): string;
begin
   if aMessage.Has1Param then
      Result := aMessage[0].DelphiType
   else
      Result := aMessage.DelphiClassname;
end;

class function TProtoServiceGenerator.RpcFunctionInput(const aRpc: TProtoBufRPC): string;
begin
   if aRpc.HasNonSteamInput then
   begin
      Result := GenerateInputString(RpcFunctionInputName(aRpc), RpcFunctionInputType(aRpc));
      if aRpc.ResultIsStream then
         Result := Format('%s; %s',[Result,RpcOutputStreamInput(aRpc)]);
   end
   else if (not IsClient) and aRpc.ArgementIsStream then
   begin
      Result := RpcInputStreamInput(aRpc);
      if aRpc.ResultIsStream then
         Result := Format('%s; %s',[Result,RpcOutputStreamInput(aRpc)]);
   end
   else if aRpc.ResultIsStream then
      Result := RpcOutputStreamInput(aRpc)
   else
      Result := '';
end;

class function TProtoServiceGenerator.RpcFunctionInputName(const aRpc: TProtoBufRPC): string;
begin
   if aRpc.HasNonSteamInput then
      Result := RpcArgumentInputName(aRpc)
   else
      Result := '';
end;

class function TProtoServiceGenerator.RpcFunctionInputType(const aRpc: TProtoBufRPC): string;
begin
   if aRpc.HasNonSteamInput then
      Result := RpcArgumentInputType(aRpc)
   else
      Result := '';
end;

class function TProtoServiceGenerator.RpcFunctionOutput(const aRpc: TProtoBufRPC): string;
begin
   Result := RpcFunctionOutputType(aRpc);
   if Result <> '' then
         Result := ': '+ Result;
end;

class function TProtoServiceGenerator.RpcFunctionOutputType(const aRpc: TProtoBufRPC): string;
var Mes: TProtoBufMessage;
begin
   if RpcIsFunction(aRpc) then
   begin
      if IsClient and aRpc.ArgementIsStream then
         Result := aRpc.InputStreamInterfaceType
      else
      begin
         Mes := aRPC.RPCResult;
         if Mes.Has1Param then
            Result := Mes[0].DelphiType
         else
            Result := Mes.DelphiClassname
      end;
   end
   else
      Result := '';
end;

class function TProtoServiceGenerator.RpcIsFunction(const aRpc: TProtoBufRPC): Boolean;
begin
   Result := (aRPC.HasOutput and (not aRpc.ResultIsStream)) or (aRpc.ArgementIsStream and IsClient);
end;

class function TProtoServiceGenerator.ServicePath(const aFile: TProtoFile; const aService: TProtoBufService): string;
begin
   Result := Format('''/%s.%s/''',[aFile.Name, aService.Name]);
end;

class function TProtoServiceGenerator.StripFirstChar(const aString: string): string;
begin
   Result := Copy(aString, 2, Length(aString));
end;

end.
