unit uProtoBufGenerator.Client;

interface

uses System.Classes, uProtoBufGenerator.Service, uProtoBufParserClasses;

type
   TProtoClientGenerator = class(TProtoServiceGenerator)
   protected
      class function IsClient: Boolean; override;
   private
      class procedure GenerateServiceType(var Text: TStringList; const aService: TProtoBufService; const aType, aParentType: string; const aExtra: string = '');
      class function GenerateServiceInterfaceName(const aService: TProtoBufService): string;
      class function GenerateClassName(const aService: TProtoBufService): string;
      class procedure DetermineCallImplementationHelpVars(const aRpc: TProtoBufRPC; out IsSimpleFunc: Boolean;
         out RequestInputType, RequestOutput, DeserializeInput, RequestString: string);
      class procedure AddCallImplementationVars(var Text: TStringList; const aRpc: TProtoBufRPC; const IsSimpleFunc: Boolean; const RequestInputType, DeserializeInput, RequestOutput: string);
      class procedure AddCallImplementationPreProcessing(var Text: TStringList; const aRpc: TProtoBufRPC; const IsSimpleFunc: Boolean; const RequestInputType: string);
   protected
      class procedure AddInputStreamInterfaceFunctions(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure AddInputStreamSend(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure AddInputStreamCloseAndRecv(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateInputStreamInterface(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateInputStreamInterfaces(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateOutputStreamCallback(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateOutputStreamCallbacks(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateClientInterface(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateClientClass(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateClientCallImplementation(var Text: TStringList; const aRpc: TProtoBufRPC; const aService: TProtoBufService);
      class function GenerateRpcImplementation(const aRPC: TProtoBufRPC; const aService: TProtoBufService): string;
      class procedure GenerateClientImplementation(var Text: TStringList; const aFile: TProtoFile; const aService: TProtoBufService);
      class procedure GenerateStreamInDeclaration(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateStreamInDeclarations(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateStreamInImplementation(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateStreamInImplementations(var Text: TStringList; const aService: TProtoBufService);
   protected
      class function GenerateInterfaceUses(const aFile: TProtoFile): string; override;
      class function GenerateImplementationUses(const aFile: TProtoFile): string; override;
      class procedure GenerateInterface(var Text: TStringList; const aFile: TProtoFile); override;
      class procedure GenerateImplementation(var Text: TStringList; const aFile: TProtoFile); override;
   public
      class function GenerateUnitName(const aFile: TProtoFile): string; override;
   end;

implementation

uses System.SysUtils, System.StrUtils, uProtoBufGenerator, uProtoBufGenerator.Types, uProtoBufGenerator.Helper;

{ TProtoClientGenerator }

class procedure TProtoClientGenerator.AddInputStreamInterfaceFunctions(var Text: TStringList; const aRpc: TProtoBufRPC);
begin
   AddInputStreamSend(Text, aRpc);
   AddInputStreamCloseAndRecv(Text, aRpc);
end;

class procedure TProtoClientGenerator.AddInputStreamSend(var Text: TStringList; const aRpc: TProtoBufRPC);
begin
   Text.Add(Format('      procedure Send(%s);',[GenerateInputString(RpcArgumentInputName(aRpc), RpcArgumentInputType(aRpc))]));
end;

class procedure TProtoClientGenerator.AddInputStreamCloseAndRecv(var Text: TStringList; const aRpc: TProtoBufRPC);
begin
   if aRpc.ResultIsStream then
      Text.Add(       '      procedure CloseSend;')
   else
      Text.Add(Format('      function  CloseAndRecv(%s): Boolean;',[RpcOutputString(aRpc)]));
end;

class procedure TProtoClientGenerator.GenerateInputStreamInterface(var Text: TStringList; const aRpc: TProtoBufRPC);
begin
   Text.Add(Format('   %s = interface',[aRpc.InputStreamInterfaceType]));
   AddInterfaceGUID(Text);
   AddInputStreamInterfaceFunctions(Text, aRpc);
   Text.Add('   end;');
   Text.Add('');
end;

class procedure TProtoClientGenerator.GenerateInputStreamInterfaces(var Text: TStringList; const aService: TProtoBufService);
var RPC: TProtoBufRPC;
begin
   for RPC in aService do
   begin
      if RPC.ArgementIsStream then
         GenerateInputStreamInterface(Text, RPC);
   end;
end;

class function TProtoClientGenerator.GenerateServiceInterfaceName(const aService: TProtoBufService): string;
begin
   Result := Format('I%s_Client',[aService.Name]);
end;

class function TProtoClientGenerator.GenerateClassName(const aService: TProtoBufService): string;
begin
   Result := Format('T%s_Client',[aService.Name]);
end;

class procedure TProtoClientGenerator.GenerateServiceType(var Text: TStringList; const aService: TProtoBufService; const aType, aParentType: string; const aExtra: string = '');
var Rpc: TProtoBufRPC;
begin
   Text.Add(Format('   %s = %s',[aType, aParentType]));
   for Rpc in aService do
      Text.Add('      '+GenerateRpcDeclaration(Rpc));
   if aExtra <> '' then
      Text.Add('      '+aExtra);
   Text.Add('   end;');
   Text.Add('');
end;

class procedure TProtoClientGenerator.DetermineCallImplementationHelpVars(const aRpc: TProtoBufRPC; out IsSimpleFunc: Boolean;
   out RequestInputType, RequestOutput, DeserializeInput, RequestString: string);
var RequestInput, InputType, OutputType, CallbackName: string;
begin
   IsSimpleFunc := not (aRpc.ArgementIsStream or aRpc.ResultIsStream);
   if aRpc.ArgementIsStream then
      RequestInput := 'nil'
   else if aRpc.HasInput then
   begin
      if aRpc.RPCArgument.Has1Param then
         RequestInputType := StripFirstChar(aRpc.RPCArgument.DelphiClassname)+'_In'
      else
         RequestInputType := RpcFunctionInputName(aRpc);
      RequestInput := RequestInputType+', ';
   end
   else
      RequestInput := '';

   if aRpc.HasInput then
      InputType := aRpc.RPCArgument.DelphiClassname+','
   else
      InputType := '';

   if aRpc.Has1OutputParam then
      CallbackName := 'SubCallback'
   else if aRpc.ResultIsStream  then
      CallbackName := RpcOutputStreamInputName(aRpc);

   OutputType := aRpc.RPCResult.DelphiClassname;
   if IsSimpleFunc then
   begin
      RequestString := Format('DoRequest%s<%s%s>(%s''%s'')',[IfThen(not aRpc.HasInput, 'NoInput'),InputType, OutputType, RequestInput, aRpc.Name]);
      if aRpc.Has1OutputParam then
         RequestString := RequestString + '.'+aRpc.RPCResult[0].Name;
      if aRpc.HasOutput then
         RequestString := 'Result := '+RequestString;
      RequestString := RequestString +';';
   end
   else if aRpc.ArgementIsStream and aRpc.ResultIsStream then
      RequestString := Format('Result := DoInAndOutputStreamRequest<%s,%s>(''%s'', %s);',[aRpc.InputStreamType, OutputType, aRpc.Name, CallbackName])
   else if aRpc.ArgementIsStream then
      RequestString := Format('Result := DoInputStreamRequest<%s>(''%s'');',[aRpc.InputStreamType,aRpc.Name])
   else if aRpc.ResultIsStream then
      RequestString := Format('DoOuputStreamRequest<%s%s>(%s''%s'', %s);',[InputType, OutputType, RequestInput, aRpc.Name, CallbackName])
end;

class procedure TProtoClientGenerator.AddCallImplementationVars(var Text: TStringList; const aRpc: TProtoBufRPC;
   const IsSimpleFunc: Boolean; const RequestInputType, DeserializeInput, RequestOutput: string);
begin
   if aRpc.Has1InputParam and (not aRpc.ArgementIsStream) then
   begin
      Text.Add(Format('var %s: %s;',[RequestInputType, aRpc.RPCArgument.DelphiClassname]));
      if aRpc.ResultIsStream and aRpc.Has1OutputParam then
         Text.Add(Format('    SubCallback: TProtoCallback<%s>;',[aRpc.RPCResult.DelphiClassname]))
   end
   else if aRpc.ResultIsStream and aRpc.Has1OutputParam then
      Text.Add(Format('var SubCallback: TProtoCallback<%s>;',[aRpc.RPCResult.DelphiClassname]))
end;

class procedure TProtoClientGenerator.AddCallImplementationPreProcessing(var Text: TStringList; const aRpc: TProtoBufRPC; const IsSimpleFunc: Boolean; const RequestInputType: string);
begin
   if aRpc.Has1InputParam and (not aRpc.ArgementIsStream) then
      Text.Add(Format('   %s.%s := %s;',[RequestInputType, aRpc.RPCArgument[0].Name, RpcArgumentInputName(aRpc)]));
   if aRpc.ResultIsStream and aRpc.Has1OutputParam then
   begin
      Text.Add(Format('   SubCallback := procedure(const aInput: %s; const aHasData, aClosed: Boolean)',[aRpc.RPCResult.DelphiClassname]));
      Text.Add(       '      begin');
      Text.Add(Format('         %s(aInput.%s, aHasData, aClosed);',[RpcOutputStreamInputName(aRpc), aRpc.RPCResult[0].Name]));
      Text.Add(       '      end;');
   end;
end;

class function TProtoClientGenerator.GenerateRpcImplementation(const aRPC: TProtoBufRPC; const aService: TProtoBufService): string;
begin
   Result := GenerateRpcFunction(aRPC, GenerateClassName(aService) + '.');
end;

class procedure TProtoClientGenerator.GenerateClientCallImplementation(var Text: TStringList; const aRpc: TProtoBufRPC; const aService: TProtoBufService);
var RequestInputType, RequestOutput, DeserializeInput, RequestString: string;
    IsSimpleFunc: Boolean;
begin
   DetermineCallImplementationHelpVars(aRpc, IsSimpleFunc, RequestInputType, RequestOutput, DeserializeInput, RequestString);

   Text.Add(GenerateRpcImplementation(aRpc, aService));
   AddCallImplementationVars(Text, aRpc, IsSimpleFunc, RequestInputType, DeserializeInput, RequestOutput);

   Text.Add('begin');
   AddCallImplementationPreProcessing(Text, aRpc, IsSimpleFunc, RequestInputType);
   Text.Add('   '+RequestString);
   Text.Add('end;');
   Text.Add('');
end;

class procedure TProtoClientGenerator.GenerateClientClass(var Text: TStringList; const aService: TProtoBufService);
begin
   GenerateServiceType(Text, aService, GenerateClassName(aService),
      Format('class(%s,%s)',[IfThen(TProtoBaseGeneratorOptions.UseCustomClient,'TCustomGrpcClient','TBaseGrpcClient'), GenerateServiceInterfaceName(aService)]),
      'function BasePath: string; override;');
end;

class procedure TProtoClientGenerator.GenerateClientInterface(var Text: TStringList; const aService: TProtoBufService);
begin
   GenerateServiceType(Text, aService, GenerateServiceInterfaceName(aService), 'interface');
end;

class procedure TProtoClientGenerator.GenerateImplementation(var Text: TStringList; const aFile: TProtoFile);
begin
   GenerateClientImplementation(Text, aFile, CurrentService);
   GenerateStreamInImplementations(Text, CurrentService);
end;

class procedure TProtoClientGenerator.GenerateClientImplementation(var Text: TStringList; const aFile: TProtoFile; const aService: TProtoBufService);
var RPC: TProtoBufRPC;
    aName: string;
begin
   aName := GenerateClassName(aService);
   GenerateStartOfClass(Text, aName);
   Text.Add(Format('function %s.BasePath: string;',[aName]));
   Text.Add(       'begin');
   Text.Add(Format('   Result := %s;',[ServicePath(aFile, aService)]));
   Text.Add(       'end;');
   Text.Add('');
   for RPC in aService do
      GenerateClientCallImplementation(Text, RPC, aService);
end;

class procedure TProtoClientGenerator.GenerateStreamInDeclaration(var Text: TStringList; const aRpc: TProtoBufRPC);
var ClassString: string;
begin
   if aRpc.HasOutput then
      ClassString := Format('%s = class(TGrpcStream_Send<%s,%s>, %s)',[aRpc.InputStreamType, aRpc.RPCArgument.DelphiClassname, aRPC.RPCResult.DelphiClassname, aRpc.InputStreamInterfaceType])
   else
      ClassString := Format('%s = class(TGrpcStream_SendNoOut<%s>, %s)',[aRpc.InputStreamType, aRpc.RPCArgument.DelphiClassname, aRpc.InputStreamInterfaceType]);
   if (not aRpc.Has1InputParam) and (not aRpc.Has1OutputParam) then
      ClassString := ClassString+';';
   Text.Add('   '+ClassString);
   if aRpc.Has1InputParam then
      AddInputStreamSend(Text, aRpc);
   if aRpc.Has1OutputParam then
      AddInputStreamCloseAndRecv(Text, aRpc);
   if aRpc.Has1InputParam or aRpc.Has1OutputParam
    then
      Text.Add('   end;');
   Text.Add('');
end;

class procedure TProtoClientGenerator.GenerateStreamInDeclarations(var Text: TStringList; const aService: TProtoBufService);
var RPC: TProtoBufRPC;
begin
   for RPC in CurrentService do
   begin
      if RPC.ArgementIsStream then
         GenerateStreamInDeclaration(Text, RPC);
   end;
end;

class procedure TProtoClientGenerator.GenerateStreamInImplementation(var Text: TStringList; const aRpc: TProtoBufRPC);
var StreamClassname, InputName, ParamName, MsgName: string;
   NeedInputFunc, NeedOutputFunc: Boolean;
begin
   NeedInputFunc := aRpc.Has1InputParam;
   NeedOutputFunc := aRpc.Has1OutputParam and (not aRpc.ResultIsStream);
   if (not NeedInputFunc) and (not NeedOutputFunc) then
      Exit;
   StreamClassname := aRpc.InputStreamType;
   GenerateStartOfClass(Text, StreamClassname);
   if aRpc.Has1InputParam then
   begin
      InputName := RpcArgumentInputName(aRpc);
      MsgName := aRpc.RPCArgument.DelphiClassname;
      ParamName := StripFirstChar(MsgName);
      Text.Add(Format('procedure %s.Send(%s);',[StreamClassname, GenerateInputString(InputName, RpcArgumentInputType(aRpc))]));
      Text.Add(Format('var %s: %s;',[ParamName, MsgName]));
      Text.Add(       'begin');
      Text.Add(Format('   %s.%s := %s;',[Paramname, aRPC.RPCArgument[0].Name ,InputName]));
      Text.Add(Format('   inherited Send(%s);',[Paramname]));
      Text.Add(       'end;');
      Text.Add('');
   end;

   if aRpc.Has1OutputParam then
   begin
      MsgName := aRpc.RPCResult.DelphiClassname;
      ParamName := StripFirstChar(MsgName);
      Text.Add(Format('function %s.CloseAndRecv(%s): Boolean;',[StreamClassname, RpcOutputString(aRpc)]));
      Text.Add(Format('var %s: %s;',[ParamName, MsgName]));
      Text.Add(       'begin');
      Text.Add(Format('   Result := inherited CloseAndRecv(%s);',[Paramname]));
      Text.Add(Format('   %s := %s.%s;',[RpcMessageName(aRpc.RPCResult), Paramname, aRPC.RPCResult[0].Name]));
      Text.Add(       'end;');
      Text.Add('');
   end;
end;

class procedure TProtoClientGenerator.GenerateStreamInImplementations(var Text: TStringList; const aService: TProtoBufService);
var RPC: TProtoBufRPC;
begin
   for RPC in CurrentService do
   begin
      if RPC.ArgementIsStream then
         GenerateStreamInImplementation(Text, RPC);
   end;
end;

class procedure TProtoClientGenerator.GenerateInterface(var Text: TStringList; const aFile: TProtoFile);
begin
   Text.Add('type');
   GenerateOutputStreamCallbacks(Text, CurrentService);
   GenerateInputStreamInterfaces(Text, CurrentService);
   GenerateClientInterface(Text, CurrentService);
   GenerateClientClass(Text, CurrentService);
   GenerateStreamInDeclarations(Text, CurrentService);
end;

class function TProtoClientGenerator.GenerateInterfaceUses(const aFile: TProtoFile): string;
begin
   Result := 'Ultraware.Grpc.Client, '
      + TProtoTypesGenerator.GenerateImportedUnitNames(aFile)
      + TProtoTypesGenerator.GenerateUnitName(aFile)
      + IfThen(TProtoBaseGeneratorOptions.UseCustomClient,', Ultraware.Grpc.Client.Custom');
end;

class procedure TProtoClientGenerator.GenerateOutputStreamCallback(var Text: TStringList; const aRpc: TProtoBufRPC);
begin
   Text.Add(Format('   %s = TProtoCallback<%s>;', [aRpc.OutputCallbackType, RpcResultType(aRpc)]));
   Text.Add('');
end;

class procedure TProtoClientGenerator.GenerateOutputStreamCallbacks(var Text: TStringList; const aService: TProtoBufService);
var RPC: TProtoBufRPC;
begin
   for RPC in aService do
   begin
      if RPC.ResultIsStream then
         GenerateOutputStreamCallback(Text, RPC);
   end;
end;

class function TProtoClientGenerator.GenerateImplementationUses(const aFile: TProtoFile): string;
begin
   Result := '';
end;

class function TProtoClientGenerator.GenerateUnitName(const aFile: TProtoFile): string;
begin
   case aFile.ProtoBufServices.NonImportedCount of
      0: Result := '';
      1: Result := aFile.Name+'.Client';
   else
         Result := aFile.Name+'.'+CurrentService.Name+'.Client';
   end;
end;

class function TProtoClientGenerator.IsClient: Boolean;
begin
   Result := True;
end;

end.
