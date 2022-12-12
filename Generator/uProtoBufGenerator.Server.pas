unit uProtoBufGenerator.Server;

interface

uses System.Classes, uProtoBufGenerator.Service, uProtoBufParserClasses;

type
   TProtoServerGenerator = class(TProtoServiceGenerator)
   protected
      class function IsClient: Boolean; override;
   protected
      class function GenerateServiceInterfaceName(const aService: TProtoBufService): string;
      class function GenerateClassName(const aService: TProtoBufService): string;
   protected
      class function NeedsInternalFunction(const aRpc: TProtoBufRPC): Boolean;
      class procedure GenerateInputStreamInterface(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateOutputStreamInterface(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateInputStreamInterfaces(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateOutputStreamInterfaces(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateClientInterface(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateClientClass(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateClientClassImplementation(var Text: TStringList; const aFile: TProtoFile; const aService: TProtoBufService);
      class function GenerateClientCallRegistration(const aRpc: TProtoBufRPC): string;
      class function GenerateRpcInternalDeclaration(const aRpc: TProtoBufRPC; const aClassname: string): string;
      class procedure GenerateRpcInternalImplementation(var Text: TStringList; const aRpc: TProtoBufRPC; const aService: TProtoBufService);
      class function GenerateRpcInternalDeclarationWithClassname(const aRpc: TProtoBufRPC; const aService: TProtoBufService): string;
      class procedure GenerateInputStreamDeclaration(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateOutputStreamDeclaration(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateInputStreamDeclarations(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateOutputStreamDeclarations(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateInputStreamImplementation(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateInputStreamImplementations(var Text: TStringList; const aService: TProtoBufService);
      class procedure GenerateOutputStreamImplementation(var Text: TStringList; const aRpc: TProtoBufRPC);
      class procedure GenerateOutputStreamImplementations(var Text: TStringList; const aService: TProtoBufService);
   protected
      class function GenerateInterfaceUses(const aFile: TProtoFile): string; override;
      class function GenerateImplementationUses(const aFile: TProtoFile): string; override;
      class procedure GenerateInterface(var Text: TStringList; const aFile: TProtoFile); override;
      class procedure GenerateImplementation(var Text: TStringList; const aFile: TProtoFile); override;
   public
      class function GenerateUnitName(const aFile: TProtoFile): string; override;
   end;

implementation

uses System.SysUtils, System.StrUtils, uProtoBufGenerator.Types, uProtoBufGenerator, uProtoBufGenerator.Helper;

{ TProtoServerGenerator }

class function TProtoServerGenerator.GenerateUnitName(const aFile: TProtoFile): string;
begin
   case aFile.ProtoBufServices.Count of
      0: Result := '';
      1: Result := aFile.Name+'.Server';
   else
         Result := aFile.Name+'.'+CurrentService.Name+'.Server';
   end;
end;

class function TProtoServerGenerator.IsClient: Boolean;
begin
   Result := False;
end;

class function TProtoServerGenerator.GenerateInterfaceUses(const aFile: TProtoFile): string;
begin
   Result := 'Ultraware.Grpc.Server, '
      + TProtoTypesGenerator.GenerateImportedUnitNames(aFile)
      + TProtoTypesGenerator.GenerateUnitName(aFile)
      + IfThen(TProtoBaseGeneratorOptions.UseCustomServer,', Ultraware.Grpc.Server.Custom');
end;

class function TProtoServerGenerator.GenerateImplementationUses(const aFile: TProtoFile): string;
begin
   Result := '';
end;

class procedure TProtoServerGenerator.GenerateInterface(var Text: TStringList; const aFile: TProtoFile);
begin
   Text.Add('type');
   GenerateInputStreamInterfaces(Text, CurrentService);
   GenerateOutputStreamInterfaces(Text, CurrentService);
   GenerateClientInterface(Text, CurrentService);
   GenerateClientClass(Text, CurrentService);
   GenerateInputStreamDeclarations(Text, CurrentService);
   GenerateOutputStreamDeclarations(Text, CurrentService);
end;

class procedure TProtoServerGenerator.GenerateOutputStreamDeclaration(var Text: TStringList; const aRpc: TProtoBufRPC);
var CustomSend: Boolean;
begin
   CustomSend := aRpc.Has1OutputParam;
   Text.Add(Format('   %s = class(TServerCallbackStream<%s>, %s)%s',[aRpc.ServerOutputClassType, aRpc.RPCResult.DelphiClassname, aRpc.ServerOutputInterfaceType, IfThen(not CustomSend,';')]));
   if CustomSend then
   begin
      Text.Add(Format('      procedure Send(%s);',[RpcServerOutputString(aRpc)]));
      Text.Add(       '   end;');
   end;
   Text.Add('');
end;

class procedure TProtoServerGenerator.GenerateOutputStreamDeclarations(var Text: TStringList; const aService: TProtoBufService);
var Rpc: TProtoBufRPC;
begin
   for Rpc in aService do
   begin
      if RPC.ResultIsStream then
         GenerateOutputStreamDeclaration(Text, Rpc);
   end;
end;

class procedure TProtoServerGenerator.GenerateOutputStreamImplementation(var Text: TStringList; const aRpc: TProtoBufRPC);
var StreamClassname,ParamName, MsgName: string;
begin
   if (not aRpc.Has1InputParam) then
      Exit;
   StreamClassname := aRpc.ServerInputStreamClassType;
   GenerateStartOfClass(Text, StreamClassname);

   MsgName := aRpc.RPCArgument.DelphiClassname;
   ParamName := StripFirstChar(MsgName);
   Text.Add(Format('function %s.Recv(out %s: %s; const aWaitTimeout: Integer): TGrpcWaitResult;',[StreamClassname, RpcArgumentInputName(aRpc), RpcArgumentInputType(aRpc)]));
   Text.Add(Format('var %s: %s;',[ParamName, MsgName]));
   Text.Add(       'begin');
   Text.Add(Format('   Result := inherited Recv(%s, aWaitTimeout);',[Paramname]));
   Text.Add(Format('   %s := %s.%s;',[RpcMessageName(aRpc.RPCArgument), Paramname, aRPC.RPCArgument[0].Name]));
   Text.Add(       'end;');
   Text.Add('');
end;

class procedure TProtoServerGenerator.GenerateOutputStreamImplementations(var Text: TStringList; const aService: TProtoBufService);
var Rpc: TProtoBufRPC;
begin
   for Rpc in aService do
   begin
      if RPC.ArgementIsStream then
         GenerateOutputStreamImplementation(Text, Rpc);
   end;
end;

class procedure TProtoServerGenerator.GenerateOutputStreamInterface(var Text: TStringList; const aRpc: TProtoBufRPC);
begin
   Text.Add(Format('   %s = interface(ICallbackStream)',[aRpc.ServerOutputInterfaceType]));
   AddInterfaceGUID(Text);
   Text.Add(Format('      procedure Send(%s);',[RpcServerOutputString(aRpc)]));
   Text.Add(       '   end;');
   Text.Add('');
end;

class procedure TProtoServerGenerator.GenerateOutputStreamInterfaces(var Text: TStringList; const aService: TProtoBufService);
var Rpc: TProtoBufRPC;
begin
   for Rpc in aService do
   begin
      if RPC.ResultIsStream then
         GenerateOutputStreamInterface(Text, Rpc);
   end;
end;

class procedure TProtoServerGenerator.GenerateInputStreamDeclaration(var Text: TStringList; const aRpc: TProtoBufRPC);
var CustomSend: Boolean;
begin
   CustomSend := aRpc.Has1InputParam;
   Text.Add(Format('   %s = class(TBaseGrpcMemStream<%s>, %s)%s',[aRpc.ServerInputStreamClassType, aRpc.RPCArgument.DelphiClassname, aRpc.ServerInputStreamInterfaceType, IfThen(not CustomSend,';')]));
   if CustomSend then
   begin
      Text.Add(Format('      function Recv(out %s: %s; const aWaitTimeout: Integer): TGrpcWaitResult;',[RpcArgumentInputName(aRpc), RpcArgumentInputType(aRpc)]));
      Text.Add(       '   end;');
   end;
   Text.Add('');
end;

class procedure TProtoServerGenerator.GenerateInputStreamDeclarations(var Text: TStringList; const aService: TProtoBufService);
var Rpc: TProtoBufRPC;
begin
   for Rpc in aService do
   begin
      if RPC.ArgementIsStream then
         GenerateInputStreamDeclaration(Text, Rpc);
   end;
end;

class procedure TProtoServerGenerator.GenerateInputStreamImplementation(var Text: TStringList; const aRpc: TProtoBufRPC);
var StreamClassname, InputName, ParamName, MsgName: string;
begin
   if (not aRpc.Has1OutputParam) then
      Exit;
   StreamClassname := aRpc.ServerOutputClassType;
   GenerateStartOfClass(Text, StreamClassname);
   InputName := RpcMessageName(aRpc.RPCResult);
   MsgName := aRpc.RPCResult.DelphiClassname;
   ParamName := StripFirstChar(MsgName);
   Text.Add(Format('procedure %s.Send(%s);',[StreamClassname, GenerateInputString(InputName, RpcResultType(aRpc))]));
   Text.Add(Format('var %s: %s;',[ParamName, MsgName]));
   Text.Add(       'begin');
   Text.Add(Format('   %s.%s := %s;',[Paramname, aRPC.RPCResult[0].Name ,InputName]));
   Text.Add(Format('   inherited Send(%s);',[Paramname]));
   Text.Add(       'end;');
   Text.Add('');
end;

class procedure TProtoServerGenerator.GenerateInputStreamImplementations(var Text: TStringList; const aService: TProtoBufService);
var Rpc: TProtoBufRPC;
begin
   for Rpc in aService do
   begin
      if RPC.ResultIsStream then
         GenerateInputStreamImplementation(Text, Rpc);
   end;
end;

class procedure TProtoServerGenerator.GenerateInputStreamInterface(var Text: TStringList; const aRpc: TProtoBufRPC);
begin
   Text.Add(Format('   %s = interface(IGrpcMemStream)',[aRpc.ServerInputStreamInterfaceType]));
   AddInterfaceGUID(Text);
   Text.Add(Format('      function Recv(out %s: %s; const aWaitTimeout: Integer): TGrpcWaitResult;',[RpcArgumentInputName(aRpc), RpcArgumentInputType(aRpc)]));
   Text.Add(       '   end;');
   Text.Add('');
end;

class procedure TProtoServerGenerator.GenerateInputStreamInterfaces(var Text: TStringList; const aService: TProtoBufService);
var Rpc: TProtoBufRPC;
begin
   for Rpc in aService do
   begin
      if RPC.ArgementIsStream then
         GenerateInputStreamInterface(Text, Rpc);
   end;
end;

class procedure TProtoServerGenerator.GenerateClientClass(var Text: TStringList; const aService: TProtoBufService);
var Rpc: TProtoBufRPC;
    IsFirstInternal: Boolean;
begin
   Text.Add(Format('   %s = class(%s, %s)',[GenerateClassName(aService), IfThen(TProtoBaseGeneratorOptions.UseCustomServer,'TCustomGrpcImplementation','TBaseGrpcImplementation'), GenerateServiceInterfaceName(aService)]));
   Text.Add('   protected');
   Text.Add('      procedure DoRegisterCalls; override;');
   Text.Add('   public');
   Text.Add('      class function BasePath: string; override;');
   IsFirstInternal := True;
   for Rpc in aService do
   begin
      if NeedsInternalFunction(Rpc) then
      begin
         if IsFirstInternal then
         begin
            Text.Add('   private');
            IsFirstInternal := False;
         end;
         Text.Add('      '+GenerateRpcInternalDeclaration(Rpc, ''{No Classname}));
      end;
   end;
   Text.Add('   public');
   for Rpc in aService do
      Text.Add('      '+GenerateRpcDeclaration(Rpc)+' virtual; abstract;');
   Text.Add('   end;');
   Text.Add('');
end;

class procedure TProtoServerGenerator.GenerateClientClassImplementation(var Text: TStringList; const aFile: TProtoFile; const aService: TProtoBufService);
var RPC: TProtoBufRPC;
    aName: string;
begin
   aName := GenerateClassName(aService);
   GenerateStartOfClass(Text, aName);
   Text.Add(Format('class function %s.BasePath: string;',[aName]));
   Text.Add(       'begin');
   Text.Add(Format('   Result := %s;',[ServicePath(aFile, aService)]));
   Text.Add(       'end;');
   Text.Add('');

   Text.Add(Format('procedure %s.DoRegisterCalls;',[aName]));
   Text.Add(       'begin');
   for RPC in aService do
      Text.Add(    '   '+GenerateClientCallRegistration(RPC));
   Text.Add(       'end;');
   Text.Add('');

   for Rpc in aService do
   begin
      if NeedsInternalFunction(Rpc) then
         GenerateRpcInternalImplementation(Text, Rpc, aService);
   end;
end;

class function TProtoServerGenerator.NeedsInternalFunction(const aRpc: TProtoBufRPC): Boolean;
begin
   if aRpc.ArgementIsStream and aRpc.ResultIsStream then
      Result := False
   else if aRpc.ArgementIsStream then
      Result := aRpc.Has1OutputParam or (not aRpc.HasOutput)
   else if aRpc.ResultIsStream then
      Result := aRpc.Has1InputParam
   else
      Result := aRpc.Has1InputParam or aRpc.Has1OutputParam or (not aRpc.HasOutput)
end;

class function TProtoServerGenerator.GenerateRpcInternalDeclaration(const aRpc: TProtoBufRPC; const aClassname: string): string;
   function _ArgumentType: string;
   begin
      Result := aRpc.RPCArgument.DelphiClassname;
   end;

   function _ResultType: string;
   begin
      Result := aRpc.RPCResult.DelphiClassname;
   end;

var Name: string;
begin
   Name := 'Internal'+aRpc.Name;
   if aClassname <> '' then
      Name := aClassname+'.'+Name;
   if aRpc.ArgementIsStream and aRpc.ResultIsStream then
      Result := ''
   else if aRpc.ArgementIsStream then
      Result := Format('function %s(%s): %s;',[Name, GenerateInputString(aRpc.ServerInputStreamInterfaceType), _ResultType])
   else if aRpc.ResultIsStream then
      Result := Format('procedure %s(%s; %s);',[Name, GenerateInputString(_ArgumentType), GenerateInputString(aRpc.ServerOutputInterfaceType)])
   else if aRpc.HasInput then
      Result := Format('function %s(%s): %s;',[Name, GenerateInputString(_ArgumentType), _ResultType])
   else
      Result := Format('function %s(): %s;',[Name, _ResultType])
end;

class procedure TProtoServerGenerator.GenerateRpcInternalImplementation(var Text: TStringList; const aRpc: TProtoBufRPC; const aService: TProtoBufService);
var HasResult: Boolean;
    InternalCode, FunctionInput: string;
begin
   HasResult := aRpc.HasOutput and (not aRpc.ResultIsStream);
   if aRpc.ArgementIsStream then
      FunctionInput := ConvertToParamName(aRpc.ServerInputStreamInterfaceType)
   else if aRpc.ResultIsStream then
      FunctionInput := Format('%s.%s, %s',[ConvertToParamName(aRpc.RPCArgument.DelphiClassname), aRpc.RPCArgument[0].Name, ConvertToParamName(aRpc.ServerOutputInterfaceType)])
   else if aRpc.HasInput then
   begin
      FunctionInput := ConvertToParamName(aRpc.RPCArgument.DelphiClassname);
      if aRpc.Has1InputParam then
         FunctionInput := FunctionInput+'.'+aRpc.RPCArgument[0].Name;
   end
   else
      FunctionInput := ''; // Default call without input
   InternalCode := Format('%s(%s);',[aRpc.Name, FunctionInput]);
   if HasResult then
      InternalCode := Format('Result%s := %s',[IfThen(aRpc.Has1OutputParam,'.'+aRPC.RPCResult[0].Name), InternalCode]);

   Text.Add(GenerateRpcInternalDeclarationWithClassname(aRpc, aService));
   Text.Add(       'begin');
   Text.Add(       '   '+InternalCode);
   Text.Add(       'end;');
   Text.Add('');
end;

class function TProtoServerGenerator.GenerateRpcInternalDeclarationWithClassname(const aRpc: TProtoBufRPC; const aService: TProtoBufService): string;
begin
   Result := GenerateRpcInternalDeclaration(aRpc, GenerateClassName(aService));
end;

class function TProtoServerGenerator.GenerateClientCallRegistration(const aRpc: TProtoBufRPC): string;
var GenericTypes, Name, RegisterName, FunctionName: string;
begin
   Result := '';
   Name := aRpc.Name;
   if NeedsInternalFunction(aRpc) then
      FunctionName := 'Internal'+Name
   else
      FunctionName := Name;

   if aRpc.ArgementIsStream then
   begin
      if aRpc.ResultIsStream then
      begin
         RegisterName := 'RegisterStreamCall';
         GenericTypes := Format('%s,%s,%s,%s',[aRpc.ServerInputStreamClassType, aRpc.ServerInputStreamInterfaceType,aRpc.ServerOutputClassType, aRpc.ServerOutputInterfaceType]);
      end
      else
      begin
         RegisterName := 'RegisterInputStreamCall';
         GenericTypes := Format('%s,%s,%s',[aRpc.ServerInputStreamClassType, aRpc.ServerInputStreamInterfaceType,aRpc.RPCResult.DelphiClassname]);
      end;
   end
   else if aRpc.ResultIsStream then
   begin
      RegisterName := 'RegisterOutputStreamCall';
      GenericTypes := Format('%s,%s',[aRpc.ServerOutputClassType, aRpc.ServerOutputInterfaceType]);
      if aRpc.HasInput then
         GenericTypes := aRpc.RPCArgument.DelphiClassname+','+GenericTypes
      else
         RegisterName := RegisterName+'NoInput';
   end
   else
   begin
      RegisterName := 'RegisterCall';
      GenericTypes := aRpc.RPCResult.DelphiClassname;
      if aRpc.HasInput then
         GenericTypes := aRpc.RPCArgument.DelphiClassname+','+GenericTypes
      else
         RegisterName := RegisterName+'NoInput';
   end;
   Result := Format('%s<%s>(''%s'',%s);',[RegisterName, GenericTypes, Name, FunctionName])
end;

class procedure TProtoServerGenerator.GenerateClientInterface(var Text: TStringList; const aService: TProtoBufService);
var Rpc: TProtoBufRPC;
begin
   Text.Add(Format('   %s = interface',[GenerateServiceInterfaceName(aService)]));
   for Rpc in aService do
      Text.Add('      '+GenerateRpcDeclaration(Rpc));
   Text.Add('   end;');
   Text.Add('');
end;

class function TProtoServerGenerator.GenerateServiceInterfaceName(const aService: TProtoBufService): string;
begin
   Result := Format('I%s_Server',[aService.Name]);
end;

class function TProtoServerGenerator.GenerateClassName(const aService: TProtoBufService): string;
begin
   Result := Format('T%s_Server',[aService.Name]);
end;

class procedure TProtoServerGenerator.GenerateImplementation(var Text: TStringList; const aFile: TProtoFile);
begin
   GenerateClientClassImplementation(Text, aFile, CurrentService);
   GenerateInputStreamImplementations(Text, CurrentService);
   GenerateOutputStreamImplementations(Text, CurrentService);
end;

end.
