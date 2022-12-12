unit GrpcExportClientTester;

interface

uses GrpcTestHelper, uProtoBufParserClasses;

type
   TGrpcExportClientTester = class(TGrpcTestHelperExport)
   private
      procedure TestCallDeclaration(const aCall: TTestServiceCall; const aExpected: string);
      procedure TestCallImplementation(const aCall: TTestServiceCall; const aExpected: string);
   published
      // Declaraties
      procedure TestSimpleFuncDeclaration;
      procedure TestStreamInputFuncDeclaration;
      procedure TestStreamOutputFuncDeclaration;
      procedure TestStreamInAndOutputFuncDeclaration;
      procedure TestInputStreamTypeDeclaration;
      procedure TestInputStreamTypeImplementation;
      procedure TestOutputStreamTypeDeclaration;
      procedure TestInterfaceDeclaration;

      procedure TestSimpleFuncImplementation;
      procedure TestStreamInputFuncImplementation;
      procedure TestStreamOutputFuncImplementation;
      procedure TestStreamInAndOutputFuncImplementation;
      procedure TestGenerateUses;
      procedure TestClientClass;
//      procedure TestCompleteFile;
   end;

implementation

uses uProtoBufGenerator.Client, uProtoBufGenerator.Types, uProtoBufGenerator.Server;

type
   TProtoClientGeneratorUnitTest = class(TProtoClientGenerator);

{ TGrpcExportClientTester }

procedure TGrpcExportClientTester.TestGenerateUses;
begin
   Parse([srvSimple]);
   CheckEquals('Ultraware.Grpc.Client, GrpcTest.Proto',TProtoClientGeneratorUnitTest.GenerateInterfaceUses(FProto));
end;

procedure TGrpcExportClientTester.TestClientClass;
begin
   Parse([srvSimple]);
   TProtoClientGeneratorUnitTest.GenerateClientClass(FText, GetServiceTest);
   CheckEquals(
      'TTestService_Client = class(TBaseGrpcClient,ITestService_Client) '+
         'function Simple(const aMsgSimple: TMsgSimple): TMsgSimple; '+
         'function BasePath: string; override; '+
      'end;', TextWithoutLinebreaks);
   FText.Clear;
   TProtoClientGeneratorUnitTest.GenerateClientImplementation(FText, FProto, GetServiceTest);
   CheckEquals(
      '{ TTestService_Client } '+
      'function TTestService_Client.BasePath: string; '+
      'begin '+
         'Result := ''/GrpcTest.TestService/''; '+
      'end; '+
      'function TTestService_Client.Simple(const aMsgSimple: TMsgSimple): TMsgSimple; '+
      'begin '+
         'Result := DoRequest<TMsgSimple,TMsgSimple>(aMsgSimple, ''Simple''); '+
      'end;', TextWithoutLinebreaks);
end;

procedure TGrpcExportClientTester.TestCallDeclaration(const aCall: TTestServiceCall; const aExpected: string);
begin
   Parse([aCall]);
   CheckEquals(aExpected, TProtoClientGeneratorUnitTest.GenerateRpcDeclaration(GetServiceTest[0]));
end;

procedure TGrpcExportClientTester.TestSimpleFuncDeclaration;
begin
   TestCallDeclaration(srvSimple,                  'function Simple(const aMsgSimple: TMsgSimple): TMsgSimple;');
   TestCallDeclaration(srvSimpleNoIn,              'function SimpleNoIn(): TMsgSimple;');
   TestCallDeclaration(srvSimple1ParamIn,          'function Simple1ParamIn(const aIntegerField: Integer): TMsgSimple;');
   TestCallDeclaration(srvSimple1ParamOut,         'function Simple1ParamOut(const aMsgSimple: TMsgSimple): Integer;');
   TestCallDeclaration(srvSimpleEmptyParamOut,     'procedure SimpleEmptyParamOut(const aMsgSimple: TMsgSimple);');
   TestCallDeclaration(srvSimple1Param,            'function Simple1Param(const aIntegerField: Integer): Integer;');
   TestCallDeclaration(srvSimple1ParamInNoOut,     'procedure Simple1ParamInNoOut(const aIntegerField: Integer);');
   TestCallDeclaration(srvSimple1ParamOutNoIn,     'function Simple1ParamOutNoIn(): Integer;');
   TestCallDeclaration(srvSimpleNoParam,           'procedure SimpleNoParam();');
end;

procedure TGrpcExportClientTester.TestStreamInputFuncDeclaration;
begin
   TestCallDeclaration(srvStreamIn,                'function StreamIn(): IStreamInStream_Send;');
   TestCallDeclaration(srvStreamIn1ParamIn,        'function StreamIn1ParamIn(): IStreamIn1ParamInStream_Send;');
   TestCallDeclaration(srvStreamIn1ParamOut,       'function StreamIn1ParamOut(): IStreamIn1ParamOutStream_Send;');
   TestCallDeclaration(srvStreamInEmptyParamOut,   'function StreamInEmptyParamOut(): IStreamInEmptyParamOutStream_Send;');
   TestCallDeclaration(srvStreamIn1Param,          'function StreamIn1Param(): IStreamIn1ParamStream_Send;');
   TestCallDeclaration(srvStreamIn1ParamInNoOut,   'function StreamIn1ParamInNoOut(): IStreamIn1ParamInNoOutStream_Send;');
end;

procedure TGrpcExportClientTester.TestStreamOutputFuncDeclaration;
begin
   TestCallDeclaration(srvStreamOut,               'procedure StreamOut(const aMsgSimple: TMsgSimple; const aStreamOutCallback: TStreamOutCallback);');
   TestCallDeclaration(srvStreamOutNoIn,           'procedure StreamOutNoIn(const aStreamOutNoInCallback: TStreamOutNoInCallback);');
   TestCallDeclaration(srvStreamOut1ParamIn,       'procedure StreamOut1ParamIn(const aIntegerField: Integer; const aStreamOut1ParamInCallback: TStreamOut1ParamInCallback);');
   TestCallDeclaration(srvStreamOut1ParamOut,      'procedure StreamOut1ParamOut(const aMsgSimple: TMsgSimple; const aStreamOut1ParamOutCallback: TStreamOut1ParamOutCallback);');
   TestCallDeclaration(srvStreamOut1Param,         'procedure StreamOut1Param(const aIntegerField: Integer; const aStreamOut1ParamCallback: TStreamOut1ParamCallback);');
   TestCallDeclaration(srvStreamOut1ParamOutNoIn,  'procedure StreamOut1ParamOutNoIn(const aStreamOut1ParamOutNoInCallback: TStreamOut1ParamOutNoInCallback);');
end;

procedure TGrpcExportClientTester.TestStreamInAndOutputFuncDeclaration;
begin
   TestCallDeclaration(srvStreamBoth,              'function StreamBoth(const aStreamBothCallback: TStreamBothCallback): IStreamBothStream_Send;');
   TestCallDeclaration(srvStreamBoth1ParamIn,      'function StreamBoth1ParamIn(const aStreamBoth1ParamInCallback: TStreamBoth1ParamInCallback): IStreamBoth1ParamInStream_Send;');
   TestCallDeclaration(srvStreamBoth1ParamOut,     'function StreamBoth1ParamOut(const aStreamBoth1ParamOutCallback: TStreamBoth1ParamOutCallback): IStreamBoth1ParamOutStream_Send;');
   TestCallDeclaration(srvStreamBoth1Param,        'function StreamBoth1Param(const aStreamBoth1ParamCallback: TStreamBoth1ParamCallback): IStreamBoth1ParamStream_Send;');
end;

procedure TGrpcExportClientTester.TestInputStreamTypeDeclaration;

   procedure _Test(const aCall: TTestServiceCall; const aExpectedInterface, aExpectedClass: string);
   begin
      Parse([aCall]);
      TProtoClientGeneratorUnitTest.GenerateInputStreamInterface(FText, GetServiceTest[0]);
      CheckEquals(aExpectedInterface, TextWithoutLinebreaks);
      FText.Clear;
      TProtoClientGeneratorUnitTest.GenerateStreamInDeclaration(FText, GetServiceTest[0]);
      CheckEquals(aExpectedClass, TextWithoutLinebreaks);
   end;

begin
   _Test(srvStreamIn,
      'IStreamInStream_Send = interface procedure Send(const aMsgSimple: TMsgSimple); function CloseAndRecv(out aMsgSimple: TMsgSimple): Boolean; end;',
      'TStreamInStream_Send = class(TGrpcStream_Send<TMsgSimple,TMsgSimple>, IStreamInStream_Send);');
   _Test(srvStreamBoth,
      'IStreamBothStream_Send = interface procedure Send(const aMsgSimple: TMsgSimple); procedure CloseSend; end;',
      'TStreamBothStream_Send = class(TGrpcStream_Send<TMsgSimple,TMsgSimple>, IStreamBothStream_Send);');
   _Test(srvStreamIn1ParamIn,
      'IStreamIn1ParamInStream_Send = interface procedure Send(const aIntegerField: Integer); function CloseAndRecv(out aMsgSimple: TMsgSimple): Boolean; end;',
      'TStreamIn1ParamInStream_Send = class(TGrpcStream_Send<TMsg1Param,TMsgSimple>, IStreamIn1ParamInStream_Send) procedure Send(const aIntegerField: Integer); end;');
   _Test(srvStreamIn1ParamOut,
      'IStreamIn1ParamOutStream_Send = interface procedure Send(const aMsgSimple: TMsgSimple); function CloseAndRecv(out aIntegerField: Integer): Boolean; end;',
      'TStreamIn1ParamOutStream_Send = class(TGrpcStream_Send<TMsgSimple,TMsg1Param>, IStreamIn1ParamOutStream_Send) function CloseAndRecv(out aIntegerField: Integer): Boolean; end;');
   _Test(srvStreamInEmptyParamOut,
      'IStreamInEmptyParamOutStream_Send = interface procedure Send(const aMsgSimple: TMsgSimple); function CloseAndRecv(): Boolean; end;',
      'TStreamInEmptyParamOutStream_Send = class(TGrpcStream_SendNoOut<TMsgSimple>, IStreamInEmptyParamOutStream_Send);');
   _Test(srvStreamIn1Param,
      'IStreamIn1ParamStream_Send = interface procedure Send(const aIntegerField: Integer); function CloseAndRecv(out aIntegerField: Integer): Boolean; end;',
      'TStreamIn1ParamStream_Send = class(TGrpcStream_Send<TMsg1Param,TMsg1Param>, IStreamIn1ParamStream_Send) procedure Send(const aIntegerField: Integer); function CloseAndRecv(out aIntegerField: Integer): Boolean; end;');
   _Test(srvStreamIn1ParamInNoOut,
      'IStreamIn1ParamInNoOutStream_Send = interface procedure Send(const aIntegerField: Integer); function CloseAndRecv(): Boolean; end;',
      'TStreamIn1ParamInNoOutStream_Send = class(TGrpcStream_SendNoOut<TMsg1Param>, IStreamIn1ParamInNoOutStream_Send) procedure Send(const aIntegerField: Integer); end;');
end;

procedure TGrpcExportClientTester.TestInputStreamTypeImplementation;
begin
   Parse([srvStreamIn1Param]);
   TProtoClientGeneratorUnitTest.GenerateStreamInImplementation(FText, GetServiceTest[0]);
   CheckEquals(
      '{ TStreamIn1ParamStream_Send } '+
      'procedure TStreamIn1ParamStream_Send.Send(const aIntegerField: Integer); '+
      'var Msg1Param: TMsg1Param; '+
      'begin '+
         'Msg1Param.IntegerField := aIntegerField; '+
         'inherited Send(Msg1Param); '+
      'end; '+
      'function TStreamIn1ParamStream_Send.CloseAndRecv(out aIntegerField: Integer): Boolean; '+
      'var Msg1Param: TMsg1Param; '+
      'begin '+
         'Result := inherited CloseAndRecv(Msg1Param); '+
         'aIntegerField := Msg1Param.IntegerField; '+
      'end;',TextWithoutLinebreaks);
end;

procedure TGrpcExportClientTester.TestInterfaceDeclaration;
begin
   Parse(C_AllCalls);
   TProtoClientGeneratorUnitTest.GenerateClientInterface(FText, GetServiceTest);
   CheckEquals('ITestService_Client = interface '+
      'function Simple(const aMsgSimple: TMsgSimple): TMsgSimple; '+
      'function SimpleNoIn(): TMsgSimple; '+
      'function Simple1ParamIn(const aIntegerField: Integer): TMsgSimple; '+
      'function Simple1ParamOut(const aMsgSimple: TMsgSimple): Integer; '+
      'procedure SimpleEmptyParamOut(const aMsgSimple: TMsgSimple); '+
      'function Simple1Param(const aIntegerField: Integer): Integer; '+
      'procedure Simple1ParamInNoOut(const aIntegerField: Integer); '+
      'function Simple1ParamOutNoIn(): Integer; '+
      'procedure SimpleNoParam(); '+
      'function StreamIn(): IStreamInStream_Send; '+
      'function StreamIn1ParamIn(): IStreamIn1ParamInStream_Send; '+
      'function StreamIn1ParamOut(): IStreamIn1ParamOutStream_Send; '+
      'function StreamInEmptyParamOut(): IStreamInEmptyParamOutStream_Send; '+
      'function StreamIn1Param(): IStreamIn1ParamStream_Send; '+
      'function StreamIn1ParamInNoOut(): IStreamIn1ParamInNoOutStream_Send; '+
      'procedure StreamOut(const aMsgSimple: TMsgSimple; const aStreamOutCallback: TStreamOutCallback); '+
      'procedure StreamOutNoIn(const aStreamOutNoInCallback: TStreamOutNoInCallback); '+
      'procedure StreamOut1ParamIn(const aIntegerField: Integer; const aStreamOut1ParamInCallback: TStreamOut1ParamInCallback); '+
      'procedure StreamOut1ParamOut(const aMsgSimple: TMsgSimple; const aStreamOut1ParamOutCallback: TStreamOut1ParamOutCallback); '+
      'procedure StreamOut1Param(const aIntegerField: Integer; const aStreamOut1ParamCallback: TStreamOut1ParamCallback); '+
      'procedure StreamOut1ParamOutNoIn(const aStreamOut1ParamOutNoInCallback: TStreamOut1ParamOutNoInCallback); '+
      'function StreamBoth(const aStreamBothCallback: TStreamBothCallback): IStreamBothStream_Send; '+
      'function StreamBoth1ParamIn(const aStreamBoth1ParamInCallback: TStreamBoth1ParamInCallback): IStreamBoth1ParamInStream_Send; '+
      'function StreamBoth1ParamOut(const aStreamBoth1ParamOutCallback: TStreamBoth1ParamOutCallback): IStreamBoth1ParamOutStream_Send; '+
      'function StreamBoth1Param(const aStreamBoth1ParamCallback: TStreamBoth1ParamCallback): IStreamBoth1ParamStream_Send; '+
      'end;',TextWithoutLinebreaks);
end;

procedure TGrpcExportClientTester.TestOutputStreamTypeDeclaration;
   procedure _Test(const aCall: TTestServiceCall; const aExpected: string);
   begin
      Parse([aCall]);
      TProtoClientGeneratorUnitTest.GenerateOutputStreamCallback(FText, GetServiceTest[0]);
      CheckEquals(aExpected, TextWithoutLinebreaks);
   end;
begin
   _Test(srvStreamOut, 'TStreamOutCallback = TProtoCallback<TMsgSimple>;');
   _Test(srvStreamOut1Param, 'TStreamOut1ParamCallback = TProtoCallback<Integer>;');
end;

procedure TGrpcExportClientTester.TestCallImplementation(const aCall: TTestServiceCall; const aExpected: string);
var srv: TProtoBufService;
begin
   Parse([aCall]);
   srv := GetServiceTest;
   TProtoClientGeneratorUnitTest.GenerateClientCallImplementation(FText, srv[0], srv);
   CheckEquals(aExpected, TextWithoutLinebreaks);
end;

procedure TGrpcExportClientTester.TestSimpleFuncImplementation;
begin
   TestCallImplementation(srvSimple,               'function TTestService_Client.Simple(const aMsgSimple: TMsgSimple): TMsgSimple; begin Result := DoRequest<TMsgSimple,TMsgSimple>(aMsgSimple, ''Simple''); end;');
   TestCallImplementation(srvSimpleNoIn,           'function TTestService_Client.SimpleNoIn(): TMsgSimple; begin Result := DoRequestNoInput<TMsgSimple>(''SimpleNoIn''); end;');
   TestCallImplementation(srvSimple1ParamIn,       'function TTestService_Client.Simple1ParamIn(const aIntegerField: Integer): TMsgSimple; var Msg1Param_In: TMsg1Param; begin Msg1Param_In.IntegerField := aIntegerField;'+' Result := DoRequest<TMsg1Param,TMsgSimple>(Msg1Param_In, ''Simple1ParamIn''); end;');
   TestCallImplementation(srvSimple1ParamOut,      'function TTestService_Client.Simple1ParamOut(const aMsgSimple: TMsgSimple): Integer; begin Result := DoRequest<TMsgSimple,TMsg1Param>(aMsgSimple, ''Simple1ParamOut'').IntegerField; end;');
   TestCallImplementation(srvSimpleEmptyParamOut,  'procedure TTestService_Client.SimpleEmptyParamOut(const aMsgSimple: TMsgSimple); begin DoRequest<TMsgSimple,TMsgEmpty>(aMsgSimple, ''SimpleEmptyParamOut''); end;');
   TestCallImplementation(srvSimple1ParamInNoOut,  'procedure TTestService_Client.Simple1ParamInNoOut(const aIntegerField: Integer); var Msg1Param_In: TMsg1Param; begin Msg1Param_In.IntegerField := aIntegerField; DoRequest<TMsg1Param,TMsgEmpty>(Msg1Param_In, ''Simple1ParamInNoOut''); end;');
   TestCallImplementation(srvSimple1ParamOutNoIn,  'function TTestService_Client.Simple1ParamOutNoIn(): Integer; begin Result := DoRequestNoInput<TMsg1Param>(''Simple1ParamOutNoIn'').IntegerField; end;');
   TestCallImplementation(srvSimpleNoParam,        'procedure TTestService_Client.SimpleNoParam(); begin DoRequestNoInput<TMsgEmpty>(''SimpleNoParam''); end;');
end;

procedure TGrpcExportClientTester.TestStreamInputFuncImplementation;
begin
   TestCallImplementation(srvStreamIn,                'function TTestService_Client.StreamIn(): IStreamInStream_Send; begin Result := DoInputStreamRequest<TStreamInStream_Send>(''StreamIn''); end;');
   TestCallImplementation(srvStreamIn1ParamIn,        'function TTestService_Client.StreamIn1ParamIn(): IStreamIn1ParamInStream_Send; begin Result := DoInputStreamRequest<TStreamIn1ParamInStream_Send>(''StreamIn1ParamIn''); end;');
   TestCallImplementation(srvStreamIn1ParamOut,       'function TTestService_Client.StreamIn1ParamOut(): IStreamIn1ParamOutStream_Send; begin Result := DoInputStreamRequest<TStreamIn1ParamOutStream_Send>(''StreamIn1ParamOut''); end;');
   TestCallImplementation(srvStreamInEmptyParamOut,   'function TTestService_Client.StreamInEmptyParamOut(): IStreamInEmptyParamOutStream_Send; begin Result := DoInputStreamRequest<TStreamInEmptyParamOutStream_Send>(''StreamInEmptyParamOut''); end;');
   TestCallImplementation(srvStreamIn1ParamInNoOut,   'function TTestService_Client.StreamIn1ParamInNoOut(): IStreamIn1ParamInNoOutStream_Send; begin Result := DoInputStreamRequest<TStreamIn1ParamInNoOutStream_Send>(''StreamIn1ParamInNoOut''); end;');
end;

procedure TGrpcExportClientTester.TestStreamOutputFuncImplementation;
begin
   TestCallImplementation(srvStreamOut,               'procedure TTestService_Client.StreamOut(const aMsgSimple: TMsgSimple; const aStreamOutCallback: TStreamOutCallback); begin DoOuputStreamRequest<TMsgSimple,TMsgSimple>(aMsgSimple, ''StreamOut'', aStreamOutCallback); end;');
   TestCallImplementation(srvStreamOutNoIn,           'procedure TTestService_Client.StreamOutNoIn(const aStreamOutNoInCallback: TStreamOutNoInCallback); begin DoOuputStreamRequest<TMsgSimple>(''StreamOutNoIn'', aStreamOutNoInCallback); end;');
   TestCallImplementation(srvStreamOut1ParamIn,       'procedure TTestService_Client.StreamOut1ParamIn(const aIntegerField: Integer; const aStreamOut1ParamInCallback: TStreamOut1ParamInCallback); var Msg1Param_In: TMsg1Param; begin Msg1Param_In.IntegerField := aIntegerField; '+'DoOuputStreamRequest<TMsg1Param,TMsgSimple>(Msg1Param_In, ''StreamOut1ParamIn'', aStreamOut1ParamInCallback); end;');
   TestCallImplementation(srvStreamOut1ParamOut,      'procedure TTestService_Client.StreamOut1ParamOut(const aMsgSimple: TMsgSimple; const aStreamOut1ParamOutCallback: TStreamOut1ParamOutCallback); var SubCallback: TProtoCallback<TMsg1Param>; '+'begin SubCallback := procedure(const aInput: TMsg1Param; const aHasData, aClosed: Boolean) begin '+'aStreamOut1ParamOutCallback(aInput.IntegerField, aHasData, aClosed); end; DoOuputStreamRequest<TMsgSimple,TMsg1Param>(aMsgSimple, ''StreamOut1ParamOut'', SubCallback); end;');
   TestCallImplementation(srvStreamOut1ParamOutNoIn,  'procedure TTestService_Client.StreamOut1ParamOutNoIn(const aStreamOut1ParamOutNoInCallback: TStreamOut1ParamOutNoInCallback); var SubCallback: TProtoCallback<TMsg1Param>; '+'begin SubCallback := procedure(const aInput: TMsg1Param; const aHasData, aClosed: Boolean) begin aStreamOut1ParamOutNoInCallback(aInput.IntegerField, aHasData, aClosed); end; DoOuputStreamRequest<TMsg1Param>(''StreamOut1ParamOutNoIn'', SubCallback); end;');
end;

procedure TGrpcExportClientTester.TestStreamInAndOutputFuncImplementation;
begin
   TestCallImplementation(srvStreamBoth,              'function TTestService_Client.StreamBoth(const aStreamBothCallback: TStreamBothCallback): IStreamBothStream_Send; begin Result := DoInAndOutputStreamRequest<TStreamBothStream_Send,TMsgSimple>(''StreamBoth'', aStreamBothCallback); end;');
   TestCallImplementation(srvStreamBoth1ParamIn,      'function TTestService_Client.StreamBoth1ParamIn(const aStreamBoth1ParamInCallback: TStreamBoth1ParamInCallback): IStreamBoth1ParamInStream_Send; '+'begin Result := DoInAndOutputStreamRequest<TStreamBoth1ParamInStream_Send,TMsgSimple>(''StreamBoth1ParamIn'', aStreamBoth1ParamInCallback); end;');
   TestCallImplementation(srvStreamBoth1ParamOut,     'function TTestService_Client.StreamBoth1ParamOut(const aStreamBoth1ParamOutCallback: TStreamBoth1ParamOutCallback): IStreamBoth1ParamOutStream_Send; var SubCallback: TProtoCallback<TMsg1Param>; '+'begin SubCallback := procedure(const aInput: TMsg1Param; const aHasData, aClosed: Boolean) begin '+'aStreamBoth1ParamOutCallback(aInput.IntegerField, aHasData, aClosed); end; Result := DoInAndOutputStreamRequest<TStreamBoth1ParamOutStream_Send,TMsg1Param>(''StreamBoth1ParamOut'', SubCallback); end;');
end;

//procedure TGrpcExportClientTester.TestCompleteFile;
//begin
//   Parse(C_AllCalls);
//   TProtoClientGeneratorUnitTest.Generate(FText, FProto, GetServiceTest);
//   {$MESSAGE 'Test -> niet altijd saven'}
//   FText.SaveToFile('GrpcTest.Client.pas');
//   FText.Clear;
//   TProtoTypesGenerator.Generate(FText, FProto);
//   {$MESSAGE 'Test -> niet altijd saven. Calls ook niet nodig om te parsen'}
//   FText.SaveToFile('GrpcTest.Proto.pas');
//   FText.Clear;
//   TProtoServerGenerator.Generate(FText, FProto, GetServiceTest);
//   FText.SaveToFile('GrpcTest.Server.pas');
//end;

end.

