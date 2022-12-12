unit GrpcExportServerTester;

interface

uses GrpcTestHelper, uProtoBufParserClasses;

type
   TGrpcExportServerTester = class(TGrpcTestHelperExport)
   private
      procedure TestCallDeclaration(const aCall: TTestServiceCall; const aExpected: string);
   published
      procedure TestInterfaceDeclaration;
      procedure TestSimpleFuncDeclaration;
      procedure TestStreamInputFuncDeclaration;
      procedure TestStreamOutputFuncDeclaration;
      procedure TestStreamInAndOutputFuncDeclaration;
      procedure TestInputStreamTypeDeclaration;
      procedure TestOutputStreamTypeDeclaration;
      procedure TestInputStreamTypeImplementation;
      procedure TestOutputStreamTypeImplementation;
      procedure TestServerClassDeclaration;
      procedure TestServerClassImplementation;
      procedure TestGenerateUses;
      procedure TestNeedsInternalFunction;
      procedure TestGenerateClientCallRegistration;
      procedure TestGenerateRpcInternalDeclaration;
      procedure TestGenerateRpcInternalImplementation;
   end;

implementation

uses uProtoBufGenerator.Server;

type
   TProtoServerGeneratorUnitTest = class(TProtoServerGenerator);

{ TGrpcExportServerTester }


procedure TGrpcExportServerTester.TestNeedsInternalFunction;
   procedure _Test(const aCall: TTestServiceCall; const aExpected: Boolean);
   begin
      Parse([aCall]);
      Check(aExpected = TProtoServerGeneratorUnitTest.NeedsInternalFunction(GetServiceTest[0]), C_TestServiceCallName[aCall]);
   end;

begin
   _Test(srvSimple,                 False);
   _Test(srvSimpleNoIn,             False);
   _Test(srvSimple1ParamIn,         True);
   _Test(srvSimple1ParamOut,        True);
   _Test(srvSimpleEmptyParamOut,    True);
   _Test(srvSimple1Param,           True);
   _Test(srvSimple1ParamInNoOut,    True);
   _Test(srvSimple1ParamOutNoIn,    True);
   _Test(srvSimpleNoParam,          True);
   _Test(srvStreamIn,               False);
   _Test(srvStreamIn1ParamIn,       False);
   _Test(srvStreamIn1ParamOut,      True);
   _Test(srvStreamInEmptyParamOut,  True);
   _Test(srvStreamIn1Param,         True);
   _Test(srvStreamIn1ParamInNoOut,  True);
   _Test(srvStreamOut,              False);
   _Test(srvStreamOutNoIn,          False);
   _Test(srvStreamOut1ParamIn,      True);
   _Test(srvStreamOut1ParamOut,     False);
   _Test(srvStreamOut1Param,        True);
   _Test(srvStreamOut1ParamOutNoIn, False);
   _Test(srvStreamBoth,             False);
   _Test(srvStreamBoth1ParamIn,     False);
   _Test(srvStreamBoth1ParamOut,    False);
   _Test(srvStreamBoth1Param,       False);
end;

procedure TGrpcExportServerTester.TestGenerateClientCallRegistration;
   procedure _Test(const aCall: TTestServiceCall; const aExpected: string);
   begin
      Parse([aCall]);
      CheckEquals(aExpected, TProtoServerGeneratorUnitTest.GenerateClientCallRegistration(GetServiceTest[0]));
   end;

begin
   _Test(srvSimple,                 'RegisterCall<TMsgSimple,TMsgSimple>(''Simple'',Simple);');
   _Test(srvSimpleNoIn,             'RegisterCallNoInput<TMsgSimple>(''SimpleNoIn'',SimpleNoIn);');
   _Test(srvSimple1ParamIn,         'RegisterCall<TMsg1Param,TMsgSimple>(''Simple1ParamIn'',InternalSimple1ParamIn);');
   _Test(srvSimple1ParamOut,        'RegisterCall<TMsgSimple,TMsg1Param>(''Simple1ParamOut'',InternalSimple1ParamOut);');
   _Test(srvSimpleEmptyParamOut,    'RegisterCall<TMsgSimple,TMsgEmpty>(''SimpleEmptyParamOut'',InternalSimpleEmptyParamOut);');
   _Test(srvSimple1Param,           'RegisterCall<TMsg1Param,TMsg1Param>(''Simple1Param'',InternalSimple1Param);');
   _Test(srvSimple1ParamInNoOut,    'RegisterCall<TMsg1Param,TMsgEmpty>(''Simple1ParamInNoOut'',InternalSimple1ParamInNoOut);');
   _Test(srvSimple1ParamOutNoIn,    'RegisterCallNoInput<TMsg1Param>(''Simple1ParamOutNoIn'',InternalSimple1ParamOutNoIn);');
   _Test(srvSimpleNoParam,          'RegisterCallNoInput<TMsgEmpty>(''SimpleNoParam'',InternalSimpleNoParam);');
   _Test(srvStreamIn,               'RegisterInputStreamCall<TStreamInStream_Recv,IStreamInStream_Recv,TMsgSimple>(''StreamIn'',StreamIn);');
   _Test(srvStreamIn1ParamIn,       'RegisterInputStreamCall<TStreamIn1ParamInStream_Recv,IStreamIn1ParamInStream_Recv,TMsgSimple>(''StreamIn1ParamIn'',StreamIn1ParamIn);');
   _Test(srvStreamIn1ParamOut,      'RegisterInputStreamCall<TStreamIn1ParamOutStream_Recv,IStreamIn1ParamOutStream_Recv,TMsg1Param>(''StreamIn1ParamOut'',InternalStreamIn1ParamOut);');
   _Test(srvStreamInEmptyParamOut,  'RegisterInputStreamCall<TStreamInEmptyParamOutStream_Recv,IStreamInEmptyParamOutStream_Recv,TMsgEmpty>(''StreamInEmptyParamOut'',InternalStreamInEmptyParamOut);');
   _Test(srvStreamIn1Param,         'RegisterInputStreamCall<TStreamIn1ParamStream_Recv,IStreamIn1ParamStream_Recv,TMsg1Param>(''StreamIn1Param'',InternalStreamIn1Param);');
   _Test(srvStreamInEmptyParamOut,  'RegisterInputStreamCall<TStreamInEmptyParamOutStream_Recv,IStreamInEmptyParamOutStream_Recv,TMsgEmpty>(''StreamInEmptyParamOut'',InternalStreamInEmptyParamOut);');
   _Test(srvStreamOut,              'RegisterOutputStreamCall<TMsgSimple,TStreamOutStream_Send,IStreamOutStream_Send>(''StreamOut'',StreamOut);');
   _Test(srvStreamOutNoIn,          'RegisterOutputStreamCallNoInput<TStreamOutNoInStream_Send,IStreamOutNoInStream_Send>(''StreamOutNoIn'',StreamOutNoIn);');
   _Test(srvStreamOut1ParamIn,      'RegisterOutputStreamCall<TMsg1Param,TStreamOut1ParamInStream_Send,IStreamOut1ParamInStream_Send>(''StreamOut1ParamIn'',InternalStreamOut1ParamIn);');
   _Test(srvStreamOut1ParamOut,     'RegisterOutputStreamCall<TMsgSimple,TStreamOut1ParamOutStream_Send,IStreamOut1ParamOutStream_Send>(''StreamOut1ParamOut'',StreamOut1ParamOut);');
   _Test(srvStreamOut1Param,        'RegisterOutputStreamCall<TMsg1Param,TStreamOut1ParamStream_Send,IStreamOut1ParamStream_Send>(''StreamOut1Param'',InternalStreamOut1Param);');
   _Test(srvStreamOut1ParamOutNoIn, 'RegisterOutputStreamCallNoInput<TStreamOut1ParamOutNoInStream_Send,IStreamOut1ParamOutNoInStream_Send>(''StreamOut1ParamOutNoIn'',StreamOut1ParamOutNoIn);');
   _Test(srvStreamBoth,             'RegisterStreamCall<TStreamBothStream_Recv,IStreamBothStream_Recv,TStreamBothStream_Send,IStreamBothStream_Send>(''StreamBoth'',StreamBoth);');
   _Test(srvStreamBoth1ParamIn,     'RegisterStreamCall<TStreamBoth1ParamInStream_Recv,IStreamBoth1ParamInStream_Recv,TStreamBoth1ParamInStream_Send,IStreamBoth1ParamInStream_Send>(''StreamBoth1ParamIn'',StreamBoth1ParamIn);');
   _Test(srvStreamBoth1ParamOut,    'RegisterStreamCall<TStreamBoth1ParamOutStream_Recv,IStreamBoth1ParamOutStream_Recv,TStreamBoth1ParamOutStream_Send,IStreamBoth1ParamOutStream_Send>(''StreamBoth1ParamOut'',StreamBoth1ParamOut);');
   _Test(srvStreamBoth1Param,       'RegisterStreamCall<TStreamBoth1ParamStream_Recv,IStreamBoth1ParamStream_Recv,TStreamBoth1ParamStream_Send,IStreamBoth1ParamStream_Send>(''StreamBoth1Param'',StreamBoth1Param);');
end;

procedure TGrpcExportServerTester.TestGenerateRpcInternalDeclaration;
   procedure _Test(const aCall: TTestServiceCall; const aExpected: string);
   begin
      Parse([aCall]);
      CheckEquals(aExpected, TProtoServerGeneratorUnitTest.GenerateRpcInternalDeclaration(GetServiceTest[0], ''));
   end;
begin
//   _Test(srvSimple,                 '');
//   _Test(srvSimpleNoIn,             '');
   _Test(srvSimple1ParamIn,         'function InternalSimple1ParamIn(const aMsg1Param: TMsg1Param): TMsgSimple;');
   _Test(srvSimple1ParamOut,        'function InternalSimple1ParamOut(const aMsgSimple: TMsgSimple): TMsg1Param;');
   _Test(srvSimpleEmptyParamOut,    'function InternalSimpleEmptyParamOut(const aMsgSimple: TMsgSimple): TMsgEmpty;');
   _Test(srvSimple1Param,           'function InternalSimple1Param(const aMsg1Param: TMsg1Param): TMsg1Param;');
   _Test(srvSimple1ParamInNoOut,    'function InternalSimple1ParamInNoOut(const aMsg1Param: TMsg1Param): TMsgEmpty;');
   _Test(srvSimple1ParamOutNoIn,    'function InternalSimple1ParamOutNoIn(): TMsg1Param;');
   _Test(srvSimpleNoParam,          'function InternalSimpleNoParam(): TMsgEmpty;');
//   _Test(srvStreamIn,               '');
//   _Test(srvStreamIn1ParamIn,       '');
   _Test(srvStreamIn1ParamOut,      'function InternalStreamIn1ParamOut(const aStreamIn1ParamOutStream_Recv: IStreamIn1ParamOutStream_Recv): TMsg1Param;');
   _Test(srvStreamInEmptyParamOut,  'function InternalStreamInEmptyParamOut(const aStreamInEmptyParamOutStream_Recv: IStreamInEmptyParamOutStream_Recv): TMsgEmpty;');
   _Test(srvStreamIn1Param,         'function InternalStreamIn1Param(const aStreamIn1ParamStream_Recv: IStreamIn1ParamStream_Recv): TMsg1Param;');
   _Test(srvStreamIn1ParamInNoOut,  'function InternalStreamIn1ParamInNoOut(const aStreamIn1ParamInNoOutStream_Recv: IStreamIn1ParamInNoOutStream_Recv): TMsgEmpty;');
//   _Test(srvStreamOut,              '');
//   _Test(srvStreamOutNoIn,          '');
   _Test(srvStreamOut1ParamIn,      'procedure InternalStreamOut1ParamIn(const aMsg1Param: TMsg1Param; const aStreamOut1ParamInStream_Send: IStreamOut1ParamInStream_Send);');
//   _Test(srvStreamOut1ParamOut,     '');
   _Test(srvStreamOut1Param,        'procedure InternalStreamOut1Param(const aMsg1Param: TMsg1Param; const aStreamOut1ParamStream_Send: IStreamOut1ParamStream_Send);');
//   _Test(srvStreamOut1ParamOutNoIn, '');
//   _Test(srvStreamBoth,             '');
//   _Test(srvStreamBoth1ParamIn,     '');
//   _Test(srvStreamBoth1ParamOut,    '');
//   _Test(srvStreamBoth1Param,       '');
end;

procedure TGrpcExportServerTester.TestGenerateRpcInternalImplementation;
   procedure _Test(const aCall: TTestServiceCall; const aExpected: string);
   begin
      Parse([aCall]);
      TProtoServerGeneratorUnitTest.GenerateRpcInternalImplementation(FText, GetServiceTest[0], GetServiceTest);
      CheckEquals(aExpected, TextWithoutLinebreaks);
   end;
begin
//   _Test(srvSimple,                 '');
//   _Test(srvSimpleNoIn,             '');
   _Test(srvSimple1ParamIn,         'function TTestService_Server.InternalSimple1ParamIn(const aMsg1Param: TMsg1Param): TMsgSimple; begin Result := Simple1ParamIn(aMsg1Param.IntegerField); end;');
   _Test(srvSimple1ParamOut,        'function TTestService_Server.InternalSimple1ParamOut(const aMsgSimple: TMsgSimple): TMsg1Param; begin Result.IntegerField := Simple1ParamOut(aMsgSimple); end;');
   _Test(srvSimpleEmptyParamOut,    'function TTestService_Server.InternalSimpleEmptyParamOut(const aMsgSimple: TMsgSimple): TMsgEmpty; begin SimpleEmptyParamOut(aMsgSimple); end;');
   _Test(srvSimple1Param,           'function TTestService_Server.InternalSimple1Param(const aMsg1Param: TMsg1Param): TMsg1Param; begin Result.IntegerField := Simple1Param(aMsg1Param.IntegerField); end;');
   _Test(srvSimple1ParamInNoOut,    'function TTestService_Server.InternalSimple1ParamInNoOut(const aMsg1Param: TMsg1Param): TMsgEmpty; begin Simple1ParamInNoOut(aMsg1Param.IntegerField); end;');
   _Test(srvSimple1ParamOutNoIn,    'function TTestService_Server.InternalSimple1ParamOutNoIn(): TMsg1Param; begin Result.IntegerField := Simple1ParamOutNoIn(); end;');
   _Test(srvSimpleNoParam,          'function TTestService_Server.InternalSimpleNoParam(): TMsgEmpty; begin SimpleNoParam(); end;');
//   _Test(srvStreamIn,               '');
//   _Test(srvStreamIn1ParamIn,       '');
   _Test(srvStreamIn1ParamOut,      'function TTestService_Server.InternalStreamIn1ParamOut(const aStreamIn1ParamOutStream_Recv: IStreamIn1ParamOutStream_Recv): TMsg1Param; begin Result.IntegerField := StreamIn1ParamOut(aStreamIn1ParamOutStream_Recv); end;');
   _Test(srvStreamInEmptyParamOut,  'function TTestService_Server.InternalStreamInEmptyParamOut(const aStreamInEmptyParamOutStream_Recv: IStreamInEmptyParamOutStream_Recv): TMsgEmpty; begin StreamInEmptyParamOut(aStreamInEmptyParamOutStream_Recv); end;');
   _Test(srvStreamIn1Param,         'function TTestService_Server.InternalStreamIn1Param(const aStreamIn1ParamStream_Recv: IStreamIn1ParamStream_Recv): TMsg1Param; begin Result.IntegerField := StreamIn1Param(aStreamIn1ParamStream_Recv); end;');
   _Test(srvStreamIn1ParamInNoOut,  'function TTestService_Server.InternalStreamIn1ParamInNoOut(const aStreamIn1ParamInNoOutStream_Recv: IStreamIn1ParamInNoOutStream_Recv): TMsgEmpty; begin StreamIn1ParamInNoOut(aStreamIn1ParamInNoOutStream_Recv); end;');
//   _Test(srvStreamOut,              '');
//   _Test(srvStreamOutNoIn,          '');
   _Test(srvStreamOut1ParamIn,      'procedure TTestService_Server.InternalStreamOut1ParamIn(const aMsg1Param: TMsg1Param; const aStreamOut1ParamInStream_Send: IStreamOut1ParamInStream_Send); begin StreamOut1ParamIn(aMsg1Param.IntegerField, aStreamOut1ParamInStream_Send); end;');
//   _Test(srvStreamOut1ParamOut,     '');
   _Test(srvStreamOut1Param,        'procedure TTestService_Server.InternalStreamOut1Param(const aMsg1Param: TMsg1Param; const aStreamOut1ParamStream_Send: IStreamOut1ParamStream_Send); begin StreamOut1Param(aMsg1Param.IntegerField, aStreamOut1ParamStream_Send); end;');
//   _Test(srvStreamOut1ParamOutNoIn, '');
//   _Test(srvStreamBoth,             '');
//   _Test(srvStreamBoth1ParamIn,     '');
//   _Test(srvStreamBoth1ParamOut,    '');
//   _Test(srvStreamBoth1Param,       '');
end;

procedure TGrpcExportServerTester.TestGenerateUses;
begin
   Parse([srvSimple]);
   CheckEquals('Ultraware.Grpc.Server, GrpcTest.Proto', TProtoServerGeneratorUnitTest.GenerateInterfaceUses(FProto));
   CheckEquals('', TProtoServerGeneratorUnitTest.GenerateImplementationUses(FProto));
end;

procedure TGrpcExportServerTester.TestInterfaceDeclaration;
begin
   Parse([srvSimple]);
   TProtoServerGeneratorUnitTest.GenerateClientInterface(FText, GetServiceTest);
   CheckEquals(
      'ITestService_Server = interface '+
         'function Simple(const aMsgSimple: TMsgSimple): TMsgSimple; '+
      'end;', TextWithoutLinebreaks);
end;

procedure TGrpcExportServerTester.TestServerClassDeclaration;
begin
   Parse([srvSimple]);
   TProtoServerGeneratorUnitTest.GenerateClientClass(FText, GetServiceTest);
   CheckEquals(
      'TTestService_Server = class(TBaseGrpcImplementation, ITestService_Server) '+
      'protected '+
         'procedure DoRegisterCalls; override; '+
      'public '+
         'class function BasePath: string; override; '+
      'public '+
         'function Simple(const aMsgSimple: TMsgSimple): TMsgSimple; virtual; abstract; '+
      'end;', TextWithoutLinebreaks);
end;

procedure TGrpcExportServerTester.TestServerClassImplementation;
begin
   Parse([srvSimple]);
   TProtoServerGeneratorUnitTest.GenerateClientClassImplementation(FText, FProto, GetServiceTest);
   CheckEquals(
   '{ TTestService_Server } '+
   'class function TTestService_Server.BasePath: string; '+
   'begin '+
      'Result := ''/GrpcTest.TestService/''; '+
   'end; '+
   'procedure TTestService_Server.DoRegisterCalls; '+
   'begin '+
      'RegisterCall<TMsgSimple,TMsgSimple>(''Simple'',Simple); '+
   'end;', TextWithoutLinebreaks);
end;

procedure TGrpcExportServerTester.TestCallDeclaration(const aCall: TTestServiceCall; const aExpected: string);
begin
   Parse([aCall]);
   CheckEquals(aExpected, TProtoServerGeneratorUnitTest.GenerateRpcDeclaration(GetServiceTest[0]));
end;

procedure TGrpcExportServerTester.TestSimpleFuncDeclaration;
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

procedure TGrpcExportServerTester.TestStreamInputFuncDeclaration;
begin
   TestCallDeclaration(srvStreamIn,                'function StreamIn(const aStreamInStream_Recv: IStreamInStream_Recv): TMsgSimple;');
   TestCallDeclaration(srvStreamIn1ParamIn,        'function StreamIn1ParamIn(const aStreamIn1ParamInStream_Recv: IStreamIn1ParamInStream_Recv): TMsgSimple;');
   TestCallDeclaration(srvStreamIn1ParamOut,       'function StreamIn1ParamOut(const aStreamIn1ParamOutStream_Recv: IStreamIn1ParamOutStream_Recv): Integer;');
   TestCallDeclaration(srvStreamInEmptyParamOut,   'procedure StreamInEmptyParamOut(const aStreamInEmptyParamOutStream_Recv: IStreamInEmptyParamOutStream_Recv);');
   TestCallDeclaration(srvStreamIn1Param,          'function StreamIn1Param(const aStreamIn1ParamStream_Recv: IStreamIn1ParamStream_Recv): Integer;');
   TestCallDeclaration(srvStreamIn1ParamInNoOut,   'procedure StreamIn1ParamInNoOut(const aStreamIn1ParamInNoOutStream_Recv: IStreamIn1ParamInNoOutStream_Recv);');
end;

procedure TGrpcExportServerTester.TestStreamOutputFuncDeclaration;
begin
   TestCallDeclaration(srvStreamOut,               'procedure StreamOut(const aMsgSimple: TMsgSimple; const aStreamOutStream_Send: IStreamOutStream_Send);');
   TestCallDeclaration(srvStreamOutNoIn,           'procedure StreamOutNoIn(const aStreamOutNoInStream_Send: IStreamOutNoInStream_Send);');
   TestCallDeclaration(srvStreamOut1ParamIn,       'procedure StreamOut1ParamIn(const aIntegerField: Integer; const aStreamOut1ParamInStream_Send: IStreamOut1ParamInStream_Send);');
   TestCallDeclaration(srvStreamOut1ParamOut,      'procedure StreamOut1ParamOut(const aMsgSimple: TMsgSimple; const aStreamOut1ParamOutStream_Send: IStreamOut1ParamOutStream_Send);');
   TestCallDeclaration(srvStreamOut1Param,         'procedure StreamOut1Param(const aIntegerField: Integer; const aStreamOut1ParamStream_Send: IStreamOut1ParamStream_Send);');
   TestCallDeclaration(srvStreamOut1ParamOutNoIn,  'procedure StreamOut1ParamOutNoIn(const aStreamOut1ParamOutNoInStream_Send: IStreamOut1ParamOutNoInStream_Send);');
end;

procedure TGrpcExportServerTester.TestStreamInAndOutputFuncDeclaration;
begin
   TestCallDeclaration(srvStreamBoth,              'procedure StreamBoth(const aStreamBothStream_Recv: IStreamBothStream_Recv; const aStreamBothStream_Send: IStreamBothStream_Send);');
   TestCallDeclaration(srvStreamBoth1ParamIn,      'procedure StreamBoth1ParamIn(const aStreamBoth1ParamInStream_Recv: IStreamBoth1ParamInStream_Recv; const aStreamBoth1ParamInStream_Send: IStreamBoth1ParamInStream_Send);');
   TestCallDeclaration(srvStreamBoth1ParamOut,     'procedure StreamBoth1ParamOut(const aStreamBoth1ParamOutStream_Recv: IStreamBoth1ParamOutStream_Recv; const aStreamBoth1ParamOutStream_Send: IStreamBoth1ParamOutStream_Send);');
   TestCallDeclaration(srvStreamBoth1Param,        'procedure StreamBoth1Param(const aStreamBoth1ParamStream_Recv: IStreamBoth1ParamStream_Recv; const aStreamBoth1ParamStream_Send: IStreamBoth1ParamStream_Send);');
end;

procedure TGrpcExportServerTester.TestInputStreamTypeDeclaration;

   procedure _Test(const aCall: TTestServiceCall; const aExpectedInterface, aExpectedClass: string);
   begin
      Parse([aCall]);
      TProtoServerGeneratorUnitTest.GenerateInputStreamInterface(FText, GetServiceTest[0]);
      CheckEquals(aExpectedInterface, TextWithoutLinebreaks);
      FText.Clear;
      TProtoServerGeneratorUnitTest.GenerateInputStreamDeclaration(FText, GetServiceTest[0]);
      CheckEquals(aExpectedClass, TextWithoutLinebreaks);
   end;

begin
   _Test(srvStreamIn,
      'IStreamInStream_Recv = interface(IGrpcMemStream) function Recv(out aMsgSimple: TMsgSimple; const aWaitTimeout: Integer): TGrpcWaitResult; end;',
      'TStreamInStream_Recv = class(TBaseGrpcMemStream<TMsgSimple>, IStreamInStream_Recv);');
   _Test(srvStreamIn1ParamIn,
      'IStreamIn1ParamInStream_Recv = interface(IGrpcMemStream) function Recv(out aIntegerField: Integer; const aWaitTimeout: Integer): TGrpcWaitResult; end;',
      'TStreamIn1ParamInStream_Recv = class(TBaseGrpcMemStream<TMsg1Param>, IStreamIn1ParamInStream_Recv) function Recv(out aIntegerField: Integer; const aWaitTimeout: Integer): TGrpcWaitResult; end;');
   _Test(srvStreamIn1ParamOut,
      'IStreamIn1ParamOutStream_Recv = interface(IGrpcMemStream) function Recv(out aMsgSimple: TMsgSimple; const aWaitTimeout: Integer): TGrpcWaitResult; end;',
      'TStreamIn1ParamOutStream_Recv = class(TBaseGrpcMemStream<TMsgSimple>, IStreamIn1ParamOutStream_Recv);');
   _Test(srvStreamInEmptyParamOut,
      'IStreamInEmptyParamOutStream_Recv = interface(IGrpcMemStream) function Recv(out aMsgSimple: TMsgSimple; const aWaitTimeout: Integer): TGrpcWaitResult; end;',
      'TStreamInEmptyParamOutStream_Recv = class(TBaseGrpcMemStream<TMsgSimple>, IStreamInEmptyParamOutStream_Recv);');
   _Test(srvStreamIn1Param,
      'IStreamIn1ParamStream_Recv = interface(IGrpcMemStream) function Recv(out aIntegerField: Integer; const aWaitTimeout: Integer): TGrpcWaitResult; end;',
      'TStreamIn1ParamStream_Recv = class(TBaseGrpcMemStream<TMsg1Param>, IStreamIn1ParamStream_Recv) function Recv(out aIntegerField: Integer; const aWaitTimeout: Integer): TGrpcWaitResult; end;');
   _Test(srvStreamIn1ParamInNoOut,
      'IStreamIn1ParamInNoOutStream_Recv = interface(IGrpcMemStream) function Recv(out aIntegerField: Integer; const aWaitTimeout: Integer): TGrpcWaitResult; end;',
      'TStreamIn1ParamInNoOutStream_Recv = class(TBaseGrpcMemStream<TMsg1Param>, IStreamIn1ParamInNoOutStream_Recv) function Recv(out aIntegerField: Integer; const aWaitTimeout: Integer): TGrpcWaitResult; end;');
end;

procedure TGrpcExportServerTester.TestOutputStreamTypeDeclaration;

   procedure _Test(const aCall: TTestServiceCall; const aExpectedInterface, aExpectedClass: string);
   begin
      Parse([aCall]);
      TProtoServerGeneratorUnitTest.GenerateOutputStreamInterface(FText, GetServiceTest[0]);
      CheckEquals(aExpectedInterface, TextWithoutLinebreaks);
      FText.Clear;
      TProtoServerGeneratorUnitTest.GenerateOutputStreamDeclaration(FText, GetServiceTest[0]);
      CheckEquals(aExpectedClass, TextWithoutLinebreaks);
   end;

begin
   _Test(srvStreamOut,
      'IStreamOutStream_Send = interface(ICallbackStream) procedure Send(const aMsgSimple: TMsgSimple); end;',
      'TStreamOutStream_Send = class(TServerCallbackStream<TMsgSimple>, IStreamOutStream_Send);');
   _Test(srvStreamOutNoIn,
      'IStreamOutNoInStream_Send = interface(ICallbackStream) procedure Send(const aMsgSimple: TMsgSimple); end;',
      'TStreamOutNoInStream_Send = class(TServerCallbackStream<TMsgSimple>, IStreamOutNoInStream_Send);');
   _Test(srvStreamOut1ParamIn,
      'IStreamOut1ParamInStream_Send = interface(ICallbackStream) procedure Send(const aMsgSimple: TMsgSimple); end;',
      'TStreamOut1ParamInStream_Send = class(TServerCallbackStream<TMsgSimple>, IStreamOut1ParamInStream_Send);');
   _Test(srvStreamOut1ParamOut,
      'IStreamOut1ParamOutStream_Send = interface(ICallbackStream) procedure Send(const aIntegerField: Integer); end;',
      'TStreamOut1ParamOutStream_Send = class(TServerCallbackStream<TMsg1Param>, IStreamOut1ParamOutStream_Send) procedure Send(const aIntegerField: Integer); end;');
   _Test(srvStreamOut1Param,
      'IStreamOut1ParamStream_Send = interface(ICallbackStream) procedure Send(const aIntegerField: Integer); end;',
      'TStreamOut1ParamStream_Send = class(TServerCallbackStream<TMsg1Param>, IStreamOut1ParamStream_Send) procedure Send(const aIntegerField: Integer); end;');
   _Test(srvStreamOut1ParamOutNoIn,
      'IStreamOut1ParamOutNoInStream_Send = interface(ICallbackStream) procedure Send(const aIntegerField: Integer); end;',
      'TStreamOut1ParamOutNoInStream_Send = class(TServerCallbackStream<TMsg1Param>, IStreamOut1ParamOutNoInStream_Send) procedure Send(const aIntegerField: Integer); end;');
end;

procedure TGrpcExportServerTester.TestInputStreamTypeImplementation;
begin
   Parse([srvStreamOut1Param]);
   TProtoServerGeneratorUnitTest.GenerateInputStreamImplementation(FText, GetServiceTest[0]);
   CheckEquals(
      '{ TStreamOut1ParamStream_Send } '+
      'procedure TStreamOut1ParamStream_Send.Send(const aIntegerField: Integer); '+
      'var Msg1Param: TMsg1Param; '+
      'begin '+
         'Msg1Param.IntegerField := aIntegerField; '+
         'inherited Send(Msg1Param); '+
      'end;',
      TextWithoutLinebreaks);
end;

procedure TGrpcExportServerTester.TestOutputStreamTypeImplementation;
begin
   Parse([srvStreamIn1Param]);
   TProtoServerGeneratorUnitTest.GenerateOutputStreamImplementation(FText, GetServiceTest[0]);
   CheckEquals(
      '{ TStreamIn1ParamStream_Recv } '+
      'function TStreamIn1ParamStream_Recv.Recv(out aIntegerField: Integer; const aWaitTimeout: Integer): TGrpcWaitResult; '+
      'var Msg1Param: TMsg1Param; '+
      'begin '+
         'Result := inherited Recv(Msg1Param, aWaitTimeout); '+
         'aIntegerField := Msg1Param.IntegerField; '+
      'end;', TextWithoutLinebreaks);
end;

end.
