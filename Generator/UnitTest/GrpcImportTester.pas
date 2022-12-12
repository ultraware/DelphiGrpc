unit GrpcImportTester;

interface

uses GrpcTestHelper, uProtoBufParserClasses;

type
   TGrpcImportTester = class(TGrpcTestHelper)
   published
      procedure TestPackageDetails;
      procedure TestImportSimpleMessage;
      procedure TestImportPropertyTypes;
      procedure TestImportNestedTypes;
      procedure TestImportServiceFuncs;
      procedure TestImportExtraPackages;
   end;

implementation

{ TGrpcImportTester }

procedure TGrpcImportTester.TestImportServiceFuncs;
var srv: TProtoBufService;
    rpc: TProtoBufRPC;
begin
   // Simple - In & Out
   Parse([srvSimple]);
   CheckEquals(1, Services.Count);
   srv := GetServiceTest;
   CheckEquals(C_ServiceName, srv.Name);

   // 1x checken of input en output goed geconverteerd wordt
   CheckEquals(1, srv.Count); // number of calls
   rpc := srv[0];
   CheckEquals(C_TestServiceCallName[srvSimple], rpc.Name);

   CheckNotNull(rpc.RPCArgument);
   CheckEquals(C_TestMessageNames[msgSimple],rpc.RPCArgument.Name);
   CheckFalse(rpc.ArgementIsStream);

   CheckNotNull(rpc.RPCResult);
   CheckEquals(C_TestMessageNames[msgSimple],rpc.RPCResult.Name);
   CheckFalse(rpc.ResultIsStream);

   // Simple - No In
   Parse([srvSimpleNoIn]);
   rpc := GetServiceTest[0];
   CheckNull(rpc.RPCArgument);
   CheckNotNull(rpc.RPCResult);
   CheckFalse(rpc.ResultIsStream);

   // Stream In - In & Out
   Parse([srvStreamIn]);
   rpc := GetServiceTest[0];
   CheckNotNull(rpc.RPCArgument);
   CheckTrue(rpc.ArgementIsStream);
   CheckNotNull(rpc.RPCResult);
   CheckFalse(rpc.ResultIsStream);

   // Stream Out - In & Out
   Parse([srvStreamOut]);
   rpc := GetServiceTest[0];
   CheckNotNull(rpc.RPCArgument);
   CheckFalse(rpc.ArgementIsStream);
   CheckNotNull(rpc.RPCResult);
   CheckTrue(rpc.ResultIsStream);

   // Stream Out - Not In
   Parse([srvStreamOutNoIn]);
   rpc := GetServiceTest[0];
   CheckNull(rpc.RPCArgument);
   CheckNotNull(rpc.RPCResult);
   CheckTrue(rpc.ResultIsStream);

   // Stream In & Out - In & Out
   Parse([srvStreamBoth]);
   rpc := GetServiceTest[0];
   CheckNotNull(rpc.RPCArgument);
   CheckTrue(rpc.ArgementIsStream);
   CheckNotNull(rpc.RPCResult);
   CheckTrue(rpc.ResultIsStream);

   // Multiple
   Parse([srvSimple, srvSimpleNoIn, srvSimple1ParamIn, srvSimple1ParamOut, srvStreamIn, srvStreamIn1ParamIn, srvStreamIn1ParamOut]);
   CheckEquals(7, GetServiceTest.Count);
end;

procedure TGrpcImportTester.TestImportSimpleMessage;
var msg: TProtoBufMessage;
begin
   Parse([msgSimple]);
   CheckEquals(1, Messages.Count);

   msg := GetMsgSimple;
   CheckEquals(C_TestMessageNames[msgSimple], msg.Name);

   CheckEquals(2, msg.Count{Number of props});

   Parse([msgSimple,msgAllTypes]);
   CheckEquals(2, Messages.Count);
end;

procedure TGrpcImportTester.TestPackageDetails;
begin
   Parse([msgSimple]);
   CheckEquals(C_PackageName, FProto.Name);
   Check(FProto.ProtoSyntaxVersion = psv3);
end;

procedure TGrpcImportTester.TestImportExtraPackages;
begin
   Parse([msgImport]);
   CheckEquals(1, FProto.Imports.Count); // Imported file
   CheckEquals('ImportPackage', FProto.Imports[0]);
   CheckEquals(2, Messages.Count); // Imported + own message
   CheckEquals(1, Services.Count); // Imported services
   CheckEquals(0, Services.NonImportedCount); // Own services
end;

procedure TGrpcImportTester.TestImportNestedTypes;
var msg: TProtoBufMessage;
    prop: TProtoBufProperty;
    enum: TProtoBufEnum;
begin
   Parse([msgSubTypes]);
   CheckEquals(2, Messages.Count);
   CheckEquals(1, Enums.Count);

   msg := GetMsgSubTypes;
   CheckEquals(2, msg.Count);

   prop := msg[0];
   CheckEquals(C_TestMessageNames[msgSubMessage],prop.PropType);
   prop := msg[1];
   CheckEquals(C_TestPropertyFieldTypes[propSubEnum],prop.PropType);

   msg := GetMsgSubMessage;
   CheckEquals(1, msg.Count);

   prop := msg[0];
   CheckEquals(C_TestPropertyFieldTypes[propString],prop.PropType);
   CheckEquals(C_TestPropertyNames[propString],prop.Name);

   enum := Enums[0];
   CheckEquals(C_TestEnumNames[enumSub], enum.Name);
end;

procedure TGrpcImportTester.TestImportPropertyTypes;
var msg: TProtoBufMessage;
    prop: TProtoBufProperty;
    propType: TTestPropertyType;
begin
   Parse([msgAllTypes]);
   msg := GetMsgAllTypes;

   for propType := Low(TTestPropertyType) to High(TTestPropertyType) do
   begin
      prop := msg[Integer(propType)];
      CheckEquals(C_TestPropertyNames[propType], prop.Name);
      CheckEquals(C_TestPropertyFieldTypes[propType], prop.PropType);
      CheckTrue(C_TestPropertyExpectedPropKind[propType] = prop.PropKind);
      CheckTrue(C_TestPropertyExpectedFieldType[propType] = StrToPropertyType(prop.PropType));
   end;
end;

end.
