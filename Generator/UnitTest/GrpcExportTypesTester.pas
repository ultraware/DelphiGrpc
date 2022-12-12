unit GrpcExportTypesTester;

interface

uses GrpcTestHelper, uProtoBufParserClasses;

type
   TGrpcExportTypesTester = class(TGrpcTestHelperExport)
   published
      procedure TestMessagePropertyDeclaration;
      procedure TestSimpleTypeDeclartion;
      procedure TestEnumDeclaration;
      procedure TestArrayDeclaration;
      procedure TestCompleteFile;
      procedure TestUses;
      procedure TestImportedFiles;
   end;

implementation

uses Classes, System.SysUtils, uProtoBufGenerator.Types;

type
   TProtoTypesGeneratorUnitTest = class(TProtoTypesGenerator);

{ TGrpcExportTypesTester }

procedure TGrpcExportTypesTester.TestCompleteFile;
begin
   Parse([msgSimple, msgAllTypes, msgSubTypes]) ;
   TProtoTypesGenerator.Generate(FText, FProto);
   CheckEquals(
      'unit GrpcTest.Proto; '+
      'interface '+
      'uses Grijjy.ProtocolBuffers; '+
      'type '+
      'TStringArray = Array of string; '+
      'TEnumSub = ( None = 0, Ascending = 1, Descending = 2); '+
      'TEnumType = ( First = 0, Last = 1); '+
      'TMsgSimple = record '+
      '[Serialize(1)] StringField: string; '+
      '[Serialize(2)] IntegerField: Integer; '+
      'end; '+
      'TMsgSimpleArray = Array of TMsgSimple; '+
      'TMsgAllTypes = record '+
      '[Serialize(1)] StringField: string; '+
      '[Serialize(2)] IntegerField: Integer; '+
      '[Serialize(3)] Int64Field: Int64; '+
      '[Serialize(4)] DoubleField: Double; '+
      '[Serialize(5)] FloatField: Single; '+
      '[Serialize(6)] BoolField: Boolean; '+
      '[Serialize(7)] TimeStampField: TDateTime; '+
      '[Serialize(8)] StringArray: TStringArray; '+
      '[Serialize(9)] Enum: TEnumType; '+
      '[Serialize(10)] SimpleMsg: TMsgSimple; '+
      '[Serialize(11)] Date: TDate; '+
      '[Serialize(12)] DateTime: TDateTime; '+
      '[Serialize(13)] Time: TTime; '+
      'end; '+
      'TMsgAllTypesArray = Array of TMsgAllTypes; '+
      'TMsgSub = record '+
      '[Serialize(1)] StringField: string; '+
      'end; '+
      'TMsgSubArray = Array of TMsgSub; '+
      'TMsgSubTypes = record '+
      '[Serialize(1)] SubMsg: TMsgSub; '+
      '[Serialize(2)] SubEnum: TEnumSub; '+
      'end; '+
      'TMsgSubTypesArray = Array of TMsgSubTypes; '+
      'implementation '+
      'end.',
   TextWithoutLinebreaks);
end;

procedure TGrpcExportTypesTester.TestEnumDeclaration;
begin
   Parse([msgAllTypes]);
   TProtoTypesGeneratorUnitTest.GenerateEnumDeclaration(FText, Enums[0]);
   CheckEquals('TEnumType = ( First = 0, Last = 1);', TextWithoutLinebreaks);

   Parse([msgSubTypes]);
   TProtoTypesGeneratorUnitTest.GenerateEnumDeclaration(FText, Enums[0]);
   CheckEquals('TEnumSub = ( None = 0, Ascending = 1, Descending = 2);', TextWithoutLinebreaks);
end;

procedure TGrpcExportTypesTester.TestMessagePropertyDeclaration;
var msg: TProtoBufMessage;
    propType: TTestPropertyType;
begin
   Parse([msgAllTypes]);
   msg := GetMsgAllTypes;
   for propType := Low(TTestPropertyType) to High(TTestPropertyType) do
   begin
      FText.Clear;
      TProtoTypesGeneratorUnitTest.GeneratePropertyDeclaration(FText, msg[Integer(propType)], 1);
      CheckEquals(Format('[Serialize(1)] %s: %s;',[C_TestPropertyNames[propType],C_TestPropertyExpectedDelphiFieldTypes[propType]]), TextWithoutLinebreaks);
   end;
end;

procedure TGrpcExportTypesTester.TestSimpleTypeDeclartion;
begin
   Parse([msgSimple]);
   TProtoTypesGeneratorUnitTest.GenerateTypeDeclaration(FText, GetMsgSimple);
   CheckEquals('TMsgSimple = record '+
      '[Serialize(1)] StringField: string; '+
      '[Serialize(2)] IntegerField: Integer; '+
      'end; '+
      'TMsgSimpleArray = Array of TMsgSimple;', TextWithoutLinebreaks);
end;

procedure TGrpcExportTypesTester.TestUses;
begin
   Parse([msgSimple]);
   CheckEquals('Grijjy.ProtocolBuffers', TProtoTypesGeneratorUnitTest.GenerateInterfaceUses(FProto));
   Parse([msgImport]);
   CheckEquals('ImportPackage.Proto, Grijjy.ProtocolBuffers', TProtoTypesGeneratorUnitTest.GenerateInterfaceUses(FProto));
end;

procedure TGrpcExportTypesTester.TestImportedFiles;
begin
   Parse([msgImport]);
   TProtoTypesGeneratorUnitTest.GenerateTypeDeclaration(FText, Messages[0]);
   CheckEquals('', TextWithoutLinebreaks); // No code for imported message
   FText.Clear;
   TProtoTypesGeneratorUnitTest.GenerateTypeDeclaration(FText, Messages[1]);
   CheckEquals('TMsgImport = record '+
      '[Serialize(1)] SubImport: TMsgImportedArray; '+
      'end; '+
      'TMsgImportArray = Array of TMsgImport;', TextWithoutLinebreaks);
end;

procedure TGrpcExportTypesTester.TestArrayDeclaration;
begin
   // No Arrays
   Parse([msgSimple]);
   TProtoTypesGeneratorUnitTest.GenerateSimpleTypeArrays(FText, FProto);
   CheckEquals('', TextWithoutLinebreaks);

   // 1 Arrays
   Parse([msgAllTypes]);
   TProtoTypesGeneratorUnitTest.GenerateSimpleTypeArrays(FText, FProto);
   CheckEquals('TStringArray = Array of string;', TextWithoutLinebreaks);

   // All arrays
   Parse([msgAllTypesArray]);
   TProtoTypesGeneratorUnitTest.GenerateSimpleTypeArrays(FText, FProto);
   CheckEquals(
      'TDoubleArray = Array of Double; '+
      'TSingleArray = Array of Single; '+
      'TIntegerArray = Array of Integer; '+
      'TInt64Array = Array of Int64; '+
      'TBooleanArray = Array of Boolean; '+
      'TStringArray = Array of string; '+
      'TTDatetimeArray = Array of TDatetime;',
      TextWithoutLinebreaks);
end;

end.
