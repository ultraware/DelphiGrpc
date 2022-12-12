unit GrpcTestHelper;

interface

uses system.Classes, ExceptionTestcase, uProtoBufParserClasses;

type
   TTestPropertyOption = (propOpNone, propOpArray);
const
   C_TestPropertyOptionValue: array[TTestPropertyOption] of string = ('','repeated ');

type
   TTestEnum = (enum, enumSub);
const
   C_TestEnumNames: array[TTestEnum] of string = ('EnumType',  'EnumSub');
   C_TestEnumValues: array[TTestEnum] of array of string = (
      ['First',   'Last'],
      ['None',    'Ascending',   'Descending']);

type
   TTestProperty = (
      propString,       propInt32,        propInt64,        propDouble,       propFloat,
      propBool,         propTimeStamp,    propStringArray,  propEnum,         propMessage,
      propDate,         propDateTime,     propTime,         propSubMessage,   propSubEnum,
      propImportedMessage, propIntArray,  propInt32Array,   propInt64Array,   propDoubleArray,
      propFloatArray,   propBoolArray,    propDateArray,    propDateTimeArray,propTimeArray);
   TTestPropertys = array of TTestProperty;
   // Subsets
   TTestPropertyType = propString..propTime;
const
   C_TestPropertyNames:       array[TTestProperty] of string =  (
      'StringField',    'IntegerField',   'Int64Field',     'DoubleField',    'FloatField',
      'BoolField',      'TimeStampField', 'StringArray',    'Enum',           'SimpleMsg',
      'Date',           'DateTime',       'Time',           'SubMsg',         'SubEnum',
      'SubImport',      'IntArray',       'Int32Array',     'Int64Array',     'DoubleArray',
      'FloatArray',     'BoolArray',      'DateArray',      'DateTimeArray',  'TimeArray');
   C_TestPropertyFieldTypes:  array[TTestProperty] of string = (
      'string',         'int32',          'int64',          'double',         'float',
      'bool',           'timestamp',      'string',         'EnumType',       'MsgSimple',
      'timestamp',      'timestamp',      'timestamp',      'MsgSub',         'EnumSub',
      'ImportPackage.MsgImported', 'int32', 'int32',        'int64',          'double',
      'float',          'bool',           'timestamp',      'timestamp',      'timestamp');
   C_TestPropertyExpectedDelphiFieldTypes:  array[TTestProperty] of string = (
      'string',         'Integer',        'Int64',          'Double',         'Single',
      'Boolean',        'TDateTime',      'TStringArray',   'TEnumType',      'TMsgSimple',
      'TDate',          'TDateTime',      'TTime',          'TMsgSub',        'TEnumSub',
      'ImportPackage.TMsgImported','TIntegerArray','TInt32Array','TInt64Array','TDoubleArray',
      'TSingleArray',    'TBooleanArray',  'TDateArray',     'TDateTimeArray', 'TTimeArray');
   C_TestPropertyOptions:     array[TTestProperty] of TTestPropertyOption =  (
      propOpNone,       propOpNone,       propOpNone,       propOpNone,       propOpNone,
      propOpNone,       propOpNone,       propOpArray,      propOpNone,       propOpNone,
      propOpNone,       propOpNone,       propOpNone,       propOpNone,       propOpNone,
      propOpArray,      propOpArray,      propOpArray,      propOpArray,      propOpArray,
      propOpArray,      propOpArray,      propOpArray,      propOpArray,      propOpArray);
   C_TestPropertyExpectedFieldType: array[TTestProperty] of TScalarPropertyType = (
      sptString,        sptInt32,         sptInt64,         sptDouble,        sptFloat,
      sptBool,          sptTimestamp,     sptString,        sptComplex,       sptComplex,
      sptTimestamp,     sptTimestamp,     sptTimestamp,     sptComplex,       sptComplex,
      sptComplex,       sptInt32,         sptInt32,         sptInt64,         sptDouble,
      sptFloat,         sptBool,          sptTimestamp,     sptTimestamp,     sptTimestamp);
   C_TestPropertyExpectedPropKind: array[TTestProperty] of TPropKind = (
      ptDefaultOptional,ptDefaultOptional,ptDefaultOptional,ptDefaultOptional,ptDefaultOptional,
      ptDefaultOptional,ptDefaultOptional,ptRepeated,       ptDefaultOptional,ptDefaultOptional,
      ptDefaultOptional,ptDefaultOptional,ptDefaultOptional,ptDefaultOptional,ptDefaultOptional,
      ptDefaultOptional,ptRepeated,       ptRepeated,       ptRepeated,       ptRepeated,
      ptRepeated,       ptRepeated,       ptRepeated,       ptRepeated,       ptRepeated);
type
   TTestMessage = (msgNone, msgSimple, msgAllTypes, msgSubTypes, msgSubMessage, msg1Param, msgEmpty, msgImport, msgImported, msgAllTypesArray);
   TTestMessages = array of TTestMessage;
const
   C_TestMessageNames: array[TTestMessage] of string = ('','MsgSimple', 'MsgAllTypes', 'MsgSubTypes','MsgSub','Msg1Param','MsgEmpty','MsgImport', 'MsgImported', 'MsgAllTypesArray');
   C_TestMessageProps: array[TTestMessage] of set of TTestProperty = (
      [],
      [propString, propInt32],
      [propString, propInt32, propInt64, propDouble, propFloat, propBool, propTimeStamp, propStringArray, propEnum, propMessage, propDate, propDateTime, propTime],
      [propSubMessage,propSubEnum],
      [propString],
      [propInt32],
      [],
      [propImportedMessage],
      [propDouble,propInt32],
      [propStringArray, propIntArray,  propInt32Array,   propInt64Array,   propDoubleArray, propFloatArray,   propBoolArray,    propDateArray,    propDateTimeArray,propTimeArray]
      );
type
   TTestServiceCall = (
      srvSimple,              srvSimpleNoIn,          srvSimple1ParamIn,      srvSimple1ParamOut,     srvSimpleEmptyParamOut,    srvSimple1Param,           srvSimple1ParamInNoOut,    srvSimple1ParamOutNoIn,    srvSimpleNoParam,
      srvStreamIn,                                    srvStreamIn1ParamIn,    srvStreamIn1ParamOut,   srvStreamInEmptyParamOut,  srvStreamIn1Param,         srvStreamIn1ParamInNoOut,
      srvStreamOut,           srvStreamOutNoIn,       srvStreamOut1ParamIn,   srvStreamOut1ParamOut,                             srvStreamOut1Param,                                   srvStreamOut1ParamOutNoIn,
      srvStreamBoth,                                  srvStreamBoth1ParamIn,  srvStreamBoth1ParamOut,                            srvStreamBoth1Param);
   // Call mag input hebben, maar moet output hebben
   TTestServiceCalls = array of TTestServiceCall;
const
   C_TestServiceCallName: array[TTestServiceCall] of string = (
      'Simple',               'SimpleNoIn',           'Simple1ParamIn',       'Simple1ParamOut',      'SimpleEmptyParamOut',     'Simple1Param',            'Simple1ParamInNoOut',     'Simple1ParamOutNoIn',     'SimpleNoParam',
      'StreamIn',                                     'StreamIn1ParamIn',     'StreamIn1ParamOut',    'StreamInEmptyParamOut',   'StreamIn1Param',          'StreamIn1ParamInNoOut',
      'StreamOut',            'StreamOutNoIn',        'StreamOut1ParamIn',    'StreamOut1ParamOut',                              'StreamOut1Param',                                    'StreamOut1ParamOutNoIn',
      'StreamBoth',                                   'StreamBoth1ParamIn',   'StreamBoth1ParamOut',                             'StreamBoth1Param');
   C_TestServiceCallInput: array[TTestServiceCall] of TTestMessage = (
      msgSimple,              msgNone,                msg1Param,              msgSimple,              msgSimple,                 msg1Param,                 msg1Param,                 msgNone,                   msgNone,
      msgSimple,                                      msg1Param,              msgSimple,              msgSimple,                 msg1Param,                 msg1Param,
      msgSimple,              msgNone,                msg1Param,              msgSimple,                                         msg1Param,                                            msgNone,
      msgSimple,                                      msg1Param,              msgSimple,                                         msg1Param);
   C_TestServiceCallInputIsStream: array[TTestServiceCall] of Boolean = (
      False,                  False,                  False,                  False,                  False,                     False,                     False,                     False,                     False,
      True,                                           True,                   True,                   True,                      True,                      True,
      False,                  False,                  False,                  False,                                             False,                                                False,
      True,                                           True,                   True,                                              True);
   C_TestServiceCallOutput: array[TTestServiceCall] of TTestMessage = (
      msgSimple,              msgSimple,              msgSimple,              msg1Param,              msgEmpty,                  msg1Param,                 msgEmpty,                  msg1Param,                 msgEmpty,
      msgSimple,                                      msgSimple,              msg1Param,              msgEmpty,                  msg1Param,                 msgEmpty,
      msgSimple,              msgSimple,              msgSimple,              msg1Param,                                         msg1Param,                                            msg1Param,
      msgSimple,                                      msgSimple,              msg1Param,                                         msg1Param);
   C_TestServiceCallOutputIsStream: array[TTestServiceCall] of Boolean = (
      False,                  False,                  False,                  False,                  False,                     False,                     False,                     False,                     False,
      False,                                          False,                  False,                  False,                     False,                     False,
      True,                   True,                   True,                   True,                                              True,                                                 True,
      True,                                           True,                   True,                                              True);
   C_AllCalls: TTestServiceCalls = [
      srvSimple,              srvSimpleNoIn,          srvSimple1ParamIn,      srvSimple1ParamOut,     srvSimpleEmptyParamOut,    srvSimple1Param,           srvSimple1ParamInNoOut,    srvSimple1ParamOutNoIn,    srvSimpleNoParam,
      srvStreamIn,                                    srvStreamIn1ParamIn,    srvStreamIn1ParamOut,   srvStreamInEmptyParamOut,  srvStreamIn1Param,         srvStreamIn1ParamInNoOut,
      srvStreamOut,           srvStreamOutNoIn,       srvStreamOut1ParamIn,   srvStreamOut1ParamOut,                             srvStreamOut1Param,                                   srvStreamOut1ParamOutNoIn,
      srvStreamBoth,                                  srvStreamBoth1ParamIn,  srvStreamBoth1ParamOut,                            srvStreamBoth1Param];
type
   TGrpcTestHelper = class(TTestCasePlus)
   private
      function GenerateEnum(const aType: TTestEnum): string;
      function GenerateProperty(const aType: TTestProperty; const aIndex: Integer): string;
      function GenerateMessage(const aType: TTestMessage): string;
      function GenerateServiceCall(const aType: TTestServiceCall): string;
      function GenerateProtoFile(const aTypes: TTestMessages; const aServieCalls: TTestServiceCalls): string;
   protected
      FProto: TProtoFile;
      procedure Parse(const aMessages: TTestMessages); overload; inline;
      procedure Parse(const aServieCalls: TTestServiceCalls); overload;
      procedure Parse(const aMessages: TTestMessages; const aServieCalls: TTestServiceCalls); overload; virtual;

      function Messages: TProtoBufMessageList; inline;
      function Message(const aType: TTestMessage): TProtoBufMessage; overload; inline;
      function Message(const aName: string): TProtoBufMessage; overload; inline;

      function Enums: TProtoBufEnumList; inline;

      function Services: TProtoBufServiceList; inline;
   protected
      function GetMsgSimple: TProtoBufMessage; inline;
      function GetMsgAllTypes: TProtoBufMessage; inline;
      function GetMsgSubTypes: TProtoBufMessage; inline;
      function GetMsgSubMessage: TProtoBufMessage; inline;

      function GetServiceTest: TProtoBufService; 
   public
      procedure SetUp; override;
      procedure TearDown; override;
   end;

const
   C_PackageName = 'GrpcTest';
   C_ServiceName = 'TestService';

type
   TGrpcTestHelperExport = class(TGrpcTestHelper)
   protected
      FText: TStringlist;
      procedure Parse(const aMessages: TTestMessages; const aServieCalls: TTestServiceCalls); overload; override;
      function TextWithoutLinebreaks: string;
   public
      procedure SetUp; override;
      procedure TearDown; override;
   end;

implementation

uses System.SysUtils, System.StrUtils;

{ TGrpcTestHelper }

procedure TGrpcTestHelper.SetUp;
begin
   inherited;
   FProto := TProtoFile.Create(nil);
   FProto.FileName := GetCurrentDir;
end;

procedure TGrpcTestHelper.TearDown;
begin
   FProto.Free;
   inherited;
end;

procedure TGrpcTestHelper.Parse(const aMessages: TTestMessages);
begin
   Parse(aMessages, []);
end;

procedure TGrpcTestHelper.Parse(const aServieCalls: TTestServiceCalls);
var MessageSet: set of TTestMessage;
    MessageArray: TTestMessages;
    ServiceCallType: TTestServiceCall;
    MessageType: TTestMessage;
begin
   MessageSet := [];
   for ServiceCallType in aServieCalls do
   begin
      Include(MessageSet, C_TestServiceCallInput[ServiceCallType]);
      Include(MessageSet, C_TestServiceCallOutput[ServiceCallType]);
   end;
   Exclude(MessageSet, msgNone);
   MessageArray := nil;

   for MessageType in MessageSet do
      MessageArray := MessageArray + [MessageType];

   Parse(MessageArray, aServieCalls);
end;

procedure TGrpcTestHelper.Parse(const aMessages: TTestMessages; const aServieCalls: TTestServiceCalls);
var iPos: Integer;
begin
   iPos := 1;
   Messages.Clear;
   Services.Clear;
   Enums.Clear;
   FProto.Imports.Clear;
   FProto.ParseFromProto(GenerateProtoFile(aMessages, aServieCalls), iPos);
end;

function TGrpcTestHelper.GenerateProtoFile(const aTypes: TTestMessages; const aServieCalls: TTestServiceCalls): string;
var MsgType: TTestMessage;
    ServiceCallType: TTestServiceCall;
    AddEnum, AddMessage: Boolean;
begin
   AddEnum := False;
   AddMessage := False;

   Result := 'syntax = "proto3";'+#13+
             'package '+C_PackageName+';'+#13;

   for MsgType in aTypes do
   begin
      if msgImport = MsgType then
         Result := Result + 'import "..\import_package.proto";'+#13;
   end;

   for MsgType in aTypes do
   begin
      Result := Result + GenerateMessage(MsgType);
      AddEnum := AddEnum or (propEnum in C_TestMessageProps[MsgType]);
      AddMessage := AddMessage or (propMessage in C_TestMessageProps[MsgType]);
   end;

   if AddMessage then
   begin
      // Do not generate double
      for MsgType in aTypes do
         AddMessage := AddMessage and (MsgType <> msgSimple);
      if AddMessage then      
         Result := Result + GenerateMessage(msgSimple);   
   end;
      
   if AddEnum then
      Result := Result + GenerateEnum(enum);
   
   if (Length(aServieCalls) > 0) then
   begin
      Result := Result + 'service '+ C_ServiceName + '{';
      for ServiceCallType in aServieCalls do
         Result := Result + GenerateServiceCall(ServiceCallType);   
      Result := Result + '}';
   end;
end;

function TGrpcTestHelper.GenerateServiceCall(const aType: TTestServiceCall): string;
begin
   Result := Format('rpc %s(%s%s) returns (%s%s) {}', [
      C_TestServiceCallName[aType],
      IfThen(C_TestServiceCallInputIsStream[aType],'stream '),
      C_TestMessageNames[C_TestServiceCallInput[aType]],
      IfThen(C_TestServiceCallOutputIsStream[aType],'stream '),
      C_TestMessageNames[C_TestServiceCallOutput[aType]]
      ]);
end;

function TGrpcTestHelper.GenerateEnum(const aType: TTestEnum): string;
var Value: string;
    Index: Integer;
begin
   Result := 'enum '+C_TestEnumNames[aType]+' {'+#13;

   Index := 0;
   for Value in C_TestEnumValues[aType] do
   begin
      Result := Format('%s  %s = %d;'+#13, [Result, Value, Index]);
      Inc(Index);
   end;

   Result := Result + '}';   
end;

function TGrpcTestHelper.GenerateMessage(const aType: TTestMessage): string;
var aProp: TTestProperty;
    Index: Integer;
begin
   Assert(aType<>msgNone);
   Result := 'message '+C_TestMessageNames[aType]+' {'+#13;

   Index := 1;
   for aProp in C_TestMessageProps[aType] do
   begin
      Result := Result + GenerateProperty(aProp, Index);
      Inc(Index);
   end;

   Result := Result + '}';
end;

function TGrpcTestHelper.GenerateProperty(const aType: TTestProperty; const aIndex: Integer): string;
begin   
   Result := Format('%s%s %s = %d;'+#13,[
      C_TestPropertyOptionValue[C_TestPropertyOptions[aType]], 
      C_TestPropertyFieldTypes[aType], 
      C_TestPropertyNames[aType], 
      aIndex]);

   case aType of
      propSubMessage:
         Result := GenerateMessage(msgSubMessage) + Result;
      propSubEnum:
         Result := GenerateEnum(enumSub) + Result;
   end;      
end;

function TGrpcTestHelper.GetMsgAllTypes: TProtoBufMessage;
begin
   Result := Message(msgAllTypes);
end;

function TGrpcTestHelper.GetMsgSimple: TProtoBufMessage;
begin
   Result := Message(msgSimple);
end;

function TGrpcTestHelper.GetMsgSubMessage: TProtoBufMessage;
begin
   Result := Message(msgSubMessage);
end;

function TGrpcTestHelper.GetMsgSubTypes: TProtoBufMessage;
begin
   Result := Message(msgSubTypes);
end;

function TGrpcTestHelper.GetServiceTest: TProtoBufService;
begin
   Result := Services[0]; // doen er toch maar 1
end;

function TGrpcTestHelper.Message(const aType: TTestMessage): TProtoBufMessage;
begin
   Result := Message(C_TestMessageNames[aType]);
end;

function TGrpcTestHelper.Message(const aName: string): TProtoBufMessage;
begin
   Result := Messages.FindByName(aName);
end;

function TGrpcTestHelper.Messages: TProtoBufMessageList;
begin
   Result := FProto.ProtoBufMessages;
end;

function TGrpcTestHelper.Enums: TProtoBufEnumList;
begin
   Result := FProto.ProtoBufEnums;
end;

function TGrpcTestHelper.Services: TProtoBufServiceList;
begin
   Result := FProto.ProtoBufServices;   
end;

{ TGrpcTestHelperExport }

procedure TGrpcTestHelperExport.Parse(const aMessages: TTestMessages; const aServieCalls: TTestServiceCalls);
begin
   inherited;
   FText.Clear;
end;

procedure TGrpcTestHelperExport.SetUp;
begin
   inherited;
   FText := TStringList.Create;
end;

procedure TGrpcTestHelperExport.TearDown;
begin
   FText.Free;
   inherited;
end;

function TGrpcTestHelperExport.TextWithoutLinebreaks: string;
begin
   Result := ReplaceLineBreaksAndIndent(FText.Text);
end;

end.

