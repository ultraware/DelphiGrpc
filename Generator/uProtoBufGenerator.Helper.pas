unit uProtoBufGenerator.Helper;

interface

uses uProtoBufParserAbstractClasses, uProtoBufParserClasses;

type
   TAbstractProtoBufParserItemHelper = class helper for TAbstractProtoBufParserItem
      function ProtoFile: TProtoFile;
   end;

   TProtoBufMessageHelper = class helper for TProtoBufMessage
      function DelphiClassname: string;
      function DelphiClassnameArray: string;
      function Has1Param: Boolean;
   end;

   TProtoBufPropertyHelper = class helper for TProtoBufProperty
      function DelphiType: string;
   end;

   TProtoBufEnumHelper = class helper for TProtoBufEnum
      function DelphiClassname: string;
      function DelphiClassnameArray: string;
   end;

   TProtoBufServiceHelper = class helper for TProtoBufService
      function DelphiClassname: string;
   end;

   TProtoBufRPCHelper = class helper for TProtoBufRPC
      function InputStreamInterfaceType: string;
      function ServerInputStreamInterfaceType: string;
      function ServerInputStreamClassType: string;
      function InputStreamType: string;
      function OutputCallbackType: string;
      function HasNonSteamInput: Boolean;
      function HasOutput: Boolean;
      function HasInput: Boolean;
      function Has1OutputParam: Boolean;
      function Has1InputParam: Boolean;
      function ServerOutputInterfaceType: string;
      function ServerOutputClassType: string;
   end;

function DelphiTypeToArray(const aDelphiType: string): string;

implementation

uses System.SysUtils, System.StrUtils;

{ TProtoBufMessageHelper }

function _LastPartOfName(const aName: string): string;
var i: Integer;
begin
   i := LastDelimiter('.', aName);
	Result := Copy(aName, i + 1, Length(aName));
end;

function _NameToDelphiClassName(const aName: string): string;
begin
	Result := 'T' + _LastPartOfName(aName);
end;

function TProtoBufMessageHelper.DelphiClassname: string;
begin
   Result := _NameToDelphiClassName(Name);
end;

function TProtoBufMessageHelper.DelphiClassnameArray: string;
begin
   Result := DelphiClassname+'Array';
end;

function TProtoBufMessageHelper.Has1Param: Boolean;
begin
   Result := Count = 1;
end;

{ TProtoBufEnumHelper }

function TProtoBufEnumHelper.DelphiClassname: string;
begin
   Result := _NameToDelphiClassName(Name);
end;

function TProtoBufEnumHelper.DelphiClassnameArray: string;
begin
   Result := DelphiClassname+'Array';
end;

{ TProtoBufServiceHelper }

function TProtoBufServiceHelper.DelphiClassname: string;
begin
   Result := _NameToDelphiClassName(Name);
end;

{ TProtoBufRPCHelper }

function TProtoBufRPCHelper.HasOutput: Boolean;
begin
   Result := RPCResult.Count > 0;
end;

function TProtoBufRPCHelper.Has1InputParam: Boolean;
begin
   Result := HasInput and RPCArgument.Has1Param;
end;

function TProtoBufRPCHelper.Has1OutputParam: Boolean;
begin
   Result := {HasOutput and }RPCResult.Has1Param;
end;

function TProtoBufRPCHelper.HasInput: Boolean;
begin
   Result := Assigned(RPCArgument);
end;

function TProtoBufRPCHelper.HasNonSteamInput: Boolean;
begin
   Result := HasInput and (not ArgementIsStream);
end;

function TProtoBufRPCHelper.InputStreamInterfaceType: string;
begin
   Assert(ArgementIsStream, 'Argument must be a stream');
   Result := Format('I%sStream_Send',[_LastPartOfName(Self.Name)]); // Stream needs input and output -> name identical to rpc call
end;

function TProtoBufRPCHelper.ServerInputStreamInterfaceType: string;
begin
   Assert(ArgementIsStream, 'Argument must be a stream');
   Result := Format('I%sStream_Recv',[_LastPartOfName(Self.Name)]); // Stream needs input and output -> name identical to rpc call
end;

function TProtoBufRPCHelper.ServerInputStreamClassType: string;
begin
   Assert(ArgementIsStream, 'Argument must be a stream');
   Result := Format('T%sStream_Recv',[_LastPartOfName(Self.Name)]); // Stream needs input and output -> name identical to rpc call
end;

function TProtoBufRPCHelper.InputStreamType: string;
begin
   Assert(ArgementIsStream, 'Argument must be a stream');
   Result := Format('T%sStream_Send',[_LastPartOfName(Self.Name)]); // Stream needs input and output -> name identical to rpc call
end;

function TProtoBufRPCHelper.OutputCallbackType: string;
begin
   Assert(ResultIsStream, 'Result must be a stream');
   Result := Format('T%sCallback',[_LastPartOfName(Self.Name)]);
end;

function TProtoBufRPCHelper.ServerOutputInterfaceType: string;
begin
   Assert(ResultIsStream, 'Result must be a stream');
   Result := Format('I%sStream_Send',[_LastPartOfName(Self.Name)]);
end;

function TProtoBufRPCHelper.ServerOutputClassType: string;
begin
   Assert(ResultIsStream, 'Result must be a stream');
   Result := Format('T%sStream_Send',[_LastPartOfName(Self.Name)]);
end;

{ TProtoBufPropertyHelper }

function TProtoBufPropertyHelper.DelphiType: string;
var StandartType: TScalarPropertyType;
	 msg: TProtoBufMessage;
    enum: TProtoBufEnum;
begin
	StandartType := StrToPropertyType(PropType);
	case StandartType of
		sptDouble:
			Result := 'Double';
		sptFloat:
			Result := 'Single';
		sptInt32:
			Result := 'Integer';
		sptInt64:
			Result := 'Int64';
		sptuInt32:
			Result := 'Cardinal';
		sptUint64:
			Result := 'UInt64';
		sptSInt32:
			Result := 'Int32';
		sptSInt64:
			Result := 'Int64';
		sptFixed32:
			Result := 'Int32';
		sptFixed64:
			Result := 'Int64';
		sptSFixed32:
			Result := 'Int32';
		sptSFixed64:
			Result := 'Int64';
		sptBool:
			Result := 'Boolean';
		sptString:
			Result := 'string';
		sptBytes:
			Result := 'TBytes';
      sptTimestamp:
      begin
         if EndsText('Date',Name) then
            Result := 'TDate'
         else if EndsText('Time', Name) and (not EndsText('DateTime', Name)) then
            Result := 'TTime'
         else
            Result := 'TDateTime';
      end;
      sptComplex:
      begin
         msg := ProtoFile.ProtoBufMessages.FindByName(_LastPartOfName(Self.PropType));
         if Assigned(msg) then
         begin
            if PropKind = TPropKind.ptRepeated then
               Result := msg.DelphiClassnameArray
            else
               Result := msg.DelphiClassname;
         end;

         enum := ProtoFile.ProtoBufEnums.FindByName(_LastPartOfName(Self.PropType));
         if Assigned(enum) then
         begin
            if PropKind = TPropKind.ptRepeated then
               Result := enum.DelphiClassnameArray
            else
               Result := enum.DelphiClassname;
         end;
         Exit;
      end
	else
	   Assert(False, 'unknown type');
	end;
   if Self.PropKind = TPropKind.ptRepeated then
      Result := DelphiTypeToArray(Result);
end;

function DelphiTypeToArray(const aDelphiType: string): string;
begin
   Result := Format('T%0:sArray',[ UpperCase(aDelphiType[1])+Copy(aDelphiType, 2, MaxInt)]);
end;

{ TAbstractProtoBufParserItemHelper }

type TAbstractProtoBufParserItem2 = class(TAbstractProtoBufParserItem);
function TAbstractProtoBufParserItemHelper.ProtoFile: TProtoFile;
begin
   Result := TAbstractProtoBufParserItem2(Self).FRoot as TProtoFile;
end;

end.
