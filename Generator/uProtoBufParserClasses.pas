unit uProtoBufParserClasses;

interface

uses
	System.Classes,
	System.Generics.Defaults,
	System.Generics.Collections,
	uProtoBufParserAbstractClasses;

type
	TPropKind = ( //
		ptDefaultOptional, //
		ptRequired, //
		ptOptional, //
		ptRepeated, //
		ptReserved);

	TScalarPropertyType = ( //
		sptComplex, //
		sptDouble, //
		sptFloat, //
		sptInt32, //
		sptInt64, //
		sptuInt32, //
		sptUint64, //
		sptSInt32, //
		sptSInt64, //
		sptFixed32, //
		sptFixed64, //
		sptSFixed32, //
		sptSFixed64, //
		sptBool, //
		sptString, //
		sptBytes,
      sptTimestamp);

   TProtoFile = class;
   TProtoBufMessage = class;

	TProtoBufPropOption = class(TAbstractProtoBufParserItem)
	private
		FOptionValue: string;
	public
		procedure ParseFromProto(const Proto: string; var iPos: integer); override;
		property OptionValue: string read FOptionValue;
	end;

	TProtoBufPropOptions = class(TAbstractProtoBufParserContainer<TProtoBufPropOption>)
	private
		function GetHasValue(const OptionName: string): Boolean;
		function GetValue(const OptionName: string): string;
	public
		procedure ParseFromProto(const Proto: string; var iPos: integer); override;
		property HasValue[const OptionName: string]: Boolean read GetHasValue;
		property Value[const OptionName: string]: string read GetValue;
	end;

	TProtoBufProperty = class(TAbstractProtoBufParserItem)
	strict private
		FPropFieldNum: integer;
		FPropType: string;
		FPropKind: TPropKind;
		FPropComment: string;
		FPropOptions: TProtoBufPropOptions;
	public
		constructor Create(ARoot: TAbstractProtoBufParserItem); override;
		destructor Destroy; override;

		procedure ParseFromProto(const Proto: string; var iPos: integer); override;

		property PropKind: TPropKind read FPropKind;
		property PropType: string read FPropType;
		property PropFieldNum: integer read FPropFieldNum;
		property PropComment: string read FPropComment;
		property PropOptions: TProtoBufPropOptions read FPropOptions;
	end;

	TProtoBufRPC = class(TAbstractProtoBufParserItem)
   strict private
   type
      TRPCVal = record
         Name: string;
         IsStream: Boolean;
      end;

	strict private
		FArgument: TRPCVal;
		FResult: TRPCVal;
		FComment: string;
      function ProtoFile: TProtoFile;
      function FindMessage(const Val: TRPCVal): TProtoBufMessage;
      function GetArgument: TProtoBufMessage;
      function GetResult: TProtoBufMessage;
    function GetArgementIsStream: Boolean;

    function GetResultIsStream: Boolean;	public
		procedure ParseFromProto(const Proto: string; var iPos: integer); override;

		property RPCArgument: TProtoBufMessage read GetArgument;
      property ArgementIsStream: Boolean read GetArgementIsStream;
		property RPCResult: TProtoBufMessage read GetResult;
      property ResultIsStream: Boolean read GetResultIsStream;
		property Comment: string read FComment;
	end;

	TProtoBufEnumValue = class(TAbstractProtoBufParserItem)
	strict private
		FValue: integer;
	public
		procedure ParseFromProto(const Proto: string; var iPos: integer); override;

		property Value: integer read FValue;
	end;

	TProtoBufEnum = class(TAbstractProtoBufParserContainer<TProtoBufEnumValue>)
	public
		procedure ParseFromProto(const Proto: string; var iPos: integer); override;
	end;

	TProtoBufMessage = class(TAbstractProtoBufParserContainer<TProtoBufProperty>)
	public
		procedure ParseFromProto(const Proto: string; var iPos: integer); override;

		function HasPropertyOfType(const APropType: string): Boolean;
	end;

	TProtoBufService = class(TAbstractProtoBufParserContainer<TProtoBufRPC>)
	public
		procedure ParseFromProto(const Proto: string; var iPos: integer); override;
	end;

	TProtoBufEnumList = class(TObjectList<TProtoBufEnum>)
	public
		function FindByName(const EnumName: string): TProtoBufEnum;
	end;

	TProtoBufMessageList = class(TObjectList<TProtoBufMessage>)
	public
		function FindByName(const MessageName: string): TProtoBufMessage;
	end;

   TProtoBufServiceList = class(TObjectList<TProtoBufService>)
      function NonImportedCount: Integer;
   end;

	TProtoSyntaxVersion = (psv2, psv3);

	TProtoFile = class(TAbstractProtoBufParserItem)
	strict private
		FProtoBufMessages: TProtoBufMessageList;
		FProtoBufServices: TProtoBufServiceList;
		FProtoBufEnums: TProtoBufEnumList;

		FInImportCounter: integer;
		FFileName: string;
		FImports: TStrings;
		FProtoSyntaxVersion: TProtoSyntaxVersion;

		procedure ImportFromProtoFile(const AFileName: string);
	private
		procedure SetFileName(const Value: string);
	public
		constructor Create(ARoot: TAbstractProtoBufParserItem); override;
		destructor Destroy; override;

		procedure ParseEnum(const Proto: string; var iPos: integer);
		procedure ParseMessage(const Proto: string; var iPos: integer; IsExtension: Boolean = False);
		procedure ParseService(const Proto: string; var iPos: integer);
		procedure ParseFromProto(const Proto: string; var iPos: integer); override;

		property Imports: TStrings read FImports;
		property ProtoBufEnums: TProtoBufEnumList read FProtoBufEnums;
		property ProtoBufMessages: TProtoBufMessageList read FProtoBufMessages;
      property ProtoBufServices: TProtoBufServiceList read FProtoBufServices;

		property FileName: string read FFileName write SetFileName;
		property ProtoSyntaxVersion: TProtoSyntaxVersion read FProtoSyntaxVersion;
	end;

function StrToPropertyType(const AStr: string): TScalarPropertyType;

implementation

uses
	System.SysUtils,
	System.StrUtils,
	System.IOUtils,
	Winapi.Windows,
	System.Character;

function PropKindToStr(APropKind: TPropKind): string;
begin
	case APropKind of
		ptDefaultOptional:
			Result := '';
		ptRequired:
			Result := 'required';
		ptOptional:
			Result := 'optional';
		ptRepeated:
			Result := 'repeated';
		ptReserved:
			Result := 'reserved';
	end;
end;

function StrToPropKind(const AStr: string): TPropKind;
var
	i: TPropKind;
begin
	Result := Low(TPropKind);
	for i := Low(TPropKind) to High(TPropKind) do
		if SameStr(PropKindToStr(i), AStr) then
		begin
			Result := i;
			Break;
		end;
end;

function PropertyTypeToStr(PropType: TScalarPropertyType): string;
begin
	Result := '';
	case PropType of
		sptComplex:
			;
		sptDouble:
			Result := 'double';
		sptFloat:
			Result := 'float';
		sptInt32:
			Result := 'int32';
		sptInt64:
			Result := 'int64';
		sptuInt32:
			Result := 'uint32';
		sptUint64:
			Result := 'uint64';
		sptSInt32:
			Result := 'sint32';
		sptSInt64:
			Result := 'sint64';
		sptFixed32:
			Result := 'fixed32';
		sptFixed64:
			Result := 'fixed64';
		sptSFixed32:
			Result := 'sfixed32';
		sptSFixed64:
			Result := 'sfixed64';
		sptBool:
			Result := 'bool';
		sptString:
			Result := 'string';
		sptBytes:
			Result := 'bytes';
      sptTimestamp:
         Result := 'timestamp';
	end;
end;

function StrToPropertyType(const AStr: string): TScalarPropertyType;
var
	i: TScalarPropertyType;
begin
	Result := Low(TScalarPropertyType);
	for i := Low(TScalarPropertyType) to High(TScalarPropertyType) do
		if SameStr(PropertyTypeToStr(i), AStr) then
		begin
			Result := i;
			Break;
		end;
end;

{ TProtoBufProperty }

constructor TProtoBufProperty.Create(ARoot: TAbstractProtoBufParserItem);
begin
	inherited;
	FPropOptions := TProtoBufPropOptions.Create(ARoot);
end;

destructor TProtoBufProperty.Destroy;
begin
	FreeAndNil(FPropOptions);
	inherited;
end;

procedure SkipWhitespaces(const Proto: string; var iPos: integer);
begin
	while Proto[iPos].IsWhiteSpace and (iPos <= Length(Proto)) do
		Inc(iPos);
end;

function ReadAllTillChar(const Proto: string; var iPos: integer; BreakSymbol: array of Char): string;
begin
	Result := '';
	while not Proto[iPos].IsInArray(BreakSymbol) and (iPos <= Length(Proto)) do
	begin
		Result := Result + Proto[iPos];
		Inc(iPos);
	end;
end;

function ReadAllToEOL(const Proto: string; var iPos: integer): string;
begin
	Result := ReadAllTillChar(Proto, iPos, [#13, #10]);
end;

function ReadCommentIfExists(const Proto: string; var iPos: integer): string;
begin
	SkipWhitespaces(Proto, iPos);
	if (Proto[iPos] = '/') and (Proto[iPos + 1] = '/') then
	begin
		Inc(iPos, 2);
		Result := ReadAllToEOL(Proto, iPos) + ' ';
	end
	else
		Result := '';
end;

procedure SkipAllComments(const Proto: string; var iPos: integer);
begin
	while ReadCommentIfExists(Proto, iPos) <> '' do;
	SkipWhitespaces(Proto, iPos);
end;

procedure SkipRequiredChar(const Proto: string; var iPos: integer; const RequiredChar: Char);
begin
	SkipWhitespaces(Proto, iPos);
	if Proto[iPos] <> RequiredChar then
		raise EParserError.Create(RequiredChar + ' not found in ProtoBuf');
	Inc(iPos);
end;

function ReadWordFromBuf(const Proto: string; var iPos: integer; BreakSymbols: array of Char): string;
begin
	SkipWhitespaces(Proto, iPos);

	Result := '';
	while not Proto[iPos].IsWhiteSpace and not Proto[iPos].IsInArray(BreakSymbols) and (iPos <= Length(Proto)) do
	begin
		Result := Result + Proto[iPos];
		Inc(iPos);
	end;
end;

procedure TProtoBufProperty.ParseFromProto(const Proto: string; var iPos: integer);
var
	Buf: string;
	tmpOption: TProtoBufPropOption;
begin
	inherited;
	FPropOptions.Clear;
	{
	  [optional] int32   DefField1  = 1  [default = 2]; // def field 1, default value 2
	  int64 DefField2 = 2;
	}
	Buf := ReadWordFromBuf(Proto, iPos, []);
	// in Buf - first word of property. Choose type
	FPropKind := StrToPropKind(Buf);
	if FPropKind = ptReserved then
	begin
		ReadAllTillChar(Proto, iPos, [';']);
		SkipRequiredChar(Proto, iPos, ';');
		exit; // reserved is not supported now by this parser
	end;
	if FPropKind <> ptDefaultOptional then // if required/optional/repeated is not skipped,
		Buf := ReadWordFromBuf(Proto, iPos, []); // read type of property

	FPropType := Buf;

	FName := ReadWordFromBuf(Proto, iPos, ['=']);

	// skip '=' character
	SkipRequiredChar(Proto, iPos, '=');

	// read property tag
	Buf := ReadWordFromBuf(Proto, iPos, [';', '[']);
	FPropFieldNum := StrToInt(Buf);

	SkipWhitespaces(Proto, iPos);
	if Proto[iPos] = '[' then
		FPropOptions.ParseFromProto(Proto, iPos);

	if Assigned(FRoot) then
		if TProtoFile(FRoot).ProtoSyntaxVersion = psv3 then
			if not FPropOptions.HasValue['packed'] then
			begin
				tmpOption := TProtoBufPropOption.Create(FRoot);
				FPropOptions.Add(tmpOption);
				tmpOption.Name := 'packed';
				tmpOption.FOptionValue := 'true';
			end;

	// read separator
	SkipRequiredChar(Proto, iPos, ';');

	FPropComment := ReadCommentIfExists(Proto, iPos);
end;

{ TProtoBufPropOption }

procedure TProtoBufPropOption.ParseFromProto(const Proto: string; var iPos: integer);
begin
	inherited;
	{ [default = Val2, packed = true]; }
	SkipWhitespaces(Proto, iPos);
	FName := ReadWordFromBuf(Proto, iPos, ['=']);

	// skip '=' character
	SkipRequiredChar(Proto, iPos, '=');

	SkipWhitespaces(Proto, iPos);
	if Proto[iPos] <> '"' then
		FOptionValue := ReadWordFromBuf(Proto, iPos, [',', ']'])
	else
	begin
		Inc(iPos);
		{ TODO : Solve problem with double "" in the middle of string... }
		FOptionValue := Trim('"' + ReadAllTillChar(Proto, iPos, [',', ']', #13, #10]));
		if not EndsStr('"', FOptionValue) then
			raise EParserError.Create('no string escape in property ' + Name);
		// SkipRequiredChar(Proto, iPos, '"');
	end;

	SkipWhitespaces(Proto, iPos);
	if Proto[iPos] = ',' then
		Inc(iPos);
end;

{ TProtoBufPropOptions }

function TProtoBufPropOptions.GetHasValue(const OptionName: string): Boolean;
var
	i: integer;
begin
	Result := False;
	for i := 0 to Count - 1 do
		if Items[i].Name = OptionName then
		begin
			Result := True;
			Break;
		end;
end;

function TProtoBufPropOptions.GetValue(const OptionName: string): string;
var
	i: integer;
begin
	Result := '';
	for i := 0 to Count - 1 do
		if Items[i].Name = OptionName then
		begin
			Result := Items[i].OptionValue;
			Break;
		end;
end;

procedure TProtoBufPropOptions.ParseFromProto(const Proto: string; var iPos: integer);
var
	Option: TProtoBufPropOption;
begin
	inherited;
	SkipRequiredChar(Proto, iPos, '[');
	SkipWhitespaces(Proto, iPos); // check for empty options
	while Proto[iPos] <> ']' do
	begin
		Option := TProtoBufPropOption.Create(FRoot);
		try
			Option.ParseFromProto(Proto, iPos);
			Add(Option);
			Option := nil;
			SkipWhitespaces(Proto, iPos);
		finally
			Option.Free;
		end;
	end;
	SkipRequiredChar(Proto, iPos, ']');
end;

{ TProtoBufEnumValue }

procedure TProtoBufEnumValue.ParseFromProto(const Proto: string; var iPos: integer);
begin
	inherited;
	{ Val1 = 1; }
	FName := ReadWordFromBuf(Proto, iPos, ['=']);
	SkipRequiredChar(Proto, iPos, '=');
	FValue := StrToInt(ReadWordFromBuf(Proto, iPos, [';']));
	SkipRequiredChar(Proto, iPos, ';');
end;

{ TProtoBufEnum }

procedure TProtoBufEnum.ParseFromProto(const Proto: string; var iPos: integer);
var
	Item: TProtoBufEnumValue;
begin
	inherited;
	(* Enum1 {
	  Val1 = 1;
	  Val2 = 2;
	  } *)

	FName := ReadWordFromBuf(Proto, iPos, ['{']);
	SkipRequiredChar(Proto, iPos, '{');
	SkipAllComments(Proto, iPos);
	while Proto[iPos] <> '}' do
	begin
		Item := TProtoBufEnumValue.Create(FRoot);
		try
			Item.ParseFromProto(Proto, iPos);
			Add(Item);
			Item := nil;
			SkipAllComments(Proto, iPos);
		finally
			Item.Free;
		end;
	end;
	SkipRequiredChar(Proto, iPos, '}');

	Sort(TComparer<TProtoBufEnumValue>.Construct(
		function(const Left, Right: TProtoBufEnumValue): integer
		begin
			Result := Left.Value - Right.Value;
		end));
end;

{ TProtoBufMessage }

function TProtoBufMessage.HasPropertyOfType(const APropType: string): Boolean;
var
	i: integer;
begin
	Result := False;
	for i := 0 to Count - 1 do
		if Items[i].PropType = APropType then
		begin
			Result := True;
			Break;
		end;
end;

procedure TProtoBufMessage.ParseFromProto(const Proto: string; var iPos: integer);
var
	Item: TProtoBufProperty;
begin
	inherited;
	(*
	  TestMsg0 {
	  required int32 Field1 = 1;
	  required int64 Field2 = 2;
	  }
	*)

	FName := ReadWordFromBuf(Proto, iPos, ['{']);
	SkipRequiredChar(Proto, iPos, '{');
	SkipAllComments(Proto, iPos);
	while Proto[iPos] <> '}' do
	begin
		SkipAllComments(Proto, iPos);
		SkipWhitespaces(Proto, iPos);
		if PosEx('enum', Proto, iPos) = iPos then
		begin
			Inc(iPos, Length('enum'));
			if FRoot is TProtoFile then
			begin
				TProtoFile(FRoot).ParseEnum(Proto, iPos);
				Continue;
			end;
		end;
		if PosEx('message', Proto, iPos) = iPos then
		begin
			Inc(iPos, Length('message'));
			if FRoot is TProtoFile then
			begin
				TProtoFile(FRoot).ParseMessage(Proto, iPos);
				Continue;
			end;
		end;

		Item := TProtoBufProperty.Create(FRoot);
		try
			Item.ParseFromProto(Proto, iPos);
			Add(Item);
			Item := nil;
			SkipAllComments(Proto, iPos);
		finally
			Item.Free;
		end;
	end;
	SkipRequiredChar(Proto, iPos, '}');

	Sort(TComparer<TProtoBufProperty>.Construct(
		function(const Left, Right: TProtoBufProperty): integer
		begin
			Result := Left.PropFieldNum - Right.PropFieldNum;
		end));
end;

procedure TProtoBufService.ParseFromProto(const Proto: string; var iPos: integer);
var
	RPC: TProtoBufRPC;
begin
	inherited;
	(*
      service DemoService {
        rpc SimpleFunc(message) returns (message) {}
        rpc StreamOutputFunc(message) returns (stream message) {}
        rpc StreamInputFunc(stream message) returns (message) {}
        rpc StreamInputAndOutputFunc(stream message) returns (stream message) {}
      }
	*)

	FName := ReadWordFromBuf(Proto, iPos, ['{']);
	SkipRequiredChar(Proto, iPos, '{');
	SkipAllComments(Proto, iPos);
	while Proto[iPos] <> '}' do
	begin
		SkipAllComments(Proto, iPos);
		SkipWhitespaces(Proto, iPos);

		RPC := TProtoBufRPC.Create(FRoot);
		try
			RPC.ParseFromProto(Proto, iPos);
			Add(RPC);
			RPC := nil;
			SkipAllComments(Proto, iPos);
		finally
			RPC.Free;
		end;

	end;
	SkipRequiredChar(Proto, iPos, '}');
end;

{ TProtoBufServiceList }

function TProtoBufServiceList.NonImportedCount: Integer;
var srv: TProtoBufService;
begin
  Result := 0;
  for srv in Self do
  begin
    if not srv.IsImported then
      Inc(Result);
  end;
end;

{ TProtoFile }

constructor TProtoFile.Create(ARoot: TAbstractProtoBufParserItem);
begin
	inherited;
	FImports := TStringList.Create;
	FProtoBufMessages := TProtoBufMessageList.Create;
	FProtoBufServices := TProtoBufServiceList.Create;
	FProtoBufEnums := TProtoBufEnumList.Create;
end;

destructor TProtoFile.Destroy;
begin
	FreeAndNil(FProtoBufEnums);
	FreeAndNil(FProtoBufServices);
	FreeAndNil(FProtoBufMessages);
	FreeAndNil(FImports);
	inherited;
end;

function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD; pszTo: PChar; dwAtrTo: DWORD): LongBool; stdcall; external 'shlwapi.dll' name 'PathRelativePathToW';
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall; external 'shlwapi.dll' name 'PathCanonicalizeW';

function AbsToRel(const AbsPath, BasePath: string): string;
var
	Path: array [0 .. MAX_PATH - 1] of Char;
begin
	PathRelativePathTo(@Path[0], PChar(BasePath), FILE_ATTRIBUTE_DIRECTORY, PChar(AbsPath), 0);
	Result := Path;
end;

function RelToAbs(const RelPath, BasePath: string): string;
var
	Dst: array [0 .. MAX_PATH - 1] of Char;
begin
	if TPath.IsRelativePath(RelPath) then
	begin
		PathCanonicalize(@Dst[0], PChar(IncludeTrailingPathDelimiter(BasePath) + RelPath));
		Result := Dst;
	end
	else
		Result := RelPath;
end;

procedure TProtoFile.ImportFromProtoFile(const AFileName: string);
var
	SL: TStringList;
	iPos: integer;
	OldFileName: string;
	OldName: string;
	sFileName: string;
begin
	if FFileName = '' then
		raise EParserError.CreateFmt('Cant import from %s, because source .proto file name not specified', [AFileName]);
	Inc(FInImportCounter);
	try
		SL := TStringList.Create;
		try
			sFileName := RelToAbs(AFileName, ExtractFilePath(FFileName));
			SL.LoadFromFile(sFileName);

			OldFileName := FFileName;
			OldName := FName;
			try
				FFileName := sFileName;
				iPos := 1;
				ParseFromProto(SL.Text, iPos);
				FImports.Add(FName);
			finally
				FFileName := OldFileName;
				FName := OldName;
			end;
		finally
			SL.Free;
		end;
	finally
		Dec(FInImportCounter);
	end;
end;

procedure TProtoFile.ParseEnum(const Proto: string; var iPos: integer);
var
	Enum: TProtoBufEnum;
begin
	Enum := TProtoBufEnum.Create(Self);
	try
		Enum.IsImported := FInImportCounter > 0;
		Enum.ParseFromProto(Proto, iPos);
		FProtoBufEnums.Add(Enum);
		Enum := nil;
	finally
		Enum.Free;
	end;
end;

procedure TProtoFile.ParseFromProto(const Proto: string; var iPos: integer);
var
	Buf: string;
	i, j: integer;
begin
	// need skip comments,
	// parse .proto package name
	while iPos < Length(Proto) do
	begin
		SkipAllComments(Proto, iPos);
		Buf := ReadWordFromBuf(Proto, iPos, []);

		if Buf = 'package' then
		begin
			FName := ReadWordFromBuf(Proto, iPos, [';']);
			SkipRequiredChar(Proto, iPos, ';');
		end;

		if Buf = 'syntax' then
		begin
			SkipRequiredChar(Proto, iPos, '=');
			Buf := Trim(ReadWordFromBuf(Proto, iPos, [';']));
			SkipRequiredChar(Proto, iPos, ';');
			if Buf = '"proto3"' then
				FProtoSyntaxVersion := psv3;
		end;

		if Buf = 'import' then
		begin
			Buf := Trim(ReadAllTillChar(Proto, iPos, [';']));
			ImportFromProtoFile(AnsiDequotedStr(Buf, '"'));
			SkipRequiredChar(Proto, iPos, ';');
		end;

		if Buf = 'enum' then
			ParseEnum(Proto, iPos);

		if Buf = 'message' then
			ParseMessage(Proto, iPos);
		if Buf = 'extend' then
			ParseMessage(Proto, iPos, True);

		if Buf = 'service' then
			ParseService(Proto, iPos);
	end;

	// can`t use QuickSort because of .proto items order
	for i := 0 to FProtoBufMessages.Count - 1 do
		for j := i + 1 to FProtoBufMessages.Count - 1 do
		begin
			if FProtoBufMessages[i].HasPropertyOfType(FProtoBufMessages[j].Name) then
				FProtoBufMessages.Exchange(i, j);
		end;
end;

procedure TProtoFile.ParseMessage(const Proto: string; var iPos: integer; IsExtension: Boolean);
var
	Msg: TProtoBufMessage;
	i: integer;
begin
	Msg := TProtoBufMessage.Create(Self);
	try
		Msg.IsImported := FInImportCounter > 0;
		Msg.ParseFromProto(Proto, iPos);
		if IsExtension then
		begin
			for i := 1 to High(integer) do
				if FProtoBufMessages.FindByName(Msg.Name + 'Extension' + IntToStr(i)) = nil then
				begin
					Msg.ExtendOf := Msg.Name;
					Msg.Name := Msg.Name + 'Extension' + IntToStr(i);
					Break;
				end;
		end;
		FProtoBufMessages.Add(Msg);
		Msg := nil;
	finally
		Msg.Free;
	end;
end;

procedure TProtoFile.ParseService(const Proto: string; var iPos: integer);
var
	Srv: TProtoBufService;
//	i: integer;
begin
	Srv := TProtoBufService.Create(Self);
	try
		Srv.IsImported := FInImportCounter > 0;
		Srv.ParseFromProto(Proto, iPos);
		FProtoBufServices.Add(Srv);
		Srv := nil;
	finally
		Srv.Free;
	end;
end;

procedure TProtoFile.SetFileName(const Value: string);
begin
	FFileName := Value;
	FName := ChangeFileExt(ExtractFileName(Value), '');
end;

{ TProtoBufMessageList }

function TProtoBufMessageList.FindByName(const MessageName: string): TProtoBufMessage;
var
	i: integer;
begin
	Result := nil;
	for i := 0 to Count - 1 do
		if Items[i].Name = MessageName then
		begin
			Result := Items[i];
			Break;
		end;
end;

{ TProtoBufEnumList }

function TProtoBufEnumList.FindByName(const EnumName: string): TProtoBufEnum;
var
	i: integer;
begin
	Result := nil;
	for i := 0 to Count - 1 do
		if Items[i].Name = EnumName then
		begin
			Result := Items[i];
			Break;
		end;
end;

{ TProtoBufRPC }

function TProtoBufRPC.GetArgementIsStream: Boolean;
begin
   Result := FArgument.IsStream;
end;

function TProtoBufRPC.GetArgument: TProtoBufMessage;
begin
   Result := FindMessage(FArgument);
end;

function TProtoBufRPC.GetResultIsStream: Boolean;
begin
   Result := FResult.IsStream;
end;

function TProtoBufRPC.GetResult: TProtoBufMessage;
begin
   Result := FindMessage(FResult);
end;

function TProtoBufRPC.ProtoFile: TProtoFile;
begin
   Result := (FRoot as TProtoFile)
end;

function TProtoBufRPC.FindMessage(const Val: TRPCVal): TProtoBufMessage;
begin
   Result := ProtoFile.ProtoBufMessages.FindByName(Val.Name)
end;

procedure TProtoBufRPC.ParseFromProto(const Proto: string; var iPos: integer);
var
	Buf: string;
begin
	inherited;
	Buf := ReadWordFromBuf(Proto, iPos, []);
	if buf <> 'rpc' then
	begin
		ReadAllTillChar(Proto, iPos, ['}']);
		SkipRequiredChar(Proto, iPos, '}');
		exit; // Stop reading if its not an RPC
	end;

	FName := ReadWordFromBuf(Proto, iPos, ['(']);

	// skip '(' character
	SkipRequiredChar(Proto, iPos, '(');

	// read Argument
	Buf := ReadWordFromBuf(Proto, iPos, [')']);
	if Buf = 'stream' then
	begin
   	FArgument.IsStream := True;
      Buf := ReadWordFromBuf(Proto, iPos, [')']);
	end;
	FArgument.Name := Buf;
   ReadAllTillChar(Proto, iPos, ['(']);
   SkipRequiredChar(Proto, iPos, '(');

	// read Result
   Buf := ReadWordFromBuf(Proto, iPos, [')']);
	if Buf = 'stream' then
	begin
   	FResult.IsStream := True;
      Buf := ReadWordFromBuf(Proto, iPos, [')']);
	end;
	FResult.Name := Buf;
   ReadAllTillChar(Proto, iPos, ['}']);
   SkipRequiredChar(Proto, iPos, '}');

	FComment := ReadCommentIfExists(Proto, iPos);
end;

end.
