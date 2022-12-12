unit uProtoBufGenerator.Connector.Base;

interface

uses uProtoBufParserClasses, uProtoBufGenerator;

type
   TProtoBufGenerator = class
   protected
      class procedure DoGenerateForClass(const aProtoFile: TProtoFile; const aOutputDir: string; const aGenerateClass: TProtoBaseGeneratorClass);
   protected
      class procedure DoGenerate(const aProtoFile: TProtoFile; const aOutputDir: string); virtual; abstract;
	public
      class function CreateAndLoadProtoFile(const aInputFile: string): TProtoFile;
		class procedure Generate(const aInputFile, aOutputDir: string);
	end;
   TProtoBufGeneratorClass = class of TProtoBufGenerator;

var GlobalGeneratorClass: TProtoBufGeneratorClass;

implementation

uses System.Classes, System.SysUtils;

{ TProtoBufGenerator }

class function TProtoBufGenerator.CreateAndLoadProtoFile(const aInputFile: string): TProtoFile;
var Input: TStringList;
	 iPos: integer;
begin
	Input := TStringList.Create;
	try
		Input.LoadFromFile(aInputFile);
		Result := TProtoFile.Create(nil);
      Result.FileName := aInputFile;
      iPos := 1;
      Result.ParseFromProto(Input.Text, iPos);
	finally
		Input.Free;
	end;
end;

class procedure TProtoBufGenerator.DoGenerateForClass(const aProtoFile: TProtoFile; const aOutputDir: string; const aGenerateClass: TProtoBaseGeneratorClass);
var Output: TStringList;
   procedure _Save;
   begin
      Output.SaveToFile(aOutputDir+aGenerateClass.GenerateUnitName(aProtoFile)+'.pas');
   end;

var Service: TProtoBufService;
begin
   Output := TStringList.Create;
   try
      if aGenerateClass.InheritsFrom(TProtoBaseServiceGenerator) then
      begin
         for Service in aProtoFile.ProtoBufServices do
         begin
            if Service.IsImported then
               Continue;
            Output.Clear;
            TProtoBaseServiceGeneratorClass(aGenerateClass).Generate(Output, aProtoFile, Service);
            _Save;
         end;
      end
      else
      begin
         aGenerateClass.Generate(Output, aProtoFile);
         _Save;
      end;
   finally
      Output.Free;
   end;
end;

class procedure TProtoBufGenerator.Generate(const aInputFile, aOutputDir: string);
var ProtoFile: TProtoFile;
begin
   ProtoFile := CreateAndLoadProtoFile(aInputFile);
   try
      DoGenerate(ProtoFile, IncludeTrailingPathDelimiter(aOutputDir));
   finally
      ProtoFile.Free;
   end;
end;

end.
