unit uProtoBufGenerator.Connector.Ultraware;

interface

uses uProtoBufParserClasses, uProtoBufGenerator.Connector.Base;

type
   TProtoBufGeneratorUltraware = class(TProtoBufGenerator)
   protected
      class procedure DoGenerate(const aProtoFile: TProtoFile; const aOutputDir: string); override;
   end;

implementation

uses uProtoBufGenerator.Types, uProtoBufGenerator.Client, uProtoBufGenerator.Server;

{ TProtoBufGeneratorUltraware }

class procedure TProtoBufGeneratorUltraware.DoGenerate(const aProtoFile: TProtoFile; const aOutputDir: string);
begin
   DoGenerateForClass(aProtoFile, aOutputDir, TProtoTypesGenerator);
   DoGenerateForClass(aProtoFile, aOutputDir, TProtoClientGenerator);
   DoGenerateForClass(aProtoFile, aOutputDir, TProtoServerGenerator);
end;

initialization
   GlobalGeneratorClass := TProtoBufGeneratorUltraware;

end.
