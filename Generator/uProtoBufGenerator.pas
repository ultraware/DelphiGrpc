unit uProtoBufGenerator;

interface

uses System.Classes, uProtoBufParserClasses, uProtoBufParserAbstractClasses;

type
   TProtoBaseGeneratorOptions = class
   public
      class var UseCustomClient: Boolean;
      class var UseCustomServer: Boolean;
   end;

   TProtoBaseGenerator = class
   protected
      class procedure GenerateStartOfClass(var Text: TStringList; const aClassname: string);
   protected
      class function GenerateInterfaceUses(const aFile: TProtoFile): string; virtual; abstract;
      class function GenerateImplementationUses(const aFile: TProtoFile): string; virtual; abstract;
      class procedure GenerateInterface(var Text: TStringList; const aFile: TProtoFile); virtual; abstract;
      class procedure GenerateImplementation(var Text: TStringList; const aFile: TProtoFile); virtual; abstract;
   public
      class function GenerateUnitName(const aFile: TProtoFile): string; virtual; abstract;
      class procedure Generate(var Text: TStringList; const aFile: TProtoFile);
   end;
   TProtoBaseGeneratorClass = class of TProtoBaseGenerator;

   TProtoBaseServiceGenerator = class(TProtoBaseGenerator)
   private
      class var FService: TProtoBufService;
   strict protected
      class property CurrentService: TProtoBufService read FService;
   public
      class procedure Generate(var Text: TStringList; const aFile: TProtoFile; const aService: TProtoBufService); reintroduce;
   end;
   TProtoBaseServiceGeneratorClass = class of TProtoBaseServiceGenerator;

implementation

uses System.SysUtils;

{ TProtoBaseGenerator }

class procedure TProtoBaseGenerator.Generate(var Text: TStringList; const aFile: TProtoFile);

   procedure _AddUses(const aUses: string);
   begin
      if aUses = '' then
         Exit;
      Text.Add('uses '+ aUses+';'   );
      Text.Add('');
   end;
begin
   Text.Clear;
   Text.Add('unit '+GenerateUnitName(aFile)+';');
   {$IFNDEF DUnit}
   Text.Add('');
   Text.Add('// This is a generated unit! Do NOT edit!');
   {$ENDIF}
   Text.Add('');
   Text.Add('interface');
   Text.Add('');
   _AddUses(GenerateInterfaceUses(aFile));
   GenerateInterface(Text, aFile);
   Text.Add('implementation');
   Text.Add('');
   _AddUses(GenerateImplementationUses(aFile));
   GenerateImplementation(Text, aFile);
   Text.Add('end.');
end;

class procedure TProtoBaseGenerator.GenerateStartOfClass(var Text: TStringList; const aClassname: string);
begin
   Text.Add(Format(' { %s }',[aClassname]));
   Text.Add('');
end;

{ TProtoBaseServiceGenerator }

class procedure TProtoBaseServiceGenerator.Generate(var Text: TStringList; const aFile: TProtoFile; const aService: TProtoBufService);
begin
   try
      FService := aService;
      inherited Generate(Text, aFile);
   finally
      FService := nil;
   end;
end;

end.
