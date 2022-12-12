unit uProtoBufGenerator.Types;

interface

uses System.Classes, uProtoBufParserClasses, uProtoBufGenerator;

type
   TProtoTypesGenerator = class(TProtoBaseGenerator)
   protected
      // Ook benaderbaar voor unitText
      class procedure GenerateEnumDeclaration(var Text: TStringList; const aEnum: TProtoBufEnum);
      class procedure GenerateTypeDeclaration(var Text: TStringList; const aMsg: TProtoBufMessage);
      class procedure GeneratePropertyDeclaration(var Text: TStringList; const aProp: TProtoBufProperty; const aIndex: Integer);
      class procedure GenerateSimpleTypeArrays(var Text: TStringList; const aFile: TProtoFile);

      class function GenerateInterfaceUses(const aFile: TProtoFile): string; override;
      class function GenerateImplementationUses(const aFile: TProtoFile): string; override;
      class procedure GenerateInterface(var Text: TStringList; const aFile: TProtoFile); override;
      class procedure GenerateImplementation(var Text: TStringList; const aFile: TProtoFile); override;
   public
      class function GenerateUnitName(const aPackageName: string): string; reintroduce; overload;
      class function GenerateUnitName(const aFile: TProtoFile): string; overload; override;
      class function GenerateImportedUnitNames(const aFile: TProtoFile): string;
   end;

implementation

uses System.SysUtils, System.StrUtils, uProtoBufGenerator.Helper;

{ TProtoTypesGenerator }

class procedure TProtoTypesGenerator.GenerateEnumDeclaration(var Text: TStringList; const aEnum: TProtoBufEnum);
var Val: TProtoBufEnumValue;
begin
   if aEnum.IsImported then
      Exit;
   Text.Add(Format('   %s = (',[aEnum.DelphiClassname]));
   for Val in aEnum do
      Text.Add(Format('      %s = %d%s',[Val.Name,Val.Value,IfThen(Val=aEnum.Last,');',',')]));
   Text.Add('');
end;

class procedure TProtoTypesGenerator.GenerateImplementation(var Text: TStringList; const aFile: TProtoFile);
begin
   // Nothing
end;

class function TProtoTypesGenerator.GenerateImplementationUses(const aFile: TProtoFile): string;
begin
   Result := ''; // No uses
end;

class procedure TProtoTypesGenerator.GenerateInterface(var Text: TStringList; const aFile: TProtoFile);
var enum: TProtoBufEnum;
    msg: TProtoBufMessage;
begin
   Text.Add('type');
   GenerateSimpleTypeArrays(Text, aFile);
   for enum in aFile.ProtoBufEnums do
      GenerateEnumDeclaration(Text, enum);
   for msg in aFile.ProtoBufMessages do
      GenerateTypeDeclaration(Text, msg);
end;

class function TProtoTypesGenerator.GenerateInterfaceUses(const aFile: TProtoFile): string;
begin
   Result := GenerateImportedUnitNames(aFile) +'Grijjy.ProtocolBuffers';
end;

class procedure TProtoTypesGenerator.GenerateTypeDeclaration(var Text: TStringList; const aMsg: TProtoBufMessage);
var Prop: TProtoBufProperty;
    Index: Integer;
begin
   if aMsg.IsImported then
      Exit;
   Text.Add(Format('   %s = record', [aMsg.DelphiClassname]));
   Index := 1;
   for Prop in aMsg do
   begin
      GeneratePropertyDeclaration(Text, Prop, Index);
      Inc(Index);
   end;
   Text.Add('   end;');
   Text.Add(Format('   %s =  Array of %s;',[aMsg.DelphiClassnameArray, aMsg.DelphiClassname]));
   Text.Add('');
end;

class procedure TProtoTypesGenerator.GeneratePropertyDeclaration(var Text: TStringList; const aProp: TProtoBufProperty; const aIndex: Integer);
begin
   Text.Add(Format('      [Serialize(%d)] %s: %s;',[aIndex, aProp.Name, aProp.DelphiType]));
end;

class procedure TProtoTypesGenerator.GenerateSimpleTypeArrays(var Text: TStringList; const aFile: TProtoFile);

   procedure _CheckType(const aDelphiType: string);
   var msg: TProtoBufMessage;
       prop: TProtoBufProperty;
       ArrayType: string;
   begin
      ArrayType := DelphiTypeToArray(aDelphiType);
      for msg in aFile.ProtoBufMessages do
      begin
         for prop in msg do
         begin
            if (prop.PropKind = ptRepeated) and SameText(ArrayType, prop.DelphiType) then
            begin
               Text.Add(Format('   %s = Array of %s;',[ArrayType, aDelphiType]));
               Exit;
            end;
         end;
      end;
   end;

begin
   _CheckType('Double');
   _CheckType('Single');
   _CheckType('Integer');
   _CheckType('Int64');
   _CheckType('Cardinal');
   _CheckType('UInt64');
   _CheckType('Int32');
   _CheckType('Boolean');
   _CheckType('string');
   _CheckType('TBytes');
   _CheckType('TTime');
   _CheckType('TDatetime');
   _CheckType('TDate');
end;

class function TProtoTypesGenerator.GenerateUnitName(const aFile: TProtoFile): string;
begin
   Result := GenerateUnitName(aFile.Name);
end;

class function TProtoTypesGenerator.GenerateUnitName(const aPackageName: string): string;
begin
   Result := aPackageName+'.Proto';
end;

class function TProtoTypesGenerator.GenerateImportedUnitNames(const aFile: TProtoFile): string;
var Import: string;
begin
   Result := '';
   for Import in aFile.Imports do
      Result := Result + GenerateUnitName(Import) + ', ';
end;


end.
