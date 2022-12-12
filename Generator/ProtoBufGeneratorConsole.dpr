program ProtoBufGeneratorConsole;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  uProtoBufGenerator,
  uProtoBufGenerator.Client in 'uProtoBufGenerator.Client.pas',
  uProtoBufGenerator.Connector.Base in 'uProtoBufGenerator.Connector.Base.pas',
  uProtoBufGenerator.Connector.Ultraware in 'uProtoBufGenerator.Connector.Ultraware.pas',
  uProtoBufGenerator.Helper in 'uProtoBufGenerator.Helper.pas',
  uCommandLineParams in 'uCommandLineParams.pas',
  uCommandLineGenerator in 'uCommandLineGenerator.pas';

begin
  ExecuteConsoleGenerator;
//  Readln;
end.

