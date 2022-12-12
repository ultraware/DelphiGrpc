program GrpcUnitTest;

{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}
//{$IF CompilerVersion >= 21.0}   // Smaller EXE, must be first in the dpr...
//  {$WEAKLINKRTTI ON}
//  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
//{$IFEND}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}


//use MadExcept or Jcl to get linenumber of errors!
{$IFNDEF USE_JEDI_JCL} {$MESSAGE ERROR 'Must define "USE_JEDI_JCL" for location info of errors'} {$ENDIF}


uses
  FastMM4,
  FastMM4Messages,
  GrpcGeneratorTestRunner in 'GrpcGeneratorTestRunner.pas',
  GrpcTestHelper in 'GrpcTestHelper.pas',
  GrpcImportTester in 'GrpcImportTester.pas',
  uProtoBufParserAbstractClasses in '..\uProtoBufParserAbstractClasses.pas',
  uProtoBufParserClasses in '..\uProtoBufParserClasses.pas',
  GrpcExportTypesTester in 'GrpcExportTypesTester.pas',
  GrpcExportClientTester in 'GrpcExportClientTester.pas',
  uProtoBufGenerator in '..\uProtoBufGenerator.pas',
  uProtoBufGenerator.Types in '..\uProtoBufGenerator.Types.pas',
  uProtoBufGenerator.Client in '..\uProtoBufGenerator.Client.pas',
  uProtoBufGenerator.Service in '..\uProtoBufGenerator.Service.pas',
  uProtoBufGenerator.Server in '..\uProtoBufGenerator.Server.pas',
  GrpcExportServerTester in 'GrpcExportServerTester.pas';

{$R *.res}

begin
   TGrpcUnitTestRunner.StartUnitTest;
end.
