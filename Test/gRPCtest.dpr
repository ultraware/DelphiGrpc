program gRPCtest;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

//use MadExcept or Jcl to get linenumber of errors!
{$IFNDEF USE_JEDI_JCL} {$MESSAGE ERROR 'Must define "USE_JEDI_JCL" for location info of errors'} {$ENDIF}

uses
  DUnitTestRunner,
  Test.gRPC in 'Test.gRPC.pas' {/  JclDebug in 'D: Delphi2\DelphiXE10\Componenten\Jedi\jcl\source\windows\JclDebug.pas';},
  TestService.impl in '..\SimpleDemo\delphi\TestService.impl.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

