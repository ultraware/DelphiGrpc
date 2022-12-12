unit GrpcGeneratorTestRunner;

interface

uses UnitTestRunner;

type
   TGrpcUnitTestRunner = class(TUltraUnitTest)
   protected
      class procedure RegisterTests; override;
   end;

implementation

uses GrpcImportTester, GrpcExportTypesTester, GrpcExportClientTester, GrpcExportServerTester;

{ TGrpcUnitTestRunner }

class procedure TGrpcUnitTestRunner.RegisterTests;
begin
   RegisterTest(TGrpcImportTester.Suite);
   RegisterTest(TGrpcExportTypesTester.Suite);
   RegisterTest(TGrpcExportClientTester.Suite);
   RegisterTest(TGrpcExportServerTester.Suite);

//   inherited RegisterTests;
end;

end.
