program ProtoBufGenerator;

uses
  Vcl.Forms,
  ufmMain in 'ufmMain.pas' {fmMain},
  uProtoBufParserClasses in 'uProtoBufParserClasses.pas',
  uProtoBufParserAbstractClasses in 'uProtoBufParserAbstractClasses.pas',
  uProtoBufGenerator in 'uProtoBufGenerator.pas',
  uProtoBufGenerator.Connector.Base in 'uProtoBufGenerator.Connector.Base.pas',
  uProtoBufGenerator.Client in 'uProtoBufGenerator.Client.pas',
  uProtoBufGenerator.Server in 'uProtoBufGenerator.Server.pas',
  uProtoBufGenerator.Service in 'uProtoBufGenerator.Service.pas',
  uProtoBufGenerator.Types in 'uProtoBufGenerator.Types.pas',
  uProtoBufGenerator.Helper in 'uProtoBufGenerator.Helper.pas',
  uProtoBufGenerator.Connector.Ultraware in 'uProtoBufGenerator.Connector.Ultraware.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
