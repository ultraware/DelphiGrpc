unit dStyle;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls;

type
  TdmStyle = class(TDataModule)
    StyleBook1: TStyleBook;
  private
  public
  end;

var
  dmStyle: TdmStyle;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
