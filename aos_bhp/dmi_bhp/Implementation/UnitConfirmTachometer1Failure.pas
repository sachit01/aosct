unit UnitConfirmTachometer1Failure;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmTachometer1Failure = class(TForm)
    BitBtnOk: TBitBtn;
    LabelTachometer1Failure: TLabel;
    procedure BitBtnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmTachometer1Failure: TFormConfirmTachometer1Failure;

implementation

{$R *.dfm}

uses UnitExchange, MMITypes;

procedure TFormConfirmTachometer1Failure.BitBtnOkClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsConfirmTachometer1Failure);
end;

end.
