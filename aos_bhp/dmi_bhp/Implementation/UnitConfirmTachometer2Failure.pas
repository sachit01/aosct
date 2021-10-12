unit UnitConfirmTachometer2Failure;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmTachometer2Failure = class(TForm)
    LabelTachometer2Failure: TLabel;
    BitBtnOk: TBitBtn;
    procedure BitBtnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmTachometer2Failure: TFormConfirmTachometer2Failure;

implementation

uses UnitExchange, MMITypes;
{$R *.dfm}

procedure TFormConfirmTachometer2Failure.BitBtnOkClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsConfirmTachometer2Failure);
end;

end.
