unit UnitConfirmFreeRollingClear;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmFreeRollingClear = class(TForm)
    BitBtnOk: TBitBtn;
    LableConfirmFreeRollingClear: TLabel;
    procedure BitBtnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmFreeRollingClear: TFormConfirmFreeRollingClear;

implementation

uses UnitExchange, MMITypes;

{$R *.dfm}

{*********************************************************
* BitBtnOkClick
* Description: Free Rolling Clear confirmation when driver clicks the Ok-button
*********************************************************}
procedure TFormConfirmFreeRollingClear.BitBtnOkClick(Sender: TObject);
begin
   Exchange.SendMMIButton(bsConfirmFreeRollingCleared);
end;

end.
