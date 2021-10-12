unit UnitConfirmDepartureTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmDepartureTest = class(TForm)
    BitBtnOk: TBitBtn;
    LableConfirmDepTest: TLabel;
    procedure BitBtnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmDepartureTest: TFormConfirmDepartureTest;

implementation

uses UnitExchange, MMITypes;

{$R *.dfm}

{*********************************************************
* BitBtnOkClick
* Description: Departure test confirmation when driver clicks the Ok-button
*********************************************************}
procedure TFormConfirmDepartureTest.BitBtnOkClick(Sender: TObject);
begin
   Exchange.SendMMIButton(bsConfirmDeparture);
end;

end.
