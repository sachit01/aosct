unit UnitConfirmBrakeTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmBrakeTest = class(TForm)
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    LabelStartBrakeTest: TLabel;
    procedure BitBtnOkClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmBrakeTest: TFormConfirmBrakeTest;

implementation

uses UnitExchange, MMITypes;

{$R *.dfm}

{*********************************************************
* BitBtnOkClick
* Description: Starts the brake-test when driver clicks the Ok-button
*********************************************************}
procedure TFormConfirmBrakeTest.BitBtnCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormConfirmBrakeTest.BitBtnOkClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsStartBrakeTest);
  Hide;
end;

end.
