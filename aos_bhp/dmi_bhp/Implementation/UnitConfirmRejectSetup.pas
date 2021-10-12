unit UnitConfirmRejectSetup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmRejectSetup = class(TForm)
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
  FormConfirmRejectSetup: TFormConfirmRejectSetup;

implementation

uses UnitExchange, MMITypes;

{$R *.dfm}

{*********************************************************
* BitBtnOkClick
* Description: Starts the brake-test when driver clicks the Ok-button
*********************************************************}
procedure TFormConfirmRejectSetup.BitBtnCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormConfirmRejectSetup.BitBtnOkClick(Sender: TObject);
begin
   Exchange.SendConfirmation(False);
  Hide;
end;

end.
