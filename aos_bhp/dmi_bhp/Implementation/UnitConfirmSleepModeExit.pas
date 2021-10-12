unit UnitConfirmSleepModeExit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmExitSleepingMode = class(TForm)
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    LabelSleepingExit: TLabel;
    procedure BitBtnOkClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmExitSleepingMode: TFormConfirmExitSleepingMode;

implementation

uses UnitExchange, MMITypes, UnitMMIFrame;

{$R *.dfm}

{*********************************************************
* BitBtnOkClick
* Description: Starts the brake-test when driver clicks the Ok-button
*********************************************************}
procedure TFormConfirmExitSleepingMode.BitBtnCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormConfirmExitSleepingMode.BitBtnOkClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsTrainConfig);
  if FormMMIFrame.Sound then
    Beep;

    Hide;
end;

end.
