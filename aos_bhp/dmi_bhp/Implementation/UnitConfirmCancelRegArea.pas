unit UnitConfirmCancelRegArea;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmCancelRegArea = class(TForm)
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    LabelConfirmCancelRegArea: TLabel;
    procedure BitBtnOkClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmCancelRegArea: TFormConfirmCancelRegArea;

implementation

uses UnitExchange, MMITypes;

{$R *.dfm}

{*********************************************************
* BitBtnOkClick
* Description: Starts the brake-test when driver clicks the Ok-button
*********************************************************}
procedure TFormConfirmCancelRegArea.BitBtnCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormConfirmCancelRegArea.BitBtnOkClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsCancelRegArea);
  Hide;
end;

end.
