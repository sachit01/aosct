unit UnitConfirmLoadedStatus;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmLoadedStatus = class(TForm)
    Labeltrainload: TLabel;
    BitBtnOK: TBitBtn;
    BitBtnCancel: TBitBtn;
    procedure BitBtnOKClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmLoadedStatus: TFormConfirmLoadedStatus;

implementation

uses UnitExchange, MMITypes;

{$R *.dfm}

procedure TFormConfirmLoadedStatus.BitBtnOKClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsConfirmChangeOfTrainLoadedStatus);
end;

procedure TFormConfirmLoadedStatus.BitBtnCancelClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsCancelChangeOfTrainLoadedStatus);
end;

end.
