unit UnitConfirmMAAccept;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, MMITypes;

type
  TFormConfirmMAAccept = class(TForm)
    BitBtnOK: TBitBtn;
    LabeledEditMAConfirm: TLabeledEdit;
    procedure BitBtnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    thisButton : TButtonStatus;
  end;

var
  FormConfirmMAAccept: TFormConfirmMAAccept;

implementation

uses UnitExchange;



{$R *.dfm}

procedure TFormConfirmMAAccept.BitBtnOKClick(Sender: TObject);
begin
  Exchange.SendMMIButton(thisButton);
end;

end.
