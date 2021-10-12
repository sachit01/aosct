unit UnitConfirmATPMode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, MMITypes;

type
  TFormConfirmNewATPMode = class(TForm)
    BitBtnOK: TBitBtn;
    LabeledEditATPMode: TLabeledEdit;
    procedure BitBtnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    thisButton : TButtonStatus;
  end;

var
  FormConfirmNewATPMode: TFormConfirmNewATPMode;

implementation

uses UnitExchange;



{$R *.dfm}

procedure TFormConfirmNewATPMode.BitBtnOKClick(Sender: TObject);
begin
  Exchange.SendMMIButton(thisButton);
end;

end.
