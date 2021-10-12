unit UnitConfirmTrainIntegrity;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmTrainIntegrity = class(TForm)
    LabelConfirmTIMS: TLabel;
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
  FormConfirmTrainIntegrity: TFormConfirmTrainIntegrity;

implementation

uses UnitExchange, MMITypes;

{$R *.dfm}

procedure TFormConfirmTrainIntegrity.BitBtnCancelClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsCancelIntegrity);
end;

procedure TFormConfirmTrainIntegrity.BitBtnOKClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsConfirmIntegrity);
end;

end.
