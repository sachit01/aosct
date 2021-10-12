unit UnitConfirmDopplerRadarFailure;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmDopplerRadarFailure = class(TForm)
    LabelDopplerRadarFailure: TLabel;
    BitBtnOk: TBitBtn;
    procedure BitBtnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmDopplerRadarFailure: TFormConfirmDopplerRadarFailure;

implementation

uses UnitExchange, MMITypes;

{$R *.dfm}

procedure TFormConfirmDopplerRadarFailure.BitBtnOkClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsConfirmDopplerFailure);
end;

end.
