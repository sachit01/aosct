unit UnitConfirmAbortLastCarBrakeTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormConfirmAbortLastCarBrake = class(TForm)
    Label1: TLabel;
    BitBtnAbortOK: TBitBtn;
    BitBtnCancel: TBitBtn;
    procedure BitBtnAbortOKClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmAbortLastCarBrake: TFormConfirmAbortLastCarBrake;

implementation

uses UnitExchange,MMITypes;
{$R *.dfm}

procedure TFormConfirmAbortLastCarBrake.BitBtnAbortOKClick(Sender: TObject);
begin
      Exchange.SendMMIButton(bsConfirmAbortLastCarBrakePressureTest);
end;

procedure TFormConfirmAbortLastCarBrake.BitBtnCancelClick(Sender: TObject);
begin
      Exchange.SendMMIButton(bsCancelAbortLastCarBrakePressureTest);
end;

end.


