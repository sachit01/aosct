unit UnitAbortLastCarBrakeTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormAbortLastCarBrakeTest = class(TForm)
    Label1: TLabel;
    BitBtnAbort: TBitBtn;
    procedure BitBtnAbortClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbortLastCarBrakeTest: TFormAbortLastCarBrakeTest;

implementation

uses UnitExchange,MMITypes;

{$R *.dfm}
procedure TFormAbortLastCarBrakeTest.BitBtnAbortClick(Sender: TObject);
begin
    Exchange.SendMMIButton(bsAbortLastCarBrakePressureTest);
end;

end.
