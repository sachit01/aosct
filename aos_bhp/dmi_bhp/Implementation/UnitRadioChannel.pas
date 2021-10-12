unit UnitRadioChannel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormRadioChannel = class(TForm)
    BitBtnOk: TBitBtn;
    LableRadioChannel: TLabel;
    EditRadioChannel: TEdit;
    procedure BitBtnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRadioChannel: TFormRadioChannel;

implementation

uses UnitExchange, MMITypes, UnitDMIDataModule;

{$R *.dfm}

{*********************************************************
* BitBtnOkClick
* Description: Departure test confirmation when driver clicks the Ok-button
*********************************************************}
procedure TFormRadioChannel.BitBtnOkClick(Sender: TObject);
begin
    FormRadioChannel.Hide;
end;

procedure TFormRadioChannel.FormShow(Sender: TObject);
var
Changed : Boolean;
begin
   EditRadioChannel.Text := DataModuleDMI.GetRadioArea(Changed);
end;

end.
