unit UnitSelectMode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormSelectMode = class(TForm)
    ButtonYard: TButton;
    ButtonConfiguration: TButton;
    ButtonPossession: TButton;
    ButtonShunting: TButton;
    procedure ButtonYardClick(Sender: TObject);
    procedure ButtonConfigurationClick(Sender: TObject);
    procedure ButtonPossessionClick(Sender: TObject);
    procedure ButtonShuntingClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSelectMode: TFormSelectMode;

implementation

uses MMITypes, UnitExchange;

{$R *.dfm}

procedure TFormSelectMode.ButtonConfigurationClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsTrainConfig);

end;

procedure TFormSelectMode.ButtonPossessionClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsPossession);
end;

procedure TFormSelectMode.ButtonShuntingClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsShunting);

end;

procedure TFormSelectMode.ButtonYardClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsEnterYardMode);
end;

end.
