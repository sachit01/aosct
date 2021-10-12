unit UnitChangeLogLevel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormChangeLogLevel = class(TForm)
    ComboBoxLogLevel: TComboBox;
    LabelLogLevel: TLabel;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormChangeLogLevel: TFormChangeLogLevel;

implementation

{$R *.dfm}

end.
