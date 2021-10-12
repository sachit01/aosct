unit UnitSelectArea;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, MMITypes;

type
  TFormSelectArea = class(TForm)
    ComboBoxArea: TComboBox;
    BitBtnOk: TBitBtn;
    procedure ComboBoxAreaChange(Sender: TObject);
    procedure BitBtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Area : Array[1..MAX_AREA_ID_COUNT] Of Byte;
    AreaCount : Byte;
  public
    { Public declarations }
    procedure ClearAreas();
    procedure AddArea(Id : Byte);
  end;

var
  FormSelectArea: TFormSelectArea;

implementation

uses UnitExchange, UnitMMIFrame;

{$R *.dfm}

procedure TFormSelectArea.BitBtnOkClick(Sender: TObject);
var
  SelectedIndex : Integer;
  SelectedId    : Byte;
begin

  SelectedIndex := ComboBoxArea.ItemIndex;

  if SelectedIndex >= 0 then
  begin

    // SelectedIndex 0 for the first item in the array
    SelectedId := Area[SelectedIndex + 1];
    Exchange.SendRegistrationArea(SelectedId);
    // disable Ok button for next time
    BitBtnOk.Enabled := false;
  end;

    // Hide form
  Hide;

end;

procedure TFormSelectArea.ComboBoxAreaChange(Sender: TObject);
begin
  if ComboBoxArea.ItemIndex >= 0
  then
  begin
    BitBtnOk.Enabled := true;
  end
  else
  begin
    BitBtnOk.Enabled := false;
  end;
end;

procedure TFormSelectArea.FormCreate(Sender: TObject);
begin
  AreaCount := 0;
end;

procedure TFormSelectArea.ClearAreas();
begin
  AreaCount := 0;
  ComboBoxArea.Clear;
end;

procedure TFormSelectArea.AddArea(Id : Byte);
begin
  Inc(AreaCount);
  Area[AreaCount] := Id;
  ComboBoxArea.Items.Add(FormMMIFrame.AreaIdToString(Id));
end;

end.
