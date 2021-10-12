unit UnitNewTrainName;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Keyboard;

type
  TFormNewTrainName = class(TForm)
    BitBtnOk: TBitBtn;
    EditTrainName: TEdit;
    BitBtnCancel: TBitBtn;
    procedure BitBtnOkClick(Sender: TObject);
    procedure EditTrainNameChange(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure EditTrainNameEnter(Sender: TObject);
    procedure EditTrainNameExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNewTrainName: TFormNewTrainName;

implementation

uses UnitExchange, KeyboardUnit, UnitMMIFrame;

{$R *.dfm}

procedure TFormNewTrainName.BitBtnCancelClick(Sender: TObject);
begin
  OSDKeyboardWinControl.Hide;
  Hide;
end;

procedure TFormNewTrainName.BitBtnOkClick(Sender: TObject);
begin

  if (Length(EditTrainName.Text) > 0) then
  begin
    Exchange.SendNewTrainName(EditTrainName.Text);
      // Hide form
    Hide;
    OSDKeyboardWinControl.Hide;
  end;

end;

procedure TFormNewTrainName.EditTrainNameChange(Sender: TObject);
begin

  if (Length(EditTrainName.Text)>0) then
    BitBtnOk.Enabled := true
   else
    BitBtnOk.Enabled := false;
end;

procedure TFormNewTrainName.EditTrainNameEnter(Sender: TObject);
begin
  ClientWidth  := OSDKeyboardWinControl.Left + OSDKeyboardWinControl.Width;
  ClientHeight := OSDKeyboardWinControl.Top + OSDKeyboardWinControl.Height;

  OSDKeyboardWinControl.Show;

end;

procedure TFormNewTrainName.EditTrainNameExit(Sender: TObject);
begin
  ClientWidth := BitBtnCancel.Left + BitBtnCancel.Width + 10;
  ClientHeight := BitBtnCancel.Top + BitBtnCancel.Height + 10;

  OSDKeyboardWinControl.Hide;

end;

procedure TFormNewTrainName.FormHide(Sender: TObject);
begin
  OSDKeyboardWinControl.Hide;
end;

procedure TFormNewTrainName.FormShow(Sender: TObject);
begin

  BitBtnCancel.SetFocus;

  Left := 10 + FormMMIFrame.Left;
  Top := 10 + FormMMIFrame.Top;

  OSDKeyboardWinControl.Parent := self;
  OSDKeyboardWinControl.FormStyle := fsStayOnTop;
  OSDKeyboardWinControl.Left := 0;
    // Place the keyboard below the Cancel-button
  OSDKeyboardWinControl.Top := BitBtnCancel.Top + BitBtnCancel.Height + 5;
  OSDKeyboardWinControl.SetWinControl(EditTrainName, EditTrainName.OnClick);


end;

end.
