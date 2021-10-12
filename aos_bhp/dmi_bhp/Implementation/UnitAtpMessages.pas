unit UnitAtpMessages;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ColorButton2, Buttons;

type
  TFormAtpMessages = class(TForm)
    Memo: TMemo;
    BitBtnOK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure CloseClick(Sender: TObject);
  private
    { Private declarations }
    Nmode: byte;
  public
    mactive : Boolean;
    modal : Boolean;
    procedure AddMessageToMemo(text: string; mode: byte);
    procedure AddMessage(text: string; mode: byte);
    procedure AddModalMessage(text: string; mode: byte);
    procedure MsgTic();
    { Public declarations }
  end;

var
  FormAtpMessages: TFormAtpMessages;

implementation

uses UnitMMIFrame;

{$R *.DFM}

procedure TFormAtpMessages.MsgTic();
begin
  if mactive then
  begin
     if Nmode = 1 then
     begin
       if (Memo.lines.count > 0) then
       begin
           Memo.lines[Memo.lines.count-1] := Memo.lines[Memo.lines.count-1] + '..';
       end;
     end;
  end
  else
     Hide;
end;
procedure TFormAtpMessages.FormCreate(Sender: TObject);
begin

  Memo.WordWrap := true;
  Hide;
  mactive := false;
  modal := false;

  Parent := FormMMIFrame.PanelMMIFrame;

end;

procedure TFormAtpMessages.CloseClick(Sender: TObject);
begin
  Memo.Clear;
  mactive := false;

  if modal then
    ModalResult := mrOk
  else
    Hide;
end;
{*********************************************************
* Function:    FormMainArea.AddMessageToMemo
* Description: Add message to memo and beep.
*********************************************************}
procedure TFormAtpMessages.AddMessageToMemo(text: string; mode: byte);
begin

  if not (Nmode = 0) then
  begin
    if (Memo.lines.count > 0) then
      Memo.lines.delete(Memo.lines.count-1);
  end;

  Nmode := mode;
  Memo.Lines.Add(text);
  mactive := true;
  if (Nmode = 1) then
  begin
     Memo.Lines.Add('...');
  end;

 // Left := FormMMIFrame.Left + (FormMMIFrame.Width div 2) - (Width div 2) ;
 // Top := FormMMIFrame.Top + (FormMMIFrame.Height div 2) - (Height div 2);

  Beep;
  Beep;
  Beep;

end;
{*********************************************************
* Function:    FormMainArea.AddModalMessage
* Description: Add message to memo and show form on top
* and modal
*********************************************************}
procedure TFormAtpMessages.AddModalMessage(text: string; mode: byte);
begin

  AddMessageToMemo(text, mode);

  Hide; { Window already visible? This is to avoid: can not make a visible window modal.
        Caller will call ShowModal after this function }

  modal := true;

end;

{*********************************************************
* Function:    FormMainArea.AddMessage
* Description: Add message to memo and show form on top
* but not modal
*********************************************************}
procedure TFormAtpMessages.AddMessage(text: string; mode: byte);
begin

  AddMessageToMemo(text, mode);

  modal := false;

  Show;

  BitBtnOK.SetFocus;

end;

end.
