
(*****************************************************************
           © COPYRIGHT Bombardier Transportation, SWEDEN 2011.
           ===================================================

    The copyright to the computer program herein is the
    property of Adtranz AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the
    written permission from Adtranz AB, or in accordance
    with the terms and conditions stipulated in the
    agreement/contract under which the program has been
    supplied.


    NAME :  KeyboardUnit.pas

    PROJECT :  Esmeralda, InterFlow TrainBorne

    Ver    Author           Date    Reason
    ---    ------           ------  ------
    1.0.0  Daniel Persson   980126  First version
    2.3.5  Bo Hermansson    110302  Delphi XE
                                    FormShow: Align to lower part of client
           Bo Hermansson    110331  Redesigned for LK
                                    Renamed class to TOSDKeyboard
                                    to avoid conflict with built-in class TKeyboard
           Bo Hermansson    110905  Renamed class to TOSDKeyboardWinControl
                                    Now compatible not only with TEdit but all
                                    classes inheriting TWinControl.

    DESCRIPTION :  Unit that display a keyboard for the user

    INTERFACE :
*********************************************************)

unit KeyboardUnitNumeric;

interface

{****************************************************************************
* UNIT USES                                                                 *
****************************************************************************}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Stdctrls, modbevel, ColorButton2, ExtCtrls;

{****************************************************************************
* UNIT TYPE DECLARATIONS                                                    *
****************************************************************************}
type
  TOSDKeyboardNumericWinControl = class(TForm)
    CButton1: TColorButton;
    CButton2: TColorButton;
    CButton3: TColorButton;
    CButton4: TColorButton;
    CButton5: TColorButton;
    CButton6: TColorButton;
    CButton7: TColorButton;
    CButton8: TColorButton;
    CButton9: TColorButton;
    CButton0: TColorButton;
    CButtonBSP: TColorButton;
    CButtonDEL: TColorButton;
    CButtonENTER: TColorButton;
    modBevel1: TmodBevel;
    Procedure FormCreate(Sender: TObject);
    Procedure CButtonClick(Sender: TObject);
    Procedure CButtonSpecialClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  Protected
  { Protected declarations }
  public
    { Public declarations }
    Procedure SetWinControl( AssociatedControl : TWinControl;Const EnterF) ;
    procedure SendKey(H: Hwnd; Key: Word);
    Procedure ScreenChange;

  Published
  { Published declarations}
  end;

{****************************************************************************
* UNIT CONST DECLARATIONS                                                   *
****************************************************************************}

{****************************************************************************
* UNIT VAR DECLARATIONS                                                     *
****************************************************************************}
var
  OSDKeyboardNumericWinControl: TOSDKeyboardNumericWinControl;
  WinControlToUse : TWinControl ;
  EnterFunction : TNotifyEvent ;

implementation
{$R *.DFM}

{****************************************************************************
* USES                                                                      *
****************************************************************************}
Uses
 UnitMainArea , UnitManualConfiguration, MMITypes, MMIStd, UnitMMIFrame;

{****************************************************************************
* CONST DECLARATIONS                                                        *
****************************************************************************}
const
  KeyboardNumLogErrorFormCreate = 1;
  KeyboardNumLogErrorCButtonClick = 2;
  KeyboardNumLogErrorCButtonSpecialClick = 3;
  KeyboardNumLogErrorSetWinControl= 4;
{****************************************************************************
* TYPE DECLARATIONS                                                         *
****************************************************************************}

{****************************************************************************
* VAR DECLARATIONS                                                          *
****************************************************************************}

{****************************************************************************
* FORWARD DECLARATIONS                                                      *
****************************************************************************}

{****************************************************************************
* FUNCTIONS AND PROCEDURES                                                  *
****************************************************************************}
procedure TOSDKeyboardNumericWinControl.ScreenChange;
var
  row,col:Integer;
procedure setButton(but:TColorButton);
begin
  but.Left:=col;
  but.Top:=row;
  but.Width:=round(but.Width*GetXfactor);
  but.Height:=round(but.Height*GetYfactor);
  but.BevelSize:=round(but.BevelSize*GetXfactor);
  but.Font.Size:=round(but.Font.Size*GetXfactor);
end;
procedure setNextButton(but:TColorButton);
begin
  col:=col+round(46*GetXfactor);
  setButton(but);
end;
begin {ScreenChange}
  Left := round(62*GetXfactor);
  Top := round(277*GetYfactor);
  Width := round(198*GetXfactor);
  Height := round(234*GetYfactor);

  // First row
  col := round(8*GetXfactor);
  row := round(7*GetYfactor);
  setButton(CButton7);
  setNextButton(CButton8);
  setNextButton(CButton9);
  setNextButton(CButtonBSP);


  // Second row
  col := round(8*GetXfactor);
  row := row+round(48*GetYfactor);
  setButton(CButton4);
  setNextButton(CButton5);
  setNextButton(CButton6);
  setNextButton(CButtonENTER);

  // Third row
  col := round(8*GetXfactor);
  row := row+round(48*GetYfactor);
  setButton(CButton1);
  setNextButton(CButton2);
  setNextButton(CButton3);

  // Fourth row
  col := round(8*GetXfactor);
  row := row+round(48*GetYfactor);
  setButton(CButton0);
  setNextButton(CButtonDEL);

  // Make sure the 'background'-color of the picture is the same as the button-color
  CButtonEnter.Picture.Bitmap.Canvas.Brush.Color := clBtnFace;
  CButtonEnter.Picture.Bitmap.Canvas.FloodFill(1,1,clBlack, fsBorder);

  CButtonBSP.Picture.Bitmap.Canvas.Brush.Color := clBtnFace;
  CButtonBSP.Picture.Bitmap.Canvas.FloodFill(1,1,clBlack, fsBorder);

end;
{*********************************************************
* Function:    TKeyboard.FormCreate
* Description: The Keyboard is positioned on the screen
               and the language specific text for
               the DEL button is assigned.
*********************************************************}
procedure TOSDKeyboardNumericWinControl.FormCreate(Sender: TObject);
begin
  Try
    Left := 62 ;
    Top := 277 ;
    CButtonDEL.Caption:=InterfaceString(ifDelBtn) ;

    ScreenChange;

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('NUMKEYB',MS_SW_ERROR,MS_NUMKEYB,KeyboardNumLogErrorFormCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in FormCreate:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate ;
    end;
 End;
end;

procedure TOSDKeyboardNumericWinControl.FormShow(Sender: TObject);
begin
  Top   := FormMMIFrame.ClientHeight - Height + round(35 * GetYFactor);
end;

{*********************************************************
* Function:    TKeyboard.CButtonClick
* Description: Normal button that only delivers a char has
               been pressed.
*********************************************************}
procedure TOSDKeyboardNumericWinControl.CButtonClick(Sender: TObject);
var
  ButtonChar : Char;
begin
  Try
    if WinControlToUse <> Nil then
    begin
      with sender as TColorButton do
          ButtonChar := Caption[1] ;

      if FormMMIFrame.Sound then beep;
      WinControlToUse.SetFocus;
      WinControlToUse.Perform(WM_CHAR, Ord(ButtonChar), 0); {Send char to control  }

    end;
  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('NUMKEYB',MS_SW_ERROR,MS_NUMKEYB,KeyboardNumLogErrorCButtonClick);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in CButtonClick:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
End;

{*********************************************************
* Function:    SendKey
* Description: Sends a sequence of WM_KEYDOWN, WM_CHAR, WM_UP
*              Seems necessary when performing e.g. a BackSpace
**********************************************************}
procedure TOSDKeyboardNumericWinControl.SendKey(H: Hwnd; Key: Word);
var
  vKey, ScanCode: Word;
  lParam  : Longword;
  ConvKey : Longint;
begin
  ConvKey := OemKeyScan(Key);
  ScanCode := ConvKey and $000000FF or $FF00;
  vKey := Key;
  lParam := LongInt(ScanCode) shl 16 or 1;
  SendMessage(H, WM_KEYDOWN, vKey, lParam);
  SendMessage(H, WM_CHAR, vKey, lParam);
  lParam := lParam or $C0000000;
  SendMessage(H, WM_KEYUP, vKey, lParam);
end;
{*********************************************************
* Function:    CButtonSpecialClick
* Description: A button that affects the edit boxes text
               has been pushed.
               The affected button is one of:
               DEL,BKspace or enter.
          Case bkspace last char is deleted
          Case DEL if user has marked text in the box
               that text is removed. If cursor is
               positioned in the text but no text
               is marked the, next char after the cursor
               is removed.
          Case Enter the function that is assigned
               to the button is executed.
*********************************************************}
procedure TOSDKeyboardNumericWinControl.CButtonSpecialClick(Sender: TObject);

begin
  try
   if WinControlToUse <> Nil then
   begin

      if FormMMIFrame.Sound then Beep ;

      if (sender = CButtonBsp) then
      begin
        WinControlToUse.SetFocus;
        SendKey (WinControlToUse.Handle,VK_BACK);
      end;


      if sender = CButtonDel then
      begin
        WinControlToUse.SetFocus;
        WinControlToUse.Perform(WM_KEYDOWN, VK_DELETE, 0);
      end;

      if sender = CButtonEnter then
      begin
         if assigned(EnterFunction) then EnterFunction(Self) ;
      end;

  end;
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('NUMKEYB',MS_SW_ERROR,MS_NUMKEYB,KeyboardNumLogErrorCButtonSpecialClick);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in CButtonSpecialClick:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{*********************************************************
* Function:    SetWinControl
* Description: Assignes the keyboard to a control that will receive the keystrokes.
               The enter function is also assigned to a
               function to execute
*********************************************************}
procedure TOSDKeyboardNumericWinControl.SetWinControl( AssociatedControl : TWinControl;Const EnterF) ;
begin
  try
    WinControlToUse:=AssociatedControl;
    EnterFunction:=TNotifyEvent(EnterF) ;

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('NUMKEYB',MS_SW_ERROR,MS_NUMKEYB,KeyboardNumLogErrorSetWinControl);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in SetWinControl:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{****************************************************************************
* INITIALIZATION PART                                                       *
****************************************************************************}
initialization

{****************************************************************************
* FINALIZATION PART                                                         *
****************************************************************************}
finalization

{****************************************************************************
* EXPORTS DECLARATIONS                                                     *
****************************************************************************}

{****************************************************************************
* RESOURCE STRING DECLARATIONS                                              *
****************************************************************************}
end.
