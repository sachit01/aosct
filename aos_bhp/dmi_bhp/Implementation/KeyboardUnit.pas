
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

unit KeyboardUnit;

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

  EnumNumberRow = (EUndefined, ENumbers ,ESymbols);
  EnumAlphabets = (EKeysUndefined, ECapitalCase, ELowerCase);

  TOSDKeyboardWinControl = class(TForm)
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
    CButtonSPACE: TColorButton;
    CButtonP: TColorButton;
    CButtonL: TColorButton;
    CButtonM: TColorButton;
    CButtonK: TColorButton;
    CButtonO: TColorButton;
    CButtonI: TColorButton;
    CButtonJ: TColorButton;
    CButtonN: TColorButton;
    CButtonB: TColorButton;
    CButtonH: TColorButton;
    CButtonU: TColorButton;
    CButtonY: TColorButton;
    CButtonG: TColorButton;
    CButtonV: TColorButton;
    CButtonC: TColorButton;
    CButtonF: TColorButton;
    CButtonT: TColorButton;
    CButtonR: TColorButton;
    CButtonE: TColorButton;
    CButtonD: TColorButton;
    CButtonX: TColorButton;
    CButtonS: TColorButton;
    CButtonW: TColorButton;
    CButtonQ: TColorButton;
    CButtonA: TColorButton;
    CButtonZ: TColorButton;
    CButtonShift: TColorButton;
    CButtonNumberShift: TColorButton;
    Procedure FormCreate(Sender: TObject);
    Procedure CButtonClick(Sender: TObject);
    Procedure CButtonSpecialClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CButtonNumberShiftClick(Sender: TObject);
    procedure CButtonShiftClick(Sender: TObject);

  private
    { Private declarations }
    ActiveEnumNumberRow : EnumNumberRow;
    ActiveEnumAlphabets : EnumAlphabets;
    Procedure setNumbers;
    Procedure setSymbols;
    Procedure setCapitalCase;
    Procedure setLowerCase;

  Protected
  { Protected declarations }
  public
    { Public declarations }
    Procedure SetWinControl( AssociatedControl : TWinControl;Const EnterF) ;
    procedure SendKey(H: Hwnd; Key: Word);
    Procedure ScreenChange;
    Procedure UpdateCaptions;

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
  OSDKeyboardWinControl: TOSDKeyboardWinControl;
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
  KeyboardLogErrorFormCreate = 1;
  KeyboardLogErrorCButtonClick = 2;
  KeyboardLogErrorCButtonSpecialClick = 3;
  KeyboardLogErrorSetWinControl= 4;

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
procedure TOSDKeyboardWinControl.ScreenChange;
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

procedure setNexttoWiderButton(but:TColorbutton);
begin
   col:=col+round(60*GetXfactor);
   setButton(but)
end;

begin {ScreenChange}

  Left := round(62*GetXfactor);
  Top := round(200*GetYfactor);
  Width := round(594*GetXfactor);
  Height := round(234*GetYfactor);

  // First row
  col := round(8 * GetXfactor);
  row := round(7 * GetYfactor);
  setButton(CButton1);
  setNextButton(CButton2);
  setNextButton(CButton3);
  setNextButton(CButton4);
  setNextButton(CButton5);
  setNextButton(CButton6);
  setNextButton(CButton7);
  setNextButton(CButton8);
  setNextButton(CButton9);
  setNextButton(CButton0);
  setNextButton(CButtonBSP);

  //Second Row
  col := round(16*GetXfactor);
  row := row+round(48*GetYfactor);
  setButton(CButtonQ);
  setNextButton(CButtonW);
  setNextButton(CButtonE);
  setNextButton(CButtonR);
  setNextButton(CButtonT);
  setNextButton(CButtonY);
  setNextButton(CButtonU);
  setNextButton(CButtonI);
  setNextButton(CButtonO);
  setNextButton(CButtonP);
  setNextButton(CButtonDEL);


  //Third Row
  col := round(20*GetXfactor);
  row := row+round(48*GetYfactor);
  setButton(CButtonNumberShift);
  setNexttoWiderButton(CButtonA);
  setNextButton(CButtonS);
  setNextButton(CButtonD);
  setNextButton(CButtonF);
  setNextButton(CButtonG);
  setNextButton(CButtonH);
  setNextButton(CButtonJ);
  setNextButton(CButtonK);
  setNextButton(CButtonL);
  setNextButton(CButtonENTER);


  //Fourth Row
  col := round(24*GetXfactor);
  row := row+round(48*GetYfactor);
  setButton(CButtonShift);
  setNexttoWiderButton(CButtonZ);
  setNextButton(CButtonX);
  setNextButton(CButtonC);
  setNextButton(CButtonV);
  setNextButton(CButtonB);
  setNextButton(CButtonN);
  setNextButton(CButtonM);
  setNextButton(CButtonSPACE);

  UpdateCaptions;

  // Make sure the 'background'-color of the picture is the same as the button-color
  CButtonEnter.Picture.Bitmap.Canvas.Brush.Color := clBtnFace;
  CButtonEnter.Picture.Bitmap.Canvas.FloodFill(1,1,clBlack, fsBorder);

  CButtonBSP.Picture.Bitmap.Canvas.Brush.Color := clBtnFace;
  CButtonBSP.Picture.Bitmap.Canvas.FloodFill(1,1,clBlack, fsBorder);

end;

Procedure TOSDKeyboardWincontrol.setNumbers;
  begin
    CButton1.Caption := '1';
    CButton2.Caption := '2';
    CButton3.Caption := '3';
    CButton4.Caption := '4';
    CButton5.Caption := '5';
    CButton6.Caption := '6';
    CButton7.Caption := '7';
    CButton8.Caption := '8';
    CButton9.Caption := '9';
    CButton0.Caption := '0';
end;

Procedure TOSDKeyboardWincontrol.setSymbols;
begin
    CButton1.Caption := '!';
    CButton2.Caption := '"';
    CButton3.Caption := '#';
    CButton4.Caption := '@';
    CButton5.Caption := '%';
    CButton6.Caption := '$';
    CButton7.Caption := '*';
    CButton8.Caption := '\';
    CButton9.Caption := '=';
    CButton0.Caption := '-';
end;

Procedure TOSDKeyboardWincontrol.setCapitalCase;
begin  // Second row
  CButtonQ.Caption:='Q';
  CButtonW.Caption:='W';
  CButtonE.Caption:='E';
  CButtonR.Caption:='R';
  CButtonT.Caption:='T';
  CButtonY.Caption:='Y';
  CButtonU.Caption:='U';
  CButtonI.Caption:='I';
  CButtonO.Caption:='O';
  CButtonP.Caption:='P';


  // Third row
  CButtonA.Caption:='A';
  CButtonS.Caption:='S';
  CButtonD.Caption:='D';
  CButtonF.Caption:='F';
  CButtonG.Caption:='G';
  CButtonH.Caption:='H';
  CButtonJ.Caption:='J';
  CButtonK.Caption:='K';
  CButtonL.Caption:='L';

  // Fourth row
  CButtonZ.Caption:='Z';
  CButtonX.Caption:='X';
  CButtonC.Caption:='C';
  CButtonV.Caption:='V';
  CButtonB.Caption:='B';
  CButtonN.Caption:='N';
  CButtonM.Caption:='M';

end;

Procedure TOSDKeyboardWincontrol.setLowerCase;
begin  // Second row
  CButtonQ.Caption:='q';
  CButtonW.Caption:='w';
  CButtonE.Caption:='e';
  CButtonR.Caption:='r';
  CButtonT.Caption:='t';
  CButtonY.Caption:='y';
  CButtonU.Caption:='u';
  CButtonI.Caption:='i';
  CButtonO.Caption:='o';
  CButtonP.Caption:='p';

  // Third row
  CButtonA.Caption:='a';
  CButtonS.Caption:='s';
  CButtonD.Caption:='d';
  CButtonF.Caption:='f';
  CButtonG.Caption:='g';
  CButtonH.Caption:='h';
  CButtonJ.Caption:='j';
  CButtonK.Caption:='k';
  CButtonL.Caption:='l';

  // Fourth row
  CButtonZ.Caption:='z';
  CButtonX.Caption:='x';
  CButtonC.Caption:='c';
  CButtonV.Caption:='v';
  CButtonB.Caption:='b';
  CButtonN.Caption:='n';
  CButtonM.Caption:='m';
end;


procedure TOSDKeyboardWinControl.UpdateCaptions;
begin
  if (ActiveEnumNumberRow = ENumbers) then
  begin
    setNumbers;
    CButtonNumberShift.caption:='{!@';
  end;

  if (ActiveEnumNumberRow = ESymbols) then
  begin
  setSymbols;
  CButtonNumberShift.caption:='123';
  end;

  if (ActiveEnumAlphabets = ECapitalCase) then
  begin
  setCapitalCase;
  CButtonShift.Caption:='abc';
  end;

  if (ActiveEnumAlphabets = ELowerCase) then
  begin
  setLowerCase;
  CButtonShift.Caption:='ABC';
  end;
end;


{*********************************************************
* Function:    TKeyboard.FormCreate
* Description: The Keyboard is positioned on the screen
               and the language specific text for
               the DEL button is assigned.
*********************************************************}
procedure TOSDKeyboardWinControl.FormCreate(Sender: TObject);
begin
  Try
    Left := 62 ;
    Top := 200 ;
    CButtonDEL.Caption:=InterfaceString(ifDelBtn) ;
    ActiveEnumNumberRow := ENumbers;
    ActiveEnumAlphabets := ECapitalCase;
    ScreenChange;
  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('KEYB',MS_SW_ERROR,MS_KEYB,KeyboardLogErrorFormCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in FormCreate:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate ;
    end;
 End;
end;

procedure TOSDKeyboardWinControl.FormShow(Sender: TObject);
begin
end;

{*********************************************************
* Function:    TKeyboard.CButtonClick
* Description: Normal button that only delivers a char has
               been pressed.
*********************************************************}
procedure TOSDKeyboardWinControl.CButtonClick(Sender: TObject);
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
      FormMMIFrame.WriteMMIStatus('KEYB',MS_SW_ERROR,MS_KEYB,KeyboardLogErrorCButtonClick);
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
procedure TOSDKeyboardWinControl.SendKey(H: Hwnd; Key: Word);
var
  vKey, ScanCode: Word;
  ConvKey : Longint;
  lParam  : Longword;
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
procedure TOSDKeyboardWinControl.CButtonNumberShiftClick(Sender: TObject);
begin
if (ActiveEnumNumberRow = ENumbers) then
begin
  ActiveEnumNumberRow := ESymbols;
  UpdateCaptions;
end
else
  ActiveEnumNumberRow := ENumbers;
  UpdateCaptions;
end;

procedure TOSDKeyboardWinControl.CButtonShiftClick(Sender: TObject);
begin
if (ActiveEnumAlphabets = ECapitalCase) then
begin
 ActiveEnumAlphabets := ELowerCase;
 UpdateCaptions;
end
else
 ActiveEnumAlphabets := ECapitalCase;
 UpdateCaptions;
end;

procedure TOSDKeyboardWinControl.CButtonSpecialClick(Sender: TObject);

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
      FormMMIFrame.WriteMMIStatus('KEYB',MS_SW_ERROR,MS_KEYB,KeyboardLogErrorCButtonSpecialClick);
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
procedure TOSDKeyboardWinControl.SetWinControl( AssociatedControl : TWinControl;Const EnterF) ;
begin
  try
    WinControlToUse:=AssociatedControl;
    EnterFunction:=TNotifyEvent(EnterF) ;
   //     if Tedit(EditBoxToUse).CanFocus and not Tedit(EditBoxToUse).Focused then
   //       Tedit(EditBoxToUse).SetFocus ;
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('KEYB',MS_SW_ERROR,MS_KEYB,KeyboardLogErrorSetWinControl);
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
