(**************************************************************************
           © COPYRIGHT Bombardier Transportation (Signal)  AB, SWEDEN 2011.
           ================================================================

    The copyright to the computer program herein is the property of
    Bombardier Transportation (Signal) AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the written permission
    from Bombardier Transportation (Signal) AB and in accordance
    with the terms and conditions stipulated in the agreement/contract
    under which the program has been supplied.


    NAME :  UnitLogin.pas

    PROJECT :  LKAB

    Ver    Author           Date    Reason
    ---    ------           ------  ------
          Bo H              120113  Enter/Return - key now acts as Login-click
                                    only if Login-button is visible and enabled.

    DESCRIPTION :  Form that displays startup-history received from the ATP

    INTERFACE :
***************************************************************************)
unit UnitLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ColorButton2, Keyboard, Buttons, ExtCtrls, MMIPanel, UnitMainLayer2,
  GestureMgr, ImgList, Menus, DB, Grids, DBGrids, UnitMainLayer1;

type


  TFormLogin = class(TFormMainLayer2)
    NameEdit: TEdit;
    PasswordEdit: TEdit;
    LoginButton: TColorButton;
    NameLabel: TLabel;
    PasswordLabel: TLabel;
    TimerLoginReenable: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NameEditClick(Sender: TObject);
    procedure PasswordEditClick(Sender: TObject);
    procedure LoginButtonClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure PasswordEditKeyPress(Sender: TObject; var Key: Char);
    procedure NameEditKeyPress(Sender: TObject; var Key: Char);
    procedure TimerLoginReenableTimer(Sender: TObject);
  private
    { Private declarations }
    procedure ScreenChange;
  public
    { Public declarations }
    function ReadID: AnsiString ;
    function ReadPassword:AnsiString ;
    procedure Clear;
    procedure EnableLoginButton(Enable:Boolean);
    procedure UpdateDMI(Refresh : Boolean);
  end;

var
  FormLogin: TFormLogin;

implementation

uses
  MMIStd, UnitMainArea, MMITypes, UnitExchange, UnitMMIFrame, KeyboardUnit,
  UnitStartupHistory, UnitDMIDataModule;

{$R *.dfm}


const
  LoginLogErrorLoginButtonClick = 1;
  LoginLogErrorNameEditClick    = 2;
  LoginLogErrorPasswordEditClick= 3;
  LoginLogErrorTimerDisplay     = 4;
  LoginLogErrorFlashBrakeIntervention = 5;

{*********************************************************
* Function:    ScreenChange
* Description: Adjusts positions and widths when the screen
*              resolution is different from the standard
*              640 * 480 pixels
*********************************************************}
procedure TFormLogin.ScreenChange;
begin  {ScreenChange}

  Width :=  FormMMIFrame.ClientWidth;
  Height := FormMMIFrame.ClientHeight;

  AdjustLabel(NameLabel);
  AdjustLabel(PasswordLabel);
  AdjustEdit(NameEdit);
  AdjustEdit(PasswordEdit);
  AdjustColorButton(LoginButton);

    Top   := FormMMIFrame.ClientHeight - Height + round(35 * GetYFactor);
      // Center
  Left  := (FormMMIFrame.ClientWidth - Width) div 2;


end;

procedure TFormLogin.TimerLoginReenableTimer(Sender: TObject);
begin
  EnableLoginButton(true);
  TimerLoginReenable.Enabled := false;
end;

procedure TFormLogin.FormCreate(Sender: TObject);
begin
  inherited;

  LoginButton.Caption:= InterfaceString(ifLoginBtn ) ;
  NameLabel.Caption:= InterfaceString(ifName) ;
  PasswordLabel.Caption:= InterfaceString(ifPsswd) ;

  Parent := FormMMIFrame.PanelMMIFrame;
  Align := alClient;
  LoginButton.Hide;
        // Adjust pos & size of fields
  ScreenChange;

end;

procedure TFormLogin.FormHide(Sender: TObject);
begin
  OSDKeyboardWinControl.Hide;
end;

procedure TFormLogin.FormShow(Sender: TObject);
begin
  inherited;

  EnableLoginButton(true);

  NameEdit.Clear;
  PasswordEdit.Clear;

  NameEdit.SetFocus;

  OSDKeyboardWinControl.Parent := self;
  OSDKeyboardWinControl.SetWinControl(NameEdit, PasswordEdit.OnClick) ;
  OSDKeyboardWinControl.Left := 95;
  OSDKeyboardWinControl.Top  := PasswordEdit.Top + 40;

  OSDKeyboardWinControl.Show;

  UpdateDMI(true);

end;

procedure TFormLogin.LoginButtonClick(Sender: TObject);
begin
      { Why check if LoginButton is enabled ?
        Because this function is called also by the OSDKeybard - EnterFunction
        }
  if LoginButton.Visible And LoginButton.Enabled
    then
    begin

      try
        if Length(NameEdit.Text)=0 then
        begin
          NameEdit.SetFocus;
          OSDKeyboardWinControl.SetWinControl(NameEdit, PasswordEdit.OnClick) ;
        end else if Length(PasswordEdit.Text)=0 then
        begin
          PasswordEdit.SetFocus;
          OSDKeyboardWinControl.SetWinControl(PasswordEdit, Loginbutton.OnClick) ;
        end else
        begin
            // Appear as pressed
          LoginButton.BevelStyle := bbLowered;
            // Send to ATP
          Exchange.SendDriverIDandPassword ;
          EnableLoginButton(false);
          TimerLoginReenable.Enabled := true;
          if FormMMIFrame.Sound then Beep ;
        end;

      except
        on E: Exception do
        begin
          FormMMIFrame.WriteMMIStatus('LOGIN',MS_SW_ERROR,MS_LOGIN,LoginLogErrorLoginButtonClick);
          if FormMMIFrame.TestMode then
            MessageBox(Handle,PWideChar('Error in LoginButtonClick:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
        end;
      end;
    end;
end;

procedure TFormLogin.NameEditClick(Sender: TObject);
begin

  try
        // Set focus in case this function called by
        // OSD-keyboard Enter-function
    NameEdit.SetFocus;
    OSDKeyboardWinControl.SetWinControl(TEdit(Sender), PasswordEdit.OnClick) ;
    if FormMMIFrame.Sound then Beep ;

  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('LOGIN',MS_SW_ERROR,MS_LOGIN,LoginLogErrorNameEditClick);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in NameEditClick:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
 end;

end;

procedure TFormLogin.NameEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
  begin
    if LoginButton.Visible And LoginButton.Enabled then
      Loginbutton.OnClick(Sender);
  end;

end;

procedure TFormLogin.PasswordEditClick(Sender: TObject);
begin

  try
        // Set focus in case this function called by
        // OSD-keyboard Enter-function
    PasswordEdit.SetFocus;
    OSDKeyboardWinControl.SetWinControl(PasswordEdit, Loginbutton.OnClick) ;
    if FormMMIFrame.Sound then Beep ;
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('LOGIN',MS_SW_ERROR,MS_LOGIN,LoginLogErrorPasswordEditClick);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in PasswordEditClick:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
 end;

end;

procedure TFormLogin.PasswordEditKeyPress(Sender: TObject; var Key: Char);
begin

  if Ord(Key) = VK_RETURN then
  begin
    if LoginButton.Visible And LoginButton.Enabled then
      Loginbutton.OnClick(Sender);
  end;
end;

{*********************************************************
* Function:    FormLogin.ReadID
* Description: Function that gives ser access to read
               the Name editbox
*********************************************************}
Function TFormLogin.ReadID: AnsiString ;
begin
      // Convert UniCode string to AnsiString
  Result:= AnsiString(NameEdit.Text) ;
end;

{*********************************************************
* Function:    FormLogin.ReadPassword
* Description: Function that gives ser access to read
               The password editbox
*********************************************************}
Function TFormLogin.ReadPassword:AnsiString ;
begin
      // Convert UniCode string to AnsiString
  Result:=AnsiString(PasswordEdit.Text) ;
end;

{*********************************************************
* Function:    FormLogin.Clear
* Description: Clear edit-fields and get ready for retry
*********************************************************}
Procedure TFormLogin.Clear ;
begin

  NameEdit.Clear;
  PasswordEdit.Clear;
  OSDKeyboardWinControl.SetWinControl(NameEdit, PasswordEdit.OnClick) ;
end;

{*************************************************************
* Function:    EnableLoginButton
* Description: Enable the Login-button and change the appearance
*              in order to indicate whether the button is possible
*              to click or not
**************************************************************}
procedure TFormLogin.EnableLoginButton(Enable:Boolean);
begin

  if Enable then
  begin
    LoginButton.Enabled := true;
    LoginButton.Font.Color := ClBlack;
    LoginButton.BevelStyle := bbRaised;
  end
  else
  begin
    LoginButton.Enabled := false;
    LoginButton.Font.Color := ClMedGray;
    LoginButton.BevelStyle := bbLowered;
  end;

end;
{ *********************************************************
* UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormLogin.UpdateDMI(Refresh : Boolean);
var
  AllowedButtons: Byte;
  AllowedButtonsChanged: Boolean;

  begin
  inherited;

  { Allowed buttons }
  AllowedButtons := DataModuleDMI.GetAllowedButtons(AllowedButtonsChanged);
  if Refresh or AllowedButtonsChanged then
  begin
    if bytebool(AllowedButtons and ALLOWED_TO_LOGIN) then
      LoginButton.Show
    else
      LoginButton.Hide;
  end;

  if (not OSDKeyboardWinControl.Visible) then
    OSDKeyboardWinControl.Show;

  //To only show indication and disable the buttons
  EButtons.Enabled := False;

end;end.