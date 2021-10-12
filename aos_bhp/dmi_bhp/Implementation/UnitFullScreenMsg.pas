(*****************************************************************************
*           © COPYRIGHT Bombardier Transportation AB, SWEDEN 2012.           *
*           ======================================================           *
*                                                                            *
*    The copyright to the computer program herein is the                     *
*    property of Bombardier Transportation AB, Sweden. All rights reserved.  *
*    The program may be used and/or copied only with the                     *
*    written permission from Bombardier Transportation AB,                   *
*    or in accordance                                                        *
*    with the terms and conditions stipulated in the                         *
*    agreement/contract under which the program has been                     *
*    supplied.                                                               *
*                                                                            *
*                                                                            *
*    NAME:  UnitFullScreenMsg.pas                                            *
*                                                                            *
*    PROJECT:  LKAB, InterFlow TrainBorne                                    *
*                                                                            *
*    Ver    Author           Date    Reason                                  *
*    ---    ------           ------  ------                                  *
*           Bo H              121025  SetDelayedMsg                          *
*                                                                            *
*    DESCRIPTION: Displays a black blank screen with a message               *
*    Use SetMessage(...) to set the message to be displayed                  *
*    Use SetDelayedMessage(...) to set a message to be displayed             *
*    after a delay . Both may be used if an initial message should           *
*    be displayed and later be overwritten with another                      *
*                                                                            *
******************************************************************************)

unit UnitFullScreenMsg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UnitMainArea, MMITypes, MMIStd, Buttons, ExtCtrls, UnitMainLayer2,
  GestureMgr, ImgList, Menus, DB, Grids, DBGrids, ColorButton2, MMIPanel,
  UnitMainLayer1;

type
  TFormFullScreenMsg = class(TFormMainLayer2)
    LabelMessage: TLabel;
    TimerDelayedMessage: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerDelayedMessageTimer(Sender: TObject);
  private
    { Private declarations }
    DelayedMessage : String;
    procedure ScreenChange;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetMessage(ifId : TIfaceEnum); overload;
    procedure SetMessage(ifString : String); overload;
    procedure SetDelayedMessage(ifId : TIfaceEnum; msDelay : Integer);
    procedure UpdateDMI(Refresh : Boolean);
  end;

var
  FormFullScreenMsg: TFormFullScreenMsg;

implementation

{$R *.DFM}

uses UnitMMIFrame, UnitStartupHistory, UnitDMIDataModule, UnitConfirmLoadedStatus;

const

  FullScreenMsgLogErrorCreate = 1;

constructor TFormFullScreenMsg.Create(AOwner: TComponent);
begin
  Try
    inherited Create(AOwner);
    Left := 0;
    Top := 0 ;


    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;


  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('FULLSCR',MS_SW_ERROR,MS_FULLSCREEN,FullScreenMsgLogErrorCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in Create:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate ;
    end;
 End;
end;

{*********************************************************
* Function:    FormCreate
* Description: Function being called once by OnCreate-event
*              It will call ScreenChange to adjust the size
*              and pos of the controls
*********************************************************}
procedure TFormFullScreenMsg.FormCreate(Sender: TObject);
begin
inherited;
  ScreenChange;

  SetMessage('');

end;

{*********************************************************
* Function:    FormShow
* Description: Function being called once by OnShow-event
*********************************************************}
procedure TFormFullScreenMsg.FormShow(Sender: TObject);
begin
inherited;

  UpdateDMI(true);

end;

{*********************************************************
* Function:    ScreenChange
* Description: Adjust the size and pos of the controls
* depending on the main-window size
* (i.e. screen resolution if full screen)
*********************************************************}
procedure TFormFullScreenMsg.ScreenChange;
Begin

  AdjustLabel(LabelMessage);

End;

{*********************************************************
* Function:    SetMessage
* Description: Function that sets the string-ID of the string to display
*********************************************************}
procedure TFormFullScreenMsg.SetMessage(ifId : TIfaceEnum);
begin

  LabelMessage.Caption:=InterfaceString(ifId) ;
  LabelMessage.Left:=0;
  LabelMessage.Width:=ClientWidth;
  LabelMessage.Alignment:=taCenter;

end;

{*********************************************************
* Function:    SetMessage
* Description: Function that sets the string to display
*********************************************************}
procedure TFormFullScreenMsg.SetMessage(ifString : String);
begin

  LabelMessage.Left:=0;
  LabelMessage.Width:=ClientWidth;
  LabelMessage.Alignment:=taCenter;
  LabelMessage.Caption:=ifString ;

end;


procedure TFormFullScreenMsg.TimerDelayedMessageTimer(Sender: TObject);
begin
  TimerDelayedMessage.Enabled := false;
  LabelMessage.Caption:=DelayedMessage;

end;

{*********************************************************
* Function:    SetDelayedMessage
* Description: Function that sets the string-ID
* of the string to display after the specified delay (in ms)
*********************************************************}
procedure TFormFullScreenMsg.SetDelayedMessage(ifId : TIfaceEnum; msDelay : Integer);
begin

  DelayedMessage := InterfaceString(ifId) ;
      { Start the timer that will display the string }
  TimerDelayedMessage.Interval:=msDelay;
  TimerDelayedMessage.Enabled := true;
  LabelMessage.Left:=0;
  LabelMessage.Width:=ClientWidth;
  LabelMessage.Alignment:=taCenter;


end;

{ *********************************************************
* UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormFullScreenMsg.UpdateDMI(Refresh: Boolean);
var
  AdditionalConfirmationInfo: Byte;
  AdditionalConfirmationInfoChanged: Boolean;

  ATPVerificationState: T_ATPVerificationStates;
  ATPVerificationStateChanged: Boolean;

  AllowedButtons: Byte;
  AllowedButtonsChanged: Boolean;

  ATPModeChanged: Boolean;
  ATPMode: T_ATPmodes;

  ATPStateChanged: Boolean;
  ATPState: T_ATPStates;

  ConfigSubStateChanged: Boolean;
  ConfigSubState: TConfigSubState;

begin
  inherited;

  AdditionalConfirmationInfo := DataModuleDMI.GetAdditionalConfirmationInfo
    (AdditionalConfirmationInfoChanged);

  if (Refresh or AdditionalConfirmationInfoChanged) then
  begin
    if (Bytebool(AdditionalConfirmationInfo and
      CONFIRM_TRAIN_LOADED_STATUS_CHANGE)) then
      FormConfirmLoadedStatus.Show
    else
      FormConfirmLoadedStatus.Hide;
  end;

  ATPVerificationState := DataModuleDMI.GetATPVerificationState
    (ATPVerificationStateChanged);
  ATPState := DataModuleDMI.GetATPState(ATPStateChanged);
  ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
  ConfigSubState := DataModuleDMI.GetConfigSubState(ConfigSubStateChanged);
  AllowedButtons := DataModuleDMI.GetAllowedButtons(AllowedButtonsChanged);

  if (Refresh or ATPStateChanged or ATPVerificationStateChanged or
    ATPModeChanged or ConfigSubStateChanged or AllowedButtonsChanged) then
  begin
    case ATPState of
      asUndefined:
        SetMessage(ifATPStU);
      asBasicsSystemStartUp:
        SetMessage(ifATPBssys_str);
      asApplicationStartUp:
        SetMessage(ifATPApplStr);
      asInactive:
        SetMessage('');

      asActivationTest:
        begin
          SetMessage(ifATPTst);
          SetDelayedMessage(ifATPStartLoco, StartLocoTextDelay);
        end;
      asSystemRestart:
        SetMessage(ifSystemRestart);

    else
      begin
        // Check Driver Authorization
        if (ATPVerificationState = avsVerificationState) then
        begin
          SetMessage(ifLoginVer); // Verifying PAssword
        end
        else if (ATPVerificationState = avsRedoinputState) then
        begin
          SetMessage(ifLoginFALSE); // Wrong Username and Password
        end
        else

        begin // Driver Authorized
          case ATPMode of
            amPowerup:
              begin
                if (Bytebool(AllowedButtons and
                  ALLOWED_TO_ENTER_CONFIG_MODE)) then
                  SetMessage(ATPManCnfWait)
                else
                  SetMessage('');
              end;
            amTrainconfiguration :
              if (ConfigSubState = cssRestartConfig) then
              begin
                if (Bytebool(AllowedButtons and
                  ALLOWED_TO_ENTER_CONFIG_MODE)) then
                  SetMessage(ATPManCnfWait)
                else
                  SetMessage('');
              end
              else
                SetMessage(ifATPDefTrn);
            amTrainregistration :
              SetMessage(ifATPRegtTrn);

          end;     //Case ATPMode

        end;      //else Driver Authorization

      end;       // Case ATPStates
    end;
  end;          //If any state changed
end;



end.