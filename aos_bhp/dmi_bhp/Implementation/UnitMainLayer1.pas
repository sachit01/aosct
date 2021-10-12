unit UnitMainLayer1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, MMIPanel, DB, Grids, DBGrids, ColorButton2,
  Menus, Buttons, ImgList, GestureMgr;

type
  TFormMainLayer1 = class(TForm)
    Information: TMMIPanel;
    MMIPanelE14: TMMIPanel;
    LabelTrainId: TLabel;
    MMIPanelE15: TMMIPanel;
    LabelLocationName: TLabel;
    MMIPanelE17: TMMIPanel;
    TimeField: TLabel;
    MMIPanelE18: TMMIPanel;
    LabelATPMode: TLabel;
    MMIPanelE19: TMMIPanel;
    RadioImage: TImage;
    MMIPanelE16: TMMIPanel;
    LabelETA: TLabel;
    TimerClock: TTimer;
    TextListArea: TMMIPanel;
    UpArrow: TColorButton;
    DownArrow: TColorButton;
    DBGridEvents: TDBGrid;
    DataSourceEvents: TDataSource;
    BitBtnMenu: TBitBtn;
    PopupMenu: TPopupMenu;
    MenuItemShowStartuphistory: TMenuItem;
    N1: TMenuItem;
    MenuItemClearDMIEventlog: TMenuItem;
    ImageListPopup: TImageList;
    AreaF: TMMIPanel;
    ColorButtonConfiguration: TColorButton;
    ColorButtonYard: TColorButton;
    ColorButtonHandlingDone: TColorButton;
    ColorButtonLogOut: TColorButton;
    ColorButtonPossession: TColorButton;
    ColorButtonShunting: TColorButton;
    N2: TMenuItem;
    MenuItemChangeTrainId: TMenuItem;
    GestureManager: TGestureManager;
    MenuItemShowVehicleData: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    ColorButtonAbortSetup: TColorButton;
    ColorButtonFreeRolling: TColorButton;
    MenuItemShowRadioArea: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    MenuItemCancelRegArea: TMenuItem;
    N7: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure TimerClockTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure TimeFieldClick(Sender: TObject);
    procedure UpArrowClick(Sender: TObject);
    procedure DownArrowClick(Sender: TObject);
    procedure BitBtnMenuClick(Sender: TObject);
    procedure MenuItemShowStartuphistoryClick(Sender: TObject);
    procedure MenuItemShowVehicleDataClick(Sender: TObject);
    procedure MenuItemClearDMIEventlogClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemChangeTrainIdClick(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure ColorButtonConfigurationClick(Sender: TObject);
    procedure ColorButtonHandlingDoneClick(Sender: TObject);
    procedure ColorButtonPossessionClick(Sender: TObject);
    procedure ColorButtonShuntingClick(Sender: TObject);
    procedure ColorButtonYardClick(Sender: TObject);
    procedure ColorButtonLogOutClick(Sender: TObject);
    procedure ColorButtonAbortSetupClick(Sender: TObject);
    procedure ColorButtonFreeRollingClick(Sender: TObject);
    procedure MenuItemShowRadioAreaClick(Sender: TObject);
    procedure MenuItemCancelRegAreaClick(Sender: TObject);
    procedure UpdateETA(Time: TDateTime);
    procedure LabelETAClick(Sender: TObject);

  private
    { Private declarations }
    FRadioActive: Boolean;
    RadioBitmap: TBitmap;
    RadioNotConBitmap: TBitmap;
    ShowDate : Boolean;
    ShowETADate : Boolean;

    procedure UpdateTime(Time: TDateTime);
    procedure UpdateScrollButtons;
    procedure UpdateFButtons(AllowedButtons : Byte);
    procedure AllowChangeTrainName(Allow: Boolean);
    procedure SetRadioActive(Value: Boolean);
    procedure ShowYardModeButton(Show: Boolean);
    procedure ShowPossessionModeButton(Show: Boolean);
    procedure ShowShuntingModeButton(Show: Boolean);
    procedure ShowConfigModeButton(Show: Boolean);
    procedure ShowLogoutButton(Show: Boolean);
    procedure ShowHandlingDoneButton(Show: Boolean);
    procedure ShowAbortSetupButton(Show: Boolean);
    procedure AllowShowTrainComp(Allow: Boolean);
    procedure AllowShowRadioArea(Allow: Boolean);
    procedure ShowFreeRollingButton(Show: Boolean);
    procedure AllowShowCancelRegArea(Allow: Boolean);
    procedure AdjustLabelFontSize(lbl : TLabel; captionLength : Word);

  protected
    ClockTick : Byte;

  public
    { Public declarations }
    procedure UpdateDMI(Refresh : Boolean);

    property RadioSymbol: Boolean read FRadioActive write SetRadioActive
      default False;

  end;

var
  FormMainLayer1: TFormMainLayer1;

implementation

uses UnitMMIFrame, MMIStd, UnitNewTrainName, MMITypes, UnitDMIDataModule,
  UnitStartupHistory, Types, UnitViewLog, UnitExchange,
  UnitConfirmATPMode, UnitTrainComposition, UnitRadioChannel, UnitConfirmCancelRegArea,
  UnitFullScreenMsg;

{$R *.dfm}


procedure TFormMainLayer1.FormCreate(Sender: TObject);
begin

  Information.Width := FormMMIFrame.ClientWidth - GetXpos2;
  Information.Height := FormMMIFrame.ClientHeight - GetYpos3;
  Information.Top := GetYpos3;
  Information.Left := GetXpos2;

  AdjustControl(TextListArea);
  AdjustControl(MMIPanelE14);
  AdjustControl(MMIPanelE16);
  AdjustControl(MMIPanelE17);
  AdjustControl(MMIPanelE18);
  AdjustControl(MMIPanelE19);

  AdjustLabel(LabelTrainId);
  AdjustLabel(LabelETA);
  AdjustLabel(TimeField);
  AdjustLabel(LabelATPMode);

          // Text area
  AdjustColorButton(UpArrow, False);
  AdjustColorButton(DownArrow, False);


{  BitBtnMenu.Caption := InterfaceString(ifMenu); }

  AreaF.Width := FormMMIFrame.ClientWidth - GetXpos3;
  AreaF.Height := GetYpos3;
  AreaF.Left := GetXpos3;

      // Buttons with configurable image
      // Config button F area
  if LoadBitmapImage(ColorButtonConfiguration.Picture.Bitmap, ImageConfiguration, 'MAIN') then
      // Clear caption if image was loaded
    ColorButtonConfiguration.Caption := ''
  else
      // Replace caption with language-dependent character
    ColorButtonConfiguration.Caption := InterfaceString(ifConfigurationFBtn);

  if LoadBitmapImage(ColorButtonYard.Picture.Bitmap, ImageYardMode,'MAIN') then
      // Clear caption if image was loaded
    ColorButtonYard.Caption := ''
  else
      // Replace caption with language-dependent character
    ColorButtonYard.Caption := InterfaceString(ifYardModeFBtn);

  if LoadBitmapImage(ColorButtonHandlingDone.Picture.Bitmap, ImageReady,'MAIN') then
      // Clear caption if image was loaded
    ColorButtonHandlingDone.Caption := ''
  else
      // Replace caption with language-dependent character
    ColorButtonHandlingDone.Caption := InterfaceString(ifReadyFBtn);

  if LoadBitmapImage(ColorButtonAbortSetup.Picture.Bitmap, ImageReady,'MAIN') then
      // Clear caption if image was loaded
    ColorButtonAbortSetup.Caption := ''
  else
      // Replace caption with language-dependent character
      ColorButtonAbortSetup.Caption := 'X';
//    ColorButtonAbortSetup.Caption := InterfaceString(ifAbortSetupFBtn);
   if LoadBitmapImage(ColorButtonFreeRolling.Picture.Bitmap, ImageReady,'MAIN') then
      // Clear caption if image was loaded
    ColorButtonFreeRolling.Caption := ''
  else
      // Replace caption with language-dependent character
      ColorButtonFreeRolling.Caption := 'F';

      // Possession and Shunting button texts/image not yet configurable

  RadioBitmap := TBitmap.Create;
  RadioBitmap.LoadFromResourceName(Hinstance, 'RADIOZAPP');
  CentreImage(RadioImage, MMIPanelE19.Width, MMIPanelE19.Height);

  RadioNotConBitmap := TBitmap.Create;
  RadioNotConBitmap.LoadFromResourceName(Hinstance, 'RADIONOTCON');
  CentreImage(RadioImage, MMIPanelE19.Width, MMIPanelE19.Height);

  AdjustColorButton(ColorButtonLogOut);

  AdjustColorButton(ColorButtonConfiguration);
  AdjustColorButton(ColorButtonYard);
  AdjustColorButton(ColorButtonHandlingDone);
  AdjustColorButton(ColorButtonPossession);
  AdjustColorButton(ColorButtonShunting);
  AdjustColorButton(ColorButtonAbortSetup);
  AdjustColorButton(ColorButtonFreeRolling);
  ShowDate := False;
  ClockTick := 0;
  ShowETADate := False;

end;

procedure TFormMainLayer1.FormDestroy(Sender: TObject);
begin
 RadioBitmap.Free;
 RadioNotConBitmap.Free;
end;

procedure TFormMainLayer1.FormHide(Sender: TObject);
begin
  TimerClock.Enabled := False;
end;

procedure TFormMainLayer1.FormShow(Sender: TObject);
begin
  TimerClock.Enabled := True;

  ShowScrollBar(DBGridEvents.Handle, SB_VERT, False);
  TDateTimeField(DBGridEvents.DataSource.DataSet.FieldByName('Time')).DisplayFormat:='hh:mm';

end;

procedure TFormMainLayer1.ColorButtonLogOutClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsLogOut);
  if FormMMIFrame.Sound then
    Beep;

end;

procedure TFormMainLayer1.MenuItemCancelRegAreaClick(Sender: TObject);
begin
    FormConfirmCancelRegArea.Show;
end;


procedure TFormMainLayer1.MenuItemChangeTrainIdClick(Sender: TObject);
var
  ChangedTrainID : Boolean;
begin
  FormNewTrainName.EditTrainName.Text := DataModuleDMI.GetTrainId(ChangedTrainId);
  FormNewTrainName.Show();
end;

procedure TFormMainLayer1.MenuItemClearDMIEventlogClick(Sender: TObject);
begin
  while DataSourceEvents.DataSet.RecordCount > 0 do
  begin     {Delete all rows}
    DataSourceEvents.DataSet.First;
    DataSourceEvents.DataSet.Delete;
  end;
end;

procedure TFormMainLayer1.MenuItemShowRadioAreaClick(Sender: TObject);
begin
        FormRadioChannel.Show;
end;

procedure TFormMainLayer1.MenuItemShowStartuphistoryClick(Sender: TObject);
begin
  FormStartupHistory.Show;
end;

procedure TFormMainLayer1.MenuItemShowVehicleDataClick(Sender: TObject);
begin
  FormTrainComposition.Show;
end;

procedure TFormMainLayer1.AllowChangeTrainName(Allow: Boolean);
begin
  MenuItemChangeTrainId.Enabled := Allow;
  if (NOT Allow) then
    FormNewTrainName.Hide;
end;

procedure TFormMainLayer1.AllowShowTrainComp(Allow: Boolean);
begin
  MenuItemShowVehicleData.Enabled := Allow;
end;

procedure TFormMainLayer1.AllowShowRadioArea(Allow: Boolean);
begin
  MenuItemShowRadioArea.Enabled := Allow;
end;

procedure TFormMainLayer1.AllowShowCancelRegArea(Allow: Boolean);
begin
  MenuItemCancelRegArea.Enabled := Allow;
end;



procedure TFormMainLayer1.TimeFieldClick(Sender: TObject);
begin
  try
    ShowDate := not ShowDate;
    UpdateTime(Now);
    if FormMMIFrame.Sound then
      Beep;

  except

    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
        MainLogErrorTimeFieldClick);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in TimeFieldClick:' + E.Message),
          PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);

    end;
  end;

end;

{ *********************************************************
* TimerClockTimer
* Description: Periodic actions
********************************************************* }
procedure TFormMainLayer1.TimerClockTimer(Sender: TObject);
begin
  UpdateTime(Now);
    {Increment clock tick up to 99 and restart}
  ClockTick := (ClockTick +1) mod 100;
  UpdateScrollButtons();

end;

{ *********************************************************
* Function:    UpArrowClick
* Description: Scroll the events up one page
********************************************************* }
procedure TFormMainLayer1.UpArrowClick(Sender: TObject);
begin
  DBGridEvents.DataSource.DataSet.Prior;
  DBGridEvents.DataSource.DataSet.Prior;
  DBGridEvents.DataSource.DataSet.Prior;
  DBGridEvents.DataSource.DataSet.Prior;
end;
{ *********************************************************
* Function:    BitBtnMenuClick
* Description: Display the PopUp-menu
********************************************************* }
procedure TFormMainLayer1.BitBtnMenuClick(Sender: TObject);
var
  ScreenPoint : TPoint;
begin

  ScreenPoint := ClientToScreen(Point(BitBtnMenu.Width, BitBtnMenu.Height));
  PopUpMenu.Popup(ScreenPoint.X, ScreenPoint.Y);
end;

procedure TFormMainLayer1.ColorButtonAbortSetupClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsAbortSetup);
  if FormMMIFrame.Sound then
    Beep;
end;

procedure TFormMainLayer1.ColorButtonFreeRollingClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsRequestFreeRolling);
  if FormMMIFrame.Sound then
    Beep;
end;

procedure TFormMainLayer1.ColorButtonConfigurationClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsTrainConfig);
  if FormMMIFrame.Sound then
    Beep;
end;

{ *********************************************************
* Function:    DownArrowClick
* Description: Scroll the events down one page
********************************************************* }
procedure TFormMainLayer1.DownArrowClick(Sender: TObject);
begin
  DBGridEvents.DataSource.DataSet.Next;
  DBGridEvents.DataSource.DataSet.Next;
  DBGridEvents.DataSource.DataSet.Next;
  DBGridEvents.DataSource.DataSet.Next;
end;

{ *********************************************************
* Function:    UpdateTime
* Description: Update time-field
********************************************************* }
procedure TFormMainLayer1.UpdateTime(Time: TDateTime);
var
  Hour, Min, Sec, MSec: Word;
begin
  try
    if not ShowDate then
    begin
      DecodeTime(Time, Hour, Min, Sec, MSec);
          { Flash the : }
      if MSec >= 500 then
        Timefield.Caption := FormatDateTime('hh:nn', Time)
      else
        Timefield.Caption := FormatDateTime('hh nn', Time);
    end
    else
      Timefield.Caption := FormatDateTime('d mmm', Time);

  except

    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
        MainLogErrorUpdateTime);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in UpdateTime:' + E.Message),
          PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);

    end;
  end;
end;

{ *********************************************************
* UpdateScrollButtons
* Description: Show scroll-buttons if more than one page (4 events) visible
********************************************************* }
procedure TFormMainLayer1.UpdateScrollButtons;
begin

  if (DataSourceEvents.DataSet.RecordCount > 4) then
  begin
    UpArrow.Show;
    DownArrow.Show;
  end
  else
  begin
    UpArrow.Hide;
    DownArrow.Hide;
  end;

end;

{ *********************************************************
* AdjustLabelFontSize
* Description: Adjusts the font-size of the Train Name / Location Name label
* in order to fit within the available size
********************************************************* }
procedure TFormMainLayer1.AdjustLabelFontSize(lbl : TLabel; captionLength : Word);
var
 fontSize : Word;
begin

  if (captionLength < 12) then
   fontSize:= 16
  else if (captionLength < 14) then
   fontSize:= 14
  else if (captionLength < 15) then
   fontSize:= 12
  else if (captionLength < 17) then
   fontSize:= 11
  else if (captionLength < 19) then
   fontSize:= 10
  else
   fontSize:= 9;

    // Adjust for screen-resolution
  lbl.Font.Size:=round(fontSize*GetYfactor);
end;

{ *********************************************************
* UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormMainLayer1.UpdateDMI(Refresh : Boolean);
var
  TrainId : String;
  TrainIdChanged : Boolean;

  ATPMode        : T_ATPmodes;
  ATPModeChanged : Boolean;

  InterfaceFlags : Byte;
  InterfaceFlagsChanged : Boolean;

  AllowedButtons : Byte;
  AllowedButtonsChanged : Boolean;

  ConfirmModeChange : Byte;
  ConfirmModeChanged : Boolean;

  AdditionalAllowedToInfo : Byte;
  AdditionalAllowedToInfoChanged : Boolean;

  LocationName : String;
  LocationNameChanged : Boolean;

  ETADateTime : TDateTime;
  ETA : LongInt;
  ETAChanged : Boolean;

begin

    {Train Id}
  TrainId := DataModuleDMI.GetTrainId(TrainIdChanged);
  if Refresh or TrainIdChanged then
  begin

    LabelTrainId.Caption := TrainId;
    AdjustLabelFontSize(LabelTrainId, Length(LabelTrainId.Caption));

  end;

    {ATP Mode}
  ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);

  if Refresh or ATPModeChanged then
  begin
    LabelATPMode.Caption:=InterfaceString(TIfaceEnum(ord(ATPMode))) ;
  end;

    {Interface flags}
  InterfaceFlags := DataModuleDMI.GetInterfaceFlags(InterfaceFlagsChanged);
  if Refresh or InterfaceFlagsChanged then
  begin
    if ByteBool(InterfaceFlags and RADIO_ACTIVE) then
    begin // Radio Ok with TCC
      SetRadioActive(true);
    end
    else
    begin
      SetRadioActive(false);
    end;
  end;


    {Area F buttons}
  AllowedButtons := DataModuleDMI.GetAllowedButtons(AllowedButtonsChanged);

  if Refresh or AllowedButtonsChanged then
  begin
    UpdateFButtons(AllowedButtons);
  end;

  {Confirm Mode Change}
  ConfirmModeChange   :=  DataModuleDMI.GetConfirmModeChange(ConfirmModeChanged);
  if Refresh or ConfirmModeChanged then
  begin
    { Show or hide confirm form}
    case ConfirmModeChange of
      CONFIRM_YARD :
        begin
          FormConfirmNewATPMode.LabeledEditATPMode.Text :=
            InterfaceString(TIfaceEnum(ord(amYard)));
          FormConfirmNewATPMode.thisButton := bsConfirmYard;
          FormConfirmNewATPMode.Show;
        end;

      CONFIRM_SHUNTING_ROUTE:
        begin
          FormConfirmNewATPMode.LabeledEditATPMode.Text :=
            InterfaceString(TIfaceEnum(ord(amShuntingRoute)));
          FormConfirmNewATPMode.thisButton := bsConfirmShuntingRoute;
          FormConfirmNewATPMode.Show;
        end;

      CONFIRM_STAFF_RESPONSIBLE:
        begin
          FormConfirmNewATPMode.LabeledEditATPMode.Text :=
            InterfaceString(TIfaceEnum(ord(amStaffResponsible)));
          FormConfirmNewATPMode.thisButton := bsConfirmSR;
          FormConfirmNewATPMode.Show;
        end;

      CONFIRM_JOIN:
        begin
          FormConfirmNewATPMode.LabeledEditATPMode.Text :=
            InterfaceString(TIfaceEnum(ord(amJoin)));
          FormConfirmNewATPMode.thisButton := bsConfirmJoin;
          FormConfirmNewATPMode.Show;
        end;

      CONFIRM_SLEEP:
        begin
          FormConfirmNewATPMode.LabeledEditATPMode.Text :=
            InterfaceString(TIfaceEnum(ord(amSleeping)));
          FormConfirmNewATPMode.thisButton := bsConfirmSleep;
          FormConfirmNewATPMode.Show;
        end;

      CONFIRM_SPLIT:
        begin
          FormConfirmNewATPMode.LabeledEditATPMode.Text :=
            InterfaceString(TIfaceEnum(ord(amSplit)));
          FormConfirmNewATPMode.thisButton := bsConfirmSplit;
          FormConfirmNewATPMode.Show;
        end;

        else {CofirmModeChange is 0 or undefined value}
        begin
          FormConfirmNewATPMode.Hide;
        end;
    end;
  end;

      {Show Train Composition}
  AdditionalAllowedToInfo :=  DataModuleDMI.GetAdditionalAllowedToInfo(AdditionalAllowedToInfoChanged);
  if Refresh or AdditionalAllowedToInfoChanged then
  begin
    if ByteBool(AdditionalAllowedToInfo and ALLOWED_TO_SHOW_TRAIN_COMP) then
          // Allowed to show train comp
      AllowShowTrainComp(True)
    else
      AllowShowTrainComp(False);

          { show Handling Done }
    if ByteBool(AdditionalAllowedToInfo and ALLOWED_TO_SHOW_HANDLING_DONE) then
      ShowHandlingDoneButton(True)
    else
      ShowHandlingDoneButton(False);

         // Show Free Rolling
    if ByteBool(AdditionalAllowedToInfo and ALLOWED_TO_SHOW_FREE_ROLLING) then
      ShowFreeRollingButton(True)
    else
      ShowFreeRollingButton(False);

        // Show Cancel Registration Area
    if ByteBool(AdditionalAllowedToInfo and
      ALLOWED_TO_SHOW_CANCEL_REG_AREA) then
      AllowShowCancelRegArea(True)
    else
      AllowShowCancelRegArea(False);

      //show Cancel Registration Area Form
    if ByteBool(AdditionalAllowedToInfo and
      ALLOWED_TO_SHOW_CANCEL_REG_AREA) then
      AllowShowCancelRegArea(True)
    else
     begin
      AllowShowCancelRegArea(False);
      // Hide Cancel Reg Area-form
      // If driver logged out
      FormConfirmCancelRegArea.Hide;
     end;

        { show radio area }
    if (ByteBool(AdditionalAllowedToInfo and ALLOWED_TO_SHOW_RADIO_AREA)) then
      AllowShowRadioArea(True)
    else
      AllowShowRadioArea(False);

  end; // End of Additional Allowed Info check

  {Location Name}
  {Clear location name when mode is not location}
  ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
  LocationName := DataModuleDMI.GetLocationName(LocationNameChanged);
  if LocationNameChanged then
  begin
    LabelLocationName.Caption := LocationName;
    AdjustLabelFontSize(LabelLocationName, Length(LabelLocationName.Caption));
  end
  else if ATPModeChanged and (ATPMode <> amLocation) then
  begin
    LabelLocationName.Caption := '';
    DataModuleDMI.SetLocationName('');
  end;

  { Estimated Time of Arrival }
  ETA := DataModuleDMI.GetETA(ETAChanged);
  if Refresh or ETAChanged then
  begin
    if ETA <> 0 then
    begin
      ETADateTime := UnixToDelphiTime(ETA);
      UpdateETA(ETADateTime);
    end
    else
      LabelETA.Caption := '';
  end;

end;
{ *********************************************************
* UpdateFButtons
* Description: Call this method to update the buttons in Area F
* Arguments: Bits for allowed buttons
********************************************************* }
procedure TFormMainLayer1.UpdateFButtons(AllowedButtons : Byte);
begin

  if bytebool(AllowedButtons and ALLOWED_TO_ENTER_YARD_MODE) then
      // Allowed to enter Yard mode
    ShowYardModeButton(True)
  else
    ShowYardModeButton(False);

  if bytebool(AllowedButtons and ALLOWED_TO_ENTER_POSSESSION_MODE) then
      // Allowed to enter Possession mode
    ShowPossessionModeButton(true)
  else
    ShowPossessionModeButton(false);

  if bytebool(AllowedButtons and ALLOWED_TO_ENTER_SHUNTING_MODE) then
      // Allowed to enter Shunting mode
    ShowShuntingModeButton(true)
  else
    ShowShuntingModeButton(false);

  if bytebool(AllowedButtons and ALLOWED_TO_ENTER_CONFIG_MODE) then
      // Allowed to enter Config mode
    ShowConfigModeButton(true)
  else
    ShowConfigModeButton(false);

  //Abort Setup
   if bytebool(AllowedButtons and ALLOWED_TO_ABORT_SETUP) then
    ShowAbortSetupButton(true)
  else
    ShowAbortSetupButton(false);

    // Logout
  if bytebool(AllowedButtons and ALLOWED_TO_LOGOUT) then
      // Allowed to logout
    ShowLogoutButton(true)
  else
    ShowLogoutButton(false);

  if bytebool(AllowedButtons and ALLOWED_TO_CHANGE_TRAIN_NAME) then
      // Allowed to enter Config mode
    AllowChangeTrainName(true)
  else
    AllowChangeTrainName(false);

end;

// Show buttons.
//
procedure TFormMainLayer1.ShowYardModeButton(Show: Boolean);
begin
  ColorButtonYard.Visible := Show;
end;

procedure TFormMainLayer1.ShowPossessionModeButton(Show: Boolean);
begin
  ColorButtonPossession.Visible := Show;
end;

procedure TFormMainLayer1.ShowShuntingModeButton(Show: Boolean);
begin
  ColorButtonShunting.Visible := Show;
end;
procedure TFormMainLayer1.ShowLogoutButton(Show: Boolean);
begin
  ColorButtonLogout.Visible := Show;
end;

procedure TFormMainLayer1.ShowConfigModeButton(Show: Boolean);
begin
  ColorButtonConfiguration.Visible := Show;
end;
procedure TFormMainLayer1.ShowHandlingDoneButton(Show: Boolean);
begin
  ColorButtonHandlingDone.Visible := Show;
end;

procedure TFormMainLayer1.ShowAbortSetupButton(Show: Boolean);
begin
  ColorButtonAbortSetup.Visible := Show;
end;

procedure TFormMainLayer1.ShowFreeRollingButton(Show: Boolean);
begin
  ColorButtonFreeRolling.Visible := Show;
end;

{ *********************************************************
* HandlingDoneClick
* Description: Sends a message about handling done.
********************************************************* }
procedure TFormMainLayer1.ColorButtonHandlingDoneClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsHandlingDone);
  if FormMMIFrame.Sound then
    Beep;
end;

procedure TFormMainLayer1.ColorButtonPossessionClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsPossession);
  if FormMMIFrame.Sound then
    Beep;

end;

procedure TFormMainLayer1.ColorButtonShuntingClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsShunting);
  if FormMMIFrame.Sound then
    Beep;

end;

{ *********************************************************
* Function:    FormMainArea.ColorButtonYardModeClick
* Description: Sends a message to The ATP requesting YardMode
********************************************************* }
procedure TFormMainLayer1.ColorButtonYardClick(Sender: TObject);
begin
  Exchange.SendMMIButton(bsEnterYardMode);
  if FormMMIFrame.Sound then
    Beep;

end;

{*********************************************************
* SetRadioActive
* Description: Set the Radio Icon
********************************************************* }
procedure TFormMainLayer1.SetRadioActive(Value: Boolean);
begin
  try
    if Value then
      FRadioActive := true
    else
      FRadioActive := false;
    if (FRadioActive) then
      RadioImage.Picture.Bitmap.Assign(RadioBitmap)
    else
      RadioImage.Picture.Bitmap.Assign(RadioNotConBitmap);

    CentreImage(RadioImage, MMIPanelE19.Width, MMIPanelE19.Height);
  except
    on E: Exception do
    begin
       FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
       MainLogErrorSetRadioActive);
       if FormMMIFrame.TestMode then
         MessageBox(Handle, PWideChar('Error in SetRadioActive:' + E.Message),
         PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{*********************************************************
* FormGesture
* Description: Handle gestures on this form
********************************************************* }
procedure TFormMainLayer1.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);

begin
              { Allow alternative screens }
  if (FormMMIFrame.AltScreens > 0) then
  begin

    if (EventInfo.GestureID = sgiRight) then
    begin
      if FormViewLog.Visible then
        FormViewLog.Hide
      else
      begin
        FormViewLog.LogRoot := FormMMIFrame.LogPath;
        FormViewLog.Show;
      end;
    end;
  end;

end;

{ *********************************************************
* Function:    UpdateETA
* Description: Update time-field
********************************************************* }
procedure TFormMainLayer1.UpdateETA(Time: TDateTime);
var
  Hour, Min, Sec, MSec: Word;
begin
  try
    if not ShowETADate then
    begin
      DecodeTime(Time, Hour, Min, Sec, MSec);
      LabelETA.Caption := FormatDateTime('hh:nn', Time)
    end
    else
        LabelETA.Caption := FormatDateTime('d mmm', Time);
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
        MainLogErrorUpdateTime);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in UpdateTime:' + E.Message),
          PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

procedure TFormMainLayer1.LabelETAClick(Sender: TObject);
  var
  ETADateTime : TDateTime;
  ETA : LongInt;
  ETAChanged : Boolean;
begin
  try
    ETA := DataModuleDMI.GetETA(ETAChanged);
    if ETA <> 0 then
    begin
      ETADateTime := UnixToDelphiTime(ETA);
      ShowETADate := not ShowETADate;
      UpdateETA(ETADateTime);
      if FormMMIFrame.Sound then
        Beep;
    end;
  except
    on E: Exception do
    begin
        FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
        MainLogErrorTimeFieldClick);
        if FormMMIFrame.TestMode then
          MessageBox(Handle, PWideChar('Error in TimeFieldClick:' + E.Message),
            PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;


end.
