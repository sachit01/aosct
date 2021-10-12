unit UnitMainLayer2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnitMainLayer1, ImgList, Menus, DB, ExtCtrls, StdCtrls, Buttons,
  Grids, DBGrids, ColorButton2, MMIPanel, GestureMgr;

type

  TBrakeGlyphActive = (bgUndefined, bgBlank, bgBrake);

  TBrakeTestGlyphActive = (bgBrakeTestUndefined, bgBrakeTestPossible,
    bgBrakeTestNotification, bgBrakeTestMandatory, bgBrakeTestAbort);

  TBTMTestGlyphActive = (btmgBTMTestUndefined, btmgBTMTestPossible,
    btmgBTMTestNotification, btmgBTMTestMandatory);

  TTIMSGlyphActive = (timsgUndefined,timgInhibitIntegrity,timsgConfirmIntegrity);


  TFormMainLayer2 = class(TFormMainLayer1)
    EButtons: TMMIPanel;
    Brake: TSpeedButton;
    BrakeIntervention: TSpeedButton;
    IntegrityAlert: TSpeedButton;
    EAlert: TSpeedButton;
    BrakeTest: TSpeedButton;
    ResumeSupervision: TSpeedButton;
    BTMTest: TSpeedButton;
    MMIPanelSystemArea2: TMMIPanel;
    MMIPanelE26: TMMIPanel;
    MMIPanelE27: TMMIPanel;
    MMIPanelE28: TMMIPanel;
    MMIPanelE29: TMMIPanel;
    SystemIconChangeMode: TImage;
    MMIPanelE30: TMMIPanel;
    SystemIconATOMode: TImage;
    SystemIconTrainLoadStatus : TImage;
    MMIPanelE31: TMMIPanel;
    MMIPanelE32: TMMIPanel;
    MMIPanelE12: TMMIPanel;
    LabeledEditBrakeTestNotification: TLabeledEdit;
    MMIPanelE13: TMMIPanel;
    LabeledEditBTMTestNotification: TLabeledEdit;
    TimerFlash: TTimer;
    IntegrityIndicationIcon: TImage;
    SystemIconBrakeSystemInUse: TImage;
    SystemIconStandStill: TImage;
    SystemIconLcsCommStatus: TImage;
    SystemIconOdometerFailureStatus: TImage;
    procedure BrakeInterventionClick(Sender: TObject);
    procedure IntegrityAlertClick(Sender: TObject);
    procedure BrakeClick(Sender: TObject);
    procedure BrakeTestClick(Sender: TObject);
    procedure BTMTestClick(Sender: TObject);
    procedure TimerFlashTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DataSourceEventsDataChange(Sender: TObject; Field: TField);
    procedure ResumeSupervisionClick(Sender: TObject);

  private
    { Private declarations }
    SystemIconWantedMode: TBitmap;
    SystemIconWantedModeDisplayed: Boolean;
    SystemIconATOUndefined: TBitmap;
    SystemIconATOManual: TBitmap;
    SystemIconATOSupervised: TBitmap;
    SystemIconATOAutomatic: TBitmap;
    SystemIconATORemote: TBitmap;
    SystemIconTrainLoaded : TBitmap;
    SystemIconBrakeSystemECPB : TBitmap;
    SystemIconBrakeSystemPB   : TBitmap;
    SystemIconLCSDisconnected : TBitmap;
    SystemIconOdometerTachoFailure : TBitmap;
    SystemIconOdometerDopplerFailure : TBitmap;
    ActiveSBrakeGlyph: TBrakeGlyphActive;
    ActiveEBrakeGlyph: TBrakeGlyphActive;
    ActiveBrakeTestGlyph: TBrakeTestGlyphActive;
    ActiveBTMTestGlyph: TBTMTestGlyphActive;
    ActiveTimsGlyph : TTIMSGlyphActive;
    FFlashBoolean: ByteBool;
    FControlToFlash: TControl;
    procedure FlashBrakeIntervention(Blank: Boolean; Sender: TControl);
    procedure UpdateEventButtons(StatusD11 : Byte);
    function TimeToTestString(RemainingTimeToTest : Word) : String;

  protected
    FlashTick : Byte;
    procedure FlashControl(Sender: TControl);

  public
    { Public declarations }

    procedure UpdateDMI(Refresh : Boolean);

  end;

var
  FormMainLayer2: TFormMainLayer2;

implementation

uses UnitMMIFrame, UnitConfirmBrakeTest, UnitAtpMessages, MMIareaD, MMITypes,
  UnitDMIDataModule, UnitExchange, MMIStd, UnitConfirmDepartureTest, UnitConfirmFreeRollingClear,
  UnitConfirmMAAccept,UnitAutomaticConfiguration , UnitConfirmTrainIntegrity,
  UnitAbortLastCarBrakeTest, UnitConfirmAbortLastCarBrakeTest , UnitConfirmTachometer2Failure,
  UnitConfirmTachometer1Failure, UnitConfirmDopplerRadarFailure;

procedure TFormMainLayer2.FormCreate(Sender: TObject);
var
  ImagePanelWidth : Word;
  ImagePanelHeight : Word;
  ImageLoadStatusWidth : Word;
  ImageLoadStatusHeight : Word;
  ImageStandStillWidth : Word;
  ImageStandStillHeight: Word;
  ImageMMIPanelE32Width : Word;
  ImageMMIPanelE32Height : Word;

begin
  inherited;
      { Configurable ATO mode icons }
  SystemIconATOUndefined := TBitmap.Create;
  SystemIconATOManual := TBitmap.Create;
  SystemIconATOSupervised := TBitmap.Create;
  SystemIconATOAutomatic := TBitmap.Create;
  SystemIconATORemote := TBitmap.Create;
  SystemIconTrainLoaded := TBitmap.Create;
  SystemIconBrakeSystemECPB := TBitmap.Create;
  SystemIconBrakeSystemPB   := TBitmap.Create;
  SystemIconLCSDisconnected := TBitmap.Create;
  SystemIconOdometerTachoFailure:= TBitmap.Create;
  SystemIconOdometerDopplerFailure := TBitmap.Create;

  LoadBitmapImage(SystemIconATOUndefined, 'ATO_UNDEFINED',
      ImageATOUndefined, 'MAIN');
  LoadBitmapImage(SystemIconATOManual, FormMMIFrame.HiRes, 'ATO_MANUAL',
      'HIRES_ATO_MANUAL', ImageATOManual, 'MAIN');
  LoadBitmapImage(SystemIconATOSupervised, FormMMIFrame.HiRes,
      'ATO_SUPERVISED', 'HIRES_ATO_SUPERVISED', ImageATOSupervised, 'MAIN');
  LoadBitmapImage(SystemIconATOAutomatic, FormMMIFrame.HiRes, 'ATO_AUTOMATIC',
      'HIRES_ATO_AUTOMATIC', ImageATOAutomatic, 'MAIN');
  LoadBitmapImage(SystemIconATORemote, FormMMIFrame.HiRes, 'ATO_REMOTE',
      'HIRES_ATO_REMOTE', ImageATORemote, 'MAIN');
  LoadBitmapImage(SystemIconTrainLoaded, FormMMIFrame.HiRes, 'TRAIN_LOADED',
      'TRAIN_LOADED', ImageTrainLoaded, 'MAIN');
  LoadBitmapImage(SystemIconBrakeSystemECPB,FormMMIFrame.HiRes,'BRAKESYSTEM_ECPB',
       'BRAKESYSTEM_ECPB',ImageBrakeSystemECPB,'MAIN');
  LoadBitmapImage(SystemIconBrakeSystemPB,FormMMIFrame.HiRes,'BRAKESYSTEM_PB',
       'BRAKESYSTEM_PB',ImageBrakeSystemPB,'MAIN');
  LoadBitmapImage(SystemIconLCSDisconnected,FormMMIFrame.HiRes,'LCS_COMM_DISCONNECTED',
         'LCS_COMM_DISCONNECTED',ImageLCSCommDisconnected,'MAIN');
  LoadBitmapImage(SystemIconOdometerTachoFailure,FormMMIFrame.HiRes,'TACHOMETER_FAILURE',
         'TACHOMETER_FAILURE',ImageOdometerFailure,'MAIN');
  LoadBitmapImage(SystemIconOdometerDopplerFailure,FormMMIFrame.HiRes,'DOPPLER_FAILURE',
          'DOPPLER_FAILURE',ImageOdometerFailure,'MAIN');


  MMIPanelSystemArea2.Width := (GetXpos3 - GetXpos2) + 54;
  MMIPanelSystemArea2.Height := (GetYpos3 - GetYpos1) div 2;
  MMIPanelSystemArea2.Top := GetYpos1 + MMIPanelSystemArea2.Height;
  MMIPanelSystemArea2.Left := GetXpos2;

  AdjustControl(MMIPanelE26);
  AdjustControl(MMIPanelE27);
  AdjustControl(MMIPanelE28);
  AdjustControl(MMIPanelE29);
  AdjustControl(MMIPanelE30);
  AdjustControl(MMIPanelE31);
  SystemIconWantedMode := TBitmap.Create;
  SystemIconWantedModeDisplayed := False;
  SystemIconATOMode.Picture.Bitmap := SystemIconATOUndefined;
  AdjustImage(SystemIconATOMode);
  AdjustImage(SystemIconTrainLoadStatus);
  AdjustImage(SystemIconLcsCommStatus);
  AdjustImage(SystemIconOdometerFailureStatus);

  ImagePanelWidth := MMIPanelE26.Width;
  ImagePanelHeight := MMIPanelE26.Height;
  ImageLoadStatusWidth := MMIPanelE31.Width;
  ImageLoadStatusHeight := MMIPanelE31.Height;
  ImageStandStillWidth := MMIPanelE28.Width;
  ImageStandStillHeight := MMIPanelE28.Height;
  ImageMMIPanelE32Width :=   MMIPanelE32.Width;
  ImageMMIPanelE32Height := MMIPanelE32.Height;

  CentreImage(IntegrityIndicationIcon, ImagePanelWidth, ImagePanelHeight);
  CentreImage(SystemIconATOMode, ImagePanelWidth, ImagePanelHeight);
  CentreImage(SystemIconTrainLoadStatus,ImageLoadStatusWidth,ImageLoadStatusHeight);
  CentreImage(SystemIconStandStill, ImageStandStillWidth,ImageStandStillHeight);
  CentreImage(SystemIconOdometerFailureStatus,ImageMMIPanelE32Width,ImageMMIPanelE32Height, true);
  CentreImage(SystemIconLcsCommStatus, ImageMMIPanelE32Width, ImageMMIPanelE32Height,false,true);
            // No image assigned yet. Centre it later when image assigned
          // CentreImage(SystemIconChangeMode, ImagePanelWidth, ImagePanelHeight);

  ActiveBrakeTestGlyph := bgBrakeTestUndefined;
  ActiveBTMTestGlyph := btmgBTMTestUndefined;
  ActiveTimsGlyph := timgInhibitIntegrity;

  IntegrityAlert.NumGlyphs := 2;
  EAlert.NumGlyphs := 2;
  BrakeTest.NumGlyphs := 2;
  ResumeSupervision.NumGlyphs := 2;
  BTMTest.NumGlyphs := 2;

  ActiveEBrakeGlyph := bgUndefined;
  ActiveSBrakeGlyph := bgUndefined;

  EButtons.Width := GetXpos1;
  EButtons.Height := FormMMIFrame.ClientHeight - GetYpos1;
  EButtons.Top := GetYpos1;

  AdjustSpeedButton(Brake); // This is indication for emergency brake
  LoadBitmapFromResource(Brake.Glyph, FormMMIFrame.HiRes, 'EMERGENCYBRAKE',
      'HIRES_EMERGENCYBRAKE');

  AdjustSpeedButton(BrakeIntervention);
    // This is indication for service brake
  LoadBitmapFromResource(BrakeIntervention.Glyph, FormMMIFrame.HiRes,
      'SERVICEBRAKE', 'HIRES_SERVICEBRAKE');

  AdjustSpeedButton(IntegrityAlert);

  AdjustSpeedButton(EAlert);
  LoadBitmapFromResource(EAlert.Glyph, FormMMIFrame.HiRes, 'EMERGENCYALERT',
      'HIRES_EMERGENCYALERT');

  AdjustSpeedButton(BrakeTest);
  AdjustSpeedButton(BTMTest);

  AdjustSpeedButton(ResumeSupervision);
  LoadBitmapFromResource(ResumeSupervision.Glyph, FormMMIFrame.HiRes, 'RESUME_SUPERVISION',
      'RESUME_SUPERVISION');

  AdjustControl(MMIPanelE12);
  AdjustControl(MMIPanelE13);
  AdjustLabeledEdit(LabeledEditBrakeTestNotification);
  AdjustLabeledEdit(LabeledEditBTMTestNotification);

  FlashTick := 0;
  FControlToFlash := Nil;
end;

procedure TFormMainLayer2.FormShow(Sender: TObject);
begin
  inherited;
  TimerFlash.Enabled := true;
end;

{ **********************************************************
* IntegrityAlertClick
* Description: Sends a message about tic inhibit to The ATP.
********************************************************* }
procedure TFormMainLayer2.IntegrityAlertClick(Sender: TObject);
begin
  begin
    if (ActiveTimsGlyph = timgInhibitIntegrity) then
    begin

      Exchange.SendMMIButton(bsTIMSInhibit);
    end;
    if (ActiveTimsGlyph = timsgConfirmIntegrity) then
    begin
      Exchange.SendMMIButton(bsManualIntegrity);
    end;

    if FormMMIFrame.Sound then

      Beep;
  end;

end;

procedure TFormMainLayer2.ResumeSupervisionClick(Sender: TObject);
begin
  inherited;
  Exchange.SendMMIButton(bsTIMSResume);
end;

procedure TFormMainLayer2.TimerFlashTimer(Sender: TObject);
begin
  inherited;
                        // Disable timer and reenable at the end of this function
  TimerFlash.Enabled := False;

  try
    FlashTick := (FlashTick + 1) mod 4;
    case FlashTick of
      0:
        begin

          FormAtpMessages.MsgTic();
                        // Flash any control ?
          if FFlashBoolean then
          begin
            if not((FControlToFlash = BrakeIntervention) or
              (FControlToFlash = Brake)) then
              FlashControl(FControlToFlash)
            else
              FlashBrakeIntervention(true, FControlToFlash);

          end
          else
          begin // This is to ensure that the right glyph is loaded
                      // when not flashing!
            FlashBrakeIntervention(False, BrakeIntervention);
            FlashBrakeIntervention(False, Brake);
          end;

// Load bitmap
          if SystemIconChangeMode.Enabled then
          begin
            if SystemIconWantedModeDisplayed then
            begin
              SystemIconChangeMode.Picture.Bitmap.LoadFromResourceName
                (Hinstance, 'SWITCH');
              SystemIconWantedModeDisplayed := False;
            end
            else
            begin
              SystemIconChangeMode.Picture.Bitmap := SystemIconWantedMode;
              SystemIconWantedModeDisplayed := true;
            end;
            CentreImage(SystemIconChangeMode, MMIPanelE29.Width,
              MMIPanelE29.Height);
          end;

        end;

      2:
        begin
           if FFlashBoolean then
          begin
            if not((FControlToFlash = BrakeIntervention) or
              (FControlToFlash = Brake)) then
            begin
              FlashControl(FControlToFlash);
              Dec(FFlashBoolean);
            end
            else
              FlashBrakeIntervention(False, FControlToFlash);
          end;
        end;
    else
    end;

  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
        MainLogErrorTIMEREvent);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in TIMEREvent:' + E.Message),
          PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);

    end;
  end;

// Application.ProcessMessages ;

  TimerFlash.Enabled := true;

end;

{ *********************************************************
* Function:    UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormMainLayer2.UpdateDMI(Refresh : Boolean);
var
  DriverInfo : TDriverInfoMT;
  DriverInfoChanged : Boolean;

  ATOMode : T_ATOModes;
  ATOModeChanged : Boolean;

  ATOSwitch : T_ATOSwitch;
  ATOSwitchChanged : Boolean;

  LocoStatus : LongWord;
  LocoStatusChanged : Boolean;

  AdditionalStatus1 : Byte;
  AdditionalStatus1Changed : Boolean;

  BrakeSystemType : Byte;
  AdditionalStatus2 : Byte;
  AdditionalStatus2Changed : Boolean;

  BrakeTestStatus : T_BrakeTestStatus;
  BrakeTestStatusChanged : Boolean;

  RemainingTimeToBrakeTest : Word;
  RemainingTimeToBrakeTestChanged : Boolean;

  RemainingTimeToBTMTest : Word;
  RemainingTimeToBTMTestChanged : Boolean;

  BrakeTestAllowed   : Boolean;
  BTMTestAllowed   : Boolean;

  AdditionalConfirmationInfo : Byte;
  AdditionalConfirmationInfoChanged : Boolean;

  PlatformStatusInfo : Byte;
  PlatformStatusInfoChanged : Boolean;

  AdditionalAllowedToInfo : Byte;
  AdditionalAllowedToInfoChanged : Boolean;

  AdditionalConfirmationInfo2 : Byte;
  AdditionalConfirmationInfo2Changed : Boolean;

begin
  inherited;

      {Driver info}
  DriverInfo := DataModuleDMI.GetDriverInfo(DriverInfoChanged);
  if Refresh or DriverInfoChanged then
  begin
    UpdateEventButtons(DriverInfo.StatusD11);
  end;

      {ATO Mode}
  ATOMode := DataModuleDMI.GetATOMode(ATOModeChanged);
  if Refresh or ATOModeChanged then
  begin
    if ATOMode = atomUndefined then
      SystemIconATOMode.Picture.Bitmap := SystemIconATOUndefined;
    if ATOMode = atomManual then
      SystemIconATOMode.Picture.Bitmap := SystemIconATOManual;
    if ATOMode = atomSupervised then
      SystemIconATOMode.Picture.Bitmap := SystemIconATOSupervised;
    if ATOMode = atomAutomatic then
      SystemIconATOMode.Picture.Bitmap := SystemIconATOAutomatic;
    if ATOMode = atomRemote then
      SystemIconATOMode.Picture.Bitmap := SystemIconATORemote;
    SystemIconATOMode.Show;
  end;

      { ATO Switch }
  ATOSwitch := DataModuleDMI.GetATOSwitch(ATOSwitchChanged);
  if Refresh or ATOSwitchChanged then
  begin
    case ATOSwitch of
      ATOSwitch_Manual:
        begin
          SystemIconWantedMode := SystemIconATOManual;
          SystemIconChangeMode.Picture.Bitmap := SystemIconWantedMode;
          SystemIconChangeMode.Enabled := true;
          CentreImage(SystemIconChangeMode, MMIPanelE29.Width, MMIPanelE29.Height);
          SystemIconChangeMode.Show;
        end;
      ATOSwitch_SupervisedAutomatic:
        begin
          SystemIconWantedMode := SystemIconATOSupervised;
          SystemIconChangeMode.Picture.Bitmap := SystemIconWantedMode;
          SystemIconChangeMode.Enabled := true;
          CentreImage(SystemIconChangeMode, MMIPanelE29.Width, MMIPanelE29.Height);
          SystemIconChangeMode.Show;
        end;
      ATOSwitch_Automatic:
        begin
          SystemIconWantedMode := SystemIconATOAutomatic;
          SystemIconChangeMode.Picture.Bitmap := SystemIconWantedMode;
          SystemIconChangeMode.Enabled := true;
          CentreImage(SystemIconChangeMode, MMIPanelE29.Width, MMIPanelE29.Height);
          SystemIconChangeMode.Show;
        end;
    else
      begin
        SystemIconChangeMode.Enabled := False;
        SystemIconChangeMode.Hide;
      end;
    end;
  end;

    { Loco Status }
  LocoStatus := DataModuleDMI.GetLocoStatus(LocoStatusChanged);
  if Refresh or LocoStatusChanged then
  begin
    // Emergency alert.
    if LongBool(LocoStatus and LS_EmergencyAlertFromDriver) or
      LongBool(LocoStatus and LS_EmergencyAlertActive) then
      EAlert.Show
    else
      EAlert.Hide;

    //Integirty Status
    if LongBool(LocoStatus and LS_TIMS_IntegrityBroken) then
    begin
      IntegrityIndicationIcon.Enabled := true;
      IntegrityIndicationIcon.Show;
    end
    else
    begin
      IntegrityIndicationIcon.Enabled := False;
      IntegrityIndicationIcon.Hide;
    end;

  end;

      // Additional status 1
  AdditionalStatus1 := DataModuleDMI.GetAdditionalStatus1(AdditionalStatus1Changed);
      // BrakeTestStatus
  BrakeTestStatus := DataModuleDMI.GetBrakeTestStatus(BrakeTestStatusChanged);
      // RemainingTimeToBrakeTest
  RemainingTimeToBrakeTest := DataModuleDMI.GetRemainingTimeToBrakeTest(RemainingTimeToBrakeTestChanged);

  if Refresh or AdditionalStatus1Changed or BrakeTestStatusChanged or RemainingTimeToBrakeTestChanged then
  begin

    if ByteBool(AdditionalStatus1 AND AS_StandStillEvent) then
    begin
      SystemIconStandStill.Enabled := true;
      SystemIconStandStill.Show;
    end
    else
    begin
      SystemIconStandStill.Hide;
    end;

   {Button enabled for brake-test?}
    BrakeTestAllowed := false;

    if ByteBool(AdditionalStatus1 AND AS_BrakeTestPossible) then
    begin
          {Only load glyph when necessary}
      if (ActiveBrakeTestGlyph <> bgBrakeTestPossible)
      then
      begin
        BrakeTest.Glyph.LoadFromResourceName(HInstance,'BRAKETEST_POSSIBLE');
        ActiveBrakeTestGlyph := bgBrakeTestPossible;
        BrakeTest.Enabled := true;
      end;
          {Only call Show / Hide when necessary}
      BrakeTest.Show;
      BrakeTestAllowed := True;
    end;

    if ByteBool(AdditionalStatus1 AND AS_BrakeTestNotification) then
    begin
           {Only show indication when Brake Test is possible}
      if ByteBool(AdditionalStatus1 AND AS_BrakeTestPossible) then
      begin

          {Only load glyph when necessary}
        if (ActiveBrakeTestGlyph <> bgBrakeTestNotification)
        then
        begin
          BrakeTest.Glyph.LoadFromResourceName(HInstance,'BRAKETEST_NOTIFICATION');
          ActiveBrakeTestGlyph := bgBrakeTestNotification;
          BrakeTest.Enabled := true;
        end;
      end;

      LabeledEditBrakeTestNotification.Text := TimeToTestString(RemainingTimeToBrakeTest);
      LabeledEditBrakeTestNotification.Show;
      LabeledEditBrakeTestNotification.EditLabel.Show;
    end
    else
    begin
      LabeledEditBrakeTestNotification.Hide;
      LabeledEditBrakeTestNotification.EditLabel.Hide;
    end;

    if ByteBool(AdditionalStatus1 AND AS_BrakeTestmandatory) then
    begin
        {Only load glyph when necessary}
      if (ActiveBrakeTestGlyph <> bgBrakeTestMandatory)
      then
      begin
        BrakeTest.Glyph.LoadFromResourceName(HInstance,'BRAKETEST_MANDATORY');
        ActiveBrakeTestGlyph := bgBrakeTestMandatory;
        BrakeTest.Enabled := true;
      end;
      BrakeTest.Show;
      BrakeTestAllowed := True;
    end;

    if (BrakeTestStatus = btsInProgress) then
    begin
        {Only load glyph when necessary}
      if (ActiveBrakeTestGlyph <> bgBrakeTestAbort)
      then
      begin
        BrakeTest.Glyph.LoadFromResourceName(HInstance,'BRAKETEST_ABORT');
        ActiveBrakeTestGlyph := bgBrakeTestAbort;
        BrakeTest.Enabled := true;
      end;
      BrakeTest.Show;
    end
    else
    begin
      if (not BrakeTestAllowed) then
      begin
        BrakeTest.Hide;
        {To hide the Confirm Brake Test in case Driver logged out}
        FormConfirmBrakeTest.Hide;
      end;
    end;

    {Train Loaded}
    if (ByteBool(AdditionalStatus1 AND TRAIN_LOADED)) then
    begin
       SystemIconTrainLoadStatus.Picture.Bitmap := SystemIconTrainLoaded;
       SystemIconTrainLoadStatus.Show;
    end
    else
       SystemIconTrainLoadStatus.Visible := false;

end;

      // Additional status 2
  AdditionalStatus2 := DataModuleDMI.GetAdditionalStatus2(AdditionalStatus2Changed);
      // RemainingTimeToBTMTest
  RemainingTimeToBTMTest := DataModuleDMI.GetRemainingTimeToBTMTest(RemainingTimeToBTMTestChanged);
  if Refresh or AdditionalStatus2Changed or RemainingTimeToBTMTestChanged then
  begin

   // ATO <-> LCS Comm.status occupy only bits 0,1 but the rest are unused.
   if not (ByteBool(AdditionalStatus2 AND AS2_ATOLCSStatus)) then
   begin
      SystemIconLcsCommStatus.Picture.Bitmap := SystemIconLCSDisconnected;
      SystemIconLcsCommStatus.Visible := true;
   end  //Brake Type
   else
      SystemIconLcsCommStatus.Visible := false;

   //Brake Sytem in Use;
   BrakeSystemType := AdditionalStatus2 AND AS2_BrakeSystemType3;

   if (BrakeSystemType = AS2_BrakeSystemType3) then
   begin
      //Handling of brake system type 3
      //Currently not handle in this project
      SystemIconBrakeSystemInUse.Picture.Bitmap := SystemIconBrakeSystemECPB;
      SystemIconBrakeSystemInUse.Visible := true;
    end
   else if (BrakeSystemType = AS2_BrakeSystemECPB) then
   begin
      SystemIconBrakeSystemInUse.Picture.Bitmap := SystemIconBrakeSystemECPB;
      SystemIconBrakeSystemInUse.Visible := true;
    end
   else if (BrakeSystemType =  AS2_BrakeSystemPB)then
   begin
      SystemIconBrakeSystemInUse.Picture.Bitmap := SystemIconBrakeSystemPB;
      SystemIconBrakeSystemInUse.Visible := true;
   end
   else
     SystemIconBrakeSystemInUse.Visible := false;

        {Button enabled for BTM-test?}
    BTMTestAllowed := false;

    if ByteBool(AdditionalStatus2 AND AS2_BTMTestPossible) then
    begin
          {Only load glyph when necessary}
      if (ActiveBTMTestGlyph <> btmgBTMTestPossible)
      then
      begin
        BTMTest.Glyph.LoadFromResourceName(HInstance,'BTMTEST_POSSIBLE');
        ActiveBTMTestGlyph := btmgBTMTestPossible;
        BTMTest.Enabled := true;
      end;
      BTMTest.Show;
      BTMTestAllowed := True;

    end;

    if ByteBool(AdditionalStatus2 AND AS2_BTMTestNotification) then
    begin

           {Only show indication when BTM Test is possible}
      if ByteBool(AdditionalStatus2 AND AS2_BTMTestPossible) then
      begin

          {Only load glyph when necessary}
        if (ActiveBTMTestGlyph <> btmgBTMTestNotification)
        then
        begin
          BTMTest.Glyph.LoadFromResourceName(HInstance,'BTMTEST_NOTIFICATION');
          ActiveBTMTestGlyph := btmgBTMTestNotification;
          BTMTest.Enabled := true;
        end;
      end;

         {Display remaining time to BTM Test}
      LabeledEditBTMTestNotification.Text := TimeToTestString(RemainingTimeToBTMTest);
      LabeledEditBTMTestNotification.Show;
      LabeledEditBTMTestNotification.EditLabel.Show;
    end
    else
    begin
      LabeledEditBTMTestNotification.Hide;
      LabeledEditBTMTestNotification.EditLabel.Hide;
    end;

    if ByteBool(AdditionalStatus2 AND AS2_BTMTestmandatory) then
    begin
        {Only load glyph when necessary}
      if (ActiveBTMTestGlyph <> btmgBTMTestMandatory)
      then
      begin
        BTMTest.Glyph.LoadFromResourceName(HInstance,'BTMTEST_MANDATORY');
        ActiveBTMTestGlyph := btmgBTMTestMandatory;
        BTMTest.Enabled := true;
      end;
    end;

    if (not BTMTestAllowed) then
      begin
        BTMTest.Hide;
      end;

  end;

  AdditionalConfirmationInfo2 :=
    DataModuleDMI.GetAdditionalConfirmationInfo2
    (AdditionalConfirmationInfo2Changed);

  if Refresh or AdditionalConfirmationInfo2Changed then
  begin
    { Tachometer 1 Failure Confirmation }
    if (ByteBool(AdditionalConfirmationInfo2 and
      CONFIRM_TACHOMETER1_FAILURE)) then
      FormConfirmTachometer1Failure.Show
    else
      FormConfirmTachometer1Failure.Hide;

    { Tachometer 2 Failure Confirmation }
    if (ByteBool(AdditionalConfirmationInfo2 and
      CONFIRM_TACHOMETER2_FAILURE)) then
      FormConfirmTachometer2Failure.Show
    else
      FormConfirmTachometer2Failure.Hide;

    { Doppler radar Failure Confirmation }
    if (ByteBool(AdditionalConfirmationInfo2 and
      CONFIRM_DOPPLER_FAILURE)) then
      FormConfirmDopplerRadarFailure.Show
    else
      FormConfirmDopplerRadarFailure.Hide;
  end;

  PlatformStatusInfo := DataModuleDMI.GetPlatformStatusInfo
    (PlatformStatusInfoChanged);

  if Refresh or PlatformStatusInfoChanged then
  begin

    if (ByteBool(PlatformStatusInfo and DOPPPLER_FAILURE)) then
    begin
      SystemIconOdometerFailureStatus.Picture.Bitmap :=
        SystemIconOdometerDopplerFailure;
      SystemIconOdometerFailureStatus.Visible := True;
    end;

    if (ByteBool(PlatformStatusInfo and TACHOMETER1_FAILURE)) then
    begin
      SystemIconOdometerFailureStatus.Picture.Bitmap :=
        SystemIconOdometerTachoFailure;
      SystemIconOdometerFailureStatus.Visible := True;
    end;

    if (ByteBool(PlatformStatusInfo and TACHOMETER2_FAILURE)) then
    begin
      SystemIconOdometerFailureStatus.Picture.Bitmap :=
        SystemIconOdometerTachoFailure;
      SystemIconOdometerFailureStatus.Visible := True;
    end;

    if Not
      (ByteBool(PlatformStatusInfo and TACHOMETER1_FAILURE) or
      ByteBool(PlatformStatusInfo and TACHOMETER2_FAILURE) or
      ByteBool(PlatformStatusInfo and DOPPPLER_FAILURE)) then
      SystemIconOdometerFailureStatus.Visible := False;

  end;


  AdditionalConfirmationInfo := DataModuleDMI.GetAdditionalConfirmationInfo
    (AdditionalConfirmationInfoChanged);
  if Refresh or AdditionalConfirmationInfoChanged then
  begin

       { Departure Test confirmation }
    if (ByteBool(AdditionalConfirmationInfo and CONFIRM_DEPARTURE_TEST)) then
      FormConfirmDepartureTest.Show
    else
      FormConfirmDepartureTest.Hide;

      { Free Rolling Clearconfirmation }
    if (ByteBool(AdditionalConfirmationInfo and
      CONFIRM_FREE_ROLLING_CLEARED)) then
      FormConfirmFreeRollingClear.Show
    else
      FormConfirmFreeRollingClear.Hide;

{ Confirm Staff Responsible MA }
    if (ByteBool(AdditionalConfirmationInfo and CONFIRM_STAFF_RESP_MA)) then
    begin
      FormConfirmMAAccept.LabeledEditMAConfirm.Text :=
        InterfaceString(TIfaceEnum(ord(amStaffResponsible)));
      FormConfirmMAAccept.thisButton := bsConfirmStaffRespMAAccept;
      FormConfirmMAAccept.Show;
    end;

    { Confirm Shunting Route MA }
    if (ByteBool(AdditionalConfirmationInfo and CONFIRM_SHUNITNG_ROUTE_MA)) then
    begin
      FormConfirmMAAccept.LabeledEditMAConfirm.Text :=
        InterfaceString(TIfaceEnum(ord(amShuntingRoute)));
      FormConfirmMAAccept.thisButton := bsConfirmShuntRouteMAAccept;
      FormConfirmMAAccept.Show;
    end;

    { Confirm Join MA }
    if (ByteBool(AdditionalConfirmationInfo and CONFIRM_JOIN_MA)) then
    begin
      FormConfirmMAAccept.LabeledEditMAConfirm.Text :=
        InterfaceString(TIfaceEnum(ord(amJoin)));
      FormConfirmMAAccept.thisButton := bsConfirmJoinMAAccept;
      FormConfirmMAAccept.Show;
    end;

    { Confirm Confirm TIMS INTEGRITY }
    if (ByteBool(AdditionalConfirmationInfo and
      CONFIRM_CONFIRM_TIMS_INTEGRITY)) then
    begin
      FormConfirmTrainIntegrity.Show;
    end
    else
      FormConfirmTrainIntegrity.Hide;

    if (not(ByteBool(AdditionalConfirmationInfo and CONFIRM_JOIN_MA))) and
      (not(ByteBool(AdditionalConfirmationInfo and CONFIRM_SHUNITNG_ROUTE_MA)))
      and (not(ByteBool(AdditionalConfirmationInfo and
      CONFIRM_STAFF_RESP_MA))) then
      FormConfirmMAAccept.Hide;
  end;

  // AdditionalConfirmationInfoChanged and  AdditionalStatus2Changed are already read
  if Refresh or AdditionalConfirmationInfoChanged or
    AdditionalStatus2Changed then
  begin

    // Abort last Car Brake Test in Progress
    if ByteBool(AdditionalStatus2 AND AS2_LastCarBrakeTest) then
    begin
      FormAbortlastCarBrakeTest.Show;
    end
    else
    begin
      FormAbortlastCarBrakeTest.Hide;
    end;

    if ByteBool(AdditionalConfirmationInfo AND
      CONFIRM_ABORT_LAST_CAR_BRAKE_TEST) then
    begin
      FormConfirmAbortLastCarBrake.Show;
      FormAbortlastCarBrakeTest.Hide;
    end
    else
    begin
      FormConfirmAbortLastCarBrake.Hide;
    end;
  end;

  //Check for train integrity inhibit supervision
  AdditionalAllowedToInfo :=  DataModuleDMI.GetAdditionalAllowedToInfo(AdditionalAllowedToInfoChanged);
  if Refresh or AdditionalAllowedToInfoChanged then
  begin
    //Check to resume the train integrity supervision
    if ByteBool(AdditionalAllowedToInfo and ALLOWED_TO_RESUME_TRAIN_INTEGRITY) then
    begin
      ResumeSupervision.Show;
      ResumeSupervision.Enabled := true;
    end
    else
    begin
      ResumeSupervision.Hide;
      ResumeSupervision.Enabled := true;
    end;
  end;

  if Refresh or LocoStatusChanged or AdditionalAllowedToInfoChanged then
  begin

    if LongBool(LocoStatus and LS_TIMS_DisablebyDriver) or
      ByteBool(AdditionalAllowedToInfo and
      ALLOWED_TO_INHIBIT_TRAIN_INTEGRITY) then
    begin

      if LongBool(LocoStatus and LS_TIMS_DisablebyDriver) then
      begin
        ActiveTimsGlyph := timsgConfirmIntegrity;
        IntegrityAlert.Glyph.LoadFromResourceName(Hinstance,
          'CONFIRMINTEGRITY');
      end
      else
      begin
        if ByteBool(AdditionalAllowedToInfo and
          ALLOWED_TO_INHIBIT_TRAIN_INTEGRITY) then
        begin
          ActiveTimsGlyph := timgInhibitIntegrity;
          IntegrityAlert.Glyph.LoadFromResourceName(Hinstance,
            'INTEGRITYALERT');
        end;
      end;
      IntegrityAlert.Show;
      IntegrityAlert.Enabled := true;
    end
    else
    begin
      IntegrityAlert.Hide;
      IntegrityAlert.Enabled := False;
    end;
  end;

end;

{ **********************************************************
  * TimeToTestString
  * Description: Create a string in the format HH:MM based on
  * a number of minutes.
  ********************************************************* }
function TFormMainLayer2.TimeToTestString(RemainingTimeToTest: Word): String;
var
  HoursToTest: Word;
  MinutesToTest: Word;
  TotalMinutesToTest: Word;
  TimeStringToTest: String;
begin

  TotalMinutesToTest := RemainingTimeToTest;
  HoursToTest        := TotalMinutesToTest Div 60;
  MinutesToTest      := TotalMinutesToTest Mod 60;
  TimeStringToTest   := LeftPad(HoursToTest,2) + ':'
                               + LeftPad(MinutesToTest,2);
  Result := TimeStringToTest;
end;

{**********************************************************
* UpdateEventButtons
* Description: Update the visibility of the buttons in the
*              EventButtons - group
********************************************************* }
procedure TFormMainLayer2.UpdateEventButtons(StatusD11 : Byte);
begin
       // Service brake indication
  if ByteBool(StatusD11 and SERVICEBRAKEAREAPPLIED) then
    BrakeIntervention.Show
  else
    BrakeIntervention.Hide;

       // Emergency brake indication
  if ByteBool(StatusD11 and EMERGENCYBRAKEAREAPPLIED) then
    Brake.Show
  else
    Brake.Hide;

       // Flashing of brake buttons
  if ByteBool(StatusD11 and FLASHEBBRAKEBUTTON) then
  begin
    FControlToFlash := Brake ;
    FFlashBoolean := True ;
    Brake.Enabled:=True;
  end
  else
  begin
    Brake.Enabled:=False;
    if ByteBool(StatusD11 and FLASHSBBRAKEBUTTON) then
    begin
      FControlToFlash := BrakeIntervention ;
      FFlashBoolean := True ;
      BrakeIntervention.Enabled:=True;
    end
    else
    begin
      FControlToFlash := Nil ;
      FFlashBoolean := False ;
      BrakeIntervention.Enabled:=False;
    end;
  end;
end;

{**********************************************************
* BrakeClick
* Description: Sends a message to The ATP and Disables the button
********************************************************* }
procedure TFormMainLayer2.BrakeClick(Sender: TObject);
begin
  FFlashBoolean := False;
  Brake.Enabled := False;
  Exchange.SendMMIButton(bsBrakeRelease);
  FormMMIFrame.LogEventStr(LogLevelDebug, AnsiString('ML2'),
      AnsiString('EB Brake release'));

  if FormMMIFrame.Sound then
    Beep;
end;

{**********************************************************
* BrakeInterventionClick
* Description: Sends a message to The ATP and Disables the button
********************************************************* }
procedure TFormMainLayer2.BrakeInterventionClick(Sender: TObject);
begin
  FFlashBoolean := False;
  BrakeIntervention.Enabled := False;
  Exchange.SendMMIButton(bsBrakeRelease);
  FormMMIFrame.LogEventStr(LogLevelDebug, AnsiString('ML2'),
      AnsiString('SB Brake release'));
  if FormMMIFrame.Sound then
    Beep;
end;
{**********************************************************
* BrakeTestClick
* Description: Sends a message to The ATP and Disables the button
********************************************************* }
procedure TFormMainLayer2.BrakeTestClick(Sender: TObject);
begin

  if (ActiveBrakeTestGlyph = bgBrakeTestAbort) then
  begin
    Exchange.SendMMIButton(bsAbortBrakeTest);
  end
  else
  begin
    FormConfirmBrakeTest.Show;
  end;
end;

procedure TFormMainLayer2.BTMTestClick(Sender: TObject);
begin
  inherited;
  Exchange.SendMMIButton(bsStartBTMTest);

end;

procedure TFormMainLayer2.DataSourceEventsDataChange(Sender: TObject;
  Field: TField);
begin
  inherited;

end;

{ *********************************************************
* FlashControl
* Description: Takes a Tcontrol object and toggles the
               visible state.
********************************************************* }
procedure TFormMainLayer2.FlashControl(Sender: TControl);
begin
  try
    if Sender is TControl then
      with Sender as TControl do
        if Sender.Visible then
          Sender.Hide
        else
          Sender.Show;

  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
        MainLogErrorFlashControl);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in FlashControl:' + E.Message),
          PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);

    end;
  end;
end;

{ *********************************************************
* FlashBrakeIntervention
* Description: Since the driver looses the possibility
               to press the button if we make it disappear,
               We can't do that. Instead we show a button with
               a blank picture.
********************************************************* }
procedure TFormMainLayer2.FlashBrakeIntervention(Blank: Boolean;
  Sender: TControl);
begin
  try

    if (Sender = BrakeIntervention) then
    begin
      if Blank then
      begin
        if not(ActiveSBrakeGlyph = bgBlank) then
        begin
          LoadBitmapFromResource(BrakeIntervention.Glyph, FormMMIFrame.HiRes,
            'SERVICEBRAKE_YELLOW', 'HIRES_SERVICEBRAKE_YELLOW');
          ActiveSBrakeGlyph := bgBlank;
        end;
      end
      else if not(ActiveSBrakeGlyph = bgBrake) then
      begin
        LoadBitmapFromResource(BrakeIntervention.Glyph, FormMMIFrame.HiRes,
          'SERVICEBRAKE', 'HIRES_SERVICEBRAKE');
        ActiveSBrakeGlyph := bgBrake;
      end;
    end;
    if (Sender = Brake) then
      if Blank then
      begin
        if not(ActiveEBrakeGlyph = bgBlank) then
        begin
          LoadBitmapFromResource(Brake.Glyph, FormMMIFrame.HiRes,
            'EMERGENCYBRAKE_YELLOW', 'HIRES_EMERGENCYBRAKE_YELLOW');
          ActiveEBrakeGlyph := bgBlank;
        end;
      end
      else if not(ActiveEBrakeGlyph = bgBrake) then
      begin
        LoadBitmapFromResource(Brake.Glyph, FormMMIFrame.HiRes,
          'EMERGENCYBRAKE', 'HIRES_EMERGENCYBRAKE');
        ActiveEBrakeGlyph := bgBrake;
      end;
    begin
    end;

  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
        MainLogErrorFlashBrakeIntervention);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in FlashBrakeIntervention:' +
          E.Message), PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);

    end;
  end;
end;


{$R *.dfm}

end.

