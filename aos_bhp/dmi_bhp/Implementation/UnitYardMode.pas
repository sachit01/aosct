(*****************************************************************************
*           © COPYRIGHT Bombardier Transportation AB, SWEDEN 2011.           *
*           ======================================================           *
*                                                                            *
*    The copyright to the computer program herein is the                     *
*    property of Bombardier Transportation AB, Sweden. All rights reserved.  *
*    The program may be used and/or copied only with the                     *
*    written permission from Bombardier Transportation AB, or in accordance  *
*    with the terms and conditions stipulated in the                         *
*    agreement/contract under which the program has been                     *
*    supplied.                                                               *
*                                                                            *
*                                                                            *
*    NAME:  UnitYardMode.pas                                                 *
*                                                                            *
*    PROJECT:  LKAB, InterFlow TrainBorne                                    *
*                                                                            *
*    Ver    Author           Date     Reason                                 *
*    ---    ------           ------   ------                                 *
*           Bo H             111110   Start                                  *
*           Bo H             120221   ATO Mode/Switch icons                  *
*           Bo H             120419   Always read from all ATx-units         *
*                                     Not only from the selected unit        *
*                                                                            *
*    DESCRIPTION:                                                            *
*                                                                            *
*    INTERFACE:                                                              *
******************************************************************************)
unit UnitYardMode;

interface


{****************************************************************************
* UNIT USES                                                                 *
****************************************************************************}
uses Windows, Forms, Dialogs, SysUtils, Graphics, ComCtrls,
  StdCtrls,ExtCtrls, ColorButton2, Buttons, Controls, MMIPanel, Classes,
  MMIareaB, MMITypes, Sockets;


{****************************************************************************
* UNIT CONST DECLARATIONS                                                   *
****************************************************************************}
const

  YardModePrefix = 'YardMode';
  YardModeCommandSuffix = 'Commands';

{****************************************************************************
* UNIT TYPE DECLARATIONS                                                    *
****************************************************************************}
type

  TYardModeTabs = (tabMsg, tabATP1, tabATP2, tabATO);

  TBrakeGlyphActive = (bgUndefined,
                       bgBlank,
                       bgBrake);


  TFormYardMode = class(TForm)
    TimerDisplay: TTimer;
    MMIareaB: TMMIareaB;
    EButton: TMMIPanel;
    Brake: TSpeedButton;  // This is indication for emergency brake
    BrakeIntervention: TSpeedButton; // This is indication for service brake
    IntegrityAlert: TSpeedButton;
    EAlert: TSpeedButton;
    AreaF: TMMIPanel;
    ConfigurationReq: TColorButton;
    MMIPanelE12: TMMIPanel;
    RadioImage: TImage;
    TimeField: TLabel;
    PageControl: TPageControl;
    TabSheetATP1: TTabSheet;
    TabSheetATP2: TTabSheet;
    TabSheetATO: TTabSheet;
    PanelCommand: TPanel;
    ComboBoxCommand: TComboBox;
    BitBtnGo: TBitBtn;
    LabelCommand: TLabel;
    BitBtnClear: TBitBtn;
    MemoATP1Response: TMemo;
    MemoATP2Response: TMemo;
    MemoATOResponse: TMemo;
    Label1: TLabel;
    LabelLastATP1Command: TLabel;
    LabelLastATP2Command: TLabel;
    LabelLastATOCommand: TLabel;
    TcpClientATP1: TTcpClient;
    TcpClientATP2: TTcpClient;
    TcpClientATO: TTcpClient;
    TimerResponse: TTimer;
    LabelSpeed: TLabel;
    MMIPanelE21: TMMIPanel;
    SystemIconChangeMode: TImage;
    MMIPanelE22: TMMIPanel;
    SystemIconATOMode: TImage;
    TabSheetMsg: TTabSheet;
    MemoMsg: TMemo;
    procedure TimeFieldClick(Sender: TObject);
    procedure BrakeInterventionClick(Sender: TObject);
    procedure BrakeClick(Sender: TObject);
    procedure ConfigurationReqClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtnGoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtnClearClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure TimerResponseTimer(Sender: TObject);
    procedure TimerDisplayTimer(Sender: TObject);

  private
    { Private declarations }
    SpeedZeroCounter: Byte;
    FRadioBool:Boolean ;
    RadioBitmap: TBitmap;
    ActiveSBrakeGlyph: TBrakeGlyphActive;
    ActiveEBrakeGlyph: TBrakeGlyphActive;

    SystemIconWantedMode: TBitmap;
    SystemIconWantedModeDisplayed:Boolean;

    SystemIconATOUndefined : TBitmap;
    SystemIconATOManual    : TBitmap;
    SystemIconATOSupervised: TBitmap;
    SystemIconATOAutomatic : TBitmap;
    SystemIconATORemote    : TBitmap;

    CommandStrings : TStrings;

    ATP1  : THostSettings;
    ATP2  : THostSettings;
    ATO   : THostSettings;

    procedure ScreenChange;

    procedure GetHostSettings;
    procedure FillComboBoxCommand(Chapter:String);
    function GetCommandString(Chapter:String;Item:String):AnsiString;
    procedure setRadiobool (Value:Boolean) ;
    procedure UpdateTime(Time: TDateTime) ;
  protected
    { Protected declarations }

  public
    { Public declarations }
    Slice: Byte ;
    FFlashBoolean: ByteBool ;
    FControlToFlash: TControl ;

    MaxYardModeResponseLines:Integer;

    property RadioSymbol: Boolean Read FRadioBool write setRadiobool default False ;
    procedure FlashControl(Sender: TControl) ;
    procedure FlashBrakeIntervention( Blank:Boolean ;Sender: TControl) ;
    procedure SetWantedATOSwitch(ATOSwitch : T_ATOSwitch);
    procedure SetATOMode(ATOMode : T_ATOModes);
    procedure AddTextItem(TextEntry : String);

  published
    { Published declarations}

  end;




{****************************************************************************
* UNIT VAR DECLARATIONS                                                     *
****************************************************************************}
var
  FormYardMode: TFormYardMode;
  NewDate: wordBool = False;

const
  NumberOfPeriodsOfZeroSpeed = 8;

implementation
{$R *.DFM}

{****************************************************************************
* USES                                                                      *
****************************************************************************}
Uses
  MMIStd, UnitMMIFrame, UnitMainArea, UnitFlash, UnitExchange, IniFiles;

{****************************************************************************
* CONST DECLARATIONS                                                        *
****************************************************************************}
const
  YardModeLogErrorSetRadioBool = 1;
  YardModeLogErrorUpdateTime  = 2;
  YardModeLogErrorFlashControl= 3;
  YardModeLogErrorFlashBrakeIntervention = 4;
  YardModeLogErrorTimeField   = 5;
  YardModeLogErrorTimerDisplay= 6;
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

{*********************************************************
* Function:    SetRadioBool
* Description: Boolean Function to set the Radio Zapp
               Icon
*********************************************************}
Procedure TFormYardMode.SetRadioBool (Value:boolean) ;
begin
  try
     if value then
        Fradiobool:= True
     else
         FRadioBool:=False ;
     if (FRadioBool and  RadioImage.Picture.Bitmap.Empty) then
        RadioImage.Picture.Bitmap.Assign(RadioBitmap)
     else if ((not FRadioBool) and (not RadioImage.Picture.Bitmap.Empty)) then
        RadioImage.Picture.Bitmap.Assign(nil) ;
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('YARDMODE',MS_SW_ERROR,MS_YARDMODE,YardModeLogErrorSetRadioBool);
      if FormMMIFrame.TestMode then
        ShowMessage('Error SetRadioBool') ;
    end;
 end;
end;

{*********************************************************
* Function:    UpdateTime
* Description: Updates The clock on the ETCS Screen
*********************************************************}
Procedure TFormYardMode.UpdateTime (Time:TDateTime) ;
Var
   Hour, Min, Sec, MSec: Word;
begin
  try
  if not NewDate then
    begin
       DecodeTime(Time, Hour, Min, Sec, MSec);
       if msec >= 500 then
         Timefield.Caption:=FormatDateTime('hh:nn',Time)
       else
         Timefield.Caption:=FormatDateTime('hh nn',Time) ;
    end
  else
      TimeField.Caption:=FormatDateTime('d mmm',Time);

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('YARDMODE',MS_SW_ERROR,MS_YARDMODE,YardModeLogErrorUpdateTime);
      if FormMMIFrame.TestMode then
        ShowMessage('Error UpdateTime') ;
    end;
 end;
end ;

{*********************************************************
* Function:     TimerResponse
* Description:  Checks for response from any unit and updates
*               response tab-sheets
*********************************************************}
procedure TFormYardMode.TimerResponseTimer(Sender: TObject);
var
  ResponseString : AnsiString;
  Finished  : Boolean;
begin

              { Read from ATP1 }
  Finished := False;

  if TcpClientATP1.Active then
  begin

    while not Finished do
    begin
      ResponseString:=TcpClientATP1.Receiveln(LF);
      if Length(ResponseString) > 0 then
      begin
        MemoATP1Response.Lines.Add(String(ResponseString)); {Append a line to the memo}
                        { Limit the no of lines }
        while (MemoATP1Response.Lines.Count > MaxYardModeResponseLines) do
        begin   { Delete the oldest line }
          MemoATP1Response.Lines.Delete(0);
        end;
      end
      else
        Finished:= True;
    end;

  end;

              { Read from ATP2 }
  Finished := False;

  if TcpClientATP2.Active then
  begin

    while not Finished do
    begin

      ResponseString:=TcpClientATP2.Receiveln(LF);
      if Length(ResponseString) > 0 then
      begin
        MemoATP2Response.Lines.Add(String(ResponseString)); {Append a line to the memo}
                        { Limit the no of lines }
        while (MemoATP2Response.Lines.Count > MaxYardModeResponseLines) do
        begin   { Delete the oldest line }
          MemoATP2Response.Lines.Delete(0);
        end;
      end
      else
        Finished:=True;
    end;

  end;

              { Read from ATO }
  Finished := False;

  if TcpClientATO.Active then
  begin
    while not Finished do
    begin
      ResponseString:=TcpClientATO.Receiveln(LF);
      if Length(ResponseString) > 0 then
      begin
        MemoATOResponse.Lines.Add(String(ResponseString)); {Append a line to the memo}
                        { Limit the no of lines }
        while (MemoATOResponse.Lines.Count > MaxYardModeResponseLines) do
        begin   { Delete the oldest line }
          MemoATOResponse.Lines.Delete(0);
        end;
      end
      else
        Finished:=True;
      end;

    end;

end;

{*********************************************************
* Function:    FlashControl
* Description: Takes a Tcontrol object and toggles the
               visible state.
*********************************************************}
Procedure TFormYardMode.FlashControl(Sender: TControl) ;
begin
  try
  if sender is Tcontrol then with sender as Tcontrol do
     if Sender.Visible
     then
        Sender.Hide
     else
        Sender.Show ;
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('YARDMODE',MS_SW_ERROR,MS_YARDMODE,YardModeLogErrorFlashControl);
      if FormMMIFrame.TestMode then
        ShowMessage('Error FlashControl') ;
    end;
 end;
end;

procedure TFormYardMode.FormCreate(Sender: TObject);
begin

  Parent := FormMMIFrame.PanelMMIFrame;
  Align := alClient;

  ScreenChange;

  CommandStrings:=TStringList.Create;

  RadioBitmap:=TBitmap.Create ;

  AdjustSpeedButton(Brake);
  LoadBitmapFromResource(Brake.Glyph, FormMMIFrame.HiRes, 'EMERGENCYBRAKE', 'HIRES_EMERGENCYBRAKE');

  AdjustSpeedButton(BrakeIntervention);
  LoadBitmapFromResource(BrakeIntervention.Glyph, FormMMIFrame.HiRes, 'SERVICEBRAKE', 'HIRES_SERVICEBRAKE');

  AdjustSpeedButton(IntegrityAlert);
  LoadBitmapFromResource(IntegrityAlert.Glyph, FormMMIFrame.HiRes, 'INTEGRITYALERT', 'HIRES_INTEGRITYALERT');

  AdjustSpeedButton(EAlert);
  LoadBitmapFromResource(EAlert.Glyph, FormMMIFrame.HiRes, 'EMERGENCYALERT', 'HIRES_EMERGENCYALERT');

  RadioBitmap.LoadFromResourceName(Hinstance,'RADIOZAPP') ;

  Brake.NumGlyphs:=4 ;
  BrakeIntervention.NumGlyphs:=4;

  IntegrityAlert.NumGlyphs:=2 ;
  EAlert.NumGlyphs:=2 ;
  ActiveEBrakeGlyph:= bgUndefined;
  ActiveSBrakeGlyph:= bgUndefined;

  SystemIconWantedMode:= TBitmap.Create;
  SystemIconWantedModeDisplayed := False;

  { Configurable ATO mode icons }
  SystemIconATOUndefined := TBitmap.Create;
  SystemIconATOManual    := TBitmap.Create;
  SystemIconATOSupervised:= TBitmap.Create;
  SystemIconATOAutomatic := TBitmap.Create;
  SystemIconATORemote    := TBitmap.Create;

  LoadBitmapImage(SystemIconATOUndefined, 'ATO_UNDEFINED', ImageATOUndefined, 'MAIN');

  LoadBitmapImage(SystemIconATOManual, FormMMIFrame.HiRes, 'ATO_MANUAL', 'HIRES_ATO_MANUAL', ImageATOManual, 'MAIN');
  LoadBitmapImage(SystemIconATOSupervised, FormMMIFrame.HiRes, 'ATO_SUPERVISED', 'HIRES_ATO_SUPERVISED', ImageATOSupervised, 'MAIN');
  LoadBitmapImage(SystemIconATOAutomatic, FormMMIFrame.HiRes, 'ATO_AUTOMATIC', 'HIRES_ATO_AUTOMATIC', ImageATOAutomatic, 'MAIN');
  LoadBitmapImage(SystemIconATORemote, FormMMIFrame.HiRes,'ATO_REMOTE','HIRES_ATO_REMOTE', ImageATORemote, 'MAIN');

  { End ATO mode icons. }

      // Config button
  if LoadBitmapImage(ConfigurationReq.Picture.Bitmap, ImageConfiguration, 'MAIN') then
      // Clear caption if image was loaded
    ConfigurationReq.Caption := ''
  else
      // Replace caption with language-dependent character
    ConfigurationReq.Caption := InterfaceString(ifConfigurationFBtn);

                                    // Init the max no of lines in the list-view.
  MaxYardModeResponseLines := FormMMIFrame.MaxYardModeResponseLines;

  EButton.Width    := Brake.Width + 4;
  EButton.Height   := 4 * (Brake.Height + 4);
  MMIAreaB.Left    := EButton.Width;
  MMIAreaB.Height := EButton.Height + LabelSpeed.Height;

  MMIAreaB.Width  := MMIAreaB.Height;

  LabelSpeed.Left := EButton.Left + EButton.Width - LabelSpeed.Width;

  PanelCommand.Left := MMIAreaB.Left + MMIAreaB.Width;
  PanelCommand.Top  := MMIPanelE22.Height;
  PanelCommand.Width:= AreaF.Left - PanelCommand.Left;
  PanelCommand.Height:= PageControl.Top - MMIPanelE22.Height;

  ComboBoxCommand.Width := PanelCommand.Width - 2 * ComboBoxCommand.Left;

  MMIPanelE21.Left := PanelCommand.Left;
  MMIPanelE22.Left := MMIPanelE21.Left + MMIPanelE21.Width;

  SpeedZeroCounter  := NumberOfPeriodsOfZeroSpeed;

end;

procedure TFormYardMode.FormDestroy(Sender: TObject);
begin
  CommandStrings.Destroy;
end;

procedure TFormYardMode.FormHide(Sender: TObject);
begin

  TimerResponse.Enabled := false;

  TcpClientATP1.Active := false;
  TcpClientATP2.Active := false;
  TcpClientATO.Active := false;

end;

{*********************************************************
* Function:    GetHostSettings
* Description: Read the host/port for the YardMode units
*
*********************************************************}
procedure TFormYardMode.GetHostSettings;
var
  FInitFile: TIniFile ;

begin

  FInitFile:= TIniFile.Create(FormMMIFrame.InitFileName) ;
  if (FInitFile <> Nil ) then
  begin

    ATP1.Host := AnsiString(FInitFile.ReadString(YardModePrefix+'ATP1','Host','192.168.30.10'));
    ATP1.Port := FInitFile.ReadInteger(YardModePrefix+'ATP1','Port',55150);
    ATP2.Host := AnsiString(FInitFile.ReadString(YardModePrefix+'ATP2','Host','192.168.30.11'));
    ATP2.Port := FInitFile.ReadInteger(YardModePrefix+'ATP2','Port',55150);
    ATO.Host := AnsiString(FInitFile.ReadString(YardModePrefix+'ATO','Host','192.168.30.12'));
    ATO.Port := FInitFile.ReadInteger(YardModePrefix+'ATO','Port',55150);

    FInitFile.Free;
  end;


end;

procedure TFormYardMode.FormShow(Sender: TObject);
begin
  PageControl.TabIndex:=Ord(tabMsg);
  PanelCommand.Visible:=false;

  LabelCommand.Caption := 'ATP1 Yard mode command';
  FillComboBoxCommand('ATP1');

//  BitBtnGo.Enabled := false;

  GetHostSettings;

  TcpClientATP1.RemoteHost := ATP1.Host;
  TcpClientATP1.RemotePort := IntToAnsiStr(ATP1.Port);

  TcpClientATP2.RemoteHost := ATP2.Host;
  TcpClientATP2.RemotePort := IntToAnsiStr(ATP2.Port);

  TcpClientATO.RemoteHost := ATO.Host;
  TcpClientATO.RemotePort := IntToAnsiStr(ATO.Port);

  TcpClientATP1.Active := true;
  TcpClientATP2.Active := true;
  TcpClientATO.Active := true;

  TimerResponse.Enabled := true;
  TimerDisplay.Enabled := true;

end;



procedure TFormYardMode.PageControlChange(Sender: TObject);
var
  SelectIndex : Integer;
begin
  case PageControl.TabIndex of
    Ord(tabMsg):
        begin
          PanelCommand.Visible := false;
        end;
    Ord(tabATP1):
        begin
          PanelCommand.Visible := true;
          LabelCommand.Caption := 'ATP1 Yard mode command';
          FillComboBoxCommand('ATP1');
          SelectIndex := ComboBoxCommand.Items.IndexOf(LabelLastATP1Command.Caption);
//          BitBtnGo.Enabled := TcpClientATP1.Connected;
        end;

    Ord(tabATP2):
        begin
          PanelCommand.Visible := true;
          LabelCommand.Caption := 'ATP2 Yard mode command';
          FillComboBoxCommand('ATP2');
          SelectIndex := ComboBoxCommand.Items.IndexOf(LabelLastATP2Command.Caption);
//          BitBtnGo.Enabled := TcpClientATP2.Connected;
        end;

    Ord(tabATO):
        begin
          PanelCommand.Visible := true;
          LabelCommand.Caption := 'ATO Yard mode command';
          FillComboBoxCommand('ATO');
          SelectIndex := ComboBoxCommand.Items.IndexOf(LabelLastATOCommand.Caption);
//          BitBtnGo.Enabled := TcpClientATO.Connected;
        end;

    else
        SelectIndex := 0;
  end;
  ComboBoxCommand.ItemIndex:=SelectIndex;
end;

{*********************************************************
* Function:    ScreenChange
* Description: Adjusts positions and widths when the screen
*              resolution is different from the standard
*              640 * 480 pixels
*********************************************************}
procedure TFormYardMode.ScreenChange;

begin  {ScreenChange}

    AreaF.Width:=FormMMIFrame.ClientWidth-GetXpos3;
    AreaF.Height:=GetYpos3;
    AreaF.Left:=GetXpos3;

    AdjustColorButton(ConfigurationReq);

//    AdjustImage(RadioImage);

    AdjustControl(PanelCommand);
    AdjustLabel(LabelCommand);
    AdjustControl(ComboBoxCommand);
    AdjustControl(BitBtnGo);
    AdjustControl(BitBtnClear);

    AdjustControl(PageControl);

    AdjustLabel(LabelSpeed);

    AdjustControl(MMIPanelE12);
    AdjustControl(MMIPanelE21);
    AdjustControl(MMIPanelE22);

    AdjustImage(SystemIconATOMode);
    CentreImage(SystemIconATOMode, MMIPanelE22.Width, MMIPanelE22.Height);

    CentreImage(RadioImage, MMIPanelE12.Width, MMIPanelE12.Height);

    AdjustLabel(TimeField);


End;

{*********************************************************
* Function:    FlashBrakeIntervention
* Description: Since the driver looses the possibility
               to press the button if we make it dissapper,
               We can't do that. Instead we show a button with
               a blank picture.
*********************************************************}
Procedure TFormYardMode.FlashBrakeIntervention (Blank:Boolean; Sender: TControl) ;
begin
  try

  if (sender = BrakeIntervention) then
  Begin
     if Blank then begin
        if not (ActiveSBrakeGlyph = bgBlank) then begin
          LoadBitmapFromResource(BrakeIntervention.Glyph, FormMMIFrame.HiRes, 'SERVICEBRAKE_YELLOW', 'HIRES_SERVICEBRAKE_YELLOW');
          ActiveSBrakeGlyph:= bgBlank;
        end;
     end
     Else
        if not (ActiveSBrakeGlyph = bgBrake) then begin
          LoadBitmapFromResource(BrakeIntervention.Glyph, FormMMIFrame.HiRes, 'SERVICEBRAKE', 'HIRES_SERVICEBRAKE');
          ActiveSBrakeGlyph:= bgBrake;
        end;
  End;
  if (sender = Brake) then
     if Blank then begin
        if not (ActiveEBrakeGlyph = bgBlank) then begin
          LoadBitmapFromResource(Brake.Glyph, FormMMIFrame.HiRes, 'EMERGENCYBRAKE_YELLOW', 'HIRES_EMERGENCYBRAKE_YELLOW');
          ActiveEBrakeGlyph:= bgBlank;
        end;
     end
     else
        if not (ActiveEBrakeGlyph = bgBrake) then
        begin
          LoadBitmapFromResource(Brake.Glyph, FormMMIFrame.HiRes, 'EMERGENCYBRAKE', 'HIRES_EMERGENCYBRAKE');
          ActiveEBrakeGlyph:= bgBrake;
        end;
  Begin
  End;

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('YARDMODE',MS_SW_ERROR,MS_YARDMODE,YardModeLogErrorFlashBrakeIntervention);
      if FormMMIFrame.TestMode then
        ShowMessage('Error flashing brake intervention') ;
    end;
  end;
end;

{*********************************************************
* Function:    FillComboBoxCommand
* Description: Fill combo-box from chapter in INI-file
               To toggle between time and date.
*********************************************************}
procedure TFormYardMode.FillComboBoxCommand(Chapter:String);
var
  FInitFile: TIniFile ;
  I : Integer;
  CommandString : AnsiString;
  EndOfKeyNamePos : Integer;
  KeyName : AnsiString;
begin

  ComboBoxCommand.Clear;
  CommandStrings.Clear;

  FInitFile:= TIniFile.Create(FormMMIFrame.InitFileName) ;
  if (FInitFile <> Nil ) then
  begin

    FInitFile.ReadSectionValues(YardModePrefix+Chapter+YardModeCommandSuffix, CommandStrings);
    for I:= 0 to (CommandStrings.Count - 1) do
    begin

      CommandString := AnsiString(CommandStrings.Strings[I]);
      EndOfKeyNamePos:=AnsiPos('=',String(CommandString));
      if EndOfKeyNamePos > 1 then
      begin
        KeyName := Copy(CommandString,1,EndOfKeyNamePos-1);
        ComboBoxCommand.Items.Add(String(KeyName));
      end;
    end;

    FInitFile.Free;

  end;

end;

{*********************************************************
* Function:    GetCommandString
* Description: Get command-string from chapter and item in INI-file
*
*********************************************************}
function TFormYardMode.GetCommandString(Chapter:String;Item:String):AnsiString;
var
  FInitFile: TIniFile ;

begin

  FInitFile:= TIniFile.Create(FormMMIFrame.InitFileName) ;
  if (FInitFile = Nil ) then
  begin
    Result:='';
  end
  else
  begin

    Result:=AnsiString(FInitFile.ReadString(YardModePrefix+Chapter+YardModeCommandSuffix, Item, ''));

    FInitFile.Free;

  end;

end;

{*********************************************************
* Function:    TimeFieldClick
* Description: Changes the timefield on the ETCS Screen
               To toggle between time and date.
*********************************************************}
procedure TFormYardMode.TimeFieldClick(Sender: TObject);
begin
  try
    NewDate:= not NewDate ;
    UpdateTime(Now);
    if FormMMIFrame.Sound then Beep ;
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('YARDMODE',MS_SW_ERROR,MS_YARDMODE,YardModeLogErrorTimeField);
      if FormMMIFrame.TestMode then
        ShowMessage('Error TimeFieldClick') ;
    end;
 end;
end;

procedure TFormYardMode.TimerDisplayTimer(Sender: TObject);
begin
                        // Disable timer and reenable at the end of this function
  TimerDisplay.Enabled := false;

                        // Check if train is at stand-still
  if MMIareaB.ActualSpeed = 0 then
  begin

    if SpeedZeroCounter > 0 then
    begin
      Dec(SpeedZeroCounter);
    end;

    if (SpeedZeroCounter = 0) then
    begin
                        // Enable the Configuration req - button if speed is stable 0
      ConfigurationReq.Visible := true;
    end
    else
    begin
      ConfigurationReq.Visible := false;
    end;



  end
  else
  begin
    SpeedZeroCounter := NumberOfPeriodsOfZeroSpeed;
    ConfigurationReq.Visible := false;
  end;


  try
   Slice:= ((Slice +1) mod 4) ;
   case Slice of
      0: begin

                        // Display time
             updateTime(Now);
                        // Flash any control ?
             if FFlashBoolean
             then
             begin

                if not ((FcontroltoFlash = BrakeIntervention) or (FcontroltoFlash = Brake))
                then
                   FlashControl(FControlToFlash)
                else
                   FlashBrakeIntervention(TRUE,FcontroltoFlash) ;

             end
             else
             begin  // This is to ensure that the right glyph is loaded
                    // when not flashing!
                FlashBrakeIntervention(FALSE,BrakeIntervention) ;
                FlashBrakeIntervention(FALSE,Brake) ;
             end;

                                 // Load bitmap
             if SystemIconChangeMode.Enabled then
             begin

                if SystemIconWantedModeDisplayed
                then
                begin
                  SystemIconChangeMode.Picture.Bitmap.LoadFromResourceName(Hinstance,'SWITCH');
                  SystemIconWantedModeDisplayed:= False;
                end
                else begin
                  SystemIconChangeMode.Picture.Bitmap:= SystemIconWantedMode ;
                  SystemIconWantedModeDisplayed:= True ;
                end;

                CentreImage(SystemIconChangeMode, MMIPanelE21.Width, MMIPanelE21.Height);
             end;


          end;

      1: begin
          ;
          end;

      2: begin
             updateTime(Now);
             if FFlashBoolean then begin
                if not ((FcontroltoFlash = BrakeIntervention) or (FcontroltoFlash = Brake)) then begin
                   FlashControl(FControlToFlash) ;
                   Dec(FFlashBoolean) ;
                end
                Else
                   FlashBrakeIntervention(FALSE,FcontrolToFlash) ;
             end;
          end;
      3: begin
            ;
          end;
   Else
   end;
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('YARDMODE',MS_SW_ERROR,MS_YARDMODE,YardModeLogErrorTimerDisplay);
      if FormMMIFrame.TestMode then
        ShowMessage('Error TIMEREvent') ;
    end;
 end;

 TimerDisplay.Enabled := true;

end;

{*********************************************************
* Function:    BrakeInterventionClick
* Description: Sends a message to The ATP and Disables the button
*********************************************************}
procedure TFormYardMode.BrakeInterventionClick(Sender: TObject);
begin
  FFlashBoolean:= False ;
  BrakeIntervention.Enabled:=FALSE ;
  Exchange.SendMMIButton(bsBrakeRelease) ;
  if FormMMIFrame.Sound then Beep ;
end;


procedure TFormYardMode.ConfigurationReqClick(Sender: TObject);
begin

  Exchange.SendMMIButton(bsTrainConfig) ;
  if FormMMIFrame.Sound then Beep ;

end;

procedure TFormYardMode.BitBtnClearClick(Sender: TObject);
begin
  case PageControl.TabIndex of
    Ord(tabATP1):
        begin
          MemoATP1Response.Clear;
        end;

    Ord(tabATP2):
        begin
          MemoATP2Response.Clear;
        end;

    Ord(tabATO):
        begin
          MemoATOResponse.Clear;
        end;

  end;

end;

procedure TFormYardMode.BitBtnGoClick(Sender: TObject);
var
  CommandString : AnsiString;
begin
  case PageControl.TabIndex of
    Ord(tabATP1):
        begin
          CommandString:=GetCommandString('ATP1',ComboBoxCommand.Text);
          TcpClientATP1.Sendln(CommandString,LF);

          LabelLastATP1Command.Caption:=ComboBoxCommand.Text;
        end;

    Ord(tabATP2):
        begin
          CommandString:=GetCommandString('ATP2',ComboBoxCommand.Text);
          TcpClientATP2.Sendln(CommandString,LF);

          LabelLastATP2Command.Caption:=ComboBoxCommand.Text;
        end;

    Ord(tabATO):
        begin
          CommandString:=GetCommandString('ATO',ComboBoxCommand.Text);
          TcpClientATO.Sendln(CommandString,LF);

          LabelLastATOCommand.Caption:=ComboBoxCommand.Text;
        end;

  end;

end;

procedure TFormYardMode.BrakeClick(Sender: TObject);
begin
  FFlashBoolean:= False ;
  Brake.Enabled:=FALSE ;
  Exchange.SendMMIButton(bsBrakeRelease) ;
  if FormMMIFrame.Sound then Beep ;
end;


{*********************************************************
* Function:     SetATOSwitch
* Description:  Set icons to display the wanted mode for the ATO
*********************************************************}
procedure TFormYardMode.SetWantedATOSwitch(ATOSwitch : T_ATOSwitch);
begin
  FormMMIFrame.LogEventStr(LogLevelDetail, AnsiString('YARD'), AnsiString('Wanted ATOSwitch ' + IntToStr(Ord(ATOSwitch))));
  case ATOSwitch of
        ATOSwitch_Manual:
          begin
            SystemIconWantedMode:= SystemIconATOManual;
            SystemIconChangeMode.Picture.Bitmap := SystemIconWantedMode;
            SystemIconChangeMode.Enabled := True;
            CentreImage(SystemIconChangeMode, MMIPanelE21.Width, MMIPanelE21.Height);
            SystemIconChangeMode.Show;
          end;
        ATOSwitch_SupervisedAutomatic:
          begin
            SystemIconWantedMode := SystemIconATOSupervised;
            SystemIconChangeMode.Picture.Bitmap := SystemIconWantedMode;
            SystemIconChangeMode.Enabled := True;
            CentreImage(SystemIconChangeMode, MMIPanelE21.Width, MMIPanelE21.Height);
            SystemIconChangeMode.Show;
          end;
        ATOSwitch_Automatic:
          begin
            SystemIconWantedMode := SystemIconATOAutomatic;
            SystemIconChangeMode.Picture.Bitmap := SystemIconWantedMode;
            SystemIconChangeMode.Enabled := True;
            CentreImage(SystemIconChangeMode, MMIPanelE21.Width, MMIPanelE21.Height);
            SystemIconChangeMode.Show;
          end;
      else
        begin
          SystemIconChangeMode.Enabled := False;
          SystemIconChangeMode.Hide;
        end;
  end;

end;

{*********************************************************
* Function:     SetATOMode
* Description:  Set icon to display the actual mode of the ATO
*********************************************************}
procedure TFormYardMode.SetATOMode(ATOMode : T_ATOModes);
begin

  FormMMIFrame.LogEventStr(LogLevelDetail, AnsiString('YARD'), AnsiString('ATOMode ' + IntToStr(Ord(ATOMode))));
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

end;

{*********************************************************
* Function:    AddTextItem
* Description: Add a text to the top of the memo
* displaying the messages
*********************************************************}
procedure TFormYardMode.AddTextItem(TextEntry : String);
var
  MsgLine : String;
begin

                                    // Always format in a fixed way, 24-hour clock
  MsgLine:=FormatDateTime('yyyy-mm-dd hh:mm:ss',Now) + ' ' + TextEntry;


  if MemoMsg.Lines.Count > MaxYardModeResponseLines then
  begin // Limit size of buffer, remove the oldest
    MemoMsg.Lines.Delete(MemoMsg.Lines.Count-1);
  end;
        // Insert last msg first
  MemoMsg.Lines.Insert(0,MsgLine);
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
