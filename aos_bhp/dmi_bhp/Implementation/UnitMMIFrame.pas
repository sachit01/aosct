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
*    NAME:  UnitMMIFrame.pas                                                 *
*                                                                            *
*    PROJECT:  LKAB, InterFlow TrainBorne                                    *
*                                                                            *
*    Ver    Author           Date    Reason                                  *
*    ---    ------           ------  ------                                  *
*           Bo H             111012  Check attributes of FInitFileName       *
*                                     (Not Read-Only)                        *
*                                    before trying to write to the file when *
*                                    ap plication is closed.                  *
*                                                                            *
*    DESCRIPTION:                                                            *
*                                                                            *
*    INTERFACE:                                                              *
******************************************************************************)

unit UnitMMIFrame;

interface

uses
  Windows, Messages, SysUtils, StrUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, MMITypes, IniFiles, Sockets, IdContext, IdBaseComponent,
  IdComponent, IdCustomTCPServer, IdTCPServer, IdSync;

type
  TFormMMIFrame = class(TForm)
    PanelMMIFrame: TPanel;
    TcpClientNJRU: TTcpClient;
    TimerPending: TTimer;
    ExchangeTimer: TTimer;
    Timer: TTimer;
    IdTCPServerRemoteDMIInterface: TIdTCPServer;
    IdTCPServerRemoteDMIInterfaceInternal: TIdTCPServer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerPendingTimer(Sender: TObject);
    procedure ExchangeTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure IdTCPServerRemoteDMIInterfaceConnect(AContext: TIdContext);
    procedure IdTCPServerRemoteDMIInterfaceDisconnect(AContext: TIdContext);
    procedure IdTCPServerRemoteDMIInterfaceExecute(AContext: TIdContext);
    procedure IdTCPServerRemoteDMIInterfaceInternalConnect(
      AContext: TIdContext);
    procedure IdTCPServerRemoteDMIInterfaceInternalDisconnect(
      AContext: TIdContext);
    procedure IdTCPServerRemoteDMIInterfaceInternalExecute(
      AContext: TIdContext);
  private
    { Private declarations }
    FLanguageFile: TIniFile ;
    FInitFile: TIniFile ;
    FInitFileName: String;
    NeedToMinimize : Boolean;
    FSound:Boolean ;
    FNoOfAbsentMessages: Word;
    FMaxNoOfAbsentMessages: Word;
    FMessagesReceived: Boolean;

    FTestMode:Boolean ;
    FComPort: integer ;
    FMMIStatus: Word ;
    Slice : Byte;

      {RemoteInterface}
    FRemoteInterfaceEnabled : Boolean;
    FRemoteInterfacePort : Word;
    FRemoteInterfaceInternalPort : Word;

    msgFromClient : String;
    responseToClient : String;

    msgInternalFromClient : String;
    responseInternalToClient : String;


    NJRU : THostSettings;
    LinesToSend : TStringList;
    LoggedOut   : Boolean;
    procedure SendPendingLine;
    procedure AppendLogLevelMenu;
    procedure WMSysCommand(var Msg: TWMSysCommand) ; message WM_SYSCOMMAND;

    Procedure SetInitFileName(Value: String) ;
    Procedure SetInitFile(Value: TIniFile) ;
    Procedure SetLanguageFile(Value: TIniFile) ;
    Procedure SetSound(Value: Boolean) ;
    Procedure SetTestMode(Value: Boolean) ;
    Procedure ReadIniFiles(MMIPath : String);
    Procedure DMIReset();

    Procedure SETUndefined (Output: String ) ;
    Procedure SETasBasicsSystemStartUp ;
    Procedure SETasInactive ;
    Procedure SETavsNoActionState ;
    Procedure SETavsInputState ;
    Procedure SETavsverificationState ;
    Procedure SETavsRedoinputState ;
    Procedure SETasActivationTest ;
    Procedure SETcssAutomatic ;
    Procedure SETcssManual ;
    Procedure SETcssReConfig ;
    Procedure SETcssRestartConfig ;
    Procedure SETamTrainregistration ;
    Procedure SETamNormal ;
    Procedure SETamManualLocation ;
    Procedure SETamNoMAControlledMode;
    Procedure SETasPowerDown;
    Procedure SETasSleeping;
    Procedure SETasSystemRestart;
    Procedure SetFatalFailure;
    Procedure SetUnregistered;

    procedure amPowerUpState(ThisATPState : T_ATPStates ;
                            ThisATPVerification : T_ATPVerificationStates);

    procedure amPoweringDownState(ThisATPState : T_ATPStates ;
                            ThisATPVerification : T_ATPVerificationStates);

    procedure amNoMAControlledModeState(ThisATPState : T_ATPStates;
                            ThisATPVerification : T_ATPVerificationStates);
    procedure amTrainConfigurationState(ThisATPState : T_ATPStates ;
                            ThisConfigSubState : TConfigSubState;
                            ThisATPVerification : T_ATPVerificationStates);
    procedure amTrainRegistrationState(ThisATPState : T_ATPStates ;
                            ThisATPVerification : T_ATPVerificationStates);
    procedure amBaliseSearchState(ThisATPState : T_ATPStates ;
                            ThisATPVerification : T_ATPVerificationStates);
    procedure amNormalState(ThisATPState : T_ATPStates ;
                            ThisATPVerification : T_ATPVerificationStates);
    procedure amLocationState(ThisATPState : T_ATPStates ;
                            ThisATPVerification : T_ATPVerificationStates);
    procedure amTrainSleepingState(ThisATPState : T_ATPStates ;
                            ThisATPVerification : T_ATPVerificationStates);
    procedure amTrainUnRegistrationState(ThisATPState : T_ATPStates  ;
                            ThisATPVerification : T_ATPVerificationStates);


    Procedure RemoteCommand();
    Procedure RemoteInternalCommand();

  public
    { Public declarations }
    HiRes : Boolean;
    MaxStartUpHistoryLines:Integer;
    MaxYardModeResponseLines:Integer;

    LogLevel : Integer;
    LogPath: String ;
    SystemRestartEnable:Boolean;
    MMICLDebug: Integer ;
    TSPos: Integer ;
    OdoPos: Integer;
    AltScreens : Integer;
    waitingForVersionFromATPCount : Byte;
    ATPVersionChecked : Boolean;

    MainCarlist:  TCarRecord ;

    procedure SendToNJRU(LogString:AnsiString);

    Property InitFileName:String Read FInitFileName Write SetInitFileName;
    Property InitFile:TIniFile Read FInitFile Write SetInitFile;
    Property LanguageFile:TIniFile Read FLanguageFile Write SetLanguageFile;
    Function TranslateString(Chapter : String; Specifier : String; DefaultStr : AnsiString; MaxSize : Integer) : AnsiString;
    Function AreaIdToString(AreaId : Byte): String;

    Property Sound: Boolean Read FSound Write SetSound;
    Property TestMode:Boolean Read FTestMode Write SetTestMode;

    Function WriteMMIStatus( LogUnit:AnsiString;
                                       TypeError:Byte ;
                                       Module:Byte ;
                                       Error: Word): Word ;
    Function ReadMMIStatus: Word ;
    Procedure CheckVersion();
    Procedure VerifyVersion(CompatibilityVersion : Byte);
    Procedure LogEvent(Level:Integer; LogUnit:AnsiString; TypeError:Byte; Module:Byte; Error:Word);
    Procedure LogEventStr(Level:Integer; LogUnit:AnsiString; LogStr:AnsiString);
    Procedure SetMMITime(Time: TDateTime) ;
    Procedure SetMMI_ATPModeAndState;
    procedure UpdateDMI;
    procedure HideAllForm;

  end;

const
  MaxPendingLogLines = 100;
  SC_MyMenuItemLogLevel = WM_USER + 1;


var
  FormMMIFrame: TFormMMIFrame;

implementation

uses UnitMainArea, MMIStd, UnitChangeLogLevel, UnitCommLost, UnitAtpMessages,
  UnitExchange, UnitFullScreenMsg, UnitFlash, UnitSafetyHalt, UnitUnregForm,
  UnitLogin, MMIareaD, UnitManualConfiguration, UnitReReg,
  UnitTrainVsTrack, UnitPowerDown, MMIareaA, MMIareaB, MMIareaC,
  UnitDMIDataModule, UnitNoMAControlled, UnitSleepingModeForm, UnitConfirmDepartureTest,
  UnitAutomaticConfiguration, DateUtils, UnitConfirmTachometer1Failure, UnitConfirmTachometer2Failure,
  UnitConfirmDopplerRadarFailure, UnitSelectArea,
  UnitConfirmAbortLastCarBrakeTest, UnitConfirmBrakeTest,
  UnitConfirmSleepModeExit, UnitConfirmFreeRollingClear,
  UnitConfirmLoadedStatus, UnitConfirmMAAccept, UnitConfirmATPMode,
  UnitTrainComposition, UnitConfirmRejectSetup, UnitRadioChannel,
  UnitNewTrainName, UnitConfirmCancelRegArea, UnitStartupHistory;

{$R *.dfm}



 procedure TFormMMIFrame.AppendLogLevelMenu ;
 const
   sMyMenuCaption = 'Change &LogLevel';
 var
   SysMenu : HMenu;
 begin
   {Get system menu}
   SysMenu := GetSystemMenu(Handle, FALSE) ;
   {Add a seperator bar to main form-form1}
   AppendMenu(SysMenu, MF_SEPARATOR, 0, '') ;
   {add our menu}
   AppendMenu(SysMenu, MF_STRING, SC_MyMenuItemLogLevel, sMyMenuCaption) ;
 end;


 procedure TFormMMIFrame.WMSysCommand(var Msg : TWMSysCommand) ;
 begin
   if Msg.CmdType = SC_MyMenuItemLogLevel then
   begin
        { Change Log level }

      FormChangeLogLevel.ComboBoxLogLevel.ItemIndex := LogLevel;
      if (FormChangeLogLevel.ShowModal = mrOk) then
      begin
        LogLevel := FormChangeLogLevel.ComboBoxLogLevel.ItemIndex;
      end;

   end
   else
    inherited;
 end;

procedure TFormMMIFrame.ExchangeTimerTimer(Sender: TObject);
begin
      { Re-enable at the end
        If you get an error that causes a modal message }
  ExchangeTimer.Enabled := false;

  if Assigned(Exchange) and (Exchange.Portopen) then begin
    if Exchange.PollPort then
    begin
      FNoOfAbsentMessages:= 0;
      FMessagesReceived:= true;
      FormCommLost.hide;
    end
    else
      FNoOfAbsentMessages:= FNoOfAbsentMessages + 1;
  end;
  //if no communication with ATP for a while.
  if (FNoOfAbsentMessages > FMaxNoOfAbsentMessages) then
  begin
    FNoOfAbsentMessages:= 0;
    FMessagesReceived:= false;
    DMIReset;
    FormCommLost.Show;
    HideAllForm;
  end;

  if waitingForVersionFromATPCount > 0 then
  begin
    Dec(waitingForVersionFromATPCount);
    if (waitingForVersionFromATPCount = 0) then
    begin
        { timeout expired without any version received from ATP }
      FormAtpMessages.AddModalMessage('Required version information not received from ATP. MMI is not compatible with ATP! Please, check the versions of MMI and ATP!',0);
      FormAtpMessages.ShowModal;

      FormMMIFrame.Close;

    end;
  end;

  ExchangeTimer.Enabled := true;

end;

procedure TFormMMIFrame.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Attributes: Integer;
begin
  {Close Remote Interface}
  IdTCPServerRemoteDMIInterface.Active := false;
  IdTCPServerRemoteDMIInterfaceInternal.Active := false;

  { Save windows pos to Ini-file }
  if (InitFile <> nil) then
  begin
    { Handle to Init-file exists }

    { Only save window pos if not minimized or maximized }
    if (WindowState = wsNormal) then
    begin
      { Only save to file if not Read-Only }
      Attributes := FileGetAttr(InitFileName);
      if ((Attributes and SysUtils.faReadOnly) = 0) then
      begin
        InitFile.WriteInteger('MMI', 'Left', FormMMIFrame.Left);
        InitFile.WriteInteger('MMI', 'Top', FormMMIFrame.Top);
      end;

    end;

  end;

end;

procedure TFormMMIFrame.FormCreate(Sender: TObject);
var
  MMIPath: String ;
begin
  Caption := 'DMI - ' + GetVersion(Application.ExeName);

  LinesToSend := TStringList.Create;

  NeedToMinimize := false;
  TestMode:= False ;
  HiRes := false;
  Slice := 0;

  MMIPath:=ExtractFilePath(Application.ExeName);
  ReadIniFiles(MMIPath);

  TcpClientNJRU.RemoteHost := NJRU.Host;
  TcpClientNJRU.RemotePort := IntToAnsiStr(NJRU.Port);

  TcpClientNJRU.Active := true;

  AppendLogLevelMenu;

  Exchange:=TExchange.Create ;
   LoggedOut := false;
end;

{*********************************************************
* Function:    DMIReset
* Description: Init variables. Shall be called at init but also
*              when AOS restarts
*
*********************************************************}
Procedure TFormMMIFrame.DMIReset();
begin

      // Init variables
  Exchange.StartUpAlreadySent:=false;  // Resend when communication recovers
  FMMIStatus:= 1 ;
  LoggedOut := false;
  DataModuleDMI.DataModuleClean;
  FormMainArea.MMIAreaD.EraseAllLists;
  FormMainArea.MMIareaD.CurrentOdometerPosition := 0;
end;

procedure TFormMMIFrame.FormDestroy(Sender: TObject);
begin

  TimerPending.Enabled := false;

  LinesToSend.Clear;
  LinesToSend.Free;

  FlanguageFile.Free ;

  Exchange.MMICLexit;
  Exchange.Destroy ;

end;


procedure TFormMMIFrame.FormShow(Sender: TObject);
Var
  InitResult: Integer ;
begin
  try
  begin
    if not Exchange.Portopen then
    begin
      InitResult:= Exchange.init(FComPort,MMICLDebug);
      if (InitResult <> 0) then
      begin

        if SystemRestartEnable then
        begin

          ShowMessage('Failed to init communication with ATP! Restart PC now [OK]!');
          if not ExitWin(EWX_REBOOT) then
            MessageBox(Handle,PWideChar('ExitWin failed'),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

        end
        else
        begin
          ShowMessage('Failed to init communication with ATP! Check port settings and MMICL.DLL');
        end;
      end;
    end;

    DataModuleDMI.ClientDataSetDMIEvents.Active := True;

    Exchange.SendStartUp;
    ExchangeTimer.Enabled:= True ;
    Timer.Enabled := true;

        // Start Remote DMI Interface
    if FRemoteInterfaceEnabled then
    begin
        // Clear any binding
      IdTCPServerRemoteDMIInterface.Bindings.Clear;
        // Add the listening port
      IdTCPServerRemoteDMIInterface.Bindings.Add.Port := FRemoteInterfacePort;
        // Start the server
      IdTCPServerRemoteDMIInterface.Active := true;

        // Clear any binding
      IdTCPServerRemoteDMIInterfaceInternal.Bindings.Clear;
        // Add the listening port
      IdTCPServerRemoteDMIInterfaceInternal.Bindings.Add.Port := FRemoteInterfaceInternalPort;
        // Start the server
      IdTCPServerRemoteDMIInterfaceInternal.Active := true;
    end;
  end;


  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorFormShow);

      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in FormShow:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

      Application.Terminate ;

    end;
  end;

end;

{*********************************************************
* Function:    SendToNJRU
* Description: Queue and send lines to N-JRU
*
*********************************************************}
procedure TFormMMIFrame.SendToNJRU(LogString:AnsiString);
begin

  LinesToSend.Append(String(LogString));

      { Remove the oldest if too many messages in queue }
  while LinesToSend.Count > MaxPendingLogLines do
    LinesToSend.Delete(0);

  SendPendingLine();

end;

procedure TFormMMIFrame.TimerPendingTimer(Sender: TObject);
begin

  TimerPending.Enabled := false;

      { Timer will be re-enabled if any line pending }
  SendPendingLine;

end;

procedure TFormMMIFrame.TimerTimer(Sender: TObject);
begin
                        // Disable timer and reenable at the end of this function
  Timer.Enabled := false;

  if NeedToMinimize then
  begin
    Application.Minimize;
    NeedToMinimize := false;
  end;

  Slice:= ((Slice +1) mod 4) ;
  case Slice of
      1:begin
                        // Move any FormFlash around the screen
          if Assigned(FormFlash) and FormFlash.Visible then
          begin
            FormFlash.Hide;
            FormFlash.Top := 10 + Random(ClientHeight - 10 - FormFlash.Height);
            FormFlash.Left := 10 + Random(ClientWidth - 10 - FormFlash.Width);
            FormFlash.Show;
          end;
        end;
      3:begin
//          if (Exchange.FComOK and FMessagesReceived) then
         if (Exchange.FComOK) then
          begin

            Exchange.SendStartUp;  // Startup - message will actually only be sent
                                      // if Not Ser.startupAlreadySent
            Exchange.SendMMIStatusToATP ;
          end;
        end;
   else
   end;

  Timer.Enabled := true;

end;

{*********************************************************
* Function:    SendPendingLine
* Description: Send the first (oldest) pending line to N-JRU
*
*********************************************************}
procedure TFormMMIFrame.SendPendingLine;
var
  LogString : AnsiString;
begin

  if LinesToSend.Count > 0 then
  begin
        { There is at least one pending line to send}
    LogString := AnsiString(LinesToSend.Strings[0]);
    if (TcpClientNJRU.Sendln(LogString,LF) > 0) then
    begin
        { Send was successful, remove from buffer }
      LinesToSend.Delete(0);
    end
    else
    begin
        { Send was not successful , close and open connection}
      TcpClientNJRU.Active := false;
      TcpClientNJRU.Active := true;

    end;

  end;

  if LinesToSend.Count > 0 then
          { Send the pending line later }
      TimerPending.Enabled := true;

end;


{*********************************************************
* Function:    FormMMIFrame.ReadIniFiles
* Description: Read DMI.INI located in the same directory
*              the MMI application
*********************************************************}

procedure TFormMMIFrame.ReadIniFiles(MMIPath : String);
var

  Chapter : String;
  Language: String ;
  I: Integer ;
  StartUpWindowState : TWindowState;

begin

          {Filename is used later when window position is saved in order to
           check that file is not Read-Only, Bo H, 2011-10-12}

          {Path to ini-file may be passed on the command-line as 1st argument
           Bo H, 2018-11-03}
  if ParamCount > 0 then
   InitFileName  := ParamStr(1)
  else
   InitFileName  := MMIPath+'DMI.ini';

  InitFile      := TIniFile.Create(InitFileName);
  if (InitFile <> Nil ) then
  begin

    NJRU.Host := AnsiString(InitFile.ReadString('N-JRU','Host','127.0.0.1'));
    NJRU.Port := InitFile.ReadInteger('N-JRU','Port',55131);


          { Windowed or full screen ?}
    if (InitFile.ReadInteger('MMI','FrameStyle',0) = 0)
    then
    begin
          {Remove caption and border}
      BorderStyle := bsNone;
          {Set frame size to screen size}
      Left:=0;
      Top:=0;
      Height:=Screen.Height;
      Width:=Screen.Width;
    end
    else
    begin

      Left:=InitFile.ReadInteger('MMI','Left',FormMMIFrame.Left);
      Top:=InitFile.ReadInteger('MMI','Top',FormMMIFrame.Top);
          { Use ClientHeight/Width to reserve extra space for the caption and border }
      ClientHeight:=InitFile.ReadInteger('MMI','Height',FormMMIFrame.ClientHeight);
      ClientWidth:=InitFile.ReadInteger('MMI','Width',FormMMIFrame.ClientWidth);

        // Window state at startup
      StartUpWindowState := TWindowState(InitFile.ReadInteger('MMI','WindowState',Ord(wsNormal)));
      if (StartUpWindowState = wsMinimized) then
       NeedToMinimize := true;

    end;
    if Width >= 1024 then
    begin
      HiRes := True;  {Use high-resolution bitmaps}
    end;

          {Hide cursor, call ShowCursor(False) until it returns -1}
    if not InitFile.ReadBool('MMI','Cursor',False) then
      while ShowCursor(False) > -1 do ;

          {Sound}
    Sound:=InitFile.ReadBool( 'MMI','Sound',False) ;

    MaxStartUpHistoryLines:= InitFile.ReadInteger( 'MMI','MaxStartUpHistoryLines',500) ;
    MaxYardModeResponseLines:= InitFile.ReadInteger( 'MMI','MaxYardModeResponseLines',5000) ;

          {Max no of absent messages before MMI assumes communication is lost}
    FMaxNoOfAbsentMessages:= InitFile.ReadInteger( 'MMI', 'MaxLostMessages', 20) ;

          {Language}
    Language:= InitFile.ReadString('MMI','Language','English') ;

          {LogLevel}
    LogLevel:= InitFile.ReadInteger( 'MMI','LogLevel',1) ;

          {System Restart}
    SystemRestartEnable :=  InitFile.ReadBool( 'MMI','SystemRestart',False) ;

          {Log path}
    LogPath := InitFile.ReadString('ErrorLog','LogPath','D:\Log') ;


          {Test mode}
    TestMode:= InitFile.ReadBool( 'TEST','TestMode',False) ;
    if TestMode then
    begin
      MMICLDebug:= InitFile.ReadInteger('TEST','MMICLDebug', 0 );
      TSPos := InitFile.ReadInteger('TEST','TSPos', 0 );
      OdoPos := InitFile.ReadInteger('TEST','OdoPos', 0 );
      AltScreens := InitFile.ReadInteger('TEST','AltScreens', 0 );
    end
    else
    begin
      MMICLDebug:= 0;
      TSPos := 0;
      AltScreens := 0;
      OdoPos := 0;
    end;

          {Remote port}
    FRemoteInterfaceEnabled:= InitFile.ReadBool( 'RemoteInterface','Enabled',False) ;
    FRemoteInterfacePort:= InitFile.ReadInteger( 'RemoteInterface','Port',30197) ;
    FRemoteInterfaceInternalPort:= InitFile.ReadInteger( 'RemoteInterface','InternalPort',30196) ;


          {MMICL DLL}
          {MMICL.DLL may either support serial or TCP/IP communication}
          {ComPort for serial port or TCP/IP - port}
    FComPort:= InitFile.ReadInteger( 'MMICL','ComPort',30130) ;

            {**** Language file ****}
    LanguageFile:=TIniFile.Create(MMIPath+Language+'.ini') ;
            { Read all interface strings.}
    if LanguageFile <> nil then
    begin
      for I:= 0 to Ord(ifMax) -1 do
      begin
        FInterfaceString.Add(LanguageFile.ReadString(Language,
                TInterfacestring[TIfaceEnum(I)][ifsIni],
                TInterfacestring[TIfaceEnum(I)][ifsDefault])) ;
      end;

      //Read all speed reason strings.
      for I:= 0 to Ord(ssrMax) -1 do
      begin
        FSpeedReasonString.Add(LanguageFile.ReadString(Language,
            TSpeedReasonStr[TCeilingSpeedReason(I)][ifsIni],
            TSpeedReasonStr[TCeilingSpeedReason(I)][ifsDefault])) ;
      end;

      // Read Adaptation speed reason string
      for I := 0 to ord(ssrLevelCrossing) do
       begin
        FAdapSpeedReasonString.Add(LanguageFile.ReadString(Language,
            TAdapSpeedReasonStr[ssrLevelCrossing][ifsIni],
            TAdapSpeedReasonStr[ssrLevelCrossing][ifsDefault])) ;
       end;

            { Read all predefined text message strings. The stringlist with
              predefined messages will be filled up with empty strings where there
              are no messages defined. This way we will not have to search for a
              specific message, the message we want will be on the same index as the
              message specifier sent from ATP. }
      for i:= 0 to 255 do
      begin
        if LanguageFile.ValueExists('PredefinedTextMsgs', intToStr(i)) then
        begin
          FPredefTextMsgString.Add(LanguageFile.ReadString(
                                     'PredefinedTextMsgs',
                                     intToStr(i),
                                     ''));
        end
        else  { Add an empty string if no message in ini-file. }
          FPredefTextMsgString.Add('');
      end;

              { Types of cars }
      for I:= 1 to MAX_VEHICLE_TYPE_BLOCKS do
      begin

        Chapter := 'MMI_CAR_TYPE_' + IntToStr(I);
        CarBlockTypes[I].Name := LanguageFile.ReadString(Chapter,'Name','') ;
        CarBlockTypes[I].ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString(Chapter,'Image','')) ;
        CarBlockTypes[I].Other := LanguageFile.ReadBool(Chapter,'Other',False);
        if CarBlockTypes[I].Other then
          CarBlockTypeOther := I;

      end;

              { Custom images }
      ImageCarsConnectedA.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','CarsConnectedA',''));
      ImageCarsConnectedB.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','CarsConnectedB',''));

      ImageLocoVSTrackAForward.ImagePath  := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','LocoVSTrackAForward',''));
      ImageLocoVSTrackAReverse.ImagePath  := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','LocoVSTrackAReverse',''));
      ImageLocoVSTrackBForward.ImagePath  := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','LocoVSTrackBForward',''));
      ImageLocoVSTrackBReverse.ImagePath  := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','LocoVSTrackBReverse',''));
      ImageTrainVSTrackAForward.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','TrainVSTrackAForward',''));
      ImageTrainVSTrackAReverse.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','TrainVSTrackAReverse',''));
      ImageTrainVSTrackBForward.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','TrainVSTrackBForward',''));
      ImageTrainVSTrackBReverse.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','TrainVSTrackBReverse',''));
      ImageTrainVSTrackAForwardCarAtA.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','TrainVSTrackAForwardCarAtA',''));
      ImageTrainVSTrackAReverseCarAtA.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','TrainVSTrackAReverseCarAtA',''));
      ImageTrainVSTrackBForwardCarAtA.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','TrainVSTrackBForwardCarAtA',''));
      ImageTrainVSTrackBReverseCarAtA.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE','TrainVSTrackBReverseCarAtA',''));
      ImageConfiguration.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'Configuration', ''));
      ImageYardMode.ImagePath      := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'YardMode', ''));
      ImageReady.ImagePath         := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'Ready', ''));
      ImageClear.ImagePath         := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'Clear', ''));
      ImageATOUndefined.ImagePath  := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'ATOUndefinde', ''));
      ImageATOManual.ImagePath     := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'ATOManual', ''));
      ImageATOSupervised.ImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'ATOSupervised', ''));
      ImageATOSupervised.HiResImagePath := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'HiRes_ATOSupervised', ImageATOSupervised.ImagePath));
      ImageATOAutomatic.ImagePath  := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'ATOAutomatic', ''));
      ImageATORemote.ImagePath     := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'ATORemote', ''));
      ImageTrainLoaded.ImagePath   := AbsoluteFilePath(MMIPath, LanguageFile.ReadString('IMAGE', 'LoadedTrain', ''));

      { Replace default short month names (used by FormatDateTime()) , ver 2.8.26 }
      for i := 1 to 12 do
      begin
        FormatSettings.ShortMonthNames[i] := LanguageFile.ReadString('ShortMonthNames',IntToStr(i),FormatSettings.ShortMonthNames[i]);
      end;


    end;
  end;
end;

{*********************************************************
* Function:    SetInitFileName
* Description: Sets the InitFileName string
*********************************************************}
Procedure TFormMMIFrame.SetInitFileName(Value: String) ;
begin

  FInitFileName := Value;

end;
{*********************************************************
* Function:    SetInitFile
* Description: Sets the InitFile property
*********************************************************}
Procedure TFormMMIFrame.SetInitFile(Value: TIniFile) ;
begin
  FInitFile := Value;
end;
{*********************************************************
* Function:    SetLanguageFile
* Description: Sets the InitFile property
*********************************************************}
Procedure TFormMMIFrame.SetLanguageFile(Value: TIniFile) ;
begin
  FLanguageFile := Value;
end;
{*********************************************************
* Function:    SetSound
* Description: Sets the Sound property
*********************************************************}
Procedure TFormMMIFrame.SetSound(Value: Boolean) ;
begin
  FSound := Value;
end;

{*********************************************************
* Function:    SetTestMode
* Description: Sets the TestMode property
*********************************************************}
Procedure TFormMMIFrame.SetTestMode(Value: Boolean) ;
begin
  FTestMode := Value;
end;


{*********************************************************
* Function:    WriteMMIStatus
* Description: access of the mmistatus variable
*********************************************************}
Function TFormMMIFrame.WriteMMIStatus( LogUnit:AnsiString;
                                       TypeError:Byte ;
                                       Module:Byte ;
                                       Error: Word): Word ;
begin
  FMMIStatus:= 1 ;
  FMMIStatus:=FMMIStatus or
                TypeError or
                Module ;

{
  if FDoUseLogFile then
  begin
    if Wordbool(TypeError) or Wordbool(Module) or Wordbool(Error) then
    begin

      LogEvent(LogLevelNormal, LogUnit, TypeError, Module, Error);

    end;
    Exchange.SendMMIStatusToATP ;
  end;
}
  result:= FMMIStatus;
end;

{*********************************************************
* Function:    ReadMMIStatus
* Description: access of the mmistatus variable
*********************************************************}
Function TFormMMIFrame.ReadMMIStatus: Word ;
begin
   Result:= FMMIStatus ;
end;


{*********************************************************
* Function:    TranslateString
* Description: Translate a string
*              Read translated string from Ini-file or use
*              default if translation not found
*********************************************************}


function TFormMMIFrame.TranslateString(Chapter : String; Specifier : String; DefaultStr : AnsiString; MaxSize : Integer) : AnsiString;
var
  TranslatedStr, ThisTranslatedStr : AnsiString;
  TagStr        : String;
  SpecifierPos, NextSpecifierPos
                : Integer;
  TagPos        : Integer;

begin

  TranslatedStr := '';

  SpecifierPos := Pos(Specifier,String(DefaultStr));
  if SpecifierPos > 0 then
  begin

    repeat
        { Where is the tag located ?}
      TagPos := SpecifierPos + Length(Specifier);

        { Any next tag in string ?}
      NextSpecifierPos := PosEx(AnsiString(Specifier), DefaultStr, TagPos);

        { Translate using tag found at TagPos}
        { Concatenate if several codes found}
      if (NextSpecifierPos = 0) then
      begin
        { No more codes in this string }
        TagStr := Copy(String(DefaultStr), TagPos, MaxSize);
        ThisTranslatedStr := AnsiString(LanguageFile.ReadString(chapter, TagStr, ''));
      end
      else
      begin
        { More codes in this string }
        TagStr := Copy(String(DefaultStr), TagPos, (NextSpecifierPos - SpecifierPos - Length(Specifier)));
        ThisTranslatedStr := AnsiString(LanguageFile.ReadString(chapter, TagStr, ''));
      end;


      if Length(ThisTranslatedStr) > 0 then
      begin
        TranslatedStr := TranslatedStr + ThisTranslatedStr;
      end
      else
      begin
        TranslatedStr := TranslatedStr + AnsiString(InterfaceString(ifUnknownTextId)  +  Specifier + TagStr );
      end;

      SpecifierPos := NextSpecifierPos;

    until (NextSpecifierPos = 0);

    if Length(TranslatedStr) > 0 then
    begin
        { Limit to Max size }
      Result := Copy(TranslatedStr, 1, MaxSize);
    end
    else
    begin
        { Translation not found }
      Result := AnsiString(InterfaceString(ifUnknownTextId)+  String(DefaultStr));

    end;

  end
  else
  begin
        { Not to be translated }
    Result := DefaultStr;
  end;

end;

{*********************************************************
* Function:    CheckVersion
* Description:
*********************************************************}
Procedure TFormMMIFrame.CheckVersion();
begin

  if Not ATPVersionChecked  then
  begin

    if (waitingForVersionFromATPCount = 0) then
    begin
      waitingForVersionFromATPCount:= COMPATIBILITY_VERSION_READ_TIMEOUT * (1000 div ExchangeTimer.Interval);
    end;

  end;


end;

{*********************************************************
* Function:    VerifyVersion
* Description:
*********************************************************}
Procedure TFormMMIFrame.VerifyVersion(CompatibilityVersion : Byte);
begin

  waitingForVersionFromATPCount := 0;

  if CompatibilityVersion <> COMPATIBILITY_VERSION then
  begin

    FormAtpMessages.AddModalMessage('Version received from ATP is not compatible with MMI! Please, check the versions of MMI and ATP!',0);
    FormAtpMessages.ShowModal;

    FormMMIFrame.Close;

  end;

  ATPVersionChecked := true;

end;

{ *********************************************************
  * Function:    LogEventStr
  * Description: Format a log-line and send to N-JRU
  ********************************************************* }
procedure TFormMMIFrame.LogEventStr(Level: integer; LogUnit: AnsiString;
  LogStr: AnsiString);

var
  LogLine: String;
begin

  if (Level <= LogLevel) then
  begin

    { Fill info about track and pos with blanks. Always format time in 24 hour mode }
    { Time sent to NJRU shall be in UTC-format }
    LogLine := FormatDateTime('hh:mm:ss.zzz',
      TTimeZone.Local.ToUniversalTime(Now)) + '                      ' + 'DMI   '
      + String(LogUnit) + StringOfChar(' ', 5 - Length(LogUnit)) + '    : ' +
      String(LogStr);
    SendToNJRU(AnsiString(LogLine));

  end;
end;


{*********************************************************
* Function:    LogEvent
* Description: Format a log-line and send to N-JRU
*********************************************************}
procedure TFormMMIFrame.LogEvent(Level:Integer; LogUnit:AnsiString; TypeError:Byte; Module:Byte; Error:Word);
var
  LogStr : String;
begin
  LogStr := 'Type:' + String(HexToStr(TypeError)) + ' Module:' + String(HexToStr(Module)) + ' Error:' + IntToStr(Error);
  LogEventStr(Level, LogUnit, AnsiString(LogStr));
end;

{*********************************************************
* Function:    SetMMITime
* Description: Sets the realtime watch in the IDU by
               calling the win32 function Setlocaltime
*********************************************************}
Procedure TFormMMIFrame.SetMMITime(Time: TDateTime) ;
Var
  SystemTime: TSystemTime ;
  TimeString: AnsiString;

begin
  try
    DateTimeToSystemTime(Time,SystemTime) ;
    SetSystemTime(SystemTime);

    LogEventStr(LogLevelDetail, AnsiString('MAIN'), AnsiString('Setting time received from ATP..'));
    TimeString:= AnsiString(IntToStr(SystemTime.wYear)+'-'+IntToStr(SystemTime.wMonth)+'-'
            +IntToStr(SystemTime.wDay)+ ' ' + IntToStr(SystemTime.wHour) + ':' + IntToStr(SystemTime.wMinute) + ':' + IntToStr(SystemTime.wSecond) + '.' + IntToStr(SystemTime.wMilliseconds));
    LogEventStr(LogLevelDetail, 'MAIN', TimeString);

    FormFullScreenMsg.SetMessage(ifMMIClk);
  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSetMMITime);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in SetMMITime:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
  end;
end;

{*********************************************************
* Function:    SetMMI_ATPModeAndState
* Description: Main Function that sets the MMI program
               in different working modes and states
*********************************************************}
procedure TFormMMIFrame.SetMMI_ATPModeAndState;
var

  ATPMode : T_ATPmodes;
  ATPModeChanged : Boolean;

  ATPState : T_ATPStates ;
  ATPStateChanged : Boolean;

  ATPVerificationState : T_ATPVerificationStates;
  ATPVerificationStateChanged : Boolean;

  ConfigSubState: TConfigSubState ;
  ConfigSubStateChanged : Boolean;

begin

  ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
  ATPState := DataModuleDMI.GetATPState(ATPStateChanged);
  ConfigSubState := DataModuleDMI.GetConfigSubState(ConfigSubStateChanged);
  ATPVerificationState := DataModuleDMI.GetATPVerificationState(ATPVerificationStateChanged);

  try
              // Assigned() will tell us if the object is created or not.
              // At start-up this routine may be executed before other objects are
              // created.

                // Hide/show the screensaver.
    if Assigned(FormFlash) then
    begin

      if (ATPState = asInactive) then
      begin
        if not FormFlash.Visible then
          FormFlash.Show;
      end
      else
        FormFlash.Hide;

    end;

              // Hide/show the fatal failure screen.
    if ATPState = asFatalFailureState then
    begin
      if Assigned(FormSafetyHalt) then
      begin
        if Not FormSafetyHalt.Visible then
          FormSafetyHalt.Show;
      end;
    end
    else
    begin
      if Assigned(FormSafetyHalt) then
        FormSafetyHalt.Hide;
    end;

        { If the mode has changed, do the MMI mode change }
    if (ATPModeChanged or ATPStateChanged or ATPVerificationStateChanged or
       (ATPMode = amTrainconfiguration) or (ATPMode = amYard)) then
    begin
          // Hide unreg form.
      if ATPMode <> amUnregistered then
          FormUnreg.Hide;

        { ********************* }
        { *  Which ATP mode?  * }
        { ********************* }
      case ATPMode of

        amUnregistered:
          begin
          amTrainUnRegistrationState(ATPState, ATPVerificationState);
          end;

        amUndefined:
          begin
            if ATPState = asFatalFailureState then
              SetFatalFailure
            else
              SetasInactive;
          end;

        amPowerup:
          begin

            amPowerUpState(ATPState, ATPVerificationState);

          end;

        amPoweringDown:
        begin
          amPoweringDownState(ATPState,ATPVerificationState);
        end;

        amYard:
          begin
            amNoMAControlledModeState(ATPState, ATPVerificationState);
          end;

        amTrainConfiguration:
          begin
            amTrainConfigurationState(ATPState, ConfigSubState, ATPVerificationState);
          end;

        amTrainregistration:
          begin
            amTrainRegistrationState(ATPState, ATPVerificationState);
          end;

        amBalisSearchMode:
          begin
            amBaliseSearchState(ATPState, ATPVerificationState);
          end;

        amNormal:
          begin
            amNormalState(ATPState, ATPVerificationState);
          end;

        amShunting:
          begin
            amNoMAControlledModeState(ATPState, ATPVerificationState);
          end;

        amLocation:
          begin
            amLocationState(ATPState, ATPVerificationState);
          end;

        amStaffResponsible:
          begin
            amNormalState(ATPState, ATPVerificationState);
          end;

        amShuntingRoute:
          begin
            amNormalState(ATPState, ATPVerificationState);
          end;

        amPossession:
          begin
            amNoMAControlledModeState(ATPState, ATPVerificationState);
          end;

        amSleeping:
          begin
            amTrainSleepingState(ATPState, ATPVerificationState);
          end;
        amSplit:
          begin
            amNormalState(ATPState, ATPVerificationState);
          end;
        amJoin:
          begin
            amNormalState(ATPState, ATPVerificationState);
          end;
        amSafeBrakeToStop:
          begin
            amNormalState(ATPState, ATPVerificationState);
          end;
        amSafetyHalt:
          begin
            SetFatalFailure;
          end;


            { default ATPMode }
        else
        begin
          case ATPState of
            asFatalFailureState:
              SetFatalFailure;

            asPowerDown:
              SETasPowerDown;

            asSystemRestart:
              SETasSystemRestart;

          else
            SETUndefined(InterfaceString(ifATPStU));
        end;
      end; { End of case ATPmode. }
    end;
  end;

  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN',MS_SW_ERROR, MS_MAIN, MainLogErrorSetMMI_ATPModeAndState);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in SetMMI_ATPModeAndState:'+ E.message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
  end;

end;

{*********************************************************
* Function:    amPowerUpState
* Description: Handle ATPState when in ATPMode PowerUp
*********************************************************}
procedure TFormMMIFrame.amPowerUpState(ThisATPState : T_ATPStates  ;
                            ThisATPVerification : T_ATPVerificationStates);
begin

  case ThisATPState of
    asUndefined:
      SETUndefined(InterfaceString(ifATPStU));

    asPowerDown:
      SETasPowerDown;

    asSystemRestart:
      SETasSystemRestart;

    asBasicsSystemStartUp:
      SETasBasicsSystemStartUp;

    asApplicationStartUp:
      begin
        SETasBasicsSystemStartUp;
        DMIReset;
        FormMainArea.MMIareaD.EraseAllLists;
        FormFullScreenMsg.Show;
      end;

    asActivationInitiation:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETavsNoActionState;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState;

          avsLoggedOnState:
          begin
            SETUndefined(InterfaceString(ATPManCnfWait));
          end;
          else
            SETUndefined(InterfaceString(ifATPVsstU));
        end;
      end;

    asInactive:
      SETasInactive;

    asActivationTest:
      SETasActivationTest;

    else
      if ThisATPState = asFatalFailureState then
        SetFatalFailure;
  end;

end;

{*********************************************************
* Function:    amPoweringDownState
* Description: Handle ATPState when in ATPMode PoweringDown
*********************************************************}
procedure TFormMMIFrame.amPoweringDownState;
begin

if ThisATPState =  asActiveState  then
  FormPowerDown.Show;

end;

{*********************************************************
* NoMAControlledModeState
* Description: Handle ATPState when in ATPMode YardMode, Posseesion or Shunting
*********************************************************}
procedure TFormMMIFrame.amNoMAControlledModeState(ThisATPState : T_ATPStates;
                               ThisATPVerification : T_ATPVerificationStates );
begin

  case ThisATPState of
    asInactive:
      SETasInactive;

    asActivationTest:
      SETasActivationTest;

    asFatalFailureState:
      SetFatalFailure;

    asActivationInitiation:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETavsNoActionState;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState

          else
            SETUndefined(InterfaceString(ifATPStU));
        end;
      end;

    asActiveState:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETamNOMAControlledMode;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState;

          avsLoggedOnState:
            SETamNOMAControlledMode;
        end;
      end;

    asPowerDown:
      SETasPowerDown();

    asSystemRestart:
      SETasSystemRestart;


    else
      SETamNOMAControlledMode;
  end;

end;
{*********************************************************
* Function:    FormMainArea.amTrainConfigurationState
* Description: Handle ATPState when in ATPMode TrainConfiguration
*********************************************************}
procedure TFormMMIFrame.amTrainConfigurationState(ThisATPState : T_ATPStates  ;
                            ThisConfigSubState : TConfigSubState;
                            ThisATPVerification : T_ATPVerificationStates);
begin

  case ThisATPState of
    asInactive:
      SETasInactive;

    asActivationTest:
      SETasActivationTest;

    asFatalFailureState:
      SetFatalFailure;

    asPowerDown:
      SETasPowerDown();

    asSystemRestart:
      SETasSystemRestart;

    asActivationInitiation:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETavsNoActionState;

          avsInputState:
          begin
            SETavsInputState;
             LoggedOut := true;
          end;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState;

          else
            SETUndefined(InterfaceString(ifATPVsstU));
        end;
      end;

    asActiveState:
      case ThisConfigSubState of
        cssManual:
          SETcssManual;
        cssReReg:
          SETcssReConfig;
        cssRestartConfig:
         SETcssRestartConfig;
        cssAutomatic:
         SETcssAutomatic;
        cssUndefined:
          SETUndefined(InterfaceString(ifATPDefTrn))
      end;
    else
      SETUndefined(InterfaceString(ifATPStU));
  end;
end;
{*********************************************************
* Function:    FormMainArea.amTrainRegistrationState
* Description: Handle ATPState when in ATPMode TrainRegistration
*********************************************************}
procedure TFormMMIFrame.amTrainRegistrationState(ThisATPState : T_ATPStates  ;
                            ThisATPVerification : T_ATPVerificationStates);
begin
  case ThisATPState of
    asInactive:
      SETasInactive;

    asActivationTest:
      SETasActivationTest;

    asFatalFailureState:
      SetFatalFailure;

    asPowerDown:
      SETasPowerDown();

    asSystemRestart:
      SETasSystemRestart;

    asActiveState:
      SETamTrainregistration;

    asActivationInitiation:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETavsNoActionState;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState

          else
            SETUndefined(InterfaceString(ifATPVsstU));
        end;
      end;
    else
      SETUndefined(InterfaceString(ifATPStU));

  end;

end;

{*********************************************************
* Function:    FormMainArea.amBaliseSearchState
* Description: Handle ATPState when in ATPMode Balise Search
*********************************************************}
procedure TFormMMIFrame.amBaliseSearchState(ThisATPState : T_ATPStates  ;
                            ThisATPVerification : T_ATPVerificationStates);
begin
  case ThisATPState of
    asInactive:
      SETasInactive;

    asActiveState:
      SETamNormal;

    asFatalFailureState:
      SetFatalFailure;

    asActivationTest:
      SETasActivationTest;

    asActivationInitiation:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETavsNoActionState;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState

          else
            SETUndefined(InterfaceString(ifATPVsstU));
        end;
      end;

    asPowerDown:
      SETasPowerDown();

    asSystemRestart:
      SETasSystemRestart;

    else
      SETUndefined(InterfaceString(ifATPStU));
  end;

end;
{*********************************************************
* Function:    amNormalState
* Description: Handle ATPState when in ATPMode Normal
*********************************************************}
procedure TFormMMIFrame.amNormalState(ThisATPState : T_ATPStates ;
                            ThisATPVerification : T_ATPVerificationStates);
begin
  case ThisATPState of
    asInactive:
      SETasInactive;

    asActivationTest:
      SETasActivationTest;

    asFatalFailureState:
      SetFatalFailure;

    asActivationInitiation:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETavsNoActionState;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState

          else
            SETUndefined(InterfaceString(ifATPStU));
        end;
      end;

    asActiveState:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETamNormal;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState;

          avsLoggedOnState:
            SETamNormal;
        end;
      end;

    asPowerDown:
      SETasPowerDown();

    asSystemRestart:
      SETasSystemRestart;


    else
      SETUndefined(InterfaceString(ifATPStU));
  end;

end;

{*********************************************************
* Function:    FormMainArea.amLocationState
* Description: Handle ATPState when in ATPMode Location
*********************************************************}
procedure TFormMMIFrame.amLocationState(ThisATPState : T_ATPStates ;
                            ThisATPVerification : T_ATPVerificationStates);
begin

  case ThisATPState of
    asInactive:
      SETasInactive;

    asActiveState:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETamManualLocation;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState;

          avsLoggedOnState:
            SETamManualLocation;

          else
            SETUndefined(InterfaceString(ifATPStU));
        end;
      end;

    asActivationTest:
      SETasActivationTest;

    asActivationInitiation:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETavsNoActionState;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState

          else
            SETUndefined(InterfaceString(ifATPStU));
        end;
      end;

    asFatalFailureState:
      SetFatalFailure;

    asPowerDown:
      SETasPowerDown();

    asSystemRestart:
      SETasSystemRestart;

    else
      SETUndefined(InterfaceString(ifATPStU));
  end;

end;

{*********************************************************
* Function:    FormMainArea.SETasPowerDown
* Description: Function that sets the MMI in PowerDown state
*********************************************************}
Procedure TFormMMIFrame.SETasPowerDown;
begin

  try
    FormLogin.Hide;
    FormManualConfiguration.Hide;
    FormReRegistration.Hide;
    FormTrainVsTrack.Hide;
    FormFullScreenMsg.Hide;
    FormNoMAControlled.Hide;
    FormPowerDown.Show;
  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETasPowerDown);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in SETasPowerDown:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
  end;

end;


{*********************************************************
* Function:    FormMainArea.amTrainSleepingState
* Description: Handle ATPState when in ATPMode Sleeping
*********************************************************}
procedure TFormMMIFrame.amTrainSleepingState(ThisATPState : T_ATPStates  ;
                            ThisATPVerification : T_ATPVerificationStates);
begin
  case ThisATPState of
    asInactive:
      SETasInactive;

    asActivationTest:
      SETasActivationTest;

    asFatalFailureState:
      SetFatalFailure;

    asPowerDown:
      SETasPowerDown();

    asSystemRestart:
      SETasSystemRestart;

    asActiveState:
    begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETasSleeping;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState;

          avsLoggedOnState:
            SETasSleeping;
        end;
    end;

      asActivationInitiation:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETavsNoActionState;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState

          else
            SETUndefined(InterfaceString(ifATPVsstU));
        end;
      end;
    else
      SETUndefined(InterfaceString(ifATPStU));

  end;

end;



{*********************************************************
* Function:    FormMainArea.amTrainUnRegistrationState
* Description: Handle ATPState when in ATPMode UnRegistration
*********************************************************}
procedure TFormMMIFrame.amTrainUnRegistrationState(ThisATPState : T_ATPStates  ;
                            ThisATPVerification : T_ATPVerificationStates);
begin
  case ThisATPState of
    asInactive:
      SETasInactive;

    asActivationTest:
      SETasActivationTest;

    asFatalFailureState:
      SetFatalFailure;

    asPowerDown:
      SETasPowerDown();

    asSystemRestart:
      SETasSystemRestart;

    asActiveState:
    begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETasSleeping;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState;

          avsLoggedOnState:
            SetUnregistered;
        end;
    end;

    asActivationInitiation:
      begin
        case ThisATPVerification of
          avsUndefined:
            SETUndefined(InterfaceString(ifATPVsstU));

          avsNoActionState:
            SETavsNoActionState;

          avsInputState:
            SETavsInputState;

          avsverificationState:
            SETavsverificationState;

          avsRedoinputState:
            SETavsRedoinputState

          else
            SETUndefined(InterfaceString(ifATPVsstU));
        end;
      end;
    else
      SETUndefined(InterfaceString(ifATPStU));

  end;

end;



{*********************************************************
* Function:    FormMainArea.SETasSleeping
* Description: Function that sets the MMI in Sleeping state
*********************************************************}
Procedure TFormMMIFrame.SETasSleeping;
begin

  try
    FormLogin.Hide;
    FormAutomaticConfiguration.Hide;
    FormManualConfiguration.Hide;
    FormReRegistration.Hide;
    FormTrainVsTrack.Hide;
    FormFullScreenMsg.Hide;
    FormNoMAControlled.Hide;
    FormConfirmDepartureTest.Hide;

    FormSleepingMode.Show;
  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETasPowerDown);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in SETasPowerDown:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
  end;

end;

{*********************************************************
* Function:    FormMainArea.SETasSystemRestart
* Description: Function that sets the MMI in System restart state
*********************************************************}
Procedure TFormMMIFrame.SETasSystemRestart;
begin
  try

   FormLogin.Hide;
   FormAutomaticConfiguration.Hide;
   FormManualConfiguration.Hide;
   FormReRegistration.Hide;
   FormTrainVsTrack.Hide;
   FormNoMAControlled.Hide;
   FormConfirmDepartureTest.Hide;
   FormPowerDown.Hide;
   FormSleepingMode.Hide;
   FormFullScreenMsg.Show;

   if FormMMIFrame.SystemRestartEnable then
   begin

     Sleep(5000);
     if not ExitWin(EWX_REBOOT) then
      MessageBox(Handle,PWideChar('ExitWin failed'),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

   end;

  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETasSystemRestart);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in SETasSystemRestart:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
  end;

end;


{*********************************************************
* Function:    FormMainArea.SETUndefined
* Description: Function that sets the MMI in an Undefined
               Mode and state
*********************************************************}
Procedure TFormMMIFrame.SETUndefined (Output: String );
begin
  try
   FormAutomaticConfiguration.Hide;
   FormLogin.Hide;

   FormManualConfiguration.Hide;

   FormReRegistration.Hide;

   FormTrainVsTrack.Hide;
   FormNoMAControlled.Hide;
   FormSleepingMode.Hide;
   FormFullScreenMsg.SetMessage(Output);
   FormFullScreenMsg.Show;

  except
    on E: Exception do
    begin

      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETUndefined);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETUndefined:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
  end;
end;

{*********************************************************
* Function:    FormMainArea.SETasBasicsSystemStartUp
* Description: Function that sets the MMI in
               Basic system startup
*********************************************************}
Procedure TFormMMIFrame.SETasBasicsSystemStartUp;
begin
  try

   FormAutomaticConfiguration.Hide;
   FormManualConfiguration.Hide;
   FormReRegistration.Hide;
   FormLogin.Hide;
   FormTrainVsTrack.Hide;
   FormSleepingMode.Hide;
   FormNoMAControlled.Hide;
   FormConfirmDepartureTest.Hide;
   FormFullScreenMsg.Show;

  except
    on E: Exception do
    begin

      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETasBasicsSystemStartUp);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETasBasicsSystemStartUp:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
 end;
end;


{*********************************************************
* Function:    FormMainArea.SETasInactive
* Description: Function that sets the MMI to: inactive
*********************************************************}
Procedure TFormMMIFrame.SETasInactive;
begin
  try
   FormAutomaticConfiguration.Hide;
   FormManualConfiguration.Hide;
   FormReRegistration.Hide;
   FormLogin.Hide;
   FormTrainVsTrack.Hide;
   FormFullScreenMsg.Hide;
   FormNoMAControlled.Hide;
   FormSleepingMode.Hide;
   FormConfirmDepartureTest.Hide;
   Show;

  except
    on E: Exception do
    begin

      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETasInactive);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETasInactive:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
 end;
end;

{*********************************************************
* Function:    FormMainArea.SETavsNoActionState
* Description: Function that sets the MMI to: no action state
*********************************************************}
Procedure TFormMMIFrame.SETavsNoActionState;
begin
 try
   FormAutomaticConfiguration.Hide;
   FormManualConfiguration.Hide;
   FormReRegistration.Hide;
   FormLogin.Hide;
   FormTrainVsTrack.Hide;
   FormNoMAControlled.Hide;
   FormFullScreenMsg.SetMessage('');
   FormSleepingMode.Hide;
   FormConfirmDepartureTest.Hide;
   FormFullScreenMsg.Show;

 except

    on E: Exception do
    begin
      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETavsNoActionState);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETavsNoActionState:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
 end;
end;

{*********************************************************
* Function:    FormMainArea.SETavsInputState
* Description:
*********************************************************}
Procedure TFormMMIFrame.SETavsInputState;
var
  ATPVerificationStateChanged : Boolean;

begin

  DataModuleDMI.GetATPVerificationState(ATPVerificationStateChanged);

  try

   if ATPVerificationStateChanged then
     FormLogin.Clear;

   FormAutomaticConfiguration.Hide;
   FormManualConfiguration.Hide;
   FormReRegistration.Hide;
   FormTrainVsTrack.Hide;
   FormFullScreenMsg.Hide;
   FormNoMAControlled.Hide;
   FormSleepingMode.Hide;
   FormConfirmDepartureTest.Hide;
   FormLogin.Show;

  except

    on E: Exception do
    begin

      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETavsInputState);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETavsInputState:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
 end;
end;

{*********************************************************
* Function:    FormMainArea.SETavsverificationState
* Description: Displays a message while the Login-credentials
*              are being verified.
*********************************************************}
Procedure TFormMMIFrame.SETavsverificationState;
begin
  try
   FormAutomaticConfiguration.Hide;
   FormManualConfiguration.Hide;
   FormReRegistration.Hide;
   FormLogin.Hide;
   FormTrainVsTrack.Hide;
   FormSleepingMode.Hide;
   FormNoMAControlled.Hide;
   FormConfirmDepartureTest.Hide;
   FormFullScreenMsg.Show;

  except

    on E: Exception do
    begin

      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETavsverificationState);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETavsverificationState:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
  end;
end;

{*********************************************************
* Function:     FormMainArea.SETavsRedoinputState
* Description:  Displays a message when the Login has failed
*               The login-screen will again be shown when
*               the verification-state is changed to InputState by ATP1
*********************************************************}
Procedure TFormMMIFrame.SETavsRedoinputState;
var
  ATPVerificationStateChanged : Boolean;

begin
  DataModuleDMI.GetATPVerificationState(ATPVerificationStateChanged);

  try

   if ATPVerificationStateChanged then
   FormLogin.Clear;
   FormAutomaticConfiguration.Hide;
   FormManualConfiguration.Hide;
   FormReRegistration.Hide;
   FormTrainVsTrack.Hide;
   FormNoMAControlled.Hide;
   FormLogin.Hide;
   FormSleepingMode.Hide;
   FormFullScreenMsg.Show;

  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETavsRedoinputState);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in SETavsRedoinputState:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
 end;
end;

{*********************************************************
* Function:    FormMainArea.SETasActivationTest
* Description:
*********************************************************}
Procedure TFormMMIFrame.SETasActivationTest ;
begin
  try
   FormAutomaticConfiguration.Hide;
   FormManualConfiguration.Hide;
   FormReRegistration.Hide;
   FormTrainVsTrack.Hide;
   FormSleepingMode.Hide;
   FormNoMAControlled.Hide;
   FormLogin.Hide;
   FormFullScreenMsg.Show;

   except
    on E: Exception do
    begin
      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETasActivationTest);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETasActivationTest:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
 end;
end;

{*********************************************************
* Function:    FormMainArea.SETcssAutomatic
* Description:
*********************************************************}
Procedure TFormMMIFrame.SETcssAutomatic;
begin
  try
    FormFullScreenMsg.Hide;
    FormNoMAControlled.Hide;
    FormSleepingMode.Hide;
    FormManualConfiguration.Hide;
    FormReRegistration.Hide;
    FormTrainVsTrack.Hide;
    FormSleepingMode.Hide;
    FormNoMAControlled.Hide;
    FormLogin.Hide;
    FormAutomaticConfiguration.show;
    FormAutomaticConfiguration.Enabled:=True;
   except
    on E: Exception do
    begin

      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN, MainLogErrorSETcssAutomatic);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETcssAutomatic:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
   end;
end;

{*********************************************************
* Function:    FormMainArea.SETcssManual
* Description:
*********************************************************}
Procedure TFormMMIFrame.SETcssManual;
begin
  try

    FormTrainVsTrack.Hide;
    FormLogin.Hide;
    FormFullScreenMsg.Hide;
    FormNoMAControlled.Hide;
    FormSleepingMode.Hide;
    FormAutomaticConfiguration.Hide;
    FormManualConfiguration.Enabled:= True;
    if (not FormManualConfiguration.Visible) then
    begin
       FormManualConfiguration.RearrangeCarBlockList;
       FormManualConfiguration.Show;
    end;

    FormReRegistration.Hide;

   except
    on E: Exception do
    begin
      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETcssManual);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETcssManual:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;

   end;
end;
{*********************************************************
* Function:    FormMainArea.SETcssReConfig
* Description:
*********************************************************}
Procedure TFormMMIFrame.SETcssReConfig;
var
 VehicleDataRec : Boolean;
begin
  try
  VehicleDataRec := DataModuleDMI.GetVehicleDataReceived();
  if(VehicleDataRec OR LoggedOut) then
  begin
    FormTrainVsTrack.Hide;

    FormLogin.Hide;

    FormFullScreenMsg.Hide;

    FormNoMAControlled.Hide;
    FormAutomaticConfiguration.Hide;
    FormManualConfiguration.Hide;
    FormManualConfiguration.Enabled:= False;
    FormSleepingMode.Hide;
    FormReRegistration.Show;
    FormReRegistration.Enabled:= True;
   end;
   except
    on E: Exception do
    begin
      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETcssReConfig);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETcssReConfig:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;

   end;
end;

{*********************************************************
* Function:    FormMainArea.SETcssRestartConfig
* Description:
*********************************************************}
Procedure TFormMMIFrame.SETcssRestartConfig;
begin
  try
   FormReRegistration.Hide;
   FormFlash.Hide;
   FormTrainVsTrack.Hide;
   FormUnreg.Hide;
   FormMainArea.Hide;
   FormNoMAControlled.Hide;
   FormLogin.Hide;
   FormManualConfiguration.Hide;
   FormSleepingMode.Hide;
   FormConfirmDepartureTest.Hide;
   FormAutomaticConfiguration.Hide;
   FormConfirmDepartureTest.Hide;
   FormAutomaticConfiguration.Hide;
   FormFullScreenMsg.Show;

   except
    on E: Exception do
    begin
      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETcssReConfig);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETcssReConfig:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;

   end;
end;


{*********************************************************
* Function:    FormMainArea.SETamTrainregistration
* Description:
*********************************************************}
Procedure TFormMMIFrame.SETamTrainregistration;
var
TrainVsTrackDirWantedReceived : Boolean;
TrainVsTrackDirWantedChanged : Boolean;
begin
  try

   FormManualConfiguration.Hide;
   FormReRegistration.Hide;
   FormConfirmDepartureTest.Hide;

   //  FormTrainVsTrack is displayed in SerUnit.pas when TrainVsTrackDirWanted
   //  message is received.

   FormLogin.Hide;
   FormNoMAControlled.Hide;
   FormSleepingMode.Hide;
   FormAutomaticConfiguration.Hide;
   TrainVsTrackDirWantedReceived :=  DataModuleDMI.GetTrainVsTrackDirWantedReceived(TrainVsTrackDirWantedChanged);
   if (TrainVsTrackDirWantedChanged And TrainVsTrackDirWantedReceived)  then
   begin
     FormTrainVsTrack.LocomotiveConnected := DataModuleDMI.GetLocoVsTrainDir;
     FormFullScreenMsg.Hide;
     FormTrainVsTrack.Show;
   end
    // or perhaps FormTrainVsTrackDir already visible
   else if Not FormTrainVsTrack.Visible then
   begin
     FormFullScreenMsg.Show;
   end;

  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETamTrainregistration);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in SETamTrainregistration:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);

    end;
   end;
end;

{*********************************************************
* Function:    FormMainArea.SETamFullATP
* Description:
*********************************************************}
Procedure TFormMMIFrame.SETamNormal;
begin
  try

    FormMainArea.Show;

    FormManualConfiguration.Hide;

    FormReRegistration.Hide;
    FormConfirmDepartureTest.Hide;
    FormTrainVsTrack.Hide;

    FormLogin.Hide;
    FormSleepingMode.Hide;
    FormFullScreenMsg.Hide;
    FormNoMAControlled.Hide;
    FormAutomaticConfiguration.Hide;

  except
    on E: Exception do
    begin
      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETamFullATP);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETamFullATP:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
   end;
end;


{*********************************************************
* Function:    FormMainArea.SETamManualLocation
* Description:
*********************************************************}
Procedure TFormMMIFrame.SETamManualLocation;
begin
  try

    FormReRegistration.Hide;

    FormTrainVsTrack.Hide;
    FormAutomaticConfiguration.Hide;
    FormLogin.Hide;
    FormSleepingMode.Hide;
    FormFullScreenMsg.Hide;
    FormNoMAControlled.Hide;
  except
    on E: Exception do
    begin
      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETamManualLocation);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETamManualLocation:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;

  end;
end;

{*********************************************************
* SETamNoMAControlledMode
* Description:
*********************************************************}
Procedure TFormMMIFrame.SETamNoMAControlledMode;
begin
  try

    FormManualConfiguration.Hide;
    FormReRegistration.Hide;
    FormTrainVsTrack.Hide;
    FormLogin.Hide;
    FormFullScreenMsg.Hide;
    FormSleepingMode.Hide;
    FormNoMAControlled.Show;

  except
    on E: Exception do
    begin
      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETamYardMode);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETamYardMode:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{*********************************************************
* Function:    SetFatalFailure
* Description:
*********************************************************}
Procedure TFormMMIFrame.SetFatalFailure;
begin
  try
    if assigned(FormSafetyHalt) then
    begin
      if Not FormSafetyHalt.Visible then
        FormSafetyHalt.Show;
    end;

  except
    on E: Exception do
    begin
      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSETFatalFailure);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SETFatalFailure:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{*********************************************************
* Function:    SetUnregistered
* Description:
*********************************************************}
Procedure TFormMMIFrame.SetUnregistered;
begin
  try

    FormMainArea.Hide;

    if assigned(FormUnreg) then
      FormUnreg.show;


  except
    on E: Exception do
    begin
      WriteMMIStatus('MAIN',MS_SW_ERROR,MS_MAIN,MainLogErrorSetUnregistered);
      if TestMode then
        MessageBox(Handle,PWideChar('Error in SetUnregistered:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{*********************************************************
* Function:    Update
* Description: Update forms with changed data
*********************************************************}
procedure TFormMMIFrame.UpdateDMI;
begin

  if FormMainArea.Visible then
    FormMainArea.UpdateDMI(False);
  if FormUnreg.Visible then
    FormUnreg.UpdateDMI(False);
  if FormNoMAControlled.Visible then
    FormNoMAControlled.UpdateDMI(False);
  if FormLogin.Visible then
    FormLogin.UpdateDMI(False);
  if FormManualConfiguration.Visible then
    FormManualConfiguration.UpdateDMI(False);
  if FormReRegistration.Visible then
    FormReRegistration.UpdateDMI(False);
  if FormPowerDown.Visible then
    FormPowerDown.UpdateDMI(False);
  if FormSafetyHalt.Visible then
    FormSafetyHalt.UpdateDMI(False);
  if FormFullScreenMsg.Visible then
    FormFullScreenMsg.UpdateDMI(False);
  if FormTrainVsTrack.Visible then
    FormTrainVsTrack.UpdateDMI(False);
  if FormTrainVsTrack.Visible then
    FormTrainVsTrack.UpdateDMI(False);
  if FormSleepingMode.Visible then
    FormSleepingMode.UpdateDMI(False);
  if FormAutomaticConfiguration.Visible then
     FormAutomaticConfiguration.UpdateDMI(False);
  {Clear changed flags}
  DataModuleDMI.ClearChanged;

end;

{*********************************************************
* Function:    HideAllForm
* Description: Hide All the forms
*********************************************************}
procedure TFormMMIFrame.HideAllForm;
begin
    // Hide full-screen forms
  FormFullScreenMsg.Hide;
  FormPowerDown.Hide;
  FormReRegistration.Hide;
  FormFlash.Hide;
  FormTrainVsTrack.Hide;
  FormUnreg.Hide;
  FormMainArea.Hide;
  FormNoMAControlled.Hide;
  FormLogin.Hide;
  FormManualConfiguration.Hide;
  FormPowerDown.Hide;
  FormSafetyHalt.Hide;
  FormSleepingMode.Hide;
  FormAutomaticConfiguration.Hide;

    // Hide overlays
  FormAtpMessages.Hide;
  FormConfirmDepartureTest.Hide;
  FormConfirmAbortLastCarBrake.Hide;
  FormConfirmBrakeTest.Hide;
  FormConfirmCancelRegArea.Hide;
  FormSelectArea.Hide;
  FormConfirmDopplerRadarFailure.Hide;
  FormConfirmTachometer1Failure.Hide;
  FormConfirmTachometer2Failure.Hide;
  FormConfirmExitSleepingMode.Hide;
  FormConfirmFreeRollingClear.Hide;
  FormConfirmLoadedStatus.Hide;
  FormConfirmMAAccept.Hide;
  FormConfirmNewATPMode.Hide;
  FormTrainComposition.Hide;
  FormConfirmRejectSetup.Hide;
  FormStartupHistory.Hide;
  FormRadioChannel.Hide;
  FormNewTrainName.Hide;

end;

{*********************************************************
* Function:    IdTCPServerRemoteDMIInterfaceConnect
* Description: Callback when Remote Interface connected
*********************************************************}
procedure TFormMMIFrame.IdTCPServerRemoteDMIInterfaceConnect(
  AContext: TIdContext);
begin

  LogEventStr(LogLevelDetail, AnsiString('MAIN'), AnsiString('RemoteDMIInterface connected!'));

end;

{*********************************************************
* Function:    IdTCPServerRemoteDMIInterfaceDisconnect
* Description: Callback when Remote Interface disconnected
*********************************************************}
procedure TFormMMIFrame.IdTCPServerRemoteDMIInterfaceDisconnect(
  AContext: TIdContext);
begin

  LogEventStr(LogLevelDetail, AnsiString('MAIN'), AnsiString('RemoteDMIInterface disconnected!'));

end;

{*********************************************************
* Function:    IdTCPServerRemoteDMIInterfaceExecute
* Description: Callback when remote command received
*********************************************************}
procedure TFormMMIFrame.IdTCPServerRemoteDMIInterfaceExecute(
  AContext: TIdContext);
begin

    // Get command from client. Terminated by a new-line
    // ReadLn will wait for any terminating new-line but as this runs
    // in a separate thread it will not affect the program flow.

  while AContext.Connection.Connected do
  begin
    msgFromClient := AContext.Connection.IOHandler.ReadLn;
    LogEventStr(LogLevelDetail, AnsiString('MAIN'), AnsiString('RemoteDMIInterface command:')+ AnsiString(msgFromClient));
    TidSync.SynchronizeMethod(RemoteCommand);
      // Do not send response on some commands.
    if (Length(responseToClient) > 0) then
      AContext.Connection.IOHandler.WriteLn(responseToClient);
  end;

end;

{*********************************************************
* Function:    IdTCPServerRemoteDMIInterfaceInternalConnect
* Description: Callback when Remote Internal Interface connected
*********************************************************}
procedure TFormMMIFrame.IdTCPServerRemoteDMIInterfaceInternalConnect(
  AContext: TIdContext);
begin

  LogEventStr(LogLevelDetail, AnsiString('MAIN'), AnsiString('RemoteDMIInterface (Internal) connected!'));

end;

{*********************************************************
* Function:    IdTCPServerRemoteDMIInterfaceInternalDisconnect
* Description: Callback when Remote Internal Interface disconnected
*********************************************************}
procedure TFormMMIFrame.IdTCPServerRemoteDMIInterfaceInternalDisconnect(
  AContext: TIdContext);
begin

  LogEventStr(LogLevelDetail, AnsiString('MAIN'), AnsiString('RemoteDMIInterface (Internal) disconnected!'));

end;

{*********************************************************
* Function:    RemoteCommand
* Description: Execute remote command
*********************************************************}
procedure TFormMMIFrame.RemoteCommand;
begin
  Exchange.RemoteCommand(msgFromClient, responseToClient);
end;

{*********************************************************
* Function:    RemoteCommand
* Description: Execute remote internal command
*********************************************************}
procedure TFormMMIFrame.RemoteInternalCommand;
begin
  Exchange.RemoteCommand(msgInternalFromClient, responseInternalToClient);
end;

{*********************************************************
* Function:    IdTCPServerRemoteDMIInterfaceInternalExecute
* Description: Callback when remote internal command received
*********************************************************}
procedure TFormMMIFrame.IdTCPServerRemoteDMIInterfaceInternalExecute(
  AContext: TIdContext);
begin

    // Get command from client. Terminated by a new-line
    // ReadLn will wait for any terminating new-line but as this runs
    // in a separate thread it will not affect the program flow.

  while AContext.Connection.Connected do
  begin
    msgInternalFromClient := AContext.Connection.IOHandler.ReadLn;
    LogEventStr(LogLevelDebug, AnsiString('MAIN'), AnsiString('RemoteDMIInterface (Internal) command:')+ AnsiString(msgInternalFromClient));
    TidSync.SynchronizeMethod(RemoteInternalCommand);

    AContext.Connection.IOHandler.WriteLn(responseInternalToClient);
  end;

end;
{*********************************************************
* Function:    AreaIdToString
* Description: Converts an Area Id to a string to present
*              to the driver.
*********************************************************}
Function TFormMMIFrame.AreaIdToString(AreaId : Byte): String;
begin

  AreaIdToString := LanguageFile.ReadString('Area', IntToStr(AreaId), IntToStr(AreaId));

end;

end.
