(*****************************************************************
           © COPYRIGHT Bombardier Transportation Signal AB, SWEDEN 2011.
           =============================================================

    The copyright to the computer program herein is the
    property of Bombardier Transportation Signal AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the written permission
    from Bombardier Transportation Signal AB, or in accordance with the terms
    and conditions stipulated in the agreement/contract under which
    the program has been supplied.


    NAME :  MTrainConfig.pas

    PROJECT :  LKAB, InterFlow TrainBorne

    Ver    Author           Date    Reason
    ---    ------           ------  ------
    1.0.0  Jan Kiessling    991116  First version
    2.0    Edward Lundin    021001  Updated for T8.
           Antbäck          100909  Hide "validate" button once pressed
           Bo Hermansson    110310  Delphi XE:StrLen(pchar()) -> Length()
           Bo Hermansson    110905  Reworked for LK (displaying blocks of cars)
           Bo Hermansson    111215  Layout changed

    DESCRIPTION :  Form to handle reconfiguration

    INTERFACE :
*********************************************************)

unit UnitReReg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UnitMainLayer2, ColorButton2, ExtCtrls, StdCtrls, UnitMainArea, MMITypes, ImgList, Grids,
  GestureMgr, Menus, DB, Buttons, DBGrids, MMIPanel;

type
  TFormReRegistration = class(TFormMainLayer2)
    Accept: TColorButton;
    Reject: TColorButton;
    LocConnected: TLabel;
    LabelCount: TLabel;
    LabelTypeOfCar: TLabel;
    DrawGrid: TDrawGrid;
    ImageList: TImageList;
    LabelTitle: TLabel;
    LabelLastCarDetectedInfo: TLabel;
    ImageListCarsConnected: TImageList;
    ImageListOther: TImageList;
    ImageCarsConnected: TImage;
    TimerTIMSStatus: TTimer;
    ColorButtonAcceptWithoutTIMS: TColorButton;
    DataSourceTrainComp: TDataSource;
    Procedure ScreenChange;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AcceptClick(Sender: TObject);
    procedure RejectClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure EnableAcceptButton(Enable:Boolean);
    procedure EnableAcceptWithoutTIMSButton(Enable:Boolean);
    procedure EnableRejectButton(Enable:Boolean);
    procedure SetCarsConnectedImage(ConnectionEnd : TLocomotiveConnected);
    procedure TimerTIMSStatusTimer(Sender: TObject);

  private
    { Private declarations }
    TIMSRequired : Boolean;
    RegCarBlocks : TCarBlocksRecord;
    LocomotiveConnected : TLocomotiveConnected;   // reported from ATP
    NewVehicleDataMsg : TVehicleDataMsg;
    procedure FillCarBlockList;
    function ImageOf(VehicleType : Byte ):Integer;
    procedure EvaluateTIMSStatus;


  public
    { Public declarations }
    procedure UpdateDMI(Refresh : Boolean);
    procedure ShowCarList;
  end;

var
  FormReRegistration: TFormReRegistration;

implementation
{$R *.DFM}

uses
  UnitExchange, MMIStd , UnitMMIFrame, UnitManualConfiguration,
  UnitDMIDataModule, UnitConfirmRejectSetup;

const
  ReConfigurationLogErrorFormCreate = 1;
  ReConfigurationLogErrorFormShow   = 2;
  ReConfigurationLogErrorAcceptClick= 3;

{*********************************************************
* Function:    FormCreate
* Description: FOrm Create
*********************************************************}
procedure TFormReRegistration.FormCreate(Sender: TObject);
var
  CarBlockIndex : Integer;
  J : Integer;
  StringIndex : Integer;

begin

  inherited;

  Try

    TIMSRequired := false;
    LocomotiveConnected := lcBEnd; // TODO

    Accept.Caption:=InterfaceString(ifAcceptBtn) ;
    Reject.Caption:=InterfaceString(ifReEnterBtn) ;
    LocConnected.Caption:=InterfaceString(ifLocDir) ;

    LabelTitle.Caption := InterfaceString(ifTrnReg);
    LabelCount.Caption := InterfaceString(ifCount);
    LabelTypeOfCar.Caption := InterfaceString(ifTypeOfCar);
    LabelLastCarDetectedInfo.Caption := InterfaceString(ifLastCarNotDetected);

    for CarBlockIndex := 1 to MAX_VEHICLE_TYPE_BLOCKS do
    begin
      if CarBlockTypes[CarBlockIndex].Name <> '' then
      begin
        if CarBlockTypes[CarBlockIndex].ImagePath <> '' then
          ReplaceImage(ImageList, CarBlockIndex, CarBlockTypes[CarBlockIndex].ImagePath,'RREG');
      end;
    end;

    NewVehicleDataMsg.NewVehicleData.NrOfVehicleDataBlocks := 0;
    for J := 1 to MAX_VEHICLE_DATA_BLOCKS do
    begin
        NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].NrOfVehicleInBlock := 0;
        NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].VehicleType := 0;
        NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].VehicleNodeId := 0;

        for StringIndex := 1 to VEHICLE_NAME_MAX_CHARS do
        begin
             NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].VehicleName[StringIndex] := #0;
        end;
    end;

    { Images }
    if FormMMIFrame.HiRes then
    begin
        { Replace built in images if 1024 * 768 or higher resolution }
        ImageListCarsConnected.Clear;
        AdjustImageList(ImageListCarsConnected);

        { Insert a question mark as first image }
        AddImageFromResource(ImageListCarsConnected, 'QUESTION_MARK' , true);
        AddImageFromResource(ImageListCarsConnected, 'HIRES_CAR_CONNECTEDA');
        AddImageFromResource(ImageListCarsConnected, 'HIRES_CAR_CONNECTEDB');
    end;


    if (ImageCarsConnectedA.ImagePath <> '') then
       ReplaceImage(ImageListCarsConnected, Ord(lcAEnd), ImageCarsConnectedA.ImagePath,'RREG');
    if (ImageCarsConnectedB.ImagePath <> '') then
       ReplaceImage(ImageListCarsConnected, Ord(lcBEnd), ImageCarsConnectedB.ImagePath,'RREG');

    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;

    ScreenChange;

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('REREG',MS_SW_ERROR,MS_RE_CNF,ReConfigurationLogErrorFormCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in FormCreate:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate ;
    end;
 End;
end;

procedure TFormReRegistration.ScreenChange;

begin  {ScreenChange}

    AdjustLabel(LabelTitle);
    AdjustLabel(LabelLastCarDetectedInfo);

    AdjustLabel(LabelCount);
    AdjustLabel(LabelTypeOfCar);
    AdjustLabel(LocConnected);

    AdjustControl(DrawGrid);
    AdjustControl(ImageCarsConnected);

    AdjustColorButton(Accept);
    AdjustColorButton(Reject);

    //AdjustColorButton(ColorButtonAcceptWithoutTIMS,false);

    if FormMMIFrame.HiRes then
    begin

      ImageListOther.Clear;
      ImageListOther.Width := ColorButtonAcceptWithoutTIMS.Width - (2 *  ColorButtonAcceptWithoutTIMS.BevelSize);
      ImageListOther.Height:= ColorButtonAcceptWithoutTIMS.Height - (2 *  ColorButtonAcceptWithoutTIMS.BevelSize);
      AddImageFromResource(ImageListOther, 'ACCEPT_WITHOUT_TIMS', true);

    end;

    AddImageFromResource(ImageListOther, 'EMPTY_BUTTON', true);
    ImageListOther.GetBitmap(1,ColorButtonAcceptWithoutTIMS.Picture.Bitmap);

end;

procedure TFormReRegistration.FormShow(Sender: TObject);
Var
I: Integer ;
J : Integer;
StringIndex : Integer;
begin

  inherited;

  Try

   //SetCarsConnectedImage(DataModuleDMI.GetLocoVsTrainDir);
   EvaluateTIMSStatus;
   EnableRejectButton(True);

   // Clear car blocks
   RegCarBlocks.NumberofCarBlocks := 0;
   for I := 1 to CAR_BLOCK_LIST_MAX  do
   begin
     RegCarBlocks.CarBlocks[I].Count :=0;
     RegCarBlocks.CarBlocks[I].ID:='';
     RegCarBlocks.CarBlocks[I].VehicleType := 0;
   end;

   NewVehicleDataMsg.NewVehicleData.NrOfVehicleDataBlocks := 0;
   for J := 1 to MAX_VEHICLE_DATA_BLOCKS do
   begin
        NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].NrOfVehicleInBlock := 0;
        NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].VehicleType := 0;
        NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].VehicleNodeId := 0;

        for StringIndex := 1 to VEHICLE_NAME_MAX_CHARS do
        begin
            NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].VehicleName[StringIndex] := #0;
        end;
   end;

   DrawGrid.ColWidths[Ord(CarBlockColumnCount)]:=Round(60*GetXfactor);
   DrawGrid.ColWidths[Ord(CarBlockColumnIcon)]:=Round(60*GetXfactor);
   DrawGrid.ColWidths[Ord(CarBlockColumnTypeOfCar)]:=Round(240*GetXfactor);

   DrawGrid.Show;

   ShowCarList;

   TimerTIMSStatus.Enabled := true;

  UpdateDMI(true);


  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('REREG',MS_SW_ERROR,MS_RE_CNF,ReConfigurationLogErrorFormShow);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in FormShow:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
 End;
end;

{*********************************************************
* Function:    FillCarBlockList
* Description: Fill list of car-blocks from list of single cars
*********************************************************}
procedure TFormReRegistration.FillCarBlockList;
var
  I : Integer;
  PrevVehicleType : Integer;
  ThisVehicleType : Integer;
  ThisVehicleTypeName : AnsiString;
  NumberOfVehicle : Integer;

begin

  PrevVehicleType := -1;
  RegCarBlocks.NumberofCarBlocks := 0;
  I := 1;
  DataSourceTrainComp.DataSet.FindFirst;
  NumberOfVehicle := (DataSourceTrainComp.DataSet.RecordCount - 1);

  // Loco will be not displayed
  DataSourceTrainComp.DataSet.FindNext;
  while (I <= NumberOfVehicle) do
  begin
    ThisVehicleType := DataSourceTrainComp.DataSet.FieldValues['VehicleType'];
    if ((ThisVehicleType > 0) and
      (ThisVehicleType <= MAX_VEHICLE_TYPE_BLOCKS)) then
      ThisVehicleTypeName := AnsiString(DataSourceTrainComp.DataSet.FieldValues['VehicleTypeName'])
    else
      ThisVehicleTypeName := AnsiString(DataSourceTrainComp.DataSet.FieldValues['VehicleName']);

    if ThisVehicleType = PrevVehicleType then
    begin
      Inc(RegCarBlocks.CarBlocks[RegCarBlocks.NumberofCarBlocks].Count);
    end
    else
    begin
      Inc(RegCarBlocks.NumberofCarBlocks);
      RegCarBlocks.CarBlocks[RegCarBlocks.NumberofCarBlocks].ID :=
        ThisVehicleTypeName;
      RegCarBlocks.CarBlocks[RegCarBlocks.NumberofCarBlocks].Count := 1;
      RegCarBlocks.CarBlocks[RegCarBlocks.NumberofCarBlocks].VehicleType :=
        ThisVehicleType;
      PrevVehicleType := ThisVehicleType;
    end;

    DataSourceTrainComp.DataSet.FindNext;
    Inc(I);
  end;

  if (RegCarBlocks.NumberofCarBlocks > 0) then
  begin
    DrawGrid.RowCount := RegCarBlocks.NumberofCarBlocks;
    DrawGrid.Visible := true;
  end
  else
    DrawGrid.Visible := false;

end; // End of method

procedure TFormReRegistration.ShowCarList;
begin

  FillCarBlockList;
  DrawGrid.Refresh;
end;


procedure TFormReRegistration.TimerTIMSStatusTimer(Sender: TObject);
begin
  EvaluateTIMSStatus;
end;

{*********************************************************
* Function:    ImageOf
* Description: Returns an index in the ImageList representing
*               the type of car
*********************************************************}
function TFormReRegistration.ImageOf(VehicleType : Byte ):Integer;
begin
  if ((VehicleType > 0) AND (VehicleType <= MAX_VEHICLE_TYPE_BLOCKS)) or (VehicleType = VEHICLE_TYPE_UNDEFINED ) then
     Result := 1
  else if((VehicleType >= LOCO_TYPE_RANGE_START) AND (VehicleType <= LOCO_TYPE_RANGE_END)) then
     Result := 2
  else
     Result := 0;

end;

{*********************************************************
* Function:    DrawGridDrawCell
* Description: Customized drawing of the grid
*********************************************************}
procedure TFormReRegistration.DrawGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if (ACol = Ord(CarBlockColumnCount)) then
  begin
    if (RegCarBlocks.NumberofCarBlocks >= ARow)
    then
    begin
      DrawGrid.Canvas.Font.Color := clWindow;
      if (RegCarBlocks.CarBlocks[ARow+1].Count > 0) then
        DrawGrid.Canvas.TextOut(Rect.Left+4,Rect.Top + 4,IntToStr(RegCarBlocks.CarBlocks[ARow+1].Count))
      else
        DrawGrid.Canvas.TextOut(Rect.Left+4,Rect.Top + 4,'+');
    end;
  end;

  if (ACol = Ord(CarBlockColumnIcon)) then
  begin
    if (RegCarBlocks.NumberofCarBlocks >= ARow)
    then
    begin
      ImageList.Draw( DrawGrid.Canvas ,Rect.Left + Round((Rect.Right - Rect.Left - ImageList.Width)/2),Rect.Top+4,ImageOf(RegCarBlocks.CarBlocks[ARow+1].VehicleType),dsNormal,itImage);
    end;
  end;

  if (ACol = Ord(CarBlockColumnTypeOfCar)) then
  begin
    if (RegCarBlocks.NumberofCarBlocks >= ARow)
    then
    begin
      DrawGrid.Canvas.Font.Color := clWindow;
      DrawGrid.Canvas.TextOut(Rect.Left+4,Rect.Top + 4,String(RegCarBlocks.CarBlocks[ARow+1].ID));
    end;
  end;


end;

Procedure TFormReRegistration.AcceptClick(Sender: TObject);
begin
   //Send confirmation to ATP
   Exchange.SendConfirmation(True);
end;

procedure TFormReRegistration.RejectClick(Sender: TObject);
begin

   FormConfirmRejectSetup.Show;
end;

{*************************************************************
* Function:    EnableAcceptButton
* Description: Enable the Accept-button and change the appearance
*              in order to indicate whether the button is possible
*              to click or not
**************************************************************}
procedure TFormReRegistration.EnableAcceptButton(Enable:Boolean);
begin

  if Enable then
  begin
    Accept.Enabled := true;
    Accept.Font.Color := ClBlack;
  end
  else
  begin
    Accept.Enabled := false;
    Accept.Font.Color := ClMedGray;
  end;

end;

{*************************************************************
* Function:    EnableAcceptWithoutTIMSButton
* Description: Enable the AcceptWithoutTIMS-button and change the appearance
*              in order to indicate whether the button is possible
*              to click or not
**************************************************************}
procedure TFormReRegistration.EnableAcceptWithoutTIMSButton(Enable:Boolean);
var
 Rect : TRect;
 ButtonText : String;

begin

  if Enable then
  begin
    ColorButtonAcceptWithoutTIMS.Enabled := true;
    ColorButtonAcceptWithoutTIMS.Font.Color := ClBlack;
    ColorButtonAcceptWithoutTIMS.BevelStyle := bbRaised;
  end
  else
  begin
    ColorButtonAcceptWithoutTIMS.Enabled := false;
    ColorButtonAcceptWithoutTIMS.Font.Color := ClMedGray;
    ColorButtonAcceptWithoutTIMS.BevelStyle := bbLowered;
  end;


  ColorButtonAcceptWithoutTIMS.Left := Accept.Left;
  ColorButtonAcceptWithoutTIMS.Top := Accept.Top;
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Width;
  Rect.Bottom := ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Height;


      // Set canvas font equal to text font
  ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Canvas.Font := ColorButtonAcceptWithoutTIMS.Font;
  ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Canvas.Pen.Color := ColorButtonAcceptWithoutTIMS.Font.Color;

  ImageListOther.GetBitmap(1,ColorButtonAcceptWithoutTIMS.Picture.Bitmap);
  ColorButtonAcceptWithoutTIMS.Picture.Bitmap.SetSize((ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Width - 20),(ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Height - 10));
  ButtonText := InterfaceString(ifAcceptBtn)+ sLineBreak + ' '+InterfaceString(ifWithoutTIMS);
  SetBkMode(ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Canvas.Handle,TRANSPARENT);
  DrawText(ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Canvas.Handle,PChar(ButtonText),Length(ButtonText),Rect,DT_CENTER OR DT_WORDBREAK);
  ColorButtonAcceptWithoutTIMS.Invalidate;

end;

{*************************************************************
* Function:    EnableRejectButton
* Description: Enable the Reject-button and change the appearance
*              in order to indicate whether the button is possible
*              to click or not
**************************************************************}
procedure TFormReRegistration.EnableRejectButton(Enable:Boolean);
begin

  if Enable then
  begin
    Reject.Enabled := True;
    Reject.Font.Color := ClBlack;
  end
  else
  begin
    Reject.Enabled := False;
    Reject.Font.Color := ClMedGray;
  end;

end;

{*************************************************************
* Function:    SetCarsConnectedImage
* Description: Set the image representing the connection end
**************************************************************}
procedure TFormReRegistration.SetCarsConnectedImage(ConnectionEnd : TLocomotiveConnected);
begin

  ImageListCarsConnected.GetBitmap(Ord(ConnectionEnd),ImageCarsConnected.Picture.Bitmap);

end;

{*************************************************************
* Function:    EvaluateTIMSStatus
* Description: Read TIMSAvailable and TIMSOk and adjust buttons and texts
**************************************************************}
procedure TFormReRegistration.EvaluateTIMSStatus;
begin

  {In reconfig TIMSRequired should be accepted regardless of TIMS state}
  if (Not TIMSRequired) then
  begin
     LabelLastCarDetectedInfo.Caption := InterfaceString(ifTIMSNotAvailable);
     EnableAcceptWithoutTIMSButton(true);
     ColorButtonAcceptWithoutTIMS.Show;

     EnableAcceptButton(false);
     Accept.Hide;
  end
  else if ((DataModuleDMI.GetTIMSAvailable)) then
  begin

    LabelLastCarDetectedInfo.Visible := false;
    EnableAcceptButton(true);
    Accept.Show;

    ColorButtonAcceptWithoutTIMS.Hide;
  end
  else
  begin

    if (NOT DataModuleDMI.GetTIMSOk) then
      LabelLastCarDetectedInfo.Caption := InterfaceString(ifLastCarNotDetected);

    if (NOT DataModuleDMI.GetTIMSAvailable) then
      LabelLastCarDetectedInfo.Caption := InterfaceString(ifTIMSNotAvailable);

    LabelLastCarDetectedInfo.Visible := true;
    EnableAcceptButton(false);
    Accept.Hide;

    ColorButtonAcceptWithoutTIMS.Hide;

  end;

end;





{ *********************************************************
* UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormReRegistration.UpdateDMI(Refresh : Boolean);
var
  ReRegOptions : Byte;
  ReRegOptionsChanged : Boolean;

begin

  inherited;

  ReRegOptions := DataModuleDMI.GetReRegOptions(ReRegOptionsChanged);
  if (Refresh or ReRegOptionsChanged) then
  begin
    if ByteBool(ReRegOptions and RECONFIG_TIMS_REQUIRED) then
      TIMSRequired := true
    else
      TIMSRequired := false;
  end;

  if  FormReRegistration.Visible then
  begin
    ShowCarList;
    SetCarsConnectedImage(DataModuleDMI.GetLocoVsTrainDir);
  end;
  EvaluateTIMSStatus;

end;

end.

