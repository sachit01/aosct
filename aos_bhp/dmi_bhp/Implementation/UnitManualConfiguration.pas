(**********************************************************************
           © COPYRIGHT Bombardier Transportation Signal AB, SWEDEN 2011.
           ============================================================

    The copyright to the computer program herein is the property of
    Bombardier Transportation Signal AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the written permission from
    Bombardier Transportation Signal AB, or in accordance with the terms and
    conditions stipulated in the agreement/contract under which
    the program has been supplied.


    NAME :  MTrainConfig.pas

    PROJECT :  LKAB, InterFlow TrainBorne

    Ver    Author           Date    Reason
    ---    ------           ------  ------
    1.0.0  Daniel Persson   980126  First version
    2.0    Edward Lundin    021001  Updated for T8.
           Bo Hermansson    110310  Delphi XE:StrLen(pchar()) -> Length()
                                    Numerous casts to/from String/AnsiString
           Bo Hermansson    111115  Configurable types of cars
           Bo Hermansson    111214  Changed layout and new images for LKAB

    DESCRIPTION :  Form that lets the driver configure a train

    INTERFACE :
*********************************************************)

unit UnitManualConfiguration;

interface

{****************************************************************************
* UNIT USES                                                                 *
****************************************************************************}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ColorButton2, StdCtrls, ExtCtrls, UnitMainArea, MMITypes, Grids, ImgList,
  UnitMainLayer2, Buttons, GestureMgr, Menus, DB, DBGrids, MMIPanel;

{****************************************************************************
* UNIT TYPE DECLARATIONS                                                    *
****************************************************************************}
type
TFormManualConfiguration = class(TFormMainLayer2)
    Accept: TColorButton;
    LocConnected: TLabel;
    RetryInfo: TLabel;
    DrawGrid: TDrawGrid;
    ImageList: TImageList;
    LabelCount: TLabel;
    ComboBox: TComboBox;
    EditCount: TEdit;
    LabelTypeOfCar: TLabel;
    SpeedButtonDel2: TSpeedButton;
    SpeedButtonDel3: TSpeedButton;
    SpeedButtonDel4: TSpeedButton;
    SpeedButtonDel5: TSpeedButton;
    SpeedButtonDel6: TSpeedButton;
    SpeedButtonDel7: TSpeedButton;
    SpeedButtonDel8: TSpeedButton;
    SpeedButtonNew: TSpeedButton;
    ComboBoxCarsConnected: TComboBox;
    ImageListCarsConnected: TImageList;
    LabelTitle: TLabel;
    ColorButtonAcceptWithoutTIMS: TColorButton;
    ImageListOther: TImageList;
    LabelLastCarDetectedInfo: TLabel;
    ComboBoxLoadStatus: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure AcceptClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure EditCountExit(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure EnableAcceptButton(Enable:Boolean);
    procedure EnableAcceptWithoutTIMSButton(Enable:Boolean);
    procedure EnableCarsConnected();
    procedure SpeedButtonDel2Click(Sender: TObject);
    procedure SpeedButtonDel3Click(Sender: TObject);
    procedure SpeedButtonDel4Click(Sender: TObject);
    procedure SpeedButtonDel5Click(Sender: TObject);
    procedure SpeedButtonDel6Click(Sender: TObject);
    procedure SpeedButtonDel7Click(Sender: TObject);
    procedure SpeedButtonDel8Click(Sender: TObject);
    procedure SpeedButtonNewClick(Sender: TObject);
    procedure ComboBoxCarsConnectedDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ComboBoxCarsConnectedChange(Sender: TObject);
    procedure ComboBoxCarsConnectedEnter(Sender: TObject);
    procedure EditCountEnter(Sender: TObject);
    procedure EditCountClick(Sender: TObject);
    procedure RearrangeCarBlockList;

  private
      { Private declarations }
    NewCarList : TCarRecord ;
    NewCarBlocks : TCarBlocksRecord;
    LocomotiveConnected : TLocomotiveConnected;
    NewVehicleDataMsg : TVehicleDataMsg;
    EditCounter : Integer;
    procedure ScreenChange;
    function FillCarList:Boolean;
    procedure ShowDeleteButtons(Rows:Integer);

   function ImageOf(TypeOfCar : AnsiString ):Integer;


    procedure EvaluateTIMSStatus;

  public
      { Public declarations }
    StandardCarCount : Integer;
    procedure UpdateDMI(Refresh : Boolean);
  end;

{****************************************************************************
* UNIT VAR DECLARATIONS                                                     *
****************************************************************************}


var
  FormManualConfiguration: TFormManualConfiguration;

implementation
{$R *.DFM}

{****************************************************************************
* USES                                                                      *
****************************************************************************}
Uses
  UnitExchange, MMIStd , UnitMMIFrame, KeyboardUnit, KeyboardUnitNumeric,
  UnitDMIDataModule, UnitConfirmLoadedStatus;

{****************************************************************************
* CONST DECLARATIONS                                                        *
****************************************************************************}
const
  ManualConfigLogErrorFormCreate  = 1;
  ManualConfigLogErrorFormHide    = 2;
  ManualConfigLogErrorAcceptClick = 3;
  ManualConfigLogErrorFormShow    = 4;

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
* Function:    TFormManualConfiguration.FormCreate
* Description: Object creat function
*********************************************************}
procedure TFormManualConfiguration.FormCreate(Sender: TObject);
var
  J : Integer;
  StringIndex : Integer;
begin

  inherited;

  Try

    LabelTitle.Caption := InterfaceString(ifManTrnCnf);
    LabelCount.Caption := InterfaceString(ifCount);
    LabelTypeOfCar.Caption := InterfaceString(ifTypeOfCar);
    LabelLastCarDetectedInfo.Caption := InterfaceString(ifLastCarNotDetected);

    ComboBoxLoadStatus.Items[0] := InterfaceString(ifNotLoaded);
    ComboBoxLoadStatus.Items[1] := InterfaceString(ifLoaded);
    ComboBoxLoadStatus.ItemIndex := 1;
    ComboBox.Clear;
    ComboBox.Items.Add(InterfaceString(ifNone));



 {  for CarBlockIndex := 1 to CAR_BLOCK_LIST_MAX do
    begin
      if CarBlockTypes[CarBlockIndex].Name <> '' then
      begin
        ComboBox.Items.Add(CarBlockTypes[CarBlockIndex].Name);
        if CarBlockTypes[CarBlockIndex].ImagePath <> '' then
          ReplaceImage(ImageList, CarBlockIndex, CarBlockTypes[CarBlockIndex].ImagePath, 'CONF');
      end;
    end;}

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

    ComboBox.ItemIndex := -1;

    ComboBoxCarsConnected.ItemIndex:=0;

    Accept.Caption:=InterfaceString(ifAcceptBtn) ;
    LocConnected.Caption:=InterfaceString(ifLocDir) ;
    RetryInfo.Caption:=InterfaceString(ifRetryConfig);


        { Images }
    if FormMMIFrame.HiRes then
    begin
        { Replace built in images if 1024 * 768 or higher resolution }
      ImageListCarsConnected.Clear;
      AdjustImageList(ImageListCarsConnected);

      AddImageFromResource(ImageListCarsConnected, 'HIRES_CAR_CONNECTEDB');
      AddImageFromResource(ImageListCarsConnected, 'HIRES_CAR_CONNECTEDA');

    end;


    if (ImageCarsConnectedA.ImagePath <> '') then
      ReplaceImage(ImageListCarsConnected, 1, ImageCarsConnectedA.ImagePath, 'CONF');
    if (ImageCarsConnectedB.ImagePath <> '') then
      ReplaceImage(ImageListCarsConnected, 0, ImageCarsConnectedB.ImagePath, 'CONF');


    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;

    ScreenChange;


  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('TRAINCFG',MS_SW_ERROR,MS_MTRN_CNF,ManualConfigLogErrorFormCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in FormCreate:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate ;
    end;
 End;
end;

{*********************************************************
* Function:    ScreenChange
* Description: Adjusts positions and widths when the screen
*              resolution is different from the standard
*              640 * 480 pixels
*********************************************************}
procedure TFormManualConfiguration.ScreenChange;
begin  {ScreenChange}


    AdjustColorButton(Accept,false);
    AdjustColorButton(ColorButtonAcceptWithoutTIMS,false);
    AdjustComboBox(ComboBoxLoadStatus);
    AdjustLabel(LabelLastCarDetectedInfo);


    if FormMMIFrame.HiRes then
    begin

      ImageListOther.Clear;
      ImageListOther.Width := ColorButtonAcceptWithoutTIMS.Width - (2 *  ColorButtonAcceptWithoutTIMS.BevelSize);
      ImageListOther.Height:= ColorButtonAcceptWithoutTIMS.Height - (2 *  ColorButtonAcceptWithoutTIMS.BevelSize);
      AddImageFromResource(ImageListOther, 'ACCEPT_WITHOUT_TIMS', true);

    end;

    AddImageFromResource(ImageListOther, 'EMPTY_BUTTON', true);

    ImageListOther.GetBitmap(1,ColorButtonAcceptWithoutTIMS.Picture.Bitmap);

    AdjustControl(ComboBoxCarsConnected);
    ComboBoxCarsConnected.ItemHeight := Accept.Height - 6;


    AdjustLabel(LabelTitle);
    AdjustLabel(LabelCount);
    AdjustLabel(LabelTypeOfCar);
    AdjustControl(DrawGrid);

    AdjustLabel(LocConnected);
    AdjustLabel(RetryInfo);
    AdjustLabel(LabelLastCarDetectedInfo);

    AdjustControl(SpeedButtonDel2);
    AdjustControl(SpeedButtonDel3);
    AdjustControl(SpeedButtonDel4);
    AdjustControl(SpeedButtonDel5);
    AdjustControl(SpeedButtonDel6);
    AdjustControl(SpeedButtonDel7);
    AdjustControl(SpeedButtonDel8);

    AdjustControl(SpeedButtonNew);

End;


{*********************************************************
* Function:    TFormManualConfiguration.FormHide
* Description: Cleans up the form when it's closed for
               next use.
*********************************************************}
procedure TFormManualConfiguration.FormHide(Sender: TObject);

begin
  Try

   DrawGrid.Hide;
   RetryInfo.Hide;
   ComboBox.Clear;
   OSDKeyboardWinControl.Hide;
   OSDKeyboardNumericWinControl.Hide;

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('TRAINCFG',MS_SW_ERROR,MS_MTRN_CNF,ManualConfigLogErrorFormHide);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in FormHide:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
 End;
end;

{*********************************************************
* Function:    TFormManualConfiguration.FillCarList

* Description: Train configuration accepted
*              Returns true if there are enough room in car-list
*********************************************************}
function TFormManualConfiguration.FillCarList:Boolean;
var
ThisCount : Integer;
ThisId : AnsiString;
TooManyCars : Boolean;
VehicleType : Byte;
VehicleIndex : Integer;
VehicleBlockCounter : Integer;
NameIndex : Byte;
VehicleCount : Integer;
begin

        // Clear
  TooManyCars := false;
  NewCarList.NumberofCars := 0;
  NewVehicleDataMsg.NewVehicleData.NrOfVehicleDataBlocks := 0;
  VehicleBlockCounter := NewCarBlocks.NumberofCarBlocks;
  VehicleCount := 0;
        // Expand the car-blocks
  for VehicleIndex := 1 to VehicleBlockCounter do
  begin
     if (NewCarBlocks.CarBlocks[VehicleIndex].Count > 0)then
    begin
        ThisCount := NewCarBlocks.CarBlocks[VehicleIndex].Count;
        ThisId    := NewCarBlocks.CarBlocks[VehicleIndex].ID;
        VehicleCount :=  VehicleCount + ThisCount;
      {Vehicle Data Fill}
      NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[VehicleIndex].NrOfVehicleInBlock := Swap(ThisCount);
      NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[VehicleIndex].VehicleNodeId := 0;
      VehicleType := DataModuleDMI.GetVehicleType(AnsiString(ThisId));
      {If Vehicle Type Found then send Vehicle Type otherwise fill Road No For locomotive}
      if VehicleType > 0 then
       begin
        NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[VehicleIndex].VehicleType := VehicleType;
        NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[VehicleIndex].VehicleName[1] := #0;
       end
      else
      begin
        NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[VehicleIndex].VehicleType := 0;
        for NameIndex := 1 to VEHICLE_NAME_MAX_CHARS  do
         begin
         if (NameIndex > Length(ThisId))
         then  { Fill trailing bytes with blanks }
          NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[VehicleIndex].VehicleName[NameIndex] := #0
         else
          NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[VehicleIndex].VehicleName[NameIndex] := AnsiChar(ThisId[NameIndex]);
        end;
      end;
    end; //End of loop
    DataModuleDMI.SetNbOfVehiclesConnected(VehicleCount);
  end;
  NewVehicleDataMsg.NewVehicleData.NrOfVehicleDataBlocks := Swap(VehicleBlockCounter);
  FormMMIFrame.MainCarlist.NumberofCars := NewCarList.NumberofCars;

  if TooManyCars then
    Result := false
  else
    Result := true;

end;

{ *********************************************************
  * Function:    TFormManualConfiguration.AcceptClick
  * Description: Train configuration accepted
  ********************************************************* }
procedure TFormManualConfiguration.AcceptClick(Sender: TObject);
begin

  try
    EditCountExit(EditCount);
    OSDKeyboardNumericWinControl.Hide;
    OSDKeyboardWinControl.Hide;

    //Send TrainLoaded Status to ATP
    Exchange.SendTrainLoaded(ComboBoxLoadStatus.ItemIndex);

        // Update FormMainArea.LocomotiveConnected
    ComboBoxCarsConnectedChange(ComboBoxCarsConnected);

        // Send LocoVSTrainDir message.
    Exchange.SendLocoVSTrainDir(LocomotiveConnected);
        // Create and send a car list message.
        // Fill new car-list from block-list

    if (FillCarList) then
    begin
       //Send Vehicle Data to ATP
       Exchange.SendVehicleData(@NewVehicleDataMsg);
       EnableAcceptButton(False);
       EnableAcceptWithoutTIMSButton(False);
       RetryInfo.Hide;

       // Disable also Count, car-type and connection end
       DrawGrid.Enabled := false;
       ComboBox.Enabled := false;
       ComboBoxCarsConnected.Enabled := false;
       SpeedButtonNew.Enabled := false;
    end
    else
    begin
      RetryInfo.Caption := InterfaceString(ifRetryConfigCount) + IntToStr(CAR_LIST_MAX);
      RetryInfo.Show;
    end;

  if FormMMIFrame.Sound then
      Beep;

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('TRAINCFG',MS_SW_ERROR, MS_MTRN_CNF, ManualConfigLogErrorAcceptClick);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in AcceptClick:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

procedure TFormManualConfiguration.ComboBoxCarsConnectedChange(Sender: TObject);
begin

  case ComboBoxCarsConnected.ItemIndex of
    0:begin
        LocomotiveConnected:= lcBEnd ;
      end;
    1: begin
       LocomotiveConnected:= lcAEnd ;
     end;

  end;

end;

procedure TFormManualConfiguration.ComboBoxCarsConnectedDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
    // This ensures the correct highlite color is used
  ComboBoxCarsConnected.Canvas.FillRect(Rect);

    // This line draws the actual bitmap
  ImageListCarsConnected.Draw(ComboBoxCarsConnected.Canvas,Rect.Left+10,Rect.Top,Index);

  ComboBoxCarsConnected.Canvas.Brush.Color := clWhite;
  ComboBoxCarsConnected.Canvas.FrameRect(Rect);
end;

procedure TFormManualConfiguration.ComboBoxCarsConnectedEnter(Sender: TObject);
begin
  OSDKeyboardWinControl.Hide;
  OSDKeyboardNumericWinControl.Hide;
end;

procedure TFormManualConfiguration.ComboBoxChange(Sender: TObject);
var
  ThisRow : Integer;
  ThisRect : TRect;
  VehicleType : Byte;
  TypicalConfigType : Byte;
  TypicalConfigVehicleTypeName : AnsiString;
  TypicalConfigCarCount : Byte;
  TypicalConfigVehicleTypeIndex : Byte;
begin

  ThisRow := DrawGrid.Row;

  if (ComboBox.ItemIndex > 0) then
  begin
    NewCarBlocks.CarBlocks[ThisRow + 1].ID := AnsiString(ComboBox.Text);
    VehicleType := DataModuleDMI.GetVehicleType(AnsiString(ComboBox.Text));
    ThisRect := DrawGrid.CellRect(Ord(CarBlockColumnIcon), ThisRow);
    // Clear the icon-field
    DrawGrid.Canvas.FillRect(ThisRect);
    // Draw the icon for the new selection
    ImageList.Draw(DrawGrid.Canvas, ThisRect.Left +
      Round((ThisRect.Right - ThisRect.Left - ImageList.Width) / 2),
      ThisRect.Top + 4, ImageOf(NewCarBlocks.CarBlocks[ThisRow + 1].ID),
      dsNormal, itImage);

    // if (ComboBox.ItemIndex = Ord(CarBlockTypeOther)) then
    if (VehicleType = 0) then
    begin
      TypicalConfigType := DataModuleDMI.GetTypicalConfigType
        (AnsiString(ComboBox.Text));
      if (TypicalConfigType = 0) then
      // Neither VehicleType Nor TypicalConfigType. Option for OTHER
      begin
        ComboBox.Style := csDropDown;
        OSDKeyboardWinControl.Show;
      end
      else
      begin
        TypicalConfigVehicleTypeName :=
          DataModuleDMI.GetTypicalConfigVehicleTypeName(TypicalConfigType);
        TypicalConfigCarCount := DataModuleDMI.GetTypicalConfigCarsCount
          (TypicalConfigType);
        TypicalConfigVehicleTypeIndex :=
          ComboBox.Items.IndexOf(TypicalConfigVehicleTypeName);
        if (TypicalConfigVehicleTypeIndex > 0) then
        begin
          NewCarBlocks.CarBlocks[ThisRow + 1].ID :=
            TypicalConfigVehicleTypeName;
          NewCarBlocks.CarBlocks[ThisRow + 1].Count := TypicalConfigCarCount;
          ComboBox.ItemIndex := TypicalConfigVehicleTypeIndex;
          DrawGrid.Refresh;
        end;
      end;
    end
    else
    begin
      ComboBox.Style := csDropDownList;
      OSDKeyboardWinControl.Hide;
    end;

  end // End of Condition Type is not specified
  else // None specified
  begin
    if (ComboBox.Style = csDropDownList) then
    begin
      NewCarBlocks.CarBlocks[ThisRow + 1].Count := 0;
      EditCount.Text := '0';
    end
    else // else we edit 'Other' type of car
      NewCarBlocks.CarBlocks[ThisRow + 1].ID := AnsiString(ComboBox.Text);
  end;
  RearrangeCarBlockList();

end;



{*********************************************************
* Function:    ImageOf
* Description: Returns an index in the ImageList representing
*               the type of car
*********************************************************}
function TFormManualConfiguration.ImageOf(TypeOfCar : AnsiString):Integer;
var
  VehicleType : Byte;
  TypicalConfigType :Byte;
begin
  VehicleType := DataModuleDMI.GetVehicleType(TypeOfCar);
  TypicalConfigType := DataModuleDMI.GetTypicalConfigType(TypeOfCar);
  if (VehicleType > 0) OR (TypicalConfigType >0 ) then
   Result := 1 //ToDo: Same Type of Icon is used
  else
   Result := 2;
end;

{*********************************************************
* Function:    DrawGridDrawCell
* Description: Customized drawing of the grid
*********************************************************}
procedure TFormManualConfiguration.DrawGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if (ACol = Ord(CarBlockColumnCount)) then
  begin
    DrawGrid.Canvas.Font.Color := clWindow;
    if (NewCarBlocks.NumberofCarBlocks > ARow)
    then
    begin
//      if (NewCarBlocks.CarBlocks[ARow+1].Count > 0) then
        DrawGrid.Canvas.TextOut(Rect.Left+4,Rect.Top + 4,IntToStr(NewCarBlocks.CarBlocks[ARow+1].Count));
//      else
//        DrawGrid.Canvas.TextOut(Rect.Left+4,Rect.Top + 4,'+');
    end
    else
    begin
        DrawGrid.Canvas.TextOut(Rect.Left+4,Rect.Top + 4,IntToStr(0));

    end;
  end;

  if (ACol = Ord(CarBlockColumnIcon)) then
  begin
    if (NewCarBlocks.NumberofCarBlocks > ARow)
    then
    begin
     ImageList.Draw( DrawGrid.Canvas ,Rect.Left + Round((Rect.Right - Rect.Left - ImageList.Width)/2),Rect.Top+4,ImageOf(NewCarBlocks.CarBlocks[ARow+1].ID),dsNormal,itImage);
    end;
  end;

  if (ACol = Ord(CarBlockColumnTypeOfCar)) then
  begin
    if (NewCarBlocks.NumberofCarBlocks > ARow)
    then
    begin
      DrawGrid.Canvas.Font.Color := clWindow;
      DrawGrid.Canvas.TextOut(Rect.Left+4,Rect.Top + 4,String(NewCarBlocks.CarBlocks[ARow+1].ID));
    end;
  end;

end;


{*************************************************************
* Function:    DrawGridSelectCell
* Description: Actions to take when the driver selects a cell
**************************************************************}
procedure TFormManualConfiguration.DrawGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var R: TRect;
    SelectedIndex: Integer;
    VehicleType : Byte;
begin
  VehicleType :=0;
  // Get position and size of selected cell on form
  R := DrawGrid.CellRect(ACol, ARow);
  R.Left := R.Left + DrawGrid.Left;
  R.Right := R.Right + DrawGrid.Left;
  R.Top := R.Top + DrawGrid.Top;
  R.Bottom := R.Bottom + DrawGrid.Top;
  EditCounter := 0;
  if (ACol = Ord(CarBlockColumnCount))
  then  // Edit count
  begin
    {Size and position the edit box to fit the cell}
    {Show the edit control}
    with EditCount do
    begin
      Left := R.Left + 1;
      Top := R.Top + 1;
      Width := (R.Right + 1) - R.Left;
      Height := (R.Bottom + 1) - R.Top;
      EditCounter :=  ARow+1;
      Text := IntToStr(NewCarBlocks.CarBlocks[ARow+1].Count);
      Visible := True;
      SetFocus;
    end;
    ComboBox.Visible := false;

    OSDKeyboardWinControl.Hide;
    OSDKeyboardNumericWinControl.Show;

  end;

  if (ACol = Ord(CarBlockColumnIcon))
  then  // No Control associated
  begin
    EditCount.Visible := false;
    ComboBox.Visible := false;
    OSDKeyboardWinControl.Hide;
    OSDKeyboardNumericWinControl.Hide;
  end;

  if (ACol = Ord(CarBlockColumnTypeOfCar))
  then    // Combobox , type of car
  begin
    {Size and position the combo box to fit the cell}
    {Show the combobox}
    if (Length(NewCarBlocks.CarBlocks[1].ID) = 0)
    then
      SelectedIndex:=0 // None
    else
    begin
      SelectedIndex := ComboBox.Items.IndexOf(String(NewCarBlocks.CarBlocks[ARow+1].ID));
      VehicleType := DataModuleDMI.GetVehicleType(AnsiString(NewCarBlocks.CarBlocks[ARow+1].ID));
    end;
//    if ((SelectedIndex >= 0) AND (SelectedIndex <> Ord(CarBlockTypeOther)))
    if ((SelectedIndex >= 0) AND (VehicleType > 0))
    then
    begin
      ComboBox.ItemIndex := SelectedIndex;
      ComboBox.Style := csDropDownList;
      OSDKeyboardWinControl.Hide;
    end
    else
    begin
      ComboBox.ItemIndex := Ord(CarBlockTypeOther);
      ComboBox.Style := csDropDown;
      OSDKeyboardWinControl.Show;
      if Length(NewCarBlocks.CarBlocks[ARow+1].ID) > 0 then
        ComboBox.Text := String(NewCarBlocks.CarBlocks[ARow+1].ID);
    end;

    with ComboBox do
    begin
      Left := R.Left + 1;
      Top := R.Top + 1;
      Width := (R.Right + 1) - R.Left;
      Height := (R.Bottom + 1) - R.Top;
      Visible := True;
      SetFocus;
    end;

    EditCount.Visible := false;
    OSDKeyboardNumericWinControl.Hide;

  end;

  CanSelect := True;

end;

{*************************************************************
* Function:    EditCountExit
* Description: Actions to take when the focus leaves the
*              count input field.
**************************************************************}
procedure TFormManualConfiguration.EditCountClick(Sender: TObject);
begin
  OSDKeyboardNumericWinControl.Show;

end;

procedure TFormManualConfiguration.EditCountEnter(Sender: TObject);
begin
  OSDKeyboardNumericWinControl.Show;

end;

procedure TFormManualConfiguration.EditCountExit(Sender: TObject);
begin
  if ((EditCounter <> 0) And (EditCounter = (DrawGrid.Row + 1))) then
  begin
  NewCarBlocks.CarBlocks[DrawGrid.Row + 1].Count := StrToIntDef(EditCount.Text,0);
  RearrangeCarBlockList();
  end;
end;

{*************************************************************
* Function:    EnableAcceptButton
* Description: Enable the Accept-button and change the appearance
*              in order to indicate whether the button is possible
*              to click or not
**************************************************************}
procedure TFormManualConfiguration.EnableAcceptButton(Enable:Boolean);
begin

  if Enable then
  begin
    Accept.Enabled := true;
    Accept.Font.Color := ClBlack;
    Accept.BevelStyle := bbRaised;
  end
  else
  begin
    Accept.Enabled := false;
    Accept.Font.Color := ClMedGray;
    Accept.BevelStyle := bbLowered;
  end;

end;
{*************************************************************
* Function:    EnableAcceptWithoutTIMSButton
* Description: Enable the AcceptWithoutTIMS-button and change the appearance
*              in order to indicate whether the button is possible
*              to click or not
**************************************************************}
procedure TFormManualConfiguration.EnableAcceptWithoutTIMSButton(Enable:Boolean);
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
  Rect.Right := ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Width - 1;
  Rect.Bottom := ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Height - 1;

      // Set canvas font equal to text font
  ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Canvas.Font := ColorButtonAcceptWithoutTIMS.Font;
  ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Canvas.Pen.Color := ColorButtonAcceptWithoutTIMS.Font.Color;

  ImageListOther.GetBitmap(1,ColorButtonAcceptWithoutTIMS.Picture.Bitmap);
  ButtonText := InterfaceString(ifAcceptBtn)+ ' '+InterfaceString(ifWithoutTIMS);
  SetBkMode(ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Canvas.Handle,TRANSPARENT);
  DrawText(ColorButtonAcceptWithoutTIMS.Picture.Bitmap.Canvas.Handle,PChar(ButtonText),Length(ButtonText),Rect,DT_CENTER OR DT_WORDBREAK);
  ColorButtonAcceptWithoutTIMS.Invalidate;

end;

{*************************************************************
* Function:    EvaluateTIMSStatus
* Description: Read TIMSAvailable and TIMSOk and adjust buttons and texts
**************************************************************}
procedure TFormManualConfiguration.EvaluateTIMSStatus;
begin

  if (DataModuleDMI.GetTIMSAvailable ) then
  begin

    LabelLastCarDetectedInfo.Visible := false;
    EnableAcceptButton(true);
    Accept.Visible := true;
    ColorButtonAcceptWithoutTIMS.Visible := false;
    EnableAcceptWithoutTIMSButton(false);
    LabelLastCarDetectedInfo.Caption := InterfaceString(ifTIMSAvailable);

  end
  else
  begin

    if (NOT DataModuleDMI.GetTIMSOk) then
      LabelLastCarDetectedInfo.Caption := InterfaceString(ifLastCarNotDetected);

    if (NOT DataModuleDMI.GetTIMSAvailable) then
     LabelLastCarDetectedInfo.Caption := InterfaceString(ifTIMSNotAvailable);


    LabelLastCarDetectedInfo.Visible := true;
    EnableAcceptButton(false);
    Accept.Visible := false;
    ColorButtonAcceptWithoutTIMS.Visible := true;
    EnableAcceptWithoutTIMSButton(true);

  end;


end;

{*************************************************************
* Function:    RearrangeCarBlockList
* Description: Make block list compact
**************************************************************}
procedure TFormManualConfiguration.RearrangeCarBlockList;
var
  I : Integer;
  CarBlockIndex : Integer;
  PrevCarBlocks : TCarBlocksRecord;
begin

  // Copy blocks
  for I := 1 to CAR_BLOCK_LIST_MAX do
  begin
    PrevCarBlocks.CarBlocks[I]:=NewCarBlocks.CarBlocks[I];
    NewCarBlocks.CarBlocks[I].Count := 0;
    NewCarBlocks.CarBlocks[I].ID := '';
  end;

      // Compact
  CarBlockIndex := 1;
  for I := 1 to CAR_BLOCK_LIST_MAX do
  begin
    if (PrevCarBlocks.CarBlocks[I].Count >= 1)
    then
    begin
      NewCarBlocks.CarBlocks[CarBlockIndex]:=PrevCarBlocks.CarBlocks[I];
      Inc(CarBlockIndex);
    end;
  end;

  NewCarBlocks.NumberofCarBlocks := CarBlockIndex -1 ;

  if (NewCarBlocks.NumberofCarBlocks < CAR_BLOCK_LIST_MAX)
  then
    DrawGrid.RowCount := NewCarBlocks.NumberofCarBlocks
  else
    DrawGrid.RowCount := CAR_BLOCK_LIST_MAX;

  ShowDeleteButtons(DrawGrid.RowCount);
  EnableCarsConnected();

end;

{*************************************************************
* Function:    ShowDeleteButtons
* Description: Show a delete button next to an existing row > 1
**************************************************************}
procedure TFormManualConfiguration.ShowDeleteButtons(Rows:Integer);
var
  MyComp  : TComponent;
  ThisRow : Integer;
begin

  for ThisRow := 2 to CAR_BLOCK_LIST_MAX do
  begin

    MyComp := FindComponent('SpeedButtonDel'+IntToStr(ThisRow));
    if MyComp is TSpeedButton then
    begin
      if (ThisRow > Rows) then
        TSpeedButton(MyComp).Visible := false
      else
        TSpeedButton(MyComp).Visible := true;
    end;
  end;

            // Enable and position new-button
  if (Rows < CAR_BLOCK_LIST_MAX) then
  begin

    MyComp := FindComponent('SpeedButtonDel'+IntToStr(Rows+1));
    if MyComp is TSpeedButton then
    begin
      SpeedButtonNew.Left := TSpeedButton(MyComp).Left;
      SpeedButtonNew.Top := TSpeedButton(MyComp).Top;
      SpeedButtonNew.Visible:=true;
    end;
  end
  else
    SpeedButtonNew.Visible:=false;

end;

{*************************************************************
* Function:    EnableCarsConnected
* Description: Disable combo-box when there is no cars connected
**************************************************************}
procedure TFormManualConfiguration.EnableCarsConnected();
begin

      { We always have at least one block with index 1 }
  if (NewCarBlocks.CarBlocks[1].Count = 0) then
      { No cars connected }
  begin
    ComboBoxCarsConnected.ItemIndex := 0; // B-end when no cars
    ComboBoxCarsConnected.Enabled := False;
  end
  else
  begin
    if ComboBox.ItemIndex > 0 then
      ComboBoxCarsConnected.Enabled := True
    else
    begin
        { Car-type none }
      ComboBoxCarsConnected.Enabled := True;
    end;
  end;

end;
procedure TFormManualConfiguration.SpeedButtonDel2Click(Sender: TObject);
begin

  if (EditCount.Focused) then
    EditCountExit(EditCount);

  NewCarBlocks.CarBlocks[2].Count := 0;
  NewCarBlocks.CarBlocks[2].ID := '';

  RearrangeCarBlockList();

end;

procedure TFormManualConfiguration.SpeedButtonDel3Click(Sender: TObject);
begin

  if (EditCount.Focused) then
    EditCountExit(EditCount);

  NewCarBlocks.CarBlocks[3].Count := 0;
  NewCarBlocks.CarBlocks[3].ID := '';

  RearrangeCarBlockList();

end;

procedure TFormManualConfiguration.SpeedButtonDel4Click(Sender: TObject);
begin

  if (EditCount.Focused) then
    EditCountExit(EditCount);

  NewCarBlocks.CarBlocks[4].Count := 0;
  NewCarBlocks.CarBlocks[4].ID := '';

  RearrangeCarBlockList();

end;

procedure TFormManualConfiguration.SpeedButtonDel5Click(Sender: TObject);
begin

  if (EditCount.Focused) then
    EditCountExit(EditCount);

  NewCarBlocks.CarBlocks[5].Count := 0;
  NewCarBlocks.CarBlocks[5].ID := '';

  RearrangeCarBlockList();

end;

procedure TFormManualConfiguration.SpeedButtonDel6Click(Sender: TObject);
begin

  if (EditCount.Focused) then
    EditCountExit(EditCount);

  NewCarBlocks.CarBlocks[6].Count := 0;
  NewCarBlocks.CarBlocks[6].ID := '';

  RearrangeCarBlockList();

end;

procedure TFormManualConfiguration.SpeedButtonDel7Click(Sender: TObject);
begin

  if (EditCount.Focused) then
    EditCountExit(EditCount);

  NewCarBlocks.CarBlocks[7].Count := 0;
  NewCarBlocks.CarBlocks[7].ID := '';

  RearrangeCarBlockList();

end;

procedure TFormManualConfiguration.SpeedButtonDel8Click(Sender: TObject);
begin

  if (EditCount.Focused) then
    EditCountExit(EditCount);

  NewCarBlocks.CarBlocks[8].Count := 0;
  NewCarBlocks.CarBlocks[8].ID := '';

  RearrangeCarBlockList();

end;

procedure TFormManualConfiguration.SpeedButtonNewClick(Sender: TObject);
begin

  if (EditCount.Focused) then
    EditCountExit(EditCount);

  DrawGrid.RowCount := DrawGrid.RowCount + 1;
  NewCarBlocks.CarBlocks[DrawGrid.RowCount].ID:= AnsiString(CarBlockTypes[CarBlockTypeStandard].Name);
  DrawGrid.Row:=DrawGrid.RowCount-1;
  DrawGrid.Col:=0;
  EditCount.SelectAll;

  ShowDeleteButtons(DrawGrid.RowCount);
end;

{***************************************************************************
  * INITIALIZATION PART                                                       *
  **************************************************************************** }
procedure TFormManualConfiguration.FormShow(Sender: TObject);
Var
  I: Integer;
  J: Integer;
  VehicleTypeIndex: Byte;
  VehicleTypeName: AnsiString;
  TypicalConfigIndex: Byte;
  TypicalConfigName: AnsiString;
  TypicalConfigVehicleTypeName: AnsiString;

begin

  inherited;
  ComboBox.Clear;
  ComboBox.Items.Add(InterfaceString(ifNone));
  DrawGrid.Enabled := true;
  ComboBox.Enabled := true;
  SpeedButtonNew.Enabled := true;
  ComboBoxLoadStatus.ItemIndex := 1;


  Try

    for VehicleTypeIndex := 1 to MAX_VEHICLE_TYPE_BLOCKS do
    begin
      VehicleTypeName := DataModuleDMI.GetVehicleTypeName(VehicleTypeIndex);
      if VehicleTypeName <> '' then
      begin
        CarBlockTypes[VehicleTypeIndex].Name := String(VehicleTypeName);
        ComboBox.Items.Add(String(VehicleTypeName));
      end;
    end;



    for TypicalConfigIndex := 1 to MAX_TYPICAL_CONFIG_BLOCKS do
    begin
      TypicalConfigName := DataModuleDMI.GetTypicalConfigName(TypicalConfigIndex);
      TypicalConfigVehicleTypeName :=DataModuleDMI.GetTypicalConfigVehicleTypeName(TypicalConfigIndex);
      if (TypicalConfigName <> '') then
      begin
        if (DataModuleDMI.VehicleTypeNameExists(TypicalConfigVehicleTypeName)) then
        begin
        ComboBox.Items.Add(String(TypicalConfigName));
        end;
      end;
    end;


    // Adding Other option in Vehicle Type List
    VehicleTypeName := 'Other';
    ComboBox.Items.Add(String(VehicleTypeName));
    CarBlockTypeOther := ComboBox.Items.IndexOf(String(VehicleTypeName));
    NewCarList.NumberofCars := 0;
    For I := 1 to CAR_LIST_MAX do
      NewCarList.Car[I].ID := '';

    NewCarBlocks.NumberofCarBlocks := 0;

    for I := 1 to CAR_BLOCK_LIST_MAX do
    begin
      NewCarBlocks.CarBlocks[I].Count := 0;
      NewCarBlocks.CarBlocks[I].ID := '';
    end;

    // Clearing Vechicle Data Msg
    NewVehicleDataMsg.NewVehicleData.NrOfVehicleDataBlocks := 0;
    for J := 1 to MAX_VEHICLE_DATA_BLOCKS do
    begin
      NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J]
        .NrOfVehicleInBlock := 0;
      NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].VehicleType := 0;
      NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].VehicleNodeId := 0;
      NewVehicleDataMsg.NewVehicleData.VehicleDataBlocks[J].VehicleName
        [1] := #0;
    end;

    DrawGrid.ColWidths[Ord(CarBlockColumnCount)] := Round(60 * GetXfactor);
    DrawGrid.ColWidths[Ord(CarBlockColumnIcon)] := Round(60 * GetXfactor);
    DrawGrid.ColWidths[Ord(CarBlockColumnTypeOfCar)] := Round(240 * GetXfactor);
    DrawGrid.DefaultRowHeight := ComboBox.Height;
    DrawGrid.RowCount := 1;
    EditCount.Height := ComboBox.Height;


   StandardCarCount := 1;

    if StandardCarCount > 0 then
    begin
      NewCarBlocks.NumberofCarBlocks := 1;
      NewCarBlocks.CarBlocks[1].Count := StandardCarCount;
      NewCarBlocks.CarBlocks[1].ID :=
        AnsiString(CarBlockTypes[CarBlockTypeStandard].Name);
      ComboBox.ItemIndex := Ord(CarBlockTypeStandard);
    end
    else
    begin
      NewCarBlocks.NumberofCarBlocks := 0;
      NewCarBlocks.CarBlocks[1].Count := 0;
      NewCarBlocks.CarBlocks[1].ID := AnsiString(InterfaceString(ifNone));
    end;

    DrawGrid.RowCount := NewCarBlocks.NumberofCarBlocks;
    ShowDeleteButtons(DrawGrid.RowCount);
    LocomotiveConnected := lcBEnd;
    ComboBoxCarsConnected.ItemIndex := 0;
    DrawGrid.Show;
    EvaluateTIMSStatus;
    RetryInfo.Hide;

    OSDKeyboardWinControl.Parent := self;
    OSDKeyboardWinControl.SetWinControl(ComboBox, ComboBox.OnClick);
    OSDKeyboardNumericWinControl.Parent := self;
    OSDKeyboardNumericWinControl.SetWinControl(EditCount, EditCount.OnClick);
    OSDKeyboardWinControl.Top := DrawGrid.Top + DrawGrid.Height + 40;
    OSDKeyboardNumericWinControl.Top := DrawGrid.Top + DrawGrid.Height + 40;

    DrawGrid.Col := 2;
    { Select other cell than count , in order to remove focus and force
      the driver to select the count field before entering }
    EnableCarsConnected;
    UpdateDMI(true);

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('TRAINCFG', MS_SW_ERROR, MS_MTRN_CNF,
        ManualConfigLogErrorFormShow);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in FormShow:' + E.Message),
          PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);
    end;

  end;
end;
{ *********************************************************
* UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormManualConfiguration.UpdateDMI(Refresh : Boolean);
var
  AdditionalConfirmationInfo : Byte;
  AdditionalConfirmationInfoChanged : Boolean;

  AdditionalAllowedToInfo : Byte;
  AdditionalAllowedToInfoChanged : Boolean;

begin
  inherited;

  EvaluateTIMSStatus;

      // Re-enable controls if any accept-button is enabled
 if (ColorButtonAcceptWithoutTIMS.Enabled or Accept.Enabled) then
  begin
    EnableCarsConnected;
    DrawGrid.Enabled := true;
    ComboBox.Enabled := true;
    SpeedButtonNew.Enabled := true;
  end;

  AdditionalConfirmationInfo := DataModuleDMI.GetAdditionalConfirmationInfo
    (AdditionalConfirmationInfoChanged);
  if (Refresh or AdditionalConfirmationInfoChanged) then
  begin
    if (ByteBool(AdditionalConfirmationInfo and
      CONFIRM_TRAIN_LOADED_STATUS_CHANGE)) then
      FormConfirmLoadedStatus.Show
    else
      FormConfirmLoadedStatus.Hide;
  end;

  AdditionalAllowedToInfo :=  DataModuleDMI.GetAdditionalAllowedToInfo(AdditionalAllowedToInfoChanged);
  if Refresh or AdditionalAllowedToInfoChanged then
  begin
    ComboBoxCarsConnected.Enabled := ByteBool(AdditionalAllowedToInfo and ALLOWED_TO_SELECT_CARS_CONNECTED_ON_A_B_SIDE);
  end;
end;

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

