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

unit UnitAutomaticConfiguration;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UnitMainLayer2, ColorButton2, ExtCtrls, StdCtrls, UnitMainArea, MMITypes, ImgList, Grids,
  GestureMgr, Menus, DB, Buttons, DBGrids, MMIPanel;

type
  TFormAutomaticConfiguration = class(TFormMainLayer2)
    ColorButtonAccept: TColorButton;
    LabelLocConnected: TLabel;
    LabelTitle: TLabel;
    ComboBoxLoaded: TComboBox;
    LabelLoadStatus: TLabel;
    ComboBoxCarsConnected: TComboBox;
    ImageListCarsConnected: TImageList;
    Procedure ScreenChange;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ColorButtonAcceptClick(Sender: TObject);
    procedure ComboBoxCarsConnectedChange(Sender: TObject);
    procedure ComboBoxCarsConnectedDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);



  private
    { Private declarations }
    LocomotiveConnected : TLocomotiveConnected;   // reported from ATP

  public
    { Public declarations }
     procedure UpdateDMI(Refresh : Boolean);
  end;

var
  FormAutomaticConfiguration: TFormAutomaticConfiguration;

implementation
{$R *.DFM}

uses
  UnitExchange, MMIStd , UnitMMIFrame, UnitManualConfiguration,
  UnitDMIDataModule, UnitConfirmRejectSetup,UnitConfirmLoadedStatus;

const
  ReConfigurationLogErrorFormCreate = 1;
  ReConfigurationLogErrorFormShow   = 2;
  ReConfigurationLogErrorAcceptClick= 3;

{*********************************************************
* Function:    ComboBoxLoadedChange
* Description: To select the loaded status
*********************************************************}
procedure TFormAutomaticConfiguration.ComboBoxCarsConnectedChange(
  Sender: TObject);
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



procedure TFormAutomaticConfiguration.ComboBoxCarsConnectedDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
   // This line draws the actual bitmap
  ImageListCarsConnected.Draw(ComboBoxCarsConnected.Canvas,Rect.Left+10,Rect.Top,Index);

  ComboBoxCarsConnected.Canvas.Brush.Color := clWhite;
  ComboBoxCarsConnected.Canvas.FrameRect(Rect);

end;

procedure TFormAutomaticConfiguration.FormCreate(Sender: TObject);
begin
   inherited;

  Try
    LocomotiveConnected := lcBEnd;
   ColorButtonAccept.Caption:=InterfaceString(ifAcceptBtn) ;
   LabelLocConnected.Caption:=InterfaceString(ifLocDir) ;
   Parent := FormMMIFrame.PanelMMIFrame;
   Align := alClient;


   ComboBoxCarsConnected.ItemIndex := 0;
   ComboBoxCarsConnected.Enabled := False;
   ComboBoxLoaded.Items[0] := InterfaceString(ifNotLoaded);
   ComboBoxLoaded.Items[1] := InterfaceString(ifLoaded);
   ComboBoxLoaded.ItemIndex := 1;

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

procedure TFormAutomaticConfiguration.ScreenChange;

begin  { ScreenChange }

  AdjustLabel(LabelLocConnected);
  AdjustComboBox(ComboBoxLoaded);
  AdjustLabel(LabelTitle);
  AdjustComboBox(ComboBoxCarsConnected);
  AdjustColorButton(ColorButtonAccept);
  AdjustLabel(LabelLocationName);
  AdjustLabel(LabelLoadStatus);

end;


procedure TFormAutomaticConfiguration.FormShow(Sender: TObject);
begin

  inherited;
  Try

    LabelLocConnected.Visible := true;
    LabelLocConnected.Enabled := true;
    LabelLoadStatus.Visible := true;
    LabelLoadStatus.Enabled := true;
    ComboBoxLoaded.Visible := true;
    ComboBoxLoaded.Enabled := true;
    ColorButtonAccept.Visible := true;
    ColorButtonAccept.Enabled := true;
    ComboBoxCarsConnected.Visible := true;
    ComboBoxCarsConnected.Enabled := true;
    ComboBoxLoaded.ItemIndex := 1;

    UpdateDMI(true);
  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('REREG', MS_SW_ERROR, MS_RE_CNF,
        ReConfigurationLogErrorFormShow);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in FormShow:' + E.Message),
          PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);
    end;
  End;

end;

Procedure TFormAutomaticConfiguration.ColorButtonAcceptClick(Sender: TObject);
begin
  Exchange.SendTrainLoaded(ComboBoxLoaded.ItemIndex);
  Exchange.SendLocoVSTrainDir(LocomotiveConnected);
  Exchange.SendMMIButton(bsAcceptAutomaticConfiguration);
end;

{ *********************************************************
* UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormAutomaticConfiguration.UpdateDMI(Refresh : Boolean);
var
  AdditionalConfirmationInfo: Byte;
  AdditionalConfirmationInfoChanged: Boolean;

  AdditionalAllowedToInfo : Byte;
  AdditionalAllowedToInfoChanged : Boolean;

begin
  inherited;
  AdditionalConfirmationInfo := DataModuleDMI.GetAdditionalConfirmationInfo
    (AdditionalConfirmationInfoChanged);

  if Refresh or AdditionalConfirmationInfoChanged then
  begin
    { Confirm Confirm Train Loaded }
   if (ByteBool(AdditionalConfirmationInfo and
      CONFIRM_TRAIN_LOADED_STATUS_CHANGE)) then
    begin
      FormConfirmLoadedStatus.Show;
    end
    else
      FormConfirmLoadedStatus.Hide;
  end;

  AdditionalAllowedToInfo :=  DataModuleDMI.GetAdditionalAllowedToInfo(AdditionalAllowedToInfoChanged);
  if Refresh or AdditionalAllowedToInfoChanged then
  begin
    ComboBoxCarsConnected.Enabled := ByteBool(AdditionalAllowedToInfo and ALLOWED_TO_SELECT_CARS_CONNECTED_ON_A_B_SIDE);
  end;

end;
end.

