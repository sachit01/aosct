(*****************************************************************
           © COPYRIGHT Bombardier Transportation, SWEDEN 2011
           ==================================================

    The copyright to the computer program herein is the
    property of Bombardier Transportation AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the
    written permission from Bombardier Transportation AB, or in accordance
    with the terms and conditions stipulated in the
    agreement/contract under which the program has been
    supplied.


    NAME :  UnitTrainVsTrack.pas

    PROJECT :  LKAB, InterFlow TrainBorne

    Ver    Author           Date    Reason
    ---    ------           ------  ------
           Bo Hermansson    110331  Designed for LK
           Bo Hermansson    111215  New symbols for LK

    DESCRIPTION :

    Displays the buttons to select Train versus Track direction.

    INTERFACE :
*********************************************************)

unit UnitTrainVsTrack;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ColorButton2, StdCtrls, MMITypes, UnitMainLayer2, GestureMgr,
  ImgList, Menus, DB, ExtCtrls, Buttons, Grids, DBGrids, MMIPanel;

type
  TFormTrainVsTrack = class(TFormMainLayer2)
    TrainVSTrack: TLabel;
    LocoVSTrackAForward: TColorButton;
    LocoVSTrackAReverse: TColorButton;
    TrainVSTrackAForward: TColorButton;
    TrainVSTrackAReverse: TColorButton;
    LocoVSTrackBForward: TColorButton;
    LocoVSTrackBReverse: TColorButton;
    TrainVSTrackBForward: TColorButton;
    TrainVSTrackBReverse: TColorButton;
    TrainVSTrackAForwardCarAtA: TColorButton;
    TrainVSTrackAReverseCarAtA: TColorButton;
    TrainVSTrackBReverseCarAtA: TColorButton;
    TrainVSTrackBForwardCarAtA: TColorButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrainVSTrackClick(Sender: TObject);
  private
    { Private declarations }
    Procedure ScreenChange;

  public
    { Public declarations }
    LocoVsTrack : Boolean;
    LocomotiveConnected : TLocomotiveConnected;

  end;

var
  FormTrainVsTrack: TFormTrainVsTrack;

implementation

{$R *.dfm}

uses UnitMainArea, MMIStd, UnitExchange, UnitMMIFrame, UnitDMIDataModule;

procedure TFormTrainVsTrack.FormCreate(Sender: TObject);
begin
  inherited;

  TrainVSTrack.Caption:= InterfaceString(ifTRnVsTrk) ;
  TrainVSTrack.Width  := ClientWidth;

      {Custom images}

  LoadBitmapImage(LocoVSTrackAForward.Picture.Bitmap, ImageLocoVSTrackAForward, 'REG');
  LoadBitmapImage(LocoVSTrackAReverse.Picture.Bitmap, ImageLocoVSTrackAReverse, 'REG');
  LoadBitmapImage(LocoVSTrackBForward.Picture.Bitmap, ImageLocoVSTrackBForward, 'REG');
  LoadBitmapImage(LocoVSTrackBReverse.Picture.Bitmap, ImageLocoVSTrackBReverse, 'REG');
  LoadBitmapImage(TrainVSTrackAForward.Picture.Bitmap, ImageTrainVSTrackAForward, 'REG');
  LoadBitmapImage(TrainVSTrackAReverse.Picture.Bitmap, ImageTrainVSTrackAReverse, 'REG');
  LoadBitmapImage(TrainVSTrackBForward.Picture.Bitmap, ImageTrainVSTrackBForward, 'REG');
  LoadBitmapImage(TrainVSTrackBReverse.Picture.Bitmap, ImageTrainVSTrackBReverse, 'REG');
  LoadBitmapImage(TrainVSTrackAForwardCarAtA.Picture.Bitmap, ImageTrainVSTrackAForwardCarAtA, 'REG');
  LoadBitmapImage(TrainVSTrackAReverseCarAtA.Picture.Bitmap, ImageTrainVSTrackAReverseCarAtA, 'REG');
  LoadBitmapImage(TrainVSTrackBForwardCarAtA.Picture.Bitmap, ImageTrainVSTrackBForwardCarAtA, 'REG');
  LoadBitmapImage(TrainVSTrackBReverseCarAtA.Picture.Bitmap, ImageTrainVSTrackBReverseCarAtA, 'REG');

  Parent := FormMMIFrame.PanelMMIFrame;
  Align := alClient;

  ScreenChange;

end;

{*********************************************************
* Function:    FormShow
* Description: Show/Hide buttons depending on if Loco or train
*              Also adjusts button positions to the center of the screen
*********************************************************}
procedure TFormTrainVsTrack.FormShow(Sender: TObject);
const
  ButtonSpace = 8; // Pixels between buttons

var
  MidForm : Integer;
  ButtonWidth : Integer;
begin
  inherited;
        // Middle of form

  If (DataModuleDMI.GetNbOfVehiclesConnected() = 0)then
     LocoVsTrack := true
  else
     LocoVsTrack := false;

  MidForm := Width Div 2;
        // Same size for all buttons
  ButtonWidth := LocoVSTrackAForward.Width;

        // Appear as not pressed
  LocoVSTrackAForward.BevelStyle := bbRaised;
  LocoVSTrackBForward.BevelStyle := bbRaised;
  LocoVSTrackAReverse.BevelStyle := bbRaised;
  LocoVSTrackBReverse.BevelStyle := bbRaised;

  TrainVSTrackAForward.BevelStyle := bbRaised;
  TrainVSTrackBForward.BevelStyle := bbRaised;
  TrainVSTrackAReverse.BevelStyle := bbRaised;
  TrainVSTrackBReverse.BevelStyle := bbRaised;

  TrainVSTrackAForwardCarAtA.BevelStyle := bbRaised;
  TrainVSTrackBForwardCarAtA.BevelStyle := bbRaised;
  TrainVSTrackAReverseCarAtA.BevelStyle := bbRaised;
  TrainVSTrackBReverseCarAtA.BevelStyle := bbRaised;

  if LocoVsTrack
    then
    begin

      LocoVSTrackAForward.Show ;
      LocoVSTrackBForward.Show ;
      LocoVSTrackAReverse.Show ;
      LocoVSTrackBReverse.Show ;

        // Center
      LocoVSTrackAForward.Left := MidForm - (ButtonSpace Div 2) - ButtonWidth;
      LocoVSTrackBForward.Left := MidForm - (ButtonSpace Div 2) - ButtonWidth;
      LocoVSTrackAReverse.Left := MidForm + (ButtonSpace Div 2);
      LocoVSTrackBReverse.Left := MidForm + (ButtonSpace Div 2);

      TrainVSTrackAForward.Hide ;
      TrainVSTrackBForward.Hide ;
      TrainVSTrackAReverse.Hide ;
      TrainVSTrackBReverse.Hide ;

      TrainVSTrackAForwardCarAtA.Hide ;
      TrainVSTrackBForwardCarAtA.Hide ;
      TrainVSTrackAReverseCarAtA.Hide ;
      TrainVSTrackBReverseCarAtA.Hide ;

    end
    else
    begin

      LocoVSTrackAForward.Hide ;
      LocoVSTrackBForward.Hide ;
      LocoVSTrackAReverse.Hide ;
      LocoVSTrackBReverse.Hide ;

      if (LocomotiveConnected = lcAEnd) then
      begin

        TrainVSTrackAForwardCarAtA.Show ;
        TrainVSTrackBForwardCarAtA.Show ;
        TrainVSTrackAReverseCarAtA.Show ;
        TrainVSTrackBReverseCarAtA.Show ;

          // Center
        TrainVSTrackAForwardCarAtA.Left := MidForm - (ButtonSpace Div 2) - ButtonWidth;
        TrainVSTrackBForwardCarAtA.Left := MidForm - (ButtonSpace Div 2) - ButtonWidth;
        TrainVSTrackAReverseCarAtA.Left := MidForm + (ButtonSpace Div 2);
        TrainVSTrackBReverseCarAtA.Left := MidForm + (ButtonSpace Div 2);

          // Same pos
        TrainVSTrackAForwardCarAtA.Top :=  TrainVSTrackAForward.Top;
        TrainVSTrackBForwardCarAtA.Top :=  TrainVSTrackBForward.Top;
        TrainVSTrackAReverseCarAtA.Top :=  TrainVSTrackAReverse.Top;
        TrainVSTrackBReverseCarAtA.Top :=  TrainVSTrackBReverse.Top;

        TrainVSTrackAForward.Hide ;
        TrainVSTrackBForward.Hide ;
        TrainVSTrackAReverse.Hide ;
        TrainVSTrackBReverse.Hide ;

      end
      else
      begin

        TrainVSTrackAForward.Show ;
        TrainVSTrackBForward.Show ;
        TrainVSTrackAReverse.Show ;
        TrainVSTrackBReverse.Show ;

          // Center
        TrainVSTrackAForward.Left := MidForm - (ButtonSpace Div 2) - ButtonWidth;
        TrainVSTrackBForward.Left := MidForm - (ButtonSpace Div 2) - ButtonWidth;
        TrainVSTrackAReverse.Left := MidForm + (ButtonSpace Div 2);
        TrainVSTrackBReverse.Left := MidForm + (ButtonSpace Div 2);

        TrainVSTrackAForwardCarAtA.Hide ;
        TrainVSTrackBForwardCarAtA.Hide ;
        TrainVSTrackAReverseCarAtA.Hide ;
        TrainVSTrackBReverseCarAtA.Hide ;

      end;

    end;

  UpdateDMI(true);

end;

{*********************************************************
* Function:    TrainVsTrackClick
* Description: OnClick - handler for all the buttons on the form.
*              Sends the selected TrainVsTrackDirection to the ATP
*********************************************************}
procedure TFormTrainVsTrack.TrainVSTrackClick(Sender: TObject);
var
  ThisButton : TColorButton;
begin
  if (Sender = TrainVSTrackAForward) or
     (Sender = TrainVSTrackAForwardCarAtA) or
     (Sender = LocoVSTrackAForward) then
    Exchange.SendTrainVSTrackDir(tdForwardDrivingForward) ;

  if (Sender = TrainVSTrackBForward) or
     (Sender = TrainVSTrackBForwardCarAtA) or
     (Sender = LocoVSTrackBForward) then
    Exchange.SendTrainVSTrackDir(tdReverseDrivingForward) ;

  if (Sender = TrainVSTrackAReverse) or
     (Sender = TrainVSTrackAReverseCarAtA) or
     (Sender = LocoVSTrackAReverse) then
    Exchange.SendTrainVSTrackDir(tdForwardDrivingReverse) ;

  if (Sender = TrainVSTrackBReverse) or
     (Sender = TrainVSTrackBReverseCarAtA) or
     (Sender = LocoVSTrackBReverse) then
    Exchange.SendTrainVSTrackDir(tdReverseDrivingReverse) ;

  if Sender is TColorButton
   then
   begin
     ThisButton := TColorButton(Sender);
     ThisButton.BevelStyle := bbLowered;
   end;

end;

{*********************************************************
* Function:    ScreenChange
* Description: Adjusts positions and widths when the screen
*              resolution is different from the standard
*              640 * 480 pixels
*********************************************************}
procedure TFormTrainVsTrack.ScreenChange;
begin  {ScreenChange}

  Width := FormMMIFrame.ClientWidth;
  Height := FormMMIFrame.ClientHeight;

  TrainVSTrack.Left:= round(TrainVSTrack.Left*GetXfactor);
  TrainVSTrack.Top:= round(TrainVSTrack.Top*GetYfactor);
  TrainVSTrack.Width:= round(TrainVSTrack.Width*GetXfactor);
  TrainVSTrack.Height:= round(TrainVSTrack.Height*GetYfactor);
  TrainVSTrack.Font.Size:= round(TrainVSTrack.Font.Size*GetXfactor);

  AdjustColorButton(LocoVSTrackAForward);
  AdjustColorButton(LocoVSTrackBForward);
  AdjustColorButton(LocoVSTrackAReverse);
  AdjustColorButton(LocoVSTrackBReverse);

  AdjustColorButton(TrainVSTrackAForward);
  AdjustColorButton(TrainVSTrackBForward);
  AdjustColorButton(TrainVSTrackAReverse);
  AdjustColorButton(TrainVSTrackBReverse);

  AdjustColorButton(TrainVSTrackAForwardCarAtA);
  AdjustColorButton(TrainVSTrackBForwardCarAtA);
  AdjustColorButton(TrainVSTrackAReverseCarAtA);
  AdjustColorButton(TrainVSTrackBReverseCarAtA);

end;


end.
