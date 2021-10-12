(*****************************************************************
           © COPYRIGHT Adtranz Signal AB, SWEDEN 1998.
           ===========================================

    The copyright to the computer program herein is the
    property of Adtranz AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the
    written permission from Adtranz AB, or in accordance
    with the terms and conditions stipulated in the
    agreement/contract under which the program has been
    supplied.


    NAME :  MMIAreaA.pas

    PROJECT :  Esmeralda, InterFlow TrainBorne

    Ver    Author           Date    Reason
    ---    ------           ----    ------
    1.0.0  Jan Kiessling    970701  First version
    1.1.0  Morgan Persson   971229  Created a reuseable component
    2.0    Edward Lundin    020917  Added the logarithmic property.
                                    Removed the use of MainArea.
                                    Added scale property of the A3-graph.
    3.2.0  Bo Hermansson    150126  Now independent of other MMI units

    DESCRIPTION :  Component for the A-area in the ETCS MMI
                   showing detailed brake information

    INTERFACE :

*********************************************************)

unit MMIareaA;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TMMIareaA = class(TGraphicControl)
  private
    { Private declarations }

    {Common properties}
    FPalette: hPalette;
    FForeColor: TColor;
    FBackColor: TColor;
    FSpeedingColor : TColor ;
    FWarningColor: TColor;
    FInterventionColor: TColor;
    FUpDateNeeded : Boolean ;
    FOrangeColor: TColor;
    FInBCA: Boolean;
    FSpeeding : Boolean;
    FWarning: Boolean;
    FIntervention: Boolean;

    {properties for area A1}
    FShowTimeToIntervention: Boolean;
    FTimeToIntervention: Integer;     //0..10

    {properties for area A2}
    FMaxScaleM: Integer;
    FScaleDivision: Integer;                  //How often will larger division be displayed on scale
    FScaleTicmark: Integer;                   //How often will small division be displayed on scale
    FLogScale: Boolean;                       //Whether or not the scale will be logarithmic. If true,
                                              //max scale and ticmarks are not variable.

    FShowDistanceToTarget: Boolean;
    FDistanceToTarget: Integer;
    FMAMargin : Integer;
    FShowPredictedStopPosition: Boolean;
    FPredictedStopPosition: Integer;

    {properties for area A3}
    FShowPredictedSpeedAtTarget: Boolean;
    FPredictedSpeedAtTarget: Integer;     //Relative to speed target.
    FPredSpeedTolerance: Integer;
    FPredSpeedMaxScale: Integer;          //The scale of the graph spans +/- this value.


    procedure Setpalette(Value: hPalette);
    procedure SetForeColor(Value: TColor);
    procedure SetBackColor(Value: TColor);
    procedure SetWarningColor(Value: TColor);
    procedure SetInterventionColor(Value: TColor);
    procedure SetOrangeColor(Value: TColor);
    procedure SetInBCA(Value: Boolean);
    Procedure SetSpeeding(Value : Boolean ) ;
    procedure SetWarning(Value: Boolean);
    procedure SetIntervention(Value: Boolean);

    procedure SetShowTimeToIntervention(Value: Boolean);
    procedure SetTimeToIntervention(Value: Integer);

    procedure SetMaxScaleM(Value: Integer);
    procedure SetScaleDivision(Value: Integer);     //sigdap990828
    procedure SetScaleTicmark(Value: Integer);      //sigdap 990828
    procedure SetLogScale(Value: Boolean);          //konedlu 2002-09-17

    procedure SetShowDistanceToTarget(Value: Boolean);
    procedure SetDistanceToTarget(Value: Integer);
    procedure SetMAMargin(Value: Integer);
    procedure SetShowPredictedStopPosition(Value: Boolean);
    procedure SetPredictedStopPosition(Value: Integer);

    procedure SetShowPredictedSpeedAtTarget(Value: Boolean);
    procedure SetPredictedSpeedAtTarget(Value: Integer);
    procedure SetPredSpeedTolerance(Value: Integer);
    procedure SetPredSpeedMaxScale(Value: Integer);  //konedlu 2002-09-17
//    function getThePalette: hPalette;

  protected
    { Protected declarations }
    procedure PaintBackground ;        // sigdao990828 (AnImage: TBitmap);
    procedure Paint; override;
    procedure PaintA1;
    procedure PaintLinA2;  //Linear scale.
    procedure PaintLogA2;  //Logarithmic scale.
    procedure PaintA3;
  public
    { Public declarations }
    XFactor : Double;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    procedure RefreshArea;
    property PaletteToUse: hPalette read FPalette write SetPalette ;
    property ForeColor: TColor read FForeColor write SetForeColor default clSilver;
    property BackColor: TColor read FBackColor write SetBackColor default clBlack;
    property WarningColor: TColor read FWarningColor write SetWarningColor;
    property InterventionColor: TColor read FInterventionColor write SetInterventionColor default clRed;
    property OrangeColor: TColor read FOrangeColor write SetOrangeColor;
    property InBCA: Boolean read FInBCA write SetInBCA default True;  //True so it is visible in designtime.
    property Speeding: Boolean read FSpeeding write SetSpeeding default False;
    property Warning: Boolean read FWarning write SetWarning default False;
    property Intervention: Boolean read FIntervention write SetIntervention default False;

    property ShowTimeToIntervention: Boolean read FShowTimeToIntervention write SetShowTimeToIntervention default True; //So that it is visible in designtime.
    property TimeToIntervention: Integer read FTimeToIntervention write SetTimeToIntervention default 500;

    property MaxScaleM: Integer read FMaxScaleM write SetMaxScaleM default 200;
    property ScaleDivision: Integer read FScaleDivision write SetScaleDivision default 50;
    property ScaleTicmark: Integer read FScaleTicmark write SetScaleTicmark stored True default 10;
    property LogScale: Boolean read FLogScale write SetLogScale default false;

    property ShowDistanceToTarget: Boolean read FShowDistanceToTarget write SetShowDistanceToTarget default False;
    property DistanceToTarget: Integer read FDistanceToTarget write SetDistanceToTarget;

    property MAMargin: Integer read FMAMargin write SetMAMargin;

    property ShowPredictedStopPosition: Boolean read FShowPredictedStopPosition write SetShowPredictedStopPosition default False;
    property PredictedStopPosition: Integer read FPredictedStopPosition write SetPredictedStopPosition;

    property ShowPredictedSpeedAtTarget: Boolean read FShowPredictedSpeedAtTarget write SetShowPredictedSpeedAtTarget default False; //True, so that it is visible in designtime!
    property PredictedSpeedAtTarget: Integer read FPredictedSpeedAtTarget write SetPredictedSpeedAtTarget;
    property PredSpeedTolerance: Integer read FPredSpeedTolerance write SetPredSpeedTolerance default 4;
    property PredSpeedMaxScale: Integer read FPredSpeedMaxScale write SetPredSpeedMaxScale default 10;
  end;

procedure Register;


implementation

uses Math,MMITypes, UnitDMIDataModule;

Const
MAXTIMETOINTERVENTION = 10 ;

var
   TheImage: TBitmap;
   ARect: TRect;   // A2 area
   SRect: TRect;   // Length staple
   PRect: TRect;   // Predicted stop_pos/speed at target whole rect
   P2Rect: TRect;  // Predicted stop_pos/speed at target orange rect
   PaintRect: TRect;
   ScreenDC : hDC;
   PixelPerMeter: Double;
   ModValue: Integer;


constructor TMMIareaA.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed,
                                 csOpaque,
                                 csFixedWidth,csFixedHeight,
                                 csNoStdEvents	] ;  // Sigdap 990828

  //Set property default values.
  FForeColor := clSilver;
  FBackColor := clBlack;
  FInterventionColor := clRed;
  FInBCA := True;  //For visibility in designtime.
  FSpeeding := False;
  FWarning := False;
  FIntervention := False;

  FShowTimeToIntervention := True;
  FTimeToIntervention := 500;

  FMaxScaleM := 200;
  FScaleDivision := 50;
  FScaleTicmark := 10;
  FLogScale := false;

  FShowDistanceToTarget := False;
  FShowPredictedStopPosition := False;

  FShowPredictedSpeedAtTarget := True;  //For visibility in designtime.
  FPredSpeedTolerance := 4;
  FPredSpeedMaxScale := 10;

  FOrangeColor := $020050E0;
  FWarningColor := FOrangeColor;
  FSpeedingColor := clYellow ;
  FUpDateNeeded := False;

  TheImage := TBitmap.Create;   // sigdap990828

  XFactor := 1;   // May be adjusted by owner when resolution changes

end;

destructor TMMIareaA.Destroy;
begin
  inherited destroy;
end;

procedure TMMIareaA.Paint;

begin
  with Canvas do begin
    try

      TheImage.Height := Height;
      TheImage.Width := Width;
      PaintRect := ClientRect;

      ScreenDC := TheImage.Canvas.Handle;
      SelectPalette(ScreenDC, PaletteToUse, False);
      RealizePalette(ScreenDC);

      try
        PaintBackground() ;
        if InBCA then begin
          if ShowTimeToIntervention then
            PaintA1();
          if LogScale then
            PaintLogA2
          else
            PaintLinA2();
          PaintA3();
        end;
      finally
      end;

      Canvas.CopyMode := cmSrcCopy;
      Canvas.Draw(0, 0, TheImage);
    finally
//      TheImage.Free;  // sigdap990828
    end;
  end;
end;

procedure TMMIareaA.PaintBackground() ; // sigdao990828 (AnImage: TBitmap);
begin
  with TheImage.Canvas do
  begin
    ARect := Rect(0, 0, Width, Height);
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    FillRect(ARect);

    // Paint the 'bsLowered style' frame
    Pen.Color := $02202020;
    Pen.Width := 1;
    MoveTo(1,Height-1);
    LineTo(Width-1, Height-1);
    LineTo(Width-1,1);
    Pen.Color := $02000000;
    LineTo(1,1);
    LineTo(1,Height-1);

  end;
end;


procedure TMMIareaA.PaintA1() ;        // sigdao990828 (TheImage: TBitmap);
var
   Value: integer;
   Time: Integer;
   Factor: Integer;

begin
   with ARect do
   begin
      Left := 0;
      Top := 0;
      Right := TheImage.Width;
      Bottom := TheImage.Width;
   end;
   Time := TimeToIntervention;
   if Time > MAXTIMETOINTERVENTION then
     Time := MAXTIMETOINTERVENTION;
   Factor := TheImage.Width div (2* MAXTIMETOINTERVENTION);
   Value := Factor * Time +1;
   InflateRect(ARect, -Value, -Value);

   with TheImage.Canvas do
   begin
      Brush.Color := FForeColor;
      if Warning then
        Brush.Color := FWarningColor
      else if Intervention then
        Brush.Color := FInterventionColor
      else
        Brush.Color := FSpeedingColor;

      Brush.Style := bsSolid;
      FillRect(ARect);
   end;

end;

procedure TMMIareaA.PaintLinA2();
var
   ScaleX1, ScaleX2: Integer;
   YPix0M: Integer;
   Y: Integer;
   Tx, Ty: Integer;
   sDistance: String;
   TempDistanceToTarget: Integer;
   LengthOfMAMarginInM : Integer;
   ModifiedDistance: Integer;
   FlashBar: Boolean;
   ScaleLengthPixel: Integer;

   ATPModeChanged : Boolean;
begin
   FlashBar := false;
   with ARect do
   begin    // This is whole A2  area
      Left := 0;
      Top := TheImage.Width;
      Right := TheImage.Width;
     if (ShowPredictedSpeedAtTarget) then
        Bottom := TheImage.Height - (TheImage.Width div 2)
      else
        Bottom := TheImage.Height;   end;

   ScaleLengthPixel := (ARect.Bottom - ARect.Top) - (2 * TheImage.Canvas.TextHeight('1'));
   PixelPerMeter := ScaleLengthPixel / MaxScaleM;

   YPix0M := ARect.Bottom;
   ScaleX1 := (TheImage.Width div 3) + 4;
   ScaleX2 := ScaleX1 + (ScaleX1 div 2);

   // Paint the scale
   with TheImage.Canvas do
   begin
      Pen.Color := FForeColor;
      Pen.Style := psSolid;
      for Y:=0 to MaxScaleM do
      begin
         if ((Y mod FScaleDivision) = 0) then
         begin
            Pen.Width := 1;
            MoveTo(ScaleX1, YPix0M - Round(Y*PixelPerMeter));
            LineTo(ScaleX2, YPix0M - Round(Y*PixelPerMeter));
            MoveTo(ScaleX1, YPix0M - Round(Y*PixelPerMeter) - 1);
            LineTo(ScaleX2, YPix0M - Round(Y*PixelPerMeter) - 1);
         end
         else
         if ((Y mod FScaleTicmark) = 0) then
         begin
            Pen.Width := 1;
            MoveTo(ScaleX1+2, YPix0M - Round(Y*PixelPerMeter));
            LineTo(ScaleX2-2, YPix0M - Round(Y*PixelPerMeter));
         end;
      end;
   end;


 // Paint distance to brake target bar
   if ShowDistanceToTarget then
   begin
     if DistanceToTarget > MaxScaleM then
     begin
       TempDistanceToTarget := MaxScaleM;
       FlashBar := True;
     end
     else
     begin
       TempDistanceToTarget := DistanceToTarget;
     end;


      if(DataModuleDMI.GetATPMode(ATPModeChanged) = amLocation) then
      // Set value to 0 to not show MA Margin bar in Location Mode
       LengthOfMAMarginInM :=  0
     else
      // truncate Length of Margin in cm to m
       LengthOfMAMarginInM := MAMargin div 100;

     if (TempDistanceToTarget > LengthofMAMarginInM) then
     begin
       // Draw grey rectangle corresponding with length TempDistanceToTarget - LengthofMAMarginInM
       with SRect do
       begin
         Left := ScaleX2 + 3;
         Top := YPix0M - Round(TempDistanceToTarget * PixelPerMeter);
         Right := Left + (TheImage.Width div 5);
         Bottom := YPix0M - Round(LengthofMAMarginInM * PixelPerMeter);
       end;
       with TheImage.Canvas do
       begin
         Brush.Color := FForeColor;
         Brush.Style := bsSolid;
         FillRect(SRect);
         if FlashBar then
         begin
           Pen.Color := BackColor;
           Pen.Style := psSolid;
           Pen.Width := 3;
           MoveTo(SRect.Left, SRect.Top + 20);
           LineTo(Round(SRect.Left + ((SRect.Right - SRect.Left) / 2) + 3),
             SRect.Top + 20);
           LineTo(Round(SRect.Left + ((SRect.Right - SRect.Left) / 2) - 3),
             SRect.Top + 25);
           LineTo(SRect.Right, SRect.Top + 25);
         end;
       end;

       // Draw Green rectangle corresponding with length LengthofMAMarginInM
       with SRect do
       begin
         Left := ScaleX2 + 3;
         Top := YPix0M - Round(LengthofMAMarginInM * PixelPerMeter);
         Right := Left + (TheImage.Width div 5);
         Bottom := YPix0M;
       end;
       with TheImage.Canvas do
       begin
         Brush.Color := clGreen;
         Brush.Style := bsSolid;
         FillRect(SRect);
       end;

     end
     else
     begin
       // Draw Green rectangle with length TempDistanceToTarget.
       // Green color bar is because the distance to target is less than MA Margin
     with SRect do
       begin
         Left := ScaleX2 + 3;
         Top := YPix0M - Round(TempDistanceToTarget* PixelPerMeter);
         Right := Left + (TheImage.Width div 5);
         Bottom := YPix0M;
       end;
       with TheImage.Canvas do
       //When Distance to Target > Length of MAMargin , it should grey ,then
       //turn to green when Lenght of MAMargin is reached.
       begin
        if (DistanceToTarget > LengthofMAMarginInM) then
         begin
           Brush.Color := FForeColor
         end
         else
         begin
           Brush.Color := clGreen;
         end;

         Brush.Style := bsSolid;
         FillRect(SRect);
         if FlashBar then
         begin
           Pen.Color := BackColor;
           Pen.Style := psSolid;
           Pen.Width := 3;
           MoveTo(SRect.Left, SRect.Top + 20);
           LineTo(Round(SRect.Left + ((SRect.Right - SRect.Left) / 2) + 3),
             SRect.Top + 20);
           LineTo(Round(SRect.Left + ((SRect.Right - SRect.Left) / 2) - 3),
             SRect.Top + 25);
           LineTo(SRect.Right, SRect.Top + 25);
         end;
       end;
     end;

    end;
   // Predicted stop position if any
   if ShowPredictedStopPosition then
   begin
      with PRect do
      begin
         Left := 9;
         Right := left + (TheImage.Width div 5);
      end;
      with TheImage.Canvas do
      begin
        Brush.Color := FForeColor;
        if (PredictedStopPosition <= DistanceToTarget) then
        begin   {We will stop before PredictedStopPosition}
          PRect.Top := SRect.Top;
          if PredictedStopPosition <= MaxScaleM then
            PRect.Bottom := YPix0M - Round(PredictedStopPosition * PixelPerMeter)
          else
            Prect.Left := Prect.Right; //Invisible!
          if PRect.Bottom > YPix0M then
            PRect.Bottom := YPix0M;
        end
        else
        begin   {We will stop after PredictedStopPosition}
          Brush.Color := FOrangeColor;
          if(DistanceToTarget <= FMaxScaleM) then          //sigdap 990828
            PRect.Top := YPix0M - Round(DistanceToTarget * PixelPerMeter) - 20
          else
            PRect.Top := YPix0M - Round(FMaxScaleM * PixelPerMeter) - 20 ;
          PRect.Bottom := YPix0M - Round(DistanceToTarget * PixelPerMeter);
          if PRect.Bottom < YPix0M - Round(FMaxScaleM * PixelPerMeter) then
            PRect.Bottom := YPix0M - Round(FMaxScaleM * PixelPerMeter);
        end;

        Brush.Style := bsSolid;
        FillRect(PRect);
      end;
   end;

   // Paint figures if no predicted stop pos
   if {Not(ShowPredictedStopPosition) and}
      (ShowDistanceToTarget) then
   begin
      ModifiedDistance := DistanceToTarget;
      sDistance := IntToStr(ModifiedDistance);
      with TheImage.Canvas do
      begin
         Tx := (TheImage.Width div 2) - (TextWidth(sDistance) div 2);
         Ty := ARect.Top + TextHeight(sDistance) div 2;
         //If the predictedStopPos bar is above scale, push figures to the right.
         if PRect.Top < (YPix0M - ScaleLengthPixel) then
           Tx := PRect.Right + 2;
         Font.Size := round(10*Xfactor);
         Font.Color := FForeColor;
         Brush.Color := BackColor;
         TextOut(Tx, Ty, sDistance);
      end;
   end;

end;

{*******************************************
 * Procedure: PaintLogA2  (konedlu 2002-09-17)
 * This procedure paints the A2 area with
 * a logarithmic scale from 0-1000 according
 * to the ETCS Standard.
 *******************************************}
procedure TMMIareaA.PaintLogA2();
var
   ScaleX1, ScaleX2, ScaleY: Integer;
   YPix0M: Integer;
   Y: Integer;
   Tx, Ty: Integer;
   sDistance: String;
   TempDistanceToTarget: Integer;
   ModifiedDistance: Integer;
   FlashBar: Boolean;
   TotalScaleLengthPx: Integer;  //Length of total scale in pixels.
   LinScaleLengthPx: Integer;    //Length of linear part in pixels.
   LogScaleLengthPx: Integer;    //Length of non linear part in pixels.
begin
   FlashBar := false;
   with ARect do
   begin    // This is whole A2  area
      Left := 0;
      Top := TheImage.Width;
      Right := TheImage.Width;
      Bottom := TheImage.Height - (TheImage.Width div 2);
   end;

   YPix0M := ARect.Bottom;
   ScaleX1 := (TheImage.Width div 3) + 4;
   ScaleX2 := ScaleX1 + (ScaleX1 div 2);

   {First 0-100 meters is linear, 100-1000 meters is logarithmical.
    The linear part of the scale will be one fourth of the total length.}
   TotalScaleLengthPx := (ARect.Bottom - ARect.Top) - (2 * TheImage.Canvas.TextHeight('1'));
   LinScaleLengthPx := TotalScaleLengthPx div 4;
   LogScaleLengthPx := TotalScaleLengthPx - LinScaleLengthPx;

   //***************
   //Paint the scale
   //***************
   with TheImage.Canvas do
   begin
      Pen.Color := FForeColor;
      Pen.Style := psSolid;
      Pen.Width := 1;

      //Paint the zero line.
      MoveTo(ScaleX1, YPix0M);
      LineTo(ScaleX2, YPix0M);
      MoveTo(ScaleX1, YPix0M - 1);
      LineTo(ScaleX2, YPix0M - 1);

      //Paint the 100 meters marker.
      MoveTo(ScaleX1+2, YPix0M - LinScaleLengthPx);
      LineTo(ScaleX2-2, YPix0M - LinScaleLengthPx);

      //Paint the logarithmic scale.
      for Y := 2 to 10 do
      begin
        Pen.Color := FForeColor;
        Pen.Width := 1;
        ScaleY := YPix0M - LinScaleLengthPx - Round(LogScaleLengthPx * log10(Y));
        //Thicker ticmarks at 500 and 1000 metres.
        if (y = 5) or (y = 10) then begin
          MoveTo(ScaleX1, ScaleY);
          LineTo(ScaleX2, ScaleY);
          MoveTo(ScaleX1, ScaleY + 1);
          LineTo(ScaleX2, ScaleY + 1);
        end
        else begin
          MoveTo(ScaleX1+2, ScaleY);
          LineTo(ScaleX2-2, ScaleY);
        end;
      end;
   end;

   //***********************************
   //Paint distance-to-brake-target bar.
   //***********************************
   if ShowDistanceToTarget then
   begin
      if DistanceToTarget > 1000 then
      begin
        TempDistanceToTarget := 1000;
        FlashBar := true;
      end
      else
        TempDistanceToTarget := DistanceToTarget;
      with SRect do
      begin
         Left := ScaleX2 + 3;
         Right := Left + (TheImage.Width div 5);
         Bottom := YPix0M;
         if TempDistanceToTarget <= 100 then begin
           Top := YPix0M - Round(TempDistanceToTarget * LinScaleLengthPx / 100);
         end
         else begin
           Top := YPix0M - LinScaleLengthPx - Round(LogScaleLengthPx * log10(TempDistanceToTarget/100));
         end;
      end;
      with TheImage.Canvas do
      begin
         Brush.Color := FForeColor;
         Brush.Style := bsSolid;
         FillRect(SRect);
         if FlashBar then
         begin
           Pen.Color := BackColor;
           Pen.Style := psSolid;
           Pen.Width := 3;
           MoveTo(SRect.Left, SRect.Top+20);
           LineTo(round(SRect.Left+((SRect.Right-SRect.Left)/2)+3), SRect.Top+20);
           LineTo(round(SRect.Left+((SRect.Right-SRect.Left)/2)-3), SRect.Top+25);
           LineTo(SRect.Right, SRect.Top+25);
         end;
      end;
   end;

   //**********************************
   //Paint predicted-stop-position bar.
   //**********************************
   if ShowPredictedStopPosition then
   begin
      with PRect do
      begin
         Left := 9;
         Right := left + (TheImage.Width div 5);
      end;
      with TheImage.Canvas do
      begin
        Brush.Color := FForeColor;
        //If we will stop before PredictedStopPosition.
        if (PredictedStopPosition <= DistanceToTarget) then
        begin
          PRect.Top := SRect.Top;
          if PredictedStopPosition <= 0 then
            PRect.Bottom := YPix0M
          else if PredictedStopPosition <= 100 then
            PRect.Bottom := YPix0M - Round(PredictedStopPosition * LinScaleLengthPx / 100)
          else if PredictedStopPosition > 1000 then begin
            Prect.Left := Prect.Right; //Invisable!
          end
          else begin
            PRect.Bottom := YPix0M - LinScaleLengthPx - Round(LogScaleLengthPx * log10(PredictedStopPosition/100));
          end;
        end
        //Else we will stop after PredictedStopPosition.
        else begin
          Brush.Color := FOrangeColor;
          PRect.Top := SRect.Top - 20;
          PRect.Bottom := SRect.Top;
        end;
        Brush.Style := bsSolid;
        FillRect(PRect);
      end;
   end;

   //******************************
   //Paint figures above the scale.
   //******************************
   if {Not(ShowPredictedStopPosition) and}
      (ShowDistanceToTarget) then
   begin
      ModifiedDistance := DistanceToTarget;
      sDistance := IntToStr(ModifiedDistance);
      with TheImage.Canvas do
      begin
         Tx := (TheImage.Width div 2) - (TextWidth(sDistance) div 2);
         Ty := ARect.Top + TextHeight(sDistance) div 2;
         //If the predictedStopPos bar is above scale, push figures to the right.
         if PRect.Top < (YPix0M - TotalScaleLengthPx) then
           Tx := PRect.Right + 2;
         Font.Size := round(10*Xfactor);
         Font.Color := FForeColor;
         Brush.Color := BackColor;
         TextOut(Tx, Ty, sDistance);
      end;
   end;

end;

procedure TMMIareaA.PaintA3() ;
var
  theMiddle: Integer;  //Y-position of the middle of the graph
  theWidth: Integer;   //Width of the graph.
  TolX: Integer;
begin
   //Paint Predicted speed at target.
   if ShowPredictedSpeedAtTarget then
   begin
      with PRect do begin
         Left := 5;
         Top := ARect.Bottom + 5;
         Right := TheImage.Width - 5;
         Bottom := TheImage.Height -5;
      end;

      //Calculate middle and width of the bar.
      theWidth := PRect.Right - PRect.Left;
      theMiddle :=  PRect.Left + theWidth div 2;

      with P2Rect do begin
         Left := theMiddle - PredictedSpeedAtTarget * theWidth div (PredSpeedMaxScale * 2);
         Top := PRect.Top;
         Right := PRect.Right;// + 1; //So that the last pixel will be drawn!
         Bottom := PRect.Bottom;
         //Check bounds of P2Rect
         if Left < PRect.Left + 2  then Left:= PRect.Left + 2;
         if Left > PRect.Right - 2 then Left:= PRect.Right - 2;
      end;
      with TheImage.Canvas do begin
         Brush.Color:= FForeColor;
         FillRect(PRect); // Outer rect
         Brush.Color:= FOrangeColor;
         FillRect(P2Rect); // Orange rect

         Pen.Color := FBackColor;
         Pen.Width := 1;
         TolX := theMiddle + abs(PredSpeedTolerance * theWidth div (PredSpeedMaxScale * 2));
         MoveTo(TolX, PRect.Top);
         LineTo(TolX, PRect.Bottom);
         if PredSpeedTolerance <> 0 then begin
           TolX := theMiddle - abs(PredSpeedTolerance * theWidth div (PredSpeedMaxScale * 2)) - 1;
           MoveTo(TolX, PRect.Top);
           LineTo(TolX, PRect.Bottom);
         end;
      end;
   end;
end;

procedure TMMIareaA.RefreshArea ;
Begin
 If FUpDateNeeded then Begin
   Refresh ;
   FUpdateNeeded := False;
 End;
End;

procedure TMMIareaA.Setpalette(Value: hPalette);
begin
  if Value <> Fpalette then begin
    Fpalette := Value;
    FUpDateNeeded := True ;
//  Refresh;
  end;
end;

procedure TMMIareaA.SetForeColor(Value: TColor);
begin
  if Value <> FForeColor then begin
    FForeColor := Value;
    FUpDateNeeded := True ;
//  Refresh;
  end;
end;

procedure TMMIareaA.SetBackColor(Value: TColor);
begin
  if Value <> FBackColor then begin
    FBackColor := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetWarningColor(Value: TColor);
begin
  if Value <> FWarningColor then begin
    FWarningColor := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetInterventionColor(Value: TColor);
begin
  if Value <> FInterventionColor then begin
    FInterventionColor := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetOrangeColor(Value: TColor);
begin
  if Value <> FOrangeColor then begin
    FOrangeColor := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetInBCA(Value: Boolean);
begin
  if Value <> FInBCA then begin
    FInBCA := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetSpeeding(Value: Boolean);
begin
  if Value <> FSpeeding then begin
    FSpeeding := Value;
    If FInBCA then
      FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetWarning(Value: Boolean);
begin
  if Value <> FWarning then begin
    FWarning := Value;
    If FInBCA then
      FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetIntervention(Value: Boolean);
begin
  if Value <> FIntervention then begin
    FIntervention := Value;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetShowTimeToIntervention(Value: Boolean);
begin
  if Value <> FShowTimeToIntervention then begin
    FShowTimeToIntervention := Value;
    If FInBCA then
      FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetTimeToIntervention(Value: Integer);
begin
  if Value <> FTimeToIntervention then begin
    FTimeToIntervention := Value;
    If FTimeToIntervention > MAXTIMETOINTERVENTION then
      If FInBCA then
        FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetMaxScaleM(Value: Integer);
begin
  if Value <> FMaxScaleM then begin
    if Value <= 0 then
      Value := 1;
    FMaxScaleM := Value;
    if FMaxScaleM > 100 then
      ModValue := 20
   else
      ModValue := 10;
   Refresh;
  end;
end;

procedure TMMIareaA.SetScaleDivision(Value: Integer);
begin
  if (Value <> FScaleDivision) AND
     (Value <> 0) then
  begin
    FScaleDivision := Value;
    Refresh;
  end;
end;

procedure TMMIareaA.SetScaleTicmark(Value: Integer);
begin
  if (Value <> FScaleTicmark) AND
     (Value <> 0) then
  begin
    FScaleTicmark := Value;
    Refresh;
  end;
end;

procedure TMMIareaA.SetLogScale(Value: Boolean);
begin
  if Value <> FLogScale then begin
    FLogScale := Value;
    Refresh;
  end;
end;

procedure TMMIareaA.SetShowDistanceToTarget(Value: Boolean);
begin
  if Value <> FShowDistanceToTarget then begin
    FShowDistanceToTarget := Value;
    If FInBCA and FShowDistanceToTarget then
      FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetDistanceToTarget(Value: Integer);
begin
  if Value <> FDistanceToTarget then
  begin
    FDistanceToTarget := Value;
    If FInBCA then
      FUpDateNeeded := True ;
//    Refresh;
  end;
end;


procedure TMMIareaA.SetMAMargin(Value: Integer);
begin
  if Value <> FMAMargin then
  begin
    FMAMargin := Value;
     if FInBCA then
     FUpDateNeeded := True;
    //Refresh
  end;
end;

procedure TMMIareaA.SetShowPredictedStopPosition(Value: Boolean);
begin
  if Value <> FShowPredictedStopPosition then
  begin
    FShowPredictedStopPosition := Value;
    If FInBCA and FShowPredictedStopPosition then
      FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetPredSpeedMaxScale(Value: Integer);  //konedlu 2002-09-17
begin
  if (Value <> FPredSpeedMaxScale) and (Value > 0) then begin
    FPredSpeedMaxScale := Value;
    Refresh;
  end;
end;

procedure TMMIareaA.SetPredictedStopPosition(Value: Integer);
begin
  if Value <> FPredictedStopPosition then
  begin
    FPredictedStopPosition := Value;
    If FInBCA then
      FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetShowPredictedSpeedAtTarget(Value: Boolean);
begin
  if Value <> FShowPredictedSpeedAtTarget then
  begin
    FShowPredictedSpeedAtTarget := Value;
    If FInBCA and FShowPredictedSpeedAtTarget then
    begin
      FUpDateNeeded := True ;
    end;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetPredictedSpeedAtTarget(Value: Integer);
begin
  if Value <> FPredictedSpeedAtTarget then
  begin
    FPredictedSpeedAtTarget := Value;
    If FInBCA then
      FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaA.SetPredSpeedTolerance(Value: Integer);
begin
  if Value <> FPredSpeedTolerance then
  begin
    FPredSpeedTolerance := Value;
    If FInBCA then
      Refresh;
  end;
end;



procedure Register;
begin
  RegisterComponents('MMI', [TMMIareaA]);
end;

end.


