(*****************************************************************
           © COPYRIGHT Bombardier Transportation AB, SWEDEN 2011.
           =======================================================

    The copyright to the computer program herein is the
    property of Bombardier Transportation AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the
    written permission from Bombardier Transportation AB, or in accordance
    with the terms and conditions stipulated in the
    agreement/contract under which the program has been
    supplied.


    NAME :  MMIAreaB.pas

    PROJECT :  LKAB, InterFlow TrainBorne

    Ver    Author           Date    Reason
    ---    ------           ------  ------
    1.0.0  Jan Kiessling    970801  First version
    1.1.0  Morgan Persson   971231  Created a reuseable component
           Bo Hermansson    111110  LKAB
                                    Global variable changed to a private var
                                    making it possible to use MMIAreaB on several forms
           Bo Hermansson    121030  property Dimmed


    DESCRIPTION :  Component for the B-area in the ETCS MMI
                   showing the speedometer

    INTERFACE :

*********************************************************)

unit MMIareaB;

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls;

type

  TPermittedDirectionEnum = (MMIUndefined,MMINone,MMIForward, MMIReverse, MMIFree);
  TDirectionArrowStyle = (dasStandard, dasTeniente8, dasLeftRight);

  TMMIareaB = class(TGraphicControl)
  private
    FPalette: hPalette ;
    TheImage: TBitmap;

    MinValue: Longint;

    FMaxValue: Longint;                       // Used to increase resolution of speedometer.
                                              // E.g. MaxValue:=50, MaxScaleValue:=50, ActualSpeed:=50 will display speed as 50.
                                              // E.g. MaxValue:=100, MaxScaleValue:=50, ActualSpeed:=50 will display speed as 25.
    FMaxScaleValue: Longint;                  // Max scale for speedometer. If changed, MaxValue will be equal to new value
    FScaleDivision: Integer;                  // How often will larger division be displayed on scale
    FScaleFigures: Integer;                   // How often will number be printed on scale
    FScaleTicmark: Integer;                   // How often will small division be displayed on scale
    FRadiusFigures: Integer;                  // On which radius will the figures be painted. Percent of total radius (0-100).
    FArrowStyle: TDirectionArrowStyle;
    FActualSpeed: Longint;                    // Speed to be displayed. Must be >= 0 or <= MaxValue
    FPermittedSpeed: Longint;                 // Must be  >= 0 or <= MaxValue.
                                              // If TargetSpeed > PermittedSpeed => TargetSpeed := new value
    FTargetSpeed: Longint;                    // Must be  >= -1 or <= MaxValue or <= PermittedSpeed.
    FInBCA: Boolean;
    FWarning: Boolean;
    FIntervention: Boolean;
    FShowPermittedDrivingDirection: Boolean;
    FIndicatePermittedSpeed: Boolean;
    FIndicateTargetSpeed: Boolean;
    FPermittedDrivingDirection: TPermittedDirectionEnum;
    FFontSize : Integer;                      // The size of the numbers font. Logo will be size/2
    FBorderStyle: TBorderStyle;
    FWarningColor: TColor;
    FBCAColor: TColor;
    FInterventionColor: TColor;
    FTargetColor: TColor;
    FForeColor: TColor;
    FBackColor: TColor;
    FUpDateNeeded : Boolean ;
    FDimmed : Boolean;
    FCurrentOdometerPosition: SmallInt;
    ShowCurrentOdometerPosition : Boolean ;

    procedure Setpalette(Value: hPalette);
    procedure PaintBackground(AnImage: TBitmap);
    procedure PaintScale(AnImage: TBitmap; PaintRect: TRect);
    procedure PaintNeedle(AnImage: TBitmap; PaintRect: TRect);
    procedure PaintLedbar(AnImage: TBitmap; PaintRect: TRect);
    procedure PaintDirectionArrow(AnImage: TBitmap; PaintRect: TRect);
    procedure PaintNut(AnImage: TBitmap; posX, posY : Integer);

    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetWarningColor(Value: TColor);
    procedure SetBCAColor(Value: TColor);
    procedure SetInterventionColor(Value: TColor);
    procedure SetTargetColor(Value: TColor);
    procedure SetForeColor(Value: TColor);
    procedure SetBackColor(Value: TColor);
    procedure SetFontSize(Value: Integer);
    procedure SetMaxValue(Value: Longint);
    procedure SetMaxScaleValue(Value: Longint);
    procedure SetScaleDivision(Value: Integer);
    procedure SetScaleFigures(Value: Integer);
    procedure SetRadiusFigures(Value: Integer);
    procedure SetArrowStyle(Value: TDirectionArrowStyle);
    procedure SetScaleTicmark(Value: Integer);
    procedure SetActualSpeed(Value: Longint);
    procedure SetPermittedSpeed(Value: Longint);
    procedure SetTargetSpeed(Value: Longint);
    procedure SetInBCA(Value: Boolean);
    procedure SetWarning(Value: Boolean);
    procedure SetIntervention(Value: Boolean);
    procedure SetShowPermittedDrivingDirection(Value: Boolean);
    procedure SetIndicatePermittedSpeed(Value: Boolean);
    procedure SetIndicateTargetSpeed(Value: Boolean);
    procedure SetPermittedDrivingDirection(Value: TPermittedDirectionEnum);
    Procedure SetCurrentOdometerPosition(Value: SmallInt);
    function GetVActPercent: Longint;
    function GetVPerPercent: Longint;
    function GetVTarPercent: Longint;
    procedure SetDimmed(Value: Boolean);


  protected
    procedure Paint; override;


  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property VActPercent: Longint read GetVActPercent;
    property VPerPercent: Longint read GetVPerPercent;
    property VTarPercent: Longint read GetVTarPercent;

  published
    procedure RefreshArea;
    property PaletteToUse: hPalette read FPalette write SetPalette ;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property WarningColor: TColor read FWarningColor write SetWarningColor default $020050E0;
    property BCAColor: TColor read FBCAColor write SetBCAColor default clYellow;
    property InterventionColor: TColor read FInterventionColor write SetInterventionColor default clRed;
    property TargetColor: TColor read FTargetColor write SetTargetColor default clGray;
    property ForeColor: TColor read FForeColor write SetForeColor default clSilver;
    property BackColor: TColor read FBackColor write SetBackColor default clBlack;
    property FontSize: Integer read FFontSize write SetFontSize default 20;
    property MaxValue: Longint read FMaxValue write SetMaxValue default 400;
    property MaxScale: Longint read FMaxScaleValue write SetMaxScaleValue default 40;
    property ScaleDivision: Integer read FScaleDivision write SetScaleDivision default 5;
    property ScaleFigures: Integer read FScaleFigures write SetScaleFigures default 10;
    property RadiusFigures: Integer read FRadiusFigures write SetRadiusFigures default 60;
    property ArrowStyle: TDirectionArrowStyle read FArrowStyle write SetArrowStyle default dasStandard;
    property ScaleTicmark: Integer read FScaleTicmark write SetScaleTicmark default 2;
    property ActualSpeed: Longint read FActualSpeed write SetActualSpeed default 0;
    property PermittedSpeed: Longint read FPermittedSpeed write SetPermittedSpeed default 0;
    property TargetSpeed: Longint read FTargetSpeed write SetTargetSpeed  default -1;
    property InBCA: Boolean read FInBCA write SetInBCA default False;
    property Warning: Boolean read FWarning write SetWarning default False;
    property Intervention: Boolean read FIntervention write SetIntervention default False;
    property ShowPermittedDrivingDirection: Boolean read FShowPermittedDrivingDirection write SetShowPermittedDrivingDirection default False;
    property IndicatePermittedSpeed: Boolean read FIndicatePermittedSpeed write SetIndicatePermittedSpeed default True;
    property IndicateTargetSpeed: Boolean read FIndicateTargetSpeed write SetIndicateTargetSpeed default True;
    property PermittedDrivingDirection: TPermittedDirectionEnum read FPermittedDrivingDirection write SetPermittedDrivingDirection default MMIForward;
    property Font ;
    property Dimmed: Boolean read FDimmed write SetDimmed default False;
    property CurrentOdometerPosition: SmallInt
                read FCurrentOdometerPosition
                write SetCurrentOdometerPosition
                default 0 ;

  end;

procedure Register;
implementation

uses MMIStd;

{ This function solves for x in the equation "x is y% of z". }
function SolveForX(Y, Z: Longint): Longint;
begin
  Result := Trunc( Z * (Y * 0.01) );
end;

{ This function solves for y in the equation "x is y% of z". }
function SolveForY(X, Z: Longint): Longint;
begin
  if Z = 0 then Result := 0
  else Result := Trunc( (X * 100.0) / Z );
end;

{ TMMIareaB }

constructor TMMIareaB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  FMaxScaleValue := 40;
  FScaleDivision := 5;
  FScaleFigures  := 10;
  FScaleTicmark  := 1;
  FRadiusFigures := 60;
  FMaxValue := FMaxScaleValue *10;

  FBorderStyle := bsSingle;
  FFontSize := 20;
  Font.Size := 20 ;
  Font.Name := 'Helvetica' ;
  Font.Color := clSilver ;
  Font.Style := [fsBold] ;
  FForeColor := clSilver;

  FWarningColor := $000679C6;
  FBCAColor := clYellow;
  FInterventionColor := clRed;
  FTargetColor := clGray;

  MinValue := 0;

  Width := 285;//300;
  Height := 300;


  FUpDateNeeded := False ;
  FDimmed := False;

  TheImage := TBitmap.Create;


  TheImage.Height := Height;
  TheImage.Width := Width;

  ShowCurrentOdometerPosition := false;

end;

destructor TMMIareaB.Destroy;
begin
  inherited destroy;
  TheImage.Destroy;
end;

procedure TMMIareaB.RefreshArea ;
Begin
 If FUpDateNeeded then Begin
   Refresh ;
   FUpdateNeeded := False;
 End;
End;


function TMMIareaB.GetVActPercent: Longint;
begin
  Result := SolveForY(FActualSpeed - MinValue, FMaxValue - MinValue);
end;

function TMMIareaB.GetVPerPercent: Longint;
begin
  Result := SolveForY(FPermittedSpeed - MinValue, FMaxValue - MinValue);
end;

function TMMIareaB.GetVTarPercent: Longint;
begin
  Result := SolveForY(FTargetSpeed - MinValue, FMaxValue - MinValue);
end;

procedure TMMIareaB.Paint;
var
  PaintRect: TRect;
  ScreenDC : hDC;

begin
  with Canvas do
  begin
    try
      TheImage.Height := Height;
      TheImage.Width := Width;
        TheImage.Canvas.Font := Self.Font ;

      PaintRect := ClientRect;
      if FBorderStyle = bsSingle then InflateRect(PaintRect, -1, -1);

      try
          ScreenDC := TheImage.Canvas.Handle;
          SelectPalette(ScreenDC, Fpalette, False);
          RealizePalette(ScreenDC);
          PaintBackground(TheImage);

        { Paint the Scale on TheImage }
          PaintScale(TheImage, PaintRect);

          if not Dimmed then
          begin
            { Paint the needle on TheImage }
            PaintNeedle(TheImage, PaintRect);

            { Paint the Ledbar on TheImage }
            PaintLedbar(TheImage, PaintRect);

            { Paint the Direction Arrow on TheImage }
            PaintDirectionArrow(TheImage, PaintRect);
          end;

      finally
      end;

      { Draw TheImage on screen }
      Canvas.CopyMode := cmSrcCopy;
      Canvas.Draw(0, 0, TheImage);


    finally

    end;
  end;
end;

procedure TMMIareaB.PaintBackground(AnImage: TBitmap);
var
  ARect: TRect;
begin
  with AnImage.Canvas do
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

procedure TMMIareaB.PaintNut(AnImage: TBitmap; posX, posY : Integer);
var
  nutDim : Integer;
Begin
  nutDim := 6;
  with AnImage.Canvas do
  begin
    Pen.Color := $02202020;
    Pen.Width := 1;
    Arc(posX-nutDim,posY-nutDim,posX+nutDim,posY+nutDim,posX-nutDim,posY+nutDim,posX+nutDim,posY-nutDim);
    Pen.Color := $02000000;
    Arc(posX-nutDim,posY-nutDim,posX+nutDim,posY+nutDim,posX+nutDim,posY-nutDim,posX-nutDim,posY+nutDim);

    Pen.Color := $02202020;
    MoveTo(posX-nutDim,posY+1);
    LineTo(posX+nutDim,posY+1);
    Pen.Color := $02000000;
    MoveTo(posX-nutDim,posY-1);
    LineTo(posX+nutDim,posY-1);

  end;
end;

procedure TMMIareaB.PaintScale(AnImage: TBitmap; PaintRect: TRect);
var
  //X, Y: Integer;
  W, H: Integer;
  XOrigo, YOrigo: Integer;
  XTip1, YTip1: Integer;
  XTip2, YTip2: Integer;
  XMax, YMax: Integer;
  Angle: Double;
  Speed: Integer;
  Radius1, Radius2, Radius3: Integer;
  sValue: String;
  Xtc,Ytc: Integer;
  RadiusTc: Integer;
  //lX1, lX2, lY1, lY2 : Integer;

begin


  with PaintRect do
  begin
//    X := Left;
//    Y := Top;
    W := Right - Left;
    H := Bottom - Top;
    if FBorderStyle = bsSingle then
    begin
      Inc(W);
      Inc(H);
    end;
  end;

  PaintNut(AnImage, W-10, H-10);
  PaintNut(AnImage, W-10, 10);
  PaintNut(AnImage, 10, H-10);
  PaintNut(AnImage, 10, 10);

  XMax := W;
  YMax := H;
  XOrigo := XMax div 2;
  YOrigo := YMax div 2;
  Radius1 := W*9 div 20;
  Radius2 := W*8 div 20;
  Radius3 := W*7 div 20;
  RadiusTc := (W * FRadiusFigures) div 200;  //(Diameter * percent) / (100 * 2) 
  //RadiusTc := W*11 div 40;

  with AnImage.Canvas do
  begin

     Pen.Color := $02000000;
     Pen.Width := 1;
     Arc(2,YMax-2,XMax-2,2,XMax,0,0,YMax);
     Pen.Color := $02202020;
     Arc(2,YMax-2,XMax-2,2,0,YMax,XMax,0);



     Pen.Color := ForeColor;
     Pen.Width := 1;
     Font.Size := FFontSize;

     for Speed:=0 to FMaxScaleValue do
     begin
        Angle := (Pi*3/2) * Speed / FMaxScaleValue - (Pi*3/4);
        XTip1 := XOrigo + Round(Sin(Angle) * Radius1);
        YTip1 := YOrigo - Round(Cos(Angle) * Radius1);
        if ((Speed mod FScaleDivision) = 0) then            //paint scaldivisor
        begin
           XTip2 := XOrigo + Round(Sin(Angle) * Radius3);
           YTip2 := YOrigo - Round(Cos(Angle) * Radius3);
           Pen.Color := ForeColor + $00202020;
        end
        else
        begin                                                 //sigdap 990824
           XTip2 := XOrigo + Round(Sin(Angle) * Radius2);
           YTip2 := YOrigo - Round(Cos(Angle) * Radius2);
           Pen.Color := ForeColor - $00202020;
        end;
        if ((Speed mod FScaleTicmark) = 0) or
           ((Speed mod FScaleDivision) = 0) then          // paint ticmarks
        begin
           MoveTo(XTip1, YTip1);
           LineTo(XTip2, YTip2);
        end;
        if ((Speed mod FScaleFigures) = 0) then          // Paint figures
        begin
           sValue := IntToStr(Speed);
           Xtc := XOrigo + Round(Sin(Angle) * RadiusTc);
           Ytc := YOrigo - Round(Cos(Angle) * RadiusTc);
           TextOut(Xtc-(TextWidth(sValue) div 2),
                   Ytc-(TextHeight(sValue) div 2),
                    sValue);
        end;
     end;
(*
     {Adtranz Loggo }
     Font.Size := FFontSize div 2;
     TextOut((W div 2)- (TextWidth('ADtranz') div 2),H div 3,'ADtranz');
     Pen.Color := $0290C010;
     Brush.Color := Pen.Color;
     lX1 := (W div 2)- (TextWidth('ADtranz') div 4);
     lX2 := lX1 + 10;
     lY1 := (H div 3)-10;
     lY2 := lY1 + 10;
     Ellipse(lX1, lY1, lX2, lY2);
*)


   end;
end;

procedure TMMIareaB.PaintNeedle(AnImage: TBitmap; PaintRect: TRect);
var
  CenterSize: Integer;
  NedleL1, NedleL2, NedleL3: Integer;
  Angle: Double;
//  X, Y: Integer;
  W, H: Integer;
  XOrigo, YOrigo: Integer;
  XOrigoShadow, YOrigoShadow: Integer;
  XTip1, YTip1: Integer;
  XTip2, YTip2: Integer;
  XTip3, YTip3: Integer;
  XTip1S, YTip1S: Integer;
  XTip2S, YTip2S: Integer;
  XTip3S, YTip3S: Integer;

  XMax, YMax: Integer;

begin
  with PaintRect do
  begin
//    X := Left;
//    Y := Top;
    W := Right - Left;
    H := Bottom - Top;
    if FBorderStyle = bsSingle then
    begin
      Inc(W);
      Inc(H);
    end;
  end;

  XMax := W;
  YMax := H;
  Angle := (Pi*3/2) * VActPercent / 100 - (Pi*3/4);

  XOrigo := XMax div 2;
  YOrigo := YMax div 2;
  XOrigoShadow := XOrigo +3;
  YOrigoShadow := YOrigo +3;

  NedleL1 := W * 4 div 10;
  NedleL2 := W * 7 div 20;
  NedleL3 := W * 13 div 40;
  XTip1 := XOrigo + Round(Sin(Angle) * NedleL1);
  YTip1 := YOrigo - Round(Cos(Angle) * NedleL1);
  XTip2 := XOrigo + Round(Sin(Angle) * NedleL2);
  YTip2 := YOrigo - Round(Cos(Angle) * NedleL2);
  XTip3 := XOrigo + Round(Sin(Angle) * NedleL3);
  YTip3 := YOrigo - Round(Cos(Angle) * NedleL3);
  XTip1S := XOrigoShadow + Round(Sin(Angle) * NedleL1);
  YTip1S := YOrigoShadow - Round(Cos(Angle) * NedleL1);
  XTip2S := XOrigoShadow + Round(Sin(Angle) * NedleL2);
  YTip2S := YOrigoShadow - Round(Cos(Angle) * NedleL2);
  XTip3S := XOrigoShadow + Round(Sin(Angle) * NedleL3);
  YTip3S := YOrigoShadow - Round(Cos(Angle) * NedleL3);
  CenterSize := W div 20 + 1;

  with AnImage.Canvas do
  begin

    { Paint nedle shadow}
    Brush.Color := clBlack;
    Pen.Color := clBlack;
    Pen.Width := 1;
    MoveTo(XOrigoShadow, YOrigoShadow);
    LineTo(XTip1S, YTip1S);
    Pen.Width := 3;
    MoveTo(XOrigoShadow, YOrigoShadow);
    LineTo(XTip2S, YTip2S);
    Pen.Width := 7;
    MoveTo(XOrigoShadow, YOrigoShadow);
    LineTo(XTip3S, YTip3S);
    { Paint centerdot shadow}
    Pie(XOrigoShadow-CenterSize,YOrigoShadow-CenterSize, XOrigoShadow+CenterSize,YOrigoShadow+CenterSize,0,0,0,0);


     if InBCA then
     begin
        Brush.Color := BCAColor;
        Pen.Color := BCAColor;
     end
     else
     begin
        Brush.Color := ForeColor;
        Pen.Color := ForeColor;
     end;

     if Warning then
     begin
        Brush.Color := FWarningColor;
        Pen.Color := FWarningColor;
     end;

     if Intervention then
     begin
        Brush.Color := InterventionColor;
        Pen.Color := InterventionColor;
     end;

    { Paint nedle }
    Pen.Width := 1;
    MoveTo(XOrigo, YOrigo);
    LineTo(XTip1, YTip1);
    Pen.Width := 3;
    MoveTo(XOrigo, YOrigo);
    LineTo(XTip2, YTip2);
    Pen.Width := 7;
    MoveTo(XOrigo, YOrigo);
    LineTo(XTip3, YTip3);



    { Paint centerdot }
    Pie(XOrigo-CenterSize,YOrigo-CenterSize, XOrigo+CenterSize,YOrigo+CenterSize,0,0,0,0);

//    PaintNut(AnImage, XOrigo, YOrigo);
    Brush.Color := clBlack;
    Pen.Color := clBlack;
    Pen.Width := 2;
    MoveTo(XOrigo, YOrigo);
    LineTo(XOrigo, YOrigo);


//    Pie(XOrigo-1,YOrigo-1,XOrigo+1,YOrigo+1,0,0,0,0);
   end;
end;

procedure TMMIareaB.PaintLedbar(AnImage: TBitmap; PaintRect: TRect);
var
  PerAngle, TarAngle, ZeroAngle: Double;
  HookDeltaAngle: Double;

//  X, Y: Integer;
  W, H: Integer; // Area limits
  XMax, YMax: Integer;

  XOrigo, YOrigo: Integer; // Center of area

  XPer, YPer: Integer;
  XTar, YTar: Integer;
  XZero, YZero: Integer;
  XHook, YHook: Integer;

  BorderSpace: Integer;    // Center ledbar to border of area
  BarThicknes: Integer;

  NormColor: TColor;
//  TargColor: TColor;
//  IconToDraw : TBitMap;

begin
  with PaintRect do
  begin
//    X := Left;
//    Y := Top;
    W := Right - Left;
    H := Bottom - Top;
    if FBorderStyle = bsSingle then
    begin
      Inc(W);
      Inc(H);
    end;
  end;

  XMax := W;
  YMax := H;
  XOrigo := XMax div 2;
  YOrigo := YMax div 2;
  BorderSpace := W div 40;
  BarThicknes := 6;

  PerAngle := (Pi*3/2) * VPerPercent / 100 - (Pi*3/4); // Permitted speed
  TarAngle := (Pi*3/2) * VTarPercent / 100 - (Pi*3/4); // Target speed
  ZeroAngle := -((Pi*3/4)+Pi/40);                      // just below zero
  HookDeltaAngle := Pi / 360;                          //  1/2 degree


  XPer := XOrigo + Round(Sin(PerAngle) * W);
  YPer := YOrigo - Round(Cos(PerAngle) * W);
  XTar := XOrigo + Round(Sin(TarAngle) * W);
  YTar := YOrigo - Round(Cos(TarAngle) * W);
  XZero := XOrigo + Round(Sin(ZeroAngle) * W);
  YZero := YOrigo - Round(Cos(ZeroAngle) * W);
  XHook := XOrigo + Round(Sin(PerAngle-HookDeltaAngle) * W);
  YHook := YOrigo - Round(Cos(PerAngle-HookDeltaAngle) * W);

  if InBCA then
  begin
     NormColor := BCAColor;
//     TargColor := TargetColor;
  end
  else
  begin
     NormColor := ForeColor;
//     TargColor := TargetColor;
  end;
  if Warning then
  begin
     NormColor := FWarningColor;
  end;
  if Intervention then
  begin
     NormColor := FInterventionColor;
  end;

  with AnImage.Canvas do
  begin
    if (FTargetSpeed >= 0) and
       (IndicateTargetSpeed) then  {We have a target speed}
    begin
//       if (FPermittedSpeed > FTargetSpeed) and
       if (VPerPercent > VTarPercent) and
          (IndicatePermittedSpeed) then  {We have a permitted speed}
       begin
          Pen.Width := BarThicknes;
          Pen.Color := NormColor;
          Arc(BorderSpace,BorderSpace, W-BorderSpace,H-BorderSpace,
              XPer,YPer, XTar,YTar);
          Pen.Color := TargetColor;
          Arc(BorderSpace,BorderSpace, W-BorderSpace,H-BorderSpace,
              XTar,YTar, XZero,YZero);
       end
       else begin
          Pen.Width := BarThicknes;
          Pen.Color := TargetColor;
          Arc(BorderSpace,BorderSpace, W-BorderSpace,H-BorderSpace,
              XTar,YTar, XZero,YZero);
      end;
    end
    else if (VPerPercent > 0) and
         (IndicatePermittedSpeed) then         {No target speed but permitted speed}
      begin
        Pen.Color := NormColor;
        Pen.Width := BarThicknes;

        Arc(BorderSpace,BorderSpace,W-BorderSpace,H-BorderSpace,
            XPer,YPer, XZero,YZero);
      end
    else
      begin
      end;

    // Paint the Hook
    if IndicatePermittedSpeed then
    begin
      Pen.Color := NormColor;
      Pen.Width := BarThicknes * 2;

      Arc(BorderSpace+BarThicknes div 2,BorderSpace+BarThicknes div 2,
          W-BorderSpace-BarThicknes div 2,H-BorderSpace-BarThicknes div 2,
          XPer,YPer, XHook,YHook);
    end
    else
    begin
    end;
  end;


end;


procedure TMMIareaB.PaintDirectionArrow(AnImage: TBitmap; PaintRect: TRect);
var
  W, H: Integer;
  OrgFontSize : Integer;
  IconToDraw : TBitMap;
  ScaledRect : TRect;

begin
  IconToDraw := Nil ;
  with PaintRect do
  begin
    W := Right - Left;
    H := Bottom - Top;
    if FBorderStyle = bsSingle then
    begin
      Inc(W);
      Inc(H);
    end;
  end;

  try
    IconToDraw := TBitMap.Create;
    if ArrowStyle = dasStandard then begin
      case FPermittedDrivingDirection of
        MMIForward:
          IconToDraw.LoadFromResourceName(HInstance,'FORWARD');
        MMIReverse:
          IconToDraw.LoadFromResourceName(HInstance,'REVERSE');
        MMIFree:
          IconToDraw.LoadFromResourceName(HInstance,'FREE');
        MMINone:
          IconToDraw.LoadFromResourceName(HInstance,'NONE');
        else
          IconToDraw.LoadFromResourceName(HInstance,'NONE');
      end;
    end
    else if ArrowStyle = dasTeniente8 then begin
      case FPermittedDrivingDirection of
        MMIForward:
          IconToDraw.LoadFromResourceName(HInstance,'MINA');
        MMIReverse:
          IconToDraw.LoadFromResourceName(HInstance,'COLON');
        MMIFree:
          IconToDraw.LoadFromResourceName(HInstance,'FREE');
        MMINone:
          IconToDraw.LoadFromResourceName(HInstance,'NONE');
        else
          IconToDraw.LoadFromResourceName(HInstance,'NONE');
      end;
    end
    else if ArrowStyle = dasLeftRight then begin
      case FPermittedDrivingDirection of
        MMIForward:
          IconToDraw.LoadFromResourceName(HInstance,'RIGHT');
        MMIReverse:
          IconToDraw.LoadFromResourceName(HInstance,'LEFT');
        MMIFree:
          IconToDraw.LoadFromResourceName(HInstance,'FREE');
        MMINone:
          IconToDraw.LoadFromResourceName(HInstance,'NONE');
        else
          IconToDraw.LoadFromResourceName(HInstance,'NONE');
      end;
    end;

    IconToDraw.Transparent := true;

    { Scale icon for PermittedDrivingDirection}
    with ScaledRect do
    begin
      Left:= (W div 2)-(round(32*GetXfactor) div 2);
      Top:= H - (H div 3);
      Right:= Left + round(32*GetXfactor);
      Bottom:= Top + round(32*GetYfactor);
    end;

    { Print odometer }
    if ShowCurrentOdometerPosition then
    begin
      OrgFontSize := AnImage.Canvas.Font.Size;
      AnImage.Canvas.Font.Size := OrgFontSize div 2;
      AnImage.Canvas.TextOut(ScaledRect.Left,ScaledRect.Bottom + 2, IntToStr(CurrentOdoMeterPosition));
      AnImage.Canvas.Font.Size := OrgFontSize;
    end;


    with AnImage.Canvas do
    begin
      { PermittedDrivingDirection arrow }
      StretchDraw(ScaledRect, IconToDraw);
      // Paint the 'bsLowered style' frame
      Pen.Color := $02202020;
      Pen.Width := 1;
      MoveTo(ScaledRect.Left,ScaledRect.Bottom);
      LineTo(ScaledRect.Right,ScaledRect.Bottom);
      LineTo(ScaledRect.Right,ScaledRect.Top);
      Pen.Color := $02000000;
      LineTo(ScaledRect.Left,ScaledRect.Top);
      LineTo(ScaledRect.Left,ScaledRect.Bottom);
    end;
  finally
    IconToDraw.Free;
  end;
end;

procedure TMMIareaB.SetInBCA(Value: Boolean);
begin
  if Value <> FInBCA then
  begin
    FInBCA := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetWarning(Value: Boolean);
begin
  if Value <> FWarning then
  begin
    FWarning := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetIntervention(Value: Boolean);
begin
  if Value <> FIntervention then
  begin
    FIntervention := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetShowPermittedDrivingDirection(Value: Boolean);
begin
  if Value <> FShowPermittedDrivingDirection then
  begin
    FShowPermittedDrivingDirection := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetIndicatePermittedSpeed(Value: Boolean);
begin
  if Value <> FIndicatePermittedSpeed then
  begin
    FIndicatePermittedSpeed := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetIndicateTargetSpeed(Value: Boolean);
begin
  if Value <> FIndicateTargetSpeed then
  begin
    FIndicateTargetSpeed := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetPermittedDrivingDirection(Value: TPermittedDirectionEnum);
begin
  if Value <> FPermittedDrivingDirection then
  begin
    FPermittedDrivingDirection := Value;
    FUpDateNeeded := True ;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    Refresh;
  end;
end;

procedure TMMIareaB.SetWarningColor(Value: TColor);
begin
  if Value <> FWarningColor then
  begin
    FWarningColor := Value;
    Refresh;
  end;
end;

procedure TMMIareaB.SetBCAColor(Value: TColor);
begin
  if Value <> FBCAColor then
  begin
    FBCAColor := Value;
    Refresh;
  end;
end;

procedure TMMIareaB.SetInterventionColor(Value: TColor);
begin
  if Value <> FInterventionColor then
  begin
    FInterventionColor := Value;
    Refresh;
  end;
end;

procedure TMMIareaB.SetTargetColor(Value: TColor);
begin
  if Value <> FTargetColor then
  begin
    FTargetColor := Value;
    Refresh;
  end;
end;


procedure TMMIareaB.Setpalette(Value: hPalette);
begin
  if Value <> Fpalette then begin
    Fpalette := Value;
    FUpDateNeeded := True ;
//  Refresh;
  end;
end;

procedure TMMIareaB.SetForeColor(Value: TColor);
begin
  if Value <> FForeColor then
  begin
    FForeColor := Value;
    Refresh;
  end;
end;

procedure TMMIareaB.SetBackColor(Value: TColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;
    Refresh;
  end;
end;

procedure TMMIareaB.SetFontSize(Value: Integer);
begin
  if Value <> FFontSize then
  begin
    FFontSize := Value;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetMaxValue(Value: Longint);
begin
  if Value <> FMaxValue then
  begin
 (*
    if Value < MinValue then
      raise EInvalidOperation.CreateResFmt(SOutOfRange, [MinValue + 1, MaxInt]);
 *)
    FMaxValue := Value;
    if FActualSpeed > Value then FActualSpeed := Value;
    if FPermittedSpeed > Value then FPermittedSpeed := Value;
    if FTargetSpeed > Value then FTargetSpeed := Value;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetMaxScaleValue(Value: Longint);
begin
  if (Value <> FMaxScaleValue) AND
     (Value <> 0) then
  begin
 (*
    if Value < MinValue then
      raise EInvalidOperation.CreateResFmt(SOutOfRange, [MinValue + 1, MaxInt]);
 *)
    FMaxScaleValue := Value;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetScaleDivision(Value: Integer);
begin
  if (Value <> FScaleDivision) AND
     (Value <> 0) then
  begin
    FScaleDivision := Value;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetScaleTicmark(Value: Integer);
begin
  if (Value <> FScaleTicmark) AND
     (Value <> 0) then
  begin
    FScaleTicmark := Value;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetScaleFigures(Value: Integer);
begin
  if (Value <> FScaleFigures) AND
     (Value <> 0) then
  begin
    FScaleFigures := Value;
//    Refresh;
  end;
end;

procedure TMMIareaB.SetRadiusFigures(Value: Integer);
begin
  if Value > 100 then
    Value := 100
  else if value < 0 then
    Value := 0;
  FRadiusFigures := Value;
end;

procedure TMMIareaB.SetArrowStyle(Value: TDirectionArrowStyle);
begin
  FArrowStyle := Value;
end;

procedure TMMIareaB.SetActualSpeed(Value: Longint);
begin
  if Value < MinValue then
    Value := MinValue
  else if Value > FMaxValue then
    Value := FMaxValue;
  if FActualSpeed <> Value then
  begin
    FActualSpeed := Value;
      FUpDateNeeded := True ;
//      Refresh;
  end;
end;

procedure TMMIareaB.SetPermittedSpeed(Value: Longint);
var
  TempPercent: Longint;
begin
  TempPercent := GetVPerPercent; { remember where we were }
  if Value < MinValue then
    Value := MinValue
  else if Value > FMaxValue then
    Value := FMaxValue;
  if FPermittedSpeed <> Value then
  begin
    FPermittedSpeed := Value;

    if GetVTarPercent > GetVPerPercent then
      SetTargetSpeed(Value);

    if TempPercent <> GetVPerPercent then { only refresh if percentage changed }
    begin
      FUpDateNeeded := True;
      // Refresh;
    end;
  end;
end;

procedure TMMIareaB.SetTargetSpeed(Value: Longint);
var
  TempPercent: Longint;
begin
  TempPercent := GetVTarPercent;  { remember where we were }
  if Value < -1 then
    Value := -1
  else if Value > FMaxValue then
    Value := FMaxValue
  else if Value > FPermittedSpeed then
    Value := FPermittedSpeed;

  if FTargetSpeed <> Value then
  begin
    FTargetSpeed := Value;
    if TempPercent <> GetVTarPercent then { only refresh if percentage changed }
    begin
      FUpDateNeeded := True ;
//      Refresh;
    end;
  end;
end;

Procedure TMMIareaB.SetDimmed(Value: Boolean);
Begin
  if Value <> FDimmed Then Begin
    FDimmed := Value;
    Refresh;
  End;
End;

Procedure TMMIareaB.SetCurrentOdometerPosition(Value: SmallInt);
begin
  ShowCurrentOdometerPosition := false;
  FCurrentOdometerPosition := Value;
end;

procedure Register;
begin
  RegisterComponents('MMI', [TMMIareaB]);
end;

end.
