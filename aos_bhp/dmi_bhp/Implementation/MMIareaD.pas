(*****************************************************
           © COPYRIGHT Adtranz Signal AB, SWEDEN 1998.
           ===========================================

    The copyright to the computer program herein is the
    property of Adtranz AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the
    written permission from Adtranz AB, or in accordance
    with the terms and conditions stipulated in the
    agreement/contract under which the program has been
    supplied.


    NAME :  MMIAreaD.pas

    PROJECT :  Esmeralda, InterFlow TrainBorne

    Ver    Author           Date    Reason
    ---    ------           ------  ------
    1.0.0  Jan Kiessling    970801  First version
    1.1.0  Morgan Persson   980101  Created a reuseable component
           SIGJKG           020422  In proc PaintD9, Max trainlength set to 150
           Edward Lundin    020830  Added property Rescaleable.
           Edward Lundin    030924  Changes in PositionStaticSpeedList to avoid
                                    that speeds > 0 is positioned on the zero
                                    level in the diagram.
           Bo Hermansson    110210  Use already existing bitmaps instead of creating
                                    new "on the fly". See procedure TMMIareaD.PaintD234
                                    in order to avoid "Out of system resources".
                                    First seen after upgrade to Delphi 6
           Bo Hermansson    121114  Corrected a minor error in AddGradientToList

    DESCRIPTION :  Component for the D-area in the ETCS MMI
                   showing the Planning Area

    INTERFACE :

    The application shall for every new MA received from the ATP:
    1. Set the CurrentOdometerPosition
    2. Clear previous lists, by calling procedure EraseAllLists
    3. Set data to be presented on the planning area
       - AddAnnouncementsToList
       - AddGradientToList
       - AddSpeedTargetToList
       - AddStaticSpeedToList
       - AddBCAStartToList

    The application shall also call the procedure OneSecTic every second.
    Text for second layer, train length shall be set


*********************************************************)

unit MMIareaD;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TSpeedTypeEnum = (SpeedSame, SpeedZero, SpeedDecrease, SpeedIncrease);

  PAnnouncementsItem = ^TAnnouncementsItem;

  TAnnouncementsItem = record
    Icon: TBitmap;
    Icon_dimm: TBitmap;
    Text: String;
    Position: Integer;
    Used: Integer;
  End;

  PGradientItem = ^TGradientItem;

  TGradientItem = record
    Gradient: Integer;
    StartPosition: LongInt;
    EndPosition: LongInt;
  End;

  PSpeedTargetItem = ^TSpeedTargetItem;

  TSpeedTargetItem = record
    SpeedType: TSpeedTypeEnum;
    StartPosition: LongInt;
  End;

  PCeilingSpeedItem = ^TCeilingSpeedItem;

  TCeilingSpeedItem = record
    Speed: Integer;
    Fraction: Byte;
    StartPosition: LongInt;
    EndPosition: LongInt;
  End;

  PBCAStartItem = ^TBCAStartItem;

  TBCAStartItem = record
    StartPosition: LongInt;
  End;

type
  TMMIareaD = class(TGraphicControl)
  private
    { Private declarations }

    AnnouncementsList: TList;
    GradientList: TList;
    SpeedTargetList: TList;
    CeilingSpeedList: TList;
    BCAStartList: TList;
//    CarStatusList: TList;

    Layer2AnnouncementsItem: PAnnouncementsItem;
    Layer2Time : Integer;
    Layer2TrainLength: Boolean;

    PixPer100m: Real;
    YPix0m: Integer;
    LastCollumn: Integer;            // 1,2 or 3, area D2/3/4
    D1W,D2W,D3W,D4W,D5W,D6W,D7W,D8W: Integer;

    {properties}
    FPalette: hPalette ;
    FMaxDistance: Integer;
    FMaxScaleDistance:Integer;
    FDirection :integer ;
    FCurrentOdometerPosition: LongInt;
    FDimmed: Boolean;
    FShowBCA: Boolean;
    FTrainLength: Integer;
    FSecondLayerTime : Integer;
    FTrainLengthSecondLayerText: String;
    FRescaleable: Boolean; //Konedlu 2002-08-30

    FBorderStyle: TBorderStyle;
    FForeColor: TColor;
    FBackColor: TColor;
    FGradPlusColor: TColor;
    FGradMinusColor: TColor;
    FSpeedProfBackColor: TColor;
    FSpeedProfForeColor: TColor;
    FBCAStartColor: TColor;


    dcol1: TColor;
    dcol2: TColor;
    dcol3: TColor;
    dcol4: TColor;
    dcol5: TColor;
    dcol6: TColor;

    procedure Setpalette(Value: hPalette);
    Procedure PaintBackground(AnImage: TBitmap);
    Procedure PaintScale(TheImage: TBitmap);
    Procedure PaintD234(TheImage: TBitmap; ARect: TRect);
    Procedure PaintGradient(TheImage: TBitmap; Gradient: Integer;
                            GStart,GEnd: Integer);
    Procedure PaintD5(TheImage: TBitmap; ARect: TRect);
    Procedure PaintD6(TheImage: TBitmap; ARect: TRect);
    Procedure PaintSpeedProfile(TheImage: TBitmap; SStart,SEnd: Integer;
                                Fraction: Integer);
    Procedure PaintD7(TheImage: TBitmap; ARect: TRect);
    Procedure PaintD8(TheImage: TBitmap; ARect: TRect);
    Procedure PaintD9(TheImage: TBitmap; ARect: TRect);

    Procedure SetSecondLayerTime(Value: Integer);
    Procedure SetBorderStyle(Value: TBorderStyle);
    Procedure SetForeColor(Value: TColor);
    Procedure SetBackColor(Value: TColor);
    Procedure SetGradPlusColor(Value: TColor);
    Procedure SetGradMinusColor(Value: TColor);
    Procedure SetSpeedProfBackColor(Value: TColor);
    Procedure SetSpeedProfForeColor(Value: TColor);
    Procedure SetBCAStartColor(Value: TColor);
    Procedure SetTrainLength(Value: Integer);
    Procedure SetTrainLengthSecondLayerText(Value: string);

    Procedure SetDimmed(Value: Boolean);
    Procedure SetShowBCA(Value: Boolean);
    Procedure PositionCeilingSpeedList;
    Procedure SetCurrentOdometerPosition(Value: LongInt);
    Procedure SetDirection (Value : integer) ;

    Procedure SetRescaleable(Value: Boolean);

  protected
    { Protected declarations }

    Procedure Paint; override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;

    Procedure ShowSecondLayer(TheCanvas: TCanvas; X,Y: Integer; Text: String);
    Procedure ShowAnnouncementsSecondLayer(TheCanvas: TCanvas;
                                           PItem: PAnnouncementsItem);
    Procedure ShowTrainLengthSecondLayer(TheCanvas: TCanvas);
    Procedure SetMaxDistance(Value: Integer);
    Procedure SetMaxScaleDistance(Value: Integer);
    function  calcFraction(speed, prevSpeed: Integer; prevFraction: Byte): Byte;


public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure AddAnnouncementsToList(Icon: TBitmap;
                                     Text: String;
                                     Position: LongInt;
                                     CurrentOdometerPosition: LongInt);
    Procedure AddGradientToList(Gradient: ShortInt;
                                StartPosition: LongInt;
                                EndPosition: LongInt;
                                CurrentOdometerPosition: LongInt;
                                NoOFGradTarg:Integer);
    Procedure AddSpeedTargetToList ;
    Procedure AddCeilingSpeedToList(Speed: Integer;
                                   StartPosition: LongInt;
                                   EndPosition: LongInt;
                                   CurrentOdometerPosition: LongInt);
    Procedure AddBCAStartToList(StartPosition: LongInt;
                                CurrentOdometerPosition: LongInt);

    Procedure OneSecTic;
    Procedure EraseAllLists;


  published
    { Published declarations }

    property direction: integer
                Read FDirection
                write SetDirection
                Default 1;

    property CurrentOdometerPosition: LongInt
               read FCurrentOdometerPosition
               write SetCurrentOdometerPosition
               Default 0 ;

    property SecondLayerTime: Integer
               read FSecondLayerTime
               write SetSecondLayerTime
               Default 3 ;

    property BorderStyle: TBorderStyle
               read FBorderStyle
               write SetBorderStyle
               default bsSingle;

    property ForeColor: TColor
               read FForeColor
               write SetForeColor
               default clSilver;

    property BackColor: TColor
               read FBackColor
               write SetBackColor
               default clBlack;

    property GradPlusColor: TColor
               read FGradPlusColor
               write SetGradPlusColor
               default clLime;

    property GradMinusColor: TColor
               read FGradMinusColor
               write SetGradMinusColor
               default clGreen;

    property SpeedProfBackColor: TColor
               read FSpeedProfBackColor
               write SetSpeedProfBackColor
               default clBlue;

    property SpeedProfForeColor: TColor
               read FSpeedProfForeColor
               write SetSpeedProfForeColor
               default clNavy;

    property BCAStartColor: TColor
               read FBCAStartColor
               write SetBCAStartColor
               default clYellow;


    property Dimmed: Boolean
               read FDimmed
               write SetDimmed
               default False;

    property ShowBCA: Boolean
               read FShowBCA
               write SetShowBCA
               default False;

    property TrainLength: Integer
               read FTrainLength
               write SetTrainLength
               default 120;

    property MaxDistance: Integer
               read FMaxDistance
               write SetMaxDistance
               default 500;

    property MaxScaleDistance: Integer
               read FMaxScaleDistance
               write SetMaxScaleDistance
               default 3000;

    property PaletteToUse: hPalette
                read FPalette
                write SetPalette ;

    property TrainLengthSecondLayerText: String
               read FTrainLengthSecondLayerText
               write SetTrainLengthSecondLayerText ;

    property Rescaleable: Boolean
             read FRescaleable
             write SetRescaleable
             default false;
End;

Procedure Register;


implementation

uses MMIStd, MMITypes, UnitTrainComposition, UnitDMIDataModule;
//  MainArea;
constructor TMMIareaD.Create(AOwner: TComponent);
var
   PItem: PBCAStartItem;
Begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  { default values }
  FBorderStyle := bsSingle;
  FForeColor := clSilver;
{  FBackColor := clBlack;}
  FGradPlusColor:= clGreen;
  FGradMinusColor:= clOlive;
  FSpeedProfBackColor:= clBlue;
  FSpeedProfForeColor:= clNavy;
  FBCAStartColor:= clYellow;

  FTrainLengthSecondLayerText := 'No information';

  FCurrentOdometerPosition := 0;
  FSecondLayerTime := 3;
  FTrainLength := 80;
  FShowBCA := true;
  Layer2TrainLength := False;
  FDirection := 1;
  FRescaleable := false;

  Width := 248;//250;
  Height := 300;
  FMaxDistance := 500;

  LastCollumn := 1;

  dcol1 := $02484848;
  dcol2 := $02404040;
  dcol3 := $02383838;
  dcol4 := $02303030;
  dcol5 := $02282828;
  dcol6 := $02202020;


  Try
    AnnouncementsList := TList.Create;
    GradientList := TList.Create;
    SpeedTargetList := TList.Create;
    CeilingSpeedList := TList.Create;
    BCAStartList := TList.Create;

    Layer2AnnouncementsItem := nil;
    New(PItem);
    BCAStartList.Add(PItem) ;
  Except
  End;

End;

destructor TMMIareaD.Destroy;
Var
I: integer ;
Begin
  inherited destroy;

  For I:= 0 to AnnouncementsList.Count-1 do
    Dispose(AnnouncementsList.Items[I]);
  AnnouncementsList.Clear ;
  AnnouncementsList.Free ;

  For I:= 0 to GradientList.Count-1 do
    Dispose(GradientList.Items[I]);
  GradientList.Clear ;
  GradientList.Free;

  For I:= 0 to SpeedTargetList.Count-1 do
    Dispose(SpeedTargetList.Items[I]);
  SpeedTargetList.Clear ;
  SpeedTargetList.Free ;

  For I:= 0 to CeilingSpeedList.Count-1 do
    Dispose(CeilingSpeedList.Items[I]);
  CeilingSpeedList.Clear ;
  CeilingSpeedList.Free;

  For I:= 0 to BCAStartList.Count-1 do
    Dispose(BCAStartList.Items[I]);  //,Sizeof(PBCAStartItem)) ;
  BCAStartList.Clear ;
  BCAStartList.Free ;


End;


Procedure TMMIareaD.Paint;
var
  TheImage: TBitmap;
  PaintRect: TRect;
  ARect: TRect;
  ScreenDC : hDC;
  AdditionalAllowedToInfo : Byte;
  AdditionalAllowedToInfoChanged : Boolean;

Begin

  Canvas.Font.Size := round(8*GetXfactor);
  D1W := Canvas.TextWidth(IntToStr(FMaxDistance)) + 10;
  D2W:= round(32*GetXfactor);
  D3W:= D2W;
  D4W:= D2W;
  D5W:= round(16*GetXfactor);
  D6W:= round(16*GetXfactor);
  D8W:= round(10*GetXfactor);
  D7W:= Width - (D1W+D2W+D3W+D4W+D5W+D6W+D8W);
  TheImage := Nil ;
  with Canvas do Begin
    try
      TheImage := TBitmap.Create;
      TheImage.Height := Height;
      TheImage.Width := Width;
      TheImage.Canvas.Font.Size := round(8*GetXfactor);


      PaintRect := ClientRect;

      //      PixPer100m is now float to prevent of accuracy for large distance scale
      PixPer100m := ((Height - (2 * TextHeight('1')))* 500.0) / (FMaxDistance *6.0) ;

      YPix0m := round(TextHeight('1') + (PixPer100m * FMaxDistance / 100.0));

      If FBorderStyle = bsSingle Then InflateRect(PaintRect, -1, -1);


      try


        ScreenDC := TheImage.Canvas.Handle;
        SelectPalette(ScreenDC, Fpalette, False);
        RealizePalette(ScreenDC);

        If Dimmed Then Begin
          ForeColor := dcol1;
          FGradPlusColor:= dcol2;
          FGradMinusColor:= dcol3;
          FSpeedProfBackColor:= dcol4;
          FSpeedProfForeColor:= dcol5;
        End
        Else Begin
          ForeColor := clSilver;
          FGradPlusColor:= clGreen;
          FGradMinusColor:= clOlive;
          FSpeedProfBackColor:= clBlue;
          FSpeedProfForeColor:= clNavy;
        End;


        PaintBackground(TheImage);
//        If Dimmed Then TheImage.Mask(clSilver);
//        If Not Dimmed then
          PaintD234(TheImage, PaintRect);  //area D2/3/4, Orders/Announcements
        PaintD6(TheImage, PaintRect);    //area D6, Speed targets
        PaintD7(TheImage, PaintRect);    //area D7, Static speed profile
        PaintD8(TheImage, PaintRect);    //area D8, Brake Curve area start
        PaintScale(TheImage);            //area D1
        PaintD5(TheImage, ARect);        //area D5, Gradient
        PaintD9(TheImage, PaintRect);    //area D9

      Finally
      End;
      { Draw TheImage on screen }


      Canvas.CopyMode := cmSrcCopy;
      Canvas.Draw(0, 0, TheImage);
      Canvas.CopyMode := cmSrcCopy;


 //     UpdateColors(ScreenDC);


      { Paint second layer info if any }
      If Assigned(Layer2AnnouncementsItem) and  not Dimmed Then Begin
        If Layer2AnnouncementsItem^.Position = 0 Then
          Layer2AnnouncementsItem := nil
        else
          ShowAnnouncementsSecondLayer(Canvas, Layer2AnnouncementsItem);

      End;
      If Layer2TrainLength and not Dimmed  Then
      begin
      AdditionalAllowedToInfo :=  DataModuleDMI.GetAdditionalAllowedToInfo(AdditionalAllowedToInfoChanged);
      if bytebool(AdditionalAllowedToInfo and ALLOWED_TO_SHOW_TRAIN_COMP) then
      begin
        FormTrainComposition.Show;
          { Clear flag that driver clicked on train-extent }
        Layer2TrainLength := false;
      end
       else
       FormTrainComposition.Hide;
      end;

    Finally
      TheImage.Destroy;
    End;
  End;
End;

Procedure TMMIareaD.PaintBackground(AnImage: TBitmap);
var
  ARect: TRect;
Begin
  with AnImage.Canvas do Begin
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

  End;
End;

{Area D1, distance scale}
Procedure TMMIareaD.PaintScale(TheImage: TBitmap);
var
//  ARect: TRect;
  Tx, Ty: Integer;
  HalfText,FullText: Integer;
  RightMargin: Integer;

Begin
  with TheImage.Canvas do Begin
    try
      Tx := 5;
      FullText := TextHeight('1');
      HalfText:= FullText div 2;
      RightMargin := Width -2;

      Brush.Color := BackColor;
      Font.Color := ForeColor;
      Pen.Color := ForeColor;
      Pen.Width := 1;
      Pen.Style := psSolid;

      TextOut(Tx, FullText-HalfText, IntToStr(FMaxDistance));
      MoveTo(D1W, FullText);
      LineTo(RightMargin,FullText);

      Ty := FullText+round(PixPer100m * FMaxDistance / 500) ;
      TextOut(Tx, Ty-HalfText, IntToStr(FMaxDistance*4 div 5));
      MoveTo(D1W, Ty);
      LineTo(RightMargin,Ty);

      Ty := FullText+round(PixPer100m * FMaxDistance*2 / 500) ;
      TextOut(Tx, Ty-HalfText, IntToStr(FMaxDistance*3 div 5));
      MoveTo(D1W, Ty);
      LineTo(RightMargin,Ty);

      Ty := FullText+round(PixPer100m * FMaxDistance*3 / 500);
      TextOut(Tx, Ty-HalfText, IntToStr(FMaxDistance*2 div 5));
      MoveTo(D1W, Ty);
      LineTo(RightMargin,Ty);

      Ty := FullText+round(PixPer100m * FMaxDistance *4 / 500) ;
      TextOut(Tx, Ty-HalfText, IntToStr(FMaxDistance div 5));
      MoveTo(D1W, Ty);
      LineTo(RightMargin,Ty);

      Ty := FullText+round(PixPer100m * FMaxDistance *5 / 500) ;
      TextOut(Tx, Ty-HalfText, '  0');
      MoveTo(D1W, Ty);
      Pen.Width := 2;
      LineTo(RightMargin,Ty);

      Ty := FullText+round(PixPer100m * FMaxDistance *6 / 500) ;
      TextOut(Tx, Ty-HalfText,'-'+IntToStr(FMaxDistance div 5));
      MoveTo(D1W, Ty);
      Pen.Width := 1;
      LineTo(RightMargin,Ty);

      {Paint verticals between areas}
  (*
      Pen.Width := 1;
      Pen.Style := psDot;
      Ty := TextHeight('1');
      Tx := D1W;
      MoveTo(Tx, Ty);
      LineTo(Tx,Height-Ty);

      Tx := Tx+D2W;
      MoveTo(Tx, Ty);
      LineTo(Tx,Height-Ty);

      Tx := Tx+D3W;
      MoveTo(Tx, Ty);
      LineTo(Tx,Height-Ty);

      Tx := Tx+D4W;
      MoveTo(Tx, Ty);
      LineTo(Tx,Height-Ty);

      Tx := Tx+D5W;
      MoveTo(Tx, Ty);
      LineTo(Tx,Height-Ty);

      Tx := Tx+D6W;
      MoveTo(Tx, Ty);
      LineTo(Tx,Height-Ty);

      Tx := Tx+D7W;
      MoveTo(Tx, Ty);
      LineTo(Tx,Height-Ty);
  *)
    Finally

    End;
  End;
End;

{Orders and announcements, Area D2/D3/D4}
Procedure TMMIareaD.PaintD234(TheImage: TBitmap; ARect: TRect);
var
  Xbmp,Ybmp: Integer;
  B: Byte;
  PItem: PAnnouncementsItem;
  Bitmap: TBitmap;
  Temp : Integer;
Begin
  If AnnouncementsList.count > 0 Then Begin
    // This is so the upper icon always is in col 3
    Temp := AnnouncementsList.count;
    LastCollumn := 3;
    while Temp > 1 do
    begin
      Temp := Temp-1;
      LastCollumn := LastCollumn - 1;
      if LastCollumn = 0 then LastCollumn := 3;
    end;

    for B := 0 to (AnnouncementsList.Count - 1) do Begin
      PItem := AnnouncementsList.Items[B];
      If (((PItem^.Position-FCurrentOdometerPosition)*direction) < FMaxDistance) and
         (((PItem^.Position-FCurrentOdometerPosition)*direction) > 0) Then Begin

// Commented out by Bo H, try using the already existing bitmap
{
         Bitmap:= TBitmap.Create;
         Bitmap.Assign(PItem^.Icon);
//         Bitmap := PItem^.Icon;
         PItem^.Used := LastCollumn;

         Xbmp := D1W+(D2W*(PItem^.Used-1));
         Ybmp := YPix0m - ((((PItem^.Position-FCurrentOdometerPosition)*direction) * PixPer100m) div 100);
         Ybmp := Ybmp - (Bitmap.Height div 2);

         Bitmap.Dormant;
         Bitmap.FreeImage;
         If Dimmed then begin
//           Bitmap.Monochrome := true;
         end;
         Bitmap.Transparent := true;
         Bitmap.TransparentColor := clBlack;
         with TheImage.Canvas do Begin
           Draw(Xbmp,Ybmp,Bitmap);
         End;
         Bitmap.Free;
}

// This block introduced by Bo H, 2011-02-10
// Do not allocate bitmaps each time. Use preallocated instead

         Bitmap := PItem^.Icon;
         PItem^.Used := LastCollumn;

         Xbmp := D1W+(D2W*(PItem^.Used-1));
         Ybmp := YPix0m - ( round(((PItem^.Position-FCurrentOdometerPosition)*direction) * PixPer100m) div 100 );
         Ybmp := Ybmp - (Bitmap.Height div 2);

         Bitmap.Transparent := true;
         Bitmap.TransparentColor := clBlack;
         with TheImage.Canvas do Begin

           Draw(Xbmp,Ybmp,Bitmap);
         End;

// End of block introduced by Bo H, 2011-02-10

      End;
      Inc(LastCollumn);
      If LastCollumn > 3 Then LastCollumn := 1;
    End;
  End;
End;


{ Gradient information }
Procedure TMMIareaD.PaintGradient(TheImage: TBitmap; Gradient: Integer; GStart,GEnd: Integer);
var
   GRect: TRect;
   TxStart: Integer;
   sGradient: String;
Begin
  With GRect do Begin
    Left := D1W+D2W+D3W+D4W+1;
    Top := YPix0m - (round(GEnd*PixPer100m) div 100);
    Right := Left+D5W;
    Bottom := YPix0m -(round(GStart*PixPer100m) div 100);
  End;

  sGradient := IntToStr(Gradient);

  with TheImage.Canvas do Begin
    try
      TxStart := (GRect.Left + (D5W div 2))- (TextWidth('+') div 2);

      If Gradient >= 0 Then
        Brush.Color := GradPlusColor
      else
        Brush.Color := GradMinusColor;

      Brush.Style := bsSolid;
      FillRect(GRect);

      Pen.Color := ForeColor;
{
      if Gradient >= 0 Then
      Font.Color := GradMinusColor
      else
      Font.Color := GradPlusColor;
}
      Font.Color := ForeColor;

      Pen.Style := psSolid;
      Pen.Width := 1;
      With GRect do
        Rectangle(Left,Top,Right,Bottom);

      if GRect.Bottom-GRect.Top > (3*TextHeight('1')) Then Begin
        if Gradient >= 0 Then Begin
          TextOut(TxStart,GRect.Top+1,'+');
          TextOut(TxStart,GRect.Bottom-TextHeight('1')-1,'+');
        End
        else Begin
          TextOut(TxStart,GRect.Top+1,'-');
          TextOut(TxStart,GRect.Bottom-TextHeight('1')-1,'-');
        End;
        TextOut(GRect.Left + ((GRect.Right - GRect.Left - TextWidth(sGradient)) div 2), {GRect.Left+2,}
                ((GRect.Bottom-GRect.Top) div 2)+ GRect.Top - (TextHeight('1') div 2),
                 sGradient);
      End
      else Begin
      End;
    Finally
    End;
  End;
End;

{Gradient profile, Area D5}
Procedure TMMIareaD.PaintD5(TheImage: TBitmap; ARect: TRect);
var
   B: Byte;
   PItem: PGradientItem;
   CurrentGradient: Integer;
   TempStart, TempEnd : integer ;

   sGradient: String;
   Xt,Yt: Integer;

Begin
   CurrentGradient:= 0;

   if GradientList.count > 0 Then
   Begin
     for B := 0 to (GradientList.Count - 1) do
     Begin
       PItem := GradientList.Items[B];

       If ((PItem^.StartPosition-FCurrentOdometerPosition)*direction) <= 0 Then
         TempStart :=0
       else
         TempStart:= (PItem^.StartPosition-FCurrentOdometerPosition)*direction;

       If ((PItem^.EndPosition-FCurrentOdometerPosition)*direction) > FMaxDistance then
         TempEnd:= FMaxDistance
       else
         TempEnd:= (PItem^.EndPosition-FCurrentOdometerPosition)*direction ;


{
       if B < (GradientList.Count - 1) then    //more items exist
       begin
         NextItem := GradientList.Items[B+1];
         if NextItem^.Gradient = 0 then
           TempEnd:= (NextItem^.StartPosition-FCurrentOdometerPosition)*direction;
       end;

}
       If (TempStart < FMaxDistance)and (TempEnd > 0) Then
         PaintGradient( TheImage,
                          PItem^.Gradient,
                          TempStart,
                          TempEnd);

       if TempStart <= 0 Then
          CurrentGradient := PItem^.Gradient;

     End;
   End;

   { Current gradient }
   if GradientList.count > 0 Then
 //   if CurrentGradient <> 0 Then
   Begin
     sGradient := IntToStr(CurrentGradient);
     with TheImage.Canvas do
     Begin
       if CurrentGradient > 0 Then
         Font.Color:= GradPlusColor
       else
         Font.Color:= GradMinusColor;

       Xt := D1W+D2W+D3W+D4W+(D5W div 2);
       Yt := YPix0m+(20);              // Sigdap 990824 not scale dependant

       Brush.Color:= BackColor;
       Font.Size:= 10;
       Font.Style:= [fsBold];
       TextOut( Xt- (TextWidth(sGradient) div 2),
                Yt- (TextHeight(sGradient) div 2),
                sGradient);
     End;
   End;
End;


{ Speed Targets, Area D6}
Procedure TMMIareaD.PaintD6(TheImage: TBitmap; ARect: TRect);
var
   B: Byte;
   PItem: PSpeedTargetItem;
   X, Y: Integer;
   SpeedIconToDraw: TBitmap;

Begin
  SpeedIconToDraw := nil;
  if SpeedTargetList.Count > 0 Then Begin
    for B := 0 to (SpeedTargetList.Count - 1) do Begin
      PItem := SpeedTargetList.Items[B];
    if ((((PItem^.StartPosition-FCurrentOdometerPosition)*direction) < FMaxDistance) and
           (((PItem^.StartPosition-FCurrentOdometerPosition)*direction) > 0)) Then Begin
        try
          SpeedIconToDraw := TBitMap.Create;
          X := D1W+D2W+D3W+D4W+D5W+1;
          Y := YPix0m - (round(((PItem^.StartPosition-FCurrentOdometerPosition)*direction) * PixPer100m) div 100) - 10;
          with TheImage.Canvas do Begin
            CopyMode := cmSrcCopy;
            if Not Dimmed then Begin
              case PItem^.SpeedType of
                SpeedIncrease: SpeedIconToDraw.LoadFromResourceName(HInstance, 'SPEED_INCREASE');
                SpeedDecrease: SpeedIconToDraw.LoadFromResourceName(HInstance, 'SPEED_DECREASE');
                SpeedZero: SpeedIconToDraw.LoadFromResourceName(HInstance, 'SPEED_ZERO');
              End;
            end
            else begin
              case PItem^.SpeedType of
                SpeedIncrease: SpeedIconToDraw.LoadFromResourceName(HInstance, 'SPEED_INCREASE_DIMM');
                SpeedDecrease: SpeedIconToDraw.LoadFromResourceName(HInstance, 'SPEED_DECREASE_DIMM');
                SpeedZero: SpeedIconToDraw.LoadFromResourceName(HInstance, 'SPEED_ZERO_DIMM');
              End;
            end;
            SpeedIconToDraw.Transparent := true;
            SpeedIconToDraw.TransparentColor := clBlack;
            Draw(X,Y,SpeedIconToDraw);
          End;
        Finally
          SpeedIconToDraw.Free;
        End;
      End;
    End;
  End;
End;

{ Static speed profile }
Procedure TMMIareaD.PaintSpeedProfile( TheImage: TBitmap;
                                       SStart,
                                       SEnd: Integer;
                                       Fraction: Integer);
var
   GRect: TRect;
   TempEnd: Integer;
//const
 //resolution: integer := 4 ;
Begin
  if SEnd > FMaxDistance Then
    TempEnd := FMaxDistance
  else
    TempEnd := SEnd;

  With GRect do Begin
    Left := D1W+D2W+D3W+D4W+D5W+D6W+1;
    Top := YPix0m - (round(TempEnd*PixPer100m) div 100);
    Right := Left+(Fraction * D7W div 4);
    Bottom := YPix0m - (round(SStart*PixPer100m) div 100);
  End;

  with TheImage.Canvas do Begin
    Pen.Color := SpeedProfForeColor;
    Brush.Color := SpeedProfForeColor;
    Brush.Style := bsSolid;
    FillRect(GRect);
  End;

End;

{ Static speed profile, Area D7 }
Procedure TMMIareaD.PaintD7(TheImage: TBitmap; ARect: TRect);
var
   GRect: TRect;
//   TxStart: Integer;
   B: Byte;
   PItem: PCeilingSpeedItem;
   TempStart, TempEnd : Integer;

Begin
   With GRect do Begin
     Left := D1W+D2W+D3W+D4W+D5W+D6W+1;
     Top := TheImage.Canvas.TextHeight('1');
     Right := Left+D7W{+D8W};
     Bottom := YPix0m;
   End;

   with TheImage.Canvas do Begin
     Pen.Color := SpeedProfBackColor;
     Brush.Color := SpeedProfBackColor;
     Brush.Style := bsSolid;
     FillRect(GRect);
   End;

   if CeilingSpeedList.Count > 0 Then Begin
     for B := 0 to (CeilingSpeedList.Count - 1) do Begin
       PItem := CeilingSpeedList.Items[B];

       If ((PItem^.EndPosition - FCurrentOdometerPosition)*direction) > FMaxDistance Then
         TempEnd := FMaxDistance
       else
         TempEnd := (PItem^.EndPosition - FCurrentOdometerPosition)*direction;

       If ((PItem^.StartPosition - FCurrentOdometerPosition)*direction) <=  0 Then
         TempStart := 0
       else
         TempStart := (PItem^.StartPosition - FCurrentOdometerPosition)*direction;

       if (TempStart < FMaxDistance) and
          (TempEnd > 0) Then
       Begin
         PaintSpeedProfile( TheImage,
                            TempStart,
                            TempEnd,
                            PItem^.Fraction);

       End;


     {
       if ((PItem^.StartPosition-FCurrentOdometerPosition )< FMaxDistance) Then Begin
       if((PItem^.startPosition-FCurrentOdometerPosition)< 0) Then Begin
           PaintSpeedProfile( TheImage,
                              0,
                              PItem^.EndPosition-FCurrentOdometerPosition,
                              PItem^.Fraction);
           End
           else Begin
           PaintSpeedProfile( TheImage,
                              PItem^.StartPosition-FCurrentOdometerPosition,
                              PItem^.EndPosition-FCurrentOdometerPosition,
                              PItem^.Fraction);
           End;

       End;
}
     End;
   End
End;


{ Brake Curve Area Start, Area D8 }
Procedure TMMIareaD.PaintD8(TheImage: TBitmap; ARect: TRect);
var
   XStart,Y,XEnd: Integer;
   B: Byte;
   PItem: PBCAStartItem;

Begin
   if (BCAStartList.count > 0) and
      (ShowBCA) Then Begin
     for B := 0 to (BCAStartList.Count - 1) do Begin
       PItem := BCAStartList.Items[B];
       if (PItem^.StartPosition < FMaxDistance) and
          (PItem^.StartPosition > 0) Then Begin
         XStart := D1W+D2W+D3W+D4W+D5W+D6W+D7W+1;
         XEnd := XStart+D8W;
         Y := YPix0m - (round(PItem^.StartPosition * PixPer100m) div 100);
         with TheImage.Canvas do Begin
           Pen.Color := BCAStartColor;
           Pen.Style := psSolid;
           Pen.Width := 2;
           MoveTo(XStart,Y);
           LineTo(XEnd,Y);
         End;
       End;
     End;
  End;
End;


{ Train length, Area D9 }
Procedure TMMIareaD.PaintD9(TheImage: TBitmap; ARect: TRect);
var
   TLRect: TRect;
   trainFits: Boolean;
Begin
  trainFits := true;
  with TLRect do Begin
    Left := D1W+D2W+D3W+D4W+D5W+(D6W div 3);
    Top := YPix0m;
    Right := Left + (D6W div 2);
    Bottom := YPix0m +(round(TrainLength * PixPer100m) div 100);
    if Bottom > Height then begin
      Bottom := Height - 2; // Don't paint on the border.
      trainFits := false;
    end;
  End;
  with TheImage.Canvas do Begin
    Pen.Color := ForeColor;
    Pen.Width := 1;
    Brush.Color := ForeColor;
    Brush.Style := bsSolid;
    FillRect(TLRect);
    //Paint end of train if whole train is visible.
    if trainFits then begin
      MoveTo(TLRect.Left,TLRect.Bottom);
      LineTo(TLRect.Right+TLRect.Right-TLRect.Left,TLRect.Bottom);
    end;
  End;
End;


procedure TMMIareaD.Setpalette(Value: hPalette);
begin
  if Value <> Fpalette then begin
    Fpalette := Value;
//    FUpDateNeeded := True ;
    Refresh;
  end;
end;

Procedure TMMIareaD.SetBorderStyle(Value: TBorderStyle);
Begin
  if Value <> FBorderStyle Then Begin
    FBorderStyle := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetForeColor(Value: TColor);
Begin
  if Value <> FForeColor Then Begin
    FForeColor := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetBackColor(Value: TColor);
Begin
  if Value <> FBackColor Then Begin
    FBackColor := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetGradPlusColor(Value: TColor);
Begin
  if Value <> FGradPlusColor Then Begin
    FGradPlusColor := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetGradMinusColor(Value: TColor);
Begin
  if Value <> FGradMinusColor Then Begin
    FGradMinusColor := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetSpeedProfBackColor(Value: TColor);
Begin
  if Value <> FSpeedProfBackColor Then Begin
    FSpeedProfBackColor := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetSpeedProfForeColor(Value: TColor);
Begin
  if Value <> FSpeedProfForeColor Then Begin
    FSpeedProfForeColor := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetBCAStartColor(Value: TColor);
Begin
  if Value <> FBCAStartColor Then Begin
    FBCAStartColor := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetCurrentOdometerPosition(Value: LongInt);
Begin
  Value := value;
  if Value <> FCurrentOdometerPosition Then
  Begin
    FCurrentOdometerPosition := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetSecondLayerTime(Value: Integer);
Begin
  if Value <> FSecondLayerTime Then Begin
    FSecondLayerTime := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetTrainLength(Value: Integer);
Begin
  if Value <> FTrainLength Then Begin
    FTrainLength := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetTrainLengthSecondLayerText(Value: String);
Begin
  if 0 <> compareStr(Value,FTrainLengthSecondLayerText) Then Begin
    FTrainLengthSecondLayerText := Value;
  End;
End;

Procedure TMMIareaD.SetMaxDistance(Value: Integer);
Begin
  if Value <> FMaxDistance Then Begin
    FMaxDistance := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetMaxScaleDistance(Value: Integer);
Begin
    FMaxScaleDistance := Value;
End;


Procedure TMMIareaD.SetRescaleable(Value: Boolean);
begin
  FRescaleable := Value;
end;

Procedure TMMIareaD.MouseDown( Button: TMouseButton;
                               Shift: TShiftState;
                               X,
                               Y: Integer);
var
   B: Byte;
   PItem: PAnnouncementsItem;
   BmpRect: TRect;
   TrainLengthPixels : Integer;

Begin
  { Check if over a track item }
  if AnnouncementsList.Count > 0 Then Begin
    for B := 0 to (AnnouncementsList.Count - 1) do Begin
      PItem := AnnouncementsList.Items[B];
      //If item is inside the viewable area.
      if (((PItem^.Position - FCurrentOdometerPosition) * direction) < FMaxDistance) and
         (((PItem^.Position - FCurrentOdometerPosition) * direction) > 0) and
         (PItem^.Used > 0) then
      begin
        with BmpRect do Begin
          Left := D1W+(D2W*(PItem^.Used-1));
          Top := YPix0m - (round(((PItem^.Position-FCurrentOdometerPosition)*direction) * PixPer100m) div 100)-
                          (PItem^.Icon.Height div 2);
          Right := Left + PItem^.Icon.Width;
          Bottom := Top + PItem^.Icon.Height;
        End;
        if PtInRect(BmpRect, Point(X,Y)) Then Begin
          if Layer2AnnouncementsItem = PItem Then Begin
            Layer2AnnouncementsItem:= nil;
            Refresh;
          End
          else Begin
            Layer2AnnouncementsItem:= PItem;
            Layer2Time := SecondLayerTime;
            Layer2TrainLength := false;
            Refresh;
          end;
          exit; //Exit procedure.
        End;
      End;
    End;
  End;

  //Check if in the trainlength clickable area.
  with BmpRect do Begin  {TrainLength}
    Left := D1W + D2W + D3W + D4W;//10;
    Top := YPix0m;
    Right := D1W+D2W+D3W+D4W+D5W+D6W+D7W+D8W;
    TrainLengthPixels := round(TrainLength * PixPer100m) div 100;
    Bottom := YPix0m + TrainLengthPixels;
  End;
  if PtInRect(BmpRect, Point(X,Y)) Then Begin
    if Layer2TrainLength Then Begin
      Layer2TrainLength := false;
      Refresh;
    End
    else Begin
      Layer2TrainLength := true;
      Layer2Time := SecondLayerTime;
      Layer2AnnouncementsItem:= nil;
      Refresh;
    End;
    exit; //Exit procedure.
  End;

  if FRescaleable and not FDimmed then begin
    //If clicked in the "scale up" area.
    with BmpRect do begin
      Left := 0;
      Top  := 0;
      Right := D1W;
      Bottom := self.Height div 8;
    end;
    if PtInRect(BmpRect, Point(X, Y)) then begin
      if FMaxDistance < MaxScaleDistance then begin
        FMaxDistance := FMaxDistance + 500;
        refresh;
      end;
      exit; //Exit procedure.
    end;

    //If clicked in the "scale down" area.
    with BmpRect do begin
      Left := 0;
      Top  := (self.Height * 7) div 8;
      Right := D1W;
      Bottom := self.Height;
    end;
    if PtInRect(BmpRect, Point(X, Y)) then begin
      if FMaxDistance > 500 then begin
        FMaxDistance := FMaxDistance - 500;
        refresh;
      end;
      exit; //Exit procedure.
    end;
  end;
End;

Procedure TMMIareaD.ShowAnnouncementsSecondLayer( TheCanvas: TCanvas;
                                                  PItem: PAnnouncementsItem);
var
   X,Y: Integer;
Begin
  X:= D1W+(D2W*(PItem^.Used-1)) + (PItem^.Icon.Width div 2);
  Y := YPix0m - (round(((PItem^.Position-FCurrentOdometerPosition)*direction) * PixPer100m) div 100);
  ShowSecondLayer(Canvas, X, Y, PItem^.Text);
End;

Procedure TMMIareaD.ShowTrainLengthSecondLayer(TheCanvas: TCanvas);
var
   X,Y: Integer;
   ARect: TRect;
   ABitmap: TBitmap;
   Bw : Integer;
   Mw : Integer;
   XOffs, YOffs: Integer;
   Text:String;
   CarNames:array of string;
   NumberOfCars,i:Integer;
   TempWidth : integer;
   WidestCarName:Integer;
Begin

  Text := TrainLengthSecondLayerText;
  NumberOfCars := 1;
  WidestCarName:= 1;
  SetLength(CarNames, CAR_LIST_MAX);  {Set max number of items in the array}

                                      {#-char is used as separator}
  for i := 1 to Length(Text) do
  begin
    if Text[i] <> '#' then
                                      {Concatenate string until separator found}
      CarNames[NumberOfCars-1] := CarNames[NumberOfCars-1] + Text[i]
    else
    begin
                                      {Separator found}
                                      {Calculate necessary width}
      tempWidth:= TheCanvas.TextWidth(CarNames[NumberOfCars-1]);
      if tempWidth > WidestCarName then
        WidestCarName := tempWidth;
                                      {Increment number of cars}
      NumberOfCars := NumberOfCars + 1;
                                      {Allocate more space if car-list is too long}
      if Length(CarNames) <= NumberOfCars then
        SetLength(CarNames, NumberOfCars + 10);
    end;
  end;

                                      {This is to catch the last entry }
                                      {Calculate necessary width}
  tempWidth := TheCanvas.TextWidth(CarNames[NumberOfCars-1]);
  if tempWidth > WidestCarName then
    WidestCarName := tempWidth;

  Bw := 6;                            { Border width }
  Mw := 4;                            { Top/Bottom-margin width }
                                      { Create the 2nd layer bitmap
                                        big enough to accomodate the cars }
  ABitmap := TBitmap.Create;
  with ABitmap do Begin
    Height := TheCanvas.TextHeight(Text)*(NumberOfCars) + 2*(Bw+Mw);
    Width  := (2*(Bw+Mw)) + WidestCarName;
  End;

                                      { Calculate the position }
  X:= D1W+D2W+D3W+D4W+D5W+(D6W div 2);
  Y:= YPix0m;
  XOffs := 20;
  YOffs := -ABitmap.Height-15;
  if (Y + YOffs) < 2 then
    YOffs := 2 - Y;

                                      { Define an area with the same size as the bitmap }
  with ARect do
  begin
    Left := 0;
    Top := 0;
    Right := ABitmap.Width;
    Bottom := ABitmap.Height;
  end;
                                      { Paint the 2nd layer pop-up }
  with ABitmap.Canvas do begin
    Brush.Color := clSilver;
    Brush.Style := bsSolid;
    FillRect(ARect);
    Brush.Color := clGray;
    Pen.Color:= clGray;

    Polygon([Point(0, ARect.Bottom-1),
             Point(ARect.Right-1, ARect.Bottom-1),
             Point(ARect.Right-1, 0),
             Point(ARect.Right-Bw, Bw),
             Point(ARect.Right-Bw, ARect.Bottom-Bw),
             Point(Bw, ARect.Bottom-Bw),
             Point(0, ARect.Bottom-1)] );

    Pen.Color:= clWhite;
    MoveTo(0,0);
    LineTo(Bw,Bw);
    InflateRect(ARect, -Bw, -Bw);

    Brush.Color := clTeal;
    Font.Color:= clYellow;
    FillRect(ARect);
                                      { Print the car names }
    For i:=1 to NumberOfCars do
      TextOut(Bw+Mw, Bw+Mw+(i-1)*TheCanvas.TextHeight('X'), CarNames[i-1]);

    Brush.Color := clYellow;
    Font.Color:= clTeal;

  end;

  with TheCanvas do Begin
    if ((X+XOffs+ABitmap.Width) > Width) Then
       XOffs := Width - (X + ABitmap.Width);
    Pen.Color:= clGray;
    Brush.Color:= clSilver;
    Polygon([Point(X+XOffs, Y+YOffs),
             Point(X, Y),
             Point(X+XOffs, Y+YOffs+ABitmap.Height),
             Point(X+XOffs, Y+YOffs)]);
    Brush.Color:= clGray;

    //If lower edge of bitmap above Y, then paint lower shadow.
    if (Y + YOffs + ABitmap.Height) < Y then begin
      Polygon([Point(X+XOffs, Y + YOffs + ABitmap.Height),
               Point(X, Y),
               Point(X+XOffs+ABitmap.Width, Y + YOffs + ABitmap.Height)]);
      Pen.Color:= clSilver;
      MoveTo(X + XOffs + 1, Y + YOffs + ABitmap.Height);
      LineTo(X+XOffs+ABitmap.Width, Y + YOffs + ABitmap.Height);
    end;

    Draw(X+XOffs+1, Y+YOffs, ABitmap);
  End;
  ABitmap.Free;


End;
{
var
   X,Y: Integer;
Begin
  X:= D1W+D2W+D3W+D4W+D5W+(D6W div 3);
  Y:= YPix0m +((TrainLength * PixPer100m) div 300);
  ShowSecondLayer(Canvas, X, Y, TrainLengthSecondLayerText);
End;
}

Procedure TMMIareaD.ShowSecondLayer( TheCanvas: TCanvas;
                                     X,
                                     Y: Integer;
                                     Text: String);
var
   ARect: TRect;
   ABitmap: TBitmap;
   Bw : Integer;
   Mw : Integer;
   XOffs, YOffs: Integer;
Begin
  Bw := 6;  // Border width
  Mw := 4;  // Top/Bottom-margin width
  XOffs := 20;
  YOffs := 15;
  ABitmap := TBitmap.Create;
  with ABitmap do Begin
    Height := TheCanvas.TextHeight(Text) + (2*(Bw+Mw));
    Width  := TheCanvas.TextWidth(Text) + (2*(Bw+Mw));
  End;
  with ARect do Begin
    Left := 0;
    Top := 0;
    Right := ABitmap.Width;
    Bottom := ABitmap.Height;
  End;
  with ABitmap.Canvas do Begin
    Brush.Color := clSilver;
    Brush.Style := bsSolid;
    FillRect(ARect);
    Brush.Color := clGray;
    Pen.Color:= clGray;

    Polygon( [Point(0, ARect.Bottom-1),
             Point(ARect.Right-1, ARect.Bottom-1),
             Point(ARect.Right-1, 0),
             Point(ARect.Right-Bw, Bw),
             Point(ARect.Right-Bw, ARect.Bottom-Bw),
             Point(Bw, ARect.Bottom-Bw),
             Point(0, ARect.Bottom-1)] );

    Pen.Color:= clWhite;
    MoveTo(0,0);
    LineTo(Bw,Bw);
    InflateRect(ARect, -Bw, -Bw);

    Brush.Color := clTeal;
    Font.Color:= clYellow;
    FillRect(ARect);
    TextOut(Bw+Mw, Bw+Mw, Text);
  End;
  with TheCanvas do Begin
    if ((X+XOffs+ABitmap.Width) > Width) Then
       XOffs := Width - (X + ABitmap.Width);
    Pen.Color:= clGray;
    Brush.Color:= clGray;
    Polygon([Point(X+XOffs, Y+YOffs),
             Point(X, Y),
             Point(X+XOffs, Y+YOffs+ABitmap.Height),
             Point(X+XOffs, Y+YOffs)]);
    Brush.Color:= clSilver;
    Polygon([Point(X+XOffs, Y+YOffs),
             Point(X, Y),
             Point(X+XOffs+ABitmap.Width, Y+YOffs),
             Point(X+XOffs, Y+YOffs)]);
    Draw(X+XOffs+1, Y+YOffs+1, ABitmap);
  End;
  ABitmap.Free;
End;


Procedure TMMIareaD.OneSecTic;
Begin
  if Assigned(Layer2AnnouncementsItem) or
    Layer2TrainLength Then Begin
    if Layer2Time > 0 Then
      Dec(Layer2Time);
    if Layer2Time = 0 Then Begin
      Layer2AnnouncementsItem := nil;
      Layer2TrainLength := false;
      refresh;
    End;
  End;
End;

Procedure TMMIareaD.SetDimmed(Value: Boolean);
Begin
  if Value <> FDimmed Then Begin
    FDimmed := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetShowBCA(Value: Boolean);
Begin
  if Value <> FShowBCA Then Begin
    FShowBCA := Value;
    Refresh;
  End;
End;

Procedure TMMIareaD.SetDirection ( Value : Integer) ;
Begin
  if Value <> FDirection Then Begin
    FDirection := Value;
//    Refresh;
  End;
End ;


Procedure TMMIareaD.EraseAllLists;
Var
i: Integer ;
Begin
//  FCurrentOdometerPosition := 0 ;

  For I:= 0 to AnnouncementsList.Count-1 do
    Dispose(AnnouncementsList.Items[I]);
  AnnouncementsList.Clear;

  For I:= 0 to GradientList.Count-1 do
    Dispose(GradientList.Items[I]);
  GradientList.Clear;

  For I:= 0 to SpeedTargetList.Count-1 do
    Dispose(SpeedTargetList.Items[I]);
  SpeedTargetList.Clear;

  For I:= 0 to CeilingSpeedList.Count-1 do
    Dispose(CeilingSpeedList.Items[I]);
  CeilingSpeedList.Clear;

  Layer2AnnouncementsItem:= nil;
//  Refresh;

End;

Procedure TMMIareaD.AddAnnouncementsToList( Icon: TBitmap;
                                            Text: String;
                                            Position: LongInt;
                                            CurrentOdometerPosition: LongInt);
var
   PItem: PAnnouncementsItem;
Begin
   Position := Position;
   New(PItem);
   PItem^.Icon := Icon;
   PItem^.Text := Text;
   PItem^.Position := Position;
   PItem^.Used := 0;
   AnnouncementsList.Add(PItem);
End;

Procedure TMMIareaD.AddGradientToList( Gradient: ShortInt;
                                       StartPosition: LongInt;
                                       EndPosition: LongInt;
                                       CurrentOdometerPosition: LongInt;
                                       NoOFGradTarg: Integer);
var
   PItem: PGradientItem;
Begin
   New(PItem);
   PItem^.Gradient := Gradient;
   PItem^.StartPosition := StartPosition;
   PItem^.EndPosition := EndPosition;
   GradientList.Add(PItem);

   {First target includes current gradient which should be strated from current odo position}
   if GradientList.Count = 1  then
      PGradientItem(GradientList.Items[GradientList.Count-1]).StartPosition :=  CurrentOdometerPosition;

   If GradientList.Count >= 2 Then
      { We have at least 2 gradient items.
        The index in the array of items is 0-based.
        Adjust the EndPos of the previous item}
     PGradientItem(GradientList.Items[GradientList.Count-2]).EndPosition := StartPosition ;

     {The last target which is primary target is not required after calculating the end position}
   if GradientList.Count = NoOFGradTarg then
         GradientList.Remove(PItem);

End;

Procedure TMMIareaD.AddSpeedTargetToList;
var
   PItem: PSpeedTargetItem;
   I : Integer ;
Begin
//  With SpeedTargetList do Begin
    For I := 0 To SpeedTargetList.Count -1 do Begin
        Dispose(SpeedTargetList.Items[I]) ;
    End;
      SpeedTargetList.Clear ;

    if CeilingSpeedList.Count >=2 Then
    for I := 1 To CeilingSpeedList.Count-1 do Begin
      if PCeilingSpeedItem(CeilingSpeedList.Items[I-1]).Speed <>
           PCeilingSpeedItem(CeilingSpeedList.Items[I]).Speed Then Begin
         New (PItem) ;
         PItem.StartPosition := 0 ;
         PItem.SpeedType :=SpeedDecrease ;

         If PCeilingSpeedItem(CeilingSpeedList.Items[I]).Speed = 0 Then
           PItem.SpeedType :=SpeedZero ;

         If PCeilingSpeedItem(CeilingSpeedList.Items[I]).Speed > 0 Then Begin
           if PCeilingSpeedItem(CeilingSpeedList.Items[I-1]).Speed >
             PCeilingSpeedItem(CeilingSpeedList.Items[I]).Speed Then
             PItem.SpeedType := SpeedDecrease;
           if PCeilingSpeedItem(CeilingSpeedList.Items[I-1]).Speed <
             PCeilingSpeedItem(CeilingSpeedList.Items[I]).Speed Then
             PItem.SpeedType := SpeedIncrease;
         End;
         PItem.StartPosition:= PCeilingSpeedItem(CeilingSpeedList.Items[I]).StartPosition;
         SpeedTargetList.Add(PItem) ;
      End;
    End;
//  End;
End;

Procedure TMMIareaD.AddCeilingSpeedToList( Speed: Integer;
                                          StartPosition: LongInt;
                                          EndPosition: LongInt;
                                          CurrentOdometerPosition: LongInt);
var
   PItem: PCeilingSpeedItem;
//   i : integer;
Begin
 //  StartPosition := StartPosition;
 //  EndPosition := EndPosition;
//   CurrentOdometerPosition := CurrentOdometerPosition;

   New(PItem);
   PItem.Speed := Speed;
   PItem.Fraction := 1 ;
   PItem.StartPosition := StartPosition;
   PItem.EndPosition := EndPosition;
   CeilingSpeedList.Add(PItem);
   If CeilingSpeedList.Count >= 2 Then Begin
     PCeilingSpeedItem(CeilingSpeedList.Items[CeilingSpeedList.Count-2]).EndPosition:= StartPosition ;
     PositionCeilingSpeedList;
     AddSpeedTargetToList;
   End;
//   if CurrentOdometerPosition = FCurrentOdometerPosition Then
//     refresh;  {if not equal, refresh is done in SetCurrentOdometerPosition }
//   SetCurrentOdometerPosition( CurrentOdometerPosition);
End;

Procedure TMMIareaD.PositionCeilingSpeedList;
Var
  i:          Integer;
  maxDec:     Integer;
  maxDecPos:  Integer;
  maxInc:     Integer;
  maxIncPos:  Integer;
  counter:    Integer;
  prevSpeed:  Integer;
  startItem:  Integer;
Begin
  maxDecPos  := 0;
  maxDec     := 0;
  maxIncPos  := 0;
  maxInc     := 0;
  counter    := 0;
  prevSpeed  := 0;

  //Find the item that is preceded by the highest nbr of speed decreases.
  for i := 0 To CeilingSpeedList.Count -1 do begin
    if PCeilingSpeedItem(CeilingSpeedList.Items[i]).Speed < prevSpeed then begin
      inc(counter);
      if counter > maxDec then begin
        maxDec := counter;
        maxDecPos := i;
      end;
    end
    else
      counter := 0;
    prevSpeed := PCeilingSpeedItem(CeilingSpeedList.Items[I]).Speed;
  end;

  //Find the item that is succeeded by the highest nbr of speed increases.
  prevSpeed := 0;
  counter := 0;
  for i := CeilingSpeedList.Count -1 downto 0 do begin
    if PCeilingSpeedItem(CeilingSpeedList.Items[i]).Speed < prevSpeed then begin
      inc(counter);
      if counter >= maxInc then begin
        maxInc := counter;
        maxIncPos := i;
      end;
    end
    else
      counter := 0;
    prevSpeed := PCeilingSpeedItem(CeilingSpeedList.Items[I]).Speed;
  end;

  //Determine the item to start from. It shall be the one that is
  //preceeded/succeeded by the highest number of speed decreases/increases.
  //If there are more than one item with the same number, we choose the one
  //with lowest index in the list.
  if (maxInc > maxDec) then
    startItem := maxIncPos
  else if (maxDec > maxInc) then
    startItem := maxDecPos
  else begin
    //maxInc and maxDec are the same, choose the first in list.
    if (maxIncPos < maxDecPos) then
      startItem := maxIncPos
    else
      startItem := maxDecPos;
  end;

  //Set fraction of start item.
  if PCeilingSpeedItem(CeilingSpeedList.Items[startItem]).Speed <= 0 then
    PCeilingSpeedItem(CeilingSpeedList.Items[startItem]).Fraction := 0
  else
    PCeilingSpeedItem(CeilingSpeedList.Items[startItem]).Fraction := 1;

  //Calculate and set fraction of speeds that come after start item.
  For i := startItem + 1 To CeilingSpeedList.Count - 1 do
  Begin
    PCeilingSpeedItem(CeilingSpeedList.Items[I]).Fraction :=
      calcFraction(PCeilingSpeedItem(CeilingSpeedList.Items[I]).Speed,
                   PCeilingSpeedItem(CeilingSpeedList.Items[I - 1]).Speed,
                   PCeilingSpeedItem(CeilingSpeedList.Items[I - 1]).Fraction);
  end;

  //Calculate and set fraction of speeds that come before start item.
  For I := startItem - 1 DownTo 0 do
  Begin
  PCeilingSpeedItem(CeilingSpeedList.Items[I]).Fraction :=
    calcFraction(PCeilingSpeedItem(CeilingSpeedList.Items[I]).Speed,
                 PCeilingSpeedItem(CeilingSpeedList.Items[I + 1]).Speed,
                 PCeilingSpeedItem(CeilingSpeedList.Items[I + 1]).Fraction);
  End;
End;

Procedure TMMIareaD.AddBCAStartToList( StartPosition: Integer;
                                       CurrentOdometerPosition: LongInt);
//var
//   PItem: PBCAStartItem;
Begin
//   StartPosition := StartPosition;
//   CurrentOdometerPosition := CurrentOdometerPosition;

//   New(PItem);
//   PItem^.StartPosition := StartPosition;
   PBCAStartItem(BCAStartList.Items[0]).StartPosition:=StartPosition ;
//   if CurrentOdometerPosition = FCurrentOdometerPosition Then
//     refresh;  {if not equal, refresh is done in SetCurrentOdometerPosition }
//   SetCurrentOdometerPosition( CurrentOdometerPosition);
End;

function TMMIareaD.calcFraction(speed, prevSpeed: Integer;
                                prevFraction: Byte): Byte;
var
  fraction: Byte;
begin
  //If higher speed.
  if (speed > prevSpeed) then begin
    if prevFraction < 4 then
      fraction := prevFraction + 1
    else
      fraction := 4;
  end
  //If lower speed.
  else if (speed < prevSpeed) then begin
    if (prevFraction > 0) then
      fraction := prevFraction - 1
    else
      fraction := 0;
  end

  //If same speed.
  else begin
    fraction := prevFraction;
  end;

  //If speed is zero, set fraction to zero.
  if speed = 0 then
    fraction := 0;

  result := fraction;
end;



Procedure Register;
Begin
  RegisterComponents('MMI', [TMMIareaD]);
End;

End.
