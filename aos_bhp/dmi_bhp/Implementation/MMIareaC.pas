unit MMIareaC;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TMMIareaC = class(TGraphicControl)
  private
    { Private declarations }
    FPalette: hPalette ;
    FForeColor: TColor;
    FBackColor: TColor;
    FVActual: Longint;
    FDimmed : Boolean;


    procedure Setpalette(Value: hPalette);
    procedure SetForeColor(Value: TColor);
    procedure SetBackColor(Value: TColor);
    procedure SetVActual(Value: Longint);
    procedure SetDimmed(Value: Boolean);


  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property PaletteToUse: hPalette read FPalette write SetPalette ;
    property ForeColor: TColor read FForeColor write SetForeColor default clSilver;
    property BackColor: TColor read FBackColor write SetBackColor default clBlack;
    property VActual: Longint read FVActual write SetVActual default 0;
    property Dimmed: Boolean read FDimmed write SetDimmed default False;

  end;

procedure Register;


implementation

uses MMIStd;
//  MainArea;

constructor TMMIareaC.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed,
                                 csOpaque,
                                 csFixedWidth,csFixedHeight,
                                 csNoStdEvents	] ;  // Sigdap 990828
  FForeColor:= clSilver;
{  FBackColor:= clBlack;}
  Height := 30;
  Width := 300;
  FDimmed := False;

end;

destructor TMMIareaC.Destroy;
begin
  inherited destroy;
//  DeleteObject(Palette256);
end;



procedure TMMIareaC.Paint;
var
  TheImage: TBitmap;
  PaintRect: TRect;
  sV: String;
  PTextC: TPoint; // Text center
  X,Xh,Xl,Y,Yh,Yl: Integer;
//  V1 : Integer;
  ScreenDC : hDC;

begin
  with Canvas do
  begin
    PaintRect := ClientRect;
    TheImage := TBitmap.Create;
    TheImage.Height := Height;
    TheImage.Width := Width;
  end;

  with PTextC do
  begin
     X := Width div 2;
     Y := Height div 2;
  end;

  ScreenDC := TheImage.Canvas.Handle;
  SelectPalette(ScreenDC, Fpalette, False);
  RealizePalette(ScreenDC);


  { Paint background }
  with TheImage.Canvas do
  begin
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    FillRect(PaintRect);

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


  with TheImage.Canvas do
  begin
     Font.Name := 'Helvetica' ;  //SIGDAP
     Font.Size := round(20*GetXfactor);
     Font.Style := [fsBold];
     Font.Color := ForeColor;

     // Paint the frame around area C1
     X := Width div 2;
     Y := Height div 2;
     Yh:= Y+(TextHeight('0') div 2)+5 ;
     Yl:= Y-(TextHeight('0') div 2)-5 ;
     Xh:= X+(3*TextWidth('0')div 2)+5 ;
     Xl:= X-(3*TextWidth('0')div 2)-5 ;
     Pen.Color := $02202020;
     MoveTo(Xl, Yh);
     LineTo(XH, Yh);
     LineTo(Xh, Yl);
     Pen.Color := $02000000;
     LineTo(Xl, Yl);
     LineTo(Xl, Yh);

     if not Dimmed then
     begin

       Y:= PTextC.Y - (TextHeight('0') div 2);
       if (9 >= VActual) then // 1 figures
         X:= PTextC.X
       else if (9 < VActual) and (99 >= VActual) then // 2 figures
         X:= PTextC.X - TextWidth('0')
       else if (99 < VActual) and (999 >= VActual) then // >3 figures
         X:= PTextC.X - 2* TextWidth('0');

       if 999 < VActual then
       begin
        X:= PTextC.X - 2* TextWidth('0');
        sV := 'EEE' ;
       end
       else
        sV := IntToStr(VActual) ;

       TextOut(X+(TextWidth('0') div 2),Y,sV);

     end;
  end;
  Canvas.CopyMode := cmSrcCopy;
  Canvas.Draw(0, 0, TheImage);

  TheImage.Free;
end;

procedure TMMIareaC.SetVActual(Value: Longint);
//var
//  TempPercent: Longint;
begin
  if FVActual <> Value then
  begin
    FVActual := Value;
    Refresh;
  end;
end;

procedure TMMIareaC.Setpalette(Value: hPalette);
begin
  if Value <> Fpalette then begin
    Fpalette := Value;
    Refresh;
  end;
end;

procedure TMMIareaC.SetForeColor(Value: TColor);
begin
  if Value <> FForeColor then
  begin
    FForeColor := Value;
    Refresh;
  end;
end;

procedure TMMIareaC.SetBackColor(Value: TColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;
    Refresh;
  end;
end;

Procedure TMMIareaC.SetDimmed(Value: Boolean);
Begin
  if Value <> FDimmed Then Begin
    FDimmed := Value;
    Refresh;
  End;
End;


procedure Register;
begin
  RegisterComponents('MMI', [TMMIareaC]);
end;

end.
