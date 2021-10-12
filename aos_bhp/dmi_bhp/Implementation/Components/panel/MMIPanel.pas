unit MMIPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TMMIPanel = class(TPanel)
  private
      FPalette: hPalette ;
    { Private declarations }
    Procedure PaintBackground(AnImage: TBitmap; Arect: TRect);
    procedure Setpalette(Value: hPalette);
  protected
    { Protected declarations }
    Procedure Paint; override;
    function GetPalette: HPALETTE; override;
  public
    { Public declarations }
  published
    { Published declarations }
    property PaletteToUse: hPalette read FPalette write SetPalette ;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation
//uses
//  MainArea;

procedure Register;
begin
  RegisterComponents('MMI', [TMMIPanel]);
end;

constructor TMMIPanel.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
//  Palette256 := getThePalette;
End;

destructor TMMIPanel.Destroy;
Begin
  inherited destroy;
//  DeleteObject(Palette256);
End;
(*
procedure TMMIPanel.QNewPalette(var Msg: TWMQueryNewPalette);
var
  i : Word;
  DC :HDC;
  HPold : HPalette;
Begin
  DC := Canvas.Handle;
  HPold := SelectPalette(DC, MainArea.MainAreaForm.Palette256, False);
  i := RealizePalette(DC);
  SelectPalette(DC, HPold, False);
  if (i>0) then InvalidateRect(Handle, Nil, False);
  Msg.Result := i;
end;

procedure TMMIPanel.PalChanged(var Msg: TWMPaletteChanged);
var
  i : Word;
  DC :HDC;
  HPold : HPalette;
Begin
  if (Msg.PalChg = Handle) then Msg.Result := 0
  else begin
    DC := Canvas.Handle;
    HPold := SelectPalette(DC, MainArea.MainAreaForm.Palette256, True);
    i := RealizePalette(DC);
    UpdateColors(DC);
    SelectPalette(DC, HPold, False);
  end;
end;
*)


procedure TMMIPanel.Setpalette(Value: hPalette);
begin
  if Value <> Fpalette then begin
    Fpalette := Value;
//    SelectPalette(ScreenDC,FPalette, False);
//    FUpDateNeeded := True ;
  Refresh;
  end;
end;

function TMMIPanel.GetPalette: HPALETTE;
begin
  result := Fpalette;
end;

Procedure TMMIPanel.Paint;
var
  TheImage: TBitmap;
  PaintRect: TRect;
//  ARect: TRect;
  ScreenDC : hDC;
Begin
  with Canvas do Begin
    try
      PaintRect := ClientRect;
      TheImage := TBitmap.Create;
      TheImage.Height := Paintrect.Bottom;
      TheImage.Width := Paintrect.Right;

 

      try
        ScreenDC := TheImage.Canvas.Handle;
        SelectPalette(ScreenDC, Fpalette, False);
        RealizePalette(ScreenDC);

        PaintBackground(TheImage, PaintRect);

      Finally
      End;
      { Draw TheImage on screen }
      Canvas.CopyMode := cmSrcCopy;
      Canvas.Draw(0, 0, TheImage);
      Canvas.CopyMode := cmSrcCopy;

    Finally
      TheImage.Destroy;
    End;
  End;
End;

Procedure TMMIPanel.PaintBackground(AnImage: TBitmap; Arect: TRect);
Begin
  with AnImage.Canvas do Begin
    Brush.Color :=  $02101010; //BackColor;
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
(*
function TMMIPanel.getThePalette: hPalette;
Const PaletteDataSize = SizeOf(TLogPalette)+(255*SizeOf(TPaletteEntry));
var
  Resource   : hRsrc;
  Memory     : hGlobal;
  InfoHeader : PBitmapInfoHeader;
  Palette    : PLogPalette;
  I          : Integer;
  BMI        : PBitmapInfo;

begin
  Resource   := FindResource(hInstance, PChar('256BMP'), rt_Bitmap);
  Memory     := LoadResource(hInstance, Resource);
  InfoHeader := PBitmapInfoHeader(LockResource(Memory));
  GetMem(Palette, PaletteDataSize);
  BMI := PBitmapInfo(InfoHeader);
  With Palette^ do Begin
    palVersion := $300;
    palNumEntries := 256;
    For I := 0 to 255 do Begin
    {$R-}
      palPalEntry[I].peRed   := BMI^.bmiColors[I].rgbRed;
      palPalEntry[I].peGreen := BMI^.bmiColors[I].rgbGreen;
      palPalEntry[I].peBlue  := BMI^.bmiColors[I].rgbBlue;
      palPalEntry[I].peFlags := 0;
    {$R+}
    End;
  End;
  Result := CreatePalette(Palette^);
  FreeMem(Palette, PaletteDataSize);
  UnlockResource(Memory);
  FreeResource(Memory);
end;
*)


end.
