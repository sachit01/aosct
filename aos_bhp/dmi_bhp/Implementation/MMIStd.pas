
(*****************************************************************
           © COPYRIGHT Bombardier Transportation AB, SWEDEN 2011.
           ======================================================

    The copyright to the computer program herein is the
    property of Bombardier Transportation AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the
    written permission from Bombardier Transportation AB,
    or in accordance with the terms and conditions stipulated in the
    agreement/contract under which the program has been supplied.


    NAME :  MMIstd.pas

    PROJECT :  LKAB

    Ver    Author           Date    Reason
    ---    ------           ------  ------
    1.0.0  Daniel Persson   980220  First version
    2.3.5  Bo Hermansson    110301  Delphi XE
                                    Function GetPlattformInfo, string handling corrected
           Bo Hermansson    110330  Redesigned for LK
           Bo Hermansson    111109  Removed obsolete functions. New function, GetVersion
           Bo Hermansson    120222  CentreImage
           Bo Hermansson    121017  function ExitWin(ExitType: Integer): Boolean;
           Bo Hermansson    121026  LoadBitmapImage
           Bo Hermansson    121030  HexDigit(..)HexToStr(...)
           Bo Hermansson    121210  AbsoluteFilePath(..)

    DESCRIPTION : Unit with common func and procedures

 *********************************************************)

unit MMIStd;

interface

{****************************************************************************
* USES                                                                      *
****************************************************************************}
Uses
MMITypes, Windows, Controls, Buttons, StdCtrls, ExtCtrls, ColorButton2,
Graphics, Classes, SysUtils;

{****************************************************************************
* UNIT TYPE DECLARATIONS                                                    *
****************************************************************************}

{****************************************************************************
* UNIT FUNCTION DECLARATION                                                 *
****************************************************************************}

function SwapSmallint(Value: SmallInt): SmallInt;
function SwapLongInt(Value: LongInt): LongInt;
function SwapLongWord(Value: LongWord): LongWord;
function SwapCardinal(Value: Cardinal): Cardinal;
function CmPerSec_ToKmPerHour(Value: Word): Word;
function DelphiToUnixTime(const Time: TDateTime): LongInt;
function UnixToDelphiTime(const Time: LongInt): TDateTime;
function GetXfactor: Double;
function GetYfactor: Double;
function GetXpos1: Integer;
function GetXpos2: Integer;
function GetXpos3: Integer;
function GetYpos1: Integer;
function GetYpos2: Integer;
function GetYpos3: Integer;
procedure AdjustControl(Cntrl: TControl);
procedure AdjustColorButton(But: TColorButton; Stretch: Boolean = true);
procedure AdjustImage(Img: TImage);
procedure CentreImage(Img: TImage; PanelWidth : Integer; PanelHeight : Integer; Lower : Boolean=false; Upper :  Boolean=false);
procedure AdjustImageList(imageList:TImageList);
procedure AdjustLabel(Lbl: TLabel);
procedure AdjustEdit(Edt: TEdit);
procedure AdjustLabeledEdit(Edt:TLabeledEdit);
procedure AdjustSpeedButton(but:TSpeedButton);
procedure AdjustComboBox(Cmbbx:TComboBox);
function GetVersion(FileName: string): string;
procedure ReplaceImage(ImageList : TImageList;Index : Integer; ImagePath : String; BlockStringID : String);
procedure AddImageFromResource(ImageList : TImageList;ResourceName : String); overload;
procedure AddImageFromResource(ImageList : TImageList; ResourceName : String; Resize : Boolean); overload;
function IntToAnsiStr(X: Integer; Width: Integer = 0): AnsiString;
function ExitWin(ExitType: Integer): Boolean;
function LoadBitmapImage(Bitmap : TBitmap; HiRes : Boolean; ResourceName : String; HiResResourceName : String; Image : TCustomImage; BlockStringID : String) : Boolean; overload;
function LoadBitmapImage(Bitmap : TBitmap; ResourceName : String; Image : TCustomImage; BlockStringID : String) : Boolean; overload;
function LoadBitmapImage(Bitmap : TBitmap; Image : TCustomImage; BlockStringID : String) : Boolean; overload;
function LoadBitmapImage(Bitmap : TBitmap; ResourceName : String; ImagePath : String; BlockStringID : String) : Boolean; overload;
procedure LoadBitmapFromResource(Bitmap : TBitmap; HiRes : Boolean; ResourceName : String; HiResResourceName : String);
function AbsoluteFilePath(MMIRoot : String; FilePath : String) : String;
function  HexDigit(Digit:Byte) : AnsiChar;
function  HexToStr(HexValue:Byte) : AnsiString;
function ResizeBmp(bitmap: TBitmap; width, height: Integer): Boolean;
function LeftPad(value:Integer; length:Integer; pad:char ='0'):String;
Function InterfaceString(choosen:TIfaceEnum): String ;
Function getPredefinedMsg(specifier: Byte): String;
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;



{****************************************************************************
* UNIT VAR DECLARATIONS                                                     *
*****************************************************************************}

implementation

{****************************************************************************
* USES                                                                      *
****************************************************************************}
uses
Forms, StrUtils,Math,Dialogs,UnitMainArea,UnitMMIFrame ;

{****************************************************************************
* CONST DECLARATIONS                                                        *
****************************************************************************}

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
function GetXfactor():Double;
begin
  result:= FormMMIFrame.ClientWidth / 800.0;
end;
function GetYfactor():Double;
begin
  result:= FormMMIFrame.ClientHeight / 600.0;
end;

function GetXpos1():Integer;
begin
  result:= round(75.0*GetXfactor);
end;
function GetXpos2():Integer;
begin
  result:= GetXpos1 + round(350.0*GetXfactor);
end;
function GetXpos3():Integer;
begin
  result:= GetXpos2 + round(320.0*GetXfactor);
end;

function GetYpos1():Integer;
begin
  result:= round(350.0*GetYfactor);
end;
function GetYpos2():Integer;
begin
  result:= GetYpos1 + round(88.0*GetYfactor);
end;
function GetYpos3():Integer;
begin
  result:= GetYpos1 + round(150.0*GetYfactor);
end;

{*********************************************************
* Function:    AdjustControl
* Description: Adjust pos and size of control depending on screen size
*********************************************************}
Procedure AdjustControl(Cntrl : TControl );
begin
  Cntrl.Left:=round(Cntrl.Left*GetXfactor);
  Cntrl.Top:=round(Cntrl.Top*GetYfactor);
  Cntrl.Width:=round(Cntrl.Width*GetXfactor);
  Cntrl.Height:=round(Cntrl.Height*GetYfactor);
end;

{*********************************************************
* Function:    Adjust...
* Description: Adjust detailed appearance of specific components
*              depending on screen size
*********************************************************}
Procedure AdjustColorButton(But:TColorButton ; Stretch : Boolean);
begin
  AdjustControl(But);

  But.BevelSize:=round(But.BevelSize*GetXfactor);
  But.Font.Size:=round(But.Font.Size*GetYfactor);
  if (Stretch) then
   But.Style:=[bsStretch];
end;

Procedure AdjustImage(Img:TImage);
begin

  AdjustControl(Img);

  Img.Stretch:= true;
end;

{*********************************************************
* Function:    CentreImage
* Description: Adjust position of image in order to
*              centre within a panel
*********************************************************}
procedure CentreImage(Img: TImage; PanelWidth: Integer; PanelHeight: Integer;
  Lower: Boolean; Upper: Boolean);
begin

  Img.Left := (PanelWidth - Img.Width) div 2;
  if Lower or Upper then
    Img.Top := ((PanelHeight div 2) - Img.height) div 2
  else
    Img.Top := (PanelHeight - Img.height) div 2;

  if Lower then
  begin
    Img.Top := Img.Top + (PanelHeight div 2);

  end;

end;

procedure AdjustImageList(imageList:TImageList);
begin

  imageList.Height := Round(GetYfactor * imageList.Height);
  imageList.Width := Round(GetXfactor * imageList.Width);

end;


Procedure AdjustLabel(Lbl:TLabel);
begin

  AdjustControl(Lbl);

  Lbl.Font.Size:=round(Lbl.Font.Size*GetYfactor);
end;

Procedure AdjustEdit(Edt:TEdit);
begin

  AdjustControl(Edt);

  Edt.Font.Size:=round(Edt.Font.Size*GetYfactor);
end;

Procedure AdjustLabeledEdit(Edt:TLabeledEdit);
begin

  AdjustControl(Edt);

  Edt.Font.Size:=round(Edt.Font.Size*GetYfactor);
  Edt.EditLabel.Font.Size:=round(Edt.EditLabel.Font.Size*GetYfactor);
end;

procedure AdjustComboBox(Cmbbx:TComboBox);
begin
 AdjustControl(Cmbbx);
 Cmbbx.Height:= Round(GetYfactor * Cmbbx.Height);
 Cmbbx.Width := Round(GetXfactor * Cmbbx.Width);
 Cmbbx.Font.Size   := Round(Cmbbx.Font.Size * GetYfactor);
end;


procedure AdjustSpeedButton(but:TSpeedButton);
begin
  AdjustControl(but);

  but.Font.Size:=round(but.Font.Size*GetYfactor);
  but.Layout:=blGlyphTop;
end;


{*********************************************************
* Function:    SwapSmallint
* Description: Function that swaps high and low byte in
               a word. This function is needed because of
               the ATP architecture (motorola) that stores
               the word in a reversed order.
*********************************************************}
Function SwapSmallint (Value :SmallInt ) : SmallInt ;
Begin
   TSmallInttype(Result).Byte0 := TSmallInttype(Value).Byte1 ;
   TSmallInttype(Result).Byte1 := TSmallInttype(Value).Byte0 ;
End;

{*********************************************************
* Function:    SwapLongInt
* Description: Function that swaps high and low bytes in
               a signed 32. This function is needed
               because of the ATP architecture (motorola)
               that stores the 32 bit in a reversed order.
               Cardinal can not be used since it isn't a
               true 32 bit unsigned. Only restrictes the
               negative values.
*********************************************************}
Function SwapLongInt (Value :LongInt ) : LongInt ;
Begin
   TLongIntType(Result).byte0 := TLongIntType(Value).byte3 ;
   TLongIntType(Result).byte1 := TLongIntType(Value).byte2 ;
   TLongIntType(Result).byte2 := TLongIntType(Value).byte1 ;
   TLongIntType(Result).byte3 := TLongIntType(Value).byte0 ;
End;

{*********************************************************
* Function:    SwapLongWord
* Description: Function that swaps high and low bytes in
               an unsigned 32-bit long word. This function is needed
               because of the ATP architecture (motorola)
               that stores the 32 bit in a reversed order.
*********************************************************}
Function SwapLongWord (Value :LongWord ) : LongWord ;
Begin
   TLongWordType(Result).byte0 := TLongWordType(Value).byte3 ;
   TLongWordType(Result).byte1 := TLongWordType(Value).byte2 ;
   TLongWordType(Result).byte2 := TLongWordType(Value).byte1 ;
   TLongWordType(Result).byte3 := TLongWordType(Value).byte0 ;
End;

{*********************************************************
* Function:    SwapCardinal
* Description: Function that swaps high and low bytes in
               an unsigned 32. This function is needed
               because of the ATP architecture (motorola)
               that stores the 32 bit in a reversed order.
*********************************************************}
Function SwapCardinal (Value: Cardinal): Cardinal;
Begin
   TCardinalType(Result).byte0 := TCardinalType(Value).byte3 ;
   TCardinalType(Result).byte1 := TCardinalType(Value).byte2 ;
   TCardinalType(Result).byte2 := TCardinalType(Value).byte1 ;
   TCardinalType(Result).byte3 := TCardinalType(Value).byte0 ;
End;

{*********************************************************
* Function:    CmPerSec_ToKmPerHour
* Description: Convert cm/sec to Km/h
*********************************************************}
Function CmPerSec_ToKmPerHour (Value :Word ) : Word ;
Begin
     Result:=Round(Value*0.36)             // this will give us 0.1 km/h
End;

{*********************************************************
* Function:    DelphiToUnixTime
* Description: Convert Delhi time to unix time
               1970 1/1 to 1899 12/31 )
*********************************************************}
Function DelphiToUnixTime(Const Time : TDateTime ) :Longint ;
Var

Days, Hours, Minutes, Seconds, Msec : Word ;
Diff: TDateTime ;
Begin
     Diff:= Time - UnixOffset ;
     Days:= Trunc(Diff) ;
     DecodeTime(Diff,Hours,Minutes,Seconds,msec);
     Result := (Days *SecsPerDay) + (Hours * SecsPerHour) + (Minutes*SecsPerMinute) +seconds ;
End;

{*********************************************************
* Function:    UnixToDelphiTime
* Description: Convert unix time to delphi time
*********************************************************}
Function UnixToDelphiTime(Const Time :LongInt) : TDateTime ;
Var
Days, Hours, Minutes, Seconds : integer ;
Begin
     Days := Abs(time) div SecsPerDay ;
     Hours := (Abs(Time) mod SecsPerDay ) div SecsPerHour ;
     Minutes := (Abs(Time)-(Days * SecsPerDay)-(Hours * SecsPerHour)) div 60 ;
     seconds := (Abs(Time)-((Days * SecsPerDay )+Hours * SecsPerHour)) mod 60 ;
     Result := EncodeTime(Hours,Minutes,Seconds,000);
     Result := Result+UnixOffset+days ;
End;

{*********************************************************
* Function:    GetVersion
* Description: Get version of file. Return as a string
*********************************************************}
function GetVersion(FileName : String) : String;
var
  Version:string;
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin

  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);

  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    Version := IntToStr(dwFileVersionMS shr 16);
    Version := Version + '.' + IntToStr(dwFileVersionMS and $FFFF);
    Version := Version + '.' + IntToStr(dwFileVersionLS shr 16);
  end;

  FreeMem(VerInfo, VerInfoSize);

  Result:=Version;
end;


{*********************************************************
* Procedure     ReplaceImage
* Description:  Replace image in an ImageList
*********************************************************}
procedure ReplaceImage(ImageList : TImageList;Index : Integer; ImagePath : String; BlockStringID : String);
var
  ImageBitmap : TBitmap;
  MaskBitmap : TBitmap;
begin

  if (FileExists(ImagePath)) then
    begin

      ImageList.Delete(Index);

      ImageBitmap := TBitmap.Create;
      ImageBitmap.LoadFromFile(ImagePath);
      ImageBitmap.TransparentColor := clBlack;
      ImageBitmap.Transparent := True;

      MaskBitmap := TBitmap.Create;
      MaskBitmap.LoadFromFile(ImagePath);
      MaskBitmap.Mask(clBlack);

      ImageList.Insert(Index,ImageBitmap,MaskBitmap);

      ImageBitmap.Free;
      MaskBitmap.Free;
    end
    else
      FormMMIFrame.LogEventStr(LogLevelNormal, AnsiString(BlockStringID), AnsiString('Could not load image from file: ' + ImagePath));
end;
{*********************************************************
* Procedure     AddImageFromResource
* Description:  Replace image in an ImageList from resource
*********************************************************}
procedure AddImageFromResource(ImageList : TImageList;ResourceName : String);
var
  ImageBitmap : TBitmap;
  MaskBitmap : TBitmap;
begin


  ImageBitmap := TBitmap.Create;
  ImageBitmap.LoadFromResourceName(HInstance, ResourceName);
  ImageBitmap.TransparentColor := clBlack;
  ImageBitmap.Transparent := True;

  MaskBitmap := TBitmap.Create;
  MaskBitmap.LoadFromResourceName(HInstance, ResourceName);
  MaskBitmap.Mask(clBlack);

  ImageList.Add(ImageBitmap,MaskBitmap);

  ImageBitmap.Free;
  MaskBitmap.Free;

end;

{*********************************************************
* Procedure     InsertImageFromResource
* Description:  Insert image in an ImageList from resource
*********************************************************}
procedure AddImageFromResource(ImageList : TImageList; ResourceName : String; Resize : Boolean);
var
  ImageBitmap : TBitmap;
  MaskBitmap : TBitmap;
begin


  ImageBitmap := TBitmap.Create;
  ImageBitmap.LoadFromResourceName(HInstance, ResourceName);
  if Resize then
    ResizeBmp(ImageBitmap, ImageList.Width, ImageList.Height);

  ImageBitmap.TransparentColor := clBlack;
  ImageBitmap.Transparent := True;

  MaskBitmap := TBitmap.Create;
  MaskBitmap.LoadFromResourceName(HInstance, ResourceName);
  if Resize then
    ResizeBmp(MaskBitmap, ImageList.Width, ImageList.Height);

  MaskBitmap.Mask(clBlack);

  ImageList.Add(ImageBitmap,MaskBitmap);

  ImageBitmap.Free;
  MaskBitmap.Free;

end;

{*********************************************************
* Procedure     LoadBitmapFromResource
* Description:  Load bitmap from resource
*               BitmapScale is 1 if designed for 640 * 400
*               1.25 for 800 * 600
*               1.6  for 1024 * 768
*********************************************************}
procedure LoadBitmapFromResource(Bitmap : TBitmap; HiRes : Boolean; ResourceName : String; HiResResourceName : String);
begin

  if HiRes then
    Bitmap.LoadFromResourceName(HInstance, HiResResourceName)
  else
    Bitmap.LoadFromResourceName(HInstance, ResourceName);

end;


function ResizeBmp(Bitmap: TBitmap; width, height: Integer): Boolean;
var
  TmpBmp: TBitmap;
  ARect: TRect;
begin
  try
    TmpBmp := TBitmap.Create;
    try
      TmpBmp.Width  := width;
      TmpBmp.Height := height;
      ARect.Left:= 0;
      ARect.Top:=0;
      ARect.Right:=width;
      ARect.Bottom:=height;
      TmpBmp.Canvas.StretchDraw(ARect, Bitmap);
      bitmap.Assign(TmpBmp);
    finally
      TmpBmp.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;


{*********************************************************
* Procedure     IntToAnsiStr
* Description:  Convert integer to AnsiString
*********************************************************}
function IntToAnsiStr(X: Integer; Width: Integer = 0): AnsiString;
begin
  Str(X: Width, Result);
end;

{*********************************************************
* Procedure     LeftPad
* Description:  Convert integer to String
*********************************************************}
function LeftPad(value:Integer; length:Integer; pad:char='0') : String;
begin
  result := RightStr(StringOfChar(pad,length) + IntToStr(value), length );
end;


{*********************************************************
* Procedure     ExitWin
* Description:  Get shutdown priviliges and Shutdown/reboot system
*********************************************************}
function ExitWin(ExitType: Integer): Boolean;
{ExitType can be any of these values:
EWX_FORCE, EWX_LOGOFF, EWX_POWEROFF, EWX_REBOOT, EWX_SHUTDOWN}

  function GetShutdownPrivilege: Boolean;
  var
    hToken: THandle;
    tkp: TTokenPrivileges;
    retlength: DWORD;
    Newt: TTokenPrivileges;
  begin
    Result := False;
    hToken := GetCurrentProcess();
    if OpenProcessToken(hToken, TOKEN_ADJUST_PRIVILEGES + TOKEN_QUERY, hToken) then
    begin
      {Get the LUID for shutdown privilege}
      if LookupPrivilegeValue(nil, 'SeShutdownPrivilege', tkp.Privileges[0].Luid) then
      begin
        tkp.PrivilegeCount := 1; {One privilege to set}
        tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        {Get shutdown privilege for this process}
        Result := AdjustTokenPrivileges(hToken, FALSE, tkp, sizeof(TTokenPrivileges),
          Newt, retlength)
      end;
    end;
  end;

begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    GetShutdownPrivilege;
  if ExitWindowsEx(ExitType, 0) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;


{*********************************************************
* Procedure     LoadBitmapImage
* Description:  Load bitmap from file
* Returns true if image was loaded from file
*********************************************************}
function LoadBitmapImage(Bitmap : TBitmap; Image : TCustomImage; BlockStringID : String) : Boolean;
begin
  if (Image.ImagePath <> '') then
  begin
    if (FileExists(Image.ImagePath)) then
    begin
      Bitmap.LoadFromFile(Image.ImagePath);
      Result := true;
    end
    else
    begin
      FormMMIFrame.LogEventStr(LogLevelNormal, AnsiString(BlockStringID), AnsiString('Could not load image from file: ' + Image.ImagePath));
      Result := false;
    end;
  end
  else
  begin
    Result := false;
  end;

end;
{*********************************************************
* Procedure     LoadBitmapImage
* Description:  Load bitmap from file or resource(default)
* Returns true if image was loaded from file
*********************************************************}
function LoadBitmapImage(Bitmap : TBitmap; ResourceName : String; Image : TCustomImage; BlockStringID : String) : Boolean;
begin
  if (Image.ImagePath <> '') then
  begin
    if (FileExists(Image.ImagePath)) then
    begin
      Bitmap.LoadFromFile(Image.ImagePath);
      Result := true;
    end
    else
    begin
      FormMMIFrame.LogEventStr(LogLevelNormal, AnsiString(BlockStringID), AnsiString('Could not load image from file: ' + Image.ImagePath));
      Bitmap.LoadFromResourceName(Hinstance, ResourceName);
      Result := false;
    end;
  end
  else
  begin
    Bitmap.LoadFromResourceName(Hinstance, ResourceName);
    Result := false;
  end;

end;
{*********************************************************
* Procedure     LoadBitmapImage
* Description:  Load bitmap from file or resource(default)
* Returns true if image was loaded from file
*********************************************************}
function LoadBitmapImage(Bitmap : TBitmap; ResourceName : String; ImagePath : String; BlockStringID : String) : Boolean;
begin
  if (ImagePath <> '') then
  begin
    if (FileExists(ImagePath)) then
    begin
      Bitmap.LoadFromFile(ImagePath);
      Result := true;
    end
    else
    begin
      FormMMIFrame.LogEventStr(LogLevelNormal, AnsiString(BlockStringID), AnsiString('Could not load image from file: ' + ImagePath));
      Bitmap.LoadFromResourceName(Hinstance, ResourceName);
      Result := false;
    end;
  end
  else
  begin
    Bitmap.LoadFromResourceName(Hinstance, ResourceName);
    Result := false;
  end;

end;
{*********************************************************
* Procedure     LoadBitmapImage
* Description:  Load bitmap from file or resource(default)
* Returns true if image was loaded from file
*********************************************************}
function LoadBitmapImage(Bitmap : TBitmap; HiRes : Boolean; ResourceName : String; HiResResourceName : String; Image : TCustomImage; BlockStringID : String) : Boolean;
var
  ThisResourceName : String;
  ThisImagePath         : String;
begin

  if HiRes then
  begin
    ThisResourceName := HiResResourceName;
    ThisImagePath := Image.HiResImagePath;
  end
  else
  begin
    ThisResourceName := ResourceName;
    ThisImagePath := Image.ImagePath;
  end;

  Result := LoadBitmapImage(Bitmap, ThisResourceName, ThisImagePath, BlockStringID);

end;

{*********************************************************
* Function:    AbsoluteFilePath
* Description: Converts any relative path to an absolute path
* using MMIRoot as root.
*
*********************************************************}
function AbsoluteFilePath(MMIRoot : String; FilePath : String) : String;
var
  AbsolutePath : String;
begin

  if (FilePath <> '') then
  begin
    if IsRelativePath(FilePath) then
    begin
      AbsolutePath := MMIRoot + FilePath;
    end
    else
    begin
      AbsolutePath := FilePath;
    end;
  end
  else
  begin
    AbsolutePath := FilePath;
  end;

  Result := AbsolutePath;
end;
{*********************************************************
* Function:    HexDigit
* Description: Returns the character corresponding
* with the hex digit 0-15
*********************************************************}
function HexDigit(Digit:Byte) : AnsiChar;
begin

  if (Digit < 10) then
    Result := AnsiChar (Ord('0') + Digit)
  else
    Result := AnsiChar (Ord('A') + (Digit - 10));

end;
{*********************************************************
* Function:    HexToStr
* Description: Returns the character string corresponding
* with the hex value 0-255
*********************************************************}
function HexToStr(HexValue:Byte) : AnsiString;
begin

  Result := HexDigit(HexValue div 16) + HexDigit(HexValue mod 16);

end;

{*********************************************************
* Function:    InterfaceString
* Description: Returns the string to use on the screen
               for the selected language
*********************************************************}
Function InterfaceString(choosen:TIfaceEnum): String ;
begin
    Result:= FInterfacestring[Ord(Choosen)] ;
end;

function getPredefinedMsg(specifier: Byte): String;
begin
  result:= '';
        // Get message string from string list if the specifier is okay.
  if (specifier > 0) and (specifier < FPredefTextMsgString.Count) then
  begin
    result:= FPredefTextMsgString.Strings[specifier];
  end;
  if result = '' then
        // Display a string for unknown EA-code followed by the code.
    result:= FPredefTextMsgString.Strings[0] + IntToStr(specifier);
end;

{*********************************************************
* Function:    Split
* Description:  string to use on the screen
               for the selected language
*********************************************************}
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
   ListOfStrings.DelimitedText   := Str;
end;


{****************************************************************************
* INITIALIZATION PART                                                       *
****************************************************************************}
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
