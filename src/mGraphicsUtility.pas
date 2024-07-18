// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mGraphicsUtility;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I mDefines.inc}

interface

uses
  {$IFDEF FPC}
  LCLIntf,
  LclType,
  LclProc,
  LResources,
  LMessages,
  InterfaceBase,
  {$ENDIF}
  {$IFDEF WINDOWS}Windows,{$ENDIF} Forms, Controls, Graphics, Types, Classes;

type
  TRectangleSide = (rsCenter, rsTop, rsLeft, rsBottom, rsRight, rsOutside);
  TTriangleOrientation = (toTop, toBottom, toLeft, toRight);

  procedure GetScreenShot (aBitmap : Graphics.TBitmap);
  procedure CropBitmap (aSourceBitmap, aDestBitmap : Graphics.TBitmap; X, Y : integer);
  function PointInTriangle (pt, v1, v2, v3 : TPoint) : boolean;
  function RectangleSide (aRect : TRect; aX, aY : integer) : TRectangleSide;

  procedure CopyDraw(ACanvas: TCanvas; Bitmap: TBitmap; const ARect: TRect);

  function DarkerColor(aColor: TColor; percent: Byte): TColor;
  function LighterColor(aColor: TColor; percent: Byte): TColor;
  function IsDark (const aColor: TColor): boolean;

  // https://stackoverflow.com/questions/20129758/algorithm-to-randomly-generate-a-color-palette-in-delphi
  function GenerateRandomColor(const Mix: TColor = clWhite): TColor;

  function ScaleForDPI (const aValue : integer) : integer;


  // https://forum.lazarus.freepascal.org/index.php?topic=36579.0
  function GetTextWidth(const aText: String; const aFont: TFont): Integer;
  procedure GetTextExtend(const aText: String; const aFont: TFont; var aSize : TPoint);

  procedure WriteText(aCanvas: TCanvas; const aRect: TRect; const aText: string; aTextAlignment: TAlignment; const aAdjustFontSize : boolean);
  procedure DrawTriangle(aCanvas: TCanvas; const aRect: TRect; const aOrientation : TTriangleOrientation);
  procedure DrawCheckedBox(aCanvas: TCanvas; const aRect: TRect; const aValue : boolean; const aColor, aBackgroundColor, aBorderColor : TColor);

  {$IFDEF FPC}
  function IsDoubleBufferedNeeded: boolean;
  {$ENDIF}

  function CtrlPressed: boolean;
  function ShiftPressed: boolean;

  // https://github.com/Alexey-T/ATSynEdit/blob/master/atsynedit/atstringproc.pas
  procedure CopyTextToClipboard(aText: string);

  function GeneratePNGThumbnailOfImage(const aSourceFile, aThumbnailFile: String; const aMaxWidth, aMaxHeight: word;out aError: String): boolean;
  function GenerateJPGThumbnailOfImage(const aSourceFile, aThumbnailFile: String; const aMaxWidth, aMaxHeight: word;out aError: String): boolean;

  procedure FlashInWindowsTaskbar(const aFlashEvenIfActive : boolean);


implementation

uses
  SysUtils, Math, Clipbrd
  {$IFDEF FPC}
    ,graphutil
  {$ELSE}
    ,System.UIConsts, System.UITypes, Vcl.Imaging.pngimage
  {$ENDIF}
  ;

{$IFDEF FPC}
const
  HLSMAX = 255;
{$ENDIF}

var
  _TextExtendBitmap : TBitmap;

{$IFDEF FPC}
function IsDoubleBufferedNeeded: boolean;
begin
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
end;
{$ENDIF}

function GetTextWidth(const aText: String; const aFont: TFont): Integer;
var
  bmp: TBitmap;
begin
  Result := 0;
  bmp := TBitmap.Create;
  try
    bmp.Canvas.Font.Assign(aFont);
    Result := bmp.Canvas.TextWidth(aText);
  finally
    bmp.Free;
  end;
end;

procedure GetTextExtend(const aText: String; const aFont: TFont; var aSize: TPoint);
var
  sz : TSize;
begin
  if not Assigned(_TextExtendBitmap) then
    _TextExtendBitmap := TBitmap.Create;
  _TextExtendBitmap.Canvas.Font.Assign(aFont);
  sz := _TextExtendBitmap.Canvas.TextExtent(aText);
  aSize.X:= sz.Width;
  aSize.Y:= sz.Height;
end;

procedure WriteText(aCanvas: TCanvas; const aRect: TRect; const aText: string; aTextAlignment: TAlignment; const aAdjustFontSize : boolean);
var
  {$IFNDEF WINDOWS}
  xPos, tw, len1, len2 : integer;
  newText : string;
  {$ELSE}
  TempFlags: cardinal;
  tmpRect : TRect;
  {$ENDIF}
  w, h : integer;
  lastSize : integer;
begin
  SetBkMode(aCanvas.Handle, TRANSPARENT);

  if aAdjustFontSize and (aText <> '') then
  begin
    aCanvas.Font.Size := max(aCanvas.Font.Size, 1);
    lastSize := aCanvas.Font.Size;

    w := aCanvas.TextWidth(aText);
    h := aCanvas.TextHeight(aText);
    if (w > aRect.Width) or (h > ARect.Height) then
    begin
      while ((w > aRect.Width) or (h > ARect.Height)) and (aCanvas.Font.Size > 1) do
      begin
        aCanvas.Font.Size := aCanvas.Font.Size - 1;
        w := aCanvas.TextWidth(aText);
        h := aCanvas.TextHeight(aText);
      end;
    end
    else if (w < aRect.Width) and (h < aRect.Height) then
    begin
      while (w < aRect.Width) and (h < aRect.Height) do
      begin
        lastSize := aCanvas.Font.Size;
        aCanvas.Font.Size := aCanvas.Font.Size + 1;
        w := aCanvas.TextWidth(aText);
        h := aCanvas.TextHeight(aText);
      end;
      aCanvas.Font.Size:= lastSize;
    end;
    // aCanvas.Font.Size := min(max(8, (aRect.Bottom - aRect.Top) - 30), max(8, (aRect.Right - aRect.Left) - 24));
  end;
  {$IFNDEF WINDOWS}
  newText := aText;
  if (aRect.Width < aCanvas.TextWidth('..')) then
    exit;
  tw := aCanvas.TextWidth(newText);
  if tw > (aRect.Right - aRect.Left) then
  begin
    len1 := Length(aText);
    if len1 = 2 then
        newText := Copy(newText, 1, 1) + '.'
    else if len1 > 2 then
    begin
      while tw > (aRect.Right - aRect.Left) do
      begin
        len2 := Length(newText);
        newText := Copy(newText, 1, len2 - 3) + '..';
        if newText = '..' then
        begin
          newText := Copy(aText, 1, 1) + '.';
          break;
        end;
        tw := aCanvas.TextWidth(newText);
      end;
    end;
  end;
  case aTextAlignment of
    taLeftJustify: xPos := aRect.Left;
    taRightJustify: xPos := aRect.Right - tw;
    taCenter: xPos := aRect.Left + ((aRect.Width - tw) div 2);
  end;
  aCanvas.TextOut(xPos, aRect.Top, newText);
  //DebugLn(IntToStr(xPos) + ' ' + newText + ' ' + IntToStr(aCanvas.Font.Size));
  {$ELSE}
  TempFlags := 0;
  case aTextAlignment of
    taLeftJustify: TempFlags := DT_LEFT;
    taRightJustify: TempFlags := DT_RIGHT;
    taCenter: TempFlags := DT_CENTER;
  end;
  TempFlags := TempFlags or (DT_VCENTER + DT_SINGLELINE {$ifndef fpc}+ DT_WORD_ELLIPSIS{$endif});

  tmpRect := aRect;
  if DrawText(aCanvas.Handle, PChar(aText), -1, tmpRect, TempFlags) = 0 then
    RaiseLastOSError;
  {$ENDIF}
end;

procedure DrawTriangle(aCanvas: TCanvas; const aRect: TRect; const aOrientation: TTriangleOrientation);
var
  P: Array[0..2] of TPoint;
begin
  case aOrientation of
    toTop:
      begin
        P[0]:= aRect.CenterPoint;
        P[0].Y:= aRect.Top;
        P[1]:= aRect.BottomRight;
        P[2]:= P[1];
        P[2].X:= aRect.Left;
      end;
    toBottom:
      begin
        P[0]:= aRect.CenterPoint;
        P[0].Y:= aRect.Bottom;
        P[1]:= aRect.TopLeft;
        P[2]:= P[1];
        P[2].X:= aRect.Right;
      end;
    toRight:
      begin
        P[0]:= aRect.CenterPoint;
        P[0].X:= aRect.Right;
        P[1]:= aRect.TopLeft;
        P[2]:= P[1];
        P[2].Y:= aRect.Bottom;
      end
    else
      begin
        P[0]:= aRect.CenterPoint;
        P[0].X:= aRect.Left;
        P[1]:= aRect.BottomRight;
        P[2]:= P[1];
        P[2].Y:= aRect.Top;
      end;
  end;
  aCanvas.Polygon(P);
end;

procedure DrawCheckedBox(aCanvas: TCanvas; const aRect: TRect; const aValue: boolean; const aColor, aBackgroundColor, aBorderColor : TColor);
var
  oldBrushColor, oldColor: TColor;
  p1, p2, p3 : TPoint;
  bw : integer;
  tmpRect : TRect;
begin
  oldBrushColor := aCanvas.Brush.Color;
  oldColor := aCanvas.Pen.Color;
  aCanvas.Brush.Color:= aBackgroundColor;
  tmpRect := aRect;
  aCanvas.FillRect(tmpRect);
  bw := 2;
  aCanvas.Pen.Width:= bw;
  aCanvas.Pen.Color:= aBorderColor;
  aCanvas.Rectangle(tmpRect);
  if aValue then
  begin
    aCanvas.Pen.Color:= aColor;
    bw := max(3, tmpRect.Width div 5);
    InflateRect(tmpRect, -1 * bw, -1 * bw);
    p1.X := tmpRect.Left;
    p1.Y := tmpRect.Top + (tmpRect.Height div 2);
    p2.X := tmpRect.Left + (tmpRect.Width div 2) - (bw div 2);
    p2.Y := tmpRect.Bottom - (bw div 2);
    p3.X := tmpRect.Right - (bw div 2);
    p3.Y := tmpRect.Top;
    aCanvas.Pen.Width:= bw;
    aCanvas.Line(p1, p2);
    aCanvas.Line(p2, p3);
  end;
  aCanvas.Brush.Color := oldBrushColor;
  aCanvas.Pen.Color:= oldColor;
end;

{$IFDEF FPC}
  procedure RGBToHLS(aColor: TColor; var H, L, S: Double);
  var
    HW, SW, LW: Word;
  begin
    ColorRGBToHLS(ColorToRgb(aColor), HW, LW, SW);
    H := HW / HLSMAX;
    L := LW / HLSMAX;
    S := SW / HLSMAX;
  end;

  function HLSToRGB(H, L, S: Double): TColor;
  begin
    Result := ColorHLSToRGB(Round(H * HLSMAX),
      Round(L * HLSMAX),
      Round(S * HLSMAX))
  end;

  function GetLuminance(aColor: TColor): double;
  var
    H, S, L: Double;
  begin
    RGBToHLS(ColorToRgb(aColor), H, L, S);
    Result := L;
  end;

  function SetLuminance(aColor: TColor; luminance: double): TColor;
  var
    H, S, L: double;
  begin
    RGBToHLS(ColorToRgb(aColor), H, L, S);
    luminance := Max(0, Min(luminance, 1));
    Result := HLSToRGB(H, luminance, S);
  end;

{$ELSE}

  function GetLuminance(aColor: TColor): double;
  var
    H, S, L: Single;
  begin
    RGBtoHSL(ColorToRgb(aColor), H, S, L);
    Result := L;
  end;

  function SetLuminance(aColor: TColor; luminance: double): TColor;
  var
    H, S, L: Single;
  begin
    RGBtoHSL(ColorToRgb(aColor), H, S, L);
    luminance := Max(0, Min(luminance, 1));
    Result := HSLtoRGB(H, S, luminance);
  end;

{$ENDIF}


  function DarkerColor(aColor: TColor; percent: Byte): TColor;
  var
    l: double;
  begin
    l := Max(GetLuminance(aColor) - (percent/100), 0.0);
    Result := SetLuminance(aColor, l);
  end;

  {$ifdef unix}

  // https://www.developpez.net/forums/d545432/autres-langages/pascal/lazarus/recherche-l-equivalent-getrvalue-sous-linux/

  function _GetRValue(RGB: TColor): Byte;
  begin
    Result := Byte(RGB);
  end;

  function _GetGValue(RGB: TColor): Byte;
  begin
    Result := Byte(RGB shr 8);
  end;

  function _GetBValue(RGB: TColor): Byte;
  begin
    Result := Byte(RGB shr 16);
  end;
  {$endif}

  (* lighter color of thisColor by thePercent value *)
  function LighterColor(aColor: TColor; percent: Byte): TColor;
  var
    cRed,
    cGreen,
    cBlue: Byte;
  begin
    cRed := {$ifdef unix}_GetRValue{$else}GetRValue{$endif}(aColor);
    cGreen := {$ifdef unix}_GetGValue{$else}GetGValue{$endif}(aColor);
    cBlue := {$ifdef unix}_GetBValue{$else}GetBValue{$endif}(aColor);
    (* a byte's range is from 0 to 255
       so Red, Green and Blue can have
       a value between 0 and 255 *)
    cRed :=
      Round(cRed * percent / 100) +
      Round(255 - percent / 100 * 255);
    cGreen :=
      Round(cGreen * percent / 100) +
      Round(255 - percent / 100 * 255);
    cBlue :=
      Round(cBlue * percent / 100) +
      Round(255 - percent / 100 * 255);
    Result := RGB(cRed, cGreen, cBlue);
  end;

  function IsDark(const aColor: TColor): boolean;
  var
    cRed, cGreen, cBlue: Byte;
    s : integer;
  begin
    cRed := GetRValue(aColor);
    cGreen := GetGValue(aColor);
    cBlue := GetBValue(aColor);
    s := (cRed + cGreen + cBlue);
    Result := ((128 * 3) - s) > 0;
  end;

  function ScaleForDPI(const aValue: integer): integer;
  begin
    Result := aValue;

    if Screen.PixelsPerInch=96 then exit;


    Result := round(aValue * (Screen.PixelsPerInch / 96));
  end;

procedure GetScreenShot (aBitmap : Graphics.TBitmap);
var
  DC : {$IFDEF LINUX}LCLType.{$ENDIF}HDC;
begin
  DC := {$IFDEF LINUX}LCLIntf.GetDC(0){$ELSE}GetDC (GetDesktopWindow){$ENDIF};
  try
    {$IFDEF LINUX}
    aBitmap.LoadFromDevice(DC);
    {$ELSE}
    aBitmap.Width := GetDeviceCaps (DC, HORZRES) ;
    aBitmap.Height := GetDeviceCaps (DC, VERTRES) ;
    BitBlt(aBitmap.Canvas.Handle, 0, 0, aBitmap.Width, aBitmap.Height, DC, 0, 0, SRCCOPY) ;
    {$ENDIF}
  finally
    {$IFDEF LINUX}
    LCLIntf.ReleaseDC(0, DC);
    {$ELSE}
    ReleaseDC (GetDesktopWindow, DC) ;
    {$ENDIF}
  end;
end;

procedure CropBitmap (aSourceBitmap, aDestBitmap : Graphics.TBitmap; X, Y : integer);
begin
  BitBlt(aDestBitmap.Canvas.Handle, 0, 0, aDestBitmap.Width, aDestBitmap.Height, aSourceBitmap.Canvas.Handle, X, Y, SRCCOPY);
end;

function Orientation(const x1, y1, x2, y2, Px, Py: Double): Integer;
var
  Orin: Double;
begin
  (* Linear determinant of the 3 points *)
  Orin := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);

  if Orin > 0.0 then
    Result := +1             (* Orientaion is to the right-hand side *)
  else if Orin < 0.0 then
    Result := -1             (* Orientaion is to the left-hand side  *)
  else
    Result := 0;             (* Orientaion is neutral aka collinear  *)
end;
(* End of Orientation *)


function PointInTriangle (pt, v1, v2, v3 : TPoint) : boolean;
var
  Or1: Integer;
  Or2: Integer;
  Or3: Integer;
begin
  Or1 := Orientation(v1.x, v1.y, v2.x, v2.y, pt.x, pt.y);
  Or2 := Orientation(v2.x, v2.y, v3.x, v3.y, pt.x, pt.y);
  Or3 := Orientation(v3.x, v3.y, v1.x, v1.y, pt.x, pt.y);

  if (Or1 = Or2) and (Or2 = Or3) then
    Result := True
  else if Or1 = 0 then
    Result := (Or2 = 0) or (Or3 = 0)
  else if Or2 = 0 then
    Result := (Or1 = 0) or (Or3 = 0)
  else if Or3 = 0 then
    Result := (Or2 = 0) or (Or1 = 0)
  else
    Result := False;
end;

function RectangleSide (aRect : TRect; aX, aY : integer) : TRectangleSide;
var
  MidX, MidY : integer;
  TempPoint, V1, V2, V3 : TPoint;
begin
  if (aX < aRect.Left) or (aX > aRect.Right) or (aY < aRect.Top) or (aY > aRect.Bottom) then
  begin
    Result := rsOutside;
    exit;
  end; 
 
  MidX := aRect.Left + ((aRect.Right - aRect.Left) div 2);
  MidY := aRect.Top + ((aRect.Bottom - aRect.Top) div 2);
  TempPoint.X := aX;
  TempPoint.Y := aY;
  V1.X := MidX;
  V1.Y := MidY;
  V2.X := aRect.Left;
  V2.Y := aRect.Top;
  V3.X := aRect.Left;
  V3.Y := aRect.Bottom;
  
  if PointInTriangle(TempPoint, V1, V2, V3)  then
  begin
    Result := rsLeft;
    exit;
  end;

  V3.X := aRect.Right;
  V3.Y := aRect.Top;
  if PointInTriangle(TempPoint, V1, V2, V3) then
  begin
    Result := rsTop;
    exit;    
  end;

  V2.X := aRect.Right;
  V2.Y := aRect.Bottom;
  if PointInTriangle(TempPoint, V1, V2, V3) then
  begin
    Result := rsRight;
    exit;    
  end;

  Result := rsBottom;  
end;

// Copies bitmap onto ACanvas
procedure CopyDraw(ACanvas: TCanvas; Bitmap: TBitmap; const ARect: TRect);
begin
  BitBlt(ACanvas.Handle,
         ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top,
         Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

function CtrlPressed: boolean;
begin
  Result := ({$IFNDEF FPC}GetAsyncKeyState{$ELSE}GetKeyState{$ENDIF}(VK_CONTROL) and $8000 <> 0)
end;

function ShiftPressed: boolean;
begin
Result := ({$IFNDEF FPC}GetAsyncKeyState{$ELSE}GetKeyState{$ENDIF}(VK_SHIFT) and $8000 <> 0)
end;

function InternalGenerateThumbnailOfImage(const aSourceFile, aThumbnailFile: String; const aMaxWidth, aMaxHeight: word; const aPng : boolean; out aError: String): boolean;
var
  sourcePicture : TPicture;
  thumbnailPNG : {$IFDEF FPC} TPortableNetworkGraphic {$ELSE} TPngImage {$ENDIF};
  thumbnailJPG : TJPEGImage;
  tmb : TCustomBitmap;
  rateWidth, rateHeight : Extended;
  r : TRect;
begin
  Result := true;
  try
    sourcePicture := TPicture.Create;
    thumbnailPNG := nil;
    thumbnailJPG := nil;
    if aPng then
    begin
      {$IFDEF FPC}
      thumbnailPNG := TPortableNetworkGraphic.Create;
      {$ELSE}
      thumbnailPNG := TPngImage.Create;
      {$ENDIF}
    end
    else
      thumbnailJPG := TJPEGImage.Create;
    try
      sourcePicture.LoadFromFile(aSourceFile);
      rateWidth := aMaxWidth / sourcePicture.Width;
      rateHeight := aMaxHeight / sourcePicture.Height;
      if rateWidth > rateHeight then
        rateWidth := rateHeight;

      if aPng then
        tmb := thumbnailPNG
      else
        tmb := thumbnailJPG;

      tmb.SetSize(round(sourcePicture.Width * rateWidth), round(sourcePicture.Height * rateHeight));
      tmb.Canvas.Brush.Color:= clWhite;
      r := Rect(0, 0, thumbnailPNG.Width, thumbnailPNG.Height);
      tmb.Canvas.FillRect(r);
      {$IFDEF FPC}
      tmb.Canvas.AntialiasingMode := amON;
      {$ENDIF}
      tmb.Canvas.StretchDraw(r, sourcePicture.Graphic);
      tmb.SaveToFile(aThumbnailFile);
    finally
      sourcePicture.Free;
      FreeAndNil(thumbnailPNG);
      FreeAndNil(thumbnailJPG);
    end;
  except
    on e: Exception do
    begin
      aError := e.Message;
      Result := false;
    end;
  end;
end;

function GeneratePNGThumbnailOfImage(const aSourceFile, aThumbnailFile: String; const aMaxWidth, aMaxHeight: word;out aError: String): boolean;
begin
  Result := InternalGenerateThumbnailOfImage(aSourceFile, aThumbnailFile, aMaxWidth, aMaxHeight, true, aError);
end;

function GenerateJPGThumbnailOfImage(const aSourceFile, aThumbnailFile: String; const aMaxWidth, aMaxHeight: word;out aError: String): boolean;
begin
  Result := InternalGenerateThumbnailOfImage(aSourceFile, aThumbnailFile, aMaxWidth, aMaxHeight, false, aError);
end;

// https://stackoverflow.com/questions/20129758/algorithm-to-randomly-generate-a-color-palette-in-delphi
function GenerateRandomColor(const Mix: TColor = clWhite): TColor;
var
  Red, Green, Blue: Integer;
begin
  Red := Random(256);
  Green := Random(256);
  Blue := Random(256);

  Red := (Red + GetRValue(ColorToRGB(Mix))) div 2;
  Green := (Green + GetGValue(ColorToRGB(Mix))) div 2;
  Blue := (Blue + GetBValue(ColorToRGB(Mix))) div 2;
  Result := RGB(Red, Green, Blue);
end;

// https://github.com/Alexey-T/ATSynEdit/blob/master/atsynedit/atstringproc.pas
procedure CopyTextToClipboard(aText: string);
begin
  {$IFDEF FPC}
    {$IFDEF LCLGTK2}
    //Workaround for Lazarus bug #0021453. LCL adds trailing zero to clipboard in Clipboard.AsText.
    if aText <> '' then
    begin
      Clipboard.AddFormat(PredefinedClipboardFormat(pcfText), aText[1], Length(aText));
      Clipboard.AsText := aText;
      {$IFNDEF WINDOWS}
      PrimarySelection.AsText := aText;
      if Clipboard.AsText <> aText then
      begin
        Clipboard.AsText := aText;
        PrimarySelection.AsText := aText;
      end;
      {$ENDIF}
    end else
      Clipboard.AsText := '';

    {$ELSE}
    Clipboard.AsText := aText;
    {$ENDIF}
  {$ELSE}
  Clipboard.AsText := aText;
  {$ENDIF}
end;

procedure FlashInWindowsTaskbar(const aFlashEvenIfActive : boolean);
begin
  {$IFDEF WINDOWS}
  {$IFDEF FPC}{$push}{$warnings off}{$ENDIF}
  begin
  // http://forum.lazarus.freepascal.org/index.php?topic=33574.0
  If aFlashEvenIfActive or (not Application.Active) Then
    FlashWindow({$IFDEF FPC}WidgetSet.AppHandle{$ELSE}Application.Handle{$ENDIF}, True);
  end;
  {$IFDEF FPC}{$pop}{$ENDIF}
  {$ENDIF}
end;


{$IFNDEF GUI}
** This unit should not be compiled in a console application **
{$ENDIF}

initialization
  _TextExtendBitmap := nil;

finalization
  FreeAndNil(_TextExtendBitmap);
end.
