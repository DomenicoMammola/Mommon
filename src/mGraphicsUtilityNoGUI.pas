// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// It needs BGRABITMAP LIBRARY:
// https://github.com/bgrabitmap/bgrabitmap
//
unit mGraphicsUtilityNoGUI;

interface

  function GeneratePNGThumbnailOfImage(const aSourceFile, aThumbnailFile: String; const aMaxWidth, aMaxHeight: word;out aError: String): boolean;

implementation

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes, BGRAGraphics;

function GeneratePNGThumbnailOfImage(const aSourceFile, aThumbnailFile: String; const aMaxWidth, aMaxHeight: word; out aError: String): boolean;
var
  sourcePicture, thumbnail: TBGRABitmap;
  rateWidth, rateHeight : Extended;
  r : TRect;
begin
  Result := false;
  try
    sourcePicture := TBGRABitmap.Create(aSourceFile);
    try
      rateWidth := aMaxWidth / sourcePicture.Width;
      rateHeight := aMaxHeight / sourcePicture.Height;
      if rateWidth > rateHeight then
        rateWidth := rateHeight;
      thumbnail := TBGRABitmap.Create(round(sourcePicture.Width * rateWidth), round(sourcePicture.Height * rateHeight), BGRAWhite);
      try
        r := Rect(0, 0, thumbnail.Width, thumbnail.Height);
        thumbnail.CanvasBGRA.FillRect(r);
        thumbnail.CanvasBGRA.AntialiasingMode := amON;
        thumbnail.CanvasBGRA.StretchDraw(r, sourcePicture);
        thumbnail.SaveToFile(aThumbnailFile);
      finally
        thumbnail.Free;
      end;
      Result := true;
    finally
      sourcePicture.Free;
    end;
  except
    on e: Exception do
    begin
      aError := e.Message;
      Result := false;
    end;
  end;
end;

end.
