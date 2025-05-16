// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (www.mammola.net)
//
unit mPdfiumLib;

interface

uses
  Graphics;

resourcestring
  SPdfiumLib_error_file_missing = 'Pdf file is missing: %s';
  SPdfiumLib_error_empty_pdf = 'Pdf file has no content: %s';
  SPdfiumLib_error_unknown_error = 'Unknown error: %s';
  SPdfiumLib_error_unsupported_image_format = 'Unsupported image format';


type

  { TPdfiumLibToolbox }

  TPdfiumLibToolbox = class
  public
    class function ExtractThumbnailOfFrontPageFromPdfAsImage(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight : word;
      const aAdaptPngSizeToPageSize: boolean; const aBackgroundColor : TColor; out aError : String) : boolean;
    class function AddImageToPdf(const aPdfFileName, aDestinationPdfFileName, aImageFileName : String; const aPageIndex, aX, aY, aRotation: integer; out aError : String) : boolean;
  end;

implementation

uses
  SysUtils, Math, IntfGraphics,
  PdfiumCore, PdfiumLib;


{ TPdfiumLibToolbox }

class function TPdfiumLibToolbox.ExtractThumbnailOfFrontPageFromPdfAsImage(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight: word; const aAdaptPngSizeToPageSize: boolean;
  const aBackgroundColor : TColor; out aError : String): boolean;
var
  doc : TPdfDocument;
  bmp : TBitmap;
  jpgImg : TJPEGImage;
  pngImg : TPortableNetworkGraphic;
  img : TFPImageBitmap;
  relPage, relViewport : Double;
  pageWidth, pageHeight : integer;
  X, Y : integer;
  ext : String;
begin
  Result := false;
  aError := '';

  if not FileExists(aPdfFileName) then
  begin
    aError := Format(SPdfiumLib_error_file_missing, [aPdfFileName]);
    exit;
  end;

  try
    doc := TPdfDocument.Create;
    try
      doc.LoadFromFile(aPdfFileName);
      if doc.PageCount < 1 then
      begin
        aError := Format(SPdfiumLib_error_empty_pdf, [aPdfFileName]);
        exit;
      end;

      ext := ExtractFileExt(aThumbnailFileName);

      pngImg := nil;
      jpgImg := nil;
      bmp := nil;

      if (CompareText(ext, '.png') = 0) then
      begin
        pngImg := TPortableNetworkGraphic.Create;
        img := pngImg;
      end
      else if (CompareText(ext, '.jpg') = 0) or (CompareText(ext, '.jpeg') = 0) then
      begin
        jpgImg := TJPEGImage.Create;
        img := jpgImg;
      end
      else
      begin
        bmp := TBitmap.Create;
        img := bmp;
      end;
      try
        if aAdaptPngSizeToPageSize then
        begin
          X := 0;
          Y := 0;
          if doc.Pages[0].Width > doc.Pages[0].Height then
          begin
            img.Width:= aWidth;
            pageWidth:= aWidth;
            relPage:= doc.Pages[0].Height / doc.Pages[0].Width;
            img.Height:= trunc(aWidth * relPage);
            pageHeight:= trunc(pageWidth * relPage);
          end
          else
          begin
            img.Height:= aHeight;
            pageHeight:= aHeight;
            relPage:= doc.Pages[0].Width / doc.Pages[0].Height;
            img.Width:= trunc(aHeight * relPage);
            pageWidth:= trunc(pageHeight * relPage);
          end;
        end
        else
        begin
          img.Width:= aWidth;
          img.Height:= aHeight;

          img.Canvas.Brush.Color:= aBackgroundColor;
          img.Canvas.FillRect(img.Canvas.ClipRect);

          relPage:= doc.Pages[0].Height / doc.Pages[0].Width;
          relViewport:= aHeight / aWidth;

          if (relViewport > relPage) then
          begin
            pageWidth := aWidth;
            pageHeight := min(aHeight, round(pageWidth * relPage));
            X := 0;
            Y := (aHeight - pageHeight) div 2;
          end
          else
          begin
            pageHeight := aHeight;
            pageWidth := min(aWidth, round(pageHeight / relPage));
            X := (aWidth - pageWidth) div 2;
            Y := 0;
          end;
        end;
        // doc.Pages[0].DrawToCanvas(img.Canvas, X, Y, pageWidth, pageHeight, prNormal, [], aBackgroundColor);
        img.SaveToFile(aThumbnailFileName);
      finally
        FreeAndNil(bmp);
        FreeAndNil(pngImg);
        FreeAndNil(jpgImg);
      end;

    finally
      doc.Free;
    end;
  except
    on e: Exception do
    begin
      aError:= Format(SPdfiumLib_error_unknown_error, [e.Message]);
      exit;
    end;
  end;

  Result := true;
end;

class function TPdfiumLibToolbox.AddImageToPdf(const aPdfFileName, aDestinationPdfFileName, aImageFileName : String; const aPageIndex, aX, aY, aRotation: integer; out aError : String) : boolean;
var
  doc : TPdfDocument;
  page_obj_image : FPDF_PAGEOBJECT;
  bmp : TPdfBitmap;
  matrix : FS_MATRIX;
  pngImg : TPortableNetworkGraphic;
  jpgImg : TJPEGImage;
  tmpLazImage : TLazIntfImage;
  ext : String;
  w, h : integer;
begin
  Result := false;
  aError := '';
  doc := TPdfDocument.Create;
  try
    doc.LoadFromFile(UnicodeString(aPdfFileName));
    if doc.PageCount < 1 then
    begin
      aError := Format(SPdfiumLib_error_empty_pdf, [aPdfFileName]);
      exit;
    end;

    ext := LowerCase(ExtractFileExt(aImageFileName));

    if (ext <> '.png') and (ext <> '.jpg') and (ext <> '.jpeg') then
    begin
      aError := SPdfiumLib_error_unsupported_image_format;
      exit;
    end;

    pngImg := TPortableNetworkGraphic.Create;
    jpgImg := TJPEGImage.Create;
    try
      if ext = '.png' then
      begin
        pngImg.LoadFromFile(aImageFileName);
        tmpLazImage := pngImg.CreateIntfImage;
        w := pngImg.Width;
        h := pngImg.Height;
      end
      else
      begin
        jpgImg.LoadFromFile(aImageFileName);
        tmpLazImage := jpgImg.CreateIntfImage;
        w := jpgImg.Width;
        h := jpgImg.Height;
      end;
      bmp := TPdfBitmap.Create(w, h, bfBGRA, tmpLazImage.PixelData, w *  4);
      try
        page_obj_image := FPDFPageObj_NewImageObj(doc.Handle);
        matrix.a:= w - 1;
        matrix.b:= aRotation;
        matrix.c:= -1 * aRotation;
        matrix.d:= w - 1;
        matrix.e := aX; // x
        matrix.f := aY; // y
        FPDFPageObj_SetMatrix(page_obj_image, @matrix);
        FPDFImageObj_SetBitmap(nil, 0, page_obj_image, bmp.Bitmap);
        FPDFPage_InsertObject(doc.Pages[aPageIndex].Handle, page_obj_image);
        FPDFPage_GenerateContent(doc.Pages[aPageIndex].Handle);
        doc.SaveToFile(UnicodeString(aDestinationPdfFileName));
      finally
        bmp.Free;
        tmpLazImage.Free;
      end;
    finally
      pngImg.Free;
      jpgImg.Free;
    end;
  finally
    doc.Free;
  end;
  Result := true;
end;

end.
