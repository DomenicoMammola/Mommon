unit mPdfiumLib;

interface

uses
  Graphics;

resourcestring
  SPdfiumLib_error_file_missing = 'Pdf file is missing: %s';
  SPdfiumLib_error_empty_pdf = 'Pdf file has no content: %s';
  SPdfiumLib_error_unknown_error = 'Unknown error: %s';


type

  { TPdfiumLibToolbox }

  TPdfiumLibToolbox = class
  public
    class function ExtractThumbnailOfFrontPageFromPdfAsPng(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight : word;
      const aAdaptPngSizeToPageSize: boolean; const aBackgroundColor : TColor; out aError : String) : boolean;
  end;

implementation

uses
  SysUtils, Math,
  PdfiumCore;


{ TPdfiumLibToolbox }

class function TPdfiumLibToolbox.ExtractThumbnailOfFrontPageFromPdfAsPng(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight: word; const aAdaptPngSizeToPageSize: boolean;
  const aBackgroundColor : TColor; out aError : String): boolean;
var
  doc : TPdfDocument;
  btm : TBitmap;
  relPage, relViewport : Double;
  pageWidth, pageHeight : integer;
  X, Y : integer;
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
      btm := TBitmap.Create;
      try
        if aAdaptPngSizeToPageSize then
        begin
          X := 0;
          Y := 0;
          if doc.Pages[0].Width > doc.Pages[0].Height then
          begin
            btm.Width:= aWidth;
            pageWidth:= aWidth;
            relPage:= doc.Pages[0].Height / doc.Pages[0].Width;
            btm.Height:= trunc(aWidth * relPage);
            pageHeight:= trunc(pageWidth * relPage);
          end
          else
          begin
            btm.Height:= aHeight;
            pageHeight:= aHeight;
            relPage:= doc.Pages[0].Width / doc.Pages[0].Height;
            btm.Width:= trunc(aHeight * relPage);
            pageWidth:= trunc(pageHeight * relPage);
          end;
        end
        else
        begin
          btm.Width:= aWidth;
          btm.Height:= aHeight;

          btm.Canvas.Brush.Color:= aBackgroundColor;
          btm.Canvas.FillRect(btm.Canvas.ClipRect);

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
        doc.Pages[0].DrawToCanvas(btm.Canvas, X, Y, pageWidth, pageHeight, prNormal, [], aBackgroundColor);
        btm.SaveToFile(aThumbnailFileName);
      finally
        btm.Free;
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

end.
