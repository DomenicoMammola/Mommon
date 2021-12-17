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
unit mImageToPdf;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type
  TConvertedPdfOrientation = (cpoPortrait, cpoLandscape, cpoAdapt);

function ConvertImageToPdf(const aSourceImageFile, aDestinationPdfFile : String; const aEnlargeToPage : boolean; const aIgnoreBorders : boolean; const aOrientation : TConvertedPdfOrientation; out aActualOrientation : TConvertedPdfOrientation): boolean; overload;
function ConvertImageToPdf(const aSourceImageFile, aDestinationPdfFile : String; const aEnlargeToPage : boolean; const aIgnoreBorders : boolean): boolean; overload;

procedure CreateSinglePageEmpyPdf(const aPdfFile: String; const aPortraitOrientation : boolean = true);

implementation


uses
  sysutils,
  fpreadjpeg,
  fppdf,
  fpparsettf;

function ConvertImageToPdf(const aSourceImageFile, aDestinationPdfFile: String; const aEnlargeToPage : boolean; const aIgnoreBorders : boolean; const aOrientation : TConvertedPdfOrientation; out aActualOrientation : TConvertedPdfOrientation): boolean;
var
  doc : TPDFDocument;
  page : TPDFPage;
  sec : TPDFSection;
  img, w, h, pageWidth, pageHeight, imgWidth, imgHeight : integer;
begin
  Result := false;
  doc := TPDFDocument.Create(Nil);
  try
    doc.Infos.CreationDate:= Now;
    doc.Options:= [poCompressFonts, poCompressText, poCompressImages];
    doc.StartDocument;
    sec := doc.Sections.AddSection; // we always need at least one section

    img := doc.Images.AddFromFile(aSourceImageFile);

    imgWidth := doc.Images[0].Width;
    imgHeight :=  doc.Images[0].Height;

    page := doc.Pages.AddPage;
    page.PaperType:= ptA4;
    page.UnitOfMeasure:= uomPixels;
    if aOrientation = cpoPortrait then
      page.Orientation:= ppoPortrait
    else if aOrientation = cpoLandscape then
      page.Orientation:= ppoLandscape
    else
    begin
      if imgWidth > imgHeight then
        page.Orientation:= ppoLandscape
      else
        page.Orientation:= ppoPortrait;
    end;

    if page.Orientation = ppoPortrait then
      aActualOrientation:= cpoPortrait
    else
      aActualOrientation:= cpoLandscape;

    sec.AddPage(page);

    if aIgnoreBorders then
    begin
      pageWidth := page.Paper.W;
      pageHeight := page.Paper.H;
    end
    else
    begin
      pageWidth := round(page.Paper.Printable.R - page.Paper.Printable.L);
      pageHeight := round(page.Paper.Printable.B - page.Paper.Printable.T);
    end;
    imgWidth := doc.Images[0].Width;
    imgHeight :=  doc.Images[0].Height;

    if aEnlargeToPage then
    begin
      page.DrawImage(1, 1, pageWidth, pageHeight, img);
    end
    else
    begin
      w := imgWidth;
      h := imgHeight;
      if imgWidth > pageWidth then
      begin
        h := round(h * (pageWidth / imgWidth));
        w := pageWidth;
        if h > pageHeight then
        begin
          w := round(w * (pageHeight / h));
          h := pageHeight;
        end;

      end;
      page.DrawImage(1, 1, w, h, img);
    end;

    doc.SaveToFile(aDestinationPdfFile);
  finally
    doc.Free;
  end;
  Result := true;
end;


function ConvertImageToPdf(const aSourceImageFile, aDestinationPdfFile : String; const aEnlargeToPage : boolean; const aIgnoreBorders : boolean): boolean; overload;
var
  tmpOrientation : TConvertedPdfOrientation;
begin
  Result := ConvertImageToPdf(aSourceImageFile, aDestinationPdfFile, aEnlargeToPage, aIgnoreBorders, cpoPortrait, tmpOrientation);
end;

procedure CreateSinglePageEmpyPdf(const aPdfFile: String; const aPortraitOrientation : boolean = true);
var
  doc : TPDFDocument;
  page : TPDFPage;
  sec : TPDFSection;
begin
  doc := TPDFDocument.Create(Nil);
  try
    doc.Infos.CreationDate:= Now;
    doc.Options:= [poCompressFonts, poCompressText, poCompressImages];
    doc.StartDocument;
    sec := doc.Sections.AddSection; // we always need at least one section

    page := doc.Pages.AddPage;
    page.PaperType:= ptA4;
    page.UnitOfMeasure:= uomPixels;
    if aPortraitOrientation then
      page.Orientation:= ppoPortrait
    else
      page.Orientation:= ppoLandscape;

    sec.AddPage(page);

    doc.SaveToFile(aPdfFile);
  finally
    doc.Free;
  end;
end;

end.
