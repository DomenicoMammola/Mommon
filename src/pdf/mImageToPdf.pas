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

uses
  fppdf;

type
  TConvertedPdfOrientation = (cpoPortrait, cpoLandscape, cpoAdapt);

function ConvertImageToPdf(const aSourceImageFile, aDestinationPdfFile : String; const aEnlargeToPage : boolean; const aIgnoreBorders : boolean; const aOrientation : TConvertedPdfOrientation; out aActualOrientation : TConvertedPdfOrientation; const aPaperType : TPDFPaperType = ptA4; const aWidth : integer = 0; const aHeight : integer = 0): boolean; overload;
function ConvertImageToPdf(const aSourceImageFile, aDestinationPdfFile : String; const aEnlargeToPage : boolean; const aIgnoreBorders : boolean): boolean; overload;

procedure CreateSinglePageEmpyPdf(const aPdfFile: String; const aWidth, aHeight : integer); overload;
procedure CreateSinglePageEmpyPdf(const aPdfFile: String; const aPaperType : TPDFPaperType; const aPortraitOrientation : boolean); overload;


implementation


uses
  SysUtils, Math,
  fpreadjpeg,
  fpparsettf;

function ConvertImageToPdf(const aSourceImageFile, aDestinationPdfFile: String; const aEnlargeToPage : boolean; const aIgnoreBorders : boolean; const aOrientation : TConvertedPdfOrientation;
  out aActualOrientation : TConvertedPdfOrientation; const aPaperType : TPDFPaperType = ptA4; const aWidth : integer = 0; const aHeight : integer = 0): boolean;
var
  doc : TPDFDocument;
  page : TPDFPage;
  sec : TPDFSection;
  img, w, h, pageWidth, pageHeight, imgWidth, imgHeight, margin : integer;
  PP: TPDFPaper;
begin
  Result := false;
  doc := TPDFDocument.Create(Nil);
  try
    doc.Infos.CreationDate:= Now;
    doc.Options:= [poCompressFonts, poCompressText, poCompressImages, poUseRawJPEG];
    doc.StartDocument;
    sec := doc.Sections.AddSection; // we always need at least one section

    img := doc.Images.AddFromFile(aSourceImageFile);

    imgWidth := doc.Images[0].Width;
    imgHeight :=  doc.Images[0].Height;

    page := doc.Pages.AddPage;
    if aPaperType = ptCustom then
    begin
      PP.H:= aHeight;
      PP.W:= aWidth;
      if aIgnoreBorders then
      begin
        PP.Printable.T:= 0;
        PP.Printable.L:= 0;
        PP.Printable.R:= aWidth;
        PP.Printable.B:= aHeight;
      end
      else
      begin
        margin := min(10, trunc(aWidth * 0.035));
        margin := min(margin, trunc(aHeight * 0.035));
        PP.Printable.T:= margin;
        PP.Printable.L:= margin;
        PP.Printable.R:= aWidth - margin;
        PP.Printable.B := aHeight - margin;
      end;
      page.Paper := PP;
      page.PaperType:= ptCustom;
    end
    else
      page.PaperType:= aPaperType;
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

procedure _CreateSinglePageEmpyPdf(const aPdfFile: String; const aPaperType : TPDFPaperType; const aPortraitOrientation : boolean; const aWidth : integer = 0; const aHeight : integer = 0);
var
  doc : TPDFDocument;
  page : TPDFPage;
  sec : TPDFSection;
  PP: TPDFPaper;
begin
  doc := TPDFDocument.Create(Nil);
  try
    doc.Infos.CreationDate:= Now;
    doc.Options:= [poCompressFonts, poCompressText, poCompressImages];
    doc.StartDocument;
    sec := doc.Sections.AddSection; // we always need at least one section

    page := doc.Pages.AddPage;
    page.UnitOfMeasure:= uomPixels;

    if (aWidth = 0) then
    begin
      page.PaperType:= aPaperType;
      if aPortraitOrientation then
        page.Orientation:= ppoPortrait
      else
        page.Orientation:= ppoLandscape;
    end
    else
    begin
      PP.H:=aHeight;
      PP.W:=aWidth;
      PP.Printable.T:=10;
      PP.Printable.L:=11;
      PP.Printable.R:=aWidth - 11;
      PP.Printable.B:=aHeight - 10;
      page.Paper := PP;
      page.PaperType:= ptCustom;
    end;

    sec.AddPage(page);

    doc.SaveToFile(aPdfFile);
  finally
    doc.Free;
  end;
end;


procedure CreateSinglePageEmpyPdf(const aPdfFile: String; const aWidth, aHeight: integer);
begin
  _CreateSinglePageEmpyPdf(aPdfFile, ptCustom, false, aWidth, aHeight);
end;

procedure CreateSinglePageEmpyPdf(const aPdfFile: String; const aPaperType: TPDFPaperType; const aPortraitOrientation: boolean);
begin
  _CreateSinglePageEmpyPdf(aPdfFile, aPaperType, aPortraitOrientation);
end;

end.
