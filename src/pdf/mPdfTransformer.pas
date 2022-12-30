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
unit mPdfTransformer;

interface

uses
  fpPDF,
  mProgress;

function GetPageType (const aWidth, aHeight : integer): TPDFPaperType;

function ReduceSizeOfPdfWithPoppler (const aSourceFileName, aDestinationFileName : String; aProgress: ImProgress; out aReductionPerc : integer; out aError : String; const aResolution : integer = 72) : boolean;

function ReduceSizeOfPdfWithGhostscript (const aSourceFileName, aDestinationFileName : String; aProgress: ImProgress; out aReductionPerc : integer; out aError : String; const aResolution : integer = 72) : boolean;

implementation

uses
  FileUtil, SysUtils, Classes, Math,
  mPoppler, mUtility, mImageToPdf, mPdfMerger, mGhostscript;

function GetPageType(const aWidth, aHeight: integer): TPDFPaperType;
var
  d1, d2 : integer;
  diff1, diff2 : Double;
  pt : TPDFPaperType;
begin
  Result := ptCustom;

  d1 := Max(aWidth, aHeight);
  d2 := Min(aWidth, aHeight);

  for pt := Low(TPDFPaperType) to High(TPDFPaperType) do
  begin
    diff1 := abs(d1 - PDFPaperSizes[pt][0]);
    diff2 := abs(d2 - PDFPaperSizes[pt][1]);
    if (diff1 <= 3) and (diff2 <= 3) then
    begin
      Result := pt;
      exit;
    end;
  end;
end;

function ReduceSizeOfPdfWithGhostscript(const aSourceFileName, aDestinationFileName: String; aProgress: ImProgress; out aReductionPerc: integer; out aError: String; const aResolution: integer): boolean;
var
  sizeSource, sizeDest : integer;
begin
  aProgress.Notify('Running optimizer...');
  Result := TGhostscriptToolbox.CompressPdf(aSourceFileName, aDestinationFileName, aResolution);
  if not Result then
  begin
    aError:= TGhostscriptToolbox.GetLastError;
  end
  else
  begin
    sizeSource := FileSize(aSourceFileName);
    sizeDest := FileSize(aDestinationFileName);

    aReductionPerc:= max(0, trunc ((sizeSource - sizeDest) / (sizeSource/100)));
  end;
end;

function ReduceSizeOfPdfWithPoppler(const aSourceFileName, aDestinationFileName: String; aProgress: ImProgress; out aReductionPerc : integer; out aError: String; const aResolution : integer): boolean;
var
  tmpFolderPages, tmpFolderJpeg, tmpFolderTemp : String;
  pdfinfo, pagePdfInfo : TPopplerPdfInfo;
  i, numOfImages : integer;
  pdfPagesFiles, jpegFiles : TStringList;

  orientation, outOrientation : TConvertedPdfOrientation;
  pt : TPDFPaperType;
  tmpSourceFile, tmpDestinationFile : string;
  filesToBeMerged : TStringList;
  sizeSource, sizeDest : integer;
begin
  Result := false;

  if not TPopplerToolbox.GetInfoFromPdf(aSourceFilename, pdfInfo) then
  begin
    aError := TPopplerToolbox.GetLastError;
    exit;
  end;

  aReductionPerc:= 0;

  tmpFolderPages := GetUniqueTemporaryFolder;

  if not DirectoryExists(tmpFolderPages) then
    ForceDirectories(tmpFolderPages);
  try
    aProgress.Notify('Extracting pages...');
    if not TPopplerToolbox.SplitPdfInPages(aSourceFileName, tmpFolderPages, 'page_%d.pdf') then
    begin
      aError := TPopplerToolbox.GetLastError;
      exit;
    end;

    tmpFolderJpeg:= IncludeTrailingPathDelimiter(tmpFolderPages) + 'jpeg';
    if not DirectoryExists(tmpFolderJpeg) then
      ForceDirectories(tmpFolderJpeg);

    tmpFolderTemp := IncludeTrailingPathDelimiter(tmpFolderPages) + 'temp';
    if not DirectoryExists(tmpFolderTemp) then
      ForceDirectories(tmpFolderTemp);

    filesToBeMerged := TStringList.Create;
    pdfPagesFiles := TStringList.Create;
    jpegFiles := TStringList.Create;
    try
      for i := 1 to pdfinfo.Pages do
      begin
        aProgress.Notify('Processing page #' + IntToStr(i) + '...');
        tmpSourceFile:= IncludeTrailingPathDelimiter(tmpFolderPages) + 'page_' + IntToStr(i) + '.pdf';

        numOfImages := 0;
        if not TPopplerToolbox.GetImagesInfoFromPdf(tmpSourceFile, numOfImages) then
        begin
          aError := TPopplerToolbox.GetLastError;
          exit;
        end;

        if numOfImages = 0 then
        begin
          filesToBeMerged.Add(tmpSourceFile);
        end
        else
        begin
          if not TPopplerToolbox.GetInfoFromPdf(tmpSourceFile, pagePdfInfo) then
          begin
            aError := TPopplerToolbox.GetLastError;
            exit;
          end;

          tmpDestinationFile:= 'page' + IntToStr(i);
          if not TPopplerToolbox.ExtractPagesFromPdfAsJpeg(tmpSourceFile, tmpFolderJpeg, tmpDestinationFile, 60, aResolution) then
          begin
            aError := TPopplerToolbox.GetLastError;
            exit;
          end;

          tmpDestinationFile:= IncludeTrailingPathDelimiter(tmpFolderJpeg) + tmpDestinationFile + '-1.jpg';

          if pagePdfInfo.PageWidth > pagePdfInfo.PageHeight then
            orientation:= cpoLandscape
          else
            orientation:= cpoPortrait;

          pt := GetPageType(pagePdfInfo.PageWidth, pagePdfInfo.PageHeight);

          if not ConvertImageToPdf(tmpDestinationFile, ChangeFileExt(tmpDestinationFile, '.pdf'), true, true, orientation, outOrientation, pt, pagePdfInfo.PageWidth, pagePdfInfo.PageHeight) then
            exit;

          tmpDestinationFile:= ChangeFileExt(tmpDestinationFile, '.pdf');


          if FileSize(tmpDestinationFile) < FileSize(tmpSourceFile) then
            filesToBeMerged.Add(tmpDestinationFile)
          else
            filesToBeMerged.Add(tmpSourceFile);
        end;
      end;

      aProgress.Notify('Merging all together...');
      if not MergeFiles(filesToBeMerged, tmpFolderTemp, aDestinationFileName, aError) then
        exit;

      sizeSource := FileSize(aSourceFileName);
      sizeDest := FileSize(aDestinationFileName);

      aReductionPerc:= max(0, trunc ((sizeSource - sizeDest) / (sizeSource/100)));

      aProgress.Notify('Cleaning...');
    finally
      jpegFiles.Free;
      pdfPagesFiles.Free;
      filesToBeMerged.Free;
    end;

  finally
    try
      DeleteDirectory(tmpFolderPages, false);
    except
      // ignored
    end;
  end;

  Result := true;
end;

end.
