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
unit mThumbnails;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I mDefines.inc}

interface

uses
  Classes;

function GeneratePNGThumbnail (const aSourceFile, aThumbnailsFolder : String; const aMaxWidth, aMaxHeight: word; out aThumbnailFileName: String; out aError: String): boolean; overload;
function GeneratePNGThumbnail (const aSourceFileName, aThumbnailsFolder : String; const aSourceData : TStream; const aMaxWidth, aMaxHeight: word; out aThumbnailData: TMemoryStream; out aError: String): boolean; overload;


implementation

uses
  sysutils,
  mUtility, mXPdf, mLog,
  {$IFDEF NOGUI}
  mGraphicsUtilityNoGUI
  {$ELSE}
  Graphics, mGraphicsUtility
  {$ENDIF};

var
  logger : TmLog;

function GeneratePNGThumbnail(const aSourceFile, aThumbnailsFolder: String; const aMaxWidth, aMaxHeight: word; out aThumbnailFileName: String; out aError: String): boolean;
var
  {$IFNDEF NOGUI}
  GraphicClass: TGraphicClass;
  {$ENDIF}
  ext : String;
  isImage, isPdf : boolean;
begin
  Result := false;
  ext := LowerCase(ExtractFileExt(aSourceFile));

  {$IFDEF NOGUI}
  isImage := (ext = '.png') or (ext = '.bmp')
    or (ext = '.jpg') or (ext = '.jpeg')
    or (ext = '.gif');
  {$ELSE}
  isImage := (GetGraphicClassForFileExtension(ext) <> nil);
  {$ENDIF}
  isPdf := (ext = '.pdf');

  if not (isImage or isPdf) then
    exit;

  aThumbnailFileName := ChangeFileExt(IncludeTrailingPathDelimiter(aThumbnailsFolder) + ExtractFileName(aSourceFile) + GenerateRandomIdString(5), '.png');

  if isPdf then
  begin
    try
      //Result := TMutoolToolbox.ExtractThumbnailOfFrontPageFromPdf(aSourceFile, aThumbnailFileName, aMaxWidth, aMaxHeight);
      Result := TXPdfToolbox.ExtractThumbnailOfFrontPageFromPdf(aSourceFile, aThumbnailFileName, aMaxWidth, aMaxHeight);
    except
      on e: Exception do
      begin
        aError := e.Message;
        exit;
      end;
    end;
  end
  else
  begin
    try
      Result := GeneratePNGThumbnailOfImage(aSourceFile, aThumbnailFileName, aMaxWidth, aMaxHeight, aError);
    except
      on e: Exception do
      begin
        Result := false;
        aError := e.Message;
        exit;
      end;
    end;
  end;
end;

function GeneratePNGThumbnail(const aSourceFileName, aThumbnailsFolder: String; const aSourceData: TStream; const aMaxWidth, aMaxHeight: word; out aThumbnailData: TMemoryStream; out aError: String): boolean;
var
  {$IFNDEF NOGUI}
  GraphicClass: TGraphicClass;
  {$ENDIF}
  ext, tmpSourceFileName, tmpThumbnailFileName : String;
  isImage, isPdf : boolean;
  ms : TMemoryStream;
begin
  Result := false;
  aThumbnailData := nil;
  ext := LowerCase(ExtractFileExt(aSourceFileName));

  {$IFDEF NOGUI}
  isImage := (ext = '.png') or (ext = '.bmp')
    or (ext = '.jpg') or (ext = '.jpeg')
    or (ext = '.gif');
  {$ELSE}
  isImage := (GetGraphicClassForFileExtension(ext) <> nil);
  {$ENDIF}
  isPdf := (ext = '.pdf');

  if not (isImage or isPdf) then
    exit;

  tmpThumbnailFileName := ChangeFileExt(IncludeTrailingPathDelimiter(aThumbnailsFolder) + ExtractFileName(aSourceFileName) + GenerateRandomIdString(5), '.png');

  ms := TMemoryStream.Create();
  try
    tmpSourceFileName := IncludeTrailingPathDelimiter(aThumbnailsFolder) + ExtractFileName(aSourceFileName);
    aSourceData.Position:= 0;
    ms.LoadFromStream(aSourceData);
    ms.SaveToFile(tmpSourceFileName);
    logger.Debug('Saved ' + tmpSourceFileName);
  finally
    ms.Free;
  end;

  try
    if isPdf then
    begin
      try
        Result := TXPdfToolbox.ExtractThumbnailOfFrontPageFromPdf(tmpSourceFileName, tmpThumbnailFileName, aMaxWidth, aMaxHeight);
        logger.Debug('Generated thumbnail for pdf file: ' + tmpThumbnailFileName);
      except
        on e: Exception do
        begin
          aError := e.Message;
          logger.Debug(e.Message);
          exit;
        end;
      end;
    end
    else
    begin
      Result := GeneratePNGThumbnailOfImage(tmpSourceFileName, tmpThumbnailFileName, aMaxWidth, aMaxHeight, aError);
      logger.Debug('Generated thumbnail for image file: ' + tmpThumbnailFileName);
    end;
    if Result then
    begin
      aThumbnailData := TMemoryStream.Create;
      aThumbnailData.LoadFromFile(tmpThumbnailFileName);
      aThumbnailData.Position:= 0;
      DeleteFile(tmpThumbnailFileName);
    end;
  finally
    DeleteFile(tmpSourceFileName);
  end;
end;

initialization
  logger := logManager.AddLog('mThumbnails');
end.
