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
// ***********************************
// http://www.xpdfreader.com
// ***********************************
//
unit mXPdf;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I mDefines.inc}

interface

uses
  Classes, sysutils;

resourcestring
  SXPdf_error_file_missing = 'Pdf file is missing: ';
  SXPdf_pdftotext_error_unable_to_run = 'Unable to run Xpdf pdftotext: ';
  SXPdf_pdftopng_error_unable_to_run = 'Unable to run Xpdf pdftopng: ';

type

  { TXPdfToolbox }

  TXPdfToolbox = class
  strict private
    class function CheckXPdf_pdftotext_ExePath : boolean;
    class function CheckXPdf_pdftopng_ExePath : boolean;
  public
    class function ExtractTextFromPdf(const aPdfFileName: string; out aText : String): boolean;
    class function ExtractThumbnailOfFrontPageFromPdf(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight : word) : boolean;
    class function GetLastError : String;
  end;

var
  XPdf_pdftotext_ExePath : string;
  XPdf_pdftopng_ExePath : string;

implementation

uses
  Process, LazUTF8,
  mUtility,
  {$IFDEF NOGUI}
  mGraphicsUtilityNoGUI
  {$ELSE}
  Graphics, mGraphicsUtility
  {$ENDIF}
  ;

var
  FLastError : String;

{ TXPdfToolbox }

class function TXPdfToolbox.CheckXPdf_pdftotext_ExePath: boolean;
begin
  Result := false;
  if not FileExists(XPdf_pdftotext_ExePath) then
  begin
    FLastError := SXPdf_pdftotext_error_unable_to_run + XPdf_pdftotext_ExePath;
    exit;
  end;
  Result := true;
end;

class function TXPdfToolbox.CheckXPdf_pdftopng_ExePath: boolean;
begin
  Result := false;
  if not FileExists(XPdf_pdftopng_ExePath) then
  begin
    FLastError := SXPdf_pdftopng_error_unable_to_run + XPdf_pdftopng_ExePath;
    exit;
  end;
  Result := true;
end;

class function TXPdfToolbox.ExtractTextFromPdf(const aPdfFileName: string; out aText: String): boolean;
var
  outputString, cmd : string;
  tempFile : string;
  list : TStringList;
begin
  Result := false;
  if not CheckXPdf_pdftotext_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := SXPdf_error_file_missing + aPdfFileName;
    exit;
  end;

  tempFile := IncludeTrailingPathDelimiter(GetTempDir) + mUtility.GenerateRandomIdString + '.txt';
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  //if RunCommand(XPdf_pdftotext_ExePath, [AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"'), tempFile], outputString, [poNoConsole, poWaitOnExit]) then
  if RunCommand(XPdf_pdftotext_ExePath, [AnsiQuotedStr(aPdfFileName,'"'), tempFile], outputString, [poNoConsole, poWaitOnExit]) then
  begin
    list := TStringList.Create;
    try
      list.LoadFromFile(tempFile);
      aText := list.Text;
    finally
      list.Free;
    end;
    if FileExists(tempFile) then
      DeleteFile(tempFile);
  end
  else
  begin
    FLastError := SXPdf_pdftotext_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TXPdfToolbox.ExtractThumbnailOfFrontPageFromPdf(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight: word): boolean;
var
  outputString, cmd : string;
  tempFile : string;
begin
  Result := false;
  if not CheckXPdf_pdftopng_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := SXPdf_error_file_missing + aPdfFileName;
    exit;
  end;

  tempFile := IncludeTrailingPathDelimiter(ExtractFilePath(aThumbnailFileName)) + GenerateRandomIdString(10);
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  //cmd := '-f 1 -l 1 ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"') + ' ' + AnsiQuotedStr(tempFile,'"');
  cmd := '-f 1 -l 1 ' + AnsiQuotedStr(aPdfFileName,'"') + ' ' + AnsiQuotedStr(tempFile,'"');
  if RunCommand(XPdf_pdftopng_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
  begin
    // PNG-root-nnnnnn.png
    if FileExists(tempFile + '-000001.png') then
    begin
      if not GeneratePNGThumbnailOfImage(tempFile + '-000001.png', aThumbnailFileName, aWidth, aHeight, outputString) then
      begin
        FLastError := SXPdf_pdftopng_error_unable_to_run + outputString;
        DeleteFile(tempFile + '-000001.png');
        exit;
      end;
      DeleteFile(tempFile + '-000001.png');
    end;
  end
  else
  begin
    FLastError := SXPdf_pdftopng_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TXPdfToolbox.GetLastError: String;
begin
  Result := FLastError;
end;


initialization
  FLastError := '';

end.
