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
// Poppler
// https://blog.alivate.com.au/poppler-windows/
// https://github.com/oschwartz10612/poppler-windows
// https://www.mankier.com/package/poppler-utils 
// ***********************************
//
unit mPoppler;

interface

uses
  sysutils, Classes;

resourcestring
  SPoppler_error_file_missing = 'Pdf file is missing: ';
  SPoppler_pdfunite_error_unable_to_run = 'Unable to run Poppler pdfunite: ';
  SPoppler_pdfseparate_error_unable_to_run = 'Unable to run Poppler pdfseparate: ';
  SPoppler_pdftoppm_error_unable_to_run = 'Unable to run Poppler pdftoppm: ';
  SPoppler_pdftotext_error_unable_to_run = 'Unable to run Poppler pdftotext: ';

type

  { TPopplerToolbox }

  TPopplerToolbox = class
  strict private
    class function CheckPoppler_pdfunite_ExePath : boolean;
    class function CheckPoppler_pdfseparate_ExePath : boolean;
    class function CheckPoppler_pdftoppm_ExePath : boolean;
    class function CheckPoppler_pdftotext_ExePath : boolean;
  public
    class function SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate : string): boolean;
    class function MergePdfFiles (const aFiles : TStringList; const aDestinationFileName : string): boolean;
    class function ExtractThumbnailOfFrontPageFromPdfAsPng(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight : word) : boolean;
    class function ExtractFrontPageFromPdfAsPng(const aPdfFileName, aDestinationFileName: string; const aResolution : integer = 72) : boolean;
    class function ExtractTextFromPdf(const aPdfFileName: string; out aText : String): boolean;
    class function GetLastError : String;
  end;

var
  Poppler_pdfunite_ExePath : string;
  Poppler_pdfseparate_ExePath : string;
  Poppler_pdftoppm_ExePath : string;
  Poppler_pdftotext_ExePath : string;

implementation

uses
  Process, LazUTF8,
  mUtility, mGraphicsUtility;

var
  FLastError : String;

{ TPopplerToolbox }

class function TPopplerToolbox.CheckPoppler_pdfunite_ExePath: boolean;
begin
  Result := false;
  if not FileExists(Poppler_pdfunite_ExePath) then
  begin
    FLastError := SPoppler_pdfunite_error_unable_to_run + Poppler_pdfunite_ExePath;
    exit;
  end;
  Result := true;
end;

class function TPopplerToolbox.CheckPoppler_pdfseparate_ExePath: boolean;
begin
  Result := false;
  if not FileExists(Poppler_pdfseparate_ExePath) then
  begin
    FLastError := SPoppler_pdfseparate_error_unable_to_run + Poppler_pdfseparate_ExePath;
    exit;
  end;
  Result := true;
end;

class function TPopplerToolbox.CheckPoppler_pdftoppm_ExePath: boolean;
begin
  Result := false;
  if not FileExists(Poppler_pdftoppm_ExePath) then
  begin
    FLastError := SPoppler_pdftoppm_error_unable_to_run + Poppler_pdftoppm_ExePath;
    exit;
  end;
  Result := true;
end;

class function TPopplerToolbox.CheckPoppler_pdftotext_ExePath: boolean;
begin
  Result := false;
  if not FileExists(Poppler_pdftotext_ExePath) then
  begin
    FLastError := SPoppler_pdftotext_error_unable_to_run + Poppler_pdftotext_ExePath;
    exit;
  end;
  Result := true;
end;

class function TPopplerToolbox.SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate: string): boolean;
var
  outputString, cmd : string;
  thumbFileTemplate : String;
begin
  Result := false;
  if not CheckPoppler_pdfseparate_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := SPoppler_error_file_missing + aPdfFileName;
    exit;
  end;

  thumbFileTemplate := IncludeTrailingPathDelimiter(aPagesFolder) + aFileNameTemplate;

  {$IFDEF UNIX}
  raise Exception.Create('Not implemented');
  {$ELSE}
  cmd := AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"') + ' "' + thumbFileTemplate + '"';
  if not RunCommand(Poppler_pdfseparate_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
  {$ENDIF}
  begin
    FLastError := SPoppler_pdfseparate_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TPopplerToolbox.MergePdfFiles(const aFiles: TStringList; const aDestinationFileName: string): boolean;
var
  outputString, cmd : string;
  i : integer;
begin
  Result := false;
  if not CheckPoppler_pdfunite_ExePath then
    exit;

  cmd := '';
  for i := 0 to aFiles.Count - 1 do
    cmd := cmd + ' ' + AnsiQuotedStr(aFiles.Strings[i],'"');

  cmd := cmd + ' ' + AnsiQuotedStr(aDestinationFileName,'"');

  {$IFDEF UNIX}
  raise Exception.Create('Not implemented');
  {$ELSE}
  if not RunCommand(Poppler_pdfunite_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
  {$ENDIF}
  begin
    FLastError := SPoppler_pdfunite_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TPopplerToolbox.ExtractThumbnailOfFrontPageFromPdfAsPng(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight: word): boolean;
var
  outputString, cmd : string;
  tempFile : string;
begin
  Result := false;
  if not CheckPoppler_pdftoppm_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := SPoppler_error_file_missing + aPdfFileName;
    exit;
  end;

  tempFile := IncludeTrailingPathDelimiter(ExtractFilePath(aThumbnailFileName)) + GenerateRandomIdString(10);
  cmd := '-singlefile -png ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"') + ' ' + AnsiQuotedStr(tempFile,'"');
  if RunCommand(Poppler_pdftoppm_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
  begin
    tempFile := ChangeFileExt(tempFile, '.png');
    // PPM-root-number.ppm
    if FileExists(tempFile) then
    begin
      if not GeneratePNGThumbnailOfImage(tempFile, aThumbnailFileName, aWidth, aHeight, outputString) then
      begin
        FLastError := SPoppler_pdftoppm_error_unable_to_run + outputString;
        DeleteFile(tempFile);
        exit;
      end;
      DeleteFile(tempFile);
    end
    else
    begin
      FLastError := SPoppler_pdftoppm_error_unable_to_run + outputString;
      exit;
    end;
  end
  else
  begin
    FLastError := SPoppler_pdftoppm_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TPopplerToolbox.ExtractFrontPageFromPdfAsPng(const aPdfFileName, aDestinationFileName: string; const aResolution: integer): boolean;
var
  outputString, cmd, tmpDestFile : string;
begin
  Result := false;
  if not CheckPoppler_pdftoppm_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := SPoppler_error_file_missing + aPdfFileName;
    exit;
  end;

  tmpDestFile := ChangeFileExt(aDestinationFileName, '');
  cmd := '-singlefile -png -r ' + IntToStr(aResolution) + ' ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"') + ' ' + AnsiQuotedStr(UTF8ToWinCP(tmpDestFile),'"');
  if RunCommand(Poppler_pdftoppm_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
  begin
    Result := true;
  end
  else
  begin
    FLastError := SPoppler_pdftoppm_error_unable_to_run + outputString;
  end;
end;

class function TPopplerToolbox.ExtractTextFromPdf(const aPdfFileName: string; out aText: String): boolean;
var
  outputString, cmd : string;
  tempFile : string;
  list : TStringList;
begin
  Result := false;
  if not CheckPoppler_pdftotext_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := SPoppler_error_file_missing + aPdfFileName;
    exit;
  end;

  tempFile := IncludeTrailingPathDelimiter(GetTempDir) + mUtility.GenerateRandomIdString + '.txt';
  cmd := AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"') + ' ' + AnsiQuotedStr(UTF8ToWinCP(tempFile),'"');
  if RunCommand(Poppler_pdftotext_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
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
    FLastError := SPoppler_pdftotext_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TPopplerToolbox.GetLastError: String;
begin
  Result := FLastError;
end;

initialization
  FLastError := '';
end.
