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
  SPoppler_error_missing_clu = 'Missing clu: ';
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

function CheckCLU (const aCLU : String): boolean;
{$IFDEF UNIX}
var
  cmd, outputString : String;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Result := false;
  if not FileExists(aCLU) then
  begin
    FLastError := SPoppler_error_missing_clu +  aCLU;
    exit;
  end;
  Result := true;
  {$ELSE}
  {$IFDEF UNIX}
  cmd := '-v ' + aCLU;
  Result := RunCommand('/bin/bash',['-c','command', cmd],outputString, [poNoConsole, poWaitOnExit]);
  if not Result then
    FLastError := SPoppler_error_missing_clu + aCLU;
  {$ELSE}
  FLastError := SPoppler_error_missing_clu +  aCLU;
  Result := false;
  {$ENDIF}
  {$ENDIF}
end;

{ TPopplerToolbox }

class function TPopplerToolbox.CheckPoppler_pdfunite_ExePath: boolean;
begin
  Result := CheckCLU(Poppler_pdfunite_ExePath);
end;

class function TPopplerToolbox.CheckPoppler_pdfseparate_ExePath: boolean;
begin
  Result := CheckCLU(Poppler_pdfseparate_ExePath);
end;

class function TPopplerToolbox.CheckPoppler_pdftoppm_ExePath: boolean;
begin
  Result := CheckCLU(Poppler_pdftoppm_ExePath);
end;

class function TPopplerToolbox.CheckPoppler_pdftotext_ExePath: boolean;
begin
  Result := CheckCLU(Poppler_pdftotext_ExePath);
end;

class function TPopplerToolbox.SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate: string): boolean;
var
  outputString : string;
  thumbFileTemplate : String;
  {$IFNDEF UNIX}
  cmd : String;
  {$ENDIF}
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
  if not RunCommandIndir(ExtractFileDir(aPdfFileName), Poppler_pdfseparate_ExePath, [aPdfFileName, thumbFileTemplate], outputString,  [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136

  cmd := AnsiQuotedStr(aPdfFileName,'"') + ' "' + thumbFileTemplate + '"';
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
  outputString : string;
  i : integer;
  {$IFDEF UNIX}
  filesArray: array of String;
  {$ELSE}
  cmd: String;
  {$ENDIF}
begin
  Result := false;
  if not CheckPoppler_pdfunite_ExePath then
    exit;

  {$IFDEF UNIX}
  setLength(filesArray, aFiles.Count + 1);
  for i := 0 to aFiles.Count - 1 do
    filesArray[i] := aFiles.Strings[i];
  filesArray[aFiles.Count] := aDestinationFileName;
  if not RunCommandIndir(ExtractFileDir(aDestinationFileName), Poppler_pdfunite_ExePath, filesArray, outputString,  [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  cmd := '';
  for i := 0 to aFiles.Count - 1 do
    cmd := cmd + ' ' + AnsiQuotedStr(aFiles.Strings[i],'"');

  cmd := cmd + ' ' + AnsiQuotedStr(aDestinationFileName,'"');
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
  outputString : string;
  tempFile : string;
  {$IFNDEF UNIX}
  cmd: string;
  {$ENDIF}
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
  {$IFDEF UNIX}
  if RunCommandIndir(ExtractFileDir(aPdfFileName), Poppler_pdftoppm_ExePath, ['-singlefile', '-png', aPdfFileName, tempFile], outputString,  [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  cmd := '-singlefile -png ' + AnsiQuotedStr(aPdfFileName,'"') + ' ' + AnsiQuotedStr(tempFile,'"');
  if RunCommand(Poppler_pdftoppm_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
  {$ENDIF}
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
  outputString, tmpDestFile : string;
  {$IFNDEF UNIX}
  cmd : String;
  {$ENDIF}
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
  {$IFDEF UNIX}
  if RunCommandIndir(ExtractFileDir(aPdfFileName), Poppler_pdftoppm_ExePath, ['-singlefile' , '-png', '-r', IntToStr(aResolution), aPdfFileName, tmpDestFile], outputString,  [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  cmd := '-singlefile -png -r ' + IntToStr(aResolution) + ' ' + AnsiQuotedStr(aPdfFileName,'"') + ' ' + AnsiQuotedStr(tmpDestFile,'"');
  if RunCommand(Poppler_pdftoppm_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
  {$ENDIF}
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
  outputString : string;
  tempFile : string;
  {$IFNDEF UNIX}
  cmd : string;
  {$ENDIF}
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
  {$IFDEF UNIX}
  if RunCommandIndir(ExtractFileDir(aPdfFileName), Poppler_pdftotext_ExePath, [aPdfFileName, tempFile], outputString,  [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  cmd := AnsiQuotedStr(aPdfFileName,'"') + ' ' + AnsiQuotedStr(tempFile,'"');
  if RunCommand(Poppler_pdftotext_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
  {$ENDIF}
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
