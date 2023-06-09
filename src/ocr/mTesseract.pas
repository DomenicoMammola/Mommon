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
// Tesseract
// https://github.com/tesseract-ocr/tesseract
// ***********************************
//
unit mTesseract;

interface

resourcestring
  STesseract_error_missing_clu = 'Missing clu: %s';
  STesseract_error_file_missing = 'File is missing: %s';
  STesseract_error_unable_to_run = 'Unable to run tesseract: %s';

type
  TTesseractToolboxLanguages = (ttlEnglish, ttlItalian);

  { TTesseractToolbox }

  TTesseractToolbox = class
  strict private
    class function CheckTesseract_ExePath : boolean;
  public
    class function ExtractTextFromImage(const aImageFileName, aDestinationFileNoExtension : string; const aLanguage : TTesseractToolboxLanguages = ttlEnglish): boolean;
    class function GetLastError : String;
  end;


var
  Tesseract_ExePath : string;

implementation

uses
  Process, SysUtils,
  mUtility;

var
  FLastError : String;

{ TTesseractToolbox }

class function TTesseractToolbox.CheckTesseract_ExePath: boolean;
begin
  Result := CheckCLU(Tesseract_ExePath);
  if not Result then
    FLastError := Format(STesseract_error_missing_clu, [Tesseract_ExePath]);
end;

class function TTesseractToolbox.ExtractTextFromImage(const aImageFileName, aDestinationFileNoExtension: string; const aLanguage: TTesseractToolboxLanguages): boolean;
var
  outputString, languageParam : string;
  {$IFNDEF UNIX}
  cmd : String;
  {$ENDIF}
begin
  Result := false;
  if not CheckTesseract_ExePath then
    exit;

  if not FileExists(aImageFileName) then
  begin
    FLastError := Format(STesseract_error_file_missing, [aImageFileName]);
    exit;
  end;

  languageParam := '-l ';
  if aLanguage = ttlItalian then
    languageParam:= languageParam + 'ita'
  else
    languageParam:= languageParam + 'eng';

  {$IFDEF UNIX}
  if not RunCommandIndir(ExtractFileDir(aImageFileName), Tesseract_ExePath, [aImageFileName, aDestinationFileNoExtension, languageParam], outputString,  [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  cmd := AnsiQuotedStr(aImageFileName,'"') + ' "' + aDestinationFileNoExtension + '"' + ' ' + languageParam;
  if not RunCommand(Tesseract_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
  {$ENDIF}
  begin
    FLastError := Format(STesseract_error_unable_to_run, [outputString]);
    exit;
  end;
  Result := true;
end;

class function TTesseractToolbox.GetLastError: String;
begin
  Result := FLastError;
end;

initialization
  FLastError := '';

end.
