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
// Coherent PDF Command Line Tools Community Release
// https://community.coherentpdf.com/
// ***********************************
//
unit mCoherentPDF;

interface

uses
  sysutils;

resourcestring
  SCpdf_error_file_missing = 'Pdf file is missing: ';
  SCpdf_error_unable_to_run = 'Unable to run Cpdf: ';
  SCpdf_error_exe_missing = 'Path value of Cpdf command is wrong or missing: ';


type
  { TCpdfToolbox }

  TCpdfToolbox = class
  strict private
    class function CheckCpdfExePath : boolean;
  public
    class function SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate : string): boolean;
    class function GetLastError : String;
  end;


var
  CpdfExePath : string;

implementation

uses
  Process;

var
  FLastError : String;

{ TCpdfToolbox }

class function TCpdfToolbox.CheckCpdfExePath : boolean;
begin
  Result := false;
  if not FileExists(CpdfExePath) then
  begin
    FLastError := SCpdf_error_exe_missing + CpdfExePath;
    exit;
  end;
  Result := true;
end;

class function TCpdfToolbox.SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate: string): boolean;
var
  outputString, cmd : string;
  thumbFileTemplate, thumbFilename : String;
  pages, i : integer;
begin
  Result := false;
  if not CheckCpdfExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := SCpdf_error_file_missing + aPdfFileName;
    exit;
  end;

  thumbFileTemplate := IncludeTrailingPathDelimiter(aPagesFolder) + aFileNameTemplate;
  thumbFilename:= StringReplace(thumbFileTemplate, '%d', '%%%', [rfReplaceAll, rfIgnoreCase]);

  {$IFDEF UNIX}
  if not RunCommand(CpdfExePath, ['-split', aPdfFileName, '-o', thumbFilename], outputString, [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  cmd := '-split "' + aPdfFileName + '" -o "' + thumbFilename + '"';
  if not RunCommand(CpdfExePath, [cmd], outputString, [poNoConsole]) then
  {$ENDIF}
  begin
    {$IFDEF UNIX}
    {$IFDEF DEBUG}
    writeln(outputString);
    {$ENDIF}
    {$ENDIF}
    FLastError := SCpdf_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TCpdfToolbox.GetLastError: String;
begin
  Result := FLastError;
end;

initialization
  FLastError := '';

end.
