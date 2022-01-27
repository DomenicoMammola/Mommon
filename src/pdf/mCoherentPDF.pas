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
  sysutils, Classes;

resourcestring
  SCpdf_error_file_missing = 'Pdf file is missing: ';
  SCpdf_error_unable_to_run = 'Unable to run Cpdf: ';
  SCpdf_error_exe_missing = 'Path value of Cpdf command is wrong or missing: ';
  SCpdf_error_rotation_angle_invalid = 'Rotation angle is invalid: ';


type
  { TCpdfToolbox }

  TCpdfToolbox = class
  strict private
    class function CheckCpdfExePath : boolean;
  public
    class function SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate : string): boolean;
    class function MergePdfFiles (const aFiles : TStringList; const aDestinationFileName : string): boolean;
    class function RotateClockwisePdf(const aPdfFileName: string; const aAngle : integer): boolean;
    class function GetLastError : String;
  end;


var
  CpdfExePath : string;

implementation

uses
  Process, LazUTF8;

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
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  //cmd := '-split ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"') + ' -o "' + thumbFilename + '"';
  cmd := '-split ' + AnsiQuotedStr(aPdfFileName,'"') + ' -o "' + thumbFilename + '"';
  if not RunCommand(CpdfExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
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

class function TCpdfToolbox.MergePdfFiles(const aFiles: TStringList; const aDestinationFileName: string): boolean;
var
  outputString, cmd : string;
  i : integer;
begin
  Result := false;
  if not CheckCpdfExePath then
    exit;

  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  cmd := '-merge ';
  for i := 0 to aFiles.Count - 1 do
    cmd := cmd + ' ' + AnsiQuotedStr(aFiles.Strings[i],'"');
//    cmd := cmd + ' ' + AnsiQuotedStr(UTF8ToWinCP(aFiles.Strings[i]),'"');

  cmd := cmd + '-end -o ' + AnsiQuotedStr(aDestinationFileName,'"');
//  cmd := cmd + '-end -o ' + AnsiQuotedStr(UTF8ToWinCP(aDestinationFileName),'"');

  outputString := '';
  {$IFDEF UNIX}
  raise Exception.Create('Not implemented');
  {$ELSE}
  if not RunCommand(CpdfExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
  {$ENDIF}
  begin
    FLastError := SCpdf_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TCpdfToolbox.RotateClockwisePdf(const aPdfFileName: string; const aAngle : integer): boolean;
var
  outputString, cmd : string;
begin
  Result := false;
  if not CheckCpdfExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := SCpdf_error_file_missing + aPdfFileName;
    exit;
  end;

  if aAngle = 0 then
  begin
    Result := true;
    exit;
  end;

  if (aAngle <> 90) and (aAngle <> 180) and (aAngle <> 270) then
  begin
    FLastError := SCpdf_error_rotation_angle_invalid + IntToStr(aAngle);
    exit;
  end;

  {$IFDEF UNIX}
  if not RunCommand(CpdfExePath, ['-rotateby '  + IntToStr(aAngle), aPdfFileName, '-o', aPdfFileName], outputString, [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  // cmd := '-rotateby ' + IntToStr(aAngle)  + ' ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"') + ' -o "' + aPdfFileName + '"';
  cmd := '-rotateby ' + IntToStr(aAngle)  + ' ' + AnsiQuotedStr(aPdfFileName,'"') + ' -o "' + aPdfFileName + '"';
  if not RunCommand(CpdfExePath, [cmd], outputString, [poNoConsole, poWaitOnExit]) then
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
