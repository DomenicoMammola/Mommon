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

interface

uses
  Classes, sysutils;

resourcestring
  SXPdf_error_file_missing = 'Pdf file is missing: ';
  SXPdf_pdftotext_error_unable_to_run = 'Unable to run Xpdf pdftotext: ';

type

  { TXPdfToolbox }

  TXPdfToolbox = class
  strict private
    class function CheckXPdf_pdftotext_ExePath : boolean;
  public
    class function ExtractTextFromPdf(const aPdfFileName: string; out aText : String): boolean;
    class function GetLastError : String;
  end;

var
  XPdf_pdftotext_ExePath : string;

implementation

uses
  Process,
  mUtility;

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
  if RunCommand(XPdf_pdftotext_ExePath, [aPdfFileName, tempFile], outputString, [poNoConsole, poWaitOnExit]) then
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

class function TXPdfToolbox.GetLastError: String;
begin
  Result := FLastError;
end;


initialization
  FLastError := '';

end.
