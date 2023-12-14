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
// https://www.ghostscript.com/releases/gsdnld.html
// https://mrcoles.com/combine-compress-pdf/
// https://wiki.archivematica.org/PDF_to_PDF/A_using_Ghostscript
// ***********************************
//
unit mGhostscript;

interface

resourcestring
  SGhostscript_error_missing_clu = 'Missing clu: %s';
  SGhostscript_error_file_missing = 'Pdf file is missing: %s';
  SGhostscript_error_unable_to_run = 'Unable to run gs: %s';

type
  TPDFAConformity = (pdfa1, pdfa2);

  { TGhostscriptToolbox }

  TGhostscriptToolbox = class
  strict private
    class function CheckGhostscript_ExePath : boolean;
  public
    class function CompressPdf(const aPdfFileName, aDestinationFileName : string; const aResolution : integer = 150; const aConvertImagesToGrayScale : boolean = false) : boolean;
    class function ConvertToPDFA(const aPdfFileName, aDestinationFileName : string; const aConformity : TPDFAConformity) : boolean;
    class function GetLastError : String;
  end;

var
  Ghostscript_gs_ExePath : string;

implementation

uses
  sysutils, FileUtil, Process,
  mUtility;

var
  FLastError : String;

{ TGhostscriptToolbox }

class function TGhostscriptToolbox.CheckGhostscript_ExePath: boolean;
begin
  Result := CheckCLU(Ghostscript_gs_ExePath);
  if not Result then
    FLastError := Format(SGhostscript_error_missing_clu, [Ghostscript_gs_ExePath]);
end;

class function TGhostscriptToolbox.CompressPdf(const aPdfFileName, aDestinationFileName: string; const aResolution : integer = 150; const aConvertImagesToGrayScale : boolean = false): boolean;
var
  outputString : string;
  cmds : array of TProcessString;
begin
  Result := false;
  if not CheckGhostscript_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := Format(SGhostscript_error_file_missing, [aPdfFileName]);
    exit;
  end;

  // -sDEVICE=pdfwrite -dCompatabilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dBATCH -dColorImageResolution=150 -sOutputFile="pippo.pdf" "TCL_R51M -EU_EN C635.pdf"

  if aConvertImagesToGrayScale then
       cmds := ['-sDEVICE=pdfwrite', '-dCompatabilityLevel=1.4',
         '-dPDFSETTINGS=/ebook', '-dNOPAUSE', '-dBATCH',   '-sProcessColorModel=DeviceGray', '-sColorConversionStrategy=Gray', '-dOverrideICC', '-dColorImageResolution=' + IntToStr(aResolution),  '-sOutputFile=' + AnsiQuotedStr(ExtractFileName(aDestinationFileName), '"'), AnsiQuotedStr(aPdfFileName, '"')]
  else
    cmds := ['-sDEVICE=pdfwrite', '-dCompatabilityLevel=1.4',
      '-dPDFSETTINGS=/ebook', '-dNOPAUSE', '-dBATCH', '-dColorImageResolution=' + IntToStr(aResolution),  '-sOutputFile=' + AnsiQuotedStr(ExtractFileName(aDestinationFileName), '"'), AnsiQuotedStr(aPdfFileName, '"')];

  if RunCommandIndir(ExtractFileDir(aDestinationFileName), Ghostscript_gs_ExePath, cmds,
      outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
    Result := true
  else
    FLastError := Format(SGhostscript_error_unable_to_run, [outputString]);
end;

class function TGhostscriptToolbox.ConvertToPDFA(const aPdfFileName, aDestinationFileName: string; const aConformity : TPDFAConformity): boolean;
var
  outputString, conf : string;
begin
  Result := false;
  if not CheckGhostscript_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := Format(SGhostscript_error_file_missing, [aPdfFileName]);
    exit;
  end;

  if aConformity = pdfa2 then
    conf := '-dPDFA=2'
  else
    conf := '-dPDFA';

  if RunCommandIndir(ExtractFileDir(aDestinationFileName), Ghostscript_gs_ExePath, ['-sDEVICE=pdfwrite',
      conf, '-dNOPAUSE', '-dBATCH', '-sColorConversionStrategy=UseDeviceIndependentColor', '-dPDFACompatibilityPolicy=1', '-sOutputFile=' + AnsiQuotedStr(ExtractFileName(aDestinationFileName), '"'), AnsiQuotedStr(aPdfFileName, '"')],
      outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
    Result := true
  else
    FLastError := Format(SGhostscript_error_unable_to_run, [outputString]);
end;

class function TGhostscriptToolbox.GetLastError: String;
begin
  Result := FLastError;
end;

initialization
  FLastError := '';
end.
