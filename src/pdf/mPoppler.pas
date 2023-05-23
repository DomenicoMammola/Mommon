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
  SPoppler_error_missing_clu = 'Missing clu: %s';
  SPoppler_error_file_missing = 'Pdf file is missing: %s';
  SPoppler_pdfunite_error_unable_to_run = 'Unable to run Poppler pdfunite: %s';
  SPoppler_pdfseparate_error_unable_to_run = 'Unable to run Poppler pdfseparate: %s';
  SPoppler_pdftoppm_error_unable_to_run = 'Unable to run Poppler pdftoppm: %s';
  SPoppler_pdftotext_error_unable_to_run = 'Unable to run Poppler pdftotext: %s';
  SPoppler_pdfinfo_error_unable_to_run = 'Unable to run Poppler pdfinfo: %s';
  SPoppler_pdfimages_error_unable_to_run = 'Unable to run Poppler pdfimages: %s';

type

  TPopplerPdfInfo = record
    Title : string;
    Subject : string;
    Creator: string;
    Producer: string;
    CreationDate : TDateTime;
    ModDate : TDateTime;
    Pages : integer;
    PageSize : string;
    PageWidth : integer;
    PageHeight : integer;
    PDFVersion : string;
  end;

  { TPopplerToolbox }

  TPopplerToolbox = class
  strict private
    class function CheckPoppler_pdfunite_ExePath : boolean;
    class function CheckPoppler_pdfseparate_ExePath : boolean;
    class function CheckPoppler_pdftoppm_ExePath : boolean;
    class function CheckPoppler_pdftotext_ExePath : boolean;
    class function CheckPoppler_pdfinfo_ExePath : boolean;
    class function CheckPoppler_pdfimages_ExePath : boolean;
    class function ExtractPagesFromPdfAsImages(const aPdfFileName, aDestinationFileName: string; const aImageType : string; const aResolution : integer; const aJpegQuality : integer; const aOnlyFrontPage : boolean) : boolean;
  public
    class function SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate : string): boolean;
    class function MergePdfFiles (const aFiles : TStrings; const aDestinationFileName : string): boolean;
    class function ExtractThumbnailOfFrontPageFromPdfAsPng(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight : word) : boolean;
    class function ExtractFrontPageFromPdfAsPng(const aPdfFileName, aDestinationFileName: string; const aResolution : integer = 72) : boolean;
    class function ExtractPagesFromPdfAsJpeg(const aPdfFileName, aDestinationFolder, aPrefixFileName : string; const aQuality : integer; const aResolution : integer = 72): boolean;
    class function ExtractTextFromPdf(const aPdfFileName: string; const aPreserveLayout: boolean; out aText : String): boolean; overload;
    class function ExtractTextFromPdf(const aPdfFileName: string; const aPreserveLayout: boolean; aLines : TStringList): boolean; overload;
    class function GetInfoFromPdf (const aPdfFileName : string; var aInfo : TPopplerPdfInfo): boolean;
    class function GetImagesInfoFromPdf (const aPdfFileName : string; out aImagesCount : integer): boolean;
    class function GetLastError : String;
  end;

var
  Poppler_pdfunite_ExePath : string;
  Poppler_pdfseparate_ExePath : string;
  Poppler_pdftoppm_ExePath : string;
  Poppler_pdftotext_ExePath : string;
  Poppler_pdfinfo_ExePath : string;
  Poppler_pdfimages_ExePath : string;

implementation

uses
  Process, LazUTF8, StrUtils, FileUtil, Math,
  mUtility, mMathUtility,
  {$IFDEF NOGUI}
  mGraphicsUtilityNoGUI
  {$ELSE}
  mGraphicsUtility
  {$ENDIF};

var
  FLastError : String;

{ TPopplerToolbox }

class function TPopplerToolbox.CheckPoppler_pdfunite_ExePath: boolean;
begin
  Result := CheckCLU(Poppler_pdfunite_ExePath);
  if not Result then
    FLastError := Format(SPoppler_error_missing_clu, [Poppler_pdfunite_ExePath]);
end;

class function TPopplerToolbox.CheckPoppler_pdfseparate_ExePath: boolean;
begin
  Result := CheckCLU(Poppler_pdfseparate_ExePath);
  if not Result then
    FLastError := Format(SPoppler_error_missing_clu, [Poppler_pdfseparate_ExePath]);
end;

class function TPopplerToolbox.CheckPoppler_pdftoppm_ExePath: boolean;
begin
  Result := CheckCLU(Poppler_pdftoppm_ExePath);
  if not Result then
    FLastError := Format(SPoppler_error_missing_clu, [Poppler_pdftoppm_ExePath]);
end;

class function TPopplerToolbox.CheckPoppler_pdftotext_ExePath: boolean;
begin
  Result := CheckCLU(Poppler_pdftotext_ExePath);
  if not Result then
    FLastError := Format(SPoppler_error_missing_clu, [Poppler_pdftotext_ExePath]);
end;

class function TPopplerToolbox.CheckPoppler_pdfinfo_ExePath: boolean;
begin
  Result := CheckCLU(Poppler_pdfinfo_ExePath);
  if not Result then
    FLastError := Format(SPoppler_error_missing_clu, [Poppler_pdfinfo_ExePath]);
end;

class function TPopplerToolbox.CheckPoppler_pdfimages_ExePath: boolean;
begin
  Result := CheckCLU(Poppler_pdfimages_ExePath);
  if not Result then
    FLastError := Format(SPoppler_error_missing_clu, [Poppler_pdfimages_ExePath]);
end;

class function TPopplerToolbox.ExtractPagesFromPdfAsImages(const aPdfFileName, aDestinationFileName: string; const aImageType: string; const aResolution: integer; const aJpegQuality : integer; const aOnlyFrontPage : boolean): boolean;
var
  outputString : string;
  {$IFDEF WINDOWS}
  cmd : String;
  {$ENDIF}
  {$IFDEF UNIX}
  ris : boolean;
  {$ENDIF}
begin
  Result := false;
  if not CheckPoppler_pdftoppm_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := Format(SPoppler_error_file_missing, [aPdfFileName]);
    exit;
  end;

  outputString:= '';

  {$IFDEF UNIX}
  if aOnlyFrontPage then
  begin
    if aImageType = 'jpeg' then
      ris :=  RunCommandIndir(ExtractFileDir(aDestinationFileName), Poppler_pdftoppm_ExePath, ['-singlefile', '-' + aImageType, '-r', IntToStr(aResolution), '-jpegopt', 'optimize=y,quality=' + IntToStr(aJpegQuality), aPdfFileName, aDestinationFileName], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut])
    else
      ris :=  RunCommandIndir(ExtractFileDir(aDestinationFileName), Poppler_pdftoppm_ExePath, ['-singlefile', '-' + aImageType, '-r', IntToStr(aResolution), aPdfFileName, aDestinationFileName], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]);
  end
  else
  begin
    if aImageType = 'jpeg' then
      ris :=  RunCommandIndir(ExtractFileDir(aDestinationFileName), Poppler_pdftoppm_ExePath, ['-' + aImageType, '-r', IntToStr(aResolution), '-jpegopt', 'optimize=y,quality=' + IntToStr(aJpegQuality), aPdfFileName, aDestinationFileName], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut])
    else
      ris :=  RunCommandIndir(ExtractFileDir(aDestinationFileName), Poppler_pdftoppm_ExePath, ['-' + aImageType, '-r', IntToStr(aResolution), aPdfFileName, aDestinationFileName], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]);
  end;
  if ris then
  {$ELSE}
  if aOnlyFrontPage then
    cmd := '-singlefile '
  else
    cmd := '';
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  cmd := cmd + '-' + aImageType + ' -r ' + IntToStr(aResolution);
  if aImageType = 'jpeg' then
    cmd := cmd + ' '  + '-jpegopt optimize=y,quality=' + IntToStr(aJpegQuality);
  cmd := cmd + ' ' + AnsiQuotedStr(aPdfFileName,'"') + ' ' + AnsiQuotedStr(aDestinationFileName,'"');
  if RunCommand(Poppler_pdftoppm_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
  {$ENDIF}
  begin
    Result := true;
  end
  else
  begin
    FLastError := Format(SPoppler_pdftoppm_error_unable_to_run, [outputString]);
  end;
end;

class function TPopplerToolbox.MergePdfFiles(const aFiles: TStrings; const aDestinationFileName: string): boolean;
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
  if not RunCommand(Poppler_pdfunite_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
  {$ENDIF}
  begin
    FLastError := Format(SPoppler_pdfunite_error_unable_to_run, [outputString]);
    exit;
  end;
  Result := true;
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
    FLastError := Format(SPoppler_error_file_missing, [aPdfFileName]);
    exit;
  end;

  thumbFileTemplate := IncludeTrailingPathDelimiter(aPagesFolder) + aFileNameTemplate;

  {$IFDEF UNIX}
  if not RunCommandIndir(ExtractFileDir(aPdfFileName), Poppler_pdfseparate_ExePath, [aPdfFileName, thumbFileTemplate], outputString,  [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136

  cmd := AnsiQuotedStr(aPdfFileName,'"') + ' "' + thumbFileTemplate + '"';
  if not RunCommand(Poppler_pdfseparate_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
  {$ENDIF}
  begin
    FLastError := Format(SPoppler_pdfseparate_error_unable_to_run, [outputString]);
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
    FLastError := Format(SPoppler_error_file_missing, [aPdfFileName]);
    exit;
  end;

  tempFile := IncludeTrailingPathDelimiter(ExtractFilePath(aThumbnailFileName)) + GenerateRandomIdString(10);
  {$IFDEF UNIX}
  if RunCommandIndir(ExtractFileDir(aPdfFileName), Poppler_pdftoppm_ExePath, ['-singlefile', '-png', aPdfFileName, tempFile], outputString,  [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  cmd := '-singlefile -png ' + AnsiQuotedStr(aPdfFileName,'"') + ' ' + AnsiQuotedStr(tempFile,'"');
  if RunCommand(Poppler_pdftoppm_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
  {$ENDIF}
  begin
    tempFile := ChangeFileExt(tempFile, '.png');
    // PPM-root-number.ppm
    if FileExists(tempFile) then
    begin
      if not GeneratePNGThumbnailOfImage(tempFile, aThumbnailFileName, aWidth, aHeight, outputString) then
      begin
        FLastError := Format(SPoppler_pdftoppm_error_unable_to_run, [outputString]);
        DeleteFile(tempFile);
        exit;
      end;
      DeleteFile(tempFile);
    end
    else
    begin
      FLastError := Format(SPoppler_pdftoppm_error_unable_to_run, [outputString]);
      exit;
    end;
  end
  else
  begin
    FLastError := Format(SPoppler_pdftoppm_error_unable_to_run, [outputString]);
    exit;
  end;
  Result := true;
end;

class function TPopplerToolbox.ExtractFrontPageFromPdfAsPng(const aPdfFileName, aDestinationFileName: string; const aResolution: integer): boolean;
var
  tmpDestFile : String;
begin
  tmpDestFile := ChangeFileExt(aDestinationFileName, '');
  Result := ExtractPagesFromPdfAsImages(aPdfFileName, tmpDestFile, 'png', aResolution, 0, true);
end;

class function TPopplerToolbox.ExtractPagesFromPdfAsJpeg(const aPdfFileName, aDestinationFolder, aPrefixFileName: string; const aQuality: integer; const aResolution: integer): boolean;
begin
  Result := ExtractPagesFromPdfAsImages(aPdfFileName, IncludeTrailingPathDelimiter(aDestinationFolder) + aPrefixFileName, 'jpeg', aResolution, aQuality, false);
end;

class function TPopplerToolbox.ExtractTextFromPdf(const aPdfFileName: string; const aPreserveLayout: boolean; out aText: String): boolean;
var
  list : TStringList;
begin
  list := TStringList.Create;
  try
    Result := TPopplerToolbox.ExtractTextFromPdf(aPdfFileName, aPreserveLayout, list);
    if Result then
      aText := list.Text
    else
      aText := '';
  finally
    list.Free;
  end;
end;

class function TPopplerToolbox.ExtractTextFromPdf(const aPdfFileName: string; const aPreserveLayout: boolean; aLines: TStringList): boolean;
var
  outputString : string;
  tempFile : string;
  {$IFNDEF UNIX}
  cmd : string;
  {$ENDIF}
  ris : boolean;
begin
  Result := false;
  if not CheckPoppler_pdftotext_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := Format(SPoppler_error_file_missing,[aPdfFileName]);
    exit;
  end;

  tempFile := IncludeTrailingPathDelimiter(GetTempDir) + mUtility.GenerateRandomIdString + '.txt';
  {$IFDEF UNIX}
  if aPreserveLayout then
    ris := RunCommandIndir(ExtractFileDir(aPdfFileName), Poppler_pdftotext_ExePath, [aPdfFileName, '-layout', tempFile], outputString,  [poStderrToOutPut, poUsePipes, poWaitOnExit])
  else
    ris := RunCommandIndir(ExtractFileDir(aPdfFileName), Poppler_pdftotext_ExePath, [aPdfFileName, tempFile], outputString,  [poStderrToOutPut, poUsePipes, poWaitOnExit]);
  {$ELSE}
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  if aPreserveLayout then
    cmd := '-layout ' + AnsiQuotedStr(aPdfFileName,'"') + ' ' + AnsiQuotedStr(tempFile,'"')
  else
    cmd := AnsiQuotedStr(aPdfFileName,'"') + ' ' + AnsiQuotedStr(tempFile,'"');
  ris := RunCommand(Poppler_pdftotext_ExePath, [cmd], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]);
  {$ENDIF}
  if ris then
  begin
    aLines.LoadFromFile(tempFile);
    if FileExists(tempFile) then
      DeleteFile(tempFile);
  end
  else
  begin
    FLastError := Format(SPoppler_pdftotext_error_unable_to_run, [outputString]);
    exit;
  end;
  Result := true;
end;

class function TPopplerToolbox.GetLastError: String;
begin
  Result := FLastError;
end;

function ExtractText (const aSourceLine : String; const aKey : String; var aValue : String): boolean;
var
  le : integer;
begin
  Result := false;
  le := Length(aKey);
  if CompareText(LeftStr(aSourceLine, le), aKey) = 0 then
  begin
    aValue:= Trim(Copy(aSourceLine, le + 1, 9999));
    Result := true;
  end;
end;

function ConvertDateTime(const aSource : String; var aValue : TDateTime): boolean;
var
  list : TStringList;
  tmpDate : TDateTime;
begin
  Result := false;
  //Thu Sep 30 18:25:50 2021 ora legale Europa occidentale
  list := TStringList.Create;
  try
    list.Delimiter:= ' ';
    list.DelimitedText:= aSource;
    if list.Count >= 5 then
    begin
      if IsNumeric(list.Strings[2], false, false) and IsNumeric(list.Strings[4], false, false) then
      begin
        if TryToUnderstandDateTimeString(list.Strings[2] + ' ' + list.Strings[1] + ' ' + list.Strings[4] + ' ' + list.Strings[3], tmpDate) then
        begin
          aValue := tmpDate;
          Result := true;
        end;
      end;
    end;
  finally
    list.Free;
  end;
end;

function ExtractPageSize (const aSource : string; out aWidth, aHeight: integer): boolean;
var
  str : String;
  i : integer;
  tmpDouble : Double;
begin
  (*
  614 x 1009 pts
  841.89 x 595.276 pts (A4)
  419.528 x 595.276 pts
  515.906 x 728.504 pts
  612 x 1008 pts
  612 x 792 pts (letter)
  *)
  Result := false;
  str := LowerCase(aSource);
  i := Pos('x', str);
  if i > 0 then
  begin
    if not mMathUtility.TryToConvertToDouble(trim(LeftStr(str, i - 1)), tmpDouble) then
      exit;
    aWidth := round(tmpDouble);
    str := Copy(str, i + 1, 999);
    i := Pos('pts', str);
    if i > 0 then
    begin
      if not mMathUtility.TryToConvertToDouble(trim(LeftStr(str, i - 1)), tmpDouble) then
        exit;
      aHeight := round(tmpDouble);
      Result := true;
    end;
  end;
end;

class function TPopplerToolbox.GetInfoFromPdf(const aPdfFileName: string; var aInfo : TPopplerPdfInfo): boolean;
var
  outputString, curStr, value, cmd : string;
  tmpList : TStringList;
  i, intValue, w, h : integer;
  dateValue : TDateTime;
  rotate : boolean;
  {$IFDEF WINDOWS}
  tmpFileName, tmpFileNameBatch : String;
  command : TStringList;
  newSourceFile : String;
  {$ENDIF}
begin
  Result := false;

  if not CheckPoppler_pdfinfo_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := Format(SPoppler_error_file_missing, [aPdfFileName]);
    exit;
  end;

  rotate := false;

  tmpList := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    command := TStringList.Create;
    try
      newSourceFile:= GetTempFileName(GetTempDir, 'popplerinfo_source_' + GenerateRandomIdString);
      CopyFile(aPdfFileName, newSourceFile);
      tmpFileNameBatch := GetTempFileName(GetTempDir, 'popplerinfo');
      tmpFileNameBatch := ChangeFileExt(tmpFileNameBatch, '.bat');
      tmpFileName := GetTempFileName(GetTempDir, '');
      command.Append(AnsiQuotedStr(Poppler_pdfinfo_ExePath,'"') + ' ' + AnsiQuotedStr(newSourceFile,'"') + ' > ' + AnsiQuotedStr(tmpFileName,'"'));
      command.SaveToFile(tmpFileNameBatch);
      try
        if RunCommand(tmpFileNameBatch, [], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
          tmpList.LoadFromFile(tmpFileName)
        else
        begin
          FLastError := Format(SPoppler_pdfinfo_error_unable_to_run, [outputString]);
          exit;
        end;
      finally
        if FileExists(tmpFileName) then
          DeleteFile(tmpFileName);
        if FileExists(tmpFileNameBatch) then
          DeleteFile(tmpFileNameBatch);
        if FileExists(newSourceFile) then
          DeleteFile(newSourceFile);
      end;
    finally
      command.Free;
    end;
    {$ELSE}
    cmd := Poppler_pdfinfo_ExePath;
    if not RunCommandIndir(ExtractFileDir(aPdfFileName), cmd, [ExtractFileName(aPdfFileName)], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
    begin
      FLastError := Format(SPoppler_pdfinfo_error_unable_to_run, [outputString]);
      exit;
    end;
    tmpList.Text:= outputString;
    (*
    tmpList.Delimiter:= '''';
    tmpList.DelimitedText:= outputString;
    *)
    {$ENDIF}

    aInfo.Title := '';
    aInfo.Subject := '';
    aInfo.Creator := '';
    aInfo.Producer := '';
    aInfo.CreationDate := 0;
    aInfo.ModDate := 0;
    aInfo.Pages := 0;
    aInfo.PageSize := '';
    aInfo.PageWidth := 0;
    aInfo.PageHeight := 0;
    aInfo.PDFVersion := '';
    (*
    Title:
    Subject:
    Keywords:
    Author:
    Creator:         PaperPort 14
    Producer:        PaperPort 14
    CreationDate:    Thu Sep 30 18:25:50 2021 ora legale Europa occidentale
    ModDate:         Thu Sep 30 18:25:57 2021 ora legale Europa occidentale
    Custom Metadata: no
    Metadata Stream: yes
    Tagged:          no
    UserProperties:  no
    Suspects:        no
    Form:            none
    JavaScript:      no
    Pages:           5
    Encrypted:       no
    Page size:       614 x 1009 pts
    Page rot:        0
    File size:       1821455 bytes
    Optimized:       no
    PDF version:     1.6
    *)
    for i := 0 to tmpList.Count - 1 do
    begin
      curStr := tmpList.Strings[i];
      value := '';
      if ExtractText(curStr, 'Title:', value) then
        aInfo.Title:= value
      else if ExtractText(curStr, 'Subject:', value) then
        aInfo.Subject:= value
      else if ExtractText(curStr, 'Creator:', value) then
        aInfo.Creator:= value
      else if ExtractText(curStr, 'Producer:', value) then
        aInfo.Producer:= value
      else if ExtractText(curStr, 'CreationDate:', value) then
      begin
        if ConvertDateTime(value, dateValue) then
          aInfo.CreationDate:= dateValue;
      end
      else if ExtractText(curStr, 'ModDate:', value) then
      begin
        if ConvertDateTime(value, dateValue) then
          aInfo.ModDate:= dateValue;
      end
      else if ExtractText(curStr, 'Pages:', value) then
      begin
        if TryToConvertToInteger(value, intValue) then
          aInfo.Pages:= intValue;
      end
      else if ExtractText(curStr, 'Page size:', value) then
      begin
        if ExtractPageSize(value, w, h) then
        begin
          aInfo.PageWidth := w;
          aInfo.PageHeight := h;
        end;
      end
      else if ExtractText(curStr, 'Page rot:', value) then
      begin
        if TryToConvertToInteger(value,  intValue) then
          if abs(intValue) = 90 then
            rotate := true;
      end
      else if ExtractText(curStr, 'PDF version:', value) then
        aInfo.PDFVersion:= value;
    end;

    if rotate then
    begin
      h := aInfo.PageHeight;
      aInfo.PageHeight:= aInfo.PageWidth;
      aInfo.PageWidth:= h;
    end;

  finally
    tmpList.Free;
  end;

  Result := true;
end;

class function TPopplerToolbox.GetImagesInfoFromPdf(const aPdfFileName: string; out aImagesCount: integer): boolean;
var
  outputString, cmd : string;
  tmpList : TStringList;
  {$IFDEF WINDOWS}
  tmpFileName, tmpFileNameBatch, newSourceFile : String;
  command : TStringList;
  {$ENDIF}
begin
  Result := false;

  if not CheckPoppler_pdfinfo_ExePath then
    exit;

  if not FileExists(aPdfFileName) then
  begin
    FLastError := Format(SPoppler_error_file_missing, [aPdfFileName]);
    exit;
  end;

  tmpList := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    command := TStringList.Create;
    try
      tmpFileNameBatch := GetTempFileName(GetTempDir, 'popplerimages');
      tmpFileNameBatch := ChangeFileExt(tmpFileNameBatch, '.bat');
      tmpFileName := GetTempFileName(GetTempDir, '');
      newSourceFile:= GetTempFileName(GetTempDir, 'popplerimages_source_' + GenerateRandomIdString);
      CopyFile(aPdfFileName, newSourceFile);

      command.Append(AnsiQuotedStr(Poppler_pdfimages_ExePath,'"') + ' -list ' + AnsiQuotedStr(newSourceFile,'"') + ' > ' + AnsiQuotedStr(tmpFileName,'"'));
      command.SaveToFile(tmpFileNameBatch);
      try
        if RunCommand(tmpFileNameBatch, [], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
          tmpList.LoadFromFile(tmpFileName)
        else
        begin
          FLastError := Format(SPoppler_pdfimages_error_unable_to_run, [outputString]);
          exit;
        end;
      finally
        if FileExists(tmpFileName) then
          DeleteFile(tmpFileName);
        if FileExists(tmpFileNameBatch) then
          DeleteFile(tmpFileNameBatch);
        if FileExists(newSourceFile) then
          DeleteFile(newSourceFile);
      end;
    finally
      command.Free;
    end;
    {$ELSE}
    cmd := Poppler_pdfimages_ExePath;
    if not RunCommandIndir(ExtractFileDir(aPdfFileName), cmd, ['-list', ExtractFileName(aPdfFileName)], outputString, [poNoConsole, poWaitOnExit, poStderrToOutPut]) then
    begin
      FLastError := Format(SPoppler_pdfimages_error_unable_to_run, [outputString]);
      exit;
    end;
    tmpList.Text:= outputString;
    (*
    tmpList.Delimiter:= '''';
    tmpList.DelimitedText:= outputString;
    *)
    {$ENDIF}

    aImagesCount:= tmpList.Count - 2;
  finally
    tmpList.Free;
  end;

  Result := true;
end;


initialization
  FLastError := '';
end.
