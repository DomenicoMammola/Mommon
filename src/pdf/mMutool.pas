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
// Mutool is part of the MuPdf package (https://mupdf.com).
// It is released under the A-GPL (Affero GPL) license. Please look at: https://mupdf.com/license.html
// ***********************************
//
unit mMutool;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, sysutils;

resourcestring
  SMutool_error_file_missing = 'Pdf file is missing: ';
  SMutool_error_unable_to_run = 'Unable to run Mutool: ';
  SMutool_error_wrong_image_format = 'Only png file can be converted.';
  SMutool_error_exe_missing = 'Path value of Mutool command is wrong or missing: ';

type

  { TMutoolToolbox }

  TMutoolToolbox = class
  strict private
    class function CheckMutoolExePath : boolean;
    class function CheckFile(const aFileName: String): boolean;
  public
    class function ExtractTextFromPdf(const aPdfFileName: string; out aText : String): boolean;
    class function ExtractImagesFromPdf(const aPdfFileName: string; aImagesFiles : TStringList) : boolean;
    class function GetInfoFromPdf(const aPdfFileName: string; out aNumOfPages : integer) : boolean;
    class function ExtractThumbnailOfFrontPageFromPdf(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight : word) : boolean;
    class function ExtractFrontPageFromPdf(const aPdfFileName, aDestinationFileName: string; const aResolution : integer = 72) : boolean;
    class function SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate : string): boolean;
    class function MergePdfFiles (const aFiles : TStringList; const aDestinationFileName : string): boolean;
    class function GetLastError : String;
  end;

var
  MutoolExePath : string;

implementation

uses
  process, strutils, LazUTF8,
  mMathUtility, mUtility;

var
  FLastError : String;

{ TMutoolToolbox }

class function TMutoolToolbox.CheckMutoolExePath : boolean;
begin
  Result := false;
  if not FileExists(MutoolExePath) then
  begin
    FLastError := SMutool_error_exe_missing + ' ' + MutoolExePath;
    exit;
  end;
  Result := true;
end;

class function TMutoolToolbox.CheckFile(const aFileName: String): boolean;
begin
  Result := false;
  if not FileExists(aFileName) then
  begin
    FLastError := SMutool_error_file_missing + aFileName;
    exit;
  end;
  Result := true;
end;

class function TMutoolToolbox.ExtractTextFromPdf(const aPdfFileName: string; out aText : String): boolean;
var
  outputString, tempFile: string;
  list : TStringList;
begin
  Result := false;
  if not CheckMutoolExePath then
    exit;
  if not CheckFile(aPdfFileName) then
    exit;

  tempFile := IncludeTrailingPathDelimiter(GetTempDir) + mUtility.GenerateRandomIdString + '.txt';

  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  //if RunCommand(MutoolExePath, ['draw -F txt -o "' + tempFile + '" ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"')], outputString, [poNoConsole, poWaitOnExit]) then
  if RunCommand(MutoolExePath, ['draw -F txt -o "' + tempFile + '" ' + AnsiQuotedStr(aPdfFileName,'"')], outputString, [poNoConsole, poWaitOnExit]) then
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
    FLastError := SMutool_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TMutoolToolbox.ExtractImagesFromPdf(const aPdfFileName: string; aImagesFiles: TStringList) : boolean;
var
  outputString, curPath, tmpString : string;
  tmpList : TStringList;
  i : integer;
begin
  Result := false;
  if not CheckMutoolExePath then
    exit;

  if not CheckFile(aPdfFileName) then
    exit;

  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  //if RunCommand(MutoolExePath, ['extract ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"')], outputString, [poNoConsole,poWaitOnExit]) then
  if RunCommand(MutoolExePath, ['extract ' + AnsiQuotedStr(aPdfFileName,'"')], outputString, [poNoConsole,poWaitOnExit]) then
  begin
    tmpList := TStringList.Create;
    try
      aImagesFiles.Clear;

      tmpList.Delimiter:= #10;
      tmpList.DelimitedText:= outputString;
      curPath:= ExtractFilePath(aPdfFileName);
      for i := 0 to tmpList.Count - 1 do
      begin
        // looking for "extracting image img-xxx.png"
        if Pos ('extracting image', tmpList.Strings[i]) >= 0 then
        begin
          tmpString := Trim(StringReplace(Copy(tmpList.Strings[i], 17, 999), #13, '', [rfReplaceAll]));
          aImagesFiles.Add(IncludeTrailingPathDelimiter(curPath) + tmpString);
        end
        else
        begin
          if Pos ('extracting font', tmpList.Strings[i]) >= 0 then
          begin
            tmpString := Trim(StringReplace(Copy(tmpList.Strings[i], 16, 999), #13, '', [rfReplaceAll]));
            DeleteFile(IncludeTrailingPathDelimiter(curPath) + tmpString);
          end;
        end;
      end;
    finally
      tmpList.Free;
    end;
  end
  else
  begin
    FLastError := SMutool_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TMutoolToolbox.GetInfoFromPdf(const aPdfFileName: string; out aNumOfPages: integer) : boolean;
var
  outputString, tmpString : string;
  tmpList : TStringList;
  i : integer;
begin
  Result := false;
  aNumOfPages:= 0;

  if not CheckMutoolExePath then
    exit;

  if not CheckFile(aPdfFileName) then
    exit;

  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  //if RunCommand(MutoolExePath, ['info ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"')], outputString, [poNoConsole,poWaitOnExit]) then
  if RunCommand(MutoolExePath, ['info ' + AnsiQuotedStr(aPdfFileName,'"')], outputString, [poNoConsole,poWaitOnExit]) then
  begin
    tmpList := TStringList.Create;
    try
      tmpList.Delimiter:= #10;
      tmpList.DelimitedText:= outputString;
      for i := 0 to tmpList.Count - 1 do
      begin
        // looking for "Pages:"
        tmpString := LowerCase(tmpList.Strings[i]);
        if Pos ('pages:', tmpString) > 0 then
        begin
          tmpString := Trim(StringReplace(tmpString, 'pages:', '' , [rfReplaceAll]));
          if tmpString = '' then
            tmpString := Trim(LowerCase(tmpList.Strings[i+1]));
          if TryToConvertToInteger(tmpString, aNumOfPages) then
            exit;
        end;
      end;
    finally
      tmpList.Free;
    end;
    aNumOfPages:= 0;
  end
  else
  begin
    FLastError := SMutool_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TMutoolToolbox.ExtractThumbnailOfFrontPageFromPdf(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight: word): boolean;
var
  outputString, cmd : string;
begin
  Result := false;
  if not CheckMutoolExePath then
    exit;
  if not CheckFile(aPdfFileName) then
    exit;

  {$IFDEF UNIX}
  if RunCommand(MutoolExePath, ['draw', '-o', aThumbnailFileName, '-w', IntToStr(aWidth), '-h', IntToStr(aHeight), aPdfFileName, '1'], outputString, [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  //cmd := 'draw -o "' + aThumbnailFileName + '" -w ' + IntToStr(aWidth) + ' -h ' + IntToStr(aHeight) + ' ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"') + ' 1';
  cmd := 'draw -o "' + aThumbnailFileName + '" -w ' + IntToStr(aWidth) + ' -h ' + IntToStr(aHeight) + ' ' + AnsiQuotedStr(aPdfFileName,'"') + ' 1';
  if RunCommand(MutoolExePath, [cmd], outputString, [poNoConsole,poWaitOnExit]) then
  {$ENDIF}
    Result := true
  else
  begin
    {$IFDEF UNIX}
    {$IFDEF DEBUG}
    writeln(outputString);
    {$ENDIF}
    {$ENDIF}
    FLastError := SMutool_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TMutoolToolbox.ExtractFrontPageFromPdf(const aPdfFileName, aDestinationFileName: string; const aResolution: integer): boolean;
var
  outputString, cmd : string;
begin
  Result := false;
  if not CheckMutoolExePath then
    exit;
  if not CheckFile(aPdfFileName) then
    exit;

  {$IFDEF UNIX}
  if RunCommand(MutoolExePath, ['draw', '-o', aDestinationFileName, '-r', IntToStr(aResolution), aPdfFileName, '1'], outputString, [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  // cmd := 'draw -o "' + aDestinationFileName + '" -r ' + IntToStr(aResolution) + ' ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"') + ' 1';
  cmd := 'draw -o "' + aDestinationFileName + '" -r ' + IntToStr(aResolution) + ' ' + AnsiQuotedStr(aPdfFileName,'"') + ' 1';
  if RunCommand(MutoolExePath, [cmd], outputString, [poNoConsole,poWaitOnExit]) then
  {$ENDIF}
    Result := true
  else
  begin
    {$IFDEF UNIX}
    {$IFDEF DEBUG}
    writeln(outputString);
    {$ENDIF}
    {$ENDIF}
    FLastError := SMutool_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TMutoolToolbox.SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate: string): boolean;
var
  outputString, cmd : string;
  thumbFileTemplate, thumbFilename : String;
  pages, i : integer;
begin
  Result := false;
  if not CheckMutoolExePath then
    exit;

  if not CheckFile(aPdfFileName) then
    exit;

  thumbFileTemplate := IncludeTrailingPathDelimiter(aPagesFolder) + aFileNameTemplate;

  Self.GetInfoFromPdf(aPdfFileName, pages);

  for i := 1 to pages do
  begin
    thumbFilename:= StringReplace(thumbFileTemplate, '%d', AddZerosFront(i, 3), [rfReplaceAll, rfIgnoreCase]);
    {$IFDEF UNIX}
    if not RunCommand(MutoolExePath, ['draw', '-o', thumbFilename, aPdfFileName, IntToStr(i)], outputString, [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
    {$ELSE}
    // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
    // cmd := 'draw -o "' + thumbFilename + '" ' + AnsiQuotedStr(UTF8ToWinCP(aPdfFileName),'"') + ' ' + IntToStr(i);
    cmd := 'draw -o "' + thumbFilename + '" ' + AnsiQuotedStr(aPdfFileName,'"') + ' ' + IntToStr(i);
    if not RunCommand(MutoolExePath, [cmd], outputString, [poNoConsole,poWaitOnExit]) then
    {$ENDIF}
    begin
      {$IFDEF UNIX}
      {$IFDEF DEBUG}
      writeln(outputString);
      {$ENDIF}
      {$ENDIF}
      FLastError := SMutool_error_unable_to_run + outputString;
      exit;
    end;
  end;
  Result := true;
end;

class function TMutoolToolbox.MergePdfFiles(const aFiles: TStringList; const aDestinationFileName: string): boolean;
var
  outputString, cmd : string;
  i : integer;
begin
  Result := false;
  if not CheckMutoolExePath then
    exit;

  // UTF8ToWinCP is no longer needed, this bug in TProcess was fixed: https://gitlab.com/freepascal.org/fpc/source/-/issues/29136
  cmd := 'merge -o "' + aDestinationFileName + '"';
  for i := 0 to aFiles.Count - 1 do
    cmd := cmd + ' ' + AnsiQuotedStr(aFiles.Strings[i],'"');
  //cmd := cmd + ' ' + AnsiQuotedStr(UTF8ToWinCP(aFiles.Strings[i]),'"');
  if not RunCommand(MutoolExePath, [cmd], outputString, [poNoConsole,poWaitOnExit]) then
  begin
    FLastError := SMutool_error_unable_to_run + outputString;
    exit;
  end;
  Result := true;
end;

class function TMutoolToolbox.GetLastError: String;
begin
  Result := FLastError;
end;

initialization
  FLastError := '';
{$IFDEF UNIX}
  MutoolExePath := '/usr/bin/mutool';
{$ENDIF}

end.
