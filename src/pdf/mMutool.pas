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
unit mMutool;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, sysutils;

resourcestring
  SMutool_error_file_missing = 'Pdf file is missing: ';
  SMutool_error_unable_to_run = 'Unable to run Mutool.';

type

  TMutoolToolboxException = class (Exception);


  { TMutoolToolbox }

  TMutoolToolbox = class
  strict private
    class procedure CheckMutoolExePath;
  public
    class function ExtractTextFromPdf(const aPdfFileName: string): string;
    class procedure ExtractImagesFromPdf(const aPdfFileName: string; aImagesFiles : TStringList);
    class procedure GetInfoFromPdf(const aPdfFileName: string; out aNumOfPages : integer);
    class function ExtractThumbnailOfFrontPageFromPdf(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight : word) : boolean;
    // aFileNameTemplate = page%d.pdf
    class function SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate : string): boolean;
  end;

var
  MutoolExePath : string;

implementation

uses
  process, strutils,
  mMathUtility;

{ TMutoolToolbox }

class procedure TMutoolToolbox.CheckMutoolExePath;
begin
  {$IFDEF WINDOWS}
  if not FileExists(MutoolExePath) then
    raise TMutoolToolboxException.Create('Mutool.exe path value is missing.');
  {$ENDIF}
end;

class function TMutoolToolbox.ExtractTextFromPdf(const aPdfFileName: string): string;
var
  outputString : string;
begin
  CheckMutoolExePath;
  if not FileExists(aPdfFileName) then
    raise TMutoolToolboxException.Create(SMutool_error_file_missing + aPdfFileName);
  if RunCommand(MutoolExePath, ['draw -F txt "' + aPdfFileName + '"'], outputString, [poNoConsole]) then
    Result := outputString
  else
    raise TMutoolToolboxException.Create(SMutool_error_unable_to_run);
end;

class procedure TMutoolToolbox.ExtractImagesFromPdf(const aPdfFileName: string; aImagesFiles: TStringList);
var
  outputString, curPath, tmpString : string;
  tmpList : TStringList;
  i : integer;
begin
  CheckMutoolExePath;
  if not FileExists(aPdfFileName) then
    raise TMutoolToolboxException.Create(SMutool_error_file_missing + aPdfFileName);
  if RunCommand(MutoolExePath, ['extract "' + aPdfFileName + '"'], outputString, [poNoConsole]) then
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
    raise TMutoolToolboxException.Create(SMutool_error_unable_to_run);
end;

class procedure TMutoolToolbox.GetInfoFromPdf(const aPdfFileName: string; out aNumOfPages: integer);
var
  outputString, tmpString : string;
  tmpList : TStringList;
  i : integer;
begin
  aNumOfPages:= 0;

  CheckMutoolExePath;
  if not FileExists(aPdfFileName) then
    raise TMutoolToolboxException.Create(SMutool_error_file_missing + aPdfFileName);

  if RunCommand(MutoolExePath, ['info "' + aPdfFileName + '"'], outputString, [poNoConsole]) then
  begin
    tmpList := TStringList.Create;
    try
      tmpList.Delimiter:= #10;
      tmpList.DelimitedText:= outputString;
      for i := 0 to tmpList.Count - 1 do
      begin
        // looking for "Pages:"
        tmpString := LowerCase(tmpList.Strings[i]);
        if Pos ('pages:', tmpString) >= 0 then
        begin
          tmpString := Trim(StringReplace(tmpString, 'pages:', '' , [rfReplaceAll]));
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
    raise TMutoolToolboxException.Create(SMutool_error_unable_to_run);
end;

class function TMutoolToolbox.ExtractThumbnailOfFrontPageFromPdf(const aPdfFileName, aThumbnailFileName: string; const aWidth, aHeight: word): boolean;
var
  outputString, cmd : string;
begin
  Result := false;
  CheckMutoolExePath;
  if not FileExists(aPdfFileName) then
    raise TMutoolToolboxException.Create(SMutool_error_file_missing + aPdfFileName);

  {$IFDEF UNIX}
  if RunCommand(MutoolExePath, ['draw', '-o', aThumbnailFileName, '-w', IntToStr(aWidth), '-h', IntToStr(aHeight), aPdfFileName, '1'], outputString, [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  cmd := 'draw -o "' + aThumbnailFileName + '" -w ' + IntToStr(aWidth) + ' -h ' + IntToStr(aHeight) + ' "' + aPdfFileName + '" 1';
  if RunCommand(MutoolExePath, [cmd], outputString, [poNoConsole]) then
  {$ENDIF}
    Result := true
  else
  begin
    {$IFDEF UNIX}
    {$IFDEF DEBUG}
    writeln(outputString);
    {$ENDIF}
    {$ENDIF}
    raise TMutoolToolboxException.Create(SMutool_error_unable_to_run + ' ' + outputString);
  end;
  //mutool draw -o pippo.png -w 250 -h 350 file.pdf 1
end;

class function TMutoolToolbox.SplitPdfInPages(const aPdfFileName, aPagesFolder, aFileNameTemplate: string): boolean;
var
  outputString, cmd : string;
  thumbFile : String;
begin
  Result := false;
  CheckMutoolExePath;
  if not FileExists(aPdfFileName) then
    raise TMutoolToolboxException.Create(SMutool_error_file_missing + aPdfFileName);

  thumbFile := IncludeTrailingPathDelimiter(aPagesFolder) + aFileNameTemplate;

  {$IFDEF UNIX}
  if RunCommand(MutoolExePath, ['draw', '-o', thumbFile, aPdfFileName], outputString, [poStderrToOutPut, poUsePipes, poWaitOnExit]) then
  {$ELSE}
  cmd := 'draw -o "' + thumbFile + '" "' + aPdfFileName + '"';
  if RunCommand(MutoolExePath, [cmd], outputString, [poNoConsole]) then
  {$ENDIF}
    Result := true
  else
  begin
    {$IFDEF UNIX}
    {$IFDEF DEBUG}
    writeln(outputString);
    {$ENDIF}
    {$ENDIF}
    raise TMutoolToolboxException.Create(SMutool_error_unable_to_run + ' ' + outputString);
  end;
end;

{$IFDEF UNIX}
initialization
  MutoolExePath := '/usr/bin/mutool';
{$ENDIF}

end.
