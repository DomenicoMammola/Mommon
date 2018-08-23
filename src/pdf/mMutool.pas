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

type

  TMutoolToolboxException = class (Exception);


  { TMutoolToolbox }

  TMutoolToolbox = class
  strict private
    class procedure CheckMutoolExePath;
  public
    class function ExtractTextFromPdf(const aPdfFileName: string): string;
    class procedure ExtractImagesFromPdf(const aPdfFileName: string; aImagesFiles : TStringList);
  end;

var
  MutoolExePath : string;

implementation

uses
  process, strutils;

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
    raise TMutoolToolboxException.Create('Pdf file is missing: ' + aPdfFileName);
  if RunCommand(MutoolExePath, ['draw -F txt "' + aPdfFileName + '"'], outputString, [poNoConsole]) then
    Result := outputString
  else
    raise TMutoolToolboxException.Create('Unable to run Mutool.');

end;

class procedure TMutoolToolbox.ExtractImagesFromPdf(const aPdfFileName: string; aImagesFiles: TStringList);
var
  outputString, curPath, tmpString : string;
  tmpList : TStringList;
  i : integer;
begin
  CheckMutoolExePath;
  if not FileExists(aPdfFileName) then
    raise TMutoolToolboxException.Create('Pdf file is missing: ' + aPdfFileName);
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
    raise TMutoolToolboxException.Create('Unable to run Mutool.');
end;

end.
