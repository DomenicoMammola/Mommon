// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mPdfMerger;

interface

uses
  Classes;

const
  MERGE_NUM_FILES_LIMIT = 10;


  function MergeFiles (aFilesToBeMerged : TStringList; const aWorkingFolder : String; const aDestinationFile : string; out aError : string) : boolean;

implementation

uses
  FileUtil, SysUtils, Math,
  mPoppler, mUtility;

function MergeFiles (aFilesToBeMerged : TStringList; const aWorkingFolder : String; const aDestinationFile : string; out aError : string) : boolean;
var
  temporaryFiles, curFilesToBeMerged : TStringList;
  k, z : integer;
  tmpFile : String;
begin
  Result := false;
  temporaryFiles := TStringList.Create;
  curFilesToBeMerged := TStringList.Create;
  try
    if aFilesToBeMerged.Count = 1 then
    begin
      CopyFile(aFilesToBeMerged.Strings[0], aDestinationFile);
      Result := true;
    end
    else
    begin
      k := 0;
      while k < aFilesToBeMerged.Count do
      begin
        curFilesToBeMerged.Clear;
        for z := k to min(aFilesToBeMerged.Count - 1, (k + MERGE_NUM_FILES_LIMIT - 1)) do
          curFilesToBeMerged.Add(aFilesToBeMerged.Strings[z]);
        tmpFile := IncludeTrailingPathDelimiter(aWorkingFolder) + GenerateRandomIdString(20) + '.pdf';
        temporaryFiles.Add(tmpFile);
        if not TPopplerToolbox.MergePdfFiles(curFilesToBeMerged, tmpFile) then
        begin
          aError := TPopplerToolbox.GetLastError;
          exit;
        end;
        k := k + MERGE_NUM_FILES_LIMIT;
      end;
      Result := MergeFiles(temporaryFiles, aWorkingFolder, aDestinationFile, aError);
    end;
  finally
    temporaryFiles.Free;
    curFilesToBeMerged.Free;
  end;

end;

end.
