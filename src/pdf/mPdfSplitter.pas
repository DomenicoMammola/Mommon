// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mPdfSplitter;

interface

uses
  Contnrs,
  mPoppler, mProgress;

type

  { TSplitCommand }

  TSplitCommand = class
  strict private
    FPageFrom : word;
    FPageTo : word;
    FFileNameDestination : string;
  public
    constructor Create;
    destructor Destroy; override;

    property PageFrom : word read FPageFrom write FPageFrom;
    property PageTo : word read FPageTo write FPageTo;
    property FileNameDestination : string read FFileNameDestination write FFileNameDestination;
  end;

  { TSplitCommands }

  TSplitCommands = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function Get (const aIndex : integer): TSplitCommand;
    function Add: TSplitCommand;
    function Count : integer;
    procedure Clear;
  end;

  function SplitPdfFile(const aSourceFileName : String; const aCommands : TSplitCommands; aProgress: ImProgress; out aError : String) : boolean;

implementation

uses
  SysUtils, Math, Classes, FileUtil,
  mUtility;

const
  MAX_FILES = 10;

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
        for z := k to min(aFilesToBeMerged.Count - 1, (k + MAX_FILES - 1)) do
          curFilesToBeMerged.Add(aFilesToBeMerged.Strings[z]);
        tmpFile := IncludeTrailingPathDelimiter(aWorkingFolder) + GenerateRandomIdString(20) + '.pdf';
        temporaryFiles.Add(tmpFile);
        if not TPopplerToolbox.MergePdfFiles(curFilesToBeMerged, tmpFile) then
        begin
          aError := TPopplerToolbox.GetLastError;
          exit;
        end;
        k := k + MAX_FILES;
      end;
      Result := MergeFiles(temporaryFiles, aWorkingFolder, aDestinationFile, aError);
    end;
  finally
    temporaryFiles.Free;
    curFilesToBeMerged.Free;
  end;

end;

function SplitPdfFile(const aSourceFileName: String; const aCommands: TSplitCommands; aProgress: ImProgress; out aError: String): boolean;
var
  tmpPagesFolder, tmpFile : string;
  pdfInfo : TPopplerPdfInfo;
  i, k, z : integer;
  start, stop : word;
  filesToBeMerged : TStringList;
begin
  Result := false;
  tmpPagesFolder:= IncludeTrailingPathDelimiter(GetTempDir) + GenerateRandomIdString(30);
  if not DirectoryExists(tmpPagesFolder) then
    if not ForceDirectories(tmpPagesFolder) then
    begin
      aError := 'Unable to create temporary folder';
      exit;
    end;
  aProgress.Notify('Retrieving information from source file...');
  if not TPopplerToolbox.GetInfoFromPdf(aSourceFileName, pdfInfo) then
  begin
    aError := TPopplerToolbox.GetLastError;
    exit;
  end;
  if pdfInfo.Pages = 0 then
  begin
    aError := 'No page found';
    exit;
  end;
  aProgress.Notify('Splitting source file in pages...');
  if not TPopplerToolbox.SplitPdfInPages(aSourceFileName, tmpPagesFolder, 'page%d.pdf') then
  begin
    aError:= TPopplerToolbox.GetLastError;
    exit;
  end;

  for i := 0 to aCommands.Count - 1 do
  begin
    aProgress.Notify(Format('Processing command #%d...', [i + 1]));
    if aCommands.Get(i).PageFrom = 0 then
      start := 1
    else
      start := aCommands.Get(i).PageFrom;
    if aCommands.Get(i).PageTo = 0 then
      stop := pdfInfo.Pages
    else
      stop := min(aCommands.Get(i).PageTo, pdfInfo.Pages);
    if start > stop then
    begin
      aError:= 'Start page cannot follow end page';
      exit;
    end;

    filesToBeMerged := TStringList.Create;
    try
      for k := start to stop do
        filesToBeMerged.Add(IncludeTrailingPathDelimiter(tmpPagesFolder) + Format('page%d.pdf', [k]));
      if not MergeFiles(filesToBeMerged, tmpPagesFolder, aCommands.Get(i).FileNameDestination, aError) then
        exit;
    finally
      filesToBeMerged.Free;
    end;
  end;
  try
    if DirectoryExists(tmpPagesFolder) then
      DeleteDirectory(tmpPagesFolder, false);
  except
    on e: Exception do
    begin
      // ignored
    end;
  end;
  Result := true;
end;

{ TSplitCommands }

constructor TSplitCommands.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TSplitCommands.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TSplitCommands.Get(const aIndex: integer): TSplitCommand;
begin
  Result := FList.Items[aIndex] as TSplitCommand;
end;

function TSplitCommands.Add: TSplitCommand;
begin
  Result := TSplitCommand.Create;
  FList.Add(Result);
end;

function TSplitCommands.Count: integer;
begin
  Result := FList.Count;
end;

procedure TSplitCommands.Clear;
begin
  FList.Clear;
end;

{ TSplitCommand }

constructor TSplitCommand.Create;
begin
  FPageFrom:= 0;
  FPageTo := 0;
  FFileNameDestination := '';
end;

destructor TSplitCommand.Destroy;
begin
  inherited Destroy;
end;

end.
