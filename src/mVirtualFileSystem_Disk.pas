// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mVirtualFileSystem_Disk;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mVirtualFileSystem, mUtility;

type

  { TDiskFileSystemManager }

  TDiskFileSystemManager = class (TmAbstractFileSystemManager)
  protected
    FFileMask: string;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ReadStream (aFile : TmFileData; aStream : TStream); override;
    procedure WriteStream (aFile : TmFileData; aStream : TStream); override;

    property FileMask : string read FFileMask write FFileMask;
  end;

  { TDiskSingleFolderFileSystemManager }

  TDiskSingleFolderFileSystemManager = class(TDiskFileSystemManager)
  strict private
    FExplodeSubFolders: boolean;
    FFullPathRootFolder : string;

    procedure FindFolder(aFolder: TmFolder);
    procedure ReloadFilesInFolder (aFolder : TmFolder);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Refresh; override;

    property FullPathRootFolder : string read FFullPathRootFolder write FFullPathRootFolder;
    property ExplodeSubFolders : boolean read FExplodeSubFolders write FExplodeSubFolders;
  end;

implementation
uses
  SysUtils, FileUtil;

{ TDiskSingleFolderFileSystemManager }

constructor TDiskSingleFolderFileSystemManager.Create;
begin
  inherited Create;
  FExplodeSubFolders := true;
end;

destructor TDiskSingleFolderFileSystemManager.Destroy;
begin
  inherited Destroy;
end;

constructor TDiskFileSystemManager.Create;
begin
  inherited Create;
  FFileMask:= '*.*';
end;

destructor TDiskFileSystemManager.Destroy;
begin
  inherited Destroy;
end;

procedure TDiskFileSystemManager.ReadStream(aFile: TmFileData; aStream: TStream);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(IncludeTrailingPathDelimiter(aFile.Path) +  aFile.FileName, fmOpenRead or fmShareDenyNoneFlags);
  try
    aStream.CopyFrom(fs, fs.Size);
    aStream.Position:= 0;
  finally
    fs.Free;
  end;
end;

procedure TDiskFileSystemManager.WriteStream(aFile: TmFileData; aStream: TStream);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(IncludeTrailingPathDelimiter(aFile.Path) +  aFile.FileName, fmCreate);
  try
    fs.Position:= 0;
    fs.CopyFrom(aStream, aStream.Position);
  finally
    fs.Free;
  end;
end;

procedure TDiskSingleFolderFileSystemManager.Refresh;
var
  root : TmFolder;
begin
  FRoots.Clear;
  root := FRoots.Add;
  root.Path:= FFullPathRootFolder;

  root.Name:= ExtractLastFolderFromPath(FFullPathRootFolder);
  ReloadFilesInFolder(root);

  if FExplodeSubFolders then
    FindFolder(root);
end;

procedure TDiskSingleFolderFileSystemManager.FindFolder(aFolder: TmFolder);
var
  F: TSearchRec;
  newFolder : TmFolder;
  fld : String;
begin
  fld := AppendPathDelim(aFolder.Path);
  try
    if FindFirst(fld + '*.*', faDirectory, F) = 0 then
    repeat
      if (F.Attr and faDirectory > 0) then
      begin
        if (F.Name<>'.') and (F.Name<>'..') then  // Not '.' and '..'
        begin
          newFolder := aFolder.Folders.Add;

          newFolder.Name:= F.Name;
          newFolder.Path:= fld + F.Name;
          ReloadFilesInFolder(newFolder);
          FindFolder(newFolder);
        end;
      end;
    until FindNext(F) <> 0;
  finally
    SysUtils.FindClose(F);
  end;
end;

procedure TDiskSingleFolderFileSystemManager.ReloadFilesInFolder(aFolder: TmFolder);
var
  F: TSearchRec;
  newFile : TmFile;
  fld : string;
begin
  fld := AppendPathDelim(aFolder.Path);
  try
    if FindFirst(fld + FFileMask, faAnyFile, F) = 0 then
    repeat
      if (F.Attr and faDirectory = 0) then
      begin
        newFile := aFolder.Files.Add;
        newFile.FileData.Path:= fld;
        newFile.FileData.FileName:= F.Name;
        newFile.FileData.Name:= F.Name;
      end;
    until FindNext(F) <> 0;
  finally
    SysUtils.FindClose(F);
  end;
end;

end.
