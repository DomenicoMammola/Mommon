// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mVirtualFileSystem_Disk_SingleFolder;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mVirtualFileSystem;


type

  { TDiskSingleFolderFileSystemManager }

  TDiskSingleFolderFileSystemManager = class(TmAbstractFileSystemManager)
  strict private
    FExplodeSubFolders: boolean;
    FFileMask: string;
    FFullPathRootFolder : string;

    procedure FindFolder(aFolder: TmFolder);
    procedure ReloadFilesInFolder (aFolder : TmFolder);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ReadStream (aFile : TmFile; aStream : TStream); override;
    procedure WriteStream (aFile : TmFile; aStream : TStream); override;

    procedure Refresh; override;

    property FullPathRootFolder : string read FFullPathRootFolder write FFullPathRootFolder;
    property ExplodeSubFolders : boolean read FExplodeSubFolders write FExplodeSubFolders;
    property FileMask : string read FFileMask write FFileMask;
  end;

implementation
uses
  SysUtils, FileUtil;

{ TDiskSingleFolderFileSystemManager }

constructor TDiskSingleFolderFileSystemManager.Create;
begin
  inherited Create;
  FFileMask:= '*.*';
  FExplodeSubFolders := true;
end;

destructor TDiskSingleFolderFileSystemManager.Destroy;
begin
  inherited Destroy;
end;


procedure TDiskSingleFolderFileSystemManager.ReadStream(aFile: TmFile; aStream: TStream);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(aFile.FileDescriptor, fmOpenRead or fmShareDenyNoneFlags);
  try
    aStream.CopyFrom(fs, fs.Size);
    aStream.Position:= 0;
  finally
    fs.Free;
  end;
end;

procedure TDiskSingleFolderFileSystemManager.WriteStream(aFile: TmFile; aStream: TStream);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(aFile.FileDescriptor, fmCreate);
  try
    fs.Position:= 0;
    fs.CopyFrom(aStream, aStream.Position);
  finally
    fs.Free;
  end;
end;

procedure TDiskSingleFolderFileSystemManager.Refresh;
var
  tmp : TStringList;
  root : TmFolder;
begin
  FRoots.Clear;
  root := FRoots.Add;
  root.Path:= FFullPathRootFolder;

  tmp := TStringList.Create;
  try
    ExtractStrings(AllowDirectorySeparators, [], PChar(FFullPathRootFolder), tmp, false );
    root.Name:= tmp.Strings[tmp.Count - 1];
  finally
    tmp.Free;
  end;

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
        newFile.FileDescriptor:= fld + F.Name;
        newFile.Name:= F.Name;
      end;
    until FindNext(F) <> 0;
  finally
    SysUtils.FindClose(F);
  end;
end;

end.
