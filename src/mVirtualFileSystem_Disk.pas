unit mVirtualFileSystem_Disk;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mVirtualFileSystem;


type

  { TDiskFileSystemManager }

  TDiskFileSystemManager = class(TmAbstractFileSystemManager)
  strict private
    procedure FindFolder (aFolder: TmFolder; const aScanForFiles: boolean; const aFileMask : string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RefreshStructure (aUser : String; aContext : String); override;
    procedure ReadStream (aFile : TmFile; aStream : TStream); override;
    procedure WriteStream (aFile : TmFile; aStream : TStream); override;

    procedure RegisterDiskFolder (const aFullPath : String; const aScanForFiles, aAddChilds: boolean; const aFileMask : string);
    procedure ReloadFiles (aFolder : TmFolder; aFileMask : String);
  end;

implementation
uses
  SysUtils, FileUtil;

{ TDiskFileSystemManager }

constructor TDiskFileSystemManager.Create;
begin
  inherited Create;
end;

destructor TDiskFileSystemManager.Destroy;
begin
  inherited Destroy;
end;

procedure TDiskFileSystemManager.RefreshStructure(aUser: String; aContext: String);
begin
  //
end;

procedure TDiskFileSystemManager.ReadStream(aFile: TmFile; aStream: TStream);
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

procedure TDiskFileSystemManager.WriteStream(aFile: TmFile; aStream: TStream);
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

procedure TDiskFileSystemManager.FindFolder(aFolder: TmFolder; const aScanForFiles: boolean; const aFileMask : string);
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
          if aScanForFiles then
            ReloadFiles(newFolder, aFileMask);
          FindFolder(newFolder, aScanForFiles, aFileMask);
        end;
      end;
    until FindNext(F) <> 0;
  finally
    SysUtils.FindClose(F);
  end;
end;

procedure TDiskFileSystemManager.RegisterDiskFolder(const aFullPath: String; const aScanForFiles, aAddChilds: boolean; const aFileMask : string);
var
  tmp : TStringList;
  root : TmFolder;
begin
  FRoots.Clear;
  root := FRoots.Add;
  root.Path:= aFullPath;
  //Name := aFullPath;

  tmp := TStringList.Create;
  try
    ExtractStrings(AllowDirectorySeparators, [], PChar(aFullPath), tmp, false );
    root.Name:= tmp.Strings[tmp.Count - 1];
  finally
    tmp.Free;
  end;

  if aScanForFiles then
    ReloadFiles(root, aFileMask);

  if aAddChilds then
    FindFolder(root, aScanForFiles, aFileMask);
end;

procedure TDiskFileSystemManager.ReloadFiles(aFolder: TmFolder; aFileMask : String);
var
  F: TSearchRec;
  newFile : TmFile;
  fld : string;
begin
  fld := AppendPathDelim(aFolder.Path);
  try
    if FindFirst(fld + aFileMask, faAnyFile, F) = 0 then
    repeat
      if (F.Attr and faDirectory <> 0) then
      begin
        newFile := aFolder.Files.Add;
        newFile.FileDescriptor:= fld + F.Name;
      end;
    until FindNext(F) <> 0;
  finally
    SysUtils.FindClose(F);
  end;
end;

end.
