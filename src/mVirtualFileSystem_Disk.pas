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
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RefreshStructure (aUser : String; aContext : String); override;
    procedure ReadStream (aFile : TmFile; aStream : TStream); override;
    procedure WriteStream (aFile : TmFile; aStream : TStream); override;

    procedure RegisterFileSystemFolder (const aFullPath : String; aAddChilds : boolean);
  end;

implementation
uses
  SysUtils;

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

procedure TDiskFileSystemManager.RegisterFileSystemFolder(const aFullPath: String; aAddChilds: boolean);
var
  tmp : TStringList;
begin
  FRoots.Clear;
  with FRoots.Add do
  begin
    Path:= aFullPath;
    Name := aFullPath;

    tmp := TStringList.Create;
    try
      ExtractStrings(AllowDirectorySeparators, [], aFullPath, tmp, false );
      Name:= tmp.Strings[tmp.Count - 1];

    finally
      tmp.Free;
    end;
  end;

end;

end.
