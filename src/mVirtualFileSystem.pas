// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mVirtualFileSystem;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Contnrs, Classes;

type

  { TmFile }

  TmFile = class (TCollectionItem)
  protected
    FName : String;
    FID : String;
    FOwner : String;
    FFileDescriptor : String;
    FTags : TStringList;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    property ID : string read FID write FID;
    property Name : String read FName write FName;
    property Owner : String read FOwner write FOwner;
    property FileDescriptor : String read FFileDescriptor write FFileDescriptor;
    property Tags : TStringList read FTags;
  end;


(*  TmConfigurationFile = class
  protected
    FUseAsDefault : Boolean;
  public
    property UseAsDefault : Boolean read FUseAsDefault write FUseAsDefault;
  end;*)

  { TmFiles }

  TmFiles = class (TCollection)
  strict private
    function GetFile(Index :integer) : TmFile;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Add : TmFile;

    property Items[index:integer]: TmFile read GetFile; default;
  end;


  TmFolders = class;

  { TmFolder }

  TmFolder = class (TCollectionItem)
  strict private
    FFiles : TmFiles;
    FName : string;
    FPath : string;
    FFolders : TmFolders;
  private
    FParentFolder : TmFolder;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    property Files : TmFiles read FFiles;
    property Folders : TmFolders read FFolders;
    property Name : String read FName write FName;
    property Path : String read FPath write FPath;
  end;

  { TmFolders }

  TmFolders = class (TCollection)
  strict private
    FParentFolder : TmFolder;
    function GetFolder(Index :integer) : TmFolder;
  public
    constructor Create (aParentFolder : TmFolder); reintroduce;
    destructor Destroy; override;
    function Add : TmFolder;

    property Items[index:integer]: TmFolder read GetFolder; default;
  end;

  { TmAbstractFileSystemManager }

  TmAbstractFileSystemManager = class abstract
  protected
    FRoots : TmFolders;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RefreshStructure (aUser : String; aContext : String); virtual; abstract;
    procedure ReadStream (aFile : TmFile; aStream : TStream); virtual; abstract;
    procedure WriteStream (aFile : TmFile; aStream : TStream); virtual; abstract;

    property Roots : TmFolders read FRoots;
  end;

implementation

{ TmFolders }

function TmFolders.GetFolder(Index: integer): TmFolder;
begin
  Result := TmFolder(inherited Items[Index]);
end;

constructor TmFolders.Create(aParentFolder : TmFolder);
begin
  inherited Create(TmFolder);
  FParentFolder := aParentFolder;
end;

destructor TmFolders.Destroy;
begin
  inherited Destroy;
end;

function TmFolders.Add: TmFolder;
begin
  Result := TmFolder(inherited Add);
  Result.FParentFolder := FParentFolder;
end;

{ TmFiles }

function TmFiles.GetFile(Index: integer): TmFile;
begin
  Result := TmFile(inherited Items[Index]);
end;

constructor TmFiles.Create;
begin
  inherited Create (TmFile);
end;

destructor TmFiles.Destroy;
begin
  inherited Destroy;
end;

function TmFiles.Add: TmFile;
begin
  Result := TmFile(inherited Add);
end;

{ TmAbstractFileSystemManager }


constructor TmAbstractFileSystemManager.Create;
begin
  FRoots := TmFolders.Create(nil);
end;

destructor TmAbstractFileSystemManager.Destroy;
begin
  FRoots.Free;
  inherited Destroy;
end;

{ TmFile }

constructor TmFile.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FTags:= TStringList.Create;
end;

destructor TmFile.Destroy;
begin
  FTags.Free;
  inherited Destroy;
end;



{ TmFolder }


constructor TmFolder.Create(ACollection: TCollection);
begin
  inherited Create(aCollection);
  FFiles := TmFiles.Create;
  FFolders := TmFolders.Create(Self);
  FParentFolder := nil;
end;

destructor TmFolder.Destroy;
begin
  FFiles.Free;
  FFolders.Free;
  inherited Destroy;
end;


end.
