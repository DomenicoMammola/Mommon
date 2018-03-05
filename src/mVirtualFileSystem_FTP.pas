// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mVirtualFileSystem_FTP;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls,

  IdTCPClient, IdFTP, IdFTPCommon,

  mVirtualFileSystem, mUtility;

type

  TFTPFileType = (ftFile, ftDir);

  { TFTPThread }

  TFTPThread = class (TThread)
  public
    procedure Execute; override;
  end;

  { TFTPFileSystemManager }

  TFTPFileSystemManager = class(TmAbstractFileSystemManager)
  protected
    FHost : string;
    FUsername : string;
    FPassword : string;
    FFileMask : string;
    function ExtractTypeAndName (const aSourceString : string; var aFileType : TFTPFileType; var aFileName : string) : boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadStream (aFile : TmFileData; aStream : TStream); override;
    procedure WriteStream (aFile : TmFileData; aStream : TStream); override;
    function ValidateFileName (aFileName : string) : string; override;

    property Host : string read FHost write FHost;
    property Username : string read FUsername write FUsername;
    property Password : string read FPassword write FPassword;
    property FileMask : string read FFileMask write FFileMask;
  end;

  { TFTPFoldersListFileSystem }

  TFTPFoldersListFileSystem = class (TFTPFileSystemManager)
  strict private
    FFTPFolders: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Refresh(const aIsGUIApplication : boolean); override;

    property FTPFolders : TStringList read FFTPFolders;
  end;


implementation

{ TFTPThread }

procedure TFTPThread.Execute;
begin

end;

{ TFTPFoldersListFileSystem }

constructor TFTPFoldersListFileSystem.Create;
begin
  inherited Create;
  FFTPFolders := TStringList.Create;
end;

destructor TFTPFoldersListFileSystem.Destroy;
begin
  FFTPFolders.Free;
  inherited Destroy;
end;

procedure TFTPFoldersListFileSystem.Refresh(const aIsGUIApplication : boolean);
var
  FTPClient : TIdFTP;
  i, k : integer;
  tmpFolder : TmFolder;
  tmpFile : TmFile;
  tmpFileType : TFTPFileType;
  tmpFileName : string;
  tmpCursor : TCursor;
begin
  if aIsGUIApplication then
  begin
    tmpCursor := Screen.Cursor;
    Screen.Cursor:= crHourGlass;
  end;
  try
    FRoots.Clear;
    FTPClient := TIdFTP.Create(nil);
    try
      FTPClient.Host:= FHost;
      FTPClient.Username:= FUsername;
      FTPClient.Password:= FPassword;
      try
        FTPClient.Connect;
      except
        on e: Exception do
        begin
          raise Exception.Create('Unable to connect to the ftp server. Error:' + sLineBreak + e.Message);
        end;
      end;
      if FTPClient.Connected then
      begin
        for i := 0 to FFTPFolders.Count - 1 do
        begin
          FTPClient.ChangeDir('/');
          FTPClient.ChangeDir(FFTPFolders.Strings[i]);

          tmpFolder := FRoots.Add;
          tmpFolder.Name:= ExtractLastFolderFromPath(FFTPFolders.Strings[i]);
          tmpFolder.Path:= FFTPFolders.Strings[i];

          FTPClient.List(FFileMask);
          for k := 0 to FTPClient.ListResult.Count - 1 do
          begin
            if ExtractTypeAndName(FTPClient.ListResult[k], tmpFileType, tmpFileName) then
            begin
              if tmpFileType = ftFile then
              begin
                tmpFile := tmpFolder.Files.Add;
                tmpFile.FileData.Name:= tmpFileName;
                tmpFile.FileData.FileName:= tmpFileName;
                tmpFile.FileData.Path:= FFTPFolders.Strings[i];
              end;
            end;
          end;
        end;
      end;
      FTPClient.Disconnect;
    finally
      FTPClient.Free;
    end;
  finally
    if aIsGUIApplication then
      Screen.Cursor:= tmpCursor;
  end;
end;

{ TFTPFileSystemManager }

function TFTPFileSystemManager.ExtractTypeAndName(const aSourceString: string; var aFileType: TFTPFileType; var aFileName: string) : boolean;
var
  tmp : TStringList;
begin
  Result := false;

  tmp := TStringList.Create;
  try
    ExtractStrings([';'], [], PChar(aSourceString), tmp, false );
    if tmp.Count >= 3 then
    begin
      if Pos('dir', tmp.Strings[0]) > 0 then
      begin
        aFileType := ftDir;
        aFileName:= Trim(tmp.Strings[2]);
      end
      else if Pos('file', tmp.Strings[0])> 0 then
      begin
        if tmp.Count < 4 then
          exit;

        aFileType:= ftFile;
        aFileName:= Trim(tmp.Strings[3]);
      end
      else
        exit;
      Result := true;
    end;
  finally
    tmp.Free;
  end;
end;

constructor TFTPFileSystemManager.Create;
begin
  inherited Create;
  FFileMask:= '*.*';
  FHost:='';
  FPassword:='';
  FUsername:='';
end;

destructor TFTPFileSystemManager.Destroy;
begin
  inherited Destroy;
end;


procedure TFTPFileSystemManager.ReadStream(aFile: TmFileData; aStream: TStream);
var
  FTPClient : TIdFTP;
begin
  FTPClient := TIdFTP.Create(nil);
  try
    FTPClient.Host:= FHost;
    FTPClient.Username:= FUsername;
    FTPClient.Password:= FPassword;
    FTPClient.Connect;
    if FTPClient.Connected then
    begin
      FTPClient.TransferType := ftBinary;
      FTPClient.Passive:= true;
      FTPClient.ChangeDir(aFile.Path);
      FTPClient.Get(aFile.FileName, aStream);
    end;
    FTPClient.Disconnect;
  finally
    FTPClient.Free;
  end;
end;

procedure TFTPFileSystemManager.WriteStream(aFile: TmFileData; aStream: TStream);
var
  FTPClient : TIdFTP;
begin
  FTPClient := TIdFTP.Create(nil);
  try
    FTPClient.Host:= FHost;
    FTPClient.Username:= FUsername;
    FTPClient.Password:= FPassword;
    FTPClient.Connect;
    if FTPClient.Connected then
    begin
      FTPClient.TransferType := ftBinary;
      FTPClient.Passive:= true;
      FTPClient.ChangeDir(aFile.Path);
      if FTPClient.Size(aFile.FileName) > 0 then
        FTPClient.Delete(aFile.FileName);
      FTPClient.Put(aStream, aFile.FileName);
    end;
    FTPClient.Disconnect;
  finally
    FTPClient.Free;
  end;
end;

function TFTPFileSystemManager.ValidateFileName(aFileName: string): string;
begin
  Result := trim(aFileName);
  if Result <> '' then
    Result := ChangeFileExt(Result, ExtractFileExt(FFileMask));
end;

end.
