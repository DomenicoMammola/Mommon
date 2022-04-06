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

{$I mDefines.inc}

uses
  Classes, SysUtils, {$IFDEF GUI}Controls, Forms, {$ENDIF}

  IdFTP, IdSSLOpenSSL,

  mVirtualFileSystem, mUtility;

type

  TFTPFileType = (ftFile, ftDir);

  { TFTPFileSystemManager }

  TFTPFileSystemManager = class(TmAbstractFileSystemManager)
  protected
    FHost : string;
    FUsername : string;
    FPassword : string;
    FFileMask : string;
    FUseFTPS : boolean;
    FSSLIoHandler: TIdSSLIOHandlerSocketOpenSSL;
    function ExtractTypeAndName (const aSourceString : string; out aFileType : TFTPFileType; out aFileName : string) : boolean;
    procedure InternalCreateSubFolders(aFTPClient : TIdFTP; const aPath: string);
    function GetFTPParentPath(const aPath : String): String;
    function GetFTPLastFolderInPath(const aPath : String): String;
    function CreateConnection : TIdFTP;
  public
    const PATH_DELIMITER = '/';
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ReadStream (const aFileName, aFileFolder : String; aStream : TStream); override;
    procedure WriteStream (const aFileName, aFileFolder : String; aStream : TStream); override;

    function ValidateFileName (const aFileName : string) : string; override;
    procedure CreatePath(const aPath: string); override;
    function IncludeTrailingPathFSDelimiter (const aPath : String): String; override;
    procedure GetAllFiles (aRoots: TmFolders); override;
    procedure DeleteFile (const aFileName, aFileFolder : String); override;
    function FileExists(const aFileName, aFileFolder : String): boolean; override;

    property Host : string read FHost write FHost;
    property Username : string read FUsername write FUsername;
    property Password : string read FPassword write FPassword;
    property FileMask : string read FFileMask write FFileMask;
    property UseFTPS : boolean read FUseFTPS write FUseFTPS;
  end;

  { TFTPFoldersListFileSystem }

  TFTPFoldersListFileSystem = class (TFTPFileSystemManager)
  strict private
    FFTPFolders: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure GetAllFiles (aRoots: TmFolders); override;

    property FTPFolders : TStringList read FFTPFolders;
  end;

  function IncludeTrailingPathFTPDelimiter (const aPath : String): String;

implementation

uses
  IdTCPClient, IdFTPCommon, IdReplyRFC, IdReplyFTP, IdExplicitTLSClientServerBase
  {$IFDEF LINUX},FileUtil, IdSSLOpenSSLHeaders{$ENDIF}
  ;

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

procedure TFTPFoldersListFileSystem.GetAllFiles (aRoots: TmFolders);
var
  FTPClient : TIdFTP;
  i, k : integer;
  tmpFolder : TmFolder;
  tmpFile : TmFile;
  tmpFileType : TFTPFileType;
  tmpFileName : string;
begin
  aRoots.Clear;
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
        FTPClient.ChangeDir(PATH_DELIMITER);
        FTPClient.ChangeDir(FFTPFolders.Strings[i]);

        tmpFolder := aRoots.Add;
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
end;


{ TFTPFileSystemManager }

function TFTPFileSystemManager.ExtractTypeAndName(const aSourceString: string; out aFileType: TFTPFileType; out aFileName: string) : boolean;
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


// https://www.howtobuildsoftware.com/index.php/how-do/bqWO/delphi-indy10-delphi-xe7-idftp-direxists-and-makedir
procedure TFTPFileSystemManager.InternalCreateSubFolders(aFTPClient : TIdFTP; const aPath: string);
var
  dirExists : boolean;
  tmpFolder : String;
begin
  try
    aFTPClient.List(nil, aPath, false);
    dirExists := True;
    if aFTPClient.LastCmdResult.NumericCode = 450 then
    begin
      if Pos('no such', LowerCase(aFTPClient.LastCmdResult.Text.Text)) > 0 then //  has a message like 'No such file or directory' or similar)
        dirExists := false;
    end;
  except
    on e: EIdReplyRFCError do
    begin
      if (e.ErrorCode <> 550) or
        (Pos('not found', LowerCase(e.Message)) = 0) then
        //(e.Message does not have a message like 'Directory not found' or similar) then begin
        raise
      else
        dirExists := false;
    end;
  end;

  if not dirExists then
  begin
    tmpFolder := GetFTPParentPath(aPath);
    if tmpFolder <> '' then
    begin
      aFTPClient.ChangeDir(PATH_DELIMITER);
      aFTPClient.ChangeDir(tmpFolder);
    end;
    tmpFolder := GetFTPLastFolderInPath(aPath);
    if tmpFolder <> '' then
      aFTPClient.MakeDir(tmpFolder);
  end;
end;

function TFTPFileSystemManager.GetFTPParentPath(const aPath: String): String;
var
  i : integer;
begin
  Result := '';
  for i := (Length(aPath) - 1) downto 1 do
  begin
    if aPath[i] = PATH_DELIMITER then
    begin
      Result := Copy(aPath, 1, i - 1);
      exit;
    end;
  end;
end;

function TFTPFileSystemManager.GetFTPLastFolderInPath(const aPath: String): String;
var
  i, l : integer;
begin
  Result := '';
  for i := (Length(aPath) - 1) downto 1 do
  begin
    if aPath[i] = PATH_DELIMITER then
    begin
      Result := Copy(aPath, i + 1, 9999);
      l := Length(Result);
      if Result[l] = PATH_DELIMITER then
      begin
        if l > 1 then
          Result := Copy(Result, 1, l-1)
        else
          Result := '';
      end;
      exit;
    end;
  end;
end;

function TFTPFileSystemManager.CreateConnection: TIdFTP;
begin
  Result := TIdFTP.Create(nil);
  {$IFDEF LINUX}
  // https://synaptica.info/2021/01/12/delphi-10-4-1-indy-ssl-on-ubuntu-20-04/
  IdOpenSSLSetLibPath(ProgramDirectory);
  {$ENDIF}
  Result.Host:= FHost;
  Result.Username:= FUsername;
  Result.Password:= FPassword;
  if FUseFTPS then
  begin
    // https://stackoverflow.com/questions/14855728/how-can-i-make-my-delphi-application-use-ftps-instead-of-ftp-indy
    if not Assigned(FSSLIoHandler) then
    begin
      FSSLIoHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      // https://en.delphipraxis.net/topic/5727-exception-message-error-connecting-with-ssl-eof-was-observed-that-violates-the-protocol/
      FSSLIoHandler.SSLOptions.Method := sslvTLSv1_2;
    end;
    Result.IOHandler:= FSSLIoHandler;
    Result.UseTLS := utUseExplicitTLS;
    Result.DataPortProtection := ftpdpsPrivate;
  end;
end;

constructor TFTPFileSystemManager.Create;
begin
  inherited Create;
  FFileMask:= '*.*';
  FHost:='';
  FPassword:='';
  FUsername:='';
  FSSLIoHandler:= nil;
  FUseFTPS:= false;
end;

destructor TFTPFileSystemManager.Destroy;
begin
  FreeAndNil(FSSLIoHandler);
  inherited Destroy;
end;


procedure TFTPFileSystemManager.ReadStream(const aFileName, aFileFolder : String; aStream : TStream);
var
  FTPClient : TIdFTP;
begin
  FTPClient := CreateConnection;
  try
    FTPClient.Connect;
    if FTPClient.Connected then
    begin
      FTPClient.TransferType := ftBinary;
      FTPClient.Passive:= true;
      FTPClient.ChangeDir(PATH_DELIMITER);
      FTPClient.ChangeDir(aFileFolder);
      FTPClient.Get(aFileName, aStream);
    end;
    FTPClient.Disconnect;
  finally
    FTPClient.Free;
  end;
end;

procedure TFTPFileSystemManager.WriteStream(const aFileName, aFileFolder : String; aStream: TStream);
var
  FTPClient : TIdFTP;
begin
  FTPClient := CreateConnection;
  try
    FTPClient.Connect;
    if FTPClient.Connected then
    begin
      FTPClient.TransferType := ftBinary;
      FTPClient.Passive:= true;
      FTPClient.ChangeDir(PATH_DELIMITER);
      FTPClient.ChangeDir(aFileFolder);
      if FTPClient.Size(aFileName) > 0 then
        FTPClient.Delete(aFileName);
      FTPClient.Put(aStream, aFileName);
    end;
    FTPClient.Disconnect;
  finally
    FTPClient.Free;
  end;
end;

function TFTPFileSystemManager.ValidateFileName(const aFileName: string): string;
begin
  Result := trim(aFileName);
  if Result <> '' then
    Result := ChangeFileExt(Result, ExtractFileExt(FFileMask));
end;

procedure TFTPFileSystemManager.CreatePath(const aPath: string);
var
  FTPClient : TIdFTP;
  list : TStringList;
  i : integer;
  curFolder : String;
begin
  FTPClient := CreateConnection;
  try
    FTPClient.Connect;
    if FTPClient.Connected then
    begin
      FTPClient.TransferType := ftBinary;
      FTPClient.Passive:= true;

      list := TStringList.Create;
      try
        list.StrictDelimiter:= true;
        list.Delimiter:= PATH_DELIMITER;
        list.DelimitedText:= aPath;
        curFolder := '';
        for i := 0 to list.Count -1 do
        begin
          if list.Strings[i] <> '' then
          begin
            if curFolder <> '' then
              curFolder := IncludeTrailingPathFTPDelimiter(curFolder);
            curFolder := IncludeTrailingPathFTPDelimiter(curFolder + list.Strings[i]);
            InternalCreateSubFolders(FTPClient, curFolder);
          end;
        end;
      finally
        list.Free;
      end;
    end;
    FTPClient.Disconnect;
  finally
    FTPClient.Free;
  end;
end;

function TFTPFileSystemManager.IncludeTrailingPathFSDelimiter(const aPath: String): String;
begin
  Result := IncludeTrailingPathFTPDelimiter(aPath);
end;

procedure TFTPFileSystemManager.GetAllFiles(aRoots: TmFolders);
begin
  //
end;

procedure TFTPFileSystemManager.DeleteFile(const aFileName, aFileFolder: String);
var
  FTPClient : TIdFTP;
begin
  FTPClient := CreateConnection;
  try
    FTPClient.Connect;
    if FTPClient.Connected then
    begin
      FTPClient.TransferType := ftBinary;
      FTPClient.Passive:= true;
      FTPClient.ChangeDir(PATH_DELIMITER);
      FTPClient.ChangeDir(aFileFolder);
      FTPClient.Delete(aFileName);
    end;
    FTPClient.Disconnect;
  finally
    FTPClient.Free;
  end;
end;

function TFTPFileSystemManager.FileExists(const aFileName, aFileFolder: String): boolean;
var
  FTPClient : TIdFTP;
  l : TStringList;
begin
  Result := false;
  l := TStringList.Create;
  FTPClient := CreateConnection;
  try
    FTPClient.Connect;
    if FTPClient.Connected then
    begin
      FTPClient.TransferType := ftBinary;
      FTPClient.Passive:= true;
      FTPClient.List(l, IncludeTrailingPathFTPDelimiter(aFileFolder) + aFileName, false);
      Result := l.Count > 0;
    end;
    FTPClient.Disconnect;
  finally
    FTPClient.Free;
    l.Free;
  end;
end;

function IncludeTrailingPathFTPDelimiter(const aPath: String): String;
begin
  Result := aPath;
  if aPath <> '' then
  begin
    if aPath[Length(aPath)] <> TFTPFileSystemManager.PATH_DELIMITER then
      Result := aPath + TFTPFileSystemManager.PATH_DELIMITER;
  end;
end;

end.
