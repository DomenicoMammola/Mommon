// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnection;

// {$DEFINE CACHE_CONNECTIONS}

{$IFDEF FPC}
{$MODE DELPHI}
// {$MODE DELPHIUNICODE}
{$ENDIF}

interface

uses
  Classes, DB, sysutils, contnrs, syncobjs,
  mDatabaseConnectionClasses, mDatabaseConnectionImpl, mDataManagerClasses, mMaps;

type
  { TmDatabaseConnection }

  TmDatabaseConnection = class (TmDataManagerTransaction)
  private
    FConnectionInfo : TmDatabaseConnectionInfo;
    FOwnsConnectionInfo : boolean;
    FLocked : boolean;
    FCached : boolean;
    FAncestor : TmDatabaseConnection;

    FImplementation : TmDatabaseConnectionImpl;
    procedure CreateImplementation;
    function FindAncestorConnection (const aConnectionInfo: TmDatabaseConnectionInfo): TmDatabaseConnection;
  public
    constructor Create; overload;
    constructor Create(aConnectionInfo : TmDatabaseConnectionInfo; const aOwnsConnectionInfo: boolean = false); overload;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Close; override;

    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    function Connected: boolean; override;

    property ConnectionInfo : TmDatabaseConnectionInfo read FConnectionInfo write FConnectionInfo;
    property OwnsConnectionInfo : boolean read FOwnsConnectionInfo write FOwnsConnectionInfo;
  end;

  { TmDatabaseConnectionsTandem }

  TmDatabaseConnectionsTandem = class
  strict private
    FOwnedConnection : TmDatabaseConnection;
    FAncestorConnection : TmDatabaseConnection;
    FSharedConnection : TmDatabaseConnection;
    FConnectionInfo : TmDatabaseConnectionInfo;
    procedure SetSharedConnection (aConnection : TmDatabaseConnection);
    procedure CreateOwnedConnectionIfNeeded;
  public
    constructor Create(const aConnectionInfo : TmDatabaseConnectionInfo; aSharedTransaction : TmDataManagerTransaction);

    destructor Destroy; override;

    procedure ConnectIfOwned;
    procedure CloseIfOwned;

    procedure StartTransactionIfOwned;
    procedure CommitIfOwned;
    procedure RollbackIfOwned;

    function Connection : TmDatabaseConnection;
  end;

  { TmAbstractDatabaseCommand }

  TmAbstractDatabaseCommand = class abstract
  protected
    FImplementation : TmAbstractDatabaseCommandImpl;
    FDatabaseConnection : TmDatabaseConnection;
    FSQL : TStringList;
    FParameters : TmQueryParameters;
    procedure CreateImplementation; virtual; abstract;
    function PrepareIfNecessary : Boolean;
    procedure ReloadParameters;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Prepare;
    procedure Unprepare;
    function Prepared : boolean;
    function ParamByName(const aValue: string): TmQueryParameter;
    procedure SetSQL(const aValue : string);
    procedure SaveSQLToFile(const aFileName : String);

    property DatabaseConnection : TmDatabaseConnection read FDatabaseConnection write FDatabaseConnection;
    // property SQL : TStringList read FSQL;
  end;


  { TmDatabaseQuery }

  TmDatabaseQuery = class (TmAbstractDatabaseCommand)
  strict private
    function GetParamCheck: boolean;
    function GetUnidirectional: boolean;
    procedure SetParamCheck(const AValue: boolean);
    procedure SetUnidirectional(const AValue: boolean);
  protected
    procedure CreateImplementation; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    procedure First;
    procedure Next;
    function Eof : boolean;
    function AsDataset : TDataset;
    property Unidirectional : boolean read GetUnidirectional write SetUnidirectional;
    property ParamCheck : boolean read GetParamCheck write SetParamCheck;
  end;

  { TmDatabaseCommand }

  TmDatabaseCommand = class (TmAbstractDatabaseCommand)
  private
    function GetParamCheck: boolean;
    procedure SetParamCheck(const AValue: boolean);
  protected
    procedure CreateImplementation; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Execute : integer;
    property ParamCheck : boolean read GetParamCheck write SetParamCheck;
  end;

  { TmDatabaseConnectionsCache }

  TmDatabaseConnectionsCache = class
  strict private
    FIndex : TmStringDictionary;
    FList : TList;
    FCriticalSection : TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CloseAll;

    function GetConnection(const aConnectionInfo : TmDatabaseConnectionInfo): TmDatabaseConnection;
    procedure ReleaseConnection (aConnection : TmDatabaseConnection);
  end;

function GetLastSQLScript (out aTitle: string) : string;

implementation

uses
  mDatabaseConnectionImplRegister, mExceptionLog;

var
  _LastSQLScript: string;
  _LastSQLScriptCriticalSection: TCriticalSection;
  _CreateImplementationCriticalSection: TCriticalSection;
  _ConnectionsCache : TmDatabaseConnectionsCache;

procedure TraceSQL (const aSQLScript: string);
begin
  _LastSQLScriptCriticalSection.Acquire;
  try
    _LastSQLScript:= aSQLScript;
  finally
    _LastSQLScriptCriticalSection.Leave;
  end;
end;

function GetLastSQLScript(out aTitle: string): string;
begin
  aTitle := 'LAST SQL SCRIPT';
  Result := _LastSQLScript;
end;

{ TmDatabaseConnectionsCache }

constructor TmDatabaseConnectionsCache.Create;
begin
  FIndex := TmStringDictionary.Create(true);
  FList := TList.Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TmDatabaseConnectionsCache.Destroy;
begin
  Self.CloseAll;
  FIndex.Free;
  FList.Free;
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TmDatabaseConnectionsCache.CloseAll;
var
  i : integer;
  curConn : TmDatabaseConnection;
begin
  FCriticalSection.Acquire;
  try
    for i := 0 to FList.Count -  1 do
    begin
      curConn := TmDatabaseConnection(FList.Items[i]);
      if curConn.Connected then
        curConn.Close;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

function TmDatabaseConnectionsCache.GetConnection(const aConnectionInfo: TmDatabaseConnectionInfo): TmDatabaseConnection;
var
  curList : TObjectList;
  curConn : TmDatabaseConnection;
  i : integer;
begin
  FCriticalSection.Acquire;
  try
    curList := FIndex.Find(aConnectionInfo.AsString) as TObjectList;
    if not Assigned(curList) then
    begin
      curList := TObjectList.Create(true);
      FIndex.Add(aConnectionInfo.AsString, curList);
    end;
    curConn := nil;
    for i := 0 to curList.Count - 1 do
    begin
      if not (curList.Items[i] as TmDatabaseConnection).FLocked then
      begin
        curConn := curList.Items[i] as TmDatabaseConnection;
        // it must be replaced as the old instance could be freed
        curConn.ConnectionInfo := aConnectionInfo;
        break;
      end;
    end;
    if not Assigned(curConn) then
    begin
      curConn := TmDatabaseConnection.Create(aConnectionInfo);
      curConn.FCached:= true;
      curList.Add(curConn);
    end;

    curConn.FLocked:= true;

    Result := curConn;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TmDatabaseConnectionsCache.ReleaseConnection(aConnection: TmDatabaseConnection);
begin
  if Assigned(aConnection) then
    aConnection.FLocked:= false;
end;


{ TmDatabaseConnectionsTandem }

constructor TmDatabaseConnectionsTandem.Create(const aConnectionInfo: TmDatabaseConnectionInfo; aSharedTransaction : TmDataManagerTransaction);
var
  ancestorConnection : TmDatabaseConnection;
begin
  FOwnedConnection := nil;
  FSharedConnection := nil;
  FAncestorConnection := nil;

  FConnectionInfo := aConnectionInfo;
  if Assigned(aSharedTransaction) then
  begin
    if  not (aSharedTransaction is TmDatabaseConnection) then
      raise TmDataConnectionException.Create('Shared transaction is not a TmDatabaseConnection. Don''t know how to handle it.');

    if (aSharedTransaction as TmDatabaseConnection).ConnectionInfo.IsEqual(FConnectionInfo) then
      Self.SetSharedConnection(aSharedTransaction as TmDatabaseConnection)
    else
    begin
      ancestorConnection := (aSharedTransaction as TmDatabaseConnection).FindAncestorConnection(FConnectionInfo);
      if Assigned(ancestorConnection) then
        Self.SetSharedConnection(ancestorConnection)
      else
        Self.FAncestorConnection := aSharedTransaction as TmDatabaseConnection;
    end;
  end;
end;

procedure TmDatabaseConnectionsTandem.SetSharedConnection(aConnection: TmDatabaseConnection);
begin
  if Assigned(aConnection) then
  begin
    if Assigned(FOwnedConnection) then
      raise TmDataConnectionException.Create('Owned connection already created!');
    FSharedConnection := aConnection;
  end;
end;

procedure TmDatabaseConnectionsTandem.CreateOwnedConnectionIfNeeded;
begin
  if not Assigned(FOwnedConnection) then
  begin
    {$IFDEF CACHE_CONNECTIONS}
      if Assigned(FConnectionInfo) then
        FOwnedConnection := _ConnectionsCache.GetConnection(FConnectionInfo)
      else
        FOwnedConnection := TmDatabaseConnection.Create;
    {$ELSE}
      FOwnedConnection := TmDatabaseConnection.Create;
      FOwnedConnection.FAncestor := Self.FAncestorConnection;
      if Assigned(FConnectionInfo) then
        FOwnedConnection.ConnectionInfo := FConnectionInfo;
    {$ENDIF}
  end;
end;

destructor TmDatabaseConnectionsTandem.Destroy;
begin
  {$IFDEF CACHE_CONNECTIONS}
  if Assigned(FOwnedConnection) then
    _ConnectionsCache.ReleaseConnection(FOwnedConnection);
  {$ELSE}
  FreeAndNil(FOwnedConnection);
  {$ENDIF}
  inherited Destroy;
end;

procedure TmDatabaseConnectionsTandem.ConnectIfOwned;
begin
  if not Assigned(FSharedConnection) then
  begin
    CreateOwnedConnectionIfNeeded;
    {$IFDEF CACHE_CONNECTIONS}
    if not FOwnedConnection.Connected then
      FOwnedConnection.Connect;
    {$ELSE}
    FOwnedConnection.Connect;
    {$ENDIF}
  end;
end;

procedure TmDatabaseConnectionsTandem.CloseIfOwned;
begin
  if not Assigned(FSharedConnection) then
  begin
    CreateOwnedConnectionIfNeeded;
    {$IFDEF CACHE_CONNECTIONS}
    //
    {$ELSE}
    FOwnedConnection.Close;
    {$ENDIF}
  end;
end;

procedure TmDatabaseConnectionsTandem.StartTransactionIfOwned;
begin
  if not Assigned(FSharedConnection) then
  begin
    CreateOwnedConnectionIfNeeded;
    FOwnedConnection.StartTransaction;
  end;
end;

procedure TmDatabaseConnectionsTandem.CommitIfOwned;
begin
  if not Assigned(FSharedConnection) then
  begin
    CreateOwnedConnectionIfNeeded;
    FOwnedConnection.Commit;
  end;
end;

procedure TmDatabaseConnectionsTandem.RollbackIfOwned;
begin
  if not Assigned(FSharedConnection) then
  begin
    CreateOwnedConnectionIfNeeded;
    FOwnedConnection.Rollback;
  end;
end;

function TmDatabaseConnectionsTandem.Connection: TmDatabaseConnection;
begin
  Result := nil;
  if Assigned(FSharedConnection) then
    Result := FSharedConnection
  else
  begin
    CreateOwnedConnectionIfNeeded;
    Result := FOwnedConnection;
  end;
end;

{ TmAbstractDatabaseCommand }

function TmAbstractDatabaseCommand.PrepareIfNecessary: Boolean;
begin
  Result := false;
  if (not FImplementation.Prepared) then
  begin
    Self.Prepare;
    Result := true;
  end
  else
  begin
    if (not FImplementation.SameSQL(FSQL)) then
    begin
      Self.Unprepare;
      Self.Prepare;
      Result := true;
    end;
  end;
end;

procedure TmAbstractDatabaseCommand.ReloadParameters;
var
  i : integer;
begin
  FParameters.Clear;
  for i := 0 to FImplementation.ParamCount - 1 do
  begin
    FParameters.Add(FImplementation.GetParam(i));
  end;
end;

constructor TmAbstractDatabaseCommand.Create;
begin
  FSQL := TStringList.Create;
  FParameters := TmQueryParameters.Create;
end;

destructor TmAbstractDatabaseCommand.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParameters);
  inherited Destroy;
end;

procedure TmAbstractDatabaseCommand.Prepare;
begin
  CreateImplementation;
  if FImplementation.Prepared then
    FImplementation.Unprepare;
  FImplementation.SetSQL(FSQL);
  FImplementation.Prepare;
  ReloadParameters;
end;

procedure TmAbstractDatabaseCommand.Unprepare;
begin
  CreateImplementation;
  FImplementation.Unprepare;
  FParameters.Clear;
end;

function TmAbstractDatabaseCommand.Prepared: boolean;
begin
  CreateImplementation;
  Result := FImplementation.Prepared;
end;

function TmAbstractDatabaseCommand.ParamByName(const aValue: string): TmQueryParameter;
begin
  TraceSQL(FSQL.Text);
  CreateImplementation;
  if PrepareIfNecessary then
  begin
    // must reload FParameters
    ReloadParameters;
  end;
  Result := FParameters.FindByName(aValue);
end;

procedure TmAbstractDatabaseCommand.SetSQL(const aValue: string);
begin
  Self.FSQL.Clear;
  Self.FSQL.Append(aValue);
end;

procedure TmAbstractDatabaseCommand.SaveSQLToFile(const aFileName: String);
begin
  FSQL.SaveToFile(aFileName);
end;

{ TmDatabaseCommand }

function TmDatabaseCommand.GetParamCheck: boolean;
begin
  CreateImplementation;
  Result := FImplementation.GetParamCheck;
end;

procedure TmDatabaseCommand.SetParamCheck(const AValue: boolean);
begin
  CreateImplementation;
  FImplementation.SetParamCheck(AValue);
end;

procedure TmDatabaseCommand.CreateImplementation;
begin
  if not Assigned(FImplementation) then
  begin
    if not Assigned(FDatabaseConnection) then
      raise TmDataConnectionException.Create('No database connection was associated to the TmDatabaseCommand');
    FImplementation := GetDataConnectionClassesRegister.GetCommandImpl(FDatabaseConnection.FImplementation.GetName);
    FImplementation.DatabaseConnectionImpl := FDatabaseConnection.FImplementation;
  end;
end;

constructor TmDatabaseCommand.Create;
begin
  inherited;
  FImplementation := nil;
end;

destructor TmDatabaseCommand.Destroy;
begin
  if Assigned(FImplementation) then
    FreeAndNil(FImplementation);
  inherited Destroy;
end;

function TmDatabaseCommand.Execute: integer;
var
  i : integer;
begin
  TraceSQL(FSQL.Text);
  CreateImplementation;
  PrepareIfNecessary;
  for i := 0 to FParameters.Count -1 do
  begin
    FImplementation.SetParamValue(FParameters.GetParam(i));
  end;
  Result := (FImplementation as TmDatabaseCommandImpl).Execute;
end;


{ TmDatabaseQuery }

function TmDatabaseQuery.GetParamCheck: boolean;
begin
  CreateImplementation;
  Result := (FImplementation as TmDatabaseQueryImpl).GetParamCheck;
end;

function TmDatabaseQuery.GetUnidirectional: boolean;
begin
  CreateImplementation;
  Result := (FImplementation as TmDatabaseQueryImpl).GetUnidirectional;
end;

procedure TmDatabaseQuery.SetParamCheck(const AValue: boolean);
begin
  CreateImplementation;
  (FImplementation as TmDatabaseQueryImpl).SetParamCheck(aValue);
end;

procedure TmDatabaseQuery.SetUnidirectional(const AValue: boolean);
begin
  CreateImplementation;
  (FImplementation as TmDatabaseQueryImpl).SetUnidirectional(aValue);
end;

procedure TmDatabaseQuery.CreateImplementation;
begin
  if not Assigned(FImplementation)  then
  begin
    if Assigned(FDatabaseConnection) then
    begin
      FImplementation := GetDataConnectionClassesRegister.GetQueryImpl(FDatabaseConnection.FImplementation.GetName);
      FImplementation.DatabaseConnectionImpl := FDatabaseConnection.FImplementation;
    end
    else
      raise TmDataConnectionException.Create('Cannot create an implementation for TmDatabaseQuery if no connection is associated to the query');
  end;
end;

constructor TmDatabaseQuery.Create;
begin
  inherited;
  FImplementation := nil;
end;

destructor TmDatabaseQuery.Destroy;
begin
  if Assigned(FImplementation) then
    FreeAndNil(FImplementation);
  inherited Destroy;
end;

procedure TmDatabaseQuery.Open;
var
  i : integer;
begin
  TraceSQL(FSQL.Text);
  CreateImplementation;
  PrepareIfNecessary;
  for i := 0 to FParameters.Count -1 do
  begin
    FImplementation.SetParamValue(FParameters.GetParam(i));
  end;
  (FImplementation as TmDatabaseQueryImpl).Open;
end;

procedure TmDatabaseQuery.Close;
begin
  CreateImplementation;
  (FImplementation as TmDatabaseQueryImpl).Close;
end;

procedure TmDatabaseQuery.First;
begin
  CreateImplementation;
  (FImplementation as TmDatabaseQueryImpl).First;
end;

procedure TmDatabaseQuery.Next;
begin
  CreateImplementation;
  (FImplementation as TmDatabaseQueryImpl).Next;
end;

function TmDatabaseQuery.Eof: boolean;
begin
  CreateImplementation;
  Result := (FImplementation as TmDatabaseQueryImpl).Eof;
end;

function TmDatabaseQuery.AsDataset: TDataset;
begin
  CreateImplementation;
  Result := (FImplementation as TmDatabaseQueryImpl).AsDataset;
end;




{ TmDatabaseConnection }

procedure TmDatabaseConnection.CreateImplementation;
begin
  _CreateImplementationCriticalSection.Acquire;
  try
    if Assigned(FConnectionInfo) then
    begin
      if not Assigned(FImplementation) then
      begin
        FImplementation := GetDataConnectionClassesRegister.GetConnectionImpl(FConnectionInfo.VendorType, FConnectionInfo.DatabaseVersion);
        if not Assigned(FImplementation) then
          raise TmDataConnectionException.Create('No connection implementation available for ' + DatabaseVendorToString(FConnectionInfo.VendorType) + '.');
        FImplementation.ConnectionInfo := FConnectionInfo;
      end;
    end
    else
    begin
      raise TmDataConnectionException.Create('Connection info is unavailable.');
    end;
  finally
    _CreateImplementationCriticalSection.Leave;
  end;
end;

function TmDatabaseConnection.FindAncestorConnection(const aConnectionInfo: TmDatabaseConnectionInfo): TmDatabaseConnection;
var
  currentAncestor : TmDatabaseConnection;
begin
  Result := nil;
  currentAncestor := Self.FAncestor;
  while Assigned(currentAncestor) do
  begin
    if currentAncestor.ConnectionInfo.IsEqual(aConnectionInfo) then
    begin
      Result := currentAncestor;
      exit;
    end
    else
      currentAncestor := currentAncestor.FAncestor;
  end;
end;

constructor TmDatabaseConnection.Create;
begin
  FImplementation := nil;
  FOwnsConnectionInfo:= false;
  FCached:= false;
  FLocked:= false;
  FAncestor := nil;
end;

constructor TmDatabaseConnection.Create(aConnectionInfo: TmDatabaseConnectionInfo; const aOwnsConnectionInfo: boolean);
begin
  Self.Create;
  FConnectionInfo := aConnectionInfo;
  FOwnsConnectionInfo:= aOwnsConnectionInfo;
end;

destructor TmDatabaseConnection.Destroy;
begin
  if Assigned(FImplementation) then
    FreeAndNil(FImplementation);
  if FOwnsConnectionInfo then
    FreeAndNil(FConnectionInfo);
  inherited Destroy;
end;

procedure TmDatabaseConnection.Connect;
begin
  CreateImplementation;
  FImplementation.Connect;
end;

procedure TmDatabaseConnection.Close;
begin
  CreateImplementation;
  FImplementation.Close;
end;

procedure TmDatabaseConnection.StartTransaction;
begin
  CreateImplementation;
  FImplementation.StartTransaction;
end;

procedure TmDatabaseConnection.EndTransaction;
begin
  CreateImplementation;
  FImplementation.EndTransaction;
end;

procedure TmDatabaseConnection.Commit;
begin
  CreateImplementation;
  FImplementation.Commit;
end;

procedure TmDatabaseConnection.Rollback;
begin
  CreateImplementation;
  FImplementation.Rollback;
end;

function TmDatabaseConnection.Connected: boolean;
begin
  CreateImplementation;
  Result := FImplementation.Connected;
end;

initialization
  _LastSQLScript := '';
  _LastSQLScriptCriticalSection := TCriticalSection.Create;
  _CreateImplementationCriticalSection := TCriticalSection.Create;
  _ConnectionsCache := TmDatabaseConnectionsCache.Create;
  RegisterExceptionLogTracer(GetLastSQLScript);

finalization
  FreeAndNil(_LastSQLScriptCriticalSection);
  FreeAndNil(_CreateImplementationCriticalSection);
  FreeAndNil(_ConnectionsCache);

end.
