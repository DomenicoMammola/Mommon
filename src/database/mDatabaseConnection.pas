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

{$IFDEF FPC}
{$MODE DELPHI}
// {$MODE DELPHIUNICODE}
{$ENDIF}

interface

uses
  Classes, DB,
  mDatabaseConnectionClasses, mDatabaseConnectionImpl, mDataManagerClasses;

type
  { TmDatabaseConnection }

  TmDatabaseConnection = class (TmDataManagerTransaction)
  private
    FConnectionInfo : TmDatabaseConnectionInfo;
    FOwnsConnectionInfo : boolean;

    FImplementation : TmDatabaseConnectionImpl;
    procedure CreateImplementation;
  public
    constructor Create; overload;
    constructor Create(aConnectionInfo : TmDatabaseConnectionInfo; const aOwnsConnectionInfo: boolean = false); overload;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Close; override;

    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    property ConnectionInfo : TmDatabaseConnectionInfo read FConnectionInfo write FConnectionInfo;
    property OwnsConnectionInfo : boolean read FOwnsConnectionInfo write FOwnsConnectionInfo;
  end;

  { TmDatabaseConnectionsTandem }

  TmDatabaseConnectionsTandem = class
  strict private
    FOwnedConnection : TmDatabaseConnection;
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
  end;

  { TmDatabaseCommand }

  TmDatabaseCommand = class (TmAbstractDatabaseCommand)
  protected
    procedure CreateImplementation; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Execute : integer;
  end;

function GetLastSQLScript (var aTitle: string) : string;

implementation

uses
  SysUtils, syncobjs,
  mDatabaseConnectionImplRegister, mExceptionLog;

var
  _LastSQLScript: string;
  _LastSQLScriptCriticalSection: TCriticalSection;

procedure TraceSQL (const aSQLScript: string);
begin
  _LastSQLScriptCriticalSection.Acquire;
  try
    _LastSQLScript:= aSQLScript;
  finally
    _LastSQLScriptCriticalSection.Leave;
  end;
end;

function GetLastSQLScript(var aTitle: string): string;
begin
  aTitle := 'LAST SQL SCRIPT';
  Result := _LastSQLScript;
end;


{ TmDatabaseConnectionsTandem }

constructor TmDatabaseConnectionsTandem.Create(const aConnectionInfo: TmDatabaseConnectionInfo; aSharedTransaction : TmDataManagerTransaction);
begin
  FOwnedConnection := nil;
  FSharedConnection := nil;

  FConnectionInfo := aConnectionInfo;
  if Assigned(aSharedTransaction) then
  begin
    if  not (aSharedTransaction is TmDatabaseConnection) then
      raise Exception.Create('Shared transaction is not a TmDatabaseConnection. Don''t know how to handle it.');

    Self.SetSharedConnection(aSharedTransaction as TmDatabaseConnection);
  end;
end;

procedure TmDatabaseConnectionsTandem.SetSharedConnection(aConnection: TmDatabaseConnection);
begin
  if Assigned(aConnection) then
  begin
    if Assigned(FOwnedConnection) then
      raise Exception.Create('Owned connection already created!');
    FSharedConnection := aConnection;
  end;
end;

procedure TmDatabaseConnectionsTandem.CreateOwnedConnectionIfNeeded;
begin
  if not Assigned(FOwnedConnection) then
  begin
    FOwnedConnection := TmDatabaseConnection.Create;
    if Assigned(FConnectionInfo) then
      FOwnedConnection.ConnectionInfo := FConnectionInfo;
  end;
end;

destructor TmDatabaseConnectionsTandem.Destroy;
begin
  FreeAndNil(FOwnedConnection);
  inherited Destroy;
end;

procedure TmDatabaseConnectionsTandem.ConnectIfOwned;
begin
  if not Assigned(FSharedConnection) then
  begin
    CreateOwnedConnectionIfNeeded;
    FOwnedConnection.Connect;
  end;
end;

procedure TmDatabaseConnectionsTandem.CloseIfOwned;
begin
  if not Assigned(FSharedConnection) then
  begin
    CreateOwnedConnectionIfNeeded;
    FOwnedConnection.Close;
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

procedure TmDatabaseCommand.CreateImplementation;
begin
  if not Assigned(FImplementation) then
  begin
    if not Assigned(FDatabaseConnection) then
      raise Exception.Create('No database connection was associated to the TmDatabaseCommand');
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

procedure TmDatabaseQuery.CreateImplementation;
begin
  if not Assigned(FImplementation) then
  begin
    FImplementation := GetDataConnectionClassesRegister.GetQueryImpl(FDatabaseConnection.FImplementation.GetName);
    FImplementation.DatabaseConnectionImpl := FDatabaseConnection.FImplementation;
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
  if Assigned(FConnectionInfo) then
  begin
    if not Assigned(FImplementation) then
    begin
      FImplementation := GetDataConnectionClassesRegister.GetConnectionImpl(FConnectionInfo.VendorType);
      if not Assigned(FImplementation) then
        raise TmDataConnectionException.Create('No connection implementation available for ' + DatabaseVendorToString(FConnectionInfo.VendorType) + '.');
      FImplementation.ConnectionInfo := FConnectionInfo;
    end;
  end
  else
  begin
    raise TmDataConnectionException.Create('Connection info is unavailable.');
  end;
end;

constructor TmDatabaseConnection.Create;
begin
  FImplementation := nil;
  FOwnsConnectionInfo:= false;
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

initialization
  _LastSQLScript := '';
  _LastSQLScriptCriticalSection := TCriticalSection.Create;
  RegisterExceptionLogTracer(GetLastSQLScript);

finalization
  _LastSQLScriptCriticalSection.Free;

end.
