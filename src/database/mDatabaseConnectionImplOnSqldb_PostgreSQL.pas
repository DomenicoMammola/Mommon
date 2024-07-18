unit mDatabaseConnectionImplOnSqldb_PostgreSQL;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  sqldb, Classes,
  mDatabaseConnectionClasses, mDatabaseConnectionImpl,
  mDatabaseConnectionImplOnSqldb;

type

  { TPostgreSQLDatabaseConnectionImpl }

  TPostgreSQLDatabaseConnectionImpl  = class (TAbstractSqldbDatabaseConnectionImpl)
  protected
    procedure SetConnectionInfo(AValue: TmDatabaseConnectionInfo); override;
  public
    constructor Create; override;
    function GetName : String; override;

    class function GetImplementationName : String;
  end;

  TPostgreSQLDatabaseQueryImpl = class(TAbstractSqldbDatabaseQueryImpl);

  TPostgreSQLDatabaseCommandImpl = class (TAbstractSqldbDatabaseCommandImpl);

implementation

uses
  pqconnection,
  mDatabaseConnectionImplRegister;

{ TPostgreSQLDatabaseConnectionImpl }

procedure TPostgreSQLDatabaseConnectionImpl.SetConnectionInfo(AValue: TmDatabaseConnectionInfo);
begin
  inherited SetConnectionInfo(AValue);
end;

constructor TPostgreSQLDatabaseConnectionImpl.Create;
begin
  inherited Create;
  FConnection := TPQConnection.Create(nil);
  FConnection.CharSet:='UTF8';
  FConnection.Transaction := FTransaction;
end;

function TPostgreSQLDatabaseConnectionImpl.GetName: String;
begin
  Result := TPostgreSQLDatabaseConnectionImpl.GetImplementationName;
end;

class function TPostgreSQLDatabaseConnectionImpl.GetImplementationName: String;
begin
  Result := 'postgresqlconn-sqldb';
end;

initialization

  GetDataConnectionClassesRegister.RegisterImplementations(TPostgreSQLDatabaseConnectionImpl.GetImplementationName, dvPostgresql, '*', TPostgreSQLDatabaseConnectionImpl, TPostgreSQLDatabaseQueryImpl, TPostgreSQLDatabaseCommandImpl);

end.
