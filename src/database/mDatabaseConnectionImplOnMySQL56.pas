unit mDatabaseConnectionImplOnMySQL56;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  sqldb, Classes,
  mDatabaseConnectionClasses, mDatabaseConnectionImpl,
  mDatabaseConnectionImplOnSqldb;

type

  { TMySQL56DatabaseConnectionImpl }

  TMySQL56DatabaseConnectionImpl  = class (TAbstractSqldbDatabaseConnectionImpl)
  protected
    procedure SetConnectionInfo(AValue: TmDatabaseConnectionInfo); override;
  public
    constructor Create; override;
    function GetName : String; override;

    class function GetImplementationName : String;
  end;

  TMySQL56DatabaseQueryImpl = class(TAbstractSqldbDatabaseQueryImpl);

  TMySQL56DatabaseCommandImpl = class (TAbstractSqldbDatabaseCommandImpl);

implementation

uses
  mysql56conn, mysql56dyn,
  mDatabaseConnectionImplRegister;

{ TMySQL56DatabaseConnectionImpl }

procedure TMySQL56DatabaseConnectionImpl.SetConnectionInfo(AValue: TmDatabaseConnectionInfo);
begin
  inherited SetConnectionInfo(AValue);
end;

constructor TMySQL56DatabaseConnectionImpl.Create;
begin
  inherited Create;
  FConnection := TMySQL56Connection.Create(nil);
  FConnection.Transaction := FTransaction;
end;


function TMySQL56DatabaseConnectionImpl.GetName: String;
begin
  Result := TMySQL56DatabaseConnectionImpl.GetImplementationName;
end;


class function TMySQL56DatabaseConnectionImpl.GetImplementationName: String;
begin
  Result := 'mysql56conn-sqldb';
end;


initialization

  GetDataConnectionClassesRegister.RegisterImplementations(TMySQL56DatabaseConnectionImpl.GetImplementationName, dvMySQL56, TMySQL56DatabaseConnectionImpl, TMySQL56DatabaseQueryImpl, TMySQL56DatabaseCommandImpl);

end.
