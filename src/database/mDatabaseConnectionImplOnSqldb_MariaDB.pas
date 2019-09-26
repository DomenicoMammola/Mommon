// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnectionImplOnSqldb_MariaDB;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  sqldb, Classes,
  mDatabaseConnectionClasses, mDatabaseConnectionImpl,
  mDatabaseConnectionImplOnSqldb;

type

  { TMariaDBDatabaseConnectionImpl }

  TMariaDBDatabaseConnectionImpl  = class (TAbstractSqldbDatabaseConnectionImpl)
  protected
    procedure SetConnectionInfo(AValue: TmDatabaseConnectionInfo); override;
  public
    constructor Create; override;
    function GetName : String; override;

    class function GetImplementationName : String;
  end;

  TMariaDBDatabaseQueryImpl = class(TAbstractSqldbDatabaseQueryImpl);

  TMariaDBDatabaseCommandImpl = class (TAbstractSqldbDatabaseCommandImpl);

implementation

uses
  mysql56conn, mysql56dyn,
  mDatabaseConnectionImplRegister;

{ TMariaDBDatabaseConnectionImpl }

procedure TMariaDBDatabaseConnectionImpl.SetConnectionInfo(AValue: TmDatabaseConnectionInfo);
begin
  inherited SetConnectionInfo(AValue);
end;

constructor TMariaDBDatabaseConnectionImpl.Create;
begin
  inherited Create;
  FConnection := TMySQL56Connection.Create(nil);
  FConnection.CharSet:='UTF8';
  FConnection.Transaction := FTransaction;
end;


function TMariaDBDatabaseConnectionImpl.GetName: String;
begin
  Result := TMariaDBDatabaseConnectionImpl.GetImplementationName;
end;


class function TMariaDBDatabaseConnectionImpl.GetImplementationName: String;
begin
  Result := 'mariadbconn-sqldb';
end;


initialization

  GetDataConnectionClassesRegister.RegisterImplementations(TMariaDBDatabaseConnectionImpl.GetImplementationName, dvMariaDB, '10.4', TMariaDBDatabaseConnectionImpl, TMariaDBDatabaseQueryImpl, TMariaDBDatabaseCommandImpl);

end.
