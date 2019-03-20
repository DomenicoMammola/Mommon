// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// To enable database access throw the sqldb components to MsSqlServer
// please check:
// http://wiki.freepascal.org/mssqlconn
unit mDatabaseConnectionImplOnMsSql;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, DB,

  sqldb,

  mDatabaseConnectionClasses,
  mDatabaseConnectionImpl, mDatabaseConnectionImplOnSqldb;

type
  TMsSqlDatabaseConnectionImpl = class(TAbstractSqldbDatabaseConnectionImpl)
  protected
    procedure SetConnectionInfo(AValue: TmDatabaseConnectionInfo); override;
  public
    function GetName : String; override;

    class function GetImplementationName : String;
  end;

  TMSSqlDatabaseQueryImpl = class(TAbstractSqldbDatabaseQueryImpl);

  TMSSqlDatabaseCommandImpl = class (TAbstractSqldbDatabaseCommandImpl);

implementation

uses
  sysutils,
  mssqlconn, dblib,
  mDatabaseConnectionImplRegister;

procedure TMsSqlDatabaseConnectionImpl.SetConnectionInfo(AValue: TmDatabaseConnectionInfo);
begin
  inherited SetConnectionInfo(AValue);
  if Assigned(FConnection) then
    FreeAndNil(FConnection);
  if (FConnectionInfo.VendorType = dvSQLServer) then
  begin
    FConnection := TMSSQLConnection.Create(nil);
    FTransaction.DataBase := FConnection;
    {$IFDEF WINDOWS}
    if Pos('v2008', FConnectionInfo.ExtraSettings) > 0 then
      DBLibLibraryName := 'dblib_2008.dll'
    else
      DBLibLibraryName := DBLIBDLL;
    {$ENDIF}
  end;
end;

function TMsSqlDatabaseConnectionImpl.GetName: String;
begin
  Result := TMsSqlDatabaseConnectionImpl.GetImplementationName;
end;

class function TMsSqlDatabaseConnectionImpl.GetImplementationName: String;
begin
  Result := 'sqldb-mssql';
end;



initialization

  GetDataConnectionClassesRegister.RegisterImplementations(TMsSqlDatabaseConnectionImpl.GetImplementationName, dvSQLServer, TMsSqlDatabaseConnectionImpl, TMsSqlDatabaseQueryImpl, TMsSqlDatabaseCommandImpl);

end.
