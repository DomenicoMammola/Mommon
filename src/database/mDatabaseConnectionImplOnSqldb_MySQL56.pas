// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnectionImplOnSqldb_MySQL56;

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
