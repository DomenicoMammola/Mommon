// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnectionImplOnSqldb_MySQL57;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  sqldb, Classes,
  mDatabaseConnectionClasses, mDatabaseConnectionImpl,
  mDatabaseConnectionImplOnSqldb;

type

  { TMySQL57DatabaseConnectionImpl }

  TMySQL57DatabaseConnectionImpl  = class (TAbstractSqldbDatabaseConnectionImpl)
  protected
    procedure SetConnectionInfo(AValue: TmDatabaseConnectionInfo); override;
  public
    constructor Create; override;
    function GetName : String; override;

    class function GetImplementationName : String;
  end;

  TMySQL57DatabaseQueryImpl = class(TAbstractSqldbDatabaseQueryImpl);

  TMySQL57DatabaseCommandImpl = class (TAbstractSqldbDatabaseCommandImpl);

implementation

uses
  mysql57conn, mysql57dyn,
  mDatabaseConnectionImplRegister;

{ TMySQL57DatabaseConnectionImpl }

procedure TMySQL57DatabaseConnectionImpl.SetConnectionInfo(AValue: TmDatabaseConnectionInfo);
begin
  inherited SetConnectionInfo(AValue);
end;

constructor TMySQL57DatabaseConnectionImpl.Create;
begin
  inherited Create;
  FConnection := TMySQL57Connection.Create(nil);
  FConnection.CharSet:='UTF8';
  FConnection.Transaction := FTransaction;
  {$ifdef unix}
  TMySQL57Connection(FConnection).SkipLibraryVersionCheck:= true;
  {$endif}
end;


function TMySQL57DatabaseConnectionImpl.GetName: String;
begin
  Result := TMySQL57DatabaseConnectionImpl.GetImplementationName;
end;


class function TMySQL57DatabaseConnectionImpl.GetImplementationName: String;
begin
  Result := 'mysql57conn-sqldb';
end;


initialization

  GetDataConnectionClassesRegister.RegisterImplementations(TMySQL57DatabaseConnectionImpl.GetImplementationName, dvMySQL, '5.7', TMySQL57DatabaseConnectionImpl, TMySQL57DatabaseQueryImpl, TMySQL57DatabaseCommandImpl);

end.
