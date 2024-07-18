// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnectionImplRegister;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Contnrs,
  mDatabaseConnectionImpl, mDatabaseConnectionClasses;

type
  { TmDatabaseConnectionImplRegister }

  TmDatabaseConnectionImplRegister = class
  strict private
    FImplementationsList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterImplementations (const aName : String; const aVendorType : TmDatabaseVendor; const aDatabaseVersion : string; const aConnectionImplementationClass : TmDatabaseConnectionImplClass;
      const aQueryImplementationClass : TmDatabaseQueryImplClass; const aCommandImplementationClass : TmDatabaseCommandImplClass);


    function GetConnectionImpl (const aVendorType: TmDatabaseVendor; const aDatabaseVersion : String) : TmDatabaseConnectionImpl;
    function GetQueryImpl(const AName : String): TmDatabaseQueryImpl;
    function GetCommandImpl(const AName : String) : TmDatabaseCommandImpl;
  end;

  function GetDataConnectionClassesRegister : TmDatabaseConnectionImplRegister;

implementation

uses
  SysUtils;

type
  TImplementationClassesShell = class
  public
    Name : String;
    VendorType : TmDatabaseVendor;
    DatabaseVersion : String;
    ConnectionClass : TmDatabaseConnectionImplClass;
    QueryClass : TmDatabaseQueryImplClass;
    CommandClass : TmDatabaseCommandImplClass;
  end;

var
  _Register : TmDatabaseConnectionImplRegister;

function GetDataConnectionClassesRegister : TmDatabaseConnectionImplRegister;
begin
  if _Register = nil then
    _Register := TmDatabaseConnectionImplRegister.Create;
  Result := _Register;
end;

{ TmDatabaseConnectionImplRegister }

constructor TmDatabaseConnectionImplRegister.Create;
begin
  FImplementationsList := TObjectList.Create(true);
end;

destructor TmDatabaseConnectionImplRegister.Destroy;
begin
  FImplementationsList.Free;
  inherited Destroy;
end;

procedure TmDatabaseConnectionImplRegister.RegisterImplementations (const aName : String; const aVendorType : TmDatabaseVendor; const aDatabaseVersion : String; const aConnectionImplementationClass : TmDatabaseConnectionImplClass;
    const aQueryImplementationClass : TmDatabaseQueryImplClass; const aCommandImplementationClass : TmDatabaseCommandImplClass);
var
  temp : TImplementationClassesShell;
begin
  temp := TImplementationClassesShell.Create;
  temp.Name := aName;
  temp.VendorType:= aVendorType;
  temp.DatabaseVersion:= aDatabaseVersion;
  temp.ConnectionClass:= aConnectionImplementationClass;
  temp.QueryClass:= aQueryImplementationClass;
  temp.CommandClass:= aCommandImplementationClass;
  FImplementationsList.Add(temp);
end;


function TmDatabaseConnectionImplRegister.GetConnectionImpl(const aVendorType: TmDatabaseVendor; const aDatabaseVersion : String): TmDatabaseConnectionImpl;
var
  i : integer;
  TempShell : TImplementationClassesShell;
begin
  for i := 0 to FImplementationsList.Count -1 do
  begin
    TempShell := FImplementationsList[i] as TImplementationClassesShell;
    if (TempShell.VendorType = aVendorType) and ((TempShell.DatabaseVersion = '') or (TempShell.DatabaseVersion='*') or (CompareText(TempShell.DatabaseVersion, aDatabaseVersion) = 0)) then
    begin
      Result := TempShell.ConnectionClass.Create;
      exit;
    end;
  end;
  Result := nil;
end;

function TmDatabaseConnectionImplRegister.GetQueryImpl(const AName: String): TmDatabaseQueryImpl;
var
  i : integer;
  TempShell : TImplementationClassesShell;
begin
  for i := 0 to FImplementationsList.Count -1 do
  begin
    TempShell := FImplementationsList[i] as TImplementationClassesShell;
    if CompareText(TempShell.Name, AName) = 0 then
    begin
      Result := TempShell.QueryClass.Create;
      exit;
    end;
  end;
  Result := nil;
end;

function TmDatabaseConnectionImplRegister.GetCommandImpl(const AName: String): TmDatabaseCommandImpl;
var
  i : integer;
  TempShell : TImplementationClassesShell;
begin
  for i := 0 to FImplementationsList.Count -1 do
  begin
    TempShell := FImplementationsList[i] as TImplementationClassesShell;
    if CompareText(TempShell.Name, AName) = 0 then
    begin
      Result := TempShell.CommandClass.Create;
      exit;
    end;
  end;
  Result := nil;
end;

initialization
  _Register := nil;

finalization
  FreeAndNil(_Register);

end.
