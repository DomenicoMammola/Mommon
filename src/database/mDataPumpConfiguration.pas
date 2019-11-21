// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDataPumpConfiguration;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs,
  mNullables, mMaps, mDatabaseConnectionClasses;

type

  { TDataReplicaFieldToField }

  TDataReplicaFieldToField = class
  strict private
    FSourceField : TNullableString;
    FDestinationField : TNullableString;
  public
    constructor Create;
    destructor Destroy; override;

    property SourceField : TNullableString read FSourceField;
    property DestinationField : TNullableString read FDestinationField;
  end;

  { TDataReplicaFields }

  TDataReplicaFields = class
  strict private
    FList : TObjectList;
    FIndexBySourceFieldName : TmStringDictionary;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RebuildIndexes;

    function Count : integer;
    function Get(const aIndex : integer) : TDataReplicaFieldToField;
    function Add: TDataReplicaFieldToField;
    function GetBySourceFieldName (const aSourceFieldName : String): TDataReplicaFieldToField;
  end;

  { TDataReplicaTableToTable }

  TDataReplicaTableToTable = class
  strict private
    FSourceSelectQuery : String;
    FDestinationSelectQuery : String;

    FDestinationTableName : String;
    FSourceKeyFields : TStringList;
    FFieldsMapping : TDataReplicaFields;
    FAllowInsert : boolean;
    FAllowUpdate : boolean;
    FAllowDelete : boolean;
    FPerformClearBefore : boolean;

    FSourceConnectionInfo: TmDatabaseConnectionInfo;
    FDestinationConnectionInfo : TmDatabaseConnectionInfo;
  public
    constructor Create;
    destructor Destroy; override;

    property SourceSelectQuery : String read FSourceSelectQuery write FSourceSelectQuery;
    property SourceKeyFields : TStringList read FSourceKeyFields;
    property DestinationSelectQuery : String read FDestinationSelectQuery write FDestinationSelectQuery;
    property FieldsMapping : TDataReplicaFields read FFieldsMapping;
    property DestinationTableName : String read FDestinationTableName write FDestinationTableName;

    property SourceConnectionInfo: TmDatabaseConnectionInfo read FSourceConnectionInfo;
    property DestinationConnectionInfo : TmDatabaseConnectionInfo read FDestinationConnectionInfo;
    property AllowInsert : boolean read FAllowInsert write FAllowInsert;
    property AllowUpdate : boolean read FAllowUpdate write FAllowUpdate;
    property AllowDelete : boolean read FAllowDelete write FAllowDelete;
    property PerformClearBefore : boolean read FPerformClearBefore write FPerformClearBefore;

  end;

  { TDataReplicaTables }

  TDataReplicaTables = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function Add : TDataReplicaTableToTable;
    function Get(const aIndex : integer): TDataReplicaTableToTable;
  end;

implementation
uses
  sysutils;

{ TDataReplicaTables }

constructor TDataReplicaTables.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TDataReplicaTables.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TDataReplicaTables.Count: integer;
begin
  Result := FList.Count;
end;

function TDataReplicaTables.Add: TDataReplicaTableToTable;
begin
  Result := TDataReplicaTableToTable.Create;
  FList.Add(Result);
end;

function TDataReplicaTables.Get(const aIndex: integer): TDataReplicaTableToTable;
begin
  Result := FList.Items[aIndex] as TDataReplicaTableToTable;
end;

{ TDataReplicaTableToTable }

constructor TDataReplicaTableToTable.Create;
begin
  FSourceSelectQuery := '';
  FSourceKeyFields := TStringList.Create;
  FDestinationSelectQuery := '';
  FFieldsMapping := TDataReplicaFields.Create;
  FDestinationTableName:= '';
  FSourceConnectionInfo:= TmDatabaseConnectionInfo.Create;
  FDestinationConnectionInfo:= TmDatabaseConnectionInfo.Create;
  FAllowInsert := true;
  FAllowUpdate := true;
  FAllowDelete := false;
  FPerformClearBefore:= false;
end;

destructor TDataReplicaTableToTable.Destroy;
begin
  FSourceKeyFields.Free;
  FFieldsMapping.Free;
  FSourceConnectionInfo.Free;
  FDestinationConnectionInfo.Free;
  inherited Destroy;
end;

{ TDataReplicaFields }

constructor TDataReplicaFields.Create;
begin
  FList := TObjectList.Create(true);
  FIndexBySourceFieldName := TmStringDictionary.Create(false);
end;

destructor TDataReplicaFields.Destroy;
begin
  FList.Free;
  FIndexBySourceFieldName.Free;
  inherited Destroy;
end;

procedure TDataReplicaFields.RebuildIndexes;
var
  i : integer;
begin
  FIndexBySourceFieldName.Clear;
  for i := 0 to Count -1 do
    FIndexBySourceFieldName.Add(Get(i).SourceField.AsUppercaseString, Get(i));
end;

function TDataReplicaFields.Count: integer;
begin
  Result := FList.Count;
end;

function TDataReplicaFields.Get(const aIndex: integer): TDataReplicaFieldToField;
begin
  Result := FList.Items[aIndex] as TDataReplicaFieldToField;
end;

function TDataReplicaFields.Add: TDataReplicaFieldToField;
begin
  Result := TDataReplicaFieldToField.Create;
  FList.Add(Result);
end;

function TDataReplicaFields.GetBySourceFieldName(const aSourceFieldName: String): TDataReplicaFieldToField;
begin
  if FIndexBySourceFieldName.Count = 0 then
    RebuildIndexes;
  Result := FIndexBySourceFieldName.Find(Uppercase(aSourceFieldName)) as TDataReplicaFieldToField;
end;

{ TDataReplicaFieldToField }

constructor TDataReplicaFieldToField.Create;
begin
  FSourceField := TNullableString.Create();
  FDestinationField := TNullableString.Create();
end;

destructor TDataReplicaFieldToField.Destroy;
begin
  FSourceField.Free;
  FDestinationField.Free;
  inherited Destroy;
end;

end.
