// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFields;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs, DB,
  mMaps;

const
  SEPARATOR_FIELDS_FROM_JOIN = '$';
  SEPARATOR_FIELDS_FROM_INTERNAL_REFERENCE = '#';
  PREFIX_SYSTEM_FIELDS = '_';


type
  TmField = class;

  TmOnChangeFieldName = procedure (aSource: TmField; const aOldFieldName : String) of object;


  { TmField }

  TmField = class
  strict private
    FFieldName : String;
    FDisplayLabel : String;
    FEditFormat : String;
    FDisplayFormat : String;
    FVisible : boolean;
    FDataType : TFieldType;
  private
    OnChangeFieldName : TmOnChangeFieldName;
    procedure SetFieldName(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AssignFromField(const aSource: TField);
    procedure Assign(const aSource: TmField);

    property FieldName : String read FFieldName write SetFieldName;
    property DisplayLabel : String read FDisplayLabel write FDisplayLabel;
    property EditFormat : String read FDisplayFormat write FDisplayFormat;
    property DisplayFormat : String read FDisplayFormat write FDisplayFormat;
    property Visible : boolean read FVisible write FVisible;
    property DataType : TFieldType read FDataType write FDataType;
  end;

  { TmFields }

  TmFields = class
  strict private
    FFieldsDictionary : TmStringDictionary;
    FList: TObjectList;
    procedure OnChangeFieldName (aSource: TmField; const aOldFieldName : String);
  public
    constructor Create;
    destructor Destroy; override;

    function Add: TmField;
    function Count : integer;
    function Get(const aIndex : integer): TmField;
    procedure Remove(const aIndex : integer);

    procedure Clear;
    procedure Assign(const aSource : TmFields);

    function FieldByName(const aFieldName : String) : TmField;
  end;

function GetSystemFieldName (aOriginalFieldName : String) : String;
function IsSystemField (aFieldName : string) : boolean; overload;

implementation

uses
  sysutils;

function GetSystemFieldName(aOriginalFieldName: String): String;
begin
  Result := PREFIX_SYSTEM_FIELDS + aOriginalFieldName;
end;

function IsSystemField (aFieldName : string) : boolean;
begin
  Result := (aFieldName[1] = PREFIX_SYSTEM_FIELDS);
  Result := Result or (Pos(SEPARATOR_FIELDS_FROM_INTERNAL_REFERENCE + PREFIX_SYSTEM_FIELDS, aFieldName) > 0);
  Result := Result or (Pos(SEPARATOR_FIELDS_FROM_JOIN + PREFIX_SYSTEM_FIELDS, aFieldName) > 0);
end;

{ TmFields }

procedure TmFields.OnChangeFieldName(aSource: TmField; const aOldFieldName: String);
begin
  if aOldFieldName <> '' then
    FFieldsDictionary.Remove(Uppercase(aOldFieldName));
  if aSource.FieldName <> '' then
    FFieldsDictionary.Add(Uppercase(aSource.FieldName), aSource);
end;

constructor TmFields.Create;
begin
  FFieldsDictionary := TmStringDictionary.Create(false);
  FList := TObjectList.Create(true);
end;

destructor TmFields.Destroy;
begin
  FFieldsDictionary.Free;
  FList.Free;
  inherited Destroy;
end;

function TmFields.Add: TmField;
begin
  Result := TmField.Create;
  FList.Add(Result);
  Result.OnChangeFieldName:= OnChangeFieldName;
end;

function TmFields.Count: integer;
begin
  Result := FList.Count;
end;

function TmFields.Get(const aIndex: integer): TmField;
begin
  Result := FList.Items[aIndex] as TmField;
end;

procedure TmFields.Remove(const aIndex: integer);
begin
  FFieldsDictionary.Remove(Uppercase(Get(aIndex).FieldName));
  FList.Remove(Get(aIndex));
end;

procedure TmFields.Clear;
begin
  FFieldsDictionary.Clear;
  FList.Clear;
end;

procedure TmFields.Assign(const aSource: TmFields);
var
  i : integer;
begin
  Self.Clear;
  for i := 0 to aSource.Count - 1 do
    Self.Add.Assign(aSource.Get(i));
end;

function TmFields.FieldByName(const aFieldName: String): TmField;
begin
  Result := FFieldsDictionary.Find(Uppercase(aFieldName)) as TmField;
end;

{ TmField }

procedure TmField.SetFieldName(AValue: String);
var
  oldValue : String;
begin
  if FFieldName=AValue then Exit;

  oldValue := FFieldName;
  FFieldName:=AValue;

  if Assigned(OnChangeFieldName) then
    OnChangeFieldName(Self, oldValue);
end;

constructor TmField.Create;
begin
  FFieldName:= '';
  FDisplayFormat:= '';
  FDisplayLabel:= '';
  FEditFormat:= '';
  FVisible:= true;
  FDataType:= ftUnknown;
  OnChangeFieldName:= nil;
end;

destructor TmField.Destroy;
begin
  inherited Destroy;
end;

procedure TmField.AssignFromField(const aSource: TField);
begin
  DataType:= aSource.DataType;
  DisplayLabel:= aSource.DisplayLabel;
  Visible:= aSource.Visible;
  if aSource is TNumericField then
  begin
    DisplayFormat:= (aSource as TNumericField).DisplayFormat;
    EditFormat:= (aSource as TNumericField).EditFormat;
  end
  else
  begin
    DisplayFormat:= '';
    EditFormat:= '';
  end;
  FieldName := aSource.FieldName;
end;

procedure TmField.Assign(const aSource: TmField);
begin
  FieldName:= aSource.FieldName;
  DisplayLabel:= aSource.DisplayLabel;
  EditFormat:= aSource.EditFormat;
  DisplayFormat:= aSource.DisplayFormat;
  Visible:= aSource.Visible;
  DataType:= aSource.DataType;
end;


end.
