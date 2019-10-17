// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDataProvider;

{$IFDEF FPC}
  {$MODE DELPHI}  
{$ENDIF}

interface

uses
  Contnrs, Classes,
  mDataProviderInterfaces, mDataProviderFieldDefs, mMaps;

type

  { TmDataProvider }

  TmDataProvider = class abstract ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDDataProvider)
  strict private
    FList : TObjectList;

    FMustRebuildIndex : boolean;
    FMainIndex : TmStringDictionary;
    FIndexes : TmStringDictionary;
    FIndexedFieldNames : TStringList;
    procedure ClearIndexes;
  protected
    procedure AddIndex (const aFieldName : String);
    procedure InternalAdd(aDatum : TObject);
    procedure InternalRemove(aDatum : TObject);
    procedure InternalDelete(const aIndex: integer);
    function InternalFindByString (aStringKey : string) : TObject;
    function InternalGetDatum (const aIndex : integer) : TObject;
  public
    constructor Create(const aOwnsObject : boolean = true); virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count : integer;
    function GetDatum(const aIndex : integer) : IVDDatum;
    function FindDatumByKey (const aKey : IVDDatumKey) : IVDDatum;
    function FindDatumByStringKey (const aStringKey : string): IVDDatum;
    function FindDatumByIndexedField (const aIndexedFieldName, aValue : String): IVDDatum;
    procedure RebuildIndex;

    procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String); virtual; abstract;
    function GetKeyFieldName : String; virtual; abstract;
    procedure GetMinimumFields(aFieldsForLookup : TStringList); virtual;
  end;

implementation

uses
  sysutils, variants;

{ TmDataProvider }

procedure TmDataProvider.ClearIndexes;
var
  i : integer;
  tmpIndex : TmStringDictionary;
begin
  for i := 0 to FIndexedFieldNames.Count - 1 do
  begin
    tmpIndex := FIndexes.Find(FIndexedFieldNames.Strings[i]) as TmStringDictionary;
    if Assigned(tmpIndex) then
      tmpIndex.Clear;
  end;
  FMainIndex.Clear;
  FMustRebuildIndex:=true;
end;

procedure TmDataProvider.AddIndex(const aFieldName: String);
begin
  if FIndexedFieldNames.IndexOf(aFieldName) < 0 then
  begin
    FIndexedFieldNames.Add(aFieldName);
    FMustRebuildIndex:= true;
  end;
end;

procedure TmDataProvider.InternalAdd(aDatum: TObject);
begin
  FList.Add(aDatum);
  FMustRebuildIndex:=true;
end;

procedure TmDataProvider.InternalRemove(aDatum: TObject);
begin
  FList.Remove(aDatum);
  FMustRebuildIndex:= true;
end;

procedure TmDataProvider.InternalDelete(const aIndex: integer);
begin
  FList.Delete(aIndex);
  FMustRebuildIndex:= true;
end;

function TmDataProvider.InternalFindByString(aStringKey: string): TObject;
begin
  if FMustRebuildIndex then
    RebuildIndex;
  Result := FMainIndex.Find(aStringKey);
  FMustRebuildIndex:=false;
end;

function TmDataProvider.InternalGetDatum(const aIndex: integer): TObject;
begin
  Result := nil;
  if aIndex < FList.Count then
  begin
    Result := FList.Items[aIndex];
    FMustRebuildIndex:= true;
  end;
end;

procedure TmDataProvider.RebuildIndex;
var
  i, k : integer;
  tmpIndex : TmStringDictionary;
  tmpValue : Variant;
  tmpStringValue : String;
begin
  ClearIndexes;

  for i := 0 to FIndexedFieldNames.Count - 1 do
  begin
    if not Assigned(FIndexes.Find(FIndexedFieldNames.Strings[i])) then
      FIndexes.Add(FIndexedFieldNames.Strings[i], TmStringDictionary.Create(false));
  end;

  for i := 0 to Self.Count -1 do
  begin
    if FMainIndex.Contains(Self.GetDatum(i).GetDatumKey.AsString) then
      raise Exception.Create('Duplicate key: ' + Self.GetDatum(i).GetDatumKey.AsString);
    FMainIndex.Add(Self.GetDatum(i).GetDatumKey.AsString, Self.InternalGetDatum(i));

    for k := 0 to FIndexedFieldNames.Count - 1 do
    begin
      tmpIndex := FIndexes.Find(FIndexedFieldNames.Strings[k]) as TmStringDictionary;
      tmpValue := Self.GetDatum(i).GetPropertyByFieldName(FIndexedFieldNames.Strings[k]);
      tmpStringValue := VarToStr(tmpValue);
      if tmpIndex.Contains(tmpStringValue) then
        raise Exception.Create('Duplicate value for index: ' + tmpStringValue);
      tmpIndex.Add(tmpStringValue, Self.InternalGetDatum(i));
    end;
  end;
  FMustRebuildIndex:= false;
end;

procedure TmDataProvider.GetMinimumFields(aFieldsForLookup: TStringList);
begin
  aFieldsForLookup.Clear;
end;

constructor TmDataProvider.Create(const aOwnsObject : boolean = true);
begin
  FList := TObjectList.Create(aOwnsObject);
  FMainIndex := TmStringDictionary.Create;
  FMustRebuildIndex:= true;
  FIndexes := TmStringDictionary.Create(true);
  FIndexedFieldNames := TStringList.Create;
end;

destructor TmDataProvider.Destroy;
begin
  FList.Free;
  FMainIndex.Free;
  FIndexes.Free;
  FIndexedFieldNames.Free;
  inherited Destroy;
end;

procedure TmDataProvider.Clear;
begin
  FList.Clear;
  ClearIndexes;
end;

function TmDataProvider.Count: integer;
begin
  Result := FList.Count;
end;

function TmDataProvider.GetDatum(const aIndex: integer): IVDDatum;
begin
  Result := InternalGetDatum(aIndex) as IVDDatum;
end;

function TmDataProvider.FindDatumByKey(const aKey: IVDDatumKey): IVDDatum;
begin
  Result := FindDatumByStringKey(aKey.AsString);
end;

function TmDataProvider.FindDatumByStringKey (const aStringKey : string): IVDDatum;
begin
  Result := Self.InternalFindByString(aStringKey) as IVDDatum;
end;

function TmDataProvider.FindDatumByIndexedField(const aIndexedFieldName, aValue: String): IVDDatum;
var
  tmpIndex : TmStringDictionary;
begin
  Result := nil;
  if FMustRebuildIndex then
    RebuildIndex;
  tmpIndex := FIndexes.Find(aIndexedFieldName) as TmStringDictionary;
  if Assigned(tmpIndex) then
    Result := tmpIndex.Find(aValue) as IVDDatum;
  FMustRebuildIndex:=false;
end;

end.
