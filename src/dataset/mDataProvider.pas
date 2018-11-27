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
  Contnrs, Dialogs, Classes,
  mDataProviderInterfaces, mDataProviderFieldDefs, mMaps;

type

  { TmDataProvider }

  TmDataProvider = class abstract ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDDataProvider)
  strict private
    FList : TObjectList;

    FMustRebuildIndex : boolean;
  protected
    FMap : TmStringDictionary;
    procedure InternalAdd(aDatum : TObject);
    procedure InternalRemove(aDatum : TObject); overload;
    procedure InternalRemove(const aIndex: integer); overload;
    function InternalFindByString (aStringKey : string) : TObject;
    function InternalGetDatum (const aIndex : integer) : TObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count : integer;
    function GetDatum(const aIndex : integer) : IVDDatum;
    function FindDatumByKey (const aKey : IVDDatumKey) : IVDDatum;
    function FindDatumByStringKey (const aStringKey : string): IVDDatum;
    procedure RebuildIndex;

    procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String); virtual; abstract;
    function GetKeyFieldName : String; virtual; abstract;
    procedure GetMinimumFields(aFieldsForLookup : TStringList); virtual;
  end;

implementation

uses
  sysutils;

{ TmDataProvider }

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

procedure TmDataProvider.InternalRemove(const aIndex: integer);
begin
  FList.Delete(aIndex);
  FMustRebuildIndex:= true;
end;

function TmDataProvider.InternalFindByString(aStringKey: string): TObject;
begin
  if FMustRebuildIndex then
    RebuildIndex;
  Result := FMap.Find(aStringKey);
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
  i : integer;
begin
  FMap.Clear;
  for i := 0 to Self.Count -1 do
  begin
    if FMap.Contains(Self.GetDatum(i).GetDatumKey.AsString) then
      raise Exception.Create('Duplicate key: ' + Self.GetDatum(i).GetDatumKey.AsString);
    FMap.Add(Self.GetDatum(i).GetDatumKey.AsString, Self.InternalGetDatum(i));
  end;
  FMustRebuildIndex:= false;
end;

procedure TmDataProvider.GetMinimumFields(aFieldsForLookup: TStringList);
begin
  aFieldsForLookup.Clear;
end;

constructor TmDataProvider.Create;
begin
  FList := TObjectList.Create(true);
  FMap := TmStringDictionary.Create;
  FMustRebuildIndex:= true;
end;

destructor TmDataProvider.Destroy;
begin
  FList.Free;
  FMap.Free;
  inherited Destroy;
end;

procedure TmDataProvider.Clear;
begin
  FList.Clear;
  FMap.Clear;
  FMustRebuildIndex:=true;

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

end.
