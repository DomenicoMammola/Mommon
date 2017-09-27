// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mVirtualDatasetDataProvider;

{$IFDEF FPC}
  {$MODE DELPHI}  
{$ENDIF}

interface

uses
  Contnrs, Dialogs,
  mInterfaces, StrHashMap, mVirtualDataSetInterfaces;

type


  { TmDatasetDataProvider }

  TmDatasetDataProvider = class abstract ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDListDataProvider)
  strict private
    FList : TObjectList;

    FMustRebuildIndex : boolean;
  protected
    FMap : TStringHashMap;
    procedure InternalAdd(aDatum : TObject);
    function InternalFindByString (aStringKey : string) : TObject;
    function InternalGetDatum (aIndex : integer) : TObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count : integer;
    function GetDatum(aIndex : integer) : IVDDatum;
    function FindDatumByKey (aKey : IVDDatumKey) : IVDDatum;
    function FindDatumByStringKey (aStringKey : string): IVDDatum;
    procedure RebuildIndex;
  end;

implementation



{ TmDatasetDataProvider }

procedure TmDatasetDataProvider.InternalAdd(aDatum: TObject);
begin
  FList.Add(aDatum);
  FMustRebuildIndex:=true;
end;

function TmDatasetDataProvider.InternalFindByString(aStringKey: string): TObject;
var
  tmp : pointer;
begin
  if FMustRebuildIndex then
    RebuildIndex;
  if FMap.Find(aStringKey, tmp) then
    Result := TObject(tmp)
  else
    Result := nil;
  FMustRebuildIndex:=false;
end;

function TmDatasetDataProvider.InternalGetDatum(aIndex: integer): TObject;
begin
  Result := nil;
  if aIndex < FList.Count then
  begin
    Result := FList.Items[aIndex];
    FMustRebuildIndex:= true;
  end;
end;

procedure TmDatasetDataProvider.RebuildIndex;
var
  i : integer;
begin
  FMap.Clear;
  for i := 0 to Self.Count -1 do
  begin
    FMap.Add(Self.GetDatum(i).GetDatumKey.AsString, Self.InternalGetDatum(i));
  end;
  FMustRebuildIndex:= false;
end;

constructor TmDatasetDataProvider.Create;
begin
  FList := TObjectList.Create(true);
  FMap := TStringHashMap.Create;
  FMustRebuildIndex:= true;
end;

destructor TmDatasetDataProvider.Destroy;
begin
  FList.Free;
  FMap.Free;
  inherited Destroy;
end;

procedure TmDatasetDataProvider.Clear;
begin
  FList.Clear;
  FMap.Clear;
  FMustRebuildIndex:=true;

end;

function TmDatasetDataProvider.Count: integer;
begin
  Result := FList.Count;
end;

function TmDatasetDataProvider.GetDatum(aIndex: integer): IVDDatum;
begin
  Result := InternalGetDatum(aIndex) as IVDDatum;
end;

function TmDatasetDataProvider.FindDatumByKey(aKey: IVDDatumKey): IVDDatum;
begin
  Result := FindDatumByStringKey(aKey.AsString);
end;

function TmDatasetDataProvider.FindDatumByStringKey (aStringKey : string): IVDDatum;
begin
  Result := Self.InternalFindByString(aStringKey) as IVDDatum;
end;

end.
