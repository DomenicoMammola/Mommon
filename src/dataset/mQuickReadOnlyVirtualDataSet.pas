// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mQuickReadOnlyVirtualDataSet;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  DB, Classes, contnrs,
  mVirtualDataSet, mVirtualDataSetInterfaces, mSortConditions;

const
  KEY_FIELD_NAME = '_KEY';

type

  { TReadOnlyVirtualDatasetProvider }

  TReadOnlyVirtualDatasetProvider = class (TVirtualDatasetDataProvider)
  strict private
    FIDataProvider : IVDListDataProvider;
    FSortedIndex : TFPList;

    FCurrentSortConditions : TSortByConditions;
    FCurrentSortFields : TStringList;
    FGarbage : TObjectList;

    function OnCompare(Item1: Pointer;Item2: Pointer):Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Init (aDataProvider : IVDListDataProvider);

    procedure GetFieldValue (const AField: TField; const AIndex: Integer; out AValue: variant); override;
    procedure DeleteRecord (const AIndex :integer); override;
    procedure EditRecord (const AIndex : integer; AModifiedFields : TList); override;
    procedure InsertRecord (const AIndex : integer; AModifiedFields : TList); override;
    function GetRecordCount : integer; override;
    function Sort(const aConditions : TSortByConditions): boolean; override;
    procedure ClearSort; override;
  end;

implementation

uses
  SysUtils,
  mUtility;

type
  TDatumShell = class
  public
    Datum : IVDDatum;
    Idx : integer;
  end;


{ TReadOnlyVirtualDatasetProvider }

function TReadOnlyVirtualDatasetProvider.OnCompare(Item1: Pointer;Item2: Pointer):Integer;
var
  d1, d2 : IVDDatum;
  tmpCondIndex : integer;
begin
  d1 := IVDDatum(Item1);
  d2 := IVDDatum(Item2);

  Result := CompareByProperties(d1, d2, FCurrentSortFields, tmpCondIndex);

  if Result <> 0 then
  begin
    if FCurrentSortConditions.Items[tmpCondIndex].SortType = stDescending then
      Result := -1 * Result;
  end;
end;


constructor TReadOnlyVirtualDatasetProvider.Create;
begin
  inherited Create;
  FSortedIndex := TFPList.Create;
  FCurrentSortFields := TStringList.Create;
  FGarbage := TObjectList.Create(true);
end;

destructor TReadOnlyVirtualDatasetProvider.Destroy;
begin
  FSortedIndex.Free;
  FCurrentSortFields.Free;
  FGarbage.Free;
  inherited Destroy;
end;


procedure TReadOnlyVirtualDatasetProvider.Init(aDataProvider: IVDListDataProvider);
begin
  FIDataProvider := aDataProvider;
  FSortedIndex.Clear;
  FGarbage.Clear;
end;

procedure TReadOnlyVirtualDatasetProvider.GetFieldValue(const AField: TField; const AIndex: Integer; out AValue: variant);
var
  tmpI : IVDDatum;
  idx : integer;
begin
  if (aIndex >= 0) then
  begin

    if FSortedIndex.Count > 0 then
    begin
      tmpI := TDatumShell(FSortedIndex.Items[aIndex]).Datum;
      idx := TDatumShell(FSortedIndex.Items[aIndex]).Idx;
    end
    else
    begin
      tmpI := FIDataProvider.GetDatum(aIndex);
      idx := aIndex;
    end;
    AValue := Null;

   if CompareText(aField.FieldName, KEY_FIELD_NAME) = 0 then
     aValue := idx
   else
     aValue := tmpI.GetPropertyByFieldName(AField.FieldName);
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.DeleteRecord(const AIndex: integer);
begin
  // do nothing
end;

procedure TReadOnlyVirtualDatasetProvider.EditRecord(const AIndex: integer; AModifiedFields: TList);
begin
  // do nothing
end;

procedure TReadOnlyVirtualDatasetProvider.InsertRecord(const AIndex: integer; AModifiedFields: TList);
begin
  // do nothing
end;

function TReadOnlyVirtualDatasetProvider.GetRecordCount: integer;
begin
    Result := FIDataProvider.Count;
end;

function TReadOnlyVirtualDatasetProvider.Sort(const aConditions: TSortByConditions) : boolean;
var
  i : integer;
  tmp : TDatumShell;
begin
  if not Assigned(FIDataProvider) then
    Result := false
  else
  begin
    // http://lazarus-ccr.sourceforge.net/docs/rtl/classes/tfplist.html
    // http://lazarus-ccr.sourceforge.net/docs/lcl/lclproc/mergesort.html
    if (not Assigned(aConditions)) or  (aConditions.Count =  0) then
      Self.ClearSort
    else
    begin
      FSortedIndex.Clear;
      FGarbage.Clear;
      for i := 0 to Self.GetRecordCount -1 do
      begin
        tmp := TDatumShell.Create;
        FGarbage.Add(tmp);
        tmp.Datum := FIDataProvider.GetDatum(i);
        tmp.Idx:= i;
        FSortedIndex.Add(tmp);
      end;
      FCurrentSortConditions := aConditions;
      FCurrentSortFields.Clear;
      for i := 0 to aConditions.Count - 1 do
        FCurrentSortFields.Append(aConditions.Items[i].FieldName);
      mUtility.MergeSort(FSortedIndex, OnCompare);
      FCurrentSortFields.Clear;
    end;
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.ClearSort;
begin
  FSortedIndex.Clear;
  FGarbage.Clear;
end;

end.
