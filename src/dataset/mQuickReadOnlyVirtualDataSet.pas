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
  DB, Classes, contnrs, Variants,
  mVirtualDataSet, mVirtualDataSetInterfaces, mSortConditions, mFilter, mIntList;

const
  KEY_FIELD_NAME = '_KEY';

type

  { TReadOnlyVirtualDatasetProvider }

  TReadOnlyVirtualDatasetProvider = class (TVirtualDatasetDataProvider)
  strict private
    FIDataProvider : IVDListDataProvider;
    FSortedIndex : TFPList;
    FFilteredIndex : TIntegerList;
    FCurrentSortFields : TStringList;
    FGarbage : TObjectList;
    FFiltered : boolean;

    function OnCompare(Item1: Pointer;Item2: Pointer):Integer;
    procedure InternalGetFieldValue (const aFieldName : string; const AIndex: Integer; out AValue: variant);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Init (aDataProvider : IVDListDataProvider);

    procedure GetFieldValue (const AField: TField; const AIndex: Integer; out AValue: variant); override;
    procedure DeleteRecord (const AIndex :integer); override;
    procedure EditRecord (const AIndex : integer; AModifiedFields : TList); override;
    procedure InsertRecord (const AIndex : integer; AModifiedFields : TList); override;
    function GetRecordCount : integer; override;

    function Refresh (const aDoSort, aDoFilter : boolean): boolean; override;
    procedure GetUniqueStringValuesForField(const aFieldName: string; aList: TStringList); override;

(*    function Sort(const aConditions : TSortByConditions): boolean; override;
    procedure ClearSort; override;
    function Filter(const aFilterConditions : TmFilters) : boolean; override;
    procedure ClearFilter; override;*)
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
  d1, d2 : TDatumShell;
  i : integer;
  val1, val2 : Variant;
begin

  d1 := TDatumShell(Item1);
  d2 := TDatumShell(Item2);


  Result := -1;
  for i := 0 to SortConditions.Count -1 do
  begin
    if CompareText(SortConditions.Items[i].FieldName, KEY_FIELD_NAME) = 0 then
    begin
      val1 := d1.Idx;
      val2 := d2.Idx;
    end
    else
    begin
      val1 := d1.Datum.GetPropertyByFieldName(SortConditions.Items[i].FieldName);
      val2 := d2.Datum.GetPropertyByFieldName(SortConditions.Items[i].FieldName);
    end;
    Result := CompareVariants(val1, val2);
    if Result <> 0 then
    begin
      if SortConditions.Items[i].SortType = stDescending then
        Result := -1 * Result;
      break;
    end;
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.InternalGetFieldValue(const aFieldName: string; const AIndex: Integer; out AValue: variant);
var
  tmpI : IVDDatum;
  idx : integer;
  actualIndex : integer;
begin
  if (aIndex >= 0) then
  begin
    if FFiltered then
      actualIndex:= FFilteredIndex.Nums[aIndex]
    else
      actualIndex:= aIndex;
    if FSortedIndex.Count > 0 then
    begin
      tmpI := TDatumShell(FSortedIndex.Items[actualIndex]).Datum;
      idx := TDatumShell(FSortedIndex.Items[actualIndex]).Idx;
    end
    else
    begin
      tmpI := FIDataProvider.GetDatum(actualIndex);
      idx := aIndex;
    end;
    AValue := Null;

   if CompareText(aFieldName, KEY_FIELD_NAME) = 0 then
     aValue := idx
   else
     aValue := tmpI.GetPropertyByFieldName(aFieldName);
  end;
end;


constructor TReadOnlyVirtualDatasetProvider.Create;
begin
  inherited Create;
  FSortedIndex := TFPList.Create;
  FFilteredIndex := TIntegerList.Create;
  FCurrentSortFields := TStringList.Create;
  FGarbage := TObjectList.Create(true);
  FFiltered:= false;
end;

destructor TReadOnlyVirtualDatasetProvider.Destroy;
begin
  FIDataProvider := nil;
  FreeAndNil(FSortedIndex);
  FreeAndNil(FCurrentSortFields);
  FreeAndNil(FGarbage);
  FreeAndNil(FFilteredIndex);
  inherited Destroy;
end;


procedure TReadOnlyVirtualDatasetProvider.Init(aDataProvider: IVDListDataProvider);
begin
  FIDataProvider := aDataProvider;
  FSortedIndex.Clear;
  FGarbage.Clear;
end;

procedure TReadOnlyVirtualDatasetProvider.GetFieldValue(const AField: TField; const AIndex: Integer; out AValue: variant);
begin
  InternalGetFieldValue(AField.FieldName, AIndex, AValue);
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
  if FFiltered then
    Result := FFilteredIndex.Count
  else
    Result := FIDataProvider.Count;
end;

function TReadOnlyVirtualDatasetProvider.Refresh(const aDoSort, aDoFilter: boolean): boolean;
var
  i, k : integer;
  tmp : TDatumShell;
  visibleRow : boolean;
begin
  Result := false;
  if not Assigned(FIDataProvider) then
    exit
  else
  begin
    // http://lazarus-ccr.sourceforge.net/docs/rtl/classes/tfplist.html
    // http://lazarus-ccr.sourceforge.net/docs/lcl/lclproc/mergesort.html
    if (not aDoSort) then
    begin
      FSortedIndex.Clear;
      FGarbage.Clear;
    end
    else
    begin
      FSortedIndex.Clear;
      FGarbage.Clear;
      for i := 0 to FIDataProvider.Count -1 do
      begin
        tmp := TDatumShell.Create;
        FGarbage.Add(tmp);
        tmp.Datum := FIDataProvider.GetDatum(i);
        tmp.Idx:= i;
        FSortedIndex.Add(tmp);
      end;
      FCurrentSortFields.Clear;
      for i := 0 to SortConditions.Count - 1 do
        FCurrentSortFields.Append(SortConditions.Items[i].FieldName);
      mUtility.MergeSort(FSortedIndex, OnCompare);
      FCurrentSortFields.Clear;
    end;
    if (not aDoFilter) then
    begin
      FFilteredIndex.Clear;
      FFiltered := false;
    end
    else
    begin
      FFilteredIndex.Clear;
      FFiltered := true;

      for i := 0 to FIDataProvider.Count -1 do
      begin
        visibleRow := true;
        for k := 0 to FilterConditions.Count -1 do
        begin
          visibleRow := visibleRow and FilterConditions.Get(k).Evaluate(FIDataProvider.GetDatum(i).GetPropertyByFieldName(FilterConditions.Get(k).FieldName));
          if not visibleRow then
            break;
        end;
        if visibleRow then
          FFilteredIndex.Add(i);
      end;
    end;
  end;
  Result := true;
end;

procedure TReadOnlyVirtualDatasetProvider.GetUniqueStringValuesForField(const aFieldName: string; aList: TStringList);
var
  i : integer;
  tmpValue : variant;
begin
  for i := 0 to Self.GetRecordCount - 1 do
  begin
    Self.InternalGetFieldValue(aFieldName, i, tmpValue);
    if not VarIsNull(tmpValue) and (aList.IndexOf(tmpValue) < 0) then
      aList.Add(tmpValue);
  end;
end;

(*
function TReadOnlyVirtualDatasetProvider.Sort(const aConditions: TSortByConditions) : boolean;
var
  i : integer;
  tmp : TDatumShell;
begin
  Result := false;
  if not Assigned(FIDataProvider) then
    exit
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
      for i := 0 to FIDataProvider.Count -1 do
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
    if FFiltered then
      Self.Filter(FCurrentFilterConditions);
  end;
  Result := true;
end;

procedure TReadOnlyVirtualDatasetProvider.ClearSort;
begin
  FSortedIndex.Clear;
  FGarbage.Clear;
  if FFiltered then
    Self.Filter(FCurrentFilterConditions);
end;

function TReadOnlyVirtualDatasetProvider.Filter(const aFilterConditions: TmFilters): boolean;
var
  i, k : integer;
  visibleRow : boolean;
begin
  FFiltered:= true;
  if not Assigned(FCurrentFilterConditions) then
    FCurrentFilterConditions := TmFilters.Create;

  FCurrentFilterConditions.CopyFrom(aFilterConditions);
  FFilteredIndex.Clear;

  for i := 0 to FIDataProvider.Count -1 do
  begin
    visibleRow := true;
    for k := 0 to aFilterConditions.Count -1 do
    begin
      visibleRow := visibleRow and aFilterConditions.Get(k).Evaluate(FIDataProvider.GetDatum(i).GetPropertyByFieldName(aFilterConditions.Get(k).FieldName));
      if not visibleRow then
        break;
    end;
    if visibleRow then
      FFilteredIndex.Add(i);
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.ClearFilter;
begin
  FFiltered:= false;
  FFilteredIndex.Clear;
  FCurrentFilterConditions.Clear;
end;
*)
end.
