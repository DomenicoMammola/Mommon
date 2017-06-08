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
  DB, Classes,
  mVirtualDataSet, mVirtualDataSetInterfaces, mSortConditions;

const
  KEY_FIELD_NAME = 'KEY';

type

  { TReadOnlyVirtualDatasetProvider }

  TReadOnlyVirtualDatasetProvider = class (TVirtualDatasetDataProvider)
  strict private
    FIDataProvider : IVDListDataProvider;
    FSortedIndex : TFPList;

    FCurrentSortConditions : TSortByConditions;
    FCurrentSortFields : TStringList;

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

end;

destructor TReadOnlyVirtualDatasetProvider.Destroy;
begin
  FSortedIndex.Free;
  FCurrentSortFields.Free;
  inherited Destroy;
end;


procedure TReadOnlyVirtualDatasetProvider.Init(aDataProvider: IVDListDataProvider);
begin
  FIDataProvider := aDataProvider;
  FSortedIndex.Clear;
end;

procedure TReadOnlyVirtualDatasetProvider.GetFieldValue(const AField: TField; const AIndex: Integer; out AValue: variant);
var
  tmpI : IVDDatum;
begin
  AValue := Null;
  if (aIndex >= 0) then
  begin
   if CompareText(aField.FieldName, KEY_FIELD_NAME) = 0 then
     aValue := aIndex
   else
   begin
     if FSortedIndex.Count > 0 then
     begin
       tmpI := IVDDatum(FSortedIndex.Items[aIndex]);
     end
     else
     begin
       tmpI := FIDataProvider.GetDatum(aIndex);
     end;
     aValue := tmpI.GetPropertyByFieldName(AField.FieldName);
   end;
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
      for i := 0 to Self.GetRecordCount -1 do
        FSortedIndex.Add(FIDataProvider.GetDatum(i));
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
end;

end.
