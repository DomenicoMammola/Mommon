// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mVirtualDatasetProvider;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


interface

uses
  Classes, DB,
  mFilter, mSummary, mDataProviderFieldDefs, mFields, mSortConditions;

type
  { TmVirtualDatasetDataProvider }

  TmVirtualDatasetDataProvider = class
  strict private
    FVirtualFieldDefs : TmVirtualFieldDefs;
    FSortConditions : TSortByConditions;
    FFilterConditions : TmFilters;
    FSummaryDefinitions : TmSummaryDefinitions;
    FSummaryValues : TmSummaryValues;
  protected
    procedure FillFieldDefOfDataset (aSource : TmVirtualFieldDef; aPrefix : string; aFieldDef : TFieldDef; aReadOnly : boolean);
    procedure FillField(aSource : TmVirtualFieldDef; aPrefix : String; aField : TmField);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetRecordCount : integer; virtual; abstract;
    procedure GetFieldValue (const AFieldName: String; const AIndex: Cardinal; out AValue: variant); virtual; abstract;
    procedure DeleteRecord (const AIndex :integer); virtual; abstract;
    procedure EditRecord (const AIndex : integer; AModifiedFields : TList); virtual; abstract;
    procedure InsertRecord (const AIndex : integer; AModifiedFields : TList); virtual; abstract;

    function Refresh (const aDoSort, aDoFilter: boolean): boolean; virtual; abstract;
    procedure GetUniqueStringValuesForField(const aFieldName: string; aList: TStringList); virtual; abstract;
    procedure CalculateSummaries; virtual; abstract;

    procedure FillFieldDefsOfDataset(aFieldDefs : TFieldDefs; const aReadOnly : boolean); virtual;
    procedure FillFields(aFields : TmFields); virtual;
    procedure SetDefaultVisibilityOfFields (aFields : TFields); virtual;

    property SortConditions : TSortByConditions read FSortConditions;
    property FilterConditions : TmFilters read FFilterConditions;
    property VirtualFieldDefs : TmVirtualFieldDefs read FVirtualFieldDefs;
    property SummaryDefinitions : TmSummaryDefinitions read FSummaryDefinitions;
    property SummaryValues : TmSummaryValues read FSummaryValues;
  end;


implementation

{ TmVirtualDatasetDataProvider }

constructor TmVirtualDatasetDataProvider.Create;
begin
  FVirtualFieldDefs := TmVirtualFieldDefs.Create;
  FSortConditions := TSortByConditions.Create;
  FFilterConditions := TmFilters.Create;
  FSummaryDefinitions := TmSummaryDefinitions.Create;
  FSummaryValues := TmSummaryValues.Create;
end;

destructor TmVirtualDatasetDataProvider.Destroy;
begin
  FSortConditions.Free;
  FFilterConditions.Free;
  FVirtualFieldDefs.Free;
  FSummaryDefinitions.Free;
  FSummaryValues.Free;
  inherited;
end;

procedure TmVirtualDatasetDataProvider.FillFieldDefOfDataset (aSource : TmVirtualFieldDef; aPrefix : string; aFieldDef : TFieldDef; aReadOnly : boolean);
var
  newName : string;
begin
  newName := aPrefix + aSource.Name;
  aFieldDef.Name := newName;
  aFieldDef.DataType := FromTmVirtualFieldDefTypeToTFieldType(aSource.DataType);
  if (aSource.DataType in [vftString, vftWideString]) then
    aFieldDef.Size := aSource.Size;
  if aSource.Required then
    aFieldDef.Attributes := [faRequired];
  if aReadOnly or aSource.ReadOnly then
    aFieldDef.Attributes := aFieldDef.Attributes + [TFieldAttribute.faReadonly];
  if (aSource.DataType = vftBCD) then
    aFieldDef.Precision := aSource.Precision;
end;

procedure TmVirtualDatasetDataProvider.FillField(aSource: TmVirtualFieldDef; aPrefix: String; aField: TmField);
var
  newName : string;
begin
  newName := aPrefix + aSource.Name;
  aField.FieldName := newName;
  aField.DataType := FromTmVirtualFieldDefTypeToTFieldType(aSource.DataType);
  if aSource.DefaultFormat <> '' then
  begin
    aField.DisplayFormat:= aSource.DefaultFormat;
    aField.EditFormat:= aSource.DefaultFormat;
  end;
end;


procedure TmVirtualDatasetDataProvider.FillFieldDefsOfDataset(aFieldDefs: TFieldDefs; const aReadOnly : boolean);
var
  i : integer;
  CurrentField : TmVirtualFieldDef;
begin
  for i := 0 to FVirtualFieldDefs.Count - 1 do
  begin
    CurrentField := FVirtualFieldDefs[i];
    FillFieldDefOfDataset(CurrentField, '', aFieldDefs.AddFieldDef, aReadOnly);
  end;
end;

procedure TmVirtualDatasetDataProvider.FillFields(aFields: TmFields);
var
  i : integer;
  currentVirtualField : TmVirtualFieldDef;
  curField : TmField;
begin
  for i := 0 to FVirtualFieldDefs.Count - 1 do
  begin
    currentVirtualField := FVirtualFieldDefs[i];
    curField := aFields.Add;
    FillField(currentVirtualField, '', curField);
  end;
end;

procedure TmVirtualDatasetDataProvider.SetDefaultVisibilityOfFields(aFields: TFields);
var
  i : integer;
begin
  for i := 0 to aFields.Count - 1 do
    aFields[i].Visible:= true;
end;

end.
