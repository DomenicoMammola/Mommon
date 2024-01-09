// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mQuickReadOnlyVirtualDataSetProvider;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  DB, Classes, contnrs, Variants, StrHashMap,
  mDataProviderInterfaces, mSortConditions, mFilter, mIntList, mLog, mFields,
  mVirtualDataSetJoins, mDataProviderFieldDefs, mVirtualDatasetFormulas, KAParser, mVirtualDatasetProvider;

const
  KEY_FIELD_NAME = '_KEY';

type

  { TReadOnlyVirtualDatasetProvider }

  TReadOnlyVirtualDatasetProvider = class (TmVirtualDatasetDataProvider)
  strict private
    FIDataProvider : IVDDataProvider;
    FSortedIndex : TFPList;
    FFilteredIndex : TCardinalList;
    FCurrentSortFields : TStringList;
    FGarbage : TObjectList;
    FFiltered : boolean;
    FBuiltInJoins : TmBuiltInJoins;
    FFormulaFields : TmFormulaFields;
    FFieldsFromJoinsAreVisibleByDefault : boolean;
    FFieldsFromJoin : TStringList;
    FParser : TKAParser;
    FCurrentDatumForParser : IVDDatum;
    FCurrentDatumForFilterEvaluation : IVDDatum;
    FFiltersEvaluator: TmFiltersEvaluator;

    function OnCompare(Item1: Pointer;Item2: Pointer):Integer;
    procedure InternalGetFieldValue (const aFieldName : string; const AIndex: Cardinal; out AValue: variant);
    procedure GetFieldValueFromDatum (const aDatum : IVDDatum; const aFieldName : string; out aValue :variant);
    procedure GetValueForParser(Sender: TObject; const valueName: string; var Value: Double; out Successfull : boolean);
    procedure GetStrValueForParser(Sender: TObject; const valueName: string; var StrValue: string; out Successfull : boolean);
    procedure GetValueForFilterEvaluator(const aFilterIndex : integer; out aValue : variant);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Init (aDataProvider : IVDDataProvider);

    procedure GetFieldValue (const AFieldName: String; const AIndex: Cardinal; out AValue: variant); override;
    procedure DeleteRecord (const AIndex :integer); override;
    procedure EditRecord (const AIndex : integer; AModifiedFields : TList); override;
    procedure InsertRecord (const AIndex : integer; AModifiedFields : TList); override;
    function GetRecordCount : integer; override;
    procedure FillFieldDefsOfDataset(aFieldDefs : TFieldDefs; const aReadOnly: boolean); override;
    procedure FillFields(aFields : TmFields); override;
    procedure SetDefaultVisibilityOfFields (aFields : TFields); override;
    function GetKeyFieldName: String;

    procedure Clear;
    function Refresh (const aDoSort, aDoFilter : boolean): boolean; override;
    procedure CalculateSummaries; override;
    procedure GetUniqueStringValuesForField(const aFieldName: string; aList: TStringList); override;

    property BuiltInJoins : TmBuiltInJoins read FBuiltInJoins;
    property FieldsFromJoinsAreVisibleByDefault : boolean read FFieldsFromJoinsAreVisibleByDefault write FFieldsFromJoinsAreVisibleByDefault;
    property FormulaFields : TmFormulaFields read FFormulaFields;
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

var
  logger : TmLog;

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
      GetFieldValueFromDatum(d1.Datum, SortConditions.Items[i].FieldName, val1);
      GetFieldValueFromDatum(d2.Datum, SortConditions.Items[i].FieldName, val2);
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

procedure TReadOnlyVirtualDatasetProvider.InternalGetFieldValue(const aFieldName: string; const AIndex: Cardinal; out AValue: variant);
var
  tmpI : IVDDatum;
  idx: integer;
  actualIndex : integer;
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
    GetFieldValueFromDatum(tmpI, aFieldName, AValue);
end;

procedure TReadOnlyVirtualDatasetProvider.GetFieldValueFromDatum(const aDatum: IVDDatum; const aFieldName : string; out aValue: variant);
var
  tmpPrefix, tmpFieldName, tmpString : string;
  tmpBuiltinJoin : TmBuiltInJoin;
  tmpObj : IVDDatum;
  tmpFormulaField : TmFormulaField;
  tmpDouble : Double;
begin
  aValue := Null;

  if not Assigned(aDatum) then
    exit;

  tmpFormulaField := FFormulaFields.FindByName(aFieldName);
  if Assigned(tmpFormulaField) then
  begin
    if Assigned(FCurrentDatumForParser) then
      raise Exception.Create('Error while computing field ' + aFieldName + ': it is not possibile to use a formula field to calculate another formula field.');
    FCurrentDatumForParser := aDatum;
    try
      if (tmpFormulaField.DataType=fftFloat) or (tmpFormulaField.DataType = fftDateTime) then
      begin
        try
          if FParser.Calculate(tmpFormulaField.Formula, tmpDouble) then
          begin
            if (tmpFormulaField.DataType = fftDateTime) and (tmpDouble = 0) then
              aValue := Null
            else
              aValue := tmpDouble
          end
          else
            aValue := Null;
        except
          aValue := Null;
        end;
      end
      else
      begin
        try
          if FParser.CalculateString(tmpFormulaField.Formula, tmpString) then
            aValue := tmpString
          else
            aValue := Null;
        except
          aValue := 'ERROR';
        end;
      end;
    finally
      FCurrentDatumForParser := nil;
    end;
  end
  else
  begin
    if FBuiltInJoins.Count > 0 then
    begin
      ExtractPrefixAndFieldName(aFieldName, tmpPrefix, tmpFieldName);
      tmpBuiltinJoin := FBuiltInJoins.FindByPrefix(tmpPrefix);
      if Assigned(tmpBuiltinJoin) then
      begin
        tmpString := tmpBuiltinJoin.DoBuildExternalEntityKey(aDatum);
        tmpObj := tmpBuiltinJoin.DoFindDatumByStringKey(tmpString);
        if Assigned(tmpObj) then
          aValue := tmpObj.GetPropertyByFieldName(tmpFieldName);
      end
      else
      begin
        aValue := aDatum.GetPropertyByFieldName(aFieldName);
      end;
    end
    else
      aValue := aDatum.GetPropertyByFieldName(aFieldName);
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.GetValueForParser(Sender: TObject; const valueName: string; var Value: Double; out Successfull: boolean);
var
  tmpVar : variant;
begin
  Successfull:= false;
  if Assigned(VirtualFieldDefs.FindByName(valueName)) or (FFieldsFromJoin.IndexOf(valueName) >= 0) then
  begin
    GetFieldValueFromDatum(FCurrentDatumForParser, uppercase(valueName), tmpVar);
    Value := TKAParser.VariantToFloat(tmpVar);
    Successfull:= true;
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.GetStrValueForParser(Sender: TObject; const valueName: string; var StrValue: string; out Successfull: boolean);
var
  tmpVar : variant;
begin
  Successfull:= false;
  if Assigned(VirtualFieldDefs.FindByName(valueName)) or (FFieldsFromJoin.IndexOf(valueName) >= 0) then
  begin
    GetFieldValueFromDatum(FCurrentDatumForParser, uppercase(valueName), tmpVar);
    StrValue := VarToStr(tmpVar);
    Successfull:= true;
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.GetValueForFilterEvaluator(const aFilterIndex: integer; out aValue: variant);
begin
  Self.GetFieldValueFromDatum(FCurrentDatumForFilterEvaluation, FilterConditions.Get(aFilterIndex).FieldName, aValue);
end;


constructor TReadOnlyVirtualDatasetProvider.Create;
begin
  inherited Create;
  FSortedIndex := TFPList.Create;
  FFilteredIndex := TCardinalList.Create;
  FCurrentSortFields := TStringList.Create;
  FGarbage := TObjectList.Create(true);
  FFiltered:= false;
  FBuiltInJoins := TmBuiltInJoins.Create;
  FParser := TKAParser.Create;
  FParser.OnGetStrValue:= Self.GetStrValueForParser;
  FParser.OnGetValue:= Self.GetValueForParser;
  FFieldsFromJoinsAreVisibleByDefault:= false;
  FFieldsFromJoin := TStringList.Create;
  FFormulaFields := TmFormulaFields.Create;
  FFiltersEvaluator:= TmFiltersEvaluator.Create;
end;

destructor TReadOnlyVirtualDatasetProvider.Destroy;
begin
  FIDataProvider := nil;
  FreeAndNil(FSortedIndex);
  FreeAndNil(FCurrentSortFields);
  FreeAndNil(FGarbage);
  FreeAndNil(FFilteredIndex);
  FreeAndNil(FParser);
  FreeAndNil(FBuiltInJoins);
  FreeAndNil(FFieldsFromJoin);
  FreeAndNil(FFormulaFields);
  FreeAndNil(FFiltersEvaluator);
  inherited Destroy;
end;


procedure TReadOnlyVirtualDatasetProvider.Init(aDataProvider: IVDDataProvider);
begin
  FIDataProvider := aDataProvider;
  FSortedIndex.Clear;
  FGarbage.Clear;
end;

procedure TReadOnlyVirtualDatasetProvider.GetFieldValue(const AFieldName: String; const AIndex: Cardinal; out AValue: variant);
begin
  InternalGetFieldValue(AFieldName, AIndex, AValue);
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

procedure TReadOnlyVirtualDatasetProvider.FillFieldDefsOfDataset(aFieldDefs: TFieldDefs; const aReadOnly: boolean);
var
  k, i : integer;
  CurrentField : TmVirtualFieldDef;
  CurrentJoin : TmBuiltInJoin;
  tmpFieldDef : TFieldDef;
begin
  inherited FillFieldDefsOfDataset(aFieldDefs, aReadOnly);

  FFieldsFromJoin.Clear;
  for k := 0 to FBuiltInJoins.Count - 1 do
  begin
    CurrentJoin := FBuiltInJoins.Get(k);
    for i := 0 to CurrentJoin.VirtualFieldDefs.Count -1 do
    begin
      CurrentField := CurrentJoin.VirtualFieldDefs[i];
      tmpFieldDef := aFieldDefs.AddFieldDef;
      Self.FillFieldDefOfDataset(CurrentField, CurrentJoin.Prefix, tmpFieldDef, aReadOnly);
      FFieldsFromJoin.Add(tmpFieldDef.Name);
    end;
  end;

  for k := 0 to FFormulaFields.Count -1 do
  begin
    tmpFieldDef := aFieldDefs.AddFieldDef;
    tmpFieldDef.Name:= FFormulaFields.Get(k).Name;
    tmpFieldDef.DataType:= FromTmFormulaFieldTypeToTFieldType(FFormulaFields.Get(k).DataType);
    tmpFieldDef.Size := FFormulaFields.Get(k).Size;
    tmpFieldDef.Attributes := tmpFieldDef.Attributes + [TFieldAttribute.faReadonly];
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.FillFields(aFields: TmFields);
var
  k, i : integer;
  CurrentField : TmVirtualFieldDef;
  CurrentJoin : TmBuiltInJoin;
  tmpField : TmField;
begin
  inherited FillFields(aFields);

  FFieldsFromJoin.Clear;
  for k := 0 to FBuiltInJoins.Count - 1 do
  begin
    CurrentJoin := FBuiltInJoins.Get(k);
    for i := 0 to CurrentJoin.VirtualFieldDefs.Count -1 do
    begin
      CurrentField := CurrentJoin.VirtualFieldDefs[i];
      tmpField := aFields.Add;
      Self.FillField(CurrentField, CurrentJoin.Prefix, tmpField);
      FFieldsFromJoin.Add(tmpField.FieldName);
    end;
  end;

  for k := 0 to FFormulaFields.Count -1 do
  begin
    tmpField := aFields.Add;
    tmpField.FieldName:= FFormulaFields.Get(k).Name;
    tmpField.DataType:= FromTmFormulaFieldTypeToTFieldType(FFormulaFields.Get(k).DataType);
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.SetDefaultVisibilityOfFields(aFields: TFields);
var
  i : integer;
begin
  if not FFieldsFromJoinsAreVisibleByDefault then
  begin
    for i := 0 to aFields.Count - 1 do
    begin
      if FFieldsFromJoin.IndexOf(aFields[i].FieldName) >= 0 then
        aFields[i].Visible:= false;
    end;
  end;
end;

function TReadOnlyVirtualDatasetProvider.GetKeyFieldName: String;
begin
  Result := FIDataProvider.GetKeyFieldName;
end;

procedure TReadOnlyVirtualDatasetProvider.Clear;
begin
  if not Assigned(FIDataProvider) then
    exit
  else
  begin
    FIDataProvider.Clear;
    Self.Refresh(true, true);
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.CalculateSummaries;
var
  i, k : integer;
  tmpValue: Variant;
begin
  SummaryValues.Clear;

  for i := 0 to SummaryDefinitions.Count -1 do
    SummaryValues.AddValue(SummaryDefinitions.Get(i));

  for i := 0 to Self.GetRecordCount - 1 do
  begin
    for k := 0 to SummaryValues.Count -1 do
    begin
      Self.InternalGetFieldValue(SummaryValues.Get(k).Definition.FieldName, i, tmpValue);
      SummaryValues.Get(k).ComputeValueInSummaries(tmpValue);
    end;
  end;
end;

function TReadOnlyVirtualDatasetProvider.Refresh(const aDoSort, aDoFilter: boolean): boolean;
var
  i : integer;
  tmp : TDatumShell;
begin
  Result := false;
  if not Assigned(FIDataProvider) then
    exit
  else
  begin
    // SORT ---------------
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
      begin
        logger.Debug('Sort on field:' + SortConditions.Items[i].FieldName);
        FCurrentSortFields.Append(SortConditions.Items[i].FieldName);
      end;
      mUtility.MergeSort(FSortedIndex, OnCompare);
      FCurrentSortFields.Clear;
    end;
    // FILTER ---------------
    if (not aDoFilter) then
    begin
      FFilteredIndex.Clear;
      if FFiltered then
      begin
        FFiltered := false;
        Self.CalculateSummaries;
      end;
    end
    else
    begin
      FFilteredIndex.Clear;
      FFiltered := true;
      logger.Debug('[TReadOnlyVirtualDatasetProvider.Refresh] - start evaluation to apply filter');

      FFiltersEvaluator.StartEvaluation(FilterConditions, Self.GetValueForFilterEvaluator);
      try
        logger.Debug('[TReadOnlyVirtualDatasetProvider.Refresh] - total row:' + IntToStr(FIDataProvider.Count));
        if FSortedIndex.Count > 0 then
        begin
          for i := 0 to FSortedIndex.Count - 1 do
          begin
            FCurrentDatumForFilterEvaluation := TDatumShell(FSortedIndex.Items[i]).Datum;

            if FFiltersEvaluator.Evaluate then
              FFilteredIndex.Add(i);
          end;
        end
        else
        begin
          for i := 0 to FIDataProvider.Count -1 do
          begin
            FCurrentDatumForFilterEvaluation := FIDataProvider.GetDatum(i);

            if FFiltersEvaluator.Evaluate then
              FFilteredIndex.Add(i);
          end;
        end;
      finally
        FFiltersEvaluator.EndEvaluation;
      end;
      Self.CalculateSummaries;
      logger.Debug('[TReadOnlyVirtualDatasetProvider.Refresh] - end evaluation. Found:' + IntToStr(FFilteredIndex.Count));
    end;
  end;
  Result := true;
end;

procedure TReadOnlyVirtualDatasetProvider.GetUniqueStringValuesForField(const aFieldName: string; aList: TStringList);
var
  i : integer;
  tmpValue : variant;
  tmpIndex : TStringHashMap;
  str : String;
begin
  tmpIndex := TStringHashMap.Create;
  try
    for i := 0 to Self.GetRecordCount - 1 do
    begin
      Self.InternalGetFieldValue(aFieldName, i, tmpValue);

      str := VarToStr(tmpValue);

      if not tmpIndex.Contains(str) then
      begin
        tmpIndex.Add(str, tmpIndex);
        aList.Add(str);
      end;
    end;
    aList.Sort;
  finally
    tmpIndex.Free;
  end;
end;


initialization
  logger := logManager.AddLog('mQuickReadOnlyVirtualDataSet');

end.
