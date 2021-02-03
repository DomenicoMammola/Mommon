// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mPivoter;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  contnrs, db, variants, Classes,
  mDataProviderInterfaces,
  mIntList, mMaps, mSummary, mNullables;

type

  TmSortByCondition = (stAscending, stDescending, stNone);

  TmGroupByOperationKind = (gpoDistinct, gpoDateYear, gpoDateMonth, gpoDateTheMonth, gpoDateDay, gpoDateTheDay, gpoDateQuarter, gpoDateTheQuarter, gpoFirstLetter, goFormula);

  TmKeyValue = class;

  { TmGroupByDef }

  TmGroupByDef = class
  strict private
    FFieldName: string;
    FDataType : TFieldType;
    FOperationKind : TmGroupByOperationKind;
    FFormula : TNullableString;
    FDisplayFormat: TNullableString;
    FDisplayLabel : TNullableString;
    FSortBy: TmSortByCondition;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const aSource : TmGroupByDef);
    class function CheckOperationKindCompatibility(const aOperationKind : TmGroupByOperationKind; const aDataType : TFieldType) : boolean;
    function IsFloat : boolean;
    function IsInteger : boolean;
    function IsString : boolean;
    function IsDate : boolean;
    function FormatValue(const aKeyValue : TmKeyValue): String;

    property FieldName : string read FFieldName write FFieldName;
    property DataType : TFieldType read FDataType write FDataType;
    property OperationKind : TmGroupByOperationKind read FOperationKind write FOperationKind;
    property Formula: TNullableString read FFormula;
    property DisplayLabel : TNullableString read FDisplayLabel;
    property DisplayFormat : TNullableString read FDisplayFormat;
    property SortBy : TmSortByCondition read FSortBy write FSortBy;
  end;

  { TmGroupByDefs }

  TmGroupByDefs = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const aSource : TmGroupByDefs);

    function Add : TmGroupByDef;
    function Count : integer;
    function Get(const aIndex : integer): TmGroupByDef;
    procedure Clear;
  end;


  { TmKeyValue }

  TmKeyValue = class
  strict private
    FKeyValueAsString : String;
    FKeyActualValue : Variant;
  public
    constructor Create(const aKeyValueAsString : String; const aKeyActualValue : Variant);
    destructor Destroy; override;

    property KeyValueAsString : String read FKeyValueAsString;
    property KeyActualValue : Variant read FKeyActualValue;
  end;
  { TmKeysIndex }

  TmKeysIndex = class
  strict private
    FKeyValues : TFPList;
    FKeyValuesDictionary : TmStringDictionary;
    FSubIndexes : TmStringDictionary;
    FGarbage : TObjectList;
    FLevel : integer;
    function OnCompareAscending(Item1: Pointer;Item2: Pointer):Integer;
    function OnCompareDescending(Item1: Pointer;Item2: Pointer):Integer;
  private
    function AddSubIndexForKey(const aKeyValue: String) : TmKeysIndex;
  public
    constructor Create(const aLevel : integer);
    destructor Destroy; override;
    procedure Clear;
    function KeyValuesCount : integer;
    function GetKeyStringValue(const aIndex: integer): String;
    function GetKeyValue(const aIndex: integer): TmKeyValue;
    function Terminal : boolean;
    procedure SortAscending;
    procedure SortDescending;

    procedure AddValueIfMissing(const aKeyValue: String; const aActualValue : Variant);
    function GetSubIndexOfKey(const aKeyValue: String) : TmKeysIndex;
    function GetSubIndex(const aIndex : integer): TmKeysIndex;

    property Level : integer read FLevel;
  end;

  { TKeyValuesForGroupByDef }

  TKeyValuesForGroupByDef = class
  strict private
    FValues : TStringList;
    FDictionary : TmStringDictionary;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddValueIfMissing (aValue : String);
    function Count : integer;
    function GetValue(const aIndex: integer) : String;
  end;

  { TKeyValuesForGroupByDefs }

  TKeyValuesForGroupByDefs = class
  strict private
    FList : TObjectList;
    function GetValue(Index: Integer): TKeyValuesForGroupByDef;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function Get(const aIndex : integer): TKeyValuesForGroupByDef;
    function Add : TKeyValuesForGroupByDef;
    procedure Clear;

    property Values[Index: Integer]: TKeyValuesForGroupByDef read GetValue; default;
  end;

  { TRecordCoordinates }

  TRecordCoordinates = class
  strict private
    FCoordinates : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Values : TStringList read FCoordinates;
  end;

  { TRecordsCoordinates }

  TRecordsCoordinates = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Get (const aIndex : integer): TRecordCoordinates;
    function Add : TRecordCoordinates;
  end;

  TmPivoterOption = (poHorizontalGrandTotal, poVerticalGrandTotal);
  TmPivoterOptions = set of TmPivoterOption;

const
  pOptionsDef = [];

type
  { TmPivoter }

  TmPivoter = class
  strict private
    FDataProvider : IVDDataProvider;
    // group by definitions
    FVerticalGroupByDefs : TmGroupByDefs;
    FHorizontalGroupByDefs : TmGroupByDefs;
    // Options
    FOptions : TmPivoterOptions;
    FEnableSort : boolean;
    // values of keys of group-by sets
    FVerticalValues: TKeyValuesForGroupByDefs;
    FHorizontalValues: TKeyValuesForGroupByDefs;
    // coordinates of every record of dataset
    FRecordCoordinates : TmStringDictionary;
    // indexes of keys of vertical and horizontal group-by sets
    FVerticalKeysIndex : TmKeysIndex;
    FHorizontalKeysIndex : TmKeysIndex;
    // definitions of pivot values
    FSummaryDefinitions : TmSummaryDefinitions;
    FValues : TmStringDictionary;
    FHorizontalGrandTotals : TmStringDictionary;
    FVerticalGrandTotals : TmStringDictionary;
    FSuperGrandTotal : TmSummaryValues;

    function GetIndexKeyValue(const aValue : Variant; out aActualValue : Variant; const aGroupByDef : TmGroupByDef): string;
    procedure DoSortIndex (aIndex : TmKeysIndex; const aGroupByDefs : TmGroupByDefs);
    procedure InternalCalculate(const aOnlyHierarchy : boolean);
  strict private
    const KEY_SEPARATOR = '^~';
  public
    const EMPTY_STRING_VALUE = '     #@#@#@#$$$$$';
  public
    constructor Create;
    destructor Destroy; override;

    procedure CalculateHierarchy;
    procedure Calculate;
    procedure Clear(const aClearSettings : boolean);

    // internal or test functions
    function GetRecords (const aVerticalKeys, aHorizontalKeys : TStringList): TCardinalList; overload;
    function GetRecords (const aVerticalKeys, aHorizontalKeys : string): TCardinalList; overload;
    function GetValue(const aVerticalKeys, aHorizontalKeys : TStringList; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue; overload;
    function GetValue(const aVerticalKeys, aHorizontalKeys : string; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue; overload;
    function GetHorizontalGrandTotalValue(const aVerticalKeys : TStringList; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue; overload;
    function GetHorizontalGrandTotalValue(const aVerticalKeys : string; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue; overload;
    function GetVerticalGrandTotalValue(const aHorizontalKeys : TStringList; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue; overload;
    function GetVerticalGrandTotalValue(const aHorizontalKeys : string; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue; overload;

    class function BuildKey(const aOldKey, aNewKeyPartValue : String) : String;
    class function StringListToKey(const aKeys : TStringList): String;

    // definitions
    property DataProvider : IVDDataProvider read FDataProvider write FDataProvider;
    property VerticalGroupByDefs : TmGroupByDefs read FVerticalGroupByDefs;
    property HorizontalGroupByDefs : TmGroupByDefs read FHorizontalGroupByDefs;
    property SummaryDefinitions : TmSummaryDefinitions read FSummaryDefinitions;
    // options
    property Options : TmPivoterOptions read FOptions write FOptions default pOptionsDef;
    property EnableSort : boolean read FEnableSort write FEnableSort;

    // output values
    property VerticalValues: TKeyValuesForGroupByDefs read FVerticalValues; // the collection of all key values for any vertical level
    property HorizontalValues: TKeyValuesForGroupByDefs read FHorizontalValues; // the collection of all key values for any vertical level
    property VerticalKeysIndex : TmKeysIndex read FVerticalKeysIndex;
    property HorizontalKeysIndex : TmKeysIndex read FHorizontalKeysIndex;
    property SuperGrandTotal : TmSummaryValues read FSuperGrandTotal;
  end;

  function TmGroupByOperationKindToString(const aValue: TmGroupByOperationKind) : String;
  function TmSortByConditionToString(const aValue: TmSortByCondition) : String;

implementation

uses
  sysutils, dateutils,
  mDateTimeUtility, mUtility;

function TmGroupByOperationKindToString(const aValue: TmGroupByOperationKind): String;
begin
  Result := 'unknown';
  if aValue = gpoDistinct then
    Result := 'distinct'
  else if aValue = gpoDateYear then
    Result := 'year'
  else if aValue = gpoDateMonth then
    Result := 'month'
  else if aValue = gpoDateTheMonth then
    Result := 'year + month'
  else if aValue = gpoDateDay then
    Result := 'day'
  else if aValue = gpoDateTheDay then
    Result := 'year + month + day'
  else if aValue = gpoDateQuarter then
    Result := 'quarter'
  else if aValue = gpoDateTheQuarter then
    Result := ' year + quarter'
  else if aValue = gpoFirstLetter then
    Result := 'first letter'
  else if aValue = goFormula then
    Result := 'formula';
end;

function TmSortByConditionToString(const aValue: TmSortByCondition): String;
begin
  if aValue = stAscending then
    Result := 'ascending'
  else if aValue = stDescending then
    Result := 'descending'
  else
    Result := 'none';
end;

{ TmKeyValue }

constructor TmKeyValue.Create(const aKeyValueAsString: String; const aKeyActualValue: Variant);
begin
  FKeyValueAsString:= aKeyValueAsString;
  FKeyActualValue:= aKeyActualValue;
end;

destructor TmKeyValue.Destroy;
begin
  inherited Destroy;
end;

{ TmKeysIndex }

function TmKeysIndex.OnCompareAscending(Item1: Pointer; Item2: Pointer): Integer;
var
  d1, d2 : TmKeyValue;
begin

  d1 := TmKeyValue(Item1);
  d2 := TmKeyValue(Item2);

  Result := CompareVariants(d1.KeyActualValue, d2.KeyActualValue);
end;

function TmKeysIndex.OnCompareDescending(Item1: Pointer; Item2: Pointer): Integer;
var
  d1, d2 : TmKeyValue;
begin

  d1 := TmKeyValue(Item1);
  d2 := TmKeyValue(Item2);

  Result := CompareVariants(d1.KeyActualValue, d2.KeyActualValue);
  if Result <> 0 then
    Result := -1 * Result;
end;

function TmKeysIndex.AddSubIndexForKey(const aKeyValue: String): TmKeysIndex;
begin
  Result := FSubIndexes.Find(aKeyValue) as TmKeysIndex;
  if not Assigned(Result) then
  begin
    Result := TmKeysIndex.Create(Self.FLevel + 1);
    FSubIndexes.Add(aKeyValue, Result);
  end;
end;

constructor TmKeysIndex.Create(const aLevel : integer);
begin
  FKeyValues := TFPList.Create;
  FKeyValuesDictionary := TmStringDictionary.Create(false);
  FSubIndexes := TmStringDictionary.Create(true);
  FGarbage := TObjectList.Create(true);
  FLevel := aLevel;
end;

destructor TmKeysIndex.Destroy;
begin
  FKeyValues.Free;
  FKeyValuesDictionary.Free;
  FSubIndexes.Free;
  FGarbage.Free;
  inherited Destroy;
end;

procedure TmKeysIndex.Clear;
begin
  FKeyValues.Clear;
  FKeyValuesDictionary.Clear;
  FSubIndexes.Clear;
  FGarbage.Clear;
end;

function TmKeysIndex.KeyValuesCount: integer;
begin
  Result := FKeyValues.Count;
end;

function TmKeysIndex.GetKeyStringValue(const aIndex: integer): String;
begin
  Result := TmKeyValue(FKeyValues.Items[aIndex]).KeyValueAsString;
end;

function TmKeysIndex.GetKeyValue(const aIndex: integer): TmKeyValue;
begin
  Result := TmKeyValue(FKeyValues.Items[aIndex]);
end;

function TmKeysIndex.Terminal: boolean;
begin
  Result := (FSubIndexes.Count = 0);
end;

procedure TmKeysIndex.SortAscending;
begin
  mUtility.MergeSort(FKeyValues, OnCompareAscending());
end;

procedure TmKeysIndex.SortDescending;
begin
  mUtility.MergeSort(FKeyValues, OnCompareDescending());
end;

procedure TmKeysIndex.AddValueIfMissing(const aKeyValue: String; const aActualValue : Variant);
var
  tmp : TmKeyValue;
begin
  if not FKeyValuesDictionary.Contains(aKeyValue) then
  begin
    tmp := TmKeyValue.Create(aKeyValue, aActualValue);
    FKeyValues.Add(tmp);
    FGarbage.Add(tmp);
    FKeyValuesDictionary.Add(aKeyValue, FKeyValuesDictionary);
  end;
end;

function TmKeysIndex.GetSubIndexOfKey(const aKeyValue: String): TmKeysIndex;
begin
  Result := FSubIndexes.Find(aKeyValue) as TmKeysIndex;
end;

function TmKeysIndex.GetSubIndex(const aIndex: integer): TmKeysIndex;
begin
  Result := GetSubIndexOfKey(Self.GetKeyStringValue(aIndex));
end;

{ TKeyValuesForGroupByDefs }

function TKeyValuesForGroupByDefs.GetValue(Index: Integer): TKeyValuesForGroupByDef;
begin
  Result := FList.Items[Index] as TKeyValuesForGroupByDef;
end;

constructor TKeyValuesForGroupByDefs.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TKeyValuesForGroupByDefs.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TKeyValuesForGroupByDefs.Count: integer;
begin
  Result := FList.Count;
end;

function TKeyValuesForGroupByDefs.Get(const aIndex: integer): TKeyValuesForGroupByDef;
begin
  Result := FList.Items[aIndex] as TKeyValuesForGroupByDef;
end;

function TKeyValuesForGroupByDefs.Add: TKeyValuesForGroupByDef;
begin
  Result := TKeyValuesForGroupByDef.Create;
  FList.Add(Result);
end;

procedure TKeyValuesForGroupByDefs.Clear;
begin
  FList.Clear;
end;

{ TmPivoter }

function TmPivoter.GetIndexKeyValue(const aValue: Variant; out aActualValue : Variant; const aGroupByDef: TmGroupByDef): string;
  function TryToConvertToDate (aSource : Variant) : TDateTime;
  begin
    try
      Result := aSource;
    except
      Result := 0;
    end;
  end;

var
  y, m, d : word;
  tmpInt : Integer;
  tmpDouble : double;
begin
  aActualValue:= Null;

  case aGroupByDef.OperationKind of
    gpoDistinct:
    begin
      Result := VarToStr(aValue);
      aActualValue:= aValue;
    end;
    gpoDateYear:
    begin
      tmpInt := YearOf(TryToConvertToDate(aValue));
      aActualValue:= tmpInt;
      Result := IntToStr(tmpInt);
    end;
    gpoDateMonth:
    begin
      tmpInt := MonthOf(TryToConvertToDate(aValue));
      aActualValue:= tmpInt;
      Result := IntToStr(tmpInt);
    end;
    gpoDateTheMonth:
    begin
      DecodeDate(TryToConvertToDate(aValue), y, m, d);
      aActualValue:= EncodeDate(y, m, 1);
      Result := IntToStr(y) + '-' + IntToStr(m);
    end;
    gpoDateDay:
    begin
      tmpInt := DayOf(TryToConvertToDate(aValue));
      aActualValue:= tmpInt;
      Result := IntToStr(tmpInt);
    end;
    gpoDateTheDay:
    begin
      DecodeDate(TryToConvertToDate(aValue), y, m, d);
      aActualValue:= aValue;
      Result := IntToStr(y) + '-' + IntToStr(m) + '-' + IntToStr(d);
    end;
    gpoDateQuarter:
    begin
      tmpInt := ((MonthOf(TryToConvertToDate(aValue)) - 1) div 3) + 1;
      aActualValue:= tmpInt;
      Result := IntToStr(tmpInt);
    end;
    gpoDateTheQuarter:
    begin
      DecodeDate(TryToConvertToDate(aValue), y, m, d);
      tmpInt := ((m - 1) div 3) + 1;
      Result := IntToStr(y) + '-' + IntToStr(tmpInt);
      aActualValue:= EncodeDate(y, 1+ ((tmpInt -1) * 3), 1);
    end;
    gpoFirstLetter:
      begin
        Result := VarToStr(aValue);
        if Length(Result) > 0 then
          Result := Copy(Result,1,1);
        aActualValue:= Result;
      end;
    else
      raise Exception.Create('[TmPivoter.GetIndexKeyValue] Unknown operation kind');
  end;
end;

procedure TmPivoter.DoSortIndex(aIndex: TmKeysIndex; const aGroupByDefs: TmGroupByDefs);
var
  i : integer;
begin
  if not Assigned(aIndex) then
    exit;

  if aGroupByDefs.Count <= aIndex.Level then
    exit;

  if aGroupByDefs.Get(aIndex.Level).SortBy = stAscending then
    aIndex.SortAscending
  else
  if aGroupByDefs.Get(aIndex.Level).SortBy = stDescending then
    aIndex.SortDescending;

  for i := 0 to aIndex.KeyValuesCount - 1 do
    DoSortIndex(aIndex.GetSubIndex(i), aGroupByDefs);
end;

constructor TmPivoter.Create;
begin
  FDataProvider := nil;
  FVerticalGroupByDefs := TmGroupByDefs.Create;
  FHorizontalGroupByDefs := TmGroupByDefs.Create;
  FVerticalValues := TKeyValuesForGroupByDefs.Create;
  FHorizontalValues := TKeyValuesForGroupByDefs.Create;
  FRecordCoordinates := TmStringDictionary.Create(true);
  FValues := TmStringDictionary.Create(true);
  FHorizontalGrandTotals := TmStringDictionary.Create(true);
  FVerticalGrandTotals := TmStringDictionary.Create(true);
  FVerticalKeysIndex := TmKeysIndex.Create(0);
  FHorizontalKeysIndex := TmKeysIndex.Create(0);
  FSummaryDefinitions := TmSummaryDefinitions.Create;
  FSuperGrandTotal := TmSummaryValues.Create;
  FEnableSort:= false;
end;

destructor TmPivoter.Destroy;
begin
  FVerticalGroupByDefs.Free;
  FHorizontalGroupByDefs.Free;
  FVerticalValues.Free;
  FHorizontalValues.Free;
  FRecordCoordinates.Free;
  FValues.Free;
  FHorizontalGrandTotals.Free;
  FVerticalGrandTotals.Free;
  FVerticalKeysIndex.Free;
  FHorizontalKeysIndex.Free;
  FSummaryDefinitions.Free;
  FSuperGrandTotal.Free;

  inherited Destroy;
end;

procedure TmPivoter.InternalCalculate(const aOnlyHierarchy : boolean);
var
  i, k, z : integer;
  currentCoord, tmpKeyValue, parentKeyValue, currentVertCoord, currentHorizCoord : string;
  tmpIndex : TmKeysIndex;
  tmpValue : Variant;
  tmpList : TCardinalList;
  tmpValues : TmSummaryValues;
  summaryValue : TmSummaryValue;
  tmpActualValue : variant;
  tmpGrandTotalValues : TmSummaryValues;
begin
  Self.Clear(false);

  for i := 0 to FVerticalGroupByDefs.Count - 1 do
    FVerticalValues.Add;
  for i := 0 to FHorizontalGroupByDefs.Count -1 do
    FHorizontalValues.Add;


  for i:= 0 to FDataProvider.Count -1 do
  begin
    currentCoord := '';
    currentVertCoord:= '';
    tmpActualValue:= Null;
    tmpIndex := FVerticalKeysIndex;
    parentKeyValue := EMPTY_STRING_VALUE;
    for k := 0 to FVerticalGroupByDefs.Count -1 do
    begin
      tmpValue := FDataProvider.GetDatum(i).GetPropertyByFieldName(FVerticalGroupByDefs.Get(k).FieldName);
      // tmpKeyValue is the calculate value based on tmpValue which is the actual value
      // GetIndexKeyValue apply the GroupByOperator of the GroupByDef to the actual value
      tmpKeyValue := GetIndexKeyValue(tmpValue, tmpActualValue, FVerticalGroupByDefs.Get(k));
      FVerticalValues.Get(k).AddValueIfMissing(tmpKeyValue);
      if parentKeyValue <> EMPTY_STRING_VALUE then
        tmpIndex := tmpIndex.AddSubIndexForKey(parentKeyValue);
      parentKeyValue := tmpKeyValue;
      tmpIndex.AddValueIfMissing(tmpKeyValue, tmpActualValue);

      currentCoord := BuildKey(currentCoord, tmpKeyValue);
      if poHorizontalGrandTotal in FOptions then
        currentVertCoord:= currentCoord;
    end;

    currentHorizCoord:= '';
    tmpIndex := FHorizontalKeysIndex;
    parentKeyValue := EMPTY_STRING_VALUE;
    for k := 0 to FHorizontalGroupByDefs.Count -1 do
    begin
      tmpValue := FDataProvider.GetDatum(i).GetPropertyByFieldName(FHorizontalGroupByDefs.Get(k).FieldName);
      tmpKeyValue := GetIndexKeyValue(tmpValue, tmpActualValue, FHorizontalGroupByDefs.Get(k));
      FHorizontalValues.Get(k).AddValueIfMissing(tmpKeyValue);
      if parentKeyValue <> EMPTY_STRING_VALUE then
        tmpIndex := tmpIndex.AddSubIndexForKey(parentKeyValue);
      parentKeyValue := tmpKeyValue;
      tmpIndex.AddValueIfMissing(tmpKeyValue, tmpActualValue);

      currentCoord := BuildKey(currentCoord, tmpKeyValue);
      if poVerticalGrandTotal in FOptions then
        currentHorizCoord:= BuildKey(currentHorizCoord, tmpKeyValue);
    end;

    tmpList := FRecordCoordinates.Find(currentCoord) as TCardinalList;
    if not Assigned(tmpList) then
    begin
      tmpList := TCardinalList.Create;
      FRecordCoordinates.Add(currentCoord, tmpList);
    end;
    tmpList.Add(i);
    if not aOnlyHierarchy then
    begin
      tmpValues := FValues.Find(currentCoord) as TmSummaryValues;
      if not Assigned(tmpValues) then
      begin
        tmpValues := TmSummaryValues.Create;
        FValues.Add(currentCoord, tmpValues);
      end;
      for z := 0 to FSummaryDefinitions.Count - 1 do
      begin
        summaryValue := tmpValues.FindByDefinition(FSummaryDefinitions.Get(z));
        if not Assigned(summaryValue) then
          summaryValue := tmpValues.AddValue(FSummaryDefinitions.Get(z), false);
        summaryValue.ComputeValueInSummaries(FDataProvider.GetDatum(i).GetPropertyByFieldName(FSummaryDefinitions.Get(z).FieldName));
      end;

      if poHorizontalGrandTotal in FOptions then
      begin
        tmpGrandTotalValues := FHorizontalGrandTotals.Find(currentVertCoord) as TmSummaryValues;
        if not Assigned(tmpGrandTotalValues) then
        begin
          tmpGrandTotalValues := TmSummaryValues.Create;
          FHorizontalGrandTotals.Add(currentVertCoord, tmpGrandTotalValues);
        end;
        for z := 0 to FSummaryDefinitions.Count - 1 do
        begin
          summaryValue := tmpGrandTotalValues.FindByDefinition(FSummaryDefinitions.Get(z));
          if not Assigned(summaryValue) then
            summaryValue := tmpGrandTotalValues.AddValue(FSummaryDefinitions.Get(z), false);
          summaryValue.ComputeValueInSummaries(FDataProvider.GetDatum(i).GetPropertyByFieldName(FSummaryDefinitions.Get(z).FieldName));

          summaryValue := FSuperGrandTotal.FindByDefinition(FSummaryDefinitions.Get(z));
          if not Assigned(summaryValue) then
            summaryValue := FSuperGrandTotal.AddValue(FSummaryDefinitions.Get(z), false);
          summaryValue.ComputeValueInSummaries(FDataProvider.GetDatum(i).GetPropertyByFieldName(FSummaryDefinitions.Get(z).FieldName));
        end;
      end;

      if poVerticalGrandTotal in FOptions then
      begin
        tmpGrandTotalValues := FVerticalGrandTotals.Find(currentHorizCoord) as TmSummaryValues;
        if not Assigned(tmpGrandTotalValues) then
        begin
          tmpGrandTotalValues := TmSummaryValues.Create;
          FVerticalGrandTotals.Add(currentHorizCoord, tmpGrandTotalValues);
        end;
        for z := 0 to FSummaryDefinitions.Count - 1 do
        begin
          summaryValue := tmpGrandTotalValues.FindByDefinition(FSummaryDefinitions.Get(z));
          if not Assigned(summaryValue) then
            summaryValue := tmpGrandTotalValues.AddValue(FSummaryDefinitions.Get(z), false);
          summaryValue.ComputeValueInSummaries(FDataProvider.GetDatum(i).GetPropertyByFieldName(FSummaryDefinitions.Get(z).FieldName));

          summaryValue := FSuperGrandTotal.FindByDefinition(FSummaryDefinitions.Get(z));
          if not Assigned(summaryValue) then
            summaryValue := FSuperGrandTotal.AddValue(FSummaryDefinitions.Get(z), false);
          summaryValue.ComputeValueInSummaries(FDataProvider.GetDatum(i).GetPropertyByFieldName(FSummaryDefinitions.Get(z).FieldName));
        end;
      end;
    end;
  end;

  if FEnableSort then
  begin
    DoSortIndex(FVerticalKeysIndex, FVerticalGroupByDefs);
    DoSortIndex(FHorizontalKeysIndex, FHorizontalGroupByDefs);
  end;
end;

procedure TmPivoter.CalculateHierarchy;
begin
  InternalCalculate(true);
end;

procedure TmPivoter.Calculate;
begin
  InternalCalculate(false);
end;

procedure TmPivoter.Clear(const aClearSettings : boolean);
begin
  FVerticalValues.Clear;
  FHorizontalValues.Clear;
  FVerticalKeysIndex.Clear;
  FHorizontalKeysIndex.Clear;

  FRecordCoordinates.Clear;
  FValues.Clear;
  FHorizontalGrandTotals.Clear;
  FVerticalGrandTotals.Clear;
  FSuperGrandTotal.Clear;

  if aClearSettings then
  begin
    FVerticalGroupByDefs.Clear;
    FHorizontalGroupByDefs.Clear;
    FSummaryDefinitions.Clear;
    FOptions:= pOptionsDef;
  end;
end;

function TmPivoter.GetRecords(const aVerticalKeys, aHorizontalKeys: TStringList): TCardinalList;
var
  tmpHor, tmpVer : String;
begin
  tmpHor := StringListToKey(aHorizontalKeys);
  tmpVer := StringListToKey(aVerticalKeys);

  Result := GetRecords(tmpVer, tmpHor);
end;

function TmPivoter.GetRecords(const aVerticalKeys, aHorizontalKeys: string): TCardinalList;
begin
  Result := FRecordCoordinates.Find(aVerticalKeys + aHorizontalKeys) as TCardinalList;
end;

function TmPivoter.GetValue(const aVerticalKeys, aHorizontalKeys : TStringList; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue;
var
  tmpHor, tmpVer : String;
begin
  tmpHor := StringListToKey(aHorizontalKeys);
  tmpVer := StringListToKey(aVerticalKeys);
  Result := GetValue(tmpVer, tmpHor, aSummaryDefinition);
end;

function TmPivoter.GetValue(const aVerticalKeys, aHorizontalKeys: string; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue;
var
  tmpValues : TmSummaryValues;
begin
  Result := nil;

  tmpValues := FValues.Find(aVerticalKeys + aHorizontalKeys) as TmSummaryValues;
  if Assigned(tmpValues) then
    Result := tmpValues.FindByDefinition(aSummaryDefinition);
end;

function TmPivoter.GetHorizontalGrandTotalValue(const aVerticalKeys: TStringList; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue;
var
  tmpVer : String;
begin
  tmpVer := StringListToKey(aVerticalKeys);
  Result := GetHorizontalGrandTotalValue(tmpVer, aSummaryDefinition);
end;

function TmPivoter.GetHorizontalGrandTotalValue(const aVerticalKeys: string; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue;
var
  tmpValues : TmSummaryValues;
begin
  Result := nil;

  tmpValues := FHorizontalGrandTotals.Find(aVerticalKeys) as TmSummaryValues;
  if Assigned(tmpValues) then
    Result := tmpValues.FindByDefinition(aSummaryDefinition);
end;

function TmPivoter.GetVerticalGrandTotalValue(const aHorizontalKeys: TStringList; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue;
var
  tmpHor : String;
begin
  tmpHor := StringListToKey(aHorizontalKeys);
  Result := GetVerticalGrandTotalValue(tmpHor, aSummaryDefinition);
end;

function TmPivoter.GetVerticalGrandTotalValue(const aHorizontalKeys: string; const aSummaryDefinition: TmSummaryDefinition): TmSummaryValue;
var
  tmpValues : TmSummaryValues;
begin
  Result := nil;

  tmpValues := FVerticalGrandTotals.Find(aHorizontalKeys) as TmSummaryValues;
  if Assigned(tmpValues) then
    Result := tmpValues.FindByDefinition(aSummaryDefinition);
end;

class function TmPivoter.BuildKey(const aOldKey, aNewKeyPartValue: String): String;
begin
  Result := aOldKey + aNewKeyPartValue + KEY_SEPARATOR;
end;

class function TmPivoter.StringListToKey(const aKeys: TStringList): String;
var
  i : integer;
begin
  Result := '';
  if Assigned(aKeys) then
  begin
    for i := 0 to aKeys.Count - 1 do
      Result := BuildKey(Result, aKeys.Strings[i]);
  end;
end;

{ TRecordsCoordinates }

constructor TRecordsCoordinates.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TRecordsCoordinates.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TRecordsCoordinates.Clear;
begin
  FList.Clear;
end;

function TRecordsCoordinates.Add: TRecordCoordinates;
begin
  Result := TRecordCoordinates.Create;
  FList.Add(Result);
end;

function TRecordsCoordinates.Get(const aIndex: integer): TRecordCoordinates;
begin
  Result := FList.Items[aIndex] as TRecordCoordinates;
end;

{ TRecordCoordinates }

constructor TRecordCoordinates.Create;
begin
  FCoordinates := TStringList.Create;
end;

destructor TRecordCoordinates.Destroy;
begin
  FCoordinates.Free;
  inherited Destroy;
end;

procedure TRecordCoordinates.Clear;
begin
  FCoordinates.Clear;
end;

{ TKeyValuesForGroupByDef }

constructor TKeyValuesForGroupByDef.Create;
begin
  FValues:= TStringList.Create;
  FDictionary:= TmStringDictionary.Create(false);
end;

destructor TKeyValuesForGroupByDef.Destroy;
begin
  FValues.Free;
  FDictionary.Free;
  inherited Destroy;
end;

procedure TKeyValuesForGroupByDef.AddValueIfMissing(aValue: String);
begin
  if not FDictionary.Contains(aValue) then
  begin
    FValues.Add(aValue);
    FDictionary.Add(aValue, FDictionary);
  end;
end;

function TKeyValuesForGroupByDef.Count: integer;
begin
  Result := FValues.Count;
end;

function TKeyValuesForGroupByDef.GetValue(const aIndex: integer): String;
begin
  Result := FValues.Strings[aIndex];
end;

{ TmGroupByDefs }

constructor TmGroupByDefs.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmGroupByDefs.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TmGroupByDefs.Assign(const aSource: TmGroupByDefs);
var
  i : integer;
begin
  Self.Clear;
  for i := 0 to aSource.Count - 1 do
    Self.Add.Assign(aSource.Get(i));
end;

function TmGroupByDefs.Add: TmGroupByDef;
begin
  Result := TmGroupByDef.Create;
  FList.Add(Result);
end;

function TmGroupByDefs.Count: integer;
begin
  Result := FList.Count;
end;

function TmGroupByDefs.Get(const aIndex: integer): TmGroupByDef;
begin
  Result := FList.Items[aIndex] as TmGroupByDef;
end;

procedure TmGroupByDefs.Clear;
begin
  FList.Clear;
end;

{ TmGroupByDef }

constructor TmGroupByDef.Create;
begin
  FFieldName:= '';
  FDataType:= ftString;
  FOperationKind:= gpoDistinct;
  FFormula := TNullableString.Create;
  FDisplayLabel := TNullableString.Create;
  FDisplayFormat := TNullableString.Create;
  FSortBy := stNone;
end;

destructor TmGroupByDef.Destroy;
begin
  FFormula.Free;
  FDisplayLabel.Free;
  FDisplayFormat.Free;
  inherited Destroy;
end;

procedure TmGroupByDef.Assign(const aSource: TmGroupByDef);
begin
  FFieldName:= aSource.FieldName;
  FDataType := aSource.DataType;
  FOperationKind := aSource.OperationKind;
  FFormula.Assign(aSource.Formula);
  FDisplayLabel.Assign(aSource.DisplayLabel);
  FDisplayFormat.Assign(aSource.DisplayFormat);
  FSortBy:= aSource.SortBy;
end;

class function TmGroupByDef.CheckOperationKindCompatibility(const aOperationKind: TmGroupByOperationKind; const aDataType: TFieldType): boolean;
begin
  if (aOperationKind =  gpoDateDay) or (aOperationKind = gpoDateYear) or (aOperationKind =  gpoDateMonth) or (aOperationKind = gpoDateTheMonth) or (aOperationKind = gpoDateDay) or (aOperationKind = gpoDateTheDay) or (aOperationKind = gpoDateQuarter) or (aOperationKind = gpoDateTheQuarter) then
    Result := aDataType in [ftDate, ftDateTime, ftTimeStamp, ftTime]
  else
    Result := true;
//  gpoDistinct, gpoDateYear, gpoDateMonth, gpoDateTheMonth, gpoDateDay, gpoDateTheDay, gpoDateQuarter, gpoDateTheQuarter, gpoFirstLetter, goFormula
end;

function TmGroupByDef.IsFloat: boolean;
begin
  Result := (FOperationKind in [gpoDistinct, goFormula]) and FieldTypeIsFloat(FDataType);
end;

function TmGroupByDef.IsInteger: boolean;
begin
  Result := (FOperationKind in [gpoDateYear, gpoDateMonth, gpoDateDay, gpoDateQuarter]) or
    ((FOperationKind in [gpoDistinct, goFormula]) and (FieldTypeIsInteger(FDataType)));
end;

function TmGroupByDef.IsString: boolean;
begin
  Result := ((FOperationKind in [gpoDistinct, goFormula]) and FieldTypeIsString(FDataType)) or
    (gpoDistinct = gpoFirstLetter);
end;

function TmGroupByDef.IsDate: boolean;
begin
  Result := (FOperationKind in [gpoDateTheMonth, gpoDateTheDay, gpoDateTheQuarter]) or
    ((FOperationKind in [gpoDistinct, goFormula]) and ((FieldTypeIsDate(FDataType) or (FieldTypeIsDateTime(FDataType)))))
end;

function TmGroupByDef.FormatValue(const aKeyValue : TmKeyValue): String;
begin
  Result := '';

  if Self.IsString then
    Result := aKeyValue.KeyValueAsString
  else if Self.IsInteger or Self.IsFloat then
  begin
    if FDisplayFormat.NotNull then
    begin
      if VarIsNull(aKeyValue.KeyActualValue) then
        Result := ''
      else
        Result := FormatFloat(FDisplayFormat.AsString, aKeyValue.KeyActualValue);
    end
    else
      Result := aKeyValue.KeyValueAsString;
  end else if Self.IsDate then
  begin
    if FDisplayFormat.NotNull then
    begin
      if VarIsNull(aKeyValue.KeyActualValue) then
        Result := ''
      else
        Result := ExtFormatDateTime(FDisplayFormat.AsString, aKeyValue.KeyActualValue);
    end
    else
      Result := aKeyValue.KeyValueAsString;
  end;
end;

end.
