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

interface

uses
  contnrs, db, variants, Classes,
  mDataProviderInterfaces,
  mIntList, mMaps;

type

  TmGroupByOperationKind = (gpoDistinct, gpoDateYear, gpoDateMonth, gpoDateDay, gpoFirstLetter, goFormula);

  { TmGroupByDef }

  TmGroupByDef = class
  strict private
    FFieldName: string;
    FDataType : TFieldType;
    FOperationKind : TmGroupByOperationKind;
    FFormula : String;
  public
    constructor Create;

    property FieldName : string read FFieldName write FFieldName;
    property DataType : TFieldType read FDataType write FDataType;
    property OperationKind : TmGroupByOperationKind read FOperationKind write FOperationKind;
    property Formula: String read FFormula write FFormula;
  end;

  { TmGroupByDefs }

  TmGroupByDefs = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Add : TmGroupByDef;
    function Count : integer;
    function Get(const aIndex : integer): TmGroupByDef;
  end;

  TmCalculationKind = (ckSum, ckCount, ckMin, ckMax, ckFormula);

  { TmCalculationDef }

  TmCalculationDef = class
  strict private
    FFieldName : String;
    FDataType : TFieldType;
    FCalculationKind : TmCalculationKind;
    FFormula : String;
  public
    constructor Create;

    property FieldName : string read FFieldName write FFieldName;
    property DataType : TFieldType read FDataType write FDataType;
    property CalculationKind : TmCalculationKind read FCalculationKind write FCalculationKind;
    property Formula : String read FFormula write FFormula;
  end;

  { TmCalculationDefs }

  TmCalculationDefs = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Add : TmCalculationDef;
    function Count : integer;
    function Get(const aIndex : integer): TmCalculationDef;
  end;

  TmValuesList = class
  strict private
  public
  end;


  TmKeysIndex = class
  strict private
    FKeysDictionary : TmStringDictionary;
    FKeysValues : TStringList;

    FLevel : integer;
    FParent : TmKeysIndex;
  private
    procedure Clear;
    function GetSubIndex (const aKey : string): TmKeysIndex;
    function GetValueList (const aKey : string): TCardinalList;
  public
    constructor Create;
    destructor Destroy; override;

    property KeysValues : TStringList read FKeysValues;
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


  { TmPivoter }

  TmPivoter = class
  strict private
    FDataProvider : IVDDataProvider;
    // group by definitions
    FVerticalGroupByDefs : TmGroupByDefs;
    FHorizontalGroupByDefs : TmGroupByDefs;
    // values of keys of group-by sets
    FVerticalValues: TKeyValuesForGroupByDefs;
    FHorizontalValues: TKeyValuesForGroupByDefs;
    // coordinates of every record of dataset
    FRecordCoordinates : TmStringDictionary;
    // indexes of keys of vertical and horizontal group-by sets
    FVerticalKeysIndex : TmKeysIndex;
    FHorizontalKeysIndex : TmKeysIndex;
    function GetIndexKeyValue(aValue : Variant; aGroupByDef : TmGroupByDef): string;
  strict private
    const KEY_SEPARATOR = '^~';
  public
    constructor Create;
    destructor Destroy; override;

    procedure CalculateHierarchy;
    function GetRecords (const aVerticalKeys, aHorizontalKeys : TStringList): TCardinalList; overload;
    function GetRecords (const aVerticalKeys, aHorizontalKeys : string): TCardinalList; overload;

    function BuildKey(const aOldKey, aNewKeyPartValue : String) : String;

    property DataProvider : IVDDataProvider read FDataProvider write FDataProvider;
    property VerticalGroupByDefs : TmGroupByDefs read FVerticalGroupByDefs;
    property HorizontalGroupByDefs : TmGroupByDefs read FHorizontalGroupByDefs;

    property VerticalValues: TKeyValuesForGroupByDefs read FVerticalValues;
    property HorizontalValues: TKeyValuesForGroupByDefs read FHorizontalValues;


  end;



implementation

uses
  sysutils, dateutils;

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

function TmPivoter.GetIndexKeyValue(aValue: Variant; aGroupByDef: TmGroupByDef): string;
  function TryToConvertToDate (aSource : Variant) : TDateTime;
  begin
    try
      Result := aSource;
    except
      Result := 0;
    end;
  end;

begin
  case aGroupByDef.OperationKind of
    gpoDistinct:
      Result := VarToStr(aValue);
    gpoDateYear:
      Result := IntToStr(YearOf(TryToConvertToDate(aValue)));
    gpoDateMonth:
      Result := IntToStr(MonthOf(TryToConvertToDate(aValue)));
    gpoDateDay:
      Result := IntToStr(DayOf(TryToConvertToDate(aValue)));
    gpoFirstLetter:
      begin
        Result := VarToStr(aValue);
        if Length(Result) > 0 then
          Result := Copy(Result,1,1);
      end;
    else
      raise Exception.Create('[TmPivoter.GetIndexKeyValue] Unknown operation kind');
  end;
end;

constructor TmPivoter.Create;
begin
  FDataProvider := nil;
  FVerticalGroupByDefs := TmGroupByDefs.Create;
  FHorizontalGroupByDefs := TmGroupByDefs.Create;
  FVerticalValues := TKeyValuesForGroupByDefs.Create;
  FHorizontalValues := TKeyValuesForGroupByDefs.Create;
  FRecordCoordinates := TmStringDictionary.Create(true);
  FVerticalKeysIndex := TmKeysIndex.Create;
  FHorizontalKeysIndex := TmKeysIndex.Create;
end;

destructor TmPivoter.Destroy;
begin
  FVerticalGroupByDefs.Free;
  FHorizontalGroupByDefs.Free;
  FVerticalValues.Free;
  FHorizontalValues.Free;
  FRecordCoordinates.Free;
  FVerticalKeysIndex.Free;
  FHorizontalKeysIndex.Free;

  inherited Destroy;
end;

procedure TmPivoter.CalculateHierarchy;
var
  i, k : integer;
  currentCoord, tmpKeyValue : string;
  tmpIndex : TmKeysIndex;
  tmpValue : Variant;
  tmpList : TCardinalList;
begin
  FVerticalValues.Clear;
  FHorizontalValues.Clear;
  FRecordCoordinates.Clear;

  for i := 0 to FVerticalGroupByDefs.Count - 1 do
    FVerticalValues.Add;
  for i := 0 to FHorizontalGroupByDefs.Count -1 do
    FHorizontalValues.Add;


  for i:= 0 to FDataProvider.Count -1 do
  begin
    currentCoord := '';
    tmpIndex := FVerticalKeysIndex;
    for k := 0 to FVerticalGroupByDefs.Count -1 do
    begin
      tmpValue := FDataProvider.GetDatum(i).GetPropertyByFieldName(FVerticalGroupByDefs.Get(k).FieldName);
      tmpKeyValue := GetIndexKeyValue(tmpValue, FVerticalGroupByDefs.Get(k));
      FVerticalValues.Get(k).AddValueIfMissing(tmpKeyValue);

      currentCoord := currentCoord + tmpKeyValue + KEY_SEPARATOR;
      if k < (FVerticalGroupByDefs.Count - 1) then
        tmpIndex := tmpIndex.GetSubIndex(tmpKeyValue)
      else
        tmpIndex.GetValueList(tmpKeyValue).Add(i);
    end;

    tmpIndex := FHorizontalKeysIndex;
    for k := 0 to FHorizontalGroupByDefs.Count -1 do
    begin
      tmpValue := FDataProvider.GetDatum(i).GetPropertyByFieldName(FHorizontalGroupByDefs.Get(k).FieldName);
      tmpKeyValue := GetIndexKeyValue(tmpValue, FHorizontalGroupByDefs.Get(k));
      FHorizontalValues.Get(k).AddValueIfMissing(tmpKeyValue);
      currentCoord := currentCoord + tmpKeyValue + KEY_SEPARATOR;
      if k < (FHorizontalGroupByDefs.Count - 1) then
        tmpIndex := tmpIndex.GetSubIndex(tmpKeyValue)
      else
        tmpIndex.GetValueList(tmpKeyValue).Add(i);
    end;

    tmpList := FRecordCoordinates.Find(currentCoord) as TCardinalList;
    if not Assigned(tmpList) then
    begin
      tmpList := TCardinalList.Create;
      FRecordCoordinates.Add(currentCoord, tmpList);
    end;
    tmpList.Add(i);
  end;

end;

function TmPivoter.GetRecords(const aVerticalKeys, aHorizontalKeys: TStringList): TCardinalList;
var
  tmpHor, tmpVer : String;
  i : integer;
begin
  tmpHor := '';
  if Assigned(aVerticalKeys) then
  begin
    for i := 0 to aVerticalKeys.Count - 1 do
      tmpHor := BuildKey(tmpHor, aVerticalKeys.Strings[i]);
      //tmp := tmp + aVerticalKeys.Strings[i] + KEY_SEPARATOR;
  end;
  tmpVer := '';
  if Assigned(aHorizontalKeys) then
  begin
    for i := 0 to aHorizontalKeys.Count - 1 do
      tmpVer := BuildKey(tmpVer, aHorizontalKeys.Strings[i]);
      //tmp := tmp + aHorizontalKeys.Strings[i] + KEY_SEPARATOR;
  end;

  Result := GetRecords(tmpVer, tmpHor); //FRecordCoordinates.Find(tmp) as TCardinalList;
end;

function TmPivoter.GetRecords(const aVerticalKeys, aHorizontalKeys: string): TCardinalList;
begin
  Result := FRecordCoordinates.Find(aVerticalKeys + aHorizontalKeys) as TCardinalList;
end;

function TmPivoter.BuildKey(const aOldKey, aNewKeyPartValue: String): String;
begin
  Result := aOldKey + aNewKeyPartValue + KEY_SEPARATOR;
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

{ TmIndex }

constructor TmKeysIndex.Create;
begin
  FKeysDictionary := TmStringDictionary.Create(true);
  FLevel := 0;
  FParent := nil;
  FKeysValues := TStringList.Create;
end;

destructor TmKeysIndex.Destroy;
begin
  FKeysDictionary.Free;
  FKeysValues.Free;
  inherited Destroy;
end;

procedure TmKeysIndex.Clear;
begin
  FKeysDictionary.Clear;
  FKeysValues.Clear;;
end;

function TmKeysIndex.GetSubIndex(const aKey: string): TmKeysIndex;
var
  tmpObj : TObject;
begin
  tmpObj := FKeysDictionary.Find(aKey);
  if Assigned(tmpObj) then
  begin
    assert(tmpObj is TmKeysIndex);
    Result := tmpObj as TmKeysIndex;
  end
  else
  begin
    Result := TmKeysIndex.Create;
    Result.FLevel:= Self.FLevel + 1;
    Result.FParent := Self;
    FKeysDictionary.Add(aKey, Result);
    FKeysValues.Add(aKey);
  end;
end;

function TmKeysIndex.GetValueList(const aKey: string): TCardinalList;
var
  tmpObj : TObject;
begin
  tmpObj := FKeysDictionary.Find(aKey);
  if Assigned(tmpObj) then
  begin
    assert(tmpObj is TCardinalList);
    Result := tmpObj as TCardinalList;
  end
  else
  begin
    Result := TCardinalList.Create;
    FKeysDictionary.Add(aKey, Result);
  end;
end;

{ TmCalculationDefs }

constructor TmCalculationDefs.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmCalculationDefs.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmCalculationDefs.Add: TmCalculationDef;
begin
  Result := TmCalculationDef.Create;
  FList.Add(Result);
end;

function TmCalculationDefs.Count: integer;
begin
  Result := FList.Count;
end;

function TmCalculationDefs.Get(const aIndex: integer): TmCalculationDef;
begin
  Result := FList.Items[aIndex] as TmCalculationDef;
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

{ TmGroupByDef }

constructor TmGroupByDef.Create;
begin
  FFieldName:= '';
  FDataType:= ftString;
  FOperationKind:= gpoDistinct;
  FFormula := '';
end;

{ TmCalculationDef }

constructor TmCalculationDef.Create;
begin
  FFieldName:= '';
  FDataType:= ftFloat;
  FCalculationKind:= ckSum;
  FFormula:= '';
end;

end.
