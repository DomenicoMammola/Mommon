unit mVirtualDatasetPivoter;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  contnrs, db, variants, Classes,
  mVirtualDataSet,
  mIntList,
  StrHashMap;

type

  TmGroupByOperationKind = (gpoDistinct, gpoDateYear, gpoDateMonth, gpoDateDay, gpoFirstLetter, goFormula);

  { TmGroupByDef }

  TmGroupByDef = class
  strict private
    FFieldName : string;
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


  { TmIndex }

  TmKeysIndex = class
  strict private
    FKeysDictionary : TStringHashMap;
    FKeysValues : TStringList;

    FLevel : integer;
    FGarbage : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetSubIndex (aKey : string): TmKeysIndex;
    function GetValueList (aKey : string): TCardinalList;
  end;

  { TKeyValuesForGroupByDef }

  TKeyValuesForGroupByDef = class
  strict private
    FValues : TStringList;
    FDictionary : TStringHashMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddValue (aValue : String);
    function Contains (const aValue : String): boolean;
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


  { TmVirtualDatasetPivoter }

  TmVirtualDatasetPivoter = class
  strict private
    // dataset
    FVirtualDataset : TmVirtualDataset;
    // group by definitions
    FVerticalGroupByDefs : TmGroupByDefs;
    FHorizontalGroupByDefs : TmGroupByDefs;
    // values of keys of group-by sets
    FVerticalValues: TObjectList;
    FHorizontalValues: TObjectList;
    // coordinates of every record of dataset
    FRecordCoordinates : TStringHashMap;
    // indexes of keys of vertical and horizontal group-by sets
    FVerticalKeysIndex : TmKeysIndex;
    FHorizontalKeysIndex : TmKeysIndex;
    // garbage collector
    FGarbage : TObjectList;

//    procedure CalculateSubIndex(aIndex: TmKeysIndex; const aCurrentRecNo: Cardinal; const aGroupByDefs : TmGroupByDefs; const aCurrentGroupByDefIndex: integer);
    function GetIndexKeyValue(aValue : Variant; aGroupByDef : TmGroupByDef): string;
  strict private
    const KEY_SEPARATOR = '^~';
  public
    constructor Create;
    destructor Destroy; override;

    procedure CalculateHierarchy;

    property VirtualDataset : TmVirtualDataset read FVirtualDataset write FVirtualDataset;
    property VerticalGroupByDefs : TmGroupByDefs read FVerticalGroupByDefs;
    property HorizontalGroupByDefs : TmGroupByDefs read FHorizontalGroupByDefs;
  end;


implementation

uses
  sysutils, dateutils;

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
  FDictionary:= TStringHashMap.Create;
end;

destructor TKeyValuesForGroupByDef.Destroy;
begin
  FValues.Free;
  FDictionary.Free;
  inherited Destroy;
end;

procedure TKeyValuesForGroupByDef.AddValue(aValue: String);
begin
  FValues.Add(aValue);
  FDictionary.Add(aValue, Self);
end;

function TKeyValuesForGroupByDef.Contains(const aValue: String): boolean;
begin
  Result := FDictionary.Contains(aValue);
end;

{ TmIndex }

constructor TmKeysIndex.Create;
begin
  FKeysDictionary := TStringHashMap.Create();
  FKeysValues := TStringList.Create();
  FLevel := 0;
  FGarbage := TObjectList.Create(true);
end;

destructor TmKeysIndex.Destroy;
begin
  FKeysDictionary.Free;
  FKeysValues.Free;
  FGarbage.Free;
  inherited Destroy;
end;

procedure TmKeysIndex.Clear;
begin
  FKeysDictionary.Clear;
  FKeysValues.Clear;
  FGarbage.Clear;
end;

function TmKeysIndex.GetSubIndex(aKey: string): TmKeysIndex;
var
  p : pointer;
begin
  if FKeysDictionary.Find(aKey, p) then
    Result := TmKeysIndex(p)
  else
  begin
    Result := TmKeysIndex.Create;
    FKeysDictionary.Add(aKey, Result);
    FKeysValues.Add(aKey);
    FGarbage.Add(Result);
  end;
end;

function TmKeysIndex.GetValueList(aKey: string): TCardinalList;
var
  p : pointer;
begin
  if FKeysDictionary.Find(aKey, p) then
    Result := TCardinalList(p)
  else
  begin
    Result := TCardinalList.Create;
    FKeysDictionary.Add(aKey, Result);
    FKeysValues.Add(aKey);
    FGarbage.Add(Result);
  end;
end;

{ TmVirtualDatasetPivoter }

function TmVirtualDatasetPivoter.GetIndexKeyValue(aValue : Variant; aGroupByDef: TmGroupByDef): String;
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
  end;
end;

constructor TmVirtualDatasetPivoter.Create;
begin
  FVerticalGroupByDefs := TmGroupByDefs.Create;
  FHorizontalGroupByDefs := TmGroupByDefs.Create;
  FGarbage := TObjectList.Create(true);
  FRecordCoordinates:= TStringHashMap.Create;
  FVerticalKeysIndex := TmKeysIndex.Create;
  FHorizontalKeysIndex := TmKeysIndex.Create;
  FVerticalValues := TObjectList.Create(true);
  FHorizontalValues := TObjectList.Create(true);
end;

destructor TmVirtualDatasetPivoter.Destroy;
begin
  FVerticalGroupByDefs.Free;
  FHorizontalGroupByDefs.Free;
  FGarbage.Free;
  FRecordCoordinates.Free;
  FVerticalKeysIndex.Free;
  FHorizontalKeysIndex.Free;
  FVerticalValues.Free;
  FHorizontalValues.Free;

  inherited Destroy;
end;

(*procedure TmVirtualDatasetPivoter.CalculateSubIndex (aIndex : TmKeysIndex; const aCurrentRecNo : Cardinal; const aGroupByDefs : TmGroupByDefs; const aCurrentGroupByDefIndex : integer);
var
  tmpValue : variant;
  tmpStr : String;
begin
  FVirtualDataset.DatasetDataProvider.GetFieldValue(aGroupByDefs.Get(aCurrentGroupByDefIndex).FieldName, aCurrentRecNo, tmpValue);
  if (aGroupByDefs.Count - 1) > aCurrentGroupByDefIndex then
  begin
    tmpStr := Self.GetIndexKeyValue(tmpValue, aGroupByDefs.Get(aCurrentGroupByDefIndex));
    CalculateSubIndex(aIndex.GetSubIndex(tmpStr), aCurrentRecNo, aGroupByDefs, aCurrentGroupByDefIndex + 1);
  end
  else
  begin
    aIndex.GetValueList(Self.GetIndexKeyValue(tmpValue, aGroupByDefs.Get(aCurrentGroupByDefIndex))).Add(pointer(aCurrentRecNo));
  end;
end;*)

procedure TmVirtualDatasetPivoter.CalculateHierarchy;
var
  i, recCount : Cardinal;
  k : integer;
  tmpValue : Variant;
  tmpKeyValue : String;
  currentCoord : String;
  tmpList : TCardinalList;
  p : pointer;
  tmpIndex : TmKeysIndex;
begin
  FGarbage.Clear;

  FVerticalValues.Clear;
  FHorizontalValues.Clear;

  recCount := FVerticalGroupByDefs.Count;
  for i := 0 to recCount - 1 do
    FVerticalValues.Add(TKeyValuesForGroupByDef.Create);
  recCount := FHorizontalGroupByDefs.Count;
  for k := 0 to recCount -1 do
    FHorizontalValues.Add(TKeyValuesForGroupByDef.Create);

  FRecordCoordinates.Clear;

  recCount := FVirtualDataset.DatasetDataProvider.GetRecordCount;
  for i:= 0 to recCount -1 do
  begin
    currentCoord := '';
    tmpIndex := FVerticalKeysIndex;
    for k := 0 to FVerticalGroupByDefs.Count -1 do
    begin
      FVirtualDataset.DatasetDataProvider.GetFieldValue(FVerticalGroupByDefs.Get(k).FieldName, i, tmpValue);
      tmpKeyValue := GetIndexKeyValue(tmpValue, FVerticalGroupByDefs.Get(k));
      if not TKeyValuesForGroupByDef(FVerticalValues[k]).Contains(tmpKeyValue) then
        TKeyValuesForGroupByDef(FVerticalValues[k]).AddValue(tmpKeyValue);
      currentCoord := currentCoord + tmpKeyValue + KEY_SEPARATOR;
      if k < (FVerticalGroupByDefs.Count - 1) then
        tmpIndex := tmpIndex.GetSubIndex(tmpKeyValue)
      else
        tmpIndex.GetValueList(tmpKeyValue).Add(i);
    end;
    tmpIndex := FHorizontalKeysIndex;
    for k := 0 to FHorizontalGroupByDefs.Count -1 do
    begin
      FVirtualDataset.DatasetDataProvider.GetFieldValue(FHorizontalGroupByDefs.Get(k).FieldName, i, tmpValue);
      tmpKeyValue := GetIndexKeyValue(tmpValue, FHorizontalGroupByDefs.Get(k));
      if not TKeyValuesForGroupByDef(FHorizontalValues[k]).Contains(tmpKeyValue) then
        TKeyValuesForGroupByDef(FHorizontalValues[k]).AddValue(tmpKeyValue);
      currentCoord := currentCoord + tmpKeyValue + KEY_SEPARATOR;
      if k < (FHorizontalGroupByDefs.Count - 1) then
        tmpIndex := tmpIndex.GetSubIndex(tmpKeyValue)
      else
        tmpIndex.GetValueList(tmpKeyValue).Add(i);
    end;

    if FRecordCoordinates.Find(currentCoord, p) then
      tmpList := TCardinalList(p)
    else
    begin
      tmpList := TCardinalList.Create;
      FRecordCoordinates.Add(currentCoord, tmpList);
      FGarbage.Add(tmpList);
    end;
    tmpList.Add(i);
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
  FList.Clear;
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
