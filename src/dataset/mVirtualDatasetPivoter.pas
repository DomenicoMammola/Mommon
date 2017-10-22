unit mVirtualDatasetPivoter;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  contnrs, db, variants, Classes,
  {$ifdef fpc}
  fgl,
  {$endif}
  mVirtualDataSet, mVirtualDatasetDataProvider,
  mBaseClassesAsObjects,
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

  TmIndex = class
  strict private
    FKeys : TStringHashMap;
    FLevel : integer;
    FGarbage : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetSubIndex (aKey : string): TmIndex;
    function GetValueList (aKey : string): TList;
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
    //
    FVerticalValues: array of TKeyValuesForGroupByDef;
    FHorizontalValues: array of TKeyValuesForGroupByDef;
    //
    FVerticalRecordCoordinates : TRecordsCoordinates;
    FHorizontalRecordCoordinates : TRecordsCoordinates;

    FIndex : TmIndex;
    FGarbage : TObjectList;

//    procedure CalculateSubIndex(aIndex: TmIndex; const aCurrentRecNo: Cardinal;
//      const aCurrentGroupByDefIndex: integer);
    function GetIndexKeyValue(aValue : Variant; aGroupByDef : TmGroupByDef): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Calculate;

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

constructor TmIndex.Create;
begin
  FKeys := TStringHashMap.Create();
  FLevel := 0;
  FGarbage := TObjectList.Create(true);
end;

destructor TmIndex.Destroy;
begin
  FKeys.Free;
  FGarbage.Free;
  inherited Destroy;
end;

procedure TmIndex.Clear;
begin
  FKeys.Clear;
  FGarbage.Clear;
end;

function TmIndex.GetSubIndex(aKey: string): TmIndex;
var
  p : pointer;
begin
  if FKeys.Find(aKey, p) then
    Result := TmIndex(p)
  else
  begin
    Result := TmIndex.Create;
    FKeys.Add(aKey, Result);
    FGarbage.Add(Result);
  end;
end;

function TmIndex.GetValueList(aKey: string): TList;
var
  p : pointer;
begin
  if FKeys.Find(aKey, p) then
    Result := TList(p)
  else
  begin
    Result := TList.Create;
    FKeys.Add(aKey, Result);
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

var
  tmpDateTime : string;
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
  FVerticalRecordCoordinates:= TRecordsCoordinates.Create;
  FHorizontalRecordCoordinates:= TRecordsCoordinates.Create;
end;

destructor TmVirtualDatasetPivoter.Destroy;
begin
  FVerticalGroupByDefs.Free;
  FHorizontalGroupByDefs.Free;
  FGarbage.Free;
  FVerticalRecordCoordinates.Free;
  FHorizontalRecordCoordinates.Free;
  inherited Destroy;
end;

(*
procedure TmVirtualDatasetPivoter.CalculateSubIndex (aIndex : TmIndex; const aCurrentRecNo : Cardinal; const aCurrentGroupByDefIndex : integer);
var
  tmpValue : variant;
  newCurrentGroupByDefIndex : integer;
begin
  newCurrentGroupByDefIndex := aCurrentGroupByDefIndex + 1;
  FVirtualDataset.DatasetDataProvider.GetFieldValue(FVerticalGroupByDefs.Get(newCurrentGroupByDefIndex).FieldName, aCurrentRecNo, tmpValue);
  if FVerticalGroupByDefs.Count > newCurrentGroupByDefIndex then
  begin
    aIndex.GetSubIndex(Self.GetIndexKeyValue(tmpValue, FVerticalGroupByDefs.Get(newCurrentGroupByDefIndex)));
  end
  else
  begin
//    tmpRecNoShell := TIntegerObject.Create(aCurrentRecNo);
//    FGarbage.Add(tmpRecNoShell);
    FIndex.GetValueList(Self.GetIndexKeyValue(tmpValue, FVerticalGroupByDefs.Get(newCurrentGroupByDefIndex))).Add(pointer(aCurrentRecNo)); //@tmpRecNoShell);
  end;
end;
*)

procedure TmVirtualDatasetPivoter.Calculate;
var
  i, recCount : Cardinal;
  k : integer;
  tmpValue : Variant;
  tmpKeyValue : String;
  currentCoord : TRecordCoordinates;
begin
  FGarbage.Clear;

  FVerticalValues := nil;
  FHorizontalValues := nil;

  SetLength(FVerticalValues, FVerticalGroupByDefs.Count);
  SetLength(FHorizontalValues, FHorizontalGroupByDefs.Count);

  FVerticalRecordCoordinates.Clear;
  FHorizontalRecordCoordinates.Clear;

  recCount := FVirtualDataset.DatasetDataProvider.GetRecordCount;
  for i:= 0 to recCount -1 do
  begin
    currentCoord := FVerticalRecordCoordinates.Add;
    for k := 0 to FVerticalGroupByDefs.Count -1 do
    begin
      FVirtualDataset.DatasetDataProvider.GetFieldValue(FVerticalGroupByDefs.Get(k).FieldName, i, tmpValue);
      tmpKeyValue := GetIndexKeyValue(tmpValue, FVerticalGroupByDefs.Get(k));
      if not FVerticalValues[k].Contains(tmpKeyValue) then
        FVerticalValues[k].AddValue(tmpKeyValue);
      currentCoord.Values[k] := tmpKeyValue;
    end;
    currentCoord := FHorizontalRecordCoordinates.Add;
    for k := 0 to FHorizontalGroupByDefs.Count -1 do
    begin
      FVirtualDataset.DatasetDataProvider.GetFieldValue(FHorizontalGroupByDefs.Get(k).FieldName, i, tmpValue);
      tmpKeyValue := GetIndexKeyValue(tmpValue, FHorizontalGroupByDefs.Get(k));
      if not FHorizontalValues[k].Contains(tmpKeyValue) then
        FHorizontalValues[k].AddValue(tmpKeyValue);
      currentCoord.Values[k] := tmpKeyValue;
    end;
  end;

(*
  recCount := FVirtualDataset.DatasetDataProvider.GetRecordCount;
  if FVerticalGroupByDefs.Count > 0 then
  begin
    for i := 0 to recCount - 1 do
      CalculateSubIndex(FIndex, i, 0);
  end;
  *)
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
