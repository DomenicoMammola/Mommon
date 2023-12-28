// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mSummary;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

uses
  Classes, contnrs, db, sysutils,
  mFloatsManagement, mNullables, mMaps;

type
  TmSummaryException = class(Exception);

  TmSummaryValueType = (svtDouble, svtInteger, svtString, svtDate, svtDateTime);

  TmSummaryDefinition = class;

  { TmSummaryScreenValue }

  TmSummaryScreenValue = class
  strict private
    FFormattedValue: string;
    FRawValue: variant;
    FDataType: TmSummaryValueType;
    FDefinition: TmSummaryDefinition;
  public
    constructor Create;
    destructor Destroy; override;

    property FormattedValue: string read FFormattedValue write FFormattedValue;
    property RawValue: variant read FRawValue write FRawValue;
    property DataType: TmSummaryValueType read FDataType write FDataType;
    property Definition: TmSummaryDefinition read FDefinition;
  end;

  { TmSummaryScreenValues }

  TmSummaryScreenValues = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function Get(const aIndex: integer): TmSummaryScreenValue;
    function Add: TmSummaryScreenValue;
    procedure Clear;
  end;

  ISummaryPanel = interface
    ['{27724709-FC36-4A12-B154-B92F566F0E94}']
    procedure Hide;
    procedure Show;
    procedure SetSummaryValues (aScreenValues: TmSummaryScreenValues);
  end;

  TmSummaryOperator = (soCount, soCountDistinct, soSum, soMax, soMin, soAverage, soAverageNotNull);

  { TmSummaryDefinition }

  TmSummaryDefinition = class
  strict private
    FFieldName : string;
    FFieldType : TFieldType;
    FSummaryOperator : TmSummaryOperator;
    FDisplayLabel : TNullableString;
    FDisplayFormat : TNullableString;
    procedure SetFieldName(AValue: string);
  private
    function GetUniqueIdentifier : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const aSource : TmSummaryDefinition);
    class function CheckOperatorCompatibility (const aOperator : TmSummaryOperator; const aFieldType : TFieldType) : boolean;

    property FieldName : string read FFieldName write SetFieldName;
    property SummaryOperator : TmSummaryOperator read FSummaryOperator write FSummaryOperator;
    property FieldType : TFieldType read FFieldType write FFieldType;
    property DisplayLabel : TNullableString read FDisplayLabel;
    property DisplayFormat : TNullableString read FDisplayFormat;
  end;


  { TmSummaryDefinitions }

  TmSummaryDefinitions = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function Get (const aIndex : integer) : TmSummaryDefinition;
    function FindByFieldNameAndOperator (const aFieldName : string; const aOperator : TmSummaryOperator) : TmSummaryDefinition;
    function Add : TmSummaryDefinition;
    procedure Delete (const aIndex : integer);
    procedure Remove (const aDefinition : TmSummaryDefinition);
    procedure Clear;
    procedure Assign(const aSource : TmSummaryDefinitions);
  end;

  { TmSummaryValue }

  TmSummaryValue = class
  private
    FDoubleValue : TNullableDouble;
    FDoubleValue2 : TNullableDouble;
    FStringValue: TNullableString;
    FIntegerValue: TNullableInteger;
    FIntegerValue2 : TNullableInteger;
    FUniqueValues: TmStringDictionary;

    FDefinition : TmSummaryDefinition;
    FOwnDefinition : boolean;

    function GetDataType: TmSummaryValueType;
    function GetValueAsString: string;
    function GetValueAsVariant: variant;
    function GetFormattedValue : string;
    procedure Init;
    procedure SetDefinition(AValue: TmSummaryDefinition);
  public
    constructor Create (const aOwnDefinition : boolean = true);
    destructor Destroy; override;

    procedure ComputeValueInSummaries (const aValue: Variant);

    property Definition : TmSummaryDefinition read FDefinition write SetDefinition;
    property ValueAsString: string read GetValueAsString;
    property ValueAsVariant: variant read GetValueAsVariant;
    property DataType: TmSummaryValueType read GetDataType;
    property FormattedValue: string read GetFormattedValue;
  end;

  { TmSummaryValues }

  TmSummaryValues = class
  strict private
    FList : TObjectList;
    FIndex : TmStringDictionary;
  public
    constructor Create;
    destructor Destroy; override;
    function AddValue (const aDefinition : TmSummaryDefinition; const aOwnDefinition : boolean = true) : TmSummaryValue;
    procedure Clear;
    function Count : integer;
    function Get(const aIndex : integer) : TmSummaryValue;
    function FindByDefinition(const aDefinition : TmSummaryDefinition): TmSummaryValue;
  end;


  function TmSummaryOperatorToString (const aOperator : TmSummaryOperator) : String;

implementation

uses
  variants,
  mDataFieldsUtility;

function TmSummaryOperatorToString(const aOperator: TmSummaryOperator): String;
begin
  Result := 'Unknown';
  case aOperator of
    soCount : Result := 'Count';
    soCountDistinct: REsult := 'Count distinct';
    soSum : Result := 'Sum';
    soMax : Result := 'Max';
    soMin : Result := 'Min';
    soAverage : Result := 'Average';
    soAverageNotNull : Result := 'Average not null';
  end;
end;

{ TmSummaryScreenValues }

constructor TmSummaryScreenValues.Create;
begin
  FList:= TObjectList.Create(true);
end;

destructor TmSummaryScreenValues.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmSummaryScreenValues.Count: integer;
begin
  Result:= FList.Count;
end;

function TmSummaryScreenValues.Get(const aIndex: integer): TmSummaryScreenValue;
begin
  Result:= FList.Items[aIndex] as TmSummaryScreenValue;
end;

function TmSummaryScreenValues.Add: TmSummaryScreenValue;
begin
  Result:= TmSummaryScreenValue.Create;
  FList.Add(Result);
end;

procedure TmSummaryScreenValues.Clear;
begin
  Flist.Clear;
end;

{ TmSummaryScreenValue }

constructor TmSummaryScreenValue.Create;
begin
  FFormattedValue:= '';
  FRawValue:= null;
  FDefinition:= TmSummaryDefinition.Create;
end;

destructor TmSummaryScreenValue.Destroy;
begin
  FDefinition.Free;
  inherited Destroy;
end;

{ TmSummaryDefinition }

procedure TmSummaryDefinition.SetFieldName(AValue: string);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
end;

function TmSummaryDefinition.GetUniqueIdentifier: String;
begin
  Result := FieldName + '#' + TmSummaryOperatorToString(SummaryOperator)
end;

constructor TmSummaryDefinition.Create;
begin
  FFieldName:= '';
  FFieldType:= ftUnknown;
  FSummaryOperator:= soCount;
  FDisplayLabel := TNullableString.Create;
  FDisplayFormat := TNullableString.Create;
end;

destructor TmSummaryDefinition.Destroy;
begin
  FDisplayLabel.Free;
  FDisplayFormat.Free;
  inherited Destroy;
end;

procedure TmSummaryDefinition.Assign(const aSource: TmSummaryDefinition);
begin
  FFieldName := aSource.FFieldName;
  FFieldType := aSource.FFieldType;
  FSummaryOperator := aSource.FSummaryOperator;
  FDisplayLabel.Assign(aSource.DisplayLabel);
  FDisplayFormat.Assign(aSource.DisplayFormat);
end;

class function TmSummaryDefinition.CheckOperatorCompatibility(const aOperator: TmSummaryOperator; const aFieldType: TFieldType): boolean;
begin
  // soCount, soCountDistinct, soSum, soMax, soMin, soAverage, soAverageNotNull
  if (aOperator = soSum) or (aOperator = soAverage) or (aOperator = soAverageNotNull) then
    Result := aFieldType in [ftInteger, ftFloat, ftCurrency, ftLargeint, ftSmallint]
  else
    Result := true;
end;

{ TmSummaryValues }

constructor TmSummaryValues.Create;
begin
  FList := TObjectList.Create(true);
  FIndex := TmStringDictionary.Create(false);
end;

destructor TmSummaryValues.Destroy;
begin
  FList.Free;
  FIndex.Free;
  inherited Destroy;
end;

function TmSummaryValues.AddValue(const aDefinition: TmSummaryDefinition; const aOwnDefinition : boolean = true): TmSummaryValue;
begin
  Result := TmSummaryValue.Create(aOwnDefinition);
  FList.Add(Result);
  if aOwnDefinition then
    Result.Definition.Assign(aDefinition)
  else
    Result.Definition := aDefinition;
  FIndex.Add(aDefinition.GetUniqueIdentifier, Result);
  Result.Init;
end;

procedure TmSummaryValues.Clear;
begin
  FList.Clear;
  FIndex.Clear;
end;

function TmSummaryValues.Count: integer;
begin
  Result := FList.Count;
end;

function TmSummaryValues.Get(const aIndex: integer): TmSummaryValue;
begin
  Result := FList.Items[aIndex] as TmSummaryValue;
end;

function TmSummaryValues.FindByDefinition(const aDefinition: TmSummaryDefinition): TmSummaryValue;
begin
  Result := FIndex.Find(aDefinition.GetUniqueIdentifier) as TmSummaryValue;
end;

{ TmSummaryValue }

procedure TmSummaryValue.Init;
begin
  if (FDefinition.SummaryOperator = soCount) or (FDefinition.SummaryOperator = soCountDistinct) or
     (FDefinition.SummaryOperator = soAverage) or (FDefinition.SummaryOperator = soAverageNotNull) then
    FIntegerValue2.Value:= 0;
end;

procedure TmSummaryValue.SetDefinition(AValue: TmSummaryDefinition);
begin
  if FOwnDefinition then
    raise TmSummaryException.Create('Definition of summary value is owned so it cannot be changed');
  if FDefinition=AValue then Exit;
  FDefinition:=AValue;
end;

function TmSummaryValue.GetValueAsString: string;
begin
  Result := '-';
  if (FDefinition.SummaryOperator = soCount) or (FDefinition.SummaryOperator = soCountDistinct) then
    Result := FIntegerValue2.AsString
  else
  if (FDefinition.SummaryOperator = soAverage) or (FDefinition.SummaryOperator = soAverageNotNull) then
  begin
    if FieldTypeIsInteger(FDefinition.FieldType) then
    begin
      if FIntegerValue.IsNull or (FIntegerValue2.AsInteger = 0) then
      begin
        if FDefinition.DisplayFormat.NotNull then
          Result := ''
        else
          Result := '-';
      end
      else
      begin
        if FDefinition.DisplayFormat.NotNull then
          Result := FormatFloat(FDefinition.DisplayFormat.AsString, FIntegerValue.AsInteger / FIntegerValue2.AsInteger)
        else
          Result := FormatFloat('#,##0', FIntegerValue.AsInteger / FIntegerValue2.AsInteger);
      end;
    end
    else if FieldTypeIsPascalDouble(FDefinition.FieldType) then
    begin
      if FDoubleValue.IsNull or (FIntegerValue2.AsInteger = 0)  then
      begin
        if FDefinition.DisplayFormat.NotNull then
          Result := ''
        else
          Result := '-';
      end
      else
      begin
        if FieldTypeIsFloat(FDefinition.FieldType) then
        begin
          if FDefinition.DisplayFormat.NotNull then
            Result := FormatFloat(FDefinition.DisplayFormat.AsString, RoundDoubleToStandardPrecision(FDoubleValue.Value / FIntegerValue2.AsInteger))
          else
            Result := FormatFloat('#,##0.0000', RoundDoubleToStandardPrecision(FDoubleValue.Value / FIntegerValue2.AsInteger));
        end
        else if FieldTypeIsDate(FDefinition.FieldType) then
          Result := DateToStr(Round(FDoubleValue.Value / FIntegerValue2.AsInteger))
        else if FieldTypeIsTime(FDefinition.FieldType) then
          Result := TimeToStr(FDoubleValue.Value / FIntegerValue2.AsInteger)
        else
          Result := DateTimeToStr(FDoubleValue.Value  / FIntegerValue2.AsInteger);
      end;
    end
    else
      Result := '';
  end
  else
  begin
    if FieldTypeIsInteger(FDefinition.FieldType) then
    begin
      if FIntegerValue.IsNull then
      begin
        if FDefinition.DisplayFormat.NotNull then
          Result := ''
        else
          Result := '-';
      end
      else
      begin
        if FDefinition.DisplayFormat.NotNull then
          Result := FormatFloat(FDefinition.DisplayFormat.AsString, FIntegerValue.Value)
        else
          Result := FormatFloat('#,##0', FIntegerValue.Value);
      end;
    end
    else if FieldTypeIsPascalDouble(FDefinition.FieldType) then
    begin
      if FDoubleValue.IsNull then
      begin
        if FDefinition.DisplayFormat.NotNull then
          Result := ''
        else
          Result := '-';
      end
      else
      begin
        if FieldTypeIsFloat(FDefinition.FieldType) then
        begin
          if FDefinition.DisplayFormat.NotNull then
            Result := FormatFloat(FDefinition.DisplayFormat.AsString, RoundDoubleToStandardPrecision(FDoubleValue.Value))
          else
            Result := FormatFloat('#,##0.0000', RoundDoubleToStandardPrecision(FDoubleValue.Value));
        end
        else if FieldTypeIsDate(FDefinition.FieldType) then
          Result := DateToStr(Round(FDoubleValue.Value))
        else if FieldTypeIsTime(FDefinition.FieldType) then
          Result := TimeToStr(FDoubleValue.Value)
        else
          Result := DateTimeToStr(FDoubleValue.Value);
      end;
    end
    else if FieldTypeIsString(FDefinition.FieldType) then
      Result := FStringValue.AsString;
  end;
end;

function TmSummaryValue.GetDataType: TmSummaryValueType;
begin
  if FieldTypeIsInteger(FDefinition.FieldType) then
    Result:= svtInteger
  else if FieldTypeIsFloat(FDefinition.FieldType) then
    Result:= svtDouble
  else if FieldTypeIsDateTime(FDefinition.FieldType) or FieldTypeIsTime(FDefinition.FieldType) then
    Result:= svtDateTime
  else if FieldTypeIsDate(FDefinition.FieldType) then
    Result:= svtDate
  else
    Result:= svtString;
end;

function TmSummaryValue.GetValueAsVariant: variant;
begin
  if (FDefinition.SummaryOperator = soCount) or (FDefinition.SummaryOperator = soCountDistinct) then
    Result := FIntegerValue2.AsVariant
  else
  if (FDefinition.SummaryOperator = soAverage) or (FDefinition.SummaryOperator = soAverageNotNull) then
  begin
    if FieldTypeIsInteger(FDefinition.FieldType) then
    begin
      if FIntegerValue.IsNull or (FIntegerValue2.AsInteger = 0) then
        Result := Null
      else
        Result := FIntegerValue.AsInteger / FIntegerValue2.AsInteger;
    end
    else if FieldTypeIsPascalDouble(FDefinition.FieldType) then
    begin
      if FDoubleValue.IsNull or (FIntegerValue2.AsInteger = 0)  then
      begin
        Result := Null;
      end
      else
      begin
        if FieldTypeIsFloat(FDefinition.FieldType) then
        begin
          Result := RoundDoubleToStandardPrecision(FDoubleValue.Value / FIntegerValue2.AsInteger);
        end
        else if FieldTypeIsDate(FDefinition.FieldType) then
          Result := Round(FDoubleValue.Value / FIntegerValue2.AsInteger)
        else
          Result := FDoubleValue.Value  / FIntegerValue2.AsInteger;
      end;
    end
    else
      Result := '';
  end
  else if FieldTypeIsInteger(FDefinition.FieldType) then
    Result := FIntegerValue.AsVariant
  else if FieldTypeIsPascalDouble(FDefinition.FieldType) then
  begin
    if FieldTypeIsDateTime(FDefinition.FieldType) or FieldTypeIsDate(FDefinition.FieldType) or FieldTypeIsTime(FDefinition.FieldType) then
    begin
      if FDoubleValue.NotNull then
        Result := VarToDateTime(FDoubleValue.AsVariant)
      else
        Result := Null;
    end
    else
      Result := FDoubleValue.AsVariant
  end
  else if FieldTypeIsString(FDefinition.FieldType) then
    Result := FStringValue.AsVariant
  else
    Result := Null
end;

constructor TmSummaryValue.Create(const aOwnDefinition : boolean = true);
begin
  FOwnDefinition := aOwnDefinition;
  if FOwnDefinition then
    FDefinition:= TmSummaryDefinition.Create;
  FDoubleValue:= TNullableDouble.Create();
  FDoubleValue2 := TNullableDouble.Create();
  FStringValue:= TNullableString.Create();
  FIntegerValue:= TNullableInteger.Create();
  FIntegerValue2 := TNullableInteger.Create();
  FUniqueValues:= TmStringDictionary.Create();
end;

destructor TmSummaryValue.Destroy;
begin
  if FOwnDefinition then
    FDefinition.Free;
  FDoubleValue.Free;
  FDoubleValue2.Free;
  FStringValue.Free;
  FIntegerValue.Free;
  FIntegerValue2.Free;
  FUniqueValues.Free;
  inherited Destroy;
end;

procedure TmSummaryValue.ComputeValueInSummaries(const aValue: Variant);
  procedure ComputeSum;
  var
    tmpInt : integer;
    tmpDouble : double;
  begin
    if not VarIsNull(aValue) then
    begin
      case FDefinition.FieldType of
        ftInteger, ftLargeint:
          begin
            tmpInt:= aValue;
            if FIntegerValue.IsNull then
              FIntegerValue.Value := tmpInt
            else
              FIntegerValue.Value := FIntegerValue.Value + tmpInt;
          end;
        ftFloat, ftDateTime, ftDate, ftTime, ftTimeStamp, ftFMTBcd, ftCurrency:
          begin
            tmpDouble:= RoundDoubleToStandardPrecision(aValue);
            if FDoubleValue.IsNull then
              FDoubleValue.Value := tmpDouble
            else
              FDoubleValue.Value := FDoubleValue.Value + tmpDouble;
          end;
      end;
    end;
  end;
var
  tmpInt : integer;
  tmpString : string;
  tmpDouble : double;
begin
  tmpString := '';

  case FDefinition.SummaryOperator of
    soCount:
      begin
        if not VarIsNull (aValue) then
          FIntegerValue2.Add(1);
      end;
    soAverage, soAverageNotNull:
      begin
        if (FDefinition.SummaryOperator = soAverage) or (not VarIsNull (aValue)) then
          FIntegerValue2.Add(1);
        ComputeSum;
      end;
    soCountDistinct:
      begin
        if not VarIsNull(aValue) then
        begin
          case FDefinition.FieldType of
            ftInteger, ftLargeint:
              begin
                tmpInt:= aValue;
                tmpString := IntToStr(tmpInt);
              end;
            ftFloat, ftDateTime, ftDate, ftTime, ftTimeStamp, ftFMTBcd, ftCurrency:
              begin
                tmpDouble:= RoundDoubleToStandardPrecision(aValue);
                tmpString := FloatToStr(tmpDouble);
              end;
            ftString, ftGuid:
              begin
                tmpString := VarToStr(aValue);
              end;
          end;
          if not FUniqueValues.Contains(tmpString) then
          begin
            FIntegerValue2.Add(1);
            FUniqueValues.Add(tmpString, FUniqueValues);
          end;
        end;
      end;
    soSum:
      begin
        ComputeSum;
      end;
    soMax:
      begin
        if not VarIsNull(aValue) then
        begin
          if FieldTypeIsInteger(FDefinition.FieldType) then
          begin
            tmpInt:= aValue;
            if FIntegerValue.IsNull then
              FIntegerValue.Value := tmpInt
            else
              if FIntegerValue.Value < tmpInt then
                FIntegerValue.Value := tmpInt;
          end
          else if FieldTypeIsPascalDouble(FDefinition.FieldType) then
          begin
            tmpDouble:= RoundDoubleToStandardPrecision(aValue);
            if FDoubleValue.IsNull then
              FDoubleValue.Value := tmpDouble
            else
              if FDoubleValue.Value < tmpDouble then
                FDoubleValue.Value := tmpDouble;
          end
          else if FieldTypeIsString(FDefinition.FieldType) then
          begin
            tmpString := VarToStr(aValue);
            if FStringValue.IsNull then
              FStringValue.Value := tmpString
            else
              if FStringValue.Value < tmpString then
                FStringValue.Value := tmpString;
          end;
        end;
      end;
    soMin:
      begin
        if not VarIsNull(aValue) then
        begin
          if FieldTypeIsInteger(FDefinition.FieldType) then
          begin
            tmpInt:= aValue;
            if FIntegerValue.IsNull then
              FIntegerValue.Value := tmpInt
            else
              if FIntegerValue.Value > tmpInt then
                FIntegerValue.Value := tmpInt;
          end
          else if FieldTypeIsPascalDouble(FDefinition.FieldType) then
          begin
            tmpDouble:= RoundDoubleToStandardPrecision(aValue);
            if FDoubleValue.IsNull then
              FDoubleValue.Value := tmpDouble
            else
              if FDoubleValue.Value > tmpDouble then
                FDoubleValue.Value := tmpDouble;
          end
          else if FieldTypeIsString(FDefinition.FieldType) then
          begin
            tmpString := VarToStr(aValue);
            if FStringValue.IsNull then
              FStringValue.Value := tmpString
            else
              if FStringValue.Value > tmpString then
                FStringValue.Value := tmpString;
          end;
        end;
      end;
  end;
end;

function TmSummaryValue.GetFormattedValue: string;
begin
  Result := TmSummaryOperatorToString(Self.Definition.SummaryOperator);
  Result := Result + '(';
  Result := Result + Self.Definition.DisplayLabel.AsString + ')= ';
  Result := Result + Self.ValueAsString;
end;

{ TmSummaryDefinitions }

constructor TmSummaryDefinitions.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmSummaryDefinitions.Destroy;
begin
  inherited Destroy;
  FList.Free;
end;

function TmSummaryDefinitions.Count: integer;
begin
  Result := FList.Count;
end;

function TmSummaryDefinitions.Get(const aIndex: integer): TmSummaryDefinition;
begin
  Result := (FList.Items[aIndex] as TmSummaryDefinition);
end;

function TmSummaryDefinitions.FindByFieldNameAndOperator (const aFieldName : string; const aOperator : TmSummaryOperator) : TmSummaryDefinition;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Self.Count - 1 do
  begin
    if CompareText(Self.Get(i).FieldName, aFieldName) = 0 then
    begin
      if Self.Get(i).SummaryOperator = aOperator then
      begin
        Result := Self.Get(i);
        break;
      end;
    end;
  end;
end;

function TmSummaryDefinitions.Add: TmSummaryDefinition;
begin
  Result := TmSummaryDefinition.Create;
  FList.Add(Result);
end;

procedure TmSummaryDefinitions.Delete(const aIndex: integer);
begin
  FList.Delete(aIndex);
end;

procedure TmSummaryDefinitions.Remove(const aDefinition: TmSummaryDefinition);
begin
  FList.Remove(aDefinition);
end;

procedure TmSummaryDefinitions.Clear;
begin
  FList.Clear;
end;

procedure TmSummaryDefinitions.Assign(const aSource: TmSummaryDefinitions);
var
  i : integer;
begin
  Self.Clear;
  for i := 0 to aSource.Count -1 do
    Self.Add.Assign(aSource.Get(i));
end;

end.
