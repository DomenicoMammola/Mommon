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
  Classes, contnrs, db,
  mFloatsManagement, mNullables, mMaps;

type

  TmSummaryValueType = (svtDouble, svtInteger, svtString, svtDate, svtDateTime);

  { TmSummaryScreenValue }

  TmSummaryScreenValue = class
  strict private
    FFormattedValue: string;
    FRawValue: variant;
    FDataType: TmSummaryValueType;
  public
    constructor Create;

    property FormattedValue: string read FFormattedValue write FFormattedValue;
    property RawValue: variant read FRawValue write FRawValue;
    property DataType: TmSummaryValueType read FDataType write FDataType;
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

  TmSummaryOperator = (soCount, soCountDistinct, soSum, soMax, soMin);

  { TmSummaryDefinition }

  TmSummaryDefinition = class
  strict private
    FFieldName : string;
    FCaption : string;
    FFieldType : TFieldType;
    FSummaryOperator : TmSummaryOperator;
    procedure SetFieldName(AValue: string);
  public
    constructor Create;
    procedure Assign(const aSource : TmSummaryDefinition);

    property FieldName : string read FFieldName write SetFieldName;
    property Caption : string read FCaption write FCaption;
    property SummaryOperator : TmSummaryOperator read FSummaryOperator write FSummaryOperator;
    property FieldType : TFieldType read FFieldType write FFieldType;
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
  end;

  { TmSummaryValue }

  TmSummaryValue = class
  private
    FDoubleValue : TNullableDouble;
    FStringValue: TNullableString;
    FIntegerValue: TNullableInteger;
    FUniqueValues: TmStringDictionary;

    FDefinition : TmSummaryDefinition;

    function GetDataType: TmSummaryValueType;
    function GetValueAsString: string;
    function GetValueAsVariant: variant;
    function GetFormattedValue : string;
    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ComputeValueInSummaries (const aValue: Variant);

    property Definition : TmSummaryDefinition read FDefinition;
    property ValueAsString: string read GetValueAsString;
    property ValueAsVariant: variant read GetValueAsVariant;
    property DataType: TmSummaryValueType read GetDataType;
    property FormattedValue: string read GetFormattedValue;
  end;

  { TmSummaryValues }

  TmSummaryValues = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddValue (const aDefinition : TmSummaryDefinition) : TmSummaryValue;
    procedure Clear;
    function Count : integer;
    function Get(const aIndex : integer) : TmSummaryValue;
  end;

  function TmSummaryOperatorToString (const aOperator : TmSummaryOperator) : String;

implementation

uses
  variants, sysutils;

function FieldTypeIsInteger(const aFieldType : TFieldType): boolean;
begin
  Result := aFieldType in [ftInteger, ftSmallint, ftLargeint];
end;

function FieldTypeIsTime(const aFieldType : TFieldType): boolean;
begin
  Result := aFieldType = ftTime;
end;

function FieldTypeIsDate(const aFieldType : TFieldType): boolean;
begin
  Result := aFieldType = ftDate;
end;

function FieldTypeIsDateTime(const aFieldType : TFieldType): boolean;
begin
  Result := (aFieldType in [ftDateTime, ftTimeStamp]);
end;

function FieldTypeIsFloat(const aFieldType : TFieldType) : boolean;
begin
  Result := aFieldType in [ftFloat, ftFMTBcd, ftCurrency];
end;

function FieldTypeIsPascalDouble(const aFieldType : TFieldType): boolean;
begin
  Result := FieldTypeIsFloat(aFieldType) or FieldTypeIsDate(aFieldType) or FieldTypeIsTime(aFieldType) or FieldTypeIsDateTime(aFieldType);
end;

function FieldTypeIsString(const aFieldType : TFieldType) : boolean;
begin
  Result := aFieldType in [ftString, ftWideString, ftMemo, ftWideMemo, ftGuid];
end;


function TmSummaryOperatorToString(const aOperator: TmSummaryOperator): String;
begin
  Result := 'Unknown';
  case aOperator of
    soCount : Result := 'Count';
    soCountDistinct: REsult := 'Count distinct';
    soSum : Result := 'Sum';
    soMax : Result := 'Max';
    soMin : Result := 'Min';
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
end;

{ TmSummaryDefinition }

procedure TmSummaryDefinition.SetFieldName(AValue: string);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  FCaption := AValue;
end;

constructor TmSummaryDefinition.Create;
begin
  FFieldName:= '';
  FCaption:= '';
  FFieldType:= ftUnknown;
  FSummaryOperator:= soCount;
end;

procedure TmSummaryDefinition.Assign(const aSource: TmSummaryDefinition);
begin
  FFieldName := aSource.FFieldName;
  FCaption := aSource.FCaption;
  FFieldType := aSource.FFieldType;
  FSummaryOperator := aSource.FSummaryOperator;
end;

{ TmSummaryValues }

constructor TmSummaryValues.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmSummaryValues.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmSummaryValues.AddValue(const aDefinition: TmSummaryDefinition): TmSummaryValue;
begin
  Result := TmSummaryValue.Create;
  FList.Add(Result);
  Result.FDefinition.Assign(aDefinition);
  Result.Init;
end;

procedure TmSummaryValues.Clear;
begin
  FList.Clear;
end;

function TmSummaryValues.Count: integer;
begin
  Result := FList.Count;
end;

function TmSummaryValues.Get(const aIndex: integer): TmSummaryValue;
begin
  Result := FList.Items[aIndex] as TmSummaryValue;
end;

{ TmSummaryValue }

procedure TmSummaryValue.Init;
begin
  if (FDefinition.SummaryOperator = soCount) or (FDefinition.SummaryOperator = soCountDistinct) then
    FIntegerValue.Value:= 0;
end;

function TmSummaryValue.GetValueAsString: string;
begin
  Result := '-';
  if (FDefinition.SummaryOperator = soCount) or (FDefinition.SummaryOperator = soCountDistinct) then
    Result := FIntegerValue.AsString
  else
  begin
    if FieldTypeIsInteger(FDefinition.FieldType) then
    begin
      if FIntegerValue.IsNull then
        Result := '-'
      else
        Result := FormatFloat('#,##0', FIntegerValue.Value);
    end
    else if FieldTypeIsPascalDouble(FDefinition.FieldType) then
    begin
      if FDoubleValue.IsNull then
        Result := '-'
      else
      begin
        if FieldTypeIsFloat(FDefinition.FieldType) then
          Result := FormatFloat('#,##0.0000', RoundDoubleToStandardPrecision(FDoubleValue.Value))
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
  if FieldTypeIsInteger(FDefinition.FieldType) then
    Result := FIntegerValue.AsVariant
  else if FieldTypeIsPascalDouble(FDefinition.FieldType) then
    Result := FDoubleValue.AsVariant
  else if FieldTypeIsString(FDefinition.FieldType) then
    Result := FStringValue.AsVariant
  else
    Result := Null
end;

constructor TmSummaryValue.Create;
begin
  FDefinition:= TmSummaryDefinition.Create;
  FDoubleValue:= TNullableDouble.Create();
  FStringValue:= TNullableString.Create();
  FIntegerValue:= TNullableInteger.Create();
  FUniqueValues:= TmStringDictionary.Create();
end;

destructor TmSummaryValue.Destroy;
begin
  FDefinition.Free;
  FDoubleValue.Free;
  FStringValue.Free;
  FIntegerValue.Free;
  FUniqueValues.Free;
  inherited Destroy;
end;

procedure TmSummaryValue.ComputeValueInSummaries(const aValue: Variant);
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
          FIntegerValue.Value := FIntegerValue.Value + 1;
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
            FIntegerValue.Value := FIntegerValue.Value + 1;
            FUniqueValues.Add(tmpString, FUniqueValues);
          end;
        end;
      end;
    soSum:
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
  Result := Result + Self.Definition.Caption + ')= ';
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

end.
