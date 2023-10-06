// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFilter;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

uses
  Contnrs, Classes, Variants, SysUtils,
  mUtility, mFilterOperators, mMaps,
  mFields;

type
  TmFilterDataType = (fdtString, fdtDate, fdtDateTime, fdtTime, fdtInteger, fdtFloat, fdtBoolean);

  { TmFilter }

  TmFilter = class
  strict private
    FFieldName : string;
    FFilterOperator : TmFilterOperator;
    FValue : Variant;
    FDisplayValue : String;
    FDataType : TmFilterDataType;
    procedure SetValue(AValue: Variant);
  public
    constructor Create;
    destructor Destroy; override;

    procedure CopyFrom (aSource : TmFilter);

    property FieldName : string read FFieldName write FFieldName;
    property FilterOperator : TmFilterOperator read FFilterOperator write FFilterOperator;
    property Value : Variant read FValue write SetValue;
    property DataType : TmFilterDataType read FDataType write FDataType;
    property DisplayValue : String read FDisplayValue write FDisplayValue;
  end;

  { TmFilterEvaluator }

  TmFilterEvaluator = class
  strict private
    FValuesAsStrings : TStringList;
    FValuesDictionary: TmStringDictionary;
    FCurrentFilter : TmFilter;
    FMinStrValue, FMaxStrValue : String;
    FMinDoubleValue, FMaxDoubleValue : double;
    FMinIntegerValue, FMaxIntegerValue : integer;
  public
    procedure StartEvaluation(aFilter: TmFilter);
    function Evaluate (aActualValue : Variant) : boolean;
    procedure EndEvaluation;
  end;


  { TmFilters }

  TmFilters = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add : TmFilter; overload;
    procedure Add(aFilter : TmFilter); overload;
    procedure Clear;
    procedure ClearForField (const aFieldName : string);
    function Count : integer;
    function Get(aIndex : integer) : TmFilter;
    procedure CopyFrom (aSource : TmFilters);
    function HasFilterForFieldName(const aFieldName: String): boolean;
  end;


  TmOnGetDataForEvaluation = procedure (const aFilterIndex : integer; out aValue : Variant) of object;

  { TmFiltersEvaluator }

  TmFiltersEvaluator = class
  strict private
    FList : TObjectList;
    FOnGetData : TmOnGetDataForEvaluation;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartEvaluation (aFilters : TmFilters; aOnGetDataForEvaluation : TmOnGetDataForEvaluation);
    procedure EndEvaluation;
    function Evaluate : boolean;
  end;

  IFilterPanel = interface
    ['{27724709-FC36-4A12-B154-B92F566F0E94}']
    procedure SetFilters (const aFilters : TmFilters; const aFields : TmFields);
  end;

implementation

uses
  mIntList, mDoubleList;

{ TmFiltersEvaluator }

constructor TmFiltersEvaluator.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmFiltersEvaluator.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TmFiltersEvaluator.StartEvaluation(aFilters: TmFilters; aOnGetDataForEvaluation : TmOnGetDataForEvaluation);
var
  i : integer;
  tmpEvaluator : TmFilterEvaluator;
begin
  FOnGetData:= aOnGetDataForEvaluation;
  FList.Clear;
  for i := 0 to aFilters.Count - 1 do
  begin
    tmpEvaluator := TmFilterEvaluator.Create;
    FList.Add(tmpEvaluator);
    tmpEvaluator.StartEvaluation(aFilters.Get(i));
  end;
end;

procedure TmFiltersEvaluator.EndEvaluation;
var
  k : integer;
begin
  for k := 0 to FList.Count -1 do
    (FList.Items[k] as TmFilterEvaluator).EndEvaluation;

  FList.Clear;
end;

function TmFiltersEvaluator.Evaluate: boolean;
var
  k : integer;
  tmpValue : variant;
begin
  Result := true;

  for k := 0 to FList.Count -1 do
  begin
    Self.FOnGetData(k, tmpValue);
    Result := (FList.Items[k] as TmFilterEvaluator).Evaluate(tmpValue);
    if not Result then
      exit;
  end;
end;

{ TmFilterEvaluator }

procedure TmFilterEvaluator.StartEvaluation(aFilter: TmFilter);
  procedure CheckValuesForBeetween (const aNumOfValues : integer);
  begin
    if aNumOfValues <> 2 then
      raise Exception.Create('BETWEEN operator needs 2 values to perform evaluation.');
  end;

var
  i : integer;
  str : string;
  sList : TStringList;
  iList : TIntegerList;
  dList : TDoubleList;
begin
  FreeAndNil(FValuesAsStrings);
  FreeAndnil(FValuesDictionary);
  FCurrentFilter:= aFilter;
  if FCurrentFilter.FilterOperator = foIn then
  begin
    FValuesAsStrings := TStringList.Create;
    FValuesDictionary := TmStringDictionary.Create();
    ConvertVariantToStringList(FCurrentFilter.Value, FValuesAsStrings);
    for i := 0 to FValuesAsStrings.Count - 1 do
    begin
      str := lowercase(FValuesAsStrings.Strings[i]);
      if not FValuesDictionary.Contains(str) then
        FValuesDictionary.Add(str, FValuesAsStrings); // FValuesAsString is just a placeholder, to be able to check return value of find as nil
    end;
  end
  else
  if FCurrentFilter.FilterOperator = foBetween then
  begin
    if FCurrentFilter.DataType = fdtString then
    begin
      sList := TStringList.Create;
      try
        mUtility.ConvertVariantToStringList(FCurrentFilter.Value, sList);
        CheckValuesForBeetween(sList.Count);
        FMinStrValue := sList.Strings[0];
        FMaxStrValue := sList.Strings[1];
      finally
        sList.Free;
      end;
    end else if aFilter.DataType = fdtDate then
    begin
      iList := TIntegerList.Create;
      try
        ConvertVariantToDateList(FCurrentFilter.Value, iList);
        CheckValuesForBeetween(iList.Count);
        FMinDoubleValue:= iList.Items[0];
        FMaxDoubleValue:= iList.Items[1];
      finally
        iList.Free;
      end;
    end else if aFilter.DataType = fdtTime then
    begin
      dList := TDoubleList.Create;
      try
        ConvertVariantToDateTimeList(FCurrentFilter.Value, dList);
        CheckValuesForBeetween(dList.Count);
        FMinDoubleValue:= dList.Items[0] - trunc(dList.Items[0]);
        FMaxDoubleValue:= dList.Items[1] - trunc (dList.Items[1]);
      finally
        dList.Free;
      end;
    end else if (aFilter.DataType = fdtDateTime) then
    begin
      dList := TDoubleList.Create;
      try
        ConvertVariantToDateTimeList(FCurrentFilter.Value, dList);
        CheckValuesForBeetween(dList.Count);
        FMinDoubleValue:= dList.Items[0];
        FMaxDoubleValue:= dList.Items[1];
      finally
        dList.Free;
      end;
    end else if aFilter.DataType = fdtInteger then
    begin
      iList := TIntegerList.Create;
      try
        ConvertVariantToDateList(FCurrentFilter.Value, iList);
        CheckValuesForBeetween(iList.Count);
        FMinIntegerValue:= iList.Items[0];
        FMaxIntegerValue:= iList.Items[1];
      finally
        iList.Free;
      end;
    end else if aFilter.DataType = fdtFloat then
    begin
      dList := TDoubleList.Create;
      try
        ConvertVariantToDoubleList(FCurrentFilter.Value, dList);
        CheckValuesForBeetween(dList.Count);
        FMinDoubleValue:= dList.Items[0];
        FMaxDoubleValue:= dList.Items[1];
      finally
        dList.Free;
      end;
    end;
  end;
end;

function TmFilterEvaluator.Evaluate(aActualValue: Variant): boolean;
var
  str, strF : string;
begin
  Result := false;
  case FCurrentFilter.FilterOperator of
    foUnknown:
      Result := false;
    foEq:
      Result := mUtility.CompareVariants(aActualValue, FCurrentFilter.Value) = 0;
    foNotEq:
      Result := mUtility.CompareVariants(aActualValue, FCurrentFilter.Value) <> 0;
    foGtOrEq:
      Result := mUtility.CompareVariants(aActualValue, FCurrentFilter.Value) >= 0;
    foGt:
      Result := mUtility.CompareVariants(aActualValue, FCurrentFilter.Value) > 0;
    foLtOrEq:
      Result := mUtility.CompareVariants(aActualValue, FCurrentFilter.Value) <= 0;
    foLt:
      Result := mUtility.CompareVariants(aActualValue, FCurrentFilter.Value) < 0;
    foLike:
      begin
        if not VarIsNull(FCurrentFilter.Value) then
        begin
          str := lowercase(VarToStr(FCurrentFilter.Value));
          strF := lowercase(VarToStr(aActualValue));
          Result := (Pos(strF, str) > 0);
        end
        else
          Result := false;
      end;
    foStartWith:
      begin
        if not VarIsNull(FCurrentFilter.Value) then
        begin
          str := lowercase(VarToStr(FCurrentFilter.Value));
          strF := lowercase(VarToStr(aActualValue));
          Result := (Pos(strF, str) = 1);
        end
        else
          Result := false;
      end;
    foEndWith:
      begin
        if not VarIsNull(FCurrentFilter.Value) then
        begin
          str := lowercase(VarToStr(FCurrentFilter.Value));
          strF := lowercase(VarToStr(aActualValue));
          Result := ((Pos(strF, str) - 1 + Length(strF)) = Length(str));
        end
        else
          Result := false;
      end;
    foIn:
      begin
        Result := FValuesDictionary.Contains(lowercase(VarToStr(aActualValue)));
      end;
    foBetween:
      begin
        if FCurrentFilter.DataType = fdtString then
        begin
          Result := (FMinStrValue <= aActualValue) and (aActualValue <= FMaxStrValue);
        end else if (FCurrentFilter.DataType = fdtDate) or (FCurrentFilter.DataType = fdtDateTime) or (FCurrentFilter.DataType = fdtFloat) or (FCurrentFilter.DataType =fdtTime)  then
        begin
          Result := (FMinDoubleValue <= aActualValue) and (aActualValue <= FMaxDoubleValue);
        end else if FCurrentFilter.DataType = fdtInteger then
        begin
          Result := (FMinIntegerValue <= aActualValue) and (aActualValue <= FMaxIntegerValue);
        end;
      end;
  end;

end;

procedure TmFilterEvaluator.EndEvaluation;
begin
  FreeAndNil(FValuesAsStrings);
  FreeAndnil(FValuesDictionary);
end;

{ TmFilters }

constructor TmFilters.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmFilters.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmFilters.Add: TmFilter;
begin
  Result := TmFilter.Create;
  FList.Add(Result);
end;

procedure TmFilters.Add(aFilter: TmFilter);
begin
  FList.Add(aFilter);
end;

procedure TmFilters.Clear;
begin
  FList.Clear;
end;

procedure TmFilters.ClearForField(const aFieldName: string);
var
  i : integer;
begin
  for i := 0 to FList.Count -1 do
  begin
    if CompareText((FList.Items[i] as TmFilter).FieldName, aFieldName) = 0 then
    begin
      FList.Delete(i);
      exit;
    end;
  end;
end;

function TmFilters.Count: integer;
begin
  Result := FList.Count;
end;

function TmFilters.Get(aIndex: integer): TmFilter;
begin
  Result := FList.Items[aIndex] as TmFilter;
end;

procedure TmFilters.CopyFrom(aSource: TmFilters);
var
  i : integer;
begin
  Self.Clear;
  for i := 0 to aSource.Count - 1 do
    Self.Add.CopyFrom(aSource.Get(i));
end;

function TmFilters.HasFilterForFieldName(const aFieldName: String): boolean;
var
  i : integer;
begin
  Result := false;
  // no index, stupid find..
  for i := 0 to Count - 1 do
  begin
    if CompareText(aFieldName, Get(i).FieldName) = 0 then
    begin
      Result := true;
      exit;
    end;
  end;
end;

{ TmFilter }

procedure TmFilter.SetValue(AValue: Variant);
begin
  FValue:=AValue;
end;

constructor TmFilter.Create;
begin
  FFilterOperator:= foUnknown;
  FDataType:= fdtString;
end;

destructor TmFilter.Destroy;
begin
  inherited Destroy;
end;

procedure TmFilter.CopyFrom(aSource: TmFilter);
begin
  Self.FFieldName := aSource.FieldName;
  Self.FFilterOperator := aSource.FFilterOperator;
  Self.FValue := aSource.FValue;
  Self.FDisplayValue := aSource.FDisplayValue;
  Self.FDataType:= aSource.FDataType;
end;


end.
