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
{$ENDIF}

interface

uses
  Contnrs, Classes, Variants, SysUtils,
  mMaps, mUtility, mFilterOperators, StrHashMap;

type
  TmFilterDataType = (fdtString, fdtDate, fdtDateTime, fdtInteger, fdtFloat);

  { TmFilter }

  TmFilter = class
  strict private
    FFieldName : string;
    FFilterOperator : TmFilterOperator;
    FValue : Variant;
    FDataType : TmFilterDataType;

    FValuesAsStrings : TStringList;
    FValuesDictionary: TStringHashMap;
    procedure SetValue(AValue: Variant);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartEvaluation;
    function Evaluate (aActualValue : Variant) : boolean;
    procedure EndEvaluation;
    procedure CopyFrom (aSource : TmFilter);

    property FieldName : string read FFieldName write FFieldName;
    property FilterOperator : TmFilterOperator read FFilterOperator write FFilterOperator;
    property Value : Variant read FValue write SetValue;
    property DataType : TmFilterDataType read FDataType write FDataType;
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
    procedure StartEvaluation;
    procedure EndEvaluation;
  end;

implementation

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

procedure TmFilters.StartEvaluation;
var
  i : integer;
begin
  for i := 0 to Self.Count -1 do
    Self.Get(i).StartEvaluation;
end;

procedure TmFilters.EndEvaluation;
var
  i : integer;
begin
  for i := 0 to Self.Count -1 do
    Self.Get(i).EndEvaluation;
end;

{ TmFilter }

procedure TmFilter.SetValue(AValue: Variant);
begin
  if CompareVariants(FValue, AValue ) = 0 then
    exit;
  FValue:=AValue;
  FreeAndNil(FValuesAsStrings);
  FreeAndNil(FValuesDictionary);
end;

constructor TmFilter.Create;
begin
  FFilterOperator:= foUnknown;
  FValuesAsStrings:= nil;
  FValuesDictionary:= nil;
  FDataType:= fdtString;
end;

destructor TmFilter.Destroy;
begin
  FreeAndNil(FValuesAsStrings);
  FreeAndNil(FValuesDictionary);
  inherited Destroy;
end;

procedure TmFilter.StartEvaluation;
begin
  FreeAndNil(FValuesAsStrings);
  FreeAndnil(FValuesDictionary);
  FValuesAsStrings := TStringList.Create;
  FValuesDictionary := TStringHashMap.Create();
end;

function TmFilter.Evaluate(aActualValue: Variant): boolean;
var
  str, strF : string;
  i : integer;
begin
  Result := false;
  case Self.FilterOperator of
    foUnknown:
      Result := false;
    foEq:
      Result := mUtility.CompareVariants(aActualValue, FValue) = 0;
    foNotEq:
      Result := mUtility.CompareVariants(aActualValue, FValue) <> 0;
    foGtOrEq:
      begin
        Result := mUtility.CompareVariants(aActualValue, FValue) >= 0;
      end;
    foLtOrEq:
      begin
        Result := mUtility.CompareVariants(aActualValue, FValue) <= 0;
      end;
    foLike:
      begin
        if not VarIsNull(FValue) then
        begin
          str := lowercase(VarToStr(FValue));
          strF := lowercase(VarToStr(aActualValue));
          Result := (Pos(strF, str) > 0);
        end
        else
          Result := false;
      end;
    foStartWith:
      begin
        if not VarIsNull(FValue) then
        begin
          str := lowercase(VarToStr(FValue));
          strF := lowercase(VarToStr(aActualValue));
          Result := (Pos(strF, str) = 1);
        end
        else
          Result := false;
      end;
    foEndWith:
      begin
        if not VarIsNull(FValue) then
        begin
          str := lowercase(VarToStr(FValue));
          strF := lowercase(VarToStr(aActualValue));
          Result := ((Pos(strF, str) - 1 + Length(strF)) = Length(str));
        end
        else
          Result := false;
      end;
    foIn:
      begin
        if (FValuesAsStrings.Count = 0) then
        begin
          ConvertVariantToStringList(FValue, FValuesAsStrings);
          for i := 0 to FValuesAsStrings.Count - 1 do
          begin
            str := lowercase(FValuesAsStrings.Strings[i]);
            if not FValuesDictionary.Contains(str) then
              FValuesDictionary.Add(str, FValuesAsStrings); // FValuesAsString is just a placeholder, to be able to check return value of find as nil
          end;
        end;
        Result := FValuesDictionary.Contains(lowercase(VarToStr(aActualValue)));
      end;
  end;
end;

procedure TmFilter.EndEvaluation;
begin
  FreeAndNil(FValuesAsStrings);
  FreeAndnil(FValuesDictionary);
end;

procedure TmFilter.CopyFrom(aSource: TmFilter);
begin
  Self.FFieldName := aSource.FieldName;
  Self.FFilterOperator := aSource.FFilterOperator;
  Self.FValue := aSource.FValue;
end;

end.
