// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSQLBuilder;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  mDatabaseConnectionClasses, mFilterOperators,
  mSQLDialectExpertImpl;

const
  PARAMETER_DELIMITER = '@';

type

  { TmSQLBuilder }

  TmSQLBuilder = class
  strict private
    FVendorType : TmDatabaseVendor;
    FParameters : TmQueryParameters;
    FSQL : String;
    FSQLDialectExpert : TSQLDialectExpertImpl;

    function GetVendorType: TmDatabaseVendor;
    procedure SetVendorType(AValue: TmDatabaseVendor);
  public
    constructor Create;
    destructor Destroy; override;
    function SQLSnippetForCondition(const aFieldName : String; const aOperator : TmFilterOperator; const aParamNameWithoutDelimiter : String) : String;
    function SQLSnippetForValue(const aParamNameWithoutDelimiter : String) : String;
    procedure PrepareSQL (aSQL : string);
    function ParamByName(const Value: string): TmQueryParameter;
    function BuildSQL : string;

    property VendorType : TmDatabaseVendor read GetVendorType write SetVendorType;
    property SQLDialectExpert : TSQLDialectExpertImpl read FSQLDialectExpert;
  end;

implementation

uses
  SysUtils, mSQLDialectExpertImplRegister,
  mSQLDialectExpertImplSQLServer, mSQLDialectExpertImplMySQL, mSQLDialectExpertImplPostgreSQL;

{ TmSQLBuilder }

function TmSQLBuilder.GetVendorType: TmDatabaseVendor;
begin
  Result := FVendorType;
end;

procedure TmSQLBuilder.SetVendorType(AValue: TmDatabaseVendor);
begin
  FVendorType:= aValue;
  // load implementation..
  if Assigned(FSQLDialectExpert) then
    FreeAndNil(FSQLDialectExpert);
  FSQLDialectExpert := GetSQLDialectExpertsRegister.GetImpl(FVendorType).Create;
end;

constructor TmSQLBuilder.Create;
begin
  FVendorType:= dvUnknown;
  FParameters := TmQueryParameters.Create;
end;

destructor TmSQLBuilder.Destroy;
begin
  FParameters.Free;
  if Assigned(FSQLDialectExpert) then
    FreeAndNil(FSQLDialectExpert);
  inherited Destroy;
end;

function TmSQLBuilder.SQLSnippetForCondition(const aFieldName: String; const aOperator: TmFilterOperator; const aParamNameWithoutDelimiter: String): String;
begin
  if not Assigned(FSQLDialectExpert) then
    raise TmDataConnectionException.Create('No database vendor was set. Unable to build definitive sql command');

  Result := '(' + FSQLDialectExpert.GetSQLForFieldname(aFieldName, aOperator) + ' ' + FSQLDialectExpert.GetSQLForConditionOperator(aOperator) + ' ' + PARAMETER_DELIMITER + aParamNameWithoutDelimiter + PARAMETER_DELIMITER +')';
end;

function TmSQLBuilder.SQLSnippetForValue(const aParamNameWithoutDelimiter: String): String;
begin
  Result := PARAMETER_DELIMITER + aParamNameWithoutDelimiter + PARAMETER_DELIMITER;
end;

procedure TmSQLBuilder.PrepareSQL(aSQL: string);
var
  start, stop, originalLength : integer;
  temp : String;
  tempParameter : TmQueryParameter;
begin
  FParameters.Clear;
  FSQL := aSQL;

  temp := FSQL;
  originalLength := Length(temp);
  start := Pos(PARAMETER_DELIMITER, temp);
  while (start >= 0) do
  begin
    temp := Copy(temp, start + 1, originalLength);
    stop := Pos(PARAMETER_DELIMITER, temp);
    if (stop > 0) then
    begin
      tempParameter := TmQueryParameter.Create;
      tempParameter.Name:= Copy(temp, 0, stop - 1);
      FParameters.Add(tempParameter);
      temp := Copy(temp, stop + 1, originalLength);
      start := Pos(PARAMETER_DELIMITER, temp);
    end
    else
    begin
      start := -1;
    end;
  end;

end;

function TmSQLBuilder.ParamByName(const Value: string): TmQueryParameter;
begin
  if FSQL = '' then
    raise Exception.Create('Missing SQL. Prepare SQL string command before.');
  Result := FParameters.FindByName(Value);
  if not Assigned(Result) then
    raise Exception.Create('Unknown parameter ' + Value);
end;

function TmSQLBuilder.BuildSQL: string;
var
  i : integer;
  tempString : String;
begin

  if not Assigned(FSQLDialectExpert) then
    raise TmDataConnectionException.Create('No database vendor was set. Unable to build definitive sql command');

  Result := FSQL;
  for i := 0 to FParameters.Count - 1 do
  begin
    tempString := FSQLDialectExpert.GetSQLForParameter(FParameters.GetParam(i));
    Result := StringReplace(Result, PARAMETER_DELIMITER + FParameters.GetParam(i).Name + PARAMETER_DELIMITER, tempString, [rfReplaceAll]);
  end;
end;

end.
