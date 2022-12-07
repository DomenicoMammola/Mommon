// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSQLDialectExpertImpl;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Variants,
  mDatabaseConnectionClasses, mFilterOperators;

type

  { TSQLDialectExpertImpl }

  TSQLDialectExpertImpl = class abstract
  protected
    function GetEscapeChar(const aParamValue : String) : String;
  public
    constructor Create; virtual; abstract;
    function GetSQLForParameter (aParam : TmQueryParameter) : string; virtual; abstract;
    function GetSQLForConditionOperator (const aOperator: TmFilterOperator) : string; virtual;
    function GetSQLForFieldname(const aFieldName: String; const aOperator: TmFilterOperator): String; virtual;
    function GetSQLForTablename(const aTableName: String): String; virtual;
  end;

  TSQLDialectExpertImplementationClass = class of TSQLDialectExpertImpl;

implementation
uses
  strutils, sysutils;


{ TSQLDialectExpertImpl }

function TSQLDialectExpertImpl.GetEscapeChar(const aParamValue: String): String;
begin
  if not AnsiContainsStr(aParamValue, '#') then
    Result := '#'
  else if not AnsiContainsStr(aParamValue, '^') then
    Result := '^'
  else if not AnsiContainsStr(aParamValue, '$') then
    Result := '$'
  else if not AnsiContainsStr(aParamValue, '+') then
    Result := '+'
  else if not AnsiContainsStr(aParamValue, '-') then
    Result := '-'
  else if not AnsiContainsStr(aParamValue, '=') then
    Result := '='
  else if not AnsiContainsStr(aParamValue, '£') then
    Result := '£'
  else
    raise Exception.Create('Unable to determine a valid escaping char');
end;

function TSQLDialectExpertImpl.GetSQLForConditionOperator(const aOperator: TmFilterOperator): string;
begin
  if aOperator = foEq then
    Result := '='
  else if aOperator = foGtOrEq then
    Result := '>='
  else if aOperator = foGt then
    Result := '>'
  else if aOperator = foLtOrEq then
    Result := '<='
  else if aOperator = foLt then
    Result := '<'
  else if (aOperator = foLike) or (aOperator = foStartWith) or (aOperator = foEndWith) then
    Result := 'LIKE'
  else if aOperator = foNotEq then
    Result := '<>'
  else if aOperator = foIn then
    Result := 'IN'
  else if aOperator = foBetween then
    Result := 'BETWEEN'
  else if aOperator = foIs then
    Result := 'IS'
  else if aOperator = foIsNot then
    Result := 'IS NOT'
  else
    Result := '?';
end;

function TSQLDialectExpertImpl.GetSQLForFieldname(const aFieldName: String; const aOperator: TmFilterOperator): String;
begin
  Result := aFieldName;
end;

function TSQLDialectExpertImpl.GetSQLForTablename(const aTableName: String): String;
begin
  Result := aTableName;
end;

end.
