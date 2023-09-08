// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSQLDialectPostgreSQL;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

function DateTimeToSQLString (aValue : TDateTime) : String;
function TimeToSQLString (aValue : TDateTime): String;
function DateToSQLString(aValue : TDate) : String;
function FloatToSQLString(aValue : Double): String;
function BooleanToSQLString(aValue : Boolean): String;
function StringToSQLString(aValue : String): String; overload;
function StringToSQLString(aValue : WideString): WideString; overload;
function WideStringToSQLString (aValue : WideString): String;

implementation

uses
  DateUtils, SysUtils,
  {$IFDEF FPC}
  LazUTF8,
  {$ENDIF}
  mUtility;

function DateTimeToSQLString (aValue : TDateTime) : String;
var
  TempYear, TempMonth, TempDay, TempHour, TempMinute, TempSecond, TempMilli : word;
begin
  DecodeDateTime(aValue, TempYear, TempMonth, TempDay, TempHour, TempMinute, TempSecond, TempMilli);
  Result := '(timestamp ''' + AddZerosFront(TempYear, 4) + '-' + AddZerosFront(TempMonth, 2) + '-' + AddZerosFront(TempDay, 2) + ' ' + AddZerosFront(TempHour, 2) + ':' + AddZerosFront(TempMinute, 2) + ':' + AddZerosFront(TempSecond, 2) + ''')';
  // date '2019-01-15' + time '16:27:42'
  // Result := 'date ''' + AddZerosFront(TempYear, 4) + '-' + AddZerosFront(TempMonth, 2) + '-' + AddZerosFront(TempDay, 2) + ''' + time ''' + AddZerosFront(TempHour, 2) + ':' + AddZerosFront(TempMinute, 2) + ':' + AddZerosFront(TempSecond, 2) + '''';
end;

function TimeToSQLString(aValue: TDateTime): String;
var
  TempYear, TempMonth, TempDay, TempHour, TempMinute, TempSecond, TempMilli : word;
begin
  DecodeDateTime(aValue, TempYear, TempMonth, TempDay, TempHour, TempMinute, TempSecond, TempMilli);
  Result := 'time ''' + AddZerosFront(TempHour, 2) + ':' + AddZerosFront(TempMinute, 2) + ':' + AddZerosFront(TempSecond, 2) + '''';
end;

function DateToSQLString (aValue : TDate) : String;
var
  TempYear, TempMonth, TempDay : word;
begin
  DecodeDate(aValue, TempYear, TempMonth, TempDay);
  Result := 'to_date(''' + AddZerosFront(TempYear, 4) + AddZerosFront(TempMonth, 2) + AddZerosFront(TempDay, 2) + ''', ''YYYYMMDD'')';
end;

function FloatToSQLString(aValue: Double): String;
begin
  Result := FloatToStr(aValue);
  Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
end;

function BooleanToSQLString(aValue: Boolean): String;
begin
  if aValue then
    Result := 'true'
  else
    Result := 'false';
end;

function StringToSQLString(aValue : String): String;
var
  i : integer;
  concatenate : boolean;
begin
  Result := '';
  concatenate := false;
  for i := 1 to Length(aValue) do
  begin
    if concatenate then
    begin
      Result := Result + ' || ''';
      concatenate := false;
    end;
    if aValue[i] = '''' then
      Result := Result + ''''''
    else if aValue[i] = '\' then
    begin
      concatenate := true;
      Result := Result + '''|| U&''\005C''';
    end
    else
      Result := Result + aValue[i];
  end;
  Result := '''' + Result;
  if not concatenate then
    Result := Result + '''';
end;

function StringToSQLString(aValue: WideString): WideString;
var
  i : integer;
  concatenate : boolean;
begin
  Result := '';
  concatenate := false;
  for i := 1 to Length(aValue) do
  begin
    if concatenate then
    begin
      Result := Result + ' || ''';
      concatenate := false;
    end;
    if aValue[i] = '''' then
      Result := Result + ''''''
    else if aValue[i] = '\' then
    begin
      concatenate := true;
      Result := Result + '''|| U&''\005C''';
    end
    else
      Result := Result + aValue[i];
  end;
  if not concatenate then
    Result := Result + '''';
end;

function WideStringToSQLString(aValue: WideString): String;
begin
  Result := LazUTF8.UTF16ToUTF8(aValue);
  Result := StringToSQLString(Result);
end;

end.
