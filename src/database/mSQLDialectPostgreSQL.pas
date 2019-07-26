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
  // date '2019-01-15' + time '16:27:42'
  Result := 'date ''' + AddZerosFront(TempYear, 4) + '-' + AddZerosFront(TempMonth, 2) + '-' + AddZerosFront(TempDay, 2) + ''' + time ''' + AddZerosFront(TempHour, 2) + ':' + AddZerosFront(TempMinute, 2) + ':' + AddZerosFront(TempSecond, 2) + '''';
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
begin
  Result := '''' + StringReplace(aValue, '''', '''''', [rfReplaceAll]) + '''';
end;

function StringToSQLString(aValue: WideString): WideString;
begin
  Result := '''' + WideStringReplace(aValue, '''', '''''', [rfReplaceAll]) + '''';
end;

function WideStringToSQLString(aValue: WideString): String;
var
  tmp, tmp2 : WideString;
  sqlString : WideString;
begin
  tmp := LazUTF8.UTF8ToUTF16('''');
  tmp2 := LazUTF8.UTF8ToUTF16('''''');
  sqlString := LazUTF8.UTF8ToUTF16('''') + WideStringReplace(aValue, tmp, tmp2, [rfReplaceAll]) + tmp;
  Result := LazUTF8.UTF16ToUTF8(sqlString);
end;

end.
