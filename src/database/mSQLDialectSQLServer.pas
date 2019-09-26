// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSQLDialectSQLServer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

function DateTimeToSQLString (aValue : TDateTime) : String;
function TimeToSQLString (aValue : TDateTime): String;
function DateToSQLString(aValue : TDate; const aAddQuotes : boolean = true) : String;
function FloatToSQLString(aValue : Double): String;
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
  Result := 'convert(datetime, ''' + AddZerosFront(TempYear, 4) + '-' + AddZerosFront(TempMonth, 2) + '-' + AddZerosFront(TempDay, 2) + ' ' + AddZerosFront(TempHour, 2) + ':' + AddZerosFront(TempMinute, 2) + ':' + AddZerosFront(TempSecond, 2) + ''', 120)';
end;

function TimeToSQLString(aValue: TDateTime): String;
var
  TempYear, TempMonth, TempDay, TempHour, TempMinute, TempSecond, TempMilli : word;
begin
  DecodeDateTime(aValue, TempYear, TempMonth, TempDay, TempHour, TempMinute, TempSecond, TempMilli);
  Result := 'convert(datetime, ''' + AddZerosFront(TempHour, 2) + ':' + AddZerosFront(TempMinute, 2) + ':' + AddZerosFront(TempSecond, 2) + ''', 114)';
end;

function DateToSQLString (aValue : TDate; const aAddQuotes : boolean = true) : String;
var
  TempYear, TempMonth, TempDay : word;
  tmpQuotes : String;
begin
  DecodeDate(aValue, TempYear, TempMonth, TempDay);
  if aAddQuotes then
    tmpQuotes := ''''
  else
    tmpQuotes := '';
  Result := tmpQuotes + AddZerosFront(TempYear, 4) + AddZerosFront(TempMonth, 2) + AddZerosFront(TempDay, 2) + tmpQuotes;
end;

function FloatToSQLString(aValue: Double): String;
begin
  Result := FloatToStr(aValue);
  Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
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
