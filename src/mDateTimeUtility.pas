// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDateTimeUtility;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

  function  AddYears(startDate: TDateTime; n: Integer) : TDateTime;
  function  AddMonths(startDate: TDateTime; n: Integer) : TDateTime;

  function ExtFormatDateTime(const Format: String; DateTime: TDateTime): String;

  function Intersect (const aStart1, aEnd1, aStart2, aEnd2 : TDateTime) : boolean;
  function EndOfQuarter(const aDate : TDateTime): TDateTime;

  // https://forum.lazarus.freepascal.org/index.php?topic=9310.0
  function ToUnixTimestamp (const aDate : TDateTime): integer;

implementation

uses
  sysutils, DateUtils, StrUtils,
  mFloatsManagement;

function  AddYears(startDate: TDateTime; n: Integer) : TDateTime;
  var
    Year, Month, Day: word;
  begin
    DecodeDate(startDate, Year, Month, Day);
    Year := Year + n;
    if (Month = 2) and (Day=29) and (not IsLeapYear(Year)) then
    begin
      Day := 28;
    end;
    Result := EncodeDate(Year, Month, Day);
    ReplaceTime(Result, startDate);
end;

function  AddMonths(startDate: TDateTime; n: Integer) : TDateTime;
begin
  Result := IncMonth(startDate, n);
end;

function ExtFormatDateTime(const Format: String; DateTime: TDateTime): String;
var
  Dummy : string;
  DoUppercase : boolean;
begin
  Result := Format;

  if ContainsStr(Result, '<1>') then
    Result := ReplaceText(Result, '<1>', IntToStr(WeekOfTheYear(DateTime)));
  if ContainsStr(Result, '<2>') then
    Result := ReplaceText(Result, '<2>', IntToStr(WeekOfTheMonth(DateTime)));
  if ContainsStr(Result, '<3>') then
    Result := ReplaceText(Result, '<3>', IntToStr(Trunc((MonthOfTheYear(DateTime) -1)/3+1)));
  if ContainsStr(Result, '~') then
    Result := ReplaceText(Result, '~', LowerCase(LeftStr(FormatDateTime('ddd', DayOfTheWeek(DateTime)), 1)));
  if ContainsStr(Result, '$') then
    Result := ReplaceText(Result, '$', UpperCase(LeftStr(FormatDateTime('ddd', DayOfTheWeek(DateTime)), 1)));
  if ContainsText(Result, '<xx>') then
  begin
    Dummy := IntToStr(WeekOfTheYear(DateTime));
    if Length(Dummy) < 2 then
      Dummy := '0' + Dummy;
    Result := ReplaceText(Result, '<xx>', Dummy);
  end;
  if ContainsText(Result, '<x>') then
     Result := ReplaceText(Result, '<x>', IntToStr(WeekOfTheYear(DateTime)));

  if CompareText(LeftStr(Result, 11), '<UPPERCASE>') = 0 then
  begin
    Result := Copy(Result, 12, 100);
    DoUppercase := true;
  end
  else
    DoUppercase := false;
  Result := FormatDateTime(Result, DateTime);
  if DoUppercase then
    Result := UpperCase(Result);
end;

function Intersect(const aStart1, aEnd1, aStart2, aEnd2: TDateTime): boolean;
var
  c1, c2, c3, c4 : boolean;
begin
  c1 := mFloatsManagement.DoubleIsLessThan(aStart1, aStart2);
  c2 := mFloatsManagement.DoubleIsLessThan(aStart1, aEnd2);
  Result := ((not c1) and c2);
  if not Result then
  begin
    c3 := mFloatsManagement.DoubleIsLessThan(aEnd1, aStart2);
    c4 := mFloatsManagement.DoubleIsLessThan(aEnd1, aEnd2);
    Result := ((not c3) and c4) or (c1 and (not c4));
  end;
end;

function EndOfQuarter(const aDate: TDateTime): TDateTime;
var
  d, m, y: Word;
begin
  DecodeDate(aDate, y, m, d);
  if (m mod 3) = 0 then
    Result := EndOfAMonth(y, m)
  else
    Result := EndOfAMonth(y, ((m div 3) + 1) * 3);
end;

function ToUnixTimestamp(const aDate: TDateTime): integer;
begin
  Result := Trunc((aDate - EncodeDate(1970, 1 ,1)) * 24 * 60 * 60);
end;

end.
