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

  // CREDITS: 11.7.99 - Pascal converting by Thomas Koehler, www.thkoehler.de
  procedure CalculateEasterDate (const year : word; const method : word; out month, day : word); overload;
  function CalculateEasterDate (const year : word; const method : word) : TDateTime; overload;

  function IsItalianPublicHoliday (const aDate : TDateTime) : boolean;

implementation

uses
  sysutils, DateUtils, StrUtils;

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

  if Pos('<1>', Result) > 0 then
    Result := ReplaceText(Result, '<1>', IntToStr(WeekOfTheYear(DateTime)));
  if Pos('<2>', Result) > 0 then
    Result := ReplaceText(Result, '<2>', IntToStr(WeekOfTheMonth(DateTime)));
  if Pos('<3>', Result) > 0 then
    Result := ReplaceText(Result, '<3>', IntToStr(Trunc((MonthOfTheYear(DateTime) -1)/3+1)));
  if Pos('~', Format) > 0 then
    Result := ReplaceText(Result, '~', LowerCase(LeftStr(FormatDateTime('ddd', DayOfTheWeek(DateTime)), 1)));
  if Pos('$', Result) > 0 then
    Result := ReplaceText(Result, '$', UpperCase(LeftStr(FormatDateTime('ddd', DayOfTheWeek(DateTime)), 1)));
  if Pos('<xx>', Result) > 0 then
  begin
    Dummy := IntToStr(WeekOfTheYear(DateTime));
    if Length(Dummy) < 2 then
      Dummy := '0' + Dummy;
    Result := ReplaceText(Result, '<xx>', Dummy);
  end;
  if Pos('<x>', Result) > 0 then
     Result := ReplaceText(Result, '<x>', IntToStr(WeekOfTheYear(DateTime)));

  if (Pos('<UPPERCASE>', Result) = 1) then
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

function IsItalianPublicHoliday(const aDate: TDateTime): boolean;
var
  year, month, day : word;
begin
  // https://en.wikipedia.org/wiki/Public_holidays_in_Italy
  DecodeDate(aDate, year, month, day);

  Result :=
    ((month = 1) and ((day = 1) or (day = 6))) or //1 January New Year's Day Capodanno - 6 January Epiphany Epifania
    ((month = 4) and (day = 25)) or // 25 April Liberation Day Festa della Liberazione Liberation of Italy from Nazi Germany, 1945
    ((month = 5) and (day = 1)) or // 1 May International Workers' Day Festa del Lavoro (or Festa dei Lavoratori)
    ((month = 6) and (day = 2)) or // 2 June Republic Day Festa della Repubblica Birth of the Italian Republic, 1946
    ((month = 8) and (day = 15)) or // 15 August Ferragosto/Assumption Day Ferragosto or Assunzione
    ((month = 11) and ((day = 1) or (day = 8))) or // 1 November All Saints' Day Tutti i santi (or Ognissanti) - 8 December Immaculate Conception Immacolata Concezione (or just Immacolata)
    ((month = 12) and ((day = 25) or (day = 26))); // 25 December Christmas Day Natale - 26 December St. Stephen's Day Santo Stefano

//Monday after Easter 	Easter Monday 	Lunedì dell'Angelo, Lunedì in Albis or more commonly Pasquetta

end;

function CalculateEasterDate(const year: word; const method: word): TDateTime;
var
  m, d : word;
begin
  CalculateEasterDate(year, method, m, d);
  Result := EncodeDate(year, m, d);
end;


procedure CalculateEasterDate (const year : word; const method : word; out month, day : word);
var
   FirstDig, Remain19, temp,              {intermediate results}
   tA, tB, tC, tD, tE         : integer;  {table A to E results}
begin

(* :=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=
  *  This algorithm is an arithmetic interpretation
  *  of the 3 step Easter Dating Method developed
  *  by Ron Mallen 1985, as a vast improvement on
  *  the method described in the Common Prayer Book

  *  Published Australian Almanac 1988
  *  Refer to this publication, or the Canberra Library
  *  for a clear understanding of the method used

  *  Because this algorithm is a direct translation of the
  *  official tables, it can be easily proved to be 100%
  *  correct

  *  It's free!  Please do not modify code or comments!

  *  11.7.99 - Pascal converting by Thomas Koehler, www.thkoehler.de

   :=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=:=*)


  FirstDig := year div 100;            {first 2 digits of year}
  Remain19 := year mod 19;             {remainder of year / 19}

  if (method = 1) or (method = 2) then
      begin
       {calculate PFM date}
      tA := ((225 - 11 * Remain19) mod 30) + 21;

       {find the next Sunday}
      tB := (tA - 19) mod 7;
      tC := (40 - FirstDig) mod 7;

      temp := year mod 100;
      tD := (temp + temp div 4) mod 7;

      tE := ((20 - tB - tC - tD) mod 7) + 1;
      day := tA + tE;

      if method = 2 then  {convert Julian to Gregorian date}
          begin
          {10 days were skipped
          in the Gregorian calendar from 5-14 Oct 1582}
          temp := 10;
          {Only 1 in every 4 century years are leap years in the Gregorian
          calendar (every century is a leap year in the Julian calendar)}
          if year > 1600 then
              temp := temp + FirstDig - 16 - ((FirstDig - 16) div 4);
          day := day + temp;
          end;
      end
  else
      begin
     {calculate PFM date}
      temp := (FirstDig - 15) div 2 + 202 - 11 * Remain19;
      if (FirstDig > 26) then temp := temp - 1;
      if (FirstDig > 38) then temp := temp - 1;
      if (FirstDig = 21) Or (FirstDig = 24) Or (FirstDig = 25)
        Or (FirstDig = 33) Or (FirstDig = 36) Or (FirstDig = 37) then
          temp := temp - 1;

      temp := temp mod 30;
      tA := temp + 21;
      if (temp = 29) then
          tA := tA - 1;
      if (temp = 28) and (Remain19 > 10) then
          tA := tA - 1;

     {find the next Sunday}
      tB := (tA - 19) mod 7;

      temp := (40 - FirstDig) mod 4;
      {//tC := temp - (temp > 1) - (temp := 3)}
      tC := temp;
      if temp > 1 then tC := tC + 1;
      if temp = 3 then tC := tC + 1;

      temp := year mod 100;
      tD := (temp + temp div 4) mod 7;

      tE := ((20 - tB - tC - tD) mod 7) + 1;
      day := tA + tE;

      end;

{return the date}
  month := 3;
  if (day > 61) then
  begin
      day := day - 61;  {when the original calculation is converted to the}
      month := 5;       {Gregorian calendar, Easter Sunday can occur in May}
  end;
  if (day > 31) then
    begin
        day := day - 31;
        month := 4;
    end;
end;

end.
