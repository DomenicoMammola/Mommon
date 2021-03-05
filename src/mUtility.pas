// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mUtility;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I mDefines.inc}

uses
  Classes, SysUtils, Variants, {$IFDEF WINDOWS}Windows, {$IFDEF FPC}{$IFDEF GRAPHICS_AVAILABLE}InterfaceBase,{$ENDIF}{$ENDIF} {$ENDIF}
  {$IFDEF GRAPHICS_AVAILABLE}Graphics,{$ENDIF}
  {$IFDEF GUI}
  Forms,
  {$ENDIF}
  mIntList, mDoubleList;

const
  TheDayWhenTimeStarted = 730120; //01/01/2000 (starting from 01/01/01)

type
  TListSortCompare = function (Item1, Item2: Pointer): Integer of object; // cloned from LCLProc but with 'of object'

function GenerateRandomIdString : string; overload;
function GenerateRandomIdString(aLength : integer): string; overload;
function CreateUniqueIdentifier : String; // actually a GUID without parentheses, to be used as unique indentifier in db tables
function IsUniqueIdentifier (const aUI : String): boolean;
function CreateHumanReadableUniqueIdentier (const aLanguageCode : String): String; // a random unique identifier which is easy to be remembered, inspired by https://github.com/PerWiklander/IdentifierSentence

procedure WordwrapStringByRows(const aSourceString : String; const aNumOfRows : integer; aRows : TStringList);

function AddZerosFront (const aValue : integer; const aLength : integer) : String; overload;
function AddZerosFront (const aValue : string; const aLength : integer) : String; overload;
function RemoveZerosFromFront (aValue : String) : String;
function CountOccurancesOfChar(const aValue: Char; const aStr : String): integer;

function DateTimeToSeconds(const aDateTime : TDateTime; const aTheDayWhenTimeStarted : integer = TheDayWhenTimeStarted) : integer;
function SecondsToDateTime(const aSeconds : integer; const aTheDayWhenTimeStarted : integer = TheDayWhenTimeStarted): TDateTime;

// try to convert the input text from the user as a date value, if it fails it returns false
// user can edit date as ddmmyy or ddmmyyyy or dmyy or with separators like '/', '\', '-', ....
function TryToUnderstandDateString(const aInputString : String; out aValue : TDateTime) : boolean;
// try to convert the input text from the user as a time value, if it fails it returns false
// user can edit time as hhmm or hhmmss or with separators like ':', '.', ....
function TryToUnderstandTimeString(const aInputString : String; out aValue : TDateTime) : boolean;
function TryToUnderstandDateTimeString(const aInputString : String; out aValue : TDateTime) : boolean;
{$IFDEF GRAPHICS_AVAILABLE}
function TryToUndestandColorString(const aInputString : String; out Value : TColor) : boolean;
{$ENDIF}
function TryToUnderstandBooleanString(const aInputString : String; out aValue : Boolean): Boolean;

// http://users.atw.hu/delphicikk/listaz.php?id=2189&oldal=11
function DateTimeStrEval(const DateTimeFormat: string; const DateTimeStr: string): TDateTime;

// https://code.google.com/p/theunknownones/
function VarRecToVariant (AValue : TVarRec) : Variant;

function CompareVariants (aVal1, aVal2 : variant) : integer;

function SafeVariantToInteger(aValue: variant; aDefaultValue : integer): integer;

// cloned from LCLProc unit but with OnCompare procedure that can be a method of a class
// this to avoid singleton dilemma when comparing operations needs extra data
// as in virtualdataset Sort method and it is possible to write thread-safe code without shared singleton resources
// http://lazarus-ccr.sourceforge.net/docs/lcl/lclproc/mergesort.html
procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare); overload;
procedure MergeSort(List: TList; const OnCompare: TListSortCompare); overload;

{$IFDEF FPC}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
{$ENDIF}

function KeepOnlyNumbers (const aSource : String) : String;
function KeepOnlyLetters (const aSource : String; const aUnderscoreForSpaces: boolean) : String;
function KeepOnlyLettersAndNumbers (const aSource : String; const aUnderscoreForSpaces: boolean) : String;

function ExtractSameLeftStringPart(const aList : TStringList): String;

function ExtractLastFolderFromPath (aFullPath : string) : string;

// https://forum.lazarus.freepascal.org/index.php/topic,33013.msg213197.html#msg213197
function SillyCryptDecrypt (const aText, aPassword: string): string;

procedure ConvertVariantToStringList (const aValue : variant; aList : TStringList);
procedure ConvertVariantToIntegerList (const aValue : variant; aList : TIntegerList);
procedure ConvertVariantToDoubleList (const aValue : variant; aList : TDoubleList);
procedure ConvertVariantToDateList (const aValue : variant; aList : TIntegerList);
procedure ConvertVariantToDateTimeList (const aValue : variant; aList : TDoubleList);

function ConvertStringListToVariant (const aList : TStringList): Variant;
function ConvertIntegerListToVariant (const aList : TIntegerList): Variant;
function ConvertDoubleListToVariant (const aList : TDoubleList): Variant;
function ConvertDoublesToVariant(const aValue1, aValue2: double): Variant;
function ConvertIntegersToVariant(const aValue1, aValue2: integer): Variant;

function GetCPUCores : integer;
function GetApplicationLocalDataFolder (const aApplicationSubDir : string) : String;
function GetApplicationDataFolder (const aApplicationSubDir : string) : String;
function GetOSUser : string;
{$IFDEF GUI}
procedure FlashInWindowsTaskbar(const aFlashEvenIfActive : boolean);
{$ENDIF}
// http://forum.codecall.net/topic/69184-solved-file-association-and-the-registry/
function RegisterDefaultApplication(const aFullPathExe : string; const aFileExtension : string; var aError : string): boolean;

// http://stackoverflow.com/questions/1285979/delphi-function-to-display-number-of-bytes-as-windows-does
// http://forum.lazarus.freepascal.org/index.php?topic=13705.0
function BytesToHumanReadableString(const bytes: UInt64): string;


// https://www.ietf.org/rfc/rfc822.txt
// Author: Ernesto D'Spirito
// http://www.howtodothings.com/computers/a1169-validating-email-addresses-in-delphi.html
function ValidEmail(const email: String): boolean;
function ValidEmails(const emails: String; out normalizedEmails : String): boolean;

// https://marc.durdin.net/2012/07/indy-tiduri-pathencode-urlencode-and-paramsencode-and-more/
// Author: Marc Durdin
function EncodeURIComponent(const aSrc: String): UTF8String;

function EncodeSVGString(const aSrc : String): String;

function EscapeSQLStringValue(const aSrc: String): String;

// https://docs.microsoft.com/it-it/windows/desktop/FileIO/naming-a-file#basic_naming_conventions
function SanitizeFileName(const aSrc: String) : String;
function SanitizeSubstringForFileName(const aSubString : String): String;
function AddNumberToFileName (const aSrc: String; const aNumber: integer): String;
function AddSuffixToFileName (const aSrc: String; const aSuffix: String) : String;

function GetTimeStampForFileName(const aInstant : TDateTime; const aAddTime : boolean = true): string;
function DecodeTimeStampForFileName(const aTimestamp: String) : TDateTime;

// encode a file to base64
procedure EncodeFileToBase64(const aFullPathInputFile: String; out aOutputData: String);
procedure DecodeBase64ToFile(const aInputData : String; const aFullPathOutputFile: String);

procedure AddUTF8BOMToStream (aStream : TStream);

function IsRunningAsRoot: boolean;
function CurrentProcessId: cardinal; // look at Indy function CurrentProcessId: TIdPID;

procedure RunConsoleApplicationAndGetOutput(const aCommand : string; const aParameters : array of string; out aOutputText : String);

implementation

uses
  DateUtils, base64, strutils, process, math,
  {$IFDEF WINDOWS}shlobj, registry, winutils,{$ELSE}LazUTF8,{$ENDIF}
  {$IFDEF LINUX}initc, ctypes, BaseUnix,{$ENDIF}
  mMathUtility;

var
  UTF8BOM : array[0..2] of byte = ($EF, $BB, $BF);

{$IFDEF LINUX}
function sysconf(i:cint):clong;cdecl;external name 'sysconf';
{$ENDIF}

procedure WordwrapStringByRows(const aSourceString: String; const aNumOfRows: integer; aRows: TStringList);
var
  remaining : String;
  start, leftSpace, curRow, i : integer;
begin
  aRows.Clear;
  if aNumOfRows = 1 then
    aRows.Add(aSourceString)
  else
  begin
    remaining:= aSourceString;
    curRow := 1;

    while curRow < aNumOfRows do
    begin
      start := Length(remaining) div (aNumOfRows - curRow + 1);
      leftSpace := -1;
      for i := start downto 1 do
      begin
        if aSourceString[i] = ' ' then
        begin
          leftSpace := i;
          break;
        end;
      end;
      if leftSpace > 0 then
      begin
        aRows.Add(LeftStr(remaining, leftSpace - 1));
        remaining := Copy(remaining, leftSpace + 1, 999999);
      end
      else
      begin
        aRows.Add(Copy(remaining, 1, start));
        remaining := Copy(remaining, start + 1, 999999);
      end;

      inc(curRow);
      if curRow = aNumOfRows then
        aRows.Add(remaining);
    end;
  end;
end;

function AddZerosFront (const aValue : integer; const aLength : integer) : String;
begin
  Result := AddZerosFront(IntToStr(aValue), aLength);
end;

function AddZerosFront(const aValue: string; const aLength: integer): String;
var
  i, l : integer;
begin
  Result := aValue;
  l := Length(Result);
  if l < aLength then
  begin
    for i := 1 to (aLength - l) do
    begin
      Result := '0' + Result;
    end;
  end;

end;

function RemoveZerosFromFront(aValue: String): String;
var
  i, l : integer;
begin
  l := Length(aValue);
  if l > 0 then
  begin
    i := 1;
    while (i <= l) and (aValue[i] = '0')  do
      inc(i);
    if i <= l then
      Result := Copy(aValue, i, 9999)
    else
      Result := '';
  end
  else
    Result := aValue;
end;

function CountOccurancesOfChar(const aValue: Char; const aStr: String): integer;
var
  c: Char;
begin
  Result := 0;
  for c in aStr do
    if c = aValue then
      inc(Result);
end;

function GenerateRandomIdString(aLength : integer): string;
var
  Temp: string;
  i: integer;
begin
  Result := '';
  Temp := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  for i := 0 to aLength - 1 do
    Result := Result + Temp[Random(35)+1];
end;

function GenerateRandomIdString : string;
begin
  Result := GenerateRandomIdString(10);
end;

(*
function RoundDateTimeToNearestInterval(vTime : TDateTime; vInterval : TDateTime = 5*60/SecsPerDay) : TDateTime;
var
  vTimeSec,vIntSec,vRoundedSec : int64;
begin
  //Rounds to nearest 5-minute by default
  vTimeSec := round(vTime * SecsPerDay);
  vIntSec := round(vInterval * SecsPerDay);

  if vIntSec = 0 then exit(vTimeSec / SecsPerDay);

  vRoundedSec := round(vTimeSec / vIntSec) * vIntSec;

  Result := vRoundedSec / SecsPerDay;
end;*)

function TruncDateTimeToSeconds(const DateTime: TDateTime): TDateTime;
{ Truncates seconds and millisec out of TDateTimeValues.}
var ts: SysUtils.TTimeStamp;
begin
{ Call DateTimeToTimeStamp to convert to TimeStamp: }
  ts  := SysUtils.DateTimeToTimeStamp(DateTime);
{ Truncate by removing the seconds and milliseconds}
  ts.Time := (ts.Time div (1000))*1000;
{ Call TimeStampToDateTime to convert back to TDateTime: }
  Result := SysUtils.TimeStampToDateTime(ts);
end;

function DateTimeToSeconds(const aDateTime : TDateTime; const aTheDayWhenTimeStarted : integer = TheDayWhenTimeStarted) : integer;
var
  ts : SysUtils.TTimeStamp;
begin
  ts := SysUtils.DateTimeToTimeStamp(aDateTime);
  if ts.Date < aTheDayWhenTimeStarted then
    raise Exception.Create('DateTimeToSeconds: datetime value is less than the origin of time');
  Result := (SecsPerDay * (ts.Date - aTheDayWhenTimeStarted)) + (ts.Time div 1000);
end;

function SecondsToDateTime(const aSeconds : integer; const aTheDayWhenTimeStarted : integer = TheDayWhenTimeStarted): TDateTime;
var
  ts : SysUtils.TTimeStamp;
begin
  ts.Date := aSeconds div SecsPerDay;
  ts.Time := (aSeconds - (ts.Date * SecsPerDay)) * 1000;
  ts.Date := ts.Date + aTheDayWhenTimeStarted;
  Result := SysUtils.TimeStampToDateTime(ts);
end;

function VarRecToVariant (AValue : TVarRec) : Variant;
begin
  case AValue.VType of
    vtInteger:
      Result:=AValue.VInteger;
    vtBoolean:
      Result:=AValue.VBoolean;
    vtChar:
      Result:=AValue.VChar;
    vtExtended:
      Result:=AValue.VExtended^;
    vtString:
      Result:=AValue.VString^;
    vtPointer:
      Result:=NativeInt(AValue.VPointer);
    vtPChar:
      Result:=StrPas(AValue.VPChar);
    vtAnsiString:
      Result:=String(AValue.VAnsiString);
    vtWideString, vtWideChar:
      result := WideCharToString(AValue.vwideString);
    vtCurrency:
      Result:=AValue.VCurrency^;
    vtVariant:
      Result:=AValue.VVariant^;
    vtInt64:
      Result := AValue.VInt64^;
    {$IFDEF UNICODE}
    vtUnicodeString:
      Result := String(PChar(AValue.VUnicodeString));
    {$ENDIF}
  else
    raise Exception.Create ('invalid data type ' + IntToStr(AValue.VType));
  end;
end;

function TryToUnderstandDateString(const aInputString : String; out aValue : TDateTime) : boolean;
var
  l, idx, i : integer;
  tmp,  sep : string;
  dString, mString, yString : string;
  day, month, year : integer;
  canTry : boolean;
begin
  Result := false;
  canTry := false;
  dString := '';
  mString := '';
  yString := '';

  tmp := Trim(aInputString);
  l := Length(tmp);


  sep := '\';
  idx := Pos(sep, tmp);
  if idx = 0 then
  begin
    sep := '/';
    idx := Pos(sep, tmp);
  end;
  if idx = 0 then
  begin
    sep := '-';
    idx := Pos(sep, tmp);
  end;
  if idx = 0 then
  begin
    sep := '.';
    idx := Pos(sep, tmp);
  end;

  if idx >= 2 then
  begin
    dString := Copy(tmp, 1, idx - 1);
    tmp := Copy (tmp, idx + 1, 999);
    idx := Pos(sep, tmp);
    if idx >= 2 then
    begin
      mString := Copy(tmp, 1, idx - 1);
      tmp := Copy (tmp, idx + 1, 999);
      if tmp <> '' then
      begin
        yString := tmp;
        canTry := true;
      end;
    end;
  end;

  if not canTry then
  begin
    if (l = 4) or (l=5) or (l = 6) or (l = 8) then
    begin
      // dmyy? dmmyy? ddmmyy? ddmmyyyy?
      if l = 4 then
      begin
        dString := Copy(tmp, 1, 1);
        mString := Copy(tmp, 2, 1);
        yString := Copy(tmp, 3, 2);
      end
      else if l=5 then
      begin
        dString := Copy(tmp, 1, 1);
        mString := Copy(tmp, 2, 2);
        yString := Copy(tmp, 4, 2);
      end
      else
      begin
        dString := Copy(tmp, 1, 2);
        mString := Copy(tmp, 3, 2);
        yString := Copy(tmp, 5, 999);
      end;
      CanTry := true;
    end;
  end;

  if CanTry then
  begin
    if IsNumeric(dString, false, false) and IsNumeric(mString, false, false) and IsNumeric(yString, false, false) then
    begin
      for i := 1 to 2 do
      begin
        if i = 1 then
        begin
          day := StrToInt(dString);
          month := StrToInt(mString);
          year := StrToInt(yString);
        end
        else
        begin
          // yyyy-mm-dd
          day := StrToInt(yString);
          month := StrToInt(mString);
          year := StrToInt(dString);
        end;

        if (month >=1) and (month <= 12) and (year >= 0) and (day >= 1) and (day <= 31) then
        begin
          if year < 100 then
            year := 2000 + year;
          if day <= DaysInAMonth(year, month) then
          begin
            Result := TryEncodeDate(year, month, day, aValue);
            exit;
          end;
        end;
      end;
    end;
  end;
end;

function TryToUnderstandTimeString(const aInputString : String; out aValue : TDateTime) : boolean;

  function DecodeWithDelimiter (const aDelimiter : Char; const aText : String) : boolean;
  var
    list : TStringList;
    hour, minutes, seconds : integer;
  begin
    Result := false;
    list := TStringList.Create;
    try
      list.Delimiter:=aDelimiter;
      list.DelimitedText:= aText;
      if (list.Count = 0) or (list.Count > 3) then
        exit;
      hour := 0;
      minutes := 0;
      seconds := 0;
      if list.Count >= 1 then
      begin
        if IsNumeric(list.Strings[0], false, false) then
        begin
          hour := StrToInt(list.Strings[0]);
          if (hour < 0) or (hour > 23) then
            exit;
        end;
      end;
      if list.Count >= 2 then
      begin
        if IsNumeric(list.Strings[1], false, false) then
        begin
          minutes := StrToInt(list.Strings[1]);
          if (minutes < 0) or (minutes > 59) then
            exit;
        end;
      end;
      if list.Count >= 3 then
      begin
        if IsNumeric(list.Strings[2], false, false) then
        begin
          seconds := StrToInt(list.Strings[2]);
          if (seconds < 0) or (seconds > 59) then
            exit;
        end;
      end;
      aValue := EncodeTime(hour, minutes, seconds, 0);
      Result := true;
    finally
      list.Free;
    end;
  end;

var
  tmp, hourStr, minutesStr, secondsStr : string;
  hour, minutes, seconds : integer;
  l : integer;
begin
  Result := false;
  tmp := Uppercase(Trim(aInputString));

  if Pos(':', tmp) >= 1 then
  begin
    // let's try with :
    Result := DecodeWithDelimiter(':', tmp);
  end
  else
  begin
    if Pos('.',tmp) >= 1 then
    begin
      // let's try with .
      Result := DecodeWithDelimiter('.', tmp);
    end
    else
    begin
      hourStr := '';
      minutesStr := '';
      secondsStr := '';

      // no separator
      l := Length(tmp);
      if (l = 3) then
      begin
        hourStr := Copy(tmp, 1, 2);
        minutesStr := Copy (tmp, 3, 1);
        secondsStr := '0';
        if IsNumeric(hourStr, false, false) then
        begin
          hour := StrToInt(hourStr);
          if (hour >= 23) then
          begin
            hourStr := Copy(tmp, 1, 1);
            minutesStr := Copy (tmp, 2, 2);
          end;
        end;
      end
      else
      if (l = 4) then
      begin
        hourStr:= Copy(tmp, 1, 2);
        minutesStr:= Copy(tmp, 3, 2);
        secondsStr:= '0';
      end
      else
      if (l=6) then
      begin
        hourStr := Copy(tmp, 1, 2);
        minutesStr := Copy(tmp, 3, 2);
        secondsStr := Copy(tmp,5, 2);
      end;

      if secondsStr <> '' then
      begin
        if IsNumeric(hourStr, false, false) and IsNumeric(minutesStr, false, false) and IsNumeric(secondsStr, false, false) then
        begin
          hour:= StrToInt(hourStr);
          minutes:= StrToInt(minutesStr);
          seconds:= StrToInt(secondsStr);

          if (hour >= 0) and (hour <= 23) and (minutes >= 0) and (minutes <= 59) and (seconds >= 0) and (seconds <= 59) then
          begin
            aValue := EncodeTime(hour, minutes, seconds, 0);
            Result := true;
          end;
        end;
      end;
    end;
  end;
  if not Result then
  begin
    try
      aValue:= StrToTime(tmp);
      Result := true;
    except
      on e: Exception do
      begin
        // ignored
        Result := false;
      end;
    end;
  end;
end;

function TryToUnderstandBooleanString(const aInputString: String; out aValue: Boolean): Boolean;
begin
  Result := TryStrToBool(aInputString, aValue);
  if not Result then
  begin
    if (aInputString = '0') or (CompareText(aInputString, 'false') = 0) then
    begin
      aValue := false;
      Result := true;
    end
    else if (aInputString = '1') or (CompareText(aInputString, 'true') = 0) then
    begin
      aValue := true;
      Result := true;
    end
  end;

end;

function TryToUnderstandDateTimeString(const aInputString: String; out aValue: TDateTime): boolean;
var
  i : integer;
  tmpDate : TDate;
  tmpTime : TDateTime;
  tmp : String;
begin
  Result := false;
  tmp := Uppercase(Trim(aInputString));
  i := Pos(' ', tmp);
  if i > 1 then
  begin
    if TryToUnderstandDateString(Copy(tmp, 1, i-1), tmpDate) then
      if TryToUnderstandTimeString(Copy(tmp, i + 1, 999), tmpTime) then
      begin
        aValue := tmpDate + tmpTime;
        Result := true;
      end;
  end;
end;

{$IFDEF GRAPHICS_AVAILABLE}

function TryToUndestandColorString(const aInputString: String; out Value: TColor): boolean;
begin
  Result := false;
  try
    Value := StringToColor(aInputString);
    Result := true;
  except
    on e: Exception do
    begin
      // ignored
    end;
  end;
end;
{$ENDIF}

(*
http://users.atw.hu/delphicikk/listaz.php?id=2189&oldal=11

This function will evaluate a DateTime string in accordance to the DateTime specifier format string supplied. The following specifiers are supported ...

  dd                                 the day as a number with a leading zero or space (01-31).
  ddd                         the day as an abbreviation (Sun-Sat)
  dddd                         the day as a full name (Sunday-Saturday)
  mm                         the month as a number with a leading zero or space (01-12).
  mmm                 the month as an abbreviation (Jan-Dec)
  mmmm                 the month as a full name (January-December)
  yy                                 the year as a two-digit number (00-99).
  yyyy                         the year as a four-digit number (0000-9999).
  hh                                 the hour with a leading zero or space (00-23)
  nn                                 the minute with a leading zero or space (00-59).
  ss                                 the second with a leading zero or space (00-59).
  zzz                                 the millisecond with a leading zero (000-999).
  ampm                 Specifies am or pm flag hours (0..12)
  ap                                 Specifies a or p flag hours (0..12)
  (Any other character corresponds to a literal or delimiter.)

NOTE : One assumption I have to make is that DAYS, MONTHS, HOURS and MINUTES have a leading                       ZERO or SPACE (ie. are 2 chars long) and MILLISECONDS are 3 chars long (ZERO or SPACE                        padded)

Using function
DateTimeStrEval(const DateTimeFormat : string; const DateTimeStr : string) : TDateTime;

The above Examples (1..4) can be evaluated as ... (Assume DT1 to DT4 equals example strings 1..4)

        1)MyDate := DateTimeStrEval('dddd dd mmmm yyyy hh:nnampm (ss xxxx)', DT1);
        2)MyDate := DateTimeStrEval('yyyymmdd', DT2);
        3)MyDate := DateTimeStrEval('dd-mmm-yy', DT3);
        4)MyDate := DateTimeStrEval('hh xxxx nn xxxxxx ss xxxxxx zzz xxxxx', DT4);
*)
function DateTimeStrEval(const DateTimeFormat: string; const DateTimeStr: string): TDateTime;
var
  i, ii, iii: integer;
  Retvar: TDateTime;
  Tmp,
    Fmt, Data, Mask, Spec: string;
  Year, Month, Day, Hour,
    Minute, Second, MSec: word;
  AmPm: integer;
begin
  Year := 1;
  Month := 1;
  Day := 1;
  Hour := 0;
  Minute := 0;
  Second := 0;
  MSec := 0;
  Fmt := UpperCase(DateTimeFormat);
  Data := UpperCase(DateTimeStr);
  i := 1;
  Mask := '';
  AmPm := 0;

  while i < length(Fmt) do
  begin
    if Fmt[i] in ['A', 'P', 'D', 'M', 'Y', 'H', 'N', 'S', 'Z'] then
    begin
      // Start of a date specifier
      Mask := Fmt[i];
      ii := i + 1;

      // Keep going till not valid specifier
      while true do
      begin
        if ii > length(Fmt) then
          Break; // End of specifier string
        Spec := Mask + Fmt[ii];

        if (Spec = 'DD') or (Spec = 'DDD') or (Spec = 'DDDD') or
          (Spec = 'MM') or (Spec = 'MMM') or (Spec = 'MMMM') or
          (Spec = 'YY') or (Spec = 'YYY') or (Spec = 'YYYY') or
          (Spec = 'HH') or (Spec = 'NN') or (Spec = 'SS') or
          (Spec = 'ZZ') or (Spec = 'ZZZ') or
          (Spec = 'AP') or (Spec = 'AM') or (Spec = 'AMP') or
          (Spec = 'AMPM') then
        begin
          Mask := Spec;
          inc(ii);
        end
        else
        begin
          // End of or Invalid specifier
          Break;
        end;
      end;

      // Got a valid specifier ? - evaluate it from data string
      if (Mask <> '') and (length(Data) > 0) then
      begin
        // Day 1..31
        if (Mask = 'DD') then
        begin
          Day := StrToIntDef(trim(copy(Data, 1, 2)), 0);
          delete(Data, 1, 2);
        end;

        // Day Sun..Sat (Just remove from data string)
        if Mask = 'DDD' then
          delete(Data, 1, 3);

        // Day Sunday..Saturday (Just remove from data string LEN)
        if Mask = 'DDDD' then
        begin
          Tmp := copy(Data, 1, 3);
          for iii := 1 to 7 do
          begin
            if Tmp = Uppercase(copy(FormatSettings.LongDayNames[iii], 1, 3)) then
            begin
              delete(Data, 1, length(FormatSettings.LongDayNames[iii]));
              Break;
            end;
          end;
        end;

        // Month 1..12
        if (Mask = 'MM') then
        begin
          Month := StrToIntDef(trim(copy(Data, 1, 2)), 0);
          delete(Data, 1, 2);
        end;

        // Month Jan..Dec
        if Mask = 'MMM' then
        begin
          Tmp := copy(Data, 1, 3);
          for iii := 1 to 12 do
          begin
            if Tmp = Uppercase(copy(FormatSettings.LongMonthNames[iii], 1, 3)) then
            begin
              Month := iii;
              delete(Data, 1, 3);
              Break;
            end;
          end;
        end;

        // Month January..December
        if Mask = 'MMMM' then
        begin
          Tmp := copy(Data, 1, 3);
          for iii := 1 to 12 do
          begin
            if Tmp = Uppercase(copy(FormatSettings.LongMonthNames[iii], 1, 3)) then
            begin
              Month := iii;
              delete(Data, 1, length(FormatSettings.LongMonthNames[iii]));
              Break;
            end;
          end;
        end;

        // Year 2 Digit
        if Mask = 'YY' then
        begin
          Year := StrToIntDef(copy(Data, 1, 2), 0);
          delete(Data, 1, 2);
          if Year < FormatSettings.TwoDigitYearCenturyWindow then
            Year := (YearOf(Date) div 100) * 100 + Year
          else
            Year := (YearOf(Date) div 100 - 1) * 100 + Year;
        end;

        // Year 4 Digit
        if Mask = 'YYYY' then
        begin
          Year := StrToIntDef(copy(Data, 1, 4), 0);
          delete(Data, 1, 4);
        end;

        // Hours
        if Mask = 'HH' then
        begin
          Hour := StrToIntDef(trim(copy(Data, 1, 2)), 0);
          delete(Data, 1, 2);
        end;

        // Minutes
        if Mask = 'NN' then
        begin
          Minute := StrToIntDef(trim(copy(Data, 1, 2)), 0);
          delete(Data, 1, 2);
        end;

        // Seconds
        if Mask = 'SS' then
        begin
          Second := StrToIntDef(trim(copy(Data, 1, 2)), 0);
          delete(Data, 1, 2);
        end;

        // Milliseconds
        if (Mask = 'ZZ') or (Mask = 'ZZZ') then
        begin
          MSec := StrToIntDef(trim(copy(Data, 1, 3)), 0);
          delete(Data, 1, 3);
        end;

        // AmPm A or P flag
        if (Mask = 'AP') then
        begin
          if Data[1] = 'A' then
            AmPm := -1
          else
            AmPm := 1;
          delete(Data, 1, 1);
        end;

        // AmPm AM or PM flag
        if (Mask = 'AM') or (Mask = 'AMP') or (Mask = 'AMPM') then
        begin
          if copy(Data, 1, 2) = 'AM' then
            AmPm := -1
          else
            AmPm := 1;
          delete(Data, 1, 2);
        end;

        Mask := '';
        i := ii;
      end;
    end
    else
    begin
      // Remove delimiter from data string
      if length(Data) > 1 then
        delete(Data, 1, 1);
      inc(i);
    end;
  end;

  if AmPm = 1 then
    Hour := Hour + 12;
  if not TryEncodeDateTime(Year, Month, Day, Hour, Minute, Second, MSec, Retvar) then
    Retvar := 0.0;
  Result := Retvar;
end;

{$IFDEF FPC}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

function KeepOnlyNumbers(const aSource: String): String;
var
  i : integer;
begin
  Result := '';
  for i := 1 to Length(aSource) do
  begin
    if aSource[i] in ['0'..'9'] then
      Result := Result + aSource[i];
  end;
end;

function KeepOnlyLetters(const aSource: String; const aUnderscoreForSpaces: boolean): String;
var
  i : integer;
begin
  Result := '';
  for i := 1 to Length(aSource) do
  begin
    if (aSource[i] in ['A'..'Z']) or (aSource[i] in ['a'..'z']) then
      Result := Result + aSource[i]
    else if aUnderscoreForSpaces and (aSource[i] = ' ') then
        Result := Result + '_';
  end
end;

function KeepOnlyLettersAndNumbers(const aSource: String; const aUnderscoreForSpaces: boolean): String;
var
  i : integer;
begin
  Result := '';
  for i := 1 to Length(aSource) do
  begin
    if (aSource[i] in ['A'..'Z']) or (aSource[i] in ['a'..'z']) or (aSource[i] in ['0'..'9']) then
      Result := Result + aSource[i]
    else if aUnderscoreForSpaces and (aSource[i] = ' ') then
      Result := Result + '_';
  end;
end;

function ExtractSameLeftStringPart(const aList: TStringList): String;
var
  i, k, maxLength : integer;
  curStr : String;
begin
  Result := '';
  maxLength:= MaxInt;
  for i := 0 to aList.Count -1 do
    maxLength:= min(Length(aList.Strings[i]), maxLength);
  for i := 1 to maxLength do
  begin
    curStr := LeftStr(aList.Strings[0], i);
    for k := 1 to aList.Count -1 do
    begin
      if not SameStr(curStr, LeftStr(aList.Strings[k], i)) then
      begin
        if i > 1 then
          Result := LeftStr(aList.Strings[0], i -1);
        exit;
      end;
    end;
  end;
  Result := LeftStr(aList.Strings[0], maxLength);
end;

function ExtractLastFolderFromPath(aFullPath: string): string;
var
  tmp : TStringList;
begin
  tmp := TStringList.Create;
  try
    ExtractStrings(AllowDirectorySeparators, [], PChar(aFullPath), tmp, false );
    Result := tmp.Strings[tmp.Count - 1];
  finally
    tmp.Free;
  end;
end;

function SillyCryptDecrypt(const aText, aPassword: string): string;
var
  i, len: integer;
  pwd : String;
begin
  len := Length(aText);
  pwd := aPassword;
  while Length(pwd) < len do
    pwd := pwd + aPassword;
  SetLength(result, len);
  for i := 1 to len do
    result[i] := Chr(Ord(aText[i]) xor Ord(pwd[i]));
end;

procedure ConvertVariantToStringList(const aValue: variant; aList: TStringList);
var
  i : integer;
begin
  if VarIsArray(aValue) then
  begin
    for i := VarArrayLowBound(aValue, 1) to VarArrayHighBound(aValue, 1) do
    begin
      aList.Add(VarToStr(aValue[i]));
    end;
  end
  else
    aList.Append(VarToStr(aValue));
end;

procedure ConvertVariantToIntegerList(const aValue: variant; aList: TIntegerList);
var
  i : integer;
begin
  if VarIsArray(aValue) then
  begin
    for i := VarArrayLowBound(aValue, 1) to VarArrayHighBound(aValue, 1) do
    begin
      aList.Add(VarAsType(aValue[i], varinteger));
    end;
  end
  else
    aList.Add(VarAsType(aValue, varinteger));
end;

procedure ConvertVariantToDoubleList(const aValue: variant; aList: TDoubleList);
var
  i : integer;
begin
  if VarIsArray(aValue) then
  begin
    for i := VarArrayLowBound(aValue, 1) to VarArrayHighBound(aValue, 1) do
    begin
      aList.Add(VarAsType(aValue[i], vardouble));
    end;
  end
  else
    aList.Add(VarAsType(aValue, vardouble));
end;

procedure ConvertVariantToDateList(const aValue: variant; aList: TIntegerList);
var
  i: integer;
  tmpDate : TDateTime;
begin
  if VarIsArray(aValue) then
  begin
    for i := VarArrayLowBound(aValue, 1) to VarArrayHighBound(aValue, 1) do
    begin
      tmpDate := VarAsType(aValue[i], vardate);
      aList.Add(trunc(tmpDate));
    end;
  end
  else
  begin
    tmpDate := VarAsType(aValue, vardate);
    aList.Add(trunc(tmpDate));
  end;
end;

procedure ConvertVariantToDateTimeList(const aValue: variant; aList: TDoubleList);
var
  i : integer;
begin
  if VarIsArray(aValue) then
  begin
    for i := VarArrayLowBound(aValue, 1) to VarArrayHighBound(aValue, 1) do
    begin
      aList.Add(VarAsType(aValue[i], vardate));
    end;
  end
  else
    aList.Add(VarAsType(aValue, vardate));
end;


function CompareVariants(aVal1, aVal2: variant): integer;
var
  valtype1, valtype2 : TVarType;
begin
  valtype1 := VarType(aVal1);
  valtype2 := VarType(aVal2);

  if valtype2 = varempty then
  begin
    if valtype1 = varempty then
      Result := 0
    else
      Result := -1;
    exit;
  end
  else if (valtype1 = varempty) then
  begin
    Result := 1;
    exit;
  end;

  if valtype2 = varnull then
  begin
    if valtype1 = varnull then
      Result := 0
    else
      Result := -1;
    exit;
  end
  else if (valtype1 = varnull) then
  begin
    Result := 1;
    exit;
  end;

  if (valtype1 <> valtype2) then
  begin
    if valtype1 < valtype2 then
      Result := -1
    else
      Result := 1;
  end
  else
  begin
    if aVal1 = aVal2 then
      Result := 0
    else if aVal1 < aVal2 then
      Result := -1
    else
      Result := 1;
   end;
end;

function SafeVariantToInteger(aValue: variant; aDefaultValue : integer): integer;
begin
  if VarIsNull(aValue) or VarIsEmpty(aValue) then
    Result := aDefaultValue
  else if VarIsOrdinal(aValue) then
    Result := aValue
  else if IsNumeric(VarToStr(aValue), false, true) then
    Result := StrToInt(VarToStr(aValue))
  else
    Result := aDefaultValue;
end;

// http://lazarus-ccr.sourceforge.net/docs/lcl/lclproc/mergesort.html
procedure _MergeSort(List: TFPList; StartIndex, EndIndex: integer; const OnCompare: TListSortCompare);
// sort so that for each i is OnCompare(List[i],List[i+1])<=0
var
  MergeList: PPointer;

  procedure SmallSort(StartPos, EndPos: PtrInt);
  // use insertion sort for small lists
  var
    i: PtrInt;
    Best: PtrInt;
    j: PtrInt;
    Item: Pointer;
  begin
    for i:=StartPos to EndPos-1 do begin
      Best:=i;
      for j:=i+1 to EndPos do
        if OnCompare(List[Best],List[j])>0 then
          Best:=j;
      if Best>i then begin
        Item:=List[i];
        List[i]:=List[Best];
        List[Best]:=Item;
      end;
    end;
  end;

  procedure Merge(Pos1, Pos2, Pos3: PtrInt);
  // merge two sorted arrays
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,a:PtrInt;
  begin
    while (Pos3>=Pos2) and (OnCompare(List[Pos2-1],List[Pos3])<=0) do
      dec(Pos3);
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=OnCompare(List[Src1Pos],List[Src2Pos]);
      if cmp>0 then begin
        MergeList[DestPos]:=List[Src1Pos];
        dec(Src1Pos);
      end else begin
        MergeList[DestPos]:=List[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      MergeList[DestPos]:=List[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for a:=DestPos+1 to Pos3 do
      List[a]:=MergeList[a];
  end;

  procedure Sort(StartPos, EndPos: PtrInt);
  // sort an interval in List. Use MergeList as work space.
  var
    mid: integer;
  begin
    if EndPos-StartPos<6 then begin
      SmallSort(StartPos,EndPos);
    end else begin
      mid:=(StartPos+EndPos) shr 1;
      Sort(StartPos,mid);
      Sort(mid+1,EndPos);
      Merge(StartPos,mid+1,EndPos);
    end;
  end;

var
  Cnt: Integer;
begin
  if (List=nil) then exit;
  Cnt:=List.Count;
  if StartIndex<0 then StartIndex:=0;
  if EndIndex>=Cnt then EndIndex:=Cnt-1;
  if StartIndex>=EndIndex then exit;
  MergeList:=GetMem(List.Count*SizeOf(Pointer));
  Sort(StartIndex,EndIndex);
  Freemem(MergeList);
end;

// http://lazarus-ccr.sourceforge.net/docs/lcl/lclproc/mergesort.html
procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare);
begin
  if List=nil then exit;
  _MergeSort(List,0,List.Count-1,OnCompare);
end;

procedure MergeSort(List: TList; const OnCompare: TListSortCompare);
var
  tmpList : TFPList;
  i : integer;
begin
  tmpList := TFPList.Create;
  try
    for i := 0 to List.Count - 1 do
      tmpList.Add(List.Items[i]);
    MergeSort(tmpList, OnCompare);
    List.Clear;
    for i := 0 to tmpList.Count - 1 do
      List.Add(tmpList.Items[i]);
  finally
    tmpList.Free;
  end
end;


function ConvertStringListToVariant(const aList: TStringList): Variant;
var
  tmpVariant : variant;
  i : integer;
begin
  tmpVariant := Variants.VarArrayCreate([0, aList.Count - 1], varolestr);
  for i := 0 to aList.Count - 1 do
    VarArrayPut(tmpVariant, aList.Strings[i], [i]);
  Result := tmpVariant;
end;

function ConvertIntegerListToVariant(const aList: TIntegerList): Variant;
var
  tmpVariant : variant;
  i : integer;
begin
  tmpVariant := Variants.VarArrayCreate([0, aList.Count - 1], varinteger);
  for i := 0 to aList.Count - 1 do
    VarArrayPut(tmpVariant, aList.Items[i], [i]);
  Result := tmpVariant;
end;

function ConvertDoubleListToVariant(const aList: TDoubleList): Variant;
var
  tmpVariant : variant;
  i : integer;
begin
  tmpVariant := Variants.VarArrayCreate([0, aList.Count - 1], vardouble);
  for i := 0 to aList.Count - 1 do
    VarArrayPut(tmpVariant, aList.Items[i], [i]);
  Result := tmpVariant;
end;

function ConvertDoublesToVariant(const aValue1, aValue2: double): Variant;
var
  tmpVariant : variant;
begin
  tmpVariant := Variants.VarArrayCreate([0, 1], vardouble);
  VarArrayPut(tmpVariant, aValue1, [0]);
  VarArrayPut(tmpVariant, aValue2, [1]);
  Result := tmpVariant;
end;

function ConvertIntegersToVariant(const aValue1, aValue2: integer): Variant;
var
  tmpVariant : variant;
begin
  tmpVariant := Variants.VarArrayCreate([0, 1], varinteger);
  VarArrayPut(tmpVariant, aValue1, [0]);
  VarArrayPut(tmpVariant, aValue2, [1]);
  Result := tmpVariant;
end;

{$IFDEF WINDOWS}
function GetCPUCores : integer;
var
  Info: TSystemInfo;
begin
  GetSystemInfo(Info);
  Result := Info.dwNumberOfProcessors;
end;
{$ELSE}

{$IFDEF LINUX}
function GetCPUCores : integer;
begin
  // http://forum.lazarus.freepascal.org/index.php?topic=33125.0
  Result := sysconf(83);
end;
{$ELSE}
{$IFDEF DARWIN}
// http://forum.lazarus.freepascal.org/index.php?topic=4098.0
function GetCPUCores : integer;
//returns number of CPUs for MacOSX computer
//example - will return 4 if the computer has two dual core CPUs
//requires Process in Uses Clause
//see http://wiki.lazarus.freepascal.org/Executing_External_Programs
var
   lProcess: TProcess;
   lLen,lPos: integer;
   lStr: string;
   lStringList: TStringList;
 begin
   Result := 1;
   lProcess := TProcess.Create(nil);
   lStringList := TStringList.Create;
   lProcess.CommandLine := 'sysctl hw.ncpu';
   lProcess.Options := lProcess.Options + [poWaitOnExit, poUsePipes];
   lProcess.Execute;
   lStringList.LoadFromStream(lProcess.Output);
   lLen := length(lStringList.Text);
   if lLen > 0 then begin
      lStr := '';
      for lPos := 1 to lLen do
             if lStringList.Text[lPos] in ['0'..'9'] then
                lStr := lStr + lStringList.Text[lPos];
      if length(lStr) > 0 then
         result := strtoint(lStr);
   end;//if at least one character returned
   lStringList.Free;
   lProcess.Free;
end;

{$ELSE}
function GetCPUCores : integer;
begin
  raise Exception.Create('GetCPUCores missing implementation for this platform');
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

function GetApplicationLocalDataFolder(const aApplicationSubDir: string): String;
var
  tmpDir : string;
  {$IFDEF WINDOWS}
  AppDataPath: Array[0..MaxPathLen] of Char; //http://wiki.lazarus.freepascal.org/Windows_Programming_Tips
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  AppDataPath:='';
  SHGetSpecialFolderPath(0,AppDataPath,CSIDL_LOCAL_APPDATA,false);
  tmpDir:= AppDataPath;
  Result := IncludeTrailingPathDelimiter(tmpDir) + aApplicationSubDir;
  {$ELSE}
    {$IFDEF LINUX}
    tmpDir:= GetEnvironmentVariableUTF8('HOME');
    Result := IncludeTrailingPathDelimiter(tmpDir) + '.' + aApplicationSubDir;
    {$ELSE}
    raise Exception.Create('GetApplicationLocalDataFolder missing implementation for this platform');
    {$ENDIF}
  {$ENDIF}
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetApplicationDataFolder(const aApplicationSubDir: string): String;
var
  tmpDir : string;
  {$IFDEF WINDOWS}
  AppDataPath: Array[0..MaxPathLen] of Char; //http://wiki.lazarus.freepascal.org/Windows_Programming_Tips
  {$ENDIF}
begin
  Result := '';
  {$IFDEF WINDOWS}
  AppDataPath:='';
  SHGetSpecialFolderPath(0,AppDataPath,CSIDL_APPDATA,false);
  tmpDir:= AppDataPath;
  Result := IncludeTrailingPathDelimiter(tmpDir) + aApplicationSubDir;
  {$ELSE}
    {$IFDEF LINUX}
    // GetAppConfigDir(false);
    raise Exception.Create('GetApplicationLocalDataFolder missing implementation for this platform');
    {$ELSE}
    raise Exception.Create('GetApplicationLocalDataFolder missing implementation for this platform');
    {$ENDIF}
  {$ENDIF}
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetOSUser: string;
begin
  Result := SysUtils.GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := SysUtils.GetEnvironmentVariable('USER');
end;

{$IFDEF GUI}
procedure FlashInWindowsTaskbar(const aFlashEvenIfActive : boolean);
begin
  {$IFDEF WINDOWS}
  {$push}{$warnings off}
  begin
  // http://forum.lazarus.freepascal.org/index.php?topic=33574.0
  If aFlashEvenIfActive or (not Application.Active) Then
    FlashWindow({$IFDEF FPC}WidgetSet.AppHandle{$ELSE}Application.Handle{$ENDIF}, True);
  end;
  {$pop}
  {$ENDIF}
end;
{$ENDIF}

function RegisterDefaultApplication(const aFullPathExe: string; const aFileExtension: string; var aError : string) : boolean;
{$IFDEF WINDOWS}
var
  a: byte;
  tmpRegistry : TRegistry;
  tmpFileExt : String;
begin
  // http://forum.codecall.net/topic/69184-solved-file-association-and-the-registry/
  Result := false;
  tmpFileExt := trim (aFileExtension);
  if Length(tmpFileExt) > 1 then
    if tmpFileExt[1] = '.' then
      tmpFileExt := Copy(tmpFileExt, 2, 999);
  if Length(tmpFileExt) = 0 then
    exit;

  a := 0;
  tmpRegistry := TRegistry.Create;
  try
    tmpRegistry.RootKey:= HKEY_CLASSES_ROOT;
    tmpRegistry.DeleteKey('\.' + tmpFileExt+ '\');
    if tmpRegistry.OpenKey('\.' + tmpFileExt + '\', True) then
      tmpRegistry.WriteString('', tmpFileExt + 'file')
    else
    begin
      {$WARNINGS OFF}
      aError := tmpRegistry.LastErrorMsg;
      {$WARNINGS ON}
      exit;
    end;
    if not tmpRegistry.OpenKey('\.' + tmpFileExt+ '\OpenWithList\ehshell.exe\', True) then
    begin
      {$WARNINGS OFF}
      aError := tmpRegistry.LastErrorMsg;
      {$WARNINGS ON}
      exit;
    end;

    if tmpRegistry.OpenKey('\.' + tmpFileExt + '\OpenWithProgIds\', True) then
      tmpRegistry.WriteBinaryData(tmpFileExt + 'file', a, 1)
    else
    begin
      {$WARNINGS OFF}
      aError := tmpRegistry.LastErrorMsg;
      {$WARNINGS ON}
      exit;
    end;

    if tmpRegistry.OpenKey('\' + tmpFileExt + 'file\shell\open\command\', True) then
      tmpRegistry.WriteString('', '"' + aFullPathExe + '" "%1"')
    else
    begin
      {$WARNINGS OFF}
      aError := tmpRegistry.LastErrorMsg;
      {$WARNINGS ON}
      exit;
    end;

    tmpRegistry.RootKey := HKEY_CURRENT_USER;

    tmpRegistry.DeleteKey('\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.' + tmpFileExt + '\');

    if tmpRegistry.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.' + tmpFileExt + '\OpenW​ithProgids\', True) then
      tmpRegistry.WriteBinaryData(tmpFileExt + 'file', a, 1)
    else
    begin
      {$WARNINGS OFF}
      aError := tmpRegistry.LastErrorMsg;
      {$WARNINGS ON}
      exit;
    end;

  finally
    tmpRegistry.Free;
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  Result := true;
{$ELSE}
begin
  Result := false;
  aError := 'OS not supported';
{$ENDIF}
end;

function CreateUniqueIdentifier: String;
var
  tmp : TGuid;
  lg : integer;
begin
  if CreateGUID(tmp) <> 0 then
    raise Exception.Create('Failed to create a new GUID');
  Result := GUIDToString(tmp);
  lg := Length(Result);
  if lg > 2 then
  begin
    if Result[1] = '{' then
    begin
      Result := Copy(Result, 2, 9999);
      dec(lg);
    end;
    if Result[lg] = '}' then
      Result := Copy(Result, 1, lg -1);
  end;
end;

function IsUniqueIdentifier(const aUI: String): boolean;
var
  s : String;
  tmpGuid : TGuid;
  lg : integer;
begin
  Result := false;
  lg := Length(aUI);
  if lg > 2 then
  begin
    if (aUI[1] <> '{') and (aUI[lg] <> '}') then
      s := '{' + aUI + '}'
    else
      s := aUI;
    Result := TryStringToGUID(s, tmpGuid);
  end;
end;

function CreateHumanReadableUniqueIdentier(const aLanguageCode : String): String;
var
  nouns : array [1..66] of string  =
      ('ants',       'badgers',     'bats',     'bees',       'butterflies',
      'cattle',      'chickens',    'clams',    'cockles',    'crabs',
      'crows',       'deer',        'dogs',     'donkeys',    'doves',
      'dragonflies', 'ducks',       'ferrets',  'flies',      'foxes',
      'frogs',       'geese',       'gorillas',  'goats',      'grasshoppers',
      'hamsters', 'hares',       'hawks',    'hedgehogs',  'herons',
      'horses',      'kingfishers', 'lobsters', 'mice',       'moles',
      'moths',       'mussles',     'newts',    'orcs',       'otters',
      'owls',        'oysters',     'parrots',  'peafowl',    'pheasants',
      'pigeons',     'pigs',        'pikes',    'platypuses', 'rabbits',
      'rats',        'robins',      'rooks',    'salmons',    'sheep',
      'snails',      'snakes',      'sparrows', 'spiders',    'squirrels',
      'starlings',   'stoats',      'swans',    'toads',      'trouts',
      'weasels');

  nouns_male_it : array [1..137] of string =
     ('tassi', 'pipistrelli', 'buoi', 'polli', 'granchi',
     'corvi', 'cervi', 'cani', 'asini', 'furetti',
     'gorilla', 'criceti', 'falchi', 'ricci', 'aironi',
     'cavalli', 'topi', 'mitili', 'tritoni',
     'orchi', 'gufi', 'pappagalli', 'pavoni', 'fagiani',
     'piccioni', 'maiali', 'lucci', 'ornitorinchi', 'conigli',
     'ratti', 'pettirossi', 'salmoni', 'serpenti', 'passeri',
     'ragni', 'scoiattoli', 'storni', 'ermellini', 'cigni',
     'rospi',

     'goblin', 'tonni', 'passeri', 'muli', 'beluga',
     'squali', 'delfini', 'totani', 'gatti', 'procioni',
     'armadilli', 'avvoltoi', 'babbuini', 'barracuda', 'bradipi',
     'branzini', 'bufali', 'cacatua', 'calabroni', 'camaleonti',
     'cammelli', 'camosci', 'caprioli', 'cardellini', 'castori',
     'casuari', 'cigni', 'cinghiali', 'coccodrilli', 'condor',
     'cormorani', 'corvi', 'coyote', 'daini', 'dromedari',
     'elefanti', 'facoceri', 'fagiani', 'gabbiani', 'fenicotteri',
     'formichieri', 'furetti', 'fringuelli', 'gechi', 'ghepardi',
     'giaguari', 'gerbilli', 'grilli', 'ippocampi', 'koala',
     'leoni', 'leopardi', 'lombrichi', 'lupi', 'mammut',
     'mandrilli', 'moscardini', 'moscerini', 'mufloni', 'narvali',
     'naselli', 'nibbi', 'orsi', 'palombi', 'panda',
     'pangolini', 'parrocchetti', 'pettirossi', 'picchi', 'pipistrelli',
     'pitoni', 'polpi', 'pony', 'porcellini', 'puma',
     'ratti', 'rinoceronti', 'rombi', 'rinoceronti', 'rospi',
     'scarabei', 'sciacalli', 'scoiattoli', 'scorpioni', 'scriccioli',
     'sgombri', 'sparvieri', 'storioni', 'struzzi', 'tafani',
     'tarli', 'tassi', 'terranova', 'tonni', 'vombati',
     'zebu', 'visoni'
     );
  nouns_female_it : array [1..91] of string =
     ('formiche', 'api', 'farfalle', 'vongole', 'telline',
     'colombe', 'libellule', 'anatre', 'mosche', 'volpi',
     'rane', 'oche', 'capre', 'cavallette', 'lepri',
     'aragoste', 'talpe', 'falene', 'lontre', 'ostriche',
     'cornacchie', 'pecore', 'lumache', 'trote', 'donnole',

     'zebre', 'coccinelle', 'mucche', 'donnole', 'acciughe',
     'alci', 'anaconde', 'vipere', 'anguille', 'aquile',
     'aringhe', 'averle', 'balene', 'balenottere', 'vespe',
     'capinere', 'chiocciole', 'cicogne', 'cimici', 'cinciallegre',
     'faraone', 'foche', 'focene', 'gallinelle', 'gazze',
     'gazzelle', 'ghiandaie',  'giraffe', 'iene', 'istrici',
     'meduse', 'linci', 'lontre', 'lucciole', 'lucertole',
     'manguste', 'mantidi', 'marmotte', 'megattere', 'murene',
     'natrici', 'nutrie', 'orate', 'orche', 'platesse',
     'poiane', 'puzzole', 'raganelle', 'quaglie', 'razze',
     'rondini', 'sardine', 'scimmie', 'sogliole', 'spatole',
     'spugne', 'sule', 'tartarughe', 'testuggini', 'tigri',
     'triglie', 'trote', 'upupe', 'verdesche', 'vipere',
     'volpi'
     );
  adjectives : array [1..128] of string =
    ('awful',    'bad',      'bashful', 'berserk',  'big',
    'bizarre',  'black',    'blue',    'boring',   'brawny',
    'bright',   'bumpy',    'burly',   'cagey',    'cheerful',
    'chilly',   'chubby',   'classy',  'clumsy',   'cold',
    'crazy',    'creepy',   'cuddly',  'dashing',  'dirty',
    'dizzy',    'drunk',    'dry',     'dull',     'dusty',
    'eager',    'evil',     'fancy',   'fast',     'fat',
    'faulty',   'fearless', 'filthy',  'foamy',    'friendly',
    'funny',    'gentle',   'giant',   'glossy',   'good',
    'goofy',    'great',    'greedy',  'green',    'groovy',
    'grumpy',   'guilty',   'hairy',   'handsome', 'happy',
    'healthy',  'heavy',    'helpful', 'high',     'huge',
    'hungry',   'icky',     'itchy',   'jazzy',    'jealous',
    'jolly',    'jumpy',    'kind',    'large',    'lazy',
    'lean',     'little',   'lively',  'lucky',    'macho',
    'magenta',  'magic',    'massive', 'meek',     'mighty',
    'mindless', 'nasty',    'needy',   'new',      'nice',
    'noisy',    'odd',      'old',     'orange',   'pretty',
    'prickly',  'proud',    'puffy',   'purple',   'quick',
    'quiet',    'rabid',    'rebel',   'red',      'righteous',
    'round',    'sad',      'sassy',   'scary',    'sedate',
    'shallow',  'short',    'silly',   'skillful', 'skinny',
    'sloppy',   'slow',     'small',   'smelly',   'sneaky',
    'snobby',   'strange',  'tacky',   'tall',     'tan',
    'tough',    'tricky',   'ugly',    'wicked',   'wise',
    'yellow', 'young', 'zany');
   verbs : array [1..128] of string =
     (
     'agree',   'applaud', 'argue',   'arise',   'arrive',
     'attack',  'awaken',  'bake',    'bathe',   'beg',
     'behave',  'bite',    'blink',   'blush',   'bounce',
     'breathe', 'burrow',  'buzz',    'charge',  'chat',
     'cheer',   'chew',    'chuckle', 'clap',    'cry',
     'dance',   'dig',     'dive',    'drink',   'eat',
     'feed',    'fight',   'flap',    'flee',    'float',
     'fly',     'gather',  'glow',    'grunt',   'hang',
     'hide',    'howl',    'hunt',    'itch',    'jog',
     'joke',    'jostle',  'jump',    'kick',    'kneel',
     'knit',    'krump',   'laugh',   'leap',    'leave',
     'look',    'lope',    'march',   'mix',     'moan',
     'nuzzle',  'observe', 'plan',    'play',    'plead',
     'point',   'pray',    'punch',   'push',    'race',
     'rejoice', 'relax',   'retire',  'return',  'roar',
     'rub',     'rumble',  'run',     'rush',    'sail',
     'scare',   'scrape',  'scratch', 'scream',  'scrub',
     'search',  'shake',   'shiver',  'shrink',  'shrug',
     'sigh',    'sing',    'sit',     'skip',    'slap',
     'sleep',   'slide',   'slip',    'smash',   'snore',
     'spar',    'speak',   'spit',    'stand',   'stare',
     'step',    'sting',   'stomp',   'stretch', 'strike',
     'study',   'stumble', 'swim',    'talk',    'think',
     'tickle',  'travel',  'trot',    'twist',   'wait',
     'walk',    'wander',  'watch',   'wave',    'whine',
     'whisper', 'work', 'wriggle'
     );

   adjectives_male_it : array [1..154] of string =
     ('orribili', 'cattivi', 'schivi', 'frenetici', 'grandi',
     'bizzarri', 'neri', 'blu', 'noiosi', 'muscolosi',
     'brillanti', 'irregolari', 'corpulenti', 'cauti', 'allegri',
     'freddi', 'paffuti', 'eleganti', 'goffi', 'freddi',
     'pazzi', 'raccapriccianti', 'coccoloni', 'sfolgoranti', 'sporchi',
     'vertiginosi', 'ubriachi', 'secchi', 'noiosi', 'polverosi',
     'desiderosi', 'cattivi', 'fantasiosi', 'veloci', 'grassi',
     'difettosi', 'impavidi', 'sudici', 'schiumosi', 'amichevoli',
     'divertenti', 'gentili', 'giganti', 'lucidi', 'buoni',
     'goffi', 'favolosi', 'avidi', 'verdi', 'eccitanti',
     'scontrosi', 'colpevoli', 'pelosi', 'belli', 'felici',
     'sani', 'pesanti', 'utili', 'alti', 'enormi',
     'affamati', 'appiccicosi', 'pruriginosi', 'jazzistici', 'gelosi',
     'gai', 'agitati', 'gentili', 'larghi', 'pigri',
     'magri', 'piccoli', 'vivaci', 'fortunati', 'macho',
     'cremisi', 'magici', 'massicci', 'miti', 'potenti',
     'senza pensieri', 'disgustosi', 'bisognosi', 'nuovi', 'simpatici',
     'rumorosi', 'strani', 'vecchi', 'arancioni', 'carini',
     'pungenti', 'fieri', 'gonfi', 'viola', 'veloci',
     'tranquilli', 'rabbiosi', 'ribelli', 'rossi', 'virtuosi',
     'tondi', 'tristi', 'impertinenti', 'spaventosi', 'sedati',
     'superficiali', 'corti', 'sciocchi', 'abili', 'magri',
     'sciatti', 'lenti', 'piccoli', 'puzzolenti', 'subdoli',
     'snob', 'strani', 'appiccicosi', 'alti', 'abbronzati',
     'spessi', 'difficili', 'brutti', 'malvagi', 'saggi',
     'gialli', 'giovani', 'buffi',

     'impertinenti', 'scontrosi', 'immortali', 'immorali',
     'garruli', 'meticolosi', 'miseri', 'pietosi', 'ignoranti',
     'studiosi', 'scivolosi', 'misteriosi', 'puliti',
     'penosi', 'pietosi', 'lisci', 'ruvidi', 'prepotenti',
     'pulciosi', 'garruli', 'zebrati', 'leopardati', 'mosci',
     'succubi', 'sereni', 'marini'
     );
   adjectives_female_it : array [1..154] of string =
     ('orribili', 'cattive', 'schive', 'frenetiche', 'grandi',
     'bizzarre', 'neri', 'blu', 'noiose', 'muscolose',
     'brillanti', 'irregolari', 'corpulente', 'caute', 'allegre',
     'fredde', 'paffute', 'eleganti', 'goffe', 'fredde',
     'pazze', 'raccapricciante', 'coccolone', 'sfolgoranto', 'sporche',
     'vertiginose', 'ubriache', 'secche', 'noiose', 'polverose',
     'desiderose', 'cattive', 'fantasiose', 'veloci', 'grasse',
     'difettose', 'impavidi', 'sudicie', 'schiumose', 'amichevoli',
     'divertenti', 'gentili', 'giganti', 'lucide', 'buone',
     'goffe', 'favolose', 'avide', 'verdi', 'eccitanti',
     'scontrose', 'colpevoli', 'pelose', 'belle', 'felici',
     'sane', 'pesanti', 'utili', 'alte', 'enormi',
     'affamate', 'appiccicose', 'pruriginose', 'jazzistiche', 'gelose',
     'gaie', 'agitate', 'gentili', 'larghe', 'pigre',
     'magre', 'piccole', 'vivaci', 'fortunate', 'macho',
     'cremisi', 'magiche', 'massiccie', 'miti', 'potenti',
     'senza pensieri', 'disgustose', 'bisognose', 'nuove', 'simpatiche',
     'rumorose', 'strane', 'vecchie', 'arancioni', 'carine',
     'pungenti', 'fiere', 'gonfie', 'viola', 'veloci',
     'tranquille', 'rabbiose', 'ribelli', 'rosse', 'virtuose',
     'tonde', 'tristi', 'impertinenti', 'spaventose', 'sedate',
     'superficiali', 'corte', 'sciocche', 'abili', 'magri',
     'sciatte', 'lente', 'piccole', 'puzzolenti', 'subdole',
     'snob', 'strane', 'appiccicose', 'alte', 'abbronzate',
     'spesse', 'difficili', 'brutte', 'malvagie', 'sagge',
     'gialle', 'giovani', 'buffe',
     'impertinenti', 'scontrose', 'immortali', 'immorali',
     'garrule', 'meticolose', 'misere', 'pietose', 'ignoranti',
     'studiose', 'scivolose', 'misteriose', 'pulite',
     'penose', 'pietose', 'liscie', 'ruvide', 'prepotenti',
     'pulciose', 'garrule', 'zebrate', 'leopardate', 'moscie',
     'succubi', 'serene', 'marine'
     );
   verbs_it : array [1..128] of string =
     (
     'concordano', 'applaudono', 'argomentano', 'sorgono', 'arrivano',
     'attaccano', 'risvegliano', 'cuociono', 'puliscono', 'mendicano',
     'girano', 'mordono', 'lampeggiano', 'arrossiscono', 'rimbalzano',
     'respirano', 'burrow', 'fischiettano', 'caricano', 'dialogano',
     'rallegrano', 'masticano', 'ridono', 'saltano', 'piangono',
     'danzano', 'scavano', 'nuotano', 'bevono', 'mangiano',
     'nutrono', 'combattono', 'ondeggiano', 'galleggiano', 'osservano',
     'volano', 'raccolgono', 'brillano', 'grugniscono', 'appendono',
     'nascondono', 'ululano', 'cacciano', 'prudono', 'mescolano',
     'scherzano', 'scaldano', 'pelano', 'calciano', 'pregano',
     'cuciono', 'ridono', 'rallentano', 'scavalcano', 'mollano',
     'guardano', 'amano', 'marciano', 'mescolano', 'mugolano',
     'pigolano', 'muggiscono', 'trasportano', 'giocano', 'supplicare',
     'pungono', 'chiedono', 'impegnano', 'spingono', 'corrono',
     'rallegrano', 'rilassano', 'ritirano', 'ritornano', 'ruggiscono',
     'strofinano', 'rimbombano', 'saltellano', 'intorpiscono', 'navigano',
     'spaventano', 'graffiano', 'feriscono', 'urlano', 'macchiano',
     'cercano', 'agitano', 'tremano', 'rimpiccioliscono', 'scrollano',
     'sospirano', 'cantano', 'seggono', 'saltano', 'schiaffeggiano',
     'dormono', 'aprono', 'scivolano', 'colpiscono', 'russare',
     'sparpagliano', 'parlano', 'sputano', 'stanno', 'fissano',
     'salgono', 'indovinano', 'chiedono', 'allungano', 'centrano',
     'studiano', 'inciampano', 'galleggiano', 'meditano', 'pensano',
     'solleticano', 'viaggiano', 'trottano', 'torcono', 'attendono',
     'camminano', 'vagano', 'vogano', 'ondeggiano', 'protestano',
     'sussurrano', 'lavorano', 'guizzano'
    );
begin
  Result := IntToStr(Random(100));

  if CompareText(aLanguageCode, 'EN') = 0 then
  begin
    Result := Result + '-' + adjectives[Random(High(adjectives)) + 1];
    Result := Result + '-' + nouns[Random(High(nouns)) + 1];
    Result := Result + '-' + verbs[Random(High(verbs)) + 1];
  end
  else if CompareText(aLanguageCode, 'IT') = 0 then
  begin
    if Random(2) = 0 then
    begin
      Result := Result + '-' + nouns_female_it[Random(High(nouns_female_it)) + 1];
      Result := Result + '-' + adjectives_female_it[Random(High(adjectives_female_it)) + 1];
    end
    else
    begin
      Result := Result + '-' + nouns_male_it[Random(High(nouns_male_it)) + 1];
      Result := Result + '-' + adjectives_male_it[Random(High(adjectives_male_it)) + 1];
    end;
    Result := Result + '-' + verbs_it[Random(High(verbs_it)) + 1];
  end
  else
    raise Exception.Create('Language ' + aLanguageCode + ' is not supported');

end;

// This function converts the integer value of file size into human readable form
// Taken from:
// http://stackoverflow.com/questions/1285979/delphi-function-to-display-number-of-bytes-as-windows-does
// http://forum.lazarus.freepascal.org/index.php?topic=13705.0
function BytesToHumanReadableString(const bytes: UInt64): string;
var
  B: byte;
  KB: word;
  MB: QWord;
  GB: QWord;
  TB: UInt64;
begin
  B  := 1; //byte
  KB := 1024 * B; //kilobyte
  MB := 1024 * KB; //megabyte
  GB := 1024 * MB; //gigabyte
  TB := 1024 * GB; //terabyte

  if bytes > TB then
    result := FormatFloat('#.## TB', bytes / TB)
  else
    if bytes > GB then
      result := FormatFloat('#.## GB', bytes / GB)
    else
      if bytes > MB then
        result := FormatFloat('#.## MB', bytes / MB)
      else
        if bytes > KB then
          result := FormatFloat('#.## KB', bytes / KB)
        else
          result := FormatFloat('#.## bytes', bytes) ;
end;


function ValidEmail(const email: String): boolean;
// Returns True if the email address is valid
// Author: Ernesto D'Spirito
const
  // Valid characters in an "atom"
  atom_chars = [#33..#255] - ['(', ')', '<', '>', '@', ',', ';', ':',
                              '\', '/', '"', '.', '[', ']', #127];
  // Valid characters in a "quoted-string"
  quoted_string_chars = [#0..#255] - ['"', #13, '\'];

  // Valid characters in a subdomain
  letters = ['A'..'Z', 'a'..'z'];
  letters_digits = ['0'..'9', 'A'..'Z', 'a'..'z'];
  //subdomain_chars = ['-', '0'..'9', 'A'..'Z', 'a'..'z'];
type
  States = (STATE_BEGIN, STATE_ATOM, STATE_QTEXT, STATE_QCHAR,
    STATE_QUOTE, STATE_LOCAL_PERIOD, STATE_EXPECTING_SUBDOMAIN,
    STATE_SUBDOMAIN, STATE_HYPHEN);
var
  State: States;
  i, n, subdomains: integer;
  c: char;
begin
  State := STATE_BEGIN;
  n := Length(email);
  i := 1;
  subdomains := 1;
  while (i <= n) do begin
    c := email[i];
    case State of
    STATE_BEGIN:
      if c in atom_chars then
        State := STATE_ATOM
      else if c = '"' then
        State := STATE_QTEXT
      else
        break;
    STATE_ATOM:
      if c = '@' then
        State := STATE_EXPECTING_SUBDOMAIN
      else if c = '.' then
        State := STATE_LOCAL_PERIOD
      else if not (c in atom_chars) then
        break;
    STATE_QTEXT:
      if c = '\' then
        State := STATE_QCHAR
      else if c = '"' then
        State := STATE_QUOTE
      else if not (c in quoted_string_chars) then
        break;
    STATE_QCHAR:
      State := STATE_QTEXT;
    STATE_QUOTE:
      if c = '@' then
        State := STATE_EXPECTING_SUBDOMAIN
      else if c = '.' then
        State := STATE_LOCAL_PERIOD
      else
        break;
    STATE_LOCAL_PERIOD:
      if c in atom_chars then
        State := STATE_ATOM
      else if c = '"' then
        State := STATE_QTEXT
      else
        break;
    STATE_EXPECTING_SUBDOMAIN:
      if c in letters then
        State := STATE_SUBDOMAIN
      else
        break;
    STATE_SUBDOMAIN:
      if c = '.' then begin
        inc(subdomains);
        State := STATE_EXPECTING_SUBDOMAIN
      end else if c = '-' then
        State := STATE_HYPHEN
      else if not (c in letters_digits) then
        break;
    STATE_HYPHEN:
      if c in letters_digits then
        State := STATE_SUBDOMAIN
      else if c <> '-' then
        break;
    end;
    inc(i);
  end;
  if i <= n then
    Result := False
  else
    Result := (State = STATE_SUBDOMAIN) and (subdomains >= 2);
end;

function ValidEmails(const emails: String; out normalizedEmails: String): boolean;
var
  stlst : TStringList;
  i : integer;
  s, sep : string;
const
  mailDelimiter : Char = ';';
begin
  Result := false;
  normalizedEmails:= '';
  sep := '';
  stlst := TStringList.Create;
  try
    stlst.Delimiter:= mailDelimiter;
    stlst.DelimitedText:= emails;
    for i := 0 to stlst.Count - 1 do
    begin
      s := Trim(LowerCase(stlst.Strings[i]));
      if s <> '' then
      begin
        if not ValidEmail(s) then
          exit;
        normalizedEmails:= normalizedEmails + sep + s;
        sep := mailDelimiter;
      end;
    end;
  finally
    stlst.Free;
  end;
  Result := (normalizedEmails <> '');
end;

function EncodeURIComponent(const aSrc: String): UTF8String;
const
  HexMap: UTF8String = '0123456789ABCDEF';

  function IsSafeChar(ch: Integer): Boolean;
  begin
    if (ch >= 48) and (ch <= 57) then Result := True    // 0-9
    else if (ch >= 65) and (ch <= 90) then Result := True  // A-Z
    else if (ch >= 97) and (ch <= 122) then Result := True  // a-z
    else if (ch = 33) then Result := True // !
    else if (ch >= 39) and (ch <= 42) then Result := True // '()*
    else if (ch >= 45) and (ch <= 46) then Result := True // -.
    else if (ch = 95) then Result := True // _
    else if (ch = 126) then Result := True // ~
    else Result := False;
  end;
var
  I, J: Integer;
  ASrcUTF8: UTF8String;
begin
  Result := '';    {Do not Localize}

  ASrcUTF8 := UTF8Encode(aSrc);
  // UTF8Encode call not strictly necessary but
  // prevents implicit conversion warning

  I := 1; J := 1;
  SetLength(Result, Length(ASrcUTF8) * 3); // space to %xx encode every byte
  while I <= Length(ASrcUTF8) do
  begin
    if IsSafeChar(Ord(ASrcUTF8[I])) then
    begin
      Result[J] := ASrcUTF8[I];
      Inc(J);
    end
    else if ASrcUTF8[I] = ' ' then
    begin
      Result[J] := '+';
      Inc(J);
    end
    else
    begin
      Result[J] := '%';
      Result[J+1] := HexMap[(Ord(ASrcUTF8[I]) shr 4) + 1];
      Result[J+2] := HexMap[(Ord(ASrcUTF8[I]) and 15) + 1];
      Inc(J,3);
    end;
    Inc(I);
  end;

  SetLength(Result, J-1);
end;

function EncodeSVGString(const aSrc: string): String;
var
  i : integer;
begin
  Result := '';
  for i := 1 to Length(aSrc) do
  begin
    if aSrc[i] = '''' then
      Result := Result + '&#x2032;'
    else if aSrc[i] = '"' then
      Result := Result + '&#x2033;'
    else if aSrc[i] = '&' then
      Result := Result + '&#x0026;'
    else if aSrc[i] = '-' then
      Result := Result + '&#x002D;'
    else if aSrc[i] = ',' then
      Result := Result + '&#x002C;'
    else if aSrc[i] = '.' then
      Result := Result + '&#x002E;'
    else if aSrc[i] = '/' then
      Result := Result + '&#x002F;'
    else if aSrc[i] = '!' then
      Result := Result + '&#x0021;'
    else if aSrc[i] = '#' then
      Result := Result + '&#x0023;'
    else if aSrc[i] = ' ' then
      Result := Result + '&#x0020;'
    else if aSrc[i] = '$' then
      Result := Result + '&#x0024;'
    else if aSrc[i] = '%' then
      Result := Result + '&#x0025;'
    else if aSrc[i] = '(' then
      Result := Result + '&#x0028;'
    else if aSrc[i] = ')' then
      Result := Result + '&#x0029;'
    else if aSrc[i] = '*' then
      Result := Result + '&#x002A;'
    else if aSrc[i] = '+' then
      Result := Result + '&#x002B;'
    else if aSrc[i] = '.' then
      Result := Result + '&#x002E;'
    else if aSrc[i] = ';' then
      Result := Result + '&#x003B;'
    else if aSrc[i] = '<' then
      Result := Result + '&#x003C;'
    else if aSrc[i] = '=' then
      Result := Result + '&#x003D;'
    else if aSrc[i] = '>' then
      Result := Result + '&#x003E;'
    else if aSrc[i] = '?' then
      Result := Result + '&#x003F;'
    else if aSrc[i] = '@' then
      Result := Result + '&#x0040;'
    else if aSrc[i] = '°' then
      Result := Result + '&#x00B0;'
    else
      Result := Result + aSrc[i];
  end;
end;

function InternalSanitizeSubstringForFileName(const aSubString: String; const aIgnoreDirectorySeparator : boolean): String;
var
  i : integer;
begin
  Result := '';
  for i := 1 to Length(aSubString) do
  begin
    if aSubString[i] = '<' then
      Result := Result + '_'
    else if aSubString[i] = '>' then
      Result := Result + '_'
    else if aSubString[i] = ':' then
      Result := Result + '_'
    else if aSubString[i] = '"' then
      Result := Result + '_'
    else if (aSubString[i] = '/') and ((not aIgnoreDirectorySeparator) or ('/' <> DirectorySeparator)) then
      Result := Result + '_'
    else if (aSubString[i] = '\') and ((not aIgnoreDirectorySeparator) or ('\' <> DirectorySeparator)) then
      Result := Result + '_'
    else if aSubString[i] = '|' then
      Result := Result + '_'
    else if aSubString[i] = '?' then
      Result := Result + '_'
    else if aSubString[i] = '*' then
      Result := Result + '_'
    else if aSubString[i] = '.' then
      Result := Result + '_'
    else if aSubString[i] = '''' then
      Result := Result + '_'
    else if Ord(aSubString[i]) <= 31 then
      Result := Result + '_'
    else
      Result := Result + aSubString[i];
  end;
end;

function EscapeSQLStringValue(const aSrc: String): String;
var
  i : integer;
  curOrd : word;
begin
  Result := '';
  for i := 1 to Length(aSrc) do
  begin
    // \n
    // \b
    // \r
    // \\
    // \%
    // \_
    // \t
    // \'
    // \"
    curOrd:= Ord(aSrc[i]);
    if curOrd = 10 then
      Result := Result + '\n'
    else if curOrd = 8 then
      Result := Result + '\b'
    else if curOrd = 13 then
      Result := Result + '\r'
    else if curOrd = 92 then
      Result := Result + '\\'
    else if curOrd = 37  then
      Result := Result + '\%'
    else if curOrd = 95 then
      Result := Result + '\_'
    else if curOrd = 9 then
      Result := Result + '\t'
    else if curOrd = 39 then
      Result := Result + '\'''
    else if curOrd = 34 then
      Result := Result + '\"'
    else
      Result := Result + aSrc[i];
  end
end;

(*
    https://docs.microsoft.com/it-it/windows/desktop/FileIO/naming-a-file#basic_naming_conventions

    Use almost any character in the current code page for a name, including Unicode characters and characters in the extended character set (128–255), except for the following:
        The following reserved characters are not allowed:
        < > : " / \ | ? *
        Characters whose integer representations are in the range from zero through 31 are not allowed.
        Any other character that the target file system does not allow.

    Do not use the following reserved device names for the name of a file: CON, PRN, AUX, NUL, COM1..COM9, LPT1..LPT9.
    Also avoid these names followed immediately by an extension; for example, NUL.txt is not recommended.

*)
function SanitizeFileName(const aSrc: String): String;
var
  tmp : String;
begin
  Result := '';
  tmp := ChangeFileExt(ExtractFileName(aSrc), '');
  Result := InternalSanitizeSubstringForFileName(tmp, true);
  tmp := ExtractFileDir(aSrc);
  if tmp <> '' then
    Result := IncludeTrailingPathDelimiter(tmp) + ChangeFileExt(Result, ExtractFileExt(aSrc))
  else
    Result := ChangeFileExt(Result, ExtractFileExt(aSrc));
end;

function SanitizeSubstringForFileName(const aSubString: String): String;
begin
  Result := InternalSanitizeSubstringForFileName(aSubString, false);
end;

function AddNumberToFileName(const aSrc: String; const aNumber: integer): String;
begin
  Result := AddSuffixToFileName(aSrc, '(' + IntToStr(aNumber) + ')');
end;

function AddSuffixToFileName(const aSrc: String; const aSuffix: String): String;
var
  dir, filename, ext: String;
begin
  dir := ExtractFileDir(aSrc);
  filename := ExtractFileName(aSrc);
  ext := ExtractFileExt(aSrc);
  filename := ChangeFileExt(filename, '');
  Result := IncludeTrailingPathDelimiter(dir) + filename + aSuffix + ext;
end;

procedure AddUTF8BOMToStream(aStream: TStream);
begin
  aStream.Write(UTF8BOM[0],3);
end;

function IsRunningAsRoot: boolean;
begin
{$IFDEF WINDOWS}
  Result := winutils.IsWindowsAdmin;
{$ENDIF}
{$IFDEF LINUX}
  // http://forum.lazarus.freepascal.org/index.php?topic=22454.0
  Result := (fpgeteuid = 0);
{$ENDIF}
end;

function CurrentProcessId: cardinal;
begin
{$IFDEF WINDOWS}
  Result := GetCurrentProcessID;
{$ELSE}
    {$IFDEF LINUX}
    Result := fpgetpid;
    {$ELSE}
    {$message error CurrentProcessId is not implemented on this platform!}
    Result := 0;
    {$ENDIF}
{$ENDIF}
end;

procedure RunConsoleApplicationAndGetOutput(const aCommand: string; const aParameters : array of string; out aOutputText: String);
var
  tmpProcess: TProcess;
  tmpStringList: TStringList;
begin
  tmpProcess := TProcess.Create(nil);
  try
    tmpProcess.Executable := aCommand;
    tmpProcess.Parameters.AddStrings(aParameters);
    tmpProcess.Options := tmpProcess.Options + [poWaitOnExit, poUsePipes];
    tmpProcess.Execute;
    tmpStringList := TStringList.Create;
    try
      tmpStringList.LoadFromStream(tmpProcess.Output);
      aOutputText := tmpStringList.Text;
    finally
      tmpStringList.Free;
    end;
  finally
    tmpProcess.Free;
  end;
end;

function GetTimeStampForFileName(const aInstant: TDateTime; const aAddTime : boolean = true): string;
var
  year, month, day : word;
  hour, minute, second, millisecond : word;
begin
  DecodeDate(aInstant, year, month, day);
  DecodeTime(aInstant, hour, minute, second, millisecond);
  Result := AddZerosFront(year, 4) + AddZerosFront(month, 2) + AddZerosFront(day, 2);
  if aAddTime then
    Result := Result  + '-' + AddZerosFront(hour, 2) + AddZerosFront(minute, 2) + AddZerosFront(second, 2);
end;

function DecodeTimeStampForFileName(const aTimestamp: String) : TDateTime;
var
  year, month, day, hours, minutes, seconds: word;
  lg : integer;
begin
  year := 0;
  month := 0;
  day := 0;
  hours := 0;
  minutes := 0;
  seconds := 0;
  lg := Length(aTimestamp);
  if lg >= 4 then
    year := StrToInt(Copy(aTimestamp, 1, 4));
  if lg >= 6 then
    month := StrToInt(RemoveZerosFromFront(Copy(aTimestamp, 5, 2)));
  if lg >= 8 then
    day := StrToInt(RemoveZerosFromFront(Copy(aTimestamp, 7, 2)));
  if (lg >= 9) and (aTimestamp[9] = '-') then
  begin
    if lg >= 11 then
      hours := StrToInt(Copy(aTimestamp, 10, 2));
    if lg >= 13 then
      minutes := StrToInt(Copy(aTimestamp, 12, 2));
    if lg >= 15 then
      seconds := StrToInt(Copy(aTimestamp, 14, 2));
    Result := EncodeDateTime(year, month, day, hours, minutes, seconds, 0);
  end
  else
    Result := EncodeDate(year, month, day);
end;

procedure EncodeFileToBase64(const aFullPathInputFile: String; out aOutputData: String);
var
  fi: TFileStream;
  fo: TStringStream;
  enc: TBase64EncodingStream;
begin
  fi := TFileStream.Create(aFullPathInputFile,fmOpenRead);
  try
    fo := TStringStream.Create('');
    try
      enc := TBase64EncodingStream.Create(fo);
      try
        enc.CopyFrom(fi, fi.Size);
      finally
        enc.Free;
      end;
      aOutputData:= fo.DataString;
    finally
      fo.Free;
    end;
  finally
    fi.Free;
  end;
end;

procedure DecodeBase64ToFile(const aInputData : String; const aFullPathOutputFile: String);
var
  fo: TFileStream;
  enc: TBase64DecodingStream;
  s: TStringStream;
begin
  fo := TFileStream.Create(aFullPathOutputFile, fmCreate);
  try
    s := TStringStream.Create(aInputData);
    try
      enc := TBase64DecodingStream.Create(s);
      try
        fo.CopyFrom(enc, enc.Size);
      finally
        enc.Free;
      end;
    finally
      s.Free;
    end;
  finally
    fo.Free;
  end;
end;

initialization
  Randomize;
end.
