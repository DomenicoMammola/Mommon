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

uses
  Classes, SysUtils, Variants, {$ifdef windows}Windows, {$ifdef fpc}InterfaceBase,{$endif} Forms,{$endif}
  mIntList, mDoubleList;

const
  TheDayWhenTimeStarted = 730120; //01/01/2000 (starting from 01/01/01)

type
  TListSortCompare = function (Item1, Item2: Pointer): Integer of object; // cloned from LCLProc but with 'of object'

function GenerateRandomIdString : string; overload;
function GenerateRandomIdString(aLength : integer): string; overload;
function CreateUniqueIdentifier : String; // actually a GUID without parentheses, to be used as unique indentifier in db tables

function AddZerosFront (aValue : integer; aLength : integer) : String;
function RemoveZerosFromFront (aValue : String) : String;

function DateTimeToSeconds(const aDateTime : TDateTime; const aTheDayWhenTimeStarted : integer = TheDayWhenTimeStarted) : integer;
function SecondsToDateTime(const aSeconds : integer; const aTheDayWhenTimeStarted : integer = TheDayWhenTimeStarted): TDateTime;

// try to convert the input text from the user as a date value, if it fails it returns a blank string
// user can edit date as ddmmyy or ddmmyyyy or dmyy or with separators like '/', '\', '-', ....
function TryToUnderstandDateString(const aInputString : String; out aValue : TDateTime) : boolean;

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
procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare);

{$IFDEF FPC}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
{$ENDIF}

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

function GetCPUCores : integer;
function GetApplicationLocalDataFolder (const aApplicationSubDir : string) : String;
function GetApplicationDataFolder (const aApplicationSubDir : string) : String;
function GetOSUser : string;
procedure FlashInWindowsTaskbar(const aFlashEvenIfActive : boolean);

implementation

uses
  DateUtils,
  {$ifdef windows}shlobj,{$else}LazUTF8,{$endif}
  {$ifdef linux}initc, ctypes,{$endif}
  mMathUtility;

{$ifdef linux}
function sysconf(i:cint):clong;cdecl;external name 'sysconf';
{$endif}

function AddZerosFront (aValue : integer; aLength : integer) : String;
var
  i, l : integer;
begin
  Result := IntToStr(aValue);
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
    while (aValue[i] = '0') and (i <= l) do
      inc(i);
    if i <= l then
      Result := Copy(aValue, i, 9999)
    else
      Result := '';
  end
  else
    Result := aValue;
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
  l, idx : integer;
  tmp,  sep : string;
  dString, mString, yString : string;
  day, month, year : integer;
  canTry : boolean;
begin
  Result := false;
  canTry := false;

  tmp := aInputString;
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
    if IsNumeric(dString, false) and IsNumeric(mString, false) and IsNumeric(yString, false) then
    begin
      day := StrToInt(dString);
      month := StrToInt(mString);
      year := StrToInt(yString);

      if (month >=1) and (month <= 12) and (year >= 0) and (day >= 1) and (day <= 31) then
      begin
        if year < 100 then
          year := 2000 + year;
        if day <= DaysInAMonth(year, month) then
        begin
          aValue := EncodeDate(year, month, day);
          Result := true;
          exit;
        end;
      end;
    end;
  end;
end;

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
begin
  len := Length(aText);
  if len > Length(aPassword) then
    len := Length(aPassword);
  SetLength(result, len);
  for i := 1 to len do
    result[i] := Chr(Ord(aText[i]) xor Ord(aPassword[i]));
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
      aList.Add(round(tmpDate));
    end;
  end
  else
  begin
    tmpDate := VarAsType(aValue, vardate);
    aList.Add(round(tmpDate));
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

{$ENDIF}

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

  if aVal1 = aVal2 then
    Result := 0
  else if aVal1 < aVal2 then
    Result := -1
  else
    Result := 1;
end;

function SafeVariantToInteger(aValue: variant; aDefaultValue : integer): integer;
begin
  if VarIsNull(aValue) or VarIsEmpty(aValue) or (not VarIsOrdinal(aValue)) then
    Result := aDefaultValue
  else
    Result := aValue;
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

{$ifdef windows}

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

function GetCPUCores : integer;
var
  Info: TSystemInfo;
begin
  GetSystemInfo(Info);
  Result := Info.dwNumberOfProcessors;
end;
{$else}

{$ifdef linux}
function GetCPUCores : integer;
begin
  // http://forum.lazarus.freepascal.org/index.php?topic=33125.0
  Result := sysconf(83);
end;
{$else}
{$ifdef Darwin}
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

{$else}
function GetCPUCores : integer;
begin
  raise Exception.Create('GetCPUCores missing implementation for this platform');
end;
{$endif}
{$endif}
{$endif}

function GetApplicationLocalDataFolder(const aApplicationSubDir: string): String;
var
  tmpDir : string;
  {$ifdef windows}
  AppDataPath: Array[0..MaxPathLen] of Char; //http://wiki.lazarus.freepascal.org/Windows_Programming_Tips
  {$endif}
begin
  {$ifdef windows}
  AppDataPath:='';
  SHGetSpecialFolderPath(0,AppDataPath,CSIDL_LOCAL_APPDATA,false);
  tmpDir:= AppDataPath;
  Result := IncludeTrailingPathDelimiter(tmpDir) + aApplicationSubDir;
  {$else}
    {$ifdef linux}
    tmpDir:= GetEnvironmentVariableUTF8('HOME');
    Result := IncludeTrailingPathDelimiter(tmpDir) + '.' + aApplicationSubDir;
    {$else}
    raise Exception.Create('GetApplicationLocalDataFolder missing implementation for this platform');
    {$endif}
  {$endif}
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetApplicationDataFolder(const aApplicationSubDir: string): String;
var
  tmpDir : string;
  {$ifdef windows}
  AppDataPath: Array[0..MaxPathLen] of Char; //http://wiki.lazarus.freepascal.org/Windows_Programming_Tips
  {$endif}
begin
  {$ifdef windows}
  AppDataPath:='';
  SHGetSpecialFolderPath(0,AppDataPath,CSIDL_APPDATA,false);
  tmpDir:= AppDataPath;
  Result := IncludeTrailingPathDelimiter(tmpDir) + aApplicationSubDir;
  {$else}
    {$ifdef linux}
    // GetAppConfigDir(false);
    raise Exception.Create('GetApplicationLocalDataFolder missing implementation for this platform');
    {$else}
    raise Exception.Create('GetApplicationLocalDataFolder missing implementation for this platform');
    {$endif}
  {$endif}
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetOSUser: string;
begin
  Result := SysUtils.GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := SysUtils.GetEnvironmentVariable('USER');
end;

procedure FlashInWindowsTaskbar(const aFlashEvenIfActive : boolean);
begin
  {$ifdef windows}
  // http://forum.lazarus.freepascal.org/index.php?topic=33574.0
  If aFlashEvenIfActive or (not Application.Active) Then
    FlashWindow({$ifdef fpc}WidgetSet.AppHandle{$else}Application.Handle{$endif}, True);
  {$endif}
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


initialization
  Randomize;
end.
