// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mStringSimilarity;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes;

// Inspired by Simon White algorithm http://www.catalysoft.com/articles/StrikeAMatch.html
// but with some changes:
// * the nested loops *must* be "bigger" than "smaller", if not you can get results higher than 1 and the result depends on the order of the two string parameters
// * as the original algorithm does not work well with strings like "PAUL" against "PAULA", added an extra algorithm which checks similar chars in similar relative positions
function CompareStrings (const aStr1, aStr2 : String; const aIgnorePunctuation : boolean; const aCheckScrambledWords : boolean = false) : double;

implementation

uses
  SysUtils, Math,
  mMathUtility;


const punctuationChars : set of Char = ['.', ':', '?', '!', '_', '^', '-', '\', '/', ';', ',', '''', '"'];

procedure LetterPairs (const aStr : String; aListOfPairs : TStringList);
var
  i : integer;
begin
  for i := 1 to Length(aStr) - 1 do
    aListOfPairs.Add(Copy(aStr, i, 2));
end;

procedure WordLetterPairs(const aStr : String; aListOfPairs : TStringList);
var
  tmpList : TStringList;
  i : integer;
begin
  tmpList := TStringList.Create;
  try
    tmpList.Delimiter:= ' ';
    tmpList.DelimitedText:= Trim(aStr);
    for i := 0 to tmpList.Count -1 do
    begin
      if Trim(tmpList.Strings[i]) <> '' then
        LetterPairs(Trim(tmpList.Strings[i]), aListOfPairs);
    end;
  finally
    tmpList.Free;
  end;
end;

function AdjustString (const aStr : String; const aIgnorePunctuation : boolean) : String;
var
  tmp : String;
  i : integer;
begin
  tmp := Trim(aStr);
  if aIgnorePunctuation then
  begin
    Result := '';
    for i := 1 to Length(tmp) do
    begin
      if not (tmp[i] in punctuationChars) then
        Result := Result + tmp[i];
    end;
  end
  else
    Result := tmp;
end;

function UndoAccent(const aStr : Char) : String;
var
  lc : String;
begin
  Result := aStr;
  lc := LowerCase(Result);

  if Pos(lc,'àáâãäå') > 0 then
    Result := 'a'
  else if Pos(lc, 'èéêë') > 0 then
    Result := 'e'
  else if Pos(lc, 'ìíîï') > 0 then
    Result := 'i'
  else if Pos(lc, 'òóôõö') > 0 then
    Result := 'o'
  else if Pos(lc, 'ùúûü') > 0 then
    Result := 'u'
  else if Pos(lc, 'ýÿ') > 0 then
    Result := 'y';

  if lc <> aStr then
    Result := Uppercase(Result);
end;

function MatchChars (const aStr1, aStr2 : String; const aIgnorePunctuation : boolean): double;
var
  lun1, lun2, bigLength, smallLength : integer;
  bigString, smallString : String;
  width, p, i, k, matching : integer;
  match : boolean;
begin
  Result := 0;

  lun1 := Length(aStr1);
  lun2 := Length(aStr2);

  if (lun1 = 0) or (lun2 = 0) then
    exit;

  if lun1 < lun2 then
  begin
    smallString := aStr1;
    bigString := aStr2;
    smallLength := lun1;
    bigLength := lun2;
  end
  else
  begin
    smallString := aStr2;
    bigString := aStr1;
    smallLength := lun2;
    bigLength := lun1;
  end;

  width := bigLength div smallLength;
  matching := 0;
  for i := 1 to smallLength do
  begin
    p := (i * width) - 1;
    match := false;
    for k := p to p + 2 do
    begin
      if (k >= 1) and (k <= bigLength) then
        match := match or (CompareText(bigString[k], smallString[i]) = 0) or
         (aIgnorePunctuation and (UndoAccent(bigString[k]) =  UndoAccent(smallString[i])));
    end;
    if match then
      inc(matching);
  end;

  Result := (smallLength / bigLength) - ((smallLength - matching) * (1 / bigLength));
end;

procedure SeparateWords (const aStr : String; aWordsList : TStringList);
var
  k : integer;
  lastWord : String;
begin
  lastWord := '';

  for k := 1 to Length(aStr) do
  begin
    if (aStr[k] in punctuationChars) or (aStr[k] = ' ') then
    begin
      if lastWord <> '' then
      begin
        aWordsList.Add(lastWord);
        lastWord := '';
      end;
    end
    else
      lastWord := lastWord + aStr[k];
  end;
  if lastWord <> '' then
    aWordsList.Add(lastWord);
end;

function MatchScrambledWords (const aStr1, aStr2: String) : double;
var
  list1, list2 : TStringList;
  maxValue, step : double;
  i, w, maxCount : integer;

begin
  Result := 0;
  list1 := TStringList.Create;
  list2 := TStringList.Create;
  try
    SeparateWords(aStr1, list1);
    SeparateWords(aStr2, list2);

    maxCount := max(list1.Count, list2.Count);
    maxValue := 1 - ((1 / maxCount) * Abs(list1.Count - list2.Count));

    step := maxValue / maxCount;


    i := list1.Count - 1;
    w := list2.Count - 1;

    while (i >= 0) do
    begin
      if list1.Strings[i] = list2.Strings[w] then
      begin
        Result := Result + step;
        list1.Delete(i);
        list2.Delete(w);
        dec(i);
        dec(w);
      end
      else
        dec(w);

      if w < 0 then
      begin
        w := list2.Count - 1;
        dec(i);
      end;
    end;

    for i := 0 to min(list1.Count, list2.Count) - 1 do
      Result := Result + (step * CompareStrings(list1.Strings[i], list2.Strings[i], false, false));

    Result := Min(1, RoundToExt(Result, rmHalfRoundAwayFromZero, 6));
  finally
    list2.Free;
    list1.Free;
  end;
end;

function CompareStrings (const aStr1, aStr2 : String; const aIgnorePunctuation : boolean; const aCheckScrambledWords : boolean = false) : double;
var
  tmpList1, tmpList2 : TStringList;
  intersection, increment, penalty : double;
  i, j, l1, l2: integer;
  tmpStr1, tmpStr2 : String;
begin
  Result := 0;

  tmpStr1 := AdjustString(aStr1, aIgnorePunctuation);
  tmpStr2 := AdjustString(aStr2, aIgnorePunctuation);
  if (trim(tmpStr1) = '') then
  begin
    if (trim(tmpStr2) = '') then
      Result := 1;
    exit;
  end
  else
  begin
    if (trim(tmpStr2) = '') then
      exit;
  end;

  if tmpStr1 = tmpStr2 then
  begin
    Result := 1;
    exit;
  end;

  tmpList1 := TStringList.Create;
  tmpList2 := TStringList.Create;
  try
    if Length(tmpStr1) >= Length(tmpStr2) then
    begin
      WordLetterPairs(tmpStr1, tmpList1);
      WordLetterPairs(tmpStr2, tmpList2);
    end
    else
    begin
      WordLetterPairs(tmpStr2, tmpList1);
      WordLetterPairs(tmpStr1, tmpList2);
    end;
    intersection := 0;
    //union := tmpList1.Count + tmpList2.Count;
    l1 := tmpList1.Count;
    l2 := tmpList2.Count;

    penalty := 0;
    for i := 0 to tmpList1.Count - 1 do
    begin
      j := 0;
      increment := 1 - penalty;
      while j < tmpList2.Count do
      begin
        if CompareText(tmpList1.Strings[i], tmpList2.Strings[j]) = 0 then
        begin
          intersection := intersection + increment;
          increment := 1;
          tmpList2.Delete(j);
        end
        else
        begin
          inc(j);
          increment := increment - (1 / l2);
          penalty := penalty + (1 / (l1 * l2 * 3));
        end;
      end;
    end;
  finally
    tmpList1.Free;
    tmpList2.Free;
  end;
  if (l1 > 0) or (l2 > 0) then
    Result := (1 / Max(l1, l2)) * intersection;
  Result := Max(Result, MatchChars(tmpStr1, tmpStr2, aIgnorePunctuation));

  if aCheckScrambledWords then
    Result := Max(Result, MatchScrambledWords(aStr1, aStr2));
end;

end.
