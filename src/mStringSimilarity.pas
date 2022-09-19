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
function CompareStrings (const aStr1, aStr2 : String; const aIgnorePunctuation : boolean) : double;

implementation

uses
  SysUtils, Math;

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
      if not (tmp[i] in ['.', ':', '?', '!', '_', '^', '-', '\', '/', ';', ',', '''', '"']) then
        Result := Result + tmp[i];
    end;
  end
  else
    Result := tmp;
end;

function MatchChars (const aStr1, aStr2 : String): double;
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
        match := match or (CompareText(bigString[k], smallString[i]) = 0);
    end;
    if match then
      inc(matching);
  end;

  Result := (smallLength / bigLength) - ((smallLength - matching) * (1 / bigLength));
end;

function CompareStrings (const aStr1, aStr2 : String; const aIgnorePunctuation : boolean) : double;
var
  tmpList1, tmpList2 : TStringList;
  intersection, increment, penalty : double;
  i, j, l1, l2: integer;
  tmpStr1, tmpStr2 : String;
begin
  tmpStr1 := AdjustString(aStr1, aIgnorePunctuation);
  tmpStr2 := AdjustString(aStr2, aIgnorePunctuation);
  if (trim(tmpStr1) = '') then
  begin
    if (trim(tmpStr2) = '') then
      Result := 1
    else
      Result := 0;
    exit;
  end
  else
  begin
    if (trim(tmpStr2) = '') then
    begin
      Result := 0;
      exit;
    end;
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
  Result := (1 / Max(l1, l2)) * intersection;
  Result := Max(Result, MatchChars(tmpStr1, tmpStr2));
//  Result := ((2.0 * intersection) / union);
end;

end.
