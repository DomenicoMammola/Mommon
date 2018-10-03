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

// Simon White algorithm
// http://www.catalysoft.com/articles/StrikeAMatch.html
function CompareStrings (aStr1, aStr2 : String; const aIgnorePunctuation : boolean) : double;

implementation

uses
  SysUtils;

procedure LetterPairs (const aStr : String; aListOfPairs : TStringList);
var
  i : integer;
begin
  for i := 1 to Length(aStr) - 1 do
  begin
    aListOfPairs.Add(Copy(aStr, i, 2));
  end;
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

function CompareStrings (aStr1, aStr2 : String; const aIgnorePunctuation : boolean) : double;
var
  tmpList1, tmpList2 : TStringList;
  intersection, union : integer;
  i, j : integer;
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
    WordLetterPairs(tmpStr1, tmpList1);
    WordLetterPairs(tmpStr2, tmpList2);
    intersection := 0;
    union := tmpList1.Count + tmpList2.Count;

    for i := 0 to tmpList1.Count - 1 do
    begin
      j := 0;
      while j < tmpList2.Count do
      begin
        if CompareText(tmpList1.Strings[i], tmpList2.Strings[j]) = 0 then
        begin
          inc (intersection);
          tmpList2.Delete(j);
        end
        else
          inc(j);
      end;
    end;
  finally
    tmpList1.Free;
    tmpList2.Free;
  end;
  Result := (2.0 * intersection) / union;
end;

end.
