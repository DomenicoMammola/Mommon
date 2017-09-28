// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mMathUtility;

interface

uses
  Math, strutils;

type
  TRoundingMethod = (rmHalfRoundDown, rmHalfRoundUp, rmHalfRoundTowardsZero, rmHalfRoundAwayFromZero, rmHalfRoundToEven, rmBankerRounding, rmHalfRoundToOdd);

// https://en.wikipedia.org/wiki/Rounding
// http://www.eetimes.com/document.asp?doc_id=1274485
// http://ec.europa.eu/economy_finance/publications/pages/publication1224_en.pdf
function RoundToExt (aValue : double; aRoundingMethod : TRoundingMethod; const Digits: integer) : double;

function TryToConvertToDouble(aValue: string; out aOutValue: Double): boolean;
function TryToConvertToInteger(aValue: string; out aOutValue: integer): boolean;
function IsNumeric(aValue: string; const aAllowFloat: Boolean): Boolean;

implementation

uses
  SysUtils;

function TryToConvertToDouble(aValue : string; out aOutValue : Double): boolean;
var
  tmpValueDouble : double;
  tmp : String;
  posComma, posDot : integer;
begin
  Result := TryStrToFloat(aValue, tmpValueDouble, SysUtils.FormatSettings);
  if Result then
    aOutValue := tmpValueDouble
  else
  begin
    posComma := Pos(',', aValue);
    posDot := Pos('.', aValue);
    if (posComma > 0) and (posDot > posComma) then // 1,547.25
    begin
      tmp := DelChars(aValue, ',');
      tmp := StringReplace(tmp, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
      Result := TryStrToFloat(tmp, tmpValueDouble, SysUtils.FormatSettings);
      if Result then
      begin
        aOutValue := tmpValueDouble;
        exit;
      end;
    end;
    if (posDot > 0) and (posComma > posDot) then // 1.547,25
    begin
      tmp := DelChars(aValue, '.');
      tmp := StringReplace(tmp, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
      Result := TryStrToFloat(tmp, tmpValueDouble, SysUtils.FormatSettings);
      if Result then
      begin
        aOutValue := tmpValueDouble;
        exit;
      end;
    end;
    if (posComma > 0) and (posDot <= 0) then // 1547,25
    begin
      tmp := StringReplace(aValue, ',', SysUtils.FormatSettings.DecimalSeparator, [rfReplaceAll]);
      Result := TryStrToFloat(tmp, tmpValueDouble, SysUtils.FormatSettings);
      if Result then
      begin
        aOutValue := tmpValueDouble;
        exit;
      end;
    end;
    if (posDot > 0) and (posComma <= 0) then // 1547.25
    begin
      tmp := StringReplace(aValue, '.', SysUtils.FormatSettings.DecimalSeparator, [rfReplaceAll]);
      Result := TryStrToFloat(tmp, tmpValueDouble, SysUtils.FormatSettings);
      if Result then
      begin
        aOutValue := tmpValueDouble;
        exit;
      end;
    end;
  end;
end;

function TryToConvertToInteger(aValue: string; out aOutValue: integer): boolean;
var
  tmpValueInt : integer;
  tmp : String;
  posDot, posDotR, posComma, posCommaR : integer;
begin
  Result := TryStrToInt(aValue, tmpValueInt);
  if Result then
    aOutValue := tmpValueInt
  else
  begin
    posComma := Pos(',', aValue);
    posCommaR := RPos(',', aValue);
    posDot := Pos('.', aValue);
    posDotR := RPos('.', aValue);

    if (SysUtils.FormatSettings.ThousandSeparator = '.') and (posDot = posDotR) then // 1.023
    begin
      tmp := StringReplace(aValue, '.', '', [rfReplaceAll]);
      Result := TryStrToInt(tmp, tmpValueInt);
      if Result then
      begin
        aOutValue := tmpValueInt;
        exit;
      end;
    end;
    if (SysUtils.FormatSettings.ThousandSeparator = ',') and (posComma = posCommaR) then // 1,023
    begin
      tmp := StringReplace(aValue, ',', '', [rfReplaceAll]);
      Result := TryStrToInt(tmp, tmpValueInt);
      if Result then
      begin
        aOutValue := tmpValueInt;
        exit;
      end;
    end;
  end;
end;

function IsNumeric(aValue: string; const aAllowFloat: Boolean): Boolean;
var
  tmpInt : integer;
  tmpDouble : double;
begin
  if aAllowFloat then
    Result := TryToConvertToDouble(aValue, tmpDouble)
  else
    Result := TryStrToInt(aValue, tmpInt);
end;

function RoundToExt(aValue: double; aRoundingMethod: TRoundingMethod; const Digits: integer): double;
var
  i1, i2, factor : Int64;
  tmpInt, tmpInt2 : Int64;

  function RoundToNearest : double;
  begin
    if i2 < 5 then
      Result := i1 / factor
    else
      Result := (i1 + Sign(aValue)) /factor;
  end;

begin

  if aRoundingMethod  = rmHalfRoundDown then
  begin
    if Sign(aValue) = 1 then
      Result:= RoundToExt(AValue, rmHalfRoundTowardsZero, Digits)
    else
      Result := RoundToExt(AValue, rmHalfRoundAwayFromZero, Digits);
  end
  else if (aRoundingMethod = rmHalfRoundUp) then
  begin
    if Sign(aValue) = 1 then
      Result:= RoundToExt(AValue, rmHalfRoundAwayFromZero, Digits)
    else
      Result := RoundToExt(AValue, rmHalfRoundTowardsZero, Digits);
  end
  else
  begin
    factor := round(power(10, Digits));

    i1 := trunc(AValue * factor);
    tmpInt:=  (factor * 10);
    tmpInt2 := (i1 * 10);
    i2 := Abs(trunc(Math.RoundTo(AValue * tmpInt, 0)) - tmpInt2);

    if (aRoundingMethod = rmHalfRoundTowardsZero) then
    begin
      if i2 = 5 then
        Result := i1 / factor
      else
        Result := RoundToNearest;
    end
    else if (aRoundingMethod = rmHalfRoundAwayFromZero) then
    begin
      if i2 = 5 then
        Result := (i1 + Sign(aValue)) /factor
      else
        Result := RoundToNearest;
    end
    else if (aRoundingMethod = rmHalfRoundToEven) or (aRoundingMethod = rmBankerRounding) then
    begin
      if i2 = 5 then
      begin
        if not odd(i1) then
          Result := i1 / factor
        else
          Result := (i1 + Sign(aValue)) / factor;
      end
      else
        Result := RoundToNearest;
    end
    else if (aRoundingMethod = rmHalfRoundToOdd) then
    begin
      if i2 = 5 then
      begin
        if odd(i1) then
          Result := i1 / factor
        else
          Result := (i1 + Sign(aValue)) / factor;
      end
      else
        Result := RoundToNearest;
    end
    else
      raise Exception.Create ('Unsupported rounding method');
  end;
end;

end.
