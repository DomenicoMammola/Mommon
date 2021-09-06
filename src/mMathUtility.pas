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
  TRoundingMethod = (rmHalfRoundDown, rmHalfRoundUp, rmHalfRoundTowardsZero, rmHalfRoundAwayFromZero, rmHalfRoundToEven, rmBankerRounding, rmHalfRoundToOdd, rmCeil);

// https://en.wikipedia.org/wiki/Rounding
// http://www.eetimes.com/document.asp?doc_id=1274485
// http://ec.europa.eu/economy_finance/publications/pages/publication1224_en.pdf
function RoundToExt (const aValue : double; const aRoundingMethod : TRoundingMethod; const Digits: integer) : double;
function GetFractionalPartDigits(const aValue: double): integer;

function TryToConvertToDouble(aValue: string; out aOutValue: Double): boolean;
function TryToConvertToInteger(aValue: string; out aOutValue: Integer): boolean;
function TryToConvertToInt64(aValue: string; out aOutValue: Int64): boolean;
function IsNumeric(const aValue: string; const aAllowFloat, aAllowSigns: Boolean): Boolean;
function ExtractFirstInteger(aValue: string; out aOutValue: Integer): boolean;

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
      {$IFDEF FPC}
      tmp := DelChars(aValue, ',');
      {$ELSE}
      tmp := StringReplace(aValue, ',', '', [rfReplaceAll]);
      {$ENDIF}
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
      {$IFDEF FPC}
      tmp := DelChars(aValue, '.');
      {$ELSE}
      tmp := StringReplace(aValue, '.', '', [rfReplaceAll]);
      {$ENDIF}
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

function TryToConvertToInteger(aValue: string; out aOutValue: Integer): boolean;
var
  tmpValueInt : integer;
  tmp : String;
  i, curpos, ln : integer;
begin
  Result := TryStrToInt(aValue, tmpValueInt);
  if Result then
    aOutValue := tmpValueInt
  else
  begin
    curpos := 4;
    tmp := '';
    ln := length(aValue);
    for i := ln downto 1 do
    begin
      if (aValue[i] = SysUtils.FormatSettings.ThousandSeparator) then
      begin
        if (ln - i + 1) = curpos then
          curpos := curpos + 4
        else
          exit;
      end
      else
        tmp := aValue[i] + tmp;
    end;
    Result := TryStrToInt(tmp, tmpValueInt);
    if Result then
    begin
      aOutValue := tmpValueInt;
      exit;
    end;
  end;
end;


function TryToConvertToInt64(aValue: string; out aOutValue: Int64): boolean;
var
  tmpValueInt : Int64;
  tmp : String;
  i, curpos, ln : integer;
begin
  Result := TryStrToInt64(aValue, tmpValueInt);
  if Result then
    aOutValue := tmpValueInt
  else
  begin
    curpos := 4;
    tmp := '';
    ln := length(aValue);
    for i := ln downto 1 do
    begin
      if (aValue[i] = SysUtils.FormatSettings.ThousandSeparator) then
      begin
        if (ln - i + 1) = curpos then
          curpos := curpos + 4
        else
          exit;
      end
      else
        tmp := aValue[i] + tmp;
    end;
    Result := TryStrToInt64(tmp, tmpValueInt);
    if Result then
    begin
      aOutValue := tmpValueInt;
      exit;
    end;
  end;
end;

function IsNumeric(const aValue: string; const aAllowFloat, aAllowSigns: Boolean): Boolean;
var
  i, lg : integer;
  tmpDouble : double;
  numCommas, numDots : integer;
begin
  Result := false;
  lg := Length(aValue);
  if lg = 0 then
    exit;

  if aAllowSigns then
  begin
    if ((aValue[1] in ['-', '+']) and (lg = 1)) then
      exit;
    if (not (aValue[1] in ['0'..'9','-', '+'])) then
      exit;
  end
  else
    if (not (aValue[1] in ['0'..'9'])) then
       exit;

  if not aAllowFloat then
  begin
    for i := 2 to lg do
    begin
      if not (aValue[i] in ['0'..'9'])  then
        exit;
    end;
  end
  else
  begin
    if not (aValue[lg] in ['0'..'9'])  then
      exit;
    numCommas := 0;
    numDots := 0;
    for i := lg downto 2 do
    begin
      if aValue[i] = '.' then
      begin
        inc (numDots);
        if (numDots > 1) or (numCommas > 0) then
          exit;
      end
      else if aValue[i] = ',' then
      begin
        inc (numCommas);
        if (numCommas > 1) or (numDots > 0) then
          exit;
      end
      else
      begin
        if not (aValue[i] in ['0'..'9'])  then
          exit;
      end;
    end;
    if not TryToConvertToDouble(aValue, tmpDouble) then
      exit;
  end;

  Result := true;
end;

function ExtractFirstInteger(aValue: string; out aOutValue: Integer): boolean;
var
  i : integer;
  candidate: String;
begin
  Result := false;
  candidate := '';
  for i := 1 to Length(aValue) do
  begin
    if IsNumeric(aValue[i], false, false) then
      candidate := candidate + aValue[i]
    else if candidate <> '' then
      break;
  end;
  if candidate <> '' then
    Result := TryToConvertToInteger(candidate, aOutValue);
end;

function RoundToExt(const aValue: double; const aRoundingMethod: TRoundingMethod; const Digits: integer): double;
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
  else if (aRoundingMethod = rmCeil) then
  begin
    factor := round(power(10, Digits));
    Result := Ceil(AValue * factor) / factor;
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

function GetFractionalPartDigits(const aValue: double): integer;
var
  str : string;
  i : integer;
begin
  Result := 0;

  str := FloatToStr(aValue);

  for i := Length(str) downto 1 do
  begin
    if (str[i]= '.') or (str[i]=',') then
    begin
      Result := Length(str) - i;
      exit;
    end;
  end;
end;

end.
