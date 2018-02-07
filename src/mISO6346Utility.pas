// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mISO6346Utility;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  mMathUtility;

resourcestring
  SErrorWrongLength = 'Wrong length. Must be 11 characters.';
  SErrorWrongOwnerCode = 'Owner code must be composed by 3 letters.';
  SErrorWrongCategoryIdentifier ='Wrong category identifier. Must be U or J or Z.';
  SErrorWrongSerialNumber = 'Only digits are allowed in serial number.';
  SErrorCheckDigitNotANumber = 'Check digit is not a digit.';
  SErrorWrongCheckDigit = 'Wrong check digit. It should be: ';


// http://www.gvct.co.uk/2011/09/how-is-the-check-digit-of-a-container-calculated/
// https://en.wikipedia.org/wiki/ISO_6346
// https://www.bic-code.org/bic-codes/
function CalculateContainerCheckDigit (const aContainerCode : String): integer;
function IsContainerCodeValid(const aContainerCode: String; var aErrorMessage: String): boolean;

implementation

uses
  math, sysutils;

function CalculateContainerCheckDigit(const aContainerCode: String): integer;
var
  dict: Array['A'..'Z'] of Integer;
  c : Char;
  acc, ax: Extended;
  i, s: integer;
begin
  i := 10;
  for c := Low(dict) to High(dict) do
  begin
    dict[c] := i;
    inc(i);
    if (i = 11) or (i = 22) or (i = 33) then
      inc(i);
  end;

  acc:= 0;
  s:= 0;

  for i:= 1 to 10 do
  begin
    s:= s*2;
    if s < 1 then
      s:= 1;
    if i < 5 then
      ax := (dict[aContainerCode[i]]*s)
    else
      ax := (StrToInt(aContainerCode[i])*s);
    acc:= acc + ax;
  end;
  Result := (Floor(acc) mod 11) mod 10;
end;

function IsContainerCodeValid(const aContainerCode: String; var aErrorMessage: String): boolean;
var
  OwnerCode, CategoryIdentifier, SerialNumberStr, CheckDigitStr : string;
  GoodCheckDigit : integer;
  i : integer;
begin
  Result := false;

  if not (Length(aContainerCode) = 11) then
  begin
    aErrorMessage:= SErrorWrongLength;
    exit;
  end;

  OwnerCode:= Copy(aContainerCode, 1, 3);
  for i := 1 to 3 do
  begin
    if (OwnerCode[i] <= 'A') or (OwnerCode[i] >= 'Z') then
    begin
      aErrorMessage:= SErrorWrongOwnerCode;
      exit;
    end;
  end;

  CategoryIdentifier:= Copy(aContainerCode, 4, 1);
  if (CategoryIdentifier <> 'U') and (CategoryIdentifier <> 'J') and (CategoryIdentifier <> 'Z') then
  begin
    aErrorMessage:= SErrorWrongCategoryIdentifier;
    exit;
  end;

  SerialNumberStr:= Copy(aContainerCode, 5, 6);
  if not IsNumeric(SerialNumberStr, false) then
  begin
    aErrorMessage:= SErrorWrongSerialNumber;
    exit;
  end;

  CheckDigitStr:= Copy(aContainerCode, 11, 1);
  if not IsNumeric(CheckDigitStr, false) then
  begin
    aErrorMessage:= SErrorCheckDigitNotANumber;
    exit;
  end;

  GoodCheckDigit:= CalculateContainerCheckDigit(aContainerCode);
  if StrToInt(CheckDigitStr) <> GoodCheckDigit then
  begin
    aErrorMessage:= SErrorWrongCheckDigit + IntToStr(GoodCheckDigit) + '.';
    exit;
  end;

  Result := true;
end;

end.
