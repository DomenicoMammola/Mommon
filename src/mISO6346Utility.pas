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
  SErrorContainerWrongLength = 'Wrong length. Must be 11 characters.';
  SErrorContainerWrongOwnerCode = 'Owner code must be composed by 3 letters.';
  SErrorContainerWrongCategoryIdentifier ='Wrong category identifier. Must be U or J or Z.';
  SErrorContainerWrongSerialNumber = 'Only digits are allowed in serial number.';
  SErrorContainerCheckDigitNotANumber = 'Check digit is not a digit.';
  SErrorContainerWrongCheckDigit = 'Wrong check digit. It should be: ';
  SErrorMRNWrongLength = 'Wrong length. Must be 18 characters.';
  SErrorMRNWrongYear = 'Wrong number of year. Must be composed by 2 digits.';
  SErrorMRNWrongCountryCode = 'Wrong country code.';
  SErrorMRNCheckDigitNotANumber = 'Check digit is not a digit.';
  SErrorMRNWrongCheckDigit = 'Wrong check digit. It should be: ';
  SErrorMRNUnallowedCharacter = 'Unallowed character.';

// --------------
// CONTAINER CODE
// --------------
// http://www.gvct.co.uk/2011/09/how-is-the-check-digit-of-a-container-calculated/
// https://en.wikipedia.org/wiki/ISO_6346
// https://www.bic-code.org/bic-codes/
function CalculateContainerCheckDigit (const aContainerCode : String): integer;
function IsContainerCodeValid(const aContainerCode: String; out aErrorMessage: String): boolean;

// -------------------------------
// MOVEMENT REFERENCE NUMBER - MRN
// -------------------------------
// http://www.tribalgod.nl/brinkster/Berekening_controlecijfer_MRN-GRN.doc
function CalculateMRNCheckDigit (const aMRNCode : String): integer;
function IsMRNCodeValid(const aMRNCode: String; out aErrorMessage: String): boolean;


implementation

uses
  math, sysutils;

var
  dictionary: Array['A'..'Z'] of Integer;
  _dictionaryInizialized : boolean;

procedure InizializeDictionary;
var
  c: Char;
  i: integer;
begin
  if not _dictionaryInizialized then
  begin
    _dictionaryInizialized:= true;
    i := 10;
    for c := Low(dictionary) to High(dictionary) do
    begin
      dictionary[c] := i;
      inc(i);
      if (i = 11) or (i = 22) or (i = 33) then
        inc(i);
    end;
  end;
end;

function CalculateContainerCheckDigit(const aContainerCode: String): integer;
var
  acc, ax: Extended;
  i, s: integer;
begin
  InizializeDictionary;

  acc:= 0;
  s:= 0;

  for i:= 1 to 10 do
  begin
    if i = 1 then
      s := 1
    else
      s:= s * 2;
    if i < 5 then
      ax := (dictionary[aContainerCode[i]]*s)
    else
      ax := (StrToInt(aContainerCode[i])*s);
    acc:= acc + ax;
  end;
  Result := (Floor(acc) mod 11) mod 10;
end;

function IsContainerCodeValid(const aContainerCode: String; out aErrorMessage: String): boolean;
var
  OwnerCode, CategoryIdentifier, SerialNumberStr, CheckDigitStr : string;
  GoodCheckDigit : integer;
  i : integer;
begin
  Result := false;

  if not (Length(aContainerCode) = 11) then
  begin
    aErrorMessage:= SErrorContainerWrongLength;
    exit;
  end;

  OwnerCode:= Copy(aContainerCode, 1, 3);
  for i := 1 to 3 do
  begin
    if not (OwnerCode[i] in ['A'..'Z']) then
    begin
      aErrorMessage:= SErrorContainerWrongOwnerCode;
      exit;
    end;
  end;

  CategoryIdentifier:= aContainerCode[4];
  if (CategoryIdentifier <> 'U') and (CategoryIdentifier <> 'J') and (CategoryIdentifier <> 'Z') then
  begin
    aErrorMessage:= SErrorContainerWrongCategoryIdentifier;
    exit;
  end;

  SerialNumberStr:= Copy(aContainerCode, 5, 6);
  if not IsNumeric(SerialNumberStr, false) then
  begin
    aErrorMessage:= SErrorContainerWrongSerialNumber;
    exit;
  end;

  CheckDigitStr:= Copy(aContainerCode, 11, 1);
  if not IsNumeric(CheckDigitStr, false) then
  begin
    aErrorMessage:= SErrorContainerCheckDigitNotANumber;
    exit;
  end;

  GoodCheckDigit:= CalculateContainerCheckDigit(aContainerCode);
  if StrToInt(CheckDigitStr) <> GoodCheckDigit then
  begin
    aErrorMessage:= SErrorContainerWrongCheckDigit + IntToStr(GoodCheckDigit) + '.';
    exit;
  end;

  Result := true;
end;

function CalculateMRNCheckDigit(const aMRNCode: String): integer;
var
  acc, ax: Extended;
  i, s: integer;
begin
  InizializeDictionary;

  acc:= 0;
  s:= 0;
  for i:= 1 to 17 do
  begin
    if i = 1 then
      s := 1
    else
      s:= s * 2;
    if (aMRNCode[i] in ['0'..'9']) then
      ax := (StrToInt(aMRNCode[i])*s)
    else
      ax := (dictionary[aMRNCode[i]]*s);
    acc:= acc + ax;
  end;
  Result := (Floor(acc) mod 11) mod 10;
end;

function IsMRNCodeValid(const aMRNCode: String; out aErrorMessage: String): boolean;
var
  YearStr, CountryCode, CheckDigitStr: String;
  i, GoodCheckDigit: integer;
begin
  Result := false;

  if not (Length(aMRNCode) = 18) then
  begin
    aErrorMessage:= SErrorMRNWrongLength;
    exit;
  end;

  YearStr:= Copy(aMRNCode, 1, 2);
  CountryCode:= Copy(aMRNCode, 3, 2);

  if not IsNumeric(YearStr, false) then
  begin
    aErrorMessage:= SErrorMRNWrongYear;
    exit;
  end;

  if (not (CountryCode[1] in ['A'..'Z'])) or
    (not (CountryCode[2] in ['A'..'Z'])) then
  begin
    aErrorMessage:= SErrorMRNWrongCountryCode;
    exit;
  end;

  CheckDigitStr:= Copy(aMRNCode, 18, 1);
  if not IsNumeric(CheckDigitStr, false) then
  begin
    aErrorMessage:= SErrorMRNCheckDigitNotANumber;
    exit;
  end;

  for i:= 5 to 10 do
  begin
    if not (aMRNCode[i] in ['0'..'9', 'A'..'Z']) then
    begin
      aErrorMessage:= SErrorMRNUnallowedCharacter;
      exit;
    end;
  end;

  GoodCheckDigit:= CalculateMRNCheckDigit(aMRNCode);
  if StrToInt(CheckDigitStr) <> GoodCheckDigit then
  begin
    aErrorMessage:= SErrorMRNWrongCheckDigit + IntToStr(GoodCheckDigit) + '.';
    exit;
  end;

  Result := true;
end;

initialization
  _dictionaryInizialized:= false;

end.
