// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mIMOUtility;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

resourcestring
  SErrorIMONumberWrongLength = 'Wrong length. Must be 7 digits.';
  SErrorIMONumberWrongCheckDigit = 'Wrong check digit. It should be: ';
  SErrorMMSIWrongLength = 'Wrong length. Must be 9 digits.';
  SErrorMMSWrongFirstCharacter = 'Wrong first digit. Must be a number from 2 to 7.';


// IMO number = International Maritime Organization
// http://www.imo.org/en/OurWork/MSAS/Pages/IMO-identification-number-scheme.aspx
// https://gcaptain.com/imo-numbers/
// http://tarkistusmerkit.teppovuori.fi/coden.htm
function IsIMONumberValid(const aIMONumber: Integer; out aErrorMessage: String): boolean;

// MMSI = Maritime Mobile Service Identity
// https://en.wikipedia.org/wiki/Maritime_Mobile_Service_Identity
function IsMMSIValid(const aMMSI : Integer; out aErrorMessage: String) : boolean;

// Check if the id is a valid MMSI or a valid IMO number
function IsVesselIdValid (const aVesselId : Integer; out aErrorMessage : String) : boolean;

implementation

uses
  sysutils;

function IsIMONumberValid(const aIMONumber: Integer; out aErrorMessage: String): boolean;
var
  str, CheckDigit : String;
  i, tot : integer;
begin
  Result := false;
  aErrorMessage:= '';

  str := IntToStr(aIMONumber);
  if Length(str) <> 7 then
  begin
    aErrorMessage:= SErrorIMONumberWrongLength;
    exit;
  end;

  (*
  The six information digits to be checked are weighted from left to right by 7, 6, 5, 4, 3 and 2.
  Products are added up.
  The sum is divided by 10. The remainder is the check digit
  *)
  tot := 0;
  for i := 1 to 6 do
  begin
    tot := tot + (StrToInt(str[i]) * (7 - i + 1));
  end;
  CheckDigit := IntToStr(tot);
  CheckDigit := Copy(CheckDigit, Length(CheckDigit), 1);

  if CheckDigit <> str[7] then
  begin
    aErrorMessage:= SErrorIMONumberWrongCheckDigit + CheckDigit;
    exit;
  end;

  Result := true;
end;

function IsMMSIValid(const aMMSI: Integer; out aErrorMessage: String): boolean;
var
  str : String;
begin
  Result := false;
  aErrorMessage:= '';

  str := IntToStr(aMMSI);
  if Length(str) <> 9 then
  begin
    aErrorMessage:= SErrorMMSIWrongLength;
    exit;
  end;
  if not(str[1] in ['2'..'7']) then
  begin
    aErrorMessage:= SErrorMMSWrongFirstCharacter;
    exit;
  end;
  Result := true;
end;

function IsVesselIdValid(const aVesselId: Integer; out aErrorMessage: String): boolean;
var
  str : String;
begin
  str := IntToStr(aVesselId);
  if Length(str) = 9 then
    Result := IsMMSIValid(aVesselId, aErrorMessage)
  else
    Result := IsIMONumberValid(aVesselId, aErrorMessage);
end;

end.
