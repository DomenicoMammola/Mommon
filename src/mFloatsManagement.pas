// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFloatsManagement;

interface

function DoublesAreEqual(const aValue1, aValue2 : double; const aFloatingPointDigits : integer): boolean; overload;
function DoublesAreEqual(const aValue1, aValue2 : double): boolean; overload;

function DoublesAreNotEqual(const aValue1, aValue2 : double; const aFloatingPointDigits : integer): boolean; overload;
function DoublesAreNotEqual(const aValue1, aValue2 : double): boolean; overload;

function DoubleIsLessThan(const aValue1, aValue2 : double) : boolean; overload;
function DoubleIsLessThan(const aValue1, aValue2 : double; const aFloatingPointDigits : integer): boolean; overload;

function DoubleIsLessOrEqual(const aValue1, aValue2 : double) : boolean; overload;
function DoubleIsLessOrEqual(const aValue1, aValue2 : double; const aFloatingPointDigits : integer): boolean; overload;

function SafeDiv (numer, denom: double): double;

procedure SetDefaultDecimalNumbers (aFloatingPointDigits : integer);

function RoundDoubleToStandardPrecision (const aValue : double) : double;

implementation

uses
  Math;

var
  DefaultDecimalNumbers : integer;
  DefaultCompareValue : double;



function DoublesAreEqual(const aValue1, aValue2 : double; const aFloatingPointDigits : integer): boolean; overload;
var
  CompareValue : double;
begin
  CompareValue := Power(10, -1 * aFloatingPointDigits) - Power(10, -1 * (aFloatingPointDigits + 1)) - Power(10, -1 * (aFloatingPointDigits + 2));
  Result := (Abs(aValue1 - aValue2) <= CompareValue);
end;

function DoublesAreEqual(const aValue1, aValue2 : double): boolean; overload;
begin
  Result := (Abs(aValue1 - aValue2) <= DefaultCompareValue);
end;

procedure SetDefaultDecimalNumbers (aFloatingPointDigits : integer);
begin
  if (DefaultDecimalNumbers <> aFloatingPointDigits) then
  begin
    DefaultCompareValue := Power(10, -1 * aFloatingPointDigits) - Power(10, -1 * (aFloatingPointDigits + 1)) - Power(10, -1 * (aFloatingPointDigits + 2));
    DefaultDecimalNumbers := aFloatingPointDigits;
  end;
end;

function RoundDoubleToStandardPrecision(const aValue: double): double;
begin
  Result := RoundTo(aValue, -1 * DefaultDecimalNumbers);
end;

function DoublesAreNotEqual(const aValue1, aValue2: double; const aFloatingPointDigits: integer): boolean;
begin
  Result := not DoublesAreEqual(aValue1, aValue2, aFloatingPointDigits);
end;

function DoublesAreNotEqual(const aValue1, aValue2: double): boolean;
begin
  Result := not DoublesAreEqual(aValue1, aValue2);
end;

function DoubleIsLessThan(const aValue1, aValue2: double): boolean;
begin
  Result := (not DoublesAreEqual(aValue1, aValue2)) and (aValue1 < aValue2);
end;

function DoubleIsLessThan(const aValue1, aValue2: double; const aFloatingPointDigits: integer): boolean;
begin
  Result := (not DoublesAreEqual(aValue1, aValue2, aFloatingPointDigits)) and (aValue1 < aValue2);
end;

function DoubleIsLessOrEqual(const aValue1, aValue2: double): boolean;
begin
  Result := DoublesAreEqual(aValue1, aValue2) or (aValue1 < aValue2);
end;

function DoubleIsLessOrEqual(const aValue1, aValue2: double; const aFloatingPointDigits: integer): boolean;
begin
  Result := DoublesAreEqual(aValue1, aValue2, aFloatingPointDigits) or (aValue1 < aValue2);
end;

function SafeDiv (numer, denom: double): double;
begin
  if (denom = 0) then
    result := 0
  else
    result:= numer/denom;
end;


initialization
  SetDefaultDecimalNumbers(5);

end.
