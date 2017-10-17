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

function DoublesAreEqual(const aValue1, aValue2 : double; const aDecimalNumbers : integer): boolean; overload;
function DoublesAreEqual(const aValue1, aValue2 : double): boolean; overload;

function DoubleIsLessThan(const aValue1, aValue2 : double) : boolean; overload;
function DoubleIsLessThan(const aValue1, aValue2 : double; const aDecimalNumbers : integer): boolean; overload;

function SafeDiv (numer, denom: double): double;

procedure SetDefaultDecimalNumbers (aDecimalNumbers : integer);

implementation

uses
  Math;

var
  DefaultDecimalNumbers : integer;
  DefaultCompareValue : double;



function DoublesAreEqual(const aValue1, aValue2 : double; const aDecimalNumbers : integer): boolean; overload;
var
  CompareValue : double;
begin
  CompareValue := Power(10, -1 * aDecimalNumbers) - Power(10, -1 * (aDecimalNumbers + 1)) - Power(10, -1 * (aDecimalNumbers + 2));
  Result := (Abs(aValue1 - aValue2) <= CompareValue);
end;

function DoublesAreEqual(const aValue1, aValue2 : double): boolean; overload;
begin
  Result := (Abs(aValue1 - aValue2) <= DefaultCompareValue);
end;

procedure SetDefaultDecimalNumbers (aDecimalNumbers : integer);
begin
  if (DefaultDecimalNumbers <> aDecimalNumbers) then
  begin
    DefaultCompareValue := Power(10, -1 * aDecimalNumbers) - Power(10, -1 * (aDecimalNumbers + 1)) - Power(10, -1 * (aDecimalNumbers + 2));
    DefaultDecimalNumbers := aDecimalNumbers;
  end;
end;

function DoubleIsLessThan(const aValue1, aValue2: double): boolean;
begin
  Result := (not DoublesAreEqual(aValue1, aValue2)) and (aValue1 < aValue2);
end;

function DoubleIsLessThan(const aValue1, aValue2: double; const aDecimalNumbers: integer): boolean;
begin
  Result := (not DoublesAreEqual(aValue1, aValue2, aDecimalNumbers)) and (aValue1 < aValue2);
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
