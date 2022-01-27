// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mArrays;

interface

uses
  Variants;

type
  TmArrayOfVariants = array of Variant;


function ArrayOfConstsToArrayOfVariants (aSourceArray : array of const) : TmArrayOfVariants;

implementation

uses
  SysUtils, mUtility;

function ArrayOfConstsToArrayOfVariants (aSourceArray : array of const) : TmArrayOfVariants;
var
  k, sourceLenght: Integer;
  arr : TmArrayOfVariants;
begin
  sourceLenght := Length(aSourceArray);
  SetLength(arr, sourceLenght);
  for k := 0 to sourceLenght - 1 do
    arr[k] := VarRecToVariant(aSourceArray[k]);
  Result := arr;
end;

end.
