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

function TryToConvertToDouble(aValue : string; var aOutValue : Double ): boolean;
function IsNumeric(aValue: string; const aAllowFloat: Boolean): Boolean;

implementation

uses
  SysUtils;

function TryToConvertToDouble(aValue : string; var aOutValue : Double): boolean;
var
  tmpValueDouble : double;
  tmp : String;
begin
  Result := TryStrToFloat(aValue, tmpValueDouble);
  if Result then
    aOutValue := tmpValueDouble
  else
  begin
    if SysUtils.FormatSettings.DecimalSeparator = '.' then
      tmp := StringReplace(aValue, ',', '.', [rfReplaceAll])
    else
      tmp := StringReplace(aValue, '.', ',', [rfReplaceAll]);
    Result := TryStrToFloat(tmp, tmpValueDouble);
    if Result then
      aOutValue := tmpValueDouble
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

end.
