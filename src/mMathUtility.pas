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

// CREDITS: https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Pascal
function IsNumeric(Value: string; const AllowFloat: Boolean): Boolean;

implementation

uses
  SysUtils;

// CREDITS: https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Pascal
function IsNumeric(Value: string; const AllowFloat: Boolean): Boolean;
var
  ValueInt: Integer;
  ValueFloat: Extended;
  ErrCode: Integer;
begin
// Check for integer: Val only accepts integers when passed integer param
Value := SysUtils.Trim(Value);
Val(Value, ValueInt, ErrCode);
Result := ErrCode = 0;      // Val sets error code 0 if OK
if not Result and AllowFloat then
    begin
    // Check for float: Val accepts floats when passed float param
    Val(Value, ValueFloat, ErrCode);
    Result := ErrCode = 0;    // Val sets error code 0 if OK
    end;
end;

end.
