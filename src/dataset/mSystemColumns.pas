// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)


unit mSystemColumns;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DB;

function GetSystemFieldName (aOriginalFieldName : String) : String;
function IsSystemField (aField : TField) : boolean; overload;

implementation

uses
  mFieldNames;

function GetSystemFieldName(aOriginalFieldName: String): String;
begin
  Result := PREFIX_SYSTEM_FIELDS + aOriginalFieldName;
end;

function IsSystemField (aField : TField) : boolean;
begin
  Result := (aField.FieldName[1] = PREFIX_SYSTEM_FIELDS);
  Result := Result or (Pos(SEPARATOR_FIELDS_FROM_INTERNAL_REFERENCE + PREFIX_SYSTEM_FIELDS, aField.FieldName) > 0);
  Result := Result or (Pos(SEPARATOR_FIELDS_FROM_JOIN + PREFIX_SYSTEM_FIELDS, aField.FieldName) > 0);
end;


end.
