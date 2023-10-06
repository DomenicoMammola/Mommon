// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mDataFieldsUtility;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DB;

  function FieldTypeIsInteger(const aFieldType : TFieldType): boolean;
  function FieldTypeIsTime(const aFieldType : TFieldType): boolean;
  function FieldTypeIsDate(const aFieldType : TFieldType): boolean;
  function FieldTypeIsDateTime(const aFieldType : TFieldType): boolean;
  function FieldTypeIsFloat(const aFieldType : TFieldType) : boolean;
  function FieldTypeIsPascalDouble(const aFieldType : TFieldType): boolean;
  function FieldTypeIsString(const aFieldType : TFieldType) : boolean;
  function FieldTypeIsBoolean(const aFieldType : TFieldType) : boolean;

implementation

  function FieldTypeIsInteger(const aFieldType : TFieldType): boolean;
  begin
    Result := aFieldType in [ftInteger, ftSmallint, ftLargeint];
  end;

  function FieldTypeIsTime(const aFieldType : TFieldType): boolean;
  begin
    Result := aFieldType = ftTime;
  end;

  function FieldTypeIsDate(const aFieldType : TFieldType): boolean;
  begin
    Result := aFieldType = ftDate;
  end;

  function FieldTypeIsDateTime(const aFieldType : TFieldType): boolean;
  begin
    Result := (aFieldType in [ftDateTime, ftTimeStamp]);
  end;

  function FieldTypeIsFloat(const aFieldType : TFieldType) : boolean;
  begin
    Result := aFieldType in [ftFloat, ftFMTBcd, ftCurrency];
  end;

  function FieldTypeIsPascalDouble(const aFieldType : TFieldType): boolean;
  begin
    Result := FieldTypeIsFloat(aFieldType) or FieldTypeIsDate(aFieldType) or FieldTypeIsTime(aFieldType) or FieldTypeIsDateTime(aFieldType);
  end;

  function FieldTypeIsString(const aFieldType : TFieldType) : boolean;
  begin
    Result := aFieldType in [ftString, ftWideString, ftMemo, ftWideMemo, ftGuid];
  end;

  function FieldTypeIsBoolean(const aFieldType: TFieldType): boolean;
  begin
    Result := aFieldType in [ftBoolean];
  end;

end.
