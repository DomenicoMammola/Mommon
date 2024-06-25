// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)  

unit mDataProviderJsonSerializer;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  mDataProviderInterfaces, mNamingConventions, mDataProviderSerializerClasses;


function SerializeDataProviderToJson (const aDataProvider : IVDDataProvider; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention;
  const aSerializeDateFunction : TSerializeDateValueFunction; const aSerializeTimeFunction : TSerializeTimeValueFunction;
  const aSerializeDateTimeFunction : TSerializeDateTimeValueFunction; const aAllowedFieldCheckFunction : TAllowFieldInclusionFunction = nil): String;
function SerializeDatumToJson(const aDatum : IVDDatum; const aFields : TSerializedFields;
  const aSerializeDateFunction : TSerializeDateValueFunction; const aSerializeTimeFunction : TSerializeTimeValueFunction;
  const aSerializeDateTimeFunction : TSerializeDateTimeValueFunction): String; overload;
function SerializeDatumToJson(const aDatum : IVDDatum; const aDataProvider : IVDDataProvider; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention;
  const aSerializeDateFunction : TSerializeDateValueFunction; const aSerializeTimeFunction : TSerializeTimeValueFunction;
  const aSerializeDateTimeFunction : TSerializeDateTimeValueFunction; const aAllowedFieldCheckFunction : TAllowFieldInclusionFunction): String; overload;

implementation

uses
  Classes, Contnrs, SysUtils, Variants,
  mDataProviderFieldDefs, mUtility;

//https://restfulapi.net/json-data-types/

//const
//  ISO8601FormatExtendedUTC='yyyy"-"mm"-"dd"T"hh":"mm":"ss"Z"';
//  ISO8601FormatExtendedUTC_Date='yyyy"-"mm"-"dd"Z"';
//  ISO8601FormatExtendedUTC_Time='"T"hh":"mm":"ss"Z"';


function SerializeDataProviderToJson(const aDataProvider: IVDDataProvider; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention; const aSerializeDateFunction : TSerializeDateValueFunction; const aSerializeTimeFunction : TSerializeTimeValueFunction;
  const aSerializeDateTimeFunction : TSerializeDateTimeValueFunction; const aAllowedFieldCheckFunction : TAllowFieldInclusionFunction = nil): String;
var
  i : integer;
  fields : TSerializedFields;
begin
  Result := '[';

  if aDataProvider.Count = 0 then
    Result := Result + ' ]'
  else
  begin
    fields := TSerializedFields.Create;
    try
      GetSerializedFields(aDataProvider, fields, aSourceNamingConvention, aDestinationNamingConvention, aAllowedFieldCheckFunction);

      for i := 0 to aDataProvider.Count - 1 do
      begin
        if i > 0 then
          Result := Result + ',{'
        else
          Result := Result + '{';
        Result := Result + SerializeDatumToJson (aDataProvider.GetDatum(i), fields, aSerializeDateFunction, aSerializeTimeFunction, aSerializeDateTimeFunction);
        Result := Result + '}';
      end;
      Result := Result + ']';

    finally
      fields.Free;
    end;
  end;
end;

function SerializeDatumToJson(const aDatum: IVDDatum; const aFields : TSerializedFields; const aSerializeDateFunction : TSerializeDateValueFunction; const aSerializeTimeFunction : TSerializeTimeValueFunction;
  const aSerializeDateTimeFunction : TSerializeDateTimeValueFunction): String;
var
  k : integer;
  value : Variant;
begin
  Result := '';
  for k := 0 to aFields.Count - 1 do
  begin
    if k > 0 then
      Result := Result + ',';

    Result := Result + '"' + aFields.Get(k).SerializedFieldName + '":';
    value := aDatum.GetPropertyByFieldName(aFields.Get(k).OriginalFieldName);
    if VarIsNull(value) then
      Result := Result + 'null'
    else
    begin
      case aFields.Get(k).DataType of
        vftInteger : Result := Result + IntToStr(value);
        vftBoolean : if value then Result := Result + 'true' else Result := Result + 'false';
        vftFloat, vftCurrency : Result := Result + FormatFloat('#.#', value);
        vftDate : if Assigned(aSerializeDateFunction) then Result := Result + '"' + aSerializeDateFunction(VarToDateTime(value)) + '"' else Result := Result + '"' + DateToJsonString(VarToDateTime(value)) + '"';
        vftTime : if Assigned(aSerializeTimeFunction) then Result := Result + '"' + aSerializeTimeFunction(VarToDateTime(value)) + '"' else Result := Result + '"' + TimeToJsonString(VarToDateTime(value)) + '"';
        vftDateTime, vftTimeStamp : if Assigned(aSerializeDateTimeFunction) then Result := Result + '"' + aSerializeDateTimeFunction(VarToDateTime(value)) + '"' else Result := Result + '"' + DateTimeToJsonString(VarToDateTime(value)) + '"';
      else
        Result := Result + '"' + EscapeStringValue(VarToStr(value), 'json') + '"';
      end;
    end;
  end;
end;

function SerializeDatumToJson(const aDatum: IVDDatum; const aDataProvider: IVDDataProvider; const aSourceNamingConvention, aDestinationNamingConvention: TmNamingConvention;
  const aSerializeDateFunction : TSerializeDateValueFunction; const aSerializeTimeFunction : TSerializeTimeValueFunction;
  const aSerializeDateTimeFunction : TSerializeDateTimeValueFunction; const aAllowedFieldCheckFunction : TAllowFieldInclusionFunction): String;
var
  fields : TSerializedFields;
begin
  fields := TSerializedFields.Create;
  try
    GetSerializedFields(aDataProvider, fields, aSourceNamingConvention, aDestinationNamingConvention, aAllowedFieldCheckFunction);
    Result := SerializeDatumToJson(aDatum, fields, aSerializeDateFunction, aSerializeTimeFunction, aSerializeDateTimeFunction);
  finally
    fields.Free;
  end;
end;


end.
