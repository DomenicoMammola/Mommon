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
  mDataProviderInterfaces, mNamingConventions;

function SerializeDataProviderToJson (const aDataProvider : IVDDataProvider; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention): String;

implementation

uses
  Classes, Contnrs, SysUtils, Variants,
  mDataProviderFieldDefs, mUtility, mDataProviderSerializerClasses;

//https://restfulapi.net/json-data-types/

//const
//  ISO8601FormatExtendedUTC='yyyy"-"mm"-"dd"T"hh":"mm":"ss"Z"';
//  ISO8601FormatExtendedUTC_Date='yyyy"-"mm"-"dd"Z"';
//  ISO8601FormatExtendedUTC_Time='"T"hh":"mm":"ss"Z"';


function SerializeDataProviderToJson(const aDataProvider: IVDDataProvider; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention): String;
var
  i, k : integer;
  curDatum : IVDDatum;
  fields : TSerializedFields;
  value : Variant;
begin
  Result := '[';

  if aDataProvider.Count = 0 then
    Result := Result + ' ]'
  else
  begin
    fields := TSerializedFields.Create;
    try
      GetSerializedFields(aDataProvider, fields, aSourceNamingConvention, aDestinationNamingConvention);

      for i := 0 to aDataProvider.Count - 1 do
      begin
        if i > 0 then
          Result := Result + ',{'
        else
          Result := Result + '{';
        curDatum := aDataProvider.GetDatum(i);
        for k := 0 to fields.Count - 1 do
        begin
          if k > 0 then
            Result := Result + ',';

          Result := Result + '"' + fields.Get(k).SerializedFieldName + '":';
          value := curDatum.GetPropertyByFieldName(fields.Get(k).OriginalFieldName);
          if VarIsNull(value) then
            Result := Result + 'null'
          else
          begin
            case fields.Get(k).DataType of
              vftInteger : Result := Result + IntToStr(value);
              vftBoolean : if value then Result := Result + 'true' else Result := Result + 'false';
              vftFloat, vftCurrency : Result := Result + FormatFloat('#.#', value);
              vftDate : Result := Result + '"' + DateToJsonString(VarToDateTime(value)) + '"';
              vftTime : Result := Result + '"' + TimeToJsonString(VarToDateTime(value)) + '"';
              vftDateTime, vftTimeStamp : Result := Result + '"' + DateTimeToJsonString(VarToDateTime(value)) + '"';
            else
              Result := Result + '"' + EscapeStringValue(VarToStr(value), 'json') + '"';
            end;
          end;
        end;
        Result := Result + '}';
      end;
      Result := Result + ']';

    finally
      fields.Free;
    end;
  end;
end;


end.
