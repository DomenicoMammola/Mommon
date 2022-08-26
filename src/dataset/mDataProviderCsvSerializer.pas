// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDataProviderCsvSerializer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  mDataProviderInterfaces, mNamingConventions;

function SerializeDataProviderToCsv (const aDataProvider : IVDDataProvider; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention;
  const aFractionPartDigits : byte;
  const aQuoteChar : string = '"'; const aDelimiter : Char = ','): String;


implementation

uses
  Classes, Variants, SysUtils,
  mCSV, mDataProviderSerializerClasses, mDataProviderFieldDefs, mUtility;

function SerializeDataProviderToCsv (const aDataProvider : IVDDataProvider; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention;
  const aFractionPartDigits : byte;
  const aQuoteChar : string = '"'; const aDelimiter : Char = ','): String;
var
  CSVBuilder : TmCSVBuilder;
  fields : TSerializedFields;
  i, k : integer;
  stream : TStringStream;
  curDatum : IVDDatum;
  value : Variant;
  formatString : String;
begin
  Result := '';
  stream := TStringStream.Create;
  CSVBuilder := TmCSVBuilder.Create;
  fields := TSerializedFields.Create;
  try
    GetSerializedFields(aDataProvider, fields, aSourceNamingConvention, aDestinationNamingConvention);
    CSVBuilder.Stream := stream;
    CSVBuilder.StartWrite;
    CSVBuilder.Delimiter:= aDelimiter;
    CSVBuilder.QuoteChar:= aQuoteChar;
    CSVBuilder.UTF8:= true;

    for i := 0 to fields.Count - 1 do
      CSVBuilder.AppendCellRFC4180(fields.Get(i).SerializedFieldName);
    CSVBuilder.AppendRow;

    formatString := '#';

    for i := 1 to aFractionPartDigits do
    begin
      if i = 1 then
        formatString := formatString + '.#'
      else
        formatString := formatString + '#';
    end;

    for i := 0 to aDataProvider.Count - 1 do
    begin
      curDatum := aDataProvider.GetDatum(i);

      for k := 0 to fields.Count - 1 do
      begin
        value := curDatum.GetPropertyByFieldName(fields.Get(k).OriginalFieldName);
        case fields.Get(k).DataType of
          vftInteger : if VarIsNull (value) then CSVBuilder.AppendCellRFC4180('') else CSVBuilder.AppendCellRFC4180(IntToStr(value));
          vftBoolean : if VarIsNull (value) then CSVBuilder.AppendCellRFC4180('') else begin if value then CSVBuilder.AppendCellRFC4180('true') else CSVBuilder.AppendCellRFC4180('false'); end;
          vftFloat, vftCurrency : if VarIsNull(value) then CSVBuilder.AppendCellRFC4180('') else CSVBuilder.AppendCellRFC4180(FormatFloat(formatString, value));
          vftDate : if VarIsNull(value) then CSVBuilder.AppendCellRFC4180('') else CSVBuilder.AppendCellRFC4180(FormatDateTime('yyyy"-"mm"-"dd', value));
          vftTime : if VarIsNull(value) then CSVBuilder.AppendCellRFC4180('') else CSVBuilder.AppendCellRFC4180(FormatDateTime('hh":"mm":"ss', value));
          vftDateTime, vftTimeStamp : if VarIsNull(value) then CSVBuilder.AppendCellRFC4180('') else CSVBuilder.AppendCellRFC4180(FormatDateTime('yyyy"-"mm"-"dd" "hh":"mm":"ss', value));
        else
          if VarIsNull(value) then CSVBuilder.AppendCellRFC4180('') else CSVBuilder.AppendCellRFC4180(EscapeStringValue(VarToStr(value), 'json'));
        end;
      end;
      CSVBuilder.AppendRow;
    end;

    CSVBuilder.EndWrite;

    Result := stream.DataString;
  finally
    fields.Free;
    CSVBuilder.Free;
    stream.Free;
  end;
end;

end.
