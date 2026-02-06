// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo@mammola.net - www.mammola.net)

unit mFilterToXML;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  mFilter, mXML;

procedure ExportFilterToXML(const aFilter : TmFilter; aXMLElement : TmXmlElement);
procedure ImportFilterFromXML(aFilter : TmFilter; const aXMLElement : TmXmlElement);

implementation

uses
  sysutils, TypInfo, Variants,
  mFilterOperators;

procedure ExportFilterToXML(const aFilter: TmFilter; aXMLElement: TmXmlElement);
begin
  aXMLElement.SetAttribute('fieldName', aFilter.FieldName);
  aXMLElement.SetAttribute('filterOperator', GetEnumName(TypeInfo(TmFilterOperator), Ord(aFilter.FilterOperator)));
  aXMLElement.SetAttribute('dataType', GetEnumName(TypeInfo(TmFilterDataType), Ord(aFilter.DataType)));
  aXMLElement.SetAttribute('displayValue', aFilter.DisplayValue);
  if not VarIsNull(aFilter.Value) then
  begin
    if aFilter.DataType = fdtString then
      aXMLElement.SetAttribute('value', VarToStr(aFilter.Value))
    else if aFilter.DataType = fdtDate then
      aXMLElement.SetDateAttribute('value', aFilter.Value)
    else if aFilter.DataType = fdtDateTime then
      aXMLElement.SetDateTimeAttribute('value', aFilter.Value)
    else if aFilter.DataType = fdtTime then
      aXMLElement.SetDateTimeAttribute('value', aFilter.Value)
    else if aFilter.DataType = fdtInteger then
      aXMLElement.SetIntegerAttribute('value', aFilter.Value)
    else if aFilter.DataType = fdtFloat then
      aXMLElement.SetFloatAttribute('value', aFilter.Value)
    else if aFilter.DataType = fdtBoolean then
      aXMLElement.SetBooleanAttribute('value', aFilter.Value);
  end;
end;

procedure ImportFilterFromXML(aFilter: TmFilter; const aXMLElement: TmXmlElement);
begin
  aFilter.FieldName := aXMLElement.GetAttribute('fieldName');
  aFilter.FilterOperator := TmFilterOperator(GetEnumValue(TypeInfo(TmFilterOperator), aXMLElement.GetAttribute('filterOperator')));
  aFilter.DataType := TmFilterDataType(GetEnumValue(TypeInfo(TmFilterDataType), aXMLElement.GetAttribute('dataType')));
  aFilter.DisplayValue := aXMLElement.GetAttribute('displayValue');
  if aXMLElement.HasAttribute('value') then
  begin
    if aFilter.DataType = fdtString then
      aFilter.Value := aXMLElement.GetAttribute('value')
    else if aFilter.DataType = fdtDate then
      aFilter.Value := aXMLElement.GetDateAttribute('value')
    else if aFilter.DataType = fdtDateTime then
      aFilter.Value := aXMLElement.GetDateTimeAttribute('value')
    else if aFilter.DataType = fdtTime then
      aFilter.Value := Frac(aXMLElement.GetDateTimeAttribute('value'))
    else if aFilter.DataType = fdtInteger then
      aFilter.Value := aXMLElement.GetIntegerAttribute('value')
    else if aFilter.DataType = fdtFloat then
      aFilter.Value := aXMLElement.GetFloatAttribute('value')
    else if aFilter.DataType = fdtBoolean then
      aFilter.Value := aXMLElement.GetBooleanAttribute('value');
  end
  else
    aFilter.Value:= Null;
end;

end.
