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
  sysutils, TypInfo, Variants, Classes,
  mFilterOperators, mUtility, mDoubleList, mIntList;

procedure ExportFilterToXML(const aFilter: TmFilter; aXMLElement: TmXmlElement);
var
  dList : TDoubleList;
  iList : TIntegerList;
  sList : TStringList;
  i : integer;
begin
  aXMLElement.SetAttribute('fieldName', aFilter.FieldName);
  aXMLElement.SetAttribute('filterOperator', GetEnumName(TypeInfo(TmFilterOperator), Ord(aFilter.FilterOperator)));
  aXMLElement.SetAttribute('dataType', GetEnumName(TypeInfo(TmFilterDataType), Ord(aFilter.DataType)));
  aXMLElement.SetAttribute('displayValue', aFilter.DisplayValue);
  if not VarIsNull(aFilter.Value) then
  begin
    if aFilter.DataType = fdtString then
    begin
      if VarIsArray(aFilter.Value) then
      begin
        sList := TStringList.Create;
        try
          ConvertVariantToStringList(aFilter.Value, sList);
          for i := 0 to sList.Count -1 do
            aXMLElement.SetAttribute('value' + IntToStr(i), sList.Strings[i]);
        finally
          sList.Free;
        end;
      end
      else
        aXMLElement.SetAttribute('value', VarToStr(aFilter.Value));
    end
    else if aFilter.DataType = fdtDate then
    begin
      if VarIsArray(aFilter.Value) then
      begin
        iList := TIntegerList.Create;
        try
          ConvertVariantToDateList(aFilter.Value, iList);
          for i := 0 to iList.Count -1 do
            aXMLElement.SetDateAttribute('value' + IntToStr(i), iList.Items[i]);
        finally
          iList.Free;
        end;
      end
      else
        aXMLElement.SetDateAttribute('value', aFilter.Value);
    end
    else if (aFilter.DataType = fdtDateTime) or (aFilter.DataType = fdtTime) then
    begin
      if VarIsArray(aFilter.Value) then
      begin
        dList := TDoubleList.Create;
        try
          ConvertVariantToDateTimeList(aFilter.Value, dList);
          for i := 0 to dList.Count - 1 do
            aXMLElement.SetDateTimeAttribute('value' + IntToStr(i), dList.Items[i]);
        finally
          dList.Free;
        end;
      end
      else
        aXMLElement.SetDateTimeAttribute('value', aFilter.Value);
    end
    else if aFilter.DataType = fdtInteger then
    begin
      if VarIsArray(aFilter.Value) then
      begin
        iList := TIntegerList.Create;
        try
          ConvertVariantToIntegerList(aFilter.Value, iList);
          for i := 0 to iList.Count - 1 do
            aXMLElement.SetIntegerAttribute('value' + IntToStr(i), iList.Items[i]);
        finally
          iList.Free;
        end;
      end
      else
        aXMLElement.SetIntegerAttribute('value', aFilter.Value);
    end
    else if aFilter.DataType = fdtFloat then
    begin
      if VarIsArray(aFilter.Value) then
      begin
        dList := TDoubleList.Create;
        try
          ConvertVariantToDoubleList(aFilter.Value, dList);
          for i := 0 to dList.Count - 1 do
            aXMLElement.SetFloatAttribute('value' + IntToStr(i), dList.Items[i]);
        finally
          dList.Free;
        end;
      end
      else
        aXMLElement.SetFloatAttribute('value', aFilter.Value);
    end
    else if aFilter.DataType = fdtBoolean then
      aXMLElement.SetBooleanAttribute('value', aFilter.Value);
  end;
end;

procedure ImportFilterFromXML(aFilter: TmFilter; const aXMLElement: TmXmlElement);
var
  dList : TDoubleList;
  iList : TIntegerList;
  sList : TStringList;
  i : integer;
  key : String;
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
  else if aXMLElement.HasAttribute('value0') then
  begin
    i := 0;
    dList := TDoubleList.Create;
    iList := TIntegerList.Create;
    sList := TStringList.Create;
    try
      while aXMLElement.HasAttribute('value' + IntToStr(i)) do
      begin
        key := 'value' + IntToStr(i);
        if aFilter.DataType = fdtString then
          sList.Add(aXMLElement.GetAttribute(key))
        else if aFilter.DataType = fdtDate then
          dList.Add(aXMLElement.GetDateAttribute(key))
        else if aFilter.DataType = fdtDateTime then
          dList.Add(aXMLElement.GetDateTimeAttribute(key))
        else if aFilter.DataType = fdtTime then
          dList.Add(Frac(aXMLElement.GetDateTimeAttribute(key)))
        else if aFilter.DataType = fdtInteger then
          iList.Add(aXMLElement.GetIntegerAttribute(key))
        else if aFilter.DataType = fdtFloat then
          dList.Add(aXMLElement.GetFloatAttribute(key));
        inc(i);
      end;
      if aFilter.DataType = fdtString then
        aFilter.Value := ConvertStringListToVariant(sList)
      else if (aFilter.DataType = fdtDate) or (aFilter.DataType = fdtDateTime) or (aFilter.DataType = fdtTime) or (aFilter.DataType = fdtFloat) then
        aFilter.Value := ConvertDoubleListToVariant(dList)
      else if aFilter.DataType = fdtInteger then
        aFilter.Value := ConvertIntegerListToVariant(iList);
    finally
      dList.Free;
      iList.Free;
      sList.Free;
    end;
  end
  else
    aFilter.Value:= Null;
end;

end.
