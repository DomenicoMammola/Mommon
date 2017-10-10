// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mVirtualDatasetFormulasToXml;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  mXML, mVirtualDatasetFormulas;

procedure SaveFormulaFieldsToXmlElement (const aSource : TmFormulaFields; aXmlElement : TmXmlElement);
procedure LoadFormulaFieldsFromXmlElement (aDestination : TmFormulaFields; aXmlElement : TmXmlElement);

implementation

procedure SaveFormulaFieldsToXmlElement(const aSource: TmFormulaFields; aXmlElement: TmXmlElement);
var
  i : integer;
  tmpElement : TmXmlElement;
begin
  for i := 0 to aSource.Count - 1 do
  begin
    tmpElement := aXmlElement.AddElement('formulaField');
    tmpElement.SetAttribute('name', aSource.Get(i).Name);
    tmpElement.SetIntegerAttribute('size', aSource.Get(i).Size);
    tmpElement.SetAttribute('dataType', TmFormulaFieldTypeToString(aSource.Get(i).DataType));
    tmpElement.SetAttribute('formula', aSource.Get(i).Formula);
  end;
end;

procedure LoadFormulaFieldsFromXmlElement(aDestination: TmFormulaFields; aXmlElement: TmXmlElement);
var
  cursor : TmXmlElementCursor;
  i : integer;
begin
  aDestination.Clear;
  cursor := TmXmlElementCursor.Create(aXmlElement, 'formulaField');
  try
    for i := 0 to cursor.Count - 1 do
    begin
      with aDestination.Add do
      begin
        Name:= cursor.Elements[i].GetAttribute('name');
        Size:= cursor.Elements[i].GetIntegerAttribute('size');
        DataType:= StringToTmFormulaFieldType(cursor.Elements[i].GetAttribute('dataType'));
        Formula:= cursor.Elements[i].GetAttribute('formula');
      end;
    end;
  finally
    cursor.Free;
  end;
end;

end.
