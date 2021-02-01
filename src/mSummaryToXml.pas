// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mSummaryToXml;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  db,
  mXML, mSummary;

procedure SaveSummaryDefinitionsToXmlElement (const aSource : TmSummaryDefinitions; aXmlElement : TmXmlElement);
procedure LoadSummaryDefinitionsFromXmlElement (aDestination : TmSummaryDefinitions; aXmlElement : TmXmlElement);

procedure SaveSummaryDefinitionToXmlElement (const aSource : TmSummaryDefinition; aXmlElement : TmXmlElement);
procedure LoadSummaryDefinitionFromXmlElement (aDestination : TmSummaryDefinition; aXmlElement : TmXmlElement);

implementation

uses
  typinfo;

procedure SaveSummaryDefinitionsToXmlElement(const aSource: TmSummaryDefinitions; aXmlElement: TmXmlElement);
var
  i : integer;
begin
  for i := 0 to aSource.Count - 1 do
    SaveSummaryDefinitionToXmlElement(aSource.Get(i), aXmlElement.AddElement('summary'));
end;

procedure LoadSummaryDefinitionsFromXmlElement(aDestination: TmSummaryDefinitions; aXmlElement: TmXmlElement);
var
  cursor : TmXmlElementCursor;
  i : integer;
begin
  aDestination.Clear;
  cursor := TmXmlElementCursor.Create(aXmlElement, 'summary');
  try
    for i := 0 to cursor.Count - 1 do
      LoadSummaryDefinitionFromXmlElement(aDestination.Add, cursor.Elements[i]);
  finally
    cursor.Free;
  end;
end;

procedure SaveSummaryDefinitionToXmlElement(const aSource: TmSummaryDefinition; aXmlElement: TmXmlElement);
begin
  aXmlElement.SetAttribute('fieldName', aSource.FieldName);
  aXmlElement.SetAttribute('operator', GetEnumName(TypeInfo(TmSummaryOperator), integer(aSource.SummaryOperator)));
  aXmlElement.SetAttribute('fieldType', GetEnumName(TypeInfo(TFieldType), integer(aSource.FieldType)));
  if aSource.DisplayLabel.NotNull then
    aXmlElement.SetAttribute('caption', aSource.DisplayLabel);
  if aSource.DisplayFormat.NotNull then
    aXmlElement.SetAttribute('displayFormat', aSource.DisplayFormat);
end;

procedure LoadSummaryDefinitionFromXmlElement(aDestination: TmSummaryDefinition; aXmlElement: TmXmlElement);
begin
  aDestination.FieldName := aXmlElement.GetAttribute('fieldName');
  aDestination.SummaryOperator:= TmSummaryOperator(GetEnumValue(TypeInfo(TmSummaryOperator), aXmlElement.GetAttribute('operator')));
  aDestination.FieldType:= TFieldType(GetEnumValue(TypeInfo(TFieldType), aXmlElement.GetAttribute('fieldType')));
  if aXmlElement.HasAttribute('caption') then
    aXmlElement.GetAttribute('caption', aDestination.DisplayLabel)
  else
    aDestination.DisplayLabel.IsNull := true;
  if aXmlElement.HasAttribute('displayFormat') then
    aXmlElement.GetAttribute('displayFormat', aDestination.DisplayFormat)
  else
    aDestination.DisplayFormat.IsNull := true;
end;

end.
