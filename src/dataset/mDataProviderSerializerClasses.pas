// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDataProviderSerializerClasses;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  Contnrs,
  mDataProviderFieldDefs, mNamingConventions, mDataProviderInterfaces;

type

  { TSerializedField }

  TSerializedField = class
  strict private
    FSerializedFieldName: String;
    FOriginalFieldName : String;
    FDataType : TmVirtualFieldDefType;
  public
    constructor Create;

    property SerializedFieldName: String read FSerializedFieldName write FSerializedFieldName;
    property OriginalFieldName : String read FOriginalFieldName write FOriginalFieldName;
    property DataType : TmVirtualFieldDefType read FDataType write FDataType;
  end;

  { TSerializedFields }

  TSerializedFields = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function Count : integer;
    function Get (const aIndex : integer): TSerializedField;
    function Add : TSerializedField;
  end;

procedure GetSerializedFields (const aDataProvider : IVDDataProvider; aFields : TSerializedFields; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention);

implementation

{ TSerializedField }

constructor TSerializedField.Create;
begin
  FSerializedFieldName := '';
  FOriginalFieldName := '';
  FDataType := vftUnknown;
end;

{ TSerializedFields }

constructor TSerializedFields.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TSerializedFields.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TSerializedFields.Count: integer;
begin
  Result := FList.Count;
end;

function TSerializedFields.Get(const aIndex: integer): TSerializedField;
begin
  Result := FList.Items[aIndex] as TSerializedField;
end;

function TSerializedFields.Add: TSerializedField;
begin
  Result := TSerializedField.Create;
  FList.Add(Result);
end;

procedure GetSerializedFields (const aDataProvider : IVDDataProvider; aFields : TSerializedFields; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention);
var
  virtualFieldDefs : TmVirtualFieldDefs;
  i : integer;
  newField : TSerializedField;
begin
  virtualFieldDefs := TmVirtualFieldDefs.Create;
  try
    aDataProvider.FillVirtualFieldDefs(virtualFieldDefs, '');
    for i := 0 to virtualFieldDefs.Count - 1 do
    begin
      newField := aFields.Add;
      newField.OriginalFieldName:= virtualFieldDefs.VirtualFieldDefs[i].Name;
      newField.SerializedFieldName:= ConvertNamingConvention(newField.OriginalFieldName, aSourceNamingConvention, aDestinationNamingConvention);
      newField.DataType:= virtualFieldDefs.VirtualFieldDefs[i].DataType;
    end;

  finally
    virtualFieldDefs.Free;
  end;
end;

end.
