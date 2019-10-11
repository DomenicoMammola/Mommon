// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mListDataProvider;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs,
  {$IFNDEF FPC}mInterfaces,{$ENDIF}
  mDataProviderFieldDefs, mDataProviderInterfaces, mNullables;

const
  LIST_DATA_PROVIDER_FIELD_LENGTH = 128;

type

  { TListDatumKey }

  TListDatumKey = class(IVDDatumKey)
  strict private
    FValue : TNullableValue;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(aSource : TObject);
    function AsString : string;

    property Value : TNullableValue read FValue;
  end;

  { TListDatum }

  TListDatum = class ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDDatum)
  strict private
    FKey : TListDatumKey;
    FDescription : String;
  public
    const FLD_DESCRIPTION = 'DESCRIPTION';
    const FLD_VALUE = 'VALUE';
  public
    constructor Create;
    destructor Destroy; override;

    function GetDatumKey : IVDDatumKey;
    function AsObject: TObject;

    class procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String);
    class function GetKeyField : String;
    function GetPropertyByFieldName(const aFieldName : String) : Variant;

    property Key : TListDatumKey read FKey;
    property Description : string read FDescription write FDescription;
  end;


  { TListDataProvider }

  TListDataProvider = class (IVDDataProvider)
  strict private
    FList : TObjectList;
    FValueDataType : TNullableDataType;
  public
    constructor Create(const aValueDataType : TNullableDataType);
    destructor Destroy; override;
    function Count : integer;
    function GetDatum(const aIndex : integer) : IVDDatum;
    function FindDatumByKey (const aKey : IVDDatumKey) : IVDDatum;
    function FindDatumByStringKey (const aStringKey : string): IVDDatum;
    procedure Clear;
    procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String);
    function GetKeyFieldName : String;
    procedure GetMinimumFields(aFieldsForLookup : TStringList);

    procedure AddValue (const aDescription : string; const aValue : variant);
  end;


implementation

uses
  sysutils;

{ TListDataProvider }

constructor TListDataProvider.Create(const aValueDataType: TNullableDataType);
begin
  FValueDataType:= aValueDataType;
  FList := TObjectList.Create(true);
  FValueDataType:= tnString;
end;

destructor TListDataProvider.Destroy;
begin
  FList.Free;
end;

function TListDataProvider.Count: integer;
begin
  Result := FList.Count;
end;

function TListDataProvider.GetDatum(const aIndex: integer): IVDDatum;
begin
  Result := FList.Items[aIndex] as IVDDatum;
end;

function TListDataProvider.FindDatumByKey(const aKey: IVDDatumKey): IVDDatum;
begin
  Result := FindDatumByStringKey(aKey.AsString);
end;

function TListDataProvider.FindDatumByStringKey(const aStringKey: string): IVDDatum;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if TListDatum(FList.Items[i]).Key.Value.AsString = aStringKey then
    begin
      Result := TListDatum(FList.Items[i]);
      exit;
    end;
  end;
end;

procedure TListDataProvider.Clear;
begin
  FList.Clear;
end;

procedure TListDataProvider.FillVirtualFieldDefs(aFieldDefs: TmVirtualFieldDefs; const aPrefix: String);
begin
  TListDatum.FillVirtualFieldDefs(aFieldDefs, aPrefix);
end;

function TListDataProvider.GetKeyFieldName: String;
begin
  Result := TListDatum.FLD_VALUE;
end;

procedure TListDataProvider.GetMinimumFields(aFieldsForLookup: TStringList);
begin
  aFieldsForLookup.Clear;
  aFieldsForLookup.Add(TListDatum.FLD_DESCRIPTION);
end;

procedure TListDataProvider.AddValue(const aDescription: string; const aValue: variant);
var
  newDatum: TListDatum;
begin
  newDatum := TListDatum.Create();
  newDatum.Key.Value.DataType:= FValueDataType;
  newDatum.Key.Value.Assign(aValue);
  newDatum.Description := aDescription;
  FList.Add(newDatum);
end;

{ TListDatum }

constructor TListDatum.Create;
begin
  FKey := TListDatumKey.Create;
  FDescription:= '';
end;

destructor TListDatum.Destroy;
begin
  FKey.Free;
  inherited;
end;

function TListDatum.GetDatumKey: IVDDatumKey;
begin
  Result := FKey;
end;

function TListDatum.AsObject: TObject;
begin
  Result := Self;
end;

class procedure TListDatum.FillVirtualFieldDefs(aFieldDefs: TmVirtualFieldDefs; const aPrefix: String);
begin
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_DESCRIPTION;
    DataType:= vftString;
    Size := LIST_DATA_PROVIDER_FIELD_LENGTH;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_VALUE;
    DataType:= vftString;
    Size := LIST_DATA_PROVIDER_FIELD_LENGTH;
  end;
end;

class function TListDatum.GetKeyField: String;
begin
  Result := FLD_VALUE;
end;

function TListDatum.GetPropertyByFieldName(const aFieldName: String): Variant;
begin
  Result := Null;
  if aFieldName = FLD_DESCRIPTION then
    Result := FDescription
  else if aFieldName = FLD_VALUE then
    Result := Key.Value.AsVariant;
end;

{ TListDatumKey }

constructor TListDatumKey.Create;
begin
  FValue := TNullableValue.Create();
end;

destructor TListDatumKey.Destroy;
begin
  FValue.Free;
end;

procedure TListDatumKey.Assign(aSource: TObject);
begin
  FValue.Assign((aSource as TListDatumKey).Value);
end;

function TListDatumKey.AsString: string;
begin
  Result := FValue.AsString;
end;


end.
