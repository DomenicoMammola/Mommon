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

type

  { TListDatumKey }

  TListDatumKey = class(IVDDatumKey)
  strict private
    FId : integer;
  public
    constructor Create (const aId : integer);

    procedure Assign(aSource : TObject);
    function AsString : string;

    property Id : integer read FId write FId;
  end;

  { TListDatum }

  TListDatum = class ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDDatum)
  strict private
    FKey : TListDatumKey;
    FDescription : String;
    FValue : TNullableValue;
  public
    const FLD_ID = 'ID';
    const FLD_DESCRIPTION = 'DESCRIPTION';
    const FLD_VALUE = 'VALUE';
  public
    constructor Create(const aId : integer);
    destructor Destroy; override;

    function GetDatumKey : IVDDatumKey;
    function AsObject: TObject;

    class procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String);
    class function GetKeyField : String;
    function GetPropertyByFieldName(const aFieldName : String) : Variant;

    property Key : TListDatumKey read FKey;
    property Description : string read FDescription write FDescription;
    property Value : TNullableValue read FValue;
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
begin
  Result := GetDatum(StrToInt(aStringKey));
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
  Result := TListDatum.FLD_ID;
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
  newDatum := TListDatum.Create(FList.Count);
  newDatum.Value.DataType:= FValueDataType;
  newDatum.Value.Assign(aValue);
  newDatum.Description := aDescription;
  FList.Add(newDatum);
end;

{ TListDatum }

constructor TListDatum.Create(const aId : integer);
begin
  FKey := TListDatumKey.Create(aId);
  FDescription:= '';
  FValue:= TNullableValue.Create();
end;

destructor TListDatum.Destroy;
begin
  FKey.Free;
  FValue.Free;
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
    Name := aPrefix + FLD_ID;
    DataType:= vftInteger;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_DESCRIPTION;
    DataType:= vftString;
    Size := 128;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_VALUE;
    DataType:= vftString;
    Size := 128;
  end;
end;

class function TListDatum.GetKeyField: String;
begin
  Result := FLD_ID;
end;

function TListDatum.GetPropertyByFieldName(const aFieldName: String): Variant;
begin
  Result := Null;
  if aFieldName = FLD_ID then
    Result := FKey.Id
  else if aFieldName = FLD_DESCRIPTION then
    Result := FDescription
  else if aFieldName = FLD_VALUE then
    Result := FValue.AsVariant;
end;

{ TListDatumKey }

constructor TListDatumKey.Create(const aId: integer);
begin
  FId := aId;
end;

procedure TListDatumKey.Assign(aSource: TObject);
begin
  FId := (aSource as TListDatumKey).Id;
end;

function TListDatumKey.AsString: string;
begin
  Result := IntToStr(FId);
end;


end.
