// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mConcatenationDataProvider;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface


uses
  Classes, contnrs,
  {$IFNDEF FPC}mInterfaces,{$ENDIF}
  mDataProviderFieldDefs, mDataProviderInterfaces, mNullables;

const
  CONCATENATION_DATA_PROVIDER_FIELD_LENGTH = 1024;

type

  { TConcatenationDatumKey }

  TConcatenationDatumKey = class(IVDDatumKey)
  strict private
    FIndex : TNullableInteger;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(aSource : TObject);
    function AsString : string;

    property Index : TNullableInteger read FIndex;
  end;

  TConcatenationDatum = class ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDDatum)
  strict private
    FKey : TConcatenationDatumKey;
    FValue : TNullableString;
    FOriginalDatum : IVDDatum;
  public
    const FLD_KEY = 'KEY';
    const FLD_VALUE = 'VALUE';
  public
    constructor Create;
    destructor Destroy; override;

    function GetDatumKey : IVDDatumKey;
    function AsObject: TObject;
    procedure Assign(const aSource : TConcatenationDatum);
    function Clone : IVDDatum;

    class procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String);
    class function GetKeyField : String;
    function GetPropertyByFieldName(const aFieldName : String) : Variant;

    property Key : TConcatenationDatumKey read FKey;
    property Value : TNullableString read FValue;
    property OriginalDatum : IVDDatum read FOriginalDatum write FOriginalDatum;
  end;

  { TConcatenationDataProvider }

  TConcatenationDataProvider = class (IVDDataProvider)
  strict private
    FExternalDataProvider : IVDDataProvider;
    FFieldsToBeConcatenated : TStringList;
    FDatumList : TObjectList;
  public
    constructor Create(const aExternalDataProvider : IVDDataProvider);
    destructor Destroy; override;
    function Count : integer;
    function GetDatum(const aIndex : integer) : IVDDatum;
    function FindDatumByKey (const aKey : IVDDatumKey) : IVDDatum;
    function FindDatumByStringKey (const aStringKey : string): IVDDatum;
    procedure Clear;
    procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String);
    function GetKeyFieldName : String;
    procedure GetMinimumFields(aFieldsForLookup : TStringList);
    procedure Refresh;

    property FieldsToBeConcatenated : TStringList read FFieldsToBeConcatenated;
  end;



implementation

uses
  Variants;

{ TConcatenationDataProvider }

constructor TConcatenationDataProvider.Create(const aExternalDataProvider: IVDDataProvider);
begin
  FExternalDataProvider := aExternalDataProvider;
  FFieldsToBeConcatenated := TStringList.Create;
  FDatumList := TObjectList.Create(true);
end;

destructor TConcatenationDataProvider.Destroy;
begin
  FDatumList.Free;
  FFieldsToBeConcatenated.Free;
end;

function TConcatenationDataProvider.Count: integer;
begin
  Result := FDatumList.Count;
end;

function TConcatenationDataProvider.GetDatum(const aIndex: integer): IVDDatum;
begin
  Result := FDatumList.Items[aIndex] as TConcatenationDatum;
end;

function TConcatenationDataProvider.FindDatumByKey(const aKey: IVDDatumKey): IVDDatum;
begin
  Result := Self.FindDatumByStringKey(aKey.AsString); // TConcatenationDatumKey(aKey)
end;

function TConcatenationDataProvider.FindDatumByStringKey(const aStringKey: string): IVDDatum;
var
  i : integer;
begin
  // eventually to be optimized..
  // useless if TConcatenationDataProvider is used together with mPivoter
  Result := nil;
  for i := 0 to FDatumList.Count - 1 do
  begin
    if (FDatumList.Items[i] as TConcatenationDatum).Key.AsString = aStringKey then
    begin
      Result := FDatumList.Items[i] as TConcatenationDatum;
      exit;
    end;
  end;
end;

procedure TConcatenationDataProvider.Clear;
begin
  FDatumList.Clear;
end;

procedure TConcatenationDataProvider.FillVirtualFieldDefs(aFieldDefs: TmVirtualFieldDefs; const aPrefix: String);
begin
  TConcatenationDatum.FillVirtualFieldDefs(aFieldDefs, aPrefix);
end;

function TConcatenationDataProvider.GetKeyFieldName: String;
begin
  Result := TConcatenationDatum.FLD_VALUE;
end;

procedure TConcatenationDataProvider.GetMinimumFields(aFieldsForLookup: TStringList);
begin
  aFieldsForLookup.Clear;
  aFieldsForLookup.Add(TConcatenationDatum.FLD_KEY);
end;

procedure TConcatenationDataProvider.Refresh;
var
  i, k : integer;
  curOriginalDatum : IVDDatum;
  concatenationDatum : TConcatenationDatum;
  sep, value : String;
begin
  FDatumList.Clear;
  for i := 0 to FExternalDataProvider.Count - 1 do
  begin
    curOriginalDatum := FExternalDataProvider.GetDatum(i);
    sep := '';
    value := '';
    for k := 0 to FFieldsToBeConcatenated.Count - 1 do
    begin
      value := value + sep + VarToStr(curOriginalDatum.GetPropertyByFieldName(FFieldsToBeConcatenated.Strings[k]));
      sep := '^?@';
    end;
    concatenationDatum := TConcatenationDatum.Create;
    FDatumList.Add(concatenationDatum);
    concatenationDatum.Value.Value:= value;
    concatenationDatum.Key.Index.Value:= i;
    concatenationDatum.OriginalDatum:= curOriginalDatum;
  end;
end;

{ TConcatenationDatum }

constructor TConcatenationDatum.Create;
begin
  FKey := TConcatenationDatumKey.Create;
  FValue := TNullableString.Create;
  FOriginalDatum:= nil;
end;

destructor TConcatenationDatum.Destroy;
begin
  FValue.Free;
  FKey.Free;
end;

function TConcatenationDatum.GetDatumKey: IVDDatumKey;
begin
  Result := FKey;
end;

function TConcatenationDatum.AsObject: TObject;
begin
  Result := Self;
end;

procedure TConcatenationDatum.Assign(const aSource: TConcatenationDatum);
begin
  Key.Assign(aSource.Key);
  Value.Assign(aSource.Value);
  OriginalDatum := aSource.OriginalDatum;
end;

function TConcatenationDatum.Clone: IVDDatum;
var
  tmp : TConcatenationDatum;
begin
  tmp := TConcatenationDatum.Create;
  tmp.Assign(Self);
  Result := tmp;
end;

class procedure TConcatenationDatum.FillVirtualFieldDefs(aFieldDefs: TmVirtualFieldDefs; const aPrefix: String);
begin
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_KEY;
    DataType:= vftInteger;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_VALUE;
    DataType:= vftString;
    Size := CONCATENATION_DATA_PROVIDER_FIELD_LENGTH;
  end;
end;

class function TConcatenationDatum.GetKeyField: String;
begin
  Result := FLD_KEY;
end;

function TConcatenationDatum.GetPropertyByFieldName(const aFieldName: String): Variant;
begin
  if aFieldName = FLD_KEY then
    Result := FKey.AsString
  else if aFieldName = FLD_VALUE then
    Result := FValue.AsVariant
  else
    Result := Null;
end;

{ TConcatenationDatumKey }

constructor TConcatenationDatumKey.Create;
begin
  FIndex := TNullableInteger.Create();
end;

destructor TConcatenationDatumKey.Destroy;
begin
  FIndex.Free;
end;

procedure TConcatenationDatumKey.Assign(aSource: TObject);
begin
  FIndex.Assign((aSource as TConcatenationDatumKey).Index);
end;

function TConcatenationDatumKey.AsString: string;
begin
  Result := FIndex.AsString;
end;

end.
