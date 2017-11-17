// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mBooleanDataProvider;

{$IFDEF FPC}
  {$MODE DELPHI}
 {$ENDIF}
 
interface

uses
  {$IFNDEF FPC}mInterfaces,{$ENDIF}
  mVirtualFieldDefs, mVirtualDataSetInterfaces;

type

  { TBooleanDatumKey }

  TBooleanDatumKey = class(IVDDatumKey)
  strict private
    FValue : boolean;
  public
    constructor Create (aValue : boolean);

    procedure Assign(aSource : TObject);
    function AsString : string;

    property Value : boolean read FValue write FValue;
  end;

  { TBooleanDatum }

  TBooleanDatum = class ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDDatum)
  strict private
    const FLD_VALUE = 'VALUE';
  strict private
    FKey : TBooleanDatumKey;
  public
    constructor Create(aValue : boolean);
    destructor Destroy; override;

    function GetDatumKey : IVDDatumKey;

    class procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; aPrefix : String);
    class function GetKeyField : String;
    function GetPropertyByFieldName(const aFieldName : String) : Variant;

    property Key : TBooleanDatumKey read FKey;
  end;

  { TBooleanDataProvider }

  TBooleanDataProvider = class ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDListDataProvider)
  strict private
    FTrueValue : TBooleanDatum;
    FFalseValue : TBooleanDatum;
  public
    constructor Create;
    destructor Destroy; override;
    function Count : integer;
    function GetDatum(const aIndex : integer) : IVDDatum;
    function FindDatumByKey (const aKey : IVDDatumKey) : IVDDatum;
    function FindDatumByStringKey (const aStringKey : string): IVDDatum;
    procedure Clear;
  end;

implementation

uses
  SysUtils;

{ TBooleanDatumKey }

constructor TBooleanDatumKey.Create(aValue: boolean);
begin
  FValue := aValue;
end;

procedure TBooleanDatumKey.Assign(aSource: TObject);
begin
  FValue:= (aSource as TBooleanDatumKey).FValue;
end;

function TBooleanDatumKey.AsString: string;
begin
  Result := BoolToStr(FValue, true);
end;

{ TBooleanDataProvider }

constructor TBooleanDataProvider.Create;
begin
  FTrueValue := TBooleanDatum.Create(true);
  FFalseValue := TBooleanDatum.Create(false);
end;

destructor TBooleanDataProvider.Destroy;
begin
  FTrueValue.Free;
  FFalseValue.Free;
  inherited Destroy;
end;

function TBooleanDataProvider.Count: integer;
begin
  Result := 2;
end;

function TBooleanDataProvider.GetDatum(const aIndex: integer): IVDDatum;
begin
  if aIndex = 0 then
    Result := FTrueValue
  else
    Result := FFalseValue;
end;

function TBooleanDataProvider.FindDatumByKey(const aKey: IVDDatumKey): IVDDatum;
begin
  Result := FindDatumByStringKey(aKey.AsString);
end;

function TBooleanDataProvider.FindDatumByStringKey(const aStringKey: string): IVDDatum;
begin
  if aStringKey = BoolToStr(true, true) then
    Result := FTrueValue
  else
    Result := FFalseValue;
end;

procedure TBooleanDataProvider.Clear;
begin
  //
end;

{ TBooleanDatum }

constructor TBooleanDatum.Create(aValue: boolean);
begin
  FKey := TBooleanDatumKey.Create(aValue);
end;

destructor TBooleanDatum.Destroy;
begin
  FKey.Free;
end;

function TBooleanDatum.GetDatumKey: IVDDatumKey;
begin
  Result := Key;
end;

class procedure TBooleanDatum.FillVirtualFieldDefs(aFieldDefs: TmVirtualFieldDefs; aPrefix: String);
begin
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_VALUE;
    DataType:= vftString;
    Size := 10;
  end;
end;

class function TBooleanDatum.GetKeyField: String;
begin
  Result := FLD_VALUE;
end;

function TBooleanDatum.GetPropertyByFieldName(const aFieldName: String): Variant;
begin
  Result := BoolToStr(FKey.Value, true);
end;

end.
