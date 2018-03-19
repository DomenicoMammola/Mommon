// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mVirtualDataSetInterfaces;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

uses
  Classes;

type

  IVDDatumKey = interface
    ['{0B49C3CB-33F9-4E9E-859D-ADE0DB1F6339}']
    procedure Assign(aSource : TObject);
    function AsString : string;
  end;


  IVDDatum = interface
    ['{DDB32FAA-C54E-47DF-A0D1-CFE37F092BAD}']
    function GetDatumKey : IVDDatumKey;
    function GetPropertyByFieldName(const aFieldName : String) : Variant;
    procedure SetPropertyByFieldName(const aFieldName: String; const aValue : Variant);
    function AsObject : TObject;
  end;

  IVDListDataProvider = interface
    ['{F3F52A84-CEEF-4567-98CF-847BC64342E7}']
    function Count : integer;
    function GetDatum(const aIndex : integer) : IVDDatum;
    function FindDatumByKey (const aKey : IVDDatumKey): IVDDatum;
    function FindDatumByStringKey (const aStringKey : string): IVDDatum;
    procedure Clear;
  end;

function CompareByProperties(aFirstDatum, aSecondDatum : IVDDatum; const aFields : TStrings; var aLastCheckedConditionIndex : integer) : integer; // -1 <, 0 =, +1 >

implementation

uses
  mUtility;

function CompareByProperties(aFirstDatum, aSecondDatum : IVDDatum; const aFields : TStrings; var aLastCheckedConditionIndex : integer) : integer; // -1 <, 0 =, +1 >
var
  i : integer;
  val1, val2 : Variant;
begin
  Result := -1;
  aLastCheckedConditionIndex := 0;
  for i := 0 to aFields.Count -1 do
  begin
    val1 := aFirstDatum.GetPropertyByFieldName(aFields[i]);
    val2 := aSecondDatum.GetPropertyByFieldName(aFields[i]);
    Result := CompareVariants(val1, val2);
    aLastCheckedConditionIndex := i;
    if Result <> 0 then
      break;
  end;
end;

end.
