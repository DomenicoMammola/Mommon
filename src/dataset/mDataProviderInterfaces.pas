// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDataProviderInterfaces;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

uses
  Classes,
  mDataProviderFieldDefs, mSortConditions, mFilter, mSummary;

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
    function AsObject : TObject;
    function Clone : IVDDatum;
  end;

  IVDDataProvider = interface
    ['{F3F52A84-CEEF-4567-98CF-847BC64342E7}']
    function Count : integer;
    function GetDatum(const aIndex : integer) : IVDDatum;
    function FindDatumByKey (const aKey : IVDDatumKey): IVDDatum;
    function FindDatumByStringKey (const aStringKey : string): IVDDatum;
    procedure Clear;
    procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String);
    function GetKeyFieldName : String;
    procedure GetMinimumFields(aFieldsForLookup : TStringList);
  end;

  TInstantQueryManagerAdditionalOptionCallback = procedure(const aValue : boolean) of object;

  TInstantQueryManagerAdditionalOption = class
  strict private
    FCaption : String;
    FDefaultValue : Boolean;
    FCallback : TInstantQueryManagerAdditionalOptionCallback;
  public
    property Caption : String read FCaption write FCaption;
    property DefaultValue : Boolean read FDefaultValue write FDefaultValue;
    property Callback : TInstantQueryManagerAdditionalOptionCallback read FCallback write FCallback;
  end;

  IVDInstantQueryManager = interface
    ['{8462163A-558F-41C5-9268-BEAE64C96359}']
    function GetDataProvider: IVDDataProvider;
    procedure FilterDataProvider(const aLookForValue: String);
    procedure Clear;
    function GetAdditionalOptions: TList;
  end;

  ISortableDatasetManager = interface
    ['{481EA485-3F80-46B1-BF24-587EF48EFE11}']
    function GetSorted : boolean;
    function GetSortByConditions : TSortByConditions;
    function Sort : boolean;
    procedure ClearSort;
  end;

  { IFilterDatasetManager }

  IFilterDatasetManager = interface
    ['{6BCEF289-FCB4-4EFE-B594-8D08DBC1B09A}']
    procedure GetUniqueStringValuesForField (const aFieldName : string; aList : TStringList);
    function DoFilter : boolean;
    function GetFiltered : boolean;
    function GetFilters : TmFilters;
    procedure RemoveFilterForField(const aFieldName: string);
    procedure RemoveFilterForFields(const aFieldNames: TStrings);
    procedure RemoveFilter;
  end;

  ISummaryDatasetManager = interface
    ['{48E41715-7DB6-4876-9E7F-AB313E0287E7}']
    function GetSummaryDefinitions : TmSummaryDefinitions;
    function GetSummaryValues : TmSummaryValues;
    procedure RefreshSummaries;
    procedure NotifyChanges;
    procedure RegisterListener (aOnRefresh : TNotifyEvent);
    //procedure RemoveListener (const aIndex : integer);
  end;


function CompareByProperties(aFirstDatum, aSecondDatum : IVDDatum; const aFields : TStrings; var aLastCheckedConditionIndex : integer) : integer; // -1 <, 0 =, +1 >
function ConcatenateFieldValues (const aDatum : IVDDatum; const aFields: TStringList) : string;

implementation

uses
  Variants,
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


function ConcatenateFieldValues(const aDatum: IVDDatum; const aFields: TStringList): string;
var
  k : integer;
  Separator : String;
begin
  Result:= '';
  Separator:= '';
  for k := 0 to aFields.Count - 1 do
  begin
    Result:= Result + Separator + VarToStr(aDatum.GetPropertyByFieldName(aFields.Strings[k]));
    Separator := ',';
  end;
end;

end.
