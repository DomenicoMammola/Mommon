// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatasetInterfaces;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

uses
  Classes,
  mSortConditions, mFilter, mSummary;

type
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

implementation

end.
