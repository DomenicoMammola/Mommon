// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSortConditions;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes;

type
  TSortType = (stAscending, stDescending);

  { TSortByCondition }

  TSortByCondition = class (TCollectionItem)
  strict private
    FSortType : TSortType;
    FFieldName : String;
  public
    constructor Create(Collection: TCollection); override;

    property SortType : TSortType read FSortType write FSortType;
    property FieldName : String read FFieldName write FFieldName;
  end;

  { TSortByConditions }

  TSortByConditions = class (TCollection)
  private
    function GetCondition(Index :integer) : TSortByCondition;
  public
    constructor Create; reintroduce;
    function Add : TSortByCondition;

    property Items[index:integer]: TSortByCondition read GetCondition; default;
  end;

  { TSortInformation }

  TSortInformation = class
  strict private
    FLastSortedField : String;
    FLastSortType : TSortType;
  public
    constructor Create;
    procedure Clear;

    property LastSortedField : string read FLastSortedField write FLastSortedField;
    property LastSortType : TSortType read FLastSortType write FLastSortType;
  end;


implementation

{ TSortInformation }

constructor TSortInformation.Create;
begin
  Clear;
end;

procedure TSortInformation.Clear;
begin
  FLastSortedField:= '';
  FLastSortType:= stAscending;
end;


{ TSortByConditions }

function TSortByConditions.GetCondition(Index: integer): TSortByCondition;
begin
  Result := TSortByCondition(inherited Items[Index]);
end;

constructor TSortByConditions.Create;
begin
  inherited Create(TSortByCondition);
end;

function TSortByConditions.Add: TSortByCondition;
begin
  Result := TSortByCondition(inherited Add);
end;

{ TSortByCondition }

constructor TSortByCondition.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFieldName:= '';
  FSortType:= stAscending;
end;

end.
