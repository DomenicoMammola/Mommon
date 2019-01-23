// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mKnapsackProblem;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs;

type

  { TEquipment }

  TEquipment = class
  strict private
    FReference : pointer;
    FWeight : integer;
    FValue : integer;
  public
    constructor Create;

    property Reference : pointer read FReference write FReference;
    property Weight : integer read FWeight write FWeight;
    property Value : integer read FValue write FValue;
  end;

  { TEquipmentList }

  TEquipmentList = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function Add : TEquipment;
    function Get(const aIndex: integer) : TEquipment;
  end;


// re-worked from: https://rosettacode.org/wiki/Knapsack_problem/0-1#Pascal
procedure OptimizeKnapsack (const aKnapsackWeight : integer; const aEquipmentList : TEquipmentList; aOptimizedEquipmentList : TList);

implementation

uses
  Math;


procedure OptimizeKnapsack(const aKnapsackWeight : integer; const aEquipmentList: TEquipmentList; aOptimizedEquipmentList: TList);
var
  i, j, maxValue, weightLeft : integer;
  maxArray : array of array of integer;
begin
  SetLength(maxArray, aEquipmentList.Count + 1, aKnapsackWeight + 1);

  for j := 0 to aKnapsackWeight do
    maxArray[0, j] := 0;

  for i := 0 to aEquipmentList.Count - 1 do
  begin
    for j := 0 to aKnapsackWeight do
    begin
      if aEquipmentList.Get(i).Weight > j then
        maxArray[i + 1, j] := maxArray[i, j]
      else
        maxArray[i + 1, j] := max(maxArray[i, j], maxArray[i, j- aEquipmentList.Get(i).Weight] + aEquipmentList.Get(i).Value);
    end;
  end;

  //get the highest total value by testing every value
  maxValue := 0;
  for i := 0 to aEquipmentList.Count - 1 do
  begin
    for j := 0 to aKnapsackWeight do
    begin
      if maxArray[i + 1,j] > maxValue then
          maxValue := maxArray[i + 1, j];
    end;
  end;


  //Work backwards through the items to find those items that go in the Knapsack
  weightLeft:= aKnapsackWeight;
  for i := aEquipmentList.Count -1  downto 0 do
  begin
    if maxArray[i + 1, weightLeft] = maxValue then
    begin
      if maxArray[i, weightLeft - aEquipmentList.Get(i).Weight] = maxValue - aEquipmentList.Get(i).Value then
      begin
        aOptimizedEquipmentList.Add(aEquipmentList.Get(i).Reference);
        maxValue:= maxValue - aEquipmentList.Get(i).Value;
        weightLeft:= weightLeft - aEquipmentList.Get(i).Weight;
      end;
    end;
  end;
end;

{ TEquipment }

constructor TEquipment.Create;
begin
  FReference:= nil;
  FWeight:= 0;
  FValue:= 0;
end;

{ TEquipmentList }

constructor TEquipmentList.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TEquipmentList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TEquipmentList.Count: integer;
begin
  Result := FList.Count;
end;

function TEquipmentList.Add: TEquipment;
begin
  Result := TEquipment.Create;
  FList.Add(Result);
end;

function TEquipmentList.Get(const aIndex: integer): TEquipment;
begin
  Result := FList.Items[aIndex] as TEquipment;
end;

end.
