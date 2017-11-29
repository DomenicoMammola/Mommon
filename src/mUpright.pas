// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mUpright;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs,
  mUtility, mFloatsManagement;

type

  { TmUprightEvent }

  TmUprightEvent = class
  strict private
    FEventDate : TDateTime;
    FDelta : double;
    FDescription : string;
    FDatumKey : String;
  private
    FUpright : double;
  public
    constructor Create;
    destructor Destroy; override;
    function CompareTo (aOther : TmUprightEvent) : integer;

    property EventDate: TDateTime read FEventDate write FEventDate;
    property Delta: double read FDelta write FDelta;
    property Description: string read FDescription write FDescription;
    property DatumKey: string read FDatumKey write FDatumKey;
    property Upright: double read FUpright;
  end;

  { TmUpright }

  TmUpright = class
  strict private
    FGarbage : TObjectList;
    FList : TFPList;

    function OnCompare (Item1, Item2: Pointer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add : TmUprightEvent;
    procedure Calculate;
    function Count : integer;
    procedure Clear;

    procedure GetMinMaxValues(var aMin, aMax : double);
  end;

implementation

uses
  Math;

{ TmUprightEvent }

constructor TmUprightEvent.Create;
begin
  EventDate:= 0;
  DatumKey:= '';
  Delta:= 0;
  Description:= '';
  FUpright:= 0;
end;

destructor TmUprightEvent.Destroy;
begin
  inherited Destroy;
end;

function TmUprightEvent.CompareTo(aOther: TmUprightEvent): integer;
begin
  if DoublesAreEqual(EventDate, aOther.EventDate) then
    Result := 0
  else if DoubleIsLessThan(EventDate, aOther.EventDate) then
    Result := -1
  else
    Result := 1;
end;

{ TmUpright }

function TmUpright.OnCompare(Item1, Item2: Pointer): Integer;
begin
  Result := TmUprightEvent(Item1).CompareTo(TmUprightEvent(Item2)) ;
end;

constructor TmUpright.Create;
begin
  FList := TFPList.Create;
  FGarbage := TObjectList.Create(true);
end;

destructor TmUpright.Destroy;
begin
  FList.Free;
  FGarbage.Free;
  inherited Destroy;
end;

function TmUpright.Add: TmUprightEvent;
begin
  Result := TmUprightEvent.Create;
  FList.Add(Result);
  FGarbage.Add(Result);
end;

procedure TmUpright.Calculate;
var
  i : integer;
  tmp : TmUprightEvent;
  lastValue : double;
begin
  MergeSort(FList, OnCompare);
  lastValue := 0;
  for i := 0 to FList.Count -1 do
  begin
    tmp:= TmUprightEvent(FList.Items[i]);
    if i = 0 then
      tmp.FUpright:= tmp.Delta
    else
      tmp.FUpright:= lastValue + tmp.Delta;
    lastValue := tmp.FUpright;
  end;
end;

function TmUpright.Count: integer;
begin
  Result := FList.Count;
end;

procedure TmUpright.Clear;
begin
  FList.Clear;
  FGarbage.Clear;
end;

procedure TmUpright.GetMinMaxValues (var aMin, aMax : double);
var
  i : integer;
  tmp : TmUprightEvent;
begin
  aMin := 0;
  aMax := 0;

  for i := 0 to FList.Count -1 do
  begin
    tmp:= TmUprightEvent(FList.Items[i]);
    if i = 0 then
    begin
      aMin := tmp.FUpright;
      aMax := tmp.FUpright;
    end
    else
    begin
      aMin := Min(aMin, tmp.FUpright);
      aMax := Max(aMax, tmp.FUpright);
    end;
  end;
end;

end.
