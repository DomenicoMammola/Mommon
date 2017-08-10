// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mDoubleList;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs;

type

  { TDoubleList }

  TDoubleList = class(TObject)
  strict private
    FGarbage : TObjectList;
    function Get(Index: Integer): Double;
    procedure Put(Index: Integer; const Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Num: Double): Integer; virtual;
    procedure AddDoubles (DoubleList : TDoubleList);
    function IndexOf(Num: Double): Integer; virtual;
    procedure Clear;
    function Count : integer;

    property Items[Index: Integer]: Double read Get write Put; default;
    property Nums[Index: Integer]: Double read Get write Put;
  end;

implementation

type
  TDoubleShell = class
  public
    Value : Double;
  end;

{ TDoubleList }

function TDoubleList.Get(Index: Integer): Double;
begin
  Result := TDoubleShell(FGarbage.Items[Index]).Value;
end;

procedure TDoubleList.Put(Index: Integer; const Value: Double);
begin
  TDoubleShell(FGarbage.Items[Index]).Value := Value;
end;

constructor TDoubleList.Create;
begin
  FGarbage := TObjectList.Create(true);
end;

destructor TDoubleList.Destroy;
begin
  FGarbage.Free;
  inherited Destroy;
end;

function TDoubleList.Add(Num: Double): Integer;
var
  tmp : TDoubleShell;
begin
  tmp := TDoubleShell.Create;
  tmp.Value:= Num;
  FGarbage.Add(tmp);
end;

procedure TDoubleList.AddDoubles(DoubleList: TDoubleList);
var
  i : integer;
begin
  for i := 0 to DoubleList.Count - 1 do
  begin
    Self.Add(DoubleList.Items[i]);
  end;
end;

function TDoubleList.IndexOf(Num: Double): Integer;
var
  i : integer;
begin
  Result := -1;
  for i := 0 to FGarbage.Count - 1 do
  begin
    if (FGarbage.Items[i] as TDoubleShell).Value = Num then
    begin
      Result := i;
      exit;
    end;
  end;
end;

procedure TDoubleList.Clear;
begin
  FGarbage.Clear;
end;

function TDoubleList.Count: integer;
begin
  Result := FGarbage.Count;
end;


end.
