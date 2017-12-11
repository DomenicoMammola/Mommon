// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSummary;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs,
  mVirtualDataSetInterfaces;

type
  TmSummaryOperator = (soCount, soSum, soMax, soMin);

  { TmSummaryDefinition }

  TmSummaryDefinition = class
  strict private
    FFieldName : string;
    FCaption : string;
    FSummaryOperator : TmSummaryOperator;
    procedure SetFieldName(AValue: string);
  public
    property FieldName : string read FFieldName write SetFieldName;
    property Caption : string read FCaption write FCaption;
    property SummaryOperator : TmSummaryOperator read FSummaryOperator write FSummaryOperator;
  end;


  { TmSummaryDefinitions }

  TmSummaryDefinitions = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function Get (const aIndex : integer) : TmSummaryDefinition;
    function FindByFieldNameAndOperator (const aFieldName : string; const aOperator : TmSummaryOperator) : TmSummaryDefinition;
    function Add : TmSummaryDefinition;
    procedure Delete (const aIndex : integer);
    procedure Remove (const aDefinition : TmSummaryDefinition);
  end;

  { TmSummaryValue }

  TmSummaryValue = class
  private
    FValue : variant;
    FDefinition : TmSummaryDefinition;

    procedure Init;
  public
    constructor Create;

    procedure ComputeDatumInSummaries (const aDatum : IVDDatum);

    property Definition : TmSummaryDefinition read FDefinition;
    property Value : Variant read FValue;
  end;

  { TmSummaryValues }

  TmSummaryValues = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddValue (const aDefinition : TmSummaryDefinition) : TmSummaryValue;
    procedure Clear;
    function Count : integer;
    function Get(const aIndex : integer) : TmSummaryValue;
  end;

  function TmSummaryOperatorToString (const aOperator : TmSummaryOperator) : String;

implementation

uses
  variants, sysutils;

function TmSummaryOperatorToString(const aOperator: TmSummaryOperator): String;
begin
  Result := 'Unknown';
  case aOperator of
    soCount : Result := 'Count';
    soSum : Result := 'Sum';
    soMax : Result := 'Max';
    soMin : Result := 'Min';
  end;
end;

{ TmSummaryDefinition }

procedure TmSummaryDefinition.SetFieldName(AValue: string);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  FCaption := AValue;
end;

{ TmSummaryValues }

constructor TmSummaryValues.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmSummaryValues.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TmSummaryValues.AddValue(const aDefinition: TmSummaryDefinition): TmSummaryValue;
begin
  Result := TmSummaryValue.Create;
  FList.Add(Result);
  Result.FDefinition := aDefinition;
  Result.Init;
end;

procedure TmSummaryValues.Clear;
begin
  FList.Clear;
end;

function TmSummaryValues.Count: integer;
begin
  Result := FList.Count;
end;

function TmSummaryValues.Get(const aIndex: integer): TmSummaryValue;
begin
  Result := FList.Items[aIndex] as TmSummaryValue;
end;

{ TmSummaryValue }

procedure TmSummaryValue.Init;
begin
  if FDefinition.SummaryOperator = soCount then
    FValue:= 0;
end;

constructor TmSummaryValue.Create;
begin
  FDefinition := nil;
  FValue := Null;
end;

procedure TmSummaryValue.ComputeDatumInSummaries(const aDatum: IVDDatum);
var
  tmpValue : Variant;
begin
  tmpValue := aDatum.GetPropertyByFieldName(FDefinition.FieldName);
  case FDefinition.SummaryOperator of
    soCount:
      begin
        if not VarIsNull (tmpValue) then
          FValue := FValue + 1;
      end;
    soSum:
      begin
        if not VarIsNull(tmpValue) then
        begin
          if VarIsNull(FValue) then
            FValue := tmpValue
          else
            FValue := FValue + tmpValue;
        end;
      end;
    soMax:
      begin
        if not VarIsNull(tmpValue) then
        begin
          if VarIsNull(FValue) then
            FValue := tmpValue
          else
            if FValue < tmpValue then
              FValue := tmpValue;
        end;
      end;
    soMin:
      begin
        if not VarIsNull(tmpValue) then
        begin
          if VarIsNull(FValue) then
            FValue := tmpValue
          else
            if FValue > tmpValue then
              FValue := tmpValue;
        end;
      end;
  end;
end;

{ TmSummaryDefinitions }

constructor TmSummaryDefinitions.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TmSummaryDefinitions.Destroy;
begin
  inherited Destroy;
  FList.Free;
end;

function TmSummaryDefinitions.Count: integer;
begin
  Result := FList.Count;
end;

function TmSummaryDefinitions.Get(const aIndex: integer): TmSummaryDefinition;
begin
  Result := (FList.Items[aIndex] as TmSummaryDefinition);
end;

function TmSummaryDefinitions.FindByFieldNameAndOperator (const aFieldName : string; const aOperator : TmSummaryOperator) : TmSummaryDefinition;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Self.Count - 1 do
  begin
    if CompareText(Self.Get(i).FieldName, aFieldName) = 0 then
    begin
      if Self.Get(i).SummaryOperator = aOperator then
      begin
        Result := Self.Get(i);
        break;
      end;
    end;
  end;
end;

function TmSummaryDefinitions.Add: TmSummaryDefinition;
begin
  Result := TmSummaryDefinition.Create;
  FList.Add(Result);
end;

procedure TmSummaryDefinitions.Delete(const aIndex: integer);
begin
  FList.Delete(aIndex);
end;

procedure TmSummaryDefinitions.Remove(const aDefinition: TmSummaryDefinition);
begin
  FList.Remove(aDefinition);
end;

end.
