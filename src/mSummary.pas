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
  {$interfaces corba}
{$ENDIF}

interface

uses
  Classes, contnrs, db,
  mVirtualDataSetInterfaces, mFloatsManagement, mNullables;

type

  ISummaryPanel = interface
    ['{27724709-FC36-4A12-B154-B92F566F0E94}']
    procedure Hide;
    procedure Show;
    procedure SetSummaryValues (aList : TStringList);
  end;

  TmSummaryOperator = (soCount, soSum, soMax, soMin);

  { TmSummaryDefinition }

  TmSummaryDefinition = class
  strict private
    FFieldName : string;
    FCaption : string;
    FFieldType : TFieldType;
    FSummaryOperator : TmSummaryOperator;
    procedure SetFieldName(AValue: string);
  public
    constructor Create;
    procedure Assign(const aSource : TmSummaryDefinition);

    property FieldName : string read FFieldName write SetFieldName;
    property Caption : string read FCaption write FCaption;
    property SummaryOperator : TmSummaryOperator read FSummaryOperator write FSummaryOperator;
    property FieldType : TFieldType read FFieldType write FFieldType;
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
    procedure Clear;
  end;

  { TmSummaryValue }

  TmSummaryValue = class
  private
    FDoubleValue : TNullableDouble;
    FStringValue: TNullableString;
    FIntegerValue: TNullableInteger;

    FDefinition : TmSummaryDefinition;

    function GetValueAsString: string;
    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ComputeDatumInSummaries (const aDatum : IVDDatum);

    property Definition : TmSummaryDefinition read FDefinition;
    property ValueAsString : string read GetValueAsString;
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

constructor TmSummaryDefinition.Create;
begin
  FFieldName:= '';
  FCaption:= '';
  FFieldType:= ftUnknown;
  FSummaryOperator:= soCount;
end;

procedure TmSummaryDefinition.Assign(const aSource: TmSummaryDefinition);
begin
  FFieldName := aSource.FFieldName;
  FCaption := aSource.FCaption;
  FFieldType := aSource.FFieldType;
  FSummaryOperator := aSource.FSummaryOperator;
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
  Result.FDefinition.Assign(aDefinition);
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
    FIntegerValue.Value:= 0;
end;

function TmSummaryValue.GetValueAsString: string;
begin
  Result := '-';
  if FDefinition.SummaryOperator = soCount then
    Result := FIntegerValue.AsString
  else
  case FDefinition.FieldType of
    ftInteger, ftLargeint:
    begin
      if FIntegerValue.IsNull then
        Result := '-'
      else
        Result := FormatFloat('#,##0', FIntegerValue.Value);
    end;
    ftFloat,ftDateTime, ftDate, ftTime, ftTimeStamp:
    begin
      if FDoubleValue.IsNull then
        Result := '-'
      else
      begin
        if FDefinition.FieldType = ftFloat then
          Result := FormatFloat('#,##0.0000', RoundDoubleToStandardPrecision(FDoubleValue.Value))
        else if FDefinition.FieldType = ftDate then
          Result := DateToStr(Round(FDoubleValue.Value))
        else if FDefinition.FieldType = ftTime then
          Result := TimeToStr(FDoubleValue.Value)
        else
          Result := DateTimeToStr(FDoubleValue.Value);
      end;
    end;
    ftString, ftGuid: Result := FStringValue.AsString;
  end;
end;

constructor TmSummaryValue.Create;
begin
  FDefinition:= TmSummaryDefinition.Create;
  FDoubleValue:= TNullableDouble.Create();
  FStringValue:= TNullableString.Create();
  FIntegerValue:= TNullableInteger.Create();
end;

destructor TmSummaryValue.Destroy;
begin
  FDefinition.Free;
  FDoubleValue.Free;
  FStringValue.Free;
  FIntegerValue.Free;
  inherited Destroy;
end;

procedure TmSummaryValue.ComputeDatumInSummaries(const aDatum: IVDDatum);
var
  tmpValue : Variant;
  tmpInt : integer;
  tmpString : string;
  tmpDouble : double;
begin
  tmpValue := aDatum.GetPropertyByFieldName(FDefinition.FieldName);
  case FDefinition.SummaryOperator of
    soCount:
      begin
        if not VarIsNull (tmpValue) then
          FIntegerValue.Value := FIntegerValue.Value + 1;
      end;
    soSum:
      begin
        if not VarIsNull(tmpValue) then
        begin
          case FDefinition.FieldType of
            ftInteger, ftLargeint:
              begin
                tmpInt:= tmpValue;
                if FIntegerValue.IsNull then
                  FIntegerValue.Value := tmpInt
                else
                  FIntegerValue.Value := FIntegerValue.Value + tmpInt;
              end;
            ftFloat, ftDateTime, ftDate, ftTime, ftTimeStamp:
              begin
                tmpDouble:= RoundDoubleToStandardPrecision(tmpValue);
                if FDoubleValue.IsNull then
                  FDoubleValue.Value := tmpDouble
                else
                  FDoubleValue.Value := FDoubleValue.Value + tmpDouble;
              end;
          end;
        end;
      end;
    soMax:
      begin
        if not VarIsNull(tmpValue) then
        begin
          case FDefinition.FieldType of
            ftInteger, ftLargeint:
              begin
                tmpInt:= tmpValue;
                if FIntegerValue.IsNull then
                  FIntegerValue.Value := tmpInt
                else
                  if FIntegerValue.Value < tmpInt then
                    FIntegerValue.Value := tmpInt;
              end;
            ftFloat, ftDateTime, ftDate, ftTime, ftTimeStamp:
              begin
                tmpDouble:= RoundDoubleToStandardPrecision(tmpValue);
                if FDoubleValue.IsNull then
                  FDoubleValue.Value := tmpDouble
                else
                  if FDoubleValue.Value < tmpDouble then
                    FDoubleValue.Value := tmpDouble;
              end;
            ftString, ftGuid:
              begin
                tmpString := VarToStr(tmpValue);
                if FStringValue.IsNull then
                  FStringValue.Value := tmpString
                else
                  if FStringValue.Value < tmpString then
                    FStringValue.Value := tmpString;
              end;
          end;
        end;
      end;
    soMin:
      begin
        if not VarIsNull(tmpValue) then
        begin
          case FDefinition.FieldType of
            ftInteger, ftLargeint:
              begin
                tmpInt:= tmpValue;
                if FIntegerValue.IsNull then
                  FIntegerValue.Value := tmpInt
                else
                  if FIntegerValue.Value > tmpInt then
                    FIntegerValue.Value := tmpInt;
              end;
            ftFloat, ftDateTime, ftDate, ftTime, ftTimeStamp:
              begin
                tmpDouble:= RoundDoubleToStandardPrecision(tmpValue);
                if FDoubleValue.IsNull then
                  FDoubleValue.Value := tmpDouble
                else
                  if FDoubleValue.Value > tmpDouble then
                    FDoubleValue.Value := tmpDouble;
              end;
            ftString, ftGuid:
              begin
                tmpString := VarToStr(tmpValue);
                if FStringValue.IsNull then
                  FStringValue.Value := tmpString
                else
                  if FStringValue.Value > tmpString then
                    FStringValue.Value := tmpString;
              end;
          end;
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

procedure TmSummaryDefinitions.Clear;
begin
  FList.Clear;
end;

end.
