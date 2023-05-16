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
  {$INTERFACES CORBA}
{$ENDIF}

interface

uses
  Classes, contnrs,
  mUtility, mFloatsManagement, mNullables, mDoubleList,
  mDataProviderInterfaces, mDataProviderFieldDefs;

const
  MAX_NUM_OF_VALUES = 10;
  MAX_NUM_OF_ATTRIBUTES = 10;

type

  TmUprightHighlightType = (uhNone, uhMax, uhMin);

  { TmUprightValueKey }

  TmUprightValueKey = class (IVDDatumKey)
  private
    FPosition : integer;
  public
    procedure Assign(aSource : TObject);
    function AsString : string;

    property Position : integer read FPosition;
  end;

  TmUprightEventType = (etuValue, etuLimit);

  { TmUprightEvent }

  TmUprightEvent = class
  strict private
    FEventDate : TDateTime;
    FSameDateSortBy : String;
    FValues : array [0..MAX_NUM_OF_VALUES-1] of TNullableDouble;
    FAttributes : array [0..MAX_NUM_OF_ATTRIBUTES-1] of TNullableString;
    FEventType : TmUprightEventType;

    FDatumKey : String;
    FReference : TObject;
    FOwnsReference : boolean;
  public
    constructor Create(const aFractionalPartDigits : byte);
    destructor Destroy; override;
    function CompareTo (aOther : TmUprightEvent) : integer;
    function GetValue (const aIndex : byte) : TNullableDouble;
    function GetAttribute (const aIndex: byte): TNullableString;
    procedure Assign(const aSource : TmUprightEvent);

    property EventDate: TDateTime read FEventDate write FEventDate;
    property SameDateSortBy : String read FSameDateSortBy write FSameDateSortBy;
    property DatumKey: string read FDatumKey write FDatumKey;
    property EventType : TmUprightEventType read FEventType write FEventType;
    property Reference : TObject read FReference write FReference;
    property OwnsReference : boolean read FOwnsReference write FOwnsReference;
  end;

  { TmUprightDatum }

  TmUprightDatum= class (IVDDatum)
  strict private
    const FLD_UPRIGHT = 'UPRIGHT';
    const FLD_ATTRIBUTE = 'ATTRIBUTE';
    const FLD_DELTA = 'DELTA';
    const FLD_HIGHLIGHT = 'HIGHLIGHT';
    const FLD_LIMIT = 'LIMIT';
    const FLD_REMAINING = 'REMAINING';

    const LENGTH_FLD_DELTA = 5;
    const LENGTH_FLD_UPRIGHT = 7;
    const LENGTH_FLD_ATTRIBUTE = 9;
    const LENGTH_FLD_LIMIT = 5;
    const LENGTH_FLD_HIGHLIGHT = 9;
    const LENGTH_FLD_REMAINING = 9;
  private
    FEvent : TmUprightEvent;
    FKey : TmUprightValueKey;
    FUprights : array[0..MAX_NUM_OF_VALUES-1] of TNullableDouble;
    FHighlights : array[0..MAX_NUM_OF_VALUES-1] of TmUprightHighlightType;
    FLimits : array[0..MAX_NUM_OF_VALUES-1] of TNullableDouble;
    FRemainings : array[0..MAX_NUM_OF_VALUES-1] of TNullableDouble;

    function GetPosition: integer;
  public
    const FLD_POSITION = 'POSITION';
    const FLD_DATE = 'DATE';
  public
    constructor Create(const aFractionalPartDigits : byte);
    destructor Destroy; override;

    function GetDatumKey : IVDDatumKey;
    function GetPropertyByFieldName(const aFieldName : String) : Variant;
    function AsObject: TObject;
    function Clone : IVDDatum;

    class procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; aPrefix : String; const aValuesCount : integer = 1; const aAttributesCount : integer = 4);
    class function GetKeyField : String;

    class function GetDeltaFieldName (const aIndex : integer) : String;
    class function GetAttributeFieldName (const aIndex : integer) : String;
    class function GetUprightFieldName (const aIndex : integer) : string;
    class function GetHighLightFieldName(const aIndex : integer): string;
    class function GetLimitFieldName(const aIndex : integer): string;
    class function GetRemainingFieldName(const aIndex : integer): string;

    function GetUpright(const aIndex: byte): TNullableDouble;
    function GetRemaining(const aIndex: byte): TNullableDouble;

    property Position : integer read GetPosition;
    property Event : TmUprightEvent read FEvent;
  end;

  { TmUpright }

  TmUpright = class (IVDDataProvider)
  strict private
    FGarbage : TObjectList;
    FDataList : TFPList;
    FEventsList : TObjectList;
    FFractionalPartDigits : byte;

    function OnCompareData (Item1, Item2: Pointer): Integer;
    function OnCompareEvents (Item1, Item2: Pointer): Integer;
    procedure CalculateHighlights;
  public
    constructor Create(const aFractionalPartDigits : byte);
    destructor Destroy; override;

    function Add : TmUprightEvent;
    procedure Calculate(const aDoSort : boolean = true);
    function Count : integer;
    procedure Clear;
    function GetDatum(const aIndex : integer) : IVDDatum;
    function FindDatumByKey (const aKey : IVDDatumKey): IVDDatum;
    function FindDatumByStringKey (const aStringKey : string): IVDDatum;
    function GetUprightDatum (const aIndex : integer): TmUprightDatum;
    procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String);
    function GetKeyFieldName : String;
    procedure GetMinimumFields(aFieldsForLookup : TStringList);
  end;

  function TmUprightHighlightTypeToVariant (const aValue : TmUprightHighlightType): Variant;

implementation

uses
  sysutils;

function TmUprightHighlightTypeToVariant(const aValue: TmUprightHighlightType): Variant;
begin
  if aValue = uhMax then
    Result := 'MAX'
  else if aValue = uhMin then
    Result := 'MIN'
  else
    Result := Null;
end;

{ TmUprightValueKey }

procedure TmUprightValueKey.Assign(aSource: TObject);
begin
  FPosition := (aSource as TmUprightValueKey).Position;
end;

function TmUprightValueKey.AsString: string;
begin
  Result := IntToStr(FPosition);
end;

{ TmUprightEvent }

function TmUprightDatum.GetPosition: integer;
begin
  Result := FKey.Position;
end;

constructor TmUprightDatum.Create(const aFractionalPartDigits : byte);
var
  i : integer;
begin
  for i := 0 to MAX_NUM_OF_VALUES - 1 do
  begin
    FUprights[i] := TNullableDouble.Create();
    FUprights[i].FractionalPartDigits:= aFractionalPartDigits;
    FHighlights[i] := uhNone;
    FLimits[i] := TNullableDouble.Create();
    FLimits[i].FractionalPartDigits:= aFractionalPartDigits;
    FRemainings[i] := TNullableDouble.Create();
    FRemainings[i].FractionalPartDigits:= aFractionalPartDigits;
  end;

  FKey := TmUprightValueKey.Create;
end;

destructor TmUprightDatum.Destroy;
var
  i : integer;
begin
  FKey.Free;
  for i := 0 to MAX_NUM_OF_VALUES - 1 do
  begin
    FUprights[i].Free;
    FLimits[i].Free;
    FRemainings[i].Free;
  end;
end;

constructor TmUprightEvent.Create(const aFractionalPartDigits : byte);
var
  i : integer;
begin
  EventDate:= 0;
  DatumKey:= '';
  EventType:= etuValue;
  FReference := nil;
  FOwnsReference:= false;

  for i := 0 to MAX_NUM_OF_ATTRIBUTES -1 do
  begin
    FAttributes[i] := TNullableString.Create();
    FValues[i] := TNullableDouble.Create();
    FValues[i].FractionalPartDigits:= aFractionalPartDigits;
  end;
end;

destructor TmUprightEvent.Destroy;
var
  i : integer;
begin
  for i:= 0 to MAX_NUM_OF_ATTRIBUTES - 1 do
  begin
    FAttributes[i].Free;
    FValues[i].Free;
  end;
  if FOwnsReference then
    FreeAndNil(FReference);
  inherited Destroy;
end;

function TmUprightEvent.CompareTo(aOther: TmUprightEvent): integer;
begin
  if DoublesAreEqual(EventDate, aOther.EventDate) then
  begin
    if (SameDateSortBy <> '') and (aOther.SameDateSortBy <> '') then
      Result := CompareStr(SameDateSortBy, aOther.SameDateSortBy)
    else
      Result := 0;
  end
  else if DoubleIsLessThan(EventDate, aOther.EventDate) then
    Result := -1
  else
    Result := 1;
end;

function TmUprightEvent.GetValue(const aIndex: byte): TNullableDouble;
begin
  Result := FValues[aIndex];
end;

function TmUprightEvent.GetAttribute(const aIndex: byte): TNullableString;
begin
  Result := FAttributes[aIndex];
end;

procedure TmUprightEvent.Assign(const aSource: TmUprightEvent);
begin
  EventDate := aSource.EventDate;
  DatumKey := aSource.DatumKey;
  EventType := aSource.EventType;
  if aSource.OwnsReference then
    raise Exception.Create('TmUprightEvent.Assign: unable to clone an owned reference')
  else
  begin
    Reference := aSource.Reference;
    OwnsReference := aSource.OwnsReference;
  end;
end;

function TmUprightDatum.GetDatumKey: IVDDatumKey;
begin
  Result := FKey;
end;

function TmUprightDatum.GetPropertyByFieldName(const aFieldName: String): Variant;
var
  i : integer;
begin
  Result := Null;
  if aFieldName = FLD_POSITION then
    Result := FKey.Position
  else if aFieldName = FLD_DATE then
    Result := FEvent.EventDate
  else begin
    i := Pos(FLD_DELTA, aFieldName);
    if i > 0 then
    begin
      i := StrToInt(Copy(aFieldName, i + LENGTH_FLD_DELTA, 999));
      Result := FEvent.GetValue(i - 1).AsVariant;
    end
    else
    begin
      i := Pos(FLD_UPRIGHT, aFieldName);
      if i > 0 then
      begin
        i := StrToInt(Copy(aFieldName, i + LENGTH_FLD_UPRIGHT, 999));
        Result := GetUpright(i - 1).AsVariant;
      end
      else
      begin
        i := Pos (FLD_ATTRIBUTE, aFieldName);
        if i > 0 then
        begin
          i := StrToInt(Copy(aFieldName, i + LENGTH_FLD_ATTRIBUTE, 999));
          Result := FEvent.GetAttribute(i - 1).AsVariant;
        end
        else
        begin
          i := Pos(FLD_HIGHLIGHT, aFieldName);
          if i > 0 then
          begin
            i := StrToInt(Copy(aFieldName, i + LENGTH_FLD_HIGHLIGHT, 999));
            Result := TmUprightHighlightTypeToVariant(FHighlights[i - 1]);
          end
          else
          begin
            i := Pos(FLD_REMAINING, aFieldName);
            if i > 0 then
            begin
              i := StrToInt(Copy(aFieldName, i + LENGTH_FLD_REMAINING, 999));
              Result := FRemainings[i - 1].AsVariant;
            end
            else
            begin
              i := Pos(FLD_LIMIT, aFieldName);
              if i > 0 then
              begin
                i := StrToInt(Copy(aFieldName, i + LENGTH_FLD_LIMIT, 999));
                Result := FLimits[i - 1].AsVariant;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;


function TmUprightDatum.AsObject: TObject;
begin
  Result := Self;
end;

function TmUprightDatum.Clone: IVDDatum;
begin
  raise Exception.Create('TmUprightDatum.Clone - not implementated');
  Result := nil;
end;

class procedure TmUprightDatum.FillVirtualFieldDefs(aFieldDefs: TmVirtualFieldDefs; aPrefix: String; const aValuesCount : integer = 1; const aAttributesCount : integer = 4);
var
  i : integer;
begin
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_POSITION;
    DataType:= vftInteger;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_DATE;
    DataType := vftDateTime;
  end;
  for i := 0 to aValuesCount - 1 do
  begin
    with aFieldDefs.AddFieldDef do
    begin
      Name := aPrefix + Self.GetDeltaFieldName(i);
      DataType:= vftFloat;
    end;
  end;
  for i := 0 to aValuesCount - 1 do
  begin
    with aFieldDefs.AddFieldDef do
    begin
      Name := aPrefix + Self.GetUprightFieldName(i);
      DataType:= vftFloat;
    end;
    with aFieldDefs.AddFieldDef do
    begin
      Name := aPrefix + Self.GetLimitFieldName(i);
      DataType:= vftFloat;
    end;
    with aFieldDefs.AddFieldDef do
    begin
      Name := aPrefix + Self.GetRemainingFieldName(i);
      DataType:= vftFloat;
    end;
    with aFieldDefs.AddFieldDef do
    begin
      Name := aPrefix + Self.GetHighLightFieldName(i);
      DataType:= vftString;
      Size := 10;
    end;
  end;

  for i := 0 to aAttributesCount - 1 do
  begin
    with aFieldDefs.AddFieldDef do
    begin
      Name := aPrefix + Self.GetAttributeFieldName(i);
      DataType:= vftString;
      Size := 255;
    end;
  end;
end;

class function TmUprightDatum.GetKeyField: String;
begin
  Result := FLD_POSITION;
end;

class function TmUprightDatum.GetDeltaFieldName(const aIndex: integer): String;
begin
  Result := FLD_DELTA + IntToStr(aIndex + 1);
end;

class function TmUprightDatum.GetAttributeFieldName(const aIndex: integer): String;
begin
  Result := FLD_ATTRIBUTE + IntToStr(aIndex + 1);
end;

class function TmUprightDatum.GetUprightFieldName(const aIndex: integer): string;
begin
  Result := FLD_UPRIGHT + IntToStr(aIndex + 1);
end;

class function TmUprightDatum.GetHighLightFieldName(const aIndex: integer): string;
begin
  Result := FLD_HIGHLIGHT + IntToStr(aIndex + 1);
end;

class function TmUprightDatum.GetLimitFieldName(const aIndex: integer): string;
begin
  Result := FLD_LIMIT + IntToStr(aIndex + 1);
end;

class function TmUprightDatum.GetRemainingFieldName(const aIndex: integer): string;
begin
  Result := FLD_REMAINING + IntToStr(aIndex + 1);
end;

function TmUprightDatum.GetUpright(const aIndex: byte): TNullableDouble;
begin
  Result := FUprights[aIndex];
end;

function TmUprightDatum.GetRemaining(const aIndex: byte): TNullableDouble;
begin
  Result := FRemainings[aIndex];
end;

{ TmUpright }

function TmUpright.OnCompareData(Item1, Item2: Pointer): Integer;
begin
  Result := TmUprightDatum(Item1).FEvent.CompareTo(TmUprightDatum(Item2).FEvent) ;
end;

function TmUpright.OnCompareEvents(Item1, Item2: Pointer): Integer;
begin
  Result := TmUprightEvent(Item1).CompareTo(TmUprightEvent(Item2)) ;
end;

procedure TmUpright.CalculateHighlights;
var
  i, k : integer;
  tmp : TmUprightDatum;
  tmpMin, tmpMax : array[0..MAX_NUM_OF_VALUES-1] of double;
  idxMin, idxMax : array[0..MAX_NUM_OF_VALUES-1] of integer;
begin
  if FDataList.Count = 0 then
    exit;

  for k := 0 to MAX_NUM_OF_VALUES -1 do
  begin
    tmpMin[k] := MaxInt;
    tmpMax[k] := -MaxInt;

    idxMin[k] := -1;
    idxMax[k] := -1;
  end;

  for i := 0 to FDataList.Count -1 do
  begin
    tmp:= TmUprightDatum(FDataList.Items[i]);
    for k := 0 to MAX_NUM_OF_VALUES -1 do
    begin
      tmp.FHighlights[k] := uhNone;
      if not mFloatsManagement.DoubleIsLessThan(tmpMin[k], tmp.GetUpright(k).AsFloat) then
      begin
        tmpMin[k] := tmp.GetUpright(k).AsFloat;
        idxMin[k] := i;
      end;
      if mFloatsManagement.DoubleIsLessThan(tmpMax[k], tmp.GetUpright(k).AsFloat) then
      begin
        tmpMax[k] := tmp.GetUpright(k).AsFloat;
        idxMax[k] := i;
      end;
    end;
  end;

  for k := 0 to MAX_NUM_OF_VALUES - 1 do
  begin
    if idxMin[k] >= 0 then
      TmUprightDatum(FDataList.Items[idxMin[k]]).FHighlights[k] := uhMin;
    if idxMax[k] >= 0 then
      TmUprightDatum(FDataList.Items[idxMax[k]]).FHighlights[k] := uhMax;
  end;
end;

constructor TmUpright.Create(const aFractionalPartDigits : byte);
begin
  FFractionalPartDigits:= aFractionalPartDigits;
  FDataList := TFPList.Create;
  FEventsList := TObjectList.Create(true);
  FGarbage := TObjectList.Create(true);
end;

destructor TmUpright.Destroy;
begin
  FDataList.Free;
  FEventsList.Free;
  FGarbage.Free;
  inherited Destroy;
end;

function TmUpright.Add: TmUprightEvent;
begin
  Result := TmUprightEvent.Create(FFractionalPartDigits);
  FEventsList.Add(Result);
end;

procedure TmUpright.Calculate (const aDoSort : boolean = true);
var
  i, k, s, q : integer;
  tmpEvent, tmpLimitEvent : TmUprightEvent;
  tmpDatum : TmUprightDatum;
  currentLimitIndex : array[0..MAX_NUM_OF_VALUES-1] of integer;
  lastValues : array [0..MAX_NUM_OF_VALUES -1] of TNullableDouble;
  tmpLimits : TFPList;
begin
  FDataList.Clear;
  FGarbage.Clear;

  tmpLimits := TFPList.Create;
  try

    for i := 0 to FEventsList.Count - 1 do
    begin
      tmpEvent:= TmUprightEvent(FEventsList.Items[i]);
      if tmpEvent.EventType = etuValue then
      begin
        tmpDatum := TmUprightDatum.Create(FFractionalPartDigits);
        tmpDatum.FEvent := tmpEvent;
        FDataList.Add(tmpDatum);
        FGarbage.Add(tmpDatum);
      end
      else
      begin
        tmpLimits.Add(tmpEvent);
      end;
    end;

    if aDoSort then
    begin
      MergeSort(FDataList, OnCompareData);
      MergeSort(tmpLimits, OnCompareEvents);
    end;

    for i := 0 to MAX_NUM_OF_VALUES -1 do
    begin
      lastValues[i] := TNullableDouble.Create();
      lastValues[i].FractionalPartDigits:= FFractionalPartDigits;
      FGarbage.Add(lastValues[i]);
      currentLimitIndex[i]:= -1;
    end;

    for i := 0 to FDataList.Count -1 do
    begin
      tmpDatum:= TmUprightDatum(FDataList.Items[i]); // datum to be computed

      if tmpLimits.Count > 0 then
      begin
        for k := 0 to MAX_NUM_OF_VALUES - 1 do
        begin
          s := currentLimitIndex[k] + 1;
          for q := s to tmpLimits.Count -1 do
          begin
            tmpLimitEvent := TmUprightEvent(tmpLimits.Items[q]);

            if (DoubleIsLessThan(tmpLimitEvent.EventDate, tmpDatum.FEvent.EventDate) or (DoublesAreEqual(tmpLimitEvent.EventDate, tmpDatum.FEvent.EventDate))) then
            begin
              if (tmpLimitEvent.GetValue(k).NotNull) then
                currentLimitIndex[k] := q;
            end
            else
              break;
          end;
        end;
      end;

      tmpDatum.FKey.FPosition := i + 1;
      for k := 0 to MAX_NUM_OF_VALUES - 1 do
      begin
        tmpDatum.GetUpright(k).Assign(lastValues[k]);
        tmpDatum.GetUpright(k).Add(tmpDatum.FEvent.GetValue(k));
        lastValues[k].Assign(tmpDatum.GetUpright(k));
      end;

      for k := 0 to MAX_NUM_OF_VALUES - 1 do
      begin
        if currentLimitIndex[k] >= 0 then
        begin
          tmpLimitEvent := TmUprightEvent(tmpLimits.Items[currentLimitIndex[k]]);
          tmpDatum.FLimits[k].Assign(tmpLimitEvent.GetValue(k));
          tmpDatum.FRemainings[k].Assign(tmpDatum.FLimits[k]);
          tmpDatum.FRemainings[k].Add(tmpDatum.GetUpright(k));
        end;
      end;
    end;
    Self.CalculateHighlights;

  finally
    tmpLimits.Free;
  end;
end;

function TmUpright.Count: integer;
begin
  Result := FDataList.Count;
end;

procedure TmUpright.Clear;
begin
  FDataList.Clear;
  FEventsList.Clear;
  FGarbage.Clear;
end;

function TmUpright.GetDatum(const aIndex: integer): IVDDatum;
begin
  Result := TmUprightDatum(FDataList.Items[aIndex]);
end;

function TmUpright.FindDatumByKey(const aKey: IVDDatumKey): IVDDatum;
begin
  Result := Self.FindDatumByStringKey(aKey.AsString);
end;

function TmUpright.FindDatumByStringKey(const aStringKey: string): IVDDatum;
begin
  Result := TmUprightDatum(FDataList.Items[StrToInt(aStringKey)]);
end;

function TmUpright.GetUprightDatum(const aIndex: integer): TmUprightDatum;
begin
  Result := TmUprightDatum(FDataList.Items[aIndex]);
end;

procedure TmUpright.FillVirtualFieldDefs(aFieldDefs: TmVirtualFieldDefs;const aPrefix: String);
begin
  TmUprightDatum.FillVirtualFieldDefs(aFieldDefs, aPrefix);
end;

function TmUpright.GetKeyFieldName: String;
begin
  Result := TmUprightDatum.GetKeyField;
end;

procedure TmUpright.GetMinimumFields(aFieldsForLookup: TStringList);
begin
  aFieldsForLookup.Add(TmUprightDatum.FLD_POSITION);
  aFieldsForLookup.Add(TmUprightDatum.FLD_DATE);
  aFieldsForLookup.Add(TmUprightDatum.GetUprightFieldName(0));
end;

end.
