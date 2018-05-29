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
  mUtility, mFloatsManagement, mNullables,
  mDataProviderInterfaces, mDataProviderFieldDefs;

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
    FValue : double;
    FAttribute1 : TNullableString;
    FAttribute2 : TNullableString;
    FAttribute3 : TNullableString;
    FAttribute4 : TNullableString;
    FEventType : TmUprightEventType;

    FDatumKey : String;
  public
    constructor Create;
    destructor Destroy; override;
    function CompareTo (aOther : TmUprightEvent) : integer;

    property EventDate: TDateTime read FEventDate write FEventDate;
    property Value: double read FValue write FValue;
    property Attribute1 : TNullableString read FAttribute1;
    property Attribute2 : TNullableString read FAttribute2;
    property Attribute3 : TNullableString read FAttribute3;
    property Attribute4 : TNullableString read FAttribute4;
    property DatumKey: string read FDatumKey write FDatumKey;
    property EventType : TmUprightEventType read FEventType write FEventType;
  end;

  { TmUprightDatum }

  TmUprightDatum= class (IVDDatum)
  private
    FEvent : TmUprightEvent;
    FKey : TmUprightValueKey;
    FUpright : double;
    FHighlight : TmUprightHighlightType;
    FLimit : TNullableDouble;
    FRemaining : TNullableDouble;
    function GetPosition: integer;
  public
    const FLD_POSITION = 'POSITION';
    const FLD_DATE = 'DATE';
    const FLD_DELTA = 'DELTA';
    const FLD_UPRIGHT = 'UPRIGHT';
    const FLD_HIGHLIGHT = 'HIGHLIGHT';
    const FLD_LIMIT = 'LIMIT';
    const FLD_REMAINING = 'REMAINING';
    const FLD_ATTRIBUTE1 = 'ATTRIBUTE_1';
    const FLD_ATTRIBUTE2 = 'ATTRIBUTE_2';
    const FLD_ATTRIBUTE3 = 'ATTRIBUTE_3';
    const FLD_ATTRIBUTE4 = 'ATTRIBUTE_4';
  public
    constructor Create;
    destructor Destroy; override;

    function GetDatumKey : IVDDatumKey;
    function GetPropertyByFieldName(const aFieldName : String) : Variant;
    procedure SetPropertyByFieldName(const aFieldName: String; const aValue : Variant);
    function AsObject: TObject;

    class procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; aPrefix : String);
    class function GetKeyField : String;

    property Position : integer read GetPosition;
    property Upright: double read FUpright;
    property Highlight : TmUprightHighlightType read FHighlight;
  end;

  { TmUpright }

  TmUpright = class (IVDDataProvider)
  strict private
    FGarbage : TObjectList;
    FDataList : TFPList;
    FEventsList : TObjectList;
    FLimitsList : TFPList;

    function OnCompareData (Item1, Item2: Pointer): Integer;
    function OnCompareEvents (Item1, Item2: Pointer): Integer;
    procedure CalculateHighlights;
  public
    constructor Create;
    destructor Destroy; override;

    function Add : TmUprightEvent;
    procedure Calculate;
    function Count : integer;
    procedure Clear;
    function GetDatum(const aIndex : integer) : IVDDatum;
    function FindDatumByKey (const aKey : IVDDatumKey): IVDDatum;
    function FindDatumByStringKey (const aStringKey : string): IVDDatum;
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

constructor TmUprightDatum.Create;
begin
  FUpright:= 0;
  FHighlight:= uhNone;
  FKey := TmUprightValueKey.Create;
  FLimit := TNullableDouble.Create;
  FRemaining := TNullableDouble.Create;
end;

destructor TmUprightDatum.Destroy;
begin
  FKey.Free;
  FLimit.Free;
  FRemaining.Free;
end;

constructor TmUprightEvent.Create;
begin
  EventDate:= 0;
  DatumKey:= '';
  Value:= 0;
  EventType:= etuValue;

  FAttribute1 := TNullableString.Create;
  FAttribute2 := TNullableString.Create();
  FAttribute3 := TNullableString.Create();
  FAttribute4 := TNullableString.Create();
end;

destructor TmUprightEvent.Destroy;
begin
  FAttribute1.Free;
  FAttribute2.Free;
  FAttribute3.Free;
  FAttribute4.Free;
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

function TmUprightDatum.GetDatumKey: IVDDatumKey;
begin
  Result := FKey;
end;

function TmUprightDatum.GetPropertyByFieldName(const aFieldName: String): Variant;
begin
  if aFieldName = FLD_POSITION then
    Result := FKey.Position
  else if aFieldName = FLD_DELTA then
    Result := FEvent.Value
  else if aFieldName = FLD_ATTRIBUTE1 then
    Result := FEvent.Attribute1.AsVariant
  else if aFieldName = FLD_ATTRIBUTE2 then
    Result := FEvent.Attribute2.AsVariant
  else if aFieldName = FLD_ATTRIBUTE3 then
    Result := FEvent.Attribute3.AsVariant
  else if aFieldName = FLD_ATTRIBUTE4 then
    Result := FEvent.Attribute4.AsVariant
  else if aFieldName = FLD_UPRIGHT then
    Result := FUpright
  else if aFieldName = FLD_HIGHLIGHT then
    Result := TmUprightHighlightTypeToVariant(FHighlight)
  else if aFieldName = FLD_REMAINING then
    Result := FRemaining.AsVariant
  else if aFieldName = FLD_LIMIT then
    Result := FLimit.AsVariant
  else if aFieldName = FLD_DATE then
    Result := FEvent.EventDate
  else
    Result := Null;
end;

procedure TmUprightDatum.SetPropertyByFieldName(const aFieldName: String; const aValue: Variant);
begin
  // none
end;

function TmUprightDatum.AsObject: TObject;
begin
  Result := Self;
end;

class procedure TmUprightDatum.FillVirtualFieldDefs(aFieldDefs: TmVirtualFieldDefs; aPrefix: String);
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
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_DELTA;
    DataType:= vftFloat;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_UPRIGHT;
    DataType:= vftFloat;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_LIMIT;
    DataType:= vftFloat;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_REMAINING;
    DataType:= vftFloat;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_HIGHLIGHT;
    DataType:= vftString;
    Size := 10;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_ATTRIBUTE1;
    DataType:= vftString;
    Size := 255;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_ATTRIBUTE2;
    DataType:= vftString;
    Size := 255;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_ATTRIBUTE3;
    DataType:= vftString;
    Size := 255;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_ATTRIBUTE4;
    DataType:= vftString;
    Size := 255;
  end;
end;

class function TmUprightDatum.GetKeyField: String;
begin
  Result := FLD_POSITION;
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
  i : integer;
  tmp : TmUprightDatum;
  tmpMin, tmpMax : double;
  idxMin, idxMax : integer;
begin
  if FDataList.Count = 0 then
    exit;

  tmpMin := 0;
  tmpMax := 0;

  for i := 0 to FDataList.Count -1 do
  begin
    tmp:= TmUprightDatum(FDataList.Items[i]);
    tmp.FHighlight := uhNone;
    if i = 0 then
    begin
      tmpMin:= tmp.FUpright;
      tmpMax:= tmp.FUpright;
      idxMin := 0;
      idxMax := 0;
    end
    else
    begin
      if not mFloatsManagement.DoubleIsLessThan(tmpMin, tmp.FUpright) then
      begin
        tmpMin := tmp.FUpright;
        idxMin := i;
      end;
      if mFloatsManagement.DoubleIsLessThan(tmpMax, tmp.FUpright) then
      begin
        tmpMax := tmp.FUpright;
        idxMax := i;
      end;
    end;
  end;
  TmUprightDatum(FDataList.Items[idxMin]).FHighlight := uhMin;
  TmUprightDatum(FDataList.Items[idxMax]).FHighlight := uhMax;
end;

constructor TmUpright.Create;
begin
  FDataList := TFPList.Create;
  FEventsList := TObjectList.Create(true);
  FGarbage := TObjectList.Create(true);
  FLimitsList := TFPList.Create;
end;

destructor TmUpright.Destroy;
begin
  FDataList.Free;
  FEventsList.Free;
  FGarbage.Free;
  FLimitsList.Free;
  inherited Destroy;
end;

function TmUpright.Add: TmUprightEvent;
begin
  Result := TmUprightEvent.Create;
  FEventsList.Add(Result);
end;

procedure TmUpright.Calculate;
var
  i : integer;
  tmpEvent : TmUprightEvent;
  tmpDatum : TmUprightDatum;
  lastValue : double;
  currentLimitIndex : integer;
begin
  FDataList.Clear;
  FLimitsList.Clear;
  FGarbage.Clear;

  for i := 0 to FEventsList.Count - 1 do
  begin
    tmpEvent:= TmUprightEvent(FEventsList.Items[i]);
    if tmpEvent.EventType = etuValue then
    begin
      tmpDatum := TmUprightDatum.Create;
      tmpDatum.FEvent := tmpEvent;
      FDataList.Add(tmpDatum);
      FGarbage.Add(tmpDatum);
    end
    else
    begin
      FLimitsList.Add(tmpEvent);
    end;
  end;

  MergeSort(FDataList, OnCompareData);
  MergeSort(FLimitsList, OnCompareEvents);
  lastValue := 0;

  currentLimitIndex:= -1;

  for i := 0 to FDataList.Count -1 do
  begin
    tmpDatum:= TmUprightDatum(FDataList.Items[i]);

    if (FLimitsList.Count > currentLimitIndex + 1) then
    begin
      tmpEvent := TmUprightEvent(FLimitsList.Items[currentLimitIndex + 1]);
      while Assigned(tmpEvent) and  DoubleIsLessThan(tmpEvent.EventDate, tmpDatum.FEvent.EventDate) do
      begin
        inc(currentLimitIndex);
        if (FLimitsList.Count > currentLimitIndex + 1) then
          tmpEvent := TmUprightEvent(FLimitsList.Items[currentLimitIndex + 1])
        else
          tmpEvent := nil;
      end;
    end;

    tmpDatum.FKey.FPosition := i + 1;
    if i = 0 then
      tmpDatum.FUpright:= tmpDatum.FEvent.Value
    else
      tmpDatum.FUpright:= lastValue + tmpDatum.FEvent.Value;
    lastValue := tmpDatum.FUpright;
    if currentLimitIndex >= 0 then
    begin
      tmpEvent := TmUprightEvent(FLimitsList.Items[currentLimitIndex]);
      tmpDatum.FLimit.Value := tmpEvent.Value;
      tmpDatum.FRemaining.Value := tmpDatum.FLimit.Value + tmpDatum.FUpright;
    end;
  end;
  Self.CalculateHighlights;
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
  FLimitsList.Clear;
end;

function TmUpright.GetDatum(const aIndex: integer): IVDDatum;
begin
  Result := TmUprightDatum(FDataList.Items[aIndex]);
end;

function TmUpright.FindDatumByKey(const aKey: IVDDatumKey): IVDDatum;
begin
  Result := TmUprightDatum(FDataList.Items[TmUprightValueKey(aKey).Position]);
end;

function TmUpright.FindDatumByStringKey(const aStringKey: string): IVDDatum;
begin
  Result := TmUprightDatum(FDataList.Items[StrToInt(aStringKey)]);
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
  aFieldsForLookup.Add(TmUprightDatum.FLD_UPRIGHT);
end;

end.
