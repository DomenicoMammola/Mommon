// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mNullables;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$modeswitch advancedrecords}
{$ENDIF}

interface
{$I mDefines.inc}

uses
  db,
  {$IFDEF GRAPHICS_AVAILABLE}
  Graphics,
  {$ENDIF}
  variants, Classes,
  mUtility, mMathUtility, mFloatsManagement;

type


  { TAbstractNullable }
  TAbstractNullable = class abstract
  strict private
    FTagChanged: Boolean;
    FIsNull: Boolean;
    FIsUnassigned: Boolean;
    procedure SetIsNull(aValue : Boolean);
    function GetIsUnassigned: Boolean;
    procedure SetIsUnassigned(AValue: Boolean);
  protected
    function GetNotNull: Boolean;
    function GetTagChanged: Boolean; virtual;
    procedure SetTagChanged (aValue : Boolean); virtual;
    function GetIsNull : Boolean; virtual;
    procedure InternalSetIsNull(aValue : Boolean); virtual;
  public
    constructor Create(); virtual;
    function AsVariant: Variant; virtual; abstract;
    procedure Assign(const aValue: variant); overload; virtual; abstract;

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; virtual; abstract;
    function AsString: String; virtual; abstract;
    function AsJson(const aFieldName : String; const aSkipIfNull : boolean; const aMaxLength: integer = 0): String; virtual;

    property IsNull: Boolean read GetIsNull write SetIsNull;
    property NotNull: Boolean read GetNotNull;
    property IsUnassigned : Boolean read GetIsUnassigned write SetIsUnassigned;
    property TagChanged: Boolean read GetTagChanged;
  end;

  { TNullablesList }

  TNullablesList = class
  strict private
    FList : TList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function Get(const aIndex : integer): TAbstractNullable;
    function AlmostOneChanged: boolean;
    procedure Add(const aNullableItem: TAbstractNullable);
  end;

  TNullableDataType = (tnDateTime, tnString, tnDouble, tnInteger, tnBoolean);

  TNullableDateTime = class;
  TNullableString = class;
  TNullableDouble = class;
  TNullableInteger = class;
  TNullableBoolean = class;

  { TNullableStringRecord }

  TNullableStringRecord = record
  private
    FValue : string;
    // https://forum.lazarus.freepascal.org/index.php?topic=31870.0
    //class operator Initialize (var aRec: TNullableStringRecord);
  public
    IsNull : boolean;
  public
    function Value : string;
    procedure SetNull;
    procedure SetValue (const aValue : String);

    procedure Assign(const aSource : TNullableStringRecord); overload;
    procedure Assign(const aSourceField : TField; const aAllowBlankValue : boolean); overload;
    procedure Assign(const aValue : Variant);overload;
    procedure Assign(const aSource : TNullableString); overload;
    function AsVariant: Variant;
    procedure Trim();
    function AsString : String;
  end;

  { TNullableDoubleRecord }

  TNullableDoubleRecord = record
  private
    FValue : double;
  public
    IsNull : boolean;
  public
    function Value : double;

    procedure SetNull;
    procedure SetValue (const aValue: Double);

    procedure Assign(const aSource : TNullableDoubleRecord); overload;
    procedure Assign(const aSourceField : TField); overload;
    procedure Assign(const aValue : String); overload;
    procedure Assign(const aValue : Variant);overload;
    procedure Assign(const aSource : TNullableDouble); overload;
    function AsVariant: Variant;
    function AsString: String;
    function AsFloat: double;
  end;

  { TNullableDateTimeRecord }

  TNullableDateTimeRecord = record
  private
    FValue : TDateTime;
  public
    IsNull : boolean;
  public
    function Value : TDateTime;
    procedure SetNull;
    procedure SetValue (const aValue : TDateTime);

    procedure Assign(const aSource : TNullableDatetimeRecord); overload;
    procedure Assign(const aSourceField : TField); overload;
    procedure Assign(const aValue : String); overload;
    procedure Assign(const aValue : Variant);overload;
    procedure Assign(const aSource : TNullableDateTime); overload;
    function AsVariant: Variant;
    function AsString (const aShowTime: boolean): String;
  end;

  { TNullableIntegerRecord }

  TNullableIntegerRecord = record
  public
    Value : integer;
    IsNull : boolean;

    procedure SetNull;
    procedure SetValue (const aValue: Integer);

    procedure Assign(const aSource : TNullableIntegerRecord); overload;
    procedure Assign(const aSourceField : TField); overload;
    procedure Assign(const aValue : String); overload;
    procedure Assign(const aValue : Variant);overload;
    procedure Assign(const aSource : TNullableInteger); overload;
    function AsVariant: Variant;
    function AsString: String;
    function AsInteger: integer;
  end;

  { TNullableBooleanRecord }

  TNullableBooleanRecord = record
  public
    Value : Boolean;
    IsNull : Boolean;

    procedure SetNull;
    procedure SetValue (const aValue: Boolean);

    procedure Assign(const aSource : TNullableBooleanRecord); overload;
    procedure Assign(const aSourceField : TField); overload;
    procedure Assign(const aValue : String); overload;
    procedure Assign(const aValue : Variant);overload;
    procedure Assign(const aSource : TNullableBoolean); overload;
    function AsVariant: Variant;
    function AsString: String;
    function AsBoolean: Boolean;
  end;


  { TNullableString }

  TNullableString = class(TAbstractNullable)
  private
    FValue : String;
    function GetValue: String;
    procedure SetValue(AValue: String);
    procedure InternalSetValue(AValue : String);
  public
    constructor Create(); override; overload;
    constructor Create(aValue: String); overload;

    procedure Assign(const aSource : TNullableString); overload;
    procedure Assign(const aSourceField : TField; const aAllowBlankValues : boolean); overload;
    procedure Assign(const aValue : Variant); override; overload;
    procedure Assign(const  aValue : String; const aAllowBlankValue : boolean); overload;
    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; override;
    function AsVariant: Variant; override;
    procedure Trim();
    procedure Uppercase();
    procedure ChangeToBlankIfNull;
    function ValueIsEqual (const aValue : String) : boolean; overload;
    function ValueIsEqual (const aValue : TNullableString) : boolean; overload;
    function ValueIsEqualCaseInsensitive (const aValue : String): boolean;

    class function StringToVariant(const aValue: String; const aAllowBlankValues : boolean): Variant;
    class function VariantToString(const aValue: Variant): String;

    function AsString : String; override;
    function AsUppercaseString : String;

    property Value : String read GetValue write SetValue;
  end;

  { TNullableDateTime }

  TNullableDateTime = class(TAbstractNullable)
    private
      FValue : TDateTime;
      function GetValue : TDateTime;
      procedure SetValue (AValue : TDateTime);
      procedure InternalSetValue (AValue : TDateTime);
    public
      constructor Create(); override; overload;
      constructor Create(aValue: TDateTime); overload;

      procedure Assign(const aSource : TNullableDateTime); overload;
      procedure Assign(const aSourceField : TField); overload;
      procedure Assign (const aValue: Variant); override; overload;
      procedure Assign (const aValue: String);  overload;
      procedure Assign (const aSource : TNullableDateTimeRecord); overload;
      function AsVariant: Variant; override;
      function CheckIfDifferentAndAssign(const aValue: Variant): boolean; override;
      function YearOf: variant;
      function ValueIsEqual (const aValue : TDateTime) : boolean; overload;
      function ValueIsEqual (const aValue : TNullableDateTime) : boolean; overload;

      class function StringToVariant(const aValue : String): Variant;
      class function VariantToString(const aValue: Variant; const aShowTime: boolean): String;

      function AsString (const aShowTime : boolean) : String; overload;
      function AsString : String; override; overload;
      function AsStringForFilename (const aShowTime, aUseSeparators: boolean): String;
      function AsJson(const aFieldName : String; const aSkipIfNull: boolean; const aMaxLength: integer = 0): String; override;

      property Value : TDateTime read GetValue write SetValue;
  end;

  { TNullableTime }

  TNullableTime = class (TAbstractNullable)
  private
    FValue : TDateTime;
    function GetValue : TDateTime;
    procedure SetValue (AValue : TDateTime);
    procedure InternalSetValue (AValue : TDateTime);
  public
    constructor Create(); override; overload;
    constructor Create(aValue: TDateTime); overload;

    procedure Assign(const aSource : TNullableTime); overload;
    procedure Assign(const aSourceField : TField); overload;
    procedure Assign (const aValue: Variant); override; overload;
    procedure Assign (const aValue: String); overload;
    function AsVariant: Variant; override;
    function CheckIfDifferentAndAssign(const aValue: Variant): boolean; override;

    class function StringToVariant(const aValue : String): Variant;
    class function VariantToString(const aValue: Variant): String;

    function AsString : String; override;
    function AsJson(const aFieldName : String; const aSkipIfNull: boolean; const aMaxLength: integer = 0): String; override;

    property Value : TDateTime read GetValue write SetValue;
  end;

  { TNullableDouble }

  TNullableDouble = class (TAbstractNullable)
  private
    FValue: Double;
    FDisplayFormat : string;
    FFractionalPartDigits : byte;
    FRoundingMethod : TRoundingMethod;
    function GetValue : Double;
    procedure SetDisplayFormat(AValue: String);
    procedure SetFractionalPartDigits(AValue: byte);
    procedure SetValue (AValue : Double);
    procedure InternalSetValue (AValue : Double);
  public
    constructor Create(); override; overload;
    constructor Create(aValue: Double); overload;

    procedure Assign(const aSource : TNullableDouble); overload;
    procedure Assign(const aSourceField : TField); overload;
    procedure Assign(const aValue : Variant); override; overload;
    procedure Assign(const aValue: String); overload;
    function AsVariant: Variant; override;

    procedure Add(const aValue : double); overload;
    procedure Add(const aValue : TNullableDouble); overload;

    procedure Subtract(const aValue : double); overload;
    procedure Subtract(const aValue : TNullableDouble); overload;

    class function StringToVariant(const aValue: String): Variant;
    class function VariantToString(const aValue: Variant; const aDisplayFormat : string): String;

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; override;

    function AsString : String; override;
    function AsFloat : Double;
    function AsFormattedString (const aFormat: String): String;
    function AsJson(const aFieldName : String; const aSkipIfNull : boolean; const aMaxLength: integer = 0): String; override;

    property Value : Double read GetValue write SetValue;
    property DisplayFormat : String read FDisplayFormat write SetDisplayFormat;
    property FractionalPartDigits : byte read FFractionalPartDigits write SetFractionalPartDigits;
    property RoundingMethod : TRoundingMethod read FRoundingMethod write FRoundingMethod;
  end;

  { TNullableBoolean }

  TNullableBoolean = class (TAbstractNullable)
  private
    FValue : Boolean;
    function GetValue : Boolean;
    procedure SetValue (AValue : Boolean);
    procedure InternalSetValue (AValue : Boolean);
  public
    constructor Create(); override; overload;
    constructor Create(aValue: Boolean); overload;

    procedure Assign(const aSource : TNullableBoolean); overload;
    procedure Assign(const aSourceField : TField); overload;
    procedure Assign(const aSourceField: TField; const aAllowNull: boolean); overload;
    procedure Assign(const aValue : Variant); override; overload;
    procedure Assign(const aValue : string); overload;

    procedure AddAnd(const aSource : TNullableBoolean); overload;
    procedure AddAnd(const aSourceField : TField); overload;
    procedure AddAnd(const aSourceField: TField; const aAllowNull: boolean); overload;
    procedure AddAnd(const aValue : Variant); overload;
    procedure AddAnd(const aValue : string); overload;

    procedure AddOr(const aSource : TNullableBoolean); overload;
    procedure AddOr(const aSourceField : TField); overload;
    procedure AddOr(const aSourceField: TField; const aAllowNull: boolean); overload;
    procedure AddOr(const aValue : Variant); overload;
    procedure AddOr(const aValue : string); overload;

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; override;
    function AsVariant: Variant; override;
    function AsString: String; override;
    function AsStringUseNumbers : String;
    function AsBoolean: Boolean;
    function ValueIsEqual (const aValue : Boolean) : boolean; overload;
    function ValueIsEqual (const aValue : TNullableBoolean) : boolean; overload;
    function AsJson(const aFieldName : String; const aSkipIfNull : boolean; const aMaxLength: integer = 0): String; override;

    class function StringToVariant(const aValue: String): Variant;
    class function VariantToString(const aValue: Variant): String;
  public
    property Value : Boolean read GetValue write SetValue;
  end;

  { TNullableInteger }

  TNullableInteger = class(TAbstractNullable)
  private
    FValue : Integer;
    function GetValue : Integer;
    procedure SetValue(AValue: Integer);
    procedure InternalSetValue(AValue: Integer);
  public
    constructor Create(); override; overload;
    constructor Create(aValue: Integer); overload;

    procedure Assign(const aSource : TNullableInteger); overload;
    procedure Assign(const aSourceField : TField); overload;
    procedure Assign(const aValue : String); overload;
    procedure Assign(const aValue : Variant); override; overload;
    procedure Assign(const aSource : TNullableIntegerRecord); overload;

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; override;
    function AsVariant: Variant; override;
    function AsString: String; override;
    function AsInteger : integer;
    function AsFormattedString (aFormat: String): String;

    procedure Add (const aValue : integer); overload;
    procedure Add (const aValue : TNullableInteger); overload;

    procedure Subtract (const aValue : integer); overload;
    procedure Subtract (const aValue : TNullableInteger); overload;

    class function StringToVariant(const aValue : String) : Variant;
    class function VariantToString(const aValue : Variant) : String;
  public
    property Value : Integer read GetValue write SetValue;
  end;

  {$IFDEF GRAPHICS_AVAILABLE}
  { TNullableColor }

  TNullableColor = class(TAbstractNullable)
  private
    FValue : TColor;
    function GetValue: TColor;
    procedure SetValue(AValue: TColor);
    procedure InternalSetValue(AValue: TColor);
  public
    constructor Create(); override; overload;
    constructor Create(aValue : TColor); overload;

    procedure Assign(const aSource : TNullableColor); overload;
    procedure Assign(const aSourceField: TField); overload;
    procedure Assign(const aValue : Variant); override; overload;
    procedure Assign(const aValue : String); overload;
    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; override;
    function AsVariant : Variant; override;
    function AsString : String; override;

    property Value : TColor read GetValue write SetValue;
  end;
  {$ENDIF}

  { TNullableValue }

  TNullableValue = class (TAbstractNullable)
  private
    FDataType: TNullableDataType;
    FActualValue : TAbstractNullable;

    procedure SetDataType(AValue: TNullableDataType);
    procedure CheckActualValue;
  protected
    function GetTagChanged: Boolean; override;
    function GetIsNull : Boolean; override;
    procedure InternalSetIsNull(aValue : Boolean); override;
  public
    constructor Create(); override;
    destructor Destroy; override;
    function AsVariant: Variant; override;
    procedure Assign(const aValue: variant); overload; override;
    procedure Assign(const aSource : TNullableValue); overload;

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; override;
    function AsString: String; override;

    property DataType : TNullableDataType read FDataType write SetDataType;
  end;


  { TNullableJsonHelper }

  TNullableJsonHelper = class
  strict private
    FList : TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddValue(const aFieldName: String; const aValue: TAbstractNullable; const aSkipIfNull: boolean; const aMaxLength : integer = 0);
    procedure AddJson(const aJsonString : String);
    function GetJson : String;
  end;


implementation

uses
  sysutils, dateutils
  , mISOTime;

{ TNullableJsonHelper }

constructor TNullableJsonHelper.Create;
begin
  FList := TStringList.Create;
end;

destructor TNullableJsonHelper.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TNullableJsonHelper.AddValue(const aFieldName: String; const aValue: TAbstractNullable; const aSkipIfNull: boolean; const aMaxLength : integer = 0);
var
  tmp : String;
begin
  tmp := aValue.AsJson(aFieldName, aSkipIfNull, aMaxLength);
  if tmp <> '' then
    FList.Add(tmp);
end;

procedure TNullableJsonHelper.AddJson(const aJsonString: String);
begin
  FList.Add(aJsonString);
end;

function TNullableJsonHelper.GetJson: String;
var
  i : integer;
  sep : String;
begin
  sep := '';
  Result := '';
  for i := 0 to FList.Count - 1 do
  begin
    Result := Result + sep + FList.Strings[i];
    sep := ',';
  end;
end;

{ TNullableValue }

procedure TNullableValue.SetDataType(AValue: TNullableDataType);
begin
  if FDataType = AValue then
    Exit;
  if Assigned(FActualValue) then
    raise Exception.Create('[TNullableValue.SetDataType] Datatype already set.');
end;

procedure TNullableValue.CheckActualValue;
begin
  if Assigned(FActualValue) then
  begin
    if ((FDataType = tnDateTime) and (not (FActualValue is TNullableDateTime))) or
       ((FDataType = tnString) and (not (FActualValue is TNullableString))) or
       ((FDataType = tnDouble) and (not (FActualValue is TNullableDouble))) or
       ((FDataType = tnInteger) and (not (FActualValue is TNullableInteger))) or
       ((FDataType = tnBoolean) and (not (FActualValue is TNullableBoolean)))  then
      FreeAndNil(FActualValue);
  end;
  if not Assigned(FActualValue) then
  begin
    if FDataType = tnDateTime then
      FActualValue := TNullableDateTime.Create()
    else if FDataType = tnString then
      FActualValue := TNullableString.Create()
    else if FDataType = tnDouble then
      FActualValue := TNullableDouble.Create()
    else if FDataType = tnInteger then
      FActualValue := TNullableInteger.Create()
    else if FDataType = tnBoolean then
      FActualValue := TNullableBoolean.Create();
  end;
end;

function TNullableValue.GetTagChanged: Boolean;
begin
  CheckActualValue;
  Result := FActualValue.TagChanged;
end;

function TNullableValue.GetIsNull: Boolean;
begin
  CheckActualValue;
  Result := FActualValue.GetIsNull;
end;

procedure TNullableValue.InternalSetIsNull(aValue: Boolean);
begin
  CheckActualValue;
  FActualValue.InternalSetIsNull(aValue);
end;

constructor TNullableValue.Create();
begin
  inherited;
  FActualValue := nil;
  FDataType:= tnString;
end;

destructor TNullableValue.Destroy;
begin
  FreeAndNil(FActualValue);
  inherited Destroy;
end;

function TNullableValue.AsVariant: Variant;
begin
  CheckActualValue;
  Result := FActualValue.AsVariant;
end;

procedure TNullableValue.Assign(const aValue: variant);
begin
  CheckActualValue;
  FActualValue.Assign(aValue);
end;

procedure TNullableValue.Assign(const aSource: TNullableValue);
begin
  Self.DataType:= aSource.DataType;
  Self.Assign(aSource.AsVariant);
end;

function TNullableValue.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
begin
  CheckActualValue;
  Result := FActualValue.CheckIfDifferentAndAssign(aValue);
end;

function TNullableValue.AsString: String;
begin
  CheckActualValue;
  Result := FActualValue.AsString;
end;

{ TNullableBooleanRecord }

procedure TNullableBooleanRecord.SetNull;
begin
  IsNull:= true;
  Value:= false;
end;

procedure TNullableBooleanRecord.SetValue(const aValue: Boolean);
begin
  IsNull:= false;
  Value:= aValue;
end;

procedure TNullableBooleanRecord.Assign(const aSource: TNullableBooleanRecord);
begin
  IsNull:= aSource.IsNull;
  Value:= aSource.Value;
end;

procedure TNullableBooleanRecord.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    SetNull
  else
    SetValue(aSourceField.AsBoolean);
end;

procedure TNullableBooleanRecord.Assign(const aValue: String);
begin
  Assign(TNullableBoolean.StringToVariant(aValue));
end;

procedure TNullableBooleanRecord.Assign(const aValue: Variant);
var
  tmpBool : Boolean;
begin
  if VarIsNull(aValue) then
  begin
    SetNull;
  end
  else
  begin
    tmpBool := aValue;
    SetValue(tmpBool);
  end;
end;

procedure TNullableBooleanRecord.Assign(const aSource: TNullableBoolean);
begin
  if aSource.IsNull then
    SetNull
  else
    SetValue(aSource.Value);
end;

function TNullableBooleanRecord.AsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := Value;
end;

function TNullableBooleanRecord.AsString: String;
begin
  if IsNull then
    Result := ''
  else
    Result := BoolToStr(Value, true);
end;

function TNullableBooleanRecord.AsBoolean: Boolean;
begin
  if IsNull then
    Result := false
  else
    Result := Value;
end;

{ TNullableIntegerRecord }

procedure TNullableIntegerRecord.SetNull;
begin
  IsNull:= true;
  Value:= 0;
end;

procedure TNullableIntegerRecord.SetValue(const aValue: Integer);
begin
  IsNull:= false;
  Value:= aValue;
end;

procedure TNullableIntegerRecord.Assign(const aSource: TNullableIntegerRecord);
begin
  IsNull:= aSource.IsNull;
  Value:= aSource.Value;
end;

procedure TNullableIntegerRecord.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    SetNull
  else
    SetValue(aSourceField.AsInteger);
end;

procedure TNullableIntegerRecord.Assign(const aValue: String);
begin
  Assign(TNullableInteger.StringToVariant(aValue));
end;

procedure TNullableIntegerRecord.Assign(const aValue: Variant);
var
  tmpInt : integer;
begin
  if VarIsNull(aValue) then
  begin
    SetNull;
  end
  else
  begin
    tmpInt := aValue;
    SetValue(tmpInt);
  end;
end;

procedure TNullableIntegerRecord.Assign(const aSource: TNullableInteger);
begin
  if aSource.IsNull then
    SetNull
  else
    SetValue(aSource.Value);
end;

function TNullableIntegerRecord.AsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := Value;
end;

function TNullableIntegerRecord.AsString: String;
begin
  if IsNull then
    Result := ''
  else
    Result := IntToStr(Value);
end;

function TNullableIntegerRecord.AsInteger: integer;
begin
  if IsNull then
    Result := 0
  else
    Result := Value;
end;

{ TNullableDateTimeRecord }

function TNullableDateTimeRecord.Value: TDateTime;
begin
  if Self.IsNull then
    Result := 0
  else
    Result := FValue;
end;

procedure TNullableDateTimeRecord.SetNull;
begin
  IsNull:= true;
  FValue:= 0;
end;

procedure TNullableDateTimeRecord.SetValue(const aValue: TDateTime);
begin
  IsNull:= false;
  FValue:= aValue;
end;

procedure TNullableDateTimeRecord.Assign(const aSource: TNullableDatetimeRecord);
begin
  if aSource.IsNull then
    Self.SetNull
  else
    Self.SetValue(aSource.Value);
end;

procedure TNullableDateTimeRecord.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    SetNull
  else
    SetValue(aSourceField.AsDateTime);
end;

procedure TNullableDateTimeRecord.Assign(const aValue: String);
begin
  Assign(TNullableDateTime.StringToVariant(aValue));
end;

procedure TNullableDateTimeRecord.Assign(const aValue: Variant);
var
  tmpDateTime : TDateTime;
begin
  if VarIsNull(aValue) then
    SetNull
  else
  begin
    tmpDateTime := aValue;
    SetValue(tmpDateTime);
  end;
end;

procedure TNullableDateTimeRecord.Assign(const aSource: TNullableDateTime);
begin
  if aSource.IsNull then
    SetNull
  else
    SetValue(aSource.Value);
end;

function TNullableDateTimeRecord.AsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := Value;
end;

function TNullableDateTimeRecord.AsString(const aShowTime: boolean): String;
begin
  if IsNull then
    Result := ''
  else
  begin
    if aShowTime then
      Result := DateTimeToStr(Value)
    else
      Result := DateToStr(Value);
  end;
end;

{ TNullableTime }

function TNullableTime.GetValue: TDateTime;
begin
  Result := FValue;
end;

procedure TNullableTime.SetValue(AValue: TDateTime);
begin
  InternalSetValue(AValue);
  SetTagChanged(false);
end;

procedure TNullableTime.InternalSetValue(AValue: TDateTime);
begin
  FValue:= aValue;
  InternalSetIsNull(false);
end;

constructor TNullableTime.Create();
begin
  inherited Create();
end;

constructor TNullableTime.Create(aValue: TDateTime);
begin
  inherited Create;
  SetValue(aValue);
end;

procedure TNullableTime.Assign(const aSource: TNullableTime);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
  SetTagChanged(false);
end;

procedure TNullableTime.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
  begin
    if aSourceField.DataType in [ftDateTime, ftTime, ftTimeStamp, ftDate] then
      Self.Value:= aSourceField.AsDateTime
    else
      Self.Assign(aSourceField.AsString);
  end;
  SetTagChanged(false);
end;

procedure TNullableTime.Assign(const aValue: Variant);
begin
  Self.CheckIfDifferentAndAssign(aValue);
  SetTagChanged(false);
end;

procedure TNullableTime.Assign(const aValue: String);
begin
  Self.Assign(TNullableTime.StringToVariant(aValue));
end;

function TNullableTime.AsVariant: Variant;
begin
  if Self.GetIsNull then
    Result := Null
  else
    Result := FValue;
end;

function TNullableTime.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpTime : TDateTime;
  SavedIsUnassigned : boolean;
begin
  SavedIsUnassigned:= IsUnassigned;
  if VarIsNull(aValue) then
  begin
    Result := Self.NotNull;
    Self.IsNull:= true;
  end
  else
  begin
    tmpTime := aValue;
    if Self.IsNull then
      Result := true
    else
      Result := not DoublesAreEqual(Self.Value, tmpTime);
    if Result then
      Self.InternalSetValue(tmpTime);
  end;
  Self.SetTagChanged(SavedIsUnassigned or Result or Self.GetTagChanged);
end;

class function TNullableTime.StringToVariant(const aValue: String): Variant;
var
  tmptime : TDateTime;
  newValue : String;
begin
  newValue := Trim(aValue);
  if newValue = '' then
    Result := Null
  else
    if TryToUnderstandTimeString(newValue, tmptime) then
      Result := tmptime
    else
      raise Exception.Create('This string cannot be converted to time: ' + aValue);
end;

class function TNullableTime.VariantToString(const aValue: Variant): String;
begin
  if VarIsNull(aValue) then
    Result := ''
  else
  begin
    Result := TimeToStr(aValue);
  end;
end;

function TNullableTime.AsString: String;
begin
  Result := TNullableTime.VariantToString(Self.AsVariant);
end;

function TNullableTime.AsJson(const aFieldName: String; const aSkipIfNull: boolean; const aMaxLength: integer = 0): String;
begin
  if Self.IsNull and aSkipIfNull then
    Result := ''
  else
    Result := '"' + aFieldName + '":"' + ISOTimeToStr(Self.Value) + '"';
end;

{ TNullablesList }

constructor TNullablesList.Create;
begin
  FList:= TList.Create;
end;

destructor TNullablesList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TNullablesList.Count: integer;
begin
  Result := FList.Count;
end;

function TNullablesList.Get(const aIndex: integer): TAbstractNullable;
begin
  Result := TAbstractNullable(FList.Items[aIndex]);
end;

function TNullablesList.AlmostOneChanged: boolean;
var
  i : integer;
begin
  Result := false;
  for i := 0 to Self.Count -1 do
  begin
    Result := Self.Get(i).TagChanged;
    if Result then
      exit;
  end;
end;

procedure TNullablesList.Add(const aNullableItem: TAbstractNullable);
begin
  FList.Add(aNullableItem);
end;

{ TNullableDoubleRecord }

function TNullableDoubleRecord.Value: double;
begin
  if IsNull then
    Result := 0
  else
    Result := FValue;
end;

procedure TNullableDoubleRecord.SetNull;
begin
  IsNull:= true;
  FValue:= 0;
end;

procedure TNullableDoubleRecord.SetValue(const aValue: Double);
begin
  IsNull:= false;
  FValue:= aValue;
end;

procedure TNullableDoubleRecord.Assign(const aSource: TNullableDoubleRecord);
begin
  if aSource.IsNull then
    SetNull
  else
    SetValue(aSource.Value);
end;

procedure TNullableDoubleRecord.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    SetNull
  else
    SetValue(aSourceField.AsFloat);
end;

procedure TNullableDoubleRecord.Assign(const aValue: String);
begin
  Assign(TNullableDouble.StringToVariant(aValue));
end;

procedure TNullableDoubleRecord.Assign(const aValue: Variant);
var
  tmpDouble : double;
begin
  if VarIsNull(aValue) then
    SetNull
  else
  begin
    tmpDouble := aValue;
    SetValue(tmpDouble);
  end;
end;

procedure TNullableDoubleRecord.Assign(const aSource: TNullableDouble);
begin
  if aSource.IsNull then
    SetNull
  else
    SetValue(aSource.Value);
end;


function TNullableDoubleRecord.AsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := Value;
end;

function TNullableDoubleRecord.AsString: String;
begin
  if IsNull then
    Result := ''
  else
    Result := FloatToStr(Value);
end;

function TNullableDoubleRecord.AsFloat: double;
begin
  if IsNull then
    Result := 0
  else
    Result := Value;
end;

{ TNullableStringRecord }

//class operator TNullableStringRecord.Initialize(var aRec: TNullableStringRecord);
//begin
//  SetNull;
//end;

function TNullableStringRecord.Value: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FValue;
end;

procedure TNullableStringRecord.SetNull;
begin
  IsNull:= true;
  FValue := '';
end;

procedure TNullableStringRecord.SetValue(const aValue: String);
begin
  IsNull := false;
  FValue:= aValue;
end;

procedure TNullableStringRecord.Assign(const aSource: TNullableStringRecord);
begin
  if aSource.IsNull then
    Self.SetNull
  else
    Self.SetValue(aSource.Value);
end;

procedure TNullableStringRecord.Assign(const aSourceField: TField; const aAllowBlankValue : boolean);
begin
  if aSourceField.IsNull then
    SetNull
  else
  begin
    if (not aAllowBlankValue) and (aSourceField.AsString = '') then
      SetNull
    else
      SetValue(aSourceField.AsString);
  end;
end;

procedure TNullableStringRecord.Assign(const aValue: Variant);
begin
  if VarIsNull(aValue) then
    SetNull
  else
    SetValue(VarToStr(aValue));
end;

procedure TNullableStringRecord.Assign(const aSource: TNullableString);
begin
  if aSource.IsNull then
    SetNull
  else
    SetValue(aSource.Value);
end;

function TNullableStringRecord.AsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := Value;
end;

procedure TNullableStringRecord.Trim();
begin
  if not IsNull then
    SetValue(SysUtils.Trim(Self.Value));
end;

function TNullableStringRecord.AsString: String;
begin
  if IsNull then
    Result := ''
  else
    Result := Value;
end;

{$IFDEF GRAPHICS_AVAILABLE}

{ TNullableColor }

function TNullableColor.GetValue: TColor;
begin
  Result := FValue;
end;

procedure TNullableColor.SetValue(AValue: TColor);
begin
  InternalSetValue(AValue);
  SetTagChanged(false);
end;

procedure TNullableColor.InternalSetValue(AValue: TColor);
begin
  FValue:= aValue;
  InternalSetIsNull(false);
end;

constructor TNullableColor.Create();
begin
  inherited Create;
end;

constructor TNullableColor.Create(aValue: TColor);
begin
  inherited Create;
  SetValue(aValue);
end;

procedure TNullableColor.Assign(const aSource: TNullableColor);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
  SetTagChanged(false);
end;

procedure TNullableColor.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= StringToColor(aSourceField.AsString);
  SetTagChanged(false);
end;

procedure TNullableColor.Assign(const aValue: Variant);
var
  str : String;
begin
  if VarIsNull(aValue) then
    Self.IsNull:= true
  else
  begin
    str := VarToStr(aValue);
    if str = '' then
      Self.IsNull:= true
    else
      Self.Value:= StringToColor(VarToStr(aValue));
  end;
  SetTagChanged(false);
end;

procedure TNullableColor.Assign(const aValue: String);
var
  tmp : Variant;
begin
  tmp := aValue;
  Self.Assign(tmp);
  SetTagChanged(false);
end;

function TNullableColor.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpValue : TColor;
  SavedIsUnassigned : boolean;
begin
  SavedIsUnassigned:= IsUnassigned;

  if VarIsNull(aValue) then
  begin
    Result:= Self.NotNull;
    Self.IsNull:= true
  end
  else
  begin
    tmpValue:= StringToColor(VarToStr(aValue));
    if Self.IsNull then
      Result:= true
    else
      Result:= tmpValue <> FValue;
    if Result then
      Self.InternalSetValue(tmpValue);
  end;
  Self.SetTagChanged(SavedIsUnassigned or Result or GetTagChanged);
end;

function TNullableColor.AsVariant: Variant;
begin
  if Self.GetIsNull then
    Result := Null
  else
    Result := ColorToString(FValue);
end;

function TNullableColor.AsString: String;
begin
  if Self.IsNull then
    Result := ''
  else
    Result := ColorToString(Self.Value);
end;
{$ENDIF}

{ TNullableInteger }

function TNullableInteger.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TNullableInteger.SetValue(AValue: Integer);
begin
  InternalSetValue(AValue);
  SetTagChanged(false);
end;

procedure TNullableInteger.InternalSetValue(AValue: Integer);
begin
  FValue:= aValue;
  InternalSetIsNull(false);
end;

constructor TNullableInteger.Create();
begin
  inherited Create;
end;

constructor TNullableInteger.Create(aValue: Integer);
begin
  inherited Create;
  SetValue(aValue);
end;

procedure TNullableInteger.Assign(const aSource: TNullableInteger);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
  SetTagChanged(false);
end;

procedure TNullableInteger.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= aSourceField.AsInteger;
  SetTagChanged(false);
end;

procedure TNullableInteger.Assign(const aValue: String);
begin
  Self.Assign(TNullableInteger.StringToVariant(aValue));
end;

procedure TNullableInteger.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
  SetTagChanged(false);
end;

procedure TNullableInteger.Assign(const aSource: TNullableIntegerRecord);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
  SetTagChanged(false);
end;

function TNullableInteger.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpInt : integer;
  SavedIsUnassigned : boolean;
begin
  SavedIsUnassigned:= IsUnassigned;

  if VarIsNull(aValue) then
  begin
    Result := Self.NotNull;
    Self.IsNull:= true;
  end
  else
  begin
    tmpInt := aValue;
    if Self.IsNull then
      Result := true
    else
      Result := tmpInt <> Self.Value;
    if Result then
      Self.InternalSetValue(tmpInt);
  end;
  Self.SetTagChanged(SavedIsUnassigned or Result or GetTagChanged);
end;

function TNullableInteger.AsVariant: Variant;
begin
  if Self.GetIsNull then
    Result := Null
  else
    Result := FValue;
end;

function TNullableInteger.AsString: String;
begin
  Result := TNullableInteger.VariantToString(Self.AsVariant);
end;

function TNullableInteger.AsInteger: integer;
begin
  if Self.IsNull then
    Result := 0
  else
    Result := FValue;
end;

function TNullableInteger.AsFormattedString(aFormat: String): String;
begin
  if Self.IsNull then
    Result := ''
  else
    Result := FormatFloat(aFormat, Self.Value);
end;


procedure TNullableInteger.Add(const aValue: integer);
begin
  if Self.IsNull then
    Self.SetValue(aValue)
  else
    Self.SetValue(aValue + Self.Value);
end;

procedure TNullableInteger.Add(const aValue: TNullableInteger);
begin
  if aValue.NotNull then
    Self.Add(aValue.Value);
end;

procedure TNullableInteger.Subtract(const aValue: integer);
begin
  if Self.IsNull then
    Self.SetValue(-1 * aValue)
  else
    Self.SetValue(Self.Value - aValue);
end;

procedure TNullableInteger.Subtract(const aValue: TNullableInteger);
begin
  if aValue.NotNull then
    Self.Subtract(aValue.Value);
end;

class function TNullableInteger.StringToVariant(const aValue: String): Variant;
var
  tmp : String;
  tmpValue : Integer;
begin
  tmp := Trim(aValue);
  if (tmp = '') then
    Result := Null
  else
    if mMathUtility.TryToConvertToInteger(tmp, tmpValue) then
      Result := tmpValue
    else
      Result := Null;
end;

class function TNullableInteger.VariantToString(const aValue: Variant): String;
begin
  if VarIsNull(aValue) then
    Result := ''
  else
    Result := IntToStr(aValue);
end;

{ TNullableBoolean }

function TNullableBoolean.GetValue: Boolean;
begin
  Result := FValue;
end;

procedure TNullableBoolean.SetValue(AValue: Boolean);
begin
  InternalSetValue(AValue);
  SetTagChanged(false);
end;

procedure TNullableBoolean.InternalSetValue(AValue: Boolean);
begin
  FValue:= aValue;
  InternalSetIsNull(false);
end;

constructor TNullableBoolean.Create();
begin
  inherited Create;
end;

constructor TNullableBoolean.Create(aValue: Boolean);
begin
  inherited Create;
  SetValue(aValue);
end;

procedure TNullableBoolean.Assign(const aSource: TNullableBoolean);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
  SetTagChanged(false);
end;

procedure TNullableBoolean.Assign(const aSourceField: TField);
begin
  Self.Assign(aSourceField, true);
end;

procedure TNullableBoolean.Assign(const aSourceField: TField; const aAllowNull: boolean);
begin
  if aSourceField.IsNull then
  begin
    if aAllowNull then
      Self.IsNull:= true
    else
      Self.Value := false;
  end
  else
  begin
    if aSourceField.DataType = ftBoolean then
      Self.Value:= aSourceField.AsBoolean
    else if aSourceField.DataType = ftString then
      Self.Value:= (aSourceField.AsString = 'T') or (aSourceField.AsString = '1') or (aSourceField.AsString = 'Y') or (aSourceField.AsString = 'S')
    else if (aSourceField.DataType = ftInteger) or (aSourceField.DataType = ftSmallint) or (aSourceField.DataType= ftLargeint) then
      Self.Value:= (aSourceField.AsInteger = 1)
    else if aSourceField.DataType = ftFixedChar then
      Self.Value := (aSourceField.AsString = 'T') or (aSourceField.AsString = '1') or (aSourceField.AsString = 'Y') or (aSourceField.AsString = 'S')
    else
      raise Exception.Create('TNullableBoolean: cannot assign value from field of type ' + Fieldtypenames[aSourceField.DataType]);
  end;
  SetTagChanged(false);
end;

procedure TNullableBoolean.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
  SetTagChanged(false);
end;

procedure TNullableBoolean.Assign(const aValue: string);
begin
  Self.Assign(TNullableBoolean.StringToVariant(aValue));
end;

procedure TNullableBoolean.AddAnd(const aSource: TNullableBoolean);
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  try
    tmp.Assign(aSource);
    Self.Value:= tmp.AsBoolean and Self.AsBoolean;
  finally
    tmp.Free;
  end;
end;

procedure TNullableBoolean.AddAnd(const aSourceField: TField);
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  try
    tmp.Assign(aSourceField);
    Self.Value:= tmp.AsBoolean and Self.AsBoolean;
  finally
    tmp.Free;
  end;
end;

procedure TNullableBoolean.AddAnd(const aSourceField: TField; const aAllowNull: boolean);
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  try
    tmp.Assign(aSourceField, aAllowNull);
    Self.Value:= tmp.AsBoolean and Self.AsBoolean;
  finally
    tmp.Free;
  end;
end;

procedure TNullableBoolean.AddAnd(const aValue: Variant);
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  try
    tmp.Assign(aValue);
    Self.Value:= tmp.AsBoolean and Self.AsBoolean;
  finally
    tmp.Free;
  end;
end;

procedure TNullableBoolean.AddAnd(const aValue: string);
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  try
    tmp.Assign(aValue);
    Self.Value:= tmp.AsBoolean and Self.AsBoolean;
  finally
    tmp.Free;
  end;
end;

procedure TNullableBoolean.AddOr(const aSource: TNullableBoolean);
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  try
    tmp.Assign(aSource);
    Self.Value:= tmp.AsBoolean or Self.AsBoolean;
  finally
    tmp.Free;
  end;

end;

procedure TNullableBoolean.AddOr(const aSourceField: TField);
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  try
    tmp.Assign(aSourceField);
    Self.Value:= tmp.AsBoolean or Self.AsBoolean;
  finally
    tmp.Free;
  end;
end;

procedure TNullableBoolean.AddOr(const aSourceField: TField; const aAllowNull: boolean);
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  try
    tmp.Assign(aSourceField, aAllowNull);
    Self.Value:= tmp.AsBoolean or Self.AsBoolean;
  finally
    tmp.Free;
  end;
end;

procedure TNullableBoolean.AddOr(const aValue: Variant);
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  try
    tmp.Assign(aValue);
    Self.Value:= tmp.AsBoolean or Self.AsBoolean;
  finally
    tmp.Free;
  end;
end;

procedure TNullableBoolean.AddOr(const aValue: string);
var
  tmp : TNullableBoolean;
begin
  tmp := TNullableBoolean.Create();
  try
    tmp.Assign(aValue);
    Self.Value:= tmp.AsBoolean or Self.AsBoolean;
  finally
    tmp.Free;
  end;
end;

function TNullableBoolean.AsVariant: Variant;
begin
  if Self.GetIsNull then
    Result := Null
  else
    Result := FValue;
end;

function TNullableBoolean.AsString: String;
begin
  if Self.NotNull then
    Result := BoolToStr(FValue, true)
  else
    Result := '';
end;

function TNullableBoolean.AsStringUseNumbers: String;
begin
  if Self.NotNull then
    Result := BoolToStr(FValue, '1', '0')
  else
    Result := '';
end;

function TNullableBoolean.AsBoolean: Boolean;
begin
  Result := (Self.NotNull) and (Self.Value);
end;

function TNullableBoolean.ValueIsEqual(const aValue: Boolean): boolean;
begin
  Result := Self.AsBoolean = aValue;
end;

function TNullableBoolean.ValueIsEqual(const aValue: TNullableBoolean): boolean;
begin
  if Self.IsNull then
    Result := aValue.IsNull
  else
    Result := aValue.NotNull and ValueIsEqual(aValue.Value);
end;

function TNullableBoolean.AsJson(const aFieldName: String; const aSkipIfNull: boolean; const aMaxLength: integer = 0): String;
begin
  if Self.IsNull and aSkipIfNull then
    Result := ''
  else
    Result := '"' + aFieldName + '":"' + BoolToStr(Self.AsBoolean, 'true', 'false') + '"';
end;

function TNullableBoolean.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpBool : boolean;
  SavedIsUnassigned : boolean;
begin
  SavedIsUnassigned:= IsUnassigned;
  if VarIsNull(aValue) then
  begin
    Result := Self.NotNull;
    Self.IsNull:= true;
  end
  else
  begin
    tmpBool := aValue;
    if Self.IsNull then
      Result := true
    else
      Result := tmpBool <> Self.Value;
    if Result then
      Self.InternalSetValue(tmpBool);
  end;
  Self.SetTagChanged(SavedIsUnassigned or Result or Self.GetTagChanged);
end;

class function TNullableBoolean.StringToVariant(const aValue: String): Variant;
var
  tmp : boolean;
begin
  if (aValue = '') then
    Result := Null
  else
    if TryToUnderstandBooleanString(aValue, tmp) then
      Result := tmp
    else
      Result := Null;
end;

class function TNullableBoolean.VariantToString(const aValue: Variant): String;
begin
  if VarIsNull(aValue) then
    Result := ''
  else
    Result := BoolToStr(aValue, true);
end;

{ TNullableString }

function TNullableString.GetValue: String;
begin
  if (GetIsNull) then
    Result:= ''
  else
    Result:= FValue;
end;

procedure TNullableString.SetValue(AValue: String);
begin
  InternalSetValue(AValue);
  SetTagChanged(false);
end;

procedure TNullableString.InternalSetValue(AValue: String);
begin
  FValue:= aValue;
  InternalSetIsNull(False);
end;

constructor TNullableString.Create();
begin
  inherited Create;
end;

constructor TNullableString.Create(aValue: String);
begin
  inherited Create;
  SetValue(aValue);
end;

procedure TNullableString.Assign(const aSource: TNullableString);
begin
  if not aSource.IsNull then
    Self.Value:= aSource.Value
  else
    Self.IsNull:= true;
  SetTagChanged(false);
end;

procedure TNullableString.Assign(const aSourceField: TField; const aAllowBlankValues : boolean);
var
  lg : integer;
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
  begin
    Self.Assign(TNullableString.StringToVariant(aSourceField.AsString, aAllowBlankValues));
    if Self.NotNull and (aSourceField.DataType = ftGuid) then
    begin
      Self.Trim();
      // remove braces
      lg := Length(Self.AsString);
      if (Self.AsString[1] = '{') and (Self.AsString[lg] = '}') then
        Self.Value := Copy(Self.AsString, 2, lg - 2);
    end;
  end;
  SetTagChanged(false);
end;

procedure TNullableString.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
  SetTagChanged(false);
end;

procedure TNullableString.Assign(const aValue: String; const aAllowBlankValue: boolean);
begin
  if (not aAllowBlankValue) and (aValue = '') then
    Self.IsNull := true
  else
    Self.InternalSetValue(aValue);
  SetTagChanged(false);
end;

function TNullableString.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  SavedIsUnassigned : boolean;
begin
  SavedIsUnassigned:= IsUnassigned;
  if VarIsNull(aValue) then
  begin
    Result:= Self.NotNull;
    Self.IsNull:= true;
  end
  else
  begin
    if Self.IsNull then
      Result:= true
    else
      Result:= aValue <> FValue;
    if Result then
      Self.InternalSetValue(aValue);
  end;
  SetTagChanged(SavedIsUnassigned or Result or GetTagChanged);
end;

function TNullableString.AsVariant: Variant;
begin
  if Self.GetIsNull then
    Result := Null
  else
    Result := FValue;
end;

procedure TNullableString.Trim();
begin
  if not Self.IsNull then
    Self.Value:= SysUtils.Trim(Self.Value);
end;

procedure TNullableString.Uppercase();
begin
  if Self.NotNull then
    Self.Value:= SysUtils.Uppercase(Self.Value);
end;

procedure TNullableString.ChangeToBlankIfNull;
begin
  if Self.IsNull then
    Self.Value:= '';
end;

function TNullableString.ValueIsEqual(const aValue: String): boolean;
begin
  Result := Self.AsString = aValue;
end;

function TNullableString.ValueIsEqual(const aValue: TNullableString): boolean;
begin
  if Self.IsNull then
    Result := aValue.IsNull
  else
    Result := aValue.NotNull and ValueIsEqual(aValue.Value);
end;

function TNullableString.ValueIsEqualCaseInsensitive(const aValue: String): boolean;
begin
  Result := (CompareText(Self.AsString, aValue) = 0);
end;

class function TNullableString.StringToVariant(const aValue: String; const aAllowBlankValues : boolean): Variant;
begin
  if (not aAllowBlankValues) and (aValue = '') then
    Result:= Null
  else
    Result:= aValue;
end;

class function TNullableString.VariantToString(const aValue: Variant): String;
begin
  if VarIsNull(aValue) then
    Result:= ''
  else
    Result:= aValue;
end;

function TNullableString.AsString: String;
begin
  Result := TNullableString.VariantToString(Self.AsVariant);
end;

function TNullableString.AsUppercaseString: String;
begin
  Result := SysUtils.Uppercase(Self.AsString);
end;


{ TNullableDouble }

function TNullableDouble.GetValue: Double;
begin
  Result := FValue;
end;

procedure TNullableDouble.SetDisplayFormat(AValue: String);
begin
  if FDisplayFormat=AValue then Exit;
  FDisplayFormat:=AValue;
end;

procedure TNullableDouble.SetFractionalPartDigits(AValue: byte);
begin
  if FFractionalPartDigits=AValue then Exit;
  FFractionalPartDigits:=AValue;
  if Self.NotNull then
    Self.SetValue(FValue);
end;

procedure TNullableDouble.SetValue(AValue: Double);
begin
  InternalSetValue(AValue);
  SetTagChanged(false);
end;

procedure TNullableDouble.InternalSetValue(AValue: Double);
begin
  if FFractionalPartDigits > 0 then
    FValue:= RoundToExt(AValue, FRoundingMethod, FFractionalPartDigits)
  else
    FValue:= RoundDoubleToStandardPrecision(aValue);
  InternalSetIsNull(false);
end;

constructor TNullableDouble.Create();
begin
  inherited Create;
  FDisplayFormat := '';
  FFractionalPartDigits:= 0;
  FRoundingMethod:= rmHalfRoundAwayFromZero;
end;

constructor TNullableDouble.Create(aValue: Double);
begin
  Self.Create;
  SetValue(aValue);
end;


procedure TNullableDouble.Assign(const aSource: TNullableDouble);
begin
  Self.FDisplayFormat:= aSource.DisplayFormat;
  Self.FFractionalPartDigits:= aSource.FractionalPartDigits;
  if not aSource.IsNull then
    Self.SetValue(aSource.Value)
  else
    Self.IsNull := true;
  SetTagChanged(false);
end;

procedure TNullableDouble.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.SetValue(aSourceField.AsFloat);
  SetTagChanged(false);
end;

procedure TNullableDouble.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
  SetTagChanged(false);
end;

procedure TNullableDouble.Assign(const aValue: String);
begin
  Self.Assign(TNullableDouble.StringToVariant(aValue));
end;

function TNullableDouble.AsVariant: Variant;
begin
  if Self.GetIsNull then
    Result := Null
  else
    Result := FValue;
end;

procedure TNullableDouble.Add(const aValue: double);
begin
  if Self.IsNull then
    Self.SetValue(aValue)
  else
    Self.SetValue(aValue + Self.Value);
end;

procedure TNullableDouble.Add(const aValue: TNullableDouble);
begin
  if aValue.NotNull then
    Self.Add(aValue.Value);
end;

procedure TNullableDouble.Subtract(const aValue: double);
begin
  if Self.IsNull then
    Self.SetValue(-1 * aValue)
  else
    Self.SetValue(Self.Value - aValue);
end;

procedure TNullableDouble.Subtract(const aValue: TNullableDouble);
begin
  if aValue.NotNull then
    Self.Subtract(aValue.Value);
end;

class function TNullableDouble.StringToVariant(const aValue: String): Variant;
var
  tmp : String;
  tmpValue : Double;
begin
  tmp := Trim(aValue);
  if (tmp = '') then
    Result := Null
  else
    if mMathUtility.TryToConvertToDouble(tmp, tmpValue) then
      Result := tmpValue
    else
      Result := Null;
end;

class function TNullableDouble.VariantToString(const aValue: Variant; const aDisplayFormat: string): String;
begin
  if VarIsNull(aValue) then
    Result := ''
  else
  begin
    if aDisplayFormat <> '' then
      Result := FormatFloat(aDisplayFormat, aValue)
    else
      Result := FloatToStr(aValue);
  end;
end;

function TNullableDouble.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpDouble : double;
  SavedIsUnassigned : boolean;
begin
  SavedIsUnassigned:= IsUnassigned;
  if VarIsNull(aValue) then
  begin
    Result := Self.NotNull;
    Self.IsNull:= true;
  end
  else
  begin
    tmpDouble := aValue;
    if Self.IsNull then
      Result := true
    else
      Result := not mFloatsManagement.DoublesAreEqual(tmpDouble, Self.Value);
    if Result then
      Self.InternalSetValue(tmpDouble);
  end;
  SetTagChanged(SavedIsUnassigned or Result or GetTagChanged);
end;

function TNullableDouble.AsString: String;
var
  str : string;
  i : integer;
begin
  if (FDisplayFormat = '') and (FFractionalPartDigits > 0) then
  begin
    str := '0.';
    for i := 0 to FFractionalPartDigits -1 do
      str := str + '#';
    Result := TNullableDouble.VariantToString(Self.AsVariant, str);
  end
  else
    Result := TNullableDouble.VariantToString(Self.AsVariant, FDisplayFormat);
end;

function TNullableDouble.AsFloat: Double;
begin
  if Self.IsNull then
    Result := 0
  else
    Result := Self.Value;
end;

function TNullableDouble.AsFormattedString(const aFormat: String): String;
begin
  if Self.IsNull then
    Result := ''
  else
    Result := FormatFloat(aFormat, Self.Value);
end;

function TNullableDouble.AsJson(const aFieldName: String; const aSkipIfNull: boolean; const aMaxLength: integer = 0): String;
begin
  if Self.IsNull and aSkipIfNull then
    Result := ''
  else
    Result := '"' + aFieldName + '":"' + StringReplace(Self.AsString, ',', '.', [rfReplaceAll]) + '"';
end;


{ TNullableAnsiString }
(*
function TNullableAnsiString.GetValue: AnsiString;
begin
  if (FIsNull) then
     Result := null
  else
    Result := FValue;
end;

procedure TNullableAnsiString.SetValue(AValue: AnsiString);
begin
  FValue:= aValue;
  FIsNull := false;
end;

constructor TNullableAnsiString.Create;
begin
  inherited Create;
end;

constructor TNullableAnsiString.Create(aValue: AnsiString);
begin
  inherited Create;
  FValue := aValue;
end;

procedure TNullableAnsiString.Assign(aSource: TNullableAnsiString);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

procedure TNullableAnsiString.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= aSourceField.AsString;
end;

procedure TNullableAnsiString.Assign(const aValue: String; const aBlankStringMeansNull: boolean);
begin
  if aBlankStringMeansNull and (aValue = '') then
    Self.IsNull:= true
  else
    Self.Value:= Utf8ToAnsi(aValue);
end;

function TNullableAnsiString.AsVariant: Variant;
begin
  if Self.GetIsNull then
    Result := Null
  else
    Result := FValue;
end;
*)
{ TNullableDateTime }

function TNullableDateTime.GetValue: TDateTime;
begin
  Result := FValue;
end;

procedure TNullableDateTime.SetValue(AValue: TDateTime);
begin
  InternalSetValue(aValue);
  SetTagChanged(false);
end;

procedure TNullableDateTime.InternalSetValue(AValue: TDateTime);
begin
  FValue:= aValue;
  InternalSetIsNull(false);
end;

constructor TNullableDateTime.Create();
begin
  inherited Create;
end;

constructor TNullableDateTime.Create(aValue: TDateTime);
begin
  inherited Create;
  SetValue(aValue);
end;

procedure TNullableDateTime.Assign(const aSource: TNullableDateTime);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
  SetTagChanged(false);
end;

procedure TNullableDateTime.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    if aSourceField.DataType in [ftDateTime, ftTime, ftTimeStamp, ftDate] then
      Self.Value:= aSourceField.AsDateTime
    else
      Self.Assign(aSourceField.AsString);
  SetTagChanged(false);
end;

procedure TNullableDateTime.Assign(const aValue: Variant);
begin
  Self.CheckIfDifferentAndAssign(aValue);
  SetTagChanged(false);
end;

procedure TNullableDateTime.Assign(const aValue: String);
begin
  Self.Assign(TNullableDateTime.StringToVariant(aValue));
end;

procedure TNullableDateTime.Assign(const aSource: TNullableDateTimeRecord);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
  SetTagChanged(false);
end;

function TNullableDateTime.AsVariant: Variant;
begin
  if Self.GetIsNull then
    Result := Null
  else
    Result := FValue;
end;

function TNullableDateTime.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpDate : TDateTime;
  SavedIsUnassigned : boolean;
begin
  SavedIsUnassigned:= IsUnassigned;
  if VarIsNull(aValue) then
  begin
    Result := Self.NotNull;
    Self.IsNull:= true;
  end
  else
  begin
    tmpDate := aValue;
    if Self.IsNull then
      Result := true
    else
      Result := not DoublesAreEqual(Self.Value, tmpDate);
    if Result then
      Self.InternalSetValue(tmpDate);
  end;
  SetTagChanged(SavedIsUnassigned or Result or GetTagChanged);
end;

function TNullableDateTime.YearOf: variant;
begin
  if Self.NotNull then
    Result := dateutils.YearOf(Self.Value)
  else
    Result := null;
end;

function TNullableDateTime.ValueIsEqual(const aValue: TDateTime): boolean;
begin
  Result := DoublesAreEqual(Self.Value, aValue);
end;

function TNullableDateTime.ValueIsEqual(const aValue: TNullableDateTime): boolean;
begin
  Result := (aValue.AsString = Self.AsString);
end;

class function TNullableDateTime.StringToVariant(const aValue: String): Variant;
var
  tmpDate : TDateTime;
  newValue : String;
begin
  newValue := Trim(aValue);
  if newValue = '' then
    Result := Null
  else
  begin
    if TryToUnderstandDateTimeString(newValue, tmpDate) then
      Result := tmpDate
    else
    begin
      if TryToUnderstandDateString(newValue, tmpDate) then
        Result := tmpDate
      else
        raise Exception.Create('This string cannot be converted to date: ' + aValue);
    end;
  end;
end;

class function TNullableDateTime.VariantToString(const aValue: Variant; const aShowTime: boolean): String;
begin
  if VarIsNull(aValue) then
    Result := ''
  else
  begin
    if (aShowTime) then
    begin
      Result := DateTimeToStr(aValue, true);
    end
    else
      Result := DateToStr(aValue);
  end;
end;

function TNullableDateTime.AsString(const aShowTime: boolean): String;
begin
  Result := TNullableDateTime.VariantToString(Self.AsVariant, aShowTime);
end;

function TNullableDateTime.AsString: String;
begin
  Result := AsString(true);
end;

function TNullableDateTime.AsStringForFilename(const aShowTime, aUseSeparators: boolean): String;
begin
  Result := '';
  if Self.NotNull then
  begin
    if aShowTime then
    begin
      if aUseSeparators then
        Result := FormatDateTime('YYYY-MM-DD HH_NN_SS', Self.Value)
      else
        Result := FormatDateTime('YYYYMMDD HHNNSS', Self.Value);
    end
    else
    begin
      if aUseSeparators  then
        Result := FormatDateTime('YYYY-MM-DD', Self.Value)
      else
        Result := FormatDateTime('YYYYMMDD', Self.Value);
    end;
  end;
end;

function TNullableDateTime.AsJson(const aFieldName: String; const aSkipIfNull: boolean; const aMaxLength: integer = 0): String;
begin
  if Self.IsNull and aSkipIfNull then
    Result := ''
  else
    Result := '"' + aFieldName + '":"' + ISODateTimeToStr(Self.Value) + '"';
end;

{ TAbstractNullable }

function TAbstractNullable.GetIsUnassigned: Boolean;
begin
  Result := FIsUnassigned;
end;

procedure TAbstractNullable.SetIsUnassigned(AValue: Boolean);
begin
  FIsUnassigned:= aValue;
  SetTagChanged(false);
end;

function TAbstractNullable.GetNotNull: Boolean;
begin
  Result := not GetIsNull;
end;

function TAbstractNullable.GetTagChanged: Boolean;
begin
  Result := FTagChanged;
end;

procedure TAbstractNullable.SetTagChanged(aValue: Boolean);
begin
  FTagChanged:= aValue;
end;

function TAbstractNullable.GetIsNull: Boolean;
begin
  Result := FIsNull;
end;

procedure TAbstractNullable.InternalSetIsNull(aValue: Boolean);
begin
  FIsNull := aValue;
  FIsUnassigned:= false;
end;

procedure TAbstractNullable.SetIsNull(aValue: Boolean);
begin
  InternalSetIsNull(aValue);
  SetTagChanged(false);
end;

constructor TAbstractNullable.Create();
begin
  FIsNull:= true;
  FIsUnassigned:= false;
  SetTagChanged(false);
end;

function TAbstractNullable.AsJson(const aFieldName: String; const aSkipIfNull: boolean; const aMaxLength: integer = 0): String;
var
  tmp : String;
begin
  if Self.IsNull and aSkipIfNull then
    Result := ''
  else
  begin
    tmp := Self.AsString;
    if aMaxLength > 0 then
      tmp := Copy(tmp, 1, aMaxLength);
    Result := '"' + aFieldName + '":"' + tmp + '"';
  end;
end;

end.
