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

uses
  db, Graphics, variants, Classes,
  mUtility, mMathUtility, mFloatsManagement;

type

  { TAbstractNullable }

  TAbstractNullable = class abstract
  protected
    FTagChanged: Boolean;
    FIsNull: Boolean;
    function GetNotNull: Boolean;
  public
    constructor Create(); virtual;
    function AsVariant: Variant; virtual; abstract;
    procedure Assign(const aValue: variant); overload; virtual; abstract;

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; virtual; abstract;
    function AsString: String; virtual; abstract;

    property IsNull: Boolean read FIsNull write FIsNull;
    property NotNull: Boolean read GetNotNull;
    property TagChanged: Boolean read FTagChanged;
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
    procedure ChangeToBlankIfNull;

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
      function AsVariant: Variant; override;
      function CheckIfDifferentAndAssign(const aValue: Variant): boolean; override;

      class function StringToVariant(const aValue : String): Variant;
      class function VariantToString(const aValue: Variant; const aShowTime: boolean): String;

      function AsString (const aShowTime : boolean) : String; overload;
      function AsString : String; override; overload;
      function AsStringForFilename (const aShowTime, aUseSeparators: boolean): String;

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

    class function StringToVariant(const aValue: String): Variant;
    class function VariantToString(const aValue: Variant; const aDisplayFormat : string): String;

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; override;

    function AsString : String; override;
    function AsFloat : Double;
    function AsFormattedString (const aFormat: String): String;

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
    procedure Assign (const aValue : Variant); override; overload;
    procedure Assign (const aValue : string); overload;

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; override;
    function AsVariant: Variant; override;
    function AsString: String; override;
    function AsBoolean: Boolean;

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

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean; override;
    function AsVariant: Variant; override;
    function AsString: String; override;
    function AsInteger : integer;
    function AsFormattedString (aFormat: String): String;

    procedure Add (const aValue : integer); overload;
    procedure Add (const aValue : TNullableInteger); overload;

    class function StringToVariant(const aValue : String) : Variant;
    class function VariantToString(const aValue : Variant) : String;
  public
    property Value : Integer read GetValue write SetValue;
  end;

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

  { TNullableStringRecord }

  TNullableStringRecord = record
  public
    Value : string;
    IsNull : boolean;

    procedure SetNull;
    procedure SetValue (const aValue : String);

    procedure Assign(const aSource : TNullableStringRecord); overload;
    procedure Assign(const aSourceField : TField; const aAllowBlankValue : boolean); overload;
    procedure Assign(const aValue : Variant);overload;
    function AsVariant: Variant;
    procedure Trim();
    function AsString : String;
  end;

  { TNullableDoubleRecord }

  TNullableDoubleRecord = record
  public
    Value : double;
    IsNull : boolean;

    procedure SetNull;
    procedure SetValue (const aValue: Double);

    procedure Assign(const aSource : TNullableDoubleRecord); overload;
    procedure Assign(const aSourceField : TField); overload;
    procedure Assign(const aValue : String); overload;
    procedure Assign(const aValue : Variant);overload;
    function AsVariant: Variant;
    function AsString: String;
    function AsFloat: double;
  end;

  { TNullableDateTimeRecord }

  TNullableDateTimeRecord = record
  public
    Value : TDateTime;
    IsNull : boolean;
    procedure SetNull;
    procedure SetValue (const aValue : TDateTime);

    procedure Assign(const aSource : TNullableDatetimeRecord); overload;
    procedure Assign(const aSourceField : TField); overload;
    procedure Assign(const aValue : String); overload;
    procedure Assign(const aValue : Variant);overload;
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
    function AsVariant: Variant;
    function AsString: String;
    function AsInteger: integer;
  end;


implementation

uses
  SysUtils {$IFDEF FPC}, LazUtf8{$ENDIF};

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

procedure TNullableDateTimeRecord.SetNull;
begin
  IsNull:= true;
  Value:= 0;
end;

procedure TNullableDateTimeRecord.SetValue(const aValue: TDateTime);
begin
  IsNull:= false;
  Value:= aValue;
end;

procedure TNullableDateTimeRecord.Assign(const aSource: TNullableDatetimeRecord);
begin
  IsNull:= aSource.IsNull;
  Value:= aSource.Value;
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
  begin
    SetNull;
  end
  else
  begin
    tmpDateTime := aValue;
    SetValue(tmpDateTime);
  end;
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
  FTagChanged:= false;
end;

procedure TNullableTime.InternalSetValue(AValue: TDateTime);
begin
  FValue:= aValue;
  FIsNull := false;
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
  FTagChanged:= false;

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
  FTagChanged:= false;
end;

procedure TNullableTime.Assign(const aValue: Variant);
begin
  Self.CheckIfDifferentAndAssign(aValue);
  FTagChanged:= false;
end;

procedure TNullableTime.Assign(const aValue: String);
begin
  Self.Assign(TNullableTime.StringToVariant(aValue));
end;

function TNullableTime.AsVariant: Variant;
begin
  if Self.FIsNull then
    Result := Null
  else
    Result := FValue;
end;

function TNullableTime.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpTime : TDateTime;
begin
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
  Self.FTagChanged:= Result or Self.FTagChanged;
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

procedure TNullableDoubleRecord.SetNull;
begin
  IsNull:= true;
  Value:= 0;
end;

procedure TNullableDoubleRecord.SetValue(const aValue: Double);
begin
  IsNull:= false;
  Value:= aValue;
end;

procedure TNullableDoubleRecord.Assign(const aSource: TNullableDoubleRecord);
begin
  IsNull:= aSource.IsNull;
  Value:= aSource.Value;
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

procedure TNullableStringRecord.SetNull;
begin
  IsNull:= true;
end;

procedure TNullableStringRecord.SetValue(const aValue: String);
begin
  IsNull := false;
  Value:= aValue;
end;

procedure TNullableStringRecord.Assign(const aSource: TNullableStringRecord);
begin
  IsNull := aSource.IsNull;
  Value := aSource.Value;
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
    Value:= SysUtils.Trim(Self.Value);
end;

function TNullableStringRecord.AsString: String;
begin
  if IsNull then
    Result := ''
  else
    Result := Value;
end;

{ TNullableColor }

function TNullableColor.GetValue: TColor;
begin
  Result := FValue;
end;

procedure TNullableColor.SetValue(AValue: TColor);
begin
  InternalSetValue(AValue);
  FTagChanged:= false;
end;

procedure TNullableColor.InternalSetValue(AValue: TColor);
begin
  FValue:= aValue;
  FIsNull:= false;
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
  FTagChanged:= false;
end;

procedure TNullableColor.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= StringToColor(aSourceField.AsString);
  FTagChanged:= false;
end;

procedure TNullableColor.Assign(const aValue: Variant);
begin
  if VarIsNull(aValue) then
    Self.IsNull:= true
  else
    Self.Value:= StringToColor(VarToStr(aValue));
  FTagChanged:= false;
end;

procedure TNullableColor.Assign(const aValue: String);
var
  tmp : Variant;
begin
  tmp := aValue;
  Self.Assign(tmp);
  FTagChanged:= false;
end;

function TNullableColor.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpValue : TColor;
begin
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
  Self.FTagChanged:= Result or FTagChanged;
end;

function TNullableColor.AsVariant: Variant;
begin
  if Self.FIsNull then
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

{ TNullableInteger }

function TNullableInteger.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TNullableInteger.SetValue(AValue: Integer);
begin
  InternalSetValue(AValue);
  FTagChanged:= false;
end;

procedure TNullableInteger.InternalSetValue(AValue: Integer);
begin
  FValue:= aValue;
  FIsNull:= false;
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
  FTagChanged:= false;
end;

procedure TNullableInteger.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= aSourceField.AsInteger;
  FTagChanged:= false;
end;

procedure TNullableInteger.Assign(const aValue: String);
begin
  Self.Assign(TNullableInteger.StringToVariant(aValue));
end;

procedure TNullableInteger.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
  FTagChanged:= false;
end;

function TNullableInteger.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpInt : integer;
begin
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
  Self.FTagChanged:= Result or FTagChanged;
end;

function TNullableInteger.AsVariant: Variant;
begin
  if Self.FIsNull then
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

class function TNullableInteger.StringToVariant(const aValue: String): Variant;
begin
  if IsNumeric(aValue, false) then
    Result := StrToInt(aValue)
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
  FTagChanged:= false;
end;

procedure TNullableBoolean.InternalSetValue(AValue: Boolean);
begin
  FValue:= aValue;
  FIsNull := false;
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
  FTagChanged:= false;
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
    else if (aSourceField.DataType = ftInteger) or (aSourceField.DataType = ftSmallint) then
      Self.Value:= (aSourceField.AsInteger = 1)
    else if aSourceField.DataType = ftFixedChar then
      Self.Value := (aSourceField.AsString = 'T') or (aSourceField.AsString = '1') or (aSourceField.AsString = 'Y') or (aSourceField.AsString = 'S')
    else
      raise Exception.Create('TNullableBoolean: cannot assign value from field of type ' + Fieldtypenames[aSourceField.DataType]);
  end;
  FTagChanged:= false;
end;

procedure TNullableBoolean.Assign(const aValue: Variant);
begin
  if VarIsNull(aValue) then
    Self.IsNull:= true
  else
    Self.Value:= aValue;
  FTagChanged:= false;
end;

procedure TNullableBoolean.Assign(const aValue: string);
begin
  Self.Assign(TNullableBoolean.StringToVariant(aValue));
end;

function TNullableBoolean.AsVariant: Variant;
begin
  if Self.FIsNull then
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

function TNullableBoolean.AsBoolean: Boolean;
begin
  Result := (Self.NotNull) and (Self.Value);
end;

function TNullableBoolean.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpBool : boolean;
begin
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
  Self.FTagChanged:= Result or Self.FTagChanged;
end;

class function TNullableBoolean.StringToVariant(const aValue: String): Variant;
begin
  if (aValue = '') then
    Result := Null
  else
    Result:= StrToBool(aValue);
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
  if (FIsNull) then
    Result:= ''
  else
    Result:= FValue;
end;

procedure TNullableString.SetValue(AValue: String);
begin
  InternalSetValue(AValue);
  FTagChanged:= false;
end;

procedure TNullableString.InternalSetValue(AValue: String);
begin
  FValue:= aValue;
  FIsNull:= False;
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
  FTagChanged:= false;
end;

procedure TNullableString.Assign(const aSourceField: TField; const aAllowBlankValues : boolean);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Assign(TNullableString.StringToVariant(aSourceField.AsString, aAllowBlankValues));
  FTagChanged:= false;
end;

procedure TNullableString.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
  FTagChanged:= false;
end;

procedure TNullableString.Assign(const aValue: String; const aAllowBlankValue: boolean);
begin
  if (not aAllowBlankValue) and (aValue = '') then
    Self.IsNull := true
  else
    Self.InternalSetValue(aValue);
  FTagChanged:= false;
end;

function TNullableString.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
begin
  if VarIsNull(aValue) then
  begin
    Result:= Self.NotNull;
    Self.IsNull:= true
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
  Self.FTagChanged:= Result or Self.FTagChanged;
end;

function TNullableString.AsVariant: Variant;
begin
  if Self.FIsNull then
    Result := Null
  else
    Result := FValue;
end;

procedure TNullableString.Trim();
begin
  if not Self.IsNull then
    Self.Value:= SysUtils.Trim(Self.Value);
end;

procedure TNullableString.ChangeToBlankIfNull;
begin
  if Self.IsNull then
    Self.Value:= '';
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
  Result := Uppercase(Self.AsString);
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
  FTagChanged:= false;
end;

procedure TNullableDouble.InternalSetValue(AValue: Double);
begin
  if FFractionalPartDigits > 0 then
    FValue:= RoundToExt(AValue, FRoundingMethod, FFractionalPartDigits)
  else
    FValue:= RoundDoubleToStandardPrecision(aValue);
  FIsNull := false;
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
  FTagChanged:= false;
end;

procedure TNullableDouble.Assign(const aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.SetValue(aSourceField.AsFloat);
  FTagChanged:= false;
end;

procedure TNullableDouble.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
  FTagChanged:= false;
end;

procedure TNullableDouble.Assign(const aValue: String);
begin
  Self.Assign(TNullableDouble.StringToVariant(aValue));
end;

function TNullableDouble.AsVariant: Variant;
begin
  if Self.FIsNull then
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

class function TNullableDouble.StringToVariant(const aValue: String): Variant;
begin
  if (aValue = '') then
    Result := Null
  else
    Result:= StrToFloat(aValue);
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
begin
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
  Self.FTagChanged:= Result or Self.FTagChanged;
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
  if Self.FIsNull then
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
  FTagChanged:= false;
end;

procedure TNullableDateTime.InternalSetValue(AValue: TDateTime);
begin
  FValue:= aValue;
  FIsNull := false;
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
  FTagChanged:= false;
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
  FTagChanged:= false;
end;

procedure TNullableDateTime.Assign(const aValue: Variant);
begin
  Self.CheckIfDifferentAndAssign(aValue);
  FTagChanged:= false;
end;

procedure TNullableDateTime.Assign(const aValue: String);
begin
  Self.Assign(TNullableDateTime.StringToVariant(aValue));
end;

function TNullableDateTime.AsVariant: Variant;
begin
  if Self.FIsNull then
    Result := Null
  else
    Result := FValue;
end;

function TNullableDateTime.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpDate : TDateTime;
begin
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
  Self.FTagChanged:= Result or Self.FTagChanged;
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
    if TryToUnderstandDateString(newValue, tmpDate) then
      Result := tmpDate
    else
      raise Exception.Create('This string cannot be converted to date: ' + aValue);
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

{ TAbstractNullable }

function TAbstractNullable.GetNotNull: Boolean;
begin
  Result := not FIsNull;
end;

constructor TAbstractNullable.Create;
begin
  FIsNull:= true;
  FTagChanged:= false;
end;

{ TNullableUnicodeString }

(*
function TNullableUnicodeString.GetValue: UnicodeString;
begin
  if (FIsNull) then
     Result := null
  else
    Result := FValue;
end;

procedure TNullableUnicodeString.SetValue(AValue: UnicodeString);
begin
  FValue:= aValue;
  FIsNull := False;
end;

constructor TNullableUnicodeString.Create;
begin
  inherited Create;
end;

constructor TNullableUnicodeString.Create(aValue: UnicodeString);
begin
  inherited Create;
  FValue := aValue;
end;

procedure TNullableUnicodeString.Assign(aSource: TNullableUnicodeString);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

procedure TNullableUnicodeString.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= aSourceField.AsWideString;
end;

procedure TNullableUnicodeString.Assign(const aValue: String; const aBlankStringMeansNull: boolean);
begin
  if aBlankStringMeansNull and (aValue = '') then
    Self.IsNull:= true
  else
    Self.Value:= {$IFDEF FPC}UTF8ToUTF16(aValue){$ELSE}aValue{$ENDIF};
end;

function TNullableUnicodeString.AsVariant: Variant;
begin
  if Self.FIsNull then
    Result := Null
  else
    Result := FValue;
end;
*)
end.
