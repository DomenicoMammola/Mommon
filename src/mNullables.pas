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
  db, Graphics, variants,
  mUtility, mMathUtility, mFloatsManagement;

type

  { TAbstractNullable }

  TAbstractNullable = class abstract
  protected
    FIsNull : Boolean;
    function GetNotNull : Boolean;
  public
    constructor Create(); virtual;
    function AsVariant: Variant; virtual; abstract;
    procedure Assign(aSourceField : TField); overload; virtual; abstract;
    procedure Assign (const aValue : String); overload; virtual; abstract;
    procedure Assign(const aValue: variant); overload; virtual; abstract;

    property IsNull : Boolean read FIsNull write FIsNull;
    property NotNull : Boolean read GetNotNull;
  end;

  { TNullableString }

  TNullableString = class(TAbstractNullable)
  private
    FValue : String;
    function GetValue: String;
    procedure SetValue(AValue: String);
  public
    constructor Create(); override; overload;
    constructor Create(aValue: String); overload;

    procedure Assign(aSource : TNullableString); overload;
    procedure Assign(aSourceField : TField); override; overload;
    procedure Assign(const aValue : String); override; overload;
    procedure Assign(const aValue : Variant); override; overload;
    procedure Assign(const  aValue : String; const aAllowBlankValue : boolean); overload;
    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean;
    function AsVariant: Variant; override;
    procedure Trim();
    procedure ChangeToBlankIfNull;

    class function StringToVariant(const aValue : String) : Variant;

    function AsString : String;

    property Value : String read GetValue write SetValue;
  end;

  { TNullableUnicodeString }
{
  TNullableUnicodeString = class (TAbstractNullable)
    private
      FValue : UnicodeString;
      function GetValue: UnicodeString;
      procedure SetValue(AValue: UnicodeString);
    public
      constructor Create(); override; overload;
      constructor Create(aValue: UnicodeString); overload;

      procedure Assign(aSource : TNullableUnicodeString); overload;
      procedure Assign(aSourceField : TField); override; overload;
      procedure Assign (const aValue : String; const aBlankStringMeansNull : boolean); override; overload;
      function AsVariant: Variant; override;

      property Value : UnicodeString read GetValue write SetValue;
  end;}

  { TNullableAnsiString }

{  TNullableAnsiString = class (TAbstractNullable)
    private
      FValue : AnsiString;
      function GetValue: AnsiString;
      procedure SetValue(AValue: AnsiString);
    public
      constructor Create(); override; overload;
      constructor Create(aValue: AnsiString); overload;

      procedure Assign(aSource : TNullableAnsiString); overload;
      procedure Assign(aSourceField : TField); override; overload;
      procedure Assign (const aValue : String; const aBlankStringMeansNull : boolean); override; overload;
      function AsVariant: Variant; override;

      property Value : AnsiString read GetValue write SetValue;
  end;
 }

  { TNullableDateTime }

  TNullableDateTime = class(TAbstractNullable)
    private
      FValue : TDateTime;
      function GetValue : TDateTime;
      procedure SetValue (AValue : TDateTime);
    public
      constructor Create(); override; overload;
      constructor Create(aValue: TDateTime); overload;

      procedure Assign(aSource : TNullableDateTime); overload;
      procedure Assign(aSourceField : TField); override; overload;
      procedure Assign (const aValue: Variant); override; overload;
      procedure Assign (const aValue: String); override; overload;
      function AsVariant: Variant; override;
      function CheckIfDifferentAndAssign(const aValue: Variant): boolean;

      class function StringToVariant(const aValue : String) : Variant;

      function AsString (aShowTime : boolean) : String;

      property Value : TDateTime read GetValue write SetValue;
  end;

  { TNullableDouble }

  TNullableDouble = class (TAbstractNullable)
  private
    FValue : Double;
    function GetValue : Double;
    procedure SetValue (AValue : Double);
  public
    constructor Create(); override; overload;
    constructor Create(aValue: Double); overload;

    procedure Assign(aSource : TNullableDouble); overload;
    procedure Assign(aSourceField : TField); override; overload;
    procedure Assign(const aValue : Variant); override; overload;
    procedure Assign(const aValue: String); override; overload;
    function AsVariant: Variant; override;

    class function StringToVariant(const aValue : String) : Variant;

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean;

    function AsString : String;
    function AsFloat : Double;

    property Value : Double read GetValue write SetValue;
  end;

  { TNullableBoolean }

  TNullableBoolean = class (TAbstractNullable)
  private
    FValue : Boolean;
    function GetValue : Boolean;
    procedure SetValue (AValue : Boolean);
  public
    constructor Create(); override; overload;
    constructor Create(aValue: Boolean); overload;

    procedure Assign(aSource : TNullableBoolean); overload;
    procedure Assign(aSourceField : TField); override; overload;
    procedure Assign (const aValue : Variant); override; overload;
    procedure Assign (const aValue : string); override; overload;
    function AsVariant: Variant; override;

    class function StringToVariant(const aValue : String) : Variant;

    property Value : Boolean read GetValue write SetValue;
  end;

  { TNullableInteger }

  TNullableInteger = class(TAbstractNullable)
  private
    FValue : Integer;
    function GetValue : Integer;
    procedure SetValue(AValue: Integer);
  public
    constructor Create(); override; overload;
    constructor Create(aValue: Integer); overload;

    procedure Assign(aSource : TNullableInteger); overload;
    procedure Assign(aSourceField : TField); override; overload;
    procedure Assign(const aValue : String); override; overload;
    procedure Assign(const aValue : Variant); override; overload;

    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean;
    function AsVariant : Variant; override;
    function AsString : String;

    class function StringToVariant(const aValue : String) : Variant;

    property Value : Integer read GetValue write SetValue;
  end;

  { TNullableColor }

  TNullableColor = class(TAbstractNullable)
  private
    FValue : TColor;
    function GetValue: TColor;
    procedure SetValue(AValue: TColor);
  public
    constructor Create(); override; overload;
    constructor Create(aValue : TColor); overload;

    procedure Assign(aSource : TNullableColor); overload;
    procedure Assign(aSourceField: TField); override; overload;
    procedure Assign(const aValue : Variant); override; overload;
    procedure Assign(const aValue : String); override; overload;
    function AsVariant : Variant; override;
    function AsString : String;

    property Value : TColor read GetValue write SetValue;
  end;

  { TNullableStringRecord }

  TNullableStringRecord = record
  public
    Value : string;
    IsNull : boolean;

    procedure CreateAsNull;
    procedure CreateWithValue (const aValue : String);

    procedure Assign(aSource : TNullableStringRecord); overload;
    procedure Assign(aSourceField : TField); overload;
    procedure Assign(const aValue : String); overload;
    procedure Assign(const aValue : Variant);overload;
    procedure Assign(const  aValue : String; const aAllowBlankValue : boolean); overload;
    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean;
    function AsVariant: Variant;
    procedure Trim();
    procedure ChangeToBlankIfNull;
    function AsString : String;
  end;

  { TNullableDoubleRecord }

  TNullableDoubleRecord = record
  public
    Value : double;
    IsNull : boolean;

    procedure CreateAsNull;
    procedure CreateWithValue (const aValue: Double);

    procedure Assign(aSource : TNullableDoubleRecord); overload;
    procedure Assign(aSourceField : TField); overload;
    procedure Assign(const aValue : String); overload;
    procedure Assign(const aValue : Variant);overload;
    function CheckIfDifferentAndAssign(const aValue : Variant) : boolean;
    function AsVariant: Variant;
    function AsString: String;
    function AsFloat: double;
  end;



implementation

uses
  SysUtils {$IFDEF FPC}, LazUtf8{$ENDIF};

{ TNullableDoubleRecord }

procedure TNullableDoubleRecord.CreateAsNull;
begin
  IsNull:= true;
  Value:= 0;
end;

procedure TNullableDoubleRecord.CreateWithValue(const aValue: Double);
begin
  IsNull:= false;
  Value:= aValue;
end;

procedure TNullableDoubleRecord.Assign(aSource: TNullableDoubleRecord);
begin
  IsNull:= aSource.IsNull;
  Value:= aSource.Value;
end;

procedure TNullableDoubleRecord.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    CreateAsNull
  else
    CreateWithValue(aSourceField.AsFloat);
end;

procedure TNullableDoubleRecord.Assign(const aValue: String);
begin
  Assign(TNullableDouble.StringToVariant(aValue));
end;

procedure TNullableDoubleRecord.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
end;

function TNullableDoubleRecord.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
var
  tmpDouble : double;
begin
  if VarIsNull(aValue) then
  begin
    Result := not IsNull;
    CreateAsNull;
  end
  else
  begin
    tmpDouble := aValue;
    if IsNull then
      Result := true
    else
      Result := not mFloatsManagement.DoublesAreEqual(tmpDouble, Value);
    if Result then
      CreateWithValue(tmpDouble);
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

procedure TNullableStringRecord.CreateAsNull;
begin
  IsNull:= true;
end;

procedure TNullableStringRecord.CreateWithValue(const aValue: String);
begin
  IsNull := false;
  Value:= aValue;
end;

procedure TNullableStringRecord.Assign(aSource: TNullableStringRecord);
begin
  IsNull := aSource.IsNull;
  Value := aSource.Value;
end;

procedure TNullableStringRecord.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    CreateAsNull
  else
    CreateWithValue(aSourceField.AsString);
end;

procedure TNullableStringRecord.Assign(const aValue: String);
begin
  Self.Assign(TNullableString.StringToVariant(aValue));
end;

procedure TNullableStringRecord.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
end;

procedure TNullableStringRecord.Assign(const aValue: String; const aAllowBlankValue: boolean);
begin
  Self.Assign(aValue);
  if aAllowBlankValue then
    Self.ChangeToBlankIfNull;
end;

function TNullableStringRecord.CheckIfDifferentAndAssign(const aValue: Variant): boolean;
begin
  if VarIsNull(aValue) then
  begin
    Result := not IsNull;
    CreateAsNull;
  end
  else
  begin
    if Self.IsNull then
      Result := true
    else
      Result := aValue <> Value;
    if Result then
      CreateWithValue(aValue);
  end;
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

procedure TNullableStringRecord.ChangeToBlankIfNull;
begin
  if IsNull then
    Value:= '';
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
  FValue:= aValue;
  FIsNull := false;
end;

constructor TNullableColor.Create();
begin
  inherited Create;
end;

constructor TNullableColor.Create(aValue: TColor);
begin
  inherited Create;
  FValue := aValue;
end;

procedure TNullableColor.Assign(aSource: TNullableColor);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

procedure TNullableColor.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= StringToColor(aSourceField.AsString);
end;

procedure TNullableColor.Assign(const aValue: Variant);
begin
  if VarIsNull(aValue) then
    Self.IsNull:= true
  else
    Self.Value:= StringToColor(VarToStr(aValue));
end;

procedure TNullableColor.Assign(const aValue: String);
var
  tmp : Variant;
begin
  tmp := aValue;
  Self.Assign(tmp);
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
  FValue:= aValue;
  FIsNull := false;
end;

constructor TNullableInteger.Create();
begin
  inherited Create;
end;

constructor TNullableInteger.Create(aValue: Integer);
begin
  inherited Create;
  FValue := aValue;
end;

procedure TNullableInteger.Assign(aSource: TNullableInteger);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

procedure TNullableInteger.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= aSourceField.AsInteger;
end;

procedure TNullableInteger.Assign(const aValue: String);
begin
  Self.Assign(TNullableInteger.StringToVariant(aValue));
end;

procedure TNullableInteger.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
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
      Self.Value:= tmpInt;
  end;
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
  if Self.IsNull then
    Result := ''
  else
    Result := IntToStr(Self.Value);
end;

class function TNullableInteger.StringToVariant(const aValue: String): Variant;
begin
  if IsNumeric(aValue, false) then
    Result := StrToInt(aValue)
  else
    Result := Null;
end;

{ TNullableBoolean }

function TNullableBoolean.GetValue: Boolean;
begin
  Result := FValue;
end;

procedure TNullableBoolean.SetValue(AValue: Boolean);
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
  FValue := aValue;
end;

procedure TNullableBoolean.Assign(aSource: TNullableBoolean);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

procedure TNullableBoolean.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= aSourceField.AsBoolean;
end;

procedure TNullableBoolean.Assign(const aValue: Variant);
begin
  if VarIsNull(aValue) then
    Self.IsNull:= true
  else
    Self.Value:= aValue;
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

class function TNullableBoolean.StringToVariant(const aValue: String): Variant;
begin
  if (aValue = '') then
    Result := Null
  else
    Result:= StrToBool(aValue);
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
  Self.Value:= aValue;
end;

procedure TNullableString.Assign(aSource: TNullableString);
begin
  if not aSource.IsNull then
    Self.Value:= aSource.Value
  else
    Self.IsNull:= true;
end;

procedure TNullableString.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= aSourceField.AsString;
end;

procedure TNullableString.Assign(const aValue: String);
begin
  Self.Assign(TNullableString.StringToVariant(aValue));
end;

procedure TNullableString.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
end;

procedure TNullableString.Assign(const aValue: String; const aAllowBlankValue: boolean);
begin
  Self.Assign(aValue);
  if aAllowBlankValue then
    Self.ChangeToBlankIfNull;
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
      Self.Value:= aValue;
  end;
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

class function TNullableString.StringToVariant(const aValue: String): Variant;
begin
  if aValue = '' then
    Result := Null
  else
    Result := aValue;
end;

function TNullableString.AsString: String;
begin
  if Self.IsNull then
    Result := ''
  else
    Result := Self.Value;
end;

{ TNullableDouble }

function TNullableDouble.GetValue: Double;
begin
  Result := FValue;
end;

procedure TNullableDouble.SetValue(AValue: Double);
begin
  FValue:= aValue;
  FIsNull := false;
end;

constructor TNullableDouble.Create();
begin
  inherited Create;
end;

constructor TNullableDouble.Create(aValue: Double);
begin
  inherited Create;
  FValue := aValue;
end;


procedure TNullableDouble.Assign(aSource: TNullableDouble);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

procedure TNullableDouble.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= aSourceField.AsFloat;
end;

procedure TNullableDouble.Assign(const aValue: Variant);
begin
  CheckIfDifferentAndAssign(aValue);
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

class function TNullableDouble.StringToVariant(const aValue: String): Variant;
begin
  if (aValue = '') then
    Result := Null
  else
    Result:= StrToFloat(aValue);
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
      Self.Value:= tmpDouble;
  end;
end;

function TNullableDouble.AsString: String;
begin
  if Self.IsNull then
    Result := ''
  else
    Result := FloatToStr(Self.Value);
end;

function TNullableDouble.AsFloat: Double;
begin
  if Self.IsNull then
    Result := 0
  else
    Result := Self.Value;
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
  FValue := aValue;
end;

procedure TNullableDateTime.Assign(aSource: TNullableDateTime);
begin
  if not aSource.IsNull then
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

procedure TNullableDateTime.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= aSourceField.AsDateTime;
end;

procedure TNullableDateTime.Assign(const aValue: Variant);
begin
  Self.CheckIfDifferentAndAssign(aValue);
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
      Self.Value:= tmpDate;
  end;
end;

class function TNullableDateTime.StringToVariant(const aValue: String): Variant;
var
  tmpDate : TDateTime;
begin
  if aValue = '' then
    Result := Null
  else
    if TryToUnderstandDateString(aValue, tmpDate) then
      Result := tmpDate
    else
      raise Exception.Create('This string cannot be converted to date: ' + aValue);
end;

function TNullableDateTime.AsString(aShowTime: boolean): String;
begin
  if Self.IsNull then
    Result := ''
  else
  begin
    if (aShowTime) then
    begin
      Result := DateTimeToStr(Value, true);
    end
    else
      Result := DateToStr(Value);
  end;
end;

{ TAbstractNullable }

function TAbstractNullable.GetNotNull: Boolean;
begin
  Result := not FIsNull;
end;

constructor TAbstractNullable.Create;
begin
  FIsNull := true;
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
