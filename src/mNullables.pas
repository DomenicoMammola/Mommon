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
{$ENDIF}

interface

uses
  db, Graphics,
  mUtility;

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
    procedure Assign (const aValue : String; const aBlankStringMeansNull : boolean); overload; virtual; abstract;

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
    procedure Assign (const aValue : String; const aBlankStringMeansNull : boolean); override; overload;
    function AsVariant: Variant; override;
    procedure Trim();
    function AsString : String;

    property Value : String read GetValue write SetValue;
  end;

  { TNullableUnicodeString }

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
  end;

  { TNullableAnsiString }

  TNullableAnsiString = class (TAbstractNullable)
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
      procedure Assign (const aValue : String; const aBlankStringMeansNull : boolean); override; overload;
      function AsVariant: Variant; override;

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
    procedure Assign (const aValue : String; const aBlankStringMeansNull : boolean); override; overload;
    function AsVariant: Variant; override;

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
    procedure Assign (const aValue : String; const aBlankStringMeansNull : boolean); override; overload;
    function AsVariant: Variant; override;

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
    procedure Assign (const aValue : String; const aBlankStringMeansNull : boolean); override; overload;
    function AsVariant : Variant; override;
    function AsString : String;

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
    procedure Assign (const aValue : String; const aBlankStringMeansNull : boolean); override; overload;
    function AsVariant : Variant; override;
    function AsString : String;

    property Value : TColor read GetValue write SetValue;
  end;


implementation

uses
  SysUtils {$IFDEF FPC}, LazUtf8{$ENDIF};

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

constructor TNullableColor.Create;
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

procedure TNullableColor.Assign(const aValue: String; const aBlankStringMeansNull: boolean);
begin
  if aBlankStringMeansNull and (aValue = '') then
    Self.IsNull:= true
  else
    Self.Value:= StringToColor(aValue);
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

constructor TNullableInteger.Create;
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

procedure TNullableInteger.Assign(const aValue: String; const aBlankStringMeansNull: boolean);
begin
  if aBlankStringMeansNull and (aValue = '') then
    Self.IsNull:= true
  else
    Self.Value:= StrToInt(aValue);
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

constructor TNullableBoolean.Create;
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

procedure TNullableBoolean.Assign(const aValue: String; const aBlankStringMeansNull: boolean);
begin
  if aBlankStringMeansNull and (aValue = '') then
    Self.IsNull:= true
  else
    Self.Value:= StrToBool(aValue);
end;

function TNullableBoolean.AsVariant: Variant;
begin
  if Self.FIsNull then
    Result := Null
  else
    Result := FValue;
end;

{ TNullableString }

function TNullableString.GetValue: String;
begin
  if (FIsNull) then
       Result := null
    else
      Result := FValue;
end;

procedure TNullableString.SetValue(AValue: String);
begin
  FValue:= aValue;
  FIsNull := False;
end;

constructor TNullableString.Create;
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
    Self.Value := aSource.Value
  else
    Self.IsNull := true;
end;

procedure TNullableString.Assign(aSourceField: TField);
begin
  if aSourceField.IsNull then
    Self.IsNull:= true
  else
    Self.Value:= aSourceField.AsString;
end;

procedure TNullableString.Assign(const aValue: String; const aBlankStringMeansNull: boolean);
begin
  if aBlankStringMeansNull and (aValue = '') then
    Self.IsNull:= true
  else
    Self.Value:= aValue;
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

constructor TNullableDouble.Create;
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

procedure TNullableDouble.Assign(const aValue: String; const aBlankStringMeansNull: boolean);
begin
  if aBlankStringMeansNull and (aValue = '') then
    Self.IsNull:= true
  else
    Self.Value:= StrToFloat(aValue);
end;

function TNullableDouble.AsVariant: Variant;
begin
  if Self.FIsNull then
    Result := Null
  else
    Result := FValue;
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

constructor TNullableDateTime.Create;
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

procedure TNullableDateTime.Assign(const aValue: String; const aBlankStringMeansNull: boolean);
var
  tmpDate : TDateTime;
begin
  if aBlankStringMeansNull and (aValue = '') then
    Self.IsNull:= true
  else
  begin
    if TryToUnderstandDateString(aValue, tmpDate) then
      Self.Value:= tmpDate
    else
      raise Exception.Create('This string cannot be converted to date: ' + aValue);
  end;
end;

function TNullableDateTime.AsVariant: Variant;
begin
  if Self.FIsNull then
    Result := Null
  else
    Result := FValue;
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

end.
