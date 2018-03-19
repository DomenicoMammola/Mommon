// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mBaseClassesAsObjects;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}


interface

type

  { TIntegerObject }

  TIntegerObject = class
  strict private
    FValue : Integer;
  public
    constructor Create(); overload;
    constructor Create(const aValue : integer); overload;

    property Value : Integer read FValue write FValue;
  end;

  { TAnsiStringObject }

  TAnsiStringObject = class
  strict private
    FValue : AnsiString;
  public
    constructor Create(); overload;
    constructor Create(const aValue : AnsiString); overload;

    property Value : AnsiString read FValue write FValue;
  end;

  { TUnicodeStringObject }

  TUnicodeStringObject = class
  strict private
    FValue : UnicodeString;
  public
    constructor Create(); overload;
    constructor Create(const aValue : UnicodeString); overload;

    property Value : UnicodeString read FValue write FValue;
  end;

  { TStringObject }

  TStringObject = class
  strict private
    FValue : String;
  public
    constructor Create(); overload;
    constructor Create(const aValue : String); overload;

    property Value : String read FValue write FValue;
  end;

  { TVariantObject }

  TVariantObject = class
  strict private
    FValue : Variant;
  public
    constructor Create(); overload;
    constructor Create(const aValue : Variant); overload;

    property Value : Variant read FValue write FValue;
  end;


  { TDoubleObject }

  TDoubleObject = class
  strict private
    FValue : Double;
  public
    constructor Create(); overload;
    constructor Create(const aValue : Double); overload;

    property Value : Double read FValue write FValue;
  end;

  { TBooleanObject }

  TBooleanObject = class
  strict private
    FValue: Boolean;
  public
    constructor Create(); overload;
    constructor Create(const aValue: Boolean); overload;

    property Value: Boolean read FValue write FValue;
  end;


implementation

{ TBooleanObject }

constructor TBooleanObject.Create();
begin
  FValue:= false;
end;

constructor TBooleanObject.Create(const aValue: boolean);
begin
  FValue:= aValue;
end;

{ TDoubleObject }

constructor TDoubleObject.Create;
begin
  FValue:= 0;
end;

constructor TDoubleObject.Create(const aValue: Double);
begin
  FValue := aValue;
end;

{ TVariantObject }

constructor TVariantObject.Create;
begin
  FValue := Null;
end;

constructor TVariantObject.Create(const aValue: Variant);
begin
  FValue := aValue;
end;

{ TStringObject }

constructor TStringObject.Create;
begin
  FValue := '';
end;

constructor TStringObject.Create(const aValue: String);
begin
  FValue := aValue;
end;

{ TUnicodeStringObject }

constructor TUnicodeStringObject.Create;
begin
  FValue := '';
end;

constructor TUnicodeStringObject.Create(const aValue: UnicodeString);
begin
  FValue := aValue;
end;

{ TAnsiStringObject }

constructor TAnsiStringObject.Create;
begin
  FValue := '';
end;

constructor TAnsiStringObject.Create(const aValue: AnsiString);
begin
  FValue := aValue;
end;

{ TIntegerObject }

constructor TIntegerObject.Create();
begin
  FValue := 0;
end;

constructor TIntegerObject.Create(const aValue: integer); overload;
begin
  FValue := aValue;
end;

end.
