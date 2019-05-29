// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSpreadsheetUtils;

interface

uses
  fpspreadsheet, fpstypes,
  mNullables;

type

  { TmSpreadsheetHelper }

  TmSpreadsheetHelper = class
  strict private
    FSheet: TsWorksheet;
  public
    constructor Create; overload;
    constructor Create (aSheet : TsWorksheet); overload;

    procedure WriteFloat(const aRow, aCol : integer; const aValue : TNullableDouble; const aFractionalPartDigits : integer); overload;
    procedure WriteFloat(const aRow, aCol : integer; const aValue : double; const aFractionalPartDigits : integer); overload;
    procedure WriteDate(const aRow, aCol : integer; const aValue: TNullableDateTime); overload;
    procedure WriteDate(const aRow, aCol : integer; const aValue : TDateTime); overload;
    procedure WriteDateTime(const aRow, aCol : integer; const aValue: TNullableDateTime); overload;
    procedure WriteDateTime(const aRow, aCol : integer; const aValue : TDateTime); overload;
    procedure WriteInteger(const aRow, aCol : integer; const aValue : TNullableInteger); overload;
    procedure WriteInteger(const aRow, aCol : integer; const aValue : integer); overload;
    procedure WriteInt64(const aRow, aCol : integer; const aValue : Int64); overload;
    procedure WriteText(const aRow, aCol : integer; const aValue : TNullableString); overload;
    procedure WriteText(const aRow, aCol : integer; const aValue : String); overload;

    procedure ReadInteger(const aRow, aCol : integer; aValue : TNullableInteger);
    procedure ReadFloat(const aRow, aCol : integer; aValue : TNullableDouble);
    procedure ReadUppercaseText(const aRow, aCol: integer; aValue : TNullableString; const aDoTrim : boolean = true);
    procedure ReadText(const aRow, aCol : integer; aValue : TNullableString; const aDoTrim : boolean = true);
    procedure ReadDate(const aRow, aCol : integer; aValue : TNullableDateTime);

    property Sheet : TsWorksheet read FSheet write FSheet;
  end;

procedure spWriteFloat(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TNullableDouble; const aFractionalPartDigits : integer); overload;
procedure spWriteFloat(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : double; const aFractionalPartDigits : integer); overload;

procedure spWriteDate(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue: TNullableDateTime); overload;
procedure spWriteDate(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TDateTime); overload;

procedure spWriteDateTime(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue: TNullableDateTime); overload;
procedure spWriteDateTime(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TDateTime); overload;

procedure spWriteInteger(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TNullableInteger); overload;
procedure spWriteInteger(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : integer); overload;
procedure spWriteInt64(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : Int64);

procedure spWriteText(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TNullableString); overload;
procedure spWriteText(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : String); overload;

procedure spReadInteger(aSheet: TsWorksheet; const aRow, aCol : integer; aValue : TNullableInteger);
procedure spReadFloat(aSheet: TsWorksheet; const aRow, aCol : integer; aValue : TNullableDouble);
procedure spReadUppercaseText(aSheet : TsWorksheet; const aRow, aCol : integer; aValue : TNullableString; const aDoTrim : boolean = true);
procedure spReadText(aSheet : TsWorksheet; const aRow, aCol : integer; aValue : TNullableString; const aDoTrim : boolean = true);
procedure spReadDate(aSheet: TsWorksheet;const aRow, aCol : integer; aValue : TNullableDateTime);

implementation

uses
  sysutils,
  mMathUtility;

procedure spWriteFloat(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableDouble; const aFractionalPartDigits: integer);
begin
  if aValue.NotNull then
    spWriteFloat(aSheet, aRow, aCol, aValue.AsFloat, aFractionalPartDigits);
end;

procedure spWriteFloat(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: double; const aFractionalPartDigits: integer);
begin
  aSheet.WriteNumber(aRow, aCol, aValue, nfFixed, aFractionalPartDigits);
end;

procedure spWriteDate(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TDateTime);
begin
  aSheet.WriteDateTime(aRow, aCol, aValue, nfShortDate);
end;

procedure spWriteDate(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableDateTime);
begin
  if aValue.NotNull then
    spWriteDate(aSheet, aRow, aCol, aValue.Value);
end;

procedure spWriteDateTime(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableDateTime);
begin
  if aValue.NotNull then
    spWriteDateTime(aSheet, aRow, aCol, aValue.Value);

end;

procedure spWriteDateTime(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TDateTime);
begin
  aSheet.WriteDateTime(aRow, aCol, aValue, nfShortDateTime);
end;

procedure spWriteInteger(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableInteger);
begin
  if aValue.NotNull then
    spWriteInteger(aSheet, aRow, aCol, aValue.AsInteger);
end;

procedure spWriteInteger(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: integer);
var
  vDouble : Double;
begin
  vDouble := aValue;
  aSheet.WriteNumber(aRow, aCol, vDouble, nfFixed, 0);
end;

procedure spWriteInt64(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: Int64);
var
  vDouble : Double;
begin
  vDouble := aValue;
  aSheet.WriteNumber(aRow, aCol, vDouble, nfFixed, 0);
end;

procedure spWriteText(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableString);
begin
  if aValue.NotNull then
    spWriteText(aSheet, aRow, aCol, aValue.AsString);
end;

procedure spWriteText(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: String);
begin
  aSheet.WriteText(aRow, aCol, aValue);
end;

procedure spReadInteger(aSheet: TsWorksheet; const aRow, aCol: integer; aValue: TNullableInteger);
var
  str : String;
  v : integer;
begin
  aValue.IsNull := true;
  str := Trim(aSheet.ReadAsText(aRow, aCol));
  if str <> '' then
  begin
    if TryToConvertToInteger(str, v) then
      aValue.Value:= v;
  end;
end;

procedure spReadFloat(aSheet: TsWorksheet; const aRow, aCol: integer; aValue: TNullableDouble);
var
  str : String;
  v : double;
  failed : boolean;
begin
  aValue.IsNull := true;
  str := Trim(aSheet.ReadAsText(aRow, aCol));
  if str <> '' then
  begin
    failed := false;
    try
      v := aSheet.ReadAsNumber(aRow, aCol);
    except
      failed := true;
    end;
    if failed then
    begin
      if TryToConvertToDouble(str, v) then
        aValue.Value := v;
    end
    else
      aValue.Value:= v;
  end;
end;

procedure spReadUppercaseText(aSheet: TsWorksheet; const aRow, aCol: integer; aValue: TNullableString; const aDoTrim : boolean = true);
var
  str : String;
begin
  str := Uppercase(aSheet.ReadAsText(aRow, aCol));
  if aDoTrim then
    str := Trim(str);
  aValue.Assign(str, false);
end;

procedure spReadText(aSheet: TsWorksheet; const aRow, aCol: integer; aValue: TNullableString; const aDoTrim : boolean = true);
var
  str : String;
begin
  str := aSheet.ReadAsText(aRow, aCol);
  if aDoTrim then
    str := Trim(str);
  aValue.Assign(str, false);
end;

procedure spReadDate(aSheet: TsWorksheet; const aRow, aCol: integer; aValue: TNullableDateTime);
begin
  aValue.Assign(aSheet.ReadAsText(aRow, aCol));
end;

{ TmSpreadsheetHelper }

constructor TmSpreadsheetHelper.Create;
begin
  Self.Create(nil);
end;

constructor TmSpreadsheetHelper.Create(aSheet: TsWorksheet);
begin
  FSheet := aSheet;
end;

procedure TmSpreadsheetHelper.WriteFloat(const aRow, aCol: integer; const aValue: TNullableDouble; const aFractionalPartDigits: integer);
begin
  spWriteFloat(FSheet, aRow, aCol, aValue, aFractionalPartDigits);
end;

procedure TmSpreadsheetHelper.WriteFloat(const aRow, aCol: integer; const aValue: double; const aFractionalPartDigits: integer);
begin
  spWriteFloat(FSheet, aRow, aCol, aValue, aFractionalPartDigits);
end;

procedure TmSpreadsheetHelper.WriteDate(const aRow, aCol: integer; const aValue: TNullableDateTime);
begin
  spWriteDate(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.WriteDate(const aRow, aCol: integer; const aValue: TDateTime);
begin
  spWriteDate(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.WriteDateTime(const aRow, aCol: integer; const aValue: TNullableDateTime);
begin
  spWriteDateTime(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.WriteDateTime(const aRow, aCol: integer; const aValue: TDateTime);
begin
  spWriteDateTime(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.WriteInteger(const aRow, aCol: integer; const aValue: TNullableInteger);
begin
  spWriteInteger(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.WriteInteger(const aRow, aCol: integer; const aValue: integer);
begin
  spWriteInteger(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.WriteInt64(const aRow, aCol: integer; const aValue: Int64);
begin
  spWriteInt64(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.WriteText(const aRow, aCol: integer; const aValue: TNullableString);
begin
  spWriteText(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.WriteText(const aRow, aCol: integer; const aValue: String);
begin
  spWriteText(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.ReadInteger(const aRow, aCol: integer; aValue: TNullableInteger);
begin
  spReadInteger(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.ReadFloat(const aRow, aCol: integer; aValue: TNullableDouble);
begin
  spReadFloat(FSheet, aRow, aCol, aValue);
end;

procedure TmSpreadsheetHelper.ReadUppercaseText(const aRow, aCol: integer; aValue: TNullableString; const aDoTrim : boolean = true);
begin
  spReadUppercaseText(FSheet, aRow, aCol, aValue, aDoTrim);
end;

procedure TmSpreadsheetHelper.ReadText(const aRow, aCol: integer; aValue: TNullableString; const aDoTrim : boolean = true);
begin
  spReadText(FSheet, aRow, aCol, aValue, aDoTrim);
end;

procedure TmSpreadsheetHelper.ReadDate(const aRow, aCol: integer; aValue: TNullableDateTime);
begin
  spReadDate(FSheet, aRow, aCol, aValue);
end;

end.
