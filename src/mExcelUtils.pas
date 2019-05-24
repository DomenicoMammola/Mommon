// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mExcelUtils;

interface

uses
  fpspreadsheet, fpstypes,
  mNullables;

procedure WriteFloat(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TNullableDouble; const aFractionalPartDigits : integer); overload;
procedure WriteFloat(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : double; const aFractionalPartDigits : integer); overload;

procedure WriteDate(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue: TNullableDateTime); overload;
procedure WriteDate(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TDateTime); overload;

procedure WriteDateTime(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue: TNullableDateTime); overload;
procedure WriteDateTime(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TDateTime); overload;

procedure WriteInteger(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TNullableInteger); overload;
procedure WriteInteger(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : integer); overload;
procedure WriteInt64(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : Int64); overload;

implementation

procedure WriteFloat(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableDouble; const aFractionalPartDigits: integer);
begin
  if aValue.NotNull then
    WriteFloat(aSheet, aRow, aCol, aValue.AsFloat, aFractionalPartDigits);
end;

procedure WriteFloat(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: double; const aFractionalPartDigits: integer);
begin
  aSheet.WriteNumber(aRow, aCol, aValue, nfFixed, aFractionalPartDigits);
end;

procedure WriteDate(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TDateTime);
begin
  aSheet.WriteDateTime(aRow, aCol, aValue, nfShortDate);
end;

procedure WriteDate(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableDateTime);
begin
  if aValue.NotNull then
    WriteDate(aSheet, aRow, aCol, aValue.Value);
end;

procedure WriteDateTime(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableDateTime);
begin
  if aValue.NotNull then
    WriteDateTime(aSheet, aRow, aCol, aValue.Value);

end;

procedure WriteDateTime(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TDateTime);
begin
  aSheet.WriteDateTime(aRow, aCol, aValue, nfShortDateTime);
end;

procedure WriteInteger(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableInteger);
begin
  if aValue.NotNull then
    WriteInteger(aSheet, aRow, aCol, aValue.AsInteger);
end;

procedure WriteInteger(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: integer);
var
  vDouble : Double;
begin
  vDouble := aValue;
  aSheet.WriteNumber(aRow, aCol, vDouble, nfFixed, 0);
end;

procedure WriteInt64(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: Int64);
var
  vDouble : Double;
begin
  vDouble := aValue;
  aSheet.WriteNumber(aRow, aCol, vDouble, nfFixed, 0);
end;

end.
