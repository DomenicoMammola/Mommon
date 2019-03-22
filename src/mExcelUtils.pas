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

procedure WriteInteger(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TNullableInteger); overload;
procedure WriteInteger(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : integer); overload;

implementation

procedure WriteFloat(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableDouble; const aFractionalPartDigits: integer);
begin
  WriteFloat(aSheet, aRow, aCol, aValue.AsFloat, aFractionalPartDigits);
end;

procedure WriteFloat(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: double; const aFractionalPartDigits: integer);
begin
  aSheet.WriteNumber(aRow, aCol, aValue, nfFixed, aFractionalPartDigits);
end;

procedure WriteInteger(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableInteger);
begin
  WriteInteger(aSheet, aRow, aCol, aValue.AsInteger);
end;

procedure WriteInteger(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: integer);
begin
  aSheet.WriteNumber(aRow, aCol, aValue, nfFixed, 0);
end;

end.
