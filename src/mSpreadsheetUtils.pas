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

  { TmSpreadsheetFont }

  TmSpreadsheetFont = class
  strict private
    FItalic : boolean;
    FBold : boolean;
    FFontName : String;
    FFontSize : integer;
  public
    constructor Create;
    destructor Destroy; override;

    property Italic : boolean read FItalic write FItalic;
    property Bold : boolean read FBold write FBold;
    property FontName : String read FFontName write FFontName;
    property FontSize : integer read FFontSize write FFontSize;
  end;

  { TmSpreadsheetHelper }

  TmSpreadsheetHelper = class
  strict private
    FSheet: TsWorksheet;
    FDefaultFont : TmSpreadsheetFont;
    FDefaultRowHeight : integer;
    procedure WriteDefaultFont(const aRow, aCol : integer);
  public
    constructor Create; overload;
    constructor Create (aSheet : TsWorksheet); overload;
    destructor Destroy; override;

    procedure LockFirstRow;

    procedure WriteFloat(const aRow, aCol : integer; const aValue : TNullableDouble); overload;
    procedure WriteFloat(const aRow, aCol : integer; const aValue : double; const aFractionalPartDigits: integer); overload;
    procedure WriteFloat(const aRow, aCol : integer; const aValue : double); overload;
    procedure WriteFloat(const aRow, aCol : integer; const aValue : double; const aFormatString: String); overload;

    procedure WriteCurrency(const aRow, aCol : integer; const aValue : TNullableDouble; const aNegativeInRed : boolean); overload;
    procedure WriteCurrency(const aRow, aCol : integer; const aValue : double; const aNegativeInRed : boolean); overload;

    procedure WritePercentage(const aRow, aCol : integer; const aValue : TNullableInteger); overload;
    procedure WritePercentage(const aRow, aCol : integer; const aValue : integer); overload;

    procedure WriteDate(const aRow, aCol : integer; const aValue: TNullableDateTime); overload;
    procedure WriteDate(const aRow, aCol : integer; const aValue : TDateTime); overload;
    procedure WriteDateTime(const aRow, aCol : integer; const aValue: TNullableDateTime); overload;
    procedure WriteDateTime(const aRow, aCol : integer; const aValue : TDateTime); overload;
    procedure WriteInteger(const aRow, aCol : integer; const aValue : TNullableInteger); overload;
    procedure WriteInteger(const aRow, aCol : integer; const aValue : integer); overload;
    procedure WriteInt64(const aRow, aCol : integer; const aValue : Int64);
    procedure WriteBoolean(const aRow, aCol : integer; const aValue : boolean); overload;
    procedure WriteBoolean(const aRow, aCol : integer; const aValue : TNullableBoolean); overload;
    procedure WriteText(const aRow, aCol : integer; const aValue : TNullableString); overload;
    procedure WriteText(const aRow, aCol : integer; const aValue : TNullableString; const aItalic, aBold : boolean; const aBackgroundColor : DWord = scNotDefined); overload;
    procedure WriteText(const aRow, aCol : integer; const aValue : String); overload;
    procedure WriteText(const aRow, aCol : integer; const aValue : String; const aItalic, aBold : boolean; const aBackgroundColor : DWord = scNotDefined); overload;

    procedure WriteSouthBorder(const aRow, aCol: integer);
    procedure WriteBorders(const aRow, aCol : integer; const aNorth, aSouth, aEast, aWest : boolean);
    procedure WriteBackgroundColor(const aRow, aCol : integer; const aColor : DWord); // $00BBGGRR
    procedure WriteFontStyle(const aRow, aCol : integer; const aItalic, aBold : boolean);


    procedure ReadInteger(const aRow, aCol : integer; aValue : TNullableInteger);
    procedure ReadFloat(const aRow, aCol : integer; aValue : TNullableDouble);
    procedure ReadUppercaseText(const aRow, aCol: integer; aValue : TNullableString; const aDoTrim : boolean = true);
    procedure ReadText(const aRow, aCol : integer; aValue : TNullableString; const aDoTrim : boolean = true);
    procedure ReadDate(const aRow, aCol : integer; aValue : TNullableDateTime);

    property Sheet : TsWorksheet read FSheet write FSheet;
    property DefaultFont : TmSpreadsheetFont read FDefaultFont;
    property DefaultRowHeight : integer read FDefaultRowHeight write FDefaultRowHeight;
  end;

procedure spWriteFloat(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TNullableDouble; const aFractionalPartDigits : integer); overload;
procedure spWriteFloat(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : double; const aFractionalPartDigits : integer); overload;
procedure spWriteFloat(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : double; const aFormatString : String); overload;

procedure spWriteCurrency(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TNullableDouble; const aNegativeInRed : boolean); overload;
procedure spWriteCurrency(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : double; const aNegativeInRed : boolean); overload;

procedure spWritePercentage(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : TNullableInteger); overload;
procedure spWritePercentage(aSheet: TsWorksheet; const aRow, aCol : integer; const aValue : integer); overload;

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

procedure spWriteFloat(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: double; const aFormatString : String);
begin
  if aFormatString <> '' then
    aSheet.WriteNumber(aRow, aCol, aValue, nfGeneral, aFormatString)
  else
    aSheet.WriteNumber(aRow, aCol, aValue, nfGeneral);
end;

procedure spWriteDate(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TDateTime);
begin
  aSheet.WriteDateTime(aRow, aCol, aValue, nfShortDate);
end;

procedure spWriteCurrency(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableDouble; const aNegativeInRed: boolean);
begin
  if aValue.NotNull then
    spWriteCurrency(aSheet, aRow, aCol, aValue.AsFloat, aNegativeInRed);
end;

procedure spWriteCurrency(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: double; const aNegativeInRed: boolean);
begin
  if aNegativeInRed then
    aSheet.WriteCurrency(aRow, aCol, aValue, nfCurrencyRed)
  else
    aSheet.WriteCurrency(aRow, aCol, aValue, nfCurrency);
end;

procedure spWritePercentage(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: TNullableInteger);
begin
  if aValue.NotNull then
    spWritePercentage(aSheet, aRow, aCol, aValue.AsInteger);
end;

procedure spWritePercentage(aSheet: TsWorksheet; const aRow, aCol: integer; const aValue: integer);
var
  vDouble : Double;
begin
  vDouble := aValue * 0.01;
  aSheet.WriteNumber(aRow, aCol, vDouble, nfPercentage, 0);
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

{ TmSpreadsheetFont }

constructor TmSpreadsheetFont.Create;
begin
  FItalic:= false;
  FBold:= false;
  FFontName:= '';
  FFontSize:= 10;
end;

destructor TmSpreadsheetFont.Destroy;
begin
  inherited Destroy;
end;

{ TmSpreadsheetHelper }

procedure TmSpreadsheetHelper.WriteDefaultFont(const aRow, aCol: integer);
begin
  if FDefaultFont.FontName <> '' then
  begin
    Sheet.WriteFontName (aRow, aCol, FDefaultFont.FontName);
    Sheet.WriteFontSize (aRow, aCol, FDefaultFont.FontSize);
    WriteFontStyle(aRow, aCol, FDefaultFont.Italic, FDefaultFont.Bold);
  end;
  if FDefaultRowHeight > 0 then
    Sheet.WriteRowHeight(aRow, FDefaultRowHeight, suPoints);
end;

constructor TmSpreadsheetHelper.Create;
begin
  Self.Create(nil);
end;

constructor TmSpreadsheetHelper.Create(aSheet: TsWorksheet);
begin
  FSheet := aSheet;
  FDefaultFont := TmSpreadsheetFont.Create;
  FDefaultRowHeight:= 0;
end;

destructor TmSpreadsheetHelper.Destroy;
begin
  FDefaultFont.Free;
  inherited Destroy;
end;

procedure TmSpreadsheetHelper.LockFirstRow;
begin
  FSheet.Options:= FSheet.Options + [soHasFrozenPanes];
  FSheet.TopPaneHeight:= 1;
end;

procedure TmSpreadsheetHelper.WriteFloat(const aRow, aCol: integer; const aValue: TNullableDouble);
begin
  spWriteFloat(FSheet, aRow, aCol, aValue, aValue.FractionalPartDigits);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteFloat(const aRow, aCol: integer; const aValue: double; const aFractionalPartDigits: integer);
begin
  spWriteFloat(FSheet, aRow, aCol, aValue, aFractionalPartDigits);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteFloat(const aRow, aCol: integer; const aValue: double);
begin
  spWriteFloat(FSheet, aRow, aCol, aValue, '');
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteFloat(const aRow, aCol: integer; const aValue: double; const aFormatString: String);
begin
  spWriteFloat(FSheet, aRow, aCol, aValue, aFormatString);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteCurrency(const aRow, aCol: integer; const aValue: TNullableDouble; const aNegativeInRed: boolean);
begin
  spWriteCurrency(FSheet, aRow, aCol, aValue, aNegativeInRed);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteCurrency(const aRow, aCol: integer; const aValue: double; const aNegativeInRed: boolean);
begin
  spWriteCurrency(FSheet, aRow, aCol, aValue, aNegativeInRed);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WritePercentage(const aRow, aCol: integer; const aValue: TNullableInteger);
begin
  spWritePercentage(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WritePercentage(const aRow, aCol: integer; const aValue: integer);
begin
  spWritePercentage(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteDate(const aRow, aCol: integer; const aValue: TNullableDateTime);
begin
  spWriteDate(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteDate(const aRow, aCol: integer; const aValue: TDateTime);
begin
  spWriteDate(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteDateTime(const aRow, aCol: integer; const aValue: TNullableDateTime);
begin
  spWriteDateTime(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteDateTime(const aRow, aCol: integer; const aValue: TDateTime);
begin
  spWriteDateTime(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteInteger(const aRow, aCol: integer; const aValue: TNullableInteger);
begin
  spWriteInteger(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteInteger(const aRow, aCol: integer; const aValue: integer);
begin
  spWriteInteger(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteInt64(const aRow, aCol: integer; const aValue: Int64);
begin
  spWriteInt64(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteBoolean(const aRow, aCol: integer; const aValue: boolean);
begin
  FSheet.WriteBoolValue(aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteBoolean(const aRow, aCol: integer; const aValue: TNullableBoolean);
begin
  if aValue.NotNull then
    WriteBoolean(aRow, aCol, aValue.Value);
  WriteDefaultFont(aRow, aCol);
end;

procedure TmSpreadsheetHelper.WriteText(const aRow, aCol: integer; const aValue: TNullableString);
begin
  Self.WriteText(aRow, aCol, aValue, false, false);
end;

procedure TmSpreadsheetHelper.WriteText(const aRow, aCol: integer; const aValue: TNullableString; const aItalic, aBold: boolean; const aBackgroundColor : DWord = scNotDefined);
begin
  spWriteText(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
  WriteFontStyle(aRow, aCol, aItalic, aBold);
  WriteBackgroundColor(aRow, aCol, aBackgroundColor);
end;

procedure TmSpreadsheetHelper.WriteText(const aRow, aCol: integer; const aValue: String);
begin
  WriteText(aRow, aCol, aValue, false, false);
end;

procedure TmSpreadsheetHelper.WriteText(const aRow, aCol: integer; const aValue: String; const aItalic, aBold: boolean; const aBackgroundColor : DWord = scNotDefined);
begin
  spWriteText(FSheet, aRow, aCol, aValue);
  WriteDefaultFont(aRow, aCol);
  WriteFontStyle(aRow, aCol, aItalic, aBold);
  WriteBackgroundColor(aRow, aCol, aBackgroundColor);
end;

procedure TmSpreadsheetHelper.WriteSouthBorder(const aRow, aCol: integer);
begin
  FSheet.WriteBorders(aRow,aCol, [cbSouth]);
end;

procedure TmSpreadsheetHelper.WriteBorders(const aRow, aCol: integer; const aNorth, aSouth, aEast, aWest: boolean);
var
  tmpBorders : TsCellBorders;
begin
  tmpBorders:= [];
  if aNorth then
    Include(tmpBorders, cbNorth);
  if aSouth then
    Include(tmpBorders, cbSouth);
  if aEast then
    Include(tmpBorders, cbEast);
  if aWest then
    Include(tmpBorders, cbWest);
  FSheet.WriteBorders(aRow, aCol, tmpBorders);
end;

procedure TmSpreadsheetHelper.WriteFontStyle(const aRow, aCol: integer; const aItalic, aBold: boolean);
var
  tmpStyle : TsFontStyles;
begin
  tmpStyle := [];
  if aBold then
    tmpStyle := tmpStyle + [fssBold];
  if aItalic then
    tmpStyle := tmpStyle + [fssItalic];
  if tmpStyle <> [] then
    Sheet.WriteFontStyle(aRow, aCol, tmpStyle);
end;

procedure TmSpreadsheetHelper.WriteBackgroundColor(const aRow, aCol: integer; const aColor: DWord);
begin
  if aColor <> scNotDefined then
    Sheet.WriteBackgroundColor(aRow, aCol, aColor);
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
