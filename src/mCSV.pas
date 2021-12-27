// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mCSV;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mNullables;

type

  { TmCSVBuilder }

  TmCSVBuilder = class
  strict private
    FDelimiter: Char;
    FQuoteChar: String;
    FLineEnding: String;
    FFilename : String;
    FExtStream : TStream;
    FOwnedFileStream : TFileStream;
    FUTF8 : boolean;
    FUTF8BOM : boolean;
    FCurrentRow: String;
    FIsNewRow: boolean;
    function CheckFileStream : TStream;
    procedure StartNewRow;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartWrite;
    procedure EndWrite;

    procedure AppendCell(const aValue: String);
    procedure AppendQuotedCell(const aValue : String);
    procedure AppendCellRFC4180(const aValue : String); overload; // https://tools.ietf.org/html/rfc4180
    procedure AppendCellRFC4180(const aValue: TAbstractNullable); overload;  // https://tools.ietf.org/html/rfc4180

    procedure AppendRow;

    property Delimiter: Char read FDelimiter write FDelimiter;
    property QuoteChar: String read FQuoteChar write FQuoteChar;
    property LineEnding: String read FLineEnding write FLineEnding;
    property FileName: String read FFilename write FFilename;
    property UTF8: Boolean read FUTF8 write FUTF8;
    property UTF8BOM : Boolean read FUTF8BOM write FUTF8BOM;
    property Stream : TStream read FExtStream write FExtStream;
  end;

implementation

uses
  sysutils,
  mUtility;

{ TmCSVBuilder }

function TmCSVBuilder.CheckFileStream : TStream;
begin
  if Assigned(FExtStream) then
    Result := FExtStream
  else if Assigned(FOwnedFileStream) then
    Result := FOwnedFileStream
  else
    raise Exception.Create('Missing file stream. Call StartWrite before.');
end;

procedure TmCSVBuilder.StartNewRow;
begin
  FCurrentRow:= '';
  FIsNewRow:= true;
end;

constructor TmCSVBuilder.Create;
begin
  FDelimiter := ',';
  FQuoteChar := '"';
  FLineEnding := sLineBreak;
  FFilename:= '';
  FOwnedFileStream:= nil;
  FExtStream:= nil;
  FUTF8:= true;
  FUTF8BOM := true;
  StartNewRow;
end;

destructor TmCSVBuilder.Destroy;
begin
  if Assigned(FOwnedFileStream) then
    EndWrite;
  inherited Destroy;
end;

procedure TmCSVBuilder.StartWrite;
begin
  if Assigned(FOwnedFileStream) then
    EndWrite;

  if Assigned(FExtStream) then
  begin
    if FUTF8 and FUTF8BOM then
      AddUTF8BOMToStream(FExtStream);
  end
  else
  begin
    if (FFilename = '') then
      raise Exception.Create('Filename is empty');
    FOwnedFileStream := TFileStream.Create(FFilename, fmCreate);
    if FUTF8 and FUTF8BOM then
      AddUTF8BOMToStream(FOwnedFileStream);
    StartNewRow;
  end;
end;

procedure TmCSVBuilder.EndWrite;
begin
  if FCurrentRow <> '' then
    AppendRow;
  FreeAndNil(FOwnedFileStream);
end;

procedure TmCSVBuilder.AppendCell(const aValue: String);
begin
  if FIsNewRow then
  begin
    FCurrentRow:= aValue;
    FIsNewRow := false;
  end
  else
    FCurrentRow := FCurrentRow + FDelimiter + aValue;
end;

procedure TmCSVBuilder.AppendQuotedCell(const aValue: String);
begin
  if FQuoteChar <> '' then
    Self.AppendCell(Self.QuoteChar + aValue + Self.QuoteChar)
  else
    Self.AppendCell(aValue);
end;

// https://tools.ietf.org/html/rfc4180
procedure TmCSVBuilder.AppendCellRFC4180(const aValue: String);
var
  tmp : String;
  useQuotes : boolean;
begin
  useQuotes := false;
  tmp := aValue;
  if FQuoteChar <> '' then
  begin
    if Pos(FQuoteChar, aValue) > 0 then
    begin
      useQuotes := true;
      tmp := StringReplace(aValue, FQuoteChar, FQuoteChar + FQuoteChar, [rfReplaceAll]);
    end;
    useQuotes := useQuotes or (Pos(FDelimiter, tmp) > 0);
  end;
  if useQuotes then
    Self.AppendQuotedCell(tmp)
  else
    Self.AppendCell(tmp);
end;

procedure TmCSVBuilder.AppendCellRFC4180(const aValue: TAbstractNullable);
begin
  if aValue.NotNull then
    Self.AppendCellRFC4180(aValue.AsString)
  else
    Self.AppendCell('');
end;

procedure TmCSVBuilder.AppendRow;
var
  s: RawByteString;
  fs : TStream;
begin
  fs := CheckFileStream;
  FCurrentRow:= FCurrentRow + sLineBreak;
  if FUTF8 then
  begin
    s := UTF8Encode(FCurrentRow);
    fs.Write(s[1], Length(s));
  end
  else
    fs.Write(FCurrentRow[1], Length(FCurrentRow));
  StartNewRow;
end;

end.
