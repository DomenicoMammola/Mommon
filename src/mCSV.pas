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
    FQuoteChar: Char;
    FLineEnding: String;
    FFilename : String;
    FExtStream : TStream;
    FOwnedFileStream : TFileStream;
    FUTF8 : boolean;
    FCurrentRow: String;
    function CheckFileStream : TStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartWrite;
    procedure EndWrite;

    procedure AppendCell(const aValue: String); overload;
    procedure AppendCell(const aValue: TAbstractNullable); overload;
    procedure AppendQuotedCell(const aValue : String);
    procedure AppendRow;

    property Delimiter: Char read FDelimiter write FDelimiter;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
    property LineEnding: String read FLineEnding write FLineEnding;
    property FileName: String read FFilename write FFilename;
    property UTF8: Boolean read FUTF8 write FUTF8;
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

constructor TmCSVBuilder.Create;
begin
  FDelimiter := ',';
  FQuoteChar := '"';
  FLineEnding := sLineBreak;
  FFilename:= '';
  FOwnedFileStream:= nil;
  FExtStream:= nil;
  FUTF8:= true;
  FCurrentRow := '';
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
    if FUTF8 then
      AddUTF8BOMToStream(FExtStream);
  end
  else
  begin
    if (FFilename = '') then
      raise Exception.Create('Filename is empty');
    FOwnedFileStream := TFileStream.Create(FFilename, fmCreate);
    if FUTF8 then
      AddUTF8BOMToStream(FOwnedFileStream);
  end;
end;

procedure TmCSVBuilder.EndWrite;
begin
  if FCurrentRow <> '' then
    AppendRow;
  FCurrentRow := '';
  FreeAndNil(FOwnedFileStream);
end;

procedure TmCSVBuilder.AppendCell(const aValue: String);
begin
  if FCurrentRow <> '' then
    FCurrentRow := FCurrentRow + FDelimiter + aValue
  else
    FCurrentRow:= aValue;
end;

procedure TmCSVBuilder.AppendCell(const aValue: TAbstractNullable);
begin
  if (aValue is TNullableString) and aValue.NotNull then
    Self.AppendQuotedCell(StringReplace(aValue.AsString, FDelimiter, '', [rfReplaceAll]))
  else
    Self.AppendCell(aValue.AsString);
end;

procedure TmCSVBuilder.AppendQuotedCell(const aValue: String);
begin
  if FQuoteChar <> '' then
    Self.AppendCell(Self.QuoteChar + StringReplace(aValue, FQuoteChar, '', [rfReplaceAll]) + Self.QuoteChar)
  else
    Self.AppendCell(aValue);
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
  FCurrentRow:= '';
end;

end.
