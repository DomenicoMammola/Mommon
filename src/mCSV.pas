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
    FFilestream : TFileStream;
    FUTF8 : boolean;
    FCurrentRow: String;
    procedure CheckFileStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartWrite;
    procedure EndWrite;

    procedure AppendCell(const aValue: String); overload;
    procedure AppendCell(const aValue: TAbstractNullable); overload;
    procedure AppendRow;

    property Delimiter: Char read FDelimiter write FDelimiter;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
    property LineEnding: String read FLineEnding write FLineEnding;
    property FileName: String read FFilename write FFilename;
    property UTF8: Boolean read FUTF8 write FUTF8;
  end;

implementation

uses
  sysutils,
  mUtility;

{ TmCSVBuilder }

procedure TmCSVBuilder.CheckFileStream;
begin
  if not Assigned(FFilestream) then
    raise Exception.Create('Missing file stream. Call StartWrite before.');
end;

constructor TmCSVBuilder.Create;
begin
  FDelimiter := ',';
  FQuoteChar := '"';
  FLineEnding := sLineBreak;
  FFilename:= '';
  FFilestream:= nil;
  FUTF8:= true;
  FCurrentRow := '';
end;

destructor TmCSVBuilder.Destroy;
begin
  if Assigned(FFilestream) then
    EndWrite;
  inherited Destroy;
end;

procedure TmCSVBuilder.StartWrite;
begin
  if FFilename = '' then
    raise Exception.Create('Filename is empty');

  if Assigned(FFilestream) then
    EndWrite;

  FFileStream := TFileStream.Create(FFilename, fmCreate);
  if FUTF8 then
    AddUTF8BOMToFileStream(FFilestream);
end;

procedure TmCSVBuilder.EndWrite;
begin
  CheckFileStream;
  if FCurrentRow <> '' then
    AppendRow;
  FCurrentRow := '';
  FreeAndNil(FFilestream);
end;

procedure TmCSVBuilder.AppendCell(const aValue: String);
begin
  CheckFileStream;
  if FCurrentRow <> '' then
    FCurrentRow := FCurrentRow + FDelimiter + aValue
  else
    FCurrentRow:= aValue;
end;

procedure TmCSVBuilder.AppendCell(const aValue: TAbstractNullable);
begin
  if aValue is TNullableString then
  begin
    if FQuoteChar <> '' then
      Self.AppendCell(FQuoteChar + StringReplace(StringReplace(aValue.AsString, FQuoteChar, '', [rfReplaceAll]), FDelimiter, '', [rfReplaceAll]) + FQuoteChar)
    else
      Self.AppendCell(StringReplace(aValue.AsString, FDelimiter, '', [rfReplaceAll]));
  end
  else
    Self.AppendCell(aValue.AsString);
end;

procedure TmCSVBuilder.AppendRow;
var
  s: RawByteString;
begin
  CheckFileStream;
  FCurrentRow:= FCurrentRow + sLineBreak;
  if FUTF8 then
  begin
    s := UTF8Encode(FCurrentRow);
    FFileStream.Write(s[1], Length(s));
  end
  else
    FFilestream.Write(FCurrentRow[1], Length(FCurrentRow));
  FCurrentRow:= '';
end;

end.
