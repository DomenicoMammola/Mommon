// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mXMLFormatterAsTxt;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mXMLFormatter;

type
  { TXmlFormatterAsText }

  TXmlFormatterAsText = class (TXmlFormatter)
  strict private
    FCurrentText : TStringList;
    FIndentationChar : Char;
  protected
    procedure WriteTag(const aText : String; const aIndentation : integer); override;
    procedure WriteClosingTag(const aText : String; const aIndentation : integer); overload; override;
    procedure WriteClosingTag(const aText : String); overload; override;
    procedure WriteValue(const aText : String); override;
    procedure WriteProlog(const aText : String); override;
    procedure WriteProcessingInstruction(const aText : String); override;
    procedure WriteComment(const aText: String; const aIndentation : integer); override;
    procedure WriteCDATA(const aText: String; const aIndentation : integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    class function XMLToText(const aXMLData : String; out aFormattedText, aError : String) : boolean;
    class function XMLToTxtFile(const aXMLData : String; const aFileName : String; out aError : String) : boolean;
  end;

implementation


{ TXmlFormatterAsText }

procedure TXmlFormatterAsText.WriteTag(const aText: String; const aIndentation: integer);
var
  s : String;
  i : integer;
begin
  s := '';
  for i := 1 to aIndentation do
    s:= s + FIndentationChar;
  FCurrentText.Append(s + aText);
end;

procedure TXmlFormatterAsText.WriteClosingTag(const aText: String; const aIndentation: integer);
begin
  WriteTag(aText, aIndentation);
end;

procedure TXmlFormatterAsText.WriteClosingTag(const aText: String);
begin
  FCurrentText.Strings[FCurrentText.Count- 1] := FCurrentText.Strings[FCurrentText.Count- 1] + aText
end;

procedure TXmlFormatterAsText.WriteValue(const aText: String);
begin
  FCurrentText.Strings[FCurrentText.Count- 1] := FCurrentText.Strings[FCurrentText.Count- 1] + aText;
end;

procedure TXmlFormatterAsText.WriteProlog(const aText: String);
begin
  FCurrentText.Append(aText);
end;

procedure TXmlFormatterAsText.WriteProcessingInstruction(const aText: String);
begin
  FCurrentText.Append(aText);
end;

procedure TXmlFormatterAsText.WriteComment(const aText: String; const aIndentation: integer);
var
  s : String;
  i : integer;
begin
  s := '';
  for i := 0 to aIndentation do
    s:= s + FIndentationChar;
  FCurrentText.Append(s + aText);
end;

procedure TXmlFormatterAsText.WriteCDATA(const aText: String; const aIndentation: integer);
var
  s : String;
  i : integer;
begin
  s := '';
  for i := 0 to aIndentation do
    s:= s + FIndentationChar;
  FCurrentText.Append(s + aText);
end;

constructor TXmlFormatterAsText.Create;
begin
  FCurrentText := TStringList.Create;
  FIndentationChar:= #9;
end;

destructor TXmlFormatterAsText.Destroy;
begin
  FCurrentText.Free;
  inherited Destroy;
end;

class function TXmlFormatterAsText.XMLToText(const aXMLData: String; out aFormattedText, aError: String): boolean;
var
  s : TXmlFormatterAsText;
begin
  aError := '';
  s := TXmlFormatterAsText.Create;
  try
    Result := s.FormatXML(aXMLData, aError);
    if Result then
      aFormattedText := s.FCurrentText.Text;
  finally
    s.Free;
  end;
end;

class function TXmlFormatterAsText.XMLToTxtFile(const aXMLData: String; const aFileName: String; out aError: String): boolean;
var
  s : TXmlFormatterAsText;
begin
  aError := '';
  s := TXmlFormatterAsText.Create;
  try
    Result := s.FormatXML(aXMLData, aError);
    if Result then
      s.FCurrentText.SaveToFile(aFileName);
  finally
    s.Free;
  end;
end;


end.
