// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mXMLFormatterAsPdf;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  fppdf,
  mXMLFormatter;

type

  { TXmlFormatterAsPdf }

  TXmlFormatterAsPdf = class (TXmlFormatter)
  strict private
    FDocument : TPDFDocument;
    FCurrentPage : TPDFPage;
    FSection : TPDFSection;
    FPageWidth, FPageHeight : TPDFFloat;
    Fx: TPDFFloat;
    FFontId : integer;
    FFontName : String;
    FFontSize : integer;
    FFontBoldId : integer;
    FFontBoldName : String;
    FFontBoldSize : integer;
    FCurrentRow : integer;
    FIndentationExtend : integer;

    FHorizontalBorder, FVerticalBorder : integer;
    function WritePdfFile(const aXMLData : String; const aFileName : String; const aTitle, aAuthor, aProducer : String; const aFontFile, aFontName, aFontBoldFile, aFontBoldName : String; out aError : String) : boolean;
    function GetY : TPDFFloat;
    function GetTextWidth (const aText : String): TPDFFloat;
    procedure AddPage;
    procedure WriteText(const aText : String; const aIndentation : integer; const aColor : TARGBColor);
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

    class function XMLToPdfFile(const aXMLData : String; const aFileName : String; const aTitle, aAuthor, aProducer : String; const aFontFile, aFontName, aFontBoldFile, aFontBoldName : String; out aError : String) : boolean;
  end;




implementation

uses
  SysUtils,
  fpreadjpeg, fpTTF, fpparsettf;

{ TXmlFormatterAsPdf }

function TXmlFormatterAsPdf.WritePdfFile(const aXMLData: String; const aFileName: String; const aTitle, aAuthor, aProducer : String; const aFontFile, aFontName, aFontBoldFile, aFontBoldName : String; out aError: String): boolean;
begin
  FHorizontalBorder:= 5;
  FVerticalBorder:= 5;
  FCurrentRow:= 0;
  FFontSize := 8;
  FFontBoldSize:= 8;
  FIndentationExtend := 3;

  FDocument := TPDFDocument.Create(Nil);
  try
    FDocument.Infos.Title:= aTitle;
    FDocument.Infos.Author:= aAuthor;
    FDocument.Infos.Producer:= aProducer;
    FDocument.Infos.CreationDate:= Now;
    FDocument.Options:= [poCompressFonts, poCompressText, poCompressImages];
    FDocument.StartDocument;
    FSection := FDocument.Sections.AddSection; // we always need at least one section

    FFontId := FDocument.AddFont(aFontFile, aFontName);
    FFontName := aFontName;
    FFontBoldId := FDocument.AddFont(aFontBoldFile, aFontBoldName);
    FFontBoldName:= aFontBoldName;

    Self.AddPage;

    Result := FormatXML(aXMLData, aError);

    if Result then
      FDocument.SaveToFile(aFileName);
  finally
    FDocument.Free;
  end;
end;

function TXmlFormatterAsPdf.GetY: TPDFFloat;
begin
  Result := FPageHeight - FVerticalBorder - (FCurrentRow * 4);
end;

function TXmlFormatterAsPdf.GetTextWidth(const aText: String): TPDFFloat;
var
  lFC: TFPFontCacheItem;
begin
  lFC := gTTFontCache.FindFont(FFontName);
  if Assigned(lFC) then
  begin
    Result := lFC.TextWidth(aText, FFontSize);
    Result := (Result * 25.4)/gTTFontCache.DPI;
  end
  else
    Result := (940 * Length(aText) * FFontSize) / 1540;
end;

procedure TXmlFormatterAsPdf.AddPage;
begin
  FCurrentPage := FDocument.Pages.AddPage;
  FCurrentPage.PaperType:= ptA4;
  FCurrentPage.UnitOfMeasure:= uomMillimeters; // uomPixels;
  FCurrentPage.Orientation:= ppoPortrait;
  FPageWidth := PDFTomm(round(FCurrentPage.Paper.Printable.R - FCurrentPage.Paper.Printable.L));
  FPageHeight := PDFTomm(round(FCurrentPage.Paper.Printable.B - FCurrentPage.Paper.Printable.T));
  FSection.AddPage(FCurrentPage);
  Fx := FHorizontalBorder;
  FCurrentPage.SetFont(FFontId, FFontSize);
end;

procedure TXmlFormatterAsPdf.WriteText(const aText: String; const aIndentation: integer; const aColor : TARGBColor);
var
  curRow : String;
  cutAt, len, i : integer;
  lastExtend : TPDFFloat;
  rows : TStringList;
begin
  FCurrentPage.SetColor(aColor, false);

  Fx := FHorizontalBorder + (aIndentation * FIndentationExtend);
  if Fx > FPageWidth - FIndentationExtend then
    Fx := FPageWidth - FIndentationExtend;
  FCurrentPage.SetFont(FFontId, FFontSize);

  rows := TStringList.Create;
  try
    curRow := '';
    len := Length(aText);
    i := 1;
    while i <= len do
    begin
      if aText[i] = #13 then
      begin
        if (i < len) and (aText[i + 1] = #10) then
          inc(i);
        rows.Add(curRow);
        curRow := '';
      end
      else if aText[i] = #10 then
      begin
        rows.Add(curRow);
        curRow := '';
      end
      else
        curRow := curRow + aText[i];
      inc(i);
    end;
    if curRow <> '' then
      rows.Add(curRow);

    for i := 0 to rows.Count - 1 do
    begin
      curRow := rows.Strings[i];
      lastExtend:= GetTextWidth(rows.Strings[i]);

      while curRow <> '' do
      begin
        cutAt := Length(curRow);
        while (cutAt > 1) and (GetTextWidth(Copy(curRow, 1, cutAt)) + Fx > FPageWidth) do
          dec(cutAt);
        if GetY > FPageHeight - FVerticalBorder then
          AddPage;
        FCurrentPage.WriteText(Fx, GetY, LeftStr(curRow, cutAt));
        curRow := Copy(curRow, cutAt + 1, Length(curRow));
        if curRow <> '' then
        begin
          lastExtend:= GetTextWidth(curRow);
          inc(FCurrentRow);
        end;
      end;

      if (rows.Count > 1) and (i < rows.Count - 1) then
        inc(FCurrentRow);
    end;
    Fx := Fx + lastExtend;
  finally
    rows.Free;
  end;
end;


procedure TXmlFormatterAsPdf.WriteTag(const aText: String; const aIndentation: integer);
begin
  inc (FCurrentRow);
  WriteText(aText, aIndentation, clPurple);
end;

procedure TXmlFormatterAsPdf.WriteClosingTag(const aText: String; const aIndentation: integer);
begin
  inc (FCurrentRow);
  WriteText(aText, aIndentation, clPurple);
end;

procedure TXmlFormatterAsPdf.WriteClosingTag(const aText: String);
begin
  WriteText(aText, 0, clPurple);
//  FCurrentPage.SetColor(clPurple, false);
//  FCurrentPage.SetFont(FFontId, FFontSize);
//  FCurrentPage.WriteText(Fx, GetY, aText);
end;

procedure TXmlFormatterAsPdf.WriteValue(const aText: String);
begin
  WriteText(aText, 0, clBlack);
//  FCurrentPage.SetColor(clBlack, false);
//  FCurrentPage.SetFont(FFontId, FFontSize);
//  FCurrentPage.WriteText(Fx, GetY, aText);
//  Fx := Fx + GetTextWidth(aText);
end;

procedure TXmlFormatterAsPdf.WriteProlog(const aText: String);
begin
  inc (FCurrentRow);
  WriteText(aText, 0, clOlive);
//  FCurrentPage.SetColor(clOlive, false);
//  FCurrentPage.SetFont(FFontId, FFontSize);
//  inc (FCurrentRow);
//  Fx := FHorizontalBorder;
//  FCurrentPage.WriteText(Fx, GetY, aText);
end;

procedure TXmlFormatterAsPdf.WriteProcessingInstruction(const aText: String);
begin
  Fx := FHorizontalBorder;
  inc (FCurrentRow);
  WriteText(aText, 0, clOlive);
//  FCurrentPage.SetColor(clOlive, false);
//  FCurrentPage.SetFont(FFontId, FFontSize);
//  Fx := FHorizontalBorder;
//  FCurrentPage.WriteText(Fx, GetY, aText);
end;

procedure TXmlFormatterAsPdf.WriteComment(const aText: String; const aIndentation: integer);
begin
  inc (FCurrentRow);
  WriteText(aText, aIndentation, clGreen);
end;

procedure TXmlFormatterAsPdf.WriteCDATA(const aText: String; const aIndentation: integer);
begin
  inc (FCurrentRow);
  WriteText(aText, aIndentation, clDkGray);
//  FCurrentPage.SetColor(clBlack, false);
//  FCurrentPage.SetFont(FFontId, FFontSize);
//  Fx := FHorizontalBorder + (aIndentation * FIndentationExtend);
//  FCurrentPage.WriteText(Fx, GetY, aText);
end;

constructor TXmlFormatterAsPdf.Create;
begin

end;

destructor TXmlFormatterAsPdf.Destroy;
begin
  inherited Destroy;
end;

class function TXmlFormatterAsPdf.XMLToPdfFile(const aXMLData: String; const aFileName: String; const aTitle, aAuthor, aProducer: String; const aFontFile, aFontName, aFontBoldFile, aFontBoldName : String;out aError: String): boolean;
var
  s : TXmlFormatterAsPdf;
begin
  aError := '';

  gTTFontCache.SearchPath.Add(ExtractFileDir(aFontFile));
  gTTFontCache.BuildFontCache;

  s := TXmlFormatterAsPdf.Create;
  try
    Result := s.WritePdfFile(aXMLData, aFileName, aTitle, aAuthor, aProducer, aFontFile, aFontName, aFontBoldFile, aFontBoldName, aError);
  finally
    s.Free;
  end;
end;




end.
