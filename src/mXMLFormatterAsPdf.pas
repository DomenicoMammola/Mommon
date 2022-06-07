// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// based on:
// https://www.freepascal.org/~michael/articles/lazpdf/lazpdf.pdf

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
    FCurrentRow : integer;
    FRowHeight : TPDFFloat;
    FSingleIndentationWidth : TPDFFloat;
    FHorizontalBorder, FVerticalBorder : TPDFFloat;

    function WritePdfToStream(const aXMLData : String; aStream: TStream; const aTitle, aAuthor, aProducer : String; const aFontFile, aFontName : String; out aError : String) : boolean;
    function GetY : TPDFFloat;
    function GetTextWidth (const aText : String): TPDFFloat;
    function GetTextHeight (const aText : String): TPDFFloat;
    procedure AddPage;
    procedure WriteText(const aText : String; const aIndentation : integer; const aAppend: boolean; const aColor : TARGBColor);
  private
    const DEFAULT_HORIZONTAL_BORDER = 5;
    const DEFAULT_VERTICAL_BORDER = 5;
    const DEFAULT_FONT_SIZE = 8;
    const DEFAULT_SINGLE_INDENTATION_WIDTH = 3;
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

    class function XMLToPdf(const aXMLData : String; const aFileName : String; const aTitle, aAuthor, aProducer : String; const aFontFile, aFontName : String;
      aFontSize : integer;
      aHorizontalBorder, aVerticalBorder : TPDFFloat;
      aSingleIndentationWidth : TPDFFloat;
      out aError : String) : boolean; overload;
    class function XMLToPdf(const aXMLData : String; const aFileName : String; const aTitle, aAuthor, aProducer : String; const aFontFile, aFontName : String;
      out aError : String) : boolean; overload;
    class function XMLToPdf(const aXMLData : String; aStream: TStream; const aTitle, aAuthor, aProducer : String; const aFontFile, aFontName : String;
      aFontSize : integer;
      aHorizontalBorder, aVerticalBorder : TPDFFloat;
      aSingleIndentationWidth : TPDFFloat;
      out aError : String) : boolean; overload;
    class function XMLToPdf(const aXMLData : String; aStream: TStream; const aTitle, aAuthor, aProducer : String; const aFontFile, aFontName : String;
      out aError : String) : boolean; overload;


    property HorizontalBorder: TPDFFloat read FHorizontalBorder write FHorizontalBorder;
    property VerticalBorder : TPDFFloat read FVerticalBorder write FVerticalBorder;
    property FontSize : integer read FFontSize write FFontSize;
    property SingleIndentationWidth : TPDFFloat read FSingleIndentationWidth write FSingleIndentationWidth;
  end;




implementation

uses
  SysUtils, Math,
  fpreadjpeg, fpTTF, fpparsettf;

{ TXmlFormatterAsPdf }



function TXmlFormatterAsPdf.WritePdfToStream(const aXMLData: String; aStream: TStream; const aTitle, aAuthor, aProducer: String; const aFontFile, aFontName: String; out aError: String): boolean;
var
  h : TPDFFloat;
begin
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

    Fx := FHorizontalBorder;
    h := GetTextHeight('W');
    FRowHeight:= h * 1.4; // Max(h * 1.3, h + 0.1) ;
    Self.AddPage;

    Result := FormatXML(aXMLData, aError);

    if Result then
      FDocument.SaveToStream(aStream);
  finally
    FDocument.Free;
  end;
end;

function TXmlFormatterAsPdf.GetY: TPDFFloat;
begin
  Result := FPageHeight - FVerticalBorder - (FCurrentRow * FRowHeight);
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

function TXmlFormatterAsPdf.GetTextHeight(const aText: String): TPDFFloat;
var
  lFC: TFPFontCacheItem;
  des : Single;
begin
  lFC := gTTFontCache.FindFont(FFontName);
  if Assigned(lFC) then
  begin
    Result := lFC.TextHeight(aText, FFontSize, des);
    Result := (Result * 25.4)/gTTFontCache.DPI;
  end
  else
    Result := (0.70 * FFontSize);
end;

procedure TXmlFormatterAsPdf.AddPage;
begin
  FCurrentPage := FDocument.Pages.AddPage;
  FCurrentPage.PaperType:= ptA4;
  FCurrentPage.UnitOfMeasure:= uomMillimeters;
  FCurrentPage.Orientation:= ppoPortrait;
  FPageWidth := PDFTomm(round(FCurrentPage.Paper.Printable.R - FCurrentPage.Paper.Printable.L));
  FPageHeight := PDFTomm(round(FCurrentPage.Paper.Printable.B - FCurrentPage.Paper.Printable.T));
  FSection.AddPage(FCurrentPage);
  FCurrentPage.SetFont(FFontId, FFontSize);
end;


procedure TXmlFormatterAsPdf.WriteText(const aText: String; const aIndentation: integer; const aAppend: boolean; const aColor : TARGBColor);
var
  curRow : String;
  cutAt, len, i : integer;
  lastExtend : TPDFFloat;
  rows : TStringList;
begin
  if (not aAppend) or (Fx = 0) then
    Fx := FHorizontalBorder + (aIndentation * FSingleIndentationWidth);

  if Fx > FPageWidth - FSingleIndentationWidth then
    Fx := FPageWidth - FSingleIndentationWidth;

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
        if GetY < FVerticalBorder then
        begin
          AddPage;
          FCurrentRow:= 0;
        end;
        FCurrentPage.SetColor(aColor, false);
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
  WriteText(aText, aIndentation,false, clPurple);
end;

procedure TXmlFormatterAsPdf.WriteClosingTag(const aText: String; const aIndentation: integer);
begin
  inc (FCurrentRow);
  WriteText(aText, aIndentation, false, clPurple);
end;

procedure TXmlFormatterAsPdf.WriteClosingTag(const aText: String);
begin
  WriteText(aText, 0, true, clPurple);
end;

procedure TXmlFormatterAsPdf.WriteValue(const aText: String);
begin
  WriteText(aText, 0, true, clBlack);
end;

procedure TXmlFormatterAsPdf.WriteProlog(const aText: String);
begin
  inc (FCurrentRow);
  WriteText(aText, 0, false, clOlive);
end;

procedure TXmlFormatterAsPdf.WriteProcessingInstruction(const aText: String);
begin
  Fx := FHorizontalBorder;
  inc (FCurrentRow);
  WriteText(aText, 0, false, clOlive);
end;

procedure TXmlFormatterAsPdf.WriteComment(const aText: String; const aIndentation: integer);
begin
  inc (FCurrentRow);
  WriteText(aText, aIndentation, false, clGreen);
end;

procedure TXmlFormatterAsPdf.WriteCDATA(const aText: String; const aIndentation: integer);
begin
  inc (FCurrentRow);
  WriteText(aText, aIndentation, false, clDkGray);
end;

constructor TXmlFormatterAsPdf.Create;
begin
  FHorizontalBorder:= DEFAULT_HORIZONTAL_BORDER;
  FVerticalBorder:= DEFAULT_VERTICAL_BORDER;
  FCurrentRow:= -1;
  FFontSize := DEFAULT_FONT_SIZE;
  FSingleIndentationWidth := DEFAULT_SINGLE_INDENTATION_WIDTH;
  FRowHeight := 0;
end;

destructor TXmlFormatterAsPdf.Destroy;
begin
  inherited Destroy;
end;

class function TXmlFormatterAsPdf.XMLToPdf(const aXMLData: String; const aFileName: String; const aTitle, aAuthor, aProducer: String; const aFontFile, aFontName: String; aFontSize: integer; aHorizontalBorder, aVerticalBorder: TPDFFloat; aSingleIndentationWidth: TPDFFloat; out aError: String): boolean;
var
  fileStream : TFileStream;
begin
  fileStream := TFileStream.Create(aFileName, fmCreate);
  try
    Result := XMLToPdf(aXMLData, fileStream, aTitle, aAuthor, aProducer, aFontFile, aFontName, aError);
  finally
    fileStream.Free;
  end;
end;

class function TXmlFormatterAsPdf.XMLToPdf(const aXMLData: String; const aFileName: String; const aTitle, aAuthor, aProducer: String; const aFontFile, aFontName : String;out aError: String): boolean;
begin
  Result := TXmlFormatterAsPdf.XMLToPdf(aXMLData, aFileName, aTitle, aAuthor, aProducer, aFontFile, aFontName,
    DEFAULT_FONT_SIZE, DEFAULT_HORIZONTAL_BORDER, DEFAULT_VERTICAL_BORDER, DEFAULT_SINGLE_INDENTATION_WIDTH, aError);
end;

class function TXmlFormatterAsPdf.XMLToPdf(const aXMLData: String; aStream: TStream; const aTitle, aAuthor, aProducer: String; const aFontFile, aFontName: String; aFontSize: integer; aHorizontalBorder, aVerticalBorder: TPDFFloat; aSingleIndentationWidth: TPDFFloat; out aError: String): boolean;
var
  s : TXmlFormatterAsPdf;
begin
  aError := '';

  gTTFontCache.SearchPath.Add(ExtractFileDir(aFontFile));
  gTTFontCache.BuildFontCache;

  s := TXmlFormatterAsPdf.Create;
  try
    s.VerticalBorder:= aVerticalBorder;
    s.HorizontalBorder:= aHorizontalBorder;
    s.FontSize:= aFontSize;
    s.FFontName:= aFontName;
    s.FSingleIndentationWidth:= aSingleIndentationWidth;
    Result := s.WritePdfToStream(aXMLData, aStream, aTitle, aAuthor, aProducer, aFontFile, aFontName, aError);
  finally
    s.Free;
  end;
end;

class function TXmlFormatterAsPdf.XMLToPdf(const aXMLData: String; aStream: TStream; const aTitle, aAuthor, aProducer: String; const aFontFile, aFontName: String; out aError: String): boolean;
begin
  Result := TXmlFormatterAsPdf.XMLToPdf(aXMLData, aStream, aTitle, aAuthor, aProducer, aFontFile, aFontName,
    DEFAULT_FONT_SIZE, DEFAULT_HORIZONTAL_BORDER, DEFAULT_VERTICAL_BORDER, DEFAULT_SINGLE_INDENTATION_WIDTH, aError);
end;




end.
