// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mXMLFormatter;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type

  { TXmlFormatter }

  TXmlFormatter = class abstract
  strict private
  protected
    procedure WriteTag(const aText : String; const aIndentation : integer); virtual; abstract;
    procedure WriteClosingTag(const aText : String; const aIndentation : integer); overload; virtual; abstract;
    procedure WriteClosingTag(const aText : String); overload; virtual; abstract;
    procedure WriteValue(const aText : String); virtual; abstract;
    procedure WriteProlog(const aText : String); virtual; abstract;
    procedure WriteProcessingInstruction(const aText : String); virtual; abstract;
    procedure WriteComment(const aText: String; const aIndentation : integer); virtual; abstract;
    procedure WriteCDATA(const aText: String; const aIndentation : integer); virtual; abstract;

    function FormatXML(const aXMLData : String; out aError : String) : boolean;
  end;

implementation

uses
  SysUtils;

type

  TXMLToken = (txUndefined, txEOF, txProlog, txProcessingInstruction, txComment, txTag, txClosingTag, txSelfClosedTag, txValue, txCDATA);

  { TLexState }

  TLexState = record
  strict private
    FXMLData : string;
    FCharIndex : integer;
    FLenXMLData : integer;
  public
    InQuotes : boolean;
    QuotesChar : Char;

    function IsEof : boolean;
    procedure SetXMLData (const aXMLData : string);
    function GetXMLData : string;
    function GetCurrentChar : Char;
    function LookAhead (const aNumOfChars: integer): String;
    procedure Advance;
    procedure AdvanceBy (const aNumOfChars : integer);
    procedure GoBack;
    function CharIndex : integer;
  end;

  { TLexResult }

  TLexResult = record
  public
    Token : TXMLToken;
    Prolog : String;
    ProcessingInstruction : String;
    Tag : String;
    Value : String;
    Comment : String;
    CDATA : String;
    Error : String;

    procedure Clear;
    procedure ComposeError(const aError : String; const aLexState : TLexState);
  end;

function CalcPrologOrProcessingInstruction(var aLexState : TLexState; out aLexResult : TLexResult) : boolean;
var
  currentPI, errType : String;
  isProlog : boolean;
  gotCR : boolean;
  lastChar : Char;
begin
  Result := false;
  gotCR := false;
  lastChar := #0;
  aLexState.Advance;
  if aLexState.IsEof then
  begin
    aLexResult.ComposeError('Got EOF inside a prolog or processing instruction tag', aLexState);
    exit;
  end;
  isProlog := aLexState.LookAhead(4) = 'xml ';
  if isProlog then
    errType:= 'prolog'
  else
    errType:= 'process instruction';

  currentPI := '';
  while true do
  begin
    if aLexState.IsEof then
    begin
      aLexResult.ComposeError('Got EOF inside the ' + errType + ' tag', aLexState);
      exit;
    end;
    if aLexState.GetCurrentChar ='?' then
    begin
      aLexState.Advance;
      if aLexState.IsEof then
      begin
        aLexResult.ComposeError('Got EOF inside the ' + errType + ' tag', aLexState);
        exit;
      end;
      if aLexState.GetCurrentChar = '>' then
      begin
        if currentPI = '' then
        begin
          aLexResult.ComposeError('Empty ' + errType, aLexState);
          exit;
        end;
        if isProlog then
        begin
          aLexResult.Token:= txProlog;
          aLexResult.Prolog := currentPI;
        end
        else
        begin
          aLexResult.Token:= txProcessingInstruction;
          aLexResult.ProcessingInstruction := currentPI;
        end;
        aLexState.Advance;
        Result := true;
        exit;
      end
      else
      begin
        aLexResult.ComposeError('Unclosed ' + errType, aLexState);
        exit;
      end;
    end
    else
    begin
      if aLexState.GetCurrentChar = '<' then
      begin
        aLexResult.ComposeError('Malformed ' + errType, aLexState);
        exit;
      end;
      if (aLexState.GetCurrentChar = #10) or (aLexState.GetCurrentChar = #13) then
        gotCR := true
      else
      begin
        if gotCR then
        begin
          if (aLexState.GetCurrentChar <> #9) and (aLexState.GetCurrentChar <> #32) then
          begin
            if lastChar <> #32 then
              currentPI := currentPI + #32;
            lastChar:= aLexState.GetCurrentChar;
            currentPI:= currentPI + aLexState.GetCurrentChar;
            gotCR := false;
          end;
        end
        else
        begin
          if (aLexState.GetCurrentChar = #9) or (aLexState.GetCurrentChar = #32) then
          begin
            if lastChar <> #32 then
            begin
              lastChar:= #32;
              currentPI:= currentPI + #32;
            end;
          end
          else
          begin
            lastChar:= aLexState.GetCurrentChar;
            currentPI:= currentPI + aLexState.GetCurrentChar;
          end;
        end;
      end;
      aLexState.Advance;
    end;
  end;
end;

function CalcClosingTag(var aLexState : TLexState; out aLexResult : TLexResult) : boolean;
var
  currentTag : String;
begin
  Result := false;
  aLexState.Advance;
  currentTag:= '';
  while true do
  begin
    if aLexState.IsEof then
    begin
      aLexResult.ComposeError('Got EOF inside a closing tag', aLexState);
      exit;
    end;
    if aLexState.GetCurrentChar ='>' then
    begin
      if currentTag = '' then
      begin
        aLexResult.ComposeError('Empty closing tag', aLexState);
        exit;
      end;
      aLexResult.Token:= txClosingTag;
      aLexResult.Tag := currentTag;
      aLexState.Advance;
      Result := true;
      exit;
    end
    else
    begin
      if aLexState.GetCurrentChar = '<' then
      begin
        aLexResult.ComposeError('Malformed closing tag', aLexState);
        exit;
      end;
      currentTag:= currentTag + aLexState.GetCurrentChar;
      aLexState.Advance;
    end;
  end;
  Result := true;
end;

function CalcCommentOrCDATA(var aLexState: TLexState; out aLexResult : TLexResult) : boolean;
var
  currentComment, CDATA : String;
   i : integer;
begin
  Result := false;
  aLexState.Advance;
  if aLexState.IsEof then
  begin
    aLexResult.ComposeError('Got EOF inside a comment or CDATA or document type', aLexState);
    exit;
  end;
  if aLexState.GetCurrentChar = '-' then
  begin
    // comment
    aLexState.Advance;
    if aLexState.IsEof then
    begin
      aLexResult.ComposeError('Got EOF inside a comment', aLexState);
      exit;
    end;
    if aLexState.GetCurrentChar = '-' then
    begin
      currentComment := '';
      while true do
      begin
        aLexState.Advance;
        if aLexState.IsEof then
        begin
          aLexResult.ComposeError('Got EOF inside a comment', aLexState);
          exit;
        end;
        currentComment := currentComment + aLexState.GetCurrentChar;
        if RightStr(currentComment, 2) = '--' then
        begin
          aLexState.Advance;
          if aLexState.IsEof then
          begin
            aLexResult.ComposeError('Got EOF inside a comment', aLexState);
            exit;
          end;
          // For compatibility, the string " -- " (double-hyphen) MUST NOT occur within comments.] Parameter entity references MUST NOT be recognized within comments.
          // https://www.w3.org/TR/xml/#sec-comments
          if aLexState.GetCurrentChar = '>' then
          begin
            if Length(currentComment) = 2 then
            begin
              aLexResult.ComposeError('Malformed comment', aLexState);
              exit;
            end;
            aLexResult.Token:= txComment;
            currentComment := Copy(currentComment, 1, Length(currentComment) - 2);
            aLexResult.Comment:= currentComment;
            aLexState.Advance;
            Result := true;
            exit;
          end
          else
          begin
            aLexResult.ComposeError('Malformed comment', aLexState);
            exit;
          end;
        end;
      end;
    end
    else
    begin
      aLexResult.ComposeError('Malformed comment', aLexState);
      exit;
    end;
  end
  else if aLexState.GetCurrentChar = '[' then
  begin
    // [CDATA[
    CDATA := aLexState.GetCurrentChar;
    for i := 1 to 6 do
    begin
      aLexState.Advance;
      if aLexState.IsEof then
      begin
        aLexResult.ComposeError('Got EOF inside CDATA', aLexState);
        exit;
      end;
      CDATA := CDATA + aLexState.GetCurrentChar;
    end;
    if CDATA <> '[CDATA[' then
    begin
      aLexResult.ComposeError('Malformed CDATA', aLexState);
      exit;
    end;
    CDATA := '';
    while true do
    begin
      aLexState.Advance;
      if aLexState.IsEof then
      begin
        aLexResult.ComposeError('Got EOF inside CDATA', aLexState);
        exit;
      end;
      CDATA := CDATA + aLexState.GetCurrentChar;
      if RightStr(CDATA, 2) = ']]' then
      begin
        aLexState.Advance;
        if aLexState.IsEof then
        begin
          aLexResult.ComposeError('Got EOF inside a comment', aLexState);
          exit;
        end;
        if aLexState.GetCurrentChar = '>' then
        begin
          if Length(CDATA) = 2 then
          begin
            aLexResult.ComposeError('Malformed CDATA', aLexState);
            exit;
          end;
          aLexResult.Token:= txCDATA;
          CDATA := Copy(CDATA, 1, Length(CDATA) - 2);
          aLexResult.CDATA:= CDATA;
          aLexState.Advance;
          Result := true;
          exit;
        end
        else
        begin
          aLexResult.ComposeError('Malformed CDATA', aLexState);
          exit;
        end;
      end;
    end;
  end
  else
  begin
    aLexResult.ComposeError('Malformed comment or CDATA or document type', aLexState);
    exit;
  end;
  Result := true;
end;

function CalcGenericTag(var aLexState : TLexState; out aLexResult : TLexResult) : boolean;
var
  currentTag : String;
  gotCR : boolean;
  lastChar : Char;
begin
  Result := false;

  gotCR := false;
  lastChar := #0;

  aLexState.Advance;
  if aLexState.IsEof then
  begin
    aLexResult.ComposeError('Got EOF', aLexState);
    exit;
  end;

  if aLexState.GetCurrentChar = '?' then
    Result := CalcPrologOrProcessingInstruction(aLexState, aLexResult)
  else if aLexState.GetCurrentChar = '/' then
    Result := CalcClosingTag(aLexState, aLexResult)
  else if aLexState.GetCurrentChar = '!' then
    Result := CalcCommentOrCDATA(aLexState, aLexResult)
  else
  begin
    if aLexState.GetCurrentChar = '>' then
    begin
      aLexResult.ComposeError('Empty tag', aLexState);
      exit;
    end;

    currentTag := aLexState.GetCurrentChar;
    aLexState.Advance;
    while true do
    begin
      if aLexState.IsEof then
      begin
        aLexResult.ComposeError('Got EOF inside a tag', aLexState);
        exit;
      end;
      if (not aLexState.InQuotes) and (aLexState.GetCurrentChar = '>') then
      begin
        aLexResult.Token:= txTag;
        aLexResult.Tag:= currentTag;
        aLexState.Advance;
        Result := true;
        exit;
      end
      else if (not aLexState.InQuotes) and (aLexState.GetCurrentChar = '/') then
      begin
        aLexState.Advance;
        if aLexState.IsEof then
        begin
          aLexResult.ComposeError('Got EOF inside a tag', aLexState);
          exit;
        end;
        if aLexState.GetCurrentChar = '>' then
        begin
          aLexResult.Token:= txSelfClosedTag;
          aLexResult.Tag:= currentTag;
          aLexState.Advance;
          Result := true;
          exit;
        end
        else
        begin
          aLexResult.ComposeError('Malformed self closed tag', aLexState);
          exit;
        end;
      end
      else
      begin
        if (aLexState.GetCurrentChar = '"') or (aLexState.GetCurrentChar = '''') then
        begin
          if aLexState.InQuotes then
          begin
            if aLexState.QuotesChar = aLexState.GetCurrentChar then
              aLexState.InQuotes:= false;
          end
          else
          begin
            aLexState.InQuotes:= true;
            aLexState.QuotesChar:= aLexState.GetCurrentChar;
          end;
        end
        else if aLexState.GetCurrentChar = '<' then
        begin
          aLexResult.ComposeError('Malformed tag', aLexState);
          exit;
        end;

        if (aLexState.GetCurrentChar = #10) or (aLexState.GetCurrentChar = #13) then
          gotCR := true
        else
        begin
          if gotCR then
          begin
            if (aLexState.GetCurrentChar <> #9) and (aLexState.GetCurrentChar <> #32) then
            begin
              if lastChar <> #32 then
                currentTag := currentTag + #32;
              lastChar:= aLexState.GetCurrentChar;
              currentTag:= currentTag + aLexState.GetCurrentChar;
              gotCR := false;
            end;
          end
          else
          begin
            if (aLexState.GetCurrentChar = #9) or (aLexState.GetCurrentChar = #32) then
            begin
              if lastChar <> #32 then
              begin
                lastChar:= #32;
                currentTag:= currentTag + #32;
              end;
            end
            else
            begin
              lastChar:= aLexState.GetCurrentChar;
              currentTag:= currentTag + aLexState.GetCurrentChar;
            end;
          end;
        end;

        aLexState.Advance;
      end;
    end;
  end;
end;

function CalcValue(var aLexState: TLexState; const aLeadingWhiteSpaces : integer; out aLexResult : TLexResult) : boolean;
var
  currentValue : String;
  i : integer;
begin
  Result := false;
  currentValue := aLexState.GetCurrentChar;
  aLexState.Advance;
  while true do
  begin
    if aLexState.IsEof then
    begin
      aLexResult.ComposeError('Got EOF inside a value', aLexState);
      exit;
    end;
    if (aLexState.GetCurrentChar = '<') then
    begin
      aLexResult.Token:= txValue;
      aLexResult.Value:= currentValue;
      for i := 1 to aLeadingWhiteSpaces do
        aLexResult.Value:= #32 + aLexResult.Value;
      Result := true;
      exit;
    end
    else if (aLexState.GetCurrentChar = '>') then
    begin
      aLexResult.ComposeError('Got > outside a tag', aLexState);
      exit;
    end
    else
    begin
      currentValue := currentValue + aLexState.GetCurrentChar;
      aLexState.Advance;
    end;
  end;
end;

function yylex (var aLexState: TLexState; out aLexResult : TLexResult) : boolean;
var
  leadingWhiteSpaces : integer;
begin
  Result := false;
  if aLexState.IsEof then
  begin
    aLexResult.Token := txEOF;
    Result := true;
    exit;
  end;

  leadingWhiteSpaces := 0;

  while (aLexState.GetCurrentChar = #32) or (aLexState.GetCurrentChar = #10) or (aLexState.GetCurrentChar = #13) or (aLexState.GetCurrentChar = #9) do
  begin
    if aLexState.GetCurrentChar = #32 then
      inc(leadingWhiteSpaces);
    aLexState.Advance;

    if aLexState.IsEof then
    begin
      aLexResult.Token := txEOF;
      Result := true;
      exit;
    end;
  end;

  if aLexState.GetCurrentChar = '<' then
    Result := CalcGenericTag(aLexState, aLexResult)
  else if aLexState.GetCurrentChar = '>' then
  begin
    aLexResult.ComposeError('Got > outside a tag', aLexState);
    exit;
  end
  else
    Result := CalcValue(aLexState, leadingWhiteSpaces, aLexResult);
end;

{ TXmlFormatter }

function TXmlFormatter.FormatXML(const aXMLData: String; out aError : String): boolean;
var
  lexState : TLexState;
  lexResult : TLexResult;
  curIndent : integer;
  sameLine : boolean;
begin
  Result := false;
  aError := '';
  lexState.SetXMLData(aXMLData);
  lexResult.Clear;
  if not yylex(lexState, lexResult) then
  begin
    aError:= lexResult.Error;
    exit;
  end;

  curIndent:= -1;
  sameLine := false;

  if lexResult.Token = txProlog then
  begin
    WriteProlog('<%' + lexResult.Prolog + '%>');
    lexResult.Clear;
    if not yylex(lexState, lexResult) then
    begin
      aError := lexResult.Error;
      exit;
    end;
  end;

  while lexResult.Token <> txEOF do
  begin
    if lexResult.Token = txProcessingInstruction then
    begin
      WriteProcessingInstruction('<%' + lexResult.ProcessingInstruction + '%>');
      sameLine := false;
    end
    else if lexResult.Token = txTag then
    begin
      inc(curIndent);
      WriteTag('<' + lexResult.Tag + '>', curIndent);
      sameLine := true;
    end
    else if lexResult.Token = txClosingTag then
    begin
      if sameLine then
        WriteClosingTag('</' + lexResult.Tag + '>')
      else
        WriteClosingTag('</' + lexResult.Tag + '>', curIndent);
      dec(curIndent);
      sameLine := false;
    end
    else if lexResult.Token = txSelfClosedTag then
    begin
      inc(curIndent);
      WriteTag('<' + lexResult.Tag + ' />', curIndent);
      dec(curIndent);
      sameLine := false;
    end
    else if lexResult.Token = txValue then
      WriteValue(lexResult.Value)
    else if lexResult.Token = txComment then
    begin
      if curIndent < 0 then
        inc(curIndent);
      WriteComment('<!-- ' + lexResult.Comment + ' -->', curIndent);
      sameLine := false;
    end
    else if lexResult.Token = txCDATA then
    begin
      inc(curIndent);
      WriteCDATA('<![CDATA[' + lexResult.CDATA + ']]>', curIndent);
      dec(curIndent);
      sameLine := false;
    end;
    lexResult.Clear;
    if not yylex(lexState, lexResult) then
    begin
      aError := lexResult.Error;
      exit;
    end;
  end;
  Result := true;
end;

{ TLexResult }

procedure TLexResult.Clear;
begin
  Token := txUndefined;
  Prolog := '';
  ProcessingInstruction := '';
  Tag := '';
  Value := '';
  Comment := '';
  CDATA := '';
  Error := '';
end;

procedure TLexResult.ComposeError(const aError: String; const aLexState: TLexState);
begin
  Error := aError + ' at char ' + IntToStr(aLexState.CharIndex) + ' ( ' + aLexState.LookAhead(30) + ' )';
end;

{ TLexState }

function TLexState.IsEof: boolean;
begin
  Result := (FCharIndex > FLenXMLData);
end;

procedure TLexState.SetXMLData(const aXMLData: string);
begin
  FXMLData := aXMLData;
  FLenXMLData := Length(FXMLData);
  FCharIndex := 1;
  InQuotes := false;
end;

function TLexState.GetXMLData: string;
begin
  Result := FXMLData;
end;

function TLexState.GetCurrentChar: Char;
begin
  if not IsEof then
    Result := FXMLData[FCharIndex]
  else
    raise Exception.Create('Outside the range of XMLData in TLexState');
end;

function TLexState.LookAhead(const aNumOfChars: integer): String;
begin
  Result := Copy(FXMLData, FCharIndex, aNumOfChars);
end;

procedure TLexState.Advance;
begin
  inc(FCharIndex);
end;

procedure TLexState.AdvanceBy(const aNumOfChars: integer);
begin
  FCharIndex := FCharIndex + aNumOfChars;
end;

procedure TLexState.GoBack;
begin
  dec(FCharIndex);
end;

function TLexState.CharIndex: integer;
begin
  Result := FCharIndex;
end;

end.
