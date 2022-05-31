unit mXMLToPdf;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes;

type

  { TXMLExporter }

  TXMLExporter = class abstract
  strict private
  protected
    procedure WriteTag(const aText : String; const aIndentation : integer); virtual; abstract;
    procedure WriteClosingTag(const aText : String; const aIndentation : integer); virtual; abstract;
    procedure WriteValue(const aText : String; const aIndentation : integer); virtual; abstract;
    procedure WriteProlog(const aText : String); virtual; abstract;
    procedure WriteComment(const aText: String; const aIndentation : integer); virtual; abstract;
    procedure WriteCDATA(const aText: String; const aIndentation : integer); virtual; abstract;

    procedure PrintXML(const aXMLData : String);
  end;

  { TXMLToTxtHelper }

  TXMLToTxtHelper = class (TXMLExporter)
  strict private
    FCurrentText : TStringList;
    FFileName : String;
    FLastIndentation : integer;
    FIndentationChar : Char;
  protected
    procedure WriteTag(const aText : String; const aIndentation : integer); override;
    procedure WriteClosingTag(const aText : String; const aIndentation : integer); override;
    procedure WriteValue(const aText : String; const aIndentation : integer); override;
    procedure WriteProlog(const aText : String); override;
    procedure WriteComment(const aText: String; const aIndentation : integer); override;
    procedure WriteCDATA(const aText: String; const aIndentation : integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    class procedure XMLToTxt(const aXMLData : String; const aFileName : String);
  end;

implementation

uses
  SysUtils,
  fpreadjpeg,
  fppdf,
  fpparsettf;


type

  TXMLToken = (txUndefined, txEOF, txProlog, txComment, txTag, txClosingTag, txSelfClosedTag, txValue, txCDATA);

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
    procedure Advance;
    procedure GoBack;
    function CharIndex : integer;
  end;

  { TLexResult }

  TLexResult = record
  public
    Token : TXMLToken;
    Prolog : String;
    Tag : String;
    Value : String;
    Comment : String;
    CDATA : String;

    procedure Clear;
  end;

procedure RaiseError(const aError : String; const aLexState : TLexState);
var
  tmp : String;
begin
  tmp := aError + ' at char ' + IntToStr(aLexState.CharIndex);
  tmp := tmp + ' ( ' + Copy(aLexState.GetXMLData, aLexState.CharIndex, 30) + ' )';
  raise Exception.Create(tmp);
end;

procedure CalcProlog(var aLexState : TLexState; out aLexResult : TLexResult);
var
  currentProlog : String;
begin
  aLexState.Advance;
  currentProlog := '';
  while true do
  begin
    if aLexState.IsEof then
      RaiseError('Got EOF inside the header tag', aLexState);
    if aLexState.GetCurrentChar ='?' then
    begin
      aLexState.Advance;
      if aLexState.IsEof then
        RaiseError('Got EOF inside the header tag', aLexState);
      if aLexState.GetCurrentChar = '>' then
      begin
        if currentProlog = '' then
          RaiseError('Empty header', aLexState);
        aLexResult.Token:= txProlog;
        aLexResult.Prolog := currentProlog;
        aLexState.Advance;
        exit;
      end
      else
        RaiseError('Unclosed header', aLexState);
    end
    else
    begin
      if aLexState.GetCurrentChar = '<' then
        RaiseError('Malformed prolog', aLexState);
      currentProlog:= currentProlog + aLexState.GetCurrentChar;
      aLexState.Advance;
    end;
  end;
end;

procedure CalcClosingTag(var aLexState : TLexState; out aLexResult : TLexResult);
var
  currentTag : String;
begin
  aLexState.Advance;
  currentTag:= '';
  while true do
  begin
    if aLexState.IsEof then
      RaiseError('Got EOF inside a closing tag', aLexState);
    if aLexState.GetCurrentChar ='>' then
    begin
      if currentTag = '' then
        RaiseError('Empty closing tag', aLexState);
      aLexResult.Token:= txClosingTag;
      aLexResult.Tag := currentTag;
      aLexState.Advance;
      exit;
    end
    else
    begin
      if aLexState.GetCurrentChar = '<' then
        RaiseError('Malformed closing tag', aLexState);
      currentTag:= currentTag + aLexState.GetCurrentChar;
      aLexState.Advance;
    end;
  end;
end;

procedure CalcCommentOrCDATA(var aLexState: TLexState; out aLexResult : TLexResult);
var
  currentComment, CDATA : String;
   i : integer;
begin
  aLexState.Advance;
  if aLexState.IsEof then
    RaiseError('Got EOF inside a comment or CDATA or document type', aLexState);
  if aLexState.GetCurrentChar = '-' then
  begin
    aLexState.Advance;
    if aLexState.IsEof then
      RaiseError('Got EOF inside a comment', aLexState);
    if aLexState.GetCurrentChar = '-' then
    begin
      currentComment := '';
      while true do
      begin
        aLexState.Advance;
        if aLexState.IsEof then
          RaiseError('Got EOF inside a comment', aLexState);
        currentComment := currentComment + aLexState.GetCurrentChar;
        if RightStr(currentComment, 2) = '--' then
        begin
          aLexState.Advance;
          if aLexState.IsEof then
            RaiseError('Got EOF inside a comment', aLexState);
          // For compatibility, the string " -- " (double-hyphen) MUST NOT occur within comments.] Parameter entity references MUST NOT be recognized within comments.
          // https://www.w3.org/TR/xml/#sec-comments
          if aLexState.GetCurrentChar = '>' then
          begin
            if Length(currentComment) = 2 then
              RaiseError('Malformed comment', aLexState);
            aLexResult.Token:= txComment;
            currentComment := Copy(currentComment, 1, Length(currentComment) - 2);
            aLexResult.Comment:= currentComment;
            aLexState.Advance;
            exit;
          end
          else
            RaiseError('Malformed comment', aLexState);
        end;
      end;
    end
    else
      RaiseError('Malformed comment', aLexState);
  end
  else if aLexState.GetCurrentChar = '[' then
  begin
    // [CDATA[
    CDATA := aLexState.GetCurrentChar;
    for i := 1 to 6 do
    begin
      aLexState.Advance;
      if aLexState.IsEof then
        RaiseError('Got EOF inside CDATA', aLexState);
      CDATA := CDATA + aLexState.GetCurrentChar;
    end;
    if CDATA <> '[CDATA[' then
      RaiseError('Malformed CDATA', aLexState);
    CDATA := '';
    while true do
    begin
      aLexState.Advance;
      if aLexState.IsEof then
        RaiseError('Got EOF inside CDATA', aLexState);
      CDATA := CDATA + aLexState.GetCurrentChar;
      if RightStr(CDATA, 2) = ']]' then
      begin
        aLexState.Advance;
        if aLexState.IsEof then
          RaiseError('Got EOF inside a comment', aLexState);
        if aLexState.GetCurrentChar = '>' then
        begin
          if Length(CDATA) = 2 then
            RaiseError('Malformed CDATA', aLexState);
          aLexResult.Token:= txCDATA;
          currentComment := Copy(CDATA, 1, Length(CDATA) - 2);
          aLexResult.CDATA:= CDATA;
          aLexState.Advance;
          exit;
        end
        else
          RaiseError('Malformed CDATA', aLexState);
      end;
    end;
  end
  else
    RaiseError('Malformed comment or CDATA or document type', aLexState);
end;

procedure CalcGenericTag(var aLexState : TLexState; out aLexResult : TLexResult);
var
  currentTag : String;
begin
  aLexState.Advance;
  if aLexState.IsEof then
    RaiseError('Got EOF', aLexState);

  if aLexState.GetCurrentChar = '?' then
    CalcProlog(aLexState, aLexResult)
  else if aLexState.GetCurrentChar = '/' then
    CalcClosingTag(aLexState, aLexResult)
  else if aLexState.GetCurrentChar = '!' then
    CalcCommentOrCDATA(aLexState, aLexResult)
  else
  begin
    if aLexState.GetCurrentChar = '>' then
      RaiseError('Empty tag', aLexState);

    currentTag := aLexState.GetCurrentChar;
    aLexState.Advance;
    while true do
    begin
      if aLexState.IsEof then
        RaiseError('Got EOF inside a tag', aLexState);
      if (not aLexState.InQuotes) and (aLexState.GetCurrentChar = '>') then
      begin
        aLexResult.Token:= txTag;
        aLexResult.Tag:= currentTag;
        aLexState.Advance;
        exit;
      end
      else if (not aLexState.InQuotes) and (aLexState.GetCurrentChar = '/') then
      begin
        aLexState.Advance;
        if aLexState.IsEof then
          RaiseError('Got EOF inside a tag', aLexState);
        if aLexState.GetCurrentChar = '>' then
        begin
          aLexResult.Token:= txSelfClosedTag;
          aLexResult.Tag:= currentTag;
          aLexState.Advance;
          exit;
        end
        else
          raiseError('Malformed self closed tag', aLexState);
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
          RaiseError('Malformed tag', aLexState);
        currentTag:= currentTag + aLexState.GetCurrentChar;
        aLexState.Advance;
      end;
    end;
  end;
end;

procedure CalcValue(var aLexState: TLexState; out aLexResult : TLexResult);
var
  currentValue : String;
begin
  currentValue := aLexState.GetCurrentChar;
  aLexState.Advance;
  while true do
  begin
    if aLexState.IsEof then
      RaiseError('Got EOF inside a value', aLexState);
    if (aLexState.GetCurrentChar = '<') then
    begin
      aLexResult.Token:= txValue;
      aLexResult.Value:= currentValue;
      exit;
    end
    else if (aLexState.GetCurrentChar = '>') then
    begin
      RaiseError('Got > outside a tag', aLexState);
    end
    else
    begin
      currentValue := currentValue + aLexState.GetCurrentChar;
      aLexState.Advance;
    end;
  end;
end;

procedure yylex (var aLexState: TLexState; out aLexResult : TLexResult);
begin
  if aLexState.IsEof then
  begin
    aLexResult.Token := txEOF;
    exit;
  end;

  while (aLexState.GetCurrentChar = ' ') or (aLexState.GetCurrentChar = #10) or (aLexState.GetCurrentChar = #13) or (aLexState.GetCurrentChar = #9) do
  begin
    aLexState.Advance;

    if aLexState.IsEof then
    begin
      aLexResult.Token := txEOF;
      exit;
    end;
  end;

  if aLexState.GetCurrentChar = '<' then
    CalcGenericTag(aLexState, aLexResult)
  else if aLexState.GetCurrentChar = '>' then
    RaiseError('Got > outside a tag', aLexState)
  else
    CalcValue(aLexState, aLexResult);
end;

{ TXMLToTxtHelper }

procedure TXMLToTxtHelper.WriteTag(const aText: String; const aIndentation: integer);
var
  s : String;
  i : integer;
begin
  s := '';
  for i := 0 to aIndentation do
    s:= s + FIndentationChar;
  FCurrentText.Append(s + aText);
  FLastIndentation:= aIndentation;
end;

procedure TXMLToTxtHelper.WriteClosingTag(const aText: String; const aIndentation: integer);
begin
  if FLastIndentation = aIndentation then
    FCurrentText.Strings[FCurrentText.Count- 1] := FCurrentText.Strings[FCurrentText.Count- 1] + aText
  else
    WriteTag(aText, aIndentation);
end;

procedure TXMLToTxtHelper.WriteValue(const aText: String; const aIndentation: integer);
begin
  FCurrentText.Strings[FCurrentText.Count- 1] := FCurrentText.Strings[FCurrentText.Count- 1] + aText;
end;

procedure TXMLToTxtHelper.WriteProlog(const aText: String);
begin
  FCurrentText.Append(aText);
end;

procedure TXMLToTxtHelper.WriteComment(const aText: String; const aIndentation: integer);
var
  s : String;
  i : integer;
begin
  s := '';
  for i := 0 to aIndentation do
    s:= s + FIndentationChar;
  FCurrentText.Append(s + aText);
  FLastIndentation:= aIndentation;
end;

procedure TXMLToTxtHelper.WriteCDATA(const aText: String; const aIndentation: integer);
var
  s : String;
  i : integer;
begin
  s := '';
  for i := 0 to aIndentation do
    s:= s + FIndentationChar;
  FCurrentText.Append(s + aText);
  FLastIndentation:= aIndentation;
end;

constructor TXMLToTxtHelper.Create;
begin
  FFileName:= '';
  FCurrentText := TStringList.Create;
  FLastIndentation:= 0;
  FIndentationChar:= #9;
end;

destructor TXMLToTxtHelper.Destroy;
begin
  FCurrentText.Free;
  inherited Destroy;
end;

class procedure TXMLToTxtHelper.XMLToTxt(const aXMLData: String; const aFileName: String);
var
  s : TXMLToTxtHelper;
begin
  s := TXMLToTxtHelper.Create;
  try
    s.FFileName:= aFileName;
    s.PrintXML(aXMLData);
    s.FCurrentText.SaveToFile(aFileName);
  finally
    s.Free;
  end;
end;



{ TXMLExporter }

procedure TXMLExporter.PrintXML(const aXMLData: String);
var
  lexState : TLexState;
  lexResult : TLexResult;
  curIndent : integer;
  curTag, curClosingTag, curValue : String;
begin
  lexState.SetXMLData(aXMLData);
  lexResult.Clear;
  yylex(lexState, lexResult);
  if lexResult.Token = txProlog then
  begin
    WriteProlog('<%' + lexResult.Prolog + '%>');
    curIndent:= -1;
    while not lexState.IsEof do
    begin
      lexResult.Clear;
      yylex(lexState, lexResult);
      if lexResult.Token = txTag then
      begin
        inc(curIndent);
        WriteTag('<' + lexResult.Tag + '>', curIndent);
      end
      else if lexResult.Token = txClosingTag then
      begin
        WriteClosingTag('</' + lexResult.Tag + '>', curIndent);
        dec(curIndent);
      end
      else if lexResult.Token = txSelfClosedTag then
      begin
        inc(curIndent);
        WriteTag('<' + lexResult.Tag + ' />', curIndent);
        dec(curIndent);
      end
      else if lexResult.Token = txValue then
        WriteValue(lexResult.Value, curIndent)
      else if lexResult.Token = txComment then
        WriteComment('<!-- ' + lexResult.Comment + ' -->', curIndent)
      else if lexResult.Token = txCDATA then
        WriteCDATA('<![CDATA[' + lexResult.CDATA + ']]>', curIndent);
    end;
  end
  else
    RaiseError('Missing prolog', lexState);
end;

{ TLexResult }

procedure TLexResult.Clear;
begin
  Token := txUndefined;
  Prolog := '';
  Tag := '';
  Value := '';
  Comment := '';
  CDATA := '';
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
    RaiseError('Outside the range', Self);
end;

procedure TLexState.Advance;
begin
  inc(FCharIndex);
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
