// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mMicroMustache;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Classes,
  mMaps;

type
  TMustacheChars = (mcCurly, mcSquare, mcRound, mcChevronsAngle);

  { TMustacheLexState }

  TMustacheLexState = class
  strict private
    FText : string;
    FCharIndex : Int64;
    FLenRow : Int64;
    FOpenTokens : Integer;
    FLastTokenString : String;
    FLeftTokenDelimiter : Char;
    FRightTokenDelimiter : Char;
  public
    constructor Create (const aMustacheChars : TMustacheChars);
    procedure SetText (const aText : String);
    procedure Advance;
    procedure GoBack;
    function Eof : boolean;
    function CurrentChar: Char;
    property OpenTokens : integer read FOpenTokens write FOpenTokens;
    property LastTokenString : String read FLastTokenString write FLastTokenString;
    property LeftTokenDelimiter : Char read FLeftTokenDelimiter;
    property RightTokenDelimiter : Char read FRightTokenDelimiter;
  end;



  { TmMicroMustache }

  TmMicroMustache = class
  strict private
    FValues : TmStringDictionary;
    FMustacheChars : TMustacheChars;
    procedure yylex (aState : TMustacheLexState; var aValue : String);
    function CalcToken (const aToken : String) : String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddValue (const aKey, aValue : string);
    function Render(aTemplate: String): String;

    property MustacheChars : TMustacheChars read FMustacheChars write FMustacheChars;
  end;

implementation

uses
  SysUtils,
  mBaseClassesAsObjects, mLog, htmlelements;

var
  logger : TmLog;

{ TMustacheLexState }

constructor TMustacheLexState.Create(const aMustacheChars: TMustacheChars);
begin
  case aMustacheChars of
    mcCurly :
      begin
        FLeftTokenDelimiter:= '{';
        FRightTokenDelimiter:= '}';
      end;
    mcSquare :
      begin
        FLeftTokenDelimiter:= '[';
        FRightTokenDelimiter:= ']';
      end;
    mcRound :
      begin
        FLeftTokenDelimiter:= '(';
        FRightTokenDelimiter:= ')';
      end;
    mcChevronsAngle :
      begin
        FLeftTokenDelimiter:= '<';
        FRightTokenDelimiter:= '>';
      end
    else
      raise Exception.Create('Unsupported mustache chars');
  end;
end;

procedure TMustacheLexState.SetText(const aText: String);
begin
  FText:= aText;
  FLenRow:= Length(FText);
  FCharIndex:= 1;
  FOpenTokens:= 0;
  FLastTokenString:= '';
end;

procedure TMustacheLexState.Advance;
begin
  inc (FCharIndex);
end;

procedure TMustacheLexState.GoBack;
begin
  dec (FCharIndex);
end;

function TMustacheLexState.Eof: boolean;
begin
  Result := FCharIndex > FLenRow;
end;

function TMustacheLexState.CurrentChar: Char;
begin
  Result := FText[FCharIndex];
end;


{ TmMicroMustache }

procedure TmMicroMustache.yylex(aState: TMustacheLexState; var aValue : String);
begin
  while not aState.Eof do
  begin
    if aState.CurrentChar = aState.LeftTokenDelimiter then
    begin
      aState.Advance;
      if aState.Eof or (aState.CurrentChar <> aState.LeftTokenDelimiter) then
      begin
        aState.GoBack;
        if aState.OpenTokens > 0 then
          aState.LastTokenString:= aState.LastTokenString + aState.CurrentChar
        else
          aValue := aValue + aState.CurrentChar;
      end
      else
        aState.OpenTokens:= aState.OpenTokens + 1;
    end
    else if (aState.CurrentChar = aState.RightTokenDelimiter) then
    begin
      aState.Advance;
      if aState.Eof or (aState.CurrentChar <> aState.RightTokenDelimiter) then
      begin
        aState.GoBack;
        if aState.OpenTokens > 0 then
          aState.LastTokenString:= aState.LastTokenString + aState.CurrentChar
        else
          aValue := aValue + aState.CurrentChar;
      end
      else
      begin
        aState.OpenTokens:= aState.OpenTokens - 1;
        if aState.OpenTokens = 0 then
        begin
          aValue := aValue + CalcToken(aState.LastTokenString);
          aState.LastTokenString:= '';
        end;
      end;
    end
    else
    begin
      if aState.OpenTokens > 0 then
        aState.LastTokenString:= aState.LastTokenString + aState.CurrentChar
      else
        aValue:= aValue + aState.CurrentChar;
    end;

    aState.Advance;
  end;

  if aState.Eof then
  begin
    if aState.OpenTokens > 0 then
    begin
      aValue := aValue + aState.LeftTokenDelimiter + aState.LeftTokenDelimiter + aState.LastTokenString;
      aState.LastTokenString:= '';
    end;
  end;
end;

function TmMicroMustache.CalcToken(const aToken: String) : string;
var
  state : TMustacheLexState;
  lc : String;
begin
  if aToken = 'miss' then
  begin
    Result := '';
    exit;
  end;

  state := TMustacheLexState.Create(FMustacheChars);
  try
    logger.Debug('CalcToken:' + aToken);
    state.SetText(aToken);
    Result := '';
    yylex(state, Result);
    lc := lowercase(Result);
    logger.Debug('Find token ' + lc);
    if FValues.Contains(lc) then
    begin
      // Result := EscapeHTML((FValues.Find(lc) as TStringObject).Value);
      Result := (FValues.Find(lc) as TStringObject).Value;
      logger.Debug('Token value: ' + Result);
    end
    else
      Result := aToken;
  finally
    state.Free;
  end;
end;

constructor TmMicroMustache.Create;
begin
  FValues := TmStringDictionary.Create(true);
  FMustacheChars:= mcCurly;
end;

destructor TmMicroMustache.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

procedure TmMicroMustache.AddValue(const aKey, aValue: string);
var
  l : string;
begin
  l := lowercase(aKey);
  if FValues.Contains(l) then
    FValues.Remove(l);
  FValues.Add(l, TStringObject.Create(aValue));
end;

function TmMicroMustache.Render (aTemplate: String): String;
var
  lexState : TMustacheLexState;
begin
  Result := '';
  lexState := TMustacheLexState.Create(FMustacheChars);
  try
    lexState.SetText(aTemplate);
    yylex(lexState, Result);
  finally
    lexState.Free;
  end;
end;

initialization

logger := logManager.AddLog('mustache');

end.

