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
  { TMustacheLexState }

  TMustacheLexState = class
  strict private
    FText : string;
    FCharIndex : integer;
    FLenRow : integer;
    FOpenTokens : integer;
    FLastTokenString : String;
  public
    procedure SetText (const aText : String);
    procedure Advance;
    procedure GoBack;
    function Eof : boolean;
    function CurrentChar: Char;
    property OpenTokens : integer read FOpenTokens write FOpenTokens;
    property LastTokenString : String read FLastTokenString write FLastTokenString;
  end;



  { TmMicroMustache }

  TmMicroMustache = class
  strict private
    FValues : TmStringDictionary;
    FTemplate : TStringList;
    procedure yylex (aState : TMustacheLexState; var aValue : String);
    function CalcToken (const aToken : String) : String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddValue (const aKey, aValue : string);
    function Render : String;

    property Template : TStringList read FTemplate;
  end;

implementation

uses
  mBaseClassesAsObjects;


{ TMustacheLexState }

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
  if aState.Eof then
  begin
    if aState.OpenTokens > 0 then
    begin
      aValue := aValue + aState.LastTokenString;
      aState.LastTokenString:= '';
    end;

    exit;
  end;

  if aState.CurrentChar = '{' then
  begin
    aState.Advance;
    if aState.Eof or (aState.CurrentChar <> '{') then
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
  else if (aState.CurrentChar = '}') then
  begin
    aState.Advance;
    if aState.Eof or (aState.CurrentChar <> '}') then
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
  yylex(aState, aValue);
end;

function TmMicroMustache.CalcToken(const aToken: String) : string;
var
  state : TMustacheLexState;
  lc : String;
begin
  state := TMustacheLexState.Create;
  try
    state.SetText(aToken);
    yylex(state, Result);
    lc := lowercase(Result);
    if FValues.Contains(lc) then
      Result := (FValues.Find(lc) as TStringObject).Value
    else
      Result := aToken;
  finally
    state.Free;
  end;
end;

constructor TmMicroMustache.Create;
begin
  FValues := TmStringDictionary.Create(true);
  FTemplate := TStringList.Create;
end;

destructor TmMicroMustache.Destroy;
begin
  FValues.Free;
  FTemplate.Free;
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

function TmMicroMustache.Render: String;
var
  lexState : TMustacheLexState;
begin
  Result := '';
  lexState := TMustacheLexState.Create;
  try
    lexState.SetText(FTemplate.Text);
    yylex(lexState, Result);
  finally
    lexState.Free;
  end;
end;

end.
