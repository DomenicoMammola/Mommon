// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mNamingConventions;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

type
  TmNamingConvention = (ncSnakeCase, ncUpperSnakeCase, ncCamelCase, ncPascalCase, ncKebabCase);
  // snake case: tiny_happy_field
  // upper snake case : TINY_HAPPY_FIELD
  // camel case : tinyHappyField
  // pascal case : TinyHappyField
  // kebab case : tiny-happy-field


function ConvertNamingConvention (const aOriginalName: String; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention): String;

implementation

uses
  SysUtils;

function CutOffHeadAndTail(const aOriginalName : String; const aSeparator : Char; out aHead, aTail : String): String;
var
  len, bodyStart, bodyEnd : integer;
begin
  aHead:= '';
  aTail:= '';
  Result := '';

  bodyStart:= 1;
  len:= Length(aOriginalName);
  bodyEnd:= len;

  while bodyStart <= len do
  begin
    if aOriginalName[bodyStart] = aSeparator then
    begin
      aHead := aHead + aSeparator;
      inc(bodyStart);
    end
    else
      break;
  end;

  while bodyEnd > bodyStart do
  begin
    if aOriginalName[bodyEnd] = aSeparator then
    begin
      aTail:= aSeparator + aTail;
      dec(bodyEnd);
    end
    else
      break;
  end;

  if (bodyStart > 1) or (bodyEnd < len) then
    Result := Copy(aOriginalName, bodyStart, bodyEnd - bodyStart + 1)
  else
    Result := aOriginalName;
end;

function ConvertNamingConvention (const aOriginalName: String; const aSourceNamingConvention, aDestinationNamingConvention : TmNamingConvention): String;
var
  i, len : integer;
  doConvert : boolean;
  isUppercase : boolean;
  tail, head, body, sep : String;
begin
  Result := aOriginalName;
  if aSourceNamingConvention = aDestinationNamingConvention then
    exit;

  if aOriginalName = '' then
    exit;

  head := '';
  tail := '';

  if (aSourceNamingConvention = ncSnakeCase) or (aSourceNamingConvention = ncUpperSnakeCase) or (aSourceNamingConvention = ncKebabCase) then
  begin
    if aDestinationNamingConvention = ncUpperSnakeCase then
    begin
      if aSourceNamingConvention = ncKebabCase then
        Result := Uppercase(StringReplace(aOriginalName, '-', '_', [rfReplaceAll]))
      else
        Result := UpperCase(aOriginalName)
    end
    else if aDestinationNamingConvention = ncSnakeCase then
    begin
      if aSourceNamingConvention = ncKebabCase then
        Result := LowerCase(StringReplace(aOriginalName, '-', '_', [rfReplaceAll]))
      else
        Result := LowerCase(aOriginalName)
    end
    else if aDestinationNamingConvention = ncKebabCase then
    begin
      body := CutOffHeadAndTail(aOriginalName, '_', head, tail);
      Result := head + Lowercase(StringReplace(body ,'_', '-', [rfReplaceAll])) + tail;
    end
    else
    begin
      body := CutOffHeadAndTail(aOriginalName, '_', head, tail);
      Result := '';
      len := Length(body);
      doConvert:= false;
      if aSourceNamingConvention = ncKebabCase then
        sep := '-'
      else
        sep := '_';
      for i := 1 to len do
      begin
        if body[i] = sep then
        begin
          doConvert:= true;
        end
        else
        begin
          if Result = '' then
          begin
            if aDestinationNamingConvention = ncCamelCase then
              Result := Result + LowerCase(body[i])
            else
              Result := Result + UpperCase(body[i]);
          end
          else if doConvert then
            Result := Result + UpperCase(body[i])
          else
            Result := Result + LowerCase(body[i]);
          doConvert:= false;
        end;
      end;
      Result := head + Result + tail;
    end;
  end
  else if (aSourceNamingConvention = ncCamelCase) or (aSourceNamingConvention = ncPascalCase) then
  begin
    body := CutOffHeadAndTail(aOriginalName, '_', head, tail);

    len := Length(body);
    if aDestinationNamingConvention = ncPascalCase then
      Result := head + UpperCase(body[1]) + Copy(body, 2, len) + tail
    else if aDestinationNamingConvention = ncCamelCase then
      Result := head + LowerCase(body[1]) + Copy(body, 2, len) + tail
    else
    begin
      if aDestinationNamingConvention = ncKebabCase then
        sep := '-'
      else
        sep := '_';
      Result := body[1];
      for i := 2 to len do
      begin
        if (Uppercase(body[i]) = body[i]) and (body[i] in ['a'..'z', 'A'..'Z']) then
          Result := Result + sep + body[i]
        else
          Result := Result + body[i];
      end;
      if aDestinationNamingConvention = ncUpperSnakeCase then
        Result := head + UpperCase(Result) + tail
      else
        Result := head + LowerCase(Result) + tail;
    end;
  end;
end;


end.
