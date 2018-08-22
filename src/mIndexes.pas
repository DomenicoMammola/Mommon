// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mIndexes;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mMaps, mIntList;

type

  { TmIndexStringKeyIntValues }

  TmIndexStringKeyIntValues = class
  strict private
    FIndex : TmStringDictionary;
    FKeys : TStringList;
    function GetKeysCount: integer;
    function GetKey(i : integer): String;
    function GetValue(i: integer; k : integer): integer;
    function GetValuesCount(i: integer): integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddValue (const aKey: string; const aValue: integer);

    property KeysCount : integer read GetKeysCount;
    property Keys[i : integer]: String read GetKey;
    property ValuesCount[i: integer] : integer read GetValuesCount;
    property Values[i: integer; k : integer]: integer read GetValue;
  end;


implementation

{ TmIndexStringKeyIntValues }

function TmIndexStringKeyIntValues.GetKey(i : integer): String;
begin
  Result := FKeys.Strings[i];
end;

function TmIndexStringKeyIntValues.GetValue(i: integer; k : integer): integer;
begin
  Result := (FKeys.Objects[i] as TIntegerList).Items[k];
end;

function TmIndexStringKeyIntValues.GetValuesCount(i: integer): integer;
begin
  Result := (FKeys.Objects[i] as TIntegerList).Count;
end;

function TmIndexStringKeyIntValues.GetKeysCount: integer;
begin
  Result := FKeys.Count;
end;

constructor TmIndexStringKeyIntValues.Create;
begin
  FIndex := TmStringDictionary.Create(true);
  FKeys := TStringList.Create;
end;

destructor TmIndexStringKeyIntValues.Destroy;
begin
  FKeys.Free;
  FIndex.Free;
  inherited Destroy;
end;

procedure TmIndexStringKeyIntValues.Clear;
begin
  FKeys.Clear;
  FIndex.Clear;
end;

procedure TmIndexStringKeyIntValues.AddValue(const aKey: string; const aValue: integer);
var
  tmpList : TIntegerList;
begin
  if FIndex.Contains(aKey) then
    tmpList := FIndex.Find(aKey) as TIntegerList
  else
  begin
    tmpList := TIntegerList.Create;
    FIndex.Add(aKey, tmpList);
    FKeys.AddObject(aKey, tmpList);
  end;
  tmpList.Add(aValue);
end;

end.
