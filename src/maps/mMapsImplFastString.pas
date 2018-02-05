// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mMapsImplFastString;

interface

uses
  mMapsImpl, StrHashMap;

type

  { TmStringDictionaryImplFast }

  TmStringDictionaryImplFast = class(TmStringDictionaryImpl)
  strict private
    FInternalDictionary : TStringHashMap;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure _Add(const aStringKey : String; aObject : TObject); override;
    procedure _Clear; override;
    function _Find(const aStringKey : String) : TObject; override;
    function _Contains(const aStringKey: String): boolean; override;
    function _Count: integer; override;
    procedure _Remove(const aStringKey : String); override;
  end;

implementation

{ TmStringDictionaryImplFast }

constructor TmStringDictionaryImplFast.Create;
begin
  FInternalDictionary := TStringHashMap.Create;
end;

destructor TmStringDictionaryImplFast.Destroy;
begin
  FInternalDictionary.Free;
  inherited Destroy;
end;

procedure TmStringDictionaryImplFast._Add(const aStringKey: String; aObject: TObject);
begin
  FInternalDictionary.Add(aStringKey, aObject);
end;

procedure TmStringDictionaryImplFast._Clear;
begin
  FInternalDictionary.Clear;
end;

function TmStringDictionaryImplFast._Find(const aStringKey: String): TObject;
var
  tmp : pointer;
begin
  if FInternalDictionary.Find(aStringKey, tmp) then
    Result := TObject(tmp)
  else
    Result := nil;
end;

function TmStringDictionaryImplFast._Contains(const aStringKey: String): boolean;
begin
  Result:= FInternalDictionary.Contains(aStringKey);
end;

function TmStringDictionaryImplFast._Count: integer;
begin
  Result := FInternalDictionary.Count;
end;

procedure TmStringDictionaryImplFast._Remove(const aStringKey: String);
begin
  FInternalDictionary.Remove(aStringKey);
end;

end.
