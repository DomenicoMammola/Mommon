// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mMaps;

interface

uses
  contnrs,
  mMapsImpl;

type

  { TmBaseDictionary }

  TmBaseDictionary = class abstract
  strict private
    FGarbage : TObjectList;
  protected
    FOwnsObjects : boolean;
    procedure InternalAdd (const aObject : TObject);
    procedure InternalRemove (aObject : TObject);
    procedure InternalClear;
  public
    constructor Create(const aOwnsObjects : boolean = false); virtual;
    destructor Destroy; override;
  end;


  { TmStringDictionary }

  TmStringDictionary = class (TmBaseDictionary)
    strict private
      FImpl : TmStringDictionaryImpl;
    public
      constructor Create(const aOwnsObjects : boolean = false); override;
      destructor Destroy; override;

      procedure Add(const aStringKey: String; aObject: TObject);
      function Find(const aStringKey: String): TObject;
      function Contains(const aStringKey: String): boolean;
      procedure Remove (const aStringKey: String);
      procedure Clear;
      function Count : integer;
  end;

  { TmIntegerDictionary }

  TmIntegerDictionary = class (TmBaseDictionary)
    strict private
      FImpl : TmIntegerDictionaryImpl;
    public
      constructor Create(const aOwnsObjects : boolean = false); override;
      destructor Destroy; override;

      procedure Add(const aIntegerKey : integer; aObject: TObject);
      function Find(const aIntegerKey : integer) : TObject;
      procedure Remove (const aIntegerKey : integer);
      procedure Clear;
      function Count : integer;
  end;

implementation

uses
  sysutils;

{ TmBaseDictionary }

procedure TmBaseDictionary.InternalAdd(const aObject: TObject);
begin
  if FOwnsObjects and Assigned(aObject) then
    FGarbage.Add(aObject);
end;

procedure TmBaseDictionary.InternalRemove(aObject: TObject);
begin
  if FOwnsObjects and Assigned(aObject) then
    FGarbage.Remove(aObject);
end;

procedure TmBaseDictionary.InternalClear;
begin
  if FOwnsObjects then
    FGarbage.Clear;
end;

constructor TmBaseDictionary.Create(const aOwnsObjects: boolean);
begin
  FOwnsObjects:= aOwnsObjects;
  if FOwnsObjects then
    FGarbage := TObjectList.Create(true);
end;

destructor TmBaseDictionary.Destroy;
begin
  FreeAndNil(FGarbage);
  inherited Destroy;
end;

{ TmIntegerDictionary }

constructor TmIntegerDictionary.Create(const aOwnsObjects : boolean = false);
begin
  inherited;
  FImpl := CreateTmIntegerDictionary;
end;

destructor TmIntegerDictionary.Destroy;
begin
  FImpl.Free;
  inherited Destroy;
end;

procedure TmIntegerDictionary.Add(const aIntegerKey: integer; aObject: TObject);
begin
  FImpl._Add(aIntegerKey, aObject);
  InternalAdd(aObject);
end;

function TmIntegerDictionary.Find(const aIntegerKey: integer): TObject;
begin
  Result := FImpl._Find(aIntegerKey);
end;

procedure TmIntegerDictionary.Remove(const aIntegerKey: integer);
var
  tmpObj : TObject;
begin
  tmpObj := Self.Find(aIntegerKey);
  FImpl._Remove(aIntegerKey);
  InternalRemove(tmpObj);
end;

procedure TmIntegerDictionary.Clear;
begin
  FImpl._Clear;
  InternalClear;
end;

function TmIntegerDictionary.Count: integer;
begin
  Result := FImpl._Count;
end;

{ TmStringDictionary }

constructor TmStringDictionary.Create(const aOwnsObjects : boolean = false);
begin
  Inherited;
  FImpl := CreateTmStringDictionary;
end;

destructor TmStringDictionary.Destroy;
begin
  FImpl.Free;
  inherited Destroy;
end;

procedure TmStringDictionary.Add(const aStringKey: String; aObject: TObject);
begin
  FImpl._Add(aStringKey, aObject);
  InternalAdd(aObject);
end;

function TmStringDictionary.Find(const aStringKey: String): TObject;
begin
  Result:= FImpl._Find(aStringKey);
end;

function TmStringDictionary.Contains(const aStringKey: String): boolean;
begin
  Result:= FImpl._Contains(aStringKey);
end;

procedure TmStringDictionary.Remove(const aStringKey: String);
var
  tmpObj : TObject;
begin
  tmpObj := Self.Find(aStringKey);
  FImpl._Remove(aStringKey);
  InternalRemove(tmpObj);
end;

procedure TmStringDictionary.Clear;
begin
  FImpl._Clear;
  InternalClear;
end;

function TmStringDictionary.Count: integer;
begin
  Result := FImpl._Count;
end;

end.
