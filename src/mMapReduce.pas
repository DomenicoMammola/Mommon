// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mMapReduce;

interface

type
  TIntegerArray = array of Integer;
  TStringArray = array of String;
  TObjectArray = array of TObject;

  TReducableIntFunc = function (a, b : Integer) : Integer;
  TMapableIntFunc = function (a : Integer) : Integer;

  TReducableStrFunc = function (a, b : String) : String;
  TMapableStrFunc = function (a : String) : String;

  TReducableObjFunc = function (a, b : TObject) : TObject;
  TMapableObjFunc = function (a : TObject) : TObject;


function IntReduce(fn : TReducableIntFunc; a : TIntegerArray; init : Integer): Integer;
procedure IntMap(fn : TMapableIntFunc; a: TIntegerArray);

procedure StrMap(fn : TMapableStrFunc; a : TStringArray);
function StrReduce(fn : TReducableStrFunc; a : TStringArray; init : string) : string;

procedure ObjMap(fn : TMapableObjFunc; a : TObjectArray);
function ObjReduce(fn : TReducableObjFunc; a : TObjectArray; init : TObject) : TObject;

(*
function sumInts(a : TIntegerArray) : Integer;
begin
    Result :=  IntReduce( SumIntFunc, a, 0 );
end;

function joinStrings(a : TStringArray) : String;
begin
  Result := StrReduce( JoinStrFunc, a, "" );
end;

function SumIntFunc(a, b : Integer) : Integer;
begin
 Result := a + b;
end;

function JoinStrFunc(a, b : String) : String;
begin
 Result := a + b;
end;
*)

implementation

function IntReduce(fn : TReducableIntFunc; a : TIntegerArray; init : Integer): Integer;
var
  s, i : Integer;
begin
  s := init;
  for i := 0 to Length(a)-1 do
    s := fn( s, a[i] );
  Result := s;
end;

procedure IntMap(fn : TMapableIntFunc; a: TIntegerArray);
var
  i : integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := fn(a[i]);
end;

function StrReduce(fn : TReducableStrFunc; a : TStringArray; init : string) : string;
var
  i : Integer;
  s : String;
begin
  s := init;
  for i := 0 to Length(a)-1 do
    s := fn( s, a[i] );
  Result := s;
end;

procedure StrMap(fn : TMapableStrFunc; a : TStringArray);
var
  i : integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := fn(a[i]);
end;

procedure ObjMap(fn : TMapableObjFunc; a : TObjectArray);
var
  i : integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := fn(a[i]);
end;

function ObjReduce(fn : TReducableObjFunc; a : TObjectArray; init : TObject) : TObject;
var
  i : Integer;
  s : TObject;
begin
  s := init;
  for i := 0 to Length(a)-1 do
    s := fn( s, a[i] );
  Result := s;
end;

end.