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
  TFilterIntFunc = function (a: Integer) : boolean;

  TReducableStrFunc = function (a, b : String) : String;
  TMapableStrFunc = function (a : String) : String;
  TFilterStrFunc = function (a: String) : boolean;

  TReducableObjFunc = function (a, b : TObject) : TObject;
  TMapableObjFunc = function (a : TObject) : TObject;
  TFilterObjFunc = function (a: TObject) : boolean;

function IntMap(fn : TMapableIntFunc; fnFilter: TFilterIntFunc; a: TIntegerArray) : TIntegerArray;
function IntReduce(fn : TReducableIntFunc; fnFilter: TFilterIntFunc; a : TIntegerArray; init : Integer): Integer;

function StrMap(fn : TMapableStrFunc; fnFilter: TFilterStrFunc; a : TStringArray) : TStringArray;
function StrReduce(fn : TReducableStrFunc; fnFilter: TFilterStrFunc; a : TStringArray; init : string) : string;

function ObjMap(fn : TMapableObjFunc; fnFilter: TFilterObjFunc; a : TObjectArray) : TObjectArray;
function ObjReduce(fn : TReducableObjFunc; fnFilter: TFilterObjFunc; a : TObjectArray; init : TObject) : TObject;

(*
function JoinStrFunc(a, b : String) : String;
begin
 Result := a + b;
end;

function joinStrings(a : TStringArray) : String;
begin
  Result := StrReduce( JoinStrFunc, nil, a, "" );
end;

function SumIntFunc(a, b : Integer) : Integer;
begin
 Result := a + b;
end;

function sumInts(a : TIntegerArray) : Integer;
begin
    Result :=  IntReduce( SumIntFunc, nil, a, 0 );
end;
*)

implementation

function IntReduce(fn : TReducableIntFunc; fnFilter: TFilterIntFunc; a : TIntegerArray; init : Integer): Integer;
var
  s, i : Integer;
begin
  s := init;
  for i := 0 to Length(a)-1 do
  begin
    if Assigned(fnFilter) and (not fnFilter(a[i])) then 
      continue;
    s := fn( s, a[i] );
  end;
  Result := s;
end;

function IntMap(fn : TMapableIntFunc; fnFilter: TFilterIntFunc; a: TIntegerArray) : TIntegerArray;
var
  i, count : Integer;
begin
  SetLength(Result, Length(a));
  count := 0;
  for i := 0 to Length(a) - 1 do
  begin
    if Assigned(fnFilter) and (not fnFilter(a[i])) then
      continue;
    Result[count] := fn(a[i]);
    Inc(count);
  end;
  SetLength(Result, count); // shrink to actual survivor count
end;

function StrReduce(fn : TReducableStrFunc; fnFilter: TFilterStrFunc; a : TStringArray; init : string) : string;
var
  i : Integer;
  s : String;
begin
  s := init;
  for i := 0 to Length(a)-1 do
  begin
    if Assigned(fnFilter) and (not fnFilter(a[i])) then 
      continue;
    s := fn( s, a[i] );
  end;
  Result := s;
end;

function StrMap(fn : TMapableStrFunc; fnFilter: TFilterStrFunc; a : TStringArray): TStringArray;
var
  i, count : Integer;
begin
  SetLength(Result, Length(a));
  count := 0;
  for i := 0 to Length(a) - 1 do
  begin
    if Assigned(fnFilter) and (not fnFilter(a[i])) then
      continue;
    Result[count] := fn(a[i]);
    Inc(count);
  end;
  SetLength(Result, count); // shrink to actual survivor count
end;

function ObjMap(fn : TMapableObjFunc; fnFilter: TFilterObjFunc; a : TObjectArray): TObjectArray;
var
  i, count : Integer;
begin
  SetLength(Result, Length(a));
  count := 0;
  for i := 0 to Length(a) - 1 do
  begin
    if Assigned(fnFilter) and (not fnFilter(a[i])) then
      continue;
    Result[count] := fn(a[i]);
    Inc(count);
  end;
  SetLength(Result, count); // shrink to actual survivor count
end;

function ObjReduce(fn : TReducableObjFunc; fnFilter: TFilterObjFunc; a : TObjectArray; init : TObject) : TObject;
var
  i : Integer;
  s : TObject;
begin
  s := init;
  for i := 0 to Length(a)-1 do
  begin
    if Assigned(fnFilter) and (not fnFilter(a[i])) then 
      continue;
    s := fn( s, a[i] );
  end;
  Result := s;
end;

end.
