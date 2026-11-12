unit TestCase1;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs, mMapReduce

  {$IFNDEF FPC}, IOUtils, TestFramework
  {$ELSE}
  ,fpcunit, testutils, testregistry
  {$ENDIF};

type

  { TTestCase1 }

  TTestCase1= class(TTestCase)
  published
    procedure TestAdd;
  end;

implementation

function DoubleMySelf(a : Integer) : Integer;
begin
  Result := a * 2;
end;

procedure TTestCase1.TestAdd;
var
  i : integer;
  a, b : TmrIntegerArray;
begin
  SetLength(a, 1000);
  for i := 0 to 999 do
  begin
    a[i] := i;
  end;
  b := IntMap(@DoubleMySelf, nil, a);
  for i := 0 to 999 do
  begin
    CheckEquals(b[i], i * 2);
  end;
  SetLength(a, 0);
  Finalize(a);
end;



initialization
{$IFDEF FPC}
  RegisterTest(TTestCase1);
{$ELSE}
  RegisterTest(TTestCase1.Suite);
{$ENDIF}
end.

