unit NullablesTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  mNullables;

type

  TNullablesTestCase= class(TTestCase)
  published
    procedure TestDouble;
  end;

implementation

procedure TNullablesTestCase.TestDouble;
var
  value : TNullableDouble;
begin
  value := TNullableDouble.Create();
  try
    value.FractionalPartDigits:= 2;
    value.Assign(75.123);
    CheckEquals(75.12, value.AsFloat);
    CheckEquals('75', value.AsFormattedString('0'));
    CheckEquals('75', value.AsFormattedString('#'));
  finally
    value.Free;
  end;
end;



initialization

  RegisterTest(TNullablesTestCase);
end.

