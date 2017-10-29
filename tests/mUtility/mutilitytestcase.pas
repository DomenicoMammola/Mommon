unit mUtilityTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  mUtility;

type

  { TTestCaseUtility }

  TTestCaseUtility= class(TTestCase)
  published
    procedure TestAddRemoveZeros;
    procedure TestGetCPUCores;
  end;

implementation

procedure TTestCaseUtility.TestAddRemoveZeros;
begin
  CheckEquals('12002', RemoveZerosFromFront('0012002'));
  CheckEquals('', RemoveZerosFromFront('0'));
  CheckEquals('', RemoveZerosFromFront('000000000'));
  CheckEquals('5', RemoveZerosFromFront('5'));
  CheckEquals('50', RemoveZerosFromFront('50'));
  CheckEquals('50', RemoveZerosFromFront('050'));

  CheckEquals('012', AddZerosFront(12, 3));
  CheckEquals('12', AddZerosFront(12, 2));
  CheckEquals('12', AddZerosFront(12, 1));
  CheckEquals('0012', AddZerosFront(12, 4));
end;

procedure TTestCaseUtility.TestGetCPUCores;
begin
  CheckTrue(GetCPUCores > 0);
end;



initialization

  RegisterTest(TTestCaseUtility);
end.

