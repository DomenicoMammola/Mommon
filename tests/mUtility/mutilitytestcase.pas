unit mUtilityTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  mUtility, mISO6346Utility;

type

  { TTestCaseUtility }

  TTestCaseUtility= class(TTestCase)
  published
    procedure TestAddRemoveZeros;
    procedure TestGetCPUCores;
    procedure TestISO6346;
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

procedure TTestCaseUtility.TestISO6346;
var
  error : string;
begin
  CheckTrue(IsContainerCodeValid('CSQU3054383', error));
  CheckFalse(IsContainerCodeValid('CSQU3054381', error));
  CheckFalse(IsContainerCodeValid('CSQU3054382', error));
  CheckFalse(IsContainerCodeValid('CSQU3054384', error));
  CheckFalse(IsContainerCodeValid('CSQU3054385', error));
  CheckFalse(IsContainerCodeValid('CSQU3054386', error));
  CheckFalse(IsContainerCodeValid('CSQU3054387', error));
  CheckFalse(IsContainerCodeValid('CSQU3054388', error));
  CheckFalse(IsContainerCodeValid('CSQU3054389', error));
  CheckFalse(IsContainerCodeValid('', error));
  CheckFalse(IsContainerCodeValid('12345678901', error));
  CheckFalse(IsContainerCodeValid('CSQK3054383', error));
  CheckFalse(IsContainerCodeValid('CSU3054383', error));
  CheckFalse(IsContainerCodeValid('CS2U3054383', error));
  CheckFalse(IsContainerCodeValid('CSQU30A4383', error));
end;



initialization

  RegisterTest(TTestCaseUtility);
end.

