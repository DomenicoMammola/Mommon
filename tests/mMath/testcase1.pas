unit TestCase1;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs, mMathUtility

  {$IFNDEF FPC}, IOUtils, TestFramework
  {$ELSE}
  ,fpcunit, testutils, testregistry
  {$ENDIF};

type

  { TTestCase1 }

  TTestCase1= class(TTestCase)
  strict private
    function UseDefaultThousandSeparator (const aValue : string; const myThousandSeparator : string = '.'): string;
  published
    procedure TestRounding;
    procedure TestGetFractionalPartDigits;
    procedure TestConversions;
  end;

implementation

function TTestCase1.UseDefaultThousandSeparator(const aValue: string; const myThousandSeparator : string = '.'): string;
begin
  Result := StringReplace(aValue, myThousandSeparator, FormatSettings.ThousandSeparator, [rfReplaceAll]);
end;

procedure TTestCase1.TestRounding;
begin
  CheckEquals(2, RoundToExt(1.5, rmHalfRoundAwayFromZero, 0));
  CheckEquals(-2, RoundToExt(-1.5,rmHalfRoundAwayFromZero, 0));
  CheckEquals(1.7, RoundToExt(1.65, rmHalfRoundAwayFromZero, 1));
  CheckEquals(-1.7, RoundToExt(-1.65,rmHalfRoundAwayFromZero, 1));
  CheckEquals(1.4, RoundToExt(1.36, rmHalfRoundAwayFromZero, 1));
  CheckEquals(1.4, RoundToExt(1.42, rmHalfRoundAwayFromZero, 1));
  CheckEquals(-1.4, RoundToExt(-1.36, rmHalfRoundAwayFromZero, 1));
  CheckEquals(-1.4, RoundToExt(-1.42, rmHalfRoundAwayFromZero, 1));

  CheckEquals(1, RoundToExt(1.5, rmHalfRoundTowardsZero, 0));
  CheckEquals(-1, RoundToExt(-1.5,rmHalfRoundTowardsZero, 0));
  CheckEquals(1.6, RoundToExt(1.65, rmHalfRoundTowardsZero, 1));
  CheckEquals(-1.6, RoundToExt(-1.65,rmHalfRoundTowardsZero, 1));
  CheckEquals(1.4, RoundToExt(1.36, rmHalfRoundTowardsZero, 1));
  CheckEquals(1.4, RoundToExt(1.42, rmHalfRoundTowardsZero, 1));
  CheckEquals(-1.4, RoundToExt(-1.36, rmHalfRoundTowardsZero, 1));
  CheckEquals(-1.4, RoundToExt(-1.42, rmHalfRoundTowardsZero, 1));

  CheckEquals(2, RoundToExt(1.5, rmHalfRoundUp, 0));
  CheckEquals(-1, RoundToExt(-1.5,rmHalfRoundUp, 0));
  CheckEquals(1.7, RoundToExt(1.65, rmHalfRoundUp, 1));
  CheckEquals(-1.6, RoundToExt(-1.65,rmHalfRoundUp, 1));
  CheckEquals(1.4, RoundToExt(1.36, rmHalfRoundUp, 1));
  CheckEquals(1.4, RoundToExt(1.42, rmHalfRoundUp, 1));
  CheckEquals(-1.4, RoundToExt(-1.36, rmHalfRoundUp, 1));
  CheckEquals(-1.4, RoundToExt(-1.42, rmHalfRoundUp, 1));


  CheckEquals(1, RoundToExt(1.5, rmHalfRoundDown, 0));
  CheckEquals(-2, RoundToExt(-1.5,rmHalfRoundDown, 0));
  CheckEquals(1.6, RoundToExt(1.65, rmHalfRoundDown, 1));
  CheckEquals(-1.7, RoundToExt(-1.65,rmHalfRoundDown, 1));
  CheckEquals(1.4, RoundToExt(1.36, rmHalfRoundDown, 1));
  CheckEquals(1.4, RoundToExt(1.42, rmHalfRoundDown, 1));
  CheckEquals(-1.4, RoundToExt(-1.36, rmHalfRoundDown, 1));
  CheckEquals(-1.4, RoundToExt(-1.42, rmHalfRoundDown, 1));

  CheckEquals(2, RoundToExt(1.5, rmHalfRoundToEven, 0));
  CheckEquals(-2, RoundToExt(-1.5,rmHalfRoundToEven, 0));
  CheckEquals(1.6, RoundToExt(1.65, rmHalfRoundToEven, 1));
  CheckEquals(-1.6, RoundToExt(-1.65,rmHalfRoundToEven, 1));
  CheckEquals(1.4, RoundToExt(1.45, rmHalfRoundToEven, 1));
  CheckEquals(-1.4, RoundToExt(-1.45,rmHalfRoundToEven, 1));
  CheckEquals(1.4, RoundToExt(1.36, rmHalfRoundToEven, 1));
  CheckEquals(1.4, RoundToExt(1.42, rmHalfRoundToEven, 1));
  CheckEquals(-1.4, RoundToExt(-1.36, rmHalfRoundToEven, 1));
  CheckEquals(-1.4, RoundToExt(-1.42, rmHalfRoundToEven, 1));

  CheckEquals(1, RoundToExt(1.5, rmHalfRoundToOdd, 0));
  CheckEquals(-1, RoundToExt(-1.5,rmHalfRoundToOdd, 0));
  CheckEquals(1.7, RoundToExt(1.65, rmHalfRoundToOdd, 1));
  CheckEquals(-1.7, RoundToExt(-1.65,rmHalfRoundToOdd, 1));
  CheckEquals(1.5, RoundToExt(1.45, rmHalfRoundToOdd, 1));
  CheckEquals(-1.5, RoundToExt(-1.45,rmHalfRoundToOdd, 1));
  CheckEquals(1.4, RoundToExt(1.36, rmHalfRoundToOdd, 1));
  CheckEquals(1.4, RoundToExt(1.42, rmHalfRoundToOdd, 1));
  CheckEquals(-1.4, RoundToExt(-1.36, rmHalfRoundToOdd, 1));
  CheckEquals(-1.4, RoundToExt(-1.42, rmHalfRoundToOdd, 1));

  CheckEquals(788938.59, RoundToExt(788938.59000154, rmHalfRoundAwayFromZero, 2));


  CheckEquals(123.12, RoundToExt(123.1125, rmCeil, 2));
  CheckEquals(123.01, RoundToExt(123.000001, rmCeil, 2));
  CheckEquals(123, RoundToExt(123, rmCeil, 2));
  CheckEquals(123, RoundToExt(123, rmCeil, 1));
  CheckEquals(123, RoundToExt(123, rmCeil, 0));
  CheckEquals(123, RoundToExt(123, rmCeil, 4));
  CheckEquals(25.001, RoundToExt(25.0001, rmCeil, 3));
  CheckEquals(25.9, RoundToExt(25.8001, rmCeil, 1));
  CheckEquals(25.83, RoundToExt(25.82113, rmCeil, 2));
end;

procedure TTestCase1.TestGetFractionalPartDigits;
begin
  CheckEquals(0, GetFractionalPartDigits(5));
  CheckEquals(0, GetFractionalPartDigits(1002));
  CheckEquals(0, GetFractionalPartDigits(-23));
  CheckEquals(1, GetFractionalPartDigits(5.7));
  CheckEquals(1, GetFractionalPartDigits(145.1));
  CheckEquals(1, GetFractionalPartDigits(-9.9));
  CheckEquals(2, GetFractionalPartDigits(8.71));
  CheckEquals(2, GetFractionalPartDigits(8.98));
  CheckEquals(2, GetFractionalPartDigits(5480.01));
  CheckEquals(2, GetFractionalPartDigits(-54.99));
  CheckEquals(3, GetFractionalPartDigits(4.578));
  CheckEquals(3, GetFractionalPartDigits(587.001));
  CheckEquals(3, GetFractionalPartDigits(-99.123));
  CheckEquals(5, GetFractionalPartDigits(58.00001));
  CheckEquals(5, GetFractionalPartDigits(58.99999));
end;

procedure TTestCase1.TestConversions;
var
  i : integer;
begin
  CheckTrue(IsNumeric('19072320140648820000001', false, true));
  CheckTrue(TryToConvertToInteger('87998540', i));
  CheckFalse(TryToConvertToInteger(UseDefaultThousandSeparator('879.98540'), i));
  CheckTrue(TryToConvertToInteger(UseDefaultThousandSeparator('87.998.540'), i));
  CheckFalse(TryToConvertToInteger(UseDefaultThousandSeparator('879.9854O'), i));
  CheckTrue(IsNumeric('+585', false, true));
  CheckFalse(IsNumeric('+585', false, false));
  CheckTrue(IsNumeric('-12.66589', true, true));
  CheckFalse(IsNumeric('-12.66589', true, false));
  CheckFalse(IsNumeric('-12.66589', false, false));
  CheckFalse(IsNumeric('12+4', false, true));
  CheckFalse(IsNumeric('12+4', false, false));
  CheckFalse(IsNumeric('12.22-45,54', true, false));
  CheckFalse(IsNumeric('-', false, true));
  CheckFalse(IsNumeric('+', false, true));
  CheckFalse(IsNumeric('-1344.223,1223', true, true));
  CheckTrue(IsNumeric('+2544,25', true, true));
  CheckTrue(IsNumeric('+2544.25', true, true));
  CheckTrue(IsNumeric('+2.544,25', true, true));
  CheckTrue(IsNumeric('+2,544.25', true, true));
  CheckTrue(IsNumeric('2544,25', true, false));
  CheckTrue(IsNumeric('2544.25', true, false));
  CheckFalse(IsNumeric('-2.544,25', true, false));
  CheckTrue(IsNumeric('12032544,251143', true, false));
  CheckTrue(IsNumeric('12032544.251143', true, false));
  CheckFalse(IsNumeric('12.032544.251143', true, false));
  CheckTrue(IsNumeric('1.236,45', true, false));
  CheckTrue(IsNumeric('-1.236,45', true, true));
  CheckTrue(IsNumeric('34.154.236,45', true, false));
  CheckTrue(IsNumeric('-34.154.236,45', true, true));
  CheckTrue(IsNumeric('1,236.45', true, false));
  CheckTrue(IsNumeric('-1,236.45', true, true));
  CheckTrue(IsNumeric('34,154,236.45', true, false));
  CheckTrue(IsNumeric('-34,154,236.45', true, true));
  CheckFalse(IsNumeric('34,1,54,236.45', true, false));
  CheckFalse(IsNumeric('34.154.23.6,45', true, false));
  CheckFalse(IsNumeric('34.154.23.6', true, false));
  CheckFalse(IsNumeric('11.', true, false));
  CheckFalse(IsNumeric('11,', true, false));
  CheckFalse(IsNumeric('.11', true, false));
  CheckFalse(IsNumeric(',11', true, false));

  CheckTrue(IsInteger('123', false, ''));
  CheckFalse(IsInteger('+23', false, ''));
  CheckTrue(IsInteger('+23', true, ''));
  CheckFalse(IsInteger('-', true, ''));
  CheckFalse(IsInteger('+', false, ''));
  CheckFalse(IsInteger('++234', true, ''));
  CheckFalse(IsInteger('+234.2', true, ''));
  CheckFalse(IsInteger('+234.2', true, '.'));
  CheckTrue(IsInteger('1.234', false, '.'));
  CheckTrue(IsInteger('1.430.000', false, '.'));
  CheckTrue(IsInteger('1.430.000', true, '.'));
  CheckTrue(IsInteger('-1.430.000', true, '.'));
  CheckTrue(IsInteger('+1.430.000', true, '.'));
  CheckFalse(IsInteger('1.430.000,00', true, '.'));
  CheckFalse(IsInteger('1.430.000', false, ','));
end;



initialization
{$IFDEF FPC}
  RegisterTest(TTestCase1);
{$ELSE}
  RegisterTest(TTestCase1.Suite);
{$ENDIF}
end.

