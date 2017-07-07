unit TestCase1;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs, mStringSimilarity

  {$IFNDEF FPC}, IOUtils, TestFramework
  {$ELSE}
  ,fpcunit, testutils, testregistry
  {$ENDIF};

type

  TTestCase1= class(TTestCase)
  published
    procedure TestStringSimilarity;
  end;

implementation

procedure TTestCase1.TestStringSimilarity;
var
  tmp : Double;
begin
  CheckEquals(1, CompareStrings('pippo', 'pippo', true));
  tmp := CompareStrings('pippo', 'pipppo', true);
  WriteLn(FloatToStr(tmp));
  CheckTrue(tmp > 0.9);
  tmp := CompareStrings('poppi', 'pipppo', true);
  WriteLn(FloatToStr(tmp));
  CheckTrue(tmp > 0.8);
  tmp := CompareStrings('ei fu siccome immobile', 'ei fu immobile siccome', true);
  WriteLn(FloatToStr(tmp));
  CheckTrue(tmp > 0.9);
  tmp := CompareStrings('ei fu siccome immobile', 'ei fu', true);
  WriteLn(FloatToStr(tmp));
  CheckTrue(tmp < 0.3);
  tmp := CompareStrings('mldnasdkl asdkksjdf hkj dlfjksah', 'fjkjsàqòr we wioe skdfkajsdjk jldsfa k', true);
  WriteLn(FloatToStr(tmp));
  CheckTrue(tmp < 0.3);
end;



initialization
{$IFDEF FPC}
  RegisterTest(TTestCase1);
{$ELSE}
  RegisterTest(TTestCase1.Suite);
{$ENDIF}
end.

