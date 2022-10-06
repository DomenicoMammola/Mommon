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

function CheckStrings(const aString1, aString2 : String; const aIgnorePunctuation : boolean): double;
var
  s : String;
begin
  Result := CompareStrings(aString1, aString2, aIgnorePunctuation);
  s := '"' + aString1 + '" compared to "' + aString2 + '"';
  if aIgnorePunctuation then
    s := s + ' (no punctuation)';
  WriteLn(s + ' = ' + FloatToStr(Result));
end;

procedure TTestCase1.TestStringSimilarity;
begin
  CheckEquals(1, CheckStrings('pippo', 'pippo', false));
  CheckTrue(CheckStrings('pippo', 'pipppo', false) >= 0.8);
  CheckTrue(CheckStrings('pippo', 'pippi', false) >= 0.8);
  CheckTrue(CheckStrings('poppi', 'pipppo', false) >= 0.5);
  CheckTrue(CheckStrings('ei fu siccome immobile', 'ei fu immobile siccome', false) >= 0.5);
  CheckTrue(CheckStrings('ei fu siccome immobile', 'ei fu', false) < 0.3);
  CheckTrue(CheckStrings('mldnasdkl asdkksjdf hkj dlfjksah', 'fjkjsàqòr we wioe skdfkajsdjk jldsfa k', false) < 0.3);
  CheckTrue(CheckStrings('Paul', 'Paula', false) >= 0.7);
  CheckTrue(CheckStrings('Orange is the new black', 'Black is the new orange', false) >= 0.5);
  CheckTrue(CheckStrings('The, same, thing;', 'The same thing', true) = 1);
  CheckTrue(CheckStrings('The, same, thing;', 'The same thing', false) >= 0.5);
  CheckTrue(CheckStrings('missing some char', 'misin som chr', false) >= 0.5);
  CheckTrue(CheckStrings('change correct order 1 2 3', 'change correct order 3 2 1', false) >= 0.75);
  CheckTrue(CheckStrings('Boooooohhhh!', 'Booohhh!', false) >= 0.55);
end;



initialization
{$IFDEF FPC}
  RegisterTest(TTestCase1);
{$ELSE}
  RegisterTest(TTestCase1.Suite);
{$ENDIF}
end.

