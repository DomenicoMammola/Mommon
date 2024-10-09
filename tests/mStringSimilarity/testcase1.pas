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

function CheckStrings(const aString1, aString2 : String; const aIgnorePunctuation, aCheckScrambledWords : boolean): double;
var
  s : String;
begin
  Result := CompareStrings(aString1, aString2, aIgnorePunctuation, aCheckScrambledWords);
  s := '"' + aString1 + '" compared to "' + aString2 + '"';
  if aIgnorePunctuation then
    s := s + ' (no punctuation)';
  if aCheckScrambledWords then
    s := s + ' (check scrambled words)';
  WriteLn(s + ' = ' + FloatToStr(Result));
end;

procedure TTestCase1.TestStringSimilarity;
begin
  CheckEquals(1, CheckStrings('pippo', 'pippo', false, false));
  CheckTrue(CheckStrings('pippo', 'pipppo', false, false) >= 0.8);
  CheckTrue(CheckStrings('pippo', 'pippi', false, false) >= 0.8);
  CheckTrue(CheckStrings('poppi', 'pipppo', false, false) >= 0.5);
  CheckTrue(CheckStrings('ei fu siccome immobile', 'ei fu immobile siccome', false, false) >= 0.5);
  CheckTrue(CheckStrings('ei fu siccome immobile', 'ei fu', false, false) < 0.3);
  CheckTrue(CheckStrings('mldnasdkl asdkksjdf hkj dlfjksah', 'fjkjsàqòr we wioe skdfkajsdjk jldsfa k', false, false) < 0.3);
  CheckTrue(CheckStrings('Paul', 'Paula', false, false) >= 0.7);
  CheckTrue(CheckStrings('Orange is the new black', 'Black is the new orange', false, false) >= 0.5);
  CheckTrue(CheckStrings('The, same, thing;', 'The same thing', true, false) = 1);
  CheckTrue(CheckStrings('The, same, thing;', 'The same thing', false, true) = 1);
  CheckTrue(CheckStrings('The, same, thing;', 'The same thing', false, false) >= 0.5);
  CheckTrue(CheckStrings('missing some char', 'misin som chr', false, false) >= 0.5);
  CheckTrue(CheckStrings('change correct order 1 2 3', 'change correct order 3 2 1', false, false) >= 0.75);
  CheckTrue(CheckStrings('change correct order 1 2 3', 'change correct order 3 2 1', false, true) = 1);
  CheckTrue(CheckStrings('Boooooohhhh!', 'Booohhh!', false, false) >= 0.55);
  CheckTrue(CheckStrings('AGRINTESA SOC. COOP. AGRICOLA', 'AGRINTESA SOC.AGR.COOP.', false, false) >= 0.6);
  CheckTrue(CheckStrings('AGRINTESA SOC. COOP. AGRICOLA', 'AGRINTESA SOC.AGR.COOP.', false, true) >= 0.8);
  CheckTrue(CheckStrings('Forli''', 'Forlì', true, false) >= 0.8);
end;



initialization
{$IFDEF FPC}
  RegisterTest(TTestCase1);
{$ELSE}
  RegisterTest(TTestCase1.Suite);
{$ENDIF}
end.

