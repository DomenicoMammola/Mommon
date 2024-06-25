unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  mPdfiumLib;

type

  TTestCasePdfiumLib= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExtractFrontPageAsPng;
  end;

implementation
uses
  Graphics;


procedure TTestCasePdfiumLib.TestExtractFrontPageAsPng;
var
  tmpError : String;
begin
  if FileExists('vertical.png') then
    DeleteFile('vertical.png');
  CheckTrue(TPdfiumLibToolbox.ExtractThumbnailOfFrontPageFromPdfAsPng('vertical.pdf', 'vertical.png', 250, 250, false, clYellow, tmpError));
  if FileExists('horizontal.png') then
    DeleteFile('horizontal.png');
  CheckTrue(TPdfiumLibToolbox.ExtractThumbnailOfFrontPageFromPdfAsPng('horizontal.pdf', 'horizontal.png', 250, 250, false, clYellow, tmpError));
  if FileExists('vertical2.png') then
    DeleteFile('vertical2.png');
  CheckTrue(TPdfiumLibToolbox.ExtractThumbnailOfFrontPageFromPdfAsPng('vertical.pdf', 'vertical2.png', 450, 450, true, clWhite, tmpError));
  if FileExists('horizontal2.png') then
    DeleteFile('horizontal2.png');
  CheckTrue(TPdfiumLibToolbox.ExtractThumbnailOfFrontPageFromPdfAsPng('horizontal.pdf', 'horizontal2.png', 450, 450, true, clWhite, tmpError));
end;

procedure TTestCasePdfiumLib.SetUp;
begin
  if FileExists('vertical.png') then
    DeleteFile('vertical.png');
  if FileExists('horizontal.png') then
    DeleteFile('horizontal.png');
  if FileExists('vertical2.png') then
    DeleteFile('vertical2.png');
  if FileExists('horizontal2.png') then
    DeleteFile('horizontal2.png');
end;

procedure TTestCasePdfiumLib.TearDown;
begin
end;

initialization

  RegisterTest(TTestCasePdfiumLib);
end.

