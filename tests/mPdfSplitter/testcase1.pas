unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  mPoppler, mPdfSplitter, mProgress;


const
  popplerFolder = 'd:\temp\poppler';

type

  { TTestCaseSplitter }

  TTestCaseSplitter= class(TTestCase)
  strict private
    FOutputFolder : string;
    FProgress : TmFakeProgress;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSplit1;
  end;

implementation
uses
  FileUtil;

procedure TTestCaseSplitter.TestSplit1;
var
  commands : TSplitCommands;
  f1, f2 : String;
  tmpInfo : TPopplerPdfInfo;
  error : String;
begin
  commands := TSplitCommands.Create;
  try
    f1 := IncludeTrailingPathDelimiter(FOutputFolder) + 'first.pdf';
    f2 := IncludeTrailingPathDelimiter(FOutputFolder) + 'second.pdf';
    with commands.Add do
    begin
      PageFrom:= 1;
      PageTo:= 5;
      FileNameDestination:= f1;
    end;
    with commands.Add do
    begin
      PageFrom:= 6;
      PageTo:= 10;
      FileNameDestination:= f2;
    end;
    CheckTrue(SplitPdfFile(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)))+ 'sample-pdf-with-images.pdf', commands, FProgress, error));
    CheckTrue(FileExists(f1));
    CheckTrue(FileExists(f2));

    TPopplerToolbox.GetInfoFromPdf(f1, tmpInfo);
    CheckEquals(5, tmpInfo.Pages);
    TPopplerToolbox.GetInfoFromPdf(f2, tmpInfo);
    CheckEquals(5, tmpInfo.Pages);
  finally
    commands.Free;
  end
end;

procedure TTestCaseSplitter.SetUp;
begin
  Poppler_pdfunite_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdfunite.exe';
  Poppler_pdfseparate_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdfseparate.exe';
  Poppler_pdftoppm_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdftoppm.exe';
  Poppler_pdftotext_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdftotext.exe';
  Poppler_pdfinfo_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdfinfo.exe';
  FOutputFolder:= IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'output';
  if not DirectoryExists(FOutputFolder) then
    ForceDirectories(FOutputFolder);
  FProgress := TmFakeProgress.Create;
end;

procedure TTestCaseSplitter.TearDown;
begin
  if DirectoryExists(FOutputFolder) then
    DeleteDirectory(FOutputFolder, false);
  FProgress.Free;
  inherited TearDown;
end;


initialization
  RegisterTest(TTestCaseSplitter);
end.
