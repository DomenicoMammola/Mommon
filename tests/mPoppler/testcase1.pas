unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  mPoppler;


type

  { TTestPoppler }

  TTestPoppler= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInfo;
    procedure TestExtractPagesAsJpeg;
  end;

implementation

uses
  DateUtils, Forms, StrUtils, FileUtil,
  mUtility;


procedure TTestPoppler.TestInfo;
var
  tmpInfo : TPopplerPdfInfo;
  ris : boolean;
begin
  ris := TPopplerToolbox.GetInfoFromPdf(IncludeTrailingPathDelimiter(Application.Location) + 'A4landscape.pdf', tmpInfo);
  CheckTrue(ris, TPopplerToolbox.GetLastError);
  CheckEquals('Draw', tmpInfo.Creator);
  CheckEquals('LibreOffice 7.2', tmpInfo.Producer);
  CheckEquals(842, tmpInfo.PageWidth);
  CheckEquals(595, tmpInfo.PageHeight);
  CheckEquals('1.6', tmpInfo.PDFVersion);
  CheckEquals(EncodeDateTime(2022, 11, 28, 17, 40, 43, 0), tmpInfo.CreationDate);

  CheckTrue(TPopplerToolbox.GetInfoFromPdf(IncludeTrailingPathDelimiter(Application.Location) + 'A4.pdf', tmpInfo));
  CheckTrue(ContainsText(tmpInfo.Creator, 'Word per Office 365'));
  CheckTrue(ContainsText(tmpInfo.Producer, 'Word per Office 365'));
  CheckEquals(595, tmpInfo.PageWidth);
  CheckEquals(842, tmpInfo.PageHeight);
  CheckEquals('1.7', tmpInfo.PDFVersion);
  CheckEquals(EncodeDateTime(2022, 12, 19, 10, 2, 40, 0), tmpInfo.CreationDate);

  CheckTrue(TPopplerToolbox.GetInfoFromPdf(IncludeTrailingPathDelimiter(Application.Location) + 'letter.pdf', tmpInfo));
  CheckEquals('Draw', tmpInfo.Creator);
  CheckEquals('LibreOffice 7.2', tmpInfo.Producer);
  CheckEquals(612, tmpInfo.PageWidth);
  CheckEquals(792, tmpInfo.PageHeight);
  CheckEquals('1.6', tmpInfo.PDFVersion);

  CheckTrue(TPopplerToolbox.GetInfoFromPdf(IncludeTrailingPathDelimiter(Application.Location) + 'A5.pdf', tmpInfo));
  CheckEquals('Draw', tmpInfo.Creator);
  CheckEquals('LibreOffice 7.2', tmpInfo.Producer);
  CheckEquals(420, tmpInfo.PageWidth);
  CheckEquals(595, tmpInfo.PageHeight);
  CheckEquals('1.6', tmpInfo.PDFVersion);
  CheckEquals(EncodeDateTime(2022, 11, 28, 17, 37, 49, 0), tmpInfo.CreationDate);

  CheckTrue(TPopplerToolbox.GetInfoFromPdf(IncludeTrailingPathDelimiter(Application.Location) + 'B5.pdf', tmpInfo));
  CheckEquals('Draw', tmpInfo.Creator);
  CheckEquals('LibreOffice 7.2', tmpInfo.Producer);
  CheckEquals(516, tmpInfo.PageWidth);
  CheckEquals(729, tmpInfo.PageHeight);
  CheckEquals('1.6', tmpInfo.PDFVersion);
  CheckEquals(EncodeDateTime(2022, 11, 28, 17, 40, 14, 0), tmpInfo.CreationDate);

  CheckTrue(TPopplerToolbox.GetInfoFromPdf(IncludeTrailingPathDelimiter(Application.Location) + 'legal.pdf', tmpInfo));
  CheckEquals('Draw', tmpInfo.Creator);
  CheckEquals('LibreOffice 7.2', tmpInfo.Producer);
  CheckEquals(612, tmpInfo.PageWidth);
  CheckEquals(1008, tmpInfo.PageHeight);
  CheckEquals('1.6', tmpInfo.PDFVersion);
  CheckEquals(EncodeDateTime(2022, 11, 28, 17, 39, 15, 0), tmpInfo.CreationDate);
end;

procedure TTestPoppler.TestExtractPagesAsJpeg;
var
  tmpFolder : String;
  ris : boolean;
begin
  tmpFolder:= GetUniqueTemporaryFolder;
  try
    ris := TPopplerToolbox.ExtractPagesFromPdfAsJpeg(IncludeTrailingPathDelimiter(Application.Location) + 'many_pages.pdf', tmpFolder, 'test_extract_pages_as_jpeg', 60, 150);
    CheckTrue(ris, TPopplerToolbox.GetLastError);
    CheckTrue(FileExists(IncludeTrailingPathDelimiter(tmpFolder) + 'test_extract_pages_as_jpeg-43.jpg'));
  finally
    DeleteDirectory(tmpFolder, false);
  end;
end;

procedure TTestPoppler.SetUp;
{$IFDEF WINDOWS}
var
  popplerFolder : String;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  popplerFolder := 'c:\temp\poppler'; // set to the right folder
  Poppler_pdfunite_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdfunite.exe';
  Poppler_pdfseparate_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdfseparate.exe';
  Poppler_pdftoppm_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdftoppm.exe';
  Poppler_pdftotext_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdftotext.exe';
  Poppler_pdfinfo_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdfinfo.exe';
  Poppler_pdfimages_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdfimages.exe';
  {$ELSE}
  Poppler_pdfunite_ExePath := 'pdfunite';
  Poppler_pdfseparate_ExePath := 'pdfseparate';
  Poppler_pdftoppm_ExePath := 'pdftoppm';
  Poppler_pdftotext_ExePath := 'pdftotext';
  Poppler_pdfinfo_ExePath := 'pdfinfo';
  Poppler_pdfimages_ExePath := 'pdfimages';
  {$ENDIF}
end;

procedure TTestPoppler.TearDown;
begin

end;

initialization

  RegisterTest(TTestPoppler);
end.

