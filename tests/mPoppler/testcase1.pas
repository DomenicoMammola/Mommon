unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  mPoppler;

const
  popplerFolder = 'd:\temp\poppler';

type

  TTestPoppler= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInfo;
  end;

implementation

uses
  DateUtils, Forms;


procedure TTestPoppler.TestInfo;
var
  tmpInfo : TPopplerPdfInfo;
begin
  CheckTrue(TPopplerToolbox.GetInfoFromPdf(IncludeTrailingPathDelimiter(Application.Location) + 'A4landscape.pdf', tmpInfo));
  CheckEquals('Draw', tmpInfo.Creator);
  CheckEquals('LibreOffice 7.2', tmpInfo.Producer);
  CheckEquals(842, tmpInfo.PageWidth);
  CheckEquals(595, tmpInfo.PageHeight);
  CheckEquals('1.6', tmpInfo.PDFVersion);
  CheckEquals(EncodeDateTime(2022, 11, 28, 17, 40, 43, 0), tmpInfo.CreationDate);
end;

procedure TTestPoppler.SetUp;
begin
  Poppler_pdfunite_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdfunite.exe';
  Poppler_pdfseparate_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdfseparate.exe';
  Poppler_pdftoppm_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdftoppm.exe';
  Poppler_pdftotext_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdftotext.exe';
  Poppler_pdfinfo_ExePath := IncludeTrailingPathDelimiter(popplerFolder) + 'pdfinfo.exe';
end;

procedure TTestPoppler.TearDown;
begin

end;

initialization

  RegisterTest(TTestPoppler);
end.

