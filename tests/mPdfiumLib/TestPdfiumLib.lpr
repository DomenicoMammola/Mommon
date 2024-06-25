program TestPdfiumLib;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testcase1;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

