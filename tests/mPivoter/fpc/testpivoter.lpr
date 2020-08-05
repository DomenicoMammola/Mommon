program testpivoter;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testcasepivoter, TestDataset;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

