program mNullablesTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, NullablesTestCase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

