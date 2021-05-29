program test_cycles;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, test_astron, UnitAstron, swissdelphi,
  unitdomainxchg, UnitProcess, test_process, UnitAPI, UnitConversions,
  test_conversions;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

