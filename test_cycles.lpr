program test_cycles;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, test_astron, UnitAstron,
  swissdelphi, unitdomainxchg, UnitProcess, test_process;
//Interfaces, Forms, GuiTestRunner, test_astron;
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

