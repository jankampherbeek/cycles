program cycles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, unitmain, swissdelphi, unitastron, UnitProcess,
  UnitAPI, UnitConversions, UnitReqResp, UnitInit,
  unitcentralcontroller, UnitValidation, unitrs, UnitDlgCycleType,
  UnitDlgCoordinate, UnitDlgPeriod, unitdlgsinglecp, unitdlgpairedcp,
  unitdlgconfirm, UnitConst, unitlinechart;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormDlgCycleType, FormDlgCycleType);
  Application.CreateForm(TFormDlgCoordinate, FormDlgCoordinate);
  Application.CreateForm(TFormDlgPeriod, FormDlgPeriod);
  Application.CreateForm(TFormDlgSingleCP, FormDlgSingleCP);
  Application.CreateForm(TFormDlgPairedCP, FormDlgPairedCP);
  Application.CreateForm(TFormDlgConfirm, FormDlgConfirm);
  Application.CreateForm(TFormLineChart, FormLineChart);
  Application.Run;
end.

