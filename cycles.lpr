program cycles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, unitstart, swissdelphi, unitastron, UnitProcess,
  UnitAPI, UnitConversions, UnitReqResp, unitdata, UnitInit,
  unitcentralcontroller, unitgraph, UnitValidation, unitrs, UnitDlgCycleType,
  UnitDlgCoordinate, UnitDlgPeriod, unitdlgsinglecp, unitdlgpairedcp, unitdlgconfirm
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TFormGraph, FormGraph);
  Application.CreateForm(TFormDlgCycleType, FormDlgCycleType);
  Application.CreateForm(TFormDlgCoordinate, FormDlgCoordinate);
  Application.CreateForm(TFormDlgPeriod, FormDlgPeriod);
  Application.CreateForm(TFormDlgSingleCP, FormDlgSingleCP);
  Application.CreateForm(TFormDlgPairedCP, FormDlgPairedCP);
  Application.CreateForm(TFormDlgConfirm, FormDlgConfirm);
  Application.Run;
end.

