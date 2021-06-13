unit unitdlgsinglecp;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, Forms, Graphics, StdCtrls, SysUtils, unitcentralcontroller, unitdomainxchg;

type

  { TFormDlgSingleCP }

  TFormDlgSingleCP = class(TForm)
    BtnCancel: TButton;
    BtnContinue: TButton;
    BtnHelp: TButton;
    CbCeres: TCheckBox;
    CbChiron: TCheckBox;
    CbEarth: TCheckBox;
    CbEris: TCheckBox;
    CbHaumea: TCheckBox;
    CbHuya: TCheckBox;
    CbIxion: TCheckBox;
    CbJuno: TCheckBox;
    CbJupiter: TCheckBox;
    CbMakeMake: TCheckBox;
    CbMars: TCheckBox;
    CbMeanApogee: TCheckBox;
    CbMeanNode: TCheckBox;
    CbMercury: TCheckBox;
    CbMoon: TCheckBox;
    CbNeptune: TCheckBox;
    CbNessus: TCheckBox;
    CbOrcus: TCheckBox;
    CbOscNode: TCheckBox;
    CbPallas: TCheckBox;
    CbPholus: TCheckBox;
    CbPluto: TCheckBox;
    CbQuaoar: TCheckBox;
    CbSaturn: TCheckBox;
    CbSedna: TCheckBox;
    CbSun: TCheckBox;
    CbTrueApogee: TCheckBox;
    CbUranus: TCheckBox;
    CbVaruna: TCheckBox;
    CbVenus: TCheckBox;
    CbVesta: TCheckBox;
    LblTitle: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CenCon: TCenCon;
    StateMachine: TFinStateMachine;
    procedure CheckObserverPos;
    procedure CheckPeriod;
    procedure DefineCPs;
    function PeriodSupported(CpIndex: integer; StartJd, EndJd: double): boolean;
    procedure SaveState;
    function SaveStateHelper(CpIndex, Count: integer): integer;
  public

  end;

var
  FormDlgSingleCP: TFormDlgSingleCP;

implementation

{$R *.lfm}

var
  AllCPs, SelectedCPs: TCelPointSpecArray;

procedure TFormDlgSingleCP.FormShow(Sender: TObject);
begin
  CenCon := TCenCon.Create;
  StateMachine := TFinStateMachine.Create;
  DefineCPs;
end;

procedure TFormDlgSingleCP.BtnContinueClick(Sender: TObject);
begin
  Hide;
  SaveState;
  StateMachine.ChangeState(SingleCPDefined);
end;

procedure TFormDlgSingleCP.BtnCancelClick(Sender: TObject);
begin
  Close;
  StateMachine.ChangeState(Cancel);
end;

procedure TFormDlgSingleCP.BtnHelpClick(Sender: TObject);
begin
  ShowMessage('To be replaced with help');
end;

procedure TFormDlgSingleCP.DefineCPs;
var
  i, NorOfCPs: integer;
begin
  AllCPs := CenCon.LookupValues.AllCelPoints;
  CheckObserverPos;
  CheckPeriod;
end;

procedure TFormDlgSingleCP.CheckObserverPos;
begin
  if StateMachine.ObserverPos.Identification = 'Geocentric' then CbEarth.Enabled := False;
  if StateMachine.ObserverPos.Identification = 'Heliocentric' then begin
    CbEarth.Enabled := True;
    CbSun.Enabled := False;
    CbMoon.Enabled := False;
    CbMeanNode.Enabled := False;
    CbOscNode.Enabled := False;
    CbMeanApogee.Enabled := False;
    CbTrueApogee.Enabled := False;
  end;
end;

function TFormDlgSingleCP.PeriodSupported(CpIndex: integer; StartJd, EndJd: double): boolean;
begin
  Result := ((AllCPs[CpIndex].FirstJd <= StartJd) and (AllCPs[CpIndex].LastJD >= EndJd));
end;

procedure TFormDlgSingleCP.CheckPeriod;
var
  StartJd, EndJD: double;
begin
  StartJd := StateMachine.Period.StartDate.JulianDay;
  EndJd := StateMachine.Period.EndDate.JulianDay;
  CbChiron.Enabled := PeriodSupported(13, StartJd, EndJd);
  CbPholus.Enabled := PeriodSupported(14, StartJd, EndJd);
  CbCeres.Enabled := PeriodSupported(15, StartJd, EndJd);
  CbVesta.Enabled := PeriodSupported(18, StartJd, EndJd);
  CbNessus.Enabled := PeriodSupported(19, StartJd, EndJd);
  CbHuya.Enabled := PeriodSupported(20, StartJd, EndJd);
  CbMakeMake.Enabled := PeriodSupported(21, StartJd, EndJd);
  CbHaumea.Enabled := PeriodSupported(22, StartJd, EndJd);
  CbEris.Enabled := PeriodSupported(23, StartJd, EndJd);
  CbIxion.Enabled := PeriodSupported(24, StartJd, EndJd);
  CbOrcus.Enabled := PeriodSupported(25, StartJd, EndJd);
  CbQuaoar.Enabled := PeriodSupported(26, StartJd, EndJd);
  CbSedna.Enabled := PeriodSupported(27, StartJd, EndJd);
  CbVaruna.Enabled := PeriodSupported(28, StartJd, EndJd);
end;

procedure TFormDlgSingleCP.SaveState;
var
  Count: integer;
begin
  Count := 0;
  if (CbSun.Checked) then Count := SaveStateHelper(0, Count);
  if (CbMoon.Checked) then Count := SaveStateHelper(1, Count);
  if (CbMercury.Checked) then Count := SaveStateHelper(2, Count);
  if (CbVenus.Checked) then Count := SaveStateHelper(3, Count);
  if (CbMars.Checked) then Count := SaveStateHelper(4, Count);
  if (CbJupiter.Checked) then Count := SaveStateHelper(5, Count);
  if (CbSaturn.Checked) then Count := SaveStateHelper(6, Count);
  if (CbUranus.Checked) then Count := SaveStateHelper(7, Count);
  if (CbNeptune.Checked) then Count := SaveStateHelper(8, Count);
  if (CbPluto.Checked) then Count := SaveStateHelper(9, Count);
  if (CbMeanNode.Checked) then Count := SaveStateHelper(10, Count);
  if (CbOscNode.Checked) then Count := SaveStateHelper(11, Count);
  if (CbEarth.Checked) then Count := SaveStateHelper(12, Count);
  if (CbChiron.Checked) then Count := SaveStateHelper(13, Count);
  if (CbPholus.Checked) then Count := SaveStateHelper(14, Count);
  if (CbCeres.Checked) then Count := SaveStateHelper(15, Count);
  if (CbPallas.Checked) then Count := SaveStateHelper(16, Count);
  if (CbJuno.Checked) then Count := SaveStateHelper(17, Count);
  if (CbVesta.Checked) then Count := SaveStateHelper(18, Count);
  if (CbNessus.Checked) then Count := SaveStateHelper(19, Count);
  if (CbHuya.Checked) then Count := SaveStateHelper(20, Count);
  if (CbMakeMake.Checked) then Count := SaveStateHelper(21, Count);
  if (CbHaumea.Checked) then Count := SaveStateHelper(22, Count);
  if (CbEris.Checked) then Count := SaveStateHelper(23, Count);
  if (CbIxion.Checked) then Count := SaveStateHelper(24, Count);
  if (CbOrcus.Checked) then Count := SaveStateHelper(25, Count);
  if (CbQuaoar.Checked) then Count := SaveStateHelper(26, Count);
  if (CbSedna.Checked) then Count := SaveStateHelper(27, Count);
  if (CbVaruna.Checked) then Count := SaveStateHelper(28, Count);
  if (CbMeanApogee.Checked) then Count := SaveStateHelper(29, Count);
  if (CbTrueApogee.Checked) then Count := SaveStateHelper(30, Count);
  StateMachine.SingleCPs := SelectedCPs;
end;

function TFormDlgSingleCP.SaveStateHelper(CpIndex, Count: integer): integer;
begin
  SelectedCPs[Count] := AllCPs[CpIndex];
  Inc(Count);
  Result := Count;
end;

end.
