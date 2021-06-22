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
  SelectedCPs:= AllCPs;
  SelectedCPs[0].Selected:= CbSun.Checked;
  SelectedCPs[1].Selected:= CbMoon.Checked;
  SelectedCPs[2].Selected:= CbMercury.Checked;
  SelectedCPs[3].Selected:= CbVenus.Checked;
  SelectedCPs[4].Selected:= CbMars.Checked;
  SelectedCPs[5].Selected:= CbJupiter.Checked;
  SelectedCPs[6].Selected:= CbSaturn.Checked;
  SelectedCPs[7].Selected:= CbUranus.Checked;
  SelectedCPs[8].Selected:= CbNeptune.Checked;
  SelectedCPs[9].Selected:= CbPluto.Checked;
  SelectedCPs[10].Selected:= CbMeanNode.Checked;
  SelectedCPs[11].Selected:= CbOscNode.Checked;
  SelectedCPs[12].Selected:= CbEarth.Checked;
  SelectedCPs[13].Selected:= CbChiron.Checked;
  SelectedCPs[14].Selected:= CbPholus.Checked;
  SelectedCPs[15].Selected:= CbCeres.Checked;
  SelectedCPs[16].Selected:= CbPallas.Checked;
  SelectedCPs[17].Selected:= CbJuno.Checked;
  SelectedCPs[18].Selected:= CbVesta.Checked;
  SelectedCPs[19].Selected:= CbNessus.Checked;
  SelectedCPs[20].Selected:= CbHuya.Checked;
  SelectedCPs[21].Selected:= CbMakeMake.Checked;
  SelectedCPs[22].Selected:= CbHaumea.Checked;
  SelectedCPs[23].Selected:= CbEris.Checked;
  SelectedCPs[24].Selected:= CbIxion.Checked;
  SelectedCPs[25].Selected:= CbOrcus.Checked;
  SelectedCPs[26].Selected:= CbQuaoar.Checked;
  SelectedCPs[27].Selected:= CbSedna.Checked;
  SelectedCPs[28].Selected:= CbVaruna.Checked;
  SelectedCPs[29].Selected:= CbMeanApogee.Checked;
  SelectedCPs[30].Selected:= CbTrueApogee.Checked;
  StateMachine.SingleCPs := SelectedCPs;
end;

end.
