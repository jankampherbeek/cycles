unit unitdlgpairedcp;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, Forms, Graphics, StdCtrls, SysUtils, unitcentralcontroller, unitdomainxchg;

type

  { TFormDlgPairedCP }

  TFormDlgPairedCP = class(TForm)
    BtnAdd: TButton;
    BtnCancel: TButton;
    BtnClear: TButton;
    BtnContinue: TButton;
    BtnHelp: TButton;
    CBoxCPLeft: TComboBox;
    CboxCPRight: TComboBox;
    LblAnd: TLabel;
    LblSet1: TLabel;
    LblSet1Value: TLabel;
    LblSet2: TLabel;
    LblSet2Value: TLabel;
    LblSet3: TLabel;
    LblSet3Value: TLabel;
    LblSet4: TLabel;
    LblSet4Value: TLabel;
    LblSet5: TLabel;
    LblSet5Value: TLabel;
    LblTitle: TLabel;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure CBoxCPLeftEditingDone(Sender: TObject);
    procedure CboxCPRightEditingDone(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CenCon: TCenCon;
    StateMachine: TFinStateMachine;
    procedure AddPair;
    procedure CheckSelections;
    procedure ClearSelections;
    procedure DefineCPs;
    procedure PopulateCBoxes;
    procedure SaveState;
  public

  end;

var
  FormDlgPairedCP: TFormDlgPairedCP;

implementation

{$R *.lfm}

var
  AllCPs, AvailableCPs: TCelPointSpecArray;
  SelectedPairs: TCelPointPairedSpecArray;
  IndexForSelectedPairs: integer;

{ TFormDlgPairedCP }

procedure TFormDlgPairedCP.FormShow(Sender: TObject);
begin
  CenCon := TCenCon.Create;
  StateMachine := TFinStateMachine.Create;
  DefineCPs;
  PopulateCBoxes;
  IndexForSelectedPairs := 0;
end;

procedure TFormDlgPairedCP.CBoxCPLeftEditingDone(Sender: TObject);
begin
  CheckSelections;
end;

procedure TFormDlgPairedCP.BtnAddClick(Sender: TObject);
begin
  AddPair;
end;

procedure TFormDlgPairedCP.BtnCancelClick(Sender: TObject);
begin
  Close;
  StateMachine.ChangeState(Cancel);
end;

procedure TFormDlgPairedCP.BtnClearClick(Sender: TObject);
begin
  ClearSelections;
end;

procedure TFormDlgPairedCP.BtnContinueClick(Sender: TObject);
begin
  Hide;
  SaveState;
  StateMachine.ChangeState(PairedCPDefined);
end;

procedure TFormDlgPairedCP.BtnHelpClick(Sender: TObject);
begin
  ShowMessage('To be replaced with help');
end;

procedure TFormDlgPairedCP.CboxCPRightEditingDone(Sender: TObject);
begin
  CheckSelections;
end;

procedure TFormDlgPairedCP.CheckSelections;
var
  LeftSelectIdent, RightSelectIdent: string;        // new selected CPs
  SelectedFirst, SelectedLast: string;              // previously selected CPs
  IsNewPair: boolean;
  i, NrOfSelectedPairs: integer;

begin
  IsNewPair := True;
  LeftSelectIdent := AvailableCPs[CBoxCPLeft.ItemIndex].Identification;
  RightSelectIdent := AvailableCPs[CboxCPRight.ItemIndex].Identification;
  if (LeftSelectIdent = RightSelectIdent) or (IndexForSelectedPairs > 4) then IsNewPair := False
  else begin
    NrOfSelectedPairs := Length(SelectedPairs);
    for i := 0 to NrOfSelectedPairs - 1 do begin
      SelectedFirst := SelectedPairs[i].FirstCP.Identification;
      SelectedLast := SelectedPairs[i].SecondCP.Identification;
      if (((LeftSelectIdent = SelectedFirst) and (RightSelectIdent = SelectedLast)) or
        ((LeftSelectIdent = SelectedLast) and (RightSelectIdent = SelectedFirst))) then IsNewPair := False;
    end;
  end;
  BtnAdd.Enabled := IsNewPair;
end;

procedure TFormDlgPairedCP.AddPair;
var
  LeftSelect, RightSelect: TCelPointSpec;
  PairToAdd: TCelPointPairedSpec;
  DescrText: string;
begin
  LeftSelect := AvailableCPs[CBoxCPLeft.ItemIndex];
  RightSelect := AvailableCPs[CboxCPRight.ItemIndex];
  PairToAdd.FirstCP := LeftSelect;
  PairToAdd.SecondCP := RightSelect;
  SetLength(SelectedPairs, IndexForSelectedPairs + 1);
  SelectedPairs[IndexForSelectedPairs] := PairtoAdd;
  DescrText := LeftSelect.Name + ' - ' + RightSelect.Name;
  case IndexForSelectedPairs of
    0: LblSet1Value.Caption := DescrText;
    1: LblSet2Value.Caption := DescrText;
    2: LblSet3Value.Caption := DescrText;
    3: LblSet4Value.Caption := DescrText;
    4: LblSet5Value.Caption := DescrText;
  end;
  Inc(IndexForSelectedPairs);
  BtnContinue.Enabled:= true;
  BtnAdd.Enabled:= false;
end;

procedure TFormDlgPairedCP.ClearSelections;
var
  i, LengthOfSel: integer;
begin
  SelectedPairs:= Default(TCelPointPairedSpecArray);
  IndexForSelectedPairs := 0;
  LblSet1Value.Caption := 'undefined';
  LblSet2Value.Caption := 'undefined';
  LblSet3Value.Caption := 'undefined';
  LblSet4Value.Caption := 'undefined';
  LblSet5Value.Caption := 'undefined';
  BtnContinue.Enabled:= false;
  BtnAdd.Enabled:= true;
end;

procedure TFormDlgPairedCP.DefineCPs;
var
  i, j, NrOfCPs: integer;
  StartJd, EndJd: double;
  PeriodSupported, ObserverPosSupported: boolean;
  ObserverPos: string;
  CurrentCP: TCelPointSpec;
begin
  j := 0;
  AllCPs := CenCon.LookupValues.AllCelPoints;
  NrOfCPs := Length(AllCPs);
  StartJd := StateMachine.Period.StartDate.JulianDay;
  EndJd := StateMachine.Period.EndDate.JulianDay;
  ObserverPos := StateMachine.ObserverPos.Identification;
  for i := 0 to NrOfCPs - 1 do begin
    CurrentCP := AllCPs[i];
    PeriodSupported := ((CurrentCP.FirstJd <= StartJd) and (CurrentCP.LastJD >= EndJd));
    ObserverPosSupported := (((CurrentCP.GeoCentric) and (ObserverPos = 'Geocentric')) or
      ((CurrentCP.HelioCentric) and (ObserverPos = 'Heliocentric')));
    if (PeriodSupported and ObserverPosSupported) then begin
      AvailableCPs[j] := CurrentCP;
      Inc(j);
    end;
  end;
end;

procedure TFormDlgPairedCP.PopulateCBoxes;
var
  i, NrOfCPs: integer;
begin
  CBoxCPLeft.Items.Clear;
  CboxCPRight.Items.Clear;
  NrOfCPs := Length(AvailableCPs);
  for i := 0 to NrOfCPs - 1 do begin
    CBoxCPLeft.Items.add(AvailableCPs[i].Name);
    CBoxCPRight.Items.add(AvailableCPs[i].Name);
  end;
  CBoxCPLeft.ItemIndex := 0;
  CboxCPRight.ItemIndex := 1;
end;

procedure TFormDlgPairedCP.SaveState;
begin
  StateMachine.PairedCPs:= SelectedPairs;
end;


end.






