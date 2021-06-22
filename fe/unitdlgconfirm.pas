unit unitdlgconfirm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, Forms, Graphics, StdCtrls, SysUtils, unitcentralcontroller, unitdomainxchg;

type

  { TFormDlgConfirm }

  TFormDlgConfirm = class(TForm)
    BtnCancel: TButton;
    BtnConfirm: TButton;
    BtnHelp: TButton;
    LblAyanamsha: TLabel;
    LblAyanamshaValue: TLabel;
    LblCelPoints: TLabel;
    LblCelPointsValue: TLabel;
    LblCoordinate: TLabel;
    LblCoordinateValue: TLabel;
    LblCycleType: TLabel;
    LblCycleTypeValue: TLabel;
    LblEndDate: TLabel;
    LblEndDateValue: TLabel;
    LblInterval: TLabel;
    LblIntervalValue: TLabel;
    LblObserverPos: TLabel;
    LblObserverPosValue: TLabel;
    LblStartDate: TLabel;
    LblStartDateValue: TLabel;
    LblTitle: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnConfirmClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CenCon: TCenCon;
    StateMachine: TFinStateMachine;
    function ConstructCelPointPairs: string;
    function ConstructCelPoints: string;
    function ConstructDate(Date: TValidatedDate): string;
    procedure Populate;

  public

  end;

var
  FormDlgConfirm: TFormDlgConfirm;

implementation

{$R *.lfm}

{ TFormDlgConfirm }

procedure TFormDlgConfirm.FormShow(Sender: TObject);
begin
  CenCon := TCenCon.Create;
  StateMachine := TFinStateMachine.Create;
  Populate;
end;

procedure TFormDlgConfirm.BtnConfirmClick(Sender: TObject);
begin
    Hide;
  StateMachine.ChangeState(Confirmed);
end;

procedure TFormDlgConfirm.BtnCancelClick(Sender: TObject);
begin
    Close;
  StateMachine.ChangeState(Cancel);
end;

procedure TFormDlgConfirm.BtnHelpClick(Sender: TObject);
begin
    ShowMessage('To be replaced with help');
end;

procedure TFormDlgConfirm.Populate;
begin
  LblCycleTypeValue.Caption := StateMachine.CycleType.Name;
  LblAyanamshaValue.Caption := StateMachine.Ayanamsha.Name;
  LblObserverPosValue.Caption := StateMachine.ObserverPos.Name;
  LblCoordinateValue.Caption := StateMachine.Coordinate.Name;
  LblStartDateValue.Caption := ConstructDate(StateMachine.Period.StartDate);
  LblEndDateValue.Caption := ConstructDate(StateMachine.Period.EndDate);
  LblIntervalValue.Caption := IntToStr(StateMachine.Period.Interval);
  if (StateMachine.CycleType.Identification = 'Angle') then LblCelPointsValue.Caption := ConstructCelPointPairs
  else
    LblCelPointsValue.Caption := ConstructCelPoints;
end;

function TFormDlgConfirm.ConstructDate(Date: TValidatedDate): string;
var
  y, m, d, cal, sep: string;
begin
  sep := '/';
  y := IntToStr(Date.Year);
  m := IntToStr(Date.Month);
  d := IntToStr(Date.Day);
  if (Date.Calendar = 1) then cal := 'Gregorian'
  else
    cal := 'Julian';
  Result := y + sep + m + sep + d + ' ' + cal;
end;

function TFormDlgConfirm.ConstructCelPoints: string;
var
  Count, i: integer;
  sep, CpText, CpName: string;
  CelPoints: TCelPointSpecArray;
begin
  sep := ' - ';
  CelPoints := StateMachine.SingleCPs;
  Count := 0;

  for i := 0 to Length(CelPoints) - 1 do begin
    CpName := CelPoints[i].Name;
    if (CelPoints[i].Selected) then begin
      Inc(Count);
      if (Count > 4) then begin
        CpText := CpText + LineEnding;
        Count := 0;
      end;
      if ((i > 0) and (Count > 0)) then CpText := CpText + sep;
      CpText := CpText + CpName;
    end;
  end;
  Result := CpText;
end;

function TFormDlgConfirm.ConstructCelPointPairs: string;
var
  Count, i: integer;
  sep, CpText: string;
  CelPointPairs: TCelPointPairedSpecArray;
begin
  sep := ' - ';
  CelPointPairs := StateMachine.PairedCPs;
  Count := 0;

  for i := 0 to Length(CelPointPairs) - 1 do begin
    Inc(Count);
    if (Length(CelPointPairs[i].FirstCP.Name) > 1) then begin
      if (Count > 2) then begin
        CpText := CpText + LineEnding;
        Count := 0;
      end;
      if ((i > 0) and (Count > 0)) then CpText := CpText + sep;
      CpText := CpText + CelPointPairs[i].FirstCP.Name + ' and ' + CelPointPairs[i].SecondCP.Name;

    end;
  end;
  Result := CpText;
end;

end.
