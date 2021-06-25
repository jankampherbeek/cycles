unit UnitDlgPeriod;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, StdCtrls, SysUtils, unitcentralcontroller,
  unitdomainxchg, UnitValidation;

type

  { TFormDlgPeriod }

  TFormDlgPeriod = class(TForm)
    BtnCancel: TButton;
    BtnContinue: TButton;
    BtnHelp: TButton;
    CbCalendar: TComboBox;
    LblCalendar: TLabel;
    LblEditEndDate: TLabeledEdit;
    LblEditInterval: TLabeledEdit;
    LblEditStartDate: TLabeledEdit;
    LblTitle: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LblEditEndDateEditingDone(Sender: TObject);
    procedure LblEditIntervalEditingDone(Sender: TObject);
    procedure LblEditStartDateEditingDone(Sender: TObject);
  private
    DateTimeValidation: TDateTimeValidation;
    CenCon: TCenCon;
    StateMachine: TFinStateMachine;
    procedure CheckDateSequence;
    procedure CheckStatus;
    function CreatePeriod: TPeriod;
    procedure ProcessEndDate;
    procedure ProcessInterval;
    procedure ProcessStartDate;
    procedure SaveState;

  public

  end;

var
  FormDlgPeriod: TFormDlgPeriod;

implementation


{$R *.lfm}

var
  ValidatedStartDate, ValidatedEndDate: TValidatedDate;
  ValidatedInterval: integer;
  StartJD, EndJD: double;
  StartDateOk, EndDateOk, DateSequenceOk, IntervalOk: boolean;

procedure TFormDlgPeriod.FormCreate(Sender: TObject);
begin
  CenCon := TCenCon.Create;
  StateMachine := TFinStateMachine.Create;
  DateTimeValidation := TDateTimeValidation.Create;
end;

procedure TFormDlgPeriod.FormShow(Sender: TObject);
begin
  ValidatedInterval:= 1;
  IntervalOk:= true;
end;

procedure TFormDlgPeriod.BtnContinueClick(Sender: TObject);
begin
  Hide;
  SaveState;
  StateMachine.ChangeState(PeriodDefined);
end;

procedure TFormDlgPeriod.BtnCancelClick(Sender: TObject);
begin
  Close;
  StateMachine.ChangeState(Cancel);
end;

procedure TFormDlgPeriod.BtnHelpClick(Sender: TObject);
begin
  ShowMessage('To be replaced with help');
end;

procedure TFormDlgPeriod.SaveState;
begin
  StateMachine.Period := CreatePeriod;
end;

procedure TFormDlgPeriod.LblEditEndDateEditingDone(Sender: TObject);
begin
  ProcessEndDate;
  CheckDateSequence;
  CheckStatus;
end;

procedure TFormDlgPeriod.LblEditIntervalEditingDone(Sender: TObject);
begin
  ProcessInterval;
  CheckStatus;
end;



procedure TFormDlgPeriod.LblEditStartDateEditingDone(Sender: TObject);
begin
  ProcessStartDate;
  CheckDateSequence;
  CheckStatus;
end;


procedure TFormDlgPeriod.ProcessStartDate;
var
  Calendar: integer;
begin
  LblEditStartDate.Color := clDefault;
  if CbCalendar.ItemIndex = 1 then Calendar := 0
  else
    Calendar := 1;
  ValidatedStartDate := DateTimeValidation.CheckDate(LblEditStartDate.Text, Calendar);
  if not ValidatedStartDate.IsValid then begin
    LblEditStartDate.Color := clYellow;
    StartDateOk := False;
  end else begin
    StartJD := ValidatedStartDate.JulianDay;
    LblEditStartDate.Color := clDefault;
    StartDateOk := True;
  end;
end;

procedure TFormDlgPeriod.ProcessEndDate;
var
  Calendar: integer;
begin
  LblEditEndDate.Color := clDefault;
  if CbCalendar.ItemIndex = 1 then Calendar := 0
  else
    Calendar := 1;
  ValidatedEndDate := DateTimeValidation.CheckDate(LblEditEndDate.Text, Calendar);
  if not ValidatedEndDate.IsValid then begin
    LblEditEndDate.Color := clYellow;
    EndDateOk := False;
  end else begin
    EndJD := ValidatedEndDate.JulianDay;
    LblEditEndDate.Color := clDefault;
    EndDateOk := True;
  end;
end;



procedure TFormDlgPeriod.CheckDateSequence;
begin
  if ValidatedStartDate.IsValid and ValidatedEndDate.IsValid then begin
    DateSequenceOk := True;
    LblEditStartDate.Color := clDefault;
    LblEditEndDate.Color := clDefault;
    if (ValidatedStartDate.Year > ValidatedEndDate.Year) or
      ((ValidatedStartDate.Year = ValidatedEndDate.Year) and (ValidatedStartDate.Month > ValidatedEndDate.Month)) or
      ((ValidatedStartDate.Year = ValidatedEndDate.Year) and (ValidatedStartDate.Month = ValidatedEndDate.Month) and
      (ValidatedStartDate.Day >= ValidatedEndDate.Day)) then begin
      DateSequenceOk := False;
      LblEditStartDate.Color := clYellow;
      LblEditEndDate.Color := clYellow;
    end;
  end;
end;

procedure TFormDlgPeriod.ProcessInterval;
var
  ErrorCode: integer;
begin
  LblEditInterval.Color := clDefault;
  Val(LblEditInterval.Text, ValidatedInterval, ErrorCode);
  if ((ErrorCode = 0) and (ValidatedInterval > 0)) then begin
    LblEditInterval.Color := clDefault;
    IntervalOk := True;
  end else begin
    LblEditInterval.Color := clYellow;
    IntervalOk := False;
  end;
end;

function TFormDlgPeriod.CreatePeriod: TPeriod;
var
  Period: Tperiod;
begin
  Period.StartDate := ValidatedStartDate;
  Period.EndDate := ValidatedEndDate;
  Period.Interval := ValidatedInterval;
  Result:= Period;
end;

procedure TFormDlgPeriod.CheckStatus;
begin
  BtnContinue.Enabled := (StartDateOk and EndDateOk and DateSequenceOk and IntervalOk);
end;



end.
