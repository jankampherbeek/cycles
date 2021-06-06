unit unitstart;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, ExtCtrls,
  Forms, Graphics, Grids, StdCtrls, SysUtils, TAIntervalSources, unitcentralcontroller,
  unitdomainxchg, UnitProcess, UnitReqResp, unitvalidation;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnOk: TButton;
    BtnHelp: TButton;
    CbCalendar: TComboBox;
    CbAyanamsha: TComboBox;
    CbCoordinate: TComboBox;
    CbCycleType: TComboBox;
    CbCelPoint: TComboBox;
    LblStatus: TLabel;
    LblCelPoint: TLabel;
    LblCycleType: TLabel;
    LblCoordinate: TLabel;
    LblAyanamsha: TLabel;
    LblCalendar: TLabel;
    LblEdEndDate: TLabeledEdit;
    LblEdInterval: TLabeledEdit;
    LblEdStartDate: TLabeledEdit;
    LblTitle: TLabel;
    SgPositions: TStringGrid;
    SgMeta: TStringGrid;
    procedure BtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    CenCon: TCenCon;
    DateTimeValidation: TDateTimeValidation;
    procedure DefineAyanamshaItems;
    procedure DefineCoordinateItems;
    procedure DefineCycleTypeItems;
    procedure DefineCelPoints;
    procedure ProcessStartDate;
    procedure ProcessEndDate;
    procedure CheckDateSequence;
    procedure ProcessInterval;
    procedure CheckCelPoints;
    function DefineRequest: TTimeSeriesRequest;
  public

  end;

var
  Form1: TForm1;
  ErrorText: string;
  ErrorFlag: boolean;

implementation


{$R *.lfm}

uses
  unitapi, unitdata, unitgraph;

var
  ValidatedStartDate, ValidatedEndDate: TValidatedDate;
  StartJD, EndJD: Double;
    SelectedCelPoints: TCelPointSpecArray;
  ValidatedInterval: integer;

const
  ERROR_START_DATE = 'Enter a valid start date.';
  ERROR_END_DATE = 'Enter a valid end date.';
  ERROR_SEQUENCE_OF_DATES = 'Check sequence of dates.';
  ERROR_INTERVAL = 'Enter a valid interval.';
  ERROR_OUT_OF_RANGE = ' not supported for this period.';
  INSTRUCTION = 'Enter all values and click ''Calculate''.';


{ TForm1 }

procedure TForm1.BtnOkClick(Sender: TObject);
var
  Api: TTimeSeriesAPI;
  Request: TTimeSeriesRequest;
  Response: TTimeSeriesResponse;
  DataFilename, MetaFileName: string;
begin
  ErrorText := '';
  ErrorFlag := False;
  LblStatus.Caption := INSTRUCTION;
  LblStatus.Color := clDefault;
  ProcessStartDate;
  ProcessEndDate;
  CheckDateSequence;
  ProcessInterval;
  CheckCelPoints;
  if not ErrorFlag then begin

    SgMeta.Clear;
    SgPositions.Clear;
    Api := TTimeSeriesApi.Create;
    Request := DefineRequest;
    Response := Api.GetTimeSeries(Request);
    MetaFileName := Response.FileNameMeta;
    SgMeta.LoadFromCSVFile(MetaFileName, ';', True, 0, True);
    DataFilename := Response.FileNameData;
    FormGraph.DataFileName := Response.FileNameData;
    SgPositions.LoadFromCSVFile(DataFileName, ';', True, 0, True);
    FormGraph.Close;
    FormGraph.Show;

  end else begin
    LblStatus.Color := clYellow;
    LblStatus.Caption := ErrorText;

  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CenCon := TCenCon.Create;
  DateTimeValidation := TDateTimeValidation.Create;
  DefineAyanamshaItems;
  DefineCoordinateItems;
  DefineCycleTypeItems;
  DefineCelPoints;
  LblStatus.Caption := INSTRUCTION;
end;


procedure TForm1.DefineAyanamshaItems;
var
  AllAyanamshas: TAyanamshaSpecArray;
  i, NrOfAyanamshas: integer;
begin
  AllAyanamshas := CenCon.LookupValues.AllAyanamshas;
  CbAyanamsha.Items.Clear;
  NrOfAyanamshas := Length(AllAyanamshas);
  for i := 0 to NrOfAyanamshas - 1 do CbAyanamsha.Items.add(AllAyanamshas[i].Name);
  CbAyanamsha.ItemIndex := 0;
end;


procedure TForm1.DefineCoordinateItems;
var
  AllCoordinates: TCoordinateSpecArray;
  i, NrOfCoordinates: integer;
begin
  AllCoordinates := CenCon.LookupValues.AllCoordinates;
  CbCoordinate.Items.Clear;
  NrOfCoordinates := Length(AllCoordinates);
  for i := 0 to NrOfCoordinates - 1 do CbCoordinate.Items.add(AllCoordinates[i].Name);
  CbCoordinate.ItemIndex := 0;
end;

procedure TForm1.DefineCycleTypeItems;
var
  AllCycleTypes: TCycleTypeSpecArray;
  i, NrOfCycleTypes: integer;
begin
  AllCycleTypes := CenCon.LookupValues.AllCycleTypes;
  CbCycleType.Items.Clear;
  NrOfCycleTypes := Length(AllCycleTypes);
  for i := 0 to NrOfCycleTypes - 1 do CbCycleType.Items.add(AllCycleTypes[i].Name);
  CbCycleType.ItemIndex := 0;
end;

procedure TForm1.DefineCelPoints;
var
  AllCelPoints: TCelPointSpecArray;
  i, NrOfCelPoints: integer;
begin
  AllCelPOints := CenCon.LookupValues.AllCelPoints;
  CbCelPoint.Items.Clear;
  NrOfCelPOints := LEngth(AllCelPoints);
  for i := 0 to NrOfCelPoints - 1 do CbCelPoint.Items.add(AllCelPoints[i].Name);
  CbCelPoint.ItemIndex := 0;
end;

function TForm1.DefineRequest: TTimeSeriesRequest;
var
  Request: TTimeSeriesRequest;
  Ayanamsha: TAyanamshaSpec;
  CelPoint1, CelPoint2: TCelPointSpec;
begin

  { TODO : Request is now retrieved from UnitProcess. It should be moved to a unit that is exchangeable. Same for response. }
  Request.Calendar := 1;
  if CbCalendar.ItemIndex = 1 then Request.Calendar := 0;
  Request.Ayanamsha := CenCon.LookupValues.AllAyanamshas[CbAyanamsha.ItemIndex];
  Request.CoordinateType := CenCon.LookupValues.AllCoordinates[CbCoordinate.ItemIndex];
  Request.CycleType := CenCon.LookupValues.AllCycleTypes[CbCycleType.ItemIndex];
  Request.StartDateTime := LblEdStartDate.Text;
  Request.EndDateTime := LblEdEndDate.Text;
  Request.Interval := StrToInt(LblEdInterval.Text);
  Request.CelPoints := SelectedCelPoints;
  Result := Request;
end;

procedure TForm1.ProcessStartDate;
var
  Calendar: integer;
begin
  LblEdStartDate.Color := clDefault;
  if CbCalendar.ItemIndex = 1 then Calendar := 0
  else
    Calendar := 1;
  ValidatedStartDate := DateTimeValidation.CheckDate(LblEdStartDate.Text, Calendar);
  if not ValidatedStartDate.IsValid then begin
    ErrorFlag := True;
    ErrorText := ErrorText + ERROR_START_DATE + LineEnding;
    LblEdStartDate.Color := clYellow;
  end else StartJD:= ValidatedStartDate.JulianDay;
end;

procedure TForm1.ProcessEndDate;
var
  Calendar: integer;
begin
  LblEdEndDate.Color := clDefault;
  if CbCalendar.ItemIndex = 1 then Calendar := 0
  else
    Calendar := 1;
  ValidatedEndDate := DateTimeValidation.CheckDate(LblEdEndDate.Text, Calendar);
  if not ValidatedEndDate.IsValid then begin
    ErrorFlag := True;
    ErrorText := ErrorText + ERROR_END_DATE + LineEnding;
    LblEdEndDate.Color := clYellow;
  end else EndJD:= ValidatedEndDate.JulianDay;
end;

procedure TForm1.CheckDateSequence;
begin
  if ValidatedStartDate.IsValid and ValidatedEndDate.IsValid then begin
    LblEdStartDate.Color := clDefault;
    LblEdEndDate.Color := clYellow;
    if (ValidatedStartDate.Year > ValidatedEndDate.Year) or
      ((ValidatedStartDate.Year = ValidatedEndDate.Year) and (ValidatedStartDate.Month > ValidatedEndDate.Month)) or
      ((ValidatedStartDate.Year = ValidatedEndDate.Year) and (ValidatedStartDate.Month = ValidatedEndDate.Month) and
      (ValidatedStartDate.Day <= ValidatedEndDate.Day)) then begin
      ErrorFlag := True;
      ErrorText := ErrorText + ERROR_SEQUENCE_OF_DATES + LineEnding;
      LblEdStartDate.Color := clYellow;
      LblEdEndDate.Color := clYellow;
    end;
  end;
end;

procedure TForm1.ProcessInterval;
var
  ErrorCode: integer;
begin
  LblEdInterval.Color := clDefault;
  Val(LblEdInterval.Text, ValidatedInterval, ErrorCode);
  if ((not ErrorCode = 0) or (ValidatedInterval < 1)) then begin
    ErrorFlag := True;
    ErrorText := ErrorText + ERROR_INTERVAL + LineEnding;
    LblEdInterval.Color := clYellow;
  end;
end;

procedure TForm1.CheckCelPoints;
var
  Count, i: integer;
begin
  SelectedCelPoints := TCelPointSpecArray.Create(CenCon.LookupValues.AllCelPoints[CbCelPoint.ItemIndex]);
  Count:= Length(SelectedCelPoints);
  for i:= 0 to Count-1 do begin
      if ((SelectedCelPoints[i].FirstJd < StartJD) or (SelectedCelPoints[i].LastJD > EndJD)) then begin
        ErrorFlag:= True;
        ErrorText:= ErrorText+ SelectedCelPoints[i].Name + ERROR_OUT_OF_RANGE + LineEnding;
      end;
  end;
end;

end.
