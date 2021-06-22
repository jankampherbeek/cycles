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
    procedure CbCelPointEditingDone(Sender: TObject);
    procedure CbCoordinateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LblEdEndDateEditingDone(Sender: TObject);
    procedure LblEdIntervalEditingDone(Sender: TObject);
    procedure LblEdStartDateEditingDone(Sender: TObject);
  private
    CenCon: TCenCon;
    FinStateMachine: TFinStateMachine;
    DateTimeValidation: TDateTimeValidation;
    procedure DefineAyanamshaItems;
    procedure DefineCoordinateItems;
    procedure DefineCycleTypeItems;
    procedure DefineCelPointsGeneral;
    procedure DefineCelPointsWaves;
    procedure ProcessStartDate;
    procedure ProcessEndDate;
    procedure CheckDateSequence;
    procedure ProcessInterval;
    procedure CheckStatus;
    //function DefineRequest: TSeriesSingleRequestObs;
  public

  end;

var
  Form1: TForm1;

implementation


{$R *.lfm}

uses
  unitapi, unitdata, unitgraph;

var
  ValidatedStartDate, ValidatedEndDate: TValidatedDate;
  SelectedCycleType: TCycleTypeSpec;
  StartJD, EndJD: double;
  AvailableCelPoints, SelectedCelPoints: TCelPointSpecArray;
  ValidatedInterval: integer;
  StartDateOk, EndDateOk, DateSequenceOk, IntervalOk: boolean;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

    FinStateMachine:= TFinStateMachine.Create;

  CenCon := TCenCon.Create;
  StartDateOk := False;
  EndDateOk := False;
  DateSequenceOk := False;
  IntervalOk := False;

  DateTimeValidation := TDateTimeValidation.Create;
  DefineAyanamshaItems;
  DefineCoordinateItems;
  DefineCycleTypeItems;
  CbCelPoint.Enabled := False;
  BtnOk.Enabled := False;


end;

procedure TForm1.FormShow(Sender: TObject);
begin
    FinStateMachine.ChangeState(Start);
end;


procedure TForm1.CbCoordinateChange(Sender: TObject);
begin
  DefineCelPointsGeneral;
end;



procedure TForm1.LblEdEndDateEditingDone(Sender: TObject);
begin
  ProcessEndDate;
  CheckDateSequence;
  CheckStatus;
end;

procedure TForm1.LblEdIntervalEditingDone(Sender: TObject);
begin
  ProcessInterval;
  CheckStatus;
end;


procedure TForm1.LblEdStartDateEditingDone(Sender: TObject);
begin
  ProcessStartDate;
  CheckDateSequence;
  CheckStatus;
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
  SelectedCycleType := AllCycleTypes[0];
end;

procedure TForm1.DefineCelPointsGeneral;
var
  AllCelPoints: TCelPointSpecArray;
  CurrentCP: TCelPointSpec;
  FirstCpFound: boolean;
  i, SelectedSeId, NrOfCelPoints, CoordIndex, FirstCpIndex, AvailableIndex: integer;
begin
  FirstCpFound := False;
  AvailableIndex := 0;
  AllCelPOints := CenCon.LookupValues.AllCelPoints;
  SelectedSeId := CbCelPoint.ItemIndex;
  CbCelPoint.Items.Clear;
  NrOfCelPoints := Length(AllCelPoints);
  CoordIndex := CbCoordinate.ItemIndex;
  for i := 0 to NrOfCelPoints - 1 do begin
    CurrentCP := AllCelPoints[i];
    if ((((CoordIndex in [0, 2, 4, 5]) and (CurrentCP.GeoCentric)) or  // GeoLong, GeolAt, RA, Decl
      ((CoordIndex in [1, 3]) and (CurrentCP.HelioCentric)) or          // HelioLong, HelioLat
      ((CoordIndex = 6) and (CurrentCP.Distance))) and                  // Distance (RADV)
      ((CurrentCP.FirstJd <= StartJD) and (CurrentCP.LastJD >= EndJD))) then begin     // supported period
      CbCelPoint.Items.add(CurrentCP.Name);
      AvailableCelPoints[AvailableIndex] := CurrentCP;    { TODO : Check for max length of array }
      Inc(AvailableIndex);
      if (not FirstCpFound) then begin
        FirstCpFound := True;
        CbCelPoint.ItemIndex := 0;
      end;
      if (CurrentCP.SeId = SelectedSeId) then begin
        CbCelPoint.ItemIndex := i;
        SelectedCelPoints[0] := CurrentCP;
      end;
    end;
  end;
end;

procedure TForm1.DefineCelPointsWaves;
var
  AllCelPoints: TCelPointSpecArray;
  CurrentCP: TCelpointSpec;
  AvailableIndex, i, NrOfCelPoints: integer;
begin
  CbCelPoint.Items.Clear;
  AllCelPOints := CenCon.LookupValues.AllCelPoints;
  NrOfCelPoints := Length(AllCelPoints);
  for i := 0 to NrOfCelPoints - 1 do begin
    CurrentCP := AllCelPoints[i];
    if ((CurrentCP.Identification = 'Jupiter') or (CurrentCP.Identification = 'Saturn') or (CurrentCP.Identification = 'Uranus') or
      (CurrentCP.Identification = 'Neptune') or (CurrentCP.Identification = 'Pluto')) then begin
      CbCelPoint.Items.add(CurrentCP.Name);
      AvailableCelPoints[AvailableIndex] := CurrentCP;
      Inc(AvailableIndex);
    end;
  end;
end;


procedure TForm1.CbCelPointEditingDone(Sender: TObject);
var
  CelPointIndex: integer;
begin
  CelPointIndex := CbCelPoint.ItemIndex;
  SelectedCelPoints[0] := AvailableCelPoints[CelPointIndex];
end;


//function TForm1.DefineRequest: TSeriesSingleRequestObs;
//var
//  Request: TSeriesSingleRequestObs;
//  Ayanamsha: TAyanamshaSpec;
//begin
//  { TODO : Request is now retrieved from UnitProcess. It should be moved to a unit that is exchangeable. Same for response. }
//  //Request.Calendar := 1;
//  //if CbCalendar.ItemIndex = 1 then Request.Calendar := 0;
//  Request.Ayanamsha := CenCon.LookupValues.AllAyanamshas[CbAyanamsha.ItemIndex];
//  Request.Coordinate := CenCon.LookupValues.AllCoordinates[CbCoordinate.ItemIndex];
//  Request.CycleType := CenCon.LookupValues.AllCycleTypes[CbCycleType.ItemIndex];
//  //Request.StartDateTime := LblEdStartDate.Text;
//  //Request.EndDateTime := LblEdEndDate.Text;
//  //Request.StartJd := StartJD;
//  //Request.EndJd := EndJd;
//  //Request.Interval := StrToInt(LblEdInterval.Text);
//  Request.CelPoints := SelectedCelPoints;
//  Result := Request;
//end;

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
    LblEdStartDate.Color := clYellow;
    StartDateOk := False;
  end else begin
    StartJD := ValidatedStartDate.JulianDay;
    LblEdStartDate.Color := clDefault;
    ;
    StartDateOk := True;
  end;
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
    LblEdEndDate.Color := clYellow;
    EndDateOk := False;
  end else begin
    EndJD := ValidatedEndDate.JulianDay;
    LblEdEndDate.Color := clDefault;
    EndDateOk := True;
  end;
end;

procedure TForm1.CheckDateSequence;
begin
  if ValidatedStartDate.IsValid and ValidatedEndDate.IsValid then begin
    DateSequenceOk := True;
    LblEdStartDate.Color := clDefault;
    LblEdEndDate.Color := clDefault;
    if (ValidatedStartDate.Year > ValidatedEndDate.Year) or
      ((ValidatedStartDate.Year = ValidatedEndDate.Year) and (ValidatedStartDate.Month > ValidatedEndDate.Month)) or
      ((ValidatedStartDate.Year = ValidatedEndDate.Year) and (ValidatedStartDate.Month = ValidatedEndDate.Month) and
      (ValidatedStartDate.Day <= ValidatedEndDate.Day)) then begin
      DateSequenceOk := False;
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
  if ((ErrorCode = 0) and (ValidatedInterval > 0)) then begin
    LblEdInterval.Color := clDefault;
    IntervalOk := True;
  end else begin
    LblEdInterval.Color := clYellow;
    IntervalOk := False;
  end;
end;


procedure TForm1.CheckStatus;
begin
  if (StartDateOk and EndDateOk and DateSequenceOk and IntervalOk) then begin
    CbCelPoint.Enabled := True;
    DefineCelPointsGeneral;
    if CbCelPoint.ItemIndex >= 0 then BtnOk.Enabled := True;
  end else begin
    CbCelPoint.Enabled := False;
    BtnOk.Enabled := False;
  end;
end;

procedure TForm1.BtnOkClick(Sender: TObject);
var
  Api: TSeriesAPI;
  //Request: TSeriesSingleRequestObs;
  //Response: TSeriesResponseObs;
  DataFilename, MetaFileName: string;
begin
  SgMeta.Clear;
  SgPositions.Clear;
  //Api := TSeriesApi.Create;
  //Request := DefineRequest;
  //Response := Api. GetTimeSeries(Request);
  //MetaFileName := Response.FileNameMeta;
  //SgMeta.LoadFromCSVFile(MetaFileName, ';', True, 0, True);
  //DataFilename := Response.FileNameData;
  //FormGraph.DataFileName := Response.FileNameData;
  //SgPositions.LoadFromCSVFile(DataFileName, ';', True, 0, True);
  //FormGraph.Close;
  //FormGraph.Show;
end;




end.
