unit unitlinechart;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, Forms, Graphics, StdCtrls, SysUtils, TAChartListbox, TACustomSeries, TAGraph,
  TAIntervalSources, TALegend, TASeries, unitcentralcontroller, unitdomainxchg;

type

  { TFormLineChart }

  TFormLineChart = class(TForm)
    BtnClose: TButton;
    BtnHelp: TButton;
    Chart1: TChart;
    LblAyanamsha: TLabel;
    LblCoordinate: TLabel;
    LblInterval: TLabel;
    LblObserverPos: TLabel;
    LblPeriod: TLabel;
    LineChartDateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TLineSeries;
    Series5: TLineSeries;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
<<<<<<< HEAD

  private
    StateMachine: TFinStateMachine;
    PrevValues: TDoubleArray;
    procedure AddDataAndCheckOverflow(Series: TLineSeries; DateValue: TDateTime; Value, PrevValue: double);
    procedure PopulateSeries;
    procedure DefineLabelsForInfo;
    procedure ConstructSeriesTitles(SplittedLine: TStringList; Cardinality: integer; Paired: boolean);
    procedure ConstructSeriesData(SplittedLine: TStringList; Cardinality: integer; Paired: boolean);
=======
    procedure LineChartDateTimeIntervalChartSourceDateTimeStepChange(Sender: TObject; ASteps: TDateTimeStep);

  private
    StateMachine: TFinStateMachine;
    procedure PopulateSeries;
>>>>>>> 4c1159e96c18c51deb336b4dd65f027a017ac4a1
    function ConstructDate(DateText: string): TDateTime;
  public

  end;

var
  FormLineChart: TFormLineChart;

implementation

{$R *.lfm}

uses
  Math;

{ TFormLineChart }

procedure TFormLineChart.FormCreate(Sender: TObject);
begin
  StateMachine := TFinStateMachine.Create;
<<<<<<< HEAD
  PrevValues := TDoubleArray.Create;
=======
>>>>>>> 4c1159e96c18c51deb336b4dd65f027a017ac4a1
end;

procedure TFormLineChart.FormShow(Sender: TObject);
begin
  PopulateSeries;
end;

<<<<<<< HEAD
=======
procedure TFormLineChart.LineChartDateTimeIntervalChartSourceDateTimeStepChange(Sender: TObject;
  ASteps: TDateTimeStep);
begin

end;


>>>>>>> 4c1159e96c18c51deb336b4dd65f027a017ac4a1

procedure TFormLineChart.PopulateSeries;
var
  DataList, SplittedLine: TStringList;
  i, j, n, Cardinality: integer;
  Paired: boolean;
<<<<<<< HEAD
begin
  Paired := StateMachine.CycleType.Identification <> 'Position';
  DataList := TStringList.Create;
  SplittedLine := TStringList.Create;
  try
    DataList.LoadFromFile(StateMachine.DataFilename);
    DefineLabelsForInfo;
    n := DataList.Count;
    SplittedLine.Delimiter := ';';
    SplittedLine.StrictDelimiter := True;
    SplittedLine.DelimitedText := DataList[0];
    Cardinality := SplittedLine.Count - 3;
    if (Paired) then Cardinality := Cardinality div 3;
    for i := 0 to n - 1 do begin
      SplittedLine.DelimitedText := DataList[i];
      if (i = 0) then ConstructSeriesTitles(SplittedLine, Cardinality, Paired)
      else
        ConstructSeriesData(SplittedLine, Cardinality, Paired);
    end;
  finally
  end;
end;

procedure TFormLineChart.DefineLabelsForInfo;
var
  StartDate, EndDate: TValidatedDate;
  CalText, StartDateText, EndDateText: string;
begin
  LblCoordinate.Caption := StateMachine.Coordinate.Name;
  LblAyanamsha.Caption := 'Ayanamsha: ' + StateMachine.Ayanamsha.Name;
  LblObserverPos.Caption := StateMachine.ObserverPos.Name;
  StartDate := StateMachine.Period.StartDate;
  EndDate := StateMachine.Period.EndDate;
  if (StartDate.Calendar = 1) then CalText := 'Greg.'
  else
    CalText := 'Jul.';
  StartDateText := StartDate.Year.ToString() + '/' + StartDate.Month.toString + '/' + StartDate.Day.toString();
  EndDateText := EndDate.Year.toString() + '/' + EndDate.Month.toString() + '/' + EndDate.Day.toString();
  LblPeriod.Caption := StartDateText + ' - ' + EndDateText + ' ' + CalText;
  LblInterval.Caption := 'Interval: ' + StateMachine.Period.Interval.toString();
end;

procedure TFormLineChart.ConstructSeriesTitles(SplittedLine: TStringList; Cardinality: integer; Paired: boolean);
var
  i: integer;
  Series: TLineSeries;
begin
  for i := 0 to (Cardinality - 1) do begin
    case i of
      0: Series := Series1;
      1: Series := Series2;
      2: Series := Series3;
      3: Series := Series4;
      4: Series := Series5;
    end;
    if (Paired) then Series.Title:= SplittedLine[2 + i * 3] + '-' + SplittedLine[3 + i * 3]
    else Series.Title := SplittedLine[i + 2];
  end;
  for i := Cardinality to 5 do case i of
      1: Series2.ShowInLegend := False;
      2: Series3.ShowInLegend := False;
      3: Series4.ShowInLegend := False;
      4: Series5.ShowInLegend := False;
    end;
end;

procedure TFormLineChart.ConstructSeriesData(SplittedLine: TStringList; Cardinality: integer; Paired: boolean);
var
  ValueText, DateText: string;
  DateValue: TDateTime;
  i: integer;
  Value: double;
  Series: TLineSeries;
begin
  SetLength(PrevValues, 5);
  DateText := trim(SplittedLine[0]);
  DateValue := ConstructDate(DateText);
  for i := 0 to (Cardinality - 1) do begin
    case i of
      0: Series := Series1;
      1: Series := Series2;
      2: Series := Series3;
      3: Series := Series4;
      4: Series := Series5;
    end;
    if (Paired) then begin
      ValueText := trim(SplittedLine[4 + i * 3]);
      Value := ValueText.ToDouble;
      Series.AddXY(DateValue, Value);
    end else begin
      ValueText := trim(SplittedLine[2 + i]);
      Value := ValueText.ToDouble;
      AddDataAndCheckOverflow(Series, DateValue, Value, PrevValues[i]);
      PrevValues[i] := Value;
    end;
  end;
end;

procedure TFormLineChart.AddDataAndCheckOverflow(Series: TLineSeries; DateValue: TDateTime; Value, PrevValue: double);
begin
  if ((abs(Value - PrevValue)) < 300.0) then Series.AddXY(DateValue, Value)
  else
    Series.AddXY(DateValue, NaN);
end;

=======
  Period: TPeriod;
  ValueText, DateText, CalText: string;
  DateValue: TDateTime;
  Value: double;
  PrevValues: TDoubleArray;

begin
  Paired := False;     // TODO check for paired
  DataList := TStringList.Create;
  SplittedLine := TStringList.Create;
  PRevValues:= TDoubleArray.Create;
  try
    SetLength(PrevValues, 5);
    DataList.LoadFromFile(StateMachine.DataFilename);
    LblCoordinate.Caption := StateMachine.Coordinate.Name;
    LblAyanamsha.Caption := 'Ayanamsha: ' + StateMachine.Ayanamsha.Name;
    LblObserverPos.Caption := StateMachine.ObserverPos.Name;
    Period := StateMachine.Period;
    if (Period.StartDate.Calendar = 1) then CalText := 'Greg.'
    else
      CalText := 'Jul.';
    LblPeriod.Caption := Period.StartDate.Year.ToString() + '/' + Period.StartDate.Month.toString +
      '/' + Period.StartDate.Day.toString() + ' - ' + Period.EndDate.Year.toString() + '/' +
      Period.EndDate.Month.toString() + '/' + Period.EndDate.Day.toString() + ' ' + CalText;
    LblInterval.Caption := Period.Interval.toString();
    n := DataList.Count;
    SplittedLine.Delimiter := ';';
    SplittedLine.StrictDelimiter := True;
    for i := 0 to n - 1 do if (i = 0) then begin
        SplittedLine.DelimitedText := DataList[i];
        Cardinality := SplittedLine.Count - 3;
        for j := 0 to Cardinality - 1 do case j of
            0: Series1.Title := SplittedLine[j + 2];
            1: Series2.Title := SplittedLine[j + 2];
            2: Series3.Title := SplittedLine[j + 2];
            3: Series4.Title := SplittedLine[j + 2];
            4: Series5.Title := SplittedLine[j + 2];
          end;
        for j := Cardinality to 5 do case j of
            1: Series2.ShowInLegend := False;
            2: Series3.ShowInLegend := False;
            3: Series4.ShowInLegend := False;
            4: Series5.ShowInLegend := False;
          end;
      end else begin
        SplittedLine.DelimitedText := DataList[i];
        DateText := trim(SplittedLine[0]);
        DateValue := ConstructDate(DateText);
        if (Paired) then for j := 0 to (Cardinality - 1) do begin

            ValueText := trim(SplittedLine[2 + j * 3]);
            Value := ValueText.ToDouble;
            case j of
              0: Series1.AddXY(DateValue, Value);
              1: Series2.AddXY(DateValue, Value);
              2: Series3.AddXY(DateValue, Value);
              3: Series4.AddXY(DateValue, Value);
              4: Series5.AddXY(DateValue, Value);
            end;
          end else
          for j := 0 to (Cardinality - 1) do begin
            ValueText := trim(SplittedLine[2 + j]);
            Value := ValueText.ToDouble;
            case j of
              0: begin
                if ((abs(Value - PrevValues[0])) < 300.0) then Series1.AddXY(DateValue, Value) else Series1.AddXY(DateValue, NaN);
                PrevValues[0]:= Value;
              end;
              1: begin
                if ((abs(Value - PrevValues[1])) < 300.0) then Series2.AddXY(DateValue, Value) else Series2.AddXY(DateValue, NaN);
                PrevValues[1]:= Value;
              end;
              2: begin
                if ((abs(Value - PrevValues[2])) < 300.0) then Series3.AddXY(DateValue, Value) else Series3.AddXY(DateValue, NaN);
                PrevValues[2]:= Value;
              end;
              3: begin
                if ((abs(Value - PrevValues[3])) < 300.0) then Series4.AddXY(DateValue, Value) else Series4.AddXY(DateValue, NaN);
                PrevValues[3]:= Value;
              end;
              4: begin
                if ((abs(Value - PrevValues[4])) < 300.0) then Series5.AddXY(DateValue, Value) else Series5.AddXY(DateValue, NaN);
                PrevValues[4]:= Value;
              end;
            end;

          end;
      end;

  finally
  end;
end;

>>>>>>> 4c1159e96c18c51deb336b4dd65f027a017ac4a1
function TFormLineChart.ConstructDate(DateText: string): TDateTime;
var
  Year, Month, Day: word;
begin
  Year := StrToInt(Copy(DateText, 1, 4));
  Month := StrToInt(Copy(DateText, 6, 2));
  Day := StrToInt(Copy(DateText, 9, 2));
  Result := EncodeDate(Year, Month, Day);
end;

end.

<<<<<<< HEAD
=======
















>>>>>>> 4c1159e96c18c51deb336b4dd65f027a017ac4a1
