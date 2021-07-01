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

  private
    StateMachine: TFinStateMachine;
    PrevValues: TDoubleArray;
    procedure AddDataAndCheckOverflow(Series: TLineSeries; DateValue: TDateTime; Value, PrevValue: double);
    procedure PopulateSeries;
    procedure DefineLabelsForInfo;
    procedure ConstructSeriesTitles(SplittedLine: TStringList; Cardinality: integer; Paired: boolean);
    procedure ConstructSeriesData(SplittedLine: TStringList; Cardinality: integer; Paired: boolean);
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
  PrevValues := TDoubleArray.Create;
end;

procedure TFormLineChart.FormShow(Sender: TObject);
begin
  PopulateSeries;
end;


procedure TFormLineChart.PopulateSeries;
var
  DataList, SplittedLine: TStringList;
  i, j, n, Cardinality: integer;
  Paired: boolean;
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

