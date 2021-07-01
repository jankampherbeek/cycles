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
    procedure LineChartDateTimeIntervalChartSourceDateTimeStepChange(Sender: TObject; ASteps: TDateTimeStep);

  private
    StateMachine: TFinStateMachine;
    procedure PopulateSeries;
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
end;

procedure TFormLineChart.FormShow(Sender: TObject);
begin
  PopulateSeries;
end;

procedure TFormLineChart.LineChartDateTimeIntervalChartSourceDateTimeStepChange(Sender: TObject;
  ASteps: TDateTimeStep);
begin

end;



procedure TFormLineChart.PopulateSeries;
var
  DataList, SplittedLine: TStringList;
  i, j, n, Cardinality: integer;
  Paired: boolean;
  Period: TPeriod;
  ValueText, DateText, CalText: string;
  DateValue: TDateTime;
  Value: double;
  PrevValues: TDoubleArray;

begin
  Paired := StateMachine.CycleType.Identification <> 'Position';
  DataList := TStringList.Create;
  SplittedLine := TStringList.Create;
  PrevValues := TDoubleArray.Create;
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
        if (Paired) then Cardinality := Cardinality div 3;
        if (Paired) then for j := 0 to Cardinality - 1 do case j of
              0: Series1.Title := SplittedLine[2] + '-' + SplittedLine[3];
              1: Series2.Title := SplittedLine[5] + '-' + SplittedLine[6];
              2: Series3.Title := SplittedLine[8] + '-' + SplittedLine[9];
              3: Series4.Title := SplittedLine[11] + '-' + SplittedLine[12];
              4: Series5.Title := SplittedLine[14] + '-' + SplittedLine[15];
            end else
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

            ValueText := trim(SplittedLine[4 + j * 3]);
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
                if ((abs(Value - PrevValues[0])) < 300.0) then Series1.AddXY(DateValue, Value)
                else
                  Series1.AddXY(DateValue, NaN);
                PrevValues[0] := Value;
              end;
              1: begin
                if ((abs(Value - PrevValues[1])) < 300.0) then Series2.AddXY(DateValue, Value)
                else
                  Series2.AddXY(DateValue, NaN);
                PrevValues[1] := Value;
              end;
              2: begin
                if ((abs(Value - PrevValues[2])) < 300.0) then Series3.AddXY(DateValue, Value)
                else
                  Series3.AddXY(DateValue, NaN);
                PrevValues[2] := Value;
              end;
              3: begin
                if ((abs(Value - PrevValues[3])) < 300.0) then Series4.AddXY(DateValue, Value)
                else
                  Series4.AddXY(DateValue, NaN);
                PrevValues[3] := Value;
              end;
              4: begin
                if ((abs(Value - PrevValues[4])) < 300.0) then Series5.AddXY(DateValue, Value)
                else
                  Series5.AddXY(DateValue, NaN);
                PrevValues[4] := Value;
              end;
            end;

          end;
      end;

  finally
  end;
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








