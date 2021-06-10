{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, Forms, Graphics, SysUtils, TAGraph, TAGUIConnectorBGRA, TASeries;

type

  TPopulationRecord = record
    DateText: string;
    JDText: string;
    ValueText: string;
  end;

  TPopulationArray = array of TPopulationRecord;


  { TFormGraph }

  TFormGraph = class(TForm)
    Chart: TChart;
    ChartGUIConnectorBGRA1: TChartGUIConnectorBGRA;
    ChartLineSeries: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDataFilename: string;
    PopulationData: TPopulationArray;
    procedure LoadPopulationData(var Data: TPopulationArray);
    procedure PopulateGraph;
  public
    property DataFileName: string write FDataFilename;
  end;

var
  FormGraph: TFormGraph;

implementation

{$R *.lfm}

{ TFormGraph }

procedure TFormGraph.FormCreate(Sender: TObject);
begin

end;

procedure TFormGraph.FormShow(Sender: TObject);
begin
  ChartLineSeries.Clear;
  PopulateGraph;
  Chart.BottomAxis.Marks.Source := ChartLineSeries.ListSource;
end;


procedure TFormGraph.PopulateGraph;
var
  DateText: string;
  DataSize, i: integer;
begin
{ TODO : Add title, should be using TStrings. Use parameters for coordinate and cycletype to define title }
  LoadPopulationData(PopulationData);
  DataSize := Length(PopulationData);
  ChartLineSeries.Title := 'Cel. point';  { TODO : Pass name(s) of celpoints to graph to show in legend }
  for i := 0 to DataSize - 1 do begin
    DateText := PopulationData[i].DateText;
    ChartLineSeries.AddXy(i, StrToFloat(PopulationData[i].ValueText), DateText);
  end;
end;

procedure TFormGraph.LoadPopulationData(var Data: TPopulationArray);

var
  List1, List2: TStringList;
  i, j, n: integer;
  s: string;
begin
  List1 := TStringList.Create;
  try
    List1.LoadFromFile(FDataFileName);
    n := List1.Count;
    Data := TPopulationArray.Create;
    SetLength(Data, n - 2);
    List2 := TStringList.Create;
    try
      List2.Delimiter := ';';
      List2.StrictDelimiter := True;
      j := 0;
      for i := 2 to n - 1 do begin
        List2.DelimitedText := List1[i];
        s := List1[i];
        with Data[j] do begin
          DateText := trim(List2[0]);
          JDText := trim(List2[1]);
          ValueText := trim(List2[2]);
        end;
        Inc(j);
      end;
    finally
      List2.Free;
    end;
  finally
    List1.Free;
  end;
end;

end.
