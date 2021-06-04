unit unitdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, TAGraph, TASources, TASeries, TAGUIConnectorBGRA,
  TACustomSource, TAIntervalSources;

type

  TPopulationRecord = record
    DateText: string;
    JDText: string;
    ValueText: string;
  end;

  TPopulationArray = array of TPopulationRecord;

  // example from: https://wiki.lazarus.freepascal.org/TAChart_Tutorial:_Userdefined_ChartSource

  { TForm2 }

  TForm2 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    ChartGUIConnectorBGRA1: TChartGUIConnectorBGRA;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    UserDefinedChartSource1: TUserDefinedChartSource;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UserDefinedChartSource1GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: integer; var AItem: TChartDataItem);
  private
    FDataFilename: string;
    PopulationData: TPopulationArray;
    procedure LoadPopulationData(const AFileName: string; var Data: TPopulationArray);
  public
    property DataFileName: string write FDataFilename;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);

begin

end;

procedure TForm2.FormShow(Sender: TObject);
//const
//  POPULATION_FILE = '20210530_212247_Sun_Data.csv';
begin

  LoadPopulationData(FDataFilename, PopulationData);
  UserDefinedChartSource1.PointsNumber := Length(PopulationData);
end;

procedure TForm2.UserDefinedChartSource1GetChartDataItem(ASource: TUserDefinedChartSource;
  AIndex: integer; var AItem: TChartDataItem);
var
  DateTime: TDateTime;
  Year, Month, Day :word;
  DateText: string;
begin
  DateText := PopulationData[AIndex].DateText;
  Year:= StrToInt(Copy(DateText, 1, 4));
  Month:= StrToInt(Copy(DateText, 6, 2));
  Day:= StrToInt(Copy(DateText, 9, 2));
  DateTime:= EncodeDate(Year, Month, Day);
  //AItem.X := StrToFloat(PopulationData[AIndex].JDText);
  AItem.X := DateTime;
  AItem.Y := StrToFloat(PopulationData[AIndex].ValueText);
end;


procedure TForm2.LoadPopulationData(const AFileName: string; var Data: TPopulationArray);

var
  List1, List2: TStringList;
  i, j, n: integer;
  s: string;
begin
  List1 := TStringList.Create;
  try
    List1.LoadFromFile(AFileName);
    n := List1.Count;
    SetLength(Data, n - 2);
    List2 := TStringList.Create;
    try
      List2.Delimiter := ';';
      List2.StrictDelimiter := True;
      j := 0;
      for i := 2 to n - 1 do
      begin
        List2.DelimitedText := List1[i];
        s := List1[i];
        with Data[j] do
        begin
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
