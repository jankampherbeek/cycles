unit unitstart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, TAIntervalSources, unitdomainxchg, UnitProcess;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnOk: TButton;
    BtnHelp: TButton;
    CbCalendar: TComboBox;
    LblCalendar: TLabel;
    LblEdEndDate: TLabeledEdit;
    LblEdInterval: TLabeledEdit;
    LblEdStartDate: TLabeledEdit;
    LblTitle: TLabel;
    SgPositions: TStringGrid;
    procedure BtnOkClick(Sender: TObject);
  private
    function DefineRequest: TTimeSeriesRequest;
  public

  end;

var
  Form1: TForm1;

implementation


{$R *.lfm}

uses
  unitastron, unitapi;

{ TForm1 }

procedure TForm1.BtnOkClick(Sender: TObject);
var
  Api: TTimeSeriesAPI;
  Request: TTimeSeriesRequest;
  Response: TTimeSeriesResponse;
begin
  Api:= TTimeSeriesApi.Create;
  Request:= DefineRequest;
  Response:= Api.GetTimeSeries(Request);
  SgPositions.LoadFromCSVFile('xxxdata.csv',';', true, 0, true);

end;


function TForm1.DefineRequest: TTimeSeriesRequest;
var
  Request: TTimeSeriesRequest;
  Ayanamsha: TAyanamsha;
  CelPoint1, CelPoint2: TCelPoint;
  AllCelPoints: TCelPointArray;
begin
  Ayanamsha.Name:= None;
  Ayanamsha.PresentationName:= 'Tropical';
  Ayanamsha.SeId:= -1;
  CelPoint1.SeId:= 0;  // Sun
  CelPoint1.PresentationName:= 'Sun';
  CelPoint1.Name:= Sun;
  CelPoint1.FirstJd:= 0.0;
  CelPoint1.LastJd:= 0.0;
  CelPoint1.Glyph:= 'a';
  //CelPoint2.SeId:= 1;  // Moon
  //CelPoint2.PresentationName:= 'Moon';
  //CelPoint2.Name:= Moon;
  //CelPoint2.FirstJd:= 0.0;
  //CelPoint2.LastJd:= 0.0;
  //CelPoint2.Glyph:= 'b';
  AllCelPOints:= TCelPointArray.Create(CelPoint1);
  { TODO : Request is now retrieved from UnitProcess. It should be moved to a unit that is exchangeable. Same for response. }
  Request.Ayanamsha:= Ayanamsha;
  Request.Calendar:= CbCalendar.ItemIndex;
  Request.CoordinateType:= GeoLongitude;
  Request.CycleType:= SinglePoint;
  Request.StartDateTime:= LblEdStartDate.Text;
  Request.EndDateTime:= LblEdEndDate.Text;
  Request.Interval := StrToInt(LblEdInterval.Text);
  Request.CelPoints:= AllCelPoints;
  Result:= Request;
end;

end.

