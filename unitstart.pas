unit unitstart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, TAIntervalSources, unitdomainxchg, UnitProcess, UnitReqResp, unitcentralcontroller;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnOk: TButton;
    BtnHelp: TButton;
    CbCalendar: TComboBox;
    CbAyanamsha: TComboBox;
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
    procedure DefineAyanamshaItems;
    function DefineRequest: TTimeSeriesRequest;
  public

  end;

var
  Form1: TForm1;

implementation


{$R *.lfm}

uses
  unitapi, unitdata;

{ TForm1 }

procedure TForm1.BtnOkClick(Sender: TObject);
var
  Api: TTimeSeriesAPI;
  Request: TTimeSeriesRequest;
  Response: TTimeSeriesResponse;
  DataFilename, MetaFileName: string;
begin
  SgMeta.Clear;
  SgPositions.Clear;
  Api:= TTimeSeriesApi.Create;
  Request:= DefineRequest;
  Response:= Api.GetTimeSeries(Request);
  MetaFileName:= Response.FileNameMeta;
  SgMeta.LoadFromCSVFile(MetaFileName, ';', true, 0, true);
  DataFilename:= Response.FileNameData;
  Form2.DataFilename:= DataFilename;
  SgPositions.LoadFromCSVFile(DataFileName,';', true, 0, true);
  Form2.Show;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CenCon:= TCenCon.Create;
  DefineAyanamshaItems;
end;


procedure TForm1.DefineAyanamshaItems;
var
  AllAyanamshas: TAyanamshaSpecArray;
  i, NrOfAyanamshas: Integer;
begin
  AllAyanamshas:= CenCon.LookupValues.AllAyanamshas;
    CbAyanamsha.Items.Clear;
  NrOfAyanamshas:= Length(AllAyanamshas);
  for i:=0 to NrOfAyanamshas-1 do
  begin
    CbAyanamsha.Items.add(AllAyanamshas[i].Name);
  end;
  CbAyanamsha.ItemIndex:= 0;
end;

function TForm1.DefineRequest: TTimeSeriesRequest;
var
  Request: TTimeSeriesRequest;
  Ayanamsha: TAyanamshaSpec;
  AyanamshaIndex: Integer;
  CelPoint1, CelPoint2: TCelPoint;
  AllCelPoints: TCelPointArray;
begin
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

  AyanamshaIndex:= CbAyanamsha.ItemIndex;
  Request.Ayanamsha:= CenCon.LookupValues.AllAyanamshas[AyanamshaIndex];
  Request.Calendar:= 1;
  if CbCalendar.ItemIndex = 1 then Request.Calendar := 0;
  Request.CoordinateType:= GeoLongitude;
  Request.CycleType:= SinglePoint;
  Request.StartDateTime:= LblEdStartDate.Text;
  Request.EndDateTime:= LblEdEndDate.Text;
  Request.Interval := StrToInt(LblEdInterval.Text);
  Request.CelPoints:= AllCelPoints;
  Result:= Request;
end;

end.

