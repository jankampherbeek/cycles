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
    CbCoordinate: TComboBox;
    CbCycleType: TComboBox;
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
    procedure DefineAyanamshaItems;
    procedure DefineCoordinateItems;
    procedure DefineCycleTypeItems;
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
  Api := TTimeSeriesApi.Create;
  Request := DefineRequest;
  Response := Api.GetTimeSeries(Request);
  MetaFileName := Response.FileNameMeta;
  SgMeta.LoadFromCSVFile(MetaFileName, ';', True, 0, True);
  DataFilename := Response.FileNameData;
  Form2.DataFilename := DataFilename;
  SgPositions.LoadFromCSVFile(DataFileName, ';', True, 0, True);
  Form2.Show;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CenCon := TCenCon.Create;
  DefineAyanamshaItems;
  DefineCoordinateItems;
  DefineCycleTypeItems;
end;


procedure TForm1.DefineAyanamshaItems;
var
  AllAyanamshas: TAyanamshaSpecArray;
  i, NrOfAyanamshas: integer;
begin
  AllAyanamshas := CenCon.LookupValues.AllAyanamshas;
  CbAyanamsha.Items.Clear;
  NrOfAyanamshas := Length(AllAyanamshas);
  for i := 0 to NrOfAyanamshas - 1 do
    CbAyanamsha.Items.add(AllAyanamshas[i].Name);
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
  for i := 0 to NrOfCoordinates - 1 do
    CbCoordinate.Items.add(AllCoordinates[i].Name);
  CbCoordinate.ItemIndex := 0;
end;

procedure TForm1.DefineCycleTypeItems;
var
  AllCycleTypes: TCycleTypeSpecArray;
  i, NrOfCycleTypes: integer;
begin
  AllCycleTypes:= CenCon.LookupValues.AllCycleTypes;
  CbCycleType.Items.Clear;
  NrOfCycleTypes:= Length(AllCycleTypes);
  for i:= 0 to NrOfCycleTypes - 1 do
    CbCycleType.Items.add(AllCycleTypes[i].Name);
  CbCycleType.ItemIndex:= 0;
end;

function TForm1.DefineRequest: TTimeSeriesRequest;
var
  Request: TTimeSeriesRequest;
  Ayanamsha: TAyanamshaSpec;
  CelPoint1, CelPoint2: TCelPoint;
  AllCelPoints: TCelPointArray;
begin
  //CelPoint1.SeId := 0;  // Sun
  CelPoint1.SeId:= 14;
  CelPoint1.PresentationName := 'Sun';
  CelPoint1.Name := Sun;
  CelPoint1.FirstJd := 0.0;
  CelPoint1.LastJd := 0.0;
  CelPoint1.Glyph := 'a';
  //CelPoint2.SeId:= 1;  // Moon
  //CelPoint2.PresentationName:= 'Moon';
  //CelPoint2.Name:= Moon;
  //CelPoint2.FirstJd:= 0.0;
  //CelPoint2.LastJd:= 0.0;
  //CelPoint2.Glyph:= 'b';
  AllCelPOints := TCelPointArray.Create(CelPoint1);
  { TODO : Request is now retrieved from UnitProcess. It should be moved to a unit that is exchangeable. Same for response. }


  Request.Calendar := 1;
  if CbCalendar.ItemIndex = 1 then
    Request.Calendar := 0;
  Request.Ayanamsha := CenCon.LookupValues.AllAyanamshas[CbAyanamsha.ItemIndex];
  Request.CoordinateType := CenCon.LookupValues.AllCoordinates[CbCoordinate.ItemIndex];
  Request.CycleType := CenCon.LookupValues.AllCycleTypes[CbCycleType.ItemIndex];
  Request.StartDateTime := LblEdStartDate.Text;
  Request.EndDateTime := LblEdEndDate.Text;
  Request.Interval := StrToInt(LblEdInterval.Text);
  Request.CelPoints := AllCelPoints;
  Result := Request;
end;

end.
