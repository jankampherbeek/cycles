unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, ExtCtrls,
  Forms, Graphics, Grids, StdCtrls, SysUtils, TAIntervalSources, unitcentralcontroller,
  unitdomainxchg, UnitProcess, UnitReqResp, unitvalidation;

type

  { TFormMain }

  TFormMain = class(TForm)
    BtnExit: TButton;
    BtnFreq: TButton;
    BtnLineChart: TButton;
    BtnNew: TButton;
    BtnHelp: TButton;
    LblTitle: TLabel;
    SgPositions: TStringGrid;
    SgMeta: TStringGrid;
    procedure BtnExitClick(Sender: TObject);
    procedure BtnFreqClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnLineChartClick(Sender: TObject);
    procedure BtnNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Populate(FilenameData, FilenameMeta: string);
  private
    CenCon: TCenCon;
    FinStateMachine: TFinStateMachine;
    procedure CheckStatus;
  public

  end;

var
  FormMain: TFormMain;

implementation


{$R *.lfm}

uses
  unitapi, unitlinechart;


{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FinStateMachine := TFinStateMachine.Create;
  CenCon := TCenCon.Create;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  FinStateMachine.ChangeState(Init);
end;

procedure TFormMain.Populate(FilenameData, FilenameMeta: string);
begin
  SgMeta.LoadFromCSVFile(FilenameMeta, ';', True, 0, True);
  SgPositions.LoadFromCSVFile(FilenameData, ';', True, 0, True);
  // enable buttons for graphics and freq
end;


procedure TFormMain.CheckStatus;
begin

end;

procedure TFormMain.BtnNewClick(Sender: TObject);

begin
  SgMeta.Clear;
  SgPositions.Clear;
  FinStateMachine.ChangeState(Start);

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

procedure TFormMain.BtnHelpClick(Sender: TObject);
begin
  ShowMessage('To be replaced with help');
end;

procedure TFormMain.BtnFreqClick(Sender: TObject);
begin
  // open FormFreq
end;

procedure TFormMain.BtnExitClick(Sender: TObject);
begin
  // send msg 'exit' to central controller

end;

procedure TFormMain.BtnLineChartClick(Sender: TObject);
begin
  FormLineChart.ShowModal;                       { TODO : Handle via StateMachine }
end;




end.
