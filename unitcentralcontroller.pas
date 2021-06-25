{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit unitcentralcontroller;

{< Unit for a set of singletons that manage the applications as a whole.}

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, unitapi, unitdomainxchg, unitinit, unitreqresp;

type

  TStates = (Initialized, Started, Cancelled, DefiningCycleType, DefiningCoordinate, DefiningPeriod, DefiningSingleCP,
    DefiningPairedCP, Confirming, Calculating);
  TStateMessages = (Init, Start, Cancel, CycleTypeDefined, CoordinateDefined, PeriodDefined,
    PairedCPDefined, SingleCPDefined, Confirmed);

  TCenCon = class
  strict private
    FLookupValues: TLookupValues;
  public
    constructor Create;
    property LookupValues: TLookupValues read FLookupValues;
  end;

  { TFinStateMachine }

  TFinStateMachine = class
  strict private
    FCycleType: TCycleTypeSpec;
    FCoordinate: TCoordinateSpec;
    FAyanamsha: TAyanamshaSpec;
    FObserverPos: TObserverPosSpec;
    FPeriod: TPeriod;
    FSingleCPs: TCelPointSpecArray;
    FPairedCPs: TCelPointPairedSpecArray;
    CurrentState: TStates;
    SeriesAPI: TSeriesAPI;
    function CreateRequest: TSeriesRequest;
  public
    constructor Create;
    property Ayanamsha: TAyanamshaSpec read FAyanamsha write FAyanamsha;
    property ObserverPos: TObserverPosSpec read FObserverPos write FObserverPos;
    property CycleType: TCycleTypeSpec read FCycleType write FCycleType;
    property Coordinate: TCoordinateSpec read FCoordinate write FCoordinate;
    property Period: TPeriod read FPeriod write FPeriod;
    property SingleCPs: TCelPointSpecArray read FSingleCPs write FSingleCPs;
    property PairedCPs: TCelPointPairedSpecArray read FPairedCPs write FPairedCPs;
    procedure ChangeState(Message: TStateMessages);
  end;



implementation

uses unitmain, unitdlgconfirm, UnitDlgCoordinate, UnitDlgCycleType, unitdlgpairedcp, UnitDlgPeriod, unitdlgsinglecp;

var
  CenConSingleton: TCenCon = nil;
  FinStateMachineSingleton: TFinStateMachine = nil;

{ TFinStateMachine }

function TFinStateMachine.CreateRequest: TSeriesRequest;
var
  Request: TSeriesRequest;
begin
  if (CycleType.Identification = 'Position') then
    Request := TSeriesSingleRequest.Create(Period, CycleType, Ayanamsha, Coordinate, ObserverPos, SingleCPs)
  else
    Request := TSeriesPairedRequest.Create(Period, CycleType, Ayanamsha, Coordinate, ObserverPos, PairedCPs);
  Result := Request;
end;

constructor TFinStateMachine.Create;
begin
  if not (Assigned(FinStateMachineSingleton)) then begin
    inherited;
    FinStateMachineSingleton := self;
    CurrentState := Initialized;
    SeriesAPI := TSeriesAPI.Create;
  end else
    self := FinStateMachineSingleton;
end;

procedure TFinStateMachine.ChangeState(Message: TStateMessages);
var
  Request: TSeriesRequest;
  Response: TSeriesResponse;
begin
  case Message of
    Init: begin
        CurrentState:= Initialized;
    end;
    Start: begin
      // recreate forms
      FormMain:= TFormMain.Create(Nil);
      FormDlgCycleType := TFormDlgCycleType.Create(Nil);
      FormDlgCoordinate := TFormDlgCoordinate.Create(Nil);
      FormDlgPeriod:= TFormDlgPeriod.Create(Nil);
      FormDlgSingleCP:= TFormDlgSingleCP.Create(Nil);
      FormDlgPairedCP:= TFormDlgPairedCP.Create(Nil);
      FormDlgConfirm:= TFormDlgConfirm.Create(Nil);


      CurrentState := DefiningCycleType;
      FormDlgCycleType.ShowModal;
    end;
    CycleTypeDefined: begin
      CurrentState := DefiningCoordinate;
      FormDlgCoordinate.ShowModal;
    end;
    CoordinateDefined: begin
      CurrentState := DefiningPeriod;
      FormDlgPeriod.ShowModal;
    end;
    PeriodDefined: if (CycleType.Identification = 'Position') then begin
        CurrentState := DefiningSingleCP;
        FormDlgSingleCP.ShowModal;
      end else begin
        CurrentState := DefiningPairedCP;
        FormDlgPairedCP.ShowModal;
      end;
    PairedCpDefined, SingleCPDefined: begin
      CurrentState := Confirming;
      FormDlgConfirm.Show;
    end;
    Confirmed: begin
      Request := CreateRequest;
      Response := SeriesAPI.GetSeries(Request);
      FormMain.Visible:= False;
      FormMain.Populate(Response.FilenameData, Response.FilenameMeta);
      FormMain.ShowModal;
    end;
  end;
end;


constructor TCenCon.Create;
begin
  if not (Assigned(CenConSingleton)) then begin
    inherited;
    CenConSingleton := self;
    FLookUpvalues := TLookupValues.Create;
  end else
    self := CenConSingleton;
end;

end.
