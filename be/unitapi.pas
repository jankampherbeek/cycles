{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitAPI;

{< Unit for API's. This should be the only entrance to units in the backend that do process information.}
{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, UnitProcess, UnitReqResp;

type

  { TSeriesAPI }

  TSeriesAPI = class
  strict private
    Handler: TSeriesHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSeriesForSingleCPs(request: TSeriesSingleRequest): TSeriesResponse;
    function GetSeriesForPairedCPs(request: TSeriesPairedRequest): TSeriesResponse;
  end;


  { Handles requests for the calculation of timeseries. }
  TTimeSeriesAPI = class
  strict private
    Handler: TSeriesHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTimeSeries(request: TSeriesSingleRequest): TSeriesResponse;
  end;


implementation

{ TSeriesAPI }

constructor TSeriesAPI.Create;
begin
  Handler:= TSeriesHandler.Create;
end;

destructor TSeriesAPI.Destroy;
begin
  FreeAndNil(Handler);
  inherited Destroy;
end;

function TSeriesAPI.GetSeriesForSingleCPs(request: TSeriesSingleRequest): TSeriesResponse;
begin

end;

function TSeriesAPI.GetSeriesForPairedCPs(request: TSeriesPairedRequest): TSeriesResponse;
begin

end;

{ TTimeSeriesAPI ----------------------------------------------------------------------------------------------------- }

constructor TTimeSeriesAPI.Create;
begin
  Handler := TSeriesHandler.Create;
end;

destructor TTimeSeriesAPI.Destroy;
begin
  FreeAndNil(Handler);
  inherited;
end;

function TTimeSeriesAPI.GetTimeSeries(request: TSeriesSingleRequest): TSeriesResponse;
begin
  //Result := Handler.HandleRequest(Request);
end;

end.

