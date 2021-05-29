{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitAPI;

{< Unit for API's. This should be the only entrance to units in the backend that do process information.}
{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, UnitProcess, UnitReqResp;

type

  { Handles requests for the calculation of timeseries. }
  TTimeSeriesAPI = class
  strict private
    Handler: TTimeSeriesHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTimeSeries(request: TTimeSeriesRequest): TTimeSeriesResponse;
  end;


implementation

{ TTimeSeriesAPI ----------------------------------------------------------------------------------------------------- }

constructor TTimeSeriesAPI.Create;
begin
  Handler := TTimeSeriesHandler.Create;
end;

destructor TTimeSeriesAPI.Destroy;
begin
  FreeAndNil(Handler);
  Inherited;
end;

function TTimeSeriesAPI.GetTimeSeries(request: TTimeSeriesRequest): TTimeSeriesResponse;
begin
  Result := Handler.HandleRequest(Request);
end;

end.

