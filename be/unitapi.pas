{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitAPI;

{< Unit for API's. This should be the only entrance to units in the backend that do process information.}
{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, unitdomainxchg, UnitProcess, UnitReqResp;

type




  { API for series. }

  { TSeriesAPI }

  TSeriesAPI = class
  strict private
    Series: TSeries;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSeries(request: TSeriesRequest): TSeriesResponse;
  end;


implementation


uses
  UnitAstron, UnitConversions;

{ TSeriesAPI }

constructor TSeriesAPI.Create;
begin
  //Handler := TSeriesHandler.Create;
end;

destructor TSeriesAPI.Destroy;
begin
  //FreeAndNil(Handler);
  inherited Destroy;
end;

function TSeriesAPI.GetSeries(Request: TSeriesRequest): TSeriesResponse;
var
  Ephemeris: TEphemeris;
  JulianDayConversion: TJulianDayConversion;
  FloatingToDecimalDegreeConversion: TFloatingToDecimalDegreeConversion;
  CycleDefinition: TCycleDefinition;
  Response: TSeriesResponse;
begin
  CycleDefinition.JdStart:= Request.Period.StartDate.JulianDay;
  CycleDefinition.JdEnd:= Request.Period.EndDate.JulianDay;
  CycleDefinition.Calendar:= Request.Period.StartDate.Calendar;
  CycleDefinition.Interval:= Request.Period.Interval;
  CycleDefinition.CoordinateType:= Request.Coordinate;
  CycleDefinition.Ayanamsha:= Request.Ayanamsha;
  CycleDefinition.CycleType:= Request.CycleType;
  CycleDefinition.ObserverPos:= Request.ObserverPos;

  Ephemeris:= TEphemeris.Create;
  JulianDayConversion:= TJulianDayConversion.Create(Ephemeris.SeFrontend);
  FloatingToDecimalDegreeConversion:= TFloatingToDecimalDegreeConversion.Create;
  Series:= TSeries.Create(Ephemeris, JulianDayConversion, FloatingToDecimalDegreeConversion);
  if (CycleDefinition.CycleType.Identification = 'Position') then
    Response:= Series.HandleCycle(CycleDefinition, (Request as TSeriesSingleRequest).CelPoints)
  else Response:= Series.HandleCycle(CycleDefinition, (Request as TSeriesPairedRequest).CelPointPairs);
  Result:= Response;
end;



end.

