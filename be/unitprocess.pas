{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitProcess;

{< Classes for processing calculations. }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitdomainxchg, unitastron;

type

  { Flags as used by then Swiss Ephemeris. Assumes that always the SE-files are used and thart speed has always to be calculated.}
  TSeFlags = class
  strict private
    FFlagsValue: longint;
    FCoordinateType: TCoordinateTypes;
    FAyanamsha: TAyanamshaNames;
    function DefineFlags: longint;
  public
    constructor Create(PCoordinateType: TCoordinateTypes; PAyanamsha: TAyanamshaNames);
    property FlagsValue: longint read FFlagsValue;
  end;

  TTimeSeriesRequest = record
    StartDateTime: string;
    EndDateTime: string;
    Calendar: Integer;
    Interval: Integer;
    CoordinateType: TCoordinateTypes;
    CycleType: TCycleTypes;
    Ayanamsha: TAyanamsha;
    CelPoints: TCelPointArray;
  end;

  { Constructs a TimeSeries for a specific celestial point, and the specs found in a CycleDefinition. }
  TTimeSeries = class
  strict private
    FCelPoint: TCelPoint;
    FCycleDefinition: TCycleDefinition;
    FTimedPositions: TList;   // filled with TTimedPosition
    FEphemeris: TEphemeris;
    function calculate: TList;
  public
    constructor Create(PEphemeris: TEphemeris; PCelPoint: TCelPoint; PCycleDefinition: TCycleDefinition);
    property TimedPositions: TList read FTimedPositions;
  end;

  TTimeSeriesArray = array of TTimeSeries;

  TTimeSeriesResponse = record
    Errors: boolean;
    ErrorText: string;
    CalculatedTimeSeries: TTimeSeriesArray;
  end;

  { Converter for date and time. Returns Julian day for UT for date in format yyyy/mm/dd.
    Calendar: 1 = Gregorian, 0 = Julian. }
  TDateTimeConversion = class
  strict private
    FEphemeris: TEphemeris;
  public
    constructor Create(PEphemeris: TEphemeris);
    function DateTextToJulianDay(PDateText: string; PCalendar: integer): double;
  end;

  { Handles requests for the calculation of a TimeSeries. }
  TTimeSeriesHandler = class
  strict private
    Ephemeris: TEphemeris;
    DatetimeConversion: TDateTimeConversion;
    StartJD, EndJD: Double;
    CelPoints: TCelPointArray;
    NrOfCelPoints, Calendar: Integer;
  public
    constructor Create;
    destructor Destroy;
    function HandleRequest(Request: TTimeSeriesRequest): TTimeSeriesResponse;
  end;

implementation

uses
  StrUtils, Types;

{ TSeFlags ----------------------------------------------------------------------------------------------------------- }

constructor TSeFlags.Create(PCoordinateType: TCoordinateTypes; PAyanamsha: TAyanamshaNames);
begin
  FCoordinateType := PCoordinateType;
  FAyanamsha := PAyanamsha;
  FFlagsValue := DefineFlags;
end;

function TSeFlags.DefineFlags: longint;
var
  Flags: longint;
begin
  Flags := 2 or 256;                                         // Swiss Ephemeris and speed
  case FCoordinateType of
    HelioLongitude, HelioLatitude: Flags := Flags or 8;      // heliocentric ecliptic
    RightAscension, Declination: Flags := Flags or 2048;     // geocentric equatorial
    GeoLongitude, GeoLatitude: Flags := 2 or 256;            // geocentric ecliptic
    { TODO : Create else for case (CoordinateTypes for Flags), should throw an exception. }
  end;
  if FAyanamsha <> None then
    Flags := Flags or (64 * 1024);   // sidereal
  Result := Flags;
end;

{ TTimeSeries -------------------------------------------------------------------------------------------------------- }

constructor TTimeSeries.Create(PEphemeris: TEphemeris; PCelPoint: TCelPoint; PCycleDefinition: TCycleDefinition);
begin
  FEphemeris := PEphemeris;
  FCelPoint := PCelPoint;
  FCycleDefinition := PCycleDefinition;
  FTimedPositions := Calculate();
end;

function TTimeSeries.Calculate: TList;
var
  ActualJd, BeginJd, EndJd, Position: double;
  SeId, Interval: integer;
  Flags: longint;
  TimedPosition: TTimedPosition;
  ResultingTimedPositions: TList;
  FullPosForCoordinate: TFullPosForCoordinate;
  CoordinateType: TCoordinateTypes;
  SeFlags: TSeFlags;
  AyanamshaName: TAyanamshaNames;
begin
  ResultingTimedPositions := TList.Create;
  CoordinateType := FCycleDefinition.coordinateType;
  AyanamshaName := FCycleDefinition.ayanamsha.Name;
  BeginJd := FCycleDefinition.JdStart;
  EndJd := FCycleDefinition.JdEnd;
  ActualJd := BeginJd;
  Interval := FCycleDefinition.Interval;
  SeId := FCelPoint.seId;
  SeFlags := TSeFlags.Create(CoordinateType, AyanamshaName);
  Flags := SeFlags.FlagsValue;
  repeat
    FullPosForCoordinate := FEphemeris.CalcCelPoint(ActualJd, SeId, flags);
    case CoordinateType of
      GeoLongitude, HelioLongitude, RightAscension: Position := FullPosForCoordinate.MainPos;
      GeoLatitude, HelioLatitude, Declination: Position := FullPosForCoordinate.DeviationPos;
      Distance: Position := FullPosForCoordinate.distancePos;
      { TODO : Create else for case (CoordinateTypes), should throw an exception. }
    end;
    TimedPosition := TTimedPosition.Create(ActualJd, Position);
    ResultingTimedPositions.add(TimedPosition);
    ActualJd := ActualJd + Interval;
  until ActualJd > EndJd;
  Result := ResultingTimedPositions;
end;

{ TDateTimeConversion ------------------------------------------------------------------------------------------------ }

constructor TDateTimeConversion.Create(PEphemeris: TEphemeris);
begin
  FEphemeris := PEphemeris;
end;

{ TODO : Add validation to conversion from datetext to JD }
function TDateTimeConversion.DateTextToJulianDay(PDateText: string; PCalendar: integer): double;
var
  TextElements: TStringDynArray;
  FDateText: string;
  Day, Month, Year, Calendar: integer;
  UT: double;
begin
  FDateTExt := PDateText;
  Calendar := PCalendar;
  UT := 0.0;
  TextElements := SplitString(PDateText, '/');
  Year := StrToInt(TextElements[0]);
  Month := StrToInt(TextElements[1]);
  Day := StrToInt(TextElements[2]);
  Result := FEphemeris.CalcJdUt(Year, Month, Day, UT, Calendar);
end;

{ TTimeSeriesHandler ------------------------------------------------------------------------------------------------- }

constructor TTimeSeriesHandler.Create;
begin
  Ephemeris:= TEphemeris.Create;
  DatetimeConversion := TDateTimeConversion.Create(Ephemeris);
end;

destructor TTimeSeriesHandler.Destroy;
begin
  FreeAndNil(DatetimeConversion);
  FreeAndNil(Ephemeris);
end;

function TTimeSeriesHandler.HandleRequest(Request: TTimeSeriesRequest): TTimeSeriesResponse;
var
  StartDate, EndDate: String;
  AllTimeSeries: TTimeSeriesArray;
  CycleDefinition: TCycleDefinition;
  Response: TTimeSeriesResponse;
  i: Integer;
begin
  StartDate:= Request.StartDateTime;
  EndDate:= Request.EndDateTime;
  Calendar:= Request.Calendar;
  CelPoints:= Request.CelPoints;
  NrOfCelPoints:= Length(CelPoints);
  SetLength(AllTimeSeries, NrOfCelPoints);
  StartJD := DatetimeConversion.DateTextToJulianDay(StartDate, Calendar);
  EndJD:= DatetimeConversion.DateTextToJulianDay(EndDate, Calendar);
  CycleDefinition.JdStart := StartJD;
  CycleDefinition.JdEnd:= EndJD;
  CycleDefinition.Interval:= Request.Interval;
  CycleDefinition.CoordinateType:= Request.CoordinateType;
  CycleDefinition.Ayanamsha:= Request.Ayanamsha;
  CycleDefinition.CycleType:= Request.CycleType;
  for i:= 0 to NrOfCelPoints-1 do begin
    AllTimeSeries[i]:= TTimeSeries.Create(Ephemeris, CelPoints[i], CycleDefinition);
  end;
  Response.CalculatedTimeSeries:= AllTimeSeries;
  Response.Errors:= false;
  Response.ErrorText:= '';
  Result:= Response;
end;


end.
