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
    Interval: integer;
    CoordinateType: TCoordinateTypes;
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
  FTimedPositions := calculate();
end;

function TTimeSeries.calculate: TList;
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
function TDateTimeConversion.DateTextToJulianDay(PDateText: String; PCalendar: Integer): double;
var
  TextElements: TStringDynArray;
  FDateText: String;
  Day, Month, Year, Calendar: Integer;
  UT: Double;
begin
  FDateTExt:= PDateText;
  Calendar:= PCalendar;
  UT:= 0.0;
  TextElements:= SplitString(PDateText, '/');
  Year := StrToInt(TextElements[0]);
  Month := StrToInt(TextElements[1]);
  Day := StrToInt(TextElements[2]);
  Result:= FEphemeris.CalcJdUt(Year, Month, Day, UT, Calendar);
end;



end.
