unit test_process;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, unitprocess, unitdomainxchg, unitastron;

type

  TestSeFlags = class(TTestCase)
  published
    procedure TestFlagsGeoEclTrop;
    procedure TestFlagsHelioEclTrop;
    procedure TestFlagsEquatTrop;
    procedure TestFlagsEclSidereal;
  end;


  TestDateTimeConversion = class(TTestCase)
  protected
    Ephemeris: TEphemeris;
    DateTimeConversion: TDateTimeConversion;
  published
    procedure TestHappyFlow;
  end;

  TestTimeSeries = class(TTestCase)
  protected
    Ephemeris: TEphemeris;
    Delta: double;
    procedure SetUp; override;
    procedure TearDown; override;
    function CreateCelPoint: TCelPoint;
    function CreateAyanamsha: TAyanamsha;
    function CreateCycleDefinition(JdStart: double; JdEnd: double; Interval: integer): TCycleDefinition;
  published
    procedure TestNrOfResults;
    procedure TestNrOfResultsWithInterval3;
    procedure TestContentOfResults;
  end;

  TestTimeSeriesHandler = class(TTestCase)
  protected
  published
    procedure TestHappyFlow;
  end;

implementation

{ TestSeFlags -------------------------------------------------------------------------------------------------------- }

procedure TestSeFlags.TestFlagsGeoEclTrop;
var
  CoordinateType: TCoordinateTypes = GeoLongitude;
  Ayanamsha: TAyanamshaNames = None;
  SeFlags: TSeFlags;
begin
  SeFlags := TSeFlags.Create(CoordinateType, Ayanamsha);
  assertEquals(258, SeFlags.FlagsValue);                // 2 or 256
end;

procedure TestSeFlags.TestFlagsHelioEclTrop;
var
  CoordinateType: TCoordinateTypes = HelioLongitude;
  Ayanamsha: TAyanamshaNames = None;
  SeFlags: TSeFlags;
begin
  SeFlags := TSeFlags.Create(CoordinateType, Ayanamsha);
  assertEquals(266, SeFlags.FlagsValue);                // 2 or 256 or 8
end;

procedure TestSeFlags.TestFlagsEquatTrop;
var
  CoordinateType: TCoordinateTypes = RightAscension;
  Ayanamsha: TAyanamshaNames = None;
  SeFlags: TSeFlags;
begin
  SeFlags := TSeFlags.Create(CoordinateType, Ayanamsha);
  assertEquals(2306, SeFlags.FlagsValue);                // 2 or 256 or 2048
end;

procedure TestSeFlags.TestFlagsEclSidereal;
var
  CoordinateType: TCoordinateTypes = GeoLongitude;
  Ayanamsha: TAyanamshaNames = Huber;
  SeFlags: TSeFlags;
begin
  SeFlags := TSeFlags.Create(CoordinateType, Ayanamsha);
  assertEquals(65794, SeFlags.FlagsValue);                // 2 or 256 or (64 * 1024))
end;


{ TestDateTimeConversion --------------------------------------------------------------------------------------------- }
procedure TestDateTimeConversion.TestHappyFlow;
var
  ExpectedJD: double = 2434406.5;
  CalculatedJD, Delta: double;
  DateText: string = '1953/01/29';
  Calendar: integer = 1;                   // Gregorian
begin
  Delta := 0.00000001;
  Ephemeris := TEphemeris.Create;
  DateTimeConversion := TDateTimeConversion.Create(Ephemeris);
  CalculatedJD := DateTimeConversion.DateTextToJulianDay(DateText, Calendar);
  AssertEquals(ExpectedJD, CalculatedJD, Delta);
end;

{ TestTimeSeries ----------------------------------------------------------------------------------------------------- }
procedure TestTimeSeries.Setup;
begin
  Delta := 0.00003;   //  < 0.1 second
  Ephemeris := TEphemeris.Create;
end;

procedure TestTimeSeries.TearDown;
begin
  FreeAndNil(Ephemeris);
end;

procedure TestTimeSeries.TestNrOfResults;
var
  CelPoint: TCelPoint;
  CycleDefinition: TCycleDefinition;
  TimeSeries: TTimeSeries;
  ResultList: TList;
begin
  CelPoint := CreateCelPoint;
  CycleDefinition := CreateCycleDefinition(2000001.5, 2000100.5, 1);
  TimeSeries := TTimeSeries.Create(Ephemeris, CelPoint, CycleDefinition);
  ResultList := TimeSeries.TimedPositions;
  AssertEquals('Size of TimeSeries', 100, ResultList.Count);
end;

procedure TestTimeSeries.TestNrOfResultsWithInterval3;
var
  CelPoint: TCelPoint;
  CycleDefinition: TCycleDefinition;
  TimeSeries: TTimeSeries;
  ResultList: TList;
begin
  CelPoint := CreateCelPoint;
  CycleDefinition := CreateCycleDefinition(2000001.5, 2000100.5, 3);
  TimeSeries := TTimeSeries.Create(Ephemeris, CelPoint, CycleDefinition);
  ResultList := TimeSeries.TimedPositions;
  AssertEquals('Size of TimeSeries', 34, ResultList.Count);          // 34, 33 comes short for the whole periode
end;

procedure TestTimeSeries.TestContentOfResults;
var
  CelPoint: TCelPoint;
  CycleDefinition: TCycleDefinition;
  TimeSeries: TTimeSeries;
  ResultList: TList;
begin
  CelPoint := CreateCelPoint;
  CycleDefinition := CreateCycleDefinition(2434406.817713, 2434409.817713, 1); // 1953-1-29 UT 7:37
  TimeSeries := TTimeSeries.Create(Ephemeris, CelPoint, CycleDefinition);
  ResultList := TimeSeries.TimedPositions;
  AssertEquals('Longitude', 309.118517546, TTimedPosition(ResultList[0]).Position, Delta);

end;

function TestTimeSeries.CreateCelPoint: TCelPoint;
var
  CelPoint: TCelPoint;
begin
  Celpoint.Name := TCelPointNames.Sun;
  CelPoint.PresentationName := 'Sun';
  CelPoint.Glyph := 'a';
  CelPoint.FirstJd := -2000000.5;
  CelPoint.LastJd := 4000100.5;
  CelPoint.SeId := 0;
  Result := CelPoint;
end;

function TestTimeSeries.CreateAyanamsha: TAyanamsha;
var
  Ayanamsha: TAyanamsha;
begin
  Ayanamsha.Name := None;
  Ayanamsha.SeId := -1;
  Ayanamsha.PresentationName := 'Tropical';
  Result := Ayanamsha;
end;

function TestTimeSeries.CreateCycleDefinition(JdStart: double; JdEnd: double; Interval: integer): TCycleDefinition;
var
  CycleDefinition: TCycleDefinition;
begin
  CycleDefinition.CycleType := SinglePoint;
  CycleDefinition.JdStart := JdStart;
  CycleDefinition.JdEnd := JdEnd;
  CycleDefinition.Interval := Interval;
  CycleDefinition.CoordinateType := GeoLongitude;
  CycleDefinition.Ayanamsha := CreateAyanamsha;
  Result := CycleDefinition;
end;


{ TestTimeSeriesHandler ----------------------------------------------------------------------------------------------- }
procedure TestTimeSeriesHandler.TestHappyFlow;
var
  Request: TTimeSeriesRequest;
  Response: TTimeSeriesResponse;
  Handler: TTimeSeriesHandler;
  Ayanamsha: TAyanamsha;
  AllCelPoints: TCelPointArray;
  CelPointSun, CelPointMoon: TCelPoint;
  TSResult: TList;
begin
  Ayanamsha.Name := None;
  Ayanamsha.PresentationName := 'Tropical';
  Ayanamsha.SeId := -1;
  CelPointSun.SeId := 0;
  CelPointSun.PresentationName := 'Sun';
  CelPointSun.Name := Sun;
  CelPointSun.FirstJd := -2000000;
  CelPointSun.LastJD := 4000000;
  CelPointSun.Glyph := 'a';
  CelPointMoon.SeId := 0;
  CelPointMoon.PresentationName := 'Moon';
  CelPointMoon.Name := Moon;
  CelPointMoon.FirstJd := -2000000;
  CelPointMoon.LastJD := 4000000;
  CelPointMoon.Glyph := 'b';
  AllCelPoints := TCelPointArray.Create(CelPointSun, CelPointMoon);
  Request.Ayanamsha := Ayanamsha;
  Request.StartDateTime := '2021/05/25';
  Request.EndDateTime := '2021/06/24';
  Request.Calendar := 1;
  Request.Interval := 1;
  Request.CoordinateType := GeoLongitude;
  Request.CycleType := SinglePoint;
  Request.CelPoints := AllCelPoints;
  Handler := TTimeSeriesHandler.Create;
  Response := Handler.HandleRequest(Request);
  AssertEquals(2, Length(Response.CalculatedTimeSeries));
  TSResult := Response.CalculatedTimeSeries[0].TimedPositions;
  AssertEquals(31, TSResult.Count);
end;

initialization
  RegisterTest('Process', TestSeFlags);
  RegisterTest('Process', TestDateTimeConversion);
  RegisterTest('Process', TestTimeSeries);
  RegisterTest('Process', TestTimeSeriesHandler);
end.

