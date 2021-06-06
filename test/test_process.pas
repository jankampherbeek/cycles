{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit test_process;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, unitprocess, unitdomainxchg, unitastron;

type

  TestSeFlags = class(TTestCase)
  strict private
    AyanamshaSpecTropical: TAyanamshaSpec;
  protected
    procedure SetUp; override;
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
    function CreateCelPoint: TCelPointSpec;
    function CreateAyanamsha: TAyanamshaSpec;
    function CreateCycleDefinition(JdStart: double; JdEnd: double; Interval: integer): TCycleDefinition;
  published
    procedure TestCreationOfFilename;
  end;

  TestTimeSeriesHandler = class(TTestCase)
  protected
  published
    procedure TestHappyFlow;
  end;

implementation

uses
  UnitReqResp;

{ TestSeFlags -------------------------------------------------------------------------------------------------------- }

procedure TestSeFlags.SetUp;
begin
  AyanamshaSpecTropical.SeId := -1;
  AyanamshaSpecTropical.Name := 'None';
  AyanamshaSpecTropical.Descr := 'Tropical';
end;

procedure TestSeFlags.TestFlagsGeoEclTrop;
var
  CoordinateSpec: TCoordinateSpec;
  SeFlags: TSeFlags;
begin
  CoordinateSpec.Identification := 'GeoLong';
  SeFlags := TSeFlags.Create(CoordinateSpec, AyanamshaSpecTropical);
  assertEquals(258, SeFlags.FlagsValue);                // 2 or 256
end;

procedure TestSeFlags.TestFlagsHelioEclTrop;
var
  CoordinateSpec: TCoordinateSpec;
  SeFlags: TSeFlags;
begin
  CoordinateSpec.Identification := 'HelioLong';
  SeFlags := TSeFlags.Create(CoordinateSpec, AyanamshaSpecTropical);
  assertEquals(266, SeFlags.FlagsValue);                // 2 or 256 or 8
end;

procedure TestSeFlags.TestFlagsEquatTrop;
var
  CoordinateSpec: TCoordinateSpec;
  SeFlags: TSeFlags;
begin
  CoordinateSpec.Identification := 'RightAsc';
  SeFlags := TSeFlags.Create(CoordinateSpec, AyanamshaSpecTropical);
  assertEquals(2306, SeFlags.FlagsValue);                // 2 or 256 or 2048
end;

procedure TestSeFlags.TestFlagsEclSidereal;
var
  CoordinateSpec: TCoordinateSpec;
  Ayanamsha: TAyanamshaSpec;
  SeFlags: TSeFlags;
begin
  CoordinateSpec.Identification := 'GeoLong';
  Ayanamsha.Name := 'Huber';
  Ayanamsha.SeId := 4;
  Ayanamsha.Descr := 'Mean value of Babylonian Ayanamshas, defined by Peter Huber';
  SeFlags := TSeFlags.Create(CoordinateSpec, Ayanamsha);
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

procedure TestTimeSeries.TestCreationOfFileName;
var
  CelPoint: TCelPointSpec;
  CycleDefinition: TCycleDefinition;
  TimeSeries: TTimeSeries;
  ResultList: TList;
begin
  CelPoint := CreateCelPoint;
  CycleDefinition := CreateCycleDefinition(2434406.817713, 2434409.817713, 1); // 1953-1-29 UT 7:37
  TimeSeries := TTimeSeries.Create(Ephemeris, CelPoint, CycleDefinition, 1);
  AssertTrue(TimeSeries.FileNameData.Contains('data'));                  // No exception occurred.

end;

function TestTimeSeries.CreateCelPoint: TCelPointSpec;
var
  CelPoint: TCelPointSpec;
begin
  Celpoint.Name := 'Sun';
  CelPoint.Identification := 'Sun';
  CelPoint.FirstJd := -2000000.5;
  CelPoint.LastJd := 4000100.5;
  CelPoint.SeId := 0;
  Result := CelPoint;
end;

function TestTimeSeries.CreateAyanamsha: TAyanamshaSpec;
var
  Ayanamsha: TAyanamshaSpec;
begin
  Ayanamsha.Name := 'None';
  Ayanamsha.Descr := 'Tropical';
  Ayanamsha.SeId := -1;
  Result := Ayanamsha;
end;

function TestTimeSeries.CreateCycleDefinition(JdStart: double; JdEnd: double; Interval: integer): TCycleDefinition;
var
  CycleDefinition: TCycleDefinition;
  CycleType: TCycleTypeSpec;
  CoordinateType: TCoordinateSpec;
begin
  CycleType.Identification := 'SinglePos';
  CycleDefinition.CycleType := CycleType;
  CycleDefinition.JdStart := JdStart;
  CycleDefinition.JdEnd := JdEnd;
  CycleDefinition.Interval := Interval;
  CoordinateType.Identification := 'GeoLong';
  CycleDefinition.CoordinateType := CoordinateType;
  CycleDefinition.Ayanamsha := CreateAyanamsha;
  Result := CycleDefinition;
end;


{ TestTimeSeriesHandler ----------------------------------------------------------------------------------------------- }
procedure TestTimeSeriesHandler.TestHappyFlow;
var
  Request: TTimeSeriesRequest;
  Response: TTimeSeriesResponse;
  Handler: TTimeSeriesHandler;
  Ayanamsha: TAyanamshaSpec;
  AllCelPoints: TCelPointSpecArray;
  CelPointSun, CelPointMoon: TCelPointSpec;
  TSResult: TList;
  CoordinateSpec: TCoordinateSpec;
  CycleTypeSpec: TCycleTypeSpec;
begin
  Ayanamsha.Name := 'None';
  Ayanamsha.Descr := 'Tropical';
  Ayanamsha.SeId := -1;
  CelPointSun.SeId := 0;
  CelPointSun.Name := 'Sun';
  CelPointSun.Identification := 'Sun';
  CelPointSun.FirstJd := -2000000;
  CelPointSun.LastJD := 4000000;
  CelPointMoon.SeId := 1;
  CelPointMoon.Name := 'Moon';
  CelPointMoon.Identification := 'Moon';
  CelPointMoon.FirstJd := -2000000;
  CelPointMoon.LastJD := 4000000;
  AllCelPoints := TCelPointSpecArray.Create(CelPointSun, CelPointMoon);
  Request.Ayanamsha := Ayanamsha;
  Request.StartDateTime := '2021/05/25';
  Request.EndDateTime := '2021/06/24';
  Request.Calendar := 1;
  Request.Interval := 1;
  CoordinateSpec.Identification := 'GeoLong';
  Request.CoordinateType := CoordinateSpec;
  CycleTypeSpec.Identification := 'SinglePos';
  Request.CycleType := CycleTYpeSpec;
  Request.CelPoints := AllCelPoints;
  Handler := TTimeSeriesHandler.Create;
  Response := Handler.HandleRequest(Request);
  AssertFalse(Response.Errors);
end;

initialization
  RegisterTest('Process', TestSeFlags);
  RegisterTest('Process', TestDateTimeConversion);
  RegisterTest('Process', TestTimeSeries);
  RegisterTest('Process', TestTimeSeriesHandler);
end.

