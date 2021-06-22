unit test_reqresp;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpcunit, SysUtils, testregistry, testutils, unitdomainxchg, UnitReqResp;

var
  TestPeriod: TPeriod;
  TestCycleType: TCycleTypeSpec;
  TestAyanamsha: TAyanamshaSpec;
  TestCoordinate: TCoordinateSpec;
  TestObserverPos: TObserverPosSpec;
  TestStartDate, TestEndDate: TValidatedDate;
  TestCelPoints: TCelPointSpecArray;
  TestCelPointPairs: TCelPointPairedSpecArray;
  TestCelPoint1, TestCelPoint2: TCelPointSpec;
  TestFileNameData, TestFileNameMeta, TestErrorText: string;
  TestErrors: boolean;

type

  { TTestSeriesRequest }

  TTestSeriesRequest = class(TTestCase)
  protected
    SeriesRequest: TSeriesRequest;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure DefineComponents;
    procedure DefineStartDate;
    procedure DefineEndDate;
    procedure DefinePeriod;
    procedure DefineCycleType;
    procedure DefineAyanamsha;
    procedure DefineCoordinate;
    procedure DefineObserverPos;
  published
    procedure TestConstructor; virtual;
  end;

  { TTestSeriesSingleRequest }
  TTestSeriesSingleRequest = class(TTestSeriesRequest)
  protected
    SeriesSingleRequest: TSeriesSingleRequest;
    procedure Setup; override;
    procedure Teardown; override;
    procedure DefineCelPoints;
  published
    procedure Testconstructor; override;
  end;

  { TTestSeriesPairedRequest }
  TTestSeriesPairedRequest = class(TTestSeriesRequest)
  protected
    SeriesPairedRequest: TSeriesPairedRequest;
    procedure Setup; override;
    procedure Teardown; override;
    procedure DefineCelPointPairs;
  published
    procedure TestConstructor; override;
  end;


  { TTestSeriesResponse }
  TTestSeriesResponse = class(TTestCase)
  protected
    SeriesResponse: TSeriesResponse;
    procedure Setup; override;
    procedure Teardown; override;
  published
    procedure TestConstructor;
  end;

implementation

{ TTestSeriesResponse }

procedure TTestSeriesResponse.Setup;
begin
  TestErrors := False;
  TestErrorText := '';
  TestFileNameData := 'pathForDatafile';
  TestFileNameMeta := 'pathForMetafile';
  SeriesResponse := TSeriesResponse.Create(TestFileNameData, TestFileNameMeta, TestErrorText, TestErrors);
end;

procedure TTestSeriesResponse.Teardown;
begin
  FreeAndNil(SeriesResponse);
  inherited Teardown;
end;

procedure TTestSeriesResponse.TestConstructor;
begin
  AssertEquals(TestFileNameData, SeriesResponse.FilenameData);
  AssertEquals(TestFilenameMeta, SeriesResponse.FileNameMeta);
  AssertEquals(TestErrorText, SeriesResponse.ErrorText);
  AssertEquals(TestErrors, SeriesResponse.Errors);
end;


{ TTestSeriesPairedRequest }

procedure TTestSeriesPairedRequest.Setup;
begin
  DefineComponents;
  DefineCelPointPairs;
  SeriesPairedRequest := TSeriesPairedRequest.Create(TestPeriod, TestCycleType, TestAyanamsha,
    TestCoordinate, TestObserverPos, TestCelPointPairs);
  inherited Setup;
end;

procedure TTestSeriesPairedRequest.Teardown;
begin
  FreeAndNil(SeriesPairedRequest);
  inherited Teardown;
end;

procedure TTestSeriesPairedRequest.DefineCelPointPairs;
var
  CelPointPair: TCelPointPairedSpec;
begin
  TestCelPoint1.Identification := 'Mercury';
  TestCelPoint1.Name := 'Name for Mercury';
  TestCelPoint1.Distance := True;
  TestCelPoint1.FirstJd := -123.456;
  TestCelPoint1.LastJd := 123.456;
  TestCelPoint1.GeoCentric := True;
  TestCelPoint1.HelioCentric := True;
  TestCelPoint1.SeId := 2;
  TestCelPoint2.Identification := 'Earth';
  TestCelPoint2.Name := 'Name for Earth';
  TestCelPoint2.Distance := True;
  TestCelPoint2.FirstJd := -123.456;
  TestCelPoint2.LastJd := 123.456;
  TestCelPoint2.GeoCentric := False;
  TestCelPoint2.HelioCentric := True;
  TestCelPoint2.SeId := 13;
  TestCelPoints[0] := TestCelPoint1;
  TestCelPoints[1] := TestCelPoint2;
  CelPointPair.FirstCP := TestCelPoint1;
  CelPointPair.SecondCP := TestCelPoint2;
  TestCelPointPairs[0] := CelPointPair;
end;

procedure TTestSeriesPairedRequest.Testconstructor;
begin
  assertEquals(TestCelPoints[0].Name, SeriesPairedRequest.CelPointPairs[0].FirstCP.Name);
  assertEquals(TestCelPoints[1].Identification, SeriesPairedRequest.CelPointPairs[0].SecondCP.Identification);
  // test if method from parent is used.
  AssertEquals(TestCoordinate.Identification, SeriesPairedRequest.Coordinate.Identification);
end;

{ TTestSeriesSingleRequest }

procedure TTestSeriesSingleRequest.Setup;
begin
  DefineComponents;
  DefineCelPoints;
  SeriesSingleRequest := TSeriesSingleRequest.Create(TestPeriod, TestCycleType, TestAyanamsha,
    TestCoordinate, TestObserverPos, TestCelPoints);
end;

procedure TTestSeriesSingleRequest.Teardown;
begin
  FreeAndNil(SeriesSingleRequest);
  inherited Teardown;
end;

procedure TTestSeriesSingleRequest.DefineCelPoints;
begin
  TestCelPoint1.Identification := 'Sun';
  TestCelPoint1.Name := 'Name for Sun';
  TestCelPoint1.Distance := True;
  TestCelPoint1.FirstJd := -123.456;
  TestCelPoint1.LastJd := 123.456;
  TestCelPoint1.GeoCentric := True;
  TestCelPoint1.HelioCentric := False;
  TestCelPoint1.SeId := 0;
  TestCelPoint2.Identification := 'Moon';
  TestCelPoint2.Name := 'Name for Moon';
  TestCelPoint2.Distance := True;
  TestCelPoint2.FirstJd := -123.456;
  TestCelPoint2.LastJd := 123.456;
  TestCelPoint2.GeoCentric := True;
  TestCelPoint2.HelioCentric := False;
  TestCelPoint2.SeId := 1;
  TestCelPoints[0] := TestCelPoint1;
  TestCelPoints[1] := TestCelPoint2;
end;

procedure TTestSeriesSingleRequest.Testconstructor;
begin
  assertEquals(TestCelPoints[0].Name, SeriesSingleRequest.CelPoints[0].Name);
  assertEquals(TestCelPoints[1].Identification, SeriesSingleRequest.CelPoints[1].Identification);
  // test if method from parent is used.
  AssertEquals(TestPeriod.Interval, SeriesSingleRequest.Period.Interval);
end;

{ TTestSeriesRequest }

procedure TTestSeriesRequest.SetUp;
begin
  DefineComponents;
  SeriesRequest := TSeriesRequest.Create(TestPeriod, TestCycleType, TestAyanamsha, TestCoordinate, TestObserverPos);
  inherited SetUp;
end;

procedure TTestSeriesRequest.TearDown;
begin
  FreeAndNil(SeriesRequest);
  inherited TearDown;
end;

procedure TTestSeriesRequest.DefineComponents;
begin
  DefineStartDate;
  DefineEndDate;
  DefinePeriod;
  DefineCycleType;
  DefineAyanamsha;
  DefineCoordinate;
  DefineObserverPos;
end;

procedure TTestSeriesRequest.DefineStartDate;
begin
  TestStartDate.Calendar := 0;
  TestStartDate.IsValid := True;
  TestStartDate.JulianDay := 12345.6789;
  TestStartDate.Year := 2021;
  TestStartDate.Month := 6;
  TestStartDate.Day := 17;
end;

procedure TTestSeriesRequest.DefineEndDate;
begin
  TestEndDate.Calendar := 0;
  TestEndDate.IsValid := True;
  TestEndDate.JulianDay := 12355.6789;
  TestEndDate.Year := 2021;
  TestEndDate.Month := 6;
  TestEndDate.Day := 27;
end;

procedure TTestSeriesRequest.DefinePeriod;
begin
  TestPeriod.StartDate := TestStartDate;
  TestPeriod.EndDate := TestEndDate;
  TestPeriod.Interval := 13;
end;

procedure TTestSeriesRequest.DefineCycleType;
begin
  TestCycleType.Identification := 'CycleId';
  TestCycleType.Name := 'CycleName';
end;

procedure TTestSeriesRequest.DefineAyanamsha;
begin
  TestAyanamsha.Name := 'AyanamshaName';
  TestAyanamsha.Descr := 'AyanamshaDescr';
  TestAyanamsha.SeId := -1;
end;

procedure TTestSeriesRequest.DefineCoordinate;
begin
  TestCoordinate.Name := 'CoordinateName';
  TestCoordinate.Identification := 'CoordinateId';
end;

procedure TTestSeriesRequest.DefineObserverPos;
begin
  TestObserverPos.Name := 'ObserverposName';
  TestObserverPos.Identification := 'ObserverposId';
end;

procedure TTestSeriesRequest.TestConstructor;
begin
  AssertEquals(TestPeriod.Interval, SeriesRequest.Period.Interval);
  AssertEquals(TestPeriod.StartDate.Day, SeriesRequest.Period.StartDate.Day);
  AssertEquals(TestPeriod.EndDate.Day, SeriesRequest.Period.EndDate.Day);
  AssertEquals(TestCycleType.Identification, SeriesRequest.CycleType.Identification);
  AssertEquals(TestAyanamsha.Name, SeriesRequest.Ayanamsha.Name);
  AssertEquals(TestCoordinate.Identification, SeriesRequest.Coordinate.Identification);
  AssertEquals(TestObserverPos.Name, SeriesRequest.ObserverPos.Name);
end;


initialization
  RegisterTest(TTestSeriesRequest);
  RegisterTest(TTestSeriesSingleRequest);
  RegisterTest(TTestSeriesPairedRequest);
  RegisterTest(TTestSeriesResponse);
end.



