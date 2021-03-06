{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit test_astron;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, unitastron, unitdomainxchg, swissdelphi;

type

  TTestSeFrontend = class(TTestCase)
  protected
    SeFrontend: TSeFrontend;
    Delta: double;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalcCelPoint;
    procedure TestJulianDay;
    procedure TestSeRevJul;
    procedure TestCheckDateHappyFlow;
    procedure TestCheckDateError;
  end;

implementation

procedure TTestSeFrontend.TestCalcCelPoint;
var
  Jd: double = 2434406.817713;       // 1953-1-29 UT 7:37
  SeId: integer = 0;
  Flags: integer = 2 or 256;
  ExpectedLon: double = 309.118517546;
  ExpectedLat: double = 2.852571618E-5;
  ExpectedRadv: double = 0.9850162773;
  ExpectedLonSpeed: double = 1.0153030451;
  ExpectedLatSpeed: double = -4.9701503829E-5;
  ExpectedRadvSpeed: double = 1.29935502401E-4;
  AllPositions: TDoubleArray;
begin
  AllPositions := SeFrontend.SeCalcCelPoint(Jd, SeId, Flags);
  AssertEquals('Longitude Sun', ExpectedLon, AllPositions[0], Delta);
  AssertEquals('Latitude Sun', ExpectedLat, AllPositions[1], Delta);
  AssertEquals('RADV Sun', ExpectedRadv, AllPositions[2], Delta);
  AssertEquals('Speed longitude Sun', ExpectedLonSpeed, AllPositions[3], Delta);
  AssertEquals('Speed latitude Sun', ExpectedLatSpeed, AllPositions[4], Delta);
  AssertEquals('Speed RADV Sun', ExpectedRadvSpeed, AllPositions[5], Delta);
end;

procedure TTestSeFrontend.TestJulianDay;
var
  Expected: double = 2434406.817361;     // 1953-1-29 UT 7:37, JD value is for UT: it includes Delta T.
  UT, Calculated: double;
begin
  Delta := 0.000001;
  UT := 7.6166666666667;
  Calculated := SeFrontend.SeCalcJdUt(1953, 1, 29, UT, 1);
  assertEquals('Calculate Julian Day for UT', Expected, Calculated, Delta);
end;

procedure TTestSeFrontend.TestSeRevJul;
var
  JdNr: Double = 2434406.817361;     // 1953-1-29 UT 7:37, JD value is for UT: it includes Delta T.
  DateTime: TSimpleDateTime;
begin
  Delta := 0.00001;
  DateTime:= SeFrontend.SeRevJul(JdNr, 1);
  AssertEquals('Year', 1953, DateTime.Year);
  AssertEquals('Month', 1, DateTime.Month);
  AssertEquals('Day', 29, DateTime.Day);
  AssertEquals('Time', 7.6166666666667, DateTime.UT, Delta);
end;

procedure TTestSeFrontend.TestCheckDateHappyFlow;
begin
  AssertTrue(SeFrontend.CheckDate(1953,1,29,1).IsValid);
end;

procedure TTestSeFrontend.TestCheckDateError;
begin
  AssertFalse(SeFrontend.CheckDate(1953,13,29,1).IsValid);
end;

procedure TTestSeFrontend.SetUp;
begin
  Delta := 0.00003;   //  < 0.1 second
  SeFrontend := TSeFrontend.Create;
end;

procedure TTestSeFrontend.TearDown;
begin
  FreeAndNil(SeFrontend);
  inherited;
end;

initialization
  RegisterTest('Astron', TTestSeFrontend);
end.

