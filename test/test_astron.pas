unit test_astron;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, unitastron, unitdomainbe, swissdelphi;

type

  TTestSeFrontend= class(TTestCase)
    var

  protected
    SeFrontend: TSeFrontend;
              Delta: Double;   //  < 0.1 second
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalcCelPoint;
  end;

implementation

procedure TTestSeFrontend.TestCalcCelPoint;
var
  Jd: Double = 2434406.817713;       // 1953-1-29 UT 7:37
  SeId: Integer = 0;
  Flags: Integer = 2 or 256;
  ExpectedLon: Double = 309.118517546;
  ExpectedLat: Double = 2.852571618E-5;
  ExpectedRadv: Double = 0.9850162773;
  ExpectedLonSpeed: Double = 1.0153030451;
  ExpectedLatSpeed: Double = -4.9701503829E-5;
  ExpectedRadvSpeed: Double = 1.29935502401E-4;
  AllPositions: TDoubleArray;
begin
  AllPositions:=SeFrontend.SeCalcCelPoint(Jd, SeId, Flags);
  AssertEquals('Longitude Sun', ExpectedLon, AllPositions[0], Delta);
  AssertEquals('Latitude Sun', ExpectedLat, AllPositions[1], Delta);
  AssertEquals('RADV Sun', ExpectedRadv, AllPositions[2], Delta);
  AssertEquals('Speed longitude Sun', ExpectedLonSpeed, AllPositions[3], Delta);
  AssertEquals('Speed latitude Sun', ExpectedLatSpeed, AllPositions[4], Delta);
  AssertEquals('Speed RADV Sun', ExpectedRadvSpeed, AllPositions[5], Delta);
end;

procedure TTestSeFrontend.SetUp;
begin
  Delta:= 0.00003;   //  < 0.1 second
  SeFrontend:=TSeFrontend.Create;
end;

procedure TTestSeFrontend.TearDown;
begin
  FreeAndNil(SeFrontend);
end;

initialization
  RegisterTest('Astron', TTestSeFrontend);
end.

