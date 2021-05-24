unit test_process;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, unitprocess, unitdomainxchg;

type

  TestSeFlags = class(TTestCase)
  published
    procedure TestFlagsGeoEclTrop;
    procedure TestFlagsHelioEclTrop;
    procedure TestFlagsEquatTrop;
    procedure TestFlagsEclSidereal;
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


initialization

  RegisterTest('Process', TestSeFlags);
end.

