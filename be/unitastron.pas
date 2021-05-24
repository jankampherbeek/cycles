{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitAstron;

{< Unit for astronomical calculations. Both calculations that are supported by the
Swiss EPhemeris and other calculations.}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitdomainxchg, swissdelphi;

type

  TSeFrontend = class
    // singleton based on last example at http://wiki.freepascal.org/Singleton_Pattern
  public
    constructor Create;
    { Calculate a celestial point. Uses Julian day for UT, the id for the object (SeId) and the combined flags
      for the type of calculation.}
    function SeCalcCelPoint(PJulianDay: double; PSeId: integer;
      PFlags: longint): TDoubleArray;
  end;

  TEphemeris = class
  public
    function CalcCelPoint(PJulianDay: double; PSeId: integer;
      PFlags: longint): TFullPosForCoordinate;
  end;


implementation

{ TSeFrontend -------------------------------------------------------------------------------------------------------- }

constructor TSeFrontend.Create;
begin
  swe_set_ephe_path('..\\se');
  // required to use the SE Data and for initialization of the SE.
end;

function TSeFrontend.SeCalcCelpoint(PJulianDay: double; PSeId: integer;
  PFlags: longint): TDoubleArray;
var
  Positions: array[0..5] of double;
  CalcResult: longint;
  ErrorText: array[0..255] of char;
begin
  CalcResult := swe_calc_ut(PJulianDay, PSeId, PFlags, Positions[0], ErrorText);
  Result := Positions;
end;

{ TEphemeris --------------------------------------------------------------------------------------------------------- }

function TEphemeris.CalcCelPoint(PJulianDay: double; PSeId: integer;
  PFlags: longint): TFullPosForCoordinate;
var
  Positions: TDoubleArray;
  SeFrontend: TSeFrontend;
  FullPosForCoordinate: TFullPosForCoordinate;
begin
  SeFrontend := TSeFrontend.Create();
  Positions := SeFrontend.seCalcCelPoint(PJulianDay, PSeId, PFlags);
  FullPosForCoordinate.mainPos := Positions[0];
  FullPosForCoordinate.deviationPos := Positions[1];
  FullPosForCoordinate.distancePos := Positions[2];
  FullPosForCoordinate.mainSpeed := Positions[3];
  FullPosForCoordinate.deviationSpeed := Positions[4];
  FullPosForCoordinate.distanceSpeed := Positions[5];
  Result := FullPosForCoordinate;
end;


end.
