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
  public
    constructor Create;
    { Calculate a celestial point. Uses Julian day for UT, the id for the object (SeId) and the combined flags
      for the type of calculation. Returns array with positionvalues. Access via TEphemeris.}
    function SeCalcCelPoint(PJulianDay: double; PSeId: integer; PFlags: longint): TDoubleArray;
    { Calculate Julian Day for UT, using Year (astronomical), Month, Day, UT(fractional hours),
      and Calendar (Gregorian=1, Julian=0.  Access via TEphemeris. }
    function SeCalcJdUt(Year: integer; Month: integer; Day: integer; UT: double; Calendar: integer): double;
    { Define date and Time (UT) in the specified calendar (1: Gregorian, 0: Julian ) for a given Julian Day number.}
    function SeRevJul(JdNr: double; Calendar: integer): TSimpleDateTime;

  end;

  TEphemeris = class
  strict private
    FSeFrontend: TSeFrontend;
  public
    constructor Create;
    destructor Destroy; override;
    property SeFrontend: TSeFrontend read FSeFrontend;
    { Calculate a celestial point. Uses Julian day for UT, the id for the object (SeId) and the combined flags
      for the type of calculation. Returns record with position. }
    function CalcCelPoint(PJulianDay: double; PSeId: integer; PFlags: longint): TFullPosForCoordinate;
    { Calculate Julian Day for UT, using Year (astronomical), Month, Day, UT(fractional hours),
      and Calendar (Gregorian=1, Julian=0. }
    function CalcJdUt(Year: integer; Month: integer; Day: integer; UT: double; Calendar: integer): double;

  end;


implementation

{ TSeFrontend -------------------------------------------------------------------------------------------------------- }

constructor TSeFrontend.Create;
begin
  swe_set_ephe_path('..\\se');
  // required to use the SE Data and for initialization of the SE.
end;

function TSeFrontend.SeCalcCelpoint(PJulianDay: double; PSeId: integer; PFlags: longint): TDoubleArray;
var
  Positions: array[0..5] of double;
  CalcResult: longint;
  ErrorText: array[0..255] of char;
begin
  CalcResult := swe_calc_ut(PJulianDay, PSeId, PFlags, Positions[0], ErrorText);
  { TODO : Check for CaclResult and throw exception if required. }
  Result := Positions;
end;

function TSeFrontend.SeCalcJdUt(Year: integer; Month: integer; Day: integer; UT: double; Calendar: integer): double;
begin
  Result := swe_julday(Year, Month, Day, UT, Calendar);
end;

{ TODO : Check UT, there is a difference the size of Delta T }
function TSeFrontend.SeRevJul(JdNr: double; Calendar: integer): TSimpleDateTime;
var
  Year: Integer = 0;
  Month: Integer = 0;
  Day: Integer = 0;
  UT: double = 0.0;
  DateTime: TSimpleDateTime;
begin
  swe_revjul(JdNr, Calendar, Year, Month, Day, UT);
  DateTime.Year := Year;
  DateTime.Month := Month;
  DateTime.Day := Day;
  DateTime.UT := UT;
  Result := DateTime;
end;


{ TEphemeris --------------------------------------------------------------------------------------------------------- }

constructor TEphemeris.Create;
begin
  FSeFrontend := TSeFrontend.Create;
end;

destructor TEphemeris.Destroy;
begin
  FreeAndNil(FSeFrontend);
  inherited;
end;

function TEphemeris.CalcCelPoint(PJulianDay: double; PSeId: integer; PFlags: longint): TFullPosForCoordinate;
var
  Positions: TDoubleArray;
  FullPosForCoordinate: TFullPosForCoordinate;
begin
  Positions := FSeFrontend.seCalcCelPoint(PJulianDay, PSeId, PFlags);
  FullPosForCoordinate.mainPos := Positions[0];
  FullPosForCoordinate.deviationPos := Positions[1];
  FullPosForCoordinate.distancePos := Positions[2];
  FullPosForCoordinate.mainSpeed := Positions[3];
  FullPosForCoordinate.deviationSpeed := Positions[4];
  FullPosForCoordinate.distanceSpeed := Positions[5];
  Result := FullPosForCoordinate;
end;

function TEphemeris.CalcJdUt(Year: integer; Month: integer; Day: integer; UT: double; Calendar: integer): double;
begin
  Result := FSeFrontend.SeCalcJdUt(Year, Month, Day, UT, Calendar);
end;

end.
