{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit unitdomainxchg;

{< Domain items that can be used by the frontend and the backend.}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDoubleArray = array of double;

  TCelPointNames = (Sun, Moon, Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto,
    MeanNode, OscNode, MeanApogee, OscApogee, Chiron, Pholus, Ceres, Pallas, Juno, Vesta,
    Nessus, Huya, Makemake, Haumea, Eris, Ixion, Orcus, Quaoar, Sedna, Varuna);

  TCelPoint = record
    Name: TCelPointNames;
    PresentationName, Glyph: string;
    FirstJd, LastJD: double;
    SeId: integer;
  end;

  TCelPointArray = array of TCelPoint;

  TCoordinateTypes = (GeoLongitude, GeoLatitude, HelioLongitude, HelioLatitude, RightAscension,
    Declination, Distance);

  TAyanamshaNames = (None, Fagan, Lahiri, Raman, Krishnamurti, Huber, GalicticCtrBrand);

  TCycleTypes = (SinglePoint, Waves);

  TAyanamsha = record
    Name: TAyanamshaNames;
    PresentationName: string;
    SeId: integer;
  end;

  TCycleDefinition = record
    JdStart, JdEnd: double;
    Interval: integer;
    CoordinateType: TCoordinateTypes;
    Ayanamsha: TAyanamsha;
    CycleType: TCycleTypes;
  end;

  TTimedPosition = class
  strict private
    FJdUt: double;
    FPosition: double;
  public
    constructor Create(PJdUt: double; PPosition: double);
    property JdUt: double read FJdUt;
    property Position: double read FPosition;
  end;

  TFullPosForCoordinate = record
    MainPos: double;
    DeviationPos: double;
    DistancePos: double;
    MainSpeed: double;
    DeviationSpeed: double;
    DistanceSpeed: double;
  end;


implementation

{ TTimedPosition ----------------------------------------------------------------------------------------------------- }

constructor TTimedPosition.Create(PJdUt: double; PPosition: double);
begin
  FJdUt := PJdUt;
  FPosition := PPosition;
end;

end.
