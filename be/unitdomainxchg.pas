{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit unitdomainxchg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
    TDoubleArray = Array of Double;

    TCelPointNames = (Sun, Moon, Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto,
                      MeanNode, OscNode, MeanApogee, OscApogee, Chiron, Pholus, Ceres, Pallas, Juno, Vesta,
                      Nessus, Huya, Makemake, Haumea, Eris, Ixion, Orcus, Quaoar, Sedna, Varuna);

    TCoordinateTypes = (GeoLongitude, GeoLatitude, HelioLongitude, HelioLatitude, RightAscension, Declination, Distance);

    TAyanamshaNames = (None, Fagan, Lahiri, Raman, Krishnamurti, Huber, GalicticCtrBrand);

    TCycleTypes = (SinglePoint, Waves);

    TCelPoint = record
      name: TCelPointNames;
      presentationName, glyph: String;
      firstJd, lastJD: Double;
      seId: Integer;
    end;

    TAyanamsha = record
      name: TAyanamshaNames;
      presentationName: String;
      seId: Integer;
    end;

    TCycleDefinition = record
      jdStart,jdEnd: Double;
      interval: Integer;
      coordinateType: TCoordinateTypes;
      ayanamsha: TAyanamsha;
      cycleType: TCycleTypes;
    end;

    TTimedPosition = record
      jdUt: Double;
      position: Double;
    end;

    TFullPosForCoordinate = record
      mainPos: Double;
      deviationPos: Double;
      distancePos: Double;
      mainSpeed: Double;
      deviationSpeed: Double;
      distanceSpeed: Double;
    end;


implementation

end.

