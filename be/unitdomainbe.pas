{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitDomainBe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
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

    TDoubleArray = Array of Double;



implementation

end.

