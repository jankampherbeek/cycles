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
  TIntegerArray = array of integer;

  TValidatedDate = record
    IsValid: boolean;
    JulianDay: double;
    Year, Month, Day, Calendar: integer;
  end;

  TValidatedJulianDay = record
    IsValid: Boolean;
    JulianDay: double;
  end;

  TCelPointSpec = record
    SeId: integer;
    Identification, Name: string;
    FirstJd, LastJD: double;
    GeoCentric, HelioCentric, Distance: Boolean;
  end;

  TCelPointSpecArray = array[0..30] of TCelPointSpec;

  TAyanamshaSpec = record
    SeId: integer;
    Name, Descr: string;
  end;

  TAyanamshaSpecArray = array of TAyanamshaSpec;

  TCoordinateSpec = record
    Identification, Name: string
  end;

  TCoordinateSpecArray = array of TCoordinateSpec;

  TCycleTypeSpec = record
    Identification, Name: string;
  end;

  TCycleTypeSpecArray = array of TCycleTypeSpec;

  TCycleDefinition = record
    JdStart, JdEnd: double;
    Interval: integer;
    CoordinateType: TCoordinateSpec;
    Ayanamsha: TAyanamshaSpec;
    CycleType: TCycleTypeSpec;
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

  TSimpleDateTime = record
    Year: integer;
    Month: integer;
    Day: integer;
    UT: double;
  end;




implementation

{ TTimedPosition ----------------------------------------------------------------------------------------------------- }

constructor TTimedPosition.Create(PJdUt: double; PPosition: double);
begin
  FJdUt := PJdUt;
  FPosition := PPosition;
end;

end.
