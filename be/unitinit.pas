{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitInit;

{< Unit for initialisation of the application.}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitdomainxchg;

type

  TLookupValues = class
  strict private
    FAllAyanamshas: TAyanamshaSpecArray;
    FAllCoordinates: TCoordinateSpecArray;
    FAllCycleTypes: TCycleTypeSpecArray;
    FAllCelpoints: TCelPointSpecArray;
    function ConstructAyanamshaSpec(SeId: integer; Name, Descr: string): TAyanamshaSpec;
    function ConstructCoordinateSpec(Identification, Name: string): TCoordinateSpec;
    function ConstructCycleTypeSpec(Identification, Name: string): TCycleTypeSpec;
    function ConstructCelpointSpec(SeId: integer; Identification, Name: string; FirstJd, lastJd: double;
      Geocentric, HelioCentric, Distance: Boolean): TCelPointSpec;
  public
    constructor Create;
    procedure DefineAllAyanamshas;
    procedure DefineAllCoordinates;
    procedure DefineAllCycleTypes;
    procedure DefineAllCelpoints;
    property AllAyanamshas: TAyanamshaSpecArray read FAllAyanamshas;
    property AllCoordinates: TCoordinateSpecArray read FAllCoordinates;
    property AllCycleTypes: TCycleTypeSpecArray read FAllCycleTypes;
    property AllCelPoints: TCelPointSpecArray read FAllCelpoints;

  end;




implementation

{ TLookupValues ------------------------------------------------------------------------------------------------------- }

constructor TLookupValues.Create;
begin
  DefineAllAyanamshas;
  DefineAllCoordinates;
  DefineAllCycleTypes;
  DefineAllCelPoints;
end;

procedure TLookupValues.DefineAllAyanamshas;
var
  nrOfAyanamshas: integer = 7;
begin
  SetLength(FAllAyanamshas, nrOfAyanamshas);
  FAllAyanamshas[0] := ConstructAyanamshaSpec(-1, 'None', 'Tropical');
  FAllAyanamshas[1] := ConstructAyanamshaSpec(0, 'Fagan', 'According to Cyril Fagan and Donald Bradley');
  FAllAyanamshas[2] := ConstructAyanamshaSpec(1, 'Lahiri', 'Official Indian Ayanamsha');
  FAllAyanamshas[3] := ConstructAyanamshaSpec(2, 'Raman', ' According to B. V. Raman');
  FAllAyanamshas[4] := ConstructAyanamshaSpec(3, 'Krishnamurti', 'According to K. S. KRishnamurti');
  FAllAyanamshas[5] := ConstructAyanamshaSpec(4, 'Huber',
    'Mean value of Babylonian Ayanamshas, defined by Peter Huber');
  FAllAyanamshas[6] := ConstructAyanamshaSpec(5, 'Galactic Center Brand', 'According to Rafael Gil Brand');

end;

function TLookupValues.ConstructAyanamshaSpec(SeId: integer; Name, Descr: string): TAyanamshaSpec;
var
  AyanamshaSpec: TAyanamshaSpec;
begin
  AyanamshaSpec.SeId := SeId;
  AyanamshaSpec.Name := Name;
  AyanamshaSpec.Descr := Descr;
  Result := AyanamshaSpec;
end;

procedure TLookupValues.DefineAllCoordinates;
var
  NrOfCoordinates: integer = 7;
begin
  SetLength(FAllCoordinates, NrOfCoordinates);
  FAllCoordinates[0] := ConstructCoordinateSpec('GeoLong', 'Geocentric Longitude');
  FAllCoordinates[1] := ConstructCoordinateSpec('HelioLong', 'Heliocentric Longitude');
  FAllCoordinates[2] := ConstructCoordinateSpec('GeoLat', 'Geocentric Latitude');
  FAllCoordinates[3] := ConstructCoordinateSpec('HelioLat', 'Heliocentric Latitude');
  FAllCoordinates[4] := ConstructCoordinateSpec('RightAsc', 'Right Ascension');
  FAllCoordinates[5] := ConstructCoordinateSpec('Decl', 'Declination');
  FAllCoordinates[6] := ConstructCoordinateSpec('Radv', 'Distance');
end;

function TLookupValues.ConstructCoordinateSpec(Identification, Name: string): TCoordinateSpec;
var
  CoordinateSpec: TCoordinateSpec;
begin
  CoordinateSpec.Identification := Identification;
  CoordinateSpec.Name := Name;
  Result := CoordinateSpec;
end;

procedure TLookupValues.DefineAllCycleTypes;
var
  NrOfCycleTypes: integer = 2;
begin
  SetLength(FAllCycleTypes, NrOfCycleTypes);
  FAllCycleTypes[0] := ConstructCycleTypeSpec('SinglePos', 'Position for single point');
  FAllCycleTypes[1] := ConstructCycleTypeSpec('Waves', 'Waves according to Robert Doolaard');
end;

function TLookupValues.ConstructCycleTypeSpec(Identification, Name: string): TCycleTypeSpec;
var
  CycleTypeSpec: TCycleTypeSpec;
begin
  CycleTypeSpec.Identification := Identification;
  CycleTypeSpec.Name := Name;
  Result := CycleTypeSpec;
end;

procedure TLookupValues.DefineAllCelpoints;
var
  NrOfCelPoints: integer = 31;
begin
  //SetLength(FAllCelpoints, NrOfCelPoints);
  FAllCelpoints[0] := ConstructCelpointSpec(0, 'Sun', 'Sun', -3026613.5, 5227458.5, true, false, true);
  FAllCelpoints[1] := ConstructCelpointSpec(1, 'Moon', 'Moon', -3026613.5, 5227458.5, true, false, true);
  FAllCelpoints[2] := ConstructCelpointSpec(2, 'Mercury', 'Mercury', -3026613.5, 5227458.5, true, true, true);
  FAllCelpoints[3] := ConstructCelpointSpec(3, 'Venus', 'Venus', -3026613.5, 5227458.5, true, true, true);
  FAllCelpoints[4] := ConstructCelpointSpec(4, 'Mars', 'Mars', -3026613.5, 5227458.5, true, true, true);
  FAllCelpoints[5] := ConstructCelpointSpec(5, 'Jupiter', 'Jupiter', -3026613.5, 5227458.5, true, true, true);
  FAllCelpoints[6] := ConstructCelpointSpec(6, 'Saturn', 'Saturn', -3026613.5, 5227458.5, true, true, true);
  FAllCelpoints[7] := ConstructCelpointSpec(7, 'Uranus', 'Uranus', -3026613.5, 5227458.5, true, true, true);
  FAllCelpoints[8] := ConstructCelpointSpec(8, 'Neptune', 'Neptune', -3026613.5, 5227458.5, true, true, true);
  FAllCelpoints[9] := ConstructCelpointSpec(9, 'Pluto', 'Pluto', -3026613.5, 5227458.5, true, true, true);
  FAllCelpoints[10] := ConstructCelpointSpec(10, 'Node_mean', 'Mean Node', -3026613.5, 5227458.5, true, false, false);
  FAllCelpoints[11] := ConstructCelpointSpec(11, 'Node_oscu', 'True Node', -3026613.5, 5227458.5, true, false, false);
  FAllCelpoints[12] := ConstructCelpointSpec(14, 'Earth', 'Earth (helioc.)', -3026613.5, 5227458.5, false, true, true);
  FAllCelpoints[13] := ConstructCelpointSpec(15, 'Chiron', 'Chiron', 1967601.5, 3419437.5, true, true, true);
  FAllCelpoints[14] := ConstructCelpointSpec(16, 'Pholus', 'Pholus', 640648.5, 4390615.5, true, true, true);
  FAllCelpoints[15] := ConstructCelpointSpec(17, 'Ceres', 'Ceres', -3026613.5, 5224242.5, true, true, true);
  FAllCelpoints[16] := ConstructCelpointSpec(18, 'Pallas', 'Pallas', -3026613.5, 5227458.5, true, true, true);
  FAllCelpoints[17] := ConstructCelpointSpec(19, 'Juno', 'Juno', -3026613.5, 5227458.5, true, true, true);
  FAllCelpoints[18] := ConstructCelpointSpec(20, 'Vesta', 'Vesta', -3026613.5, 5221544.5, true, true, true);
  FAllCelpoints[19] := ConstructCelpointSpec(17066, 'Nessus', 'Nessus', 625372.5, 2816371.5, true, true, true);
  FAllCelpoints[20] := ConstructCelpointSpec(48628, 'Huya', 'Huya', 625296.5, 2816295.5, true, true, true);
  FAllCelpoints[21] := ConstructCelpointSpec(146472, 'Makemake', 'MakeMake', 625292.5, 2816291.5, true, true, true);
  FAllCelpoints[22] := ConstructCelpointSpec(146108, 'Haumea', 'Haumea', 625292.5, 2816291.5, true, true, true);
  FAllCelpoints[23] := ConstructCelpointSpec(146199, 'Eris', 'Eris', 625384.5, 2816383.5, true, true, true);
  FAllCelpoints[24] := ConstructCelpointSpec(38978, 'Ixion', 'Ixion', 625296.5, 2816295.5, true, true, true);
  FAllCelpoints[25] := ConstructCelpointSpec(100482, 'Orcus', 'Orcus', 625296.5, 2816295.5, true, true, true);
  FAllCelpoints[26] := ConstructCelpointSpec(60000, 'Quaoar', 'Quaoar', 625292.5, 2816291.5, true, true, true);
  FAllCelpoints[27] := ConstructCelpointSpec(100377, 'Sedna', 'Sedna', 624947.5, 2816295.5, true, true, true);
  FAllCelpoints[28] := ConstructCelpointSpec(30000, 'Varuna', 'Varuna', 625296.5, 2816295.5, true, true, true);
  FAllCelpoints[29] := ConstructCelpointSpec(12, 'Apogee_mean', 'Mean Apogee', -3026613.5, 5227458.5, true, false, false);
  FAllCelpoints[30] := ConstructCelpointSpec(13, 'Apogee_oscu', 'Oscul. Apogee', -3026613.5, 5227458.5, true, false, false);
end;

function TLookupValues.ConstructCelpointSpec(SeId: integer; Identification, Name: string;
  FirstJd, LastJd: double; Geocentric, HelioCentric, Distance: boolean): TCelPointSpec;
var
  CelPointSpec: TCelPointSpec;
begin
  CelPointSpec.SeId := SeId;
  CelPointSpec.Identification := Identification;
  CelPointSpec.Name := Name;
  CelPointSpec.FirstJd := FirstJd;
  CelPointSpec.LastJD := LastJd;
  CelPointSpec.GeoCentric := Geocentric;
  CelPointSpec.HelioCentric := HelioCentric;
  CelPointSpec.Distance := Distance;
  Result := CelPointSpec;
end;



end.

