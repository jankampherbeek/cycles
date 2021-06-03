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
    function ConstructAyanamshaSpec(SeId: integer; Name: string; Descr: string): TAyanamshaSpec;
    function ConstructCoordinateSpec(Identification: string; Name: string): TCoordinateSpec;
    function ConstructCycleTypeSpec(Identification: string; Name: string): TCycleTypeSpec;
  public
    constructor Create;
    procedure DefineAllAyanamshas;
    procedure DefineAllCoordinates;
    procedure DefineAllCycleTypes;
    property AllAyanamshas: TAyanamshaSpecArray read FAllAyanamshas;
    property AllCoordinates: TCoordinateSpecArray read FAllCoordinates;
    property AllCycleTypes: TCycleTypeSpecArray read FAllCycleTypes;

  end;




implementation

{ TLookupValues ------------------------------------------------------------------------------------------------------- }

constructor TLookupValues.Create;
begin
  DefineAllAyanamshas;
  DefineAllCoordinates;
  DefineAllCycleTypes;
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

function TLookupValues.ConstructAyanamshaSpec(SeId: integer; Name: string; Descr: string): TAyanamshaSpec;
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

function TLookupValues.ConstructCoordinateSpec(Identification: string; Name: string): TCoordinateSpec;
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
  FAllCycleTypes[0]:= ConstructCycleTypeSpec('SinglePos', 'Position for single point');
  FAllCycleTypes[1]:= ConstructCycleTypeSpec('Waves', 'Waves according to Robert Doolaard');
end;

function TLookupValues.ConstructCycleTypeSpec(Identification: string; Name: string): TCycleTypeSpec;
var
  CycleTypeSpec: TCycleTypeSpec;
begin
  CycleTypeSpec.Identification:= Identification;
  CycleTypeSpec.Name:= Name;
  Result:= CycleTypeSpec;
end;

end.

