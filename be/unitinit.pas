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
    function ConstructAyanamshaSpec(SeId: integer; Name: string; Descr: string): TAyanamshaSpec;
  public
    Constructor Create;
    Destructor Destroy;
    procedure DefineAllAyanamshas;
    property AllAyanamshas: TAyanamshaSpecArray read FAllAyanamshas;
  end;




implementation

{ TLookupValues ------------------------------------------------------------------------------------------------------- }

Constructor TLookupValues.Create;
begin
  DefineAllAyanamshas;
end;

Destructor TLookupValues.Destroy;
begin
  FreeAndNil(FAllAyanamshas);
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
  FAllAyanamshas[5] := ConstructAyanamshaSpec(4, 'Huber', 'Mean value of Babylonian Ayanamshas, defined by Peter Huber');
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


end.


