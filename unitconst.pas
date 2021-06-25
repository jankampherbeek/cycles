{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitConst;

{< Unit for global constants.}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  // Min and max values for frequencies.
  MIN_INDEX_LON = 1;
  MAX_INDEX_LON = 360;
  MIN_INDEX_LAT = -90;
  MAX_INDEX_LAT = 90;
  MIN_INDEX_RA = 1;
  MAX_INDEX_RA = 360;
  MIN_INDEX_DECL = -90;
  MAX_INDEX_DECL = 90;

  COORD_LONG = 'Longitude';
  COORD_LAT = 'Latitude';
  COORD_RA = 'RightAsc';
  COORD_DECL= 'Decl';
  COORD_RADV = 'Radv';

implementation

end.

