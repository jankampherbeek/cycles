{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitReqResp;

{< Definitions for requests and responses. }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitdomainxchg;

type

  TTimeSeriesRequest = record
    StartDateTime, EndDateTime: string;
    Calendar, Interval: integer;
    StartJd, EndJd: Double;
    CoordinateType: TCoordinateSpec;
    CycleType: TCycleTypeSpec;
    Ayanamsha: TAyanamshaSpec;
    CelPoints: TCelPointSpecArray;
  end;

  TTimeSeriesResponse = record
    Errors: boolean;
    ErrorText: string;
    FileNameData, FileNameMeta: string;
  end;

implementation

end.

