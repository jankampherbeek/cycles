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
    StartDateTime: string;
    EndDateTime: string;
    Calendar: integer;
    Interval: integer;
    CoordinateType: TCoordinateTypes;
    CycleType: TCycleTypes;
    Ayanamsha: TAyanamshaSpec;
    CelPoints: TCelPointArray;
  end;

  TTimeSeriesResponse = record
    Errors: boolean;
    ErrorText: string;
    FileNameData, FileNameMeta: string;
  end;

implementation

end.

