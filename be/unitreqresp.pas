{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitReqResp;

{< Definitions for requests and responses. }
{$mode objfpc}{$H+}

interface

{ TODO : Remove unit UnitReqRest, is obsolete. }
uses
  Classes, SysUtils, unitdomainxchg;

type

  { Parent for several types of requests that use time series. }
  TSeriesRequest = class
  strict private
    FPeriod: TPeriod;
    FCycleType: TCycleTypeSpec;
    FAyanamsha: TAyanamshaSpec;
    FCoordinate: TCoordinateSpec;
    FObserverPos: TObserverPosSpec;
  public
    constructor Create(PPeriod: TPeriod; PCycleType: TCycleTypeSpec; PAyanamsha: TAyanamshaSpec;
      PCoordinate: TCoordinateSpec; PObserverPos: TObserverPosSpec);
    property Period: TPeriod read FPeriod;
    property CycleType: TCycleTypeSpec read FCycleType;
    property Ayanamsha: TAyanamshaSpec read FAyanamsha;
    property Coordinate: TCoordinateSpec read FCoordinate;
    property ObserverPos: TObserverPosSpec read FObserverPos;
  end;


  { Request for a series with positions for one or more celestial points. }
  TSeriesSingleRequest = class(TSeriesRequest)
  strict private
    FCelPoints: TCelPointSpecArray;
  public
    constructor Create(PPeriod: TPeriod; PCycleType: TCycleTypeSpec; PAyanamsha: TAyanamshaSpec;
      PCoordinate: TCoordinateSpec; PObserverPos: TObserverPosSpec; PCelPoints: TCelPointSpecArray);
    property CelPoints: TCelPointSpecArray read FCelPoints;
  end;

  { Request for a series with positions for two celestial points and a combined result. }
  TSeriesPairedRequest = class(TSeriesRequest)
  strict private
    FCelPointPairs: TCelPointPairedSpecArray;
  public
    constructor Create(PPeriod: TPeriod; PCycleType: TCycleTypeSpec; PAyanamsha: TAyanamshaSpec;
      PCoordinate: TCoordinateSpec; PObserverPos: TObserverPosSpec; PCelPointPairs: TCelPointPairedSpecArray);
    property CelPointPairs: TCelPointPairedSpecArray read FCelPointPairs;
  end;

  { Response for several types of series requests. }

  { TSeriesResponse }

  TSeriesResponse = class
  strict private
    FErrors: boolean;
    FErrorText: string;
    FFileNameData, FFileNameMeta: string;
  public
    constructor Create(PFilenameData, PFilenameMeta, PErrorText: string; PErrors: boolean);
    property Errors: boolean read FErrors;
    property ErrorText: string read FErrorText;
    property FilenameData: string read FFileNameData;
    property FileNameMeta: string read FFileNameMeta;
  end;



implementation

{ TSeriesResponse }

constructor TSeriesResponse.Create(PFilenameData, PFilenameMeta, PErrorText: string; PErrors: boolean);
begin
  FFileNameData := PFilenameData;
  FFileNameMeta := PFilenameMeta;
  FErrorText := PErrorText;
  FErrors := PErrors;
end;



{ TSeriesRequest }

constructor TSeriesRequest.Create(PPeriod: TPeriod; PCycleType: TCycleTypeSpec;
  PAyanamsha: TAyanamshaSpec; PCoordinate: TCoordinateSpec; PObserverPos: TObserverPosSpec);
begin
  Fperiod := PPeriod;
  FCycleType := PCycleType;
  FAyanamsha := PAyanamsha;
  FCoordinate := PCoordinate;
  FObserverPos := PObserverPos;
end;

{ TSeriesPairedRequest }

constructor TSeriesPairedRequest.Create(PPeriod: TPeriod; PCycleType: TCycleTypeSpec;
  PAyanamsha: TAyanamshaSpec; PCoordinate: TCoordinateSpec; PObserverPos: TObserverPosSpec;
  PCelPointPairs: TCelPointPairedSpecArray);
begin
  FCelPointPairs := PCelPointPairs;
  inherited Create(PPeriod, PCycleType, PAyanamsha, PCoordinate, PObserverPos);
end;

{ TSeriesSingleRequest }

constructor TSeriesSingleRequest.Create(PPeriod: TPeriod; PCycleType: TCycleTypeSpec;
  PAyanamsha: TAyanamshaSpec; PCoordinate: TCoordinateSpec; PObserverPOs: TObserverPosSpec;
  PCelPoints: TCelPointSpecArray);
begin
  FCelPoints := PCelPoints;
  inherited Create(PPeriod, PCycleType, PAyanamsha, PCoordinate, PObserverPos);
end;

end.
