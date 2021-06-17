{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitProcess;

{< Classes for processing calculations. }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitastron, UnitConversions, unitdomainxchg, UnitReqResp;

type

  { Flags as used by then Swiss Ephemeris. Assumes that always the SE-files are used and thart speed has always to be calculated.}
  TSeFlags = class
  strict private
    FFlagsValue: longint;
    FCoordinateType: TCoordinateSpec;
    FAyanamsha: TAyanamshaSpec;
    function DefineFlags: longint;
  public
    constructor Create(PCoordinateSpec: TCoordinateSpec; PAyanamsha: TAyanamshaSpec);
    property FlagsValue: longint read FFlagsValue;
  end;

  { TSingleSeries }

  TSingleSeries = class
  strict private
    FloatingToDecimalDegreeConversion: TFloatingToDecimalDegreeConversion;
    JulianDayConversion: TJulianDayConversion;
    Ephemeris: TEphemeris;
  public
    constructor Create;
    destructor Destroy; override;
    function Positions(CelPoints: TCelPointSpecArray; Flags: longint; Coordinate: TCoordinateSpec;
      JdNr: double; Calendar: integer): string;
  end;


    { TPairedSeries }

  TPairedSeries = class
  strict private
    FloatingToDecimalDegreeConversion: TFloatingToDecimalDegreeConversion;
    JulianDayConversion: TJulianDayConversion;
    Ephemeris: TEphemeris;
    function DefineAngle(Pos1, Pos2: Double): double;
  public
    constructor Create;
    destructor Destroy; override;
    function Positions(CelPointPairs: TCelPointPairedSpecArray;
      CycleType: TCycleTypeSpec; Flags: longint; Coordinate: TCoordinateSpec;
  JdNr: double; Calendar: integer): string;
  end;

  //{ Constructs a TimeSeries for a specific celestial point, and the specs found in a CycleDefinition. }
  //TTimeSeries = class
  //strict private
  //  FCelPoint: TCelPointSpec;
  //  FCycleDefinition: TCycleDefinition;
  //  FEphemeris: TEphemeris;
  //  FJulianDayConversion: TJulianDayConversion;
  //  FloatingToDecimalDegreeConversion: TFloatingToDecimalDegreeConversion;
  //  FCalendar: integer;
  //  FileNamePrefix: string;
  //  FFileNameData, FFileNameMeta: string;
  //  procedure DefineFiles;
  //  procedure WriteMeta;
  //  procedure Calculate;

  //public
  //  constructor Create(PEphemeris: TEphemeris; PCelPoint: TCelPointSpec; PCycleDefinition: TCycleDefinition;
  //    Calendar: integer);
  //  property FileNameData: string read FFileNameData;
  //  property FileNameMeta: string read FFileNameMeta;
  //end;

  //TSeriesArray = array of TTimeSeries;

  { Converter for date and time. Returns Julian day for UT for date in format yyyy/mm/dd.
    Calendar: 1 = Gregorian, 0 = Julian. }
  TDateTimeConversion = class
  strict private
    FEphemeris: TEphemeris;
  public
    constructor Create(PEphemeris: TEphemeris);
    function DateTextToJulianDay(PDateText: string; PCalendar: integer): double;
  end;

  { Handles requests for the calculation of a TimeSeries. }

  { TSeriesHandler }

  TSeriesHandler = class
  strict private
    Ephemeris: TEphemeris;
    DatetimeConversion: TDateTimeConversion;
    JulianDayConversion: TJulianDayConversion;
    SingleSeries: TSingleSeries;
    StartJD, EndJD: double;
    CelPoints: TCelPointSpecArray;
    CelPointPairs: TCelPointPairedSpecArray;
    NrOfCelPoints, Calendar: integer;
    MetaHeader, MetaFilename, CsvHeader, CsvFilename: string;
    CsvFile, MetaFile: TextFile;
    procedure HandleMetaFile(CycleType: TCycleTypeSpec; Coordinate: TCoordinateSpec;
      Ayanamsha: TAyanamshaSpec; Period: TPeriod);
  public
    constructor Create;
    destructor Destroy; override;
    function HandleSingleRequest(Request: TSeriesSingleRequest): TSeriesResponse;
    function HandlePairedRequest(Request: TSeriesPairedRequest): TSeriesResponse;
  end;

implementation

uses
  StrUtils, swissdelphi, Types;

{ TPairedSeries }

function TPairedSeries.DefineAngle(Pos1, Pos2: Double): double;
var
  Angle: Double;
begin
  Angle:= Pos1 - Pos2;
  if (Angle < 0.0) then Angle:= Angle + 360.0;
  if (Angle > 180.0) then Angle:= 360.0 - Angle;
  result:= Angle;
end;

constructor TPairedSeries.Create;
begin
  Ephemeris := TEphemeris.Create;
  JulianDayConversion := TJulianDayConversion.Create(Ephemeris.SeFrontend);
end;

destructor TPairedSeries.Destroy;
begin
  FreeAndNil(JulianDayConversion);
  FreeAndNil(Ephemeris);
  inherited Destroy;
end;

function TPairedSeries.Positions(CelPointPairs: TCelPointPairedSpecArray; CycleType: TCycleTypeSpec;
  Flags: longint; Coordinate: TCoordinateSpec; JdNr: double; Calendar: integer
  ): string;
var
  i: integer;
  FirstPosition, SecondPosition, CombinedValue: double;
  CsvString, DateTimeText, PositionText: string;
  FirstFullPosForCoordinate, SecondFullPosForCoordinate: TFullPosForCoordinate;
begin
  DateTimeText := JulianDayConversion.ConvertJdToDateText(Jdnr, Calendar);
  PositionText := '';
  for i := 0 to Length(CelPointPairs) - 1 do begin
    FirstFullPosForCoordinate := Ephemeris.CalcCelPoint(JdNr, CelPointPairs[i].FirstCP.SeId, flags);
    SecondFullPosForCoordinate := Ephemeris.CalcCelPoint(JdNr, CelPointPairs[i].SecondCP.SeId, flags);
    case Coordinate.Identification of
      'Longitude', 'RightAsc': begin
        FirstPosition := FirstFullPosForCoordinate.MainPos;
        SecondPosition:= SecondFullPosForCoordinate.MainPos;
      end;
      'Latitude', 'Decl': begin
        FirstPosition := FirstFullPosForCoordinate.DeviationPos;
        SecondPosition:= SecondFullPosForCoordinate.DeviationPos;
      end;
      'Radv': begin
        FirstPosition := FirstFullPosForCoordinate.DistancePos;
        SecondPosition:= SecondFullPosForCoordinate.DistancePos;
      end;

    end;

    // calculate value, depending on cycletype      voor CycleTYpe 'Angle' de kortste hoek berekenen
    case CycleType.Identification of
    'Angle': CombinedValue:= DefineAngle(FirstPosition, SecondPosition);
    end;
    PositionText := PositionText + FloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(FirstPosition) +
    ';' + FloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(SecondPosition) +
    ';' + FloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(CombinedValue);



end;

Result := DateTimeText + '; ' + FloatToStr(JdNr) + '; ' + PositionText;

end;

{ TSingleSeries }

constructor TSingleSeries.Create();
begin
  Ephemeris := TEphemeris.Create;
  JulianDayConversion := TJulianDayConversion.Create(Ephemeris.SeFrontend);
end;

destructor TSingleSeries.Destroy;
begin
  FreeAndNil(JulianDayConversion);
  FreeAndNil(Ephemeris);
  inherited Destroy;
end;

function TSingleSeries.Positions(CelPoints: TCelPointSpecArray; Flags: longint;
  Coordinate: TCoordinateSpec; JdNr: double; Calendar: integer): string;
var
  i: integer;
  Position: double;
  CsvString, DateTimeText, PositionText: string;
  FullPosForCoordinate: TFullPosForCoordinate;
begin
  DateTimeText := JulianDayConversion.ConvertJdToDateText(Jdnr, Calendar);
  PositionText := '';
  for i := 0 to Length(CelPoints) - 1 do begin
    FullPosForCoordinate := Ephemeris.CalcCelPoint(JdNr, CelPoints[i].SeId, flags);
    case Coordinate.Identification of
      'Longitude', 'RightAsc': Position := FullPosForCoordinate.MainPos;
      'Latitude', 'Decl': Position := FullPosForCoordinate.DeviationPos;
      'Radv': Position := FullPosForCoordinate.DistancePos;
    end;
    PositionText := PositionText + FloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(Position) + ';';
  end;
  Result := DateTimeText + '; ' + FloatToStr(JdNr) + '; ' + PositionText;
end;


{ TSeFlags ----------------------------------------------------------------------------------------------------------- }

constructor TSeFlags.Create(PCoordinateSpec: TCoordinateSpec; PAyanamsha: TAyanamshaSpec);
begin
  FCoordinateType := PCoordinateSpec;
  FAyanamsha := PAyanamsha;
  FFlagsValue := DefineFlags;
end;

function TSeFlags.DefineFlags: longint;
var
  Flags: longint;
begin
  Flags := 2 or 256;                                   // Swiss Ephemeris and speed
  case FCoordinateType.Identification of

    'HelioLong', 'HelioLat': Flags := Flags or 8;      // heliocentric ecliptic
    'RightAsc', 'Decl': Flags := Flags or 2048;        // geocentric equatorial
    'GeoLong', 'GeoLat': Flags := 2 or 256;            // geocentric ecliptic
    { TODO : Create flags for topocentric }

    { TODO : Create else for case (CoordinateTypes for Flags), should throw an exception. }
  end;
  { TODO : Test compariosn with AyanamshaSpec.Name using i18N }
  if FAyanamsha.Name <> 'None' then Flags := Flags or (64 * 1024);   // sidereal
  Result := Flags;
end;

//{ TTimeSeries -------------------------------------------------------------------------------------------------------- }

//constructor TTimeSeries.Create(PEphemeris: TEphemeris; PCelPoint: TCelPointSpec;
//  PCycleDefinition: TCycleDefinition; Calendar: integer);
//begin
//  FEphemeris := PEphemeris;
//  FCalendar := Calendar;
//  FJulianDayConversion := TJulianDayConversion.Create(FEphemeris.SeFrontend);
//  FCelPoint := PCelPoint;
//  FCycleDefinition := PCycleDefinition;
//  DefineFiles;
//  WriteMeta;
//  Calculate;
//end;

//procedure TTimeSeries.DefineFiles;
//begin
//  FileNamePrefix := 'data/' + FCelPoint.Name;
//  FFileNameData := FilenamePrefix + '_data.csv';
//  FFileNameMeta := FileNamePrefix + '_meta.csv';
//end;

//{ TODO : Variablenames in file should vary based on i18n. }
//procedure TTimeSeries.WriteMeta;
//var
//  MetaHeading: string;
//  MetaFile: TextFile;
//begin
//  MetaHeading := 'Variable; Value';
//  AssignFile(MetaFile, FFileNameMeta);
//  try
//    rewrite(MetaFile);
//    writeLn(MetaFile, MetaHeading);
//    writeln(MetaFile, 'File identification;' + FileNamePrefix);
//    writeln(MetaFile, 'Celestial Point;' + FCelPoint.Name);
//    writeln(MetaFile, 'CycleType;' + FCycleDefinition.CycleType.Name);
//    writeln(MetaFile, 'Coordinate;' + FCycleDefinition.CoordinateType.Name);
//    writeln(MetaFile, 'Ayanamsha;' + FCycleDefinition.Ayanamsha.Name);
//    writeln(MetaFile, 'Start Date;' + FJulianDayConversion.ConvertJdToDateText(FCycleDefinition.JdStart, FCalendar));
//    writeln(MetaFile, 'End Date;' + FJulianDayConversion.ConvertJdToDateText(FCycleDefinition.JdEnd, FCalendar));
//    writeln(MetaFile, 'Interval;' + IntToStr(FCycleDefinition.Interval));
//  finally
//    CloseFile(MetaFile);
//  end;
//end;

//procedure TTimeSeries.Calculate;
//var
//  ActualJd, BeginJd, EndJd, Position: double;
//  SeId, Interval, i: integer;
//  Flags: longint;
//  FullPosForCoordinate: TFullPosForCoordinate;
//  CoordinateSpec: TCoordinateSpec;
//  SeFlags: TSeFlags;
//  CsvFile: TextFile;
//  CsvLine, CsvHeading, DateTimeText, PositionText: string;
//begin
//  CoordinateSpec := FCycleDefinition.coordinateType;
//  BeginJd := FCycleDefinition.JdStart;
//  EndJd := FCycleDefinition.JdEnd;
//  ActualJd := BeginJd;
//  Interval := FCycleDefinition.Interval;
//  SeId := FCelPoint.seId;
//  SeFlags := TSeFlags.Create(CoordinateSpec, FCycleDefinition.Ayanamsha);
//  Flags := SeFlags.FlagsValue;
//  { TODO : Check use of 'None' in combination with i18N }
//  if not (FCycleDefinition.Ayanamsha.Name = 'None') then swe_set_sid_mode(FCycleDefinition.Ayanamsha.SeId, 0.0, 0.0);
//  CsvHeading := Concat('Date; Julian Day nr;');
//  AssignFile(CsvFile, FFileNameData);
//  try
//    rewrite(CsvFile);
//    writeLn(CsvFile, CsvHeading);
//    repeat
//      FullPosForCoordinate := FEphemeris.CalcCelPoint(ActualJd, SeId, flags);
//      case CoordinateSpec.Identification of
//        'GeoLong', 'HelioLong', 'RightAsc': Position := FullPosForCoordinate.MainPos;
//        'GeoLat', 'HelioLat', 'Decl': Position := FullPosForCoordinate.DeviationPos;
//        'Radv': Position := FullPosForCoordinate.distancePos;
//        { TODO : Create else for case (CoordinateTypes), should throw an exception. }
//      end;
//      DateTimeText := FJulianDayConversion.ConvertJdToDateText(ActualJd, FCalendar);
//      PositionText := FloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(Position);
//      CsvLine := DateTimeText + '; ' + FloatToStr(ActualJd) + '; ' + PositionText;
//      writeln(CsvFile, CsvLine);
//      ActualJd := ActualJd + Interval;
//    until ActualJd > EndJd;
//  finally
//    CloseFile(CsvFile);
//  end;
//end;



{ TDateTimeConversion ------------------------------------------------------------------------------------------------ }

constructor TDateTimeConversion.Create(PEphemeris: TEphemeris);
begin
  FEphemeris := PEphemeris;
end;

{ TODO : Add validation to conversion from datetext to JD }
{ TODO : combine with DateTimeValidation }
function TDateTimeConversion.DateTextToJulianDay(PDateText: string; PCalendar: integer): double;
var
  TextElements: TStringDynArray;
  Day, Month, Year, Calendar: integer;
  UT: double;
begin

  Calendar := PCalendar;
  UT := 0.0;
  TextElements := SplitString(PDateText, '/');
  Year := StrToInt(TextElements[0]);
  Month := StrToInt(TextElements[1]);
  Day := StrToInt(TextElements[2]);
  Result := FEphemeris.CalcJdUt(Year, Month, Day, UT, Calendar);
end;

{ TSeriesHandler ------------------------------------------------------------------------------------------------- }

constructor TSeriesHandler.Create;
begin
  Ephemeris := TEphemeris.Create;
  SingleSeries := TSingleSeries.Create;
  JulianDayConversion := TJulianDayConversion.Create(Ephemeris.SeFrontend);
end;

destructor TSeriesHandler.Destroy;
begin
  FreeAndNil(JulianDayConversion);
  FreeAndNil(SingleSeries);
  FreeAndNil(Ephemeris);
  inherited;
end;

function TSeriesHandler.HandleSingleRequest(Request: TSeriesSingleRequest): TSeriesResponse;
var
  CycleDefinition: TCycleDefinition;
  Response: TSeriesResponse;
  SeFlags: TSeFlags;
  i: integer;
  Flags: longint;
  CurrentJd: double;
  CsvLine: string;
begin
  CelPoints := Request.CelPoints;
  CycleDefinition.ObserverPos := Request.ObserverPos;
  Calendar := Request.Period.StartDate.Calendar;
  SeFlags := TSeFlags.Create(Request.Coordinate, Request.Ayanamsha);
  Flags := SeFlags.FlagsValue;
  try
    CsvFileName := Request.Coordinate.Name + '_data.csv';
    AssignFile(CsvFile, CsvFilename);
    CsvHeader := Concat('Date; Julian Day nr;');
    for i := 0 to Length(CelPoints) do CsvHeader := CsvHeader + CelPoints[i].Name + ';';
    rewrite(CsvFile);
    writeLn(CsvFile, CsvHeader);
    CurrentJd := Request.Period.StartDate.JulianDay;
    repeat
      CsvLine := SingleSeries.Positions(CelPoints, Flags, Request.Coordinate, CurrentJd, Calendar);
      writeln(CsvFile, CsvLine);
    until CurrentJd > Request.Period.EndDate.JulianDay;
  finally
    CloseFile(CsvFile);
  end;
  HandleMetaFile(Request.CycleType, Request.Coordinate, Request.Ayanamsha, Request.Period);
end;

function TSeriesHandler.HandlePairedRequest(Request: TSeriesPairedRequest): TSeriesResponse;
var
  CycleDefinition: TCycleDefinition;
  Response: TSeriesResponse;
  SeFlags: TSeFlags;
  i: integer;
  Flags: longint;
  CurrentJd: double;
  CsvLine, PairText: string;
begin
  CelPointPairs := Request.CelPointPairs;
  CycleDefinition.ObserverPos := Request.ObserverPos;
  Calendar := Request.Period.StartDate.Calendar;
  SeFlags := TSeFlags.Create(Request.Coordinate, Request.Ayanamsha);
  Flags := SeFlags.FlagsValue;
  try
    CsvFileName := Request.Coordinate.Name + '_data.csv';
    AssignFile(CsvFile, CsvFilename);
    CsvHeader := Concat('Date; Julian Day nr;');
    for i:= 0 to Length(CelPointPairs) -1 do begin
      PairText:= CelPointPairs[0].FirstCP.Name + ';' + CelPointPairs[0].SecondCP.Name + ';' + 'Value';
      CsvHeader:= CsvHeader + PairText;
    end;
    rewrite(CsvFile);
    writeLn(CsvFile, CsvHeader);
    CurrentJd := Request.Period.StartDate.JulianDay;
    repeat
          //  voor CycleTYpe 'Angle' de kortste hoek berekenen

    until CurrentJd > Request.Period.EndDate.JulianDay;

  finally
  end;


end;

procedure TSeriesHandler.HandleMetaFile(CycleType: TCycleTypeSpec; Coordinate: TCoordinateSpec;
  Ayanamsha: TAyanamshaSpec; Period: TPeriod);
begin
  try
    MetaFilename := Coordinate.Name + '_meta.csv';
    AssignFile(MetaFile, MetaFilename);
    writeln(MetaFile, 'File identification;' + CsvFilename);
    writeln(MetaFile, 'CycleType;' + CycleType.Name);
    writeln(MetaFile, 'Coordinate;' + Coordinate.Name);
    writeln(MetaFile, 'Ayanamsha;' + Ayanamsha.Name);
    writeln(MetaFile, 'Start Date;' + JulianDayConversion.ConvertJdToDateText(Period.StartDate.JulianDay, Calendar));
    writeln(MetaFile, 'End Date;' + JulianDayConversion.ConvertJdToDateText(Period.EndDate.JulianDay, Calendar));
    writeln(MetaFile, 'Interval;' + IntToStr(Period.Interval));
  finally
    CloseFile(MetaFile);
  end;
end;




//{ TODO : Remove obsolete function HandleRequest from TSeriesHandler }
//function TSeriesHandler.HandleRequest(Request: TTimeSeriesRequest): TSeriesResponse;
//var

//  StartDate, EndDate, CsvLine: string;
//  AllTimeSeries: TSeriesArray;
//  CycleDefinition: TCycleDefinition;
//  Response: TSeriesResponse;
//  i: integer;
//  Flags: longint;
//  SeFlags: TSeFlags;
//  CurrentJd: double;
//begin
//  StartDate := Request.StartDateTime;
//  EndDate := Request.EndDateTime;
//  Calendar := Request.Calendar;
//  CelPoints := Request.CelPoints;
//  NrOfCelPoints := Length(CelPoints);
//  SetLength(AllTimeSeries, NrOfCelPoints);
//  CycleDefinition.JdStart := Request.StartJD;
//  CycleDefinition.JdEnd := Request.EndJD;
//  CycleDefinition.Interval := Request.Interval;
//  CycleDefinition.CoordinateType := Request.CoordinateType;
//  CycleDefinition.Ayanamsha := Request.Ayanamsha;
//  CycleDefinition.CycleType := Request.CycleType;




//  // define header for csv


//  SeFlags := TSeFlags.Create(Request.CoordinateType, Request.Ayanamsha);
//  Flags := SeFlags.FlagsValue;
//  CurrentJd := CycleDefinition.JdStart;
//  repeat
//    CsvLine := SingleSeries.Positions(CelPoints, Flags, CycleDefinition.CoordinateType, CurrentJd, Calendar);
//    // write csvLine to file
//  until CurrentJd > CycleDefinition.JdEnd;



//  Response.Errors := False;
//  Response.ErrorText := '';
//  Response.FileNameData := AllTimeSeries[0].FileNameData;
//  Response.FileNameMeta := AllTimeSeries[0].FileNameMeta;
//  Result := Response;
//end;


end.
