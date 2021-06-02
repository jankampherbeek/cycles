{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitProcess;

{< Classes for processing calculations. }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitdomainxchg, unitastron, UnitConversions, UnitReqResp;

type

  { Flags as used by then Swiss Ephemeris. Assumes that always the SE-files are used and thart speed has always to be calculated.}
  TSeFlags = class
  strict private
    FFlagsValue: longint;
    FCoordinateType: TCoordinateTypes;
    FAyanamsha: TAyanamshaSpec;
    function DefineFlags: longint;
  public
    constructor Create(PCoordinateType: TCoordinateTypes; PAyanamsha: TAyanamshaSpec);
    property FlagsValue: longint read FFlagsValue;
  end;


  { Constructs a TimeSeries for a specific celestial point, and the specs found in a CycleDefinition. }
  TTimeSeries = class
  strict private
    FCelPoint: TCelPoint;
    FCycleDefinition: TCycleDefinition;
    FEphemeris: TEphemeris;
    FJulianDayConversion: TJulianDayConversion;
    FloatingToDecimalDegreeConversion: TFloatingToDecimalDegreeConversion;
    FCalendar: integer;
    FileNamePrefix: string;
    FFileNameData, FFileNameMeta: string;
    procedure DefineFiles;
    procedure WriteMeta;
    procedure Calculate;

  public
    constructor Create(PEphemeris: TEphemeris; PCelPoint: TCelPoint; PCycleDefinition: TCycleDefinition;
      Calendar: integer);
      property FileNameData: string read FFileNameData;
      property FileNameMEta: string read FFileNameMeta;
  end;

  TTimeSeriesArray = array of TTimeSeries;

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
  TTimeSeriesHandler = class
  strict private
    Ephemeris: TEphemeris;
    DatetimeConversion: TDateTimeConversion;
    StartJD, EndJD: double;
    CelPoints: TCelPointArray;
    NrOfCelPoints, Calendar: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function HandleRequest(Request: TTimeSeriesRequest): TTimeSeriesResponse;
  end;

implementation

uses
  StrUtils, Types, swissdelphi;

{ TSeFlags ----------------------------------------------------------------------------------------------------------- }

constructor TSeFlags.Create(PCoordinateType: TCoordinateTypes; PAyanamsha: TAyanamshaSpec);
begin
  FCoordinateType := PCoordinateType;
  FAyanamsha := PAyanamsha;
  FFlagsValue := DefineFlags;
end;

function TSeFlags.DefineFlags: longint;
var
  Flags: longint;
begin
  Flags := 2 or 256;                                         // Swiss Ephemeris and speed
  case FCoordinateType of
    HelioLongitude, HelioLatitude: Flags := Flags or 8;      // heliocentric ecliptic
    RightAscension, Declination: Flags := Flags or 2048;     // geocentric equatorial
    GeoLongitude, GeoLatitude: Flags := 2 or 256;            // geocentric ecliptic
    { TODO : Create else for case (CoordinateTypes for Flags), should throw an exception. }
  end;
  { TODO : Test compariosn with AyanamshaSpec.Name using i18N }
  if FAyanamsha.Name <> 'None' then
    Flags := Flags or (64 * 1024);   // sidereal
  Result := Flags;
end;

{ TTimeSeries -------------------------------------------------------------------------------------------------------- }

constructor TTimeSeries.Create(PEphemeris: TEphemeris; PCelPoint: TCelPoint;
  PCycleDefinition: TCycleDefinition; Calendar: integer);
begin
  FEphemeris := PEphemeris;
  FCalendar := Calendar;
  FJulianDayConversion := TJulianDayConversion.Create(FEphemeris.SeFrontend);
  FCelPoint := PCelPoint;
  FCycleDefinition := PCycleDefinition;
  DefineFiles;
  WriteMeta;
  Calculate;
end;

procedure TTimeSeries.DefineFiles;
begin
  FileNamePrefix := FormatDateTime('YYYYMMDD_HHNNSS', Now) + '_' + FCelPoint.PresentationName;
  FFileNameData:= FilenamePrefix + '_data.csv';
  FFileNameMeta:= FileNamePrefix + '_meta.csv';
end;

{ TODO : Variablenames in file should vary based on i18n. }
procedure TTimeSeries.WriteMeta;
var
  MetaHeading: string;
  CycleTypeText, CoordinateTypeText: string;
  MetaFile: TextFile;
begin
  MetaHeading := 'Variable; Value';
  WriteStr(CycleTypeText, FCycleDefinition.CycleType);
  WriteStr(CoordinateTypeText, FCycleDefinition.CoordinateType);
  AssignFile(MetaFile, FFileNameMeta);
  try
    rewrite(MetaFile);
    writeLn(MetaFile, MetaHeading);
    writeln(MetaFile, 'File identification;' + FileNamePrefix);
    writeln(MetaFile, 'Celestial Point;' + FCelPoint.PresentationName);
    writeln(MetaFile, 'CycleType;' + CycleTypeText);
    writeln(MetaFile, 'Coordinate;' + CoordinateTypeText);
    writeln(MetaFile, 'Ayanamsha;' + FCycleDefinition.Ayanamsha.Name);
    writeln(MetaFile, 'Start Date;' + FJulianDayConversion.ConvertJdToDateText(FCycleDefinition.JdStart, FCalendar));
    writeln(MetaFile, 'End Date;' + FJulianDayConversion.ConvertJdToDateText(FCycleDefinition.JdEnd, FCalendar));
    writeln(MetaFile, 'Interval;' + IntToStr(FCycleDefinition.Interval));
  finally
    CloseFile(MetaFile);
  end;
end;

procedure TTimeSeries.Calculate;
var
  ActualJd, BeginJd, EndJd, Position: double;
  SeId, Interval: integer;
  Flags: longint;
  FullPosForCoordinate: TFullPosForCoordinate;
  CoordinateType: TCoordinateTypes;
  SeFlags: TSeFlags;
  CsvFile: TextFile;
  CsvLine: string;
  CsvHeading: string;
  DateTimeText, PositionText: string;
begin
  CoordinateType := FCycleDefinition.coordinateType;
  BeginJd := FCycleDefinition.JdStart;
  EndJd := FCycleDefinition.JdEnd;
  ActualJd := BeginJd;
  Interval := FCycleDefinition.Interval;
  SeId := FCelPoint.seId;
  SeFlags := TSeFlags.Create(CoordinateType, FCycleDefinition.Ayanamsha);
  Flags := SeFlags.FlagsValue;
  { TODO : Check use of 'None' in combination with i18N }
  if not(FCycleDefinition.Ayanamsha.Name = 'None') then begin
    swe_set_sid_mode(FCycleDefinition.Ayanamsha.SeId, 0.0, 0.0);
  end;


  CsvHeading := Concat('Date; Julian Day nr; Geoc. Longitude ', FCelPoint.PresentationName);
  AssignFile(CsvFile, FFileNameData);
  try
    rewrite(CsvFile);
    writeLn(CsvFile, CsvHeading);
    repeat
      FullPosForCoordinate := FEphemeris.CalcCelPoint(ActualJd, SeId, flags);
      case CoordinateType of
        GeoLongitude, HelioLongitude, RightAscension: Position := FullPosForCoordinate.MainPos;
        GeoLatitude, HelioLatitude, Declination: Position := FullPosForCoordinate.DeviationPos;
        Distance: Position := FullPosForCoordinate.distancePos;
        { TODO : Create else for case (CoordinateTypes), should throw an exception. }
      end;
      DateTimeText := FJulianDayConversion.ConvertJdToDateText(ActualJd, FCalendar);
      PositionText := FloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(Position);
      CsvLine := DateTimeText + '; ' + FloatToStr(ActualJd) + '; ' + PositionText;
      writeln(CsvFile, CsvLine);
      ActualJd := ActualJd + Interval;
    until ActualJd > EndJd;
  finally
    CloseFile(CsvFile);
  end;
end;



{ TDateTimeConversion ------------------------------------------------------------------------------------------------ }

constructor TDateTimeConversion.Create(PEphemeris: TEphemeris);
begin
  FEphemeris := PEphemeris;
end;

{ TODO : Add validation to conversion from datetext to JD }
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

{ TTimeSeriesHandler ------------------------------------------------------------------------------------------------- }

constructor TTimeSeriesHandler.Create;
begin
  Ephemeris := TEphemeris.Create;
  DatetimeConversion := TDateTimeConversion.Create(Ephemeris);
end;

destructor TTimeSeriesHandler.Destroy;
begin
  FreeAndNil(DatetimeConversion);
  FreeAndNil(Ephemeris);
  Inherited;
end;

function TTimeSeriesHandler.HandleRequest(Request: TTimeSeriesRequest): TTimeSeriesResponse;
var
  StartDate, EndDate: string;
  AllTimeSeries: TTimeSeriesArray;
  CycleDefinition: TCycleDefinition;
  Response: TTimeSeriesResponse;
  i: integer;
begin
  StartDate := Request.StartDateTime;
  EndDate := Request.EndDateTime;
  Calendar := Request.Calendar;
  CelPoints := Request.CelPoints;
  NrOfCelPoints := Length(CelPoints);
  SetLength(AllTimeSeries, NrOfCelPoints);
  StartJD := DatetimeConversion.DateTextToJulianDay(StartDate, Calendar);
  EndJD := DatetimeConversion.DateTextToJulianDay(EndDate, Calendar);
  CycleDefinition.JdStart := StartJD;
  CycleDefinition.JdEnd := EndJD;
  CycleDefinition.Interval := Request.Interval;
  CycleDefinition.CoordinateType := Request.CoordinateType;
  CycleDefinition.Ayanamsha := Request.Ayanamsha;
  CycleDefinition.CycleType := Request.CycleType;
  for i := 0 to NrOfCelPoints - 1 do
  begin
    { TODO : Add handling of multiple celestial points }
    AllTimeSeries[i] := TTimeSeries.Create(Ephemeris, CelPoints[i], CycleDefinition, Calendar);
  end;
  Response.Errors := False;
  Response.ErrorText := '';
  Response.FileNameData:= AllTimeSeries[0].FileNameData;
  Response.FileNameMeta:= AllTimeSeries[0].FileNameMeta;
  Result := Response;
end;


end.
