{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitProcess;

{< Classes for processing calculation results. }
{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, Types, unitastron, UnitConversions, unitdomainxchg, UnitReqResp;

const
  SEMI_COLON = ';';
  SEMI_COL_SPACE = '; ';
  DATAFILE_POSTFIX = '_data.csv';
  METAFILE_POSTFIX = '_meta.csv';
  FREQFILE_POSTFIX = '_freq.csv';

type

  { TSeries }

  TSeries = class
  strict private
    FFloatingToDecimalDegreeConversion: TFloatingToDecimalDegreeConversion;
    FJulianDayConversion: TJulianDayConversion;
    FEphemeris: TEphemeris;
    Frequencies: TIntegerDynArray;
    FilenameData, FilenameFreq, FilenameMeta, ErrorText: string;
    Errors: boolean;
    procedure InitFrequency(PCycleDefinition: TCycleDefinition);
    procedure HandleFrequency(Position: double; Coordinate: TCoordinateSpec);
    procedure WriteCsvFile(CycleDefinition: TCycleDefinition; CelPoints: TCelPointSpecArray);
    procedure WriteCsvFile(CycleDefinition: TCycleDefinition; CelPointPairs: TCelPointPairedSpecArray);
    procedure WriteMetaFile(CycleDefinition: TCycleDefinition);
    procedure WriteFrequenciesFile(CycleDefinition: TCycleDefinition);
  public
    constructor Create(PEphemeris: TEphemeris; PJulianDayConversion: TJulianDayConversion;
      PFloatingToDecimaldegreeConversion: TFloatingToDecimalDegreeConversion);
    destructor Destroy; override;
    function HandleCycle(PCycleDefinition: TCycleDefinition; PCelPoints: TCelPointSpecArray): TSeriesResponse;
    function HandleCycle(PCycleDefinition: TCycleDefinition;
      PCelPointPairs: TCelPointPairedSpecArray): TSeriesResponse;
  end;


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



  { Converter for date and time. Returns Julian day for UT for date in format yyyy/mm/dd.
    Calendar: 1 = Gregorian, 0 = Julian. }
  TDateTimeConversion = class
  strict private
    FEphemeris: TEphemeris;
  public
    constructor Create(PEphemeris: TEphemeris);
    function DateTextToJulianDay(PDateText: string; PCalendar: integer): double;
  end;


implementation

uses
  Math, StrUtils, swissdelphi, UnitConst;

{ TSeries }

constructor TSeries.Create(PEphemeris: TEphemeris; PJulianDayConversion: TJulianDayConversion;
  PFloatingToDecimaldegreeConversion: TFloatingToDecimalDegreeConversion);
begin
  FEphemeris := PEphemeris;
  FJulianDayConversion := PJulianDayConversion;
  FFloatingToDecimalDegreeConversion := PFloatingToDecimaldegreeConversion;
end;

destructor TSeries.Destroy;
begin
  FreeAndNil(FJulianDayConversion);
  inherited Destroy;
end;

function TSeries.HandleCycle(PCycleDefinition: TCycleDefinition; PCelPoints: TCelPointSpecArray): TSeriesResponse;
begin
  Errors:= false;
  ErrorText:= '';
  InitFrequency(PCycleDefinition);
  WriteCsvFile(PCycleDefinition, PCelPoints);
  WriteMetaFile(PCycleDefinition);
  WriteFrequenciesFile(PCycleDefinition);
  Result:= TSeriesResponse.Create(FilenameData, FilenameFreq, FilenameMeta, ErrorText, Errors);
end;

function TSeries.HandleCycle(PCycleDefinition: TCycleDefinition;
  PCelPointPairs: TCelPointPairedSpecArray): TSeriesResponse;
begin
    Errors:= false;
  ErrorText:= '';
  InitFrequency(PCycleDefinition);
  WriteCsvFile(PCycleDefinition, PCelPointPairs);
  WriteMetaFile(PCycleDefinition);
  WriteFrequenciesFile(PCycleDefinition);
  Result:= TSeriesResponse.Create(FilenameData, FilenameFreq, FilenameMeta, ErrorText, Errors);
end;

procedure TSeries.InitFrequency(PCycleDefinition: TCycleDefinition);
var
  i: integer;
begin
  case PCycleDefinition.CoordinateType.Identification of
    COORD_LONG: SetLength(Frequencies, MAX_INDEX_LON - MIN_INDEX_LON + 1);
    COORD_LAT: SetLength(Frequencies, MAX_INDEX_LAT - MIN_INDEX_LAT + 1);
    COORD_RA: SetLength(Frequencies, MAX_INDEX_RA - MIN_INDEX_RA + 1);
    COORD_DECL: SetLength(Frequencies, MAX_INDEX_DECL - MIN_INDEX_DECL + 1);
    else begin
      SetLength(Frequencies, 0);
      Errors:= true;
      ErrorText:= ErrorText + 'Wrong coordinate while initializing frequency: ' + PCycleDefinition.CoordinateType.Identification;
    end;
  end;
  for i := 0 to Length(Frequencies) - 1 do Frequencies[i] := 0;
end;

procedure TSeries.HandleFrequency(Position: double; Coordinate: TCoordinateSpec);
var
  IntegerPos, Index: integer;
begin
  IntegerPos := Ceil(Position);
  case Coordinate.Identification of
    COORD_LONG: Index := IntegerPos - MIN_INDEX_LON;
    COORD_LAT: Index := IntegerPos - MIN_INDEX_LAT;
    COORD_RA: Index := IntegerPos - MIN_INDEX_RA;
    COORD_DECL: Index := IntegerPos - MIN_INDEX_DECL;
    else begin
      Index := -1;
      Errors:= true;
      ErrorText:= ErrorText + 'Wrong coordinate while calculating frequency: ' + Coordinate.Identification;
    end;
  end;
  if (Index >= 0) then Inc(Frequencies[Index]);
end;


procedure TSeries.WriteCsvFile(CycleDefinition: TCycleDefinition; CelPoints: TCelPointSpecArray);
var
  CsvHeader, CsvLine, DateTimeText, PositionText: string;
  CsvFile: TextFile;
  Position, CurrentJd: double;
  SeFlags: TSeFlags;
  Flags: longint;
  i: integer;
  FullPosForCoordinate: TFullPosForCoordinate;
begin
  SeFlags := TSeFlags.Create(CycleDefinition.CoordinateType, CycleDefinition.Ayanamsha);
  Flags := SeFlags.FlagsValue;
  CurrentJd := CycleDefinition.JdStart;
  try             { TODO : add exception branch for creating errortext }
    FilenameData := CycleDefinition.CoordinateType.Name + DATAFILE_POSTFIX;
    AssignFile(CsvFile, FilenameData);
    CsvHeader := Concat('Date; Julian Day nr;');
    for i := 0 to Length(CelPoints) - 1 do if (CelPoints[i].Selected) then
        CsvHeader := CsvHeader + CelPoints[i].Name + SEMI_COLON;
    rewrite(CsvFile);
    writeLn(CsvFile, CsvHeader);
    repeat
      DateTimeText := FJulianDayConversion.ConvertJdToDateText(CurrentJd, CycleDefinition.Calendar);
      PositionText := '';
      for i := 0 to Length(CelPoints) - 1 do if (CelPoints[i].Selected) then begin
          FullPosForCoordinate := FEphemeris.CalcCelPoint(CurrentJD, CelPoints[i].SeId, flags);
          case CycleDefinition.CoordinateType.Identification of
            COORD_LONG, COORD_RA: Position := FullPosForCoordinate.MainPos;
            COORD_LAT, COORD_DECL: Position := FullPosForCoordinate.DeviationPos;
            COORD_RADV: Position := FullPosForCoordinate.DistancePos;
          end;
          HandleFrequency(Position, CycleDefinition.CoordinateType);
          PositionText := PositionText + FFloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(
            Position) + SEMI_COLON;
        end;
      CsvLine := DateTimeText + SEMI_COL_SPACE + FloatToStr(CurrentJd) + SEMI_COL_SPACE + PositionText;
      writeln(CsvFile, CsvLine);
      CurrentJd := CurrentJd + 1.0;
    until CurrentJd > CycleDefinition.JdEnd;
  finally
    CloseFile(CsvFile);
  end;
end;

procedure TSeries.WriteCsvFile(CycleDefinition: TCycleDefinition; CelPointPairs: TCelPointPairedSpecArray);
var
  CsvHeader, CsvLine, DateTimeText, PositionText, PairText: string;
  CsvFile: TextFile;
  Position1, Position2, PosMax, PosMin, Angle, CurrentJd: double;
  SeFlags: TSeFlags;
  Flags: longint;
  i: integer;
  FullPosForCoordinate1, FullPosForCoordinate2: TFullPosForCoordinate;
begin
  SeFlags := TSeFlags.Create(CycleDefinition.CoordinateType, CycleDefinition.Ayanamsha);
  Flags := SeFlags.FlagsValue;
  CurrentJd := CycleDefinition.JdStart;
  try              { TODO : add exception branch for creating errortext }
    FilenameData := CycleDefinition.CoordinateType.Name + DATAFILE_POSTFIX;
    AssignFile(CsvFile, FilenameData);
    CsvHeader := Concat('Date; Julian Day nr;');
    for i := 0 to Length(CelPointPairs) - 1 do begin
      PairText := CelPointPairs[i].FirstCP.Name + SEMI_COL_SPACE + CelPointPairs[i].SecondCP.Name +
        SEMI_COL_SPACE + 'Value' + SEMI_COL_SPACE;
      CsvHeader := CsvHeader + PairText;
    end;
    rewrite(CsvFile);
    writeLn(CsvFile, CsvHeader);
    repeat
      DateTimeText := FJulianDayConversion.ConvertJdToDateText(CurrentJd, CycleDefinition.Calendar);
      PairText := '';
      for i := 0 to Length(CelPointPairs) - 1 do begin
        FullPosForCoordinate1 := FEphemeris.CalcCelPoint(CurrentJd, CelPointPairs[i].FirstCP.SeId, flags);
        FullPosForCoordinate2 := FEphemeris.CalcCelPoint(CurrentJd, CelPointPairs[i].SecondCP.SeId, flags);
        case CycleDefinition.CoordinateType.Identification of
          'Longitude', 'RightAsc': begin
            Position1 := FullPosForCoordinate1.MainPos;
            Position2 := FullPosForCoordinate2.MainPos;
          end;
          'Latitude', 'Decl': begin
            Position1 := FullPosForCoordinate1.DeviationPos;
            Position2 := FullPosForCoordinate2.DeviationPos;
          end;
          'Radv': begin
            Position1 := FullPosForCoordinate1.DistancePos;
            Position2 := FullPosForCoordinate2.DistancePos;
          end;
        end;
        // Calculate angle, after adding more combinations there should be a selection and separate classes or methods for each comparison.
        PosMax := Max(Position1, Position2);
        PosMin := Min(Position1, Position2);
        Angle := PosMax - PosMin;
        if (Angle > 180.0) then Angle := 360.0 - Angle;
        HandleFrequency(Angle, CycleDefinition.CoordinateType);
        PairText := PairText + FFloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(Position1) +
          SEMI_COL_SPACE + FFloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(Position2) +
          SEMI_COL_SPACE + FFloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(Angle) + SEMI_COLON;
      end;
      CsvLine := DateTimeText + SEMI_COL_SPACE + FloatToStr(CurrentJd) + SEMI_COL_SPACE + PairText;
      writeln(CsvFile, CsvLine);
      CurrentJd := CUrrentJd + 1.0;
    until CurrentJd > CycleDefinition.JdEnd;
  finally
    CloseFile(CsvFile);
  end;
end;

procedure TSeries.WriteMetaFile(CycleDefinition: TCycleDefinition);
var
  MetaFile: TextFile;
begin
  try
    FilenameMeta := CycleDefinition.CoordinateType.Name + METAFILE_POSTFIX;
    AssignFile(MetaFile, FilenameMeta);
    rewrite(MetaFile);
    writeln(MetaFile, 'Property; Value');
    writeln(MetaFile, 'CycleType; ' + CycleDefinition.CycleType.Name);
    writeln(MetaFile, 'Coordinate; ' + CycleDefinition.CoordinateType.Name);
    writeln(MetaFile, 'Ayanamsha; ' + CycleDefinition.Ayanamsha.Name);
    writeln(MetaFile, 'Start Date; ' + FJulianDayConversion.ConvertJdToDateText(CycleDefinition.JdStart,
      CycleDefinition.Calendar));
    writeln(MetaFile, 'End Date; ' + FJulianDayConversion.ConvertJdToDateText(CycleDefinition.JdEnd,
      CycleDefinition.Calendar));
    writeln(MetaFile, 'Interval; ' + IntToStr(CycleDefinition.Interval));
  finally                                         { TODO : add exception branch for creating errortext }
    CloseFile(MetaFile);
  end;

end;

procedure TSeries.WriteFrequenciesFile(CycleDefinition: TCycleDefinition);
var
  i, Offset: integer;
  FreqText: string;
  FreqFile: TextFile;
begin
  case CycleDefinition.CoordinateType.Identification of
    COORD_LONG: Offset := MIN_INDEX_LON;
    COORD_LAT: Offset := MIN_INDEX_LAT;
    COORD_RA: Offset := MIN_INDEX_RA;
    COORD_DECL: Offset := MIN_INDEX_DECL;
    else Offset := 0;
  end;
  try
    FilenameFreq := CycleDefinition.CoordinateType.Name + FREQFILE_POSTFIX;
    AssignFile(FreqFile, FileNameFreq);
    rewrite(FreqFile);
    writeln(FreqFile, 'Bucket; Value');
    for i := 0 to Length(Frequencies) - 1 do begin
      FreqText := IntToStr(i + Offset) + SEMI_COL_SPACE + IntToStr(Frequencies[i]);
      writeln(FreqFile, FreqText);
    end;
  finally
    CloseFile(FreqFile);
  end;
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


end.
