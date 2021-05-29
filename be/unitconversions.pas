{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit UnitConversions;

{< Unit for conversions and formatting of results.}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnitAstron, unitdomainxchg;

type

  { Converts a Julian Day number into a string with a formatted date, using yyyy/mm/dd }
  TJulianDayConversion = class
  strict private
    FSeFrontend: TSeFrontend;
    FmtMonthDay: string;
    FmtYear: string;
  public
    constructor Create(SeFrontend: TSeFrontend);
    function ConvertJdToDateText(JdNr: double; Calendar: integer): string;
  end;

  { Converts a Double into a string representing a decimal degree, using ddd.fffff, 3 positions for degrees
  and 5 for the fraction }
  TFloatingToDecimalDegreeConversion = class
  public
    function ConvertDoubleToFormattedText(FormatValue: double): string;
  end;


implementation

{ TJulianDayConversion ----------------------------------------------------------------------------------------------- }

constructor TJulianDayConversion.Create(SeFrontend: TSeFrontend);
begin
  FSeFrontend := SeFrontend;
  FmtMonthDay := '%2.2d';
  FmtYear := '%4.4d';
end;

function TJulianDayConversion.ConvertJdToDateText(JdNr: double; Calendar: integer): string;
var
  DateTime: TSimpleDateTime;
  DateText: string;
begin
  DateTime := FSeFrontend.SeRevJul(JdNr, Calendar);
  DateText := format(fmtYear, [DateTime.Year]) + '/' + format(fmtMonthDay, [DateTime.Month]) +
    '/' + format(fmtMonthDay, [DateTime.Day]);
  Result := DateText;

end;

{ TFloatingToDecimalDegreeConversion --------------------------------------------------------------------------------- }

{ TODO : Find solution for decimal separator. Locality dependant or configurable. }
function TFloatingToDecimalDegreeConversion.ConvertDoubleToFormattedText(FormatValue: double): string;
var
  FormatString: string;
begin
  FormatString:= '000.00000';
  Result:= FormatFloat(FormatString,FormatValue);
end;

end.

