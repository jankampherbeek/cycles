{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit test_conversions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, UnitConversions, UnitAstron;

type

  TestJulianDayConversion = class(TTestCase)
  protected
    JulianDayConversion: TJulianDayConversion;
        SeFrontend: TSeFrontend;
    Delta: double;
    procedure SetUp; override;
    procedure TearDown; override;
    published
      procedure TestConvertJdToDateText;
  end;

  TestFloatingToDecimalDegreeConversion = class(TTestCase)
    protected
      Conversion: TFloatingToDecimalDegreeConversion;
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure TestConvertDoubleToFormattedText;
  end;

implementation

{ TestJulianDayConversion -------------------------------------------------------------------------------------------- }
procedure TestJulianDayConversion.SetUp;
begin
  SeFrontend:= TSeFrontend.Create;
  JulianDayConversion:= TJulianDayConversion.Create(SeFrontend);
  inherited;
end;

procedure TestJulianDayConversion.TearDown;
begin
  FreeAndNil(SeFrontend);
  inherited;
end;

procedure TestJulianDayConversion.TestConvertJdToDateText;
var
  DateText: string;
  Jd: Double = 2434406.5;       // 1953-1-29
begin
  DateText:= JulianDayConversion.ConvertJdToDateText(Jd, 1);
  AssertEquals('1953/01/29', DateText);
end;


{ TestFloatingToDecimalDegreeConversion ------------------------------------------------------------------------------ }

procedure TestFloatingToDecimalDegreeConversion.SetUp;
begin
  Conversion:= TFloatingToDecimalDegreeConversion.Create;
end;

procedure TestFloatingToDecimalDegreeConversion.TearDown;
begin
  FreeAndNil(Conversion);
end;

procedure TestFloatingToDecimalDegreeConversion.TestConvertDoubleToFormattedText;
begin
  AssertEquals('024,50000', Conversion.ConvertDoubleToFormattedText(24.5));
end;

initialization

  RegisterTest('Conversions', TestJulianDayConversion);
  RegisterTest('Conversions', TestFloatingToDecimalDegreeConversion);
end.

