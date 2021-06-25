unit UnitValidation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitdomainxchg, unitastron;

type

  TDateTimeValidation = class
  strict private
    SeFrontend: TSeFrontend;
  public
    constructor Create;
    destructor Destroy; override;
    function CheckDate(DateText: string; PCalendar: integer): TValidatedDate;
  end;

implementation

uses
  StrUtils;

{ TDateTimeValidation ------------------------------------------------------------------------------------------------- }

constructor TDateTimeValidation.Create;
begin
  SeFrontend := TSeFrontend.Create;
end;

destructor TDateTimeValidation.Destroy;
begin
  FreeAndNil(SeFrontend);
  inherited;
end;

function TDateTimeValidation.CheckDate(DateText: string; PCalendar: integer): TValidatedDate;
var
  Year, Month, Day, ErrorCode: integer;
  SplittedInput: array of string;
  Valid: boolean;
  ValidatedDate: TValidatedDate;
  ValidatedJulianDay: TValidatedJulianDay;
begin
       Valid := True;
  SplittedInput := SplitString(DateText, '/');
  Val(SplittedInput[0], Year, ErrorCode);
  if ErrorCode <> 0 then Valid := False;
  Val(SplittedInput[1], Month, ErrorCode);
  if ErrorCode <> 0 then Valid := False;
  Val(SplittedInput[2], Day, ErrorCode);
  if ErrorCode <> 0 then Valid := False;
  if not ((PCalendar = 0) or (PCalendar = 1)) then Valid := False;
  if Valid then begin
    ValidatedJulianDay:= SeFrontend.CheckDate(Year, Month, Day, PCalendar);
    if (ValidatedJulianDay.IsValid) then ValidatedDate.JulianDay:= ValidatedJulianDay.JulianDay else Valid:= false;
  end;
  ValidatedDate.Year := Year;
  ValidatedDate.Month := Month;
  ValidatedDate.Day := Day;
  ValidatedDate.Calendar:= PCalendar;
  ValidatedDate.IsValid := Valid;
  Result := ValidatedDate;
end;


end.
