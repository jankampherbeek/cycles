unit test_validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, unitvalidation, unitdomainxchg;

type

  TestDatetimeValidation= class(TTestCase)
  protected
    DateTimeValidation: TDateTimeValidation;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCheckDateHappyFlow;
    procedure TestCheckDateError;
  end;

implementation

procedure TestDatetimeValidation.TestCheckDateHappyFlow;
var
  ValidatedDate: TValidatedDate;
begin
  ValidatedDate:= DateTimeValidation.CheckDate('2021/06/05', 1);
  AssertTrue(ValidatedDate.isValid);
  AssertEquals(2021, ValidatedDate.Year);
  AssertEquals(6, ValidatedDate.Month);
  AssertEquals(5, ValidatedDate.Day);
end;

procedure TestDatetimeValidation.TestCheckDateError;
var
  ValidatedDate: TValidatedDate;
begin
  ValidatedDate:= DateTimeValidation.CheckDate('2021/6/55', 1);
  AssertFalse(ValidatedDate.isValid);
end;

procedure TestDatetimeValidation.SetUp;
begin
  DateTimeValidation:= TDatetimeValidation.Create;
  inherited;
end;

procedure TestDatetimeValidation.TearDown;
begin
  FreeAndNil(DateTimeValidation);
  inherited;
end;

initialization

  RegisterTest(TestDatetimeValidation);
end.

