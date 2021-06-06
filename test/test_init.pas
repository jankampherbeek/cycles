{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit test_init;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, unitinit, unitdomainxchg;

type

  TTestLookupValues= class(TTestCase)
  strict private
    LookupValues: TLookupValues;
  published
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TestAyanamshas;
    procedure TestCoordinates;
    procedure TestCycleTypes;
  end;

implementation

procedure TTestLookupValues.SetUp;
begin
  LookUpValues:= TLookupValues.Create;
  inherited;
end;

procedure TTestLookupValues.TearDown;
begin
  FreeAndNil(LookupValues);
  inherited;
end;

procedure TTestLookupValues.TestAyanamshas;
begin
  AssertEquals(7, Length(LookUpValues.AllAyanamshas));
end;

procedure TTestLookupValues.TestCoordinates;
begin
  AssertEquals(7, Length(LookupValues.AllCoordinates));
end;

procedure TTestLookupValues.TestCycleTypes;
begin
  AssertEquals(2, Length(LookupValues.AllCycleTypes));
end;

initialization

  RegisterTest(TTestLookupValues);
end.

