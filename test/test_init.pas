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
  end;

implementation

procedure TTestLookupValues.SetUp;
begin
  LookUpValues:= TLookupValues.Create;
end;

procedure TTestLookupValues.TearDown;
begin
  FreeAndNil(LookupValues);
end;

procedure TTestLookupValues.TestAyanamshas;
begin
  AssertEquals(7, Length(LookUpValues.AllAyanamshas));
end;



initialization

  RegisterTest(TTestLookupValues);
end.

