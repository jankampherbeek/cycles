{ Jan Kampherbeek, (c)  2021.
  Enigma Cycles is open source.
  Please check the file copyright.txt in the root of the source for further details }
unit unitcentralcontroller;

{< Unit for a set of singletons that manage the applications as a whole.}

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, unitdomainxchg, unitinit;

type

  TStates = (Init, Started, Cancelled, DefiningCycleType, DefiningCoordinate, DefiningPeriod, DefiningSingleCP,
    DefiningPairedCP, Confirming, Calculating);
  TStateMessages = (Start, Cancel, CycleTypeDefined, CoordinateDefined, PeriodDefined,
    PairedCPDefined, SingleCPDefined);

  TCenCon = class
  strict private
    FLookupValues: TLookupValues;
  public
    constructor Create;
    property LookupValues: TLookupValues read FLookupValues;
  end;

  { TFinStateMachine }

  TFinStateMachine = class
  strict private
    FCycleType: TCycleTypeSpec;
    FCoordinate: TCoordinateSpec;
    FAyanamsha: TAyanamshaSpec;
    FObserverPos: TObserverPosSpec;
    FPeriod: TPeriod;
    FSingleCPs: TCelPointSpecArray;
    FPairedCPs: TCelPointPairedSpecArray;
    CurrentState: TStates;
    CurrentForm: TForm;
  public
    constructor Create;
    property Ayanamsha: TAyanamshaSpec read FAyanamsha write FAyanamsha;
    property ObserverPos: TObserverPosSpec read FObserverPos write FObserverPos;
    property CycleType: TCycleTypeSpec read FCycleType write FCycleType;
    property Coordinate: TCoordinateSpec read FCoordinate write FCoordinate;
    property Period: TPeriod read FPeriod write FPeriod;
    property SingleCPs: TCelPointSpecArray read FSingleCPs write FSingleCPs;
    property PairedCPs: TCelPointPairedSpecArray read FPairedCPs write FPairedCPs;
    procedure ChangeState(Message: TStateMessages);
  end;



implementation

uses UnitDlgCoordinate, UnitDlgCycleType, UnitDlgPeriod, unitdlgsinglecp, unitdlgpairedcp, unitdlgconfirm;

var
  CenConSingleton: TCenCon = nil;
  FinStateMachineSingleton: TFinStateMachine = nil;

{ TFinStateMachine }

constructor TFinStateMachine.Create;
begin
  if not (Assigned(FinStateMachineSingleton)) then begin
    inherited;
    FinStateMachineSingleton := self;
    CurrentState := Init;
  end else
    self := FinStateMachineSingleton;
end;

procedure TFinStateMachine.ChangeState(Message: TStateMessages);
begin
  case Message of
    Start: begin
      CurrentState := DefiningCycleType;
      FormDlgCycleType.ShowModal;
    end;
    CycleTypeDefined: begin
      CurrentState := DefiningCoordinate;
      FormDlgCoordinate.ShowModal;
    end;
    CoordinateDefined: begin
      CurrentState:= DefiningPeriod;
      FormDlgPeriod.ShowModal;
    end;
    PeriodDefined: begin
      if ((CycleType.Identification = 'Position') or (CycleType.Identification = 'Frequency')) then begin
        CurrentState:= DefiningSingleCP;
        FormDlgSingleCP.ShowModal;
      end else begin
        CurrentState:= DefiningPairedCP;
        FormDlgPairedCP.ShowModal;
      end;
    end;
    PairedCpDefined, SingleCPDefined: begin
      CurrentState:= Confirming;
      FormDlgConfirm.ShowModal;
    end;

  end;

end;

constructor TCenCon.Create;
begin
  if not (Assigned(CenConSingleton)) then begin
    inherited;
    CenConSingleton := self;
    FLookUpvalues := TLookupValues.Create;
  end else
    self := CenConSingleton;
end;

end.
