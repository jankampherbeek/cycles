unit UnitDlgCoordinate;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, Forms, Graphics, StdCtrls, SysUtils, UnitCentralController;

type

  { TFormDlgCoordinate }

  TFormDlgCoordinate = class(TForm)
    BtnCancel: TButton;
    BtnContinue: TButton;
    BtnHelp: TButton;
    CbCoordinate: TComboBox;
    LblTitle: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CenCon: TCenCon;
    StateMachine: TFinStateMachine;
    procedure SaveState;
    procedure DefineCoordinateItems;

  public

  end;

var
  FormDlgCoordinate: TFormDlgCoordinate;

implementation

{$R *.lfm}

uses
  unitdomainxchg;

var
  SelectedCoordinate: TCoordinateSpec;

procedure TFormDlgCoordinate.FormShow(Sender: TObject);
begin
  CenCon := TCenCon.Create;
  StateMachine := TFinStateMachine.Create;
  DefineCoordinateItems;
end;

procedure TFormDlgCoordinate.SaveState;
begin
  StateMachine.Coordinate := SelectedCoordinate;
end;

procedure TFormDlgCoordinate.BtnCancelClick(Sender: TObject);
begin
  Close;
  StateMachine.ChangeState(Cancel);
end;

procedure TFormDlgCoordinate.BtnContinueClick(Sender: TObject);
begin
  Hide;
  SaveState;
  StateMachine.ChangeState(CoordinateDefined);
end;

procedure TFormDlgCoordinate.BtnHelpClick(Sender: TObject);
begin
  ShowMessage('To be replaced with help');
end;


procedure TFormDlgCoordinate.DefineCoordinateItems;
var
  AllCoordinates: TCoordinateSpecArray;
  i, NrOfCoordinates: integer;
  var ObserverPos, Coordinate: string;
begin
  AllCoordinates := CenCon.LookupValues.AllCoordinates;
  CbCoordinate.Items.Clear;
  NrOfCoordinates := Length(AllCoordinates);
  Observerpos:= StateMachine.ObserverPos.Identification;
  for i := 0 to NrOfCoordinates - 1 do begin
    Coordinate:= AllCoordinates[i].Identification;
    if (not(Observerpos = 'Heliocentric')
      or (not(Coordinate = 'RightAsc') and not (Coordinate = 'Decl')))
    then CbCoordinate.Items.add(AllCoordinates[i].Name);
  end;
  CbCoordinate.ItemIndex := 0;
  SelectedCoordinate := AllCoordinates[0];
end;




end.

