unit UnitDlgCycleType;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, Forms, Graphics, StdCtrls, SysUtils, unitcentralcontroller;

type

  { TFormDlgCycleType }

  TFormDlgCycleType = class(TForm)
    BtnCancel: TButton;
    BtnContinue: TButton;
    BtnHelp: TButton;
    CbAyanamsha: TComboBox;
    CbCycleType: TComboBox;
    CbObserverPos: TComboBox;
    LblAyanamsha: TLabel;
    LblCycleType: TLabel;
    LblObserverPos: TLabel;
    LblTitle: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure CbAyanamshaSelect(Sender: TObject);
    procedure CbCycleTypeSelect(Sender: TObject);
    procedure CbObserverPosSelect(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CenCon: TCenCon;
    StateMachine: TFinStateMachine;
    procedure SaveState;
    procedure DefineAyanamshaItems;
    procedure DefineCycleTypeItems;
    procedure DefineObserverPos;

  public

  end;

var
  FormDlgCycleType: TFormDlgCycleType;

implementation


{$R *.lfm}

uses UnitDomainXchg;

var
  SelectedCycleType: TCycleTypeSpec;
  SelectedAyanamsha: TAyanamshaSpec;
  SelectedObserverPos: TObserverPosSpec;
  AllCycleTypes: TCycleTypeSpecArray;
  AllAyanamshas: TAyanamshaSpecArray;
  AllObserverPos: TObserverPosSpecArray;

procedure TFormDlgCycleType.FormShow(Sender: TObject);
begin
  CenCon := TCenCon.Create;
  StateMachine := TFinStateMachine.Create;
  DefineCycleTypeItems;
  DefineAyanamshaItems;
  DefineObserverPos;
end;

procedure TFormDlgCycleType.BtnContinueClick(Sender: TObject);
begin
  Hide;
  SaveState;
  StateMachine.ChangeState(CycleTypeDefined);
end;

procedure TFormDlgCycleType.BtnHelpClick(Sender: TObject);
begin
  ShowMessage('To be replaced with help');
end;

procedure TFormDlgCycleType.BtnCancelClick(Sender: TObject);
begin
  Close;
  StateMachine.ChangeState(Cancel);
end;

procedure TFormDlgCycleType.CbAyanamshaSelect(Sender: TObject);
var
  Index: Integer;
begin
  Index:= CbAyanamsha.ItemIndex;
  SelectedAyanamsha:= AllAyanamshas[Index];
end;

procedure TFormDlgCycleType.CbCycleTypeSelect(Sender: TObject);
var
  Index: Integer;
begin
  Index:= CbCycleType.ItemIndex;
  SelectedCycleType:= AllCycleTypes[Index];
end;

procedure TFormDlgCycleType.CbObserverPosSelect(Sender: TObject);
var
  Index: integer;
begin
  Index:= CbObserverPos.ItemIndex;
  SelectedObserverPos:= AllObserverPos[Index];
end;

procedure TFormDlgCycleType.SaveState;
begin
  StateMachine.CycleType := SelectedCycleType;
  StateMachine.Ayanamsha := SelectedAyanamsha;
  StateMachine.ObserverPos := SelectedObserverPos;
end;

procedure TFormDlgCycleType.DefineCycleTypeItems;
var
  i, NrOfCycleTypes: integer;
begin
  AllCycleTypes := CenCon.LookupValues.AllCycleTypes;
  CbCycleType.Items.Clear;
  NrOfCycleTypes := Length(AllCycleTypes);
  for i := 0 to NrOfCycleTypes - 1 do CbCycleType.Items.add(AllCycleTypes[i].Name);
  CbCycleType.ItemIndex := 0;
  SelectedCycleType := AllCycleTypes[0];
end;

procedure TFormDlgCycleType.DefineObserverPos;
var
  i, NrOfObserverPos: integer;
begin
  AllObserverPos := CenCon.LookupValues.AllObserverPos;
  CbObserverPos.Items.Clear;
  NrOfObserverPos := Length(AllObserverPos);
  for i := 0 to NrOfObserverPos - 1 do CbObserverPos.Items.add(AllObserverPos[i].Name);
  CbObserverPos.ItemIndex := 0;
  SelectedObserverPos := AllObserverPos[0];

end;

procedure TFormDlgCycleType.DefineAyanamshaItems;
var
  i, NrOfAyanamshas: integer;
begin
  AllAyanamshas := CenCon.LookupValues.AllAyanamshas;
  CbAyanamsha.Items.Clear;
  NrOfAyanamshas := Length(AllAyanamshas);
  for i := 0 to NrOfAyanamshas - 1 do CbAyanamsha.Items.add(AllAyanamshas[i].Name);
  CbAyanamsha.ItemIndex := 0;
  SelectedAyanamsha := AllAyanamshas[0];
end;


end.

