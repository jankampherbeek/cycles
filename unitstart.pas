unit unitstart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAIntervalSources;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    CbCalendar: TComboBox;
    LblCalendar: TLabel;
    LblEdEndDate: TLabeledEdit;
    LblEdInterval: TLabeledEdit;
    LblEdStartDate: TLabeledEdit;
    LblTitle: TLabel;
    procedure BtnOkClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation


{$R *.lfm}

uses
  unitastron, unitdomainxchg;

{ TForm1 }

procedure TForm1.BtnOkClick(Sender: TObject);
var
  calculatedPosition: TFullPosForCoordinate;
  ephemeris: TEphemeris;
begin
  //ephemeris:=TEphemeris.Create;
  //calculatedPosition:=ephemeris.CalcCelPoint(2000000, 1, 1);
  //LblTitle.Caption:= FloatToStr(calculatedPosition.mainPos);


end;

end.

