unit unitcentralcontroller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unitinit;

type
  TCenCon = class
  strict private
    FLookupValues: TLookupValues;
  public
    constructor Create;
    property LookupValues: TLookupValues read FLookupValues;
  end;

implementation

var
  CenConSingleton: TCenCon = nil;

constructor TCenCon.Create;
begin
  if not (Assigned(CenConSingleton)) then
  begin
    //inherited;
    CenConSingleton := self;
    FLookUpvalues := TLookupValues.Create;
  end
  else
  begin
    self := CenConSingleton;
  end;
end;

end.
