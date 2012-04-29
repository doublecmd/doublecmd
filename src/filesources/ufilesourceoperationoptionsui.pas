unit uFileSourceOperationOptionsUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  TFileSourceOperationOptionsUIClass = class of TFileSourceOperationOptionsUI;

  { TFileSourceOperationOptionsUI }

  TFileSourceOperationOptionsUI = class(TFrame)
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); virtual; reintroduce;
    class function GetOptionsClass: TFileSourceOperationOptionsUIClass;
    procedure SaveOptions; virtual; abstract;
    {en
       Set operation options from GUI controls.
    }
    procedure SetOperationOptions(Operation: TObject); virtual; abstract;
  end;

implementation

{ TFileSourceOperationOptionsUI }

constructor TFileSourceOperationOptionsUI.Create(AOwner: TComponent; AFileSource: IInterface);
begin
  inherited Create(AOwner);
end;

class function TFileSourceOperationOptionsUI.GetOptionsClass: TFileSourceOperationOptionsUIClass;
begin
  Result := Self;
end;

end.

