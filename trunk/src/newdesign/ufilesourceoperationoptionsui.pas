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
    class function GetOptionsClass: TFileSourceOperationOptionsUIClass;
    procedure SaveOptions; virtual; abstract;
    {en
       Set operation options from GUI controls.
    }
    procedure SetOperationOptions(Operation: TObject); virtual; abstract;
  end;

implementation

{ TFileSourceOperationOptionsUI }

class function TFileSourceOperationOptionsUI.GetOptionsClass: TFileSourceOperationOptionsUIClass;
begin
  Result := Self;
end;

end.

