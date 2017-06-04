unit Interfaces;

{$mode objfpc}{$H+}

interface

uses
  InterfaceBase, qtint;

type

  { TQtWidgetSetEx }

  TQtWidgetSetEx = Class(TQtWidgetSet)
  public
    procedure AppRun(const ALoop: TApplicationMainLoop); override;
  end;

implementation

uses
  Forms;

{ TQtWidgetSetEx }

procedure TQtWidgetSetEx.AppRun(const ALoop: TApplicationMainLoop);
begin
  // Use LCL loop
  if Assigned(ALoop) then ALoop;
end;

initialization
  CreateWidgetset(TQtWidgetSetEx);

finalization
  FreeWidgetset;

end.
