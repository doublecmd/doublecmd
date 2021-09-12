unit KASButtonPanel;

{$mode Delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TKASButtonPanel }

  TKASButtonPanel = class(TPanel)
  protected
    procedure ButtonsAutoSize;
    procedure DoAutoSize; override;
  end;

procedure Register;

implementation

uses
  StdCtrls;

procedure Register;
begin
  RegisterComponents('KASComponents', [TKASButtonPanel]);
end;

{ TKASButtonPanel }

procedure TKASButtonPanel.ButtonsAutoSize;
var
  Index: Integer;
  AControl: TControl;
  AMaxWidth, AMaxHeight: Integer;
begin
  AMaxWidth:= 0;
  AMaxHeight:= 0;
  for Index:= 0 to ControlCount - 1 do
  begin
    AControl:= Controls[Index];
    if AControl is TCustomButton then
    begin
      if AControl.Width > AMaxWidth then AMaxWidth:= AControl.Width;
      if AControl.Height > AMaxHeight then AMaxHeight:= AControl.Height;
    end;
  end;
  for Index:= 0 to ControlCount - 1 do
  begin
    AControl:= Controls[Index];
    if AControl is TCustomButton then
    begin
      AControl.Constraints.MinWidth:= AMaxWidth;
      AControl.Constraints.MinHeight:= AMaxHeight;
    end;
  end;
end;

procedure TKASButtonPanel.DoAutoSize;
begin
  inherited DoAutoSize;
  if AutoSize then ButtonsAutosize;
end;

end.
