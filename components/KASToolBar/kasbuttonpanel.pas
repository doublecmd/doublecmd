unit KASButtonPanel;

{$mode Delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TKASButtonPanel }

  TKASButtonPanel = class(TPanel)
  private
    FSameWidth: Boolean;
    FSameHeight: Boolean;
  protected
    procedure ButtonsAutoSize;
    procedure DoAutoSize; override;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property SameWidth: Boolean read FSameWidth write FSameWidth default True;
    property SameHeight: Boolean read FSameHeight write FSameHeight default True;
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
      if FSameWidth and (AControl.Width > AMaxWidth) then AMaxWidth:= AControl.Width;
      if FSameHeight and (AControl.Height > AMaxHeight) then AMaxHeight:= AControl.Height;
    end;
  end;
  for Index:= 0 to ControlCount - 1 do
  begin
    AControl:= Controls[Index];
    if AControl is TCustomButton then
    begin
      if FSameWidth then AControl.Constraints.MinWidth:= AMaxWidth;
      if FSameHeight then AControl.Constraints.MinHeight:= AMaxHeight;
    end;
  end;
end;

procedure TKASButtonPanel.DoAutoSize;
begin
  inherited DoAutoSize;
  if csDesigning in ComponentState then Exit;
  if AutoSize and (FSameWidth or FSameHeight) then ButtonsAutosize;
end;

constructor TKASButtonPanel.Create(TheOwner: TComponent);
begin
  FSameWidth:= True;
  FSameHeight:= True;
  inherited Create(TheOwner);
end;

end.
