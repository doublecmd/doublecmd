unit KASControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls;

  type

    { TKASPanel }

    TKASPanel = class(TPanel)
    private
      FOnAutoSize: TNotifyEvent;
    protected
      procedure DoAutoSize; override;
    published
      property OnAutoSize: TNotifyEvent read FOnAutoSize write FOnAutoSize;
    end;

    { TKASGroupBox }

    TKASGroupBox = class(TGroupBox)
    private
      FOnAutoSize: TNotifyEvent;
    protected
      procedure DoAutoSize; override;
    published
      property OnAutoSize: TNotifyEvent read FOnAutoSize write FOnAutoSize;
    end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KASComponents', [TKASPanel, TKASGroupBox]);
end;

procedure LabelsAutoSize(Self: TWinControl);
var
  Index: Integer;
  AControl: TControl;
begin
  with Self do
  begin
    DisableAutoSizing;
    for Index:= 0 to ControlCount - 1 do
    begin
      AControl:= Controls[Index];
      if (not (AControl is TLabel)) and Assigned(AControl.AnchorSide[akTop].Control) then
      begin
        if AControl.AnchorSide[akTop].Control is TLabel then
          AControl.AnchorSide[akTop].Control.Constraints.MinHeight:= AControl.Height;
      end;
    end;
    EnableAutoSizing;
  end;
end;

{ TKASPanel }

procedure TKASPanel.DoAutoSize;
begin
  inherited DoAutoSize;
  if not (csDesigning in ComponentState) then
  begin
    LabelsAutoSize(Self);
    if Assigned(FOnAutoSize) then FOnAutoSize(Self);
  end;
end;

{ TKASGroupBox }

procedure TKASGroupBox.DoAutoSize;
begin
  inherited DoAutoSize;
  if not (csDesigning in ComponentState) then
  begin
    LabelsAutoSize(Self);
    if Assigned(FOnAutoSize) then FOnAutoSize(Self);
  end;
end;

end.

