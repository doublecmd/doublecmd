unit KASStatusBar;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

  { TKASStatusBar }

  TKASStatusBar = class(TStatusBar)
  public
    procedure InvalidatePanel(PanelIndex: Integer; PanelParts: TPanelParts); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KASComponents', [TKASStatusBar]);
end;

{ TKASStatusBar }

procedure TKASStatusBar.InvalidatePanel(PanelIndex: Integer; PanelParts: TPanelParts);
begin
  if (PanelIndex >= 0) and (ppText in PanelParts) then
  begin
    if Length(Panels[PanelIndex].Text) > 0 then
      Panels[PanelIndex].Width:= Canvas.TextWidth('WW' + Panels[PanelIndex].Text);
  end;
  inherited InvalidatePanel(PanelIndex, PanelParts);
end;

end.
