unit KASToolPanel;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Toolwin;

type

  { TKASToolPanel }

  TKASToolPanel = class(TToolWindow)
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Align default alNone;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    property EdgeBorders default [ebTop];
    property EdgeInner;
    property EdgeOuter;
    property OnResize;
    property TabOrder;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KASComponents',[TKASToolPanel]);
end;

{ TKASToolPanel }

constructor TKASToolPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EdgeBorders:= [ebTop];
end;

end.
