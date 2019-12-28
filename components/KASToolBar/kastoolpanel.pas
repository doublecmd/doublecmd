unit KASToolPanel;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Toolwin;

type
  TKASToolPanel = class(TToolWindow)
  published
    property Align default alNone;
    property AutoSize;
    property EdgeBorders default [ebTop];
    property EdgeInner;
    property EdgeOuter;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KASComponents',[TKASToolPanel]);
end;

end.
