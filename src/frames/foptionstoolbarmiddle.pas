unit fOptionsToolbarMiddle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, fOptionsFrame,
  fOptionsToolbarBase;

type

  { TfrmOptionsToolbarMiddle }

  TfrmOptionsToolbarMiddle = class(TfrmOptionsToolbarBase)
  private

  protected
    procedure Load; override;
    class function GetNode: String; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    constructor Create(TheOwner: TComponent); override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  KASToolBar, DCXmlConfig, uGlobs, uGlobsPaths, uSpecialDir, uLng;

{ TfrmOptionsToolbarMiddle }

procedure TfrmOptionsToolbarMiddle.Load;
var
  ToolBarNode: TXmlNode;
  ToolBar: TKASToolBar;
begin
  trbBarSize.Position   := gMiddleToolBarButtonSize div 2;
  trbIconSize.Position  := gMiddleToolBarIconSize div 2;
  cbFlatButtons.Checked := gMiddleToolBarFlat;
  cbShowCaptions.Checked := gMiddleToolBarShowCaptions;
  cbReportErrorWithCommands.Checked := gMiddleToolbarReportErrorWithCommands;

  lblBarSizeValue.Caption  := IntToStr(trbBarSize.Position*2);
  lblIconSizeValue.Caption := IntToStr(trbIconSize.Position*2);

  FCurrentButton := nil;
  CloseToolbarsBelowCurrentButton;

  ToolBar := GetTopToolbar;
  ToolBarNode := gConfig.FindNode(gConfig.RootNode, GetNode, False);
  LoadToolbar(ToolBar, gConfig, ToolBarNode, tocl_FlushCurrentToolbarContent);
  if ToolBar.ButtonCount > 0 then
    PressButtonDown(ToolBar.Buttons[0]);
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);

  FUpdateHotKey := False;
end;

class function TfrmOptionsToolbarMiddle.GetNode: String;
begin
  Result:= 'Toolbars/MiddleToolbar';
end;

function TfrmOptionsToolbarMiddle.Save: TOptionsEditorSaveFlags;
var
  ToolBarNode: TXmlNode;
  ToolBar: TKASToolBar;
begin
  ApplyEditControls;

  gMiddleToolBarFlat         := cbFlatButtons.Checked;
  gMiddleToolBarShowCaptions := cbShowCaptions.Checked;
  gMiddleToolbarReportErrorWithCommands := cbReportErrorWithCommands.Checked;
  gMiddleToolBarButtonSize   := trbBarSize.Position * 2;
  gMiddleToolBarIconSize     := trbIconSize.Position * 2;

  ToolBar := GetTopToolbar;
  if Assigned(ToolBar) then
  begin
    ToolBarNode := gConfig.FindNode(gConfig.RootNode, GetNode, True);
    gConfig.ClearNode(ToolBarNode);
    Toolbar.SaveConfiguration(gConfig, ToolBarNode);
  end;

  if FUpdateHotKey then
  begin
    FUpdateHotKey := False;
    HotMan.Save(gpCfgDir + gNameSCFile);
  end;

  Result := [];
end;

constructor TfrmOptionsToolbarMiddle.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name := 'frmOptionsToolbarMiddle';
end;

class function TfrmOptionsToolbarMiddle.GetTitle: String;
begin
  Result:= rsOptionsEditorToolbarMiddle;
end;

end.

