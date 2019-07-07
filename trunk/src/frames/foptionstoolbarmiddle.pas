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
  trbBarSize.Position   := gToolBarButtonSize div 2;
  trbIconSize.Position  := gToolBarIconSize div 2;
  cbFlatButtons.Checked := gToolBarFlat;
  cbShowCaptions.Checked := gToolBarShowCaptions;
  cbReportErrorWithCommands.Checked := gToolbarReportErrorWithCommands;

  lblBarSizeValue.Caption  := IntToStr(trbBarSize.Position*2);
  lblIconSizeValue.Caption := IntToStr(trbIconSize.Position*2);

  FCurrentButton := nil;
  CloseToolbarsBelowCurrentButton;

  ToolBar := GetTopToolbar;
  ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MiddleToolbar', False);
  LoadToolbar(ToolBar, gConfig, ToolBarNode, tocl_FlushCurrentToolbarContent);
  if ToolBar.ButtonCount > 0 then
    PressButtonDown(ToolBar.Buttons[0]);
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);

  FUpdateHotKey := False;
end;

function TfrmOptionsToolbarMiddle.Save: TOptionsEditorSaveFlags;
var
  ToolBarNode: TXmlNode;
  ToolBar: TKASToolBar;
begin
  ApplyEditControls;

//  gToolBarFlat         := cbFlatButtons.Checked;
//  gToolBarShowCaptions := cbShowCaptions.Checked;
//  gToolbarReportErrorWithCommands := cbReportErrorWithCommands.Checked;
//  gToolBarButtonSize   := trbBarSize.Position * 2;
//  gToolBarIconSize     := trbIconSize.Position * 2;

  ToolBar := GetTopToolbar;
  if Assigned(ToolBar) then
  begin
    ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MiddleToolbar', True);
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

