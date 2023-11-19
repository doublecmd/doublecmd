unit fOptionsColors;

{$mode ObjFPC}{$H+}

{$IF DEFINED(darwin)}
  {$DEFINE DARKWIN}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Dialogs, StdCtrls,
  DividerBevel, fOptionsFrame, KASComboBox, LMessages;

type

  { TfrmOptionsColors }

  TfrmOptionsColors = class(TOptionsEditor)
    cbAdded: TKASColorBoxButton;
    cbBookBackground: TKASColorBoxButton;
    cbBookText: TKASColorBoxButton;
    cbbUseGradientInd: TCheckBox;
    cbDeleted: TKASColorBoxButton;
    cbError: TKASColorBoxButton;
    cbImageBackground1: TKASColorBoxButton;
    cbImageBackground2: TKASColorBoxButton;
    cbIndBackColor: TKASColorBoxButton;
    cbIndColor: TKASColorBoxButton;
    cbIndThresholdColor: TKASColorBoxButton;
    cbInformation: TKASColorBoxButton;
    cbLeft: TKASColorBoxButton;
    cbModifiedBinary: TKASColorBoxButton;
    cbRight: TKASColorBoxButton;
    cbSuccess: TKASColorBoxButton;
    cbUnknown: TKASColorBoxButton;
    cmbGroup: TComboBox;
    cbModified: TKASColorBoxButton;
    dbBookMode: TDividerBevel;
    dbImageMode: TDividerBevel;
    DividerBevel11: TDividerBevel;
    dbTextMode: TDividerBevel;
    DividerBevel4: TDividerBevel;
    dbBinaryMode: TDividerBevel;
    DividerBevel6: TDividerBevel;
    DividerBevel9: TDividerBevel;
    lblCategory: TLabel;
    lblAdded: TLabel;
    lblBookBackground: TLabel;
    lblBookText: TLabel;
    lblDeleted: TLabel;
    lblError: TLabel;
    lblImageBackground1: TLabel;
    lblImageBackground2: TLabel;
    lblIndBackColor: TLabel;
    lblIndColor: TLabel;
    lblIndThresholdColor: TLabel;
    lblInformation: TLabel;
    lblLeft: TLabel;
    lblModified: TLabel;
    lblModifiedBinary: TLabel;
    lblRight: TLabel;
    lblSuccess: TLabel;
    lblUnknown: TLabel;
    nbColors: TNotebook;
    pgDriveFreeInd: TPage;
    pbxFakeDrive: TPaintBox;
    pgDarkMode: TPage;
    pgDiffer: TPage;
    pgLog: TPage;
    pgSyncDirs: TPage;
    pgViewer: TPage;
    rgDarkMode: TRadioGroup;
    procedure cbbUseGradientIndChange(Sender: TObject);
    procedure cbIndColorChange(Sender: TObject);
    procedure cmbGroupChange(Sender: TObject);
    procedure pbxFakeDriveClick(Sender: TObject);
    procedure pbxFakeDrivePaint(Sender: TObject);
{$IF DEFINED(DARKWIN)}
  private
    FAppMode: Integer;
{$ENDIF}
  protected
    procedure Init; override;
    procedure Load; override;
    procedure DoAutoSize; override;
    function Save: TOptionsEditorSaveFlags; override;
    procedure CMThemeChanged(var Message: TLMessage); message CM_THEMECHANGED;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uLng, uDCUtils, fMain
{$IF DEFINED(DARKWIN)}
  , DCStrUtils, uEarlyConfig
  {$IF not DEFINED(darwin)}
  , uDarkStyle
  {$ELSE}
  , uMyDarwin
  {$ENDIF}
{$ENDIF}
  ;

{ TfrmOptionsColors }

procedure TfrmOptionsColors.cmbGroupChange(Sender: TObject);
begin
  nbColors.PageIndex:= cmbGroup.ItemIndex;
end;

procedure TfrmOptionsColors.pbxFakeDriveClick(Sender: TObject);
begin
  pbxFakeDrive.Tag:= (pbxFakeDrive.ScreenToClient(Mouse.CursorPos).X * 100) div pbxFakeDrive.Width;
  pbxFakeDrive.Hint:= pbxFakeDrive.Tag.ToString + '%';
  pbxFakeDrive.Repaint;
end;

procedure TfrmOptionsColors.cbbUseGradientIndChange(Sender: TObject);
var vNoGradient: boolean;
begin
  vNoGradient := not (cbbUseGradientInd.Checked);

  lblIndThresholdColor.Enabled := vNoGradient;
  lblIndColor.Enabled := vNoGradient;
  lblIndBackColor.Enabled := vNoGradient;

  cbIndThresholdColor.Enabled := vNoGradient;
  cbIndColor.Enabled := vNoGradient;
  cbIndBackColor.Enabled := vNoGradient;

  pbxFakeDrive.Repaint;
end;

procedure TfrmOptionsColors.cbIndColorChange(Sender: TObject);
begin
  pbxFakeDrive.Repaint;
end;

procedure TfrmOptionsColors.pbxFakeDrivePaint(Sender: TObject);
begin
  frmMain.PaintDriveFreeBar(pbxFakeDrive, cbbUseGradientInd.Checked,
    cbIndColor.Selected, cbIndThresholdColor.Selected, cbIndBackColor.Selected);
end;

procedure TfrmOptionsColors.Init;
begin
  cmbGroup.Items.Add(rsToolViewer);
  cmbGroup.Items.Add(rsToolDiffer);
  cmbGroup.Items.Add(rsOptionsEditorLog);
  cmbGroup.Items.Add(rsHotkeyCategorySyncDirs);
  cmbGroup.Items.Add(rsDriveFreeSpaceIndicator);
  cmbGroup.ItemIndex:= 0;
{$IF DEFINED(DARKWIN)}
  FAppMode:= gAppMode;
  ParseLineToList(rsDarkModeOptions, rgDarkMode.Items);
  cmbGroup.ItemIndex:= cmbGroup.Items.Add(rsDarkMode);
  nbColors.PageIndex:= cmbGroup.ItemIndex;
{$ENDIF}
end;

procedure TfrmOptionsColors.Load;
begin
{$IF DEFINED(DARKWIN)}
  case FAppMode of
    1: rgDarkMode.ItemIndex:= 0;
    2: rgDarkMode.ItemIndex:= 1;
    3: rgDarkMode.ItemIndex:= 2;
  end;
{$ENDIF}
  with gColors.Viewer^ do
  begin
    cbBookText.Selected:= BookFontColor;
    cbBookBackground.Selected:= BookBackgroundColor;
    cbImageBackground1.Selected:= ImageBackColor1;
    cbImageBackground2.Selected:= ImageBackColor2;
  end;
  with gColors.Differ^ do
  begin
    cbAdded.Selected:= AddedColor;
    cbDeleted.Selected:= DeletedColor;
    cbModified.Selected:= ModifiedColor;
    cbModifiedBinary.Selected:= ModifiedBinaryColor;
  end;
  with gColors.Log^ do
  begin
    cbInformation.Selected:= InfoColor;
    cbSuccess.Selected:= SuccessColor;
    cbError.Selected:= ErrorColor;
  end;
  with gColors.SyncDirs^ do
  begin
    cbLeft.Selected:= LeftColor;
    cbRight.Selected:= RightColor;
    cbUnknown.Selected:= UnknownColor;
  end;
  with gColors.FreeSpaceInd^ do
  begin
    cbIndColor.Selected:= ForeColor;
    cbIndBackColor.Selected:= BackColor;
    cbIndThresholdColor.Selected:= ThresholdForeColor;
  end;
  cbbUseGradientInd.Checked:= gIndUseGradient;
  pbxFakeDrive.Hint:= pbxFakeDrive.Tag.ToString + '%';
end;

procedure TfrmOptionsColors.DoAutoSize;
var
  I, J: Integer;
  AControl: TControl;
  AMaxWidth: Integer = 0;
begin
  inherited DoAutoSize;
  if csDesigning in ComponentState then Exit;

  pbxFakeDrive.Constraints.MaxHeight:= cbbUseGradientInd.Height;

  for I:= 0 to nbColors.PageCount - 1 do
  begin
    with nbColors.Page[I] do
    begin
      for J := 0 to ControlCount - 1 do
      begin
        AControl:= Controls[J];
        if AControl is TLabel then
        begin
          if (AControl.Width > AMaxWidth) then AMaxWidth:= AControl.Width;
        end;
      end;
    end;
  end;
  for I:= 0 to nbColors.PageCount - 1 do
  begin
    with nbColors.Page[I] do
    begin
      for J := 0 to ControlCount - 1 do
      begin
        AControl:= Controls[J];
        if AControl is TLabel then
        begin
          AControl.Constraints.MinWidth:= AMaxWidth;
        end;
      end;
    end;
  end;
end;

function TfrmOptionsColors.Save: TOptionsEditorSaveFlags;
begin
  Result:= [];
{$IF DEFINED(DARKWIN)}
  case rgDarkMode.ItemIndex of
    0: gAppMode:= 1;
    1: gAppMode:= 2;
    2: gAppMode:= 3;
  end;
  if gAppMode <> FAppMode then
  try
    FAppMode:= gAppMode;
    {$IF not DEFINED(darwin)}
    Result:= [oesfNeedsRestart];
    {$ELSE}
    setMacOSAppearance( gAppMode );
    {$ENDIF}
    SaveEarlyConfig;
  except
    on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
{$ENDIF}
  with gColors.Viewer^ do
  begin
    BookFontColor:= cbBookText.Selected;
    BookBackgroundColor:= cbBookBackground.Selected;
    ImageBackColor1:= cbImageBackground1.Selected;
    ImageBackColor2:= cbImageBackground2.Selected;
  end;
  with gColors.Differ^ do
  begin
    AddedColor:= cbAdded.Selected;
    DeletedColor:= cbDeleted.Selected;
    ModifiedColor:= cbModified.Selected;
    ModifiedBinaryColor:= cbModifiedBinary.Selected;
  end;
  with gColors.Log^ do
  begin
    InfoColor:= cbInformation.Selected;
    SuccessColor:= cbSuccess.Selected;
    ErrorColor:= cbError.Selected;
  end;
  with gColors.SyncDirs^ do
  begin
    LeftColor:= cbLeft.Selected;
    RightColor:= cbRight.Selected;
    UnknownColor:= cbUnknown.Selected;
  end;
  gIndUseGradient:= cbbUseGradientInd.Checked;
  with gColors.FreeSpaceInd^ do
  begin
    ForeColor := cbIndColor.Selected;
    BackColor := cbIndBackColor.Selected;
    ThresholdForeColor := cbIndThresholdColor.Selected;
  end;
end;

procedure TfrmOptionsColors.CMThemeChanged(var Message: TLMessage);
begin
  LoadSettings;
end;

class function TfrmOptionsColors.GetIconIndex: Integer;
begin
  Result:= 4;
end;

class function TfrmOptionsColors.GetTitle: String;
begin
  Result:= rsOptionsEditorColors;
end;

end.

