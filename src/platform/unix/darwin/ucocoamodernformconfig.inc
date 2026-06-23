{%MainUnit ucocoamodernformconfig.pas}

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  LCLType, Forms, Menus, ComCtrls, Graphics,
  fMain, fEditor, fDiffer, fViewer,
  uHotkeyManager, uEarlyConfig, uGlobs, uLng, uDCUtils, uVectorImage,
  uFileView, uBriefFileView, uColumnsFileView, uThumbFileView,
  uFileSourceProperty,
  uDarwinApplication, uDarwinPanel, uDarwinFile, uDarwinUtil, uDarwinImage,
  uDarwinFinder, uDarwinFinderModel,
  uDarwinFileView, uDarwinFileViewHistory,
  CocoaAll, CocoaConfig, CocoaToolBar, CocoaThemes, Cocoa_Extra,
  LResources, uTranslator, Translations;

type

  { TDCCocoaModernFormUtils }

  TDCCocoaModernFormUtils = class
    class procedure initConfig;
    class function isEnabled: Boolean;
    class procedure checkAndSetPrivilegeItem;
  end;

implementation

procedure translateItemConfig(
  const poFile: TPOFile;
  const identifier: String;
  var item: TCocoaConfigToolBarItemWithUI );
var
  s: String;
begin
  s:= poFile.Translate( identifier, item.tips );
  item.title:= s;
  item.tips:= s;
end;

function shortCutToMacOSName( const shortCutString: String ): String;
begin
  Result:= shortCutString.Replace( 'Cmd+', 'Command+' );
  Result:= Result.Replace( 'Alt+', 'Option+' );
end;

procedure addShortCutTipsToItemConfig(
  const formName: String;
  const command: String;
  var item: TCocoaConfigToolBarItemWithAction );
var
  form: THMForm;
  hotkey: THotKey;
begin
  form:= HotMan.Forms.Find( formName );
  if form = nil then
    Exit;

  hotkey:= form.Hotkeys.FindByCommand( command );
  if hotkey = nil then
    Exit;

  if Length(hotkey.Shortcuts) = 0 then
    Exit;

  item.tips:= item.tips + ' (' + shortCutToMacOSName(hotkey.Shortcuts[0]) + ')';
end;

{$i ucocoamodernformconfig_main.inc}
{$i ucocoamodernformconfig_editor.inc}
{$i ucocoamodernformconfig_differ.inc}
{$i ucocoamodernformconfig_viewer.inc}

const
  transparentTitleFormConfig: TCocoaConfigForm = (
    name: 'frmFindDlg;frmMultiRename;frmSyncDirsDlg;frmOptions';
    className: '';
    isMainForm: False;

    titleBar: (
      transparent: True;
      separatorStyle: NSTitlebarSeparatorStyleNone;
    );
  );

  separateTitleFormConfig: TCocoaConfigForm = (
    name: '*';
    className: '';
    isMainForm: False;

    titleBar: (
      transparent: False;
      separatorStyle: NSTitlebarSeparatorStyleLine;
    );
  );


procedure doInitConfig;
begin
  doInitMainFormConfig;
  doInitEditorFormConfig;
  doInitDifferFormConfig;
  doInitViewerFormConfig;

  CocoaConfigForms:= [
    mainFormConfig,
    editorFormConfig,
    differFormConfig,
    viewerFormConfig,
    transparentTitleFormConfig,
    separateTitleFormConfig
  ];
end;

class procedure TDCCocoaModernFormUtils.initConfig;
begin
  if NOT TDCCocoaModernFormUtils.isEnabled then
    Exit;

  fMain.onFileViewUpdated:= @onFileViewUpdated;
  toolBarMenuHandler:= TToolBarMenuHandler.Create;
  TCocoaThemeServices.addObserver( toolBarMenuHandler );
  doInitConfig;
end;

class function TDCCocoaModernFormUtils.isEnabled: Boolean;
begin
  Result:= gModernUI and TDarwinApplicationUtil.supportsModernForm;
end;

class procedure TDCCocoaModernFormUtils.checkAndSetPrivilegeItem;
var
  toolBar: NSToolBar;
  index: Integer;
begin
  if NOT TDCCocoaModernFormUtils.isEnabled then
    Exit;

  if NOT TDarwinApplicationUtil.hasFullDiskAccess then
    Exit;

  toolBar:= TCocoaToolBarUtils.getToolBar( frmMain );
  index:= TCocoaToolBarUtils.findItemIndexByIdentifier( toolBar, 'MainForm.Privilege' );
  TCocoaToolBarUtils.removeItemByIndex( toolBar, index );
end;

