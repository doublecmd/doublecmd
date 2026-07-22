{%MainUnit ucocoamodernformconfig.pas}

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  LCLType, Forms, Menus, ComCtrls, Graphics,
  DCBasicTypes,
  fMain, fEditor, fDiffer, fViewer,
  uHotkeyManager, uKeyboard, uEarlyConfig, uGlobs, uLng, uDCUtils, uVectorImage,
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
begin
  item.title:= poFile.Translate( identifier+'.caption', item.title );
  item.tips:= poFile.Translate( identifier+'.hint', item.tips );
end;

procedure translateItemConfigWithSameTitleTips(
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

function shortCutToMacOSString( const shortCut: TShortCut ): String;

  function shiftToString( const shift: TShiftState ): String;
  begin
    Result:= '';
    if ssAltGr in shift then
      Result:= Result + #$F0#$9F#$8C#$90#$EF#$B8#$8E;    // Fn 🌐
    if ssCtrl in shift then
      Result:= Result + '⌃';
    if ssAlt in shift then
      Result:= Result + '⌥';
    if ssShift in shift then
      Result:= Result + '⇧';
    if ssMeta in shift then
      Result:= Result + '⌘';
  end;

  function keyToString( const key: Word ): String;
  begin
    Result:= '';
    case key of
      VK_F1..VK_F24:
        Result:= 'F' + IntToStr(key-VK_F1+1);
      VK_0..VK_9, VK_A..VK_Z:
        Result:= Char(key);
      VK_SPACE:
        Result:= '␣';
      VK_TAB:
        Result:= '⇥';
      VK_RETURN:
        Result:= '↩';
      VK_ESCAPE:
        Result:= '⎋';
      VK_BACK:
        Result:= '⌫';
      VK_DELETE:
        Result:= '⌦';
      VK_UP:
        Result:= '↑';
      VK_DOWN:
        Result:= '↓';
      VK_LEFT:
        Result:= '←';
      VK_RIGHT:
        Result:= '→';
      VK_PRIOR:
        Result:= '⇞';
      VK_NEXT:
        Result:= '⇟';
      VK_HOME:
        Result:= '↖';
      VK_END:
        Result:= '↘';
      else
        Result:= UpperCase( Char(Key) );
    end;
  end;

var
  key: Word;
  shift: TShiftState;
begin
  ShortCutToKey( shortCut, key, shift );
  Result:= shiftToString(shift) + keyToString(key);
end;

procedure addShortCutTipsToItemConfig(
  const shortCut: TShortCut;
  var item: TCocoaConfigToolBarItemWithUI );
begin
  item.tips:= item.tips + '      ' + shortCutToMacOSString(shortCut);
end;

procedure addShortCutTipsToItemConfig(
  const formName: String;
  const command: String;
  const params: TDynamicStringArray;
  var item: TCocoaConfigToolBarItemWithUI );
var
  form: THMForm;
  hotkey: THotKey;
begin
  form:= HotMan.Forms.Find( formName );
  if form = nil then
    Exit;

  hotkey:= form.Hotkeys.FindByCommand( command, params );
  if hotkey = nil then
    Exit;

  if Length(hotkey.Shortcuts) = 0 then
    Exit;

  addShortCutTipsToItemConfig( TextToShortCutEx(hotkey.Shortcuts[0]), item );
end;

procedure addShortCutTipsToItemConfig(
  const formName: String;
  const command: String;
  var item: TCocoaConfigToolBarItemWithUI ); inline;
begin
  addShortCutTipsToItemConfig( formName, command, nil, item );
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

