{%MainUnit ucocoamodernformconfig.pas}

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  LCLType, Forms, Menus,
  fMain, fEditor, fDiffer,
  uHotkeyManager, uGlobs, uLng, uDCUtils,
  uFileView, uBriefFileView, uColumnsFileView, uThumbFileView,
  uFileSystemFileSource,
  uDarwinUtil, uDarwinApplication, uDarwinFile,
  uDarwinFinder, uDarwinFinderModel, uDarwinFileView, uDarwinPanel,
  CocoaAll, CocoaConfig, CocoaToolBar, Cocoa_Extra,
  LResources, uTranslator, Translations;

procedure initCocoaModernFormConfig;

implementation

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

  CocoaConfigForms:= [
    mainFormConfig,
    editorFormConfig,
    differFormConfig,
    transparentTitleFormConfig,
    separateTitleFormConfig
  ];
end;

procedure initCocoaModernFormConfig;
begin
  if NSAppKitVersionNumber < NSAppKitVersionNumber11_0 then
    Exit;

  fMain.onFileViewUpdated:= @onFileViewUpdated;
  toolBarMenuHandler:= TToolBarMenuHandler.Create;
  doInitConfig;
end;

end.

