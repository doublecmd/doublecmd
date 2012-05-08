{
   Double Commander
   ------------------------------------------------------------
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   Globals variables and some consts

   contributors:

   Copyright (C) 2006-2009 Alexander Koblov (Alexx2000@mail.ru)
   
   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   
   Copyright (C) 2008  Vitaly Zotov (vitalyzotov@mail.ru)
   
}

unit uGlobs;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms, uExts, uColorExt, Graphics, DCClassesUtf8, uMultiArc,
  uColumns, uHotkeyManager, uSearchTemplate, uFileSourceOperationOptions,
  uWFXModule, uWCXModule, uWDXModule, uwlxmodule, udsxmodule, DCXmlConfig,
  uInfoToolTip, fQuickSearch, uTypes, uClassesEx;

type
  { Log options }
  TLogOptions = set of (log_cp_mv_ln, log_delete, log_dir_op, log_arc_op,
                        log_vfs_op, log_success, log_errors, log_info);
  { Watch dirs options }
  TWatchOptions = set of (watch_file_name_change, watch_attributes_change,
                          watch_only_foreground, watch_exclude_dirs);
  { Tabs options }
  TTabsOptions = set of (tb_always_visible, tb_multiple_lines, tb_same_width,
                         tb_text_length_limit, tb_confirm_close_all,
                         tb_close_on_dbl_click, tb_open_new_in_foreground,
                         tb_open_new_near_current, tb_show_asterisk_for_locked,
                         tb_activate_panel_on_click, tb_show_close_button);

  TTabsPosition = (tbpos_top, tbpos_bottom);
  { Show icons mode }
  TShowIconsMode = (sim_none, sim_standart, sim_all, sim_all_and_exe);
  { Show tooltip mode }
  TShowToolTipMode = set of (stm_show_for_all, stm_only_large_name);
  TScrollMode = (smLineByLineCursor, smLineByLine, smPageByPage);
  { Sorting directories mode }
  TSortFolderMode = (sfmSortNameShowFirst, sfmSortLikeFileShowFirst, sfmSortLikeFile);
  { Where to insert new files in the filelist }
  TNewFilesPosition = (nfpTop, nfpTopAfterDirectories, nfpSortedPosition, nfpBottom);
  { Where to move updated files in the filelist }
  TUpdatedFilesPosition = (ufpSameAsNewFiles, ufpSortedPosition, ufpNoChange);
  { How initially progress is shown for file operations }
  TFileOperationsProgressKind = (fopkSeparateWindow, fopkSeparateWindowMinimized, fopkOperationsPanel);

  TExternalTool = (etViewer, etEditor, etDiffer);
  TExternalToolOptions = record
    Enabled: Boolean;
    Path: String;
    Parameters: String;
    RunInTerminal: Boolean;
    KeepTerminalOpen: Boolean;
  end;
  TExternalToolsOptions = array[TExternalTool] of TExternalToolOptions;

  TDCFont = (dcfMain, dcfViewer, dcfEditor, dcfLog, dcfViewerBook);
  TDCFontOptions = record
    Name: String;
    Size: Integer;
    Style: TFontStyles;
  end;
  TDCFontsOptions = array[TDCFont] of TDCFontOptions;

  // fswmPreventDelete - prevents deleting watched directories
  // fswmAllowDelete   - does not prevent deleting watched directories
  // fswmWholeDrive    - watch whole drives instead of single directories to omit problems with deleting watched directories
  TWatcherMode = (fswmPreventDelete, fswmAllowDelete, fswmWholeDrive);

  TDrivesListButtonOption = (dlbShowLabel, dlbShowFileSystem, dlbShowFreeSpace);
  TDrivesListButtonOptions = set of TDrivesListButtonOption;

  TKeyTypingModifier = (ktmNone, ktmAlt, ktmCtrlAlt);
  TKeyTypingAction = (ktaNone, ktaCommandLine, ktaQuickSearch, ktaQuickFilter);

const
  { Default hotkey list version number }
  hkVersion     = 13;

  // Previously existing names if reused must check for ConfigVersion >= X.
  // History:
  // 2   - removed Layout/SmallIcons
  //       renamed Layout/SmallIconSize to Layout/IconSize
  // 3   - Layout/DriveMenuButton -> Layout/DrivesListButton and added subnodes:
  //         ShowLabel, ShowFileSystem, ShowFreeSpace
  // 4   - changed QuickSearch/Enabled, QuickSearch/Mode and same for QuickFilter
  //       to Keyboard/Typing.
  // 5   - changed Behaviours/SortCaseSensitive to FilesViews/Sorting/CaseSensitivity
  //       changed Behaviours/SortNatural to FilesViews/Sorting/NaturalSorting
  ConfigVersion = 5;

  TKeyTypingModifierToShift: array[TKeyTypingModifier] of TShiftState =
    ([], [ssAlt], [ssCtrl, ssAlt]);

var
  { For localization }
  gPOFileName,
  gHelpLang: String;

  { DSX plugins }
  gDSXPlugins: TDSXModuleList;
  { WCX plugins }
  gWCXPlugins: TWCXModuleList;
  { WDX plugins }
  gWDXPlugins:TWDXModuleList;
  { WFX plugins }
  gWFXPlugins: TWFXModuleList;
  { WLX plugins }
  gWLXPlugins: TWLXModuleList;
  { MultiArc addons }
  gMultiArcList: TMultiArcList;

  { Columns Set }
  ColSet:TPanelColumnsList;
  
  { Layout page }
  gMainMenu,
  gButtonBar,
  gToolBarFlat,
  gDriveBar1,
  gDriveBar2,
  gDriveBarFlat,
  gDrivesListButton,
  gDirectoryTabs,
  gCurDir,
  gTabHeader,
  gStatusBar,
  gCmdLine,
  gLogWindow,
  gTermWindow,
  gKeyButtons,
  gInterfaceFlat,
  gDriveInd,
  gDriveFreeSpace,
  gProgInMenuBar,
  gPanelOfOp,
  gHorizontalFilePanels,
  gShortFormatDriveInfo: Boolean;
  gDrivesListButtonOptions: TDrivesListButtonOptions;

  { Toolbar }
  gToolBarButtonSize,
  gToolBarIconSize: Integer;

  gRepeatPassword:Boolean;  // repeat password when packing files
  gDirHistoryCount:Integer; // how many history we remember
  gShowSystemFiles:Boolean;
  gRunInTerm: String;
  gRunTerm: String;
  gSortCaseSensitivity: TCaseSensitivity;
  gSortNatural: Boolean;
  gSortFolderMode: TSortFolderMode;
  gNewFilesPosition: TNewFilesPosition;
  gUpdatedFilesPosition: TUpdatedFilesPosition;
  gLynxLike:Boolean;
  gFirstTextSearch: Boolean;

  gMouseSelectionEnabled: Boolean;
  gMouseSelectionButton: Integer;

  gAutoFillColumns: Boolean;
  gAutoSizeColumn: Integer;
  
  glsHotDir:TStringListEx;
  glsDirHistory:TStringListEx;
  glsMaskHistory : TStringListEx;
  glsSearchHistory : TStringListEx;
  glsReplaceHistory : TStringListEx;
  glsIgnoreList : TStringListEx;
  gOnlyOneAppInstance,
  gCutTextToColWidth : Boolean;
  gSpaceMovesDown: Boolean;
  gScrollMode: TScrollMode;
  gWheelScrollLines: Integer;
  gAlwaysShowTrayIcon: Boolean;
  gMinimizeToTray: Boolean;
  gShortFileSizeFormat:Boolean;
  gDateTimeFormat : String;
  gDriveBlackList: String;
  gDriveBlackListUnmounted: Boolean; // Automatically black list unmounted devices
  gListFilesInThread: Boolean;
  gLoadIconsSeparately: Boolean;
  gDelayLoadingTabs: Boolean;
  gHighlightUpdatedFiles: Boolean;
  gLastUsedPacker: String;

  { Tools page }
  gExternalTools: TExternalToolsOptions;

  gLuaLib:String;
  gExts:TExts;
  gColorExt:TColorExt;
  gFileInfoToolTip: TFileInfoToolTip;

  { Fonts page }
  gFonts: TDCFontsOptions;

  { File panels color page }
  
  gBackColor, //Background color
  gBackColor2, //Background color 2
  gForeColor,  //text color
  gMarkColor,  // Mark color
  gCursorColor, //Cursor color
  gCursorText,  //text color under cursor
  gIndForeColor,    // foreColor of use space on drive label
  gIndBackColor: TColor; // backColor of free space on drive label

  gUseInvertedSelection: Boolean;
  gInactivePanelBrightness: Integer; // 0 .. 100 (black .. full color)
  gUseFrameCursor: Boolean;
  gIndUseGradient : Boolean;             // use gradient on drive label

  gShowIcons: TShowIconsMode;
  gShowIconsNew: TShowIconsMode;
  gIconOverlays : Boolean;
  gIconsSize,
  gIconsSizeNew : Integer;
  gCustomDriveIcons : Boolean; // for use custom drive icons under windows

  { Keys page }
  gKeyTyping: array[TKeyTypingModifier] of TKeyTypingAction;

  { File operations page }

  gCopyBlockSize : Integer;
  gUseMmapInSearch : Boolean;
  gPartialNameSearch: Boolean;
  gSkipFileOpError: Boolean;
  gDropReadOnlyFlag : Boolean;
  gWipePassNumber: Integer;
  gProcessComments: Boolean;
  gShowCopyTabSelectPanel:boolean;
  gUseTrash : Boolean; // if using delete to trash by default
  gRenameSelOnlyName:boolean;
  gShowDialogOnDragDrop: Boolean;
  gOverwriteFolder: Boolean;
  gFileOperationsProgressKind: TFileOperationsProgressKind;

  { Folder tabs page }

  gDirTabOptions : TTabsOptions;
  gDirTabLimit : Integer;
  gDirTabPosition : TTabsPosition;

  { Log page }
  gLogFile : Boolean;
  gLogFileName : String;
  gLogOptions : TLogOptions;

  { Configuration page }
  gUseConfigInProgramDir,
  gUseConfigInProgramDirNew,
  gSaveConfiguration,
  gSaveSearchReplaceHistory,
  gSaveDirHistory,
  gSaveCmdLineHistory,
  gSaveFileMaskHistory : Boolean;
  
  { Quick Search page }
  gQuickSearchOptions: TQuickSearchOptions;
  gQuickFilterAutoHide: Boolean;

  { Misc page }
  gGridVertLine,
  gGridHorzLine,
  gShowWarningMessages,
  gDirBrackets: Boolean;
  gShowToolTipMode: TShowToolTipMode;
  { Auto refresh page }
  gWatchDirs: TWatchOptions;
  gWatchDirsExclude: String;
  gWatcherMode: TWatcherMode;

  { Ignore list page }
  gIgnoreListFileEnabled: Boolean;
  gIgnoreListFile: UTF8String;

  gSearchTemplateList: TSearchTemplateList;

  {HotKey Manager}
  HotMan:THotKeyManager;
  gNameSCFile: string;
  
  {Copy/Move operation options}
  gOperationOptionSymLinks: TFileSourceOperationOptionSymLink;
  gOperationOptionCorrectLinks: Boolean;
  gOperationOptionFileExists: TFileSourceOperationOptionFileExists;
  gOperationOptionDirectoryExists: TFileSourceOperationOptionDirectoryExists;
  gOperationOptionSetPropertyError: TFileSourceOperationOptionSetPropertyError;
  gOperationOptionCheckFreeSpace: Boolean;
  gOperationOptionCopyAttributes: Boolean;
  gOperationOptionCopyTime: Boolean;
  gOperationOptionCopyOwnership: Boolean;

  {Error file}
  gErrorFile: String;

  {Viewer}
  gPreviewVisible,
  gImageStretch,
  gSaveThumb: Boolean;
  gCopyMovePath1,
  gCopyMovePath2,
  gCopyMovePath3,
  gCopyMovePath4,
  gCopyMovePath5,
  gImagePaintMode: String;
  gImagePaintWidth,
  gColCount,
  gViewerMode: Integer;
  gImagePaintColor,
  gBookBackgroundColor,
  gBookFontColor: TColor;
  gTextPosition:PtrInt;

  gUseShellForFileOperations: Boolean;

function InitGlobs: Boolean;
function LoadGlobs: Boolean;
procedure SaveGlobs;
procedure LoadIniConfig;
procedure SaveIniConfig;
procedure LoadXmlConfig;
procedure SaveXmlConfig;
procedure ConvertIniToXml;

procedure LoadDefaultHotkeyBindings;

function InitPropStorage(Owner: TComponent): TIniPropStorageEx;

procedure FontToFontOptions(Font: TFont; out Options: TDCFontOptions);
procedure FontOptionsToFont(Options: TDCFontOptions; Font: TFont);

function GetKeyTypingAction(ShiftStateEx: TShiftState): TKeyTypingAction;
function IsFileSystemWatcher: Boolean;

const
  cMaxStringItems=50;
  
var
  gIni: TIniFileEx = nil;
  gConfig: TXmlConfig = nil;

implementation

uses
   LCLProc, SysUtils, uGlobsPaths, uLng, uShowMsg, uFileProcs, uOSUtils,
   uDCUtils, fMultiRename, uFile, uDCVersion, uDebug, uFileFunctions,
   uDefaultPlugins, Lua, uKeyboard, DCOSUtils, DCStrUtils
   {$IF DEFINED(MSWINDOWS)}
    , win32proc
   {$ENDIF}
   ;

const
  TKeyTypingModifierToNodeName: array[TKeyTypingModifier] of String =
    ('NoModifier', 'Alt', 'CtrlAlt');

var
  // Double Commander version
  // loaded from configuration file
  gPreviousVersion: UTF8String = '';

procedure LoadDefaultHotkeyBindings;
var
  HMForm: THMForm;
  HMControl: THMControl;
begin
  // Note: Increase hkVersion if you change default hotkeys list

  // Shortcuts that can conflict with default OS shortcuts for some controls
  // should be put only to Files Panel.
  // Shortcuts Ctrl+Alt+<letter> should not be added as the combinations may be
  // used to enter international characters on Windows (where Ctrl+Alt = AltGr).
  // For a list of such possible shortcuts see THotKeyManager.IsShortcutConflictingWithOS.
  // If adding multiple shortcuts for the same command use:
  //  AddIfNotExists([Shortcut1, Param1, Shortcut2, Param2, ...], Command);

  HMForm := HotMan.Forms.FindOrCreate('Main');
  with HMForm.Hotkeys do
    begin
      AddIfNotExists(['F1'],[],'cm_About');
      AddIfNotExists(['F2'],[],'cm_RenameOnly');
      AddIfNotExists(['F3'],[],'cm_View');
      AddIfNotExists(['F4'],[],'cm_Edit');
      AddIfNotExists(['F5'],[],'cm_Copy');
      AddIfNotExists(['F6'],[],'cm_Rename');
      AddIfNotExists(['F7'],[],'cm_MakeDir');
      AddIfNotExists(['F8','','',
                      'Shift+F8','','trashcan=reversesetting',''], 'cm_Delete');
      AddIfNotExists(['F9'],[],'cm_RunTerm');
      AddIfNotExists(['Ctrl+7'],[],'cm_ShowCmdLineHistory');
      AddIfNotExists(['Ctrl+D'],[],'cm_DirHotList');
      AddIfNotExists(['Ctrl+F'],[],'cm_QuickFilter');
      AddIfNotExists(['Ctrl+H'],[],'cm_DirHistory');
      AddIfNotExists(['Ctrl+L'],[],'cm_CalculateSpace');
      AddIfNotExists(['Ctrl+M'],[],'cm_MultiRename');
      AddIfNotExists(['Ctrl+P'],[],'cm_AddPathToCmdLine');
      AddIfNotExists(['Ctrl+Q'],[],'cm_QuickView');
      AddIfNotExists(['Ctrl+S'],[],'cm_QuickSearch');
      AddIfNotExists(['Ctrl+R'],[],'cm_Refresh');
      AddIfNotExists(['Ctrl+T'],[],'cm_NewTab');
      AddIfNotExists(['Ctrl+U'],[],'cm_Exchange');
      AddIfNotExists(['Ctrl+W'],[],'cm_RemoveTab');
      AddIfNotExists(['Ctrl+Z'],[],'cm_EditComment');
      AddIfNotExists(['Ctrl+F1'],[],'cm_BriefView');
      AddIfNotExists(['Ctrl+F2'],[],'cm_ColumnsView');
      AddIfNotExists(['Ctrl+F3'],[],'cm_SortByName');
      AddIfNotExists(['Ctrl+F4'],[],'cm_SortByExt');
      AddIfNotExists(['Ctrl+F5'],[],'cm_SortByDate');
      AddIfNotExists(['Ctrl+F6'],[],'cm_SortBySize');
      AddIfNotExists(['Ctrl+Down'],[],'cm_ShowCmdLineHistory');
      AddIfNotExists(['Ctrl+Enter'],[],'cm_AddFilenameToCmdLine');
      AddIfNotExists(['Ctrl+PgDn'],[],'cm_OpenArchive');
      AddIfNotExists(['Ctrl+PgUp'],[],'cm_ChangeDirToParent');
      AddIfNotExists(['Ctrl+Alt+Enter'],[],'cm_ShellExecute');
      AddIfNotExists(['Ctrl+Shift+C'],[],'cm_CopyFullNamesToClip');
      AddIfNotExists(['Ctrl+Shift+H'],[],'cm_HorizontalFilePanels');
      AddIfNotExists(['Ctrl+Shift+X'],[],'cm_CopyNamesToClip');
      AddIfNotExists(['Ctrl+Shift+Enter'],[],'cm_AddPathAndFilenameToCmdLine');
      AddIfNotExists(['Ctrl+Shift+Tab'],[],'cm_PrevTab');
      AddIfNotExists(['Ctrl+Tab'],[],'cm_NextTab');
      AddIfNotExists(['Ctrl+Up'],[],'cm_OpenDirInNewTab');
      AddIfNotExists(['Ctrl+\'],[],'cm_ChangeDirToRoot');
      AddIfNotExists(['Ctrl+.'],[],'cm_ShowSysFiles');
      AddIfNotExists(['Shift+F2'],[],'cm_FocusCmdLine');
      AddIfNotExists(['Shift+F4'],[],'cm_EditNew');
      AddIfNotExists(['Shift+F5'],[],'cm_CopySamePanel');
      AddIfNotExists(['Shift+F6'],[],'cm_RenameOnly');
      AddIfNotExists(['Shift+F10'],[],'cm_ContextMenu');
      AddIfNotExists(['Alt+V'],[],'cm_OperationsViewer');
      AddIfNotExists(['Alt+X'],[],'cm_Exit');
      AddIfNotExists(['Alt+Z'],[],'cm_TargetEqualSource');
      AddIfNotExists(['Alt+F1'],[],'cm_LeftOpenDrives');
      AddIfNotExists(['Alt+F2'],[],'cm_RightOpenDrives');
      AddIfNotExists(['Alt+F5'],[],'cm_PackFiles');
      AddIfNotExists(['Alt+F7'],[],'cm_Search');
      AddIfNotExists(['Alt+F9'],[],'cm_ExtractFiles');
      AddIfNotExists(['Alt+Del'],[],'cm_Wipe');
      AddIfNotExists(['Alt+Down'],[],'cm_DirHistory');
      AddIfNotExists(['Alt+Enter'],[],'cm_FileProperties');
      AddIfNotExists(['Alt+Left'],[],'cm_ViewHistoryPrev');
      AddIfNotExists(['Alt+Right'],[],'cm_ViewHistoryNext');
      AddIfNotExists(['Alt+Shift+Enter'],[],'cm_CountDirContent');
      AddIfNotExists(['Alt+Shift+F9'],[],'cm_TestArchive');
    end;

  HMControl := HMForm.Controls.FindOrCreate('Files Panel');
  with HMControl.Hotkeys do
    begin
      AddIfNotExists(['Del','','',
                      'Shift+Del','','trashcan=reversesetting',''], 'cm_Delete');
      AddIfNotExists(['Ctrl+A','','',
                      'Ctrl+Num+','',''],'cm_MarkMarkAll', ['Ctrl+A'], []);
      AddIfNotExists(['Num+'],[],'cm_MarkPlus');
      AddIfNotExists(['Shift+Num+'],[],'cm_MarkCurrentExtension');
      AddIfNotExists(['Ctrl+Num-'],[],'cm_MarkUnmarkAll');
      AddIfNotExists(['Num-'],[],'cm_MarkMinus');
      AddIfNotExists(['Shift+Num-'],[],'cm_UnmarkCurrentExtension');
      AddIfNotExists(['Num*'],[],'cm_MarkInvert');
      AddIfNotExists(['Ctrl+C'],[],'cm_CopyToClipboard');
      AddIfNotExists(['Ctrl+V'],[],'cm_PasteFromClipboard');
      AddIfNotExists(['Ctrl+X'],[],'cm_CutToClipboard');
      AddIfNotExists(['Ctrl+Left'],[],'cm_TransferLeft');
      AddIfNotExists(['Ctrl+Right'],[],'cm_TransferRight');
    end;

  HMForm := HotMan.Forms.FindOrCreate('Viewer');
  with HMForm.Hotkeys do
    begin
      AddIfNotExists(['F1'],[],'cm_About');
      AddIfNotExists(['F2'],[],'cm_Reload');
      AddIfNotExists(['N'],[],'cm_LoadNextFile');
      AddIfNotExists(['P'],[],'cm_LoadPrevFile');
    end;

  HMForm := HotMan.Forms.FindOrCreate('Copy/Move Dialog');
  with HMForm.Hotkeys do
    begin
      AddIfNotExists(['F2'],[],'cm_AddToQueue');
    end;

  if not mbFileExists(gpCfgDir + gNameSCFile) then
    gNameSCFile := 'shortcuts.scf';
  HotMan.Save(gpCfgDir + gNameSCFile);
end;

function InitPropStorage(Owner: TComponent): TIniPropStorageEx;
var
  sWidth, sHeight: String;
begin
  Result:= TIniPropStorageEx.Create(Owner);
  Result.IniFileName:= gpCfgDir + 'session.ini';
  if Owner is TCustomForm then
    with Owner as TCustomForm do
    begin
      sWidth:= IntToStr(Monitor.Width);
      sHeight:= IntToStr(Monitor.Height);
      Result.IniSection:= ClassName + '(' + sWidth + 'x' + sHeight + ')';
    end;
end;

procedure FontToFontOptions(Font: TFont; out Options: TDCFontOptions);
begin
  with Options do
  begin
    Name  := Font.Name;
    Size  := Font.Size;
    Style := Font.Style;
  end;
end;

procedure FontOptionsToFont(Options: TDCFontOptions; Font: TFont);
begin
  with Options do
  begin
    Font.Name  := Name;
    Font.Size  := Size;
    Font.Style := Style;
  end;
end;

procedure OldKeysToNew(ActionEnabled: Boolean; ShiftState: TShiftState; Action: TKeyTypingAction);
var
  Modifier: TKeyTypingModifier;
begin
  if ActionEnabled then
  begin
    for Modifier in TKeyTypingModifier do
    begin
      if TKeyTypingModifierToShift[Modifier] = ShiftState then
        gKeyTyping[Modifier] := Action
      else if gKeyTyping[Modifier] = Action then
        gKeyTyping[Modifier] := ktaNone;
    end;
  end
  else
  begin
    for Modifier in TKeyTypingModifier do
    begin
      if gKeyTyping[Modifier] = Action then
      begin
        gKeyTyping[Modifier] := ktaNone;
        Break;
      end;
    end;
  end;
end;

function LoadStringsFromFile(var list: TStringListEx; const sFileName:String;
                             MaxStrings: Integer = 0): Boolean;
var
  i:Integer;
begin
  Assert(list <> nil,'LoadStringsFromFile: list=nil');
  list.Clear;
  Result:=False;
  if mbFileExists(sFileName) then
  begin
    list.LoadFromFile(sFileName);
    if MaxStrings > 0 then
    begin
      for i:=list.Count-1 downto 0 do
        if i>MaxStrings then
          list.Delete(i)
        else
          Break;
    end;
    Result:=True;
  end;
end;

procedure LoadDirHotList(AConfig: TXmlConfig; Node: TXmlNode);
var
  Name, Path: String;
begin
  glsHotDir.Clear;

  Node := Node.FindNode('DirectoryHotList');
  if Assigned(Node) then
  begin
    Node := Node.FirstChild;
    while Assigned(Node) do
    begin
      if Node.CompareName('HotDir') = 0 then
      begin
        if AConfig.TryGetAttr(Node, 'Name', Name) and
           AConfig.TryGetAttr(Node, 'Path', Path) then
        begin
          glsHotDir.Add(Name + '=' + Path);
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(Node) + '.');
      end;
      Node := Node.NextSibling;
    end;
  end;
end;

procedure ConvertIniToXml;
var
  MultiRename: TfrmMultiRename = nil;
  tmpFiles: TFiles = nil;
begin
  SaveXmlConfig;

  // Force loading Multi-rename config if it wasn't used yet.
  tmpFiles := TFiles.Create(mbGetCurrentDir);
  MultiRename := TfrmMultiRename.Create(nil, nil, tmpFiles);
  try
    MultiRename.LoadPresetsIni(gIni);
    MultiRename.PublicSavePresets;
  finally
    FreeThenNil(MultiRename);
    FreeThenNil(tmpFiles);
  end;

  FreeAndNil(gIni);

  if mbFileExists(gpGlobalCfgDir + 'doublecmd.ini') then
    mbRenameFile(gpGlobalCfgDir + 'doublecmd.ini', gpGlobalCfgDir + 'doublecmd.ini.obsolete');
  if mbFileExists(gpCfgDir + 'doublecmd.ini') then
    mbRenameFile(gpCfgDir + 'doublecmd.ini', gpCfgDir + 'doublecmd.ini.obsolete');
end;

procedure CopySettingsFiles;
begin
  { Create default configuration files if need }
  if gpCfgDir <> gpGlobalCfgDir then
    begin
      // extension file
      if not mbFileExists(gpCfgDir + 'doublecmd.ext') then
        CopyFile(gpGlobalCfgDir + 'doublecmd.ext.example', gpCfgDir + 'doublecmd.ext.example');
      // pixmaps file
      if not mbFileExists(gpCfgDir + 'pixmaps.txt') then
        CopyFile(gpGlobalCfgDir + 'pixmaps.txt', gpCfgDir + 'pixmaps.txt');
      // multiarc configuration file
      if not mbFileExists(gpCfgDir + 'multiarc.ini') then
        CopyFile(gpGlobalCfgDir + 'multiarc.ini', gpCfgDir + 'multiarc.ini');
    end;
end;

procedure CreateGlobs;
begin
  gExts := TExts.Create;
  gColorExt := TColorExt.Create;
  gFileInfoToolTip := TFileInfoToolTip.Create;
  glsHotDir := TStringListEx.Create;
  glsDirHistory := TStringListEx.Create;
  glsMaskHistory := TStringListEx.Create;
  glsSearchHistory := TStringListEx.Create;
  glsReplaceHistory := TStringListEx.Create;
  glsIgnoreList := TStringListEx.Create;
  gSearchTemplateList := TSearchTemplateList.Create;
  gDSXPlugins := TDSXModuleList.Create;
  gWCXPlugins := TWCXModuleList.Create;
  gWDXPlugins := TWDXModuleList.Create;
  gWFXPlugins := TWFXModuleList.Create;
  gWLXPlugins := TWLXModuleList.Create;
  gMultiArcList := TMultiArcList.Create;
  ColSet := TPanelColumnsList.Create;
  HotMan := THotKeyManager.Create;
end;

procedure DestroyGlobs;
begin
  FreeThenNil(gColorExt);
  FreeThenNil(gFileInfoToolTip);
  FreeThenNil(glsDirHistory);
  FreeThenNil(glsHotDir);
  FreeThenNil(glsMaskHistory);
  FreeThenNil(glsSearchHistory);
  FreeThenNil(glsReplaceHistory);
  FreeThenNil(glsIgnoreList);
  FreeThenNil(gExts);
  FreeThenNil(gIni);
  FreeThenNil(gConfig);
  FreeThenNil(gSearchTemplateList);
  FreeThenNil(gDSXPlugins);
  FreeThenNil(gWCXPlugins);
  FreeThenNil(gWDXPlugins);
  FreeThenNil(gWFXPlugins);
  FreeThenNil(gMultiArcList);
  FreeThenNil(gWLXPlugins);
  FreeThenNil(ColSet);
  FreeThenNil(HotMan);
end;

procedure SetDefaultConfigGlobs;

  procedure SetDefaultExternalTool(var ExternalToolOptions: TExternalToolOptions);
  begin
    with ExternalToolOptions do
    begin
      Enabled := False;
      Path := '';
      Parameters := '';
      RunInTerminal := False;
      KeepTerminalOpen := False;
    end;
  end;

begin
  { Language page }
  gPOFileName := '';

  { Behaviours page }
  gRunInTerm := RunInTerm;
  gRunTerm := RunTerm;
  gOnlyOneAppInstance := False;
  gLynxLike := True;
  gSortCaseSensitivity := cstNotSensitive;
  gSortNatural := False;
  gSortFolderMode := sfmSortNameShowFirst;
  gNewFilesPosition := nfpSortedPosition;
  gUpdatedFilesPosition := ufpNoChange;
  gShortFileSizeFormat := True;
  gMinimizeToTray := False;
  gAlwaysShowTrayIcon := False;
  gMouseSelectionEnabled := True;
  gMouseSelectionButton := 0;  // Left
  gScrollMode := smLineByLine;
  gWheelScrollLines:= Mouse.WheelScrollLines;
  gAutoFillColumns := False;
  gAutoSizeColumn := 1;
  gDateTimeFormat := 'dd.mm.yy';
  gCutTextToColWidth := True;
  gShowSystemFiles := False;
  // Under Mac OS X loading file list in separate thread are very very slow
  // so disable and hide this option under Mac OS X Carbon
  gListFilesInThread := {$IFDEF LCLCARBON}False{$ELSE}True{$ENDIF};
  gLoadIconsSeparately := True;
  gDelayLoadingTabs := True;
  gHighlightUpdatedFiles := True;
  gDriveBlackList := '';
  gDriveBlackListUnmounted := False;

  { Tools page }
  SetDefaultExternalTool(gExternalTools[etViewer]);
  SetDefaultExternalTool(gExternalTools[etEditor]);
  SetDefaultExternalTool(gExternalTools[etDiffer]);

  { Fonts page }
  gFonts[dcfMain].Name := 'default';
  gFonts[dcfMain].Size := 10;
  gFonts[dcfMain].Style := [fsBold];
  gFonts[dcfEditor].Name := MonoSpaceFont;
  gFonts[dcfEditor].Size := 14;
  gFonts[dcfEditor].Style := [];
  gFonts[dcfViewer].Name := MonoSpaceFont;
  gFonts[dcfViewer].Size := 14;
  gFonts[dcfViewer].Style := [];
  gFonts[dcfLog].Name := MonoSpaceFont;
  gFonts[dcfLog].Size := 12;
  gFonts[dcfLog].Style := [];
  gFonts[dcfViewerBook].Name := 'default';
  gFonts[dcfViewerBook].Size := 16;
  gFonts[dcfViewerBook].Style := [fsBold];

  { Colors page }
  gForeColor := clDefault;
  gBackColor := clWhite;
  gBackColor2 := clWhite;
  gMarkColor := clRed;
  gCursorColor := clHighlight;
  gCursorText := clHighlightText;
  gIndForeColor := clBlack;
  gIndBackColor := clWhite;
  gUseInvertedSelection := False;
  gInactivePanelBrightness := 100; // Full brightness
  gIndUseGradient := True;

  { Layout page }
  gMainMenu := True;
  gButtonBar := True;
  gToolBarFlat := True;
  gToolBarButtonSize := 24;
  gToolBarIconSize := 16;
  gDriveBar1 := True;
  gDriveBar2 := True;
  gDriveBarFlat := True;
  gDrivesListButton := True;
  gDirectoryTabs := True;
  gCurDir := True;
  gTabHeader := True;
  gStatusBar := True;
  gCmdLine := True;
  gLogWindow := False;
  gTermWindow := False;
  gKeyButtons := True;
  gInterfaceFlat := True;
  gDriveInd := False;
  gDriveFreeSpace := True;
  gProgInMenuBar := False;
  gPanelOfOp := True;
  gShortFormatDriveInfo := True;
  gHorizontalFilePanels := False;
  gDrivesListButtonOptions := [dlbShowLabel, dlbShowFileSystem, dlbShowFreeSpace];

  { Keys page }
  gKeyTyping[ktmNone]    := ktaCommandLine;
  gKeyTyping[ktmAlt]     := ktaNone;
  gKeyTyping[ktmCtrlAlt] := ktaQuickSearch;

  { File operations page }
  gCopyBlockSize := 524288;
  gUseMmapInSearch := False;
  gPartialNameSearch := True;
  gWipePassNumber := 1;
  gDropReadOnlyFlag := True;
  gProcessComments := True;
  gRenameSelOnlyName := False;
  gShowCopyTabSelectPanel := False;
  gUseTrash := True;
  gSkipFileOpError := False;
  gShowDialogOnDragDrop := False;
  gOverwriteFolder := False;
  gFileOperationsProgressKind := fopkSeparateWindow;
  // Operations options
  gOperationOptionSymLinks := fsooslNone;
  gOperationOptionCorrectLinks := False;
  gOperationOptionFileExists := fsoofeNone;
  gOperationOptionDirectoryExists := fsoodeNone;
  gOperationOptionSetPropertyError := fsoospeNone;
  gOperationOptionCheckFreeSpace := True;
  gOperationOptionCopyAttributes := True;
  gOperationOptionCopyTime := True;
  gOperationOptionCopyOwnership := True;


  { Tabs page }
  gDirTabOptions := [tb_always_visible,
                     tb_confirm_close_all,
                     tb_show_asterisk_for_locked,
                     tb_activate_panel_on_click];
  gDirTabLimit := 32;
  gDirTabPosition := tbpos_top;

  { Log page }
  gLogFile := False;
  gLogFileName := gpCfgDir + 'doublecmd.log';
  gLogOptions := [log_cp_mv_ln, log_delete, log_dir_op, log_arc_op,
                  log_vfs_op, log_success, log_errors, log_info];

  { Configuration page }
  gSaveConfiguration := True;
  gSaveSearchReplaceHistory := True;
  gSaveDirHistory := True;
  gSaveCmdLineHistory := True;
  gSaveFileMaskHistory := True;

  { Quick Search/Filter page }
  gQuickSearchOptions.Match := [qsmBeginning, qsmEnding];
  gQuickSearchOptions.Items := qsiFilesAndDirectories;
  gQuickSearchOptions.SearchCase := qscInsensitive;
  gQuickFilterAutoHide := True;

  { Miscellaneous page }
  gGridVertLine := False;
  gGridHorzLine := False;
  gShowWarningMessages := True;
  gSpaceMovesDown := False;
  gDirBrackets := True;
  gShowToolTipMode := [stm_show_for_all];

  { Auto refresh page }
  gWatchDirs := [watch_file_name_change, watch_attributes_change];
  gWatchDirsExclude := '';
  gWatcherMode := fswmPreventDelete;

  { Icons page }
  gShowIcons := sim_all_and_exe;
  gShowIconsNew := gShowIcons;
  gIconOverlays := False;
  gIconsSize := 16;
  gIconsSizeNew := gIconsSize;
  gCustomDriveIcons := False;

  { Ignore list page }
  gIgnoreListFileEnabled := False;
  gIgnoreListFile := gpCfgDir + 'ignorelist.txt';

  {Viewer}
  gImageStretch := False;
  gPreviewVisible := False;
  gSaveThumb := True;
  gCopyMovePath1 := '';
  gCopyMovePath2 := '';
  gCopyMovePath3 := '';
  gCopyMovePath4 := '';
  gCopyMovePath5 := '';
  gImagePaintMode := 'Pen';
  gImagePaintWidth := 5;
  gColCount := 1;
  gImagePaintColor := clRed;
  gBookBackgroundColor := clBlack;
  gBookFontColor := clWhite;
  gTextPosition:= 0;
  gViewerMode:= 0;

  { - Other - }
  gLuaLib := LuaDLL;
  gNameSCFile := 'shortcuts.scf';
  gLastUsedPacker := 'zip';
  gUseShellForFileOperations :=
    {$IF DEFINED(MSWINDOWS)}WindowsVersion >= wvVista{$ELSE}False{$ENDIF};

  gExts.Clear;
  gColorExt.Clear;
  gFileInfoToolTip.Clear;
  glsHotDir.Clear;
  glsDirHistory.Clear;
  glsMaskHistory.Clear;
  glsSearchHistory.Clear;
  glsReplaceHistory.Clear;
  glsIgnoreList.Clear;
  gSearchTemplateList.Clear;
  gDSXPlugins.Clear;
  gWCXPlugins.Clear;
  gWDXPlugins.Clear;
  gWFXPlugins.Clear;
  gWLXPlugins.Clear;
  gMultiArcList.Clear;
  ColSet.Clear;
end;

procedure SetDefaultNonConfigGlobs;
begin
  { - Not in config - }
  gHelpLang := '';
  gRepeatPassword := True;
  gDirHistoryCount := 30;
  gFirstTextSearch := True;
  gErrorFile := gpCfgDir + ExtractOnlyFileName(Application.ExeName) + '.err';
end;

function OpenConfig: Boolean;
begin
  if Assigned(gConfig) then
    Exit(True);

  // Check global directory for XML config.
  if not Assigned(gConfig) and (gpCmdLineCfgDir = EmptyStr) and
     mbFileExists(gpGlobalCfgDir + 'doublecmd.xml') then
  begin
    if mbFileAccess(gpGlobalCfgDir + 'doublecmd.xml', fmOpenRead) then
    begin
      gConfig := TXmlConfig.Create(gpGlobalCfgDir + 'doublecmd.xml');
      gUseConfigInProgramDir := gConfig.GetValue(gConfig.RootNode, 'Configuration/UseConfigInProgramDir', False);
      if not gUseConfigInProgramDir then
      begin
        if mbFileExists(gpCfgDir + 'doublecmd.xml') then
          // Close global config so that the local config is opened below.
          FreeAndNil(gConfig)
        else
          // Local config is used but it doesn't exist. Use global config that has just
          // been read but set file name accordingly and later save to local config.
          gConfig.FileName := gpCfgDir + 'doublecmd.xml';
      end;
    end
    else
      // File is not readable - print warning and continue below to check config in user directory.
      DCDebug('Warning: Config file ' + gpGlobalCfgDir + 'doublecmd.xml' +
              ' exists but is not readable.');
  end;

  // Check user directory for XML config.
  if not Assigned(gConfig) and mbFileExists(gpCfgDir + 'doublecmd.xml') then
  begin
    if mbFileAccess(gpCfgDir + 'doublecmd.xml', fmOpenRead) then
    begin
      gConfig := TXmlConfig.Create(gpCfgDir + 'doublecmd.xml');
      gUseConfigInProgramDir := False;
    end
    else
    begin
      DCDebug('Error: Cannot read config file ' + gpGlobalCfgDir + 'doublecmd.xml.');
      Exit(False);
    end;
  end;

  if not Assigned(gConfig) then
  begin
    // Open INI config if present.

    // Check global directory for INI config.
    if not Assigned(gIni) and mbFileAccess(gpGlobalCfgDir + 'doublecmd.ini', fmOpenRead) then
    begin
      gIni := TIniFileEx.Create(gpGlobalCfgDir + 'doublecmd.ini', fmOpenRead);
      gUseConfigInProgramDir := gIni.ReadBool('Configuration', 'UseIniInProgramDir', False);
      if not gUseConfigInProgramDir then
        FreeAndNil(gIni)
      else
      begin
	      if mbFileAccess(gpGlobalCfgDir + 'doublecmd.ini', fmOpenWrite) then
	      begin
	        FreeAndNil(gIni);
          gIni := TIniFileEx.Create(gpGlobalCfgDir + 'doublecmd.ini');
	      end
        else
	      begin
          DCDebug('Warning: Config file ' + gpGlobalCfgDir + 'doublecmd.ini' +
                  ' is not accessible for writing. Configuration will not be saved.');
	      end;
	    end;
    end;

    // Check user directory for INI config.
    if not Assigned(gIni) and mbFileAccess(gpCfgDir + 'doublecmd.ini', fmOpenRead) then
    begin
      gIni := TIniFileEx.Create(gpCfgDir + 'doublecmd.ini');
      gUseConfigInProgramDir := False;
    end;

    if Assigned(gIni) then
    begin
      if gUseConfigInProgramDir then
        gConfig := TXmlConfig.Create(gpGlobalCfgDir + 'doublecmd.xml')
      else
        gConfig := TXmlConfig.Create(gpCfgDir + 'doublecmd.xml');
    end;
  end;

  // By default use config in user directory.
  if not Assigned(gConfig) then
  begin
    gConfig := TXmlConfig.Create(gpCfgDir + 'doublecmd.xml');
    gUseConfigInProgramDir := False;
  end;

  gUseConfigInProgramDirNew := gUseConfigInProgramDir;

  // If global config is used then set config directory as global config directory.
  if gUseConfigInProgramDir then
    gpCfgDir := gpGlobalCfgDir;

  if mbFileExists(gpCfgDir + 'doublecmd.xml') and
     (not mbFileAccess(gpCfgDir + 'doublecmd.xml', fmOpenWrite)) then
  begin
    DCDebug('Warning: Config file ' + gpCfgDir + 'doublecmd.xml' +
            ' is not accessible for writing. Configuration will not be saved.');
  end
  else
    gConfig.SaveOnDestroy := True;

  if not mbDirectoryExists(gpCfgDir) then
    mbForceDirectory(gpCfgDir);

  Result := True;
end;

function LoadGlobs: Boolean;
begin
  Result := False;
  if not OpenConfig then
    Exit;

  DCDebug('Loading configuration...');
  SetDefaultConfigGlobs;
  if Assigned(gIni) then
    LoadIniConfig
  else if Assigned(gConfig) then
    LoadXmlConfig
  else
  begin
    DCDebug('Error: No config created.');
    Exit(False);
  end;

  // Update plugins if DC version is changed
  if (gPreviousVersion <> dcVersion) then UpdatePlugins;

  // Set secondary variables for options that need restart.
  gShowIconsNew := gShowIcons;
  gIconsSizeNew := gIconsSize;

  CopySettingsFiles;

  if mbFileExists(gpCfgDir + 'doublecmd.ext') then
    gExts.LoadFromFile(gpCfgDir + 'doublecmd.ext');

  LoadStringsFromFile(glsDirHistory, gpCfgDir + 'dirhistory.txt', cMaxStringItems);
  LoadStringsFromFile(glsMaskHistory, gpCfgDir + 'maskhistory.txt', cMaxStringItems);
  LoadStringsFromFile(glsSearchHistory, gpCfgDir + 'searchhistory.txt', cMaxStringItems);
  LoadStringsFromFile(glsReplaceHistory, gpCfgDir + 'replacehistory.txt', cMaxStringItems);
  LoadStringsFromFile(glsIgnoreList, ReplaceEnvVars(gIgnoreListFile));

  { Hotkeys }
  if not mbFileExists(gpCfgDir + gNameSCFile) then
    gNameSCFile := 'shortcuts.scf';
  // Rename old shortcuts file to new name.
  if mbFileExists(gpCfgDir + 'shortcuts.ini') and
     not mbFileExists(gpCfgDir + gNameSCFile) then
       mbRenameFile(gpCfgDir + 'shortcuts.ini', gpCfgDir + gNameSCFile);
  HotMan.Load(gpCfgDir + gNameSCFile);

  { MultiArc addons }
  if mbFileExists(gpCfgDir + 'multiarc.ini') then
    gMultiArcList.LoadFromFile(gpCfgDir + 'multiarc.ini');

  { Localization }
  DoLoadLng;
  msgLoadLng;
  FillFileFuncList;
  Result := True;
end;

procedure SaveGlobs;
var
  TmpConfig: TXmlConfig;
  Ini: TIniFileEx = nil;
  FileName: UTF8String;
begin
  if (gUseConfigInProgramDirNew <> gUseConfigInProgramDir) and
     (gpCmdLineCfgDir = EmptyStr) then
    begin
      LoadPaths;
      if gUseConfigInProgramDirNew then
        gpCfgDir := gpGlobalCfgDir;

      { Save location of configuration files }

      if Assigned(gIni) then
      begin
        // Still using INI config.
        FreeThenNil(gIni);
        try
          Ini:= TIniFileEx.Create(gpGlobalCfgDir + 'doublecmd.ini');
          Ini.WriteBool('Configuration', 'UseIniInProgramDir', gUseConfigInProgramDirNew);
        finally
          FreeThenNil(Ini);
        end;
        gIni := TIniFileEx.Create(gpCfgDir + 'doublecmd.ini');
      end;

      TmpConfig := TXmlConfig.Create(gpGlobalCfgDir + 'doublecmd.xml');
      try
        TmpConfig.SetValue(TmpConfig.RootNode, 'Configuration/UseConfigInProgramDir', gUseConfigInProgramDirNew);
        TmpConfig.Save;
      finally
        TmpConfig.Free;
      end;
      gConfig.FileName := gpCfgDir + 'doublecmd.xml';
    end;

  gExts.SaveToFile(gpCfgDir + 'doublecmd.ext');

  if gSaveDirHistory then
    glsDirHistory.SaveToFile(gpCfgDir + 'dirhistory.txt');
  if gSaveFileMaskHistory then
    glsMaskHistory.SaveToFile(gpCfgDir + 'maskhistory.txt');
  if gSaveSearchReplaceHistory then
  begin
    glsSearchHistory.SaveToFile(gpCfgDir + 'searchhistory.txt');
    glsReplaceHistory.SaveToFile(gpCfgDir + 'replacehistory.txt');
  end;
  if gIgnoreListFileEnabled then
  begin
    FileName:= ReplaceEnvVars(gIgnoreListFile);
    mbForceDirectory(ExtractFileDir(FileName));
    glsIgnoreList.SaveToFile(FileName);
  end;
  gMultiArcList.SaveToFile(gpCfgDir + 'multiarc.ini');

  { Hotkeys }
  if not mbFileExists(gpCfgDir + gNameSCFile) then
    gNameSCFile := 'shortcuts.scf';
  HotMan.Save(gpCfgDir + gNameSCFile);

  if Assigned(gIni) then
    SaveIniConfig;
  SaveXmlConfig;

  // Force saving config to file.
  gConfig.Save;
end;

procedure LoadIniConfig;
var
  oldQuickSearch: Boolean = True;
  oldQuickFilter: Boolean = False;
  oldQuickSearchMode: TShiftState = [ssCtrl, ssAlt];
  oldQuickFilterMode: TShiftState = [];
begin
  { Layout page }

  gButtonBar := gIni.ReadBool('Layout', 'ButtonBar', True);
  gToolBarFlat := gIni.ReadBool('ButtonBar', 'FlatIcons', True);
  gToolBarButtonSize := gIni.ReadInteger('ButtonBar', 'ButtonHeight', 16);
  gToolBarIconSize := gIni.ReadInteger('ButtonBar', 'SmallIconSize', 16);
  gDriveBar1 := gIni.ReadBool('Layout', 'DriveBar1', True);
  gDriveBar2 := gIni.ReadBool('Layout', 'DriveBar2', True);
  gDriveBarFlat := gIni.ReadBool('Layout', 'DriveBarFlat', True);
  gDrivesListButton := gIni.ReadBool('Layout', 'DriveMenuButton', True);
  gDirectoryTabs := gIni.ReadBool('Layout', 'DirectoryTabs', True);
  gCurDir := gIni.ReadBool('Layout', 'CurDir', True);
  gTabHeader := gIni.ReadBool('Layout', 'TabHeader', True);
  gStatusBar := gIni.ReadBool('Layout', 'StatusBar', True);
  gCmdLine := gIni.ReadBool('Layout', 'CmdLine', True);
  gLogWindow := gIni.ReadBool('Layout', 'LogWindow', True);
  gTermWindow := gIni.ReadBool('Layout', 'TermWindow', False);
  gKeyButtons := gIni.ReadBool('Layout', 'KeyButtons', True);
  gInterfaceFlat := gIni.ReadBool('Layout', 'InterfaceFlat', True);

  gShowSystemFiles := gIni.ReadBool('Configuration', 'ShowSystemFiles', False);
  gPOFileName := gIni.ReadString('Configuration', 'Language', '?');
  gRunInTerm := gIni.ReadString('Configuration', 'RunInTerm', RunInTerm);
  gOnlyOneAppInstance:= gIni.ReadBool('Configuration', 'OnlyOnce', False);
  if gIni.ReadBool('Configuration', 'CaseSensitiveSort', False) = False then
    gSortCaseSensitivity := cstNotSensitive
  else
    gSortCaseSensitivity := cstLocale;
  gLynxLike := gIni.ReadBool('Configuration', 'LynxLike', True);
  gShortFileSizeFormat := gIni.ReadBool('Configuration', 'ShortFileSizeFormat', True);
  gScrollMode := TScrollMode(gIni.ReadInteger('Configuration', 'ScrollMode', Integer(gScrollMode)));
  gMinimizeToTray := gIni.ReadBool('Configuration', 'MinimizeToTray', False);
  gAlwaysShowTrayIcon := gIni.ReadBool('Configuration', 'AlwaysShowTrayIcon', False);
  gDateTimeFormat := gIni.ReadString('Configuration', 'DateTimeFormat', 'dd.mm.yy');
  gDriveBlackList:= gIni.ReadString('Configuration', 'DriveBlackList', '');
  gSpaceMovesDown := gIni.ReadBool('Configuration', 'SpaceMovesDown', False);

  {$IFNDEF LCLCARBON}
  // Under Mac OS X loading file list in separate thread are very very slow
  // so disable and hide this option under Mac OS X Carbon
  gListFilesInThread := gIni.ReadBool('Configuration', 'ListFilesInThread', gListFilesInThread);
  {$ENDIF}
  gLoadIconsSeparately := gIni.ReadBool('Configuration', 'LoadIconsSeparately', gLoadIconsSeparately);

  gMouseSelectionEnabled:= gIni.ReadBool('Configuration', 'MouseSelectionEnabled', True);
  gMouseSelectionButton := gIni.ReadInteger('Configuration', 'MouseSelectionButton', 0);

  gAutoFillColumns:= gIni.ReadBool('Configuration', 'AutoFillColumns', False);
  gAutoSizeColumn := gIni.ReadInteger('Configuration', 'AutoSizeColumn', 1);

  gDirTabOptions := TTabsOptions(gIni.ReadInteger('Configuration', 'DirTabOptions', Integer(gDirTabOptions)));
  gDirTabLimit :=  gIni.ReadInteger('Configuration', 'DirTabLimit', 32);
  gDirTabPosition := TTabsPosition(gIni.ReadInteger('Configuration', 'DirTabPosition', Integer(gDirTabPosition)));

  gExternalTools[etEditor].Enabled := gIni.ReadBool('Configuration', 'UseExtEdit', False);
  gExternalTools[etViewer].Enabled := gIni.ReadBool('Configuration', 'UseExtView', False);
  gExternalTools[etDiffer].Enabled := gIni.ReadBool('Configuration', 'UseExtDiff', False);
  gExternalTools[etEditor].Path := gIni.ReadString('Configuration', 'ExtEdit', '');
  gExternalTools[etViewer].Path := gIni.ReadString('Configuration', 'ExtView', '');
  gExternalTools[etDiffer].Path := gIni.ReadString('Configuration', 'ExtDiff', '');

  gRunTerm := gIni.ReadString('Configuration', 'RunTerm', RunTerm);

  gLuaLib:=gIni.ReadString('Configuration', 'LuaLib', gLuaLib);

  { Fonts }
  gFonts[dcfMain].Name:=gIni.ReadString('Configuration', 'Font.Name', 'default');
  gFonts[dcfEditor].Name:=gIni.ReadString('Editor', 'Font.Name', MonoSpaceFont);
  gFonts[dcfViewer].Name:=gIni.ReadString('Viewer', 'Font.Name', MonoSpaceFont);
  gFonts[dcfMain].Size:=gIni.ReadInteger('Configuration', 'Font.Size', 10);
  gFonts[dcfEditor].Size:=gIni.ReadInteger('Editor', 'Font.Size', 14);
  gFonts[dcfViewer].Size:=gIni.ReadInteger('Viewer', 'Font.Size', 14);
  gFonts[dcfMain].Style := TFontStyles(gIni.ReadInteger('Configuration', 'Font.Style', 1));
  gFonts[dcfEditor].Style := TFontStyles(gIni.ReadInteger('Editor', 'Font.Style', 0));
  gFonts[dcfViewer].Style := TFontStyles(gIni.ReadInteger('Viewer', 'Font.Style', 0));

  { Colors }
  gForeColor  := gIni.ReadInteger('Colors', 'ForeColor', clDefault);
  gBackColor := gIni.ReadInteger('Colors', 'BackColor', clWhite);
  gBackColor2 := gIni.ReadInteger('Colors', 'BackColor2', clWhite);
  gMarkColor := gIni.ReadInteger('Colors', 'MarkColor', clRed);
  gCursorColor := gIni.ReadInteger('Colors', 'CursorColor', clHighlight);
  gCursorText := gIni.ReadInteger('Colors', 'CursorText', clHighlightText);
  gUseInvertedSelection:= gIni.ReadBool('Colors', 'UseInvertedSelection', False);
  gInactivePanelBrightness:= gIni.ReadInteger('Colors', 'InactivePanelBrightness', gInactivePanelBrightness);
  gUseFrameCursor:= gIni.ReadBool('Colors', 'UseFrameCursor', gUseFrameCursor);

  { File operations }
  gCopyBlockSize := gIni.ReadInteger('Configuration', 'CopyBlockSize', 65536);
  gSkipFileOpError:= gIni.ReadBool('Configuration', 'SkipFileOpError', False);
  gDropReadOnlyFlag := gIni.ReadBool('Configuration', 'DropReadOnlyFlag', True);
  gUseMmapInSearch := gIni.ReadBool('Configuration', 'UseMmapInSearch', False);
  gWipePassNumber:= gIni.ReadInteger('Configuration', 'WipePassNumber', 1);
  gProcessComments := gIni.ReadBool('Configuration', 'ProcessComments', True);
  gRenameSelOnlyName:= gIni.ReadBool('Configuration', 'RenameSelOnlyName', false);
  gShowCopyTabSelectPanel:= gIni.ReadBool('Configuration', 'ShowCopyTabSelectPanel', false);
  gUseTrash := gIni.ReadBool('Configuration', 'UseTrash', True); // 05.05.2009 - read global trash option from configuration file
  gShowDialogOnDragDrop := gIni.ReadBool('Configuration', 'ShowDialogOnDragDrop', gShowDialogOnDragDrop);

  { Log }
  gLogFile := gIni.ReadBool('Configuration', 'LogFile', True);
  gLogFileName := gIni.ReadString('Configuration', 'LogFileName', gpCfgDir + 'doublecmd.log');
  gLogOptions := TLogOptions(gIni.ReadInteger('Configuration', 'LogOptions', Integer(gLogOptions)));
  { Configuration page }
  gSaveDirHistory := gIni.ReadBool('Configuration', 'SaveDirHistory', True);
  gSaveCmdLineHistory := gIni.ReadBool('Configuration', 'SaveCmdLineHistory', True);
  gSaveFileMaskHistory := gIni.ReadBool('Configuration', 'SaveFileMaskHistory', True);
  { Quick Search page}
  oldQuickSearch := gIni.ReadBool('Configuration', 'QuickSearch', oldQuickSearch);
  oldQuickSearchMode := TShiftState(gIni.ReadInteger('Configuration', 'QuickSearchMode', Integer(oldQuickSearchMode)));
  OldKeysToNew(oldQuickSearch, oldQuickSearchMode, ktaQuickSearch);
  oldQuickFilter := gIni.ReadBool('Configuration', 'QuickFilter', oldQuickFilter);
  oldQuickFilterMode := TShiftState(gIni.ReadInteger('Configuration', 'QuickFilterMode', Integer(oldQuickFilterMode)));
  OldKeysToNew(oldQuickFilter, oldQuickFilterMode, ktaQuickFilter);
  if gIni.ReadBool('Configuration', 'QuickSearchMatchBeginning', qsmBeginning in gQuickSearchOptions.Match) then
    Include(gQuickSearchOptions.Match, qsmBeginning)
  else
    Exclude(gQuickSearchOptions.Match, qsmBeginning);
  if gIni.ReadBool('Configuration', 'QuickSearchMatchEnding', qsmEnding in gQuickSearchOptions.Match) then
    Include(gQuickSearchOptions.Match, qsmEnding)
  else
    Exclude(gQuickSearchOptions.Match, qsmEnding);
  { Misc page }
  gGridVertLine:= gIni.ReadBool('Configuration', 'GridVertLine', False);
  gGridHorzLine:= gIni.ReadBool('Configuration', 'GridHorzLine', False);
  gShowWarningMessages := gIni.ReadBool('Configuration', 'ShowWarningMessages', True);
  gDirBrackets:= gIni.ReadBool('Configuration', 'DirBrackets', True);
  gShowToolTipMode:= TShowToolTipMode(gIni.ReadInteger('Configuration', 'ShowToolTipMode', Integer(gShowToolTipMode)));
  { Auto refresh page }
  gWatchDirs := TWatchOptions(gIni.ReadInteger('Configuration', 'WatchDirs', Integer(gWatchDirs)));
  gWatchDirsExclude := gIni.ReadString('Configuration', 'WatchDirsExclude', '');
  { Icons page }
  gShowIcons := TShowIconsMode(gIni.ReadInteger('Configuration', 'ShowIcons', Integer(gShowIcons)));
  gIconOverlays:= gIni.ReadBool('Configuration', 'IconOverlays', True);
  gIconsSize := gIni.ReadInteger('Configuration', 'IconsSize', 16);
  gCustomDriveIcons := gIni.ReadBool('Configuration', 'CustomDriveIcons', False);
  { Ignore list page }
  gIgnoreListFileEnabled:= gIni.ReadBool('Configuration', 'IgnoreListFileEnabled', False);
  gIgnoreListFile:= gIni.ReadString('Configuration', 'IgnoreListFile', gpCfgDir + 'ignorelist.txt');

  gCutTextToColWidth := gIni.ReadBool('Configuration', 'CutTextToColWidth', True);

  gImageStretch:=  gIni.ReadBool('Viewer', 'Image.Stretch', False);

  { Operations options }
  gOperationOptionSymLinks := TFileSourceOperationOptionSymLink(
                                gIni.ReadInteger('Operations', 'Symlink', Integer(gOperationOptionSymLinks)));
  gOperationOptionCorrectLinks := gIni.ReadBool('Operations', 'CorrectLinks', gOperationOptionCorrectLinks);
  gOperationOptionFileExists := TFileSourceOperationOptionFileExists(
                                  gIni.ReadInteger('Operations', 'FileExists', Integer(gOperationOptionFileExists)));
  gOperationOptionDirectoryExists := TFileSourceOperationOptionDirectoryExists(
                                       gIni.ReadInteger('Operations', 'DirectoryExists', Integer(gOperationOptionDirectoryExists)));
  gOperationOptionCheckFreeSpace := gIni.ReadBool('Operations', 'CheckFreeSpace', gOperationOptionCheckFreeSpace);

  gIni.ReadSectionRaw('DirectoryHotList', glsHotDir);

  gColorExt.LoadIni;

  { Search template list }
  gSearchTemplateList.LoadFromIni(gIni);

  { Columns sets }
  ColSet.Load(gIni);

  { Plugins }
  gDSXPlugins.Load(gIni);
  gWCXPlugins.Load(gIni);
  gWDXPlugins.Load(gIni);
  gWFXPlugins.Load(gIni);
  gWLXPlugins.Load(gIni);
end;

procedure SaveIniConfig;
var
  I: LongInt;
begin
  gIni.EraseSection('DirectoryHotList');
  for I:= 0 to glsHotDir.Count - 1 do
    gIni.WriteString('DirectoryHotList', glsHotDir.Names[I], glsHotDir.ValueFromIndex[I]);

  { Layout page }

  gIni.WriteBool('Layout', 'ButtonBar', gButtonBar);
  gIni.WriteBool('ButtonBar', 'FlatIcons', gToolBarFlat);
  gIni.WriteInteger('ButtonBar', 'ButtonHeight', gToolBarButtonSize);
  gIni.WriteInteger('ButtonBar', 'SmallIconSize', gToolBarIconSize);
  gIni.WriteBool('Layout', 'DriveBar1', gDriveBar1);
  gIni.WriteBool('Layout', 'DriveBar2', gDriveBar2);
  gIni.WriteBool('Layout', 'DriveBarFlat', gDriveBarFlat);
  gIni.WriteBool('Layout', 'DriveMenuButton', gDrivesListButton);
  gIni.WriteBool('Layout', 'DirectoryTabs', gDirectoryTabs);
  gIni.WriteBool('Layout', 'CurDir', gCurDir);
  gIni.WriteBool('Layout', 'TabHeader', gTabHeader);
  gIni.WriteBool('Layout', 'StatusBar', gStatusBar);
  gIni.WriteBool('Layout', 'CmdLine', gCmdLine);
  gIni.WriteBool('Layout', 'LogWindow', gLogWindow);
  gIni.WriteBool('Layout', 'TermWindow', gTermWindow);
  gIni.WriteBool('Layout', 'KeyButtons', gKeyButtons);
  gIni.WriteBool('Layout', 'InterfaceFlat', gInterfaceFlat);

  gIni.WriteBool('Configuration', 'ShowSystemFiles', gShowSystemFiles);
  gIni.WriteString('Configuration', 'Language', gPOFileName);
  gIni.WriteString('Configuration', 'RunInTerm', gRunInTerm);
  gIni.WriteBool('Configuration', 'OnlyOnce', gOnlyOneAppInstance);
  if gSortCaseSensitivity = cstNotSensitive then
    gIni.WriteBool('Configuration', 'CaseSensitiveSort', False)
  else
    gIni.WriteBool('Configuration', 'CaseSensitiveSort', True);
  gIni.WriteBool('Configuration', 'LynxLike', gLynxLike);

  gIni.WriteBool('Configuration', 'ShortFileSizeFormat', gShortFileSizeFormat);
  gIni.WriteInteger('Configuration', 'ScrollMode', Integer(gScrollMode));
  gIni.WriteBool('Configuration', 'MinimizeToTray', gMinimizeToTray);
  gIni.WriteBool('Configuration', 'AlwaysShowTrayIcon', gAlwaysShowTrayIcon);
  gIni.WriteString('Configuration', 'DateTimeFormat', gDateTimeFormat);
  gIni.WriteString('Configuration', 'DriveBlackList', gDriveBlackList);
  gIni.WriteBool('Configuration', 'SpaceMovesDown', gSpaceMovesDown);

  {$IFNDEF LCLCARBON}
  // Under Mac OS X loading file list in separate thread are very very slow
  // so disable and hide this option under Mac OS X Carbon
  gIni.WriteBool('Configuration', 'ListFilesInThread', gListFilesInThread);
  {$ENDIF}
  gIni.WriteBool('Configuration', 'LoadIconsSeparately', gLoadIconsSeparately);

  gIni.WriteBool('Configuration', 'MouseSelectionEnabled', gMouseSelectionEnabled);
  gIni.WriteInteger('Configuration', 'MouseSelectionButton', gMouseSelectionButton);

  gIni.WriteBool('Configuration', 'AutoFillColumns', gAutoFillColumns);
  gIni.WriteInteger('Configuration', 'AutoSizeColumn', gAutoSizeColumn);

  gIni.WriteInteger('Configuration', 'DirTabOptions', Integer(gDirTabOptions));
  gIni.WriteInteger('Configuration', 'DirTabLimit', gDirTabLimit);
  gIni.WriteInteger('Configuration', 'DirTabPosition', Integer(gDirTabPosition));

  gIni.WriteBool('Configuration', 'UseExtEdit', gExternalTools[etEditor].Enabled);
  gIni.WriteBool('Configuration', 'UseExtView', gExternalTools[etViewer].Enabled);
  gIni.WriteBool('Configuration', 'UseExtDiff', gExternalTools[etDiffer].Enabled);

  gIni.WriteString('Configuration', 'ExtEdit', gExternalTools[etEditor].Path);
  gIni.WriteString('Configuration', 'ExtView', gExternalTools[etViewer].Path);
  gIni.WriteString('Configuration', 'ExtDiff', gExternalTools[etDiffer].Path);
  gIni.WriteString('Configuration', 'RunTerm', gRunTerm);

  gIni.WriteString('Configuration', 'LuaLib', gLuaLib);

  { Fonts }
  gIni.WriteString('Configuration', 'Font.Name', gFonts[dcfMain].Name);
  gIni.WriteString('Editor', 'Font.Name', gFonts[dcfEditor].Name);
  gIni.WriteString('Viewer', 'Font.Name', gFonts[dcfViewer].Name);

  gIni.WriteInteger('Configuration', 'Font.Size', gFonts[dcfMain].Size);
  gIni.WriteInteger('Editor', 'Font.Size', gFonts[dcfEditor].Size);
  gIni.WriteInteger('Viewer', 'Font.Size', gFonts[dcfViewer].Size);

  gIni.WriteInteger('Configuration', 'Font.Style', Integer(gFonts[dcfMain].Style));
  gIni.WriteInteger('Editor', 'Font.Style', Integer(gFonts[dcfEditor].Style));
  gIni.WriteInteger('Viewer', 'Font.Style', Integer(gFonts[dcfViewer].Style));
  { Colors }
  gIni.WriteInteger('Colors', 'ForeColor', gForeColor);
  gIni.WriteInteger('Colors', 'BackColor', gBackColor);
  gIni.WriteInteger('Colors', 'BackColor2', gBackColor2);
  gIni.WriteInteger('Colors', 'MarkColor', gMarkColor);
  gIni.WriteInteger('Colors', 'CursorColor', gCursorColor);
  gIni.WriteInteger('Colors', 'CursorText', gCursorText);
  gIni.WriteBool('Colors', 'UseInvertedSelection', gUseInvertedSelection);
  gIni.WriteInteger('Colors', 'InactivePanelBrightness', gInactivePanelBrightness);
  gIni.WriteBool('Colors', 'UseFrameCursor', gUseFrameCursor);

  { File operations }
  gIni.WriteInteger('Configuration', 'CopyBlockSize', gCopyBlockSize);
  gIni.WriteBool('Configuration', 'SkipFileOpError', gSkipFileOpError);
  gIni.WriteBool('Configuration', 'DropReadOnlyFlag', gDropReadOnlyFlag);
  gIni.WriteBool('Configuration', 'UseMmapInSearch', gUseMmapInSearch);
  gIni.WriteInteger('Configuration', 'WipePassNumber', gWipePassNumber);
  gIni.WriteBool('Configuration', 'ProcessComments', gProcessComments);
  gIni.WriteBool('Configuration', 'RenameSelOnlyName', gRenameSelOnlyName);
  gIni.WriteBool('Configuration', 'ShowCopyTabSelectPanel', gShowCopyTabSelectPanel);
  gIni.WriteBool('Configuration', 'UseTrash', gUseTrash);
  gIni.WriteBool('Configuration', 'ShowDialogOnDragDrop', gShowDialogOnDragDrop);

  { Log }
  gIni.WriteBool('Configuration', 'LogFile', gLogFile);
  gIni.WriteString('Configuration', 'LogFileName', gLogFileName);
  gIni.WriteInteger('Configuration', 'LogOptions', Integer(gLogOptions));
  { Configuration page }
  gIni.WriteBool('Configuration', 'SaveDirHistory', gSaveDirHistory);
  gIni.WriteBool('Configuration', 'SaveCmdLineHistory', gSaveCmdLineHistory);
  gIni.WriteBool('Configuration', 'SaveFileMaskHistory', gSaveFileMaskHistory);
  { Quick Search page}
  gIni.WriteBool('Configuration', 'QuickSearchMatchBeginning', qsmBeginning in gQuickSearchOptions.Match);
  gIni.WriteBool('Configuration', 'QuickSearchMatchEnding', qsmEnding in gQuickSearchOptions.Match);
  { Misc page }
  gIni.WriteBool('Configuration', 'GridVertLine', gGridVertLine);
  gIni.WriteBool('Configuration', 'GridHorzLine', gGridHorzLine);
  gIni.WriteBool('Configuration', 'ShowWarningMessages', gShowWarningMessages);
  gIni.WriteBool('Configuration', 'DirBrackets', gDirBrackets);
  gIni.WriteInteger('Configuration', 'ShowToolTipMode', Integer(gShowToolTipMode));
  { Auto refresh page }
  gIni.WriteInteger('Configuration', 'WatchDirs', Integer(gWatchDirs));
  gIni.WriteString('Configuration', 'WatchDirsExclude', gWatchDirsExclude);
  { Icons page }
  gIni.WriteInteger('Configuration', 'ShowIcons', Integer(gShowIconsNew));
  gIni.WriteBool('Configuration', 'IconOverlays', gIconOverlays);
  gIni.WriteInteger('Configuration', 'IconsSize', gIconsSizeNew);
  { Ignore list page }
  gIni.WriteBool('Configuration', 'IgnoreListFileEnabled', gIgnoreListFileEnabled);
  gIni.WriteString('Configuration', 'IgnoreListFile', gIgnoreListFile);

  gIni.WriteBool('Configuration', 'CutTextToColWidth', gCutTextToColWidth);

  gIni.WriteBool('Viewer', 'Image.Stretch', gImageStretch);

  { Operations options }
  gIni.WriteInteger('Operations', 'Symlink', Integer(gOperationOptionSymLinks));
  gIni.WriteBool('Operations', 'CorrectLinks', gOperationOptionCorrectLinks);
  gIni.WriteInteger('Operations', 'FileExists', Integer(gOperationOptionFileExists));
  gIni.WriteInteger('Operations', 'DirectoryExists', Integer(gOperationOptionDirectoryExists));
  gIni.WriteBool('Operations', 'CheckFreeSpace', gOperationOptionCheckFreeSpace);

  gColorExt.SaveIni;

  { Search template list }
  gSearchTemplateList.SaveToIni(gIni);

  { Plugins }
  gDSXPlugins.Save(gIni);
  gWCXPlugins.Save(gIni);
  gWDXPlugins.Save(gIni);
  gWFXPlugins.Save(gIni);
  gWLXPlugins.Save(gIni);
end;

procedure LoadXmlConfig;

  procedure GetExtTool(Node: TXmlNode; var ExternalToolOptions: TExternalToolOptions);
  begin
    if Assigned(Node) then
      with ExternalToolOptions do
      begin
        Enabled          := gConfig.GetAttr(Node, 'Enabled', Enabled);
        Path             := gConfig.GetValue(Node, 'Path', Path);
        Parameters       := gConfig.GetValue(Node, 'Parameters', Parameters);
        RunInTerminal    := gConfig.GetValue(Node, 'RunInTerminal', RunInTerminal);
        KeepTerminalOpen := gConfig.GetValue(Node, 'KeepTerminalOpen', KeepTerminalOpen);
      end;
  end;
  procedure GetDCFont(Node: TXmlNode; var FontOptions: TDCFontOptions);
  begin
    if Assigned(Node) then
      gConfig.GetFont(Node, '', FontOptions.Name, FontOptions.Size, Integer(FontOptions.Style),
                                FontOptions.Name, FontOptions.Size, Integer(FontOptions.Style));
  end;
  procedure LoadOption(Node: TXmlNode; var Options: TDrivesListButtonOptions; Option: TDrivesListButtonOption; AName: String);
  var
    Value: Boolean;
  begin
    if gConfig.TryGetValue(Node, AName, Value) then
    begin
      if Value then
        Include(Options, Option)
      else
        Exclude(Options, Option);
    end;
  end;
var
  Root, Node, SubNode: TXmlNode;
  LoadedConfigVersion: Integer;
  oldQuickSearch: Boolean = True;
  oldQuickFilter: Boolean = False;
  oldQuickSearchMode: TShiftState = [ssCtrl, ssAlt];
  oldQuickFilterMode: TShiftState = [];
  KeyTypingModifier: TKeyTypingModifier;
begin
  with gConfig do
  begin
    Root := gConfig.RootNode;

    { Double Commander Version }
    gPreviousVersion:= GetAttr(Root, 'DCVersion', EmptyStr);
    LoadedConfigVersion := GetAttr(Root, 'ConfigVersion', ConfigVersion);

    { Language page }
    gPOFileName := GetValue(Root, 'Language/POFileName', gPOFileName);

    { Behaviours page }
    Node := Root.FindNode('Behaviours');
    if Assigned(Node) then
    begin
      gRunInTerm := GetValue(Node, 'RunInTerminal', gRunInTerm);
      gRunTerm := GetValue(Node, 'RunTerminal', gRunTerm);
      gOnlyOneAppInstance := GetValue(Node, 'OnlyOneAppInstance', gOnlyOneAppInstance);
      gLynxLike := GetValue(Node, 'LynxLike', gLynxLike);
      if LoadedConfigVersion < 5 then
      begin
        if GetValue(Node, 'SortCaseSensitive', False) = False then
          gSortCaseSensitivity := cstNotSensitive
        else
          gSortCaseSensitivity := cstLocale;
        gSortNatural := GetValue(Node, 'SortNatural', gSortNatural);
      end;
      gShortFileSizeFormat := GetValue(Node, 'ShortFileSizeFormat', gShortFileSizeFormat);
      gMinimizeToTray := GetValue(Node, 'MinimizeToTray', gMinimizeToTray);
      gAlwaysShowTrayIcon := GetValue(Node, 'AlwaysShowTrayIcon', gAlwaysShowTrayIcon);
      gMouseSelectionEnabled := GetAttr(Node, 'Mouse/Selection/Enabled', gMouseSelectionEnabled);
      gMouseSelectionButton := GetValue(Node, 'Mouse/Selection/Button', gMouseSelectionButton);
      gScrollMode := TScrollMode(GetValue(Node, 'Mouse/ScrollMode', Integer(gScrollMode)));
      gWheelScrollLines:= GetValue(Node, 'Mouse/WheelScrollLines', gWheelScrollLines);
      gAutoFillColumns := GetValue(Node, 'AutoFillColumns', gAutoFillColumns);
      gAutoSizeColumn := GetValue(Node, 'AutoSizeColumn', gAutoSizeColumn);
      gDateTimeFormat := GetValue(Node, 'DateTimeFormat', gDateTimeFormat);
      gCutTextToColWidth := GetValue(Node, 'CutTextToColumnWidth', gCutTextToColWidth);
      gShowSystemFiles := GetValue(Node, 'ShowSystemFiles', gShowSystemFiles);
      {$IFNDEF LCLCARBON}
      // Under Mac OS X loading file list in separate thread are very very slow
      // so disable and hide this option under Mac OS X Carbon
      gListFilesInThread := GetValue(Node, 'ListFilesInThread', gListFilesInThread);
      {$ENDIF}
      gLoadIconsSeparately := GetValue(Node, 'LoadIconsSeparately', gLoadIconsSeparately);
      gDelayLoadingTabs := GetValue(Node, 'DelayLoadingTabs', gDelayLoadingTabs);
      gHighlightUpdatedFiles := GetValue(Node, 'HighlightUpdatedFiles', gHighlightUpdatedFiles);
      gDriveBlackList := GetValue(Node, 'DriveBlackList', gDriveBlackList);
      gDriveBlackListUnmounted := GetValue(Node, 'DriveBlackListUnmounted', gDriveBlackListUnmounted);
    end;

    { Tools page }
    GetExtTool(gConfig.FindNode(Root, 'Tools/Viewer'), gExternalTools[etViewer]);
    GetExtTool(gConfig.FindNode(Root, 'Tools/Editor'), gExternalTools[etEditor]);
    GetExtTool(gConfig.FindNode(Root, 'Tools/Differ'), gExternalTools[etDiffer]);

    { Fonts page }
    GetDCFont(gConfig.FindNode(Root, 'Fonts/Main'), gFonts[dcfMain]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/Editor'), gFonts[dcfEditor]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/Viewer'), gFonts[dcfViewer]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/Log'), gFonts[dcfLog]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/ViewerBook'), gFonts[dcfViewerBook]);

    { Colors page }
    Node := Root.FindNode('Colors');
    if Assigned(Node) then
    begin
      gForeColor := GetValue(Node, 'Foreground', gForeColor);
      gBackColor := GetValue(Node, 'Background', gBackColor);
      gBackColor2 := GetValue(Node, 'Background2', gBackColor2);
      gMarkColor := GetValue(Node, 'Mark', gMarkColor);
      gCursorColor := GetValue(Node, 'Cursor', gCursorColor);
      gCursorText := GetValue(Node, 'CursorText', gCursorText);
      gUseInvertedSelection := GetValue(Node, 'UseInvertedSelection', gUseInvertedSelection);
      gInactivePanelBrightness := GetValue(Node, 'InactivePanelBrightness', gInactivePanelBrightness);
      gUseFrameCursor:= GetValue(Node,'UseFrameCursor', gUseFrameCursor);
      gIndForeColor := GetValue(Node, 'FreeSpaceIndicator/ForeColor', gIndForeColor);
      gIndBackColor := GetValue(Node, 'FreeSpaceIndicator/BackColor', gIndBackColor);
      gIndUseGradient := GetValue(Node, 'FreeSpaceIndicator/UseGradient', gIndUseGradient);

      gColorExt.Load(gConfig, Node);
    end;

    { ToolTips page }
    Node := Root.FindNode('ToolTips');
    if Assigned(Node) then
    begin
      gShowToolTipMode := TShowToolTipMode(GetValue(Node, 'ShowToolTipMode', Integer(gShowToolTipMode)));
      gFileInfoToolTip.Load(gConfig, Node);
    end;

    { Layout page }
    Node := Root.FindNode('Layout');
    if Assigned(Node) then
    begin
      gMainMenu := GetValue(Node, 'MainMenu', gMainMenu);
      SubNode := Node.FindNode('ButtonBar');
      if Assigned(SubNode) then
      begin
        gButtonBar := GetAttr(SubNode, 'Enabled', gButtonBar);
        gToolBarFlat := GetValue(SubNode, 'FlatIcons', gToolBarFlat);
        gToolBarButtonSize := GetValue(SubNode, 'ButtonHeight', gToolBarButtonSize);
        if LoadedConfigVersion <= 1 then
          gToolBarIconSize := GetValue(SubNode, 'SmallIconSize', gToolBarIconSize)
        else
          gToolBarIconSize := GetValue(SubNode, 'IconSize', gToolBarIconSize);
      end;
      gDriveBar1 := GetValue(Node, 'DriveBar1', gDriveBar1);
      gDriveBar2 := GetValue(Node, 'DriveBar2', gDriveBar2);
      gDriveBarFlat := GetValue(Node, 'DriveBarFlat', gDriveBarFlat);
      if LoadedConfigVersion < 3 then
        gDrivesListButton := GetValue(Node, 'DriveMenuButton', gDrivesListButton)
      else
      begin
        SubNode := Node.FindNode('DrivesListButton');
        if Assigned(SubNode) then
        begin
          gDrivesListButton := GetAttr(SubNode, 'Enabled', gDrivesListButton);
          LoadOption(SubNode, gDrivesListButtonOptions, dlbShowLabel, 'ShowLabel');
          LoadOption(SubNode, gDrivesListButtonOptions, dlbShowFileSystem, 'ShowFileSystem');
          LoadOption(SubNode, gDrivesListButtonOptions, dlbShowFreeSpace, 'ShowFreeSpace');
        end;
      end;
      gDirectoryTabs := GetValue(Node, 'DirectoryTabs', gDirectoryTabs);
      gCurDir := GetValue(Node, 'CurrentDirectory', gCurDir);
      gTabHeader := GetValue(Node, 'TabHeader', gTabHeader);
      gStatusBar := GetValue(Node, 'StatusBar', gStatusBar);
      gCmdLine := GetValue(Node, 'CmdLine', gCmdLine);
      gLogWindow := GetValue(Node, 'LogWindow', gLogWindow);
      gTermWindow := GetValue(Node, 'TermWindow', gTermWindow);
      gKeyButtons := GetValue(Node, 'KeyButtons', gKeyButtons);
      gInterfaceFlat := GetValue(Node, 'InterfaceFlat', gInterfaceFlat);
      gDriveFreeSpace := GetValue(Node, 'DriveFreeSpace', gDriveFreeSpace);
      gDriveInd := GetValue(Node, 'DriveIndicator', gDriveInd);
      gProgInMenuBar := GetValue(Node, 'ProgressInMenuBar', gProgInMenuBar);
      gPanelOfOp := GetValue(Node, 'PanelOfOperationsInBackground', gPanelOfOp);
      gHorizontalFilePanels := GetValue(Node, 'HorizontalFilePanels', gHorizontalFilePanels);
      gShortFormatDriveInfo := GetValue(Node, 'ShortFormatDriveInfo', gShortFormatDriveInfo);
    end;

    { Files views }
    Node := Root.FindNode('FilesViews');
    if Assigned(Node) then
    begin
      SubNode := Node.FindNode('Sorting');
      if Assigned(SubNode) then
      begin
        gSortCaseSensitivity := TCaseSensitivity(GetValue(SubNode, 'CaseSensitivity', Integer(gSortCaseSensitivity)));
        gSortNatural := GetValue(SubNode, 'NaturalSorting', gSortNatural);
        gSortFolderMode:= TSortFolderMode(GetValue(SubNode, 'SortFolderMode', Integer(gSortFolderMode)));
        gNewFilesPosition := TNewFilesPosition(GetValue(SubNode, 'NewFilesPosition', Integer(gNewFilesPosition)));
        gUpdatedFilesPosition := TUpdatedFilesPosition(GetValue(SubNode, 'UpdatedFilesPosition', Integer(gUpdatedFilesPosition)));
      end;
    end;

    { Keys page }
    Node := Root.FindNode('Keyboard');
    if Assigned(Node) then
    begin
      SubNode := FindNode(Node, 'Typing/Actions');
      if Assigned(SubNode) then
      begin
        for KeyTypingModifier in TKeyTypingModifier do
          gKeyTyping[KeyTypingModifier] := TKeyTypingAction(GetValue(SubNode,
            TKeyTypingModifierToNodeName[KeyTypingModifier], Integer(gKeyTyping[KeyTypingModifier])));
      end;
    end;

    { File operations page }
    Node := Root.FindNode('FileOperations');
    if Assigned(Node) then
    begin
      gCopyBlockSize := GetValue(Node, 'BufferSize', gCopyBlockSize);
      gUseMmapInSearch := GetValue(Node, 'UseMmapInSearch', gUseMmapInSearch);
      gPartialNameSearch := GetValue(Node, 'PartialNameSearch', gPartialNameSearch);
      gWipePassNumber := GetValue(Node, 'WipePassNumber', gWipePassNumber);
      gDropReadOnlyFlag := GetValue(Node, 'DropReadOnlyFlag', gDropReadOnlyFlag);
      gProcessComments := GetValue(Node, 'ProcessComments', gProcessComments);
      gRenameSelOnlyName := GetValue(Node, 'RenameSelOnlyName', gRenameSelOnlyName);
      gShowCopyTabSelectPanel := GetValue(Node, 'ShowCopyTabSelectPanel', gShowCopyTabSelectPanel);
      gUseTrash := GetValue(Node, 'UseTrash', gUseTrash);
      gSkipFileOpError := GetValue(Node, 'SkipFileOpError', gSkipFileOpError);
      gShowDialogOnDragDrop := GetValue(Node, 'ShowDialogOnDragDrop', gShowDialogOnDragDrop);
      gOverwriteFolder := GetValue(Node, 'OverwriteFolder', gOverwriteFolder);
      gFileOperationsProgressKind := TFileOperationsProgressKind(GetValue(Node, 'ProgressKind', Integer(gFileOperationsProgressKind)));
      // Operations options
      SubNode := Node.FindNode('Options');
      if Assigned(SubNode) then
      begin
        gOperationOptionSymLinks := TFileSourceOperationOptionSymLink(GetValue(SubNode, 'Symlink', Integer(gOperationOptionSymLinks)));
        gOperationOptionCorrectLinks := GetValue(SubNode, 'CorrectLinks', gOperationOptionCorrectLinks);
        gOperationOptionFileExists := TFileSourceOperationOptionFileExists(GetValue(SubNode, 'FileExists', Integer(gOperationOptionFileExists)));
        gOperationOptionDirectoryExists := TFileSourceOperationOptionDirectoryExists(GetValue(SubNode, 'DirectoryExists', Integer(gOperationOptionDirectoryExists)));
        gOperationOptionSetPropertyError := TFileSourceOperationOptionSetPropertyError(GetValue(SubNode, 'SetPropertyError', Integer(gOperationOptionSetPropertyError)));
        gOperationOptionCheckFreeSpace := GetValue(SubNode, 'CheckFreeSpace', gOperationOptionCheckFreeSpace);
        gOperationOptionCopyAttributes := GetValue(SubNode, 'CopyAttributes', gOperationOptionCopyAttributes);
        gOperationOptionCopyTime := GetValue(SubNode, 'CopyTime', gOperationOptionCopyTime);
        gOperationOptionCopyOwnership := GetValue(SubNode, 'CopyOwnership', gOperationOptionCopyOwnership);
      end;
    end;

    { Tabs page }
    Node := Root.FindNode('Tabs');
    if Assigned(Node) then
    begin
      gDirTabOptions := TTabsOptions(GetValue(Node, 'Options', Integer(gDirTabOptions)));
      gDirTabLimit := GetValue(Node, 'CharacterLimit', gDirTabLimit);
      gDirTabPosition := TTabsPosition(GetValue(Node, 'Position', Integer(gDirTabPosition)));
    end;

    { Log page }
    Node := Root.FindNode('Log');
    if Assigned(Node) then
    begin
      gLogFile := GetAttr(Node, 'Enabled', gLogFile);
      gLogFileName := GetValue(Node, 'FileName', gLogFileName);
      gLogOptions := TLogOptions(GetValue(Node, 'Options', Integer(gLogOptions)));
    end;

    { Configuration page }
    gSaveConfiguration := GetAttr(Root, 'Configuration/Save', gSaveConfiguration);
    gSaveSearchReplaceHistory:= GetAttr(Root, 'History/SearchReplaceHistory/Save', gSaveSearchReplaceHistory);
    gSaveDirHistory := GetAttr(Root, 'History/DirHistory/Save', gSaveDirHistory);
    gSaveCmdLineHistory := GetAttr(Root, 'History/CmdLineHistory/Save', gSaveCmdLineHistory);
    gSaveFileMaskHistory := GetAttr(Root, 'History/FileMaskHistory/Save', gSaveFileMaskHistory);

    { Quick Search/Filter page }
    Node := Root.FindNode('QuickSearch');
    if Assigned(Node) then
    begin
      if LoadedConfigVersion < 4 then
      begin
        oldQuickSearch := GetAttr(Node, 'Enabled', oldQuickSearch);
        oldQuickSearchMode := TShiftState(GetValue(Node, 'Mode', Integer(oldQuickSearchMode)));
        OldKeysToNew(oldQuickSearch, oldQuickSearchMode, ktaQuickSearch);
      end;
      if GetValue(Node, 'MatchBeginning', qsmBeginning in gQuickSearchOptions.Match) then
        Include(gQuickSearchOptions.Match, qsmBeginning)
      else
        Exclude(gQuickSearchOptions.Match, qsmBeginning);
      if GetValue(Node, 'MatchEnding', qsmEnding in gQuickSearchOptions.Match) then
        Include(gQuickSearchOptions.Match, qsmEnding)
      else
        Exclude(gQuickSearchOptions.Match, qsmEnding);
      gQuickSearchOptions.SearchCase := TQuickSearchCase(GetValue(Node, 'Case', Integer(gQuickSearchOptions.SearchCase)));
      gQuickSearchOptions.Items := TQuickSearchItems(GetValue(Node, 'Items', Integer(gQuickSearchOptions.Items)));
    end;
    Node := Root.FindNode('QuickFilter');
    if Assigned(Node) then
    begin
      if LoadedConfigVersion < 4 then
      begin
        oldQuickFilter := GetAttr(Node, 'Enabled', oldQuickFilter);
        oldQuickFilterMode := TShiftState(GetValue(Node, 'Mode', Integer(oldQuickFilterMode)));
        OldKeysToNew(oldQuickFilter, oldQuickFilterMode, ktaQuickFilter);
      end;
      gQuickFilterAutoHide := GetValue(Node, 'AutoHide', gQuickFilterAutoHide);
    end;

    { Miscellaneous page }
    Node := Root.FindNode('Miscellaneous');
    if Assigned(Node) then
    begin
      gGridVertLine := GetValue(Node, 'GridVertLine', gGridVertLine);
      gGridHorzLine := GetValue(Node, 'GridHorzLine', gGridHorzLine);
      gShowWarningMessages := GetValue(Node, 'ShowWarningMessages', gShowWarningMessages);
      gSpaceMovesDown := GetValue(Node, 'SpaceMovesDown', gSpaceMovesDown);
      gDirBrackets := GetValue(Node, 'DirBrackets', gDirBrackets);
    end;

    { Auto refresh page }
    Node := Root.FindNode('AutoRefresh');
    if Assigned(Node) then
    begin
      gWatchDirs := TWatchOptions(GetValue(Node, 'Options', Integer(gWatchDirs)));
      gWatchDirsExclude := GetValue(Node, 'ExcludeDirs', gWatchDirsExclude);
      gWatcherMode := TWatcherMode(GetValue(Node, 'Mode', Integer(gWatcherMode)));
    end;

    { Icons page }
    Node := Root.FindNode('Icons');
    if Assigned(Node) then
    begin
      gShowIcons := TShowIconsMode(GetValue(Node, 'ShowMode', Integer(gShowIcons)));
      gIconOverlays := GetValue(Node, 'ShowOverlays', gIconOverlays);
      gIconsSize := GetValue(Node, 'Size', gIconsSize);
      gCustomDriveIcons := GetValue(Node, 'CustomDriveIcons', gCustomDriveIcons);
    end;

    { Ignore list page }
    Node := Root.FindNode('IgnoreList');
    if Assigned(Node) then
    begin
      gIgnoreListFileEnabled:= GetAttr(Node, 'Enabled', gIgnoreListFileEnabled);
      gIgnoreListFile:= GetValue(Node, 'IgnoreListFile', gIgnoreListFile);
    end;

    { Directories HotList }
    LoadDirHotList(gConfig, Root);

    {Viewer}
    Node := Root.FindNode('Viewer');
    if Assigned(Node) then
    begin
      gImageStretch := GetValue(Node, 'ImageStretch', gImageStretch);
      gPreviewVisible := GetValue(Node, 'PreviewVisible', gPreviewVisible);
      gSaveThumb := GetValue(Node, 'SaveThumbnails', gSaveThumb);
      gCopyMovePath1 := GetValue(Node, 'CopyMovePath1', gCopyMovePath1);
      gCopyMovePath2 := GetValue(Node, 'CopyMovePath2', gCopyMovePath2);
      gCopyMovePath3 := GetValue(Node, 'CopyMovePath3', gCopyMovePath3);
      gCopyMovePath4 := GetValue(Node, 'CopyMovePath4', gCopyMovePath4);
      gCopyMovePath5 := GetValue(Node, 'CopyMovePath5', gCopyMovePath5);
      gImagePaintMode := GetValue(Node, 'PaintMode', gImagePaintMode);
      gImagePaintWidth := GetValue(Node, 'PaintWidth', gImagePaintWidth);
      gColCount := GetValue(Node, 'NumberOfColumns', gColCount);
      gViewerMode := GetValue(Node, 'ViewerMode', gViewerMode);
      gImagePaintColor := GetValue(Node, 'PaintColor', gImagePaintColor);
      gBookBackgroundColor := GetValue(Node, 'BackgroundColor', gBookBackgroundColor);
      gBookFontColor := GetValue(Node, 'FontColor', gBookFontColor);
      gTextPosition := GetValue(Node, 'TextPosition',  gTextPosition);
    end;
    { - Other - }
    gLuaLib := GetValue(Root, 'Lua/PathToLibrary', gLuaLib);
    gNameSCFile:= GetValue(Root, 'NameShortcutFile', gNameSCFile);
    gLastUsedPacker:= GetValue(Root, 'LastUsedPacker', gLastUsedPacker);
    gUseShellForFileOperations:= GetValue(Root, 'UseShellForFileOperations', gUseShellForFileOperations);
  end;

  { Search template list }
  gSearchTemplateList.LoadFromXml(gConfig, Root);

  { Columns sets }
  ColSet.Load(gConfig, Root);

  { Plugins }
  Node := gConfig.FindNode(Root, 'Plugins');
  if Assigned(Node) then
  begin
    gDSXPlugins.Load(gConfig, Node);
    gWCXPlugins.Load(gConfig, Node);
    gWDXPlugins.Load(gConfig, Node);
    gWFXPlugins.Load(gConfig, Node);
    gWLXPlugins.Load(gConfig, Node);
  end;
end;

procedure SaveXmlConfig;

  procedure SetExtTool(Node: TXmlNode; const ExternalToolOptions: TExternalToolOptions);
  begin
    if Assigned(Node) then
      with ExternalToolOptions do
      begin
        gConfig.SetAttr(Node, 'Enabled', Enabled);
        gConfig.SetValue(Node, 'Path', Path);
        gConfig.SetValue(Node, 'Parameters', Parameters);
        gConfig.SetValue(Node, 'RunInTerminal', RunInTerminal);
        gConfig.SetValue(Node, 'KeepTerminalOpen', KeepTerminalOpen);
      end;
  end;
  procedure SetDCFont(Node: TXmlNode; const FontOptions: TDCFontOptions);
  begin
    if Assigned(Node) then
      gConfig.SetFont(Node, '', FontOptions.Name, FontOptions.Size, Integer(FontOptions.Style));
  end;
var
  I: Integer;
  Root, Node, SubNode: TXmlNode;
  KeyTypingModifier: TKeyTypingModifier;
begin
  with gConfig do
  begin
    Root := gConfig.RootNode;

    SetAttr(Root, 'DCVersion', dcVersion);
    SetAttr(Root, 'ConfigVersion', ConfigVersion);

    SetValue(Root, 'Configuration/UseConfigInProgramDir', gUseConfigInProgramDirNew);

    { Language page }
    SetValue(Root, 'Language/POFileName', gPOFileName);

    { Behaviours page }
    Node := FindNode(Root, 'Behaviours', True);
    ClearNode(Node);
    SetValue(Node, 'RunInTerminal', gRunInTerm);
    SetValue(Node, 'RunTerminal', gRunTerm);
    SetValue(Node, 'OnlyOneAppInstance', gOnlyOneAppInstance);
    SetValue(Node, 'LynxLike', gLynxLike);
    SetValue(Node, 'ShortFileSizeFormat', gShortFileSizeFormat);
    SetValue(Node, 'MinimizeToTray', gMinimizeToTray);
    SetValue(Node, 'AlwaysShowTrayIcon', gAlwaysShowTrayIcon);
    SubNode := FindNode(Node, 'Mouse', True);
    SetAttr(SubNode, 'Selection/Enabled', gMouseSelectionEnabled);
    SetValue(SubNode, 'Selection/Button', gMouseSelectionButton);
    SetValue(SubNode, 'ScrollMode', Integer(gScrollMode));
    SetValue(SubNode, 'WheelScrollLines', gWheelScrollLines);
    SetValue(Node, 'AutoFillColumns', gAutoFillColumns);
    SetValue(Node, 'AutoSizeColumn', gAutoSizeColumn);
    SetValue(Node, 'DateTimeFormat', gDateTimeFormat);
    SetValue(Node, 'CutTextToColumnWidth', gCutTextToColWidth);
    SetValue(Node, 'ShowSystemFiles', gShowSystemFiles);
    {$IFNDEF LCLCARBON}
    // Under Mac OS X loading file list in separate thread are very very slow
    // so disable and hide this option under Mac OS X Carbon
    SetValue(Node, 'ListFilesInThread', gListFilesInThread);
    {$ENDIF}
    SetValue(Node, 'LoadIconsSeparately', gLoadIconsSeparately);
    SetValue(Node, 'DelayLoadingTabs', gDelayLoadingTabs);
    SetValue(Node, 'HighlightUpdatedFiles', gHighlightUpdatedFiles);
    SetValue(Node, 'DriveBlackList', gDriveBlackList);
    SetValue(Node, 'DriveBlackListUnmounted', gDriveBlackListUnmounted);

    { Tools page }
    SetExtTool(gConfig.FindNode(Root, 'Tools/Viewer', True), gExternalTools[etViewer]);
    SetExtTool(gConfig.FindNode(Root, 'Tools/Editor', True), gExternalTools[etEditor]);
    SetExtTool(gConfig.FindNode(Root, 'Tools/Differ', True), gExternalTools[etDiffer]);

    { Fonts page }
    SetDCFont(gConfig.FindNode(Root, 'Fonts/Main', True), gFonts[dcfMain]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/Editor', True), gFonts[dcfEditor]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/Viewer', True), gFonts[dcfViewer]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/Log', True), gFonts[dcfLog]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/ViewerBook', True), gFonts[dcfViewerBook]);

    { Colors page }
    Node := FindNode(Root, 'Colors', True);
    SetValue(Node, 'Foreground', gForeColor);
    SetValue(Node, 'Background', gBackColor);
    SetValue(Node, 'Background2', gBackColor2);
    SetValue(Node, 'Mark', gMarkColor);
    SetValue(Node, 'Cursor', gCursorColor);
    SetValue(Node, 'CursorText', gCursorText);
    SetValue(Node, 'UseInvertedSelection', gUseInvertedSelection);
    SetValue(Node, 'InactivePanelBrightness', gInactivePanelBrightness);
    SetValue(Node, 'UseFrameCursor', gUseFrameCursor);
    SetValue(Node, 'FreeSpaceIndicator/ForeColor', gIndForeColor);
    SetValue(Node, 'FreeSpaceIndicator/BackColor', gIndBackColor);
    SetValue(Node, 'FreeSpaceIndicator/UseGradient', gIndUseGradient);
    gColorExt.Save(gConfig, Node);

    { ToolTips page }
    Node := FindNode(Root, 'ToolTips', True);
    SetValue(Node, 'ShowToolTipMode', Integer(gShowToolTipMode));
    gFileInfoToolTip.Save(gConfig, Node);

    { Layout page }
    Node := FindNode(Root, 'Layout', True);
    ClearNode(Node);
    SetValue(Node, 'MainMenu', gMainMenu);
    SubNode := FindNode(Node, 'ButtonBar', True);
    SetAttr(SubNode, 'Enabled', gButtonBar);
    SetValue(SubNode, 'FlatIcons', gToolBarFlat);
    SetValue(SubNode, 'ButtonHeight', gToolBarButtonSize);
    SetValue(SubNode, 'IconSize', gToolBarIconSize);
    SetValue(Node, 'DriveBar1', gDriveBar1);
    SetValue(Node, 'DriveBar2', gDriveBar2);
    SetValue(Node, 'DriveBarFlat', gDriveBarFlat);
    SubNode := FindNode(Node, 'DrivesListButton', True);
    SetAttr(SubNode, 'Enabled', gDrivesListButton);
    SetValue(SubNode, 'ShowLabel', dlbShowLabel in gDrivesListButtonOptions);
    SetValue(SubNode, 'ShowFileSystem', dlbShowFileSystem in gDrivesListButtonOptions);
    SetValue(SubNode, 'ShowFreeSpace', dlbShowFreeSpace in gDrivesListButtonOptions);
    SetValue(Node, 'DirectoryTabs', gDirectoryTabs);
    SetValue(Node, 'CurrentDirectory', gCurDir);
    SetValue(Node, 'TabHeader', gTabHeader);
    SetValue(Node, 'StatusBar', gStatusBar);
    SetValue(Node, 'CmdLine', gCmdLine);
    SetValue(Node, 'LogWindow', gLogWindow);
    SetValue(Node, 'TermWindow', gTermWindow);
    SetValue(Node, 'KeyButtons', gKeyButtons);
    SetValue(Node, 'InterfaceFlat', gInterfaceFlat);
    SetValue(Node, 'DriveFreeSpace', gDriveFreeSpace);
    SetValue(Node, 'DriveIndicator', gDriveInd);
    SetValue(Node, 'ProgressInMenuBar', gProgInMenuBar);
    SetValue(Node, 'PanelOfOperationsInBackground', gPanelOfOp);
    SetValue(Node, 'HorizontalFilePanels', gHorizontalFilePanels);
    SetValue(Node, 'ShortFormatDriveInfo', gShortFormatDriveInfo);

    { Files views }
    Node := FindNode(Root, 'FilesViews', True);
    SubNode := FindNode(Node, 'Sorting', True);
    SetValue(SubNode, 'CaseSensitivity', Integer(gSortCaseSensitivity));
    SetValue(SubNode, 'NaturalSorting', gSortNatural);
    SetValue(SubNode, 'SortFolderMode', Integer(gSortFolderMode));
    SetValue(SubNode, 'NewFilesPosition', Integer(gNewFilesPosition));
    SetValue(SubNode, 'UpdatedFilesPosition', Integer(gUpdatedFilesPosition));

    { Keys page }
    Node := FindNode(Root, 'Keyboard', True);
    SubNode := FindNode(Node, 'Typing/Actions', True);
    for KeyTypingModifier in TKeyTypingModifier do
      SetValue(SubNode, TKeyTypingModifierToNodeName[KeyTypingModifier],
        Integer(gKeyTyping[KeyTypingModifier]));

    { File operations page }
    Node := FindNode(Root, 'FileOperations', True);
    SetValue(Node, 'BufferSize', gCopyBlockSize);
    SetValue(Node, 'UseMmapInSearch', gUseMmapInSearch);
    SetValue(Node, 'PartialNameSearch', gPartialNameSearch);
    SetValue(Node, 'WipePassNumber', gWipePassNumber);
    SetValue(Node, 'DropReadOnlyFlag', gDropReadOnlyFlag);
    SetValue(Node, 'ProcessComments', gProcessComments);
    SetValue(Node, 'RenameSelOnlyName', gRenameSelOnlyName);
    SetValue(Node, 'ShowCopyTabSelectPanel', gShowCopyTabSelectPanel);
    SetValue(Node, 'UseTrash', gUseTrash);
    SetValue(Node, 'SkipFileOpError', gSkipFileOpError);
    SetValue(Node, 'ShowDialogOnDragDrop', gShowDialogOnDragDrop);
    SetValue(Node, 'OverwriteFolder', gOverwriteFolder);
    SetValue(Node, 'ProgressKind', Integer(gFileOperationsProgressKind));
    // Operations options
    SubNode := FindNode(Node, 'Options', True);
    SetValue(SubNode, 'Symlink', Integer(gOperationOptionSymLinks));
    SetValue(SubNode, 'CorrectLinks', gOperationOptionCorrectLinks);
    SetValue(SubNode, 'FileExists', Integer(gOperationOptionFileExists));
    SetValue(SubNode, 'DirectoryExists', Integer(gOperationOptionDirectoryExists));
    SetValue(SubNode, 'SetPropertyError', Integer(gOperationOptionSetPropertyError));
    SetValue(SubNode, 'CheckFreeSpace', gOperationOptionCheckFreeSpace);
    SetValue(SubNode, 'CopyAttributes', gOperationOptionCopyAttributes);
    SetValue(SubNode, 'CopyTime', gOperationOptionCopyTime);
    SetValue(SubNode, 'CopyOwnership', gOperationOptionCopyOwnership);

    { Tabs page }
    Node := FindNode(Root, 'Tabs', True);
    SetValue(Node, 'Options', Integer(gDirTabOptions));
    SetValue(Node, 'CharacterLimit', gDirTabLimit);
    SetValue(Node, 'Position', Integer(gDirTabPosition));

    { Log page }
    Node := FindNode(Root, 'Log', True);
    SetAttr(Node, 'Enabled', gLogFile);
    SetValue(Node, 'FileName', gLogFileName);
    SetValue(Node, 'Options', Integer(gLogOptions));

    { Configuration page }
    SetAttr(Root, 'Configuration/Save', gSaveConfiguration);
    SetAttr(Root, 'History/SearchReplaceHistory/Save', gSaveSearchReplaceHistory);
    SetAttr(Root, 'History/DirHistory/Save', gSaveDirHistory);
    SetAttr(Root, 'History/CmdLineHistory/Save', gSaveCmdLineHistory);
    SetAttr(Root, 'History/FileMaskHistory/Save', gSaveFileMaskHistory);

    { Quick Search/Filter page }
    Node := FindNode(Root, 'QuickSearch', True);
    SetValue(Node, 'MatchBeginning', qsmBeginning in gQuickSearchOptions.Match);
    SetValue(Node, 'MatchEnding', qsmEnding in gQuickSearchOptions.Match);
    SetValue(Node, 'Case', Integer(gQuickSearchOptions.SearchCase));
    SetValue(Node, 'Items', Integer(gQuickSearchOptions.Items));
    Node := FindNode(Root, 'QuickFilter', True);
    SetValue(Node, 'AutoHide', gQuickFilterAutoHide);

    { Misc page }
    Node := FindNode(Root, 'Miscellaneous', True);
    SetValue(Node, 'GridVertLine', gGridVertLine);
    SetValue(Node, 'GridHorzLine', gGridHorzLine);
    SetValue(Node, 'ShowWarningMessages', gShowWarningMessages);
    SetValue(Node, 'SpaceMovesDown', gSpaceMovesDown);
    SetValue(Node, 'DirBrackets', gDirBrackets);

    { Auto refresh page }
    Node := FindNode(Root, 'AutoRefresh', True);
    SetValue(Node, 'Options', Integer(gWatchDirs));
    SetValue(Node, 'ExcludeDirs', gWatchDirsExclude);
    SetValue(Node, 'Mode', Integer(gWatcherMode));

    { Icons page }
    Node := FindNode(Root, 'Icons', True);
    SetValue(Node, 'ShowMode', Integer(gShowIconsNew));
    SetValue(Node, 'ShowOverlays', gIconOverlays);
    SetValue(Node, 'Size', gIconsSizeNew);
    SetValue(Node, 'CustomDriveIcons', gCustomDriveIcons);

    { Ignore list page }
    Node := FindNode(Root, 'IgnoreList', True);
    SetAttr(Node, 'Enabled', gIgnoreListFileEnabled);
    SetValue(Node, 'IgnoreListFile', gIgnoreListFile);

    { Directories HotList }
    Node := FindNode(Root, 'DirectoryHotList', True);
    gConfig.ClearNode(Node);
    for I:= 0 to glsHotDir.Count - 1 do
    begin
      SubNode := AddNode(Node, 'HotDir');
      SetAttr(SubNode, 'Name', glsHotDir.Names[I]);
      SetAttr(SubNode, 'Path', glsHotDir.ValueFromIndex[I]);
    end;

    {Viewer}
    Node := FindNode(Root, 'Viewer',True);
    SetValue(Node, 'PreviewVisible',gPreviewVisible);
    SetValue(Node, 'SaveThumbnails', gSaveThumb);
    SetValue(Node, 'ImageStretch',gImageStretch);
    SetValue(Node, 'CopyMovePath1', gCopyMovePath1);
    SetValue(Node, 'CopyMovePath2', gCopyMovePath2);
    SetValue(Node, 'CopyMovePath3', gCopyMovePath3);
    SetValue(Node, 'CopyMovePath4', gCopyMovePath4);
    SetValue(Node, 'CopyMovePath5', gCopyMovePath5);
    SetValue(Node, 'PaintMode', gImagePaintMode);
    SetValue(Node, 'PaintWidth', gImagePaintWidth);
    SetValue(Node, 'NumberOfColumns', gColCount);
    SetValue(Node, 'ViewerMode', gViewerMode);
    SetValue(Node, 'PaintColor', gImagePaintColor);
    SetValue(Node, 'BackgroundColor', gBookBackgroundColor);
    SetValue(Node, 'FontColor', gBookFontColor);
    SetValue(Node, 'TextPosition', gTextPosition);

    { - Other - }
    SetValue(Root, 'Lua/PathToLibrary', gLuaLib);
    SetValue(Root, 'NameShortcutFile', gNameSCFile);
    SetValue(Root, 'LastUsedPacker', gLastUsedPacker);
    SetValue(Root, 'UseShellForFileOperations', gUseShellForFileOperations);
  end;

  { Search template list }
  gSearchTemplateList.SaveToXml(gConfig, Root);

  { Columns sets }
  ColSet.Save(gConfig, Root);

  { Plugins }
  Node := gConfig.FindNode(Root, 'Plugins', True);
  gDSXPlugins.Save(gConfig, Node);
  gWCXPlugins.Save(gConfig, Node);
  gWDXPlugins.Save(gConfig, Node);
  gWFXPlugins.Save(gConfig, Node);
  gWLXPlugins.Save(gConfig, Node);
end;

function InitGlobs: Boolean;
begin
  CreateGlobs;
  if not OpenConfig then
    Exit(False);
  SetDefaultNonConfigGlobs;
  if not LoadGlobs then
    Exit(False);

  // If a new config was created and the file doesn't yet exist then save it.
  if not mbFileExists(gpCfgDir + 'doublecmd.xml') then
  begin
    SaveXmlConfig;
    gConfig.Save;
  end;

  Result := True;
end;

function GetKeyTypingAction(ShiftStateEx: TShiftState): TKeyTypingAction;
var
  Modifier: TKeyTypingModifier;
begin
  for Modifier in TKeyTypingModifier do
    if ShiftStateEx * KeyModifiersShortcutNoText = TKeyTypingModifierToShift[Modifier] then
      Exit(gKeyTyping[Modifier]);
  Result := ktaNone;
end;

function IsFileSystemWatcher: Boolean;
begin
  Result := ([watch_file_name_change, watch_attributes_change] * gWatchDirs <> []);
end;

initialization

finalization
  DestroyGlobs;
end.
