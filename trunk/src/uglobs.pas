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

interface
uses
  Classes, Controls, Forms, uExts, uColorExt, Graphics, uClassesEx,
  uColumns, uhotkeymanger, uActs, uSearchTemplate, uFileSourceOperationOptions,
  uWFXModule, uWCXModule, uWDXModule, uwlxmodule, udsxmodule, uXmlConfig;

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

const
  { Default hotkey list version number }
  hkVersion: String = '0.4.6.r2345';
  ConfigVersion = '1';

var
  { Double Commander Version }
  dcVersion: String;

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

  { Columns Set }
  ColSet:TPanelColumnsList;
  
  { Layout page }
  gButtonBar,
  gToolBarFlat,
  gDriveBar1,
  gDriveBar2,
  gDriveBarFlat,
  gDriveMenuButton,
  gDirectoryTabs,
  gCurDir,
  gTabHeader,
  gStatusBar,
  gCmdLine,
  gLogWindow,
  gTermWindow,
  gKeyButtons,
  gInterfaceFlat : Boolean;

  { Toolbar }
  gToolBarButtonSize,
  gToolBarIconSize: Integer;
  gToolBarSmallIcons: Boolean;

  gDirSortFirst:Boolean=True; // want to show dir first in panels
  gDirHistoryCount:Integer=30; // how many history we remember
  gShowSystemFiles:Boolean=True;
  gRunInTerm: String;
  gRunTerm: String;
  gCaseSensitiveSort:Boolean=True;
  gLynxLike:Boolean=True;
  gFirstTextSearch: Boolean = True;

  gMouseSelectionEnabled: Boolean = True;
  gMouseSelectionButton: Integer = 0;

  gAutoFillColumns: Boolean;
  gAutoSizeColumn: Integer;
  
  glsHotDir:TStringListEx;
  glsDirHistory:TStringListEx;
  glsMaskHistory : TStringListEx;
  glsSearchHistory : TStringListEx;
  glsReplaceHistory : TStringListEx;
  glsIgnoreList : TStringListEx;
  gOnlyOnce,
  gCutTextToColWidth : Boolean;
  gSpaceMovesDown: Boolean;
  gScrollMode: Integer;
  gAlwaysShowTrayIcon: Boolean;
  gMinimizeToTray: Boolean;
  gShortFileSizeFormat:Boolean=True;

  gDateTimeFormat : String;
  
  gDriveBlackList: String;

  gListFilesInThread: Boolean = False;
  gLoadIconsSeparately: Boolean = False;

  
  { Tools page }

  gUseExtEdit:Boolean=False;
  gUseExtView:Boolean=False;
  gUseExtDiff:Boolean=False;

  gExtEdit:String='emacs';
  gExtView:String='emacs';
  gExtDiff:String='gtk-diff';
  gLuaLib:String='/usr/lib/liblua5.1.so';
  gExts:TExts;
  gColorExt:TColorExt;

  gFontName: String;
  gFontSize: Integer;
  gFontStyle: TFontStyles;
  gEditorFontName: String;
  gEditorFontSize: Integer;
  gEditorFontStyle: TFontStyles;
  gViewerFontName: String;
  gViewerFontSize: Integer;
  gViewerFontStyle: TFontStyles;
  gViewerImageStretch: Boolean;
  
  { File panels color page }
  
  gBackColor, //Background color
  gBackColor2, //Background color 2
  gForeColor,  //text color
  gMarkColor,  // Mark color
  gCursorColor, //Cursor color
  gCursorText : TColor; //text color under cursor
  gUseInvertedSelection: Boolean = False;
  gInactivePanelBrightness: Integer = 100; // 0 - black, 100 - full color
  
  gShowIcons: TShowIconsMode = sim_all_and_exe;
  gShowIconsNew: TShowIconsMode;
  gIconOverlays : Boolean;
  gIconsSize,
  gIconsSizeNew : Integer;
  gUseMmapInSearch : Boolean;
  gCustomDriveIcons : Boolean; // for use custom drive icons under windows
  
  { File operations page }

  gCopyBlockSize : Integer;
  gSkipFileOpError: Boolean;
  gDropReadOnlyFlag : Boolean = True;
  gWipePassNumber: Integer;
  gProcessComments: Boolean;
  gShowCopyTabSelectPanel:boolean;
  gUseTrash : Boolean = True; // 05.05.2009 - global trash variable. Enabled by default.
  gRenameSelOnlyName:boolean;
  gShowDialogOnDragDrop: Boolean = False;

  { Folder tabs page }

  gDirTabOptions : TTabsOptions = [tb_always_visible,
                                   tb_confirm_close_all,
                                   tb_show_asterisk_for_locked,
                                   tb_activate_panel_on_click];
  gDirTabLimit : Integer;
  gDirTabPosition : TTabsPosition = tbpos_top;

  { Log page }
  gLogFile : Boolean;
  gLogFileName : String;
  gLogOptions : TLogOptions = [log_cp_mv_ln, log_delete, log_dir_op, log_arc_op,
                               log_vfs_op, log_success, log_errors, log_info];

  { Configuration page }
  gUseConfigInProgramDir,
  gUseConfigInProgramDirNew,
  gSaveDirHistory,
  gSaveCmdLineHistory,
  gSaveFileMaskHistory : Boolean;
  
  {  Quick Search page}
  gQuickSearch : Boolean;
  gQuickSearchMode : TShiftState = [ssCtrl, ssAlt];
  gQuickSearchMatchBeginning,
  gQuickSearchMatchEnding : Boolean;
  gQuickFilter : Boolean = False;
  gQuickFilterMode : TShiftState = [];

  { Misc page }
  gGridVertLine,
  gGridHorzLine,
  gShowWarningMessages,
  gDirBrackets: Boolean;
  gShowToolTipMode: TShowToolTipMode = [stm_show_for_all];
  { Auto refresh page }
  gWatchDirs: TWatchOptions;
  gWatchDirsExclude: String;
  { Ignore list page }
  gIgnoreListFileEnabled: Boolean;
  gIgnoreListFile: UTF8String;

  gSearchTemplateList: TSearchTemplateList;

  {HotKey Manager}
  HotMan:THotKeyManager;
  
  {Actions}
  Actions:TActs;

  {Copy/Move operation options}
  gOperationOptionSymLinks: TFileSourceOperationOptionSymLink = fsooslNone;
  gOperationOptionCorrectLinks: Boolean = False;
  gOperationOptionFileExists: TFileSourceOperationOptionFileExists = fsoofeNone;
  gOperationOptionDirectoryExists: TFileSourceOperationOptionDirectoryExists = fsoodeNone;
  gOperationOptionCheckFreeSpace: Boolean = True;

  {Error file}
  gErrorFile: String;

function LoadGlobs(Version: String = '') : Boolean;
procedure SaveGlobs;
procedure LoadIniConfig;
procedure SaveIniConfig;
procedure LoadXmlConfig;
procedure SaveXmlConfig;
procedure ConvertIniToXml;

function LoadStringsFromFile(var list:TStringListEx; const sFileName:String):boolean;

procedure LoadDefaultHotkeyBindings;

function InitPropStorage(Owner: TComponent): TIniPropStorageEx;

const
  cMaxStringItems=50;
  
var
  gIni: TIniFileEx = nil;
  gConfig: TXmlConfig = nil;

implementation

uses
   LCLProc, SysUtils, uGlobsPaths, uLng, uShowMsg, uFileProcs, uOSUtils,
   uDCUtils, uUniqueInstance, fMultiRename, uFile;

procedure LoadDefaultHotkeyBindings;
begin
  // Note: Update hkVersion if you change default hotkeys list
  with HotMan do
    begin
      AddHotKeyEx('Alt+X','cm_Exit','','FrmMain','FrmMain');
      AddHotKeyEx('F1','cm_About','','FrmMain','FrmMain');
      AddHotKeyEx('F2','cm_RenameOnly','','FrmMain','FrmMain');
      AddHotKeyEx('F3','cm_View','','FrmMain','FrmMain');
      AddHotKeyEx('F4','cm_Edit','','FrmMain','FrmMain');
      AddHotKeyEx('F5','cm_Copy','','FrmMain','FrmMain');
      AddHotKeyEx('F6','cm_Rename','','FrmMain','FrmMain');
      AddHotKeyEx('F7','cm_MakeDir','','FrmMain','FrmMain');
      AddHotKeyEx('F8','cm_Delete','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+F8','cm_Delete','recyclesettingrev','FrmMain','FrmMain');
      AddHotKeyEx('Del','cm_Delete','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+Del','cm_Delete','recyclesettingrev','FrmMain','FrmMain');
      AddHotKeyEx('F9','cm_RunTerm','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+A','cm_MarkMarkAll','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+R','cm_Refresh','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+F7','cm_Search','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+D','cm_DirHotList','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+F3','cm_SortByName','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+F4','cm_SortByExt','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+F6','cm_SortBySize','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+F5','cm_SortByDate','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+M','cm_MultiRename','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+F5','cm_CopySamePanel','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+F6','cm_RenameOnly','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+F4','cm_EditNew','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+F10','cm_ContextMenu','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+H','cm_DirHistory','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+7','cm_ShowCmdLineHistory','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+L','cm_CalculateSpace','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+Shift+F9','cm_TestArchive','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+Shift+Enter','cm_CountDirContent','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+Enter','cm_FileProperties','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+T','cm_NewTab','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+W','cm_RemoveTab','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+Z','cm_EditComment','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+Q','cm_QuickView','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+F5','cm_PackFiles','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+F9','cm_ExtractFiles','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+F1','cm_LeftOpenDrives','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+F2','cm_RightOpenDrives','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+P','cm_AddPathToCmdLine','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+F2','cm_FocusCmdLine','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+Left','cm_TransferLeft','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+Right','cm_TransferRight','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+PgDn','cm_OpenArchive','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+S','cm_QuickSearch','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+F','cm_QuickFilter','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+Ctrl+X','cm_CopyNamesToClip','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+Ctrl+C','cm_CopyFullNamesToClip','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+Z','cm_TargetEqualSource','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+U','cm_Exchange','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+Del','cm_Wipe','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+Tab','cm_NextTab','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+Ctrl+Tab','cm_PrevTab','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+X','cm_CutToClipboard','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+C','cm_CopyToClipboard','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+V','cm_PasteFromClipboard','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+\','cm_ChangeDirToRoot','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+PgUp','cm_ChangeDirToParent','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+Up','cm_OpenDirInNewTab','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+Down','cm_ShowCmdLineHistory','','FrmMain','FrmMain');
      AddHotKeyEx('Ctrl+Enter','cm_AddFilenameToCmdLine','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+Ctrl+Enter','cm_AddPathAndFilenameToCmdLine','','FrmMain','FrmMain');
//      AddHotKeyEx('','cm_','','FrmMain','FrmMain');
    end;
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

function LoadStringsFromFile(var list:TStringListEx; const sFileName:String):boolean;
var
  i:Integer;
begin
  Assert(list <> nil,'LoadStringsFromFile: list=nil');
  list.Clear;
  Result:=False;
  if not mbFileExists(sFileName) then Exit;
  list.LoadFromFile(sFileName);
  for i:=list.Count-1 downto 0 do
    if i>cMaxStringItems then
      list.Delete(i)
    else
      Break;
  Result:=True;
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
          DebugLn('Invalid entry in configuration: ' + AConfig.GetPathFromNode(Node) + '.');
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

procedure InitGlobs;
begin
  gExts := TExts.Create;
  gColorExt := TColorExt.Create;
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
  ColSet := TPanelColumnsList.Create;
  HotMan := THotKeyManager.Create;
  Actions := TActs.Create;
end;

procedure DeInitGlobs;
begin
  if Assigned(gColorExt) then
    FreeAndNil(gColorExt);
  if Assigned(glsDirHistory) then
    FreeAndNil(glsDirHistory);
  if Assigned(glsHotDir) then
    FreeAndNil(glsHotDir);
  if Assigned(glsMaskHistory) then
    FreeAndNil(glsMaskHistory);
  if Assigned(glsSearchHistory) then
    FreeAndNil(glsSearchHistory);
  if Assigned(glsReplaceHistory) then
    FreeAndNil(glsReplaceHistory);
  if Assigned(glsIgnoreList) then
    FreeAndNil(glsIgnoreList);
  if Assigned(gExts) then
    FreeAndNil(gExts);
  if Assigned(gIni) then
    FreeAndNil(gIni);
  if Assigned(gConfig) then
    FreeAndNil(gConfig);
  if Assigned(gSearchTemplateList) then
    FreeAndNil(gSearchTemplateList);
  if Assigned(gDSXPlugins) then
    FreeAndNil(gDSXPlugins);
  if Assigned(gWCXPlugins) then
    FreeAndNil(gWCXPlugins);
  if Assigned(gWDXPlugins) then
    FreeAndNil(gWDXPlugins);
  if Assigned(gWFXPlugins) then
    FreeAndNil(gWFXPlugins);
  if Assigned(gWLXPlugins) then
    FreeAndNil(gWLXPlugins);
  if Assigned(ColSet) then
    FreeAndNil(ColSet);
  if Assigned(HotMan) then
    FreeAndNil(HotMan);
  if Assigned(Actions) then
    FreeAndNil(Actions);
end;

function OpenConfig: Boolean;
begin
  // Check global directory for XML config.
  if not Assigned(gConfig) and mbFileExists(gpGlobalCfgDir + 'doublecmd.xml') then
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
      // File is not accessible - print warning and continue below to check config in user directory.
      DebugLn('Warning: Config file ' + gpGlobalCfgDir + 'doublecmd.xml' +
              ' exists but is not accessible.');
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
      DebugLn('Error: Cannot access config file ' + gpGlobalCfgDir + 'doublecmd.xml.');
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
        FreeAndNil(gIni);
    end;

    // Check user directory for INI config.
    if not Assigned(gIni) and mbFileAccess(gpCfgDir + 'doublecmd.ini', fmOpenRead) then
    begin
      gIni := TIniFileEx.Create(gpCfgDir + 'doublecmd.ini', fmOpenRead);
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
    DebugLn('Warning: Config file ' + gpCfgDir + 'doublecmd.xml' +
            ' is not accessible for writing. Configuration will not be saved.');
  end
  else
    gConfig.SaveOnDestroy := True;

  Result := True;
end;

procedure CopySettingsFiles;
begin
  { Create default configuration files if need }
  if gpCfgDir <> gpGlobalCfgDir then
    begin
      // toolbar file
      if not mbFileExists(gpCfgDir + 'default.bar') then
        CopyFile(gpGlobalCfgDir + 'default.bar', gpCfgDir + 'default.bar');
      // extension file
      if not mbFileExists(gpCfgDir + 'doublecmd.ext') then
        CopyFile(gpGlobalCfgDir + 'doublecmd.ext.example', gpCfgDir + 'doublecmd.ext.example');
      // pixmaps file
      if not mbFileExists(gpCfgDir + 'pixmaps.txt') then
        CopyFile(gpGlobalCfgDir + 'pixmaps.txt', gpCfgDir + 'pixmaps.txt');
      // editor highlight file1
      if not mbFileExists(gpCfgDir + 'editor.col') then
        CopyFile(gpGlobalCfgDir + 'editor.col', gpCfgDir + 'editor.col');
      // editor highlight file2
      if not mbFileExists(gpCfgDir + 'twilight.col') then
        CopyFile(gpGlobalCfgDir + 'twilight.col', gpCfgDir + 'twilight.col');
    end;
end;

function LoadGlobs(Version: String) : Boolean;
begin
  Result := False;
  dcVersion:= Version;
  DebugLn('Loading configuration...');
  InitGlobs;
  if not OpenConfig then
    Exit;
  if Assigned(gIni) then
    LoadIniConfig
  else if Assigned(gConfig) then
    LoadXmlConfig
  else
  begin
    DebugLn('Error: No config created.');
    Exit(False);
  end;

  { Check is unique instance }
  if gOnlyOnce and not IsUniqueInstance(ApplicationName) then Exit(False);

  gErrorFile := gpCfgDir + ExtractOnlyFileName(Application.ExeName) + '.err';

  CopySettingsFiles;

  if mbFileExists(gpCfgDir + 'doublecmd.ext') then
    gExts.LoadFromFile(gpCfgDir + 'doublecmd.ext');

  if mbFileExists(gpCfgDir + 'dirhistory.txt') then
    LoadStringsFromFile(glsDirHistory, gpCfgDir + 'dirhistory.txt');

  if mbFileExists(gpCfgDir + 'maskhistory.txt') then
    LoadStringsFromFile(glsMaskHistory, gpCfgDir + 'maskhistory.txt');

  if mbFileExists(gpCfgDir + 'searchhistory.txt') then
    LoadStringsFromFile(glsSearchHistory, gpCfgDir + 'searchhistory.txt');

  if mbFileExists(gpCfgDir + 'replacehistory.txt') then
    LoadStringsFromFile(glsReplaceHistory, gpCfgDir + 'replacehistory.txt');

  if mbFileExists(gIgnoreListFile) then
    LoadStringsFromFile(glsIgnoreList, gIgnoreListFile);

  { Localization }
  DoLoadLng;
  msgLoadLng;
  Result := True;
end;

procedure SaveGlobs;
var
  TmpConfig: TXmlConfig;
  Ini: TIniFileEx;
begin
  if gUseConfigInProgramDirNew <> gUseConfigInProgramDir then
    begin
      LoadPaths;
      if gUseConfigInProgramDirNew then
        gpCfgDir := gpGlobalCfgDir;

      { Save location of configuration files }

      if Assigned(gIni) then
      begin
        // Still using INI config.
        FreeThenNil(gIni);
        Ini:= TIniFileEx.Create(gpGlobalCfgDir + 'doublecmd.ini');
        try
          Ini.WriteBool('Configuration', 'UseIniInProgramDir', gUseConfigInProgramDirNew);
        finally
          Ini.Free;
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

  glsSearchHistory.SaveToFile(gpCfgDir + 'searchhistory.txt');
  glsReplaceHistory.SaveToFile(gpCfgDir + 'replacehistory.txt');
  glsIgnoreList.SaveToFile(gIgnoreListFile);

  //TODO: Save hotkeys
  //HotMan.Save();

  if Assigned(gIni) then
    SaveIniConfig;
  SaveXmlConfig;
end;

procedure LoadIniConfig;
begin
  { Layout page }

  gButtonBar := gIni.ReadBool('Layout', 'ButtonBar', True);
  gToolBarFlat := gIni.ReadBool('ButtonBar', 'FlatIcons', True);
  gToolBarButtonSize := gIni.ReadInteger('ButtonBar', 'ButtonHeight', 16);
  gToolBarIconSize := gIni.ReadInteger('ButtonBar', 'SmallIconSize', 16);
  gToolBarSmallIcons:= gIni.ReadBool('ButtonBar', 'SmallIcons', False);
  gDriveBar1 := gIni.ReadBool('Layout', 'DriveBar1', True);
  gDriveBar2 := gIni.ReadBool('Layout', 'DriveBar2', True);
  gDriveBarFlat := gIni.ReadBool('Layout', 'DriveBarFlat', True);
  gDriveMenuButton := gIni.ReadBool('Layout', 'DriveMenuButton', True);
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
  gOnlyOnce:= gIni.ReadBool('Configuration', 'OnlyOnce', False);
  gCaseSensitiveSort := gIni.ReadBool('Configuration', 'CaseSensitiveSort', False);
  gLynxLike := gIni.ReadBool('Configuration', 'LynxLike', True);
  gShortFileSizeFormat := gIni.ReadBool('Configuration', 'ShortFileSizeFormat', True);
  gScrollMode := gIni.ReadInteger('Configuration', 'ScrollMode', 0);
  gMinimizeToTray := gIni.ReadBool('Configuration', 'MinimizeToTray', False);
  gAlwaysShowTrayIcon := gIni.ReadBool('Configuration', 'AlwaysShowTrayIcon', False);
  gDateTimeFormat := gIni.ReadString('Configuration', 'DateTimeFormat', 'dd.mm.yy');
  gDriveBlackList:= gIni.ReadString('Configuration', 'DriveBlackList', '');
  gSpaceMovesDown := gIni.ReadBool('Configuration', 'SpaceMovesDown', False);

  gListFilesInThread := gIni.ReadBool('Configuration', 'ListFilesInThread', gListFilesInThread);
  gLoadIconsSeparately := gIni.ReadBool('Configuration', 'LoadIconsSeparately', gLoadIconsSeparately);

  gMouseSelectionEnabled:= gIni.ReadBool('Configuration', 'MouseSelectionEnabled', True);
  gMouseSelectionButton := gIni.ReadInteger('Configuration', 'MouseSelectionButton', 0);

  gAutoFillColumns:= gIni.ReadBool('Configuration', 'AutoFillColumns', False);
  gAutoSizeColumn := gIni.ReadInteger('Configuration', 'AutoSizeColumn', 1);

  gDirTabOptions := TTabsOptions(gIni.ReadInteger('Configuration', 'DirTabOptions', Integer(gDirTabOptions)));
  gDirTabLimit :=  gIni.ReadInteger('Configuration', 'DirTabLimit', 32);
  gDirTabPosition := TTabsPosition(gIni.ReadInteger('Configuration', 'DirTabPosition', Integer(gDirTabPosition)));

  gUseExtEdit := gIni.ReadBool('Configuration', 'UseExtEdit', False);
  gUseExtView := gIni.ReadBool('Configuration', 'UseExtView', False);
  gUseExtDiff := gIni.ReadBool('Configuration', 'UseExtDiff', False);

  gExtEdit := gIni.ReadString('Configuration', 'ExtEdit', '');
  gExtView := gIni.ReadString('Configuration', 'ExtView', '');
  gExtDiff := gIni.ReadString('Configuration', 'ExtDiff', '');
  gRunTerm := gIni.ReadString('Configuration', 'RunTerm', RunTerm);

  gLuaLib:=gIni.ReadString('Configuration', 'LuaLib', gLuaLib);


  { Fonts }
  gFontName:=gIni.ReadString('Configuration', 'Font.Name', 'default');
  DebugLn('gFontName:',gFontName);
  gEditorFontName:=gIni.ReadString('Editor', 'Font.Name', MonoSpaceFont);
  DebugLn('gEditorFontName:',gEditorFontName);
  gViewerFontName:=gIni.ReadString('Viewer', 'Font.Name', MonoSpaceFont);
  DebugLn('gViewerEditorFontName:',gViewerFontName);
  gFontSize:=gIni.ReadInteger('Configuration', 'Font.Size', 10);
  gEditorFontSize:=gIni.ReadInteger('Editor', 'Font.Size', 14);
  gViewerFontSize:=gIni.ReadInteger('Viewer', 'Font.Size', 14);
  gFontStyle := TFontStyles(gIni.ReadInteger('Configuration', 'Font.Style', 1));
  gEditorFontStyle:= TFontStyles(gIni.ReadInteger('Editor', 'Font.Style', 0));
  gViewerFontStyle:= TFontStyles(gIni.ReadInteger('Viewer', 'Font.Style', 0));

  { Colors }
  gForeColor  := gIni.ReadInteger('Colors', 'ForeColor', clDefault);
  gBackColor := gIni.ReadInteger('Colors', 'BackColor', clWhite);
  gBackColor2 := gIni.ReadInteger('Colors', 'BackColor2', clWhite);
  gMarkColor := gIni.ReadInteger('Colors', 'MarkColor', clRed);
  gCursorColor := gIni.ReadInteger('Colors', 'CursorColor', clHighlight);
  gCursorText := gIni.ReadInteger('Colors', 'CursorText', clHighlightText);
  gUseInvertedSelection:= gIni.ReadBool('Colors', 'UseInvertedSelection', False);
  gInactivePanelBrightness:= gIni.ReadInteger('Colors', 'InactivePanelBrightness', gInactivePanelBrightness);

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
  gQuickSearch := gIni.ReadBool('Configuration', 'QuickSearch', True);
  gQuickSearchMode := TShiftState(gIni.ReadInteger('Configuration', 'QuickSearchMode', Integer(gQuickSearchMode)));
  gQuickSearchMatchBeginning := gIni.ReadBool('Configuration', 'QuickSearchMatchBeginning', True);
  gQuickSearchMatchEnding := gIni.ReadBool('Configuration', 'QuickSearchMatchEnding', True);
  gQuickFilter := gIni.ReadBool('Configuration', 'QuickFilter', gQuickFilter);
  gQuickFilterMode := TShiftState(gIni.ReadInteger('Configuration', 'QuickFilterMode', Integer(gQuickFilterMode)));
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
  gShowIconsNew:= gShowIcons;
  gIconOverlays:= gIni.ReadBool('Configuration', 'IconOverlays', True);
  gIconsSize := gIni.ReadInteger('Configuration', 'IconsSize', 16);
  gIconsSizeNew:= gIconsSize;
  gCustomDriveIcons := gIni.ReadBool('Configuration', 'CustomDriveIcons', False);
  { Ignore list page }
  gIgnoreListFileEnabled:= gIni.ReadBool('Configuration', 'IgnoreListFileEnabled', False);
  gIgnoreListFile:= gIni.ReadString('Configuration', 'IgnoreListFile', gpCfgDir + 'ignorelist.txt');

  gCutTextToColWidth := gIni.ReadBool('Configuration', 'CutTextToColWidth', True);

  gViewerImageStretch:=  gIni.ReadBool('Viewer', 'Image.Stretch', False);

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
  gIni.WriteBool('ButtonBar', 'SmallIcons', gToolBarSmallIcons);
  gIni.WriteBool('Layout', 'DriveBar1', gDriveBar1);
  gIni.WriteBool('Layout', 'DriveBar2', gDriveBar2);
  gIni.WriteBool('Layout', 'DriveBarFlat', gDriveBarFlat);
  gIni.WriteBool('Layout', 'DriveMenuButton', gDriveMenuButton);
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
  gIni.WriteBool('Configuration', 'OnlyOnce', gOnlyOnce);
  gIni.WriteBool('Configuration', 'CaseSensitiveSort', gCaseSensitiveSort);
  gIni.WriteBool('Configuration', 'LynxLike', gLynxLike);

  gIni.WriteBool('Configuration', 'ShortFileSizeFormat', gShortFileSizeFormat);
  gIni.WriteInteger('Configuration', 'ScrollMode', gScrollMode);
  gIni.WriteBool('Configuration', 'MinimizeToTray', gMinimizeToTray);
  gIni.WriteBool('Configuration', 'AlwaysShowTrayIcon', gAlwaysShowTrayIcon);
  gIni.WriteString('Configuration', 'DateTimeFormat', gDateTimeFormat);
  gIni.WriteString('Configuration', 'DriveBlackList', gDriveBlackList);
  gIni.WriteBool('Configuration', 'SpaceMovesDown', gSpaceMovesDown);

  gIni.WriteBool('Configuration', 'ListFilesInThread', gListFilesInThread);
  gIni.WriteBool('Configuration', 'LoadIconsSeparately', gLoadIconsSeparately);

  gIni.WriteBool('Configuration', 'MouseSelectionEnabled', gMouseSelectionEnabled);
  gIni.WriteInteger('Configuration', 'MouseSelectionButton', gMouseSelectionButton);

  gIni.WriteBool('Configuration', 'AutoFillColumns', gAutoFillColumns);
  gIni.WriteInteger('Configuration', 'AutoSizeColumn', gAutoSizeColumn);

  gIni.WriteInteger('Configuration', 'DirTabOptions', Integer(gDirTabOptions));
  gIni.WriteInteger('Configuration', 'DirTabLimit', gDirTabLimit);
  gIni.WriteInteger('Configuration', 'DirTabPosition', Integer(gDirTabPosition));

  gIni.WriteBool('Configuration', 'UseExtEdit', gUseExtEdit);
  gIni.WriteBool('Configuration', 'UseExtView', gUseExtView);
  gIni.WriteBool('Configuration', 'UseExtDiff', gUseExtDiff);

  gIni.WriteString('Configuration', 'ExtEdit', gExtEdit);
  gIni.WriteString('Configuration', 'ExtView', gExtView);
  gIni.WriteString('Configuration', 'ExtDiff', gExtDiff);
  gIni.WriteString('Configuration', 'RunTerm', gRunTerm);

  gIni.WriteString('Configuration', 'LuaLib', gLuaLib);

  { Fonts }
  gIni.WriteString('Configuration', 'Font.Name', gFontName);
  gIni.WriteString('Editor', 'Font.Name', gEditorFontName);
  gIni.WriteString('Viewer', 'Font.Name', gViewerFontName);

  gIni.WriteInteger('Configuration', 'Font.Size', gFontSize);
  gIni.WriteInteger('Editor', 'Font.Size', gEditorFontSize);
  gIni.WriteInteger('Viewer', 'Font.Size', gViewerFontSize);

  gIni.WriteInteger('Configuration', 'Font.Style', Integer(gFontStyle));
  gIni.WriteInteger('Editor', 'Font.Style', Integer(gEditorFontStyle));
  gIni.WriteInteger('Viewer', 'Font.Style', Integer(gViewerFontStyle));
  { Colors }
  gIni.WriteInteger('Colors', 'ForeColor', gForeColor);
  gIni.WriteInteger('Colors', 'BackColor', gBackColor);
  gIni.WriteInteger('Colors', 'BackColor2', gBackColor2);
  gIni.WriteInteger('Colors', 'MarkColor', gMarkColor);
  gIni.WriteInteger('Colors', 'CursorColor', gCursorColor);
  gIni.WriteInteger('Colors', 'CursorText', gCursorText);
  gIni.WriteBool('Colors', 'UseInvertedSelection', gUseInvertedSelection);
  gIni.WriteInteger('Colors', 'InactivePanelBrightness', gInactivePanelBrightness);

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
  gIni.WriteBool('Configuration', 'QuickSearch', gQuickSearch);
  gIni.WriteInteger('Configuration', 'QuickSearchMode', Integer(gQuickSearchMode));
  gIni.WriteBool('Configuration', 'QuickSearchMatchBeginning', gQuickSearchMatchBeginning);
  gIni.WriteBool('Configuration', 'QuickSearchMatchEnding', gQuickSearchMatchEnding);
  gIni.WriteBool('Configuration', 'QuickFilter', gQuickFilter);
  gIni.WriteInteger('Configuration', 'QuickFilterMode', Integer(gQuickFilterMode));
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

  gIni.WriteBool('Viewer', 'Image.Stretch', gViewerImageStretch);

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
var
  Root, Node, SubNode: TXmlNode;
begin
  with gConfig do
  begin
    Root := gConfig.RootNode;

    { Language page }
    gPOFileName := GetValue(Root, 'Language/POFileName', '');

    { Behaviours page }
    Node := Root.FindNode('Behaviours');
    if Assigned(Node) then
    begin
      gRunInTerm := GetValue(Node, 'RunInTerminal', RunInTerm);
      gRunTerm := GetValue(Node, 'RunTerminal', RunTerm);
      gOnlyOnce := GetValue(Node, 'OnlyOneInstance', gOnlyOnce);
      gLynxLike := GetValue(Node, 'LynxLike', True);
      gCaseSensitiveSort := GetValue(Node, 'CaseSensitiveSort', False);
      gShortFileSizeFormat := GetValue(Node, 'ShortFileSizeFormat', True);
      gMinimizeToTray := GetValue(Node, 'MinimizeToTray', False);
      gAlwaysShowTrayIcon := GetValue(Node, 'AlwaysShowTrayIcon', False);
      gMouseSelectionEnabled := GetAttr(Node, 'Mouse/Selection/Enabled', True);
      gMouseSelectionButton := GetValue(Node, 'Mouse/Selection/Button', 0);
      gScrollMode := GetValue(Node, 'Mouse/ScrollMode', 0);
      gAutoFillColumns := GetValue(Node, 'AutoFillColumns', False);
      gAutoSizeColumn := GetValue(Node, 'AutoSizeColumn', 1);
      gDateTimeFormat := GetValue(Node, 'DateTimeFormat', 'dd.mm.yy');
      gCutTextToColWidth := GetValue(Node, 'CutTextToColumnWidth', True);
      gShowSystemFiles := GetValue(Node, 'ShowSystemFiles', False);
      gListFilesInThread := GetValue(Node, 'ListFilesInThread', gListFilesInThread);
      gLoadIconsSeparately := GetValue(Node, 'LoadIconsSeparately', gLoadIconsSeparately);
      gDriveBlackList := GetValue(Node, 'DriveBlackList', '');
    end;

    { Tools page }
    gUseExtEdit := GetAttr(Root, 'Tools/Editor/Enabled', False);
    gExtEdit := GetValue(Root, 'Tools/Editor/Path', '');
    gUseExtView := GetAttr(Root, 'Tools/Viewer/Enabled', False);
    gExtView := GetValue(Root, 'Tools/Viewer/Path', '');
    gUseExtDiff := GetAttr(Root, 'Tools/Differ/Enabled', False);
    gExtDiff := GetValue(Root, 'Tools/Differ/Path', '');

    { Fonts page }
    gConfig.GetFont(Root, 'Fonts/Main', gFontName, gFontSize, Integer(gFontStyle),
                    'default', 10, 1);
    gConfig.GetFont(Root, 'Fonts/Editor', gEditorFontName, gEditorFontSize, Integer(gEditorFontStyle),
                    MonoSpaceFont, 14, 0);
    gConfig.GetFont(Root, 'Fonts/Viewer', gViewerFontName, gViewerFontSize, Integer(gViewerFontStyle),
                     MonoSpaceFont, 14, 0);

    { Colors page }
    Node := Root.FindNode('Colors');
    if Assigned(Node) then
    begin
      gForeColor := GetValue(Node, 'Foreground', clDefault);
      gBackColor := GetValue(Node, 'Background', clWhite);
      gBackColor2 := GetValue(Node, 'Background2', gBackColor2);
      gMarkColor := GetValue(Node, 'Mark', clRed);
      gCursorColor := GetValue(Node, 'Cursor', clHighlight);
      gCursorText := GetValue(Node, 'CursorText', clHighlightText);
      gUseInvertedSelection := GetValue(Node, 'UseInvertedSelection', False);
      gInactivePanelBrightness := GetValue(Node, 'InactivePanelBrightness', gInactivePanelBrightness);

      gColorExt.Load(gConfig, Node);
    end;

    { Layout page }
    Node := Root.FindNode('Layout');
    if Assigned(Node) then
    begin
      SubNode := Node.FindNode('ButtonBar');
      if Assigned(SubNode) then
      begin
        gButtonBar := GetAttr(SubNode, 'Enabled', True);
        gToolBarFlat := GetValue(SubNode, 'FlatIcons', True);
        gToolBarButtonSize := GetValue(SubNode, 'ButtonHeight', 16);
        gToolBarIconSize := GetValue(SubNode, 'SmallIconSize', 16);
        gToolBarSmallIcons := GetValue(SubNode, 'SmallIcons', False);
      end;
      gDriveBar1 := GetValue(Node, 'DriveBar1', True);
      gDriveBar2 := GetValue(Node, 'DriveBar2', True);
      gDriveBarFlat := GetValue(Node, 'DriveBarFlat', True);
      gDriveMenuButton := GetValue(Node, 'DriveMenuButton', True);
      gDirectoryTabs := GetValue(Node, 'DirectoryTabs', True);
      gCurDir := GetValue(Node, 'CurrentDirectory', True);
      gTabHeader := GetValue(Node, 'TabHeader', True);
      gStatusBar := GetValue(Node, 'StatusBar', True);
      gCmdLine := GetValue(Node, 'CmdLine', True);
      gLogWindow := GetValue(Node, 'LogWindow', True);
      gTermWindow := GetValue(Node, 'TermWindow', False);
      gKeyButtons := GetValue(Node, 'KeyButtons', True);
      gInterfaceFlat := GetValue(Node, 'InterfaceFlat', True);
    end;

    { File operations page }
    Node := Root.FindNode('FileOperations');
    if Assigned(Node) then
    begin
      gCopyBlockSize := GetValue(Node, 'BufferSize', 65536);
      gUseMmapInSearch := GetValue(Node, 'UseMmapInSearch', False);
      gWipePassNumber := GetValue(Node, 'WipePassNumber', 1);
      gDropReadOnlyFlag := GetValue(Node, 'DropReadOnlyFlag', True);
      gProcessComments := GetValue(Node, 'ProcessComments', True);
      gRenameSelOnlyName := GetValue(Node, 'RenameSelOnlyName', False);
      gShowCopyTabSelectPanel := GetValue(Node, 'ShowCopyTabSelectPanel', False);
      gUseTrash := GetValue(Node, 'UseTrash', True);
      gSkipFileOpError := GetValue(Node, 'SkipFileOpError', False);
      gShowDialogOnDragDrop := GetValue(Node, 'ShowDialogOnDragDrop', gShowDialogOnDragDrop);
      // Operations options
      SubNode := Node.FindNode('Options');
      if Assigned(SubNode) then
      begin
        gOperationOptionSymLinks := TFileSourceOperationOptionSymLink(GetValue(SubNode, 'Symlink', Integer(gOperationOptionSymLinks)));
        gOperationOptionCorrectLinks := GetValue(SubNode, 'CorrectLinks', gOperationOptionCorrectLinks);
        gOperationOptionFileExists := TFileSourceOperationOptionFileExists(GetValue(SubNode, 'FileExists', Integer(gOperationOptionFileExists)));
        gOperationOptionDirectoryExists := TFileSourceOperationOptionDirectoryExists(GetValue(SubNode, 'DirectoryExists', Integer(gOperationOptionDirectoryExists)));
        gOperationOptionCheckFreeSpace := GetValue(SubNode, 'CheckFreeSpace', gOperationOptionCheckFreeSpace);
      end;
    end;

    { Tabs page }
    Node := Root.FindNode('Tabs');
    if Assigned(Node) then
    begin
      gDirTabOptions := TTabsOptions(GetValue(Node, 'Options', Integer(gDirTabOptions)));
      gDirTabLimit := GetValue(Node, 'CharacterLimit', 32);
      gDirTabPosition := TTabsPosition(GetValue(Node, 'Position', Integer(gDirTabPosition)));
    end;

    { Log page }
    Node := Root.FindNode('Log');
    if Assigned(Node) then
    begin
      gLogFile := GetAttr(Node, 'Enabled', True);
      gLogFileName := GetValue(Node, 'FileName', gpCfgDir + 'doublecmd.log');
      gLogOptions := TLogOptions(GetValue(Node, 'Options', Integer(gLogOptions)));
    end;

    { Configuration page }
    gSaveDirHistory := GetAttr(Root, 'History/DirHistory/Save', True);
    gSaveCmdLineHistory := GetAttr(Root, 'History/CmdLineHistory/Save', True);
    gSaveFileMaskHistory := GetAttr(Root, 'History/FileMaskHistory/Save', True);

    { Quick Search/Filter page }
    Node := Root.FindNode('QuickSearch');
    if Assigned(Node) then
    begin
      gQuickSearch := GetAttr(Node, 'Enabled', True);
      gQuickSearchMode := TShiftState(GetValue(Node, 'Mode', Integer(gQuickSearchMode)));
      gQuickSearchMatchBeginning := GetValue(Node, 'MatchBeginning', True);
      gQuickSearchMatchEnding := GetValue(Node, 'MatchEnding', True);
    end;
    Node := Root.FindNode('QuickFilter');
    if Assigned(Node) then
    begin
      gQuickFilter := GetAttr(Node, 'Enabled', gQuickFilter);
      gQuickFilterMode := TShiftState(GetValue(Node, 'Mode', Integer(gQuickFilterMode)));
    end;

    { Miscellaneous page }
    Node := Root.FindNode('Miscellaneous');
    if Assigned(Node) then
    begin
      gGridVertLine := GetValue(Node, 'GridVertLine', False);
      gGridHorzLine := GetValue(Node, 'GridHorzLine', False);
      gShowWarningMessages := GetValue(Node, 'ShowWarningMessages', True);
      gSpaceMovesDown := GetValue(Node, 'SpaceMovesDown', False);
      gDirBrackets := GetValue(Node, 'DirBrackets', True);
      gShowToolTipMode := TShowToolTipMode(GetValue(Node, 'ShowToolTipMode', Integer(gShowToolTipMode)));
    end;

    { Auto refresh page }
    Node := Root.FindNode('AutoRefresh');
    if Assigned(Node) then
    begin
      gWatchDirs := TWatchOptions(GetValue(Node, 'Options', Integer(gWatchDirs)));
      gWatchDirsExclude := GetValue(Node, 'ExcludeDirs', '');
    end;

    { Icons page }
    Node := Root.FindNode('Icons');
    if Assigned(Node) then
    begin
      gShowIcons := TShowIconsMode(GetValue(Node, 'ShowMode', Integer(gShowIcons)));
      gShowIconsNew := gShowIcons;
      gIconOverlays := GetValue(Node, 'ShowOverlays', True);
      gIconsSize := GetValue(Node, 'Size', 16);
      gIconsSizeNew := gIconsSize;
      gCustomDriveIcons := GetValue(Node, 'CustomDriveIcons', False);
    end;

    { Ignore list page }
    Node := Root.FindNode('IgnoreList');
    if Assigned(Node) then
    begin
      gIgnoreListFileEnabled:= GetAttr(Node, 'Enabled', False);
      gIgnoreListFile:= GetValue(Node, 'IgnoreListFile', gpCfgDir + 'ignorelist.txt');
    end;

    { Directories HotList }
    LoadDirHotList(gConfig, Root);

    { - Other - }
    gLuaLib := GetValue(Root, 'Lua/PathToLibrary', gLuaLib);
    gViewerImageStretch := GetValue(Root, 'Viewer/ImageStretch', False);
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
var
  I: Integer;
  Root, Node, SubNode: TXmlNode;
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
    SetValue(Node, 'RunInTerminal', gRunInTerm);
    SetValue(Node, 'RunTerminal', gRunTerm);
    SetValue(Node, 'OnlyOneInstance', gOnlyOnce);
    SetValue(Node, 'LynxLike', gLynxLike);
    SetValue(Node, 'CaseSensitiveSort', gCaseSensitiveSort);
    SetValue(Node, 'ShortFileSizeFormat', gShortFileSizeFormat);
    SetValue(Node, 'MinimizeToTray', gMinimizeToTray);
    SetValue(Node, 'AlwaysShowTrayIcon', gAlwaysShowTrayIcon);
    SubNode := FindNode(Node, 'Mouse', True);
    SetAttr(SubNode, 'Selection/Enabled', gMouseSelectionEnabled);
    SetValue(SubNode, 'Selection/Button', gMouseSelectionButton);
    SetValue(SubNode, 'ScrollMode', gScrollMode);
    SetValue(Node, 'AutoFillColumns', gAutoFillColumns);
    SetValue(Node, 'AutoSizeColumn', gAutoSizeColumn);
    SetValue(Node, 'DateTimeFormat', gDateTimeFormat);
    SetValue(Node, 'CutTextToColumnWidth', gCutTextToColWidth);
    SetValue(Node, 'ShowSystemFiles', gShowSystemFiles);
    SetValue(Node, 'ListFilesInThread', gListFilesInThread);
    SetValue(Node, 'LoadIconsSeparately', gLoadIconsSeparately);
    SetValue(Node, 'DriveBlackList', gDriveBlackList);

    { Tools page }
    SetAttr(Root, 'Tools/Editor/Enabled', gUseExtEdit);
    SetValue(Root, 'Tools/Editor/Path', gExtEdit);
    SetAttr(Root, 'Tools/Viewer/Enabled', gUseExtView);
    SetValue(Root, 'Tools/Viewer/Path', gExtView);
    SetAttr(Root, 'Tools/Differ/Enabled', gUseExtDiff);
    SetValue(Root, 'Tools/Differ/Path', gExtDiff);

    { Fonts page }
    gConfig.SetFont(Root, 'Fonts/Main', gFontName, gFontSize, Integer(gFontStyle));
    gConfig.SetFont(Root, 'Fonts/Editor', gEditorFontName, gEditorFontSize, Integer(gEditorFontStyle));
    gConfig.SetFont(Root, 'Fonts/Viewer', gViewerFontName, gViewerFontSize, Integer(gViewerFontStyle));

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
    gColorExt.Save(gConfig, Node);

    { Layout page }
    Node := FindNode(Root, 'Layout', True);
    SubNode := FindNode(Node, 'ButtonBar', True);
    SetAttr(SubNode, 'Enabled', gButtonBar);
    SetValue(SubNode, 'FlatIcons', gToolBarFlat);
    SetValue(SubNode, 'ButtonHeight', gToolBarButtonSize);
    SetValue(SubNode, 'SmallIconSize', gToolBarIconSize);
    SetValue(SubNode, 'SmallIcons', gToolBarSmallIcons);
    SetValue(Node, 'DriveBar1', gDriveBar1);
    SetValue(Node, 'DriveBar2', gDriveBar2);
    SetValue(Node, 'DriveBarFlat', gDriveBarFlat);
    SetValue(Node, 'DriveMenuButton', gDriveMenuButton);
    SetValue(Node, 'DirectoryTabs', gDirectoryTabs);
    SetValue(Node, 'CurrentDirectory', gCurDir);
    SetValue(Node, 'TabHeader', gTabHeader);
    SetValue(Node, 'StatusBar', gStatusBar);
    SetValue(Node, 'CmdLine', gCmdLine);
    SetValue(Node, 'LogWindow', gLogWindow);
    SetValue(Node, 'TermWindow', gTermWindow);
    SetValue(Node, 'KeyButtons', gKeyButtons);
    SetValue(Node, 'InterfaceFlat', gInterfaceFlat);

    { File operations page }
    Node := FindNode(Root, 'FileOperations', True);
    SetValue(Node, 'BufferSize', gCopyBlockSize);
    SetValue(Node, 'UseMmapInSearch', gUseMmapInSearch);
    SetValue(Node, 'WipePassNumber', gWipePassNumber);
    SetValue(Node, 'DropReadOnlyFlag', gDropReadOnlyFlag);
    SetValue(Node, 'ProcessComments', gProcessComments);
    SetValue(Node, 'RenameSelOnlyName', gRenameSelOnlyName);
    SetValue(Node, 'ShowCopyTabSelectPanel', gShowCopyTabSelectPanel);
    SetValue(Node, 'UseTrash', gUseTrash);
    SetValue(Node, 'SkipFileOpError', gSkipFileOpError);
    SetValue(Node, 'ShowDialogOnDragDrop', gShowDialogOnDragDrop);
    // Operations options
    SubNode := FindNode(Node, 'Options', True);
    SetValue(SubNode, 'Symlink', Integer(gOperationOptionSymLinks));
    SetValue(SubNode, 'CorrectLinks', gOperationOptionCorrectLinks);
    SetValue(SubNode, 'FileExists', Integer(gOperationOptionFileExists));
    SetValue(SubNode, 'DirectoryExists', Integer(gOperationOptionDirectoryExists));
    SetValue(SubNode, 'CheckFreeSpace', gOperationOptionCheckFreeSpace);

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
    SetAttr(Root, 'History/DirHistory/Save', gSaveDirHistory);
    SetAttr(Root, 'History/CmdLineHistory/Save', gSaveCmdLineHistory);
    SetAttr(Root, 'History/FileMaskHistory/Save', gSaveFileMaskHistory);

    { Quick Search/Filter page }
    Node := FindNode(Root, 'QuickSearch', True);
    SetAttr(Node, 'Enabled', gQuickSearch);
    SetValue(Node, 'Mode', Integer(gQuickSearchMode));
    SetValue(Node, 'MatchBeginning', gQuickSearchMatchBeginning);
    SetValue(Node, 'MatchEnding', gQuickSearchMatchEnding);
    Node := FindNode(Root, 'QuickFilter', True);
    SetAttr(Node, 'Enabled', gQuickFilter);
    SetValue(Node, 'Mode', Integer(gQuickFilterMode));

    { Misc page }
    Node := FindNode(Root, 'Miscellaneous', True);
    SetValue(Node, 'GridVertLine', gGridVertLine);
    SetValue(Node, 'GridHorzLine', gGridHorzLine);
    SetValue(Node, 'ShowWarningMessages', gShowWarningMessages);
    SetValue(Node, 'SpaceMovesDown', gSpaceMovesDown);
    SetValue(Node, 'DirBrackets', gDirBrackets);
    SetValue(Node, 'ShowToolTipMode', Integer(gShowToolTipMode));

    { Auto refresh page }
    Node := FindNode(Root, 'AutoRefresh', True);
    SetValue(Node, 'Options', Integer(gWatchDirs));
    SetValue(Node, 'ExcludeDirs', gWatchDirsExclude);

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

    { - Other - }
    SetValue(Root, 'Lua/PathToLibrary', gLuaLib);
    SetValue(Root, 'Viewer/ImageStretch', gViewerImageStretch);
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

initialization

finalization
  DeInitGlobs;
end.
