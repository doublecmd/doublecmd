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
  Classes, Controls, Forms, uExts, uColorExt, Graphics, uClassesEx, uMultiArc,
  uColumns, uhotkeymanger, uActs, uSearchTemplate, uFileSourceOperationOptions,
  uWFXModule, uWCXModule, uWDXModule, uwlxmodule, udsxmodule, uXmlConfig,
  uInfoToolTip;

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

const
  { Default hotkey list version number }
  hkVersion: String = '0.4.6.r3603';
  ConfigVersion = '1';

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
  gDriveMenuButton,
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
  gHorizontalFilePanels: Boolean;

  { Toolbar }
  gToolBarButtonSize,
  gToolBarIconSize: Integer;
  gToolBarSmallIcons: Boolean;

  gDirSortFirst:Boolean; // if directories are shown as first in panels
  gDirHistoryCount:Integer; // how many history we remember
  gShowSystemFiles:Boolean;
  gRunInTerm: String;
  gRunTerm: String;
  gSortCaseSensitive,
  gSortNatural:Boolean;
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
  gListFilesInThread: Boolean;
  gLoadIconsSeparately: Boolean;
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
  gCursorText : TColor; //text color under cursor
  gUseInvertedSelection: Boolean;
  gInactivePanelBrightness: Integer; // 0 .. 100 (black .. full color)
  gUseFrameCursor: Boolean;

  gShowIcons: TShowIconsMode;
  gShowIconsNew: TShowIconsMode;
  gIconOverlays : Boolean;
  gIconsSize,
  gIconsSizeNew : Integer;
  gCustomDriveIcons : Boolean; // for use custom drive icons under windows
  
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
  
  {  Quick Search page}
  gQuickSearch : Boolean;
  gQuickSearchMode : TShiftState;
  gQuickSearchMatchBeginning,
  gQuickSearchMatchEnding : Boolean;
  gQuickFilter : Boolean;
  gQuickFilterMode : TShiftState;

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
  
  {Actions}
  Actions:TActs;

  {Copy/Move operation options}
  gOperationOptionSymLinks: TFileSourceOperationOptionSymLink;
  gOperationOptionCorrectLinks: Boolean;
  gOperationOptionFileExists: TFileSourceOperationOptionFileExists;
  gOperationOptionDirectoryExists: TFileSourceOperationOptionDirectoryExists;
  gOperationOptionCheckFreeSpace: Boolean;

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

function IsFileSystemWatcher: Boolean;

const
  cMaxStringItems=50;
  
var
  gIni: TIniFileEx = nil;
  gConfig: TXmlConfig = nil;

implementation

uses
   LCLProc, SysUtils, uGlobsPaths, uLng, uShowMsg, uFileProcs, uOSUtils,
   uDCUtils, fMultiRename, uFile, uDCVersion, uDebug, uFileFunctions;

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
      AddHotKeyEx('Alt+Left','cm_ViewHistoryPrev','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+Right','cm_ViewHistoryNext','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+Down','cm_DirHistory','','FrmMain','FrmMain');
      AddHotKeyEx('Shift+Ctrl+H','cm_HorizontalFilePanels','','FrmMain','FrmMain');
      AddHotKeyEx('Alt+V','cm_OperationsViewer','','FrmMain','FrmMain');

//      AddHotKeyEx('','cm_','','FrmMain','FrmMain');

      // Viewer
      AddHotKeyEx('F1','cm_Viewer_About','','FrmViewer','FrmViewer');
      AddHotKeyEx('F1','cm_Viewer_DeleteFile','','FrmViewer','FrmViewer');

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
  Actions := TActs.Create;
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
  FreeThenNil(Actions);
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
  gSortCaseSensitive := False;
  gSortNatural := False;
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
  gListFilesInThread := False;
  gLoadIconsSeparately := True;
  gDriveBlackList := '';

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
  gUseInvertedSelection := False;
  gInactivePanelBrightness := 100; // Full brightness

  { Layout page }
  gMainMenu := True;
  gButtonBar := True;
  gToolBarFlat := True;
  gToolBarButtonSize := 16;
  gToolBarIconSize := 16;
  gToolBarSmallIcons := False;
  gDriveBar1 := True;
  gDriveBar2 := True;
  gDriveBarFlat := True;
  gDriveMenuButton := True;
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
  gHorizontalFilePanels := False;

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
  // Operations options
  gOperationOptionSymLinks := fsooslNone;
  gOperationOptionCorrectLinks := False;
  gOperationOptionFileExists := fsoofeNone;
  gOperationOptionDirectoryExists := fsoodeNone;
  gOperationOptionCheckFreeSpace := True;

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
  gQuickSearch := True;
  gQuickSearchMode := [ssCtrl, ssAlt];
  gQuickSearchMatchBeginning := True;
  gQuickSearchMatchEnding := True;
  gQuickFilter := False;
  gQuickFilterMode := [];

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
  gIconOverlays := True;
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
  gLuaLib := '/usr/lib/liblua5.1.so';
  gNameSCFile := 'shortcuts.scf';
  gLastUsedPacker := 'zip';

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
  gDirSortFirst := True;
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
    glsIgnoreList.SaveToFile(ReplaceEnvVars(gIgnoreListFile));
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
  gOnlyOneAppInstance:= gIni.ReadBool('Configuration', 'OnlyOnce', False);
  gSortCaseSensitive := gIni.ReadBool('Configuration', 'CaseSensitiveSort', False);
  gLynxLike := gIni.ReadBool('Configuration', 'LynxLike', True);
  gShortFileSizeFormat := gIni.ReadBool('Configuration', 'ShortFileSizeFormat', True);
  gScrollMode := TScrollMode(gIni.ReadInteger('Configuration', 'ScrollMode', Integer(gScrollMode)));
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
  gIni.WriteBool('Configuration', 'OnlyOnce', gOnlyOneAppInstance);
  gIni.WriteBool('Configuration', 'CaseSensitiveSort', gSortCaseSensitive);
  gIni.WriteBool('Configuration', 'LynxLike', gLynxLike);

  gIni.WriteBool('Configuration', 'ShortFileSizeFormat', gShortFileSizeFormat);
  gIni.WriteInteger('Configuration', 'ScrollMode', Integer(gScrollMode));
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
var
  Root, Node, SubNode: TXmlNode;
begin
  with gConfig do
  begin
    Root := gConfig.RootNode;

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
      gSortCaseSensitive := GetValue(Node, 'SortCaseSensitive', gSortCaseSensitive);
      gSortNatural := GetValue(Node, 'SortNatural', gSortNatural);
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
      gListFilesInThread := GetValue(Node, 'ListFilesInThread', gListFilesInThread);
      gLoadIconsSeparately := GetValue(Node, 'LoadIconsSeparately', gLoadIconsSeparately);
      gDriveBlackList := GetValue(Node, 'DriveBlackList', gDriveBlackList);
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
        gToolBarIconSize := GetValue(SubNode, 'SmallIconSize', gToolBarIconSize);
        gToolBarSmallIcons := GetValue(SubNode, 'SmallIcons', gToolBarSmallIcons);
      end;
      gDriveBar1 := GetValue(Node, 'DriveBar1', gDriveBar1);
      gDriveBar2 := GetValue(Node, 'DriveBar2', gDriveBar2);
      gDriveBarFlat := GetValue(Node, 'DriveBarFlat', gDriveBarFlat);
      gDriveMenuButton := GetValue(Node, 'DriveMenuButton', gDriveMenuButton);
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
      gQuickSearch := GetAttr(Node, 'Enabled', gQuickSearch);
      gQuickSearchMode := TShiftState(GetValue(Node, 'Mode', Integer(gQuickSearchMode)));
      gQuickSearchMatchBeginning := GetValue(Node, 'MatchBeginning', gQuickSearchMatchBeginning);
      gQuickSearchMatchEnding := GetValue(Node, 'MatchEnding', gQuickSearchMatchEnding);
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
    SetValue(Node, 'OnlyOneAppInstance', gOnlyOneAppInstance);
    SetValue(Node, 'LynxLike', gLynxLike);
    SetValue(Node, 'SortCaseSensitive', gSortCaseSensitive);
    SetValue(Node, 'SortNatural', gSortNatural);
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
    SetValue(Node, 'ListFilesInThread', gListFilesInThread);
    SetValue(Node, 'LoadIconsSeparately', gLoadIconsSeparately);
    SetValue(Node, 'DriveBlackList', gDriveBlackList);

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
    gColorExt.Save(gConfig, Node);

    { ToolTips page }
    Node := FindNode(Root, 'ToolTips', True);
    SetValue(Node, 'ShowToolTipMode', Integer(gShowToolTipMode));
    gFileInfoToolTip.Save(gConfig, Node);

    { Layout page }
    Node := FindNode(Root, 'Layout', True);
    SetValue(Node, 'MainMenu', gMainMenu);
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
    SetValue(Node, 'DriveFreeSpace', gDriveFreeSpace);
    SetValue(Node, 'DriveIndicator', gDriveInd);
    SetValue(Node, 'ProgressInMenuBar', gProgInMenuBar);
    SetValue(Node, 'PanelOfOperationsInBackground', gPanelOfOp);
    SetValue(Node, 'HorizontalFilePanels', gHorizontalFilePanels);

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
    SetAttr(Root, 'Configuration/Save', gSaveConfiguration);
    SetAttr(Root, 'History/SearchReplaceHistory/Save', gSaveSearchReplaceHistory);
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

function IsFileSystemWatcher: Boolean;
begin
  Result := ([watch_file_name_change, watch_attributes_change] * gWatchDirs <> []);
end;

initialization

finalization
  DestroyGlobs;
end.
