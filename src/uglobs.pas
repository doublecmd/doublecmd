{
   Double Commander
   -------------------------------------------------------------------------
   Globals variables and some consts

   Copyright (C) 2008-2020 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.

   Original comment:
   ------------------------------------------------------------
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   Globals variables and some consts

   contributors:

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Copyright (C) 2008  Vitaly Zotov (vitalyzotov@mail.ru)
   Copyright (C) 2006-2019 Alexander Koblov (alexx2000@mail.ru)

}

unit uGlobs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Grids, Types, uExts, uColorExt, Graphics,
  LCLVersion, DCClassesUtf8, uMultiArc, uColumns, uHotkeyManager, uSearchTemplate,
  uFileSourceOperationOptions, uWFXModule, uWCXModule, uWDXModule, uwlxmodule,
  udsxmodule, DCXmlConfig, uInfoToolTip, fQuickSearch, uTypes, uClassesEx,
  uHotDir, uSpecialDir, SynEdit, SynEditTypes, uFavoriteTabs, fTreeViewMenu, uConvEncoding;

type
  { Configuration options }
  TSortConfigurationOptions = (scoClassicLegacy, scoAlphabeticalButLanguage);
  TConfigurationTreeState = (ctsFullExpand, ctsFullCollapse);

  { Log options }
  TLogOptions = set of (log_cp_mv_ln, log_delete, log_dir_op, log_arc_op,
                        log_vfs_op, log_success, log_errors, log_info,
                        log_start_shutdown, log_commandlineexecution);
  { Watch dirs options }
  TWatchOptions = set of (watch_file_name_change, watch_attributes_change,
                          watch_only_foreground, watch_exclude_dirs);
  { Tabs options }
  TTabsOptions = set of (tb_always_visible, tb_multiple_lines, tb_same_width,
                         tb_text_length_limit, tb_confirm_close_all,
                         tb_close_on_dbl_click, tb_open_new_in_foreground,
                         tb_open_new_near_current, tb_show_asterisk_for_locked,
                         tb_activate_panel_on_click, tb_show_close_button,
                         tb_close_duplicate_when_closing,
                         tb_close_on_doubleclick, tb_show_drive_letter,
                         tb_reusing_tab_when_possible,
                         tb_confirm_close_locked_tab,
                         tb_keep_renamed_when_back_normal);

  TTabsOptionsDoubleClick = (tadc_Nothing, tadc_CloseTab, tadc_FavoriteTabs, tadc_TabsPopup);

  TTabsPosition = (tbpos_top, tbpos_bottom);
  { Show icons mode }
  TShowIconsMode = (sim_none, sim_standart, sim_all, sim_all_and_exe);
  { Custom icons mode }
  TCustomIconsMode = set of (cimDrive, cimFolder, cimArchive);
  TScrollMode = (smLineByLineCursor, smLineByLine, smPageByPage);
  { Sorting directories mode }
  TSortFolderMode = (sfmSortNameShowFirst, sfmSortLikeFileShowFirst, sfmSortLikeFile);
  { Where to insert new files in the filelist }
  TNewFilesPosition = (nfpTop, nfpTopAfterDirectories, nfpSortedPosition, nfpBottom);
  { Where to move updated files in the filelist }
  TUpdatedFilesPosition = (ufpSameAsNewFiles, ufpSortedPosition, ufpNoChange);
  { How initially progress is shown for file operations }
  TFileOperationsProgressKind = (fopkSeparateWindow, fopkSeparateWindowMinimized, fopkOperationsPanel);
  { Operations with confirmation }
  TFileOperationsConfirmation = (focCopy, focMove, focDelete, focDeleteToTrash, focVerifyChecksum);
  TFileOperationsConfirmations = set of TFileOperationsConfirmation;

  { Multi-Rename }
  TMulRenLaunchBehavior = (mrlbLastMaskUnderLastOne, mrlbLastPreset, mrlbFreshNew);
  TMulRenExitModifiedPreset = (mrempIgnoreSaveLast, mrempPromptUser, mrempSaveAutomatically);
  TMulRenSaveRenamingLog = (mrsrlPerPreset, mrsrlAppendSameLog);

  { Internal Associations}
  //What the use wish for the context menu
  // uwcmComplete : DEFAULT, or user specifically wish the "Windows' one + the actions".
  // uwcmJustDCAction : User specifically wish only the actions, even if default set is not it.
  TUserWishForContextMenu = (uwcmComplete, uwcmJustDCAction);

  TExternalTool = (etViewer, etEditor, etDiffer);
  TExternalToolOptions = record
    Enabled: Boolean;
    Path: String;
    Parameters: String;
    RunInTerminal: Boolean;
    KeepTerminalOpen: Boolean;
  end;
  TExternalToolsOptions = array[TExternalTool] of TExternalToolOptions;
  TResultingFramePositionAfterCompare = (rfpacActiveOnLeft, rfpacLeftOnLeft);

  //Related with the Viewer
  TViewerPaintTool = (vptPen, vptRectangle, vptEllipse);

  TPluginType = (ptDSX, ptWCX, ptWDX, ptWFX, ptWLX); //*Important: Keep that order to to fit with procedures LoadXmlConfig/SaveXmlConfig when we save/restore widths of "TfrmTweakPlugin".
  TWcxCfgViewMode = (wcvmByPlugin, wcvmByExtension);

  TDCFont = (dcfMain, dcfEditor, dcfViewer, dcfViewerBook, dcfLog, dcfConsole, dcfPathEdit, dcfSearchResults, dcfFunctionButtons, dcfTreeViewMenu);
  TDCFontOptions = record
    Usage: string;
    Name: string;
    Size: Integer;
    Style: TFontStyles;
    Quality: TFontQuality;
    MinValue: integer;
    MaxValue: integer;
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

  tDesiredDropTextFormat=record
    Name:string;
    DesireLevel:longint;
  end;

  tDuplicatedRename = (drLegacyWithCopy, drLikeWindows7, drLikeTC);

  TBriefViewMode = (bvmFixedWidth, bvmFixedCount, bvmAutoSize);

  TFiltersOnNewSearch = (fonsKeep, fonsClear, fonsPrompt);

  THotKeySortOrder = (hksoByCommand, hksoByHotKeyGrouped, hksoByHotKeyOnePerRow);

  TToolTipMode = (tttmCombineDcSystem, tttmDcSystemCombine, tttmDcIfPossThenSystem, tttmDcOnly, tttmSystemOnly);
  TToolTipHideTimeOut = (ttthtSystem, tttht1Sec, tttht2Sec, tttht3Sec, tttht5Sec, tttht10Sec, tttht30Sec, tttht1Min, ttthtNeverHide);

  TConfigFilenameStyle = (pfsAbsolutePath, pfsRelativeToDC, pfsRelativeToFollowingPath);

  tToolbarPathModifierElement = (tpmeIcon, tpmeCommand, tpmeStartingPath);
  tToolbarPathModifierElements = set of tToolbarPathModifierElement;

  tFileAssocPathModifierElement = (fameIcon, fameCommand, fameStartingPath);
  tFileAssocPathModifierElements = set of tFileAssocPathModifierElement;

  tHotDirPathModifierElement = (hdpmSource, hdpmTarget);
  tHotDirPathModifierElements = set of tHotDirPathModifierElement;

const
  { Default hotkey list version number }
  hkVersion = 52;
  // 52 - In "Main" context, add shortcut "Ctrl+Shift+B" for "cm_FlatViewSel".
  // 51 - In "Multi-Rename" context, added the "Shift+F4" shortcut for the "cm_EditNewNames".
  // 50 - To load shortcut keys for the "Multi-Rename" which is now driven with "cm_Actions".
  // 49 - In "Viewer" context, added the "F6" for "cm_ShowCaret".
  // 48 - In "Viewer" context, added the "CTRL+P" for the "cm_Print".
  // 47 - In "Copy/Move Dialog" context, add the shortcuts "F5" and "F6" for "cm_ToggleSelectionInName".
  // 46 - In "Main" context, add shortcut "Shift+Tab" for "cm_FocusTreeView".
  // 45 - Automatically add default shortcuts to internal editor (shortcuts had not converted correctly without hkVersion update)
  // 44 - Attempt to repair shortcut keys for "cm_ShowCmdLineHistory" in "Main" context.
  // 43 - To load shortcut keys for the "Synchronize Directories" which is driven with "cm_Actions".
  // 42 - In "Find Files" context, added the "CTRL+TAB" and "CTRL+SHIFT+TAB" shortcut keys for the "cm_PageNext" and "cm_PagePrev" commands.
  // 41 - Keyboard shortcuts to change encoding in Viewer (A, S, Z and X).
  // 40 - In "Main" context, added the "Ctrl+Shift+F7" for "cm_AddNewSearch".
  //      In "Find Files" context, changed "cm_Start" that was "Enter" for "F9".
  //      In "Find Files" context, added "Alt+F7" as a valid alternative for "cm_PageStandard".

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
  // 6   - changed Behaviours/ShortFileSizeFormat to Behaviours/FileSizeFormat
  // 7   - changed Viewer/SaveThumbnails to Thumbnails/Save
  // 8   - changed Behaviours/BriefViewFileExtAligned to FilesViews/BriefView/FileExtAligned
  // 9   - few new options regarding tabs
  // 10  - changed Icons/CustomDriveIcons to Icons/CustomIcons
  // 11  - During the last 2-3 years the default font for search result was set in file, not loaded and different visually than was was stored.
  // 12  - Split Behaviours/HeaderFooterSizeFormat to Behaviours/HeaderSizeFormat and Behaviours/FooterSizeFormat
  //       Loading a config prior of version 11 should ignore that setting and keep default.
  ConfigVersion = 12;

  // Configuration related filenames
  sMULTIARC_FILENAME = 'multiarc.ini';

  TKeyTypingModifierToShift: array[TKeyTypingModifier] of TShiftState =
    ([], [ssAlt], [ssCtrl, ssAlt]);

  { Related with the drop of text over panels}
  NbOfDropTextFormat = 4;
  DropTextRichText_Index=0;
  DropTextHtml_Index=1;
  DropTextUnicode_Index=2;
  DropTextSimpleText_Index=3;

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
  gTweakPluginWidth: array[ord(ptDSX)..ord(ptWLX)] of integer;
  gTweakPluginHeight: array[ord(ptDSX)..ord(ptWLX)] of integer;
  gPluginInAutoTweak: boolean;
  gWCXConfigViewMode: TWcxCfgViewMode;
  gPluginFilenameStyle: TConfigFilenameStyle = pfsAbsolutePath;
  gPluginPathToBeRelativeTo: string = '%COMMANDER_PATH%';
  
  { MultiArc addons }
  gMultiArcList: TMultiArcList;

  { Columns Set }
  ColSet:TPanelColumnsList;
  
  { Layout page }
  gMainMenu,
  gButtonBar,
  gToolBarFlat,
  gMiddleToolBar,
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
  gUpperCaseDriveLetter,
  gShowColonAfterDrive,
  gShortFormatDriveInfo: Boolean;
  gDrivesListButtonOptions: TDrivesListButtonOptions;
  gSeparateTree: Boolean;

  { Toolbar }
  gMiddleToolBarFlat,
  gMiddleToolBarShowCaptions,
  gMiddleToolbarReportErrorWithCommands: Boolean;
  gMiddleToolBarButtonSize,
  gMiddleToolBarIconSize,
  gToolBarButtonSize,
  gToolBarIconSize: Integer;
  gToolBarShowCaptions: Boolean;
  gToolbarReportErrorWithCommands: boolean;
  gToolbarFilenameStyle: TConfigFilenameStyle;
  gToolbarPathToBeRelativeTo: string;
  gToolbarPathModifierElements: tToolbarPathModifierElements;

  gRepeatPassword:Boolean;  // repeat password when packing files
  gDirHistoryCount:Integer; // how many history we remember
  gShowSystemFiles:Boolean;
  gRunInTermStayOpenCmd: String;
  gRunInTermStayOpenParams: String;
  gRunInTermCloseCmd: String;
  gRunInTermCloseParams: String;
  gRunTermCmd: String;
  gRunTermParams: String;
  gSortCaseSensitivity: TCaseSensitivity;
  gSortNatural: Boolean;
  gSortSpecial: Boolean;
  gSortFolderMode: TSortFolderMode;
  gNewFilesPosition: TNewFilesPosition;
  gUpdatedFilesPosition: TUpdatedFilesPosition;
  gLynxLike:Boolean;
  gFirstTextSearch: Boolean;
  gExtraLineSpan: Integer;
  gFolderPrefix,
  gFolderPostfix: String;

  { Mouse }
  gMouseSelectionEnabled: Boolean;
  gMouseSelectionButton: Integer;
  gMouseSingleClickStart: Integer;
  gMouseSelectionIconClick: Integer;

  gAutoFillColumns: Boolean;
  gAutoSizeColumn: Integer;
  gColumnsAutoSaveWidth: Boolean;
  gColumnsTitleStyle: TTitleStyle;
  gCustomColumnsChangeAllColumns: Boolean;
  
  gSpecialDirList:TSpecialDirList=nil;
  gDirectoryHotlist:TDirectoryHotlist;
  gHotDirAddTargetOrNot: Boolean;
  gHotDirFullExpandOrNot: Boolean;
  gShowPathInPopup: boolean;
  gShowOnlyValidEnv: boolean = TRUE;
  gWhereToAddNewHotDir: TPositionWhereToAddHotDir;
  gHotDirFilenameStyle: TConfigFilenameStyle;
  gHotDirPathToBeRelativeTo: string;
  gHotDirPathModifierElements: tHotDirPathModifierElements;

  glsDirHistory:TStringListEx;
  glsCmdLineHistory: TStringListEx;
  glsMaskHistory : TStringListEx;
  glsSearchHistory : TStringListEx;
  glsSearchPathHistory : TStringListEx;
  glsReplaceHistory : TStringListEx;
  glsReplacePathHistory : TStringListEx;
  glsCreateDirectoriesHistory : TStringListEx;
  glsRenameNameMaskHistory : TStringListEx;
  glsRenameExtMaskHistory : TStringListEx;
  glsSearchDirectories: TStringList;
  glsSearchExcludeFiles: TStringList;
  glsSearchExcludeDirectories: TStringList;
  glsIgnoreList : TStringListEx;
  gOnlyOneAppInstance,
  gCutTextToColWidth : Boolean;
  gExtendCellWidth : Boolean;
  gSpaceMovesDown: Boolean;
  gScrollMode: TScrollMode;
  gWheelScrollLines: Integer;
  gAlwaysShowTrayIcon: Boolean;
  gMinimizeToTray: Boolean;
  gFileSizeFormat: TFileSizeFormat;
  gHeaderSizeFormat: TFileSizeFormat;
  gFooterSizeFormat: TFileSizeFormat;
  gOperationSizeFormat: TFileSizeFormat;
  gFileSizeDigits: Integer;
  gHeaderDigits: Integer;
  gFooterDigits: Integer;
  gOperationSizeDigits: Integer;
  gSizeDisplayUnits: array[LOW(TFileSizeFormat) .. HIGH(TFileSizeFormat)] of string;
  gDateTimeFormat : String;
  gDriveBlackList: String;
  gDriveBlackListUnmounted: Boolean; // Automatically black list unmounted devices
  gListFilesInThread: Boolean;
  gLoadIconsSeparately: Boolean;
  gDelayLoadingTabs: Boolean;
  gHighlightUpdatedFiles: Boolean;
  gLastUsedPacker: String;
  gLastDoAnyCommand: String;
  gbMarkMaskCaseSensitive: boolean;
  gbMarkMaskIgnoreAccents: boolean;
  gMarkMaskFilterWindows: boolean;
  gMarkShowWantedAttribute: boolean;
  gMarkDefaultWantedAttribute: string;
  gMarkLastWantedAttribute: string;

  { Favorite Tabs }
  gFavoriteTabsUseRestoreExtraOptions: boolean;
  gFavoriteTabsList: TFavoriteTabsList;
  gWhereToAddNewFavoriteTabs: TPositionWhereToAddFavoriteTabs;
  gFavoriteTabsFullExpandOrNot: boolean;
  gFavoriteTabsGoToConfigAfterSave: boolean;
  gFavoriteTabsGoToConfigAfterReSave: boolean;
  gDefaultTargetPanelLeftSaved: TTabsConfigLocation;
  gDefaultTargetPanelRightSaved: TTabsConfigLocation;
  gDefaultExistingTabsToKeep: TTabsConfigLocation;
  gFavoriteTabsSaveDirHistory: boolean;

  { Brief view page }
  gBriefViewFixedWidth: Integer;
  gBriefViewFixedCount: Integer;
  gBriefViewMode: TBriefViewMode;
  gBriefViewFileExtAligned: Boolean;

  { Tools page }
  gExternalTools: TExternalToolsOptions;

  gResultingFramePositionAfterCompare:TResultingFramePositionAfterCompare;
  gLuaLib:String;
  gExts:TExts;
  gColorExt:TColorExt;
  gFileInfoToolTip: TFileInfoToolTip;
  gFileInfoToolTipValue: array[0..ord(ttthtNeverHide)] of integer = (-1, 1000, 2000, 3000, 5000, 10000, 30000, 60000, integer.MaxValue);

  { Fonts page }
  gFonts: TDCFontsOptions;

  { File panels color page }
  gUseCursorBorder: Boolean;
  gCursorBorderColor: TColor;
  gUseFrameCursor: Boolean;
  gForeColor,  //text color
  gBackColor, //Background color
  gBackColor2, //Background color 2
  gMarkColor,  // Mark color
  gCursorColor, //Cursor color
  gCursorText,  //text color under cursor
  gInactiveCursorColor, //Inactive cursor color
  gInactiveMarkColor: TColor; //Inactive Mark color
  gUseInvertedSelection: Boolean;
  gUseInactiveSelColor: Boolean;
  gAllowOverColor: Boolean;
  gBorderFrameWidth :integer;

  gPathActiveColor,
  gPathActiveFontColor,
  gPathInactiveColor,
  gPathInactiveFontColor: TColor;

  gInactivePanelBrightness: Integer; // 0 .. 100 (black .. full color)
  gIndUseGradient : Boolean; // use gradient on drive label
  gIndForeColor, // foreColor of use space on drive label
  gIndBackColor: TColor; // backColor of free space on drive label

  gLogInfoColor,
  gLogErrorColor,
  gLogSuccessColor: TColor;

  gSyncLeftColor,
  gSyncRightColor,
  gSyncUnknownColor: TColor;

  gShowIcons: TShowIconsMode;
  gShowIconsNew: TShowIconsMode;
  gIconOverlays : Boolean;
  gIconsSize,
  gIconsSizeNew : Integer;
  gDiskIconsSize : Integer;
  gDiskIconsAlpha : Integer;
  gToolIconsSize: Integer;
  gFiOwnDCIcon : PtrInt;
  gIconsExclude: Boolean;
  gIconsExcludeDirs: String;
  gPixelsPerInch: Integer;
  gCustomIcons : TCustomIconsMode; // for use custom icons under windows
  gIconsInMenus: Boolean;
  gIconsInMenusSize,
  gIconsInMenusSizeNew: Integer;
  gShowHiddenDimmed: Boolean;
  gIconTheme: String;

  { Keys page }
  gKeyTyping: array[TKeyTypingModifier] of TKeyTypingAction;

  { File operations page }
  gLongNameAlert: Boolean;
  gCopyBlockSize : Integer;
  gHashBlockSize : Integer;
  gUseMmapInSearch : Boolean;
  gPartialNameSearch: Boolean;
  gInitiallyClearFileMask : Boolean;
  gNewSearchClearFiltersAction : TFiltersOnNewSearch;
  gShowMenuBarInFindFiles : Boolean;
  gSkipFileOpError: Boolean;
  gTypeOfDuplicatedRename: tDuplicatedRename;
  gDropReadOnlyFlag : Boolean;
  gWipePassNumber: Integer;
  gProcessComments: Boolean;
  gShowCopyTabSelectPanel:boolean;
  gUseTrash : Boolean; // if using delete to trash by default
  gRenameSelOnlyName:boolean;
  gShowDialogOnDragDrop: Boolean;
  gDragAndDropDesiredTextFormat:array[0..pred(NbOfDropTextFormat)] of tDesiredDropTextFormat;
  gDragAndDropAskFormatEachTime: Boolean;
  gDragAndDropTextAutoFilename: Boolean;
  gDragAndDropSaveUnicodeTextInUFT8: Boolean;
  gNtfsHourTimeDelay: Boolean;
  gAutoExtractOpenMask: String;
  gFileOperationsProgressKind: TFileOperationsProgressKind;
  gFileOperationsConfirmations: TFileOperationsConfirmations;

  { Multi-Rename}
  gMulRenShowMenuBarOnTop : boolean;
  gMulRenInvalidCharReplacement : string;
  gMulRenLaunchBehavior : TMulRenLaunchBehavior;
  gMulRenExitModifiedPreset : TMulRenExitModifiedPreset;
  gMulRenSaveRenamingLog : TMulRenSaveRenamingLog;
  gMulRenLogFilename : string;
  gMultRenDailyIndividualDirLog: boolean;
  gMulRenFilenameWithFullPathInLog:boolean;
  gMulRenPathRangeSeparator: string;

  { Folder tabs page }
  gDirTabOptions : TTabsOptions;
  gDirTabActionOnDoubleClick : TTabsOptionsDoubleClick;
  gDirTabLimit : Integer;
  gDirTabPosition : TTabsPosition;

  { Log page }
  gLogFile : Boolean;
  gLogFileWithDateInName : Boolean;
  gLogFileCount: Integer;
  gLogFileName : String;
  gLogOptions : TLogOptions;

  { Configuration page }
  gUseConfigInProgramDir,
  gUseConfigInProgramDirNew,
  gSaveConfiguration,
  gSaveWindowState,
  gSaveFolderTabs,
  gSaveSearchReplaceHistory,
  gSaveDirHistory,
  gSaveCmdLineHistory,
  gSaveFileMaskHistory : Boolean;
  gSortOrderOfConfigurationOptionsTree: TSortConfigurationOptions;
  gCollapseConfigurationOptionsTree: TConfigurationTreeState;
  
  { Quick Search page }
  gQuickSearchOptions: TQuickSearchOptions;
  gQuickFilterAutoHide: Boolean;
  gQuickFilterSaveSessionModifications: Boolean;

  { Misc page }
  gGridVertLine,
  gGridHorzLine,
  gShowWarningMessages,
  gDirBrackets,
  gInplaceRename,
  gInplaceRenameButton,
  gDblClickToParent,
  gGoToRoot: Boolean;
  gActiveRight: Boolean;
  gShowToolTip: Boolean;
  gShowToolTipMode: TToolTipMode;
  gToolTipHideTimeOut: TToolTipHideTimeOut;
  gThumbSize: TSize;
  gThumbSave: Boolean;
  gSearchDefaultTemplate: String;
  gSearchTemplateList: TSearchTemplateList;
  gDescCreateUnicode: Boolean;
  gDescReadEncoding: TMacroEncoding;
  gDescWriteEncoding: TMacroEncoding;

  { Auto refresh page }
  gWatchDirs: TWatchOptions;
  gWatchDirsExclude: String;
  gWatcherMode: TWatcherMode;

  { Ignore list page }
  gIgnoreListFileEnabled: Boolean;
  gIgnoreListFile: String;

  {HotKey Manager}
  HotMan:THotKeyManager;
  gNameSCFile: string;
  gHotKeySortOrder: THotKeySortOrder;
  gUseEnterToCloseHotKeyEditor: boolean;
  
  {Copy/Move operation options}
  gOperationOptionSymLinks: TFileSourceOperationOptionSymLink;
  gOperationOptionCorrectLinks: Boolean;
  gOperationOptionCopyOnWrite: TFileSourceOperationOptionGeneral;
  gOperationOptionFileExists: TFileSourceOperationOptionFileExists;
  gOperationOptionDirectoryExists: TFileSourceOperationOptionDirectoryExists;
  gOperationOptionSetPropertyError: TFileSourceOperationOptionSetPropertyError;
  gOperationOptionReserveSpace: Boolean;
  gOperationOptionCheckFreeSpace: Boolean;
  gOperationOptionCopyAttributes: Boolean;
  gOperationOptionCopyXattributes: Boolean;
  gOperationOptionCopyTime: Boolean;
  gOperationOptionVerify: Boolean;
  gOperationOptionCopyOwnership: Boolean;
  gOperationOptionCopyPermissions: Boolean;
  gOperationOptionExcludeEmptyDirectories: Boolean;

  {Extract dialog options}
  gExtractOverwrite: Boolean;

  {Error file}
  gErrorFile: String;

  {Viewer}
  gPreviewVisible,
  gImageStretch: Boolean;
  gImageExifRotate: Boolean;
  gImageStretchOnlyLarge: Boolean;
  gImageCenter: Boolean;
  gCopyMovePath1,
  gCopyMovePath2,
  gCopyMovePath3,
  gCopyMovePath4,
  gCopyMovePath5: String;
  gImagePaintMode: TViewerPaintTool;
  gImagePaintWidth,
  gColCount,
  gViewerMode,
  gMaxTextWidth,
  gTabSpaces : Integer;
  gImagePaintColor,
  gBookBackgroundColor,
  gBookFontColor: TColor;
  gTextPosition:PtrInt;
  gPrintMargins: TRect;
  gShowCaret: Boolean;

  { Editor }
  gEditWaitTime: Integer;
  gEditorSynEditOptions: TSynEditorOptions;
  gEditorSynEditTabWidth,
  gEditorSynEditRightEdge: Integer;

  { Differ }
  gDifferIgnoreCase,
  gDifferKeepScrolling,
  gDifferLineDifferences,
  gDifferPaintBackground,
  gDifferIgnoreWhiteSpace: Boolean;
  gDifferAddedColor: TColor;
  gDifferDeletedColor: TColor;
  gDifferModifiedColor: TColor;

  {SyncDirs}
  gSyncDirsSubdirs,
  gSyncDirsByContent,
  gSyncDirsAsymmetric,
  gSyncDirsIgnoreDate,
  gSyncDirsAsymmetricSave,
  gSyncDirsShowFilterCopyRight,
  gSyncDirsShowFilterEqual,
  gSyncDirsShowFilterNotEqual,
  gSyncDirsShowFilterCopyLeft,
  gSyncDirsShowFilterDuplicates,
  gSyncDirsShowFilterSingles: Boolean;
  gSyncDirsFileMask: string;

  { Internal Associations}
  gFileAssociationLastCustomAction: string;
  gOfferToAddToFileAssociations: boolean;
  gExtendedContextMenu: boolean;
  gOpenExecuteViaShell: boolean;
  gExecuteViaTerminalClose: boolean;
  gExecuteViaTerminalStayOpen: boolean;
  gIncludeFileAssociation: boolean;
  gFileAssocFilenameStyle: TConfigFilenameStyle;
  gFileAssocPathToBeRelativeTo: string;
  gFileAssocPathModifierElements: tFileAssocPathModifierElements;

  { TreeViewMenu }
  gUseTreeViewMenuWithDirectoryHotlistFromMenuCommand: boolean;
  gUseTreeViewMenuWithDirectoryHotlistFromDoubleClick: boolean;
  gUseTreeViewMenuWithFavoriteTabsFromMenuCommand: boolean;
  gUseTreeViewMenuWithFavoriteTabsFromDoubleClick: boolean;
  gUseTreeViewMenuWithDirHistory: boolean;
  gUseTreeViewMenuWithViewHistory: boolean;
  gUseTreeViewMenuWithCommandLineHistory: boolean;
  gTreeViewMenuUseKeyboardShortcut: boolean;
  gTVMBackgroundColor: TColor;
  gTVMShortcutColor: TColor;
  gTVMNormalTextColor: TColor;
  gTVMSecondaryTextColor: TColor;
  gTVMFoundTextColor: TColor;
  gTVMUnselectableTextColor: TColor;
  gTVMCursorColor: TColor;
  gTVMShortcutUnderCursor: TColor;
  gTVMNormalTextUnderCursor: TColor;
  gTVMSecondaryTextUnderCursor: TColor;
  gTVMFoundTextUnderCursor: TColor;
  gTVMUnselectableUnderCursor: TColor;
  gTreeViewMenuOptions: array [0..(ord(tvmcLASTONE)-2)] of TTreeViewMenuOptions;
  gTreeViewMenuShortcutExit: boolean;
  gTreeViewMenuSingleClickExit: boolean;
  gTreeViewMenuDoubleClickExit: boolean;
  crArrowCopy: Integer = 1;
  crArrowMove: Integer = 2;
  crArrowLink: Integer = 3;

  { TotalCommander Import/Export }
  {$IFDEF MSWINDOWS}
  gTotalCommanderExecutableFilename:string;
  gTotalCommanderConfigFilename:string;
  gTotalCommanderToolbarPath:string;
  {$ENDIF}

function LoadConfig: Boolean;
function InitGlobs: Boolean;
function LoadGlobs: Boolean;
procedure SaveGlobs;
procedure LoadXmlConfig;
procedure SaveXmlConfig;

procedure LoadDefaultHotkeyBindings;

function InitPropStorage(Owner: TComponent): TIniPropStorageEx;

procedure FontToFontOptions(Font: TFont; var Options: TDCFontOptions);
procedure FontOptionsToFont(Options: TDCFontOptions; Font: TFont);


function GetKeyTypingAction(ShiftStateEx: TShiftState): TKeyTypingAction;
function IsFileSystemWatcher: Boolean;
function GetValidDateTimeFormat(const aFormat, ADefaultFormat: string): string;

procedure RegisterInitialization(InitProc: TProcedure);

const
  cMaxStringItems=50;
  
var
  gConfig: TXmlConfig = nil;

implementation

uses
   LCLProc, LCLType, Dialogs, Laz2_XMLRead, LazUTF8, uExifWdx, uSynDiffControls,
   uGlobsPaths, uLng, uShowMsg, uFileProcs, uOSUtils, uFindFiles,
   uDCUtils, fMultiRename, uFile, uDCVersion, uDebug, uFileFunctions,
   uDefaultPlugins, Lua, uKeyboard, DCOSUtils, DCStrUtils, uPixMapManager
   {$IF DEFINED(MSWINDOWS)}
    , ShlObj
   {$ENDIF}
   {$if lcl_fullversion >= 2010000}
   , SynEditMiscClasses
   {$endif}
   ;

const
  TKeyTypingModifierToNodeName: array[TKeyTypingModifier] of String =
    ('NoModifier', 'Alt', 'CtrlAlt');

type
  TLoadConfigProc = function(var ErrorMessage: String): Boolean;

var
  DefaultDateTimeFormat: String;
  // Double Commander version
  // loaded from configuration file
  gPreviousVersion: String = '';
  FInitList: array of TProcedure;

function LoadConfigCheckErrors(LoadConfigProc: TLoadConfigProc;
                               ConfigFileName: String;
                               var ErrorMessage: String): Boolean;
  procedure AddMsg(Msg, eMsg: String);
  begin
    AddStrWithSep(ErrorMessage, Msg + ':', LineEnding + LineEnding);
    AddStrWithSep(ErrorMessage, ConfigFileName, LineEnding);
    if eMsg <> EmptyStr then
      AddStrWithSep(ErrorMessage, eMsg, LineEnding);
  end;
begin
  Result := False;
  try
    Result := LoadConfigProc(ErrorMessage);
  except
    // If the file does not exist or is empty,
    // simply default configuration is applied.
    on EXmlConfigNotFound do
      Result := True;
    on EXmlConfigEmpty do
      Result := True;
    on e: EFOpenError do
      AddMsg(rsMsgErrEOpen, e.Message);
    on e: EStreamError do
      AddMsg(rsMsgErrERead, e.Message);
    on e: EXMLReadError do
      AddMsg(rsMsgInvalidFormatOfConfigurationFile, e.Message);
  end;
end;

type
  TSaveCfgProc = procedure;

procedure SaveWithCheck(SaveProc: TSaveCfgProc; CfgDescription: String; var ErrMsg: String);
begin
  try
    SaveProc;
  except
    on E: EStreamError do
      ErrMsg := ErrMsg + 'Cannot save ' + CfgDescription + ': ' + e.Message;
  end;
end;

procedure SaveCfgIgnoreList;
var
  FileName: String;
begin
  if gIgnoreListFileEnabled then
  begin
    FileName:= ReplaceEnvVars(gIgnoreListFile);
    mbForceDirectory(ExtractFileDir(FileName));
    glsIgnoreList.SaveToFile(FileName);
  end;
end;

procedure SaveCfgMainConfig;
begin
  SaveXmlConfig;

  // Force saving config to file.
  gConfig.Save;
end;

function AskUserOnError(var ErrorMessage: String): Boolean;
begin
  // Show error messages.
  if ErrorMessage <> EmptyStr then
  begin
    Result := QuestionDlg(Application.Title + ' - ' + rsMsgErrorLoadingConfiguration,
                          ErrorMessage, mtWarning,
                          [1, rsDlgButtonContinue, 'isdefault',
                           2, rsDlgButtonExitProgram], 0) = 1;
    // Reset error message.
    ErrorMessage := '';
  end
  else
    Result := True;
end;

function LoadGlobalConfig(var {%H-}ErrorMessage: String): Boolean;
begin
  Result := gConfig.Load;
end;

function LoadExtsConfig(var {%H-}ErrorMessage: String): Boolean;
begin
  gExts.Load;
  Result := True;
end;

function LoadHotManConfig(var {%H-}ErrorMessage: String): Boolean;
begin
  HotMan.Load(gpCfgDir + gNameSCFile);
  Result := True;
end;

function LoadMultiArcConfig(var {%H-}ErrorMessage: String): Boolean;
begin
  gMultiArcList.LoadFromFile(gpCfgDir + sMULTIARC_FILENAME);
  Result := True;
end;

function LoadHistoryConfig(var {%H-}ErrorMessage: String): Boolean;
var
  Root: TXmlNode;
  History: TXmlConfig;

  procedure LoadHistory(const NodeName: String; HistoryList: TStrings; LoadObj: Boolean = False);
  var
    Idx: Integer;
    Node: TXmlNode;
  begin
    Node := History.FindNode(Root, NodeName);
    if Assigned(Node) then
    begin
      HistoryList.Clear;
      Node := Node.FirstChild;
      while Assigned(Node) do
      begin
        if Node.CompareName('Item') = 0 then
        begin
          Idx:= HistoryList.Add(History.GetContent(Node));
          if LoadObj then begin
            HistoryList.Objects[Idx]:= TObject(UIntPtr(History.GetAttr(Node, 'Tag', 0)));
          end;
          if HistoryList.Count >= cMaxStringItems then Break;
        end;
        Node := Node.NextSibling;
      end;
    end;
  end;

begin
  Result:= False;
  History:= TXmlConfig.Create(gpCfgDir + 'history.xml', True);
  try
    Root:= History.FindNode(History.RootNode, 'History');
    if Assigned(Root) then
    begin
      LoadHistory('Navigation', glsDirHistory);
      LoadHistory('CommandLine', glsCmdLineHistory);
      LoadHistory('FileMask', glsMaskHistory);
      LoadHistory('SearchText', glsSearchHistory, True);
      LoadHistory('SearchTextPath', glsSearchPathHistory);
      LoadHistory('ReplaceText', glsReplaceHistory);
      LoadHistory('ReplaceTextPath', glsReplacePathHistory);
      LoadHistory('CreateDirectories', glsCreateDirectoriesHistory);
      LoadHistory('RenameNameMask', glsRenameNameMaskHistory);
      LoadHistory('RenameExtMask', glsRenameExtMaskHistory);
      LoadHistory('SearchDirectories', glsSearchDirectories);
      LoadHistory('SearchExcludeFiles', glsSearchExcludeFiles);
      LoadHistory('SearchExcludeDirectories', glsSearchExcludeDirectories);
    end;
    Result:= True;
  finally
    History.Free;
  end;
end;

procedure SaveHistoryConfig;
var
  Root: TXmlNode;
  History: TXmlConfig;

  procedure SaveHistory(const NodeName: String; HistoryList: TStrings; SaveObj: Boolean = False);
  var
    I: Integer;
    Node, SubNode: TXmlNode;
  begin
    Node := History.FindNode(Root, NodeName, True);
    History.ClearNode(Node);
    for I:= 0 to HistoryList.Count - 1 do
    begin
      SubNode := History.AddNode(Node, 'Item');
      History.SetContent(SubNode, HistoryList[I]);
      if SaveObj then begin
        History.SetAttr(SubNode, 'Tag', UInt32(UIntPtr(HistoryList.Objects[I])));
      end;
      if I >= cMaxStringItems then Break;
    end;
  end;

begin
  History:= TXmlConfig.Create(gpCfgDir + 'history.xml');
  try
    Root:= History.FindNode(History.RootNode, 'History', True);
    if gSaveDirHistory then SaveHistory('Navigation', glsDirHistory);
    if gSaveCmdLineHistory then SaveHistory('CommandLine', glsCmdLineHistory);
    if gSaveFileMaskHistory then SaveHistory('FileMask', glsMaskHistory);
    if gSaveSearchReplaceHistory then
    begin
      SaveHistory('SearchText', glsSearchHistory, True);
      SaveHistory('SearchTextPath', glsSearchPathHistory);
      SaveHistory('ReplaceText', glsReplaceHistory);
      SaveHistory('ReplaceTextPath', glsReplacePathHistory);
      SaveHistory('CreateDirectories', glsCreateDirectoriesHistory);
      SaveHistory('RenameNameMask', glsRenameNameMaskHistory);
      SaveHistory('RenameExtMask', glsRenameExtMaskHistory);
      SaveHistory('SearchDirectories', glsSearchDirectories);
      SaveHistory('SearchExcludeFiles', glsSearchExcludeFiles);
      SaveHistory('SearchExcludeDirectories', glsSearchExcludeDirectories);
    end;
    History.Save;
  finally
    History.Free;
  end;
end;

function GetValidDateTimeFormat(const aFormat, ADefaultFormat: string): string;
begin
  try
    SysUtils.FormatDateTime(aFormat, Now);
    Result := aFormat;
  except
    on EConvertError do
      Result := ADefaultFormat;
  end;
end;

procedure RegisterInitialization(InitProc: TProcedure);
begin
  SetLength(FInitList, Length(FInitList) + 1);
  FInitList[High(FInitList)]:= InitProc;
end;

procedure LoadDefaultHotkeyBindings;
var
  HMForm: THMForm;
  HMHotKey: THotkey;
  HMControl: THMControl;
begin
  // Note: Increase hkVersion if you change default hotkeys list

  // Shortcuts that can conflict with default OS shortcuts for some controls
  // should be put only to Files Panel.
  // For a list of such possible shortcuts see THotKeyManager.IsShortcutConflictingWithOS.
  // If adding multiple shortcuts for the same command use:
  //  AddIfNotExists([Shortcut1, Param1, Shortcut2, Param2, ...], Command);
  //
  // Shortcuts Ctrl+Alt+<letter> should not be added as the combinations may be
  // used to enter international characters on Windows (where Ctrl+Alt = AltGr).

  HMForm := HotMan.Forms.FindOrCreate('Main');
  with HMForm.Hotkeys do
    begin
      AddIfNotExists(['F1'],[],'cm_About');
      AddIfNotExists(['F2','','',
                      'Shift+F6','',''],'cm_RenameOnly');
      AddIfNotExists(['F3'],[],'cm_View');
      AddIfNotExists(['F4'],[],'cm_Edit');
      AddIfNotExists(['F5'],[],'cm_Copy');
      AddIfNotExists(['F6'],[],'cm_Rename');
      AddIfNotExists(['F7'],[],'cm_MakeDir');
      AddIfNotExists(['F8','','',
                      'Shift+F8','','trashcan=reversesetting',''], 'cm_Delete');
      AddIfNotExists(['F9'],[],'cm_RunTerm');

      if HotMan.Version < 44 then
      begin
        HMHotKey:= FindByCommand('cm_ShowCmdLineHistory');
        if Assigned(HMHotKey) and HMHotKey.SameShortcuts(['Ctrl+7']) then
        begin
          Remove(HMHotKey);
          AddIfNotExists(['Alt+F8'],'cm_ShowCmdLineHistory',['Ctrl+Down'],[]);
        end;
      end;

      AddIfNotExists(['Alt+F8','','',
                      'Ctrl+Down','',''], 'cm_ShowCmdLineHistory');
      AddIfNotExists(['Ctrl+B'],[],'cm_FlatView');
      AddIfNotExists(['Ctrl+D'],[],'cm_DirHotList');
      AddIfNotExists(['Ctrl+F'],[],'cm_QuickFilter');
      AddIfNotExists(['Ctrl+H'],[],'cm_DirHistory');
      AddIfNotExists(['Alt+Down'],'cm_DirHistory',['Ctrl+H'],[]); //Historic backward support reason...
      AddIfNotExists(['Ctrl+L'],[],'cm_CalculateSpace');
      AddIfNotExists(['Ctrl+M'],[],'cm_MultiRename');
      AddIfNotExists(['Ctrl+O'],[],'cm_ToggleFullscreenConsole');
      AddIfNotExists(['Ctrl+P'],[],'cm_AddPathToCmdLine');
      AddIfNotExists(['Ctrl+Q'],[],'cm_QuickView');
      AddIfNotExists(['Ctrl+S'],[],'cm_QuickSearch');
      AddIfNotExists(['Ctrl+R'],[],'cm_Refresh');
      AddIfNotExists(['Ctrl+T'],[],'cm_NewTab');
      AddIfNotExists(['Ctrl+U'],[],'cm_Exchange');
      AddIfNotExists(['Ctrl+W'],[],'cm_CloseTab');
      AddIfNotExists(['Ctrl+F1'],[],'cm_BriefView');
      AddIfNotExists(['Ctrl+F2'],[],'cm_ColumnsView');
      AddIfNotExists(['Ctrl+F3'],[],'cm_SortByName');
      AddIfNotExists(['Ctrl+F4'],[],'cm_SortByExt');
      AddIfNotExists(['Ctrl+F5'],[],'cm_SortByDate');
      AddIfNotExists(['Ctrl+F6'],[],'cm_SortBySize');
      AddIfNotExists(['Ctrl+Enter'],[],'cm_AddFilenameToCmdLine');
      AddIfNotExists(['Ctrl+PgDn'],[],'cm_OpenArchive');
      AddIfNotExists(['Ctrl+PgUp'],[],'cm_ChangeDirToParent');
      AddIfNotExists(['Ctrl+Alt+Enter'],[],'cm_ShellExecute');
      AddIfNotExists(['Ctrl+Shift+B'],[],'cm_FlatViewSel');
      AddIfNotExists(['Ctrl+Shift+C'],[],'cm_CopyFullNamesToClip');
      AddIfNotExists(['Ctrl+Shift+D'],[],'cm_ConfigDirHotList');
      AddIfNotExists(['Ctrl+Shift+H'],[],'cm_HorizontalFilePanels');
      AddIfNotExists(['Ctrl+Shift+X'],[],'cm_CopyNamesToClip');
      AddIfNotExists(['Ctrl+Shift+F1'],[],'cm_ThumbnailsView');
      AddIfNotExists(['Ctrl+Shift+Enter'],[],'cm_AddPathAndFilenameToCmdLine');
      AddIfNotExists(['Ctrl+Shift+Tab'],[],'cm_PrevTab');
      AddIfNotExists(['Ctrl+Shift+F7'],[],'cm_AddNewSearch');
      AddIfNotExists(['Ctrl+Shift+F8'],[],'cm_TreeView');
      AddIfNotExists(['Ctrl+Tab'],[],'cm_NextTab');
      AddIfNotExists(['Ctrl+Up'],[],'cm_OpenDirInNewTab');
      AddIfNotExists(['Ctrl+\'],[],'cm_ChangeDirToRoot');
      AddIfNotExists(['Ctrl+.'],[],'cm_ShowSysFiles');
      AddIfNotExists(['Shift+F2'],[],'cm_FocusCmdLine');
      AddIfNotExists(['Shift+F4'],[],'cm_EditNew');
      AddIfNotExists(['Shift+F5'],[],'cm_CopySamePanel');
      AddIfNotExists(['Shift+F10'],[],'cm_ContextMenu');
      AddIfNotExists(['Shift+F12'],[],'cm_DoAnyCmCommand');
      AddIfNotExists(['Shift+Tab'],[],'cm_FocusTreeView');
      AddIfNotExists(['Alt+V'],[],'cm_OperationsViewer');
      AddIfNotExists(['Alt+X'],[],'cm_Exit');
      AddIfNotExists(['Alt+Z'],[],'cm_TargetEqualSource');
      AddIfNotExists(['Alt+F1'],[],'cm_LeftOpenDrives');
      AddIfNotExists(['Alt+F2'],[],'cm_RightOpenDrives');
      AddIfNotExists(['Alt+F5'],[],'cm_PackFiles');
      AddIfNotExists(['Alt+F7'],[],'cm_Search');
      AddIfNotExists(['Alt+F9'],[],'cm_ExtractFiles');
      AddIfNotExists(['Alt+Del'],[],'cm_Wipe');
      AddIfNotExists(['Alt+Enter'],[],'cm_FileProperties');
      AddIfNotExists(['Alt+Left'],[],'cm_ViewHistoryPrev');
      AddIfNotExists(['Alt+Right'],[],'cm_ViewHistoryNext');
      AddIfNotExists(['Alt+Shift+Enter'],[],'cm_CountDirContent');
      AddIfNotExists(['Alt+Shift+F9'],[],'cm_TestArchive');
      AddIfNotExists([
         'Alt+1','','index=1','',
         'Alt+2','','index=2','',
         'Alt+3','','index=3','',
         'Alt+4','','index=4','',
         'Alt+5','','index=5','',
         'Alt+6','','index=6','',
         'Alt+7','','index=7','',
         'Alt+8','','index=8','',
         'Alt+9','','index=9','',
         'Alt+`','','index=-1',''],
       'cm_ActivateTabByIndex');
      AddIfNotExists([
        'Ctrl+1','','index=1','',
        'Ctrl+2','','index=2','',
        'Ctrl+3','','index=3','',
        'Ctrl+4','','index=4','',
        'Ctrl+5','','index=5','',
        'Ctrl+6','','index=6','',
        'Ctrl+7','','index=7','',
        'Ctrl+8','','index=8','',
        'Ctrl+9','','index=9',''],
      'cm_OpenDriveByIndex');

      if HotMan.Version < 38 then
      begin
        HMHotKey:= FindByCommand('cm_EditComment');
        if Assigned(HMHotKey) and HMHotKey.SameShortcuts(['Ctrl+Z']) then
          Remove(HMHotKey);
      end;
    end;

  HMControl := HMForm.Controls.FindOrCreate('Files Panel');
  with HMControl.Hotkeys do
    begin
      AddIfNotExists(['Del'      ,'','',
                      'Shift+Del','','trashcan=reversesetting',''], 'cm_Delete');
      AddIfNotExists(['Ctrl+A'   ,'','',
                      'Ctrl+Num+','',''],'cm_MarkMarkAll', ['Ctrl+A'], []);
      AddIfNotExists(['Num+'],[],'cm_MarkPlus');
      AddIfNotExists(['Shift+Num+'],[],'cm_MarkCurrentExtension');
      AddIfNotExists(['Ctrl+Num-'],[],'cm_MarkUnmarkAll');
      AddIfNotExists(['Num-'],[],'cm_MarkMinus');
      AddIfNotExists(['Shift+Num-'],[],'cm_UnmarkCurrentExtension');
      AddIfNotExists(['Num*'],[],'cm_MarkInvert');
      AddIfNotExists(['Ctrl+Z'],[],'cm_EditComment');
      AddIfNotExists(['Ctrl+Shift+Home'],[],'cm_ChangeDirToHome');
      AddIfNotExists(['Ctrl+Left'],[],'cm_TransferLeft');
      AddIfNotExists(['Ctrl+Right'],[],'cm_TransferRight');

      if HotMan.Version < 46 then
      begin
        HMHotKey:= FindByCommand('cm_NextGroup');
        if Assigned(HMHotKey) then Remove(HMHotKey);
      end;

      AddIfNotExists(VK_C, [ssModifier], 'cm_CopyToClipboard');
      AddIfNotExists(VK_V, [ssModifier], 'cm_PasteFromClipboard');
      AddIfNotExists(VK_X, [ssModifier], 'cm_CutToClipboard');
    end;

  HMForm := HotMan.Forms.FindOrCreate('Viewer');
  with HMForm.Hotkeys do
    begin

      AddIfNotExists(['F1'],[],'cm_About');
      AddIfNotExists(['F2'],[],'cm_Reload');

      AddIfNotExists(['N'   ,'','',
                      'Right','',''],'cm_LoadNextFile'); //, ['N'], []);

      AddIfNotExists(['P'   ,'','',
                      'Left','',''],'cm_LoadPrevFile'); //, ['P'], []);

      AddIfNotExists(['1'],[],'cm_ShowAsText');
      AddIfNotExists(['2'],[],'cm_ShowAsBin');
      AddIfNotExists(['3'],[],'cm_ShowAsHex');
      AddIfNotExists(['4'],[],'cm_ShowAsWrapText');
      AddIfNotExists(['5'],[],'cm_ShowAsBook');
      AddIfNotExists(['6'],[],'cm_ShowGraphics');
      AddIfNotExists(['7'],[],'cm_ShowPlugins');

      AddIfNotExists(['F6'],[],'cm_ShowCaret');

      AddIfNotExists(['Q'   ,'','',
                      'Esc','',''],'cm_ExitViewer');


      AddIfNotExists(['F'             ,'','',
                      SmkcSuper + 'F' ,'','',
                      'F7'            ,'',''],'cm_Find'); // , ['F'], []);

      AddIfNotExists(['F3'],[],'cm_FindNext');
      AddIfNotExists(['Shift+F3'],[],'cm_FindPrev');

      AddIfNotExists(['`'],[],'cm_Preview');  // til'da on preview mode

      AddIfNotExists(['Num+'],[],'cm_ZoomIn');
      AddIfNotExists(['Num-'],[],'cm_ZoomOut');

      AddIfNotExists(['Alt+Enter'],[],'cm_Fullscreen');

      //AddIfNotExists(['Up'],[],'cm_Rotate270');  // how at once add this keys only to Image control?
      //AddIfNotExists(['Down'],[],'cm_Rotate90');

      AddIfNotExists(VK_P, [ssModifier], 'cm_Print');
      AddIfNotExists(VK_A, [ssModifier], 'cm_SelectAll');
      AddIfNotExists(VK_C, [ssModifier], 'cm_CopyToClipboard');

      AddIfNotExists(['A','','ANSI','',
                      'S','','OEM','',
                      'Z','','UTF-8','',
                      'X','','UTF-16LE',''],'cm_ChangeEncoding');
    end;


  HMForm := HotMan.Forms.FindOrCreate('Differ');
  with HMForm.Hotkeys do
    begin
      AddIfNotExists(['Ctrl+R'],[],'cm_Reload');
      AddIfNotExists(['Alt+Down'],[],'cm_NextDifference');
      AddIfNotExists(['Alt+Up'],[],'cm_PrevDifference');
      AddIfNotExists(['Alt+Home'],[],'cm_FirstDifference');
      AddIfNotExists(['Alt+End'],[],'cm_LastDifference');
      AddIfNotExists(['Alt+X'],[],'cm_Exit');
      AddIfNotExists(['Alt+Left'],[],'cm_CopyRightToLeft');
      AddIfNotExists(['Alt+Right'],[],'cm_CopyLeftToRight');
    end;

  HMForm := HotMan.Forms.FindOrCreate('Copy/Move Dialog');
  with HMForm.Hotkeys do
    begin
      AddIfNotExists(['F2'], [],'cm_AddToQueue');
      AddIfNotExists(['F5', '', '',
                      'F6', '', ''], 'cm_ToggleSelectionInName');
    end;

  HMForm := HotMan.Forms.FindOrCreate('Edit Comment Dialog');
  with HMForm.Hotkeys do
    begin
      AddIfNotExists(['F2'],[],'cm_SaveDescription');
    end;

  HMForm := HotMan.Forms.FindOrCreate('Synchronize Directories');
  with HMForm.Hotkeys do
    begin
      AddIfNotExists(VK_M, [ssModifier], 'cm_SelectClear');
      AddIfNotExists(VK_D, [ssModifier], 'cm_SelectCopyDefault');
      AddIfNotExists(VK_W, [ssModifier], 'cm_SelectCopyReverse');
      AddIfNotExists(VK_L, [ssModifier], 'cm_SelectCopyLeftToRight');
      AddIfNotExists(VK_R, [ssModifier], 'cm_SelectCopyRightToLeft');
    end;

  HMForm := HotMan.Forms.FindOrCreate('Editor');
  with HMForm.Hotkeys do
    begin
      if HotMan.Version < 45 then
      begin
        HMHotKey:= FindByCommand('cm_EditFind');
        if Assigned(HMHotKey) and HMHotKey.SameShortcuts(['F7']) then
          Remove(HMHotKey);
        HMHotKey:= FindByCommand('cm_FileSave');
        if Assigned(HMHotKey) and HMHotKey.SameShortcuts(['F2']) then
          Remove(HMHotKey);
        HMHotKey:= FindByCommand('cm_FileExit');
        if Assigned(HMHotKey) and HMHotKey.SameShortcuts(['Esc']) then
          Remove(HMHotKey);
      end;

      AddIfNotExists([SmkcSuper + 'F' ,'','',
                      'F7'            ,'',''],'cm_EditFind');
      AddIfNotExists(['F2'            ,'','',
                      SmkcSuper + 'S' ,'',''],'cm_FileSave');
      AddIfNotExists(['F3'],[],'cm_EditFindNext');
      AddIfNotExists(['Shift+F3'],[],'cm_EditFindPrevious');
      AddIfNotExists(['Alt+X', '', '', //Let is be first since by legacy what we get used to see in main menu as shortcut was "Alt+X".
                      'Esc', '', ''], 'cm_FileExit');

      AddIfNotExists(VK_X, [ssModifier], 'cm_EditCut');
      AddIfNotExists(VK_N, [ssModifier], 'cm_FileNew');
      AddIfNotExists(VK_O, [ssModifier], 'cm_FileOpen');
      AddIfNotExists(VK_R, [ssModifier], 'cm_EditRplc');
      AddIfNotExists(VK_C, [ssModifier], 'cm_EditCopy');
      AddIfNotExists(VK_Z, [ssModifier], 'cm_EditUndo');
      AddIfNotExists(VK_V, [ssModifier], 'cm_EditPaste');
      AddIfNotExists(VK_A, [ssModifier], 'cm_EditSelectAll');
      AddIfNotExists(VK_Z, [ssModifier, ssShift], 'cm_EditRedo');
      AddIfNotExists(VK_G, [ssModifier], 'cm_EditGotoLine');
    end;


  HMForm := HotMan.Forms.FindOrCreate('Find Files');
  with HMForm.Hotkeys do
    begin
      AddIfNotExists(['F3'],[],'cm_View');
      AddIfNotExists(['F4'],[],'cm_Edit');
      AddIfNotExists(['F7'],[],'cm_IntelliFocus');
      AddIfNotExists(['F9'],[],'cm_Start');
      AddIfNotExists(['Esc'],[],'cm_CancelClose');
      AddIfNotExists(['Ctrl+N'],[],'cm_NewSearch');
      AddIfNotExists(['Ctrl+Shift+N'],[],'cm_NewSearchClearFilters');
      AddIfNotExists(['Ctrl+L'],[],'cm_LastSearch');
      AddIfNotExists(['Alt+1','','',
                      'Alt+F7','',''],'cm_PageStandard');
      AddIfNotExists(['Alt+2'],[],'cm_PageAdvanced');
      AddIfNotExists(['Alt+3'],[],'cm_PagePlugins');
      AddIfNotExists(['Alt+4'],[],'cm_PageLoadSave');
      AddIfNotExists(['Alt+5'],[],'cm_PageResults');
      AddIfNotExists(['Alt+F4','',''],'cm_FreeFromMem');

      AddIfNotExists(VK_TAB, [ssModifier], 'cm_PageNext');
      AddIfNotExists(VK_TAB, [ssModifier, ssShift], 'cm_PagePrev');
    end;

  HMForm := HotMan.Forms.FindOrCreate(HotkeysCategoryMultiRename);
  with HMForm.Hotkeys do
    begin
      AddIfNotExists(['Ctrl+R'],[],'cm_ResetAll');
      AddIfNotExists(['Ctrl+I'],[],'cm_InvokeEditor');
      AddIfNotExists(['F3'],[],'cm_LoadNamesFromFile');
      AddIfNotExists(['F4'],[],'cm_EditNames');
      AddIfNotExists(['Shift+F4'],[],'cm_EditNewNames');
      AddIfNotExists(['F10'],[],'cm_Config');
      AddIfNotExists(['F9'],[],'cm_Rename');
      AddIfNotExists(['Esc'],[],'cm_Close');

      AddIfNotExists(['Shift+F2'],[],'cm_ShowPresetsMenu');
      AddIfNotExists(['F2'],[],'cm_DropDownPresetList');
      AddIfNotExists(['Alt+0'],[],'cm_LoadLastPreset');
      AddIfNotExists(['Alt+1'],[],'cm_LoadPreset1');
      AddIfNotExists(['Alt+2'],[],'cm_LoadPreset2');
      AddIfNotExists(['Alt+3'],[],'cm_LoadPreset3');
      AddIfNotExists(['Alt+4'],[],'cm_LoadPreset4');
      AddIfNotExists(['Alt+5'],[],'cm_LoadPreset5');
      AddIfNotExists(['Alt+6'],[],'cm_LoadPreset6');
      AddIfNotExists(['Alt+7'],[],'cm_LoadPreset7');
      AddIfNotExists(['Alt+8'],[],'cm_LoadPreset8');
      AddIfNotExists(['Alt+9'],[],'cm_LoadPreset9');
      AddIfNotExists(['Ctrl+S'],[],'cm_SavePreset');
      AddIfNotExists(['F12'],[],'cm_SavePresetAs');
      AddIfNotExists(['Shift+F6'],[],'cm_RenamePreset');
      AddIfNotExists(['Ctrl+D'],[],'cm_DeletePreset');
      AddIfNotExists(['Ctrl+Shift+S'],[],'cm_SortPresets');

      AddIfNotExists(['Ctrl+F2'],[],'cm_AnyNameMask');
      AddIfNotExists(['Ctrl+F3'],[],'cm_NameNameMask');
      AddIfNotExists(['Ctrl+F4'],[],'cm_ExtNameMask');
      AddIfNotExists(['Ctrl+F7'],[],'cm_CtrNameMask');
      AddIfNotExists(['Ctrl+F5'],[],'cm_DateNameMask');
      AddIfNotExists(['Ctrl+F6'],[],'cm_TimeNameMask');
      AddIfNotExists(['Ctrl+F1'],[],'cm_PlgnNameMask');

      AddIfNotExists(['Ctrl+Shift+F2'],[],'cm_AnyExtMask');
      AddIfNotExists(['Ctrl+Shift+F3'],[],'cm_NameExtMask');
      AddIfNotExists(['Ctrl+Shift+F4'],[],'cm_ExtExtMask');
      AddIfNotExists(['Ctrl+Shift+F7'],[],'cm_CtrExtMask');
      AddIfNotExists(['Ctrl+Shift+F5'],[],'cm_DateExtMask');
      AddIfNotExists(['Ctrl+Shift+F6'],[],'cm_TimeExtMask');
      AddIfNotExists(['Ctrl+Shift+F1'],[],'cm_PlgnExtMask');
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
    if (Monitor = nil) then
      Result.IniSection:= ClassName
    else begin
      sWidth:= IntToStr(Monitor.Width);
      sHeight:= IntToStr(Monitor.Height);
      Result.IniSection:= ClassName + '(' + sWidth + 'x' + sHeight + ')';
    end;
  end;
end;

procedure FontToFontOptions(Font: TFont; var Options: TDCFontOptions);
begin
  with Options do
  begin
    Name    := Font.Name;
    Size    := Font.Size;
    Style   := Font.Style;
    Quality := Font.Quality;
  end;
end;

procedure FontOptionsToFont(Options: TDCFontOptions; Font: TFont);
begin
  with Options do
  begin
    Font.Name    := Name;
    Font.Size    := Size;
    Font.Style   := Style;
    Font.Quality := Quality;
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

procedure CopySettingsFiles;
begin
  { Create default configuration files if need }
  if gpCfgDir <> gpGlobalCfgDir then
    begin
      // extension file
      if not mbFileExists(gpCfgDir + gcfExtensionAssociation) then
        CopyFile(gpGlobalCfgDir + gcfExtensionAssociation, gpCfgDir + gcfExtensionAssociation);
      // pixmaps file
      if not mbFileExists(gpCfgDir + 'pixmaps.txt') then
        CopyFile(gpGlobalCfgDir + 'pixmaps.txt', gpCfgDir + 'pixmaps.txt');
      // multiarc configuration file
      if not mbFileExists(gpCfgDir + sMULTIARC_FILENAME) then
        CopyFile(gpGlobalCfgDir + sMULTIARC_FILENAME, gpCfgDir + sMULTIARC_FILENAME);
    end;
end;

procedure CreateGlobs;
begin
  gExts := TExts.Create;
  gColorExt := TColorExt.Create;
  gFileInfoToolTip := TFileInfoToolTip.Create;
  gDirectoryHotlist := TDirectoryHotlist.Create;
  gFavoriteTabsList := TFavoriteTabsList.Create;
  glsDirHistory := TStringListEx.Create;
  glsCmdLineHistory := TStringListEx.Create;
  glsMaskHistory := TStringListEx.Create;
  glsSearchHistory := TStringListEx.Create;
  glsSearchPathHistory := TStringListEx.Create;
  glsReplaceHistory := TStringListEx.Create;
  glsReplacePathHistory := TStringListEx.Create;
  glsCreateDirectoriesHistory := TStringListEx.Create;
  glsRenameNameMaskHistory := TStringListEx.Create;
  glsRenameExtMaskHistory := TStringListEx.Create;
  glsIgnoreList := TStringListEx.Create;
  glsSearchDirectories := TStringList.Create;
  glsSearchExcludeFiles:= TStringList.Create;
  glsSearchExcludeDirectories:= TStringList.Create;
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
  FreeThenNil(glsCmdLineHistory);
  FreeThenNil(gSpecialDirList);
  FreeThenNil(gDirectoryHotlist);
  FreeThenNil(gFavoriteTabsList);
  FreeThenNil(glsMaskHistory);
  FreeThenNil(glsSearchHistory);
  FreeThenNil(glsSearchPathHistory);
  FreeThenNil(glsReplaceHistory);
  FreeThenNil(glsReplacePathHistory);
  FreeAndNil(glsCreateDirectoriesHistory);
  FreeAndNil(glsRenameNameMaskHistory);
  FreeAndNil(glsRenameExtMaskHistory);
  FreeThenNil(glsIgnoreList);
  FreeThenNil(glsSearchDirectories);
  FreeThenNil(glsSearchExcludeFiles);
  FreeThenNil(glsSearchExcludeDirectories);
  FreeThenNil(gExts);
  FreeThenNil(gConfig);
  FreeThenNil(gSearchTemplateList);
  FreeThenNil(gDSXPlugins);
  FreeThenNil(gWCXPlugins);
  FreeThenNil(gWDXPlugins);
  FreeThenNil(gWFXPlugins);
  FreeThenNil(gWLXPlugins);
  FreeThenNil(gMultiArcList);
  FreeThenNil(ColSet);
  FreeThenNil(HotMan);
end;

{$IFDEF MSWINDOWS}
function GetPathNameIfItMatch(SpecialConstant:integer; FilenameSearched:string):string;
var
  MaybePath:string;
  FilePath: array [0..Pred(MAX_PATH)] of WideChar = '';
begin
  result:='';

  FillChar(FilePath, MAX_PATH, 0);
  SHGetSpecialFolderPathW(0, @FilePath[0], SpecialConstant, FALSE);
  if FilePath<>'' then
  begin
    MaybePath:=IncludeTrailingPathDelimiter(UTF16ToUTF8(WideString(FilePath)));
    if mbFileExists(MaybePath+FilenameSearched) then result:=MaybePath+FilenameSearched;
  end;
end;
{$ENDIF}

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

var
  iIndexContextMode:integer;

begin
  { Language page }
  gPOFileName := '';

  { Behaviours page }
  gRunInTermStayOpenCmd := RunInTermStayOpenCmd;
  gRunInTermStayOpenParams := RunInTermStayOpenParams;
  gRunInTermCloseCmd := RunInTermCloseCmd;
  gRunInTermCloseParams := RunInTermCloseParams;
  gRunTermCmd := RunTermCmd;
  gRunTermParams := RunTermParams;
  gOnlyOneAppInstance := False;
  gLynxLike := True;
  gSortCaseSensitivity := cstNotSensitive;
  gSortNatural := False;
  gSortSpecial := False;
  gSortFolderMode := sfmSortLikeFileShowFirst;
  gNewFilesPosition := nfpSortedPosition;
  gUpdatedFilesPosition := ufpNoChange;
  gFileSizeFormat := fsfFloat;
  gHeaderSizeFormat := fsfFloat;
  gFooterSizeFormat := fsfFloat;
  gOperationSizeFormat := fsfFloat;
  gFileSizeDigits := 1;
  gHeaderDigits := 1;
  gFooterDigits := 1;
  gOperationSizeDigits := 1;
  //NOTES: We're intentionnaly not setting our default memory immediately because language file has not been loaded yet.
  //       We'll set them *after* after language has been loaded since we'll know the correct default to use.
  gMinimizeToTray := False;
  gAlwaysShowTrayIcon := False;
  gMouseSelectionEnabled := True;
  gMouseSelectionButton := 0;  // Left
  gMouseSingleClickStart := 0;
  gMouseSelectionIconClick := 0;
  gScrollMode := smLineByLine;
  gWheelScrollLines:= Mouse.WheelScrollLines;
  gAutoFillColumns := False;
  gAutoSizeColumn := 1;
  gColumnsAutoSaveWidth := True;
  gColumnsTitleStyle := {$IFDEF LCLWIN32}tsNative{$ELSE}tsStandard{$ENDIF};
  gCustomColumnsChangeAllColumns := False;
  gDateTimeFormat := DefaultDateTimeFormat;
  gCutTextToColWidth := True;
  gExtendCellWidth := False;
  gShowSystemFiles := False;
  // Under Mac OS X loading file list in separate thread are very very slow
  // so disable and hide this option under Mac OS X Carbon
  gListFilesInThread := {$IFDEF LCLCARBON}False{$ELSE}True{$ENDIF};
  gLoadIconsSeparately := True;
  gDelayLoadingTabs := True;
  gHighlightUpdatedFiles := True;
  gDriveBlackList := '';
  gDriveBlackListUnmounted := False;

  { File views page }
  gExtraLineSpan := 2;
  gFolderPrefix := '[';
  gFolderPostfix := ']';
  { Brief view page }
  gBriefViewFixedCount := 2;
  gBriefViewFixedWidth := 100;
  gBriefViewMode := bvmAutoSize;
  gBriefViewFileExtAligned := False;

  { Tools page }
  SetDefaultExternalTool(gExternalTools[etViewer]);
  SetDefaultExternalTool(gExternalTools[etEditor]);
  SetDefaultExternalTool(gExternalTools[etDiffer]);

  { Differ related}
  gResultingFramePositionAfterCompare := rfpacActiveOnLeft;

  { Fonts page }
  gFonts[dcfMain].Name := 'default';
  gFonts[dcfMain].Size := 10;
  gFonts[dcfMain].Style := [fsBold];
  gFonts[dcfMain].Quality := fqDefault;
  gFonts[dcfMain].MinValue := 6;
  gFonts[dcfMain].MaxValue := 200;

  gFonts[dcfEditor].Name := MonoSpaceFont;
  gFonts[dcfEditor].Size := 14;
  gFonts[dcfEditor].Style := [];
  gFonts[dcfEditor].Quality := fqDefault;
  gFonts[dcfEditor].MinValue := 6;
  gFonts[dcfEditor].MaxValue := 200;

  gFonts[dcfViewer].Name := MonoSpaceFont;
  gFonts[dcfViewer].Size := 14;
  gFonts[dcfViewer].Style := [];
  gFonts[dcfViewer].Quality := fqDefault;
  gFonts[dcfViewer].MinValue := 6;
  gFonts[dcfViewer].MaxValue := 200;

  gFonts[dcfViewerBook].Name := 'default';
  gFonts[dcfViewerBook].Size := 16;
  gFonts[dcfViewerBook].Style := [fsBold];
  gFonts[dcfViewerBook].Quality := fqDefault;
  gFonts[dcfViewerBook].MinValue := 6;
  gFonts[dcfViewerBook].MaxValue := 200;

  gFonts[dcfLog].Name := MonoSpaceFont;
  gFonts[dcfLog].Size := 12;
  gFonts[dcfLog].Style := [];
  gFonts[dcfLog].Quality := fqDefault;
  gFonts[dcfLog].MinValue := 6;
  gFonts[dcfLog].MaxValue := 200;

  gFonts[dcfConsole].Name := MonoSpaceFont;
  gFonts[dcfConsole].Size := 12;
  gFonts[dcfConsole].Style := [];
  gFonts[dcfConsole].Quality := fqDefault;
  gFonts[dcfConsole].MinValue := 6;
  gFonts[dcfConsole].MaxValue := 200;

  gFonts[dcfPathEdit].Name := 'default';
  gFonts[dcfPathEdit].Size := 8;
  gFonts[dcfPathEdit].Style := [];
  gFonts[dcfPathEdit].Quality := fqDefault;
  gFonts[dcfPathEdit].MinValue := 6;
  gFonts[dcfPathEdit].MaxValue := 200;

  gFonts[dcfFunctionButtons].Name := 'default';
  gFonts[dcfFunctionButtons].Size := 8;
  gFonts[dcfFunctionButtons].Style := [];
  gFonts[dcfFunctionButtons].Quality := fqDefault;
  gFonts[dcfFunctionButtons].MinValue := 6;
  gFonts[dcfFunctionButtons].MaxValue := 200;

  gFonts[dcfSearchResults].Name := 'default';
  gFonts[dcfSearchResults].Size := 9;
  gFonts[dcfSearchResults].Style := [];
  gFonts[dcfSearchResults].Quality := fqDefault;
  gFonts[dcfSearchResults].MinValue := 6;
  gFonts[dcfSearchResults].MaxValue := 200;

  gFonts[dcfTreeViewMenu].Name := 'default';
  gFonts[dcfTreeViewMenu].Size := 10;
  gFonts[dcfTreeViewMenu].Style := [];
  gFonts[dcfTreeViewMenu].Quality := fqDefault;
  gFonts[dcfTreeViewMenu].MinValue := 6;
  gFonts[dcfTreeViewMenu].MaxValue := 200;

  { Colors page }
  gUseCursorBorder := False;
  gCursorBorderColor := clHighlight;
  gUseFrameCursor := False;
  gForeColor := clWindowText;
  gBackColor := clWindow;
  gBackColor2 := clWindow;
  gMarkColor := clRed;
  gCursorColor := clHighlight;
  gCursorText := clHighlightText;
  gInactiveCursorColor := clInactiveCaption;
  gInactiveMarkColor := clMaroon;
  gUseInvertedSelection := False;
  gUseInactiveSelColor := False;
  gAllowOverColor := True;
  gBorderFrameWidth:=1;

  gPathActiveColor := clHighlight;
  gPathActiveFontColor := clHighlightText;
  gPathInactiveColor := clBtnFace;
  gPathInactiveFontColor := clBtnText;

  gInactivePanelBrightness := 100; // Full brightness
  gIndUseGradient := True;
  gIndForeColor := clBlack;
  gIndBackColor := clWhite;

  gLogInfoColor:= clNavy;
  gLogErrorColor:= clRed;
  gLogSuccessColor:= clGreen;

  gSyncLeftColor:= clGreen;
  gSyncRightColor:= clBlue;
  gSyncUnknownColor:= clRed;

  { Layout page }
  gMainMenu := True;
  gButtonBar := True;
  gToolBarFlat := True;
  gMiddleToolBar := False;
  gToolBarButtonSize := 24;
  gToolBarIconSize := 16;
  gToolBarShowCaptions := False;
  gToolbarReportErrorWithCommands := FALSE;

  gMiddleToolBarFlat := True;
  gMiddleToolBarButtonSize := 24;
  gMiddleToolBarIconSize := 16;
  gMiddleToolBarShowCaptions := False;
  gMiddleToolbarReportErrorWithCommands := FALSE;

  gToolbarFilenameStyle := pfsAbsolutePath;
  gToolbarPathToBeRelativeTo := EnvVarCommanderPath;
  gToolbarPathModifierElements := [];

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
  gUpperCaseDriveLetter := False;
  gShowColonAfterDrive := False;
  gDrivesListButtonOptions := [dlbShowLabel, dlbShowFileSystem, dlbShowFreeSpace];
  gSeparateTree := False;

  { Keys page }
  gKeyTyping[ktmNone]    := ktaQuickSearch;
  gKeyTyping[ktmAlt]     := ktaNone;
  gKeyTyping[ktmCtrlAlt] := ktaQuickFilter;

  { File operations page }
  gLongNameAlert := True;
  gCopyBlockSize := 524288;
  gHashBlockSize := 8388608;
  gUseMmapInSearch := False;
  gPartialNameSearch := True;
  gInitiallyClearFileMask := True;
  gNewSearchClearFiltersAction := fonsKeep;
  gShowMenuBarInFindFiles := True;
  gWipePassNumber := 1;
  gDropReadOnlyFlag := False;
  gProcessComments := False;
  gRenameSelOnlyName := False;
  gShowCopyTabSelectPanel := False;
  gUseTrash := True;
  gSkipFileOpError := False;
  gTypeOfDuplicatedRename := drLegacyWithCopy;
  gShowDialogOnDragDrop := True;
  gDragAndDropDesiredTextFormat[DropTextRichText_Index].Name:='Richtext format';
  gDragAndDropDesiredTextFormat[DropTextRichText_Index].DesireLevel:=0;
  gDragAndDropDesiredTextFormat[DropTextHtml_Index].Name:='HTML format';
  gDragAndDropDesiredTextFormat[DropTextHtml_Index].DesireLevel:=1;
  gDragAndDropDesiredTextFormat[DropTextUnicode_Index].Name:='Unicode format';
  gDragAndDropDesiredTextFormat[DropTextUnicode_Index].DesireLevel:=2;
  gDragAndDropDesiredTextFormat[DropTextSimpleText_Index].Name:='Simple text format';
  gDragAndDropDesiredTextFormat[DropTextSimpleText_Index].DesireLevel:=3;
  gDragAndDropAskFormatEachTime := False;
  gDragAndDropTextAutoFilename := False;
  gDragAndDropSaveUnicodeTextInUFT8 := True;
  gNtfsHourTimeDelay := False;
  gAutoExtractOpenMask := EmptyStr;
  gFileOperationsProgressKind := fopkSeparateWindow;
  gFileOperationsConfirmations := [focCopy, focMove, focDelete, focDeleteToTrash];

  { Multi-Rename }
  gMulRenShowMenuBarOnTop := True;
  gMulRenInvalidCharReplacement := '.';
  gMulRenLaunchBehavior := mrlbLastMaskUnderLastOne;
  gMulRenExitModifiedPreset := mrempIgnoreSaveLast;
  gMulRenSaveRenamingLog := mrsrlPerPreset;
  gMulRenLogFilename := EnvVarConfigPath + PathDelim + 'multirename.log';
  gMultRenDailyIndividualDirLog := True;
  gMulRenFilenameWithFullPathInLog:= False;
  gMulRenPathRangeSeparator := ' - ';

  // Operations options
  gOperationOptionSymLinks := fsooslNone;
  gOperationOptionCorrectLinks := False;
  gOperationOptionCopyOnWrite := fsoogNo;
  gOperationOptionFileExists := fsoofeNone;
  gOperationOptionDirectoryExists := fsoodeNone;
  gOperationOptionSetPropertyError := fsoospeNone;
  gOperationOptionReserveSpace := True;
  gOperationOptionCheckFreeSpace := True;
  gOperationOptionCopyAttributes := True;
  gOperationOptionCopyXattributes := True;
  gOperationOptionCopyTime := True;
  gOperationOptionVerify := False;
  gOperationOptionCopyOwnership := False;
  gOperationOptionCopyPermissions := False;
  gOperationOptionExcludeEmptyDirectories := True;

  // Extract
  gExtractOverwrite := False;

  { Tabs page }
  gDirTabOptions := [tb_always_visible,
                     tb_confirm_close_all,
                     tb_show_asterisk_for_locked,
                     tb_activate_panel_on_click,
                     tb_close_on_doubleclick,
                     tb_reusing_tab_when_possible,
                     tb_confirm_close_locked_tab];
  gDirTabActionOnDoubleClick := tadc_FavoriteTabs;
  gDirTabLimit := 32;
  gDirTabPosition := tbpos_top;

  { Favorite Tabs}
  gFavoriteTabsUseRestoreExtraOptions := False;
  gWhereToAddNewFavoriteTabs := afte_Last;
  gFavoriteTabsFullExpandOrNot := True;
  gFavoriteTabsGoToConfigAfterSave := False;
  gFavoriteTabsGoToConfigAfterReSave := False;
  gDefaultTargetPanelLeftSaved := tclLeft;
  gDefaultTargetPanelRightSaved := tclRight;
  gDefaultExistingTabsToKeep := tclNone;
  gFavoriteTabsSaveDirHistory := False;

  { Log page }
  gLogFile := False;
  gLogFileCount:= 0;
  gLogFileWithDateInName := FALSE;
  gLogFileName := EnvVarConfigPath + PathDelim + 'doublecmd.log';
  gLogOptions := [log_cp_mv_ln, log_delete, log_dir_op, log_arc_op,
                  log_vfs_op, log_success, log_errors, log_info,
                  log_start_shutdown, log_commandlineexecution];

  { Configuration page }
  gSaveConfiguration := True;
  gSaveWindowState := True;
  gSaveFolderTabs := True;
  gSaveSearchReplaceHistory := True;
  gSaveDirHistory := True;
  gSaveCmdLineHistory := True;
  gSaveFileMaskHistory := True;
  gPluginInAutoTweak := False;
  gWCXConfigViewMode := wcvmByPlugin;

  { Quick Search/Filter page }
  gQuickSearchOptions.Match := [qsmBeginning, qsmEnding];
  gQuickSearchOptions.Items := qsiFilesAndDirectories;
  gQuickSearchOptions.SearchCase := qscInsensitive;
  gQuickFilterAutoHide := True;
  gQuickFilterSaveSessionModifications := False; //Legacy...

  { Miscellaneous page }
  gGridVertLine := False;
  gGridHorzLine := False;
  gShowWarningMessages := True;
  gSpaceMovesDown := False;
  gDirBrackets := True;
  gInplaceRename := False;
  gInplaceRenameButton := True;
  gDblClickToParent := False;
  gHotDirAddTargetOrNot := False;
  gHotDirFullExpandOrNot:=False;
  gShowPathInPopup:=FALSE;
  gShowOnlyValidEnv:=TRUE;
  gWhereToAddNewHotDir := ahdSmart;
  gHotDirFilenameStyle := pfsAbsolutePath;
  gHotDirPathToBeRelativeTo := EnvVarCommanderPath;
  gHotDirPathModifierElements := [];

  gShowToolTip := True;
  gShowToolTipMode := tttmCombineDcSystem;
  gToolTipHideTimeOut := ttthtSystem;
  gThumbSave := True;
  gThumbSize.cx := 128;
  gThumbSize.cy := 128;
  gSearchDefaultTemplate := EmptyStr;
  gDescReadEncoding:= meUTF8;
  gDescWriteEncoding:= meUTF8BOM;
  gDescCreateUnicode:= True;

  { Auto refresh page }
  gWatchDirs := [watch_file_name_change, watch_attributes_change];
  gWatchDirsExclude := '';
  gWatcherMode := fswmAllowDelete;

  { Icons page }
  gShowIcons := sim_all_and_exe;
  gShowIconsNew := gShowIcons;
  gIconOverlays := False;
  gIconsSize := 32;
  gIconsSizeNew := gIconsSize;
  gDiskIconsSize := 16;
  gDiskIconsAlpha := 50;
  gToolIconsSize := 24;
  gIconsExclude := False;
  gIconsExcludeDirs := EmptyStr;
  gPixelsPerInch := 96;
  gCustomIcons := [];
  gIconsInMenus := False;
  gIconsInMenusSize := 16;
  gIconsInMenusSizeNew := gIconsInMenusSize;
  gShowHiddenDimmed := False;
  gIconTheme := DC_THEME_NAME;

  { Ignore list page }
  gIgnoreListFileEnabled := False;
  gIgnoreListFile := EnvVarConfigPath + PathDelim + 'ignorelist.txt';

  {Viewer}
  gImageStretch := False;
  gImageExifRotate := True;
  gImageStretchOnlyLarge := False;
  gImageCenter := True;
  gPreviewVisible := False;
  gCopyMovePath1 := '';
  gCopyMovePath2 := '';
  gCopyMovePath3 := '';
  gCopyMovePath4 := '';
  gCopyMovePath5 := '';
  gImagePaintMode := vptPen;
  gImagePaintWidth := 5;
  gColCount := 1;
  gTabSpaces := 8;
  gMaxTextWidth := 1024;
  gImagePaintColor := clRed;
  gBookBackgroundColor := clBlack;
  gBookFontColor := clWhite;
  gTextPosition:= 0;
  gViewerMode:= 0;
  gShowCaret := False;
  gPrintMargins:= Classes.Rect(200, 200, 200, 200);

  { Editor }
  gEditWaitTime := 2000;
  gEditorSynEditOptions := SYNEDIT_DEFAULT_OPTIONS;
  gEditorSynEditTabWidth := 8;
  gEditorSynEditRightEdge := 80;

  { Differ }
  gDifferIgnoreCase := False;
  gDifferKeepScrolling := True;
  gDifferPaintBackground := True;
  gDifferLineDifferences := False;
  gDifferIgnoreWhiteSpace := False;
  gDifferAddedColor := clPaleGreen;
  gDifferDeletedColor := clPaleRed;
  gDifferModifiedColor := clPaleBlue;

  {SyncDirs}
  gSyncDirsSubdirs := False;
  gSyncDirsByContent := False;
  gSyncDirsAsymmetric := False;
  gSyncDirsIgnoreDate := False;
  gSyncDirsAsymmetricSave := False;
  gSyncDirsShowFilterCopyRight := True;
  gSyncDirsShowFilterEqual := True;
  gSyncDirsShowFilterNotEqual := True;
  gSyncDirsShowFilterCopyLeft := True;
  gSyncDirsShowFilterDuplicates := True;
  gSyncDirsShowFilterSingles := True;
  gSyncDirsFileMask := '*';

  { Internal Associations}
  gFileAssociationLastCustomAction := rsMsgDefaultCustomActionName;
  gOfferToAddToFileAssociations := False;
  gExtendedContextMenu := False;
  gOpenExecuteViaShell := False;
  gExecuteViaTerminalClose := False;
  gExecuteViaTerminalStayOpen := False;
  gIncludeFileAssociation := False;
  gFileAssocFilenameStyle := pfsAbsolutePath;
  gFileAssocPathToBeRelativeTo := EnvVarCommanderPath;
  gFileAssocPathModifierElements := [];

  { Tree View Menu }
  gUseTreeViewMenuWithDirectoryHotlistFromMenuCommand := False;
  gUseTreeViewMenuWithDirectoryHotlistFromDoubleClick := False;
  gUseTreeViewMenuWithFavoriteTabsFromMenuCommand := False;
  gUseTreeViewMenuWithFavoriteTabsFromDoubleClick := False;
  gUseTreeViewMenuWithDirHistory := False;
  gUseTreeViewMenuWithViewHistory := False;
  gUseTreeViewMenuWithCommandLineHistory := False;
  gTreeViewMenuShortcutExit := True;
  gTreeViewMenuSingleClickExit := True;
  gTreeViewMenuDoubleClickExit := True;
  for iIndexContextMode:=0 to (ord(tvmcLASTONE)-2) do
  begin
    gTreeViewMenuOptions[iIndexContextMode].CaseSensitive := False;
    gTreeViewMenuOptions[iIndexContextMode].IgnoreAccents := True;
    gTreeViewMenuOptions[iIndexContextMode].ShowWholeBranchIfMatch := False;
  end;
  gTreeViewMenuUseKeyboardShortcut := True;
  gTVMBackgroundColor := clForm;
  gTVMShortcutColor := clRed;
  gTVMNormalTextColor := clWindowText;
  gTVMSecondaryTextColor := clWindowFrame;
  gTVMFoundTextColor := clHighLight;
  gTVMUnselectableTextColor := clGrayText;
  gTVMCursorColor := clHighlight;
  gTVMShortcutUnderCursor := clHighlightText;
  gTVMNormalTextUnderCursor := clHighlightText;
  gTVMSecondaryTextUnderCursor := clBtnHighlight;
  gTVMFoundTextUnderCursor := clYellow;
  gTVMUnselectableUnderCursor := clGrayText;

  { - Other - }
  gGoToRoot := False;
  gLuaLib := LuaDLL;
  gActiveRight := False;
  gNameSCFile := 'shortcuts.scf';
  gHotKeySortOrder := hksoByCommand;
  gUseEnterToCloseHotKeyEditor := True;
  gLastUsedPacker := 'zip';
  gLastDoAnyCommand := 'cm_Refresh';
  gbMarkMaskCaseSensitive := False;
  gbMarkMaskIgnoreAccents := False;
  gMarkMaskFilterWindows := False;
  gMarkShowWantedAttribute := False;
  gMarkDefaultWantedAttribute := '';
  gMarkLastWantedAttribute := '';

  { TotalCommander Import/Export }
  //Will search minimally where TC could be installed so the default value would have some chances to be correct.
  {$IFDEF MSWINDOWS}
  gTotalCommanderExecutableFilename:='';
  gTotalCommanderConfigFilename:='';
  gTotalCommanderToolbarPath:='';

  if mbFileExists('c:\totalcmd\TOTALCMD.EXE') then gTotalCommanderExecutableFilename:='c:\totalcmd\TOTALCMD.EXE';
  if (gTotalCommanderExecutableFilename='') AND  (mbFileExists('c:\totalcmd\TOTALCMD64.EXE')) then gTotalCommanderExecutableFilename:='c:\totalcmd\TOTALCMD64.EXE';
  if gTotalCommanderExecutableFilename='' then gTotalCommanderExecutableFilename:=GetPathNameIfItMatch(CSIDL_COMMON_PROGRAMS,'totalcmd\TOTALCMD.EXE');
  if gTotalCommanderExecutableFilename='' then gTotalCommanderExecutableFilename:=GetPathNameIfItMatch(CSIDL_PROGRAMS,'totalcmd\TOTALCMD.EXE');
  if gTotalCommanderExecutableFilename='' then gTotalCommanderExecutableFilename:=GetPathNameIfItMatch(CSIDL_PROGRAM_FILESX86,'totalcmd\TOTALCMD.EXE');
  if gTotalCommanderExecutableFilename='' then gTotalCommanderExecutableFilename:=GetPathNameIfItMatch(CSIDL_PROGRAM_FILES_COMMON,'totalcmd\TOTALCMD.EXE');
  if gTotalCommanderExecutableFilename='' then gTotalCommanderExecutableFilename:=GetPathNameIfItMatch(CSIDL_PROGRAM_FILES_COMMONX86,'totalcmd\TOTALCMD.EXE');
  if gTotalCommanderExecutableFilename='' then gTotalCommanderExecutableFilename:=GetPathNameIfItMatch(CSIDL_COMMON_PROGRAMS,'totalcmd\TOTALCMD64.EXE');
  if gTotalCommanderExecutableFilename='' then gTotalCommanderExecutableFilename:=GetPathNameIfItMatch(CSIDL_PROGRAMS,'totalcmd\TOTALCMD64.EXE');
  if gTotalCommanderExecutableFilename='' then gTotalCommanderExecutableFilename:=GetPathNameIfItMatch(CSIDL_PROGRAM_FILES_COMMON,'totalcmd\TOTALCMD64.EXE');

  if mbFileExists('c:\totalcmd\wincmd.ini') then gTotalCommanderConfigFilename:='c:\totalcmd\wincmd.ini';
  if gTotalCommanderConfigFilename='' then gTotalCommanderConfigFilename:=GetPathNameIfItMatch(CSIDL_APPDATA,'GHISLER\wincmd.ini');
  if gTotalCommanderConfigFilename='' then gTotalCommanderConfigFilename:=GetPathNameIfItMatch(CSIDL_PROFILE,'wincmd.ini');
  if gTotalCommanderConfigFilename='' then gTotalCommanderConfigFilename:=GetPathNameIfItMatch(CSIDL_WINDOWS,'wincmd.ini'); //Don't laugh. The .INI file were originally saved in windows folder for many programs!

  if gTotalCommanderConfigFilename<>'' then gTotalCommanderToolbarPath:=ExtractFilePath(gTotalCommanderConfigFilename);
  {$ENDIF}

  gExts.Clear;
  gColorExt.Clear;
  gFileInfoToolTip.Clear;
  gDirectoryHotlist.Clear;
  gFavoriteTabsList.Clear;
  glsDirHistory.Clear;
  glsMaskHistory.Clear;
  glsSearchHistory.Clear;
  glsSearchPathHistory.Clear;
  glsReplaceHistory.Clear;
  glsReplacePathHistory.Clear;
  glsCreateDirectoriesHistory.Clear;
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
  DefaultDateTimeFormat := FormatSettings.ShortDateFormat + ' hh:nn:ss';
  FormatSettings.DecimalSeparator:='.';
end;

function OpenConfig(var ErrorMessage: String): Boolean;
begin
  if Assigned(gConfig) then
    Exit(True);

  // Check global directory for XML config.
  if (gpCmdLineCfgDir = EmptyStr) then
  begin
    gUseConfigInProgramDir:= mbFileExists(gpGlobalCfgDir + 'doublecmd.inf');

    if gUseConfigInProgramDir or mbFileExists(gpGlobalCfgDir + 'doublecmd.xml') then
    begin
      gConfig := TXmlConfig.Create(gpGlobalCfgDir + 'doublecmd.xml');
      if mbFileExists(gpGlobalCfgDir + 'doublecmd.xml') then
      begin
        if mbFileAccess(gpGlobalCfgDir + 'doublecmd.xml', fmOpenRead or fmShareDenyWrite) then
        begin
          LoadConfigCheckErrors(@LoadGlobalConfig, gpGlobalCfgDir + 'doublecmd.xml', ErrorMessage);
          gConfig.TryGetValue(gConfig.RootNode, 'Configuration/UseConfigInProgramDir', gUseConfigInProgramDir);

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
        begin
          // Configuration file is not readable.
          AddStrWithSep(ErrorMessage,
              'Config file "' + gpGlobalCfgDir + 'doublecmd.xml' +
              '" exists but is not readable.',
              LineEnding);
          Exit(False);
        end;
      end;
    end;
  end;

  // Check user directory for XML config.
  if not Assigned(gConfig) and mbFileExists(gpCfgDir + 'doublecmd.xml') then
  begin
    gConfig := TXmlConfig.Create(gpCfgDir + 'doublecmd.xml');
    gUseConfigInProgramDir := False;
    if mbFileAccess(gpCfgDir + 'doublecmd.xml', fmOpenRead or fmShareDenyWrite) then
    begin
      LoadConfigCheckErrors(@LoadGlobalConfig, gpCfgDir + 'doublecmd.xml', ErrorMessage);
    end
    else
    begin
      // Configuration file is not readable.
      AddStrWithSep(ErrorMessage,
          'Config file "' + gpCfgDir + 'doublecmd.xml' +
          '" exists but is not readable.',
          LineEnding);
      Exit(False);
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
  begin
    gpCfgDir := gpGlobalCfgDir;
    UpdateEnvironmentVariable;
  end;

  if mbFileExists(gpCfgDir + 'doublecmd.xml') and
     (not mbFileAccess(gpCfgDir + 'doublecmd.xml', fmOpenWrite or fmShareDenyWrite)) then
  begin
    DCDebug('Warning: Config file "' + gpCfgDir + 'doublecmd.xml' +
            '" is not accessible for writing. Configuration will not be saved.');
  end;

  if not mbDirectoryExists(gpCfgDir) then
    mbForceDirectory(gpCfgDir);

  Result := True;
end;

function LoadGlobs: Boolean;
var
  ErrorMessage: String = '';
begin
  Result := False;
  if not OpenConfig(ErrorMessage) then
    Exit;

  DCDebug('Loading configuration from ', gpCfgDir);

  SetDefaultConfigGlobs;
  if Assigned(gConfig) then
    LoadXmlConfig
  else
  begin
    DCDebug('Error: No config created.');
    Exit(False);
  end;

  { Favorite Tabs }
  gFavoriteTabsList.LoadAllListFromXml;

  // Update plugins if DC version is changed
  if (gPreviousVersion <> dcVersion) then UpdatePlugins;

  // Adjust icons size
  gIconsSize:= AdjustIconSize(gIconsSize, gPixelsPerInch);
  gDiskIconsSize:= AdjustIconSize(gDiskIconsSize, gPixelsPerInch);
  gToolBarIconSize:= AdjustIconSize(gToolBarIconSize, gPixelsPerInch);
  gToolBarButtonSize:= AdjustIconSize(gToolBarButtonSize, gPixelsPerInch);
  // Set secondary variables for options that need restart.
  gShowIconsNew := gShowIcons;
  gIconsSizeNew := gIconsSize;
  gIconsInMenusSizeNew := gIconsInMenusSize;

  CopySettingsFiles;

  { Internal associations }
  //"LoadExtsConfig" checks itself if file is present or not
  LoadConfigCheckErrors(@LoadExtsConfig, gpCfgDir + gcfExtensionAssociation, ErrorMessage);

  if mbFileExists(gpCfgDir + 'dirhistory.txt') then
  begin
    LoadStringsFromFile(glsDirHistory, gpCfgDir + 'dirhistory.txt', cMaxStringItems);
    mbRenameFile(gpCfgDir + 'dirhistory.txt', gpCfgDir + 'dirhistory.txt.obsolete');
  end;
  if mbFileExists(gpCfgDir + 'cmdhistory.txt') then
  begin
    LoadStringsFromFile(glsCmdLineHistory, gpCfgDir + 'cmdhistory.txt', cMaxStringItems);
    mbRenameFile(gpCfgDir + 'cmdhistory.txt', gpCfgDir + 'cmdhistory.txt.obsolete');
  end;
  if mbFileExists(gpCfgDir + 'maskhistory.txt') then
  begin
    LoadStringsFromFile(glsMaskHistory, gpCfgDir + 'maskhistory.txt', cMaxStringItems);
    mbRenameFile(gpCfgDir + 'maskhistory.txt', gpCfgDir + 'maskhistory.txt.obsolete');
  end;
  if mbFileExists(gpCfgDir + 'searchpathhistory.txt') then
  begin
    LoadStringsFromFile(glsSearchPathHistory, gpCfgDir + 'searchpathhistory.txt', cMaxStringItems);
    mbRenameFile(gpCfgDir + 'searchpathhistory.txt', gpCfgDir + 'searchpathhistory.txt.obsolete');
  end;
  if mbFileExists(gpCfgDir + 'searchhistory.txt') then
  begin
    LoadStringsFromFile(glsSearchHistory, gpCfgDir + 'searchhistory.txt', cMaxStringItems);
    mbRenameFile(gpCfgDir + 'searchhistory.txt', gpCfgDir + 'searchhistory.txt.obsolete');
  end;
  if mbFileExists(gpCfgDir + 'replacehistory.txt') then
  begin
    LoadStringsFromFile(glsReplaceHistory, gpCfgDir + 'replacehistory.txt', cMaxStringItems);
    mbRenameFile(gpCfgDir + 'replacehistory.txt', gpCfgDir + 'replacehistory.txt.obsolete');
  end;
  if mbFileExists(gpCfgDir + 'replacehpathhistory.txt') then
  begin
    LoadStringsFromFile(glsReplacePathHistory, gpCfgDir + 'replacepathhistory.txt', cMaxStringItems);
    mbRenameFile(gpCfgDir + 'replacepathhistory.txt', gpCfgDir + 'replacepathhistory.txt.obsolete');
  end;
  LoadStringsFromFile(glsIgnoreList, ReplaceEnvVars(gIgnoreListFile));

  { Hotkeys }
  if not mbFileExists(gpCfgDir + gNameSCFile) then
    gNameSCFile := 'shortcuts.scf';
  // Rename old shortcuts file to new name.
  if mbFileExists(gpCfgDir + 'shortcuts.ini') and
     not mbFileExists(gpCfgDir + gNameSCFile) then
       mbRenameFile(gpCfgDir + 'shortcuts.ini', gpCfgDir + gNameSCFile);
  LoadConfigCheckErrors(@LoadHotManConfig, gpCfgDir + gNameSCFile, ErrorMessage);

  { MultiArc addons }
  if mbFileExists(gpCfgDir + sMULTIARC_FILENAME) then
    LoadConfigCheckErrors(@LoadMultiArcConfig, gpCfgDir + sMULTIARC_FILENAME, ErrorMessage);

  { Various history }
  if mbFileExists(gpCfgDir + 'history.xml') then
    LoadConfigCheckErrors(@LoadHistoryConfig, gpCfgDir + 'history.xml', ErrorMessage);

  { Localization }
  msgLoadLng;

  FillFileFuncList;

  { Specialdir }
  if gShowOnlyValidEnv=FALSE then gSpecialDirList.PopulateSpecialDir;  //We must reload it if user has included the unsignificant environment variable. But anyway, this will not happen often.

  Result := AskUserOnError(ErrorMessage);
end;

procedure SaveGlobs;
var
  TmpConfig: TXmlConfig;
  ErrMsg: String = '';
begin
  if (gUseConfigInProgramDirNew <> gUseConfigInProgramDir) and
     (gpCmdLineCfgDir = EmptyStr) then
    begin
      LoadPaths;

      if gUseConfigInProgramDirNew then
      begin
        gpCfgDir := gpGlobalCfgDir;
        UpdateEnvironmentVariable;
        FileClose(mbFileCreate(gpGlobalCfgDir + 'doublecmd.inf'));
      end
      else begin
        if mbFileExists(gpGlobalCfgDir + 'doublecmd.inf') then
          mbDeleteFile(gpGlobalCfgDir + 'doublecmd.inf')
      end;

      { Remove location of configuration files from XML}
      if mbFileAccess(gpGlobalCfgDir + 'doublecmd.xml', fmOpenWrite or fmShareDenyWrite) then
      begin
        TmpConfig := TXmlConfig.Create(gpGlobalCfgDir + 'doublecmd.xml', True);
        try
          TmpConfig.DeleteNode(TmpConfig.RootNode, 'Configuration/UseConfigInProgramDir');
          TmpConfig.Save;
        finally
          TmpConfig.Free;
        end;
      end;

      gConfig.FileName := gpCfgDir + 'doublecmd.xml';
    end;

  if mbFileAccess(gpCfgDir, fmOpenWrite or fmShareDenyNone) then
  begin
    SaveWithCheck(@SaveCfgIgnoreList, 'ignore list', ErrMsg);
    SaveWithCheck(@SaveCfgMainConfig, 'main configuration', ErrMsg);
    SaveWithCheck(@SaveHistoryConfig, 'various history', ErrMsg);

    if ErrMsg <> EmptyStr then
      DebugLn(ErrMsg);
  end
  else
    DebugLn('Not saving configuration - no write access to ', gpCfgDir);
end;

procedure LoadContentPlugins;
var
  I: Integer;
  Module: TWdxModule;
  Template: TSearchTemplate;
  Content: TPluginSearchRec;
begin
  for I:= 0 to gSearchTemplateList.Count - 1 do
  begin
    Template:= gSearchTemplateList.Templates[I];
    if Template.SearchRecord.ContentPlugin then
    begin
      for Content in Template.SearchRecord.ContentPlugins do
      begin
        Module:= gWDXPlugins.GetWdxModule(Content.Plugin);
        if Assigned(Module) and (Module.IsLoaded = False) then
        begin
          Module.LoadModule;
        end;
      end;
    end;
  end;
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
      gConfig.GetFont(Node, '', FontOptions.Name, FontOptions.Size, Integer(FontOptions.Style), Integer(FontOptions.Quality),
                                FontOptions.Name, FontOptions.Size, Integer(FontOptions.Style), Integer(FontOptions.Quality));
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
  LoadedConfigVersion, iIndexContextMode: Integer;
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

    DoLoadLng;

    { Since language file has been loaded, we'll not set our default memory size string. They will be in the correct language }
    gSizeDisplayUnits[fsfFloat] := ''; //Not used, but at least it will be defined.
    gSizeDisplayUnits[fsfByte] := ''; //Not user changeable by legacy and empty by legacy.
    gSizeDisplayUnits[fsfKilo] := ' ' + Trim(rsLegacyDisplaySizeSingleLetterKilo); //Not user changeable by legacy, taken from language file since 2018-11.
    gSizeDisplayUnits[fsfMega] := ' ' + Trim(rsLegacyDisplaySizeSingleLetterMega); //Not user changeable by legacy, taken from language file since 2018-11.
    gSizeDisplayUnits[fsfGiga] := ' ' + Trim(rsLegacyDisplaySizeSingleLetterGiga); //Not user changeable by legacy, taken from language file since 2018-11.
    gSizeDisplayUnits[fsfTera] := ' ' + Trim(rsLegacyDisplaySizeSingleLetterTera); //Not user changeable by legacy, taken from language file since 2018-11.
    gSizeDisplayUnits[fsfPersonalizedFloat] := ''; //Not used, but at least it will be defined.
    gSizeDisplayUnits[fsfPersonalizedByte] := rsDefaultPersonalizedAbbrevByte;
    gSizeDisplayUnits[fsfPersonalizedKilo] := rsDefaultPersonalizedAbbrevKilo;
    gSizeDisplayUnits[fsfPersonalizedMega] := rsDefaultPersonalizedAbbrevMega;
    gSizeDisplayUnits[fsfPersonalizedGiga] := rsDefaultPersonalizedAbbrevGiga;
    gSizeDisplayUnits[fsfPersonalizedTera] := rsDefaultPersonalizedAbbrevTera;

    { Since language has been loaded, we may now load our font usage name}
    gFonts[dcfMain].Usage := rsFontUsageMain;
    gFonts[dcfEditor].Usage := rsFontUsageEditor;
    gFonts[dcfViewer].Usage := rsFontUsageViewer;
    gFonts[dcfViewerBook].Usage := rsFontUsageViewerBook;
    gFonts[dcfLog].Usage := rsFontUsageLog;
    gFonts[dcfConsole].Usage := rsFontUsageConsole;
    gFonts[dcfPathEdit].Usage := rsFontUsagePathEdit;
    gFonts[dcfFunctionButtons].Usage := rsFontUsageFunctionButtons;
    gFonts[dcfSearchResults].Usage := rsFontUsageSearchResults;
    gFonts[dcfTreeViewMenu].Usage := rsFontUsageTreeViewMenu;

    { Behaviours page }
    Node := Root.FindNode('Behaviours');
    if Assigned(Node) then
    begin
      gGoToRoot := GetValue(Node, 'GoToRoot', gGoToRoot);
      gActiveRight := GetValue(Node, 'ActiveRight', gActiveRight);

      //Trick to split initial legacy command for terminal
      //  Initial name in config was "RunInTerminal".
      //  If it is still present in config, it means we're running from an older version.
      //  So if it's different than our setting, let's split it to get actual "cmd" and "params".
      //  New version uses "RunInTerminalCloseCmd" from now on.
      //  ALSO, in the case of Windows, installation default was "cmd.exe /K ..." which means Run-and-stayopen
      //        in the case of Unix, installation default was "xterm -e sh -c ..." which means Run-and-close
      //  So because of these two different behavior, transition is done slightly differently.
      {$IF DEFINED(MSWINDOWS)}
      gRunInTermStayOpenCmd := GetValue(Node, 'RunInTerminal', gRunInTermStayOpenCmd);
      if gRunInTermStayOpenCmd<>RunInTermCloseCmd then
      begin
        SplitCmdLineToCmdParams(gRunInTermStayOpenCmd, gRunInTermStayOpenCmd, gRunInTermStayOpenParams);
        if gRunInTermStayOpenParams<>'' then gRunInTermStayOpenParams:=gRunInTermStayOpenParams+' {command}' else gRunInTermStayOpenParams:='{command}';
      end
      else
      begin
        gRunInTermStayOpenCmd := GetValue(Node, 'RunInTerminalStayOpenCmd', RunInTermStayOpenCmd);
        gRunInTermStayOpenParams := GetValue(Node, 'RunInTerminalStayOpenParams', RunInTermStayOpenParams);
      end;
      gRunInTermCloseCmd := GetValue(Node, 'RunInTerminalCloseCmd', RunInTermCloseCmd);
      gRunInTermCloseParams := GetValue(Node, 'RunInTerminalCloseParams', RunInTermCloseParams);
      {$ELSE}
      gRunInTermCloseCmd := GetValue(Node, 'RunInTerminal', gRunInTermCloseCmd);
      if gRunInTermCloseCmd<>RunInTermCloseCmd then
      begin
        SplitCmdLineToCmdParams(gRunInTermCloseCmd, gRunInTermCloseCmd, gRunInTermCloseParams);
        if gRunInTermCloseParams<>'' then gRunInTermCloseParams:=gRunInTermCloseParams+' {command}' else gRunInTermStayOpenParams:='{command}';
      end
      else
      begin
        gRunInTermCloseCmd := GetValue(Node, 'RunInTerminalCloseCmd', RunInTermCloseCmd);
        gRunInTermCloseParams := GetValue(Node, 'RunInTerminalCloseParams', RunInTermCloseParams);
      end;
      gRunInTermStayOpenCmd := GetValue(Node, 'RunInTerminalStayOpenCmd', RunInTermStayOpenCmd);
      gRunInTermStayOpenParams := GetValue(Node, 'RunInTerminalStayOpenParams', RunInTermStayOpenParams);
      {$ENDIF}

      // Let's try to be backward comptible and re-load possible old values for terminal launch command
      gRunTermCmd := GetValue(Node, 'JustRunTerminal', '');
      if gRunTermCmd = '' then
      begin
        gRunTermCmd := GetValue(Node, 'RunTerminal', RunTermCmd);
        SplitCmdLineToCmdParams(gRunTermCmd, gRunTermCmd,gRunTermParams);
      end
      else
      begin
        gRunTermParams := GetValue(Node, 'JustRunTermParams', RunTermParams);
      end;

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
      if LoadedConfigVersion < 6 then
      begin
        if GetValue(Node, 'ShortFileSizeFormat', True) then
          gFileSizeFormat := fsfFloat
        else
          gFileSizeFormat := fsfByte;
      end
      else
      begin
        gFileSizeFormat := TFileSizeFormat(GetValue(Node, 'FileSizeFormat', Ord(gFileSizeFormat)));
      end;
      if LoadedConfigVersion < 12 then
      begin
        gHeaderDigits := GetValue(Node, 'HeaderFooterDigits', gHeaderDigits);
        gFooterDigits := GetValue(Node, 'HeaderFooterDigits', gFooterDigits);
        gHeaderSizeFormat := TFileSizeFormat(GetValue(Node,'HeaderFooterSizeFormat', ord(gHeaderSizeFormat)));
        gFooterSizeFormat := TFileSizeFormat(GetValue(Node,'HeaderFooterSizeFormat', ord(gFooterSizeFormat)));
      end
      else begin
        gHeaderDigits := GetValue(Node, 'HeaderDigits', gHeaderDigits);
        gFooterDigits := GetValue(Node, 'FooterDigits', gFooterDigits);
        gHeaderSizeFormat := TFileSizeFormat(GetValue(Node,'HeaderSizeFormat', ord(gHeaderSizeFormat)));
        gFooterSizeFormat := TFileSizeFormat(GetValue(Node,'FooterSizeFormat', ord(gFooterSizeFormat)));
      end;
      gOperationSizeFormat := TFileSizeFormat(GetValue(Node, 'OperationSizeFormat', Ord(gOperationSizeFormat)));
      gFileSizeDigits := GetValue(Node, 'FileSizeDigits', gFileSizeDigits);
      gOperationSizeDigits := GetValue(Node, 'OperationSizeDigits', gOperationSizeDigits);
      gSizeDisplayUnits[fsfPersonalizedByte] := Trim(GetValue(Node, 'PersonalizedByte', gSizeDisplayUnits[fsfPersonalizedByte]));
      if gSizeDisplayUnits[fsfPersonalizedByte]<>'' then gSizeDisplayUnits[fsfPersonalizedByte] := ' ' + gSizeDisplayUnits[fsfPersonalizedByte];
      gSizeDisplayUnits[fsfPersonalizedKilo] := ' ' + Trim(GetValue(Node, 'PersonalizedKilo', gSizeDisplayUnits[fsfPersonalizedKilo]));
      gSizeDisplayUnits[fsfPersonalizedMega] := ' ' + Trim(GetValue(Node, 'PersonalizedMega', gSizeDisplayUnits[fsfPersonalizedMega]));
      gSizeDisplayUnits[fsfPersonalizedGiga] := ' ' + Trim(GetValue(Node, 'PersonalizedGiga', gSizeDisplayUnits[fsfPersonalizedGiga]));
      gSizeDisplayUnits[fsfPersonalizedTera] := ' ' + Trim(GetValue(Node, 'PersonalizedTera', gSizeDisplayUnits[fsfPersonalizedTera]));
      gMinimizeToTray := GetValue(Node, 'MinimizeToTray', gMinimizeToTray);
      gAlwaysShowTrayIcon := GetValue(Node, 'AlwaysShowTrayIcon', gAlwaysShowTrayIcon);
      gMouseSelectionEnabled := GetAttr(Node, 'Mouse/Selection/Enabled', gMouseSelectionEnabled);
      gMouseSelectionButton := GetValue(Node, 'Mouse/Selection/Button', gMouseSelectionButton);
      gMouseSingleClickStart := GetValue(Node, 'Mouse/SingleClickStart', gMouseSingleClickStart);
      gMouseSelectionIconClick := GetValue(Node, 'Mouse/Selection/IconClick', gMouseSelectionIconClick);
      gScrollMode := TScrollMode(GetValue(Node, 'Mouse/ScrollMode', Integer(gScrollMode)));
      gWheelScrollLines:= GetValue(Node, 'Mouse/WheelScrollLines', gWheelScrollLines);
      gAutoFillColumns := GetValue(Node, 'AutoFillColumns', gAutoFillColumns);
      gAutoSizeColumn := GetValue(Node, 'AutoSizeColumn', gAutoSizeColumn);
      gDateTimeFormat := GetValidDateTimeFormat(GetValue(Node, 'DateTimeFormat', gDateTimeFormat), DefaultDateTimeFormat);
      gCutTextToColWidth := GetValue(Node, 'CutTextToColumnWidth', gCutTextToColWidth);
      gExtendCellWidth := GetValue(Node, 'ExtendCellWidth', gExtendCellWidth);
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
      if LoadedConfigVersion < 8 then begin
        gBriefViewFileExtAligned := GetValue(Node, 'BriefViewFileExtAligned', gBriefViewFileExtAligned);
      end;
    end;

    { Tools page }
    GetExtTool(gConfig.FindNode(Root, 'Tools/Viewer'), gExternalTools[etViewer]);
    GetExtTool(gConfig.FindNode(Root, 'Tools/Editor'), gExternalTools[etEditor]);
    GetExtTool(gConfig.FindNode(Root, 'Tools/Differ'), gExternalTools[etDiffer]);

    { Differ related}
    Node := Root.FindNode('Tools');
    SubNode := FindNode(Node, 'Differ', TRUE);
    gResultingFramePositionAfterCompare := TResultingFramePositionAfterCompare(GetValue(SubNode, 'FramePosAfterComp', Integer(gResultingFramePositionAfterCompare)));

    { Fonts page }

    GetDCFont(gConfig.FindNode(Root, 'Fonts/Main'), gFonts[dcfMain]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/Editor'), gFonts[dcfEditor]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/Viewer'), gFonts[dcfViewer]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/ViewerBook'), gFonts[dcfViewerBook]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/Log'), gFonts[dcfLog]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/Console'), gFonts[dcfConsole]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/PathEdit'), gFonts[dcfPathEdit]);
    GetDCFont(gConfig.FindNode(Root, 'Fonts/FunctionButtons'), gFonts[dcfFunctionButtons]);
    if LoadedConfigVersion >= 11 then GetDCFont(gConfig.FindNode(Root, 'Fonts/SearchResults'), gFonts[dcfSearchResults]); //Let's ignore possible previous setting for this and keep our default.
    GetDCFont(gConfig.FindNode(Root, 'Fonts/TreeViewMenu'), gFonts[dcfTreeViewMenu]);

    { Colors page }
    Node := Root.FindNode('Colors');
    if Assigned(Node) then
    begin
      gUseCursorBorder := GetValue(Node, 'UseCursorBorder', gUseCursorBorder);
      gCursorBorderColor := GetValue(Node, 'CursorBorderColor', gCursorBorderColor);
      gUseFrameCursor := GetValue(Node, 'UseFrameCursor', gUseFrameCursor);
      gForeColor := GetValue(Node, 'Foreground', gForeColor);
      gBackColor := GetValue(Node, 'Background', gBackColor);
      gBackColor2 := GetValue(Node, 'Background2', gBackColor2);
      gMarkColor := GetValue(Node, 'Mark', gMarkColor);
      gCursorColor := GetValue(Node, 'Cursor', gCursorColor);
      gCursorText := GetValue(Node, 'CursorText', gCursorText);
      gInactiveCursorColor := GetValue(Node, 'InactiveCursor', gInactiveCursorColor);
      gInactiveMarkColor := GetValue(Node, 'InactiveMark', gInactiveMarkColor);
      gUseInvertedSelection := GetValue(Node, 'UseInvertedSelection', gUseInvertedSelection);
      gUseInactiveSelColor := GetValue(Node, 'UseInactiveSelColor', gUseInactiveSelColor);
      gAllowOverColor   := GetValue(Node, 'AllowOverColor', gAllowOverColor);
      gBorderFrameWidth := GetValue(Node, 'gBorderFrameWidth', gBorderFrameWidth);

      gPathActiveColor := GetValue(Node, 'PathLabel/ActiveColor', gPathActiveColor);
      gPathActiveFontColor := GetValue(Node, 'PathLabel/ActiveFontColor', gPathActiveFontColor);
      gPathInactiveColor := GetValue(Node, 'PathLabel/InactiveColor', gPathInactiveColor);
      gPathInactiveFontColor := GetValue(Node, 'PathLabel/InactiveFontColor', gPathInactiveFontColor);

      gInactivePanelBrightness := GetValue(Node, 'InactivePanelBrightness', gInactivePanelBrightness);
      gIndUseGradient := GetValue(Node, 'FreeSpaceIndicator/UseGradient', gIndUseGradient);
      gIndForeColor := GetValue(Node, 'FreeSpaceIndicator/ForeColor', gIndForeColor);
      gIndBackColor := GetValue(Node, 'FreeSpaceIndicator/BackColor', gIndBackColor);

      gLogInfoColor:= GetValue(Node, 'LogWindow/Info', gLogInfoColor);
      gLogErrorColor:= GetValue(Node, 'LogWindow/Error', gLogErrorColor);
      gLogSuccessColor:= GetValue(Node, 'LogWindow/Success', gLogSuccessColor);
    end;

    { ToolTips page }
    Node := Root.FindNode('ToolTips');
    if Assigned(Node) then
    begin
      gShowToolTip := GetValue(Node, 'ShowToolTipMode', gShowToolTip);
      gShowToolTipMode := TToolTipMode(GetValue(Node, 'ActualToolTipMode', Integer(gShowToolTipMode)));
      gToolTipHideTimeOut := TToolTipHideTimeOut(GetValue(Node, 'ToolTipHideTimeOut', Integer(gToolTipHideTimeOut)));
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
        gToolBarShowCaptions := GetValue(SubNode, 'ShowCaptions', gToolBarShowCaptions);
        gToolbarReportErrorWithCommands := GetValue(SubNode,'ReportErrorWithCommands',gToolbarReportErrorWithCommands);
        gToolbarFilenameStyle := TConfigFilenameStyle(GetValue(SubNode, 'FilenameStyle', ord(gToolbarFilenameStyle)));
        gToolbarPathToBeRelativeTo := gConfig.GetValue(SubNode, 'PathToBeRelativeTo', gToolbarPathToBeRelativeTo);
        gToolbarPathModifierElements := tToolbarPathModifierElements(GetValue(SubNode, 'PathModifierElements', Integer(gToolbarPathModifierElements)));
      end;
      SubNode := Node.FindNode('MiddleBar');
      if Assigned(SubNode) then
      begin
        gMiddleToolBar := GetAttr(SubNode, 'Enabled', gMiddleToolBar);
        gMiddleToolBarFlat := GetValue(SubNode, 'FlatIcons', gMiddleToolBarFlat);
        gMiddleToolBarButtonSize := GetValue(SubNode, 'ButtonHeight', gMiddleToolBarButtonSize);
        gMiddleToolBarIconSize := GetValue(SubNode, 'IconSize', gMiddleToolBarIconSize);
        gMiddleToolBarShowCaptions := GetValue(SubNode, 'ShowCaptions', gMiddleToolBarShowCaptions);
        gMiddleToolbarReportErrorWithCommands := GetValue(SubNode,'ReportErrorWithCommands', gMiddleToolbarReportErrorWithCommands);
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
      gSeparateTree := GetValue(Node, 'SeparateTree', gSeparateTree);
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
      gUpperCaseDriveLetter := GetValue(Node, 'UppercaseDriveLetter', gUpperCaseDriveLetter);
      gShowColonAfterDrive := GetValue(Node, 'ShowColonAfterDrive', gShowColonAfterDrive);
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
        gSortSpecial := GetValue(SubNode, 'SpecialSorting', gSortSpecial);
        gSortFolderMode:= TSortFolderMode(GetValue(SubNode, 'SortFolderMode', Integer(gSortFolderMode)));
        gNewFilesPosition := TNewFilesPosition(GetValue(SubNode, 'NewFilesPosition', Integer(gNewFilesPosition)));
        gUpdatedFilesPosition := TUpdatedFilesPosition(GetValue(SubNode, 'UpdatedFilesPosition', Integer(gUpdatedFilesPosition)));
      end;
      SubNode := FindNode(Node, 'ColumnsView');
      if Assigned(SubNode) then
      begin
        gColumnsAutoSaveWidth := GetValue(SubNode, 'AutoSaveWidth', gColumnsAutoSaveWidth);
        gColumnsTitleStyle := TTitleStyle(GetValue(SubNode, 'TitleStyle', Integer(gColumnsTitleStyle)));
      end;
      SubNode := Node.FindNode('BriefView');
      if Assigned(SubNode) then
      begin
        gBriefViewFileExtAligned := GetValue(SubNode, 'FileExtAligned', gBriefViewFileExtAligned);
        SubNode := SubNode.FindNode('Columns');
        if Assigned(SubNode) then
        begin
          gBriefViewFixedWidth := GetValue(SubNode, 'FixedWidth', gBriefViewFixedWidth);
          gBriefViewFixedCount := GetValue(SubNode, 'FixedCount', gBriefViewFixedCount);
          gBriefViewMode := TBriefViewMode(GetValue(SubNode, 'AutoSize', Integer(gBriefViewMode)));
        end;
      end;
      gExtraLineSpan := GetValue(Node, 'ExtraLineSpan', gExtraLineSpan);
      gFolderPrefix := GetValue(Node, 'FolderPrefix', gFolderPrefix);
      gFolderPostfix := GetValue(Node, 'FolderPostfix', gFolderPostfix);
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
      gLongNameAlert := GetValue(Node, 'LongNameAlert', gLongNameAlert);
      gHashBlockSize := GetValue(Node, 'HashBufferSize', gHashBlockSize);
      gUseMmapInSearch := GetValue(Node, 'UseMmapInSearch', gUseMmapInSearch);
      gPartialNameSearch := GetValue(Node, 'PartialNameSearch', gPartialNameSearch);
      gInitiallyClearFileMask := GetValue(Node, 'InitiallyClearFileMask', gInitiallyClearFileMask);
      gNewSearchClearFiltersAction := TFiltersOnNewSearch(GetValue(Node, 'NewSearchClearFiltersAction', integer(gNewSearchClearFiltersAction)));
      gShowMenuBarInFindFiles := GetValue(Node, 'ShowMenuBarInFindFiles', gShowMenuBarInFindFiles);
      gWipePassNumber := GetValue(Node, 'WipePassNumber', gWipePassNumber);
      gDropReadOnlyFlag := GetValue(Node, 'DropReadOnlyFlag', gDropReadOnlyFlag);
      gProcessComments := GetValue(Node, 'ProcessComments', gProcessComments);
      gRenameSelOnlyName := GetValue(Node, 'RenameSelOnlyName', gRenameSelOnlyName);
      gShowCopyTabSelectPanel := GetValue(Node, 'ShowCopyTabSelectPanel', gShowCopyTabSelectPanel);
      gUseTrash := GetValue(Node, 'UseTrash', gUseTrash);
      gSkipFileOpError := GetValue(Node, 'SkipFileOpError', gSkipFileOpError);
      gTypeOfDuplicatedRename := tDuplicatedRename(GetValue(Node, 'TypeOfDuplicatedRename', Integer(gTypeOfDuplicatedRename)));
      gShowDialogOnDragDrop := GetValue(Node, 'ShowDialogOnDragDrop', gShowDialogOnDragDrop);
      gDragAndDropDesiredTextFormat[DropTextRichText_Index].DesireLevel := GetValue(Node, 'DragAndDropTextRichtextDesireLevel', gDragAndDropDesiredTextFormat[DropTextRichText_Index].DesireLevel);
      gDragAndDropDesiredTextFormat[DropTextHtml_Index].DesireLevel := GetValue(Node, 'DragAndDropTextHtmlDesireLevel',gDragAndDropDesiredTextFormat[DropTextHtml_Index].DesireLevel);
      gDragAndDropDesiredTextFormat[DropTextUnicode_Index].DesireLevel := GetValue(Node, 'DragAndDropTextUnicodeDesireLevel',gDragAndDropDesiredTextFormat[DropTextUnicode_Index].DesireLevel);
      gDragAndDropDesiredTextFormat[DropTextSimpleText_Index].DesireLevel := GetValue(Node, 'DragAndDropTextSimpletextDesireLevel',gDragAndDropDesiredTextFormat[DropTextSimpleText_Index].DesireLevel);
      gDragAndDropAskFormatEachTime := GetValue(Node,'DragAndDropAskFormatEachTime', gDragAndDropAskFormatEachTime);
      gDragAndDropTextAutoFilename := GetValue(Node, 'DragAndDropTextAutoFilename', gDragAndDropTextAutoFilename);
      gDragAndDropSaveUnicodeTextInUFT8 := GetValue(Node, 'DragAndDropSaveUnicodeTextInUFT8', gDragAndDropSaveUnicodeTextInUFT8);
      gNtfsHourTimeDelay := GetValue(Node, 'NtfsHourTimeDelay', gNtfsHourTimeDelay);
      gAutoExtractOpenMask := GetValue(Node, 'AutoExtractOpenMask', gAutoExtractOpenMask);
      gSearchDefaultTemplate := GetValue(Node, 'SearchDefaultTemplate', gSearchDefaultTemplate);
      gFileOperationsProgressKind := TFileOperationsProgressKind(GetValue(Node, 'ProgressKind', Integer(gFileOperationsProgressKind)));
      gFileOperationsConfirmations := TFileOperationsConfirmations(GetValue(Node, 'Confirmations', Integer(gFileOperationsConfirmations)));
      // Operations options
      SubNode := Node.FindNode('Options');
      if Assigned(SubNode) then
      begin
        gOperationOptionSymLinks := TFileSourceOperationOptionSymLink(GetValue(SubNode, 'Symlink', Integer(gOperationOptionSymLinks)));
        gOperationOptionCorrectLinks := GetValue(SubNode, 'CorrectLinks', gOperationOptionCorrectLinks);
        gOperationOptionCopyOnWrite := TFileSourceOperationOptionGeneral(GetValue(SubNode, 'CopyOnWrite', Integer(gOperationOptionCopyOnWrite)));
        gOperationOptionFileExists := TFileSourceOperationOptionFileExists(GetValue(SubNode, 'FileExists', Integer(gOperationOptionFileExists)));
        gOperationOptionDirectoryExists := TFileSourceOperationOptionDirectoryExists(GetValue(SubNode, 'DirectoryExists', Integer(gOperationOptionDirectoryExists)));
        gOperationOptionSetPropertyError := TFileSourceOperationOptionSetPropertyError(GetValue(SubNode, 'SetPropertyError', Integer(gOperationOptionSetPropertyError)));
        gOperationOptionReserveSpace := GetValue(SubNode, 'ReserveSpace', gOperationOptionReserveSpace);
        gOperationOptionCheckFreeSpace := GetValue(SubNode, 'CheckFreeSpace', gOperationOptionCheckFreeSpace);
        gOperationOptionCopyAttributes := GetValue(SubNode, 'CopyAttributes', gOperationOptionCopyAttributes);
        gOperationOptionCopyXattributes := GetValue(SubNode, 'CopyXattributes', gOperationOptionCopyXattributes);
        gOperationOptionVerify := GetValue(SubNode, 'Verify', gOperationOptionVerify);
        gOperationOptionCopyTime := GetValue(SubNode, 'CopyTime', gOperationOptionCopyTime);
        gOperationOptionCopyOwnership := GetValue(SubNode, 'CopyOwnership', gOperationOptionCopyOwnership);
        gOperationOptionCopyPermissions := GetValue(SubNode, 'CopyPermissions', gOperationOptionCopyPermissions);
        gOperationOptionExcludeEmptyDirectories := GetValue(SubNode, 'ExcludeEmptyTemplateDirectories', gOperationOptionExcludeEmptyDirectories);
      end;
      // Extract
      SubNode := Node.FindNode('Extract');
      if Assigned(SubNode) then
      begin
        gExtractOverwrite := GetValue(SubNode, 'Overwrite', gExtractOverwrite);
      end;

      // Multi-Rename
      SubNode := Node.FindNode('MultiRename');
      if Assigned(SubNode) then
      begin
        gMulRenShowMenuBarOnTop := GetValue(SubNode, 'MulRenShowMenuBarOnTop', gMulRenShowMenuBarOnTop);
        gMulRenInvalidCharReplacement := GetValue(SubNode, 'MulRenInvalidCharReplacement', gMulRenInvalidCharReplacement);
        gMulRenLaunchBehavior := TMulRenLaunchBehavior(GetValue(SubNode, 'MulRenLaunchBehavor', Integer(gMulRenLaunchBehavior)));
        gMulRenExitModifiedPreset := TMulRenExitModifiedPreset(GetValue(SubNode, 'MulRenExitModifiedPreset', Integer(gMulRenExitModifiedPreset)));
        gMulRenSaveRenamingLog := TMulRenSaveRenamingLog(GetValue(SubNode, 'MulRenSaveRenamingLog', Integer(gMulRenSaveRenamingLog)));
        gMulRenLogFilename := GetValue(SubNode, 'MulRenLogFilename', gMulRenLogFilename);
        gMultRenDailyIndividualDirLog := GetValue(SubNode, 'MultRenDailyIndividualDirLog', gMultRenDailyIndividualDirLog);
        gMulRenFilenameWithFullPathInLog := GetValue(SubNode, 'MulRenFilenameWithFullPathInLog', gMulRenFilenameWithFullPathInLog);
        gMulRenPathRangeSeparator := GetValue(SubNode, 'MulRenPathRangeSeparator', gMulRenPathRangeSeparator);
      end;
    end;

    { Tabs page }
    Node := Root.FindNode('Tabs');
    if Assigned(Node) then
    begin
      // Loading tabs relating option respecting legacy order of options setting and wanted default values.
      // The default action on double click is to close tab simply to respect legacy of what it was doing hardcoded before.
      gDirTabOptions := TTabsOptions(GetValue(Node, 'Options', Integer(gDirTabOptions)));
      if LoadedConfigVersion<9 then
      begin
        gDirTabOptions := gDirTabOptions + [tb_close_on_doubleclick , tb_reusing_tab_when_possible, tb_confirm_close_locked_tab]; //The "tb_close_on_doubleclick" is useless but anyway... :-)
        gDirTabActionOnDoubleClick:=tadc_CloseTab;
      end;
      gDirTabLimit := GetValue(Node, 'CharacterLimit', gDirTabLimit);
      gDirTabPosition := TTabsPosition(GetValue(Node, 'Position', Integer(gDirTabPosition)));
      gDirTabActionOnDoubleClick := TTabsOptionsDoubleClick(GetValue(Node, 'ActionOnDoubleClick', Integer(tadc_CloseTab)));
    end;

    { Log page }
    Node := Root.FindNode('Log');
    if Assigned(Node) then
    begin
      gLogFile := GetAttr(Node, 'Enabled', gLogFile);
      gLogFileCount := GetAttr(Node, 'Count', gLogFileCount);
      gLogFileWithDateInName := GetAttr(Node, 'LogFileWithDateInName', gLogFileWithDateInName);
      gLogFileName := GetValue(Node, 'FileName', gLogFileName);
      gLogOptions := TLogOptions(GetValue(Node, 'Options', Integer(gLogOptions)));
    end;

    { Configuration page }
    gSaveConfiguration := GetAttr(Root, 'Configuration/Save', gSaveConfiguration);
    gSaveWindowState := GetAttr(Root, 'MainWindow/Position/Save', gSaveWindowState);
    gSaveFolderTabs := GetAttr(Root, 'Configuration/FolderTabs/Save', gSaveFolderTabs);
    gSaveSearchReplaceHistory:= GetAttr(Root, 'History/SearchReplaceHistory/Save', gSaveSearchReplaceHistory);
    gSaveDirHistory := GetAttr(Root, 'History/DirHistory/Save', gSaveDirHistory);
    gSaveCmdLineHistory := GetAttr(Root, 'History/CmdLineHistory/Save', gSaveCmdLineHistory);
    gSaveFileMaskHistory := GetAttr(Root, 'History/FileMaskHistory/Save', gSaveFileMaskHistory);
    gSortOrderOfConfigurationOptionsTree := TSortConfigurationOptions(GetAttr(Root, 'Configuration/SortOrder', Integer(scoAlphabeticalButLanguage)));
    gCollapseConfigurationOptionsTree := TConfigurationTreeState(GetAttr(Root, 'Configuration/TreeType', Integer(ctsFullExpand)));

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
      gQuickFilterSaveSessionModifications := GetValue(Node, 'SaveSessionModifications', gQuickFilterSaveSessionModifications);
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
      gInplaceRename := GetValue(Node, 'InplaceRename', gInplaceRename);
      gInplaceRenameButton := GetValue(Node, 'InplaceRenameButton', gInplaceRenameButton);
      gDblClickToParent := GetValue(Node, 'DblClickToParent', gDblClickToParent);
      gHotDirAddTargetOrNot:=GetValue(Node, 'HotDirAddTargetOrNot', gHotDirAddTargetOrNot);
      gHotDirFullExpandOrNot:=GetValue(Node, 'HotDirFullExpandOrNot', gHotDirFullExpandOrNot);
      gShowPathInPopup:=GetValue(Node, 'ShowPathInPopup', gShowPathInPopup);
      gShowOnlyValidEnv:=GetValue(Node, 'ShowOnlyValidEnv', gShowOnlyValidEnv);
      gWhereToAddNewHotDir:=TPositionWhereToAddHotDir(GetValue(Node, 'WhereToAddNewHotDir', Integer(gWhereToAddNewHotDir)));
      gHotDirFilenameStyle := TConfigFilenameStyle(GetValue(Node, 'FilenameStyle', ord(gHotDirFilenameStyle)));
      gHotDirPathToBeRelativeTo := gConfig.GetValue(Node, 'PathToBeRelativeTo', gHotDirPathToBeRelativeTo);
      gHotDirPathModifierElements := tHotDirPathModifierElements(GetValue(Node, 'PathModifierElements', Integer(gHotDirPathModifierElements)));
    end;

    { Thumbnails }
    Node := Root.FindNode('Thumbnails');
    if Assigned(Node) then
    begin
      gThumbSave := GetAttr(Node, 'Save', gThumbSave);
      gThumbSize.cx := GetValue(Node, 'Width', gThumbSize.cx);
      gThumbSize.cy := GetValue(Node, 'Height', gThumbSize.cy);
    end;

    { Description }
    Node := Root.FindNode('Description');
    if Assigned(Node) then
    begin
      gDescCreateUnicode := GetValue(Node, 'CreateNewUnicode', gDescCreateUnicode);
      gDescReadEncoding  := TMacroEncoding(GetValue(Node, 'DefaultEncoding', Integer(gDescReadEncoding)));
      gDescWriteEncoding := TMacroEncoding(GetValue(Node, 'CreateNewEncoding', Integer(gDescWriteEncoding)));
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
      gIconTheme := GetValue(Node, 'Theme', gIconTheme);
      gShowHiddenDimmed := GetValue(Node, 'ShowHiddenDimmed', gShowHiddenDimmed);
      gShowIcons := TShowIconsMode(GetValue(Node, 'ShowMode', Integer(gShowIcons)));
      gIconOverlays := GetValue(Node, 'ShowOverlays', gIconOverlays);
      gIconsSize := GetValue(Node, 'Size', gIconsSize);
      gDiskIconsSize := GetValue(Node, 'DiskSize', gDiskIconsSize);
      gDiskIconsAlpha := GetValue(Node, 'DiskAlpha', gDiskIconsAlpha);
      gToolIconsSize := GetValue(Node, 'ToolSize', gToolIconsSize);
      gIconsExclude := GetValue(Node, 'Exclude', gIconsExclude);
      gIconsExcludeDirs := GetValue(Node, 'ExcludeDirs', gIconsExcludeDirs);
      gPixelsPerInch := GetValue(Node, 'PixelsPerInch', gPixelsPerInch);
      if LoadedConfigVersion < 10 then
      begin
        if GetValue(Node, 'CustomDriveIcons', False) then
          gCustomIcons += [cimDrive];
        DeleteNode(Node, 'CustomDriveIcons');
      end;
      gCustomIcons := TCustomIconsMode(GetValue(Node, 'CustomIcons', Integer(gCustomIcons)));
      gIconsInMenus := GetAttr(Node, 'ShowInMenus/Enabled', gIconsInMenus);
      gIconsInMenusSize := GetValue(Node, 'ShowInMenus/Size', gIconsInMenusSize);
      Application.ShowButtonGlyphs := TApplicationShowGlyphs(GetValue(Node, 'ShowButtonGlyphs', Integer(Application.ShowButtonGlyphs)));
    end;

    { Ignore list page }
    Node := Root.FindNode('IgnoreList');
    if Assigned(Node) then
    begin
      gIgnoreListFileEnabled:= GetAttr(Node, 'Enabled', gIgnoreListFileEnabled);
      gIgnoreListFile:= GetValue(Node, 'IgnoreListFile', gIgnoreListFile);
    end;

    { Directories HotList }
    gDirectoryHotlist.LoadFromXML(gConfig, Root);

    { Viewer }
    Node := Root.FindNode('Viewer');
    if Assigned(Node) then
    begin
      gImageStretch := GetValue(Node, 'ImageStretch', gImageStretch);
      gImageExifRotate := GetValue(Node, 'ImageExifRotate', gImageExifRotate);
      gImageStretchOnlyLarge := GetValue(Node, 'ImageStretchLargeOnly', gImageStretchOnlyLarge);
      gImageCenter := GetValue(Node, 'ImageCenter', gImageCenter);
      gPreviewVisible := GetValue(Node, 'PreviewVisible', gPreviewVisible);
      gCopyMovePath1 := GetValue(Node, 'CopyMovePath1', gCopyMovePath1);
      gCopyMovePath2 := GetValue(Node, 'CopyMovePath2', gCopyMovePath2);
      gCopyMovePath3 := GetValue(Node, 'CopyMovePath3', gCopyMovePath3);
      gCopyMovePath4 := GetValue(Node, 'CopyMovePath4', gCopyMovePath4);
      gCopyMovePath5 := GetValue(Node, 'CopyMovePath5', gCopyMovePath5);
      gImagePaintMode := TViewerPaintTool(GetValue(Node, 'PaintMode', Integer(gImagePaintMode)));
      gImagePaintWidth := GetValue(Node, 'PaintWidth', gImagePaintWidth);
      gColCount    := GetValue(Node, 'NumberOfColumns', gColCount);
      gTabSpaces := GetValue(Node, 'TabSpaces', gTabSpaces);
      gMaxTextWidth := GetValue(Node, 'MaxTextWidth', gMaxTextWidth);
      gViewerMode  := GetValue(Node, 'ViewerMode'  , gViewerMode);
      gPrintMargins := GetValue(Node, 'PrintMargins'  , gPrintMargins);
      gShowCaret := GetValue(Node, 'ShowCaret'  , gShowCaret);

      gImagePaintColor := GetValue(Node, 'PaintColor', gImagePaintColor);
      gBookBackgroundColor := GetValue(Node, 'BackgroundColor', gBookBackgroundColor);
      gBookFontColor := GetValue(Node, 'FontColor', gBookFontColor);
      gTextPosition := GetValue(Node, 'TextPosition',  gTextPosition);
      if LoadedConfigVersion < 7 then
      begin
        gThumbSave := GetValue(Node, 'SaveThumbnails', gThumbSave);
      end;
    end;

    { Editor }
    Node := Root.FindNode('Editor');
    if Assigned(Node) then
    begin
      gEditWaitTime := GetValue(Node, 'EditWaitTime', gEditWaitTime);
      gEditorSynEditOptions := TSynEditorOptions(GetValue(Node, 'SynEditOptions', Integer(gEditorSynEditOptions)));
      gEditorSynEditTabWidth := GetValue(Node, 'SynEditTabWidth', gEditorSynEditTabWidth);
      gEditorSynEditRightEdge := GetValue(Node, 'SynEditRightEdge', gEditorSynEditRightEdge);
    end;

    { Differ }
    Node := Root.FindNode('Differ');
    if Assigned(Node) then
    begin
      gDifferIgnoreCase := GetValue(Node, 'IgnoreCase', gDifferIgnoreCase);
      gDifferKeepScrolling := GetValue(Node, 'KeepScrolling', gDifferKeepScrolling);
      gDifferPaintBackground := GetValue(Node, 'PaintBackground', gDifferPaintBackground);
      gDifferLineDifferences := GetValue(Node, 'LineDifferences', gDifferLineDifferences);
      gDifferIgnoreWhiteSpace := GetValue(Node, 'IgnoreWhiteSpace', gDifferIgnoreWhiteSpace);
      SubNode := FindNode(Node, 'Colors');
      if Assigned(SubNode) then
      begin
        gDifferAddedColor := GetValue(SubNode, 'Added', gDifferAddedColor);
        gDifferDeletedColor := GetValue(SubNode, 'Deleted', gDifferDeletedColor);
        gDifferModifiedColor := GetValue(SubNode, 'Modified', gDifferModifiedColor);
      end;
    end;

    { SyncDirs }
    Node := Root.FindNode('SyncDirs');
    if Assigned(Node) then
    begin
      gSyncDirsSubdirs := GetValue(Node, 'Subdirs', gSyncDirsSubdirs);
      gSyncDirsByContent := GetValue(Node, 'ByContent', gSyncDirsByContent);
      gSyncDirsAsymmetric := GetValue(Node, 'Asymmetric', gSyncDirsAsymmetric);
      gSyncDirsAsymmetricSave := GetAttr(Node, 'Asymmetric/Save', gSyncDirsAsymmetricSave);
      gSyncDirsIgnoreDate := GetValue(Node, 'IgnoreDate', gSyncDirsIgnoreDate);
      gSyncDirsShowFilterCopyRight := GetValue(Node, 'FilterCopyRight', gSyncDirsShowFilterCopyRight);
      gSyncDirsShowFilterEqual := GetValue(Node, 'FilterEqual', gSyncDirsShowFilterEqual);
      gSyncDirsShowFilterNotEqual := GetValue(Node, 'FilterNotEqual', gSyncDirsShowFilterNotEqual);
      gSyncDirsShowFilterCopyLeft := GetValue(Node, 'FilterCopyLeft', gSyncDirsShowFilterCopyLeft);
      gSyncDirsShowFilterDuplicates := GetValue(Node, 'FilterDuplicates', gSyncDirsShowFilterDuplicates);
      gSyncDirsShowFilterSingles := GetValue(Node, 'FilterSingles', gSyncDirsShowFilterSingles);
      gSyncDirsFileMask := GetValue(Node, 'FileMask', gSyncDirsFileMask);
      SubNode := FindNode(Node, 'Colors');
      if Assigned(SubNode) then
      begin
        gSyncLeftColor := GetValue(SubNode, 'Left', gSyncLeftColor);
        gSyncRightColor := GetValue(SubNode, 'Right', gSyncRightColor);
        gSyncUnknownColor := GetValue(SubNode, 'Unknown', gSyncUnknownColor);
      end;
    end;

    { Internal Associations}
    Node := Root.FindNode('InternalAssociations');
    if Assigned(Node) then
    begin
      gOfferToAddToFileAssociations := GetValue(Node, 'OfferToAddNewFileType', gOfferToAddToFileAssociations);
      gFileAssociationLastCustomAction := GetValue(Node, 'LastCustomAction', gFileAssociationLastCustomAction);
      gExtendedContextMenu := GetValue(Node, 'ExpandedContextMenu', gExtendedContextMenu);
      gOpenExecuteViaShell := GetValue(Node,'ExecuteViaShell', gOpenExecuteViaShell);
      gExecuteViaTerminalClose := GetValue(Node,'OpenSystemWithTerminalClose', gExecuteViaTerminalClose);
      gExecuteViaTerminalStayOpen := GetValue(Node,'OpenSystemWithTerminalStayOpen', gExecuteViaTerminalStayOpen);
      gIncludeFileAssociation := GetValue(Node,'IncludeFileAssociation',gIncludeFileAssociation);
      gFileAssocFilenameStyle := TConfigFilenameStyle(GetValue(Node, 'FilenameStyle', ord(gFileAssocFilenameStyle)));
      gFileAssocPathToBeRelativeTo := GetValue(Node, 'PathToBeRelativeTo', gFileAssocPathToBeRelativeTo);
      gFileAssocPathModifierElements := tFileAssocPathModifierElements(GetValue(Node, 'PathModifierElements', Integer(gFileAssocPathModifierElements)));
    end;

    { Tree View Menu }
    Node := Root.FindNode('TreeViewMenu');
    if Assigned(Node) then
    begin
      gUseTreeViewMenuWithDirectoryHotlistFromMenuCommand := GetValue(Node, 'UseTVMDirectoryHotlistFMC', gUseTreeViewMenuWithDirectoryHotlistFromMenuCommand);
      gUseTreeViewMenuWithDirectoryHotlistFromDoubleClick := GetValue(Node, 'UseTVMDirectoryHotlistFDC', gUseTreeViewMenuWithDirectoryHotlistFromDoubleClick);
      gUseTreeViewMenuWithFavoriteTabsFromMenuCommand := GetValue(Node, 'UseTVMFavoriteTabsFMC', gUseTreeViewMenuWithFavoriteTabsFromMenuCommand);
      gUseTreeViewMenuWithFavoriteTabsFromDoubleClick := GetValue(Node, 'UseTVMFavoriteTabsFDC', gUseTreeViewMenuWithFavoriteTabsFromDoubleClick);
      gUseTreeViewMenuWithDirHistory := GetValue(Node, 'UseTVMDirHistory', gUseTreeViewMenuWithDirHistory);
      gUseTreeViewMenuWithViewHistory := GetValue(Node, 'UseTVMViewHistory', gUseTreeViewMenuWithViewHistory);
      gUseTreeViewMenuWithCommandLineHistory := GetValue(Node, 'UseTVMCommandLineHistory', gUseTreeViewMenuWithCommandLineHistory);
      gTreeViewMenuShortcutExit := GetValue(Node, 'TreeViewMenuShortcutExit', gTreeViewMenuShortcutExit);
      gTreeViewMenuSingleClickExit := GetValue(Node, 'TreeViewMenuSingleClickExit', gTreeViewMenuSingleClickExit);
      gTreeViewMenuDoubleClickExit := GetValue(Node, 'TreeViewMenuDoubleClickExit', gTreeViewMenuDoubleClickExit);
      for iIndexContextMode:=0 to (ord(tvmcLASTONE)-2) do
      begin
        SubNode := Node.FindNode(Format('Context%.2d',[iIndexContextMode]));
        gTreeViewMenuOptions[iIndexContextMode].CaseSensitive := GetValue(SubNode, 'CaseSensitive', gTreeViewMenuOptions[iIndexContextMode].CaseSensitive);
        gTreeViewMenuOptions[iIndexContextMode].IgnoreAccents := GetValue(SubNode, 'IgnoreAccents', gTreeViewMenuOptions[iIndexContextMode].IgnoreAccents);
        gTreeViewMenuOptions[iIndexContextMode].ShowWholeBranchIfMatch := GetValue(SubNode, 'ShowWholeBranchIfMatch', gTreeViewMenuOptions[iIndexContextMode].ShowWholeBranchIfMatch);
      end;
      gTreeViewMenuUseKeyboardShortcut := GetValue(Node, 'TreeViewMenuUseKeyboardShortcut', gTreeViewMenuUseKeyboardShortcut);
      gTVMBackgroundColor := GetValue(Node, 'BackgroundColor', gTVMBackgroundColor);
      gTVMShortcutColor := GetValue(Node, 'ShortcutColor', gTVMShortcutColor);
      gTVMNormalTextColor := GetValue(Node, 'NormalTextColor', gTVMNormalTextColor);
      gTVMSecondaryTextColor := GetValue(Node, 'SecondaryTextColor', gTVMSecondaryTextColor);
      gTVMFoundTextColor := GetValue(Node, 'FoundTextColor', gTVMFoundTextColor);
      gTVMUnselectableTextColor := GetValue(Node, 'UnselectableTextColor', gTVMUnselectableTextColor);
      gTVMCursorColor := GetValue(Node, 'CursorColor', gTVMCursorColor);
      gTVMShortcutUnderCursor := GetValue(Node, 'ShortcutUnderCursor', gTVMShortcutUnderCursor);
      gTVMNormalTextUnderCursor := GetValue(Node, 'NormalTextUnderCursor', gTVMNormalTextUnderCursor);
      gTVMSecondaryTextUnderCursor := GetValue(Node, 'SecondaryTextUnderCursor', gTVMSecondaryTextUnderCursor);
      gTVMFoundTextUnderCursor := GetValue(Node, 'FoundTextUnderCursor', gTVMFoundTextUnderCursor);
      gTVMUnselectableUnderCursor := GetValue(Node, 'UnselectableUnderCursor', gTVMUnselectableUnderCursor);
    end;

    { Favorite Tabs }
    Node := Root.FindNode('FavoriteTabsOptions');
    if Assigned(Node) then
    begin
      gFavoriteTabsUseRestoreExtraOptions := GetValue(Node, 'FavoriteTabsUseRestoreExtraOptions', gFavoriteTabsUseRestoreExtraOptions);
      gWhereToAddNewFavoriteTabs := TPositionWhereToAddFavoriteTabs(GetValue(Node, 'WhereToAdd', Integer(gWhereToAddNewFavoriteTabs)));
      gFavoriteTabsFullExpandOrNot := GetValue(Node, 'Expand', gFavoriteTabsFullExpandOrNot);
      gFavoriteTabsGoToConfigAfterSave := GetValue(Node, 'GotoConfigAftSav', gFavoriteTabsGoToConfigAfterSave);
      gFavoriteTabsGoToConfigAfterReSave := GetValue(Node, 'GotoConfigAftReSav', gFavoriteTabsGoToConfigAfterReSave);
      gDefaultTargetPanelLeftSaved := TTabsConfigLocation(GetValue(Node, 'DfltLeftGoTo', Integer(gDefaultTargetPanelLeftSaved)));
      gDefaultTargetPanelRightSaved := TTabsConfigLocation(GetValue(Node, 'DfltRightGoTo', Integer(gDefaultTargetPanelRightSaved)));
      gDefaultExistingTabsToKeep := TTabsConfigLocation(GetValue(Node, 'DfltKeep', Integer(gDefaultExistingTabsToKeep)));
      gFavoriteTabsSaveDirHistory := GetValue(Node, 'DfltSaveDirHistory', gFavoriteTabsSaveDirHistory);
      gFavoriteTabsList.LastFavoriteTabsLoadedUniqueId := StringToGUID(GetValue(Node,'FavTabsLastUniqueID',GUIDtoString(DCGetNewGUID)));
    end;

    { - Other - }
    gLuaLib := GetValue(Root, 'Lua/PathToLibrary', gLuaLib);
    gNameSCFile:= GetValue(Root, 'NameShortcutFile', gNameSCFile);
    gHotKeySortOrder := THotKeySortOrder(GetValue(Root, 'HotKeySortOrder', Integer(hksoByCommand)));
    gUseEnterToCloseHotKeyEditor := GetValue(Root,'UseEnterToCloseHotKeyEditor',gUseEnterToCloseHotKeyEditor);
    gLastUsedPacker:= GetValue(Root, 'LastUsedPacker', gLastUsedPacker);
    gLastDoAnyCommand:=GetValue(Root, 'LastDoAnyCommand', gLastDoAnyCommand);
    gbMarkMaskCaseSensitive := GetValue(Root, 'MarkMaskCaseSensitive', gbMarkMaskCaseSensitive);
    gbMarkMaskIgnoreAccents := GetValue(Root, 'MarkMaskIgnoreAccents', gbMarkMaskIgnoreAccents);
    gMarkMaskFilterWindows := GetValue(Root, 'MarkMaskFilterWindows', gMarkMaskFilterWindows);
    gMarkShowWantedAttribute := GetValue(Root, 'MarkShowWantedAttribute', gMarkShowWantedAttribute);
    gMarkDefaultWantedAttribute := GetValue(Root, 'MarkDefaultWantedAttribute', gMarkDefaultWantedAttribute);
    gMarkLastWantedAttribute := GetValue(Root, 'MarkLastWantedAttribute', gMarkLastWantedAttribute);

    { TotalCommander Import/Export }
    {$IFDEF MSWINDOWS}
    Node := Root.FindNode('TCSection');
    if Assigned(Node) then
    begin
      gTotalCommanderExecutableFilename := GetValue(Node, 'TCExecutableFilename', gTotalCommanderExecutableFilename);
      gTotalCommanderConfigFilename := GetValue(Node, 'TCConfigFilename', gTotalCommanderConfigFilename);
      gTotalCommanderToolbarPath:=GetValue(Node,'TCToolbarPath',gTotalCommanderToolbarPath);
    end;
    {$ENDIF}
  end;

  { Search template list }
  gSearchTemplateList.LoadFromXml(gConfig, Root);

  { File type colors, load after search templates }
  Node := Root.FindNode('Colors');
  if Assigned(Node) then
  begin
    gColorExt.Load(gConfig, Node);
  end;

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
    for iIndexContextMode:=ord(ptDSX) to ord(ptWLX) do
    begin
      gTweakPluginWidth[iIndexContextMode]:=gConfig.GetValue(Node, Format('TweakPluginWidth%d',[iIndexContextMode]), 0);
      gTweakPluginHeight[iIndexContextMode]:=gConfig.GetValue(Node, Format('TweakPluginHeight%d',[iIndexContextMode]), 0);
    end;
    gPluginFilenameStyle := TConfigFilenameStyle(gConfig.GetValue(Node, 'PluginFilenameStyle', ord(gPluginFilenameStyle)));
    gPluginPathToBeRelativeTo := gConfig.GetValue(Node, 'PluginPathToBeRelativeTo', gPluginPathToBeRelativeTo);
    gPluginInAutoTweak := gConfig.GetValue(Node, 'AutoTweak', gPluginInAutoTweak);
    gWCXConfigViewMode :=  TWcxCfgViewMode(gConfig.GetValue(Node, 'WCXConfigViewMode', Integer(gWCXConfigViewMode)));
  end;
  gWDXPlugins.Add(TExifWdx.Create);

  { Load content plugins used in search templates }
  LoadContentPlugins;
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
      gConfig.SetFont(Node, '', FontOptions.Name, FontOptions.Size, Integer(FontOptions.Style), Integer(FontOptions.Quality));
  end;
var
  Root, Node, SubNode: TXmlNode;
  KeyTypingModifier: TKeyTypingModifier;
  iIndexContextMode: integer;
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
    SetValue(Node, 'GoToRoot', gGoToRoot);
    SetValue(Node, 'ActiveRight', gActiveRight);
    SetValue(Node, 'RunInTerminalStayOpenCmd', gRunInTermStayOpenCmd);
    SetValue(Node, 'RunInTerminalStayOpenParams', gRunInTermStayOpenParams);
    SetValue(Node, 'RunInTerminalCloseCmd', gRunInTermCloseCmd);
    SetValue(Node, 'RunInTerminalCloseParams', gRunInTermCloseParams);
    SetValue(Node, 'JustRunTerminal', gRunTermCmd);
    SetValue(Node, 'JustRunTermParams', gRunTermParams);

    SetValue(Node, 'OnlyOneAppInstance', gOnlyOneAppInstance);
    SetValue(Node, 'LynxLike', gLynxLike);
    SetValue(Node, 'FileSizeFormat', Ord(gFileSizeFormat));
    SetValue(Node, 'OperationSizeFormat', Ord(gOperationSizeFormat));
    SetValue(Node, 'HeaderSizeFormat', Ord(gHeaderSizeFormat));
    SetValue(Node, 'FooterSizeFormat', Ord(gFooterSizeFormat));
    SetValue(Node, 'FileSizeDigits', gFileSizeDigits);
    SetValue(Node, 'HeaderDigits', gHeaderDigits);
    SetValue(Node, 'FooterDigits', gFooterDigits);
    SetValue(Node, 'OperationSizeDigits', gOperationSizeDigits);
    SetValue(Node, 'PersonalizedByte', Trim(gSizeDisplayUnits[fsfPersonalizedByte]));
    SetValue(Node, 'PersonalizedKilo', Trim(gSizeDisplayUnits[fsfPersonalizedKilo]));
    SetValue(Node, 'PersonalizedMega', Trim(gSizeDisplayUnits[fsfPersonalizedMega]));
    SetValue(Node, 'PersonalizedGiga', Trim(gSizeDisplayUnits[fsfPersonalizedGiga]));
    SetValue(Node, 'PersonalizedTera', Trim(gSizeDisplayUnits[fsfPersonalizedTera]));
    SetValue(Node, 'MinimizeToTray', gMinimizeToTray);
    SetValue(Node, 'AlwaysShowTrayIcon', gAlwaysShowTrayIcon);
    SubNode := FindNode(Node, 'Mouse', True);
    SetAttr(SubNode, 'Selection/Enabled', gMouseSelectionEnabled);
    SetValue(SubNode, 'Selection/Button', gMouseSelectionButton);
    SetValue(SubNode, 'SingleClickStart', gMouseSingleClickStart);
    SetValue(SubNode, 'Selection/IconClick', gMouseSelectionIconClick);
    SetValue(SubNode, 'ScrollMode', Integer(gScrollMode));
    SetValue(SubNode, 'WheelScrollLines', gWheelScrollLines);
    SetValue(Node, 'AutoFillColumns', gAutoFillColumns);
    SetValue(Node, 'AutoSizeColumn', gAutoSizeColumn);
    SetValue(Node, 'CustomColumnsChangeAllColumns', gCustomColumnsChangeAllColumns);
    SetValue(Node, 'BriefViewFileExtAligned', gBriefViewFileExtAligned);
    SetValue(Node, 'DateTimeFormat', gDateTimeFormat);
    SetValue(Node, 'CutTextToColumnWidth', gCutTextToColWidth);
    SetValue(Node, 'ExtendCellWidth', gExtendCellWidth);
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

    { Differ related}
    Node := Root.FindNode('Tools');
    SubNode := FindNode(Node, 'Differ', TRUE);
    SetValue(SubNode, 'FramePosAfterComp', Integer(gResultingFramePositionAfterCompare));

    { Fonts page }
    SetDCFont(gConfig.FindNode(Root, 'Fonts/Main', True), gFonts[dcfMain]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/Editor', True), gFonts[dcfEditor]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/Viewer', True), gFonts[dcfViewer]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/ViewerBook', True), gFonts[dcfViewerBook]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/Log', True), gFonts[dcfLog]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/Console', True), gFonts[dcfConsole]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/PathEdit',True), gFonts[dcfPathEdit]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/FunctionButtons',True), gFonts[dcfFunctionButtons]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/SearchResults',True), gFonts[dcfSearchResults]);
    SetDCFont(gConfig.FindNode(Root, 'Fonts/TreeViewMenu', True), gFonts[dcfTreeViewMenu]);

    { Colors page }
    Node := FindNode(Root, 'Colors', True);
    SetValue(Node, 'UseCursorBorder', gUseCursorBorder);
    SetValue(Node, 'CursorBorderColor', gCursorBorderColor);
    SetValue(Node, 'UseFrameCursor', gUseFrameCursor);
    SetValue(Node, 'Foreground', gForeColor);
    SetValue(Node, 'Background', gBackColor);
    SetValue(Node, 'Background2', gBackColor2);
    SetValue(Node, 'Cursor', gCursorColor);
    SetValue(Node, 'CursorText', gCursorText);
    SetValue(Node, 'Mark', gMarkColor);
    SetValue(Node, 'InactiveCursor', gInactiveCursorColor);
    SetValue(Node, 'InactiveMark', gInactiveMarkColor);
    SetValue(Node, 'UseInvertedSelection', gUseInvertedSelection);
    SetValue(Node, 'UseInactiveSelColor', gUseInactiveSelColor);
    SetValue(Node, 'AllowOverColor', gAllowOverColor);
    SetValue(Node, 'gBorderFrameWidth', gBorderFrameWidth);

    SetValue(Node, 'PathLabel/ActiveColor', gPathActiveColor);
    SetValue(Node, 'PathLabel/ActiveFontColor', gPathActiveFontColor);
    SetValue(Node, 'PathLabel/InactiveColor', gPathInactiveColor);
    SetValue(Node, 'PathLabel/InactiveFontColor', gPathInactiveFontColor);

    SetValue(Node, 'InactivePanelBrightness', gInactivePanelBrightness);
    SetValue(Node, 'FreeSpaceIndicator/UseGradient', gIndUseGradient);
    SetValue(Node, 'FreeSpaceIndicator/ForeColor', gIndForeColor);
    SetValue(Node, 'FreeSpaceIndicator/BackColor', gIndBackColor);

    SetValue(Node, 'LogWindow/Info', gLogInfoColor);
    SetValue(Node, 'LogWindow/Error', gLogErrorColor);
    SetValue(Node, 'LogWindow/Success', gLogSuccessColor);

    gColorExt.Save(gConfig, Node);

    { ToolTips page }
    Node := FindNode(Root, 'ToolTips', True);
    SetValue(Node, 'ShowToolTipMode', gShowToolTip);
    SetValue(Node, 'ActualToolTipMode', Integer(gShowToolTipMode));
    SetValue(Node, 'ToolTipHideTimeOut', Integer(gToolTipHideTimeOut));
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
    SetValue(SubNode, 'ShowCaptions', gToolBarShowCaptions);
    SetValue(SubNode, 'ReportErrorWithCommands', gToolbarReportErrorWithCommands);
    SetValue(SubNode, 'FilenameStyle', ord(gToolbarFilenameStyle));
    SetValue(SubNode, 'PathToBeRelativeTo', gToolbarPathToBeRelativeTo);
    SetValue(SubNode, 'PathModifierElements', Integer(gToolbarPathModifierElements));

    SubNode := FindNode(Node, 'MiddleBar', True);
    SetAttr(SubNode, 'Enabled', gMiddleToolBar);
    SetValue(SubNode, 'FlatIcons', gMiddleToolBarFlat);
    SetValue(SubNode, 'ButtonHeight', gMiddleToolBarButtonSize);
    SetValue(SubNode, 'IconSize', gMiddleToolBarIconSize);
    SetValue(SubNode, 'ShowCaptions', gMiddleToolBarShowCaptions);
    SetValue(SubNode,'ReportErrorWithCommands', gMiddleToolbarReportErrorWithCommands);

    SetValue(Node, 'DriveBar1', gDriveBar1);
    SetValue(Node, 'DriveBar2', gDriveBar2);
    SetValue(Node, 'DriveBarFlat', gDriveBarFlat);
    SubNode := FindNode(Node, 'DrivesListButton', True);
    SetAttr(SubNode, 'Enabled', gDrivesListButton);
    SetValue(SubNode, 'ShowLabel', dlbShowLabel in gDrivesListButtonOptions);
    SetValue(SubNode, 'ShowFileSystem', dlbShowFileSystem in gDrivesListButtonOptions);
    SetValue(SubNode, 'ShowFreeSpace', dlbShowFreeSpace in gDrivesListButtonOptions);
    SetValue(Node, 'SeparateTree', gSeparateTree);
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
    SetValue(Node, 'UppercaseDriveLetter', gUpperCaseDriveLetter);
    SetValue(Node, 'ShowColonAfterDrive', gShowColonAfterDrive);

    { Files views }
    Node := FindNode(Root, 'FilesViews', True);
    SubNode := FindNode(Node, 'Sorting', True);
    SetValue(SubNode, 'CaseSensitivity', Integer(gSortCaseSensitivity));
    SetValue(SubNode, 'NaturalSorting', gSortNatural);
    SetValue(SubNode, 'SpecialSorting', gSortSpecial);
    SetValue(SubNode, 'SortFolderMode', Integer(gSortFolderMode));
    SetValue(SubNode, 'NewFilesPosition', Integer(gNewFilesPosition));
    SetValue(SubNode, 'UpdatedFilesPosition', Integer(gUpdatedFilesPosition));
    SubNode := FindNode(Node, 'ColumnsView', True);
    SetValue(SubNode, 'AutoSaveWidth', gColumnsAutoSaveWidth);
    SetValue(SubNode, 'TitleStyle', Integer(gColumnsTitleStyle));
    SubNode := FindNode(Node, 'BriefView', True);
    SetValue(SubNode, 'FileExtAligned', gBriefViewFileExtAligned);
    SubNode := FindNode(SubNode, 'Columns', True);
    SetValue(SubNode, 'FixedWidth', gBriefViewFixedWidth);
    SetValue(SubNode, 'FixedCount', gBriefViewFixedCount);
    SetValue(SubNode, 'AutoSize', Integer(gBriefViewMode));
    SetValue(Node, 'ExtraLineSpan', gExtraLineSpan);
    SetValue(Node, 'FolderPrefix', gFolderPrefix);
    SetValue(Node, 'FolderPostfix', gFolderPostfix);

    { Keys page }
    Node := FindNode(Root, 'Keyboard', True);
    SubNode := FindNode(Node, 'Typing/Actions', True);
    for KeyTypingModifier in TKeyTypingModifier do
      SetValue(SubNode, TKeyTypingModifierToNodeName[KeyTypingModifier],
        Integer(gKeyTyping[KeyTypingModifier]));

    { File operations page }
    Node := FindNode(Root, 'FileOperations', True);
    SetValue(Node, 'BufferSize', gCopyBlockSize);
    SetValue(Node, 'LongNameAlert', gLongNameAlert);
    SetValue(Node, 'HashBufferSize', gHashBlockSize);
    SetValue(Node, 'UseMmapInSearch', gUseMmapInSearch);
    SetValue(Node, 'PartialNameSearch', gPartialNameSearch);
    SetValue(Node, 'InitiallyClearFileMask', gInitiallyClearFileMask);
    SetValue(Node, 'NewSearchClearFiltersAction', integer(gNewSearchClearFiltersAction));
    SetValue(Node, 'ShowMenuBarInFindFiles', gShowMenuBarInFindFiles);
    SetValue(Node, 'WipePassNumber', gWipePassNumber);
    SetValue(Node, 'DropReadOnlyFlag', gDropReadOnlyFlag);
    SetValue(Node, 'ProcessComments', gProcessComments);
    SetValue(Node, 'RenameSelOnlyName', gRenameSelOnlyName);
    SetValue(Node, 'ShowCopyTabSelectPanel', gShowCopyTabSelectPanel);
    SetValue(Node, 'UseTrash', gUseTrash);
    SetValue(Node, 'SkipFileOpError', gSkipFileOpError);
    SetValue(Node, 'TypeOfDuplicatedRename', Integer(gTypeOfDuplicatedRename));
    SetValue(Node, 'ShowDialogOnDragDrop', gShowDialogOnDragDrop);
    SetValue(Node, 'DragAndDropTextRichtextDesireLevel', gDragAndDropDesiredTextFormat[DropTextRichText_Index].DesireLevel);
    SetValue(Node, 'DragAndDropTextHtmlDesireLevel',gDragAndDropDesiredTextFormat[DropTextHtml_Index].DesireLevel);
    SetValue(Node, 'DragAndDropTextUnicodeDesireLevel',gDragAndDropDesiredTextFormat[DropTextUnicode_Index].DesireLevel);
    SetValue(Node, 'DragAndDropTextSimpletextDesireLevel',gDragAndDropDesiredTextFormat[DropTextSimpleText_Index].DesireLevel);
    SetValue(Node, 'DragAndDropAskFormatEachTime', gDragAndDropAskFormatEachTime);
    SetValue(Node, 'DragAndDropTextAutoFilename', gDragAndDropTextAutoFilename);
    SetValue(Node, 'DragAndDropSaveUnicodeTextInUFT8', gDragAndDropSaveUnicodeTextInUFT8);
    SetValue(Node, 'NtfsHourTimeDelay', gNtfsHourTimeDelay);
    SetValue(Node, 'AutoExtractOpenMask', gAutoExtractOpenMask);
    SetValue(Node, 'SearchDefaultTemplate', gSearchDefaultTemplate);
    SetValue(Node, 'ProgressKind', Integer(gFileOperationsProgressKind));
    SetValue(Node, 'Confirmations', Integer(gFileOperationsConfirmations));
    // Operations options
    SubNode := FindNode(Node, 'Options', True);
    SetValue(SubNode, 'Symlink', Integer(gOperationOptionSymLinks));
    SetValue(SubNode, 'CorrectLinks', gOperationOptionCorrectLinks);
    SetValue(SubNode, 'CopyOnWrite', Integer(gOperationOptionCopyOnWrite));
    SetValue(SubNode, 'FileExists', Integer(gOperationOptionFileExists));
    SetValue(SubNode, 'DirectoryExists', Integer(gOperationOptionDirectoryExists));
    SetValue(SubNode, 'SetPropertyError', Integer(gOperationOptionSetPropertyError));
    SetValue(SubNode, 'ReserveSpace', gOperationOptionReserveSpace);
    SetValue(SubNode, 'CheckFreeSpace', gOperationOptionCheckFreeSpace);
    SetValue(SubNode, 'CopyAttributes', gOperationOptionCopyAttributes);
    SetValue(SubNode, 'CopyXattributes', gOperationOptionCopyXattributes);
    SetValue(SubNode, 'Verify', gOperationOptionVerify);
    SetValue(SubNode, 'CopyTime', gOperationOptionCopyTime);
    SetValue(SubNode, 'CopyOwnership', gOperationOptionCopyOwnership);
    SetValue(SubNode, 'CopyPermissions', gOperationOptionCopyPermissions);
    SetValue(SubNode, 'ExcludeEmptyTemplateDirectories', gOperationOptionExcludeEmptyDirectories);
    // Extract
    SubNode := FindNode(Node, 'Extract', True);
    if Assigned(SubNode) then
    begin
      SetValue(SubNode, 'Overwrite', gExtractOverwrite);
    end;

    // Multi-Rename
    SubNode := FindNode(Node, 'MultiRename', True);
    SetValue(SubNode, 'MulRenShowMenuBarOnTop', gMulRenShowMenuBarOnTop);
    SetValue(SubNode, 'MulRenInvalidCharReplacement', gMulRenInvalidCharReplacement);
    SetValue(SubNode, 'MulRenLaunchBehavor', Integer(gMulRenLaunchBehavior));
    SetValue(SubNode, 'MulRenExitModifiedPreset', Integer(gMulRenExitModifiedPreset));
    SetValue(SubNode, 'MulRenSaveRenamingLog', Integer(gMulRenSaveRenamingLog));
    SetValue(SubNode, 'MulRenLogFilename', gMulRenLogFilename);
    SetValue(SubNode, 'MultRenDailyIndividualDirLog', gMultRenDailyIndividualDirLog);
    SetValue(SubNode, 'MulRenFilenameWithFullPathInLog', gMulRenFilenameWithFullPathInLog);
    SetValue(SubNode, 'MulRenPathRangeSeparator', gMulRenPathRangeSeparator);

    { Tabs page }
    Node := FindNode(Root, 'Tabs', True);
    SetValue(Node, 'Options', Integer(gDirTabOptions));
    SetValue(Node, 'CharacterLimit', gDirTabLimit);
    SetValue(Node, 'Position', Integer(gDirTabPosition));
    SetValue(Node, 'ActionOnDoubleClick',Integer(gDirTabActionOnDoubleClick));

    { Log page }
    Node := FindNode(Root, 'Log', True);
    SetAttr(Node, 'Enabled', gLogFile);
    SetAttr(Node, 'Count', gLogFileCount);
    SetAttr(Node, 'LogFileWithDateInName', gLogFileWithDateInName);
    SetValue(Node, 'FileName', gLogFileName);
    SetValue(Node, 'Options', Integer(gLogOptions));

    { Configuration page }
    SetAttr(Root, 'Configuration/Save', gSaveConfiguration);
    SetAttr(Root, 'MainWindow/Position/Save', gSaveWindowState);
    SetAttr(Root, 'Configuration/FolderTabs/Save', gSaveFolderTabs);
    SetAttr(Root, 'History/SearchReplaceHistory/Save', gSaveSearchReplaceHistory);
    SetAttr(Root, 'History/DirHistory/Save', gSaveDirHistory);
    SetAttr(Root, 'History/CmdLineHistory/Save', gSaveCmdLineHistory);
    SetAttr(Root, 'History/FileMaskHistory/Save', gSaveFileMaskHistory);
    SetAttr(Root, 'Configuration/SortOrder', Integer(gSortOrderOfConfigurationOptionsTree));
    SetAttr(Root, 'Configuration/TreeType', Integer(gCollapseConfigurationOptionsTree));

    { Quick Search/Filter page }
    Node := FindNode(Root, 'QuickSearch', True);
    SetValue(Node, 'MatchBeginning', qsmBeginning in gQuickSearchOptions.Match);
    SetValue(Node, 'MatchEnding', qsmEnding in gQuickSearchOptions.Match);
    SetValue(Node, 'Case', Integer(gQuickSearchOptions.SearchCase));
    SetValue(Node, 'Items', Integer(gQuickSearchOptions.Items));
    Node := FindNode(Root, 'QuickFilter', True);
    SetValue(Node, 'AutoHide', gQuickFilterAutoHide);
    SetValue(Node, 'SaveSessionModifications', gQuickFilterSaveSessionModifications);

    { Misc page }
    Node := FindNode(Root, 'Miscellaneous', True);
    SetValue(Node, 'GridVertLine', gGridVertLine);
    SetValue(Node, 'GridHorzLine', gGridHorzLine);
    SetValue(Node, 'ShowWarningMessages', gShowWarningMessages);
    SetValue(Node, 'SpaceMovesDown', gSpaceMovesDown);
    SetValue(Node, 'DirBrackets', gDirBrackets);
    SetValue(Node, 'InplaceRename', gInplaceRename);
    SetValue(Node, 'InplaceRenameButton', gInplaceRenameButton);
    SetValue(Node, 'DblClickToParent', gDblClickToParent);
    SetValue(Node, 'HotDirAddTargetOrNot',gHotDirAddTargetOrNot);
    SetValue(Node, 'HotDirFullExpandOrNot', gHotDirFullExpandOrNot);
    SetValue(Node, 'ShowPathInPopup', gShowPathInPopup);
    SetValue(Node, 'ShowOnlyValidEnv', gShowOnlyValidEnv);
    SetValue(Node, 'WhereToAddNewHotDir', Integer(gWhereToAddNewHotDir));
    SetValue(Node, 'FilenameStyle', ord(gHotDirFilenameStyle));
    SetValue(Node, 'PathToBeRelativeTo', gHotDirPathToBeRelativeTo);
    SetValue(Node, 'PathModifierElements', Integer(gHotDirPathModifierElements));

    { Thumbnails }
    Node := FindNode(Root, 'Thumbnails', True);
    SetAttr(Node, 'Save', gThumbSave);
    SetValue(Node, 'Width', gThumbSize.cx);
    SetValue(Node, 'Height', gThumbSize.cy);

    { Description }
    Node := FindNode(Root, 'Description', True);
    SetValue(Node, 'CreateNewUnicode', gDescCreateUnicode);
    SetValue(Node, 'DefaultEncoding', Integer(gDescReadEncoding));
    SetValue(Node, 'CreateNewEncoding', Integer(gDescWriteEncoding));

    { Auto refresh page }
    Node := FindNode(Root, 'AutoRefresh', True);
    SetValue(Node, 'Options', Integer(gWatchDirs));
    SetValue(Node, 'ExcludeDirs', gWatchDirsExclude);
    SetValue(Node, 'Mode', Integer(gWatcherMode));

    { Icons page }
    Node := FindNode(Root, 'Icons', True);
    SetValue(Node, 'Theme', gIconTheme);
    SetValue(Node, 'ShowHiddenDimmed', gShowHiddenDimmed);
    SetValue(Node, 'ShowMode', Integer(gShowIconsNew));
    SetValue(Node, 'ShowOverlays', gIconOverlays);
    SetValue(Node, 'Size', gIconsSizeNew);
    SetValue(Node, 'DiskSize', gDiskIconsSize);
    SetValue(Node, 'DiskAlpha', gDiskIconsAlpha);
    SetValue(Node, 'ToolSize', gToolIconsSize);
    SetValue(Node, 'Exclude', gIconsExclude);
    SetValue(Node, 'ExcludeDirs', gIconsExcludeDirs);
    SetValue(Node, 'CustomIcons', Integer(gCustomIcons));
    SetValue(Node, 'PixelsPerInch', Screen.PixelsPerInch);
    SetAttr(Node, 'ShowInMenus/Enabled', gIconsInMenus);
    SetValue(Node, 'ShowInMenus/Size', gIconsInMenusSizeNew);
    SetValue(Node, 'ShowButtonGlyphs', Integer(Application.ShowButtonGlyphs));

    { Ignore list page }
    Node := FindNode(Root, 'IgnoreList', True);
    SetAttr(Node, 'Enabled', gIgnoreListFileEnabled);
    SetValue(Node, 'IgnoreListFile', gIgnoreListFile);

    { Directories HotList }
    gDirectoryHotlist.SaveToXml(gConfig, Root, TRUE);

    { Viewer }
    Node := FindNode(Root, 'Viewer',True);
    SetValue(Node, 'PreviewVisible',gPreviewVisible);
    SetValue(Node, 'ImageStretch',gImageStretch);
    SetValue(Node, 'ImageExifRotate', gImageExifRotate);
    SetValue(Node, 'ImageStretchLargeOnly',gImageStretchOnlyLarge);
    SetValue(Node, 'ImageCenter',gImageCenter);
    SetValue(Node, 'CopyMovePath1', gCopyMovePath1);
    SetValue(Node, 'CopyMovePath2', gCopyMovePath2);
    SetValue(Node, 'CopyMovePath3', gCopyMovePath3);
    SetValue(Node, 'CopyMovePath4', gCopyMovePath4);
    SetValue(Node, 'CopyMovePath5', gCopyMovePath5);
    SetValue(Node, 'PaintMode', Integer(gImagePaintMode));
    SetValue(Node, 'PaintWidth', gImagePaintWidth);
    SetValue(Node, 'NumberOfColumns', gColCount);
    SetValue(Node, 'TabSpaces', gTabSpaces);
    SetValue(Node, 'MaxTextWidth', gMaxTextWidth);
    SetValue(Node, 'ViewerMode' , gViewerMode);
    SetValue(Node, 'PrintMargins', gPrintMargins);
    SetValue(Node, 'ShowCaret'  , gShowCaret);

    SetValue(Node, 'PaintColor', gImagePaintColor);
    SetValue(Node, 'BackgroundColor', gBookBackgroundColor);
    SetValue(Node, 'FontColor', gBookFontColor);
    SetValue(Node, 'TextPosition', gTextPosition);

    { Editor }
    Node := FindNode(Root, 'Editor',True);
    SetValue(Node, 'EditWaitTime', gEditWaitTime);
    SetValue(Node, 'SynEditOptions', Integer(gEditorSynEditOptions));
    SetValue(Node, 'SynEditTabWidth', gEditorSynEditTabWidth);
    SetValue(Node, 'SynEditRightEdge', gEditorSynEditRightEdge);

    { Differ }
    Node := FindNode(Root, 'Differ',True);
    SetValue(Node, 'IgnoreCase', gDifferIgnoreCase);
    SetValue(Node, 'KeepScrolling', gDifferKeepScrolling);
    SetValue(Node, 'PaintBackground', gDifferPaintBackground);
    SetValue(Node, 'LineDifferences', gDifferLineDifferences);
    SetValue(Node, 'IgnoreWhiteSpace', gDifferIgnoreWhiteSpace);
    SubNode := FindNode(Node, 'Colors', True);
    SetValue(SubNode, 'Added', gDifferAddedColor);
    SetValue(SubNode, 'Deleted', gDifferDeletedColor);
    SetValue(SubNode, 'Modified', gDifferModifiedColor);

    { SyncDirs }
    Node := FindNode(Root, 'SyncDirs', True);
    SetValue(Node, 'Subdirs', gSyncDirsSubdirs);
    SetValue(Node, 'ByContent', gSyncDirsByContent);
    SetValue(Node, 'Asymmetric', gSyncDirsAsymmetric and gSyncDirsAsymmetricSave);
    SetAttr(Node, 'Asymmetric/Save', gSyncDirsAsymmetricSave);
    SetValue(Node, 'IgnoreDate', gSyncDirsIgnoreDate);
    SetValue(Node, 'FilterCopyRight', gSyncDirsShowFilterCopyRight);
    SetValue(Node, 'FilterEqual', gSyncDirsShowFilterEqual);
    SetValue(Node, 'FilterNotEqual', gSyncDirsShowFilterNotEqual);
    SetValue(Node, 'FilterCopyLeft', gSyncDirsShowFilterCopyLeft);
    SetValue(Node, 'FilterDuplicates', gSyncDirsShowFilterDuplicates);
    SetValue(Node, 'FilterSingles', gSyncDirsShowFilterSingles);
    SetValue(Node, 'FileMask', gSyncDirsFileMask);
    SubNode := FindNode(Node, 'Colors', True);
    SetValue(SubNode, 'Left', gSyncLeftColor);
    SetValue(SubNode, 'Right', gSyncRightColor);
    SetValue(SubNode, 'Unknown', gSyncUnknownColor);

    { Internal Associations}
    Node := FindNode(Root, 'InternalAssociations', True);
    SetValue(Node, 'OfferToAddNewFileType', gOfferToAddToFileAssociations);
    SetValue(Node, 'LastCustomAction', gFileAssociationLastCustomAction);
    SetValue(Node, 'ExpandedContextMenu', gExtendedContextMenu);
    SetValue(Node, 'ExecuteViaShell', gOpenExecuteViaShell);
    SetValue(Node, 'OpenSystemWithTerminalClose', gExecuteViaTerminalClose);
    SetValue(Node, 'OpenSystemWithTerminalStayOpen', gExecuteViaTerminalStayOpen);
    SetValue(Node, 'IncludeFileAssociation', gIncludeFileAssociation);
    SetValue(Node, 'FilenameStyle', ord(gFileAssocFilenameStyle));
    SetValue(Node, 'PathToBeRelativeTo', gFileAssocPathToBeRelativeTo);
    SetValue(Node, 'PathModifierElements', Integer(gFileAssocPathModifierElements));

    { Tree View Menu }
    Node := FindNode(Root, 'TreeViewMenu', True);
    SetValue(Node, 'UseTVMDirectoryHotlistFMC', gUseTreeViewMenuWithDirectoryHotlistFromMenuCommand);
    SetValue(Node, 'UseTVMDirectoryHotlistFDC', gUseTreeViewMenuWithDirectoryHotlistFromDoubleClick);
    SetValue(Node, 'UseTVMFavoriteTabsFMC', gUseTreeViewMenuWithFavoriteTabsFromMenuCommand);
    SetValue(Node, 'UseTVMFavoriteTabsFDC', gUseTreeViewMenuWithFavoriteTabsFromDoubleClick);
    SetValue(Node, 'UseTVMDirHistory', gUseTreeViewMenuWithDirHistory);
    SetValue(Node, 'UseTVMViewHistory', gUseTreeViewMenuWithViewHistory);
    SetValue(Node, 'UseTVMCommandLineHistory', gUseTreeViewMenuWithCommandLineHistory);
    SetValue(Node, 'TreeViewMenuShortcutExit', gTreeViewMenuShortcutExit);
    SetValue(Node, 'TreeViewMenuSingleClickExit', gTreeViewMenuSingleClickExit);
    SetValue(Node, 'TreeViewMenuDoubleClickExit', gTreeViewMenuDoubleClickExit);
    for iIndexContextMode:=0 to (ord(tvmcLASTONE)-2) do
    begin
      SubNode := FindNode(Node, Format('Context%.2d',[iIndexContextMode]), True);
      SetValue(SubNode, 'CaseSensitive', gTreeViewMenuOptions[iIndexContextMode].CaseSensitive);
      SetValue(SubNode, 'IgnoreAccents', gTreeViewMenuOptions[iIndexContextMode].IgnoreAccents);
      SetValue(SubNode, 'ShowWholeBranchIfMatch', gTreeViewMenuOptions[iIndexContextMode].ShowWholeBranchIfMatch);
    end;
    SetValue(Node, 'TreeViewMenuUseKeyboardShortcut', gTreeViewMenuUseKeyboardShortcut);
    SetValue(Node, 'BackgroundColor', gTVMBackgroundColor);
    SetValue(Node, 'ShortcutColor', gTVMShortcutColor);
    SetValue(Node, 'NormalTextColor', gTVMNormalTextColor);
    SetValue(Node, 'SecondaryTextColor', gTVMSecondaryTextColor);
    SetValue(Node, 'FoundTextColor', gTVMFoundTextColor);
    SetValue(Node, 'UnselectableTextColor', gTVMUnselectableTextColor);
    SetValue(Node, 'CursorColor', gTVMCursorColor);
    SetValue(Node, 'ShortcutUnderCursor', gTVMShortcutUnderCursor);
    SetValue(Node, 'NormalTextUnderCursor', gTVMNormalTextUnderCursor);
    SetValue(Node, 'SecondaryTextUnderCursor', gTVMSecondaryTextUnderCursor);
    SetValue(Node, 'FoundTextUnderCursor', gTVMFoundTextUnderCursor);
    SetValue(Node,'UnselectableUnderCursor', gTVMUnselectableUnderCursor);

    { Favorite Tabs }
    Node := FindNode(Root, 'FavoriteTabsOptions', True);
    SetValue(Node, 'FavoriteTabsUseRestoreExtraOptions', gFavoriteTabsUseRestoreExtraOptions);
    SetValue(Node, 'WhereToAdd', Integer(gWhereToAddNewFavoriteTabs));
    SetValue(Node, 'Expand', gFavoriteTabsFullExpandOrNot);
    SetValue(Node, 'GotoConfigAftSav', gFavoriteTabsGoToConfigAfterSave);
    SetValue(Node, 'GotoConfigAftReSav', gFavoriteTabsGoToConfigAfterReSave);
    SetValue(Node, 'DfltLeftGoTo', Integer(gDefaultTargetPanelLeftSaved));
    SetValue(Node, 'DfltRightGoTo', Integer(gDefaultTargetPanelRightSaved));
    SetValue(Node, 'DfltKeep', Integer(gDefaultExistingTabsToKeep));
    SetValue(Node, 'DfltSaveDirHistory', gFavoriteTabsSaveDirHistory);
    SetValue(Node, 'FavTabsLastUniqueID',GUIDtoString(gFavoriteTabsList.LastFavoriteTabsLoadedUniqueId));

    { - Other - }
    SetValue(Root, 'Lua/PathToLibrary', gLuaLib);
    SetValue(Root, 'NameShortcutFile', gNameSCFile);
    SetValue(Root, 'HotKeySortOrder', Integer(gHotKeySortOrder));
    SetValue(Root, 'UseEnterToCloseHotKeyEditor', gUseEnterToCloseHotKeyEditor);
    SetValue(Root, 'LastUsedPacker', gLastUsedPacker);
    SetValue(Root, 'LastDoAnyCommand', gLastDoAnyCommand);
    SetValue(Root, 'MarkMaskCaseSensitive', gbMarkMaskCaseSensitive);
    SetValue(Root, 'MarkMaskIgnoreAccents', gbMarkMaskIgnoreAccents);
    SetValue(Root, 'MarkMaskFilterWindows', gMarkMaskFilterWindows);
    SetValue(Root, 'MarkShowWantedAttribute', gMarkShowWantedAttribute);
    SetValue(Root, 'MarkDefaultWantedAttribute', gMarkDefaultWantedAttribute);
    SetValue(Root, 'MarkLastWantedAttribute', gMarkLastWantedAttribute);

    {$IFDEF MSWINDOWS}
    { TotalCommander Import/Export }
    //We'll save the last TC executable filename AND TC configuration filename ONLY if both has been set
    if (gTotalCommanderExecutableFilename<>'') AND (gTotalCommanderConfigFilename<>'') then
    begin
      Node := FindNode(Root, 'TCSection', True);
      if Assigned(Node) then
      begin
        SetValue(Node, 'TCExecutableFilename', gTotalCommanderExecutableFilename);
        SetValue(Node, 'TCConfigFilename', gTotalCommanderConfigFilename);
        SetValue(Node,'TCToolbarPath',gTotalCommanderToolbarPath);
      end;
    end;
    {$ENDIF}
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
  for iIndexContextMode:=ord(ptDSX) to ord(ptWLX) do
  begin
    gConfig.SetValue(Node, Format('TweakPluginWidth%d',[iIndexContextMode]), gTweakPluginWidth[iIndexContextMode]);
    gConfig.SetValue(Node, Format('TweakPluginHeight%d',[iIndexContextMode]), gTweakPluginHeight[iIndexContextMode]);
  end;
  gConfig.SetValue(Node, 'AutoTweak', gPluginInAutoTweak);
  gConfig.SetValue(Node, 'WCXConfigViewMode', Integer(gWCXConfigViewMode));
  gConfig.SetValue(Node, 'PluginFilenameStyle', ord(gPluginFilenameStyle));
  gConfig.SetValue(Node,'PluginPathToBeRelativeTo', gPluginPathToBeRelativeTo);  
end;

function LoadConfig: Boolean;
var
  ErrorMessage: String = '';
begin
  Result := LoadConfigCheckErrors(@LoadGlobalConfig, gConfig.FileName, ErrorMessage);
  if not Result then
    Result := AskUserOnError(ErrorMessage);
end;

function InitGlobs: Boolean;
var
  InitProc: TProcedure;
  ErrorMessage: String = '';
begin
  CreateGlobs;
  if not OpenConfig(ErrorMessage) then
  begin
    if not AskUserOnError(ErrorMessage) then
      Exit(False);
  end;

  SetDefaultNonConfigGlobs;

  if not LoadGlobs then
  begin
    if not AskUserOnError(ErrorMessage) then
      Exit(False);
  end;

  for InitProc in FInitList do
    InitProc();

  Result := AskUserOnError(ErrorMessage);
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
