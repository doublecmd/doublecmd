{
   Double Commander
   -------------------------------------------------------------------------
   Licence  : GNU GPL v 2.0
   Copyright (C) 2006-2019 Alexander Koblov (Alexx2000@mail.ru)

   Main Dialog window

   based on:

   Seksi Commander (radekc.regnet.cz)
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   Main Dialog window and other stuff

   contributors:


   based on (heavy rewriten):

   main Unit of PFM : Peter's File Manager
   ---------------------------------------

   Copyright : Peter Cernoch 2002
   Contact   : pcernoch@volny.cz
   Licence   : GNU GPL v 2.0
   
   contributors:

   Copyright (C) 2008 Vitaly Zotov (vitalyzotov@mail.ru)

}

unit fMain;

{$mode objfpc}{$H+}

interface

uses
  ufavoritetabs, Graphics, Forms, Menus, Controls, StdCtrls, ExtCtrls, ActnList,
  Buttons, SysUtils, Classes, SynEdit, LCLType, ComCtrls, LResources,
  KASToolBar, KASComboBox, uCmdBox, uFilePanelSelect, uBriefFileView,
  uFileView, uFileSource, uFileViewNotebook, uFile, LCLVersion,
  uOperationsManager, uFileSourceOperation, uDrivesList, uTerminal, DCClassesUtf8,
  DCXmlConfig, uDrive, uDriveWatcher, uDCVersion, uMainCommands, uFormCommands,
  uOperationsPanel, KASToolItems, uKASToolItemsExtended, uCmdLineParams, uOSForms
  {$IF DEFINED(LCLQT)}
  , Qt4, QtWidgets
  {$ELSEIF DEFINED(LCLQT5)}
  , Qt5, QtWidgets
  {$ELSEIF DEFINED(LCLGTK2)}
  , Glib2, Gtk2
  {$ENDIF}
  , Types;

type

  TForEachViewFunction = procedure (AFileView: TFileView; UserData: Pointer) of object;

  { TfrmMain }

  TfrmMain = class(TAloneForm, IFormCommands)
    actExtractFiles: TAction;
    actAddPathToCmdLine: TAction;
    actFileAssoc: TAction;
    actFocusCmdLine: TAction;
    actContextMenu: TAction;
    actCopyNamesToClip: TAction;
    actCopyFullNamesToClip: TAction;
    actCutToClipboard: TAction;
    actCopyToClipboard: TAction;
    actChangeDirToRoot: TAction;
    actCountDirContent: TAction;
    actCheckSumVerify: TAction;
    actCheckSumCalc: TAction;
    actClearLogFile: TAction;
    actClearLogWindow: TAction;
    actChangeDir: TAction;
    actAddFilenameToCmdLine: TAction;
    actAddPathAndFilenameToCmdLine: TAction;
    actCopyNoAsk: TAction;
    actChangeDirToParent: TAction;
    actEditPath: TAction;
    actHorizontalFilePanels: TAction;
    actGoToFirstEntry: TAction;
    actGoToLastEntry: TAction;
    actGoToFirstFile: TAction;
    actGoToLastFile: TAction;
    actCompareDirectories: TAction;
    actCmdLineNext: TAction;
    actCmdLinePrev: TAction;
    actBriefView: TAction;
    actColumnsView: TAction;
    actChangeDirToHome: TAction;
    actCopyFileDetailsToClip: TAction;
    actFlatView: TAction;
    actConfigDirHotList: TAction;
    actCopyPathOfFilesToClip: TAction;
    actCopyPathNoSepOfFilesToClip: TAction;
    actDoAnyCmCommand: TAction;
    actCloseDuplicateTabs: TAction;
    actCopyAllTabsToOpposite: TAction;
    actConfigTreeViewMenus: TAction;
    actConfigTreeViewMenusColors: TAction;
    actConfigSavePos: TAction;
    actConfigSaveSettings: TAction;
    actExecuteScript: TAction;
    actFocusSwap: TAction;
    actConfigArchivers: TAction;
    actConfigTooltips: TAction;
    actConfigPlugins: TAction;
    actUnmarkCurrentNameExt: TAction;
    actMarkCurrentNameExt: TAction;
    actUnmarkCurrentName: TAction;
    actMarkCurrentName: TAction;
    actUnmarkCurrentPath: TAction;
    actMarkCurrentPath: TAction;
    actTreeView: TAction;
    actFocusTreeView: TAction;
    actToggleFullscreenConsole: TAction;
    actSrcOpenDrives: TAction;
    actRightReverseOrder: TAction;
    actLeftReverseOrder: TAction;
    actRightFlatView: TAction;
    actLeftFlatView: TAction;
    actRightSortByAttr: TAction;
    actRightSortByDate: TAction;
    actRightSortBySize: TAction;
    actRightSortByExt: TAction;
    actRightSortByName: TAction;
    actLeftSortByAttr: TAction;
    actLeftSortByDate: TAction;
    actLeftSortBySize: TAction;
    actLeftSortByExt: TAction;
    actLeftSortByName: TAction;
    actLeftThumbView: TAction;
    actRightThumbView: TAction;
    actRightColumnsView: TAction;
    actLeftColumnsView: TAction;
    actRightBriefView: TAction;
    actLeftBriefView: TAction;
    actWorkWithDirectoryHotlist: TAction;
    actUniversalSingleDirectSort: TAction;
    actViewLogFile: TAction;
    actLoadTabs: TAction;
    actSaveTabs: TAction;
    actSyncDirs: TAction;
    actThumbnailsView: TAction;
    actShellExecute: TAction;
    actRenameTab: TAction;
    actOperationsViewer: TAction;
    actNetworkDisconnect: TAction;
    actNetworkQuickConnect: TAction;
    actNetworkConnect: TAction;
    actViewHistory: TAction;
    actViewHistoryPrev: TAction;
    actViewHistoryNext: TAction;
    actLoadSelectionFromClip: TAction;
    actLoadSelectionFromFile: TAction;
    actSaveSelectionToFile: TAction;
    actSaveSelection: TAction;
    actRestoreSelection: TAction;
    actSwitchIgnoreList: TAction;
    actTestArchive: TAction;
    actQuickView: TAction;
    actOpenBar: TAction;
    actSetFileProperties: TAction;
    actQuickFilter: TAction;
    actRenameNoAsk: TAction;
    actPanelsSplitterPerPos: TAction;
    actMinimize: TAction;
    actRightEqualLeft: TAction;
    actLeftEqualRight: TAction;
    actPasteFromClipboard: TAction;
    actExchange: TAction;
    actEditComment: TAction;
    actHelpIndex: TAction;
    actVisitHomePage: TAction;
    actKeyboard: TAction;
    actPrevTab: TAction;
    actNextTab: TAction;
    actActivateTabByIndex: TAction;
    actCloseAllTabs: TAction;
    actSetTabOptionNormal: TAction;
    actSetTabOptionPathLocked: TAction;
    actSetTabOptionPathResets: TAction;
    actSetTabOptionDirsInNewTab: TAction;
    actUnmarkCurrentExtension: TAction;
    actMarkCurrentExtension: TAction;
    actWipe: TAction;
    actOpenDirInNewTab: TAction;
    actTargetEqualSource: TAction;
    actOpen: TAction;
    actQuickSearch: TAction;
    actShowButtonMenu: TAction;
    actOpenArchive: TAction;
    actTransferRight: TAction;
    actTransferLeft: TAction;
    actRightOpenDrives: TAction;
    actLeftOpenDrives: TAction;
    actOpenVirtualFileSystemList: TAction;
    actPackFiles: TAction;
    actCloseTab: TAction;
    actNewTab: TAction;
    actConfigToolbars: TAction;
    actDebugShowCommandParameters: TAction;
    actOpenDriveByIndex: TAction;
    btnF10: TSpeedButton;
    btnF3: TSpeedButton;
    btnF4: TSpeedButton;
    btnF5: TSpeedButton;
    btnF6: TSpeedButton;
    btnF7: TSpeedButton;
    btnF8: TSpeedButton;
    btnF9: TSpeedButton;
    btnLeftDirectoryHotlist: TSpeedButton;
    btnRightDirectoryHotlist: TSpeedButton;
    dskLeft: TKASToolBar;
    dskRight: TKASToolBar;
    edtCommand: TComboBoxWithDelItems;
    imgLstActions: TImageList;
    imgLstDirectoryHotlist: TImageList;
    lblRightDriveInfo: TLabel;
    lblLeftDriveInfo: TLabel;
    lblCommandPath: TLabel;
    miConfigArchivers: TMenuItem;
    mnuConfigSavePos: TMenuItem;
    mnuConfigSaveSettings: TMenuItem;
    miLine55: TMenuItem;
    mnuConfigureFavoriteTabs: TMenuItem;
    mnuRewriteFavoriteTabs: TMenuItem;
    mnuCreateNewFavoriteTabs: TMenuItem;
    mnuReloadActiveFavoriteTabs: TMenuItem;
    mnuFavoriteTabs: TMenuItem;
    mnuCloseDuplicateTabs: TMenuItem;
    miCloseDuplicateTabs: TMenuItem;
    mnuTreeView: TMenuItem;
    mnuCmdConfigDirHotlist: TMenuItem;
    mnuLoadTabs: TMenuItem;
    mnuSaveTabs: TMenuItem;
    miLine38: TMenuItem;
    miFlatView: TMenuItem;
    miMakeDir: TMenuItem;
    miWipe: TMenuItem;
    miDelete: TMenuItem;
    miLine50: TMenuItem;
    miCopyFileDetailsToClip: TMenuItem;
    mnuCmdSyncDirs: TMenuItem;
    mnuContextRenameOnly: TMenuItem;
    mnuContextCopy: TMenuItem;
    mnuContextOpen: TMenuItem;
    mnuContextLine1: TMenuItem;
    mnuContextLine2: TMenuItem;
    mnuContextFileProperties: TMenuItem;
    mnuContextDelete: TMenuItem;
    mnuContextView: TMenuItem;
    mnuThumbnailsView: TMenuItem;
    mnuColumnsView: TMenuItem;
    mnuBriefView: TMenuItem;
    miLine33: TMenuItem;
    mnuAllOperStart: TMenuItem;
    mnuAllOperStop: TMenuItem;
    mnuAllOperPause: TMenuItem;
    mnuAllOperProgress: TMenuItem;
    miCompareDirectories: TMenuItem;
    miLine37: TMenuItem;
    miRenameTab: TMenuItem;
    pnlMain: TPanel;
    tbChangeDir: TMenuItem;
    mnuShowHorizontalFilePanels: TMenuItem;
    miLine20: TMenuItem;
    miNetworkDisconnect: TMenuItem;
    miNetworkQuickConnect: TMenuItem;
    miNetworkConnect: TMenuItem;
    mnuNetwork: TMenuItem;
    pnlDskLeft: TPanel;
    pnlDiskLeftInner: TPanel;
    pnlDskRight: TPanel;
    pnlDiskRightInner: TPanel;
    Timer: TTimer;
    PanelAllProgress: TPanel;
    pbxRightDrive: TPaintBox;
    pbxLeftDrive: TPaintBox;
    tbPaste: TMenuItem;
    tbCopy: TMenuItem;
    tbCut: TMenuItem;
    tbSeparator: TMenuItem;
    mnuLoadSelectionFromClip: TMenuItem;
    mnuLoadSelectionFromFile: TMenuItem;
    mnuSaveSelectionToFile: TMenuItem;
    mnuRestoreSelection: TMenuItem;
    mnuSaveSelection: TMenuItem;
    miLine47: TMenuItem;
    mnuTestArchive: TMenuItem;
    mnuQuickView: TMenuItem;
    miLine32: TMenuItem;
    miLine14: TMenuItem;
    mnuTabOptionNormal: TMenuItem;
    mnuTabOptionDirsInNewTabs: TMenuItem;
    mnuTabOptions: TMenuItem;
    miTabOptionPathResets: TMenuItem;
    miTabOptionDirsInNewTab: TMenuItem;
    miTabOptionPathLocked: TMenuItem;
    miTabOptionNormal: TMenuItem;
    miTabOptions: TMenuItem;
    miLine19: TMenuItem;
    mnuSetFileProperties: TMenuItem;
    mnuShowOperations: TMenuItem;
    miLine13: TMenuItem;
    miLogClear: TMenuItem;
    miLogHide: TMenuItem;
    miLine25: TMenuItem;
    miLogSelectAll: TMenuItem;
    miLogCopy: TMenuItem;
    miLine24: TMenuItem;
    miTrayIconRestore: TMenuItem;
    miLine8: TMenuItem;
    miTrayIconExit: TMenuItem;
    mnuCheckSumCalc: TMenuItem;
    mnuCheckSumVerify: TMenuItem;
    mnuCountDirContent: TMenuItem;
    miLine22: TMenuItem;
    miLine18: TMenuItem;
    mnuHelpIndex: TMenuItem;
    mnuHelpVisitHomePage: TMenuItem;
    mnuHelpKeyboard: TMenuItem;
    MenuItem2: TMenuItem;
    mnuPrevTab: TMenuItem;
    mnuNextTab: TMenuItem;
    miLine17: TMenuItem;
    miLine16: TMenuItem;
    mnuTabOptionPathLocked: TMenuItem;
    mnuTabOptionPathResets: TMenuItem;
    mnuCloseAllTabs: TMenuItem;
    mnuCloseTab: TMenuItem;
    miLine15: TMenuItem;
    mnuOpenDirInNewTab: TMenuItem;
    mnuNewTab: TMenuItem;
    miCloseAllTabs: TMenuItem;
    miCloseTab: TMenuItem;
    miNewTab: TMenuItem;
    miEditComment: TMenuItem;
    mnuMarkCurrentExtension: TMenuItem;
    mnuTabs: TMenuItem;
    mnuUnmarkCurrentExtension: TMenuItem;
    miSymLink: TMenuItem;
    miHardLink: TMenuItem;
    miCancel: TMenuItem;
    miLine12: TMenuItem;
    miCopy: TMenuItem;
    miMove: TMenuItem;
    mi8020: TMenuItem;
    mi7030: TMenuItem;
    mi6040: TMenuItem;
    mi5050: TMenuItem;
    mi4060: TMenuItem;
    mi3070: TMenuItem;
    mi2080: TMenuItem;
    miCopyFullNamesToClip: TMenuItem;
    miCopyNamesToClip: TMenuItem;
    mnuFileAssoc: TMenuItem;
    nbConsole: TPageControl;
    pgConsole: TTabSheet;
    pnlCmdLine: TPanel;
    MainSplitter: TPanel;
    MainToolBar: TKASToolBar;
    mnuOpenVFSList: TMenuItem;
    mnuExtractFiles: TMenuItem;
    pmContextMenu: TPopupMenu;
    pmSplitterPercent: TPopupMenu;
    pnlCommand: TPanel;
    pnlKeys: TPanel;
    pnlLeftTools: TPanel;
    pnlRightTools: TPanel;
    pnlRight: TPanel;
    pnlLeft: TPanel;
    btnLeftDrive: TSpeedButton;
    btnLeftHome: TSpeedButton;
    btnLeftUp: TSpeedButton;
    btnLeftRoot: TSpeedButton;
    btnRightDrive: TSpeedButton;
    btnRightHome: TSpeedButton;
    btnRightUp: TSpeedButton;
    btnRightRoot: TSpeedButton;
    LogSplitter: TSplitter;
    pmColumnsMenu: TPopupMenu;
    pmDropMenu: TPopupMenu;
    pmTabMenu: TPopupMenu;
    pmTrayIconMenu: TPopupMenu;
    pmLogMenu: TPopupMenu;
    seLogWindow: TSynEdit;
    btnRightEqualLeft: TSpeedButton;
    btnLeftEqualRight: TSpeedButton;
    ConsoleSplitter: TSplitter;
    tbDelete: TMenuItem;
    tbEdit: TMenuItem;
    mnuMain: TMainMenu;
    pnlNotebooks: TPanel;
    pnlDisk: TPanel;
    mnuHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuShow: TMenuItem;
    mnuShowName: TMenuItem;
    mnuShowExtension: TMenuItem;
    mnuShowTime: TMenuItem;
    mnuShowSize: TMenuItem;
    mnuShowAttrib: TMenuItem;
    miLine7: TMenuItem;
    mnuShowReverse: TMenuItem;
    mnuShowReread: TMenuItem;
    mnuFiles: TMenuItem;
    mnuPackFiles : TMenuItem;
    mnuFilesSplit: TMenuItem;
    mnuFilesCombine: TMenuItem;
    mnuCmd: TMenuItem;
    mnuCmdDirHotlist: TMenuItem;
    miLine2: TMenuItem;
    mnuFilesSpace: TMenuItem;
    mnuFilesAttrib: TMenuItem;
    mnuFilesProperties: TMenuItem;
    miLine6: TMenuItem;
    mnuCmdSwapSourceTarget: TMenuItem;
    mnuCmdTargetIsSource: TMenuItem;
    miLine3: TMenuItem;
    mnuFilesShwSysFiles: TMenuItem;
    miLine1: TMenuItem;
    mnuFilesHardLink: TMenuItem;
    mnuFilesSymLink: TMenuItem;
    mnuConfig: TMenuItem;
    mnuConfigOptions: TMenuItem;
    mnuMark: TMenuItem;
    mnuMarkSGroup: TMenuItem;
    mnuMarkUGroup: TMenuItem;
    mnuMarkSAll: TMenuItem;
    mnuMarkUAll: TMenuItem;
    mnuMarkInvert: TMenuItem;
    miLine5: TMenuItem;
    mnuCmdSearch: TMenuItem;
    mnuCmdAddNewSearch:TMenuItem;
    mnuCmdViewSearches:TMenuItem;
    actionLst: TActionList;
    actExit: TAction;
    actView: TAction;
    actEdit: TAction;
    actCopy: TAction;
    actRename: TAction;
    actMakeDir: TAction;
    actDelete: TAction;
    actAbout: TAction;
    actShowSysFiles: TAction;
    actOptions: TAction;
    mnuFilesCmpCnt: TMenuItem;
    actCompareContents: TAction;
    actShowMainMenu: TAction;
    actRefresh: TAction;
    actSearch: TAction;
    actAddNewSearch: TAction;
    actViewSearches: TAction;
    actDeleteSearches: TAction;
    actConfigSearches: TAction;
    actConfigHotKeys: TAction;
    actDirHotList: TAction;
    actMarkMarkAll: TAction;
    actMarkInvert: TAction;
    actMarkUnmarkAll: TAction;
    pmHotList: TPopupMenu;
    actMarkPlus: TAction;
    actMarkMinus: TAction;
    actSymLink: TAction;
    actHardLink: TAction;
    actReverseOrder: TAction;
    actSortByName: TAction;
    actSortByExt: TAction;
    actSortBySize: TAction;
    actSortByDate: TAction;
    actSortByAttr: TAction;
    miLine4: TMenuItem;
    miExit: TMenuItem;
    actMultiRename: TAction;
    miMultiRename: TMenuItem;
    actCopySamePanel: TAction;
    actRenameOnly: TAction;
    actEditNew: TAction;
    actDirHistory: TAction;
    pmDirHistory: TPopupMenu;
    actShowCmdLineHistory: TAction;
    actRunTerm: TAction;
    miLine9: TMenuItem;
    miRunTerm: TMenuItem;
    actBenchmark: TAction;
    actCalculateSpace: TAction;
    actFileProperties:  TAction;
    actFileLinker: TAction;
    actFileSpliter: TAction;
    pmToolBar: TPopupMenu;
    MainTrayIcon: TTrayIcon;
    TreePanel: TPanel;
    TreeSplitter: TSplitter;
    ShellTreeView: TCustomTreeView;
    miLine10: TMenuItem;
    miLine11: TMenuItem;
    miLine21: TMenuItem;
    miLine23: TMenuItem;
    miLine26: TMenuItem;
    miLine39: TMenuItem;
    miLine40: TMenuItem;
    actSetAllTabsOptionNormal: TAction;
    actSetAllTabsOptionPathLocked: TAction;
    actSetAllTabsOptionPathResets: TAction;
    actSetAllTabsOptionDirsInNewTab: TAction;
    actConfigFolderTabs: TAction;
    actLoadFavoriteTabs: TAction;
    actConfigFavoriteTabs: TAction;
    actSaveFavoriteTabs: TAction;
    actReloadFavoriteTabs: TAction;
    actNextFavoriteTabs: TAction;
    actPreviousFavoriteTabs: TAction;
    pmFavoriteTabs: TPopupMenu;
    mnuRenameTab: TMenuItem;
    mnuConfigFolderTabs: TMenuItem;
    mnuConfigFavoriteTabs: TMenuItem;
    mnuConfigurationFavoriteTabs: TMenuItem;
    mnuSaveFavoriteTabs: TMenuItem;
    mnuLoadFavoriteTabs: TMenuItem;
    mnuConfigurationFolderTabs: TMenuItem;
    mnuSetAllTabsOptionNormal: TMenuItem;
    mnuSetAllTabsOptionPathLocked: TMenuItem;
    mnuSetAllTabsOptionPathResets: TMenuItem;
    mnuSetAllTabsOptionDirsInNewTab: TMenuItem;
    miConfigFolderTabs: TMenuItem;
    miConfigFavoriteTabs: TMenuItem;
    miNextTab: TMenuItem;
    miPrevTab: TMenuItem;
    miSaveTabs: TMenuItem;
    miLoadTabs: TMenuItem;
    miSaveFavoriteTabs: TMenuItem;
    miLoadFavoriteTabs: TMenuItem;
    miSetAllTabsOptionNormal: TMenuItem;
    miSetAllTabsOptionPathLocked: TMenuItem;
    miSetAllTabsOptionPathResets: TMenuItem;
    miSetAllTabsOptionDirsInNewTab: TMenuItem;
    miOpenDirInNewTab: TMenuItem;
    actResaveFavoriteTabs: TAction;
    procedure actExecute(Sender: TObject);
    procedure btnF3MouseWheelDown(Sender: TObject; Shift: TShiftState;
      {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure btnF3MouseWheelUp(Sender: TObject; Shift: TShiftState;
      {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure btnF8MouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormKeyUp( Sender: TObject; var {%H-}Key: Word; Shift: TShiftState) ;
    function MainToolBarToolItemShortcutsHint(ToolItem: TKASNormalItem): String;
    procedure mnuAllOperStartClick(Sender: TObject);
    procedure mnuAllOperStopClick(Sender: TObject);
    procedure mnuAllOperPauseClick(Sender: TObject);
    procedure mnuAllOperProgressClick(Sender: TObject);
    procedure btnF8Click(Sender: TObject);
    procedure btnLeftClick(Sender: TObject);
    procedure btnLeftDirectoryHotlistClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure btnRightDirectoryHotlistClick(Sender: TObject);
    procedure btnDriveMouseUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure ConsoleSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var {%H-}Accept: Boolean);
    procedure dskLeftResize(Sender: TObject);
    procedure dskRightResize(Sender: TObject);
    procedure dskLeftRightToolButtonDragDrop(Sender, {%H-}Source: TObject; {%H-}X, {%H-}Y: Integer);
    procedure dskToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lblAllProgressPctClick(Sender: TObject);
    procedure MainToolBarToolButtonDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MainToolBarToolButtonDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean;
      NumberOfButton: Integer);
    procedure MainToolBarToolButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MainToolBarToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure miLogMenuClick(Sender: TObject);
    procedure miTrayIconExitClick(Sender: TObject);
    procedure miTrayIconRestoreClick(Sender: TObject);
    procedure PanelButtonClick(Button: TSpeedButton; FileView: TFileView);
    procedure ShellTreeViewSelect;
    procedure ShellTreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ShellTreeViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbDeleteClick(Sender: TObject);
    procedure dskLeftToolButtonClick(Sender: TObject);
    procedure dskRightToolButtonClick(Sender: TObject);
    procedure btnVirtualDriveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MainSplitterDblClick(Sender: TObject);
    procedure MainSplitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainSplitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MainSplitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);   
    procedure MainTrayIconClick(Sender: TObject);
    procedure lblDriveInfoDblClick(Sender: TObject);
    procedure MainToolBarDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MainToolBarDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    function MainToolBarLoadButtonGlyph(ToolItem: TKASToolItem;
      iIconSize: Integer; clBackColor: TColor): TBitmap;
    function MainToolBarLoadButtonOverlay(ToolItem: TKASToolItem;
      iIconSize: Integer; clBackColor: TColor): TBitmap;
    procedure MainToolBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure frmMainClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frmMainAfterShow(Sender: TObject);
    procedure frmMainShow(Sender: TObject);
    procedure mnuDropClick(Sender: TObject);
    procedure mnuSplitterPercentClick(Sender: TObject);
    procedure mnuTabMenuExecute(Sender: TObject);
    procedure mnuTabMenuClick(Sender: TObject);
    procedure nbPageAfterMouseDown(Data: PtrInt);
    procedure nbPageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure nbPageChanged(Sender: TObject);
    procedure nbPageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NotebookDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure NotebookDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure NotebookCloseTabClicked(Sender: TObject);
    procedure pmDropMenuClose(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure edtCommandKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pmToolBarPopup(Sender: TObject);

    procedure ShellTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);

    procedure pnlLeftResize(Sender: TObject);
    procedure pnlLeftRightDblClick(Sender: TObject);
    procedure pnlNotebooksResize(Sender: TObject);
    procedure pnlRightResize(Sender: TObject);
    procedure sboxDrivePaint(Sender: TObject);
    procedure PaintDriveFreeBar(Sender: TObject; bIndUseGradient:boolean; pIndForeColor,pIndBackColor:TColor);
    procedure seLogWindowSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);

    function FileViewBeforeChangePath(FileView: TFileView; NewFileSource: IFileSource; const NewPath : String): Boolean;
    procedure FileViewAfterChangePath(FileView: TFileView);
    procedure FileViewActivate(FileView: TFileView);
    procedure FileViewFilesChanged(FileView: TFileView);
    procedure edtCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtCommandExit(Sender: TObject);
    procedure tbChangeDirClick(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
    procedure OnUniqueInstanceMessage(Sender: TObject; Params: TCommandLineParams);
    procedure tbPasteClick(Sender: TObject);
    procedure AllProgressOnUpdateTimer(Sender: TObject);
    procedure OnCmdBoxInput(ACmdBox: TCmdBox; AInput: String);
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  private
    QEventHook: QObject_hookH;
    function QObjectEventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
{$ENDIF}
{$IF DEFINED(LCLGTK2)}
    procedure WindowStateUpdate(Data: PtrInt);
{$ENDIF}
  private
    { Private declarations }
    FMainSplitterPos: Double;
    PanelSelected: TFilePanelSelect;
    DrivesList : TDrivesList;
    MainSplitterHintWnd: THintWindow;
    HiddenToTray: Boolean;
    HidingTrayIcon: Boolean; // @true if the icon is in the process of hiding
    nbLeft, nbRight: TFileViewNotebook;
    cmdConsole: TCmdBox;
    FCommands: TMainCommands;
    FInitializedView: Boolean;
    {en
       Used to pass drag&drop parameters to pmDropMenu. Single variable
       can be used, because the user can do only one menu popup at a time. }
    FDropParams: TDropParams;
    FDrivesListPopup: TDrivesListPopup;
    FOperationsPanel: TOperationsPanel;

    // frost_asm begin
    // mainsplitter
    MainSplitterLeftMouseBtnDown: Boolean;
    MainSplitterMouseDownX, MainSplitterMouseDownY: Integer;
    FResizingFilePanels: Boolean;
    // lastWindowState
    lastWindowState:TWindowState;
    // frost_asm end
    // for dragging buttons and etc
    NumberOfMoveButton, NumberOfNewMoveButton: integer;
    Draging : boolean;
    FUpdateDiskCount: Boolean;
    FModalOperationResult: Boolean;

    procedure CheckCommandLine(ShiftEx: TShiftState; var Key: Word);
    function ExecuteCommandFromEdit(sCmd: String; bRunInTerm: Boolean): Boolean;
    procedure SetMainSplitterPos(AValue: Double);
    procedure SetPanelSelected(AValue: TFilePanelSelect);
    procedure UpdateActionIcons;
    procedure UpdateHotDirIcons;
    procedure TypeInCommandLine(Str: String);
    procedure AddVirtualDriveButton(dskPanel: TKASToolBar);
    procedure AddSpecialButtons(dskPanel: TKASToolBar);
    procedure HideToTray;
    procedure RestoreFromTray;
    procedure ShowTrayIcon(bShow: Boolean);
    procedure HideTrayIconDelayed(Data: PtrInt);

    procedure PopupDragDropMenu(var DropParams: TDropParams);
    procedure CloseNotebook(ANotebook: TFileViewNotebook);

    procedure DriveListDriveSelected(Sender: TObject; ADriveIndex: Integer;
      APanel: TFilePanelSelect);
    procedure DriveListClose(Sender: TObject);
    function  FindMatchingDrive(Address, Path: String): Integer;
    procedure UpdateDriveToolbarSelection(DriveToolbar: TKAStoolBar; FileView: TFileView);
    procedure UpdateDriveButtonSelection(DriveButton: TSpeedButton; FileView: TFileView);
    procedure UpdateSelectedDrive(ANoteBook: TFileViewNotebook);
    procedure OnDriveWatcherEvent(EventType: TDriveWatcherEvent; const ADrive: PDrive);
    procedure AppActivate(Sender: TObject);
    procedure AppDeActivate(Sender: TObject);
    procedure AppEndSession(Sender: TObject);
    procedure AppQueryEndSession(var Cancel: Boolean);
    procedure AppException(Sender: TObject; E: Exception);
    procedure AppShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    {en
       Convert toolbar configuration from .bar file to global config.
    }
    procedure ConvertToolbarBarConfig(BarFileName: String);
    procedure ConvertIniToolbarItem(Loader: TKASToolBarIniLoader; var Item: TKASToolItem; const Shortcut: String);
    procedure CreateDefaultToolbar;
    procedure EditToolbarButton(Button: TKASToolButton);
    procedure ToolbarExecuteCommand(ToolItem: TKASToolItem);
    procedure ToolbarExecuteProgram(ToolItem: TKASToolItem);
    procedure LeftDriveBarExecuteDrive(ToolItem: TKASToolItem);
    procedure RightDriveBarExecuteDrive(ToolItem: TKASToolItem);
    procedure SetDragCursor(Shift: TShiftState);

  protected
    procedure CreateWnd; override;
{$if lcl_fullversion >= 1070000}
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                            const AXProportion, AYProportion: Double); override;
{$endif}

  public
    constructor Create(TheOwner: TComponent); override;
    procedure AfterConstruction; override;
    Function ActiveFrame: TFileView;  // get Active frame
    Function NotActiveFrame: TFileView; // get NotActive frame :)
    function ActiveNotebook: TFileViewNotebook;
    function NotActiveNotebook: TFileViewNotebook;
    function FrameLeft: TFileView;
    function FrameRight: TFileView;
    procedure ForEachView(CallbackFunction: TForEachViewFunction; UserData: Pointer);
    procedure GetListOpenedPaths(const APaths:TStringList);
    //check selected count and generate correct msg, parameters is lng indexs
    Function GetFileDlgStr(sLngOne, sLngMulti : String; Files: TFiles):String;
    procedure HotDirSelected(Sender:TObject);
    procedure HotDirActualSwitchToDir(Index:longint);
    procedure HistorySelected(Sender:TObject);
    procedure ViewHistorySelected(Sender:TObject);
    procedure ViewHistoryPrevSelected(Sender:TObject);
    procedure ViewHistoryNextSelected(Sender:TObject);
    procedure CreatePopUpDirHistory;
    procedure ShowFileViewHistory(const Params: array of string);
    procedure ShowFileViewHistory(const Params: array of string; FromFileSourceIndex, FromPathIndex, ToFileSourceIndex, ToPathIndex: Integer);
    procedure miHotAddOrConfigClick(Sender: TObject);
    procedure OnCopyOutTempStateChanged(Operation: TFileSourceOperation; State: TFileSourceOperationState);

    {en
       Returns @true if copy operation has been successfully started.
    }
    function CopyFiles(SourceFileSource, TargetFileSource: IFileSource;
                       var SourceFiles: TFiles; TargetPath: String;
                       bShowDialog: Boolean;
                       QueueIdentifier: TOperationsManagerQueueIdentifier = FreeOperationsQueueId): Boolean; overload;
    {en
       Returns @true if move operation has been successfully started.
    }
    function MoveFiles(SourceFileSource, TargetFileSource: IFileSource;
                       var SourceFiles: TFiles; TargetPath: String;
                       bShowDialog: Boolean;
                       QueueIdentifier: TOperationsManagerQueueIdentifier = FreeOperationsQueueId): Boolean; overload;
    function CopyFiles(sDestPath: String; bShowDialog: Boolean;
                       QueueIdentifier: TOperationsManagerQueueIdentifier = FreeOperationsQueueId): Boolean; overload; //  this is for F5 and Shift+F5
    function MoveFiles(sDestPath: String; bShowDialog: Boolean;
                       QueueIdentifier: TOperationsManagerQueueIdentifier = FreeOperationsQueueId): Boolean; overload;
    procedure GetDestinationPathAndMask(SourceFiles: TFiles;
                                        TargetFileSource: IFileSource;
                                        EnteredPath: String; BaseDir: String;
                                        out DestPath, DestMask: String); overload;
    procedure GetDestinationPathAndMask(SourceFiles: TFiles;
                                        SourceFileSource: IFileSource;
                                        var TargetFileSource: IFileSource;
                                        EnteredPath: String; BaseDir: String;
                                        out DestPath, DestMask: String); overload;
    procedure SetActiveFrame(panel: TFilePanelSelect);
    procedure SetActiveFrame(FileView: TFileView);
    procedure UpdateShellTreeView;
    procedure UpdateTreeViewPath;
    procedure UpdateTreeView;
    procedure UpdateDiskCount;
    procedure UpdateSelectedDrives;
    procedure UpdateGUIFunctionKeys;
    procedure CreateDiskPanel(dskPanel : TKASToolBar);
    procedure SetPanelDrive(aPanel: TFilePanelSelect; Drive: PDrive; ActivateIfNeeded: Boolean);
    function CreateFileView(sType: String; Page: TFileViewPage; AConfig: TXmlConfig; ANode: TXmlNode): TFileView;
    procedure AssignEvents(AFileView: TFileView);
    function RemovePage(ANoteBook: TFileViewNotebook; iPageIndex:Integer; CloseLocked: Boolean = True; ConfirmCloseLocked: integer = 0; ShowButtonAll: Boolean = False): LongInt;
    procedure LoadTabsXml(AConfig: TXmlConfig; ABranch:string; ANoteBook: TFileViewNotebook);
    procedure SaveTabsXml(AConfig: TXmlConfig; ABranch:string; ANoteBook: TFileViewNotebook; ASaveHistory: boolean);
    procedure LoadTheseTabsWithThisConfig(Config: TXmlConfig; ABranch:string; Source, Destination:TTabsConfigLocation; DestinationToKeep : TTabsConfigLocation; var TabsAlreadyDestroyedFlags:TTabsFlagsAlreadyDestroyed);
    procedure ToggleConsole;
    procedure UpdateWindowView;
    procedure MinimizeWindow;
    procedure RestoreWindow;
    procedure LoadTabs;
    procedure LoadTabsCommandLine(Params: TCommandLineParams);
    procedure LoadWindowState;
    procedure SaveWindowState;
    procedure LoadMainToolbar;
    procedure SaveMainToolBar;
    procedure ShowLogWindow(Data: PtrInt);
    function  IsCommandLineVisible: Boolean;
    procedure ShowCommandLine(AFocus: Boolean);
    procedure ConfigSaveSettings(bForce: Boolean);
    procedure ShowDrivesList(APanel: TFilePanelSelect);
    procedure ExecuteCommandLine(bRunInTerm: Boolean);
    procedure UpdatePrompt;
    procedure UpdateFreeSpace(Panel: TFilePanelSelect);
    procedure ReLoadTabs(ANoteBook: TFileViewNotebook);
    procedure ToggleFullscreenConsole;

    {en
       This function is called from various points to handle dropping files
       into the panel. It converts drop effects available on the system
       into TDragDropOperation operations.
       Handles freeing DropParams. }
    procedure DropFiles(var DropParams: TDropParams);

    {en
       Performs all drag&drop actions.
       Frees DropParams.
    }
    procedure DoDragDropOperation(Operation: TDragDropOperation;
                                  var DropParams: TDropParams);


    property Drives: TDrivesList read DrivesList;
    property Commands: TMainCommands read FCommands implements IFormCommands;
    property SelectedPanel: TFilePanelSelect read PanelSelected write SetPanelSelected;
    property LeftTabs: TFileViewNotebook read nbLeft;
    property RightTabs: TFileViewNotebook read nbRight;
    property MainSplitterPos: Double read FMainSplitterPos write SetMainSplitterPos;
  end;

var
  frmMain: TfrmMain;
  Cons: TConsoleThread = nil;

implementation

{$R *.lfm}

uses
  uFileProcs, uShellContextMenu, fTreeViewMenu, uSearchResultFileSource,
  Math, LCLIntf, Dialogs, uGlobs, uLng, uMasks, fCopyMoveDlg, uQuickViewPanel,
  uShowMsg, uDCUtils, uLog, uGlobsPaths, LCLProc, uOSUtils, uPixMapManager, LazUTF8,
  uDragDropEx, uKeyboard, uFileSystemFileSource, fViewOperations, uMultiListFileSource,
  uFileSourceOperationTypes, uFileSourceCopyOperation, uFileSourceMoveOperation,
  uFileSourceProperty, uFileSourceExecuteOperation, uArchiveFileSource, uThumbFileView,
  uShellExecute, fSymLink, fHardLink, uExceptions, uUniqueInstance, Clipbrd, ShellCtrls,
  uFileSourceOperationOptionsUI, uDebug, uHotkeyManager, uFileSourceUtil, uTempFileSystemFileSource,
  Laz2_XMLRead, DCOSUtils, DCStrUtils, fOptions, fOptionsFrame, fOptionsToolbar, uClassesEx,
  uHotDir, uFileSorting, DCBasicTypes, foptionsDirectoryHotlist, uConnectionManager
  {$IFDEF COLUMNSFILEVIEW_VTV}
  , uColumnsFileViewVtv
  {$ELSE}
  , uColumnsFileView
  {$ENDIF}
  ;

const
  HotkeysCategory = 'Main';
  DCToolItemClipboardHeader = 'DOUBLECMD#TOOLBAR#XMLDATA';
  TCToolbarClipboardHeader  = 'TOTALCMD#BAR#DATA';
  DCToolbarClipboardHeader  = 'DOUBLECMD#BAR#DATA';

{$IF DEFINED(LCLGTK2) or DEFINED(LCLQT) or DEFINED(LCLQT5)}
var
  LastActiveWindow: TCustomForm = nil;
{$ENDIF}

{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
var
  CloseQueryResult: Boolean = False;
{$ENDIF}

{$IFDEF LCLGTK2}
var
  MinimizedWindowButton: Boolean = False;
{$ENDIF}

var
  FunctionButtonsCaptions: array[0..7] of record
                             ACaption: String;
                             ACommand: String;
                           end =
     ((ACaption: ''; ACommand: 'cm_View'),
      (ACaption: ''; ACommand: 'cm_Edit'),
      (ACaption: ''; ACommand: 'cm_Copy'),
      (ACaption: ''; ACommand: 'cm_Rename'),
      (ACaption: ''; ACommand: 'cm_MakeDir'),
      (ACaption: ''; ACommand: 'cm_Delete'),
      (ACaption: ''; ACommand: 'cm_RunTerm'),
      (ACaption: ''; ACommand: 'cm_Exit'));

function HistoryIndexesToTag(aFileSourceIndex, aPathIndex: Integer): Longint;
begin
  Result := (aFileSourceIndex << 16) or aPathIndex;
end;

procedure HistoryIndexesFromTag(aTag: Longint; out aFileSourceIndex, aPathIndex: Integer);
begin
  aFileSourceIndex := aTag >> 16;
  aPathIndex := aTag and ((1<<16) - 1);
end;

procedure TfrmMain.FormCreate(Sender: TObject);

  function CreateNotebook(aParent: TWinControl; aSide: TFilePanelSelect): TFileViewNotebook;
  begin
    Result := TFileViewNotebook.Create(aParent, aSide);
    Result.Align := alClient;
    Result.Options := [nboHidePageListPopup];
    {$if lcl_fullversion >= 1070000}
      Result.Options := Result.Options + [nboDoChangeOnSetIndex];
    {$endif}

    Result.OnCloseTabClicked := @NotebookCloseTabClicked;
    Result.OnMouseDown := @nbPageMouseDown;
    Result.OnMouseUp := @nbPageMouseUp;
    Result.OnChange := @nbPageChanged;
    Result.OnDblClick := @pnlLeftRightDblClick;
    Result.OnDragOver:= @NotebookDragOver;
    Result.OnDragDrop:= @NotebookDragDrop;
  end;
  function GenerateTitle():String;
  var 
    ServernameString: String;
  begin
    ServernameString := '';
    if Length(UniqueInstance.ServernameByUser) > 0 then
      ServernameString := ' [' + UniqueInstance.ServernameByUser + ']';

    Result := Format('%s%s %s build %s; %s',
        ['Double Commander',
        ServernameString,
        dcVersion,
        dcRevision,
        dcBuildDate]
    );
  end;

var
  HMMainForm: THMForm;
  I: Integer;
begin
  Application.OnException := @AppException;
  Application.OnActivate := @AppActivate;
  Application.OnDeActivate := @AppDeActivate;
  Application.OnShowHint := @AppShowHint;
  Application.OnEndSession := @AppEndSession;
  Application.OnQueryEndSession := @AppQueryEndSession;

  // Use LCL's method of dropping files from external
  // applications if we don't support it ourselves.
  if not IsExternalDraggingSupported then
    frmMain.OnDropFiles := @FormDropFiles;

  {$IF DEFINED(DARWIN)}
  // MainForm receives in Mac OS closing events on system shortcut Command-Q
  // See details at http://doublecmd.sourceforge.net/mantisbt/view.php?id=712
  Application.MainForm.OnClose := @frmMainClose;
  Application.MainForm.OnCloseQuery := @FormCloseQuery;
  {$ENDIF}

  ConvertToolbarBarConfig(gpCfgDir + 'default.bar');
  CreateDefaultToolbar;

  //Caption of main window
  Self.Caption := GenerateTitle();
  // Remove the initial caption of the button, which is just a text of the associated action.
  // The text would otherwise be briefly shown before the drive button was updated.
  btnLeftDrive.Caption := '';
  btnRightDrive.Caption := '';

  //Have the correct button label to indicate root
  btnLeftRoot.Caption:=DirectorySeparator;
  btnRightRoot.Caption:=DirectorySeparator;

  for I := 0 to pnlKeys.ControlCount - 1 do
    FunctionButtonsCaptions[I].ACaption := pnlKeys.Controls[I].Caption;

  {$IF DEFINED(LCLGTK2)}
  // Workaround: "Layout and line"
  // http://doublecmd.sourceforge.net/mantisbt/view.php?id=573
  TreePanel.Visible := False;
  pnlLeftTools.Visible := False;
  pnlRightTools.Visible := False;
  PanelAllProgress.Visible := False;
  {$ENDIF}

  InitPropStorage(Self);

  PanelSelected:=fpLeft;

  seLogWindow.FixDefaultKeystrokes;

  HMMainForm := HotMan.Register(Self, HotkeysCategory);
  HotMan.Register(edtCommand, 'Command Line');

  nbLeft := CreateNotebook(pnlLeft, fpLeft);
  nbRight := CreateNotebook(pnlRight, fpRight);

  FDrivesListPopup := TDrivesListPopup.Create(Self, Self);
  FDrivesListPopup.OnDriveSelected := @DriveListDriveSelected;
  FDrivesListPopup.OnClose := @DriveListClose;

  //NOTE: we don't check gOnlyOneAppInstance anymore, because cmdline option "--client" was implemented,
  //      so, we should always listen for the messages
  if Assigned(UniqueInstance) then
    UniqueInstance.OnMessage:= @OnUniqueInstanceMessage;

  MainFormCreate(Self);

  // Load command line history
  edtCommand.Items.Assign(glsCmdLineHistory);

  // Initialize actions.
  actShowSysFiles.Checked := uGlobs.gShowSystemFiles;
  actHorizontalFilePanels.Checked := gHorizontalFilePanels;

  AllowDropFiles := not uDragDropEx.IsExternalDraggingSupported;

  MainToolBar.AddToolItemExecutor(TKASCommandItem, @ToolbarExecuteCommand);
  MainToolBar.AddToolItemExecutor(TKASProgramItem, @ToolbarExecuteProgram);

  // Use the same tooltips for some left and right panel butttons.
  btnRightDirectoryHotlist.Hint := btnLeftDirectoryHotlist.Hint;
  btnRightHome.Hint := btnLeftHome.Hint;
  btnRightRoot.Hint := btnLeftRoot.Hint;
  btnRightUp.Hint := btnLeftUp.Hint;

  { *HotKeys* }
  if (HotMan.Forms.Count = 0) or (HotMan.Version < hkVersion) then
    LoadDefaultHotkeyBindings;

  // Register action list for main form hotkeys.
  HMMainForm.RegisterActionList(actionlst);
  { *HotKeys* }

  lastWindowState:= WindowState;

  UpdateActionIcons;

  LoadTabs;

  // Must be after LoadTabs
  TDriveWatcher.Initialize(Handle);
  TDriveWatcher.AddObserver(@OnDriveWatcherEvent);

{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  // Fixes bug - [0000033] "DC cancels shutdown in KDE"
  // http://doublecmd.sourceforge.net/mantisbt/view.php?id=33
  QEventHook:= QObject_hook_create(TQtWidget(Self.Handle).Widget);
  QObject_hook_hook_events(QEventHook, @QObjectEventFilter);
{$ENDIF}

  UpdateWindowView;
  gFavoriteTabsList.AssociatedMainMenuItem := mnuFavoriteTabs;
  gFavoriteTabsList.RefreshAssociatedMainMenu;

  // Update selected drive and free space before main form is shown,
  // otherwise there is a bit of delay.
  UpdateTreeView;
  UpdateTreeViewPath;
  UpdateSelectedDrives;
  UpdateFreeSpace(fpLeft);
  UpdateFreeSpace(fpRight);
end;

procedure TfrmMain.btnLeftClick(Sender: TObject);
begin
  PanelButtonClick(Sender as TSpeedButton, FrameLeft);
end;

procedure TfrmMain.actExecute(Sender: TObject);
var
  cmd: string;
begin
  cmd := (Sender as TAction).Name;
  cmd := 'cm_' + Copy(cmd, 4, Length(cmd) - 3);
  Commands.Commands.ExecuteCommand(cmd, []);
end;

procedure TfrmMain.btnF3MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (ssCtrl in Shift) and (gFonts[dcfFunctionButtons].Size > MIN_FONT_SIZE_FUNCTION_BUTTONS) then
  begin
    Dec(gFonts[dcfFunctionButtons].Size);
    UpdateGUIFunctionKeys;
  end;
end;

procedure TfrmMain.btnF3MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (ssCtrl in Shift) and (gFonts[dcfFunctionButtons].Size < MAX_FONT_SIZE_FUNCTION_BUTTONS) then
  begin
    Inc(gFonts[dcfFunctionButtons].Size);
    UpdateGUIFunctionKeys;
  end;
end;

procedure TfrmMain.btnF8MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Point: TPoint;
begin
  if Button = mbRight then
  begin
    Point := (Sender as TControl).ClientToScreen(Classes.Point(X, Y));
    ShowTrashContextMenu(Self, Point.X, Point.Y, nil);
  end;
end;

procedure TfrmMain.mnuAllOperStopClick(Sender: TObject);
begin
  OperationsManager.StopAll;
end;

procedure TfrmMain.mnuAllOperPauseClick(Sender: TObject);
begin
  OperationsManager.PauseAll;
end;

procedure TfrmMain.mnuAllOperProgressClick(Sender: TObject);
begin
  ShowOperationsViewer;
end;

procedure TfrmMain.mnuAllOperStartClick(Sender: TObject);
begin
  OperationsManager.UnPauseAll;
end;

procedure TfrmMain.btnF8Click(Sender: TObject);
begin
  if GetKeyShiftStateEx * KeyModifiersShortcut = [ssShift] then
    Commands.cm_Delete(['trashcan=reversesetting'])
  else
    Commands.cm_Delete([]);
end;

{ TfrmMain.btnLeftDirectoryHotlistClick}
//To make appear the Directory Hotlist popup menu when pressing "*" button on left
//
procedure TfrmMain.btnLeftDirectoryHotlistClick(Sender: TObject);
var P:TPoint;
begin
  if tb_activate_panel_on_click in gDirTabOptions then SetActiveFrame(fpLeft);
  gDirectoryHotlist.PopulateMenuWithHotDir(pmHotList,@HotDirSelected,@miHotAddOrConfigClick,mpHOTDIRSWITHCONFIG,0);
  p := Classes.Point(btnLeftDirectoryHotlist.Left,btnLeftDirectoryHotlist.Height);
  p := pnlLeftTools.ClientToScreen(p);
  pmHotList.PopUp(P.x,P.y);
end;

procedure TfrmMain.btnRightClick(Sender: TObject);
begin
  PanelButtonClick(Sender as TSpeedButton, FrameRight);
end;

{ TfrmMain.btnRightDirectoryHotlistClick}
//To make appear the Directory Hotlist popup menu when pressing "*" button on right
//
procedure TfrmMain.btnRightDirectoryHotlistClick(Sender: TObject);
var P:TPoint;
begin
  if tb_activate_panel_on_click in gDirTabOptions then SetActiveFrame(fpRight);
  gDirectoryHotlist.PopulateMenuWithHotDir(pmHotList,@HotDirSelected,@miHotAddOrConfigClick,mpHOTDIRSWITHCONFIG,0);
  p := Classes.Point(btnRightDirectoryHotlist.Left,btnRightDirectoryHotlist.Height);
  p := pnlRightTools.ClientToScreen(p);
  pmHotList.PopUp(P.x,P.y);
end;

procedure TfrmMain.btnDriveMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  if Button = mbRight then
      with Sender as TSpeedButton do
      begin
        if (Tag >= 0) and (Tag < DrivesList.Count) then
          begin
            pt.X := X;
            pt.Y := Y;
            pt := ClientToScreen(pt);
            ShowDriveContextMenu(Parent, DrivesList[Tag], pt.X, pt.Y, nil);
          end;
      end;
end;

procedure TfrmMain.ConsoleSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  // ConsoleSplitter is trying to resize pnlCommand,
  // so NewSize is the new size of pnlCommand.
  // Instead, resize nbConsole by the same difference.
  nbConsole.Height := nbConsole.Height + NewSize - pnlCommand.Height;
end;

procedure TfrmMain.ConvertToolbarBarConfig(BarFileName: String);
var
  ToolBarLoader: TKASToolBarIniLoader;
  MainToolBarNode: TXmlNode;
begin
  MainToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', False);
  if not Assigned(MainToolBarNode) then
  begin
    if mbFileExists(BarFileName) then
    begin
      ToolBarLoader := TKASToolBarIniLoader.Create(Commands.Commands);
      try
        ToolBarLoader.Load(BarFileName, MainToolBar, nil, @ConvertIniToolbarItem);
        SaveMainToolBar;
        SaveGlobs; // Save toolbar and hotkeys
        mbRenameFile(BarFileName, BarFileName + '.obsolete');
      finally
        ToolBarLoader.Free;
      end;
    end;
  end;
end;

procedure TfrmMain.dskLeftResize(Sender: TObject);
begin
  pnlDskLeft.ClientHeight := dskLeft.Height + pnlDskLeft.BevelWidth * 2;
  pnlDiskLeftInner.ClientHeight := dskLeft.Height + pnlDiskLeftInner.BevelWidth * 2;
end;

procedure TfrmMain.dskRightResize(Sender: TObject);
begin
  pnlDskRight.ClientHeight := dskRight.Height + pnlDskRight.BevelWidth * 2;
  pnlDiskRightInner.ClientHeight := dskRight.Height + pnlDiskRightInner.BevelWidth * 2;
end;

procedure TfrmMain.dskRightToolButtonClick(Sender: TObject);
var
  FileView : TFileView;
begin
  if gDriveBar2 then
    FileView := FrameRight
  else
    FileView := ActiveFrame;
  PanelButtonClick(Sender as TKASToolButton, FileView);
end;

procedure TfrmMain.dskLeftRightToolButtonDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  ToolItem: TKASToolItem;
  SourceFiles: TFiles;
  TargetFileSource: IFileSource;
  TargetPath: String;
begin
  if Sender is TKASToolButton then
  begin
    SourceFiles := ActiveFrame.CloneSelectedOrActiveFiles;
    try
      ToolItem := TKASToolButton(Sender).ToolItem;
      if ToolItem is TKASDriveItem then
      begin
        TargetPath := TKASDriveItem(ToolItem).Drive^.Path;
        TargetFileSource := ParseFileSource(TargetPath, ActiveFrame.FileSource);
        TargetPath := IncludeTrailingPathDelimiter(TargetPath);
        if not Assigned(TargetFileSource) then
          TargetFileSource := TFileSystemFileSource.GetFileSource;
        case GetDropEffectByKeyAndMouse(GetKeyShiftState, mbLeft) of
          DropCopyEffect:
            Self.CopyFiles(ActiveFrame.FileSource, TargetFileSource, SourceFiles, TargetPath, gShowDialogOnDragDrop);
          DropMoveEffect:
            Self.MoveFiles(ActiveFrame.FileSource, TargetFileSource, SourceFiles, TargetPath, gShowDialogOnDragDrop);
        end;
      end;
    finally
      SourceFiles.Free;
    end;
  end;
end;

procedure TfrmMain.dskToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  btnDriveMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TfrmMain.EditToolbarButton(Button: TKASToolButton);
var
  Editor: TOptionsEditor;
  Options: IOptionsDialog;
begin
  Options := ShowOptions(TfrmOptionsToolbar);
  Application.ProcessMessages;
  Editor := Options.GetEditor(TfrmOptionsToolbar);
  if Assigned(Button) then
  begin
    (Editor as TfrmOptionsToolbar).SelectButton(Button.Tag);
  end;
  Application.ProcessMessages;
  if Editor.CanFocus then  Editor.SetFocus;
end;

procedure TfrmMain.lblAllProgressPctClick(Sender: TObject);
begin
  ShowOperationsViewer;
end;

procedure TfrmMain.MainToolBarToolButtonDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  I: LongWord;
  SelectedFiles: TFiles = nil;
  Param: string;
  ToolItem: TKASToolItem;
begin
  if (ssShift in GetKeyShiftState) then
    // Button was moved.
    SaveMainToolBar
  else
    if Sender is TKASToolButton and not Draging then
      begin
        ToolItem := TKASToolButton(Sender).ToolItem;
        if ToolItem is TKASProgramItem then
        begin
          SelectedFiles := ActiveFrame.CloneSelectedOrActiveFiles;
          try
            if SelectedFiles.Count > 0 then
              begin
                Param:= EmptyStr;
                for I := 0 to SelectedFiles.Count - 1 do
                begin
                  // Workaround for not fully implemented TMultiListFileSource.
                  if ActiveFrame.FileSource.IsClass(TMultiListFileSource) then
                    Param := Param + QuoteStr(SelectedFiles[I].FullPath) + ' '
                  else
                    Param := Param + QuoteStr(ActiveFrame.CurrentAddress + SelectedFiles[I].FullPath) + ' ';
                end;

                TKASProgramItem(ToolItem).Command   := ReplaceEnvVars(ReplaceTilde(TKASProgramItem(ToolItem).Command));
                Param := PrepareParameter(Param, nil, []);

                if not (Commands.Commands.ExecuteCommand(TKASProgramItem(ToolItem).Command, [Param]) = cfrSuccess) then
                  ProcessExtCommandFork(TKASProgramItem(ToolItem).Command, Param, TKASProgramItem(ToolItem).StartPath);
              end;
          finally
            FreeAndNil(SelectedFiles);
          end;
        end;
      end;
end;

procedure TfrmMain.MainToolBarToolButtonDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean; NumberOfButton: Integer);
var
  aFile: TFile;
  tmp: Integer;
  ToolItem: TKASProgramItem;
begin
  if (ssShift in GetKeyShiftState) then
    begin
      if not (Source is TKASToolButton) and not Draging then
        begin
          aFile := ActiveFrame.CloneActiveFile;
          try
            if Assigned(aFile) and aFile.IsNameValid then
              begin
                ToolItem := TKASProgramItem.Create;
                ToolItem.Command := aFile.FullPath;
                ToolItem.StartPath := aFile.Path;
                ToolItem.Icon := aFile.FullPath;
                ToolItem.Hint := ExtractOnlyFileName(aFile.Name);
                // ToolItem.Text := ExtractOnlyFileName(aFile.Name);
                MainToolBar.InsertButton(Sender as TKASToolButton, ToolItem);

                NumberOfMoveButton := (Sender as TSpeedButton).Tag;
                NumberOfNewMoveButton := (Sender as TSpeedButton).Tag-1;
                Draging := True;
                Accept := True;
              end
            else
              begin
                Accept := False;
                Exit;
              end;
          finally
            FreeAndNil(aFile);
          end;
        end;
      if (NumberOfMoveButton <> (Sender as TSpeedButton).Tag) then
        begin
          Draging := True;
          if Source is TSpeedButton then
            MainToolBar.MoveButton((Source as TSpeedButton).Tag, (Sender as TSpeedButton).Tag)
          else
            begin
              tmp:= (Sender as TSpeedButton).Tag;
              MainToolBar.MoveButton(NumberOfNewMoveButton, (Sender as TSpeedButton).Tag);
              NumberOfNewMoveButton := tmp;
            end;
          NumberOfMoveButton := (Sender as TSpeedButton).Tag;
          Accept := True;
        end;
    end
  else
    begin
      Accept := not Draging and
                (Sender is TKASToolButton) and
                (TKASToolButton(Sender).ToolItem is TKASProgramItem);
      if Accept then
      begin
        aFile := ActiveFrame.CloneActiveFile;
        try
          Accept := Assigned(aFile) and aFile.IsNameValid;
        finally
          FreeAndNil(aFile);
        end;
      end;
    end;
end;

procedure TfrmMain.MainToolBarToolButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssShift in Shift) and (Button = mbLeft) then
    begin
      (Sender as TKASToolButton).BeginDrag(False, 5);
      NumberOfMoveButton:= (Sender as TKASToolButton).Tag;
    end;
  Draging:= False;
end;

procedure TfrmMain.MainToolBarToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    MainToolBarMouseUp(Sender, Button, Shift, X, Y);
end;

function TfrmMain.MainToolBarToolItemShortcutsHint(ToolItem: TKASNormalItem): String;
begin
  Result := ShortcutsToText(TfrmOptionsToolbar.GetShortcuts(ToolItem));
end;

procedure TfrmMain.miLogMenuClick(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    0:
      seLogWindow.CopyToClipboard;
    1:
      seLogWindow.SelectAll;
    2:
      Commands.cm_ClearLogWindow([]);
    3:
      ShowLogWindow(PtrInt(False));
  end;
end;

procedure TfrmMain.miTrayIconExitClick(Sender: TObject);
begin
  RestoreFromTray;
  Close;
end;

procedure TfrmMain.miTrayIconRestoreClick(Sender: TObject);
begin
  RestoreFromTray;
end;

procedure TfrmMain.PanelButtonClick(Button: TSpeedButton; FileView: TFileView);
begin
  with FileView do
  begin
    if Button.Caption = DirectorySeparator then
      Commands.DoChangeDirToRoot(FileView)
    else if Button.Caption = '..' then
      ChangePathToParent(True)
    else if Button.Caption = '~' then
      SetFileSystemPath(FileView, GetHomeDir);
  end;

  if tb_activate_panel_on_click in gDirTabOptions then
    SetActiveFrame(FileView);
end;

procedure TfrmMain.ShellTreeViewSelect;
begin
  ShellTreeView.Tag := 1;
  try
    SetFileSystemPath(ActiveFrame, (ShellTreeView as TShellTreeView).Path);
  finally
    ShellTreeView.Tag := 0;
  end;
end;

procedure TfrmMain.ShellTreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then ShellTreeViewSelect;
end;

procedure TfrmMain.ShellTreeViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AFile: TFile;
  AFiles: TFiles;
  APoint: TPoint;
  AFileName: String;
begin
{$IF DEFINED(MSWINDOWS)}
  if Button = mbRight then
  try
    AFileName:= ExcludeTrailingBackslash((ShellTreeView as TShellTreeView).Path);
    AFile:= TFileSystemFileSource.CreateFileFromFile(AFileName);
    try
      AFiles:= TFiles.Create(AFile.Path);
      AFiles.Add(AFile);
      APoint := ShellTreeView.ClientToScreen(Classes.Point(X, Y));
      ShowContextMenu(ShellTreeView, AFiles, APoint.X, APoint.Y, False, nil);
    finally
      FreeAndNil(AFiles);
    end;
  except
    on E: EContextMenuException do
      ShowException(E)
    else;
  end;
{$ENDIF}
  if Button = mbLeft then
    ShellTreeViewSelect;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DCDebug('Destroying main form');

  if Assigned(HotMan) then
  begin
    HotMan.UnRegister(edtCommand);
    HotMan.UnRegister(Self);
  end;

  TDriveWatcher.RemoveObserver(@OnDriveWatcherEvent);
  TDriveWatcher.Finalize;
  DCDebug('Drive watcher finished');

  // Close all tabs.
  CloseNotebook(LeftTabs);
  CloseNotebook(RightTabs);

  FreeAndNil(DrivesList);

{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  QObject_hook_destroy(QEventHook);
{$ENDIF}

  DCDebug('Main form destroyed');
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if OperationsManager.OperationsCount > 0 then
  begin
    CanClose := MessageDlg(rsMsgFileOperationsActive,
      rsMsgFileOperationsActiveLong + LineEnding + rsMsgConfirmQuit,
      mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes;
  end
  else
    CanClose := True;
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  CloseQueryResult:= CanClose;
{$ENDIF}
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  TargetFileView: TFileView = nil;
  TargetControl: TControl;
  I: Integer;
  Files: TFiles = nil;
  FileNamesList: TStringList = nil;
  Point: TPoint;
  DropParams: TDropParams;
begin
  Point.x := 0;
  Point.y := 0;
  TargetControl := FindLCLControl(Mouse.CursorPos);
  while TargetControl <> nil do
  begin
    if TargetControl = FrameLeft then
    begin
      // drop on left panel
      TargetFileView := FrameLeft;
      break;
    end
    else if TargetControl = FrameRight then
    begin
      // drop on right panel
      TargetFileView := FrameRight;
      break;
    end;

    TargetControl := TargetControl.Parent;
  end;

  if Assigned(TargetFileView) then
  try
    // fill file list by files
    FileNamesList := TStringList.Create;
    for I := Low(FileNames) to High(FileNames) do
    begin
      if Length(FileNames[I]) > 0 then
        FileNamesList.Add(FileNames[I]);
    end;

    if FileNamesList.Count > 0 then
    try
      Files := TFileSystemFileSource.CreateFilesFromFileList(
          ExtractFilePath(FileNamesList[0]), FileNamesList);

      if Files.Count > 0 then
      begin
        GetCursorPos(Point);

        DropParams := TDropParams.Create(
            Files,
            GetDropEffectByKeyAndMouse(GetKeyShiftState, mbLeft),
            Point, False,
            nil, TargetFileView,
            TargetFileView.FileSource,
            TargetFileView.CurrentPath);

        DropFiles(DropParams);
      end;

    except
      on e: EFileNotFound do
        MessageDlg(e.Message, mtError, [mbOK], 0);
    end;

  finally
    FreeAndNil(Files);
    FreeAndNil(FileNamesList);
  end;
end;

procedure TfrmMain.DropFiles(var DropParams: TDropParams);
begin
  if Assigned(DropParams) then
  begin
    if DropParams.Files.Count > 0 then
    begin
      case DropParams.DropEffect of

        DropMoveEffect:
          DropParams.TargetPanel.DoDragDropOperation(ddoMove, DropParams);

        DropCopyEffect:
          DropParams.TargetPanel.DoDragDropOperation(ddoCopy, DropParams);

        DropLinkEffect:
          DropParams.TargetPanel.DoDragDropOperation(ddoSymLink, DropParams);

        DropAskEffect:
          begin
            // Ask the user what he would like to do by displaying a menu.
            // Returns immediately after showing menu.
            PopupDragDropMenu(DropParams);
          end;

        else
          FreeAndNil(DropParams);

      end;
    end
    else
      FreeAndNil(DropParams);
  end;
end;

procedure TfrmMain.DoDragDropOperation(Operation: TDragDropOperation;
                                       var DropParams: TDropParams);
var
  SourceFileName, TargetFileName: string;
begin
  try
    with DropParams do
    begin
      if Assigned(TargetFileSource) then
      begin

{$IF DEFINED(MSWINDOWS)}
        // If drop from external application and from temporary directory then
        // in most cases it is a drop from archiver application that extracting
        // files via temporary directory and requires run operation in the main thread
        // See http://doublecmd.sourceforge.net/mantisbt/view.php?id=1124
        if (GetDragDropType = ddtExternal) and (Operation in [ddoMove, ddoCopy]) and
           IsInPath(GetTempDir, DropParams.Files[0].FullPath, True, True) then
        begin
          if gShowDialogOnDragDrop then
          begin
            case Operation of
              ddoMove: SourceFileName := GetFileDlgStr(rsMsgRenSel, rsMsgRenFlDr, DropParams.Files);
              ddoCopy: SourceFileName := GetFileDlgStr(rsMsgCpSel, rsMsgCpFlDr, DropParams.Files);
            end;
            if MessageDlg(SourceFileName, mtConfirmation, [mbOK, mbCancel], 0) <> mrOK then
              Exit;
          end;
          case Operation of
            ddoMove: Self.MoveFiles(TFileSystemFileSource.GetFileSource,
                           TargetFileSource, Files, TargetPath, False, ModalQueueId);
            ddoCopy: Self.CopyFiles(TFileSystemFileSource.GetFileSource,
                           TargetFileSource, Files, TargetPath, False, ModalQueueId);
          end;
        end else
{$ENDIF}

        case Operation of

          ddoMove:
            if GetDragDropType = ddtInternal then
            begin
              if Self.MoveFiles(SourcePanel.FileSource,
                                TargetFileSource,
                                Files, TargetPath,
                                gShowDialogOnDragDrop) then
              begin
                SourcePanel.MarkFiles(False);
              end;
            end
            else
            begin
              Self.MoveFiles(TFileSystemFileSource.GetFileSource,
                             TargetFileSource,
                             Files, TargetPath,
                             gShowDialogOnDragDrop);
            end;

          ddoCopy:
            if GetDragDropType = ddtInternal then
            begin
              if Self.CopyFiles(SourcePanel.FileSource,
                                TargetFileSource,
                                Files, TargetPath,
                                gShowDialogOnDragDrop) then
              begin
                SourcePanel.MarkFiles(False);
              end;
            end
            else
            begin
              Self.CopyFiles(TFileSystemFileSource.GetFileSource,
                             TargetFileSource,
                             Files, TargetPath,
                             gShowDialogOnDragDrop);
            end;

          ddoSymLink, ddoHardLink:
            begin
              // Only for filesystem.
              if  ((GetDragDropType = ddtExternal) or
                  (SourcePanel.FileSource.IsClass(TFileSystemFileSource)))
              and (TargetFileSource.IsClass(TFileSystemFileSource)) then
              begin
                // TODO: process multiple files

                SourceFileName := Files.Items[0].FullPath;
                TargetFileName := TargetPath + ExtractFileName(SourceFileName);

                if ((Operation = ddoSymLink) and
                   ShowSymLinkForm(SourceFileName, TargetFileName, TargetPath))
                or ((Operation = ddoHardLink) and
                   ShowHardLinkForm(SourceFileName, TargetFileName, TargetPath))
                then
                  TargetFileSource.Reload(TargetPath);
              end
              else
              begin
                msgWarning(rsMsgErrNotSupported);
              end;
            end;
        end;
      end
      else
        msgWarning(rsMsgErrNotSupported);
    end;

  finally
    FreeAndNil(DropParams);
  end;
end;

procedure TfrmMain.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  // Either left or right panel has to be focused.
  if not FrameLeft.Focused and
     not FrameRight.Focused then
  begin
    Exit;
  end;

  // Check for certain Ascii keys.
  if (not ((Length(UTF8Key) = 1) and (UTF8Key[1] in ['-', '*', '+', #0..#32]))) then
  begin
    if (gKeyTyping[ktmNone] = ktaCommandLine)
{$IFDEF MSWINDOWS}
       // Allow entering international characters with Ctrl+Alt on Windows,
       // if there is no action for Ctrl+Alt and command line typing has no modifiers.
       or (HasKeyboardAltGrKey and
           (GetKeyShiftStateEx * KeyModifiersShortcutNoText = [ssCtrl, ssAlt]) and
           (gKeyTyping[ktmCtrlAlt] = ktaNone))
{$ENDIF}
      then
      begin
        TypeInCommandLine(UTF8Key);
        UTF8Key := '';
      end;
  end
end;

{$IF DEFINED(LCLGTK2)}
procedure TfrmMain.WindowStateUpdate(Data: PtrInt);
begin
  Resizing(lastWindowState);
end;
{$ENDIF}

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  if FUpdateDiskCount and (WindowState <> wsMinimized) then
  begin
    UpdateDiskCount;
    FUpdateDiskCount:= False;
  end;

{$IF DEFINED(LCLGTK2)}
  if MinimizedWindowButton then
  begin
    MinimizedWindowButton:= False;
    Application.QueueAsyncCall(@WindowStateUpdate, 0);
    Exit;
  end;
{$ENDIF}

  if WindowState = wsMinimized then
  begin  // Minimized
    MainToolBar.Top:= 0; // restore toolbar position
    if not HiddenToTray then
    begin
{$IF DEFINED(LCLGTK2)}
      MinimizedWindowButton:= True;
{$ENDIF}
      if gMinimizeToTray or gAlwaysShowTrayIcon then
      begin
        HideToTray;
      end;
    end
    else begin
      // If we get wsMinimized while HiddenToTray is true,
      // then this means it was sent by LCL when a hidden, minimized window was shown.
      // We don't react to this message in this case.
      HiddenToTray := False;
{$IF DEFINED(LCLGTK2)}
      Application.QueueAsyncCall(@WindowStateUpdate, 0);
{$ENDIF}
    end;
  end
  else
  begin  // Not minimized
    // save window state before minimize for
    // future loading after restore from tray
    lastWindowState:=WindowState;
    HiddenToTray := False;
  end;
end;

procedure TfrmMain.MainSplitterDblClick(Sender: TObject);
begin
  // To prevent MainSplitterMouseUp processing
  MainSplitterLeftMouseBtnDown:=false;
  MainSplitter.ParentColor:=true;
  // Set splitter to 50/50
  Commands.DoPanelsSplitterPerPos(50);
end;

procedure TfrmMain.MainSplitterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and (not MainSplitterLeftMouseBtnDown) then
  begin
   // Under Linux MainSplitter.Color:=clBlack Doesn't work
   MainSplitter.ParentColor:=true;
   MainSplitter.Color:=ColorToRGB(clBlack);

   MainSplitterMouseDownX:=X;
   MainSplitterMouseDownY:=Y;
   MainSplitterLeftMouseBtnDown:=true;
  end;
end;

procedure TfrmMain.MainSplitterMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  APoint: TPoint;
  Rect: TRect;
  sHint: String;
  Moved: Boolean = False;
begin
  if MainSplitterLeftMouseBtnDown then
  begin
    if not gHorizontalFilePanels and
       (MainSplitter.Left + X > MainSplitter.Width) and
       (MainSplitter.Left + X + MainSplitter.Width < pnlNotebooks.Width) then
    begin
      MainSplitter.Left := MainSplitter.Left + X - MainSplitterMouseDownX;
      Moved := True;
    end
    else if gHorizontalFilePanels and
       (MainSplitter.Top + Y > MainSplitter.Height) and
       (MainSplitter.Top + Y + MainSplitter.Height < pnlNotebooks.Height) then
    begin
      MainSplitter.Top := MainSplitter.Top + Y - MainSplitterMouseDownY;
      Moved := True;
    end;

    if Moved then
    begin
      // create hint
      if not Assigned(MainSplitterHintWnd) then
      begin
        MainSplitterHintWnd := THintWindow.Create(nil);
        MainSplitterHintWnd.Color := Application.HintColor;
      end;

      // calculate percent
      if not gHorizontalFilePanels then
        FMainSplitterPos:= MainSplitter.Left * 100 / (pnlNotebooks.Width-MainSplitter.Width)
      else
        FMainSplitterPos:= MainSplitter.Top * 100 / (pnlNotebooks.Height-MainSplitter.Height);

      // generate hint text
      sHint:= FloatToStrF(FMainSplitterPos, ffFixed, 15, 1) + '%';

      // calculate hint position
      Rect:= MainSplitterHintWnd.CalcHintRect(200, sHint, nil);
      APoint:= Mouse.CursorPos;
      with Rect do
      begin
        Right:= APoint.X + 8 + Right;
        Bottom:= APoint.Y + 12 + Bottom;
        Left:= APoint.X + 8;
        Top:= APoint.Y + 12;
      end;

      // show hint
      MainSplitterHintWnd.ActivateHint(Rect, sHint);
    end;
  end;
end;

procedure TfrmMain.MainSplitterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // hide and destroy hint
  if Assigned(MainSplitterHintWnd) then
  begin
    MainSplitterHintWnd.Hide;
    FreeAndNil(MainSplitterHintWnd);
  end;

  if (MainSplitterLeftMouseBtnDown) then
  begin
    MainSplitter.ParentColor:=true;
    MainSplitterLeftMouseBtnDown:=false;

    if not FResizingFilePanels then
    begin
      FResizingFilePanels := True;
      if not gHorizontalFilePanels then
        pnlLeft.Width := MainSplitter.Left
      else
        pnlLeft.Height := MainSplitter.Top;
      FResizingFilePanels := False;
    end;
  end;
end;

procedure TfrmMain.MainTrayIconClick(Sender: TObject);
begin
  // Only react to clicks if the icon is not scheduled to be hidden.
  if not HidingTrayIcon then
  begin
    if WindowState = wsMinimized then
    begin
      RestoreFromTray;
    end
    else
    begin
      MinimizeWindow;
      HideToTray;
    end;
  end;
end;

{ TfrmMain.lblDriveInfoDblClick }
//Shows the Directory Hotlist at the cursor position after double-clicking the panel.
//This is NOT like TC but was here as legacy in DC. Let it there for respect to original authors.
//Double-clicking on the "FPathLabel" of the "TFileViewHeader" does the same as in TC and is implemented now.
//
procedure TfrmMain.lblDriveInfoDblClick(Sender: TObject);
begin
  if tb_activate_panel_on_click in gDirTabOptions then
  begin
    if Sender = lblRightDriveInfo then
      SetActiveFrame(fpRight)
    else if Sender = lblLeftDriveInfo then
      SetActiveFrame(fpLeft);
  end;
  Commands.cm_DirHotList(['position=cursor']);
end;

procedure TfrmMain.LeftDriveBarExecuteDrive(ToolItem: TKASToolItem);
var
  DriveItem: TKASDriveItem;
begin
  DriveItem := ToolItem as TKASDriveItem;
  SetPanelDrive(fpLeft, DriveItem.Drive, True);
end;

procedure TfrmMain.MainToolBarDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aFile: TFile;
  ToolItem: TKASProgramItem;
begin
  if not (Source is TSpeedButton) and not Draging and (ssShift in GetKeyShiftState) then
    begin
      aFile := ActiveFrame.CloneActiveFile;
      try
        if Assigned(aFile) and aFile.IsNameValid then
        begin
          ToolItem := TKASProgramItem.Create;
          ToolItem.Command := GetToolbarFilenameToSave(tpmeCommand, aFile.FullPath);
          ToolItem.StartPath := GetToolbarFilenameToSave(tpmeStartingPath, aFile.Path);
          ToolItem.Hint := ExtractOnlyFileName(aFile.Name);
          // ToolItem.Text := ExtractOnlyFileName(aFile.Name);
          ToolItem.Icon := GetToolbarFilenameToSave(tpmeIcon, aFile.FullPath);
          MainToolBar.AddButton(ToolItem);
        end;
      finally
        FreeAndNil(aFile);
      end;
    end;
  SaveMainToolBar;
  Draging := False;
end;

procedure TfrmMain.MainToolBarDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  aFile: TFile;
begin
  if (ssShift in GetKeyShiftState) and not (Source is TSpeedButton) then
    begin
      aFile := ActiveFrame.CloneActiveFile;
      try
        Accept := Assigned(aFile) and aFile.IsNameValid;
      finally
        FreeAndNil(aFile);
      end;
    end
  else  Accept := false;
end;

function TfrmMain.MainToolBarLoadButtonGlyph(ToolItem: TKASToolItem;
  iIconSize: Integer; clBackColor: TColor): TBitmap;
begin
  if ToolItem is TKASNormalItem then
    Result := PixMapManager.LoadBitmapEnhanced(TKASNormalItem(ToolItem).Icon, iIconSize, True, clBackColor, nil)
  else
    Result := nil;
end;

function TfrmMain.MainToolBarLoadButtonOverlay(ToolItem: TKASToolItem;
  iIconSize: Integer; clBackColor: TColor): TBitmap;
begin
  if ToolItem is TKASMenuItem then
    Result := PixMapManager.LoadBitmapEnhanced('emblem-symbolic-link', iIconSize, True, clBackColor, nil)
  else
    Result := nil;
end;

procedure TfrmMain.tbDeleteClick(Sender: TObject);
var
  Button: TKASToolButton;
begin
  Button := TKASToolButton(pmToolBar.Tag);
  if Assigned(Button) then
  begin
    if msgYesNo(Format(rsMsgDelSel, [Button.Hint])) then
    begin
       MainToolBar.RemoveButton(Button);
       SaveMainToolBar;
    end;
  end;
end;

procedure TfrmMain.dskLeftToolButtonClick(Sender: TObject);
begin
  PanelButtonClick(Sender as TKASToolButton, FrameLeft);
end;

procedure TfrmMain.btnVirtualDriveClick(Sender: TObject);
var
  TargetPanel: TFileView;
begin
  if gDriveBar1 and gDriveBar2 then
  begin
    if (Sender as TSpeedButton).Parent = dskLeft then
      TargetPanel := FrameLeft
    else
      TargetPanel := FrameRight;
  end
  else
    TargetPanel := ActiveFrame;
  Commands.DoOpenVirtualFileSystemList(TargetPanel);
end;

procedure TfrmMain.MainToolBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Point : TPoint;
begin
  case Button of
    mbRight:
      begin
        Point.X := X;
        Point.Y := Y;
        Point := (Sender as TControl).ClientToScreen(Point);
        if Sender is TKASToolButton then
          pmToolBar.Tag := PtrInt(Sender)
        else
          pmToolBar.Tag := 0;
        pmToolBar.PopUp(Point.X, Point.Y);
      end;
  end;
end;

procedure TfrmMain.frmMainClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Process all queued asynchronous events before closing
  // (frmMainAfterShow, nbPageAfterMouseDown, etc.).
  Application.ProcessMessages;

  if tb_close_duplicate_when_closing in gDirTabOptions then
  begin
    Commands.cm_CloseDuplicateTabs(['LeftTabs']);
    Commands.cm_CloseDuplicateTabs(['RightTabs']);
  end;

  if gSaveConfiguration then ConfigSaveSettings(False);

  FreeAndNil(Cons);

  Application.Terminate;
end;

procedure TfrmMain.frmMainAfterShow(Sender: TObject);
begin
  OnPaint := nil;
  if Assigned(ActiveFrame) then
    ActiveFrame.SetFocus
  else begin
    DCDebug('ActiveFrame = nil');
  end;
  HiddenToTray := False;
end;

procedure TfrmMain.frmMainShow(Sender: TObject);
begin
  DCDebug('frmMain.frmMainShow');
{$IF NOT (DEFINED(LCLWIN32) or DEFINED(LCLGTK2) or (DEFINED(DARWIN) and DEFINED(LCLQT)))}
  OnPaint := @frmMainAfterShow;
{$ELSE}
  Application.QueueAsyncCall(TDataEvent(@frmMainAfterShow), 0);
{$ENDIF}
end;

procedure TfrmMain.mnuDropClick(Sender: TObject);
var
  DropParamsRef: TDropParams;
begin
  if (Sender is TMenuItem) and Assigned(FDropParams) then
    begin
      // Make a copy of the reference to parameters and clear FDropParams,
      // so that they're not destroyed if pmDropMenuClose is called while we're processing.
      DropParamsRef := FDropParams;
      FDropParams := nil; // release ownership

      with DropParamsRef do
      begin
        if (Sender as TMenuItem).Name = 'miMove' then
          begin
            TargetPanel.DoDragDropOperation(ddoMove, DropParamsRef);
          end
        else if (Sender as TMenuItem).Name = 'miCopy' then
          begin
            TargetPanel.DoDragDropOperation(ddoCopy, DropParamsRef);
          end
        else if (Sender as TMenuItem).Name = 'miSymLink' then
          begin
            TargetPanel.DoDragDropOperation(ddoSymLink, DropParamsRef);
          end
        else if (Sender as TMenuItem).Name = 'miHardLink' then
          begin
            TargetPanel.DoDragDropOperation(ddoHardLink, DropParamsRef);
          end
        else if (Sender as TMenuItem).Name = 'miCancel' then
          begin
            FreeAndNil(DropParamsRef);
          end;
      end; //with
    end;
end;

procedure TfrmMain.PopupDragDropMenu(var DropParams: TDropParams);
begin
  // Disposing of the params is handled in pmDropMenuClose or mnuDropClick.
  if Assigned(DropParams) then
  begin
    FDropParams := DropParams;
    DropParams := nil;
    pmDropMenu.PopUp(FDropParams.ScreenDropPoint.X,
                     FDropParams.ScreenDropPoint.Y);
  end;
end;

procedure TfrmMain.pmDropMenuClose(Sender: TObject);
begin
  // Free drop parameters given to drop menu.
  FreeAndNil(FDropParams);
end;

procedure TfrmMain.mnuSplitterPercentClick(Sender: TObject);
begin
  with (Sender as TMenuItem) do
  begin
    Commands.DoPanelsSplitterPerPos(Tag);
  end;
end;

procedure TfrmMain.mnuTabMenuExecute(Sender: TObject);
begin
  (Sender as TAction).OnExecute:= @actExecute;
end;

procedure TfrmMain.mnuTabMenuClick(Sender: TObject);
var
  Cmd: String;
  MenuItem: TMenuItem;
  NoteBook: TFileViewNotebook;
begin
  MenuItem := (Sender as TMenuItem);
  NoteBook := (pmTabMenu.Parent as TFileViewNotebook);

  // pmTabMenu.Tag stores tab page nr where the menu was activated.

  if MenuItem = miCloseTab then
    Commands.DoCloseTab(NoteBook, pmTabMenu.Tag)
  else if MenuItem = miRenameTab then
    Commands.DoRenameTab(NoteBook.Page[pmTabMenu.Tag])
  else if MenuItem = miTabOptionNormal then
    NoteBook.Page[pmTabMenu.Tag].LockState := tlsNormal
  else if MenuItem = miTabOptionPathLocked then
    NoteBook.Page[pmTabMenu.Tag].LockState := tlsPathLocked
  else if MenuItem = miTabOptionPathResets then
    NoteBook.Page[pmTabMenu.Tag].LockState := tlsPathResets
  else if MenuItem = miTabOptionDirsInNewTab then
    NoteBook.Page[pmTabMenu.Tag].LockState := tlsDirsInNewTab
  else
  begin
    Cmd:= MenuItem.Action.Name;
    Cmd:= 'cm_' + Copy(Cmd, 4, Length(Cmd) - 3);

    Commands.Commands.ExecuteCommand(Cmd, NoteBook.Name);
  end;
  // On click first the OnClick and then the Action.OnExecute is called
  MenuItem.Action.OnExecute:= @mnuTabMenuExecute;
end;

procedure TfrmMain.nbPageAfterMouseDown(Data: PtrInt);
var
  Notebook: TFileViewNotebook;
begin
  if TObject(Data) is TFileViewNotebook then
  begin
    Notebook := TObject(Data) as TFileViewNotebook;

    if (Notebook = nbLeft) and (FrameLeft <> nil) then
      begin
        if PanelSelected = fpLeft then // same panel
          FrameLeft.SetFocus
        else if (tb_activate_panel_on_click in gDirTabOptions) then
          SetActiveFrame(fpLeft)
        else
          FrameRight.SetFocus;
      end;
    if (Notebook = nbRight) and (FrameRight <> nil) then
      begin
        if PanelSelected = fpRight then // same panel
          FrameRight.SetFocus
        else if (tb_activate_panel_on_click in gDirTabOptions) then
          SetActiveFrame(fpRight)
        else
          FrameLeft.SetFocus;
      end;
  end;
end;

procedure TfrmMain.nbPageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Application.QueueAsyncCall(@nbPageAfterMouseDown, PtrInt(Sender));
end;

procedure TfrmMain.nbPageChanged(Sender: TObject);
var
  Notebook: TFileViewNotebook;
  Page: TFileViewPage;
begin
  Notebook := Sender as TFileViewNotebook;
  Page := Notebook.ActivePage;
  if Assigned(Page) then
  begin
    if Page.LockState = tlsPathResets then // if locked with directory change
      ChooseFileSource(Page.FileView, Page.LockPath);

    // Update selected drive only on non-active panel,
    // because active panel is updated on focus change.
    if (PanelSelected <> Notebook.Side) and
       not (tb_activate_panel_on_click in gDirTabOptions) then
    begin
      UpdateSelectedDrive(Notebook);
      UpdateFreeSpace(Notebook.Side);
    end;
  end;

  if Assigned(QuickViewPanel) then
    Commands.cm_QuickView(['Close']);

  UpdatePrompt;
  UpdateTreeViewPath;
end;

procedure TfrmMain.nbPageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  PopUpPoint: TPoint;
  NoteBook: TFileViewNotebook;
  TabNr: Integer;
begin
  NoteBook := Sender as TFileViewNotebook;

  case Button of

    mbMiddle:
      begin
        TabNr := NoteBook.IndexOfPageAt(Point(X, Y));
        if TabNr <> -1 then
        begin
          Commands.DoCloseTab(NoteBook, TabNr);
        end;
      end;

    mbRight:
      begin
        TabNr := NoteBook.IndexOfPageAt(Point(X, Y));
        if TabNr <> -1 then
        begin
          PopUpPoint := NoteBook.ClientToScreen(Point(X, Y));

          // Check tab options items.
          case NoteBook.Page[TabNr].LockState of
            tlsNormal:
              miTabOptionNormal.Checked := True;
            tlsPathLocked:
              miTabOptionPathLocked.Checked := True;
            tlsDirsInNewTab:
              miTabOptionDirsInNewTab.Checked := True;
            tlsPathResets:
              miTabOptionPathResets.Checked := True;
          end;

          pmTabMenu.Parent := NoteBook;
          pmTabMenu.Tag := TabNr;
          pmTabMenu.PopUp(PopUpPoint.x, PopUpPoint.y);
        end;
      end;

  end;
end;

procedure TfrmMain.NotebookDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  ATabIndex: Integer;
  TargetPath: String;
  SourceFiles: TFiles;
  TargetFileSource: IFileSource;
  ANotebook: TFileViewNotebook absolute Sender;
begin
  if (Source is TWinControl) and (TWinControl(Source).Parent is TFileView) then
  begin
    ATabIndex := ANotebook.IndexOfPageAt(Classes.Point(X, Y));
    if (ATabIndex > -1) then
    begin
      SourceFiles := ActiveFrame.CloneSelectedOrActiveFiles;
      try
        begin
          TargetPath := ANotebook.View[ATabIndex].CurrentPath;
          TargetFileSource := ANotebook.View[ATabIndex].FileSource;
          case GetDropEffectByKeyAndMouse(GetKeyShiftState, mbLeft) of
            DropCopyEffect:
              Self.CopyFiles(ActiveFrame.FileSource, TargetFileSource, SourceFiles, TargetPath, gShowDialogOnDragDrop);
            DropMoveEffect:
              Self.MoveFiles(ActiveFrame.FileSource, TargetFileSource, SourceFiles, TargetPath, gShowDialogOnDragDrop);
          end;
        end;
      finally
        SourceFiles.Free;
      end;
    end;
  end;
end;

procedure TfrmMain.NotebookDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  ATabIndex: Integer;
  APage: TFileViewPage;
  ANotebook: TFileViewNotebook absolute Sender;
begin
  Accept := False;
  if (Source is TWinControl) and (TWinControl(Source).Parent is TFileView) then
  begin
    ATabIndex := ANotebook.IndexOfPageAt(Classes.Point(X, Y));
    if (ATabIndex > -1) then
    begin
      APage:= ANotebook.Page[ATabIndex];
      Accept := (APage.FileView <> TWinControl(Source).Parent) and
                ((APage.LockState = tlsNormal) or (APage.LockPath = APage.FileView.CurrentPath));
    end;
  end;
end;

procedure TfrmMain.NotebookCloseTabClicked(Sender: TObject);
begin
  with (Sender As TFileViewPage) do
  if PageIndex <> -1 then
  begin
    RemovePage(Notebook, PageIndex);
  end;
end;

procedure TfrmMain.ConvertIniToolbarItem(Loader: TKASToolBarIniLoader; var Item: TKASToolItem; const Shortcut: String);
  procedure ConvertHotkeys(CommandItem: TKASCommandItem; Hotkeys: THotkeys; SearchHotkey: THotkey);
  var
    Hotkey: THotkey;
  begin
    Hotkey := Hotkeys.FindByContents(SearchHotkey);
    if Assigned(Hotkey) then
    begin
      Hotkey.Command := 'cm_ExecuteToolbarItem';
      Hotkey.Params := nil;
      SetValue(Hotkey.Params, 'ToolItemID', CommandItem.ID);
    end;
  end;
var
  HMForm: THMForm;
  Hotkey: THotkey;
  CommandItem: TKASCommandItem;
  MenuItem: TKASMenuItem;
  BarFileName: String;
begin
  if Item is TKASCommandItem then
  begin
    CommandItem := TKASCommandItem(Item);
    // Convert toolbar hotkey to use ID as parameter.
    if Shortcut <> '' then
    begin
      Hotkey := THotkey.Create;
      try
        Hotkey.Command := 'cm_Int_RunCommandFromBarFile';
        AddString(Hotkey.Shortcuts, Shortcut);
        Hotkey.Params := Hotkey.Shortcuts;
        HMForm := HotMan.Forms.Find('Main');
        if Assigned(HMForm) then
          ConvertHotkeys(CommandItem, HMForm.Hotkeys, Hotkey);
      finally
        Hotkey.Free;
      end;
    end;

    if ((CommandItem.Command = 'cm_OpenBar') or
        (CommandItem.Command = 'cm_ShowButtonMenu')) and
       (Length(CommandItem.Params) > 0) then
    begin
      BarFileName := CommandItem.Params[0];
      if Pos(PathDelim, BarFileName) <> 0 then
        BarFileName := GetCmdDirFromEnvVar(BarFileName)
      else
        BarFileName := gpCfgDir + BarFileName;
      if mbFileExists(BarFileName) then
      begin
        MenuItem := TKASMenuItem.Create;
        MenuItem.Assign(Item); // Copy whatever is possible from Command item
        Loader.Load(BarFileName, nil, MenuItem, @ConvertIniToolbarItem);
        mbRenameFile(BarFileName, BarFileName + '.obsolete');
        Item.Free;
        Item := MenuItem;
      end;
    end;
  end;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
var
  ModifierKeys: TShiftState;
begin
  // Either left or right panel has to be focused.
  if not FrameLeft.Focused and
     not FrameRight.Focused then
  begin
    Exit;
  end;

  ModifierKeys := GetKeyShiftStateEx;

  if gCmdLine and  // If command line is enabled
     (GetKeyTypingAction(ModifierKeys) = ktaCommandLine) and
     not ((Key in ['-', '*', '+', #0..#32]) and (Trim(edtCommand.Text) = '')) then
  begin
    TypeInCommandLine(Key);
    Key := #0;
  end;
end;

function TfrmMain.ActiveFrame: TFileView;
begin
  case PanelSelected of
    fpLeft:
      Result := FrameLeft;
    fpRight:
      Result := FrameRight;
  else
    assert(false,'Bad active frame');
  end;
end;

function TfrmMain.NotActiveFrame: TFileView;
begin
  case PanelSelected of
    fpRight:
      Result := FrameLeft;
    fpLeft:
      Result := FrameRight;
  else
    assert(false,'Bad active frame');
    Result:=FrameLeft;// only for compilator warning;
  end;
end;

function TfrmMain.ActiveNotebook: TFileViewNotebook;
begin
  case PanelSelected of
    fpLeft:
      Result := nbLeft;
    fpRight:
      Result := nbRight;
  else
    assert(false,'Bad active notebook');
  end;
end;

function TfrmMain.NotActiveNotebook: TFileViewNotebook;
begin
  case PanelSelected of
    fpLeft:
      Result := nbRight;
    fpRight:
      Result := nbLeft;
  else
    assert(false,'Bad active notebook');
  end;
end;

function TfrmMain.FrameLeft: TFileView;
begin
  Result := nbLeft.ActiveView;
end;

function TfrmMain.FrameRight: TFileView;
begin
  Result := nbRight.ActiveView;
end;

procedure TfrmMain.ForEachView(CallbackFunction: TForEachViewFunction; UserData: Pointer);
  procedure EnumerateNotebook(ANoteBook: TFileViewNotebook);
  var
    i: Integer;
  begin
    for i := 0 to ANoteBook.PageCount - 1 do
      CallbackFunction(ANoteBook.View[i], UserData);
  end;
begin
  EnumerateNotebook(nbLeft);
  EnumerateNotebook(nbRight);
end;

procedure TfrmMain.GetListOpenedPaths(const APaths: TStringList);

  procedure GetNotebookPaths(ANoteBook: TFileViewNotebook);
  var
    S: String;
    I: Integer;
  begin
    for I := 0 to ANoteBook.PageCount - 1 do
    begin
      S:= ANoteBook.View[I].CurrentPath;
      APaths.Add(S);
    end;
  end;

begin
  APaths.Clear;
  GetNotebookPaths(nbLeft);
  GetNotebookPaths(nbRight);
end;

procedure TfrmMain.AppException(Sender: TObject; E: Exception);
begin
  WriteExceptionToErrorFile;
  ShowExceptionDialog;
end;

procedure TfrmMain.AppShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  // Refresh monitor list
  Screen.UpdateMonitors;
  // Show hint only when application is active
  CanShow:= Application.Active;
end;

constructor TfrmMain.Create(TheOwner: TComponent);
begin
  FMainSplitterPos := 50.0;
  inherited Create(TheOwner);
  FCommands := TMainCommands.Create(Self, actionLst);

  Screen.Cursors[crArrowCopy] := LoadCursorFromLazarusResource('ArrowCopy');
  Screen.Cursors[crArrowMove] := LoadCursorFromLazarusResource('ArrowMove');
  Screen.Cursors[crArrowLink] := LoadCursorFromLazarusResource('ArrowLink');
end;

procedure TfrmMain.AfterConstruction;
begin
  FResizingFilePanels:= True;
  inherited AfterConstruction;
  FResizingFilePanels:= False;
  pnlNotebooksResize(pnlNotebooks);
end;

procedure TfrmMain.UpdateActionIcons;
var
  I: Integer;
  imgIndex: Integer;
  iconsDir: String;
  fileName: String;
  iconImg: TPicture;
  actionName: TComponentName;
begin
  if not gIconsInMenus then Exit;

  actionLst.Images := nil;
  pmTabMenu.Images := nil;
  mnuMain.Images := nil;
  imgLstActions.Clear;

  // Temporarily while feature is not implemented
  // http://doublecmd.sourceforge.net/mantisbt/view.php?id=11
  fileName := IntToStr(gIconsInMenusSize);
  iconsDir := gpPixmapPath + 'dctheme' + PathDelim + fileName;
  iconsDir := iconsDir + 'x' + fileName + PathDelim + 'actions';
  if not mbDirectoryExists(iconsDir) then Exit;

  iconImg := TPicture.Create;
  try
    imgLstActions.Width := gIconsInMenusSize;
    imgLstActions.Height := gIconsInMenusSize;

    actionLst.Images := imgLstActions;
    pmTabMenu.Images := imgLstActions;
    mnuMain.Images := imgLstActions;

    for I:= 0 to actionLst.ActionCount - 1 do
    begin
      actionName := UTF8LowerCase(actionLst.Actions[I].Name);
      fileName := iconsDir + PathDelim + 'cm_' + UTF8Copy(actionName, 4, Length(actionName) - 3) + '.png';
      if mbFileExists(fileName) then
      try
        iconImg.LoadFromFile(fileName);
        imgIndex := imgLstActions.Add(iconImg.Bitmap, nil);
        if imgIndex >= 0 then
        begin
           TAction(actionLst.Actions[I]).ImageIndex := imgIndex;
        end;
      except
        // Skip
      end;
    end;

  finally
    FreeAndNil(iconImg);
  end;
end;

procedure TfrmMain.UpdateHotDirIcons;
var
  I: Integer;
  iconsDir: String;
  fileName: String;
  iconImg: TPicture;
begin
  pmHotList.Images:=nil; { TODO -oDB : The images of popup menu in configuration should also be nilled to be correct }
  imgLstDirectoryHotlist.Clear;

  fileName := IntToStr(gIconsInMenusSize);
  iconsDir := gpPixmapPath + 'dctheme' + PathDelim + fileName;
  iconsDir := iconsDir + 'x' + fileName + PathDelim + 'actions';
  if not mbDirectoryExists(iconsDir) then Exit;

  iconImg := TPicture.Create;
  try
    fileName := IntToStr(gIconsInMenusSize);
    iconsDir := gpPixmapPath + 'dctheme' + PathDelim + fileName;
    iconsDir := iconsDir + 'x' + fileName + PathDelim + 'dirhotlist';
    imgLstDirectoryHotlist.Width := gIconsInMenusSize;
    imgLstDirectoryHotlist.Height := gIconsInMenusSize;
    pmHotList.Images:=imgLstDirectoryHotlist;

    for I:=0 to pred(length(ICONINDEXNAME)) do
    begin
      filename:=iconsDir+PathDelim+ICONINDEXNAME[I]+'.png';
      if mbFileExists(fileName) then
      try
        iconImg.LoadFromFile(fileName);
        imgLstDirectoryHotlist.Add(iconImg.Bitmap, nil);
      except
        // Skip
      end;
    end;

  finally
    FreeAndNil(iconImg);
  end;
end;

procedure TfrmMain.CreateDefaultToolbar;
  procedure AddCommand(Command: String);
  var
    CommandItem: TKASCommandItem;
  begin
    CommandItem := TKASCommandItem.Create(Commands.Commands);
    CommandItem.Icon := UTF8LowerCase(Command);
    CommandItem.Command := Command;
    // Leave CommandItem.Hint empty. It will be loaded at startup based on language.
    MainToolBar.AddButton(CommandItem);
  end;
  procedure AddSeparator;
  begin
    MainToolBar.AddButton(TKASSeparatorItem.Create);
  end;
var
  MainToolBarNode: TXmlNode;
begin
  if MainToolBar.ButtonCount = 0 then
  begin
    MainToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', False);
    if not Assigned(MainToolBarNode) then
    begin
      AddCommand('cm_Refresh');
      AddCommand('cm_RunTerm');
      AddCommand('cm_Options');
      AddSeparator;
      AddCommand('cm_BriefView');
      AddCommand('cm_ColumnsView');
      AddCommand('cm_ThumbnailsView');
      AddSeparator;
      AddCommand('cm_FlatView');
      AddSeparator;
      AddCommand('cm_ViewHistoryPrev');
      AddCommand('cm_ViewHistoryNext');
      AddSeparator;
      AddCommand('cm_MarkPlus');
      AddCommand('cm_MarkMinus');
      AddCommand('cm_MarkInvert');
      AddSeparator;
      AddCommand('cm_PackFiles');
      AddCommand('cm_ExtractFiles');
      AddSeparator;
      AddCommand('cm_Search');
      AddCommand('cm_MultiRename');
      AddCommand('cm_SyncDirs');
      AddCommand('cm_CopyFullNamesToClip');
      SaveMainToolBar;
    end;
  end;
end;

function TfrmMain.GetFileDlgStr(sLngOne, sLngMulti: String; Files: TFiles): String;
begin
  if Files.Count = 0 then
    raise Exception.Create(rsMsgNoFilesSelected);

  if Files.Count > 1 then
    Result := Format(sLngMulti, [Files.Count])
  else
    Result := Format(sLngOne, [Files[0].Name]);
end;

procedure TfrmMain.miHotAddOrConfigClick(Sender: TObject);
begin
  with Sender as TComponent do Commands.cm_WorkWithDirectoryHotlist(['action='+HOTLISTMAGICWORDS[tag], 'source='+QuoteStr(ActiveFrame.CurrentLocation), 'target='+QuoteStr(NotActiveFrame.CurrentLocation), 'index=0']);
end;

procedure TfrmMain.CreatePopUpDirHistory;
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  pmDirHistory.Items.Clear;

  for I:= 0 to Min(gDirHistoryCount, glsDirHistory.Count - 1) do
  begin
    MenuItem:= TMenuItem.Create(pmDirHistory);
    MenuItem.Caption:= glsDirHistory[I].Replace('&','&&');
    MenuItem.Hint:= glsDirHistory[I];
    MenuItem.OnClick:= @HistorySelected;
    pmDirHistory.Items.Add(MenuItem);
  end;
end;

procedure TfrmMain.ShowFileViewHistory(const Params: array of string);
begin
  ShowFileViewHistory(Params, -1, -1, -1, -1);
end;

procedure TfrmMain.ShowFileViewHistory(const Params: array of string; FromFileSourceIndex, FromPathIndex, ToFileSourceIndex, ToPathIndex: Integer);
const
  MaxItemsShown = 20;
var
  ItemsBackward: Integer = 0;
  ItemsForward: Integer = 0;

  function GoBack(var FileSourceIndex, PathIndex: Integer): Boolean;
  begin
    if PathIndex = 0 then
    begin
      if FileSourceIndex = 0 then
        Result := False
      else
      begin
        Dec(FileSourceIndex);
        PathIndex := ActiveFrame.PathsCount[FileSourceIndex] - 1;
        Result := True;
      end;
    end
    else
    begin
      Dec(PathIndex);
      Result := True;
    end;
  end;

  function GoForward(var FileSourceIndex, PathIndex: Integer): Boolean;
  begin
    if PathIndex = ActiveFrame.PathsCount[FileSourceIndex] - 1 then
    begin
      if FileSourceIndex = ActiveFrame.FileSourcesCount - 1 then
        Result := False
      else
      begin
        Inc(FileSourceIndex);
        PathIndex := 0;
        Result := True;
      end;
    end
    else
    begin
      Inc(PathIndex);
      Result := True;
    end;
  end;

  procedure AddCaptionItem(s: String);
  var
    mi: TMenuItem;
  begin
    mi := TMenuItem.Create(pmDirHistory);
    mi.Caption := s;
    mi.Enabled := False;
    pmDirHistory.Items.Add(mi);
  end;

  procedure FindBoundsBackward;
  var
    I: Integer;
  begin
    GoBack(ToFileSourceIndex, ToPathIndex);

    FromFileSourceIndex := ToFileSourceIndex;
    FromPathIndex := ToPathIndex;

    for i := 0 to MaxItemsShown - 1 do
    begin
      if GoBack(FromFileSourceIndex, FromPathIndex) then
        Inc(ItemsBackward);
    end;
  end;

  procedure FindBoundsFromCenter;
  var
    I: Integer;
  begin
    FromFileSourceIndex := ActiveFrame.CurrentFileSourceIndex;
    FromPathIndex := ActiveFrame.CurrentPathIndex;
    ToFileSourceIndex := FromFileSourceIndex;
    ToPathIndex := FromPathIndex;

    for i := 0 to (MaxItemsShown div 2) - 1 do
    begin
      if GoBack(FromFileSourceIndex, FromPathIndex) then
        Inc(ItemsBackward);
      if GoForward(ToFileSourceIndex, ToPathIndex) then
        Inc(ItemsForward);
    end;

    for i := ItemsForward to (MaxItemsShown div 2) - 1 do
    begin
      if GoBack(FromFileSourceIndex, FromPathIndex) then
        Inc(ItemsBackward);
    end;

    for i := ItemsBackward to (MaxItemsShown div 2) - 1 do
    begin
      if GoForward(ToFileSourceIndex, ToPathIndex) then
        Inc(ItemsForward);
    end;
  end;

  procedure FindBoundsForward;
  var
    I: Integer;
  begin
    GoForward(FromFileSourceIndex, FromPathIndex);

    ToFileSourceIndex := FromFileSourceIndex;
    ToPathIndex := FromPathIndex;

    for i := 0 to MaxItemsShown - 1 do
    begin
      if GoForward(ToFileSourceIndex, ToPathIndex) then
        Inc(ItemsForward);
    end;
  end;

var
  bUseTreeViewMenu: boolean = false;
  bUsePanel: boolean = false; // As opposed as the other popup, for that one, by legacy, the position of the popup is the cursor position instead of top left corner of active panel.
  p: TPoint;
  iWantedWidth: integer = 0;
  iWantedHeight: integer = 0;
  sMaybeMenuItem: TMenuItem = nil;
  I: Integer;
  mi: TMenuItem;
begin
  pmDirHistory.Items.Clear;
  p.x := 0;
  p.y := 0;

  if FromFileSourceIndex <> -1 then
    FindBoundsForward
  else if ToFileSourceIndex <> - 1 then
    FindBoundsBackward
  else
    FindBoundsFromCenter;

  if (FromFileSourceIndex > 0) or (FromPathIndex > 0) then
  begin
    mi := TMenuItem.Create(pmDirHistory);
    mi.Caption := '...';
    mi.OnClick := @ViewHistoryPrevSelected;
    mi.Tag := HistoryIndexesToTag(FromFileSourceIndex, FromPathIndex);
    pmDirHistory.Items.Add(mi);
  end;

  for i := 0 to ItemsForward + ItemsBackward do
  begin
    mi := TMenuItem.Create(pmDirHistory);
    pmDirHistory.Items.Add(mi);

    mi.Caption := ActiveFrame.Path[FromFileSourceIndex, FromPathIndex].Replace('&','&&');
    mi.OnClick := @ViewHistorySelected;
    // Remember indexes into history.
    mi.Tag := HistoryIndexesToTag(FromFileSourceIndex, FromPathIndex);
    // Mark current history position.
    if (FromFileSourceIndex = ActiveFrame.CurrentFileSourceIndex) and
       (FromPathIndex = ActiveFrame.CurrentPathIndex) then
      mi.Checked := True;

    if not GoForward(FromFileSourceIndex, FromPathIndex) then
      Break;

    // Add separator and address of a file source as a caption.
    if FromPathIndex = 0 then
    begin
      AddCaptionItem('-');
      AddCaptionItem('- ' + ActiveFrame.FileSources[FromFileSourceIndex].CurrentAddress.Replace('&','&&') + ' -');
    end;
  end;

  if (ToFileSourceIndex < ActiveFrame.FileSourcesCount - 1) or
     (ToPathIndex < ActiveFrame.PathsCount[ToFileSourceIndex] - 1) then
  begin
    mi := TMenuItem.Create(pmDirHistory);
    mi.Caption := '...';
    mi.OnClick := @ViewHistoryNextSelected;
    mi.Tag := HistoryIndexesToTag(ToFileSourceIndex, ToPathIndex);
    pmDirHistory.Items.Add(mi);
  end;

  Application.ProcessMessages;

  // 1. Let's parse our parameters.
  Commands.DoParseParametersForPossibleTreeViewMenu(Params, gUseTreeViewMenuWithViewHistory, gUseTreeViewMenuWithViewHistory, bUseTreeViewMenu, bUsePanel, p);

  // 2. Show the appropriate menu.
  if bUseTreeViewMenu then
  begin
    if not bUsePanel then
      iWantedHeight := ((frmMain.ActiveFrame.ClientToScreen(Classes.Point(0, 0)).y + frmMain.ActiveFrame.Height) - p.y)
    else
    begin
      iWantedWidth := frmMain.ActiveFrame.Width;
      iWantedHeight := frmMain.ActiveFrame.Height;
    end;

    sMaybeMenuItem := GetUserChoiceFromTreeViewMenuLoadedFromPopupMenu(pmDirHistory, tvmcViewHistory, p.X, p.Y, iWantedWidth, iWantedHeight);
    if sMaybeMenuItem <> nil then sMaybeMenuItem.OnClick(sMaybeMenuItem);
  end
  else
  begin
    pmDirHistory.Popup(p.X, p.Y);
  end;
end;

{ TfrmMain.HotDirActualSwitchToDir }
// Actual routine called when user click the item from the Directory Hotlist popup menu to switch to a hot directory
// The index received is the index from the "gDirectoryHotlist" to read hotdir entry from.
//
procedure TfrmMain.HotDirActualSwitchToDir(Index:longint);
var
  aPath: String;
  isSHIFTDown, isCTRLDown: boolean;
  PossibleCommande,PossibleParam: string;
  PosFirstSpace: integer;
  Editor: TOptionsEditor;
  Options: IOptionsDialog;
begin
  // This handler is used by HotDir AND SpecialDir.
  // HotDirs AND SpecialDirs are only supported by filesystem.

  // If the index is larger or equal to "TAGOFFSET_FORCHANGETOSPECIALDIR", it means it's a "SpecialDir" and we'll change accordingly
  if (Index < TAGOFFSET_FORCHANGETOSPECIALDIR) AND (Index >= 0) then
    begin
      isSHIFTDown:=((GetKeyState(VK_SHIFT) AND $80) <> 0);
      if not isSHIFTDown then //if SHIFT is NOT down, it's to change directory
      begin
        case gDirectoryHotlist.HotDir[Index].Dispatcher of
          hd_CHANGEPATH:
            begin
              isCTRLDown:=((GetKeyState(VK_CONTROL) AND $80) <> 0); //if CTRL is down, it's to request to DON'T CHANGE TARGET even if in the directoryhotlist entry it was requesting to do so

              aPath := gDirectoryHotlist.HotDir[Index].HotDirPath;
              if aPath<>'' then
                begin
                  case gDirectoryHotlist.HotDir[Index].HotDirPathSort of
                    1: Commands.cm_UniversalSingleDirectSort([STR_ACTIVEFRAME,STR_NAME,STR_ASCENDING]); //Name, a-z
                    2: Commands.cm_UniversalSingleDirectSort([STR_ACTIVEFRAME,STR_NAME,STR_DESCENDING]); //Name, z-a
                    3: Commands.cm_UniversalSingleDirectSort([STR_ACTIVEFRAME,STR_EXTENSION,STR_ASCENDING]); //Ext, a-z
                    4: Commands.cm_UniversalSingleDirectSort([STR_ACTIVEFRAME,STR_EXTENSION,STR_DESCENDING]); //Ext, z-a
                    5: Commands.cm_UniversalSingleDirectSort([STR_ACTIVEFRAME,STR_SIZE,STR_DESCENDING]); //Size 9-0
                    6: Commands.cm_UniversalSingleDirectSort([STR_ACTIVEFRAME,STR_SIZE,STR_ASCENDING]); //Size 0-9
                    7: Commands.cm_UniversalSingleDirectSort([STR_ACTIVEFRAME,STR_MODIFICATIONDATETIME,STR_DESCENDING]); //Date 9-0
                    8: Commands.cm_UniversalSingleDirectSort([STR_ACTIVEFRAME,STR_MODIFICATIONDATETIME,STR_ASCENDING]); //Date 0-9
                  end;

                  aPath := mbExpandFileName(aPath);
                  ChooseFileSource(ActiveFrame, aPath);

                  if (not isCTRLDown) then //We don't change target folder if CTRL key is pressed
                  begin
                    aPath := gDirectoryHotlist.HotDir[Index].HotDirTarget;
                    if aPath<>'' then
                      begin
                        case gDirectoryHotlist.HotDir[Index].HotDirTargetSort of
                          1: Commands.cm_UniversalSingleDirectSort([STR_NOTACTIVEFRAME,STR_NAME,STR_ASCENDING]); //Name, a-z
                          2: Commands.cm_UniversalSingleDirectSort([STR_NOTACTIVEFRAME,STR_NAME,STR_DESCENDING]); //Name, z-a
                          3: Commands.cm_UniversalSingleDirectSort([STR_NOTACTIVEFRAME,STR_EXTENSION,STR_ASCENDING]); //Ext, a-z
                          4: Commands.cm_UniversalSingleDirectSort([STR_NOTACTIVEFRAME,STR_EXTENSION,STR_DESCENDING]); //Ext, z-a
                          5: Commands.cm_UniversalSingleDirectSort([STR_NOTACTIVEFRAME,STR_SIZE,STR_DESCENDING]); //Size 9-0
                          6: Commands.cm_UniversalSingleDirectSort([STR_NOTACTIVEFRAME,STR_SIZE,STR_ASCENDING]); //Size 0-9
                          7: Commands.cm_UniversalSingleDirectSort([STR_NOTACTIVEFRAME,STR_MODIFICATIONDATETIME,STR_DESCENDING]); //Date 9-0
                          8: Commands.cm_UniversalSingleDirectSort([STR_NOTACTIVEFRAME,STR_MODIFICATIONDATETIME,STR_ASCENDING]); //Date 0-9
                        end;

                        aPath := mbExpandFileName(aPath);
                        ChooseFileSource(NotActiveFrame, aPath);
                      end;
                  end;
                end;
            end; //hd_CHANGEPATH:

          hd_COMMAND:
            begin
              PosFirstSpace:=pos(' ',gDirectoryHotlist.HotDir[Index].HotDirPath);

              if PosFirstSpace=0 then
                begin
                  PossibleCommande:=gDirectoryHotlist.HotDir[Index].HotDirPath;
                  PossibleParam:='';
                end
              else
                begin
                  PossibleCommande:=leftstr(gDirectoryHotlist.HotDir[Index].HotDirPath,(PosFirstSpace-1));
                  PossibleParam:=rightstr(gDirectoryHotlist.HotDir[Index].HotDirPath,length(gDirectoryHotlist.HotDir[Index].HotDirPath)-PosFirstSpace);
                end;

                Commands.Commands.ExecuteCommand(PossibleCommande, SplitString(PossibleParam,' '));
            end;
        end; //case gDirectoryHotlist.HotDir[Index].Dispatcher of
      end
      else
      begin //if SHIFT IS down, it's to EDIT current selected entry from the Directory Hotlist that the current selected popup menu selection is pointing.
        Options := ShowOptions(TfrmOptionsDirectoryHotlist);
        Editor := Options.GetEditor(TfrmOptionsDirectoryHotlist);
        Application.ProcessMessages;
        if Editor.CanFocus then  Editor.SetFocus;
        TfrmOptionsDirectoryHotlist(Editor).SubmitToAddOrConfigToHotDirDlg(ACTION_DIRECTLYCONFIGENTRY,ActiveFrame.CurrentPath,NotActiveFrame.CurrentPath,Index);
      end;
    end
  else
    begin
      if Index>=0 then
      begin
        //So it's a SpecialDir...
        Index:=Index-TAGOFFSET_FORCHANGETOSPECIALDIR;
        aPath := mbExpandFileName((gSpecialDirList.SpecialDir[Index].PathValue));
        ChooseFileSource(ActiveFrame, aPath);
      end;
    end;
end;

procedure TfrmMain.HotDirSelected(Sender: TObject);
begin
  HotDirActualSwitchToDir((Sender as TMenuItem).Tag);
end;

procedure TfrmMain.HistorySelected(Sender: TObject);
var
  aPath: String;
begin
  // This handler is used by DirHistory.
  aPath := (Sender as TMenuItem).Hint;
  aPath := mbExpandFileName(aPath);
  ChooseFileSource(ActiveFrame, aPath);
end;

procedure TfrmMain.ViewHistorySelected(Sender: TObject);
var
  FileSourceIndex, PathIndex: Integer;
begin
  if Sender is TMenuItem then
  begin
    HistoryIndexesFromTag((Sender as TMenuItem).Tag, FileSourceIndex, PathIndex);
    ActiveFrame.GoToHistoryIndex(FileSourceIndex, PathIndex);
  end;
end;

procedure TfrmMain.ViewHistoryPrevSelected(Sender:TObject);
var
  FileSourceIndex, PathIndex: Integer;
begin
  if Sender is TMenuItem then
  begin
    HistoryIndexesFromTag((Sender as TMenuItem).Tag, FileSourceIndex, PathIndex);
    ShowFileViewHistory([], -1, -1, FileSourceIndex, PathIndex);
  end;
end;

procedure TfrmMain.ViewHistoryNextSelected(Sender:TObject);
var
  FileSourceIndex, PathIndex: Integer;
begin
  if Sender is TMenuItem then
  begin
    HistoryIndexesFromTag((Sender as TMenuItem).Tag, FileSourceIndex, PathIndex);
    ShowFileViewHistory([], FileSourceIndex, PathIndex, -1, -1);
  end;
end;

procedure TfrmMain.edtCommandKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key=VK_Down) and (Shift=[ssCtrl]) and (edtCommand.Items.Count>0) then
  begin
    Key:=0;
    edtCommand.DroppedDown:=True;
    edtCommand.SetFocus;
  end;
end;

procedure TfrmMain.OnCopyOutTempStateChanged(Operation: TFileSourceOperation;
  State: TFileSourceOperationState);
begin
  FModalOperationResult:= Operation.Result = fsorFinished;
end;

function TfrmMain.CopyFiles(SourceFileSource, TargetFileSource: IFileSource;
                            var SourceFiles: TFiles; TargetPath: String;
                            bShowDialog: Boolean;
                            QueueIdentifier: TOperationsManagerQueueIdentifier): Boolean;
var
  sDestination: String;
  sDstMaskTemp: String;
  FileSource: IFileSource;
  TargetFiles: TFiles = nil;
  CopyDialog: TfrmCopyDlg = nil;
  OperationTemp: Boolean = False;
  OperationType: TFileSourceOperationType;
  OperationClass: TFileSourceOperationClass;
  Operation: TFileSourceCopyOperation = nil;
  OperationOptionsUIClass: TFileSourceOperationOptionsUIClass = nil;
begin
  Result := False;
  try
    if SourceFiles.Count = 0 then
      Exit;

    if (SourceFiles.Count = 1) and
       ((not (SourceFiles[0].IsDirectory or SourceFiles[0].IsLinkToDirectory)) or
        (TargetPath = ''))
    then
      sDestination := TargetPath + SourceFiles[0].Name
    else
      sDestination := TargetPath + '*.*';

    // If same file source and address
    if (fsoCopy in SourceFileSource.GetOperationsTypes) and
       (fsoCopy in TargetFileSource.GetOperationsTypes) and
       SourceFileSource.Equals(TargetFileSource) and
       SameText(SourceFileSource.GetCurrentAddress, TargetFileSource.GetCurrentAddress) then
    begin
      OperationType := fsoCopy;
      FileSource := SourceFileSource;
      OperationClass := SourceFileSource.GetOperationClass(fsoCopy);
    end
    else if TargetFileSource.IsClass(TFileSystemFileSource) and
            (fsoCopyOut in SourceFileSource.GetOperationsTypes) then
    begin
      OperationType := fsoCopyOut;
      FileSource := SourceFileSource;
      OperationClass := SourceFileSource.GetOperationClass(fsoCopyOut);
    end
    else if SourceFileSource.IsClass(TFileSystemFileSource) and
            (fsoCopyIn in TargetFileSource.GetOperationsTypes) then
    begin
      OperationType := fsoCopyIn;
      FileSource := TargetFileSource;
      OperationClass := TargetFileSource.GetOperationClass(fsoCopyIn);
    end
    else if (fsoCopyOut in SourceFileSource.GetOperationsTypes) and
            (fsoCopyIn in TargetFileSource.GetOperationsTypes) then
    begin
      OperationTemp := True;
      OperationType := fsoCopyOut;
      FileSource := SourceFileSource;
      OperationClass := SourceFileSource.GetOperationClass(fsoCopyOut);
      if (fspCopyOutOnMainThread in SourceFileSource.Properties) or
         (fspCopyInOnMainThread in TargetFileSource.Properties) then
      begin
        QueueIdentifier:= ModalQueueId;
      end;
    end
    else
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    if bShowDialog then
    begin
      if Assigned(OperationClass) then
        OperationOptionsUIClass := OperationClass.GetOptionsUIClass;

      CopyDialog := TfrmCopyDlg.Create(Application, cmdtCopy, FileSource, OperationOptionsUIClass);
      CopyDialog.edtDst.Text := sDestination;
      CopyDialog.edtDst.ReadOnly := OperationTemp;
      CopyDialog.lblCopySrc.Caption := GetFileDlgStr(rsMsgCpSel, rsMsgCpFlDr, SourceFiles);

      if OperationTemp and (QueueIdentifier = ModalQueueId) then
      begin
        CopyDialog.QueueIdentifier:= QueueIdentifier;
        CopyDialog.btnAddToQueue.Visible:= False;
        CopyDialog.btnCreateSpecialQueue.Visible:= False;
        CopyDialog.btnOptions.Visible:= False;
      end;

      while True do
      begin
        if CopyDialog.ShowModal = mrCancel then
          Exit;

        sDestination := CopyDialog.edtDst.Text;

        GetDestinationPathAndMask(SourceFiles, SourceFileSource,
                                  TargetFileSource, sDestination,
                                  SourceFiles.Path, TargetPath, sDstMaskTemp);

        if (TargetFileSource = nil) or (Length(TargetPath) = 0) then
        begin
          MessageDlg(rsMsgInvalidPath, rsMsgErrNotSupported, mtWarning, [mbOK], 0);
          Continue;
        end;

        if HasPathInvalidCharacters(TargetPath) then
          MessageDlg(rsMsgInvalidPath, Format(rsMsgInvalidPathLong, [TargetPath]),
            mtWarning, [mbOK], 0)
        else
          Break;
      end;

      QueueIdentifier := CopyDialog.QueueIdentifier;
    end
    else
      GetDestinationPathAndMask(SourceFiles, TargetFileSource, sDestination,
                                SourceFiles.Path, TargetPath, sDstMaskTemp);

    // Copy via temp directory
    if OperationTemp then
    begin
      // Execute both operations in one new queue
      if QueueIdentifier = FreeOperationsQueueId then
        QueueIdentifier := OperationsManager.GetNewQueueIdentifier;
      // Save real target
      sDestination := TargetPath;
      FileSource := TargetFileSource;
      TargetFiles := SourceFiles.Clone;
      // Replace target by temp directory
      TargetFileSource := TTempFileSystemFileSource.Create();
      TargetPath := TargetFileSource.GetRootDir;
      ChangeFileListRoot(TargetPath, TargetFiles);
    end;

    case OperationType of
      fsoCopy:
        begin
          // Copy within the same file source.
          Operation := SourceFileSource.CreateCopyOperation(
                         SourceFiles,
                         TargetPath) as TFileSourceCopyOperation;
        end;
      fsoCopyOut:
        // CopyOut to filesystem.
        Operation := SourceFileSource.CreateCopyOutOperation(
                       TargetFileSource,
                       SourceFiles,
                       TargetPath) as TFileSourceCopyOperation;
      fsoCopyIn:
        // CopyIn from filesystem.
        Operation := TargetFileSource.CreateCopyInOperation(
                       SourceFileSource,
                       SourceFiles,
                       TargetPath) as TFileSourceCopyOperation;
    end;

    if Assigned(Operation) then
    begin
      // Set operation options based on settings in dialog.
      Operation.RenameMask := sDstMaskTemp;

      if Assigned(CopyDialog) then
        CopyDialog.SetOperationOptions(Operation);

      if OperationTemp and (QueueIdentifier = ModalQueueId) then
      begin
        Operation.AddStateChangedListener([fsosStopped], @OnCopyOutTempStateChanged);
      end;

      // Start operation.
      OperationsManager.AddOperation(Operation, QueueIdentifier, False, True);
      Result := True;
    end
    else
      msgWarning(rsMsgNotImplemented);

    // Copy via temp directory
    if OperationTemp and Result and ((QueueIdentifier <> ModalQueueId) or FModalOperationResult) then
    begin
      // CopyIn from temp filesystem
      Operation := FileSource.CreateCopyInOperation(
                     TargetFileSource,
                     TargetFiles,
                     sDestination) as TFileSourceCopyOperation;

      Result := Assigned(Operation);
      if Result then
      begin
        if Assigned(CopyDialog) then
          CopyDialog.SetOperationOptions(Operation);

        // Start operation.
        OperationsManager.AddOperation(Operation, QueueIdentifier, False, True);
      end;
    end;

  finally
    FreeAndNil(TargetFiles);
    FreeAndNil(SourceFiles);
    FreeAndNil(CopyDialog);
  end;
end;

function TfrmMain.MoveFiles(SourceFileSource, TargetFileSource: IFileSource;
                            var SourceFiles: TFiles; TargetPath: String;
                            bShowDialog: Boolean;
                            QueueIdentifier: TOperationsManagerQueueIdentifier = FreeOperationsQueueId): Boolean;
var
  sDestination: String;
  sDstMaskTemp: String;
  Operation: TFileSourceMoveOperation;
  bMove: Boolean;
  MoveDialog: TfrmCopyDlg = nil;
begin
  Result := False;
  try
    // Special case for Search Result File Source
    if SourceFileSource.IsClass(TSearchResultFileSource) then begin
      SourceFileSource:= ISearchResultFileSource(SourceFileSource).FileSource;
    end;
    // Only allow moving within the same file source.
    if (SourceFileSource.IsInterface(TargetFileSource) or
        TargetFileSource.IsInterface(SourceFileSource)) and
       (SourceFileSource.CurrentAddress = TargetFileSource.CurrentAddress) and
       (fsoMove in SourceFileSource.GetOperationsTypes) and
       (fsoMove in TargetFileSource.GetOperationsTypes) then
    begin
      bMove := True;
    end
    else if ((fsoCopyOut in SourceFileSource.GetOperationsTypes) and
             (fsoCopyIn in TargetFileSource.GetOperationsTypes)) then
    begin
      bMove := False;  // copy + delete through temporary file system
      msgWarning(rsMsgNotImplemented);
      Exit;
    end
    else
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    if SourceFiles.Count = 0 then
      Exit;

    if (SourceFiles.Count = 1) and
       (not (SourceFiles[0].IsDirectory or SourceFiles[0].IsLinkToDirectory))
    then
      sDestination := TargetPath + ExtractFileName(SourceFiles[0].Name)
    else
      sDestination := TargetPath + '*.*';

    if bShowDialog then
    begin
      MoveDialog := TfrmCopyDlg.Create(Application, cmdtMove, SourceFileSource,
        SourceFileSource.GetOperationClass(fsoMove).GetOptionsUIClass);
      MoveDialog.edtDst.Text := sDestination;
      MoveDialog.lblCopySrc.Caption := GetFileDlgStr(rsMsgRenSel, rsMsgRenFlDr, SourceFiles);

      while True do
      begin
        if MoveDialog.ShowModal = mrCancel then
          Exit;

        sDestination := MoveDialog.edtDst.Text;

        GetDestinationPathAndMask(SourceFiles, SourceFileSource,
                                  TargetFileSource, sDestination,
                                  SourceFiles.Path, TargetPath, sDstMaskTemp);

        if (TargetFileSource = nil) or (Length(TargetPath) = 0) then
        begin
          MessageDlg(EmptyStr, rsMsgInvalidPath, mtWarning, [mbOK], 0);
          Continue;
        end;

        if HasPathInvalidCharacters(TargetPath) then
          MessageDlg(rsMsgInvalidPath, Format(rsMsgInvalidPathLong, [TargetPath]),
            mtWarning, [mbOK], 0)
        else
          Break;
      end;

      QueueIdentifier := MoveDialog.QueueIdentifier;
    end
    else
      GetDestinationPathAndMask(SourceFiles, TargetFileSource, sDestination,
                                SourceFiles.Path, TargetPath, sDstMaskTemp);

    if bMove then
    begin
      Operation := SourceFileSource.CreateMoveOperation(
                     SourceFiles, TargetPath) as TFileSourceMoveOperation;

      if Assigned(Operation) then
      begin
        // Set operation options based on settings in dialog.
        Operation.RenameMask := sDstMaskTemp;

        if Assigned(MoveDialog) then
          MoveDialog.SetOperationOptions(Operation);

        // Start operation.
        OperationsManager.AddOperation(Operation, QueueIdentifier, False, True);
        Result := True;
      end
      else
        msgWarning(rsMsgNotImplemented);
    end
    else
    begin
      // Use CopyOut, CopyIn operations.
    end;

  finally
    FreeAndNil(SourceFiles);
    FreeAndNil(MoveDialog);
  end;
end;

function TfrmMain.CopyFiles(sDestPath: String; bShowDialog: Boolean;
                            QueueIdentifier: TOperationsManagerQueueIdentifier = FreeOperationsQueueId): Boolean;
var
  FileSource: IFileSource;
  SourceFiles: TFiles = nil;
begin
  SourceFiles := ActiveFrame.CloneSelectedOrActiveFiles;
  if Assigned(SourceFiles) then
  begin
    if Length(sDestPath) > 0 then
      FileSource := NotActiveFrame.FileSource
    else begin
      FileSource := ActiveFrame.FileSource;
    end;
    try
      Result := CopyFiles(ActiveFrame.FileSource, FileSource,
                          SourceFiles, sDestPath, bShowDialog, QueueIdentifier);
      if Result then
        ActiveFrame.MarkFiles(False);

    finally
      FreeAndNil(SourceFiles);
    end;
  end
  else
    Result := False;
end;

function TfrmMain.MoveFiles(sDestPath: String; bShowDialog: Boolean;
                            QueueIdentifier: TOperationsManagerQueueIdentifier = FreeOperationsQueueId): Boolean;
var
  SourceFiles: TFiles = nil;
begin
  SourceFiles := ActiveFrame.CloneSelectedOrActiveFiles;
  if Assigned(SourceFiles) then
  begin
    try
      Result := MoveFiles(ActiveFrame.FileSource, NotActiveFrame.FileSource,
                          SourceFiles, sDestPath, bShowDialog, QueueIdentifier);
      if Result then
        ActiveFrame.MarkFiles(False);

    finally
      FreeAndNil(SourceFiles);
    end;
  end
  else
    Result := False;
end;

procedure TfrmMain.GetDestinationPathAndMask(SourceFiles: TFiles;
                                             TargetFileSource: IFileSource;
                                             EnteredPath: String; BaseDir: String;
                                             out DestPath, DestMask: String);
var
  AbsolutePath: String;
begin
  if TargetFileSource.GetPathType(EnteredPath) = ptAbsolute then
    AbsolutePath := EnteredPath
  else
  begin
    // This only work for filesystem for now.
    if TargetFileSource.IsClass(TFileSystemFileSource) then
      AbsolutePath := BaseDir + EnteredPath
    else
      AbsolutePath := PathDelim{TargetFileSource.GetRoot} + EnteredPath;
  end;

  AbsolutePath := NormalizePathDelimiters(AbsolutePath);  // normalize path delimiters
  AbsolutePath := ExpandAbsolutePath(AbsolutePath);

  if Length(AbsolutePath) = 0 then Exit;

  // If the entered path ends with a path delimiter
  // treat it as a path to a not yet existing directory
  // which should be created.
  if (AbsolutePath[Length(AbsolutePath)] = PathDelim) or
     ((TargetFileSource.IsClass(TFileSystemFileSource)) and
      mbDirectoryExists(AbsolutePath)) then
  begin
    // Destination is a directory.
    DestPath := AbsolutePath;
    DestMask := '*.*';
  end
  else
  begin
    // Destination is a file name or mask.
    DestPath := ExtractFilePath(AbsolutePath);
    DestMask := ExtractFileName(AbsolutePath);

    if (SourceFiles.Count > 1) and not ContainsWildcards(DestMask) then
    begin
      // Assume it is a path to a directory because cannot put multiple
      // files/directories into one file.
      DestPath := AbsolutePath;
      DestMask := '*.*';
    end
    // For convenience, treat '*' as "whole file name".
    // To remove extension '*.' can be used.
    else if DestMask = '*' then
      DestMask := '*.*';
  end;
end;

procedure TfrmMain.GetDestinationPathAndMask(SourceFiles: TFiles;
  SourceFileSource: IFileSource; var TargetFileSource: IFileSource;
  EnteredPath: String; BaseDir: String; out DestPath, DestMask: String);
var
  FileSourceIndex, PathIndex: Integer;
begin
  // If it is a file source root and we trying to copy/move to parent directory
  if StrBegins(EnteredPath, '..') and SourceFileSource.IsPathAtRoot(SourceFiles.Path) then
  begin
    // Change to previous file source and last path.
    FileSourceIndex := ActiveFrame.CurrentFileSourceIndex - 1;
    if FileSourceIndex < 0 then
      TargetFileSource := nil // No parent file sources.
    else
    begin
      PathIndex := ActiveFrame.PathsCount[FileSourceIndex] - 1;
      if PathIndex < 0 then
        TargetFileSource := nil // No paths.
      else
      begin
        TargetFileSource := ActiveFrame.FileSources[FileSourceIndex];
        // Determine destination type
        if (Length(EnteredPath) = 2) or (EnteredPath[Length(EnteredPath)] = PathDelim) then
          EnteredPath:= EmptyStr // Destination is a directory
        else
          EnteredPath:= ExtractFileName(EnteredPath); // Destination is a file name or mask
        // Combine destination path
        EnteredPath := ActiveFrame.Path[FileSourceIndex, PathIndex] + EnteredPath;
      end;
    end;
  end;
  // Target file source is valid
  if Assigned(TargetFileSource) then
  begin
    GetDestinationPathAndMask(SourceFiles, TargetFileSource,
                              EnteredPath, BaseDir, DestPath, DestMask);
  end;
end;

procedure TfrmMain.SetDragCursor(Shift: TShiftState);
begin
  FrameLeft.SetDragCursor(Shift);
  FrameRight.SetDragCursor(Shift);
end;

procedure TfrmMain.CreateWnd;
begin
  // Must be before CreateWnd
  LoadWindowState;

  inherited CreateWnd;

  // Save real main form handle
  Application.MainForm.Tag:= Handle;
end;

{$if lcl_fullversion >= 1070000}
procedure TfrmMain.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    DisableAutoSizing;
    try
      // ScaleFontsPPI(AYProportion);
      BorderSpacing.AutoAdjustLayout(AXProportion, AYProportion);
      Constraints.AutoAdjustLayout(AXProportion, AYProportion);
    finally
      EnableAutoSizing;
    end;
  end;
end;
{$endif}

procedure TfrmMain.FormKeyUp( Sender: TObject; var Key: Word;
  Shift: TShiftState) ;
begin
  SetDragCursor(Shift);
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ShiftEx : TShiftState;
  CmdText : String;
begin
  SetDragCursor(Shift);

  // Either left or right panel has to be focused.
  if not FrameLeft.Focused and
     not FrameRight.Focused then
  begin
    Exit;
  end;

  ShiftEx := GetKeyShiftStateEx;

  case Key of
    VK_BACK:
      if IsCommandLineVisible and
         (GetKeyTypingAction(ShiftEx) = ktaCommandLine) and (edtCommand.Text <> '') then
      begin
        // Delete last character.
        CmdText := edtCommand.Text;
        UTF8Delete(CmdText, UTF8Length(CmdText), 1);
        edtCommand.Text := CmdText;

        edtCommand.SetFocus;
        edtCommand.SelStart := UTF8Length(edtCommand.Text) + 1;
        Key := 0;
      end;

    VK_ESCAPE:
      if IsCommandLineVisible and
         (GetKeyTypingAction(ShiftEx) = ktaCommandLine) and
         (edtCommand.Text <> '') then
      begin
        edtCommand.Text := '';
        Key := 0;
      end;

    VK_RETURN, VK_SELECT:
      if IsCommandLineVisible and
         (GetKeyTypingAction(ShiftEx) = ktaCommandLine) and
         (edtCommand.Text <> '') then
      begin
        // execute command line (in terminal with Shift)
        ExecuteCommandLine(Shift = [ssShift]);
        Key := 0;
      end;

    VK_SPACE:
      if (GetKeyTypingAction(ShiftEx) = ktaCommandLine) and (edtCommand.Text <> '') then
      begin
        TypeInCommandLine(' ');
        Key := 0;
      end;

    VK_TAB:
      begin
        // Select opposite panel.
        case PanelSelected of
          fpLeft: SetActiveFrame(fpRight);
          fpRight: SetActiveFrame(fpLeft);
        else SetActiveFrame(fpLeft);
        end;
        Key := 0;
      end;
  end;

  CheckCommandLine(ShiftEx, Key);
end;

procedure TfrmMain.pmToolBarPopup(Sender: TObject);
var
  sText: String;
  sDir: String;
  bPaste: Boolean;
  ToolItem: TKASToolItem;
  Button: TKASToolButton;
begin
  Button := TKASToolButton(pmToolBar.Tag);
  tbSeparator.Visible:= Assigned(Button);
  tbCut.Visible:= Assigned(Button);
  tbCopy.Visible:= Assigned(Button);
  tbChangeDir.Visible:= False;
  tbDelete.Visible:= Assigned(Button);
  if Assigned(Button) then
  begin
    ToolItem := Button.ToolItem;
    if ToolItem is TKASProgramItem then
    begin
      sDir := TKASProgramItem(ToolItem).StartPath;
      sDir:= PrepareParameter(sDir, nil, [ppoNormalizePathDelims, ppoReplaceTilde]);
      tbChangeDir.Caption := 'CD ' + sDir;
      tbChangeDir.Visible := True;
    end;
  end;

  sText:= Clipboard.AsText;
  bPaste:= StrBegins(sText, DCToolItemClipboardHeader) or
           StrBegins(sText, DCToolbarClipboardHeader) or
           StrBegins(sText, TCToolbarClipboardHeader);
  if bPaste then
    tbSeparator.Visible:= True;
  tbPaste.Visible:= bPaste;
end;

procedure TfrmMain.ShellTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
begin
  Node.ImageIndex:= 0;
  Node.SelectedIndex:= 0;
end;

procedure TfrmMain.pnlLeftResize(Sender: TObject);
begin
  if gDriveBar1 and gDriveBar2 and not gHorizontalFilePanels then
    pnlDskLeft.Width := pnlNotebooks.Width - pnlRight.Width;

  // Put splitter after left panel.
  if not gHorizontalFilePanels then
  begin
    MainSplitter.Left   := pnlLeft.Width;
    MainSplitter.Top    := pnlLeft.Top;
    MainSplitter.Height := pnlLeft.Height;
    MainSplitter.Width  := 3;
  end
  else
  begin
    MainSplitter.Top    := pnlLeft.Height;
    MainSplitter.Left   := pnlLeft.Left;
    MainSplitter.Width  := pnlLeft.Width;
    MainSplitter.Height := 3;
  end;
end;

procedure TfrmMain.pnlLeftRightDblClick(Sender: TObject);
var
  APanel: TPanel;
  APoint: TPoint;
  FileViewNotebook: TFileViewNotebook;
begin
  if Sender is TPanel then
  begin
    APanel := Sender as TPanel;
    if APanel = pnlLeft then
      begin
        APoint := FrameLeft.ClientToScreen(Classes.Point(0, FrameLeft.Top));
        if Mouse.CursorPos.Y < APoint.Y then Commands.DoNewTab(nbLeft);
      end
    else if APanel = pnlRight then
      begin
        APoint := FrameRight.ClientToScreen(Classes.Point(0, FrameRight.Top));
        if Mouse.CursorPos.Y < APoint.Y then Commands.DoNewTab(nbRight);
      end;
  end;
  if Sender is TFileViewNotebook then
  begin
    FileViewNotebook:= Sender as TFileViewNotebook;
    if FileViewNotebook.DoubleClickPageIndex < 0 then
      Commands.DoNewTab(FileViewNotebook)
    else
    begin
      case gDirTabActionOnDoubleClick of
        tadc_Nothing: begin end;
        tadc_CloseTab: Commands.DoCloseTab(FileViewNotebook, FileViewNotebook.DoubleClickPageIndex);
        tadc_FavoriteTabs: Commands.cm_LoadFavoriteTabs(['position=cursor']);
        tadc_TabsPopup:
          begin
            if FileViewNotebook.DoubleClickPageIndex<>-1 then
            begin
              // Check tab options items.
              case FileViewNotebook.Page[FileViewNotebook.DoubleClickPageIndex].LockState of
                tlsNormal: miTabOptionNormal.Checked := True;
                tlsPathLocked: miTabOptionPathLocked.Checked := True;
                tlsDirsInNewTab: miTabOptionDirsInNewTab.Checked := True;
                tlsPathResets: miTabOptionPathResets.Checked := True;
              end;
              pmTabMenu.Parent := FileViewNotebook;
              pmTabMenu.Tag := FileViewNotebook.DoubleClickPageIndex;
              pmTabMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
            end;
          end;
      end;
    end;
  end;
end;

procedure TfrmMain.pnlNotebooksResize(Sender: TObject);
begin
  if not FResizingFilePanels then
  begin
    FResizingFilePanels := True;
    if not gHorizontalFilePanels then
      pnlLeft.Width := Round(Double(pnlNotebooks.Width - MainSplitter.Width) * FMainSplitterPos / 100.0)
    else
      pnlLeft.Height := Round(Double(pnlNotebooks.Height - MainSplitter.Height) * FMainSplitterPos / 100.0);
    FResizingFilePanels := False;
  end;
end;

procedure TfrmMain.pnlRightResize(Sender: TObject);
begin
  if gDriveBar1 and not gHorizontalFilePanels then
  begin
    if gDriveBar2 then
      pnlDskRight.Width := pnlRight.Width + 1
    else
      pnlDskRight.Width := pnlNotebooks.Width - 2;
  end;
end;

procedure TfrmMain.sboxDrivePaint(Sender: TObject);
begin
  PaintDriveFreeBar(Sender, gIndUseGradient, gIndForeColor, gIndBackColor);
end;

procedure TfrmMain.PaintDriveFreeBar(Sender: TObject; bIndUseGradient:boolean; pIndForeColor,pIndBackColor:TColor);
var
  pbxDrive: TPaintBox absolute Sender;
  FillPercentage: PtrInt;
  i: Integer;
  AColor, AColor2: TColor;
  ARect: TRect;
begin
  FillPercentage:= pbxDrive.Tag;
  if FillPercentage <> -1 then
  begin
    pbxDrive.Canvas.Brush.Color:= clBlack;
    pbxDrive.Canvas.FrameRect(0, 0, pbxDrive.Width - 1, pbxDrive.Height - 1);

    ARect.Top    := 1;
    ARect.Bottom := pbxDrive.Height - 2;

    if not bIndUseGradient then
      begin
        ARect.Left  := 1;
        ARect.Right := 1 + FillPercentage * (pbxDrive.Width - 2) div 100;
        AColor := pIndForeColor;
        pbxDrive.Canvas.GradientFill(ARect, LightColor(AColor, 25), DarkColor(AColor, 25), gdVertical);
        ARect.Left  := ARect.Right + 1;
        ARect.Right := pbxDrive.Width - 2;
        AColor := pIndBackColor;
        pbxDrive.Canvas.GradientFill(ARect, DarkColor(AColor, 25), LightColor(AColor, 25), gdVertical);
      end
    else
      begin
        ARect.Right := 1;
        for i := 0 to FillPercentage - 1 do
        begin
          if i <= 50 then
            AColor:= RGB(0 + 5 * i, 255, 0)
          else
            AColor:= RGB(255, 255 - 5 * (i - 50), 0);
          AColor2:= DarkColor(AColor, 50);

          ARect.Left  := ARect.Right;
          ARect.Right := 1 + (i + 1) * (pbxDrive.Width - 2) div 100;

          pbxDrive.Canvas.GradientFill(ARect, AColor, AColor2, gdVertical);
        end;
        ARect.Left  := ARect.Right;
        ARect.Right := pbxDrive.Width - 2;
        pbxDrive.Canvas.GradientFill(ARect, clSilver, clWhite, gdVertical);
      end;
  end;
end;

procedure TfrmMain.seLogWindowSpecialLineColors(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
var
  LogMsgTypeObject: TObject;
  LogMsgType : TLogMsgType absolute LogMsgTypeObject;
begin
  LogMsgTypeObject := seLogWindow.Lines.Objects[Line-1];
  Special := True;
  case LogMsgType of
  lmtInfo:
    FG := gLogInfoColor;
  lmtSuccess:
    FG := gLogSuccessColor;
  lmtError:
    FG := gLogErrorColor
  else
    FG := clWindowText;
  end;
end;

function TfrmMain.FileViewBeforeChangePath(FileView: TFileView; NewFileSource: IFileSource; const NewPath: String): Boolean;
var
  ANoteBook: TFileViewNotebook;
  Page, NewPage: TFileViewPage;
  PageAlreadyExists: Boolean = False;
  i: Integer;
  tlsLockStateToEvaluate: TTabLockState;
begin
  Result:= True;
  if FileView.NotebookPage is TFileViewPage then
  begin
    Page := FileView.NotebookPage as TFileViewPage;

    tlsLockStateToEvaluate:=Page.LockState;
    if tlsLockStateToEvaluate=tlsPathLocked then
      if MsgBox(Format(rsMsgTabForOpeningInNewTab,[Page.Caption]), [msmbYes, msmbCancel], msmbCancel, msmbCancel) = mmrYes then
        tlsLockStateToEvaluate:=tlsDirsInNewTab;

    case tlsLockStateToEvaluate of
      tlsPathLocked:
        Result := False;  // do not change directory in this tab

      tlsDirsInNewTab:
        begin
          Result := False;  // do not change directory in this tab

          if Assigned(NewFileSource) then
          begin
            ANoteBook := Page.Notebook;

            if tb_reusing_tab_when_possible in gDirTabOptions then
            begin
              for i := 0 to ANotebook.PageCount - 1 do
              begin
                NewPage := ANotebook.Page[i];
                PageAlreadyExists := Assigned(NewPage.FileView) and
                  mbCompareFileNames(NewPage.FileView.CurrentPath, NewPath);
                if PageAlreadyExists then
                  Break;
              end;
            end;

            if not PageAlreadyExists then
            begin
              // Open in a new page, cloned view.
              NewPage := ANotebook.NewPage(Page.FileView);
              NewPage.FileView.AddFileSource(NewFileSource, NewPath);
            end;
            NewPage.MakeActive;
          end;
        end;
    end;
  end;
end;

procedure TfrmMain.FileViewAfterChangePath(FileView: TFileView);
var
  Index: Integer;
  Page: TFileViewPage;
  ANoteBook : TFileViewNotebook;
begin
  if FileView.NotebookPage is TFileViewPage then
    begin
      Page := FileView.NotebookPage as TFileViewPage;
      ANoteBook := Page.Notebook;

      if Page.IsActive then
      begin
        if Assigned(FileView.FileSource) then
        begin
          if FileView.FileSource.IsClass(TFileSystemFileSource) then
          begin
            // Store only first 255 items
            if glsDirHistory.Count > $FF then begin
              glsDirHistory.Delete(glsDirHistory.Count - 1);
            end;
            Index:= glsDirHistory.IndexOf(FileView.CurrentPath);
            if Index = -1 then
              glsDirHistory.Insert(0, FileView.CurrentPath)
            else begin
              glsDirHistory.Move(Index, 0);
            end;
            UpdateTreeViewPath;
          end;

          UpdateSelectedDrive(ANoteBook);
          UpdatePrompt;
        end;
        // Update page hint
        ANoteBook.Hint := FileView.CurrentPath;
      end;

      {if (fspDirectAccess in FileView.FileSource.GetProperties) then
        begin
          if gTermWindow and Assigned(Cons) then
            Cons.Terminal.SetCurrentDir(FileView.CurrentPath);
        end;}
    end;
end;

procedure TfrmMain.FileViewActivate(FileView: TFileView);
var
  Page: TFileViewPage;
begin
  if FileView.NotebookPage is TFileViewPage then
    begin
      Page := FileView.NotebookPage as TFileViewPage;
      SelectedPanel := Page.Notebook.Side;
      UpdateSelectedDrive(Page.Notebook);
      UpdateFreeSpace(Page.Notebook.Side);
    end;
end;

procedure TfrmMain.FileViewFilesChanged(FileView: TFileView);
var
  Page: TFileViewPage;
begin
  if FileView.NotebookPage is TFileViewPage then
    begin
      Page := FileView.NotebookPage as TFileViewPage;

      if Page.IsActive then
      begin
        UpdateFreeSpace(Page.Notebook.Side);
      end;
    end;
end;

procedure TfrmMain.SetActiveFrame(panel: TFilePanelSelect);
begin
  SelectedPanel:= panel;
  SetActiveFrame(ActiveFrame);
end;

procedure TfrmMain.SetActiveFrame(FileView: TFileView);
begin
  FileView.SetFocus;
  if (fspDirectAccess in FileView.FileSource.GetProperties) then
  begin
    if gTermWindow and Assigned(Cons) then
      Cons.Terminal.SetCurrentDir(FileView.CurrentPath);
  end;
end;

type
  TCustomShellTreeViewCrack = class(TCustomShellTreeView);

procedure TfrmMain.UpdateShellTreeView;
begin
  actTreeView.Checked := gSeparateTree;
  TreeSplitter.Visible := gSeparateTree;
  TreePanel.Visible := gSeparateTree;

  if gSeparateTree and (ShellTreeView = nil) then
  begin
    ShellTreeView := TShellTreeView.Create(TreePanel);
    ShellTreeView.Parent := TreePanel;
    ShellTreeView.Align := alClient;
    ShellTreeView.ScrollBars := ssAutoBoth;

    with ShellTreeView as TShellTreeView do
    begin
      ReadOnly := True;
      RightClickSelect := True;
      FileSortType := fstFoldersFirst;
      TCustomShellTreeViewCrack(ShellTreeView).PopulateWithBaseFiles;

      Images := TImageList.Create(Self);
      Images.Width := gIconsSize;
      Images.Height := gIconsSize;
      Images.Add(PixMapManager.GetFolderIcon(gIconsSize, ShellTreeView.Color), nil);

      OnKeyDown := @ShellTreeViewKeyDown;
      OnMouseUp := @ShellTreeViewMouseUp;
      OnAdvancedCustomDrawItem := @ShellTreeViewAdvancedCustomDrawItem;

      ExpandSignType := tvestPlusMinus;
      Options := Options - [tvoThemedDraw];
      Options := Options + [tvoReadOnly, tvoRightClickSelect];
    end;
  end;

  if gSeparateTree then
  begin
    ShellTreeView.Font.Color := gForeColor;
    ShellTreeView.BackgroundColor := gBackColor;
    ShellTreeView.SelectionColor := gCursorColor;
    FontOptionsToFont(gFonts[dcfMain], ShellTreeView.Font);
  end;
end;

procedure TfrmMain.UpdateTreeViewPath;
begin
  if (gSeparateTree = False) then Exit;
  if (ShellTreeView.Tag <> 0) then Exit;
  if (fspDirectAccess in ActiveFrame.FileSource.Properties) then
  try
    (ShellTreeView as TShellTreeView).Path := ActiveFrame.CurrentPath;
  except
    // Skip
  end;
end;

procedure TfrmMain.UpdateTreeView;
begin
  if (ShellTreeView = nil) then Exit;
  with (ShellTreeView as TShellTreeView) do
  begin
    if gShowSystemFiles then
      ObjectTypes:= ObjectTypes + [otHidden]
    else begin
      ObjectTypes:= ObjectTypes - [otHidden];
    end;
  end;
end;

function CompareDrives(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(PDrive(Item1)^.DisplayName, PDrive(Item2)^.DisplayName);
end;

procedure TfrmMain.UpdateDiskCount;
var
  I: Integer;
  Drive: PDrive;
begin
  DrivesList.Free;
  DrivesList := TDriveWatcher.GetDrivesList;
  DrivesList.Sort(@CompareDrives);

  { Delete drives that in drives black list }
  for I:= DrivesList.Count - 1 downto 0 do
    begin
      Drive := DrivesList[I];
      if (gDriveBlackListUnmounted and not Drive^.IsMounted) or
         MatchesMaskList(Drive^.Path, gDriveBlackList) or
         MatchesMaskList(Drive^.DeviceId, gDriveBlackList) then
        DrivesList.Remove(I);
    end;

  UpdateDriveList(DrivesList);

  // create drives drop down menu
  FDrivesListPopup.UpdateDrivesList(DrivesList);

  // create drives left/right panels
  if gDriveBar1 then
  begin
    CreateDiskPanel(dskRight);
    if gDriveBar2 then
      CreateDiskPanel(dskLeft);
  end;
  dskLeft.AddToolItemExecutor(TKASDriveItem, @LeftDriveBarExecuteDrive);
  dskRight.AddToolItemExecutor(TKASDriveItem, @RightDriveBarExecuteDrive);
end;

procedure TfrmMain.AddVirtualDriveButton(dskPanel: TKASToolBar);
const
  btnCaption = PathDelim + PathDelim;
var
  bmpBitmap: TBitmap;
  ToolItem: TKASNormalItem;
  Button: TKASToolButton;
begin
  (*virtual drive button*)
  ToolItem := TKASNormalItem.Create;
  ToolItem.Hint := StripHotkey(actOpenVirtualFileSystemList.Caption);
  ToolItem.Text := btnCaption;
  Button := dskPanel.AddButton(ToolItem);
  bmpBitmap:= PixMapManager.GetVirtualDriveIcon(dskPanel.GlyphSize, clBtnFace);
  try
    Button.Glyph.Assign(bmpBitmap);
    Button.GroupIndex := 0;
    Button.Layout := blGlyphLeft;
    Button.OnClick:= @btnVirtualDriveClick;
  finally
    bmpBitmap.Free;
  end;
end;

procedure TfrmMain.AddSpecialButtons(dskPanel: TKASToolBar);
  procedure AddItem(FromButton: TSpeedButton);
  var
    Button: TKASToolButton;
    ToolItem: TKASNormalItem;
  begin
    ToolItem := TKASNormalItem.Create;
    ToolItem.Text := FromButton.Caption;
    ToolItem.Hint := FromButton.Hint;
    Button := dskPanel.AddButton(ToolItem);
    Button.GroupIndex := 0;
  end;
begin
  AddItem(btnLeftRoot);
  AddItem(btnLeftUp);
  AddItem(btnLeftHome);
end;

procedure TfrmMain.CreateDiskPanel(dskPanel: TKASToolBar);
var
  I, Count: Integer;
  Drive : PDrive;
  BitmapTmp: Graphics.TBitmap;
  ToolItem: TKASDriveItem;
  Button: TKASToolButton;
begin
  dskPanel.BeginUpdate;
  try
    dskPanel.Clear;
    dskPanel.Flat := gDriveBarFlat;
    Count := DrivesList.Count - 1;

    for I := 0 to Count do
    begin
      Drive := DrivesList.Items[I];

      ToolItem := TKASDriveItem.Create;
      ToolItem.Drive := Drive;
      ToolItem.Text := Drive^.DisplayName;
      ToolItem.Hint := GetDriveLabelOrStatus(Drive);
      Button := dskPanel.AddButton(ToolItem);

      // Set drive icon.
      BitmapTmp := PixMapManager.GetDriveIcon(Drive, dskPanel.GlyphSize, clBtnFace);
      Button.Glyph.Assign(BitmapTmp);
      FreeAndNil(BitmapTmp);

      {Set Buttons Transparent. Is need? }
      Button.Glyph.Transparent := True;
      Button.Transparent := True;
      {/Set Buttons Transparent}
      Button.Layout := blGlyphLeft;
    end; // for

    // Add virtual drive button
    AddVirtualDriveButton(dskPanel);

    // Add special buttons
    if not gDrivesListButton then
      AddSpecialButtons(dskPanel);

  finally
    dskPanel.EndUpdate;
  end;
end;

function TfrmMain.CreateFileView(sType: String; Page: TFileViewPage; AConfig: TXmlConfig; ANode: TXmlNode): TFileView;
var
  FileViewFlags: TFileViewFlags = [];
begin
  // This function should be changed to a separate TFileView factory.

  if gDelayLoadingTabs then
    FileViewFlags := [fvfDelayLoadingFiles];
  if sType = 'columns' then
    Result := TColumnsFileView.Create(Page, AConfig, ANode, FileViewFlags)
  else if sType = 'brief' then
    Result := TBriefFileView.Create(Page, AConfig, ANode, FileViewFlags)
  else if sType = 'thumbnails' then
    Result := TThumbFileView.Create(Page, AConfig, ANode, FileViewFlags)
  else
    raise Exception.Create('Invalid file view type');
end;

procedure TfrmMain.AssignEvents(AFileView: TFileView);
begin
  with AFileView do
  begin
    OnBeforeChangePath := @FileViewBeforeChangePath;
    OnAfterChangePath := @FileViewAfterChangePath;
    OnActivate := @FileViewActivate;
    OnFileListChanged := @FileViewFilesChanged;
  end;
end;

//                         We ask the closing locked tab confirmation according to...
// ConfirmCloseLocked = 0  ...option "tb_confirm_close_locked_tab".
// ConfirmCloseLocked = 1  ...no matter the option, we ask confirmation
// ConfirmCloseLocked = 2  ...no matter the option, we do not ask confirmation
function TfrmMain.RemovePage(ANoteBook: TFileViewNotebook; iPageIndex:Integer; CloseLocked: Boolean = True; ConfirmCloseLocked: integer = 0; ShowButtonAll: Boolean = False): LongInt;
var
  UserAnswer: TMyMsgResult;
begin
  Result:= -1;
  if (ANoteBook.PageCount > 1) and
     (iPageIndex >= 0) and
     (iPageIndex < ANoteBook.PageCount) then
  begin
    if (ANoteBook.Page[iPageIndex].LockState <> tlsNormal) AND (((ConfirmCloseLocked=0) AND (tb_confirm_close_locked_tab in gDirTabOptions)) OR (ConfirmCloseLocked=1)) then
    begin
       if CloseLocked then
       begin
         if ShowButtonAll then
           UserAnswer := MsgBox(Format(rsMsgCloseLockedTab, [ANoteBook.Page[iPageIndex].Caption]), [msmbYes, msmbAll, msmbNo, msmbCancel], msmbYes, msmbCancel)
         else
           UserAnswer := MsgBox(Format(rsMsgCloseLockedTab, [ANoteBook.Page[iPageIndex].Caption]), [msmbYes, msmbNo, msmbCancel], msmbYes, msmbCancel);

         case UserAnswer of
          mmrNo:
            Exit(1);
          mmrCancel, mmrNone:
            Exit(2);
         end
       end
       else
         Exit(1);
    end;
    if Assigned(QuickViewPanel) then QuickViewClose;
    ANoteBook.RemovePage(iPageIndex);

    if UserAnswer=mmrAll then Result:=3 else Result:= 0;
  end;
end;

procedure TfrmMain.ReLoadTabs(ANoteBook: TFileViewNotebook);
var
  I : Integer;
begin
  for I := 0 to ANoteBook.PageCount - 1 do
  begin
    ANoteBook.View[I].UpdateView;
  end;
end;

procedure TfrmMain.LoadTabsXml(AConfig: TXmlConfig; ABranch:string; ANoteBook: TFileViewNotebook);
// default was ABranch: 'Tabs/OpenedTabs/'
var
  sPath, sViewType: String;
  iActiveTab: Integer;
  Page: TFileViewPage;
  AFileView: TFileView;
  AFileViewFlags: TFileViewFlags;
  aFileSource: IFileSource;
  RootNode, TabNode, ViewNode: TXmlNode;
begin
  RootNode := AConfig.FindNode(AConfig.RootNode,ABranch);

  if Assigned(RootNode) then
  begin
    TabNode := RootNode.FirstChild;
    while Assigned(TabNode) do
    begin
      if TabNode.CompareName('Tab') = 0 then
      begin
        Page := nil;
        AFileView := nil;

        ViewNode := AConfig.FindNode(TabNode, 'FileView', False);
        if Assigned(ViewNode) then
        begin
          // File view has its own configuration.
          if AConfig.TryGetAttr(ViewNode, 'Type', sViewType) then
          begin
            Page := ANoteBook.AddPage;
            Page.LoadConfiguration(AConfig, TabNode);
            AFileView := CreateFileView(sViewType, Page, AConfig, ViewNode);
          end
          else
            DCDebug('File view type not specified in configuration: ' + AConfig.GetPathFromNode(ViewNode) + '.');
        end
        // Else try old configuration.
        else if AConfig.TryGetValue(TabNode, 'Path', sPath) then
        begin
          sPath := GetDeepestExistingPath(sPath);
          if sPath <> EmptyStr then
          begin
            Page := ANoteBook.AddPage;
            Page.LoadConfiguration(AConfig, TabNode);
            AFileView := CreateFileView('columns', Page, AConfig, TabNode);
            AFileView.AddFileSource(TFileSystemFileSource.GetFileSource, sPath);
          end;
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(TabNode) + '.');

        if Assigned(Page) then
        begin
          if (not Assigned(AFileView)) or (AFileView.FileSourcesCount = 0) then
          begin
            ANoteBook.RemovePage(Page);
          end
          else
          begin
            if (Page.LockState in [tlsPathLocked, tlsPathResets, tlsDirsInNewTab]) and (Page.LockPath = '') then
              Page.LockPath := AFileView.CurrentPath;
            // Assign events after loading file source.
            AssignEvents(AFileView);
          end;
        end;
      end;
      TabNode := TabNode.NextSibling;
    end;
  end;

  // Create at least one tab.
  if ANoteBook.PageCount = 0 then
  begin
    Page := ANoteBook.AddPage;
    aFileSource := TFileSystemFileSource.GetFileSource;
    if gDelayLoadingTabs then
      AFileViewFlags := [fvfDelayLoadingFiles]
    else
      AFileViewFlags := [];
    AFileView := TColumnsFileView.Create(Page, aFileSource, gpExePath, AFileViewFlags);
    AssignEvents(AFileView);
  end
  else if Assigned(RootNode) then
  begin
    // read active tab index
    iActiveTab := AConfig.GetValue(RootNode, 'ActiveTab', 0);
    // set active tab
    if (iActiveTab >= 0) and (iActiveTab < ANoteBook.PageCount) then
    begin
      if ANoteBook.PageIndex = iActiveTab then
        nbPageChanged(ANoteBook)
      else
        ANoteBook.PageIndex := iActiveTab;
    end;
  end;
end;


procedure TfrmMain.SaveTabsXml(AConfig: TXmlConfig;ABranch:string; ANoteBook: TFileViewNotebook; ASaveHistory:boolean);
// default was: 'Tabs/OpenedTabs'
var
  I: Integer;
  TabsSection: String;
  Page: TFileViewPage;
  RootNode, TabNode, ViewNode: TXmlNode;
begin
  RootNode := AConfig.FindNode(AConfig.RootNode, ABranch, True);
  if ANoteBook = nbLeft then
    TabsSection := 'Left'
  else
    TabsSection := 'Right';
  RootNode := AConfig.FindNode(RootNode, TabsSection, True);

  AConfig.ClearNode(RootNode);
  AConfig.AddValue(RootNode, 'ActiveTab', ANoteBook.PageIndex);

  for I:= 0 to ANoteBook.PageCount - 1 do
    begin
      TabNode := AConfig.AddNode(RootNode, 'Tab');
      ViewNode := AConfig.AddNode(TabNode, 'FileView');

      Page := ANoteBook.Page[I];
      Page.SaveConfiguration(AConfig, TabNode);
      Page.FileView.SaveConfiguration(AConfig, ViewNode, ASaveHistory);
    end;
end;

{ TfrmMain.LoadTheseTabsWithThisConfig }
// Will have tabs section from Xml from either left or right and store it back on actual form on left, right, active, inactive, both or none, with setting keep or not etc.
// The "ABranch" *must* include the trailing slash when calling here.
procedure TfrmMain.LoadTheseTabsWithThisConfig(Config: TXmlConfig; ABranch:string; Source, Destination:TTabsConfigLocation; DestinationToKeep : TTabsConfigLocation; var TabsAlreadyDestroyedFlags:TTabsFlagsAlreadyDestroyed);
var
  sSourceSectionName: string;
  CheckNode: TXmlNode;
begin
  if Destination<>tclNone then
  begin
    // 1. Normalize our destination side and destination to keep in case params specified active/inactive
    if ((Destination=tclActive) and (ActiveFrame=FrameLeft)) OR ((Destination=tclInactive) and (NotActiveFrame=FrameLeft)) then Destination:=tclLeft;
    if ((Destination=tclActive) and (ActiveFrame=FrameRight)) OR ((Destination=tclInactive) and (NotActiveFrame=FrameRight)) then Destination:=tclRight;
    if ((DestinationToKeep=tclActive) and (ActiveFrame=FrameLeft)) OR ((DestinationToKeep=tclInactive) and (NotActiveFrame=FrameLeft)) then DestinationToKeep:=tclLeft;
    if ((DestinationToKeep=tclActive) and (ActiveFrame=FrameRight)) OR ((DestinationToKeep=tclInactive) and (NotActiveFrame=FrameRight)) then DestinationToKeep:=tclRight;

    // 2. Setup our source section name.
    case Source of
      tclLeft: sSourceSectionName := 'Left';
      tclRight: sSourceSectionName := 'Right';
    end;

    // 3. Actual load infos from config file.
    if (Destination=tclLeft) OR (Destination=tclBoth) then
    begin
      CheckNode := Config.FindNode(Config.RootNode, ABranch + sSourceSectionName);
      if Assigned(CheckNode) then
      begin
        if (DestinationToKeep<>tclLeft) AND (DestinationToKeep<>tclBoth) AND (not(tfadLeft in TabsAlreadyDestroyedFlags)) then
        begin
          frmMain.LeftTabs.DestroyAllPages;
          TabsAlreadyDestroyedFlags := TabsAlreadyDestroyedFlags + [tfadLeft]; // To don't delete it twice in case both target are left.
        end;
      end;
      LoadTabsXml(Config, ABranch + sSourceSectionName, LeftTabs);
    end;

    if (Destination=tclRight) OR (Destination=tclBoth) then
    begin
      CheckNode := Config.FindNode(Config.RootNode, ABranch + sSourceSectionName);
      if Assigned(CheckNode) then
      begin
        if (DestinationToKeep<>tclRight) AND (DestinationToKeep<>tclBoth) AND (not(tfadRight in TabsAlreadyDestroyedFlags)) then
          begin
            frmMain.RightTabs.DestroyAllPages;
            TabsAlreadyDestroyedFlags := TabsAlreadyDestroyedFlags + [tfadRight]; // To don't delete it twice in case both target are right.
          end;
        LoadTabsXml(Config, ABranch + sSourceSectionName, RightTabs);
      end;
    end;

    // 4. Refresh content of tabs.
    case Destination of
      tclLeft: FrameLeft.Flags := FrameLeft.Flags - [fvfDelayLoadingFiles];
      tclRight: FrameRight.Flags := FrameRight.Flags - [fvfDelayLoadingFiles];
      tclBoth:
        begin
          FrameLeft.Flags := FrameLeft.Flags - [fvfDelayLoadingFiles];
          FrameRight.Flags := FrameRight.Flags - [fvfDelayLoadingFiles];
        end;
    end;
  end; // if Destination<>tclNone then
end;

procedure TfrmMain.OnCmdBoxInput(ACmdBox: TCmdBox; AInput: String);
begin
  Cons.Terminal.Write_pty(AInput + LineEnding);
  ACmdBox.StartRead(clWhite, clBlack, ACmdBox.Hint, clWhite, clBlack);
end;

procedure TfrmMain.ToggleConsole;
begin
  if gTermWindow then
    begin
      if not Assigned(cmdConsole) then
      begin
        cmdConsole:= TCmdBox.Create(pgConsole);
        cmdConsole.Parent:= pgConsole;
        cmdConsole.Align:= alClient;
        cmdConsole.AutoFollow:= True;
        cmdConsole.LineCount:= 256;
        cmdConsole.ShowHint:= False;
        cmdConsole.CaretType:= cartSubBar;
        cmdConsole.OnInput:= @OnCmdBoxInput;
        ShowScrollBar(cmdConsole.Handle, SB_Horz, False);
        FontOptionsToFont(gFonts[dcfConsole], cmdConsole.Font);
        cmdConsole.Hint:= Format(fmtCommandPath, [GetComputerNetName]);
      end;
      if gCmdLine then
      begin
        cmdConsole.Tag := 0;
        cmdConsole.StopRead;
      end
      else if cmdConsole.Tag = 0 then
      begin
        cmdConsole.Tag := MaxInt;
        cmdConsole.Writeln(EmptyStr);
        cmdConsole.StartRead(clWhite, clBlack, cmdConsole.Hint, clWhite, clBlack);
      end;
      if not Assigned(Cons) then
        begin
          Cons:= CreateConsoleThread;
          Cons.ColsCount:= 80;
          Cons.RowsCount:= cmdConsole.LineCount;
          Cons.CmdBox:= cmdConsole;
          Cons.Start;
        end;
    end
  else
    begin
      if Assigned(cmdConsole) then
      begin
        cmdConsole.Hide;
        FreeAndNil(cmdConsole);
      end;
      FreeAndNil(Cons);
    end;

  nbConsole.Visible:= gTermWindow;
  ConsoleSplitter.Visible:= gTermWindow;
end;

procedure TfrmMain.ToggleFullscreenConsole;
begin
  if  nbConsole.Height < (nbConsole.Height + pnlNotebooks.Height - 1) then
    begin
      nbConsole.Height := nbConsole.Height + pnlNotebooks.Height;
      if (not gCmdLine) and cmdConsole.CanFocus then cmdConsole.SetFocus;
    end
  else
    begin
      nbConsole.Height := 0;
      if (not gCmdLine) and ActiveFrame.CanFocus then ActiveFrame.SetFocus;
    end;
end;

procedure TfrmMain.ToolbarExecuteCommand(ToolItem: TKASToolItem);
var
  CommandItem: TKASCommandItem;
  CommandFuncResult: TCommandFuncResult;
begin
  if not Draging then
  begin
    CommandItem := ToolItem as TKASCommandItem;
    CommandFuncResult:=Commands.Commands.ExecuteCommand(CommandItem.Command, CommandItem.Params);
    if gToolbarReportErrorWithCommands AND (CommandFuncResult=cfrNotFound) then
    begin
      MsgError('Command not found! ('+CommandItem.Command+')');
    end;
  end;
  Draging := False;
end;

procedure TfrmMain.ToolbarExecuteProgram(ToolItem: TKASToolItem);
var
  ProgramItem: TKASProgramItem;
  CommandExecResult: boolean;
begin
  if not Draging then
  begin
    ProgramItem := ToolItem as TKASProgramItem;
    CommandExecResult:=ProcessExtCommandFork(ProgramItem.Command, ProgramItem.Params, ProgramItem.StartPath);
    if gToolbarReportErrorWithCommands AND (CommandExecResult=FALSE) then
      MsgError(Format(rsMsgProblemExecutingCommand,[ProgramItem.Command]));
  end;
  Draging := False;
end;

procedure TfrmMain.UpdateWindowView;

  procedure UpdateNoteBook(NoteBook: TFileViewNotebook);
  var
    I: Integer;
  begin
    NoteBook.ShowTabs := ((NoteBook.PageCount > 1) or (tb_always_visible in gDirTabOptions)) and gDirectoryTabs;

    if tb_show_close_button in gDirTabOptions then
      begin
        NoteBook.Options := NoteBook.Options + [nboShowCloseButtons];
      end
    else
      begin
        NoteBook.Options := NoteBook.Options - [nboShowCloseButtons];
      end;

    if nbcMultiline in NoteBook.GetCapabilities then
      NoteBook.MultiLine := tb_multiple_lines in gDirTabOptions;

    case gDirTabPosition of
      tbpos_top:    NoteBook.TabPosition := tpTop;
      tbpos_bottom: NoteBook.TabPosition := tpBottom;
      else          NoteBook.TabPosition := tpTop;
    end;

    if FInitializedView then
    begin
      for I := 0 to NoteBook.PageCount - 1 do  //  change on all tabs
        NoteBook.View[I].UpdateView;
    end;
  end;

  procedure AnchorHorizontalBetween(AControl, ALeftSibling, ARightSibling: TControl);
  begin
    AControl.Anchors := AControl.Anchors + [akLeft, akRight];
    AControl.AnchorSide[akLeft].Control   := ALeftSibling;
    AControl.AnchorSide[akLeft].Side      := asrRight;
    AControl.AnchorSide[akRight].Control  := ARightSibling;
    AControl.AnchorSide[akRight].Side     := asrLeft;
  end;

  procedure AnchorHorizontal(AControl, ASibling: TControl);
  begin
    AControl.Anchors := AControl.Anchors + [akLeft, akRight];
    AControl.AnchorSide[akLeft].Control   := ASibling;
    AControl.AnchorSide[akLeft].Side      := asrLeft;
    AControl.AnchorSide[akRight].Control  := ASibling;
    AControl.AnchorSide[akRight].Side     := asrRight;
  end;

  procedure AnchorFreeSpace(LeftControl, RightControl: TControl; ExcludeVert: Boolean);
  begin
    if gDrivesListButton then
    begin
      AnchorHorizontalBetween(LeftControl, btnLeftDrive, btnLeftDirectoryHotlist);
      AnchorHorizontalBetween(RightControl, btnRightDrive, btnRightDirectoryHotlist);

      if not ExcludeVert then
      begin
        LeftControl.AnchorVerticalCenterTo(pnlLeftTools);
        RightControl.AnchorVerticalCenterTo(pnlRightTools);
      end;
    end
    else
    begin
      AnchorHorizontal(LeftControl, pnlLeftTools);
      AnchorHorizontal(RightControl, pnlRightTools);

      if not ExcludeVert then
      begin
        LeftControl.AnchorSide[akTop].Control  := pnlLeftTools;
        LeftControl.AnchorSide[akTop].Side     := asrTop;
        RightControl.AnchorSide[akTop].Control := pnlRightTools;
        RightControl.AnchorSide[akTop].Side    := asrTop;
      end;
    end;
  end;

var
  I: Integer;
  HMForm: THMForm;
  FunButton: TSpeedButton;
  Hotkey: THotkey;
begin
  DisableAutoSizing;
  try
    if gHorizontalFilePanels then
    begin
      pnlLeft.Align := alTop;
      pnlLeft.BorderSpacing.Right  := 0;
      pnlLeft.BorderSpacing.Bottom := 3;
      MainSplitter.Cursor := crVSplit;
    end
    else
    begin
      pnlLeft.Align := alLeft;
      pnlLeft.BorderSpacing.Right  := 3;
      pnlLeft.BorderSpacing.Bottom := 0;
      MainSplitter.Cursor := crHSplit;
    end;
    pnlLeftResize(pnlLeft);
    pnlNotebooksResize(pnlNotebooks);

    (* Disk Panels *)
    if gHorizontalFilePanels and gDriveBar1 and gDriveBar2 then
    begin
      dskLeft.Parent := pnlDiskLeftInner;
      dskRight.Parent := pnlDiskRightInner;
    end
    else
    begin
      dskLeft.Parent := pnlDskLeft;
      dskRight.Parent := pnlDskRight;
    end;

    dskLeft.GlyphSize:= gDiskIconsSize;
    dskRight.GlyphSize:= gDiskIconsSize;
    dskLeft.ButtonHeight:= gDiskIconsSize + 6;
    dskRight.ButtonHeight:= gDiskIconsSize + 6;

    pnlDiskLeftInner.Visible := gHorizontalFilePanels and gDriveBar1 and gDriveBar2;
    pnlDiskRightInner.Visible := gHorizontalFilePanels and gDriveBar1 and gDriveBar2;
    pnlDskLeft.Visible := not gHorizontalFilePanels and gDriveBar1 and gDriveBar2;
    pnlDskRight.Visible := gDriveBar1 and (not gHorizontalFilePanels or not gDriveBar2);
    pnlDisk.Visible := pnlDskLeft.Visible or pnlDskRight.Visible;

    // Create disk panels after assigning parent.
    UpdateDiskCount; // Update list of showed drives
    (*/ Disk Panels *)

    FDrivesListPopup.UpdateView;

    (*Main menu*)
    Commands.DoShowMainMenu(gMainMenu);

    (*Tool Bar*)
    MainToolBar.Visible:= gButtonBar;
    MainToolBar.Flat:= gToolBarFlat;
    MainToolBar.GlyphSize:= gToolBarIconSize;
    MainToolBar.ShowCaptions:= gToolBarShowCaptions;
    MainToolBar.SetButtonSize(gToolBarButtonSize, gToolBarButtonSize);
    MainToolBar.ChangePath:= gpExePath;
    MainToolBar.EnvVar:= '%commander_path%';
    LoadMainToolbar;

    btnLeftDrive.Visible := gDrivesListButton;
    btnLeftDrive.Flat := gInterfaceFlat;
    btnLeftRoot.Visible := gDrivesListButton;
    btnLeftRoot.Flat := gInterfaceFlat;
    btnLeftUp.Visible := gDrivesListButton;
    btnLeftUp.Flat := gInterfaceFlat;
    btnLeftHome.Visible := gDrivesListButton;
    btnLeftHome.Flat := gInterfaceFlat;
    btnLeftDirectoryHotlist.Visible := gDrivesListButton;
    btnLeftDirectoryHotlist.Flat := gInterfaceFlat;
    btnLeftEqualRight.Visible := gDrivesListButton;
    btnLeftEqualRight.Flat:= gInterfaceFlat;
    lblLeftDriveInfo.Visible:= gDriveFreeSpace;
    pbxLeftDrive.Visible := gDriveInd;
    pnlLeftTools.Visible:= gDrivesListButton or gDriveFreeSpace or gDriveInd;
    pnlLeftTools.DoubleBuffered := True;

    btnRightDrive.Visible := gDrivesListButton;
    btnRightDrive.Flat := gInterfaceFlat;
    btnRightRoot.Visible := gDrivesListButton;
    btnRightRoot.Flat := gInterfaceFlat;
    btnRightUp.Visible := gDrivesListButton;
    btnRightUp.Flat := gInterfaceFlat;
    btnRightHome.Visible := gDrivesListButton;;
    btnRightHome.Flat := gInterfaceFlat;
    btnRightDirectoryHotlist.Visible := gDrivesListButton;
    btnRightDirectoryHotlist.Flat := gInterfaceFlat;
    btnRightEqualLeft.Visible := gDrivesListButton;
    btnRightEqualLeft.Flat:= gInterfaceFlat;
    lblRightDriveInfo.Visible:= gDriveFreeSpace;
    pbxRightDrive.Visible := gDriveInd;
    pnlRightTools.Visible:= gDrivesListButton or gDriveFreeSpace or gDriveInd;
    pnlRightTools.DoubleBuffered := True;

    // Free space indicator.
    if gDriveFreeSpace then
    begin
      AnchorFreeSpace(lblLeftDriveInfo, lblRightDriveInfo, gDriveInd);
      if gDriveInd then
      begin
        lblLeftDriveInfo.AnchorSide[akTop].Side  := asrTop;
        lblRightDriveInfo.AnchorSide[akTop].Side := asrTop;
      end;
    end;

    // Drive free space indicator
    if gDriveInd then
    begin
      AnchorFreeSpace(pbxLeftDrive, pbxRightDrive, gDriveFreeSpace);
      if gDriveFreeSpace then
      begin
        pbxLeftDrive.AnchorSide[akTop].Control  := lblLeftDriveInfo;
        pbxLeftDrive.AnchorSide[akTop].Side     := asrBottom;
        pbxRightDrive.AnchorSide[akTop].Control := lblRightDriveInfo;
        pbxRightDrive.AnchorSide[akTop].Side    := asrBottom;
      end;
    end;

    // Separate tree
    UpdateShellTreeView;

    // Operations panel and menu
    if (gPanelOfOp = False) then
      FreeAndNil(FOperationsPanel)
    else if (FOperationsPanel = nil) then
    begin
      FOperationsPanel := TOperationsPanel.Create(Self);
      FOperationsPanel.Parent := PanelAllProgress;
      FOperationsPanel.DoubleBuffered := True;
      PanelAllProgress.OnResize := @FOperationsPanel.ParentResized;
    end;
    PanelAllProgress.Visible := gPanelOfOp;
    Timer.Enabled := gPanelOfOp or gProgInMenuBar;

    // Log window
    seLogWindow.Visible := gLogWindow;
    LogSplitter.Visible := gLogWindow;
    // Align log window
    seLogWindow.Top := 0;
    LogSplitter.Top := 0;
    FontOptionsToFont(gFonts[dcfLog], seLogWindow.Font);

    // Command line
    pnlCmdLine.Visible := gCmdLine;
    pnlCommand.Visible := gCmdLine or gTermWindow;

    // Align command line and terminal window
    pnlCommand.Top := -Height;
    ConsoleSplitter.Top:= -Height;
    ToggleConsole;

    // Function keys
    pnlKeys.Visible := gKeyButtons;
    if gKeyButtons then
    begin
      pnlKeys.Top:= Height * 2;
      HMForm := HotMan.Forms.Find('Main');
      for I := 0 to pnlKeys.ControlCount - 1 do
      begin
        if pnlKeys.Controls[I] is TSpeedButton then
        begin
          FunButton := pnlKeys.Controls[I] as TSpeedButton;
          FunButton.Flat := gInterfaceFlat;
          if Assigned(HMForm) then
          begin
            Hotkey := HMForm.Hotkeys.FindByCommand(FunctionButtonsCaptions[I].ACommand);
            if Assigned(Hotkey) then
              FunButton.Caption := FunctionButtonsCaptions[I].ACaption + ' ' + ShortcutsToText(Hotkey.Shortcuts);
          end;
        end;
      end;
      UpdateGUIFunctionKeys;
    end;

    UpdateNoteBook(nbLeft);
    UpdateNoteBook(nbRight);

    if FInitializedView then
    begin
      UpdateSelectedDrives;
      UpdateFreeSpace(fpLeft);
      UpdateFreeSpace(fpRight);
    end;

    UpdateHotDirIcons; // Preferable to be loaded even if not required in popupmenu *because* in the tree it's a must, especially when checking for missing directories
    ShowTrayIcon(gAlwaysShowTrayIcon);

    FInitializedView := True;
  finally
    EnableAutoSizing;
  end;
end;

procedure TfrmMain.edtCommandKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not edtCommand.DroppedDown and ((Key=VK_UP) or (Key=VK_DOWN)) then
    begin
      ActiveFrame.SetFocus;
      Key:= 0;
    end
  else if edtCommand.DroppedDown and (Key in [VK_RETURN, VK_SELECT, VK_ESCAPE]) then
    begin
      edtCommand.DroppedDown := False;
      Key := 0;
    end
  else
    case Key of
      VK_ESCAPE:
        begin
          if edtCommand.Text <> '' then
            edtCommand.Text := ''
          else
            ActiveFrame.SetFocus;
          Key := 0;
        end;

      VK_RETURN, VK_SELECT:
        begin
          if (Shift * [ssCtrl, ssAlt, ssMeta, ssAltGr] = []) then
          begin
            ExecuteCommandLine(ssShift in Shift);
            Key := 0;
          end;
        end;

      VK_TAB:
        begin
          ActiveFrame.SetFocus;
          Key := 0;
        end;

      VK_PAUSE:
        begin
          if gTermWindow and Assigned(Cons) then
            Cons.Terminal.SendBreak_pty();
        end;
    end;

  CheckCommandLine(GetKeyShiftStateEx, Key);
end;

procedure TfrmMain.edtCommandExit(Sender: TObject);
begin
  // Hide command line if it was temporarily shown.
  if (not gCmdLine) and IsCommandLineVisible and (edtCommand.Text = '') then
  begin
    pnlCmdLine.Visible := False;
    pnlCommand.Visible := gTermWindow;
  end;
end;

procedure TfrmMain.tbChangeDirClick(Sender: TObject);
var
  sDir: String;
  ToolItem: TKASToolItem;
  Button: TKASToolButton;
begin
  Button := TKASToolButton(pmToolBar.Tag);
  if Assigned(Button) then
  begin
    ToolItem := Button.ToolItem;
    if ToolItem is TKASProgramItem then
    begin
      sDir := TKASProgramItem(ToolItem).StartPath;
      sDir := PrepareParameter(sDir, nil, [ppoNormalizePathDelims, ppoReplaceTilde]);
      Commands.cm_ChangeDir([sDir]);
    end;
  end;
end;

procedure TfrmMain.tbCopyClick(Sender: TObject);
var
  ToolItem: TKASToolItem;
  ItemClone: TKASToolItem = nil;
  Serializer: TKASToolBarSerializer = nil;
  Stream: TStringStream = nil;
  Button: TKASToolButton;
begin
  Button := TKASToolButton(pmToolBar.Tag);
  if Assigned(Button) then
  try
    ToolItem := Button.ToolItem;

    // Create a copy so that ID of the button is different.
    if Sender = tbCopy then
    begin
      ItemClone := ToolItem.Clone;
      ToolItem := ItemClone;
    end;

    Stream := TStringStream.Create('');
    Stream.WriteString(DCToolItemClipboardHeader);
    Serializer := TKASToolBarSerializer.Create;
    Serializer.Serialize(Stream, ToolItem);
    Clipboard.SetFormat(PredefinedClipboardFormat(pcfText), Stream);

    if Sender = tbCut then
      MainToolBar.RemoveButton(Button);

    SaveMainToolBar;
  finally
    ItemClone.Free;
    Serializer.Free;
    Stream.Free;
  end;
end;

procedure TfrmMain.tbEditClick(Sender: TObject);
begin
  EditToolbarButton(TKASToolButton(pmToolBar.Tag));
end;

procedure TfrmMain.OnUniqueInstanceMessage(Sender: TObject; Params: TCommandLineParams);
begin
  RestoreWindow;
  LoadTabsCommandLine(Params);
end;

procedure TfrmMain.tbPasteClick(Sender: TObject);
var
  Data: TStringList = nil;
  ProgramItem: TKASProgramItem;
  ToolItem: TKASToolItem;
  Loader: TKASToolBarLoader = nil;
  Serializer: TKASToolBarSerializer = nil;
  Stream: TStringStream = nil;
  Pasted: Boolean = False;
  Button: TKASToolButton;
begin
  Stream := TStringStream.Create('');
  if Clipboard.GetFormat(PredefinedClipboardFormat(pcfText), Stream) then
  try
    Button := TKASToolButton(pmToolBar.Tag);

    // Cut any trailing zeros.
    while Stream.DataString[Length(Stream.DataString)] = #0 do
      Stream.Size := Stream.Size - 1;

    if StrBegins(Stream.DataString, TCToolbarClipboardHeader) or
       StrBegins(Stream.DataString, DCToolbarClipboardHeader) then
    begin
      Data:= TStringList.Create;
      Data.Text:= Stream.DataString;
      if Data.Count < 6 then Exit;
      if (Data[0] = TCToolbarClipboardHeader) or
         (Data[0] = DCToolbarClipboardHeader) then
      begin
        ProgramItem := TKASProgramItem.Create;
        ProgramItem.Command   := Data[1];
        ProgramItem.Params    := Data[2];
        ProgramItem.Icon      := Data[3];
        ProgramItem.Hint      := Data[4];
        ProgramItem.StartPath := Data[5];
        MainToolBar.InsertButton(Button, ProgramItem);
        SaveMainToolBar;
        Pasted := True;
      end;
    end
    else if StrBegins(Stream.DataString, DCToolItemClipboardHeader) then
    begin
      Stream.Position := Length(DCToolItemClipboardHeader);
      Serializer := TKASToolBarSerializer.Create;
      Loader := TKASToolBarExtendedLoader.Create(Commands.Commands);
      try
        ToolItem := Serializer.Deserialize(Stream, Loader);
        MainToolBar.InsertButton(Button, ToolItem);
        SaveMainToolBar;
        Pasted := True;
      except
        on EXMLReadError do;
      end;
    end;

    if not Pasted then
      MessageDlg(Application.Title, rsClipboardContainsInvalidToolbarData, mtWarning, [mbOK], 0);

  finally
    Data.Free;
    Loader.Free;
    Serializer.Free;
    Stream.Free;
  end;
end;

procedure TfrmMain.CheckCommandLine(ShiftEx: TShiftState; var Key: Word);
var
  ModifierKeys: TShiftState;
  UTF8Char: TUTF8Char;
  KeyTypingModifier: TKeyTypingModifier;
begin
  for KeyTypingModifier in TKeyTypingModifier do
  begin
    if gKeyTyping[KeyTypingModifier] = ktaCommandLine then
    begin
      ModifierKeys := TKeyTypingModifierToShift[KeyTypingModifier];
      if ((ModifierKeys <> []) and
         (ShiftEx * KeyModifiersShortcutNoText = ModifierKeys))
{$IFDEF MSWINDOWS}
      // Allow entering international characters with Ctrl+Alt on Windows,
      // if there is no action for Ctrl+Alt and command line typing has no modifiers.
      or (HasKeyboardAltGrKey and
          (ShiftEx * KeyModifiersShortcutNoText = [ssCtrl, ssAlt]) and
          (gKeyTyping[ktmCtrlAlt] = ktaNone) and
          (gKeyTyping[ktmNone] = ktaCommandLine))
{$ENDIF}
      then
      begin
        if (Key <> VK_SPACE) or (edtCommand.Text <> '') then
        begin
          UTF8Char := VirtualKeyToUTF8Char(Key, ShiftEx - ModifierKeys);
          if (UTF8Char <> '') and
             (not ((Length(UTF8Char) = 1) and (UTF8Char[1] in [#0..#31]))) then
          begin
            TypeInCommandLine(UTF8Char);
            Key := 0;
          end;
        end;
      end;
      Break;
    end;
  end;
end;

function TfrmMain.ExecuteCommandFromEdit(sCmd: String; bRunInTerm: Boolean): Boolean;
var
  iIndex: Integer;
  sDir, sParams: String;
  sFilename: String = '';
  Operation: TFileSourceExecuteOperation = nil;
  aFile: TFile = nil;
begin
  Result:= True;

  InsertFirstItem(sCmd, edtCommand);
  // only cMaxStringItems(see uGlobs.pas) is stored
  if edtCommand.Items.Count>cMaxStringItems then
    edtCommand.Items.Delete(edtCommand.Items.Count-1);
  edtCommand.DroppedDown:= False;

  if (fspDirectAccess in ActiveFrame.FileSource.GetProperties) then
    begin
      iIndex:= Pos('cd ', sCmd);
      if (iIndex = 1) or (sCmd = 'cd') then
        begin
          sCmd:= ReplaceEnvVars(sCmd);

          if (iIndex <> 1) then
            sDir:= GetHomeDir
          else
            begin
              sDir:= RemoveQuotation(Copy(sCmd, iIndex + 3, Length(sCmd)));
              sDir:= NormalizePathDelimiters(Trim(sDir));

              if (sDir = DirectorySeparator) or (sDir = '..') then
              begin
                if (sDir = DirectorySeparator) then
                  Commands.DoChangeDirToRoot(ActiveFrame)
                else begin
                  ActiveFrame.ChangePathToParent(True);
                end;
                Exit;
              end;

              sDir:= ReplaceTilde(sDir);
              sDir:= GetAbsoluteFileName(ActiveFrame.CurrentPath, sDir);
              if mbFileExists(sDir) then //if user entered an existing file, let's switch to the parent folder AND select that file
              begin
                sFilename:= ExtractFileName(sDir);
                sDir:= ExtractFileDir(sDir);
              end;
            end;

          // Choose FileSource by path
          ChooseFileSource(ActiveFrame, sDir, True);
          if sFilename <> '' then
            ActiveFrame.SetActiveFile(sFilename);

          if SameText(ExcludeBackPathDelimiter(ActiveFrame.CurrentPath), sDir) then
            begin
              if gTermWindow and Assigned(Cons) then
                Cons.Terminal.SetCurrentDir(sDir);
            end;
        end
      else
        begin
          {$IFDEF MSWINDOWS}
          //Let's see if user typed something like "c:", "X:", etc. and if so, switch active panel to that drive (like TC)
          if (length(sCmd)=2) AND (pos(':',sCmd)=2) then
          begin
            iIndex:=0;

            while (iIndex<DrivesList.Count) AND (sCmd<>'') do
            begin
              if lowercase(sCmd[1]) = lowercase(PDrive(DrivesList.Items[iIndex])^.DisplayName) then
              begin
                SetPanelDrive(PanelSelected, DrivesList.Items[iIndex],false);
                sCmd:='';
              end;
              inc(iIndex);
            end;
          end;
          {$ENDIF}

          if sCmd<>'' then
          begin
            if gTermWindow and Assigned(Cons) then
            begin
              sCmd:= ReplaceEnvVars(sCmd);
              Cons.Terminal.Write_pty(sCmd + sLineBreak)
            end
            else
            begin
              try
                SplitCmdLineToCmdParams(sCmd, sCmd, sParams); //TODO:Hum...
                ProcessExtCommandFork(sCmd, sParams, ActiveFrame.CurrentPath, nil, bRunInTerm, bRunInTerm);
              except
                on e: EInvalidCommandLine do
                  MessageDlg(rsMsgInvalidCommandLine, rsMsgInvalidCommandLine + ': ' + e.Message, mtError, [mbOK], 0);
              end;
            end;
          end;
      end;
    end
  else
    begin
      aFile:= ActiveFrame.CloneActiveFile;
      if Assigned(aFile) then
        try
          sCmd:= 'quote' + #32 + sCmd;
          aFile.FullPath:= ActiveFrame.CurrentPath;
          Operation:= ActiveFrame.FileSource.CreateExecuteOperation(
                                           aFile,
                                           ActiveFrame.CurrentPath,
                                           sCmd) as TFileSourceExecuteOperation;
          if Assigned(Operation) then
            Operation.Execute;
        finally
          FreeThenNil(aFile);
          FreeThenNil(Operation);
        end;
    end;
end;

procedure TfrmMain.SetMainSplitterPos(AValue: Double);
begin
  if (AValue >= 0) and (AValue <= 100) then
    FMainSplitterPos:= AValue;
end;

procedure TfrmMain.SetPanelSelected(AValue: TFilePanelSelect);
begin
  if PanelSelected = AValue then Exit;
  PanelSelected := AValue;
  UpdateTreeViewPath;
  UpdatePrompt;
end;

procedure TfrmMain.TypeInCommandLine(Str: String);
begin
  Commands.cm_FocusCmdLine([]);
  edtCommand.Text := edtCommand.Text + Str;
  edtCommand.SelStart := UTF8Length(edtCommand.Text) + 1;
end;

//LaBero begin
//Minimize the main window
procedure TfrmMain.MinimizeWindow;
begin
  Self.WindowState := wsMinimized;
end;
//LaBero end

procedure TfrmMain.RestoreWindow;
begin
  if HiddenToTray then
    RestoreFromTray
  else
  begin
    WindowState:= lastWindowState;
    BringToFront;
  end;
end;

procedure TfrmMain.LoadTabs;
begin
  LoadTabsXml(gConfig,'Tabs/OpenedTabs/Left', nbLeft);
  LoadTabsXml(gConfig,'Tabs/OpenedTabs/Right', nbRight);

  if not CommandLineParams.ActivePanelSpecified then
  begin
    CommandLineParams.ActivePanelSpecified:= True;
    CommandLineParams.ActiveRight:= gActiveRight;
  end;

  LoadTabsCommandLine(CommandLineParams);

  if gDelayLoadingTabs then
  begin
    // Load only the current active tab of each notebook.
    FrameLeft.Flags  := FrameLeft.Flags  - [fvfDelayLoadingFiles];
    FrameRight.Flags := FrameRight.Flags - [fvfDelayLoadingFiles];
  end;
end;

procedure TfrmMain.LoadTabsCommandLine(Params: TCommandLineParams);

  procedure AddTab(ANoteBook: TFileViewNotebook; aPath: String);
  var
    Page: TFileViewPage;
    AFileView: TFileView;
    AFileViewFlags: TFileViewFlags;
    aFileSource: IFileSource;
  begin
    Page := ANoteBook.AddPage;
    aFileSource := TFileSystemFileSource.GetFileSource;
    if gDelayLoadingTabs then
      AFileViewFlags := [fvfDelayLoadingFiles]
    else
      AFileViewFlags := [];
    AFileView := TColumnsFileView.Create(Page, aFileSource, aPath, AFileViewFlags);
    AssignEvents(AFileView);
    ANoteBook.PageIndex := ANoteBook.PageCount - 1;
  end;

  procedure LoadPanel(aNoteBook: TFileViewNotebook; aPath: String);
  begin
    if Length(aPath) <> 0 then
    begin
      aPath:= ReplaceEnvVars(ReplaceTilde(aPath));
      if not mbFileSystemEntryExists(aPath) then
        aPath:= GetDeepestExistingPath(aPath);
      if Length(aPath) <> 0 then
      begin
        if Params.NewTab then
          AddTab(aNoteBook, aPath)
        else
          aNoteBook.ActivePage.FileView.ChangePathAndSetActiveFile(aPath)
      end;
    end;
  end;

begin
  //-- set path for left panel (if set)
  LoadPanel(nbLeft,  Params.LeftPath);

  //-- set path for right panel (if set)
  LoadPanel(nbRight, Params.RightPath);

  //-- set active panel, if needed
  if Params.ActivePanelSpecified then
  begin
    if Params.ActiveRight then
      SetActiveFrame(fpRight)
    else
      SetActiveFrame(fpLeft);
  end;

  //-- set path for active panel (if set)
  if ActiveFrame.NotebookPage is TFileViewPage then
  begin
    LoadPanel((ActiveFrame.NotebookPage as TFileViewPage).Notebook, Params.ActivePanelPath);
  end;

  ActiveFrame.SetFocus;
end;

procedure TfrmMain.LoadWindowState;
var
  ANode: TXmlNode;
  FPixelsPerInch: Integer;
  ALeft, ATop, AWidth, AHeight: Integer;
begin
  (* Load window bounds and state *)
  ANode := gConfig.FindNode(gConfig.RootNode, 'MainWindow/Position', True);
  begin
    MainSplitterPos := gConfig.GetValue(ANode, 'Splitter', 50.0);
    ALeft := gConfig.GetValue(ANode, 'Left', 80);
    ATop := gConfig.GetValue(ANode, 'Top', 48);
    AWidth := gConfig.GetValue(ANode, 'Width', 800);
    AHeight := gConfig.GetValue(ANode, 'Height', 480);
    FPixelsPerInch := gConfig.GetValue(ANode, 'PixelsPerInch', DesignTimePPI);
    if Scaled and (Screen.PixelsPerInch <> FPixelsPerInch) then
    begin
      AWidth := MulDiv(AWidth, Screen.PixelsPerInch, FPixelsPerInch);
      AHeight := MulDiv(AHeight, Screen.PixelsPerInch, FPixelsPerInch);
    end;
    SetBounds(ALeft, ATop, AWidth, AHeight);
    if gConfig.GetValue(ANode, 'Maximized', True) then
      Self.WindowState := wsMaximized;
  end;
end;

procedure TfrmMain.SaveWindowState;
var
  ANode: TXmlNode;
begin
  (* Save window bounds and state *)
  ANode := gConfig.FindNode(gConfig.RootNode, 'MainWindow/Position', True);
  // save window size only if it's not Maximized (for not break normal size)
  if (WindowState <> wsMaximized) then
  begin
    gConfig.SetValue(ANode, 'Left', Left);
    gConfig.SetValue(ANode, 'Top', Top);
    gConfig.SetValue(ANode, 'Width', Width);
    gConfig.SetValue(ANode, 'Height', Height);
    gConfig.SetValue(ANode, 'PixelsPerInch', Screen.PixelsPerInch);
  end;
  gConfig.SetValue(ANode, 'Maximized', (WindowState = wsMaximized));
  gConfig.SetValue(ANode, 'Splitter', FMainSplitterPos);
end;

procedure TfrmMain.LoadMainToolbar;
var
  ToolBarLoader: TKASToolBarExtendedLoader;
  ToolBarNode: TXmlNode;
begin
  MainToolBar.BeginUpdate;
  ToolBarLoader := TKASToolBarExtendedLoader.Create(Commands.Commands);
  try
    MainToolBar.Clear;
    ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', False);
    if Assigned(ToolBarNode) then
      MainToolBar.LoadConfiguration(gConfig, ToolBarNode, ToolBarLoader, tocl_FlushCurrentToolbarContent);
  finally
    ToolBarLoader.Free;
    MainToolBar.EndUpdate;
  end;
end;

procedure TfrmMain.SaveMainToolBar;
var
  ToolBarNode: TXmlNode;
begin
  ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', True);
  gConfig.ClearNode(ToolBarNode);
  MainToolBar.SaveConfiguration(gConfig, ToolBarNode);
end;

procedure TfrmMain.ShowLogWindow(Data: PtrInt);
var
  bShow: Boolean absolute Data;
begin
  LogSplitter.Visible:= bShow;
  seLogWindow.Visible:= bShow;
  LogSplitter.Top:= seLogWindow.Top - LogSplitter.Height;
end;

procedure TfrmMain.ConfigSaveSettings(bForce: Boolean);
begin
  try
    DebugLn('Saving configuration');

    if gSaveCmdLineHistory then
      glsCmdLineHistory.Assign(edtCommand.Items);

    (* Save all tabs *)
    if gSaveFolderTabs or bForce then
    begin
      SaveTabsXml(gConfig, 'Tabs/OpenedTabs/', nbLeft, gSaveDirHistory);
      SaveTabsXml(gConfig, 'Tabs/OpenedTabs/', nbRight, gSaveDirHistory);
    end;

    if gSaveWindowState then SaveWindowState;

    if gButtonBar then SaveMainToolBar;

    SaveGlobs; // Should be last, writes configuration file
  except
    on E: Exception do
      DebugLn('Cannot save main configuration: ', e.Message);
  end;
end;

function TfrmMain.IsCommandLineVisible: Boolean;
begin
  Result := (edtCommand.Visible and pnlCommand.Visible and pnlCmdLine.Visible);
end;

procedure TfrmMain.ShowCommandLine(AFocus: Boolean);
begin
  if edtCommand.Visible then
  begin
    // Show temporarily command line on user request.
    if not (gCmdLine and frmMain.IsCommandLineVisible) then
    begin
      pnlCommand.Show;
      pnlCmdLine.Show;
    end;
    if AFocus then edtCommand.SetFocus;
  end;
end;

function TfrmMain.FindMatchingDrive(Address, Path: String): Integer;
var
  I : Integer;
  DrivePath: String;
  DrivePathLen: PtrInt;
  LongestPathLen: Integer = 0;
begin
  Result := -1;

  if Assigned(DrivesList) then
  begin
    Path := UTF8UpperCase(Path);

    for I := 0 to DrivesList.Count - 1 do
    begin
      if DrivesList[I]^.DriveType = dtSpecial then
      begin
        if Pos(Address, DrivesList[I]^.Path) = 1 then
          Exit(I);
      end
      else begin
        DrivePath := UTF8UpperCase(DrivesList[I]^.Path);
        DrivePathLen := UTF8Length(DrivePath);
        if (DrivePathLen > LongestPathLen) and IsInPath(DrivePath, Path, True, True) then
        begin
          LongestPathLen := DrivePathLen;
          Result := I;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.UpdateDriveToolbarSelection(DriveToolbar: TKAStoolBar; FileView: TFileView);
var
  DriveIndex: Integer;
begin
  DriveIndex := FindMatchingDrive(FileView.CurrentAddress, FileView.CurrentPath);
  if (DriveIndex >= 0) and (DriveIndex < DriveToolbar.ButtonCount) then
    DriveToolbar.Buttons[DriveIndex].Down := True
  else
    // Path not found in toolbar.
    DriveToolbar.UncheckAllButtons;
end;

procedure TfrmMain.UpdateDriveButtonSelection(DriveButton: TSpeedButton; FileView: TFileView);
var
  BitmapTmp: Graphics.TBitmap = nil;
  DriveIndex: Integer;
  Drive: PDrive;
begin
  if not gDrivesListButton then
    Exit;

  DriveIndex := FindMatchingDrive(FileView.CurrentAddress, FileView.CurrentPath);
  if DriveIndex >= 0 then
  begin
    Drive := DrivesList[DriveIndex];
    DriveButton.Caption := Drive^.DisplayName;
    DriveButton.Tag := DriveIndex;
    BitmapTmp := PixMapManager.GetDriveIcon(Drive, gDiskIconsSize, DriveButton.Color);
  end
  else
  begin
    DriveButton.Caption := '';
    DriveButton.Tag := -1;

    if FileView.FileSource.IsClass(TArchiveFileSource) then
      BitmapTmp := PixMapManager.GetArchiveIcon(gDiskIconsSize, DriveButton.Color)
    else
      BitmapTmp := PixMapManager.GetDefaultDriveIcon(gDiskIconsSize, DriveButton.Color);
  end;

  DriveButton.Glyph := BitmapTmp;

  DriveButton.Width := DriveButton.Glyph.Width
                     + DriveButton.Canvas.TextWidth(DriveButton.Caption) + 24;

  FreeAndNil(BitmapTmp);
end;

procedure TfrmMain.UpdateSelectedDrive(ANoteBook: TFileViewNotebook);
var
  FileView: TFileView;
begin
  FileView := ANoteBook.ActiveView;
  if Assigned(FileView) then
  begin
    // Change left drive toolbar for left drive button.
    if (ANoteBook = nbLeft) then
    begin
      if gDriveBar1 then // If drives toolbar enabled at all
      begin
        if gDriveBar2 then // If showing two toolbars
          UpdateDriveToolbarSelection(dskLeft, FileView)
        else
          // dskRight is the main toolbar.
          UpdateDriveToolbarSelection(dskRight, FileView);
      end;
      UpdateDriveButtonSelection(btnLeftDrive, FileView);
    end
    // Change right drive toolbar for right drive button
    else if (ANoteBook = nbRight) then
    begin
      if gDriveBar1 then
        UpdateDriveToolbarSelection(dskRight, FileView);
      UpdateDriveButtonSelection(btnRightDrive, FileView);
    end;
  end;
end;

procedure TfrmMain.UpdateSelectedDrives;
begin
  if gDriveBar1 then
  begin
    if gDriveBar2 then
    begin
      UpdateDriveToolbarSelection(dskLeft, FrameLeft);
      UpdateDriveToolbarSelection(dskRight, FrameRight);
    end
    else
      // dskRight is the main toolbar.
      UpdateDriveToolbarSelection(dskRight, ActiveFrame);
  end;
  UpdateDriveButtonSelection(btnLeftDrive, FrameLeft);
  UpdateDriveButtonSelection(btnRightDrive, FrameRight);
end;

procedure TfrmMain.UpdateGUIFunctionKeys;
var
  I: Integer;
  H: Integer = 0;
  AButton: TSpeedButton;
begin
  for I:= 0 to pnlKeys.ControlCount - 1 do
  begin
    if pnlKeys.Controls[I] is TSpeedButton then
    begin
      AButton:= TSpeedButton(pnlKeys.Controls[I]);
      AButton.Font.Size := gFonts[dcfFunctionButtons].Size;
      H:= Max(H, AButton.Canvas.TextHeight(AButton.Caption));
    end;
  end;
  pnlKeys.Height := H + 4;
end;

procedure TfrmMain.ShowDrivesList(APanel: TFilePanelSelect);
var
  p: TPoint;
  ADriveIndex: Integer;
begin
  if tb_activate_panel_on_click in gDirTabOptions then
    SetActiveFrame(APanel);

  case APanel of
    fpLeft:
      begin
        p := Classes.Point(btnLeftDrive.Left, btnLeftDrive.Height);
        p := pnlLeftTools.ClientToScreen(p);
        ADriveIndex := btnLeftDrive.Tag;
      end;
    fpRight:
      begin
        p := Classes.Point(btnRightDrive.Left, btnRightDrive.Height);
        p := pnlRightTools.ClientToScreen(p);
        ADriveIndex := btnRightDrive.Tag;
      end;
  end;
  p := ScreenToClient(p);
  FDrivesListPopup.Show(p, APanel, ADriveIndex);
end;

procedure TfrmMain.HideToTray;
{$IF DEFINED(LCLGTK2) or DEFINED(LCLQT) or DEFINED(LCLQT5)}
var
  ActiveWindow: HWND;
  LCLObject: TObject;
{$ENDIF}
begin
{
  If a modal form is active we have to hide it first before hiding the main form
  to avoid bugs:

  On GTK2 a modal form loses it's modal state after the main window is
  restored (GTK still says the window is modal and resetting modal state
  doesn't do anything).

  On QT the tray icon does not receive any mouse events (because the modal
  window has capture) thus preventing the user from restoring the main window.
  So when the main form is hidden the modal window is hidden too.
}
{$IF DEFINED(LCLGTK2) or DEFINED(LCLQT) or DEFINED(LCLQT5)}
  LastActiveWindow := nil;
  if not Self.Active then    // If there is another window active
  begin
    ActiveWindow := GetActiveWindow;
    if ActiveWindow <> 0 then
    begin
      LCLObject := GetLCLOwnerObject(ActiveWindow);
      if Assigned(LCLObject) and
         (LCLObject is TCustomForm) and
         (fsModal in (LCLObject as TCustomForm).FormState) then // only for modal windows
      begin
        LastActiveWindow := LCLObject as TCustomForm;
{$IFDEF LCLGTK2}
        // Cannot use Hide method, because it closes the modal form.
        // We only want to hide it.
        LastActiveWindow.Visible := False;
{$ENDIF}
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
        // Have to use QT directly to hide the window for this to work.
        TQtWidget(LastActiveWindow.Handle).setVisible(False);
{$ENDIF}
      end;
    end;
  end;
{$ENDIF}

  Hide;
  ShowTrayIcon(True);
  HiddenToTray := True;
end;

procedure TfrmMain.RestoreFromTray;
begin
  if lastWindowState=wsMaximized then  WindowState:=wsMaximized;
  ShowOnTop;

  if not gAlwaysShowTrayIcon then
    ShowTrayIcon(False);

  // After the main form is shown, restore the last active modal form if there was any.
{$IF DEFINED(LCLGTK2) or DEFINED(LCLQT) or DEFINED(LCLQT5)}
   if Assigned(LastActiveWindow) then
   begin
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
     TQtWidget(LastActiveWindow.Handle).setVisible(true);
{$ENDIF}
{$IFDEF LCLGTK2}
     LastActiveWindow.Show;
{$ENDIF}
     LastActiveWindow := nil;
   end;
{$ENDIF}
end;

procedure TfrmMain.RightDriveBarExecuteDrive(ToolItem: TKASToolItem);
var
  DriveItem: TKASDriveItem;
  Panel: TFilePanelSelect;
begin
  DriveItem := ToolItem as TKASDriveItem;
  if gDriveBar2 then
    Panel := fpRight
  else
    Panel := ActiveNotebook.Side;
  SetPanelDrive(Panel, DriveItem.Drive, True);
end;

procedure TfrmMain.ShowTrayIcon(bShow: Boolean);
begin
  if (bShow <> MainTrayIcon.Visible) and (HidingTrayIcon = False) then
  begin
    if bShow then
    begin
      MainTrayIcon.Visible := True;
    end
    else
    begin
      // ShowTrayIcon might be called from within OnClick event of the icon
      // (MainTrayIconClick->RestoreFromTray->ShowTrayIcon), so the MainTrayIcon
      // cannot be hidden here, because it would be destroyed causing A/V.
      // Hiding it must be delayed until after the mouse click handler of the icon is finished.
      HidingTrayIcon := True;
      Application.QueueAsyncCall(@HideTrayIconDelayed, 0);
    end;
  end;
end;

procedure TfrmMain.HideTrayIconDelayed(Data: PtrInt);
begin
  MainTrayIcon.Visible := False;
  HidingTrayIcon := False;
end;

procedure TfrmMain.ExecuteCommandLine(bRunInTerm: Boolean);
begin
  mbSetCurrentDir(ActiveFrame.CurrentPath);
  ExecuteCommandFromEdit(edtCommand.Text, bRunInTerm);
  edtCommand.Text := '';
  ActiveFrame.Reload;
  ActiveFrame.SetFocus;
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  // workaround for GTK
  // edtCommandExit is not always called when losing focus
  edtCommandExit(Self);
{$ENDIF}
end;

procedure TfrmMain.UpdatePrompt;
const
  PTLen = 40;
var
  st: String;
  Properties: TFileSourceProperties;
begin
  if (fsoExecute in ActiveFrame.FileSource.GetOperationsTypes) then
  begin
    with lblCommandPath do
    begin
      Visible := True;
      AutoSize := False;
      if UTF8Length(ActiveFrame.CurrentPath) > PTLen
      then
        st:= UTF8Copy(ActiveFrame.CurrentPath,
                              UTF8Length(ActiveFrame.CurrentPath) - PTLen,
                              PTLen)
      else
        st:= ActiveFrame.CurrentPath;
      //
      Caption := Format(fmtCommandPath, [st]);
      AutoSize := True;
      Left := 1;
    end;

    // Change path in terminal
    if (fspDirectAccess in ActiveFrame.FileSource.GetProperties) then
      begin
        if gTermWindow and Assigned(Cons) then
          Cons.Terminal.SetCurrentDir(ActiveFrame.CurrentPath);
      end;

    edtCommand.Visible := True;
  end
  else
  begin
    lblCommandPath.Visible := False;
    edtCommand.Visible := False;
  end;
  // Change program current path
  Properties := ActiveFrame.FileSource.GetProperties;
  if (fspDirectAccess in Properties) and not (fspLinksToLocalFiles in Properties) then
  begin
    mbSetCurrentDir(ActiveFrame.CurrentPath);
  end;
end;

procedure TfrmMain.UpdateFreeSpace(Panel: TFilePanelSelect);
var
  FreeSize, TotalSize: Int64;
  aFileView: TFileView;
  sboxDrive: TPaintBox;
  lblDriveInfo: TLabel;
begin
  case Panel of
    fpLeft :
      begin
        sboxDrive := pbxLeftDrive;
        aFileView := FrameLeft;
        lblDriveInfo:=lblLeftDriveInfo;
      end;
    fpRight:
      begin
        sboxDrive := pbxRightDrive;
        aFileView := FrameRight;
        lblDriveInfo:=lblRightDriveInfo;
      end;
  end;

  if aFileView.FileSource.GetFreeSpace(aFileView.CurrentPath, FreeSize, TotalSize) then
    begin
      if gDriveInd = True then
        begin
          if TotalSize > 0 then
            sboxDrive.Tag:= 100 - Round((FreeSize / TotalSize) * 100) // Save busy percent
          else
            sboxDrive.Tag := -1;
          sboxDrive.Invalidate;
        end;
      lblDriveInfo.Hint := Format(rsFreeMsg, [cnvFormatFileSize(FreeSize, uoscHeaderFooter), cnvFormatFileSize(TotalSize, uoscHeaderFooter)]); //It's not an "operation" but most probably the closest wanted form.
      if gShortFormatDriveInfo then
        lblDriveInfo.Caption := Format(rsFreeMsgShort, [cnvFormatFileSize(FreeSize, uoscHeaderFooter)])
      else
        lblDriveInfo.Caption := lblDriveInfo.Hint;
      sboxDrive.Hint := lblDriveInfo.Hint;
    end
  else
    begin
      lblDriveInfo.Caption := '';
      lblDriveInfo.Hint := '';
      sboxDrive.Hint := '';
      sboxDrive.Tag := -1;
      sboxDrive.Invalidate;
    end;
end;

procedure TfrmMain.CloseNotebook(ANotebook: TFileViewNotebook);
var
  I: Integer;
begin
  for I := 0 to ANotebook.PageCount - 1 do
    ANotebook.View[I].Clear;
  ANotebook.DestroyAllPages;
end;

procedure TfrmMain.DriveListDriveSelected(Sender: TObject; ADriveIndex: Integer;
  APanel: TFilePanelSelect);
begin
  SetPanelDrive(APanel, DrivesList.Items[ADriveIndex], True);
end;

procedure TfrmMain.DriveListClose(Sender: TObject);
begin
  SetActiveFrame(SelectedPanel);
end;

procedure TfrmMain.AllProgressOnUpdateTimer(Sender: TObject);
var
  AllProgressPoint: Integer;
begin
  // Hide progress bar if there are no operations
  if OperationsManager.OperationsCount = 0 then
    begin
      mnuAllOperProgress.Visible:= False;
      mnuAllOperPause.Visible:= False;
      mnuAllOperStart.Visible:= False;
    end
  else
    begin
      if gPanelOfOp = True then
        FOperationsPanel.UpdateView;

      if gProgInMenuBar = True then
        begin
          AllProgressPoint:= Round(OperationsManager.AllProgressPoint * 100);
          // Show in menu line
          mnuAllOperProgress.Caption:=IntToStr(AllProgressPoint) + ' %';
          mnuAllOperProgress.Visible:= True;
          mnuAllOperPause.Visible:= True;
          mnuAllOperStart.Visible:= True;
        end;
    end;
end;

procedure TfrmMain.SetPanelDrive(aPanel: TFilePanelSelect; Drive: PDrive; ActivateIfNeeded: Boolean);
var
  Index: Integer;
  DriveIndex: Integer;
  FoundPath: Boolean = False;
  aFileView, OtherFileView: TFileView;
begin
  if (Drive^.DriveType in [dtSpecial, dtVirtual]) or IsAvailable(Drive, Drive^.AutoMount) then
  begin
    case aPanel of
      fpLeft:
        begin
          aFileView := FrameLeft;
          OtherFileView := FrameRight;
        end;
      fpRight:
        begin
          aFileView := FrameRight;
          OtherFileView := FrameLeft;
        end;
    end;

    // Special case for special drive
    if Drive^.DriveType = dtSpecial then
    begin
      ChooseFileSource(aFileView, Drive^.Path);
      if ActivateIfNeeded and (tb_activate_panel_on_click in gDirTabOptions) then
        SetActiveFrame(aPanel);
      Exit;
    end;

    // Special case for virtual drive
    if Drive^.DriveType = dtVirtual then
    begin
      ChooseFileSource(aFileView, GetNetworkPath(Drive));
      if ActivateIfNeeded and (tb_activate_panel_on_click in gDirTabOptions) then
        SetActiveFrame(aPanel);
      Exit;
    end;

    // Copy path opened in the other panel if the file source and drive match
    // and that path is not already opened in this panel.
    if OtherFileView.FileSource.IsClass(TFileSystemFileSource) and
       mbCompareFileNames(ExtractRootDir(OtherFileView.CurrentPath), ExcludeTrailingPathDelimiter(Drive^.Path)) and
       not mbCompareFileNames(OtherFileView.CurrentPath, aFileView.CurrentPath) and not gGoToRoot then
    begin
      FoundPath:= True;
      SetFileSystemPath(aFileView, OtherFileView.CurrentPath);
    end
    // Open latest path from history for chosen drive
    else if (gGoToRoot = False) and aFileView.FileSource.IsClass(TFileSystemFileSource) and
            not mbCompareFileNames(ExtractRootDir(aFileView.CurrentPath), ExcludeTrailingPathDelimiter(Drive^.Path)) then
    begin
      for Index:= 0 to glsDirHistory.Count - 1 do
      begin
        DriveIndex:= FindMatchingDrive(EmptyStr, glsDirHistory[Index]);
        if (DriveIndex >= 0) and (DriveIndex < DrivesList.Count) then
        begin
          if mbCompareFileNames(Drive^.Path, DrivesList[DriveIndex]^.Path) then
          begin
            if mbDirectoryExists(ExcludeBackPathDelimiter(glsDirHistory[Index])) then
            begin
              SetFileSystemPath(aFileView, glsDirHistory[Index]);
              FoundPath:= True;
              Break;
            end;
          end;
        end;
      end;
    end;

    if not FoundPath then begin
      SetFileSystemPath(aFileView, Drive^.Path);
    end;

    if ActivateIfNeeded and (tb_activate_panel_on_click in gDirTabOptions) then
      SetActiveFrame(aPanel);
  end
  else
  begin
    msgWarning(rsMsgDiskNotAvail);

    // Restore previous selected button.
    case aPanel of
      fpLeft:
        UpdateSelectedDrive(LeftTabs);
      fpRight:
        UpdateSelectedDrive(RightTabs);
    end;
  end;
end;

procedure TfrmMain.OnDriveWatcherEvent(EventType: TDriveWatcherEvent; const ADrive: PDrive);
begin
  // Update disk panel does not work correctly when main
  // window is minimized. So set FUpdateDiskCount flag instead
  // and update disk count later in WindowStateChange event
  if WindowState = wsMinimized then
    FUpdateDiskCount:= True
  else begin
    UpdateDiskCount;
  end;

  if (FrameLeft = nil) or (FrameRight = nil) then Exit;

  if (EventType = dweDriveRemoved) and Assigned(ADrive) then
  begin
    if IsInPath(ADrive^.Path, ActiveFrame.CurrentPath, True, True) then
      ActiveFrame.CurrentPath:= gpExePath
    else if IsInPath(ADrive^.Path, NotActiveFrame.CurrentPath, True, True) then
      NotActiveFrame.CurrentPath:= gpExePath;
  end;

  UpdateSelectedDrives;
end;

procedure TfrmMain.AppActivate(Sender: TObject);
begin
  if Assigned(FrameLeft) then
    FrameLeft.ReloadIfNeeded;
  if Assigned(FrameRight) then
    FrameRight.ReloadIfNeeded;
end;

procedure TfrmMain.AppDeActivate(Sender: TObject);
begin
  if Assigned(frmTreeViewMenu) then
  begin
    frmTreeViewMenu.Close;
  end;
  Application.CancelHint;
end;

procedure TfrmMain.AppEndSession(Sender: TObject);
var
  CloseAction: TCloseAction;
begin
  frmMainClose(Sender, CloseAction);
end;

procedure TfrmMain.AppQueryEndSession(var Cancel: Boolean);
var
  CanClose: Boolean = True;
begin
  FormCloseQuery(Self, CanClose);
  Cancel := not CanClose;
end;

{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
function TfrmMain.QObjectEventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result:= False;
  if QEvent_type(Event) = QEventClose then
  begin
    TQtWidget(Self.Handle).SlotClose;
    Result:= CloseQueryResult;
    if Result then
      QEvent_accept(Event)
    else
      QEvent_ignore(Event);
  end;
end;
{$ENDIF}

initialization
  {$I DragCursors.lrs}
  TFormCommands.RegisterCommandsForm(TfrmMain, HotkeysCategory, @rsHotkeyCategoryMain);

end.

