{
   Double Commander
   -------------------------------------------------------------------------
   Licence  : GNU GPL v 2.0
   Copyright (C) 2006-2010 Alexander Koblov (Alexx2000@mail.ru)

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
  Graphics, Forms, Menus, Controls, StdCtrls, ExtCtrls, ActnList,
  Buttons, SysUtils, Classes, SynEdit, LCLType, ComCtrls,
  KASToolBar, KASBarMenu, KASBarFiles,
  uCmdBox, uFilePanelSelect,
  uFileView, uColumnsFileView, uFileSource, uFileViewNotebook, uFile,
  uOperationsManager, uFileSourceOperation, uDrivesList, uTerminal, uClassesEx,
  uXmlConfig, uDrive, uDriveWatcher, uDCVersion;

const
  cHistoryFile='cmdhistory.txt';

type

  { TfrmMain }

  TfrmMain = class(TForm)
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
    actGoToFirstFile: TAction;
    actGoToLastFile: TAction;
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
    actRemoveAllTabs: TAction;
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
    actRemoveTab: TAction;
    actNewTab: TAction;
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
    edtCommand: TComboBox;
    lblRightDriveInfo: TLabel;
    lblLeftDriveInfo: TLabel;
    lblCommandPath: TLabel;
    AllOpPause: TMenuItem;
    AllOpProgressInd: TMenuItem;
    AllOpCancel: TMenuItem;
    AllOpStart: TMenuItem;
    AllOpPct: TMenuItem;
    miRenameTab: TMenuItem;
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
    sboxOperations: TScrollBox;
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
    mnuRemoveAllTabs: TMenuItem;
    mnuRemoveTab: TMenuItem;
    miLine15: TMenuItem;
    mnuOpenDirInNewTab: TMenuItem;
    mnuNewTab: TMenuItem;
    miRemoveAllTabs: TMenuItem;
    miRemoveTab: TMenuItem;
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
    pmButtonMenu: TKASBarMenu;
    MainToolBar: TKASToolBar;
    mnuOpenVFSList: TMenuItem;
    mnuExtractFiles: TMenuItem;
    pmDrivesMenu: TPopupMenu;
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
    actCalculateSpace: TAction;
    actFileProperties:  TAction;
    actFileLinker: TAction;
    actFileSpliter: TAction;
    pmToolBar: TPopupMenu;
    MainTrayIcon: TTrayIcon;

    procedure actExecute(Sender: TObject);
    procedure AllOpCancelClick(Sender: TObject);
    procedure AllOpPauseClick(Sender: TObject);
    procedure AllOpPctClick(Sender: TObject);
    procedure AllOpStartClick(Sender: TObject);
    procedure btnF8Click(Sender: TObject);
    procedure btnLeftClick(Sender: TObject);
    procedure btnLeftDirectoryHotlistClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure btnRightDirectoryHotlistClick(Sender: TObject);
    procedure btnDriveMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ConsoleSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure dskLeftResize(Sender: TObject);
    procedure dskRightResize(Sender: TObject);
    procedure dskLeftRightToolButtonDragDrop(Sender, Source: TObject; X, Y: Integer; NumberOfButton: Integer);
    procedure dskToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);
    procedure lblAllProgressPctClick(Sender: TObject);
    procedure MainToolBarToolButtonDragDrop(Sender, Source: TObject; X,
      Y: Integer; NumberOfButton: Integer);
    procedure MainToolBarToolButtonDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean;
      NumberOfButton: Integer);
    procedure MainToolBarToolButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      NumberOfButton: Integer);
    procedure MainToolBarToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);


    procedure miLogMenuClick(Sender: TObject);
    procedure miTrayIconExitClick(Sender: TObject);
    procedure miTrayIconRestoreClick(Sender: TObject);
    procedure PanelButtonClick(Button: TSpeedButton; SourceFrame: TFileView;
                               PanelSelect: TFilePanelSelect);
    procedure DeleteClick(Sender: TObject);
    procedure dskToolButtonClick(Sender: TObject; NumberOfButton: Integer);
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
    function MainToolBarLoadButtonGlyph(sIconFileName: String;
      iIconSize: Integer; clBackColor: TColor): TBitmap;
    procedure MainToolBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainToolBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
    procedure frmMainClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frmMainAfterShow(Data: PtrInt);
    procedure frmMainShow(Sender: TObject);
    procedure mnuDropClick(Sender: TObject);
    procedure mnuSplitterPercentClick(Sender: TObject);
    procedure mnuTabMenuClick(Sender: TObject);
    procedure nbPageAfterMouseDown(Data: PtrInt);
    procedure nbPageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure nbPageChanged(Sender: TObject);
    procedure nbPageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NotebookCloseTabClicked(Sender: TObject);
    function pmButtonMenuLoadButtonGlyph(sIconFileName: String;
      iIconSize: Integer; clBackColor: TColor): TBitmap;
    procedure pmButtonMenuMenuButtonClick(Sender: TObject;
      NumberOfButton: Integer);
    procedure pmDropMenuClose(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure edtCommandKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pmToolBarPopup(Sender: TObject);

    procedure pnlLeftResize(Sender: TObject);
    procedure pnlLeftRightDblClick(Sender: TObject);
    procedure pnlNotebooksResize(Sender: TObject);
    procedure pnlRightResize(Sender: TObject);
    procedure sboxDrivePaint(Sender: TObject);
    procedure sboxOperationsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sboxOperationsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sboxOperationsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sboxOperationsPaint(Sender: TObject);
    procedure seLogWindowSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);

    function FileViewBeforeChangePath(FileView: TFileView; NewFileSource: IFileSource; const NewPath : String): Boolean;
    procedure FileViewAfterChangePath(FileView: TFileView);
    procedure FileViewActivate(FileView: TFileView);
    procedure FileViewReload(FileView: TFileView);
    procedure edtCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtCommandExit(Sender: TObject);
    procedure tbChangeDirClick(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
    procedure OnUniqueInstanceMessage(Sender: TObject; Params: array of UTF8String; ParamCount: Integer);
    procedure tbPasteClick(Sender: TObject);
    procedure AllProgressOnUpdateTimer(Sender: TObject);
  private
    { Private declarations }
    PanelSelected: TFilePanelSelect;
    DrivesList : TDrivesList;
    MainSplitterHintWnd: THintWindow;
    HiddenToTray: Boolean;
    HidingTrayIcon: Boolean; // @true if the icon is in the process of hiding
    nbLeft, nbRight: TFileViewNotebook;
    cmdConsole: TCmdBox;
    {en
       Used to pass drag&drop parameters to pmDropMenu. Single variable
       can be used, because the user can do only one menu popup at a time. }
    FDropParams: TDropParams;
    FDrivesListPopup: TDrivesListPopup;

    // frost_asm begin
    // mainsplitter
    MainSplitterLeftMouseBtnDown: Boolean;
    MainSplitterMouseDownX, MainSplitterMouseDownY: Integer;
    FResizingFilePanels: Boolean;
    // lastWindowState
    lastWindowState:TWindowState;
    // frost_asm end
    PressLMB: boolean;
    widthOfItem, ItemEnd: integer;
    // for dragging buttons and etc
    NumberOfMoveButton, NumberOfNewMoveButton: integer;
    Draging : boolean;

    procedure CheckCommandLine(ShiftEx: TShiftState; var Key: Word);
    function ExecuteCommandFromEdit(sCmd: String; bRunInTerm: Boolean): Boolean;
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
    procedure SetFileSystemPath(aFileView: TFileView; aPath: UTF8String);
    procedure SetPanelDrive(aPanel: TFilePanelSelect; Drive: PDrive);
    procedure OnDriveWatcherEvent(EventType: TDriveWatcherEvent; const ADrive: PDrive);
    procedure AppActivate(Sender: TObject);
    procedure AppException(Sender: TObject; E: Exception);

  public
    Function ActiveFrame: TFileView;  // get Active frame
    Function NotActiveFrame: TFileView; // get NotActive frame :)
    function ActiveNotebook: TFileViewNotebook;
    function NotActiveNotebook: TFileViewNotebook;
    function FrameLeft: TFileView;
    function FrameRight: TFileView;
    //check selected count and generate correct msg, parameters is lng indexs
    Function GetFileDlgStr(sLngOne, sLngMulti : String; Files: TFiles):String;
    procedure HotDirSelected(Sender:TObject);
    procedure ViewHistorySelected(Sender:TObject);
    procedure ViewHistoryPrevSelected(Sender:TObject);
    procedure ViewHistoryNextSelected(Sender:TObject);
    procedure CreatePopUpHotDir;
    procedure CreatePopUpDirHistory;
    procedure ShowFileViewHistory;
    procedure ShowFileViewHistory(FromFileSourceIndex, FromPathIndex,
                                  ToFileSourceIndex, ToPathIndex: Integer);
    procedure miHotAddClick(Sender: TObject);
    procedure miHotDeleteClick(Sender: TObject);
    procedure miHotConfClick(Sender: TObject);
    {en
       Returns @true if copy operation has been successfully started.
    }
    function CopyFiles(SourceFileSource, TargetFileSource: IFileSource;
                       var SourceFiles: TFiles; TargetPath: String;
                       bShowDialog: Boolean): Boolean; overload;
    {en
       Returns @true if move operation has been successfully started.
    }
    function MoveFiles(SourceFileSource, TargetFileSource: IFileSource;
                       var SourceFiles: TFiles; TargetPath: String;
                       bShowDialog: Boolean): Boolean; overload;
    function CopyFiles(sDestPath: String; bShowDialog: Boolean): Boolean; overload; //  this is for F5 and Shift+F5
    function MoveFiles(sDestPath: String; bShowDialog: Boolean): Boolean; overload;
    procedure GetDestinationPathAndMask(TargetFileSource: IFileSource;
                                        EnteredPath: String; BaseDir: String;
                                        out DestPath, DestMask: String);
    procedure SetActiveFrame(panel: TFilePanelSelect);
    procedure UpdateDiskCount;
    procedure CreateDiskPanel(dskPanel : TKASToolBar);
    function CreateFileView(sType: String; Page: TFileViewPage; AConfig: TIniFileEx; ASectionName: String; ATabIndex: Integer): TFileView;
    function CreateFileView(sType: String; Page: TFileViewPage; AConfig: TXmlConfig; ANode: TXmlNode): TFileView;
    procedure AssignEvents(AFileView: TFileView);
    function RemovePage(ANoteBook: TFileViewNotebook; iPageIndex:Integer): LongInt;
    procedure LoadTabsIni(ANoteBook: TFileViewNotebook);
    procedure LoadTabsXml(ANoteBook: TFileViewNotebook);
    procedure SaveTabsIni(ANoteBook: TFileViewNotebook);
    procedure SaveTabsXml(ANoteBook: TFileViewNotebook);
    function ExecCmd(Cmd: String; Param: String=''; StartPath: String='') : Boolean;
    function ExecuteToolButton(Sender: TObject; NumberOfButton:Integer) : Boolean;
    procedure ToggleConsole;
    procedure UpdateWindowView;
    procedure MinimizeWindow;
    procedure LoadWindowState;
    procedure SaveWindowState;
    procedure SaveMainToolBar;
    function  IsCommandLineVisible: Boolean;
    function  FindMatchingDrive(Path: UTF8String): Integer;
    procedure UpdateDriveToolbarSelection(DriveToolbar: TKAStoolBar; FileView: TFileView);
    procedure UpdateDriveButtonSelection(DriveButton: TSpeedButton; FileView: TFileView);
    procedure UpdateSelectedDrive(ANoteBook: TFileViewNotebook);
    procedure ShowDrivesList(APanel: TFilePanelSelect);
    procedure ExecuteCommandLine(bRunInTerm: Boolean);
    procedure UpdatePrompt;
    procedure UpdateFreeSpace(Panel: TFilePanelSelect);
    procedure ReLoadTabs(ANoteBook: TFileViewNotebook);

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

  published
    property SelectedPanel: TFilePanelSelect read PanelSelected;
    property LeftTabs: TFileViewNotebook read nbLeft;
    property RightTabs: TFileViewNotebook read nbRight;
  end;

var
  frmMain: TfrmMain;
  Cons: TConsoleThread = nil;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLVersion, Dialogs, uGlobs, uLng, fConfigToolBar, uMasks, fCopyMoveDlg, uQuickViewPanel,
  uShowMsg, fHotDir, uDCUtils, uLog, uGlobsPaths, LCLProc, uOSUtils, uOSForms, uPixMapManager,
  uDragDropEx, uKeyboard, uFileSystemFileSource, fViewOperations,
  uFileSourceOperationTypes, uFileSourceCopyOperation, uFileSourceMoveOperation,
  fFileOpDlg, uFileSourceProperty, uFileSourceExecuteOperation, uArchiveFileSource,
  uShellExecute, uActs, fSymLink, fHardLink, uExceptions, uUniqueInstance, Clipbrd,
  uFileSourceOperationOptionsUI, uDebug, uHotkeyManager
  {$IFDEF LCLQT}
    , qtwidgets
  {$ENDIF}
  ;

{$IF DEFINED(LCLGTK2) or DEFINED(LCLQT)}
var
  LastActiveWindow: TCustomForm = nil;
{$ENDIF}

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

    Result.OnCloseTabClicked := @NotebookCloseTabClicked;
    Result.OnMouseDown := @nbPageMouseDown;
    Result.OnMouseUp := @nbPageMouseUp;
    {$IF DECLARED(lcl_fullversion) and (lcl_fullversion >= 093100) and (lazRevision >= lazRevOnPageChangedRemoved)}
    Result.OnChange := @nbPageChanged;
    {$ELSE}
    Result.OnPageChanged := @nbPageChanged;
    {$ENDIF}
    {$IF DEFINED(LCLGTK2)}
    Result.OnDblClick := @pnlLeftRightDblClick;
    {$ENDIF}
  end;
  function GenerateTitle():String;
  begin
    Result := Format('%s %s build %s; %s', ['Double Commander', dcVersion, dcRevision, dcBuildDate]);
  end;

var
  slCommandHistory: TStringListEx;
  I: Integer;
  HMMainForm: THMForm;
begin
  Application.OnException := @AppException;
  Application.OnActivate := @AppActivate;

  //Caption of main window
  Self.Caption := GenerateTitle();

  InitPropStorage(Self);

  DrivesList := nil;
  FDropParams := nil;
  cmdConsole:= nil;

  PanelSelected:=fpLeft;
  HiddenToTray := False;
  HidingTrayIcon := False;
  FResizingFilePanels := False;

  HMMainForm := HotMan.Register(Self, 'Main');
  HotMan.Register(edtCommand, 'Command Line');

  nbLeft := CreateNotebook(pnlLeft, fpLeft);
  nbRight := CreateNotebook(pnlRight, fpRight);

  FDrivesListPopup := TDrivesListPopup.Create(Self, Self);
  FDrivesListPopup.OnDriveSelected := @DriveListDriveSelected;
  FDrivesListPopup.OnClose := @DriveListClose;

  TDriveWatcher.Initialize(Handle);
  TDriveWatcher.AddObserver(@OnDriveWatcherEvent);

  if gOnlyOneAppInstance and Assigned(UniqueInstance) then
    UniqueInstance.OnMessage:= @OnUniqueInstanceMessage;

  // frost_asm begin
  MainSplitterLeftMouseBtnDown:=false;
  // frost_asm end
  MainFormCreate(Self);

  if mbFileExists(gpCfgDir + cHistoryFile) then
    begin
      slCommandHistory:= TStringListEx.Create;
      try
        slCommandHistory.LoadFromFile(gpCfgDir + cHistoryFile);
        edtCommand.Items.Assign(slCommandHistory);
        edtCommand.Text := '';
      finally
        FreeAndNil(slCommandHistory);
      end;
    end;

  // Initialize actions.
  actShowSysFiles.Checked := uGlobs.gShowSystemFiles;
  actHorizontalFilePanels.Checked := gHorizontalFilePanels;

  AllowDropFiles := not uDragDropEx.IsExternalDraggingSupported;

  pmButtonMenu.BarFile.ChangePath := gpExePath;
  pmButtonMenu.BarFile.EnvVar := '%commander_path%';

  // Use the same tooltips for some left and right panel butttons.
  btnRightDirectoryHotlist.Hint := btnLeftDirectoryHotlist.Hint;
  btnRightHome.Hint := btnLeftHome.Hint;
  btnRightRoot.Hint := btnLeftRoot.Hint;
  btnRightUp.Hint := btnLeftUp.Hint;

  { *HotKeys* }
  for i:=0 to actionLst.ActionCount -1 do
    if actionLst[i] is TAction then
      Actions.AddAction(TAction(actionLst[i]));

  if (HotMan.Forms.Count = 0) or (HotMan.Version < hkVersion) then
    LoadDefaultHotkeyBindings;

  // Register action list for main form hotkeys.
  HMMainForm.RegisterActionList(actionlst);
  { *HotKeys* }

  LoadWindowState;

  // frost_asm begin
    lastWindowState:=WindowState;
  // frost_asm end

  UpdateWindowView;
  //DCDebug('frmMain.FormCreate Done');
  Draging:=false;
end;

procedure TfrmMain.btnLeftClick(Sender: TObject);
begin
  PanelButtonClick(Sender as TSpeedButton, FrameLeft, fpLeft);
end;

procedure TfrmMain.actExecute(Sender: TObject);
var
  cmd: string;
begin
  cmd := (Sender as TAction).Name;
  cmd := 'cm_' + Copy(cmd, 4, Length(cmd) - 3);
  Actions.Execute(cmd);
end;

procedure TfrmMain.AllOpCancelClick(Sender: TObject);
begin
  OperationsManager.CancelAll;
end;

procedure TfrmMain.AllOpPauseClick(Sender: TObject);
begin
  OperationsManager.PauseRunning;
end;

procedure TfrmMain.AllOpPctClick(Sender: TObject);
begin
  ShowOperationsViewer;
end;

procedure TfrmMain.AllOpStartClick(Sender: TObject);
begin
   OperationsManager.StartRunning;
end;

procedure TfrmMain.btnF8Click(Sender: TObject);
begin
  if GetKeyShiftStateEx * KeyModifiersShortcut = [ssShift] then
    Actions.cm_Delete('recyclesettingrev')
  else
    Actions.cm_Delete('');
end;

procedure TfrmMain.btnLeftDirectoryHotlistClick(Sender: TObject);
Var P:TPoint;
begin
  SetActiveFrame(fpLeft);
  CreatePopUpHotDir;// TODO: i thing in future this must call on create or change
  p := Classes.Point(btnLeftDirectoryHotlist.Left,btnLeftDirectoryHotlist.Height);
  p := pnlLeftTools.ClientToScreen(p);
  pmHotList.PopUp(P.x,P.y);
end;

procedure TfrmMain.btnRightClick(Sender: TObject);
begin
  PanelButtonClick(Sender as TSpeedButton, FrameRight, fpRight);
end;

procedure TfrmMain.btnRightDirectoryHotlistClick(Sender: TObject);
Var P:TPoint;
begin
  SetActiveFrame(fpRight);
  CreatePopUpHotDir;// TODO: i thing in future this must call on create or change
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

procedure TfrmMain.dskLeftRightToolButtonDragDrop(Sender, Source: TObject; X,
  Y: Integer; NumberOfButton: Integer);
var
  TargetPath: String;
begin
  TargetPath := ((Sender as TSpeedButton).Parent as TKASToolBar).Commands[NumberOfButton];
  case GetDropEffectByKeyAndMouse(GetKeyShiftState, mbLeft) of
    DropCopyEffect:
      Self.CopyFiles(TargetPath, gShowDialogOnDragDrop);
    DropMoveEffect:
      Self.MoveFiles(TargetPath, gShowDialogOnDragDrop);
  end;
end;

procedure TfrmMain.dskToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);
begin
  btnDriveMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TfrmMain.lblAllProgressPctClick(Sender: TObject);
begin
  ShowOperationsViewer;
end;

procedure TfrmMain.MainToolBarToolButtonDragDrop(Sender, Source: TObject; X,
  Y: Integer; NumberOfButton: Integer);
var
  aFile: TFile;
  Cmd, Param, Path: string;
begin
  if (ssShift in GetKeyShiftState) then
    SaveMainToolBar
  else
    if Sender is TSpeedButton and not Draging then
      begin
        aFile := ActiveFrame.CloneActiveFile;
        try
          if Assigned(aFile) and aFile.IsNameValid then
            begin
              Cmd:= MainToolBar.GetButtonX(NumberOfButton, CmdX);
              Path:= MainToolBar.GetButtonX(NumberOfButton, PathX);
              Param:= QuoteStr(aFile.FullPath);
              ExecCmd(Cmd, Param, Path);
            end;
        finally
          FreeAndNil(aFile);
        end;
      end;
end;

procedure TfrmMain.MainToolBarToolButtonDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean; NumberOfButton: Integer);
var
  aFile: TFile;
  tmp: integer;
begin
  if (ssShift in GetKeyShiftState) then
    begin
      if not (Source is TSpeedButton) and not Draging then
        begin
          aFile := ActiveFrame.CloneActiveFile;
          try
            if Assigned(aFile) and aFile.IsNameValid then
              begin
                MainToolBar.InsertButtonX((Sender as TSpeedButton).Tag, '', aFile.FullPath, '', aFile.Path,
                                          ExtractOnlyFileName(aFile.Name), '',  aFile.FullPath);
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
      aFile := ActiveFrame.CloneActiveFile;
      try
        Accept := Assigned(aFile) and aFile.IsNameValid and not Draging;
      finally
        FreeAndNil(aFile);
      end;
    end;
end;

procedure TfrmMain.MainToolBarToolButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  NumberOfButton: Integer);
begin
  if (ssShift in Shift) and (Button = mbLeft) then
    begin
      MainToolBar.Buttons[NumberOfButton].BeginDrag(False, 5);
      NumberOfMoveButton:= (Sender as TSpeedButton).Tag;
    end;
  Draging:= False;
end;

procedure TfrmMain.MainToolBarToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);
begin
  if Button = mbRight then
    MainToolBarMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TfrmMain.miLogMenuClick(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    0:
      seLogWindow.CopyToClipboard;
    1:
      seLogWindow.SelectAll;
    2:
      Actions.cm_ClearLogWindow();
    3:
      ShowLogWindow(False);
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

procedure TfrmMain.PanelButtonClick(Button: TSpeedButton; SourceFrame: TFileView;
                                    PanelSelect: TFilePanelSelect);
var
  aFile : UTF8String;
begin
  with SourceFrame do
  begin
    if Button.Caption = '/' then
      CurrentPath := FileSource.GetRootDir(CurrentPath)
    else if Button.Caption = '..' then
      begin
        aFile := ExtractFileName(ExcludeTrailingPathDelimiter(CurrentPath));
        CurrentPath := FileSource.GetParentDir(CurrentPath);
        ActiveFrame.SetActiveFile(aFile);
      end
    else if Button.Caption = '~' then
      SetFileSystemPath(SourceFrame, GetHomeDir);
  end;

  SetActiveFrame(PanelSelect);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  slCommandHistory: TStringListEx;
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

  if gSaveCmdLineHistory then
    begin
      try
        slCommandHistory:= TStringListEx.Create;
        slCommandHistory.Assign(edtCommand.Items);
        slCommandHistory.SaveToFile(gpCfgDir + cHistoryFile);
      finally
        FreeThenNil(slCommandHistory);
      end;
    end;

  if gSaveConfiguration then
    begin
      // Save main toolbar
      SaveMainToolBar;
    end;

  FreeAndNil(DrivesList);

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
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  TargetFileView: TFileView;
  TargetControl: TControl;
  I: Integer;
  Files: TFiles = nil;
  FileNamesList: TStringList = nil;
  Point: TPoint;
  DropParams: TDropParams;
begin
  TargetFileView := nil;

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
      FileNamesList.Add(FileNames[I]);

    Files := TFileSystemFileSource.CreateFilesFromFileList(
        ExtractFilePath(FileNames[Low(FileNames)]), FileNamesList);

    GetCursorPos(Point);

    DropParams := TDropParams.Create(
        Files,
        GetDropEffectByKeyAndMouse(GetKeyShiftState, mbLeft),
        Point, False,
        nil, TargetFileView,
        TargetFileView.CurrentPath);

    DropFiles(DropParams);

  finally
    if Assigned(Files) then
      FreeAndNil(Files);
    if Assigned(FileNamesList) then
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
      case Operation of

        ddoMove:
          if GetDragDropType = ddtInternal then
          begin
            Self.MoveFiles(SourcePanel.FileSource,
                           TargetPanel.FileSource,
                           Files, TargetPath,
                           gShowDialogOnDragDrop);
          end
          else
          begin
            Self.MoveFiles(TFileSystemFileSource.GetFileSource,
                           TargetPanel.FileSource,
                           Files, TargetPath,
                           gShowDialogOnDragDrop);
          end;

        ddoCopy:
          if GetDragDropType = ddtInternal then
          begin
            Self.CopyFiles(SourcePanel.FileSource,
                           TargetPanel.FileSource,
                           Files, TargetPath,
                           gShowDialogOnDragDrop);
          end
          else
          begin
            Self.CopyFiles(TFileSystemFileSource.GetFileSource,
                           TargetPanel.FileSource,
                           Files, TargetPath,
                           gShowDialogOnDragDrop);
          end;

        ddoSymLink, ddoHardLink:
          begin
            // Only for filesystem.
            if  ((GetDragDropType = ddtExternal) or
                (SourcePanel.FileSource.IsClass(TFileSystemFileSource)))
            and (TargetPanel.FileSource.IsClass(TFileSystemFileSource)) then
            begin
              // TODO: process multiple files

              SourceFileName := Files.Items[0].FullPath;
              TargetFileName := TargetPath + ExtractFileName(SourceFileName);

              if ((Operation = ddoSymLink) and
                 ShowSymLinkForm(SourceFileName, TargetFileName, TargetPath))
              or ((Operation = ddoHardLink) and
                 ShowHardLinkForm(SourceFileName, TargetFileName, TargetPath))
              then
                TargetPanel.Reload;
            end
            else
            begin
              msgWarning(rsMsgErrNotSupported);
            end;
          end;
      end;
    end;

  finally
    if Assigned(DropParams) then
      FreeAndNil(DropParams);
  end;
end;

procedure TfrmMain.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  ModifierKeys: TShiftState;
begin
  // Either left or right panel has to be focused.
  if not FrameLeft.Focused and
     not FrameRight.Focused then
  begin
    Exit;
  end;

  if (edtCommand.Tag = 0) and
     // Check for certain Ascii keys.
     (not ((Length(UTF8Key) = 1) and (UTF8Key[1] in ['-', '*', '+', #0..#32]))) then
  begin
    ModifierKeys := GetKeyShiftStateEx;

    if (gKeyTyping[ktmNone] = ktaCommandLine)
{$IFDEF MSWINDOWS}
       // Allow entering international characters with Ctrl+Alt on Windows,
       // if there is no action for Ctrl+Alt and command line typing has no modifiers.
       or ((ModifierKeys * KeyModifiersShortcutNoText = [ssCtrl, ssAlt]) and
           (gKeyTyping[ktmCtrlAlt] = ktaNone))
{$ENDIF}
      then
      begin
        TypeInCommandLine(UTF8Key);
        UTF8Key := '';
      end;
  end
end;

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
  begin  // Minimized
    MainToolBar.Top:= 0; // restore toolbar position
    if not HiddenToTray then
    begin
      if gMinimizeToTray or gAlwaysShowTrayIcon then
      begin
        HideToTray;
      end;
    end
    else
      // If we get wsMinimized while HiddenToTray is true,
      // then this means it was sent by LCL when a hidden, minimized window was shown.
      // We don't react to this message in this case.
      HiddenToTray := False;
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
  Actions.cm_PanelsSplitterPerPos('50');
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

      // calculate persent
      if not gHorizontalFilePanels then
        sHint:= FloatToStrF(MainSplitter.Left*100 / (pnlNotebooks.Width-MainSplitter.Width), ffFixed, 15, 1) + '%'
      else
        sHint:= FloatToStrF(MainSplitter.Top*100 / (pnlNotebooks.Height-MainSplitter.Height), ffFixed, 15, 1) + '%';

      //calculate hint position
      Rect:= MainSplitterHintWnd.CalcHintRect(200, sHint, nil);
      APoint:= Mouse.CursorPos;
      with Rect do
      begin
        Right:= APoint.X + 8 + Right;
        Bottom:= APoint.Y + 12 + Bottom;
        Left:= APoint.X + 8;
        Top:= APoint.Y + 12;
      end;

      //show hint
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

procedure TfrmMain.lblDriveInfoDblClick(Sender: TObject);
begin
  if Sender = lblRightDriveInfo then
      SetActiveFrame(fpRight)
  else if Sender = lblLeftDriveInfo then
      SetActiveFrame(fpLeft);
  Actions.cm_DirHotList('');
//  actDirHotList.Execute;
end;

procedure TfrmMain.MainToolBarDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aFile: TFile;
  Cmd,Param,Path : string;
  TargetControl: TControl;
begin
  if not (Source is TSpeedButton) and not Draging and (ssShift in GetKeyShiftState) then
    begin
      aFile := ActiveFrame.CloneActiveFile;
      try
        if Assigned(aFile) and aFile.IsNameValid then
        begin
          MainToolBar.AddButtonX('', aFile.FullPath, '', aFile.Path,
                                 ExtractOnlyFileName(aFile.Name), '',  aFile.FullPath);
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

function TfrmMain.MainToolBarLoadButtonGlyph(sIconFileName: String;
  iIconSize: Integer; clBackColor: TColor): TBitmap;
begin
  Result := PixMapManager.LoadBitmapEnhanced(sIconFileName, iIconSize, True, clBackColor);
end;

procedure TfrmMain.DeleteClick(Sender: TObject);
begin
  if pmToolBar.Tag >= 0 then
  begin
    if msgYesNo(Format(rsMsgDelSel, [MainToolBar.Buttons[pmToolBar.Tag].Hint])) then
    begin
       MainToolBar.RemoveButton (pmToolBar.Tag);
       SaveMainToolBar;
    end;
  end;
end;

procedure TfrmMain.dskToolButtonClick(Sender: TObject; NumberOfButton: Integer);
var
  dskPanel : TKASToolBar;
  FileView : TFileView;
  DriveNr  : Integer;
begin
  dskPanel := (Sender as TKASToolBar);

  if (dskPanel = dskLeft) or (not gDriveBar2 and (PanelSelected = fpLeft))  then
    begin
      FileView := FrameLeft;
      PanelSelected := fpLeft;
    end
  else
    begin
      FileView := FrameRight;
      PanelSelected := fpRight;
    end;
    
  if dskPanel.Buttons[NumberOfButton].GroupIndex = 0 then
    begin
      // Command := dskPanel.Commands[NumberOfButton];
      PanelButtonClick(dskPanel.Buttons[NumberOfButton], FileView, PanelSelected)
    end
  else
    begin
      DriveNr := dskPanel.Buttons[NumberOfButton].Tag;
      if (Tag >= 0) and (Tag < DrivesList.Count) then
        SetPanelDrive(PanelSelected, DrivesList[DriveNr]);
    end;

  SetActiveFrame(PanelSelected);
end;

procedure TfrmMain.btnVirtualDriveClick(Sender: TObject);
begin
  Actions.cm_OpenVirtualFileSystemList(((Sender as TSpeedButton).Parent as TKASToolBar).Name);
end;

procedure TfrmMain.MainToolBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Point : TPoint;
begin
  if (Sender is TSpeedButton) then
    pmToolBar.Tag := (Sender as TSpeedButton).Tag
  else
    pmToolBar.Tag := -1;

  case Button of
    mbMiddle:
      begin
        tbEditClick(Sender);
      end;

    mbRight:
      begin
        Point.X := X;
        Point.Y := Y;
        Point := (Sender as TControl).ClientToScreen(Point);
        if (Sender is TSpeedButton) then
          begin
            tbDelete.Enabled := True;
          end
        else
          begin
            tbDelete.Enabled := False;
          end;

        pmToolBar.PopUp(Point.X, Point.Y);
      end;
  end;
end;

procedure TfrmMain.MainToolBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
begin
  if not Draging then
    begin
      ExecuteToolButton(Sender, NumberOfButton);
      DCDebug(MainToolBar.Commands[NumberOfButton]);
    end;
  Draging := false;
end;

procedure TfrmMain.frmMainClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Process all queued asynchronous events before closing
  // (frmMainAfterShow, nbPageAfterMouseDown, etc.).
  Application.ProcessMessages;

  if gSaveConfiguration then
  try
    if Assigned(gIni) then
      uGlobs.ConvertIniToXml;
    SaveWindowState;
    SaveGlobs;
  except
  end;

  if Assigned(Cons) then
    Cons.Free;

  Application.Terminate;
end;

procedure TfrmMain.frmMainAfterShow(Data: PtrInt);
begin
  ActiveFrame.SetFocus;
  HiddenToTray := False;
end;

procedure TfrmMain.frmMainShow(Sender: TObject);
begin
  DCDebug('frmMain.frmMainShow');
  Application.QueueAsyncCall(@frmMainAfterShow, 0);
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
  if Assigned(FDropParams) then
    FreeAndNil(FDropParams);
end;

procedure TfrmMain.mnuSplitterPercentClick(Sender: TObject);
begin
  with (Sender as TMenuItem) do
  begin
    Actions.cm_PanelsSplitterPerPos(inttostr(Tag));
  end;
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

  if MenuItem = miRemoveTab then
    Actions.DoRemoveTab(NoteBook, pmTabMenu.Tag)
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

    Actions.Execute(Cmd, NoteBook.Name);
  end;
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
      Page.FileView.CurrentPath := Page.LockPath;

  // Update selected drive only on non-active panel,
  // because active panel is updated on focus change.
    if Assigned(ActiveFrame) and (ActiveFrame.Parent.Parent <> Sender) and
     not (tb_activate_panel_on_click in gDirTabOptions) then
  begin
    UpdateSelectedDrive(Notebook);
    UpdateFreeSpace(Notebook.Side);
  end;
  end;
  if Assigned(QuickViewPanel) then
    Actions.cm_QuickView('Close');
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
        TabNr := NoteBook.TabIndexAtClientPos(Point(X, Y));
        if TabNr <> -1 then
        begin
          Actions.DoRemoveTab(NoteBook, TabNr);
        end;
      end;

    mbRight:
      begin
        TabNr := NoteBook.TabIndexAtClientPos(Point(X, Y));
        if TabNr <> -1 then
        begin
          PopUpPoint := NoteBook.ClientToScreen(Point(X, Y));

{$IFDEF LCLQT}
          // In QT the NoteBook.ClientToScreen calculates coordinates
          // relative to Page contents (client rectangle),
          // so we must substract the height and width of the tab bar.
          PopUpPoint.X := PopUpPoint.X - (NoteBook.Width - NoteBook.ClientWidth);
          PopUpPoint.Y := PopUpPoint.Y - (NoteBook.Height - NoteBook.ClientHeight);
{$ENDIF}

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

procedure TfrmMain.NoteBookCloseTabClicked(Sender: TObject);
begin
  with (Sender As TFileViewPage) do
  if PageIndex <> -1 then
  begin
    RemovePage(Notebook, PageIndex);
  end;
end;

function TfrmMain.pmButtonMenuLoadButtonGlyph(sIconFileName: String;
  iIconSize: Integer; clBackColor: TColor): TBitmap;
begin
  Result := PixMapManager.LoadBitmapEnhanced(sIconFileName, iIconSize, True, clBackColor);
end;

procedure TfrmMain.pmButtonMenuMenuButtonClick(Sender: TObject;
  NumberOfButton: Integer);
begin
  ExecuteToolButton(Sender, NumberOfButton);
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
     (edtCommand.Tag = 0) and
     not ((Key in ['-', '*', '+', #0..#32]) and (Trim(edtCommand.Text) = '')) then
  begin
    TypeInCommandLine(Key);
    Key := #0;
  end;
end;

Function TfrmMain.ActiveFrame: TFileView;
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

Function TfrmMain.NotActiveFrame: TFileView;
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

procedure TfrmMain.AppException(Sender: TObject; E: Exception);
begin
  WriteExceptionToErrorFile;
  ShowExceptionDialog;
end;

Function TfrmMain.GetFileDlgStr(sLngOne, sLngMulti: String; Files: TFiles):String;
begin
  if Files.Count = 0 then
    raise Exception.Create(rsMsgNoFilesSelected);

  if Files.Count > 1 then
    Result := Format(sLngMulti, [Files.Count])
  else
    Result := Format(sLngOne, [Files[0].Name]);
end;


procedure TfrmMain.miHotAddClick(Sender: TObject);
var
  sName: UTF8String;
begin
  sName:= StringReplace(GetLastDir(ActiveFrame.CurrentPath), '&', '&&', [rfReplaceAll]);
  glsHotDir.Add(sName + '=' + ActiveFrame.CurrentPath);
end;

procedure TfrmMain.miHotDeleteClick(Sender: TObject);
var
  I: Integer;
begin
  I:= glsHotDir.IndexOfValue(ActiveFrame.CurrentPath);
  if I >= 0 then glsHotDir.Delete(I);
end;

procedure TfrmMain.miHotConfClick(Sender: TObject);
begin
  with TfrmHotDir.Create(Application) do
  begin
    try
      LoadFromGlob;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.CreatePopUpDirHistory;
var
  mi: TMenuItem;
  I: Integer;
begin
  pmDirHistory.Items.Clear;

  // store only first gDirHistoryCount of DirHistory
  for I:= glsDirHistory.Count - 1 downto 0 do
    if I > gDirHistoryCount then
      glsDirHistory.Delete(I)
    else
      Break;

  for I:= 0 to glsDirHistory.Count - 1 do
  begin
    mi:= TMenuItem.Create(pmDirHistory);
    mi.Caption:= glsDirHistory[I];
    mi.Hint:= mi.Caption;
    mi.OnClick:= @HotDirSelected;
    pmDirHistory.Items.Add(mi);
  end;
end;

procedure TfrmMain.ShowFileViewHistory;
begin
  ShowFileViewHistory(-1, -1, -1, -1);
end;

procedure TfrmMain.ShowFileViewHistory(FromFileSourceIndex, FromPathIndex,
                                       ToFileSourceIndex, ToPathIndex: Integer);
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
  I: Integer;
  mi: TMenuItem;
  p: TPoint;
begin
  pmDirHistory.Items.Clear;

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

    mi.Caption := ActiveFrame.Path[FromFileSourceIndex, FromPathIndex];
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
      AddCaptionItem('- ' + ActiveFrame.FileSources[FromFileSourceIndex].CurrentAddress + ' -');
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

  p := ActiveFrame.ClientToScreen(Classes.Point(0,0));
  pmDirHistory.Popup(p.X, p.Y);
end;

procedure TfrmMain.CreatePopUpHotDir;
var
  mi: TMenuItem;
  I: Integer;
begin
  // Create All popup menu
  pmHotList.Items.Clear;
  for I:= 0 to glsHotDir.Count - 1 do
  begin
    mi:= TMenuItem.Create(pmHotList);
    if Pos('&', glsHotDir.Names[I]) = 0 then
      mi.Caption:= '&' + glsHotDir.Names[I]
    else
      mi.Caption:= glsHotDir.Names[I];
    mi.Hint:= glsHotDir.ValueFromIndex[I];
    mi.OnClick:= @HotDirSelected;
    pmHotList.Items.Add(mi);
  end;
  // now add delimiter
  mi:= TMenuItem.Create(pmHotList);
  mi.Caption:= '-';
  pmHotList.Items.Add(mi);
  // now add ADD or DELETE item

  mi:= TMenuItem.Create(pmHotList);
  if glsHotDir.IndexOfValue(ActiveFrame.CurrentPath) >= 0 then
    begin
      mi.Caption:= Format(rsMsgPopUpHotDelete,[ActiveFrame.CurrentPath]);
      mi.OnClick:= @miHotDeleteClick;
    end
  else
    begin
      mi.Caption:= Format(rsMsgPopUpHotAdd,[ActiveFrame.CurrentPath]);
      mi.OnClick:= @miHotAddClick;
    end;
  pmHotList.Items.Add(mi);

  // now add configure item
  mi:= TMenuItem.Create(pmHotList);
  mi.Caption:= rsMsgPopUpHotCnf;
  mi.OnClick:= @miHotConfClick;
  pmHotList.Items.Add(mi);
end;

procedure TfrmMain.HotDirSelected(Sender: TObject);
var
  aPath: String;
begin
  // This handler is used by HotDir and DirHistory.
  // Hot dirs are only supported by filesystem.
  aPath := (Sender as TMenuItem).Hint;
  aPath := mbExpandFileName(aPath);
  SetFileSystemPath(ActiveFrame, aPath);
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
    ShowFileViewHistory(-1, -1, FileSourceIndex, PathIndex);
  end;
end;

procedure TfrmMain.ViewHistoryNextSelected(Sender:TObject);
var
  FileSourceIndex, PathIndex: Integer;
begin
  if Sender is TMenuItem then
  begin
    HistoryIndexesFromTag((Sender as TMenuItem).Tag, FileSourceIndex, PathIndex);
    ShowFileViewHistory(FileSourceIndex, PathIndex, -1, -1);
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

function TfrmMain.CopyFiles(SourceFileSource, TargetFileSource: IFileSource;
                            var SourceFiles: TFiles; TargetPath: String;
                            bShowDialog: Boolean): Boolean;
var
  sDestination: String;
  sDstMaskTemp: String;
  Operation: TFileSourceCopyOperation = nil;
  OperationHandle: TOperationHandle;
  OperationType: TFileSourceOperationType;
  ProgressDialog: TfrmFileOp;
  CopyDialog: TfrmCopyDlg = nil;
  OperationStartingState: TOperationStartingState = ossAutoStart;
  OperationClass: TFileSourceOperationClass;
  OperationOptionsUIClass: TFileSourceOperationOptionsUIClass = nil;
  ExcludeRootDir: Boolean = False;
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
      OperationClass := SourceFileSource.GetOperationClass(fsoCopy);
    end
    else if TargetFileSource.IsClass(TFileSystemFileSource) and
            (fsoCopyOut in SourceFileSource.GetOperationsTypes) then
    begin
      OperationType := fsoCopyOut;
      OperationClass := SourceFileSource.GetOperationClass(fsoCopyOut);
    end
    else if SourceFileSource.IsClass(TFileSystemFileSource) and
            (fsoCopyIn in TargetFileSource.GetOperationsTypes) then
    begin
      OperationType := fsoCopyIn;
      OperationClass := TargetFileSource.GetOperationClass(fsoCopyIn);
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

      CopyDialog := TfrmCopyDlg.Create(Application, cmdtCopy, OperationOptionsUIClass);
      with CopyDialog do
      begin
        edtDst.Text := sDestination;
        lblCopySrc.Caption := GetFileDlgStr(rsMsgCpSel, rsMsgCpFlDr, SourceFiles);

        if ShowModal = mrCancel then
          Exit;

        sDestination := edtDst.Text;
      end;

      // Check TargetPath before it is modified in GetDestinationPathAndMask.
      if SourceFiles.Count = 1 then
      begin
        if SourceFiles[0].IsDirectory or SourceFiles[0].IsLinkToDirectory then
        begin
          if (Pos('*', sDestination) = 0) and (Pos('?', sDestination) = 0) and
             (not StrEnds(sDestination, PathDelim)) then
          begin
            // Assume it is a path to a directory.
            sDestination := IncludeTrailingPathDelimiter(sDestination) + '*.*';

            if (SourceFiles.Path = TargetPath) or (TargetPath = '') then
              ExcludeRootDir := True; // CopySamePanel
          end;
        end;
        // else single file - leave unchanged
      end
      else
      begin
        if (Pos('*', sDestination) = 0) and (Pos('?', sDestination) = 0) then
          // Assume it is a path to a directory.
          sDestination := IncludeTrailingPathDelimiter(sDestination);
      end;

      OperationStartingState := CopyDialog.OperationStartingState;
    end;

    GetDestinationPathAndMask(TargetFileSource, sDestination,
                              SourceFiles.Path, TargetPath, sDstMaskTemp);

    case OperationType of
      fsoCopy:
        begin
          // Copy within the same file source.
          Operation := SourceFileSource.CreateCopyOperation(
                         SourceFiles,
                         TargetPath) as TFileSourceCopyOperation;

          Operation.ExcludeRootDir := ExcludeRootDir;
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

      // Start operation.
      OperationHandle := OperationsManager.AddOperation(Operation, OperationStartingState);

      ProgressDialog := TfrmFileOp.Create(OperationHandle);
      ProgressDialog.Show;

      Result := True;
    end
    else
      msgWarning(rsMsgNotImplemented);

  finally
    if Assigned(SourceFiles) then
      FreeAndNil(SourceFiles);
    if Assigned(CopyDialog) then
      FreeAndNil(CopyDialog);
  end;
end;

function TfrmMain.MoveFiles(SourceFileSource, TargetFileSource: IFileSource;
                            var SourceFiles: TFiles; TargetPath: String;
                            bShowDialog: Boolean): Boolean;
var
  sDestination: String;
  sDstMaskTemp: String;
  Operation: TFileSourceMoveOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
  bMove: Boolean;
  MoveDialog: TfrmCopyDlg = nil;
  OperationStartingState: TOperationStartingState = ossAutoStart;
begin
  Result := False;
  try
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
      MoveDialog := TfrmCopyDlg.Create(Application, cmdtMove,
        SourceFileSource.GetOperationClass(fsoMove).GetOptionsUIClass);

      with MoveDialog do
      begin
        edtDst.Text := sDestination;
        lblCopySrc.Caption := GetFileDlgStr(rsMsgRenSel, rsMsgRenFlDr, SourceFiles);

        if ShowModal = mrCancel then
          Exit;

        sDestination := edtDst.Text;
      end;

      OperationStartingState := MoveDialog.OperationStartingState;
    end;

    GetDestinationPathAndMask(TargetFileSource, sDestination,
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
        OperationHandle := OperationsManager.AddOperation(Operation, OperationStartingState);

        ProgressDialog := TfrmFileOp.Create(OperationHandle);
        ProgressDialog.Show;

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
    if Assigned(SourceFiles) then
      FreeAndNil(SourceFiles);
    if Assigned(MoveDialog) then
      FreeAndNil(MoveDialog);
  end;
end;

function TfrmMain.CopyFiles(sDestPath: String; bShowDialog: Boolean): Boolean;
var
  SourceFiles: TFiles = nil;
begin
  SourceFiles := ActiveFrame.CloneSelectedFiles;
  if Assigned(SourceFiles) then
  begin
    try
      Result := CopyFiles(ActiveFrame.FileSource, NotActiveFrame.FileSource,
                          SourceFiles, sDestPath, bShowDialog);
    finally
      if Assigned(SourceFiles) then
        FreeAndNil(SourceFiles);
    end;
  end
  else
    Result := False;
end;

function TfrmMain.MoveFiles(sDestPath: String; bShowDialog: Boolean): Boolean;
var
  SourceFiles: TFiles = nil;
begin
  SourceFiles := ActiveFrame.CloneSelectedFiles;
  if Assigned(SourceFiles) then
  begin
    try
      Result := MoveFiles(ActiveFrame.FileSource, NotActiveFrame.FileSource,
                          SourceFiles, sDestPath, bShowDialog);
    finally
      if Assigned(SourceFiles) then
        FreeAndNil(SourceFiles);
    end;
  end
  else
    Result := False;
end;

procedure TfrmMain.GetDestinationPathAndMask(TargetFileSource: IFileSource;
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

  if (TargetFileSource.IsClass(TFileSystemFileSource)) and
     (AbsolutePath[Length(AbsolutePath)] = PathDelim) then
  begin
    // If the entered path ends with a path delimiter
    // treat it as a path to a not yet existing directory
    // which should be created.
    DestPath := AbsolutePath;
    DestMask := '*.*';
  end
  else if (TargetFileSource.IsClass(TFileSystemFileSource)) and
          mbDirectoryExists(AbsolutePath) then
  begin
    // Destination is a directory.
    DestPath := IncludeTrailingPathDelimiter(AbsolutePath);
    DestMask := '*.*';
  end
  else
  begin
    // Destination is a file name or mask.
    DestPath := ExtractFilePath(AbsolutePath);
    DestMask := ExtractFileName(AbsolutePath);

    // For convenience, treat '*' as "whole file name".
    // To remove extension '*.' can be used.
    if DestMask = '*' then
      DestMask := '*.*';
  end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ShiftEx : TShiftState;
  CmdText : UTF8String;
begin
  // Either left or right panel has to be focused.
  if not FrameLeft.Focused and
     not FrameRight.Focused then
  begin
    Exit;
  end;

  ShiftEx := GetKeyShiftStateEx;

  case Key of
    VK_BACK:
      if (GetKeyTypingAction(ShiftEx) = ktaCommandLine) and (edtCommand.Text <> '') then
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
  I: Integer;
  sText: String;
  sDir: String;
  bPaste: Boolean;
begin
  I:= pmToolBar.Tag;
  tbSeparator.Visible:= (I >= 0);
  tbCut.Visible:= (I >= 0);
  tbCopy.Visible:= (I >= 0);
  tbChangeDir.Visible:= false;
  if I >= 0 then
  begin
    sDir:= MainToolBar.GetButtonX(I, PathX);
    sDir:= PrepareParameter(sDir, FrameLeft, FrameRight, ActiveFrame, [ppoNormalizePathDelims, ppoReplaceTilde]);
    tbChangeDir.Caption := 'CD ' + sDir;
    if sDir <> '' then
      tbChangeDir.Visible:= true;
  end;

  sText:= Clipboard.AsText;
  bPaste:= (Pos('DOUBLECMD#BAR#DATA', sText) = 1) or (Pos('TOTALCMD#BAR#DATA', sText) = 1);
  if bPaste then
    tbSeparator.Visible:= True;
  tbPaste.Visible:= bPaste;
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
begin
  if Sender is TPanel then
  begin
    APanel := Sender as TPanel;
    if APanel = pnlLeft then
      Actions.DoNewTab(nbLeft)
    else if APanel = pnlRight then
      Actions.DoNewTab(nbRight);
  end;
  {$IF DEFINED(LCLGTK2)}
  if Sender is TFileViewNotebook then
    Actions.DoNewTab(Sender as TFileViewNotebook);
  {$ENDIF}
end;

procedure TfrmMain.pnlNotebooksResize(Sender: TObject);
begin
  if not FResizingFilePanels then
  begin
    FResizingFilePanels := True;
    if not gHorizontalFilePanels then
      pnlLeft.Width := (pnlNotebooks.Width - MainSplitter.Width) div 2
    else
      pnlLeft.Height := (pnlNotebooks.Height - MainSplitter.Height) div 2;
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
var
  pbxDrive: TPaintBox absolute Sender;
  indexColor: Int64;
  i: Integer;
begin
  indexColor:= pbxDrive.Tag;
  if indexColor <> -1 then
    begin
      if IndexColor<=50 then pbxDrive.Canvas.Brush.Color := RGB (0+5*(IndexColor),255,0)
                        else pbxDrive.Canvas.Brush.Color := RGB (255,255-5*(IndexColor-50),0);

      pbxDrive.Canvas.FillRect (
                                 2,
                                 0,
                                 pbxDrive.Width-2,
                                 pbxDrive.Height-1);

      pbxDrive.Canvas.Brush.Color := clBtnFace;

      pbxDrive.Canvas.FillRect(
                                3,
                                1,
                                pbxDrive.Width-3,
                                pbxDrive.Height-2);

      for i:=0 to  IndexColor-2 do
        begin
          if i<=50 then pbxDrive.Canvas.Brush.Color := RGB (0+5*i,255,0)
                   else pbxDrive.Canvas.Brush.Color := RGB (255,255-5*(i-50),0);

          pbxDrive.Canvas.FillRect(
                                    4+i*(pbxDrive.Width-4) div 100,
                                    2,
                                    4+(i+1)*(pbxDrive.Width-4) div 100,
                                    pbxDrive.Height-3);
        end;
    end;
end;

procedure TfrmMain.sboxOperationsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  OperationNumber: Integer;
  CursorPos: TPoint;
  Operation: TFileSourceOperation;
begin
  CursorPos := Mouse.CursorPos;
  CursorPos := sboxOperations.ScreenToClient(CursorPos);
  OperationNumber := CursorPos.X div widthOfItem;
  if (OperationNumber < 0) or (OperationNumber > OperationsManager.OperationsCount) then Exit;
  case Button of
    mbMiddle:
      begin
        OperationsManager.MoveOperation(OperationNumber, OperationNumber - 1);
      end;
    mbRight:
      begin
        Operation:=OperationsManager.GetOperationByIndex(OperationNumber);
         if Assigned(Operation) then
           begin
             if Operation.State = fsosRunning then
               begin
                 OperationsManager.SetPauseRunning(OperationsManager.GetHandleById(OperationNumber), True);
                 Operation.Pause;
               end
             else
               begin
                 OperationsManager.InQueue(OperationsManager.GetHandleById(OperationNumber), true);
               end;
           end;
      end;
    mbLeft:
      begin
        indexFocus:= OperationNumber;
        PressLMB:=true;
        ItemEnd:= OperationNumber;
      end;
  end;
end;

procedure TfrmMain.sboxOperationsMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if PressLMB then ItemEnd:= X div widthOfItem;
end;

procedure TfrmMain.sboxOperationsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OperationDialog: TfrmFileOp;
begin
  if Button=mbLeft then
    begin
      PressLMB:=false;
      if (ItemEnd < 0) then OperationsManager.MoveOperation(IndexFocus, 0);
      if (ItemEnd > OperationsManager.OperationsCount) then OperationsManager.MoveOperation(IndexFocus, OperationsManager.OperationsCount);
      if IndexFocus=ItemEnd then
        begin
          if OperationsManager.GetFormCreate (OperationsManager.GetHandleById(IndexFocus)) = False then //Check if current operation has a form
            begin
              OperationDialog := TfrmFileOp.Create(OperationsManager.GetHandleById(IndexFocus)); // create it if it hasn't
              OperationDialog.Show;
              OperationsManager.SetFormCreate (OperationsManager.GetHandleById(IndexFocus), True); // Show form presence
            end;
         end
      else OperationsManager.MoveOperation(IndexFocus, ItemEnd);
    end;
  IndexFocus:=ItemEnd;
end;

procedure TfrmMain.sboxOperationsPaint(Sender: TObject);
var
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  StartingState: TOperationStartingState;
  i, textHeight: Integer;
  OutString: String;
begin
  if OperationsManager.OperationsCount > 0 then
    begin
      // Calculate text height
      textHeight:= sboxOperations.Canvas.TextHeight('Pg');
      // Calculate item width
      if (sboxOperations.Width / OperationsManager.OperationsCount) < 120 then
        widthOfItem := Round(sboxOperations.Width / OperationsManager.OperationsCount)
      else
        widthOfItem := 120;

    for i := 0 to OperationsManager.OperationsCount - 1 do
    begin
      if (i=ItemEnd) and (PressLMB) or (i=IndexFocus) and (PressLMB) then
        begin
          sboxOperations.Canvas.Brush.Color := clMenuHighlight;
          sboxOperations.Canvas.Rectangle(10 + (widthOfItem * i), 5,  widthOfItem-10 + (widthOfItem * i),  sboxOperations.Height - 8);
          if IndexFocus<>ItemEnd then  OutString:= 'Move here' else  OutString:= 'Move from';
          if i= IndexFocus then OutString:= 'Move from';
          sboxOperations.Canvas.TextOut(18 + (widthOfItem * i), 7, OutString);
        end
      else
      begin
      Operation := OperationsManager.GetOperationByIndex(i);
      if Assigned(Operation) then
      begin
      OperationHandle := OperationsManager.GetHandleById(i);
      case Operation.ID of
        fsoCopy, fsoCopyIn, fsoCopyOut:
          OutString := 'Copying';
        fsoMove:
          OutString := 'Moving';
        fsoDelete:
          OutString := 'Delete';
        fsoWipe:
          OutString := 'Erasing';
        fsoCalcChecksum:
          OutString := 'Counting';
        else
          OutString := 'Unknown';
      end;

      OutString := IntToStr(OperationHandle) + ': '
                 + OutString + ' - '
                 + FloatToStrF(Operation.Progress * 100, ffFixed, 0, 0) + ' %';

      StartingState := OperationsManager.GetStartingState(OperationHandle);
      if OperationsManager.GetFormCreate (OperationHandle) = True  then
         sboxOperations.Canvas.Brush.Color := clBtnShadow
      else
         sboxOperations.Canvas.Brush.Color := Canvas.Brush.Color;
      // Draw border
      sboxOperations.Canvas.Rectangle(0 + (widthOfItem * i), 0,  widthOfItem + (widthOfItem * i),  sboxOperations.Height - 4);
      // Draw output string
      sboxOperations.Canvas.TextOut(3 + (widthOfItem * i), 2, OutString);

      // set progress bar color by operation state

      sboxOperations.Canvas.Brush.Color := clRed; // If it is stopped then it should be red

      if (StartingState in [ossQueueIn, ossQueueFirst, ossQueueLast]) then     // Yellow if in queue
        sboxOperations.Canvas.Brush.Color := clYellow;
      if Operation.State = fsosRunning then             //Blue if running
        sboxOperations.Canvas.Brush.Color := clHighlight;
      // Draw progress bar
      sboxOperations.Canvas.FillRect(
        3 + (widthOfItem * i),
        2 + textHeight,
        Round(5 + (widthOfItem * i) + (widthOfItem - 10) * Operation.Progress),
        10 + textHeight);
      end;
    end;
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
    FG := clNavy;
  lmtSuccess:
    FG := clGreen;
  lmtError:
    FG := clRed
  else
    FG := clWindowText;
  end;
end;

function TfrmMain.FileViewBeforeChangePath(FileView: TFileView; NewFileSource: IFileSource; const NewPath: String): Boolean;
var
  ANoteBook: TFileViewNotebook;
  Page, NewPage: TFileViewPage;
begin
  Result:= True;
  if FileView.NotebookPage is TFileViewPage then
  begin
    Page := FileView.NotebookPage as TFileViewPage;

    case Page.LockState of
      tlsPathLocked:
        Result := False;  // do not change directory in this tab

      tlsDirsInNewTab:
        begin
          Result := False;  // do not change directory in this tab

          if Assigned(NewFileSource) then
          begin
            ANoteBook := Page.Notebook;

            // Create same type
            NewPage := ANoteBook.AddPage;
            Page.FileView.Clone(NewPage);
            NewPage.FileView.AddFileSource(NewFileSource, NewPath);
            NewPage.MakeActive;
          end;
        end;
    end;
  end;
end;

procedure TfrmMain.FileViewAfterChangePath(FileView: TFileView);
var
  ANoteBook : TFileViewNotebook;
  Page: TFileViewPage;
begin
  if FileView.NotebookPage is TFileViewPage then
    begin
      Page := FileView.NotebookPage as TFileViewPage;
      ANoteBook := Page.Notebook;

      if Page.LockState = tlsNormal then // if not locked tab
        Page.UpdateCaption(GetLastDir(FileView.CurrentPath));

      if Page.IsActive then
      begin
        if Assigned(FileView.FileSource) then
        begin
          if FileView.FileSource.IsClass(TFileSystemFileSource) then
          begin
            if glsDirHistory.IndexOf(FileView.CurrentPath) = -1 then
              glsDirHistory.Insert(0, FileView.CurrentPath);
          end;

          UpdateSelectedDrive(ANoteBook);
          UpdateFreeSpace(ANoteBook.Side);
          UpdatePrompt;
        end;
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
      PanelSelected := Page.Notebook.Side;
      UpdateSelectedDrive(Page.Notebook);
      UpdatePrompt;
      UpdateFreeSpace(Page.Notebook.Side);
    end;
end;

procedure TfrmMain.FileViewReload(FileView: TFileView);
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
  PanelSelected:=panel;
  ActiveFrame.SetFocus;
  if (fspDirectAccess in ActiveFrame.FileSource.GetProperties) then
    begin
      if gTermWindow and Assigned(Cons) then
        Cons.Terminal.SetCurrentDir(ActiveFrame.CurrentPath);
    end;
end;

function CompareDrives(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(PDrive(Item1)^.DisplayName, PDrive(Item2)^.DisplayName);
end;

procedure TfrmMain.UpdateDiskCount;
var
  I: Integer;
begin
  DrivesList.Free;
  DrivesList := TDriveWatcher.GetDrivesList;
  DrivesList.Sort(@CompareDrives);

  { Delete drives that in drives black list }
  for I:= DrivesList.Count - 1 downto 0 do
    begin
      if MatchesMaskList(DrivesList[I]^.Path, gDriveBlackList) then
        DrivesList.Remove(I);
    end;

  // create drives drop down menu
  FDrivesListPopup.UpdateDrivesList(DrivesList);

  // create drives left/right panels
  if gDriveBar1 then
  begin
    CreateDiskPanel(dskRight);
    if gDriveBar2 then
      CreateDiskPanel(dskLeft);
  end;
end;

procedure TfrmMain.AddVirtualDriveButton(dskPanel: TKASToolBar);
const
  btnCaption = ':' + PathDelim + PathDelim;
var
  btnIndex : Integer;
  bmpBitmap: TBitmap = nil;
begin
  (*virtual drive button*)
  bmpBitmap:= PixMapManager.GetVirtualDriveIcon(dskPanel.GlyphSize, clBtnFace);
  btnIndex := dskPanel.AddButtonX(btnCaption, '', '', '', '', '', bmpBitmap);
  dskPanel.Buttons[btnIndex].Caption:= btnCaption;
  dskPanel.Buttons[btnIndex].Hint:= actOpenVirtualFileSystemList.Caption;
  dskPanel.Buttons[btnIndex].GroupIndex := 0;
  dskPanel.Buttons[btnIndex].Layout := blGlyphLeft;
  dskPanel.Buttons[btnIndex].OnClick:= @btnVirtualDriveClick;
  FreeAndNil(bmpBitmap);
end;

procedure TfrmMain.AddSpecialButtons(dskPanel: TKASToolBar);
var
  btnIndex : Integer;
begin
  (*root button*)
  btnIndex := dskPanel.AddButtonX(btnLeftRoot.Caption, btnLeftRoot.Caption, '', '', btnLeftRoot.Hint, '', '');
  dskPanel.Buttons[btnIndex].GroupIndex := 0;
  (*up button*)
  btnIndex := dskPanel.AddButtonX(btnLeftUp.Caption, btnLeftUp.Caption, '', '', btnLeftUp.Hint, '', '');
  dskPanel.Buttons[btnIndex].GroupIndex := 0;
  (*home button*)
  btnIndex := dskPanel.AddButtonX(btnLeftHome.Caption, btnLeftHome.Caption, '', '', btnLeftHome.Hint, '', '');
  dskPanel.Buttons[btnIndex].GroupIndex := 0;
end;

procedure TfrmMain.CreateDiskPanel(dskPanel: TKASToolBar);
var
  I, Count : Integer;
  Drive : PDrive;
  BitmapTmp: Graphics.TBitmap;
begin
  dskPanel.Clear;
  dskPanel.Flat := gDriveBarFlat;
  Count := DrivesList.Count - 1;

  for I := 0 to Count do
  begin
    Drive := DrivesList.Items[I];

    // get drive icon
    BitmapTmp := PixMapManager.GetDriveIcon(Drive, dskPanel.GlyphSize, clBtnFace);

    dskPanel.AddButtonX(Drive^.DisplayName, Drive^.Path, '', '', GetDriveLabelOrStatus(Drive), '', BitmapTmp);

    if Assigned(BitmapTmp) then
      FreeAndNil(BitmapTmp);
    {Set Buttons Transparent. Is need? }
    dskPanel.Buttons[I].Glyph.Transparent := True;
    dskPanel.Buttons[I].Transparent := True;
    {/Set Buttons Transparent}
    dskPanel.Buttons[I].Layout := blGlyphLeft;
  end; // for

  // Add virtual drive button
  AddVirtualDriveButton(dskPanel);

  // Add special buttons
  if not gDrivesListButton then
    AddSpecialButtons(dskPanel);
end;

function TfrmMain.CreateFileView(sType: String; Page: TFileViewPage; AConfig: TIniFileEx; ASectionName: String; ATabIndex: Integer): TFileView;
begin
  // This function should be changed to a separate TFileView factory.

  if sType = 'columns' then
    Result := TColumnsFileView.Create(Page, AConfig, ASectionName, ATabIndex)
  else
    raise Exception.Create('Invalid file view type');
end;

function TfrmMain.CreateFileView(sType: String; Page: TFileViewPage; AConfig: TXmlConfig; ANode: TXmlNode): TFileView;
begin
  // This function should be changed to a separate TFileView factory.

  if sType = 'columns' then
    Result := TColumnsFileView.Create(Page, AConfig, ANode)
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
    OnReload := @FileViewReload;
  end;
end;

function TfrmMain.RemovePage(ANoteBook: TFileViewNotebook; iPageIndex:Integer): LongInt;
begin
  Result:= -1;
  if (ANoteBook.PageCount > 1) and
     (iPageIndex >= 0) and
     (iPageIndex < ANoteBook.PageCount) then
  begin
    if ANoteBook.Page[iPageIndex].LockState <> tlsNormal then
      case msgYesNoCancel(Format(rsMsgCloseLockedTab, [ANoteBook.Page[iPageIndex].Caption])) of
        mmrNo:
          Exit(1);
        mmrCancel, mmrNone:
          Exit(2);
      end;

    ANoteBook.RemovePage(iPageIndex);
    Result:= 0;
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

procedure TfrmMain.LoadTabsIni(ANoteBook: TFileViewNotebook);
var
  I: Integer;
  sIndex,
  TabsSection: String;
  sCurrentDir,
  sPath, sCaption: String;
  iActiveTab: Integer;
  Page: TFileViewPage;
  AFileView: TFileView;
  aFileSource: IFileSource;
begin
  if ANoteBook = nbLeft then
    begin
      TabsSection:= 'lefttabs';
    end
  else
    begin
      TabsSection:= 'righttabs';
    end;

  I:= 0;
  sIndex:= '0';
  // create one tab in any way
  sCurrentDir:= mbGetCurrentDir; // default path
  sPath:= gIni.ReadString(TabsSection, sIndex + '_path', sCurrentDir);
  while True do
    begin
      if mbDirectoryExists(sPath) then
        begin
          sCaption:= gIni.ReadString(TabsSection, sIndex + '_caption', EmptyStr);
          if sCaption = EmptyStr then
            sCaption:= GetLastDir(sPath);
        end
      else
        begin // find exists directory
          repeat
            sPath:= GetParentDir(sPath);
            if sPath = EmptyStr then
              sPath:= sCurrentDir;
          until mbDirectoryExists(sPath);
          sCaption:= GetLastDir(sPath);
        end;

      if sCaption <> '' then
        if (tb_text_length_limit in gDirTabOptions) and (Length(sCaption) > gDirTabLimit) then
          sCaption := Copy(sCaption, 1, gDirTabLimit) + '...';

      Page := ANoteBook.AddPage(sCaption);

      aFileSource := TFileSystemFileSource.GetFileSource;

      AFileView := CreateFileView('columns', Page, gIni, TabsSection, StrToInt(sIndex));
      if not Assigned(AFileView) then
      begin
        ANoteBook.RemovePage(Page);
        continue;
      end;

      Page.LockState := TTabLockState(gIni.ReadInteger(TabsSection, sIndex + '_options', Integer(tlsNormal)));
      if Page.LockState = tlsPathResets then // if locked tab with directory change
        Page.LockPath := sPath;

      AFileView.AddFileSource(aFileSource, sPath);
      // Assign events after loading file source.
      AssignEvents(AFileView);

      Inc(I);
      // get page index in string representation
      sIndex:= IntToStr(I);
      // get path of next tab
      sPath:= gIni.ReadString(TabsSection, sIndex + '_path', '');
      // if not found then break
      if sPath = '' then Break;
    end;

  // read active tab index
  iActiveTab:= gIni.ReadInteger(TabsSection, 'activetab', 0);
  // set active tab
  if (iActiveTab >= 0) and (iActiveTab < ANoteBook.PageCount) then
    ANoteBook.PageIndex := iActiveTab;
end;

procedure TfrmMain.LoadTabsXml(ANoteBook: TFileViewNotebook);
var
  sPath, sCaption, sViewType: String;
  iActiveTab: Integer;
  Page: TFileViewPage;
  AFileView: TFileView;
  aFileSource: IFileSource;
  RootNode, TabNode, ViewNode: TXmlNode;
begin
  if ANoteBook = nbLeft then
    RootNode := gConfig.FindNode(gConfig.RootNode, 'Tabs/OpenedTabs/Left')
  else
    RootNode := gConfig.FindNode(gConfig.RootNode, 'Tabs/OpenedTabs/Right');

  if Assigned(RootNode) then
  begin
    TabNode := RootNode.FirstChild;
    while Assigned(TabNode) do
    begin
      if TabNode.CompareName('Tab') = 0 then
      begin
        Page := nil;
        AFileView := nil;

        ViewNode := gConfig.FindNode(TabNode, 'FileView', False);
        if Assigned(ViewNode) then
        begin
          // File view has its own configuration.
          if gConfig.TryGetAttr(ViewNode, 'Type', sViewType) then
          begin
            Page := ANoteBook.AddPage(EmptyStr);
            AFileView := CreateFileView(sViewType, Page, gConfig, ViewNode);
          end
          else
            DCDebug('File view type not specified in configuration: ' + gConfig.GetPathFromNode(ViewNode) + '.');
        end
        // Else try old configuration.
        else if gConfig.TryGetValue(TabNode, 'Path', sPath) then
        begin
          sPath := GetDeepestExistingPath(sPath);
          if sPath <> EmptyStr then
          begin
            Page := ANoteBook.AddPage(EmptyStr);
            AFileView := CreateFileView('columns', Page, gConfig, TabNode);
            AFileView.AddFileSource(TFileSystemFileSource.GetFileSource, sPath);
          end;
        end
        else
          DCDebug('Invalid entry in configuration: ' + gConfig.GetPathFromNode(TabNode) + '.');

        if Assigned(Page) then
        begin
          if (not Assigned(AFileView)) or (AFileView.FileSourcesCount = 0) then
          begin
            ANoteBook.RemovePage(Page);
          end
          else
          begin
            Page.LockState := TTabLockState(gConfig.GetValue(TabNode, 'Options', Integer(tlsNormal)));
            if Page.LockState = tlsPathResets then // if locked tab with directory change
              Page.LockPath := gConfig.GetValue(TabNode, 'LockPath', AFileView.CurrentPath);

            sCaption := gConfig.GetValue(TabNode, 'Caption', EmptyStr);
            if sCaption = EmptyStr then
              sCaption := GetLastDir(AFileView.CurrentPath);
            Page.UpdateCaption(sCaption);

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
    sPath := mbGetCurrentDir;
    Page := ANoteBook.AddPage(EmptyStr);
    Page.UpdateCaption(GetLastDir(sPath));
    aFileSource := TFileSystemFileSource.GetFileSource;
    AFileView := TColumnsFileView.Create(Page, aFileSource, sPath);
    AssignEvents(AFileView);
  end
  else if Assigned(RootNode) then
  begin
    // read active tab index
    iActiveTab := gConfig.GetValue(RootNode, 'ActiveTab', 0);
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

procedure TfrmMain.SaveTabsIni(ANoteBook: TFileViewNotebook);
var
  I, Count: Integer;
  sIndex,
  TabsSection: String;
  sPath : String;
  Page: TFileViewPage;
begin
  if ANoteBook = nbLeft then
    TabsSection := 'lefttabs'
  else
    TabsSection := 'righttabs';

  // delete tabs section
  gIni.EraseSection(TabsSection);
  // get page count
  Count:= ANoteBook.PageCount - 1;
  for I:= 0 to Count do
    begin
      Page := ANoteBook.Page[I];

      // get page index in string representation
      sIndex:= IntToStr(I);

      if Page.LockState = tlsPathResets then // if locked tab with directory change
        sPath := Page.LockPath
      else
        sPath := Page.FileView.CurrentPath;

      gIni.WriteString(TabsSection, sIndex + '_path', sPath);
      gIni.WriteString(TabsSection, sIndex + '_caption', Page.Caption);
      gIni.WriteInteger(TabsSection, sIndex + '_options', Integer(Page.LockState));

      Page.FileView.SaveConfiguration(TabsSection, I);
    end;

  gIni.WriteInteger(TabsSection, 'activetab', ANoteBook.PageIndex);
end;

procedure TfrmMain.SaveTabsXml(ANoteBook: TFileViewNotebook);
var
  I: Integer;
  TabsSection: String;
  Page: TFileViewPage;
  RootNode, TabNode, ViewNode: TXmlNode;
begin
  RootNode := gConfig.FindNode(gConfig.RootNode, 'Tabs/OpenedTabs', True);
  if ANoteBook = nbLeft then
    TabsSection := 'Left'
  else
    TabsSection := 'Right';
  RootNode := gConfig.FindNode(RootNode, TabsSection, True);

  gConfig.ClearNode(RootNode);
  gConfig.SetValue(RootNode, 'ActiveTab', ANoteBook.PageIndex);

  for I:= 0 to ANoteBook.PageCount - 1 do
    begin
      TabNode := gConfig.AddNode(RootNode, 'Tab');
      ViewNode := gConfig.AddNode(TabNode, 'FileView');

      Page := ANoteBook.Page[I];

      gConfig.AddValue(TabNode, 'Caption', Page.Caption);
      gConfig.AddValue(TabNode, 'Options', Integer(Page.LockState));
      if Page.LockState = tlsPathResets then // if locked tab with directory change
        gConfig.AddValue(TabNode, 'LockPath', Page.LockPath);

      Page.FileView.SaveConfiguration(gConfig, ViewNode);
    end;
end;

(* Execute internal or external command *)

function TfrmMain.ExecCmd(Cmd: String; Param: String=''; StartPath: String=''): Boolean;
begin
  // For Command only replace environment variables and tilde.
  Cmd   := ReplaceEnvVars(ReplaceTilde(Cmd));
  Param := PrepareParameter(Param, FrameLeft, FrameRight, ActiveFrame);

  if Actions.Execute(Cmd, Param) <> uActs.cf_Error then
    Result:= True
  else
  begin
    StartPath := PrepareParameter(StartPath, FrameLeft, FrameRight, ActiveFrame, [ppoNormalizePathDelims, ppoReplaceTilde]);

    // Only add a space after command if there are parameters.
    if Length(Param) > 0 then
      Param := ' ' + Param;
    if StartPath <> '' then
      mbSetCurrentDir(StartPath);
    Result:= ExecCmdFork(Format('"%s"%s', [Cmd, Param]));
  end;
end;

function TfrmMain.ExecuteToolButton(Sender: TObject; NumberOfButton: Integer): Boolean;
var
  Cmd, Param, Path: String;
begin
  if Sender is TKASToolBar then
  begin
    with Sender as TKASToolBar do
    begin
      Cmd:= GetButtonX(NumberOfButton, CmdX);
      Param:= GetButtonX(NumberOfButton, ParamX);
      Path:= GetButtonX(NumberOfButton, PathX);
    end;
  end
  else if Sender is TKASBarMenu then
  begin
    with Sender as TKASBarMenu do
    begin
      Cmd:= BarFile.GetButtonX(NumberOfButton, CmdX);
      Param:= BarFile.GetButtonX(NumberOfButton, ParamX);
      Path:= BarFile.GetButtonX(NumberOfButton, PathX);
    end;
  end
  else
    raise Exception.Create('Invalid Sender object');

  Result := ExecCmd(Cmd, Param, Path);
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
      end;
      if not Assigned(Cons) then
        begin
          Cons:= CreateConsoleThread;
          Cons.ColsCount:= 80;
          Cons.RowsCount:= cmdConsole.LineCount;
          Cons.CmdBox:= cmdConsole;
          Cons.Resume;
        end;
    end
  else
    begin
      if Assigned(cmdConsole) then
      begin
        cmdConsole.Hide;
        FreeAndNil(cmdConsole);
      end;
      if Assigned(Cons) then
        FreeAndNil(Cons);
    end;

  nbConsole.Visible:= gTermWindow;
  ConsoleSplitter.Visible:= gTermWindow;
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

    for I := 0 to NoteBook.PageCount - 1 do  //  change on all tabs
    begin
      if NoteBook.Page[I].LockState = tlsNormal then
        NoteBook.Page[I].UpdateCaption(GetLastDir(NoteBook.View[I].CurrentPath));
      {
      else
        NoteBook.Page[I].UpdateCaption(GetLastDir(NoteBook.Page[I].LockPath));
      }
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
  IniBarFile: TIniFileEx = nil;
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

    pnlDiskLeftInner.Visible := gHorizontalFilePanels and gDriveBar1 and gDriveBar2;
    pnlDiskRightInner.Visible := gHorizontalFilePanels and gDriveBar1 and gDriveBar2;
    pnlDskLeft.Visible := not gHorizontalFilePanels and gDriveBar1 and gDriveBar2;
    pnlDskRight.Visible := gDriveBar1 and (not gHorizontalFilePanels or not gDriveBar2);
    pnlDisk.Visible := pnlDskLeft.Visible or pnlDskRight.Visible;

    // Create disk panels after assigning parent.
    UpdateDiskCount; // Update list of showed drives

    UpdateDriveToolbarSelection(dskLeft, FrameLeft);
    UpdateDriveToolbarSelection(dskRight, FrameRight);
    UpdateDriveButtonSelection(btnLeftDrive, FrameLeft);
    UpdateDriveButtonSelection(btnRightDrive, FrameRight);
    (*/ Disk Panels *)

    FDrivesListPopup.UpdateView;

    (*Main menu*)
    Actions.DoShowMainMenu(gMainMenu);

    (*Tool Bar*)
    MainToolBar.Visible:= gButtonBar;
    if gButtonBar then
      begin
        MainToolBar.Flat:= gToolBarFlat;
        MainToolBar.GlyphSize:= gToolBarIconSize;
        MainToolBar.SetButtonSize(gToolBarButtonSize, gToolBarButtonSize);

        MainToolBar.ChangePath:= gpExePath;
        MainToolBar.EnvVar:= '%commander_path%';
        try
          IniBarFile:= TIniFileEx.Create(gpCfgDir + 'default.bar', fmOpenRead or fmShareDenyNone);
          MainToolBar.LoadFromIniFile(IniBarFile);
        finally
          FreeThenNil(IniBarFile);
        end;
      end;

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

    // Tabs
    UpdateNoteBook(nbLeft);
    UpdateNoteBook(nbRight);

    // Log window
    seLogWindow.Visible := gLogWindow;
    LogSplitter.Visible := gLogWindow;
    // Align log window
    seLogWindow.Top := 0;
    LogSplitter.Top := 0;
    FontOptionsToFont(gFonts[dcfLog], seLogWindow.Font);

    // Command line
    pnlCommand.Visible := gCmdLine;
    edtCommand.Tag := 0;
    // Align command line and terminal window
    pnlCommand.Top := -Height;
    ConsoleSplitter.Top:= -Height;
    ToggleConsole;

    // Function keys
    pnlKeys.Visible := gKeyButtons;
    if gKeyButtons then
    begin
      pnlKeys.Height := Canvas.TextHeight('Wg') + 4;
      pnlKeys.Top:= Height * 2;
      for I := 0 to pnlKeys.ControlCount - 1 do
        if pnlKeys.Controls[I] is TSpeedButton then
          (pnlKeys.Controls[I] as TSpeedButton).Flat := gInterfaceFlat;
    end;

    ShowTrayIcon(gAlwaysShowTrayIcon);

    UpdateFreeSpace(fpLeft);
    UpdateFreeSpace(fpRight);
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
  else if (edtCommand.DroppedDown) and (Key in [VK_RETURN, VK_SELECT, VK_ESCAPE]) then
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

      else
        edtCommand.Tag:= 1;
    end;

  CheckCommandLine(GetKeyShiftStateEx, Key);
end;

procedure TfrmMain.edtCommandExit(Sender: TObject);
begin
  edtCommand.Tag:= 0;

  // Hide command line if it was temporarily shown.
  if (not gCmdLine) and IsCommandLineVisible then
    pnlCommand.Hide;
end;

procedure TfrmMain.tbChangeDirClick(Sender: TObject);
var
  I: Integer;
  sDir: String;
begin
  I:= pmToolBar.Tag;
  if I < 0 then
    Exit;

  sDir := MainToolBar.GetButtonX(I, PathX);
  sDir := PrepareParameter(sDir, FrameLeft, FrameRight, ActiveFrame, [ppoNormalizePathDelims, ppoReplaceTilde]);
  Actions.cm_ChangeDir(sDir);
end;

procedure TfrmMain.tbCopyClick(Sender: TObject);
var
  I: Integer;
begin
  I:= pmToolBar.Tag;
  if I >= 0 then
  begin
    with MainToolBar do
    Clipboard.AsText:= 'DOUBLECMD#BAR#DATA' + LineEnding +
                       GetButtonX(I, CmdX) + LineEnding +
                       GetButtonX(I, ParamX) + LineEnding +
                       GetButtonX(I, ButtonX) + LineEnding +
                       GetButtonX(I, MenuX) + LineEnding +
                       GetButtonX(I, PathX) + LineEnding;
    if (Sender is TMenuItem) and ((Sender as TMenuItem).Name = 'tbCut') then
      MainToolBar.RemoveButton(I);
    SaveMainToolBar;
  end;
end;

procedure TfrmMain.tbEditClick(Sender: TObject);
var
  IniFile: TIniFileEx;
begin
  if ShowConfigToolbar(MainToolBar.BarFile.CurrentBar, pmToolBar.Tag) then
    begin
      // apply new parameters to main toolbar
      MainToolBar.GlyphSize:= gToolBarIconSize;
      MainToolBar.SetButtonSize(gToolBarButtonSize, gToolBarButtonSize);
      MainToolBar.Flat:= gToolBarFlat;
      try
        IniFile:= TIniFileEx.Create(MainToolBar.BarFile.CurrentBar, fmOpenRead);
        MainToolBar.LoadFromIniFile(IniFile);
      finally
        FreeThenNil(IniFile);
      end;
    end;
end;

procedure TfrmMain.OnUniqueInstanceMessage(Sender: TObject; Params: array of UTF8String; ParamCount: Integer);
var
  I: Integer;
begin
  WindowState:= lastWindowState;
  BringToFront;
  for I:= 0 to ParamCount - 1 do
    DCDebug(Params[I]);
end;

procedure TfrmMain.tbPasteClick(Sender: TObject);
var
  I: Integer;
  Data: TStringList;
begin
  try
    Data:= TStringList.Create;
    Data.Text:= Clipboard.AsText;
    if Data.Count < 6 then Exit;
    I:= pmToolBar.Tag;
    if I < 0 then I:= MainToolBar.ButtonCount;
    MainToolBar.InsertButtonX(I, EmptyStr, Data[1], Data[2], Data[5], Data[4], EmptyStr, Data[3]);
    SaveMainToolBar;
  finally
    Data.Free;
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
      or ((ShiftEx * KeyModifiersShortcutNoText = [ssCtrl, ssAlt]) and
          (gKeyTyping[ktmCtrlAlt] = ktaNone) and
          (gKeyTyping[ktmNone] = ktaCommandLine))
{$ENDIF}
      then
      begin
        UTF8Char := VirtualKeyToUTF8Char(Key, ShiftEx - ModifierKeys);
        if (UTF8Char <> '') and
           (not ((Length(UTF8Char) = 1) and (UTF8Char[1] in [#0..#31]))) then
        begin
          TypeInCommandLine(UTF8Char);
          Key := 0;
        end;
      end;
      Break;
    end;
  end;
end;

function TfrmMain.ExecuteCommandFromEdit(sCmd: String; bRunInTerm: Boolean): Boolean;
var
  iIndex: Integer;
  sDir: String;
  Operation: TFileSourceExecuteOperation = nil;
  aFile: TFile = nil;
begin
  Result:= True;

  if edtCommand.Items.IndexOf(sCmd) = -1 then
  begin
    edtCommand.Items.Insert(0,sCmd);
    // only cMaxStringItems(see uGlobs.pas) is stored
    if edtCommand.Items.Count>cMaxStringItems then
      edtCommand.Items.Delete(edtCommand.Items.Count-1);
  end;
  edtCommand.DroppedDown:= False;

  sCmd:= ReplaceEnvVars(sCmd);

  if (fspDirectAccess in ActiveFrame.FileSource.GetProperties) then
    begin
      iIndex:= Pos('cd ', sCmd);
      if (iIndex = 1) or (sCmd = 'cd') then
        begin
          if (iIndex <> 1) then
            sDir:= GetHomeDir
          else
            begin
              sDir:= Trim(RemoveQuotation(Copy(sCmd, iIndex + 3, Length(sCmd))));
              sDir:= IncludeTrailingBackslash(sDir);
              sDir:= ReplaceTilde(sDir);
            end;
          logWrite('Chdir to: ' + sDir);
          if not mbSetCurrentDir(sDir) then
            begin
              msgWarning(Format(rsMsgChDirFailed, [sDir]));
            end
          else
            begin
              sDir := mbGetCurrentDir;
              ActiveFrame.CurrentPath := sDir;
              DCDebug(sDir);
              if gTermWindow and Assigned(Cons) then
                Cons.Terminal.SetCurrentDir(sDir);
            end;
        end
      else
        begin
          if gTermWindow and Assigned(Cons) then
            Cons.Terminal.Write_pty(sCmd + #13)
          else if bRunInTerm then
            ExecCmdFork(sCmd, True, gRunInTerm)
          else
            ExecCmdFork(sCmd);
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

procedure TfrmMain.TypeInCommandLine(Str: String);
begin
  Actions.cm_FocusCmdLine();
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

procedure TfrmMain.LoadWindowState;
var
  ANode: TXmlNode;
begin
  (* Load window bounds and state *)
  if Assigned(gIni) then
  begin
    Left := gIni.ReadInteger('Configuration', 'Main.Left', 80);
    Top := gIni.ReadInteger('Configuration', 'Main.Top', 48);
    Width :=  gIni.ReadInteger('Configuration', 'Main.Width', 800);
    Height :=  gIni.ReadInteger('Configuration', 'Main.Height', 480);
    if gIni.ReadBool('Configuration', 'Maximized', True) then
      Self.WindowState := wsMaximized;
  end
  else
  begin
    ANode := gConfig.FindNode(gConfig.RootNode, 'MainWindow/Position');
    if Assigned(ANode) then
    begin
      Left := gConfig.GetValue(ANode, 'Left', 80);
      Top := gConfig.GetValue(ANode, 'Top', 48);
      Width := gConfig.GetValue(ANode, 'Width', 800);
      Height := gConfig.GetValue(ANode, 'Height', 480);
      if gConfig.GetValue(ANode, 'Maximized', True) then
        Self.WindowState := wsMaximized;
    end;
  end;

  (* Load all tabs *)
  if Assigned(gIni) then
  begin
    LoadTabsIni(nbLeft);
    LoadTabsIni(nbRight);
  end
  else
  begin
    LoadTabsXml(nbLeft);
    LoadTabsXml(nbRight);
  end;
end;

procedure TfrmMain.SaveWindowState;
var
  //x: Integer;
  ANode: TXmlNode;
begin
  (* Save  columns widths *)
  // Save columns settings for all colsets.
{
    for x:=0 to ColSet.GetColumnSet(ActiveColm).ColumnsCount - 1 do
      ColSet.GetColumnSet(ActiveColm).SetColumnWidth(x, dgPanel.ColWidths[x]);
    ColSet.GetColumnSet(ActiveColm).Save(gIni);
}

  (* Save all tabs *)
  SaveTabsXml(nbLeft);
  SaveTabsXml(nbRight);

  (* Save window bounds and state *)
  ANode := gConfig.FindNode(gConfig.RootNode, 'MainWindow/Position', True);
  // save window size only if it's not Maximized (for not break normal size)
  if (WindowState <> wsMaximized) then
  begin
    gConfig.SetValue(ANode, 'Left', Left);
    gConfig.SetValue(ANode, 'Top', Top);
    gConfig.SetValue(ANode, 'Width', Width);
    gConfig.SetValue(ANode, 'Height', Height);
  end;
  gConfig.SetValue(ANode, 'Maximized', (WindowState = wsMaximized));
end;

procedure TfrmMain.SaveMainToolBar;
var
  IniBarFile: TIniFileEx;
begin
  if MainToolBar.BarFile.CurrentBar <> EmptyStr then
  begin
    IniBarFile:= TIniFileEx.Create(MainToolBar.BarFile.CurrentBar);
    try
      MainToolBar.SaveToIniFile(IniBarFile);
    finally
      FreeThenNil(IniBarFile);
    end;
  end;
end;

function TfrmMain.IsCommandLineVisible: Boolean;
begin
  Result := (edtCommand.Visible and pnlCommand.Visible);
end;

function TfrmMain.FindMatchingDrive(Path: UTF8String): Integer;
var
  i : Integer;
  LongestPathLen: Integer = 0;
  DrivePath: UTF8String;
  DrivePathLen: PtrInt;
begin
  Result := -1;

  if Assigned(DrivesList) then
  begin
    Path := UTF8UpperCase(Path);

    for i := 0 to DrivesList.Count - 1 do
    begin
      DrivePath := UTF8UpperCase(DrivesList[i]^.Path);
      DrivePathLen := UTF8Length(DrivePath);
      if (DrivePathLen > LongestPathLen) and IsInPath(DrivePath, Path, True, True) then
      begin
        LongestPathLen := DrivePathLen;
        Result := i;
      end;
    end;
  end;
end;

procedure TfrmMain.UpdateDriveToolbarSelection(DriveToolbar: TKAStoolBar; FileView: TFileView);
var
  DriveIndex: Integer;
begin
  if DriveToolbar.IsVisible then
  begin
    DriveIndex := FindMatchingDrive(FileView.CurrentPath);
    if (DriveIndex >= 0) and (DriveIndex < DriveToolbar.ButtonCount) then
      DriveToolbar.Buttons[DriveIndex].Down := True
    else
      // Path not found in toolbar.
      DriveToolbar.UncheckAllButtons;
  end;
end;

procedure TfrmMain.UpdateDriveButtonSelection(DriveButton: TSpeedButton; FileView: TFileView);
var
  BitmapTmp: Graphics.TBitmap = nil;
  DriveIndex: Integer;
  Drive: PDrive;
begin
  if not gDrivesListButton then
    Exit;

  DriveIndex := FindMatchingDrive(FileView.CurrentPath);
  if DriveIndex >= 0 then
  begin
    Drive := DrivesList[DriveIndex];
    DriveButton.Caption := Drive^.DisplayName;
    DriveButton.Tag := DriveIndex;
    BitmapTmp := PixMapManager.GetDriveIcon(Drive, 22, DriveButton.Color);
  end
  else
  begin
    DriveButton.Caption := '';
    DriveButton.Tag := -1;

    if FileView.FileSource.IsClass(TArchiveFileSource) then
      BitmapTmp := PixMapManager.GetArchiveIcon(22, DriveButton.Color)
    else
      BitmapTmp := PixMapManager.GetDefaultDriveIcon(22, DriveButton.Color);
  end;

  DriveButton.Glyph := BitmapTmp;

  DriveButton.Width := DriveButton.Glyph.Width
                     + DriveButton.Canvas.TextWidth(DriveButton.Caption) + 16;

  if Assigned(BitmapTmp) then
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
      UpdateDriveToolbarSelection(dskLeft, FileView);
      UpdateDriveButtonSelection(btnLeftDrive, FileView);

      // If only one drive toolbar is displayed then also change it.
      if gDriveBar1 and not gDriveBar2 then
        // dskRight is the main toolbar.
        UpdateDriveToolbarSelection(dskRight, FileView);
    end
    // Change right drive toolbar for right drive button
    else if (ANoteBook = nbRight) then
    begin
      UpdateDriveToolbarSelection(dskRight, FileView);
      UpdateDriveButtonSelection(btnRightDrive, FileView);
    end;
  end;
end;

procedure TfrmMain.ShowDrivesList(APanel: TFilePanelSelect);
var
  p: TPoint;
  ADriveIndex: Integer;
begin
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
{$IF DEFINED(LCLGTK2) or DEFINED(LCLQT)}
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
{$IF DEFINED(LCLGTK2) or DEFINED(LCLQT)}
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
{$IFDEF LCLQT}
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
{$IF DEFINED(LCLGTK2) or DEFINED(LCLQT)}
   if Assigned(LastActiveWindow) then
   begin
{$IFDEF LCLQT}
     TQtWidget(LastActiveWindow.Handle).setVisible(true);
{$ENDIF}
{$IFDEF LCLGTK2}
     LastActiveWindow.Show;
{$ENDIF}
     LastActiveWindow := nil;
   end;
{$ENDIF}
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
begin
  if (fsoExecute in ActiveFrame.FileSource.GetOperationsTypes) then
  begin
    with lblCommandPath do
    begin
      Visible := True;
      AutoSize := False;
      if Length(ActiveFrame.CurrentPath) > PTLen then
        Caption := '[' + Copy(ActiveFrame.CurrentPath,
                              Length(ActiveFrame.CurrentPath) - PTLen,
                              PTLen) + ']$:'
      else
        Caption := '[' + ActiveFrame.CurrentPath + ']$:';

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
  if (fspDirectAccess in ActiveFrame.FileSource.GetProperties) then
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
      lblDriveInfo.Caption := Format(rsFreeMsgShort, [cnvFormatFileSize(FreeSize)]);
      lblDriveInfo.Hint := Format(rsFreeMsg, [cnvFormatFileSize(FreeSize), cnvFormatFileSize(TotalSize)]);
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
begin
  ANotebook.DestroyAllPages;
end;

procedure TfrmMain.DriveListDriveSelected(Sender: TObject; ADriveIndex: Integer;
  APanel: TFilePanelSelect);
begin
  SetPanelDrive(APanel, DrivesList.Items[ADriveIndex]);
end;

procedure TfrmMain.DriveListClose(Sender: TObject);
begin
  if Sender is TDrivesListPopup then
    SetActiveFrame(TDrivesListPopup(Sender).Panel);
end;

procedure TfrmMain.AllProgressOnUpdateTimer(Sender: TObject);
var
  Pct: string;
  i, AllProgressPoint: integer;
  Operation: TFileSourceOperation;
  visiblePanel: boolean;
begin
  // Hide progress bar if there are no operations
  if OperationsManager.OperationsCount = 0 then
    begin
      PanelAllProgress.Visible:=false;
      AllOpPct.Visible:= false;
      AllOpProgressInd.Visible:= false;
      AllOpStart.Visible:= false;
      AllOpPause.Visible:= false;
      AllOpCancel.Visible:= false;
    end
  else
    begin
      if gPanelOfOp = True then
        begin
          PanelAllProgress.Height := sboxOperations.Canvas.TextHeight('Pg') * 2 + 8;
          // Make panel visible if at least one operation has no form
          visiblePanel:= False;
           for i := 0 to  OperationsManager.OperationsCount - 1 do
           begin
             if OperationsManager.GetFormCreate(OperationsManager.GetHandleById(i)) = False then
               visiblePanel:= True;
           end;
           if visiblePanel = True then
             PanelAllProgress.Visible:= True
           else
             PanelAllProgress.Visible:= False;
           sboxOperations.Invalidate; // force redraw
        end;

      if gProgInMenuBar = true then
        begin
          AllProgressPoint:= Round(OperationsManager.AllProgressPoint*100);
          AllOpPct.Caption:=IntToStr(AllProgressPoint)+' %'; // Show in menu line
          Pct:='';
          for i:=0 to 25 do
          begin
            if i <=  AllProgressPoint/4 then
              Pct:=Pct+'|' else Pct:= Pct+'.';
          end;
          AllOpProgressInd.Caption:='['+Pct+']';
          AllOpPct.Visible:= true;
          AllOpProgressInd.Visible:= true;
          AllOpStart.Visible:= true;
          AllOpPause.Visible:= true;
          AllOpCancel.Visible:= true;
        end;
    end;
    if not(PanelAllProgress.Visible) then PressLMB:= PanelAllProgress.Visible;
end;

procedure TfrmMain.SetFileSystemPath(aFileView: TFileView; aPath: UTF8String);
var
  i: Integer;
begin
  // Search for filesystem file source in this view, and remove others.
  with aFileView do
  begin
    for i := FileSourcesCount - 1 downto 0 do
    begin
      if FileSources[i].IsClass(TFileSystemFileSource) then
      begin
        CurrentPath := aPath;
        Break;
      end
      else
        RemoveCurrentFileSource;
    end;

    if FileSourcesCount = 0 then
    begin
      // If not found, get a new filesystem file source.
      AddFileSource(TFileSystemFileSource.GetFileSource, aPath);
    end;
  end;
end;

procedure TfrmMain.SetPanelDrive(aPanel: TFilePanelSelect; Drive: PDrive);
var
  aFileView, OtherFileView: TFileView;
begin
  if IsAvailable(Drive, True) then
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

    // Copy path opened in the other panel if the file source and drive match
    // and that path is not already opened in this panel.
    if OtherFileView.FileSource.IsClass(TFileSystemFileSource) and
       (OtherFileView.FileSource.GetRootDir(OtherFileView.CurrentPath) = Drive^.Path) and
       (OtherFileView.CurrentPath <> aFileView.CurrentPath) then
    begin
      SetFileSystemPath(aFileView, OtherFileView.CurrentPath);
    end
    else
    begin
      SetFileSystemPath(aFileView, Drive^.Path);
    end;
  end
  else
  begin
    msgWarning(rsMsgDiskNotAvail);
  end;
end;

procedure TfrmMain.OnDriveWatcherEvent(EventType: TDriveWatcherEvent; const ADrive: PDrive);
begin
  UpdateDiskCount;
end;

procedure TfrmMain.AppActivate(Sender: TObject);
begin
  if Assigned(FrameLeft) then
    FrameLeft.ReloadIfNeeded;
  if Assigned(FrameRight) then
    FrameRight.ReloadIfNeeded;
end;

end.

