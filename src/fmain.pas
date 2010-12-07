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
  LResources,
  Graphics, Forms, Menus, Controls, StdCtrls, ExtCtrls, ActnList,
  Buttons, SysUtils, Classes, SynEdit, LCLType, ComCtrls,
  KASToolBar, KASBarMenu, KASBarFiles,
  uCmdBox, uFileSystemWatcher, uFilePanelSelect,
  uFileView, uColumnsFileView, uFileSource, uFileViewNotebook, uFile,
  uOperationsManager, uFileSourceOperation, uDrivesList, uTerminal, uClassesEx,
  uXmlConfig, uDrive, uDriveWatcher;

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
    actDriveContextMenu: TAction;
    actCopyNoAsk: TAction;
    actChangeDirToParent: TAction;
    actEditPath: TAction;
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
    miNetworkDisconnect: TMenuItem;
    miNetworkQuickConnect: TMenuItem;
    miNetworkConnect: TMenuItem;
    mnuNetwork: TMenuItem;
    pnlLeftSyncSize: TPanel;
    pnlRightSyncSize: TPanel;
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
    miLine10: TMenuItem;
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
    pnlDisk: TPanel;
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
    pnlSyncSize: TPanel;
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
    mnuFilesLink: TMenuItem;
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
    mnuMarkCmpDir: TMenuItem;
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
    actShowMenu: TAction;
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
    procedure btnLeftClick(Sender: TObject);
    procedure btnLeftDirectoryHotlistClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure btnRightDirectoryHotlistClick(Sender: TObject);
    procedure dskRightResize(Sender: TObject);
    procedure lblAllProgressPctClick(Sender: TObject);


    procedure miLogMenuClick(Sender: TObject);
    procedure miTrayIconExitClick(Sender: TObject);
    procedure miTrayIconRestoreClick(Sender: TObject);
    procedure mnuShowOperationsClick(Sender: TObject);
    procedure PanelButtonClick(Button: TSpeedButton; SourceFrame: TFileView;
                               PanelSelect: TFilePanelSelect);
    procedure DeleteClick(Sender: TObject);
    procedure dskToolBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dskToolButtonClick(Sender: TObject; NumberOfButton: Integer);
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
    procedure MainToolBarMouseUp(Sender: TOBject; Button: TMouseButton;
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
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure edtCommandKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pmToolBarPopup(Sender: TObject);

    procedure pnlLeftResize(Sender: TObject);
    procedure pnlLeftRightDblClick(Sender: TObject);
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

    function FileViewBeforeChangeDirectory(Sender: TFileView; const NewDir : String): Boolean;
    procedure FileViewAfterChangeDirectory(Sender: TFileView; const NewDir : String);
    procedure FileViewChangeFileSource(Sender: TFileView);
    procedure FileViewActivate(Sender: TFileView);
    procedure FileViewReload(Sender: TFileView);
    procedure edtCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtCommandEnter(Sender: TObject);
    procedure edtCommandExit(Sender: TObject);
    procedure ConsoleSplitterChangeBounds(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
    procedure FramePanelOnWatcherNotifyEvent(Sender: TObject; NotifyData: PtrInt);
    procedure OnUniqueInstanceMessage(Sender: TObject; Params: array of UTF8String; ParamCount: Integer);
    procedure tbPasteClick(Sender: TObject);
    procedure AllProgressOnUpdateTimer(Sender: TObject);
  private
    { Private declarations }
    PanelSelected: TFilePanelSelect;
    LeftFrameWatcher,
    RightFrameWatcher: TFileSystemWatcher;
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
    MainSplitterLeftMouseBtnDown:boolean;
    MainSplitterMouseDownX:integer;
    // lastWindowState
    lastWindowState:TWindowState;
    // frost_asm end
    PressLMB: boolean;
    widthOfItem, ItemEnd: integer;

    function ExecuteCommandFromEdit(sCmd: String; bRunInTerm: Boolean): Boolean;
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
    procedure SetPanelDrive(aPanel: TFilePanelSelect; aPath: UTF8String);
    procedure OnDriveWatcherEvent(EventType: TDriveWatcherEvent; const ADrive: PDrive);

  public
    Function ActiveFrame: TFileView;  // get Active frame
    Function NotActiveFrame: TFileView; // get NotActive frame :)
    function ActiveNotebook: TFileViewNotebook;
    function NotActiveNotebook: TFileViewNotebook;
    function FrameLeft: TFileView;
    function FrameRight: TFileView;
    procedure AppException(Sender: TObject; E: Exception);
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
    procedure CopyFiles(SourceFileSource, TargetFileSource: IFileSource;
                        var SourceFiles: TFiles; TargetPath: String;
                        bShowDialog: Boolean); overload;
    procedure MoveFiles(SourceFileSource, TargetFileSource: IFileSource;
                        var SourceFiles: TFiles; TargetPath: String;
                        bShowDialog: Boolean); overload;
    procedure CopyFiles(sDestPath: String; bShowDialog: Boolean); overload; //  this is for F5 and Shift+F5
    procedure MoveFiles(sDestPath: String; bShowDialog: Boolean); overload;
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
    function ExecCmd(Cmd:string; param:string='') : Boolean;
    function ExecCmdEx(Sender: TObject; NumberOfButton:Integer) : Boolean;
    procedure ToggleConsole;
    procedure ToggleFileSystemWatcher;
    procedure UpdateWindowView;
    procedure MinimizeWindow;
    procedure LoadWindowState;
    procedure SaveWindowState;
    procedure SaveShortCuts;
    procedure LoadShortCuts;
    procedure SaveMainToolBar;
    function  IsCommandLineVisible: Boolean;
    procedure UpdateDriveToolbarSelection(DriveToolbar: TKAStoolBar; FileView: TFileView);
    procedure UpdateDriveButtonSelection(DriveButton: TSpeedButton; FileView: TFileView);
    procedure UpdateSelectedDrive(ANoteBook: TFileViewNotebook);
    procedure ShowDrivesList(APanel: TFilePanelSelect);
    procedure EnableHotkeys(Enable: Boolean);
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

uses
  LCLIntf, uGlobs, uLng, fConfigToolBar, uMasks, fCopyMoveDlg, uQuickViewPanel,
  uShowMsg, fHotDir, uDCUtils, uLog, uGlobsPaths, LCLProc, uOSUtils, uOSForms, uPixMapManager,
  uDragDropEx, StrUtils, uKeyboard, uFileSystemFileSource, fViewOperations,
  uFileSourceOperationTypes, uFileSourceCopyOperation, uFileSourceMoveOperation,
  fFileOpDlg, uFileSystemCopyOperation, uFileSystemMoveOperation, uFileSourceProperty,
  uFileSourceExecuteOperation, uArchiveFileSource, uShellExecute, uActs,
  fSymLink, fHardLink, uExceptions, uUniqueInstance, Clipbrd
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
    Result.Options := [nboHidePageListPopup];

    Result.OnCloseTabClicked := @NotebookCloseTabClicked;
    Result.OnMouseDown := @nbPageMouseDown;
    Result.OnMouseUp := @nbPageMouseUp;
    Result.OnPageChanged := @nbPageChanged;
    {$IF DEFINED(LCLGTK2)}
    Result.OnDblClick := @pnlLeftRightDblClick;
    {$ENDIF}
  end;

var
  slCommandHistory: TStringListEx;
  I: Integer;
begin
  Application.OnException := @AppException;

  DrivesList := nil;
  LeftFrameWatcher:= nil;
  RightFrameWatcher:= nil;
  FDropParams := nil;
  cmdConsole:= nil;

  PanelSelected:=fpLeft;
  HiddenToTray := False;
  HidingTrayIcon := False;

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
  SetMyWndProc(Handle);

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

  LoadWindowState;

  // frost_asm begin
    lastWindowState:=WindowState;
  // frost_asm end

  actShowSysFiles.Checked:=uGlobs.gShowSystemFiles;

  AllowDropFiles := not uDragDropEx.IsExternalDraggingSupported;

  pmButtonMenu.BarFile.ChangePath := gpExePath;
  pmButtonMenu.BarFile.EnvVar := '%commander_path%';

  // Use the same tooltips for some left and right panel butttons.
  btnRightDirectoryHotlist.Hint := btnLeftDirectoryHotlist.Hint;
  btnRightHome.Hint := btnLeftHome.Hint;
  btnRightRoot.Hint := btnLeftRoot.Hint;
  btnRightUp.Hint := btnLeftUp.Hint;


  { *HotKeys* }
  LoadShortCuts;

  HotMan.RegisterHotkeyManager(Self);
  HotMan.RegisterHotkeyManager(edtCommand);

  if (HotMan.HotkeyList.Count = 0) or (CompareText(HotMan.Version, hkVersion) <> 0) then
    LoadDefaultHotkeyBindings;
  // load shortcuts to action list for showing it in menu
  HotMan.LoadShortCutToActionList(ActionLst);

  for i:=0 to actionLst.ActionCount -1 do
    // Have to cast TContainedAction to TAction here, which may be unsafe.
    Actions.AddAction(TAction(actionLst[i]));
  { *HotKeys* }

  UpdateWindowView;
  //DebugLn('frmMain.FormCreate Done');
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
  if not Assigned(frmViewOperations) then
    Application.CreateForm(TfrmViewOperations, frmViewOperations);

  frmViewOperations.ShowOnTop;
end;

procedure TfrmMain.AllOpStartClick(Sender: TObject);
begin
   OperationsManager.StartRunning;
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

procedure TfrmMain.lblAllProgressPctClick(Sender: TObject);
begin
     if not Assigned(frmViewOperations) then
    Application.CreateForm(TfrmViewOperations, frmViewOperations);

  frmViewOperations.ShowOnTop;
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

procedure TfrmMain.mnuShowOperationsClick(Sender: TObject);
begin
  if not Assigned(frmViewOperations) then
    Application.CreateForm(TfrmViewOperations, frmViewOperations);

  frmViewOperations.ShowOnTop;
end;

procedure TfrmMain.PanelButtonClick(Button: TSpeedButton; SourceFrame: TFileView;
                                    PanelSelect: TFilePanelSelect);
begin
  with SourceFrame do
  begin
    if Button.Caption = '/' then
      CurrentPath := FileSource.GetRootDir(CurrentPath)
    else if Button.Caption = '..' then
      CurrentPath := FileSource.GetParentDir(CurrentPath)
    else if Button.Caption = '~' then
      SetFileSystemPath(SourceFrame, GetHomeDir);
  end;

  SetActiveFrame(PanelSelect);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  slCommandHistory: TStringListEx;
begin
  DebugLn('Destroying main form');

  // Disable file watcher.
  if Assigned(LeftFrameWatcher) then
    FreeAndNil(LeftFrameWatcher);
  if Assigned(RightFrameWatcher) then
    FreeAndNil(RightFrameWatcher);
  DebugLn('File watchers finished');

  TDriveWatcher.RemoveObserver(@OnDriveWatcherEvent);
  TDriveWatcher.Finalize;
  DebugLn('Drive watcher finished');

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

  // Save main toolbar
  SaveMainToolBar;

  FreeAndNil(DrivesList);

  DebugLn('Main form destroyed');
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if OperationsManager.OperationsCount > 0 then
  begin
    msgWarning('There are operations running!');
    CanClose := False;
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

  if (edtCommand.Tag = 0) then
    begin
      // quick search by Letter only

      // Check for certain Ascii keys.
      if (Length(UTF8Key) = 1) and ((Ord(UTF8Key[1]) <= 32) or
         (UTF8Key[1] in ['+','-','*','/','\'])) then Exit;

      ModifierKeys := GetKeyShiftStateEx;

      if ((gQuickSearch and (gQuickSearchMode = [])) or
          (gQuickFilter and (gQuickFilterMode = []))) and
         // Check only ssCtrl and ssAlt.
         (ModifierKeys * [ssCtrl, ssAlt] = []) then
        begin
          // Let the panel receive this message.
        end
      else
        begin
          Actions.cm_FocusCmdLine();
          edtCommand.Text := edtCommand.Text + UTF8Key;
          edtCommand.SelStart := UTF8Length(edtCommand.Text) + 1;
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
  // чтоби не обробативалось  MainSplitterMouseUp
  MainSplitterLeftMouseBtnDown:=false;
  MainSplitter.ParentColor:=true;
  // 50 50
  Actions.cm_PanelsSplitterPerPos('50');
end;

procedure TfrmMain.MainSplitterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and (not MainSplitterLeftMouseBtnDown) then
  begin
   // под линуксом MainSplitter.Color:=clBlack не работает
   MainSplitter.ParentColor:=true;
   MainSplitter.Color:=ColorToRGB(clBlack);

   MainSplitterMouseDownX:=X;
   MainSplitterLeftMouseBtnDown:=true;
   // create hint
   if not Assigned(MainSplitterHintWnd) then
    MainSplitterHintWnd:= THintWindow.Create(nil);
   MainSplitterHintWnd.Color:= Application.HintColor;
  end;
end;

procedure TfrmMain.MainSplitterMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  APoint: TPoint;
  Rect: TRect;
  sHint: String;
begin
  if MainSplitterLeftMouseBtnDown and (MainSplitter.Left+X>3) and (MainSplitter.Left+X+3<pnlNotebooks.Width) then
  begin
   MainSplitter.Left:=MainSplitter.Left+X-MainSplitterMouseDownX;

   // hint
   if not Assigned(MainSplitterHintWnd) then  Exit;
   // calculate persent
   sHint:= FloatToStrF(MainSplitter.Left*100 / (pnlNotebooks.Width-MainSplitter.Width), ffFixed, 15, 1) + '%';
   //calculate hint position
   Rect:= MainSplitterHintWnd.CalcHintRect(1000, sHint, nil);
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

procedure TfrmMain.MainSplitterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (MainSplitterLeftMouseBtnDown) then
  begin
    // hide and destroy hint
    if Assigned(MainSplitterHintWnd) then
    begin
      MainSplitterHintWnd.Hide;
      FreeAndNil(MainSplitterHintWnd);
    end;

    MainSplitter.ParentColor:=true;
    MainSplitterLeftMouseBtnDown:=false;
    pnlLeft.Width:=MainSplitter.Left;
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
begin
  aFile := ActiveFrame.ActiveFile;
  if Assigned(aFile) and aFile.IsNameValid then
  begin
    MainToolBar.AddButtonX('', aFile.FullPath, '', aFile.Path,
                           ExtractOnlyFileName(aFile.Name), '',  aFile.FullPath);
    SaveMainToolBar;
  end;
end;

procedure TfrmMain.MainToolBarDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  aFile: TFile;
begin
  aFile := ActiveFrame.ActiveFile;
  if Assigned(aFile) and aFile.IsNameValid then
    Accept := True
  else
    Accept := False;
end;

function TfrmMain.MainToolBarLoadButtonGlyph(sIconFileName: String;
  iIconSize: Integer; clBackColor: TColor): TBitmap;
begin
  Result := PixMapManager.LoadBitmapEnhanced(sIconFileName, iIconSize, clBackColor);
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

procedure TfrmMain.dskToolBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  if (Button = mbRight) and (Sender is TSpeedButton) then
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

procedure TfrmMain.dskToolButtonClick(Sender: TObject; NumberOfButton: Integer);
var
  dskPanel : TKASToolBar;
  FileView : TFileView;
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
      SetPanelDrive(PanelSelected, dskPanel.Commands[NumberOfButton]);
    end;

  SetActiveFrame(PanelSelected);
end;

procedure TfrmMain.MainToolBarMouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
Point : TPoint;
begin
if (Button = mbRight) then
   begin
    Point := Mouse.CursorPos;
    if (Sender is TSpeedButton) then
        begin
          pmToolBar.Tag := (Sender as TSpeedButton).Tag;
          tbDelete.Enabled := true;
        end
    else
        begin
          pmToolBar.Tag := -1;
          tbDelete.Enabled := false;
        end;

    pmToolBar.PopUp(Point.X, Point.Y);
   end
else
   if (Button = mbMiddle) then
      begin
         tbEditClick(Sender)
      end;

end;

procedure TfrmMain.MainToolBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
begin
  ExecCmdEx(Sender, NumberOfButton);
  DebugLn(MainToolBar.Commands[NumberOfButton]);
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
    SaveShortCuts;
    SaveGlobs;
  except
  end;

 if assigned(Cons) then
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
  DebugLn('frmMain.frmMainShow');
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
      Page.FileView.CurrentPath := Page.LockPath
    else if (Notebook = nbLeft) and (FrameLeft <> nil) then
      begin
        if Assigned(LeftFrameWatcher) and (LeftFrameWatcher.WatchPath <> FrameLeft.CurrentPath) then
          LeftFrameWatcher.WatchPath:= FrameLeft.CurrentPath;
      end
    else if (Notebook = nbRight) and (FrameRight <> nil) then
      begin
        if Assigned(RightFrameWatcher) and (RightFrameWatcher.WatchPath <> FrameRight.CurrentPath) then
          RightFrameWatcher.WatchPath:= FrameRight.CurrentPath;
      end;

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
  Result := PixMapManager.LoadBitmapEnhanced(sIconFileName, iIconSize, clBackColor);
end;

procedure TfrmMain.pmButtonMenuMenuButtonClick(Sender: TObject;
  NumberOfButton: Integer);
begin
    ExecCmdEx(Sender, NumberOfButton);
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  pnlLeft.Width:= (frmMain.Width div 2) - (MainSplitter.Width div 2);

  //DebugLN('pnlDisk.Width == ' + IntToStr(pnlDisk.Width));

  { Synchronize width of left and right disk panels }

  pnlDisk.Width := pnlSyncSize.ClientWidth - (pnlSyncSize.ClientWidth mod 2);

  pnlLeftSyncSize.Width := (pnlDisk.ClientWidth div 2) - pnlDisk.BevelWidth;

  //DebugLN('dskLeft.Width == ' + IntToStr(dskLeft.Width));
  //DebugLN('dskRight.Width == ' + IntToStr(dskRight.Width));

  dskLeft.Repaint;
  dskRight.Repaint;
End;

procedure TfrmMain.dskRightResize(Sender: TObject);
begin
  dskLeft.Height:= dskRight.Height;
  pnlDisk.ClientHeight:= dskRight.Height;
  pnlSyncSize.ClientHeight:= dskRight.Height + pnlDisk.BevelWidth * 2;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // Either left or right panel has to be focused.
  if not FrameLeft.Focused and
     not FrameRight.Focused then
  begin
    Exit;
  end;

  if gCmdLine then  // If command line is enabled
  begin
    case Key of
      #32..#254:
      begin
        if (Key in ['-', '*', '+', ' ']) and (Trim(edtCommand.Text) = '') then
          Exit;

        if (edtCommand.Tag = 0) then
        begin
          edtCommand.SetFocus; // handle first char of command line specially
          edtCommand.Text := edtCommand.Text + Key;
          edtCommand.SelStart := UTF8Length(edtCommand.Text) + 1;
          Key := #0;
        end;
      end;
    end;
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
  if I > 0 then glsHotDir.Delete(I);
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
  if glsHotDir.IndexOfValue(ActiveFrame.CurrentPath) > 0 then
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

procedure TfrmMain.CopyFiles(SourceFileSource, TargetFileSource: IFileSource;
                             var SourceFiles: TFiles; TargetPath: String;
                             bShowDialog: Boolean);
var
  sDestination: String;
  sDstMaskTemp: String;
  Operation: TFileSourceCopyOperation = nil;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
  CopyDialog: TfrmCopyDlg = nil;
  OperationStartingState: TOperationStartingState = ossAutoStart;
begin
  try
    if not ((fsoCopyOut in SourceFileSource.GetOperationsTypes) and
            (fsoCopyIn  in TargetFileSource.GetOperationsTypes)) then
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
      CopyDialog := TfrmCopyDlg.Create(Application, cmdtCopy);

      with CopyDialog do
      begin
        edtDst.Text := sDestination;
        lblCopySrc.Caption := GetFileDlgStr(rsMsgCpSel, rsMsgCpFlDr, SourceFiles);

        if ShowModal = mrCancel then
          Exit;

        sDestination := edtDst.Text;
      end;

      OperationStartingState := CopyDialog.OperationStartingState;
    end;

    GetDestinationPathAndMask(TargetFileSource, sDestination,
                              SourceFiles.Path, TargetPath, sDstMaskTemp);

    // If same file source and address
    if (fsoCopy in SourceFileSource.GetOperationsTypes) and
       (fsoCopy in TargetFileSource.GetOperationsTypes) and
       (SourceFileSource = TargetFileSource) and
       SameText(SourceFileSource.GetCurrentAddress, TargetFileSource.GetCurrentAddress) then
       begin
         // Copy to between same file source
         Operation := SourceFileSource.CreateCopyOperation(
                        SourceFiles,
                        TargetPath) as TFileSourceCopyOperation;
       end
    else if TargetFileSource.IsClass(TFileSystemFileSource) then
    begin
      // CopyOut to filesystem.
      Operation := SourceFileSource.CreateCopyOutOperation(
                       TargetFileSource,
                       SourceFiles,
                       TargetPath) as TFileSourceCopyOperation;
    end
    else if SourceFileSource.IsClass(TFileSystemFileSource) then
    begin
      {if TargetFileSource is TArchiveFileSource then
      begin
        ShowPackDlg(SourceFileSource,
                    TargetFileSource as TArchiveFileSource,
                    SourceFiles,
                    TargetPath);
        Exit;
      end;}

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
      begin
        if Operation is TFileSystemCopyOperation then
          CopyDialog.SetOperationOptions(Operation as TFileSystemCopyOperation);
      end;

      // Start operation.
      OperationHandle := OperationsManager.AddOperation(Operation, OperationStartingState);

      ProgressDialog := TfrmFileOp.Create(OperationHandle);
      ProgressDialog.Show;
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

procedure TfrmMain.MoveFiles(SourceFileSource, TargetFileSource: IFileSource;
                             var SourceFiles: TFiles; TargetPath: String;
                             bShowDialog: Boolean);
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
      MoveDialog := TfrmCopyDlg.Create(Application, cmdtMove);

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
        begin
          if Operation is TFileSystemMoveOperation then
            MoveDialog.SetOperationOptions(Operation as TFileSystemMoveOperation);
        end;

        // Start operation.
        OperationHandle := OperationsManager.AddOperation(Operation, OperationStartingState);

        ProgressDialog := TfrmFileOp.Create(OperationHandle);
        ProgressDialog.Show;
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

procedure TfrmMain.CopyFiles(sDestPath: String; bShowDialog: Boolean);
var
  SourceFiles: TFiles = nil;
begin
  SourceFiles := ActiveFrame.SelectedFiles;
  if Assigned(SourceFiles) then
  try
    CopyFiles(ActiveFrame.FileSource, NotActiveFrame.FileSource,
              SourceFiles, sDestPath, bShowDialog);
  finally
    if Assigned(SourceFiles) then
      FreeAndNil(SourceFiles);
  end;
end;

procedure TfrmMain.MoveFiles(sDestPath: String; bShowDialog: Boolean);
var
  SourceFiles: TFiles = nil;
begin
  SourceFiles := ActiveFrame.SelectedFiles;
  if Assigned(SourceFiles) then
  try
    MoveFiles(ActiveFrame.FileSource, NotActiveFrame.FileSource,
              SourceFiles, sDestPath, bShowDialog);
  finally
    if Assigned(SourceFiles) then
      FreeAndNil(SourceFiles);
  end;
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
  CmdText : UTF8String;
begin
  // Either left or right panel has to be focused.
  if not FrameLeft.Focused and
     not FrameRight.Focused then
  begin
    Exit;
  end;

  case Key of
    VK_BACK:
      if edtCommand.Text <> '' then
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
      if edtCommand.Text <> '' then
      begin
        edtCommand.Text := '';
        Key := 0;
      end;

    VK_RETURN, VK_SELECT:
      if IsCommandLineVisible and (edtCommand.Text <> '') and
         (Shift - [ssCaps, ssShift] = []) then
      begin
        // execute command line (in terminal with Shift)
        ExecuteCommandLine(Shift = [ssShift]);
        Key:=0;
      end;

    VK_TAB:
      begin
        // Select opposite panel.
        case PanelSelected of
          fpLeft: SetActiveFrame(fpRight);
          fpRight: SetActiveFrame(fpLeft);
        end;
        Key := 0;
      end;
  end;
end;

procedure TfrmMain.pmToolBarPopup(Sender: TObject);
var
  I: Integer;
  sText: String;
  bPaste: Boolean;
begin
  I:= pmToolBar.Tag;
  tbSeparator.Visible:= (I >= 0);
  tbCut.Visible:= (I >= 0);
  tbCopy.Visible:= (I >= 0);

  sText:= Clipboard.AsText;
  bPaste:= (Pos('DOUBLECMD#BAR#DATA', sText) = 1) or (Pos('TOTALCMD#BAR#DATA', sText) = 1);
  if bPaste then
    tbSeparator.Visible:= True;
  tbPaste.Visible:= bPaste;
end;

procedure TfrmMain.pnlLeftResize(Sender: TObject);
begin
  // ставим спліттер в нужную позицию при смене размера левой панели
  MainSplitter.Left:=pnlLeft.Width;
  MainSplitter.Height:=pnlLeft.Height;
  MainSplitter.top:=pnlLeft.top;
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
          if OperationsManager.GetFormCreate (OperationsManager.GetHandleById(IndexFocus)) = False then //проверяем наличие формы у указанной операции
            begin
              OperationDialog := TfrmFileOp.Create(OperationsManager.GetHandleById(IndexFocus)); // если нет то создаем
              OperationDialog.Show;
              OperationsManager.SetFormCreate (OperationsManager.GetHandleById(IndexFocus), True); // показываем что форма есть
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
                 + IntToStr(Operation.Progress) + ' %';

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

      sboxOperations.Canvas.Brush.Color := clRed; // если стоит по каким то причинам значит красная

      if (StartingState in [ossQueueIn, ossQueueFirst, ossQueueLast]) then     // если в очереди то желтая
        sboxOperations.Canvas.Brush.Color := clYellow;
      if Operation.State = fsosRunning then             //Если идет значит синяя
        sboxOperations.Canvas.Brush.Color := clHighlight;
      // Draw progress bar
      sboxOperations.Canvas.FillRect(
        3 + (widthOfItem * i),
        2 + textHeight,
        5 + (widthOfItem * i) + (widthOfItem - 10) * Operation.Progress div 100,
        10 + textHeight);
      end;
    end; // for
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

function TfrmMain.FileViewBeforeChangeDirectory(Sender: TFileView; const NewDir: String): Boolean;
var
  ANoteBook: TFileViewNotebook;
  Page, NewPage: TFileViewPage;
begin
  Result:= True;
  if Sender.NotebookPage is TFileViewPage then
  begin
    Page := Sender.NotebookPage as TFileViewPage;

    case Page.LockState of
      tlsPathLocked:
        Result := False;  // do not change directory in this tab

      tlsDirsInNewTab:
        begin
          Result := False;  // do not change directory in this tab

          ANoteBook := Page.Notebook;

          // Create same type
          NewPage := ANoteBook.AddPage;
          Page.FileView.Clone(NewPage);
          NewPage.FileView.CurrentPath := NewDir;
          NewPage.MakeActive;
        end;
    end;
  end;
end;

procedure TfrmMain.FileViewAfterChangeDirectory(Sender: TFileView; const NewDir: String);
var
  ANoteBook : TFileViewNotebook;
  Page: TFileViewPage;
  sCaption : String;
begin
  if Sender.NotebookPage is TFileViewPage then
    begin
      Page := Sender.NotebookPage as TFileViewPage;
      ANoteBook := Page.Notebook;
      if Page.LockState = tlsNormal then // if not locked tab
        begin
          sCaption := GetLastDir(NewDir);
          Page.UpdateCaption(sCaption);
        end;

      // update file system watcher directory
      if (Page.FileView.FileSource.IsClass(TFileSystemFileSource)) then
      begin
        if (ANoteBook = nbLeft) and Assigned(LeftFrameWatcher) then
          LeftFrameWatcher.WatchPath:= NewDir
        else if (ANoteBook = nbRight) and Assigned(RightFrameWatcher) then
            RightFrameWatcher.WatchPath:= NewDir;

        if glsDirHistory.IndexOf(NewDir) = -1 then
          glsDirHistory.Insert(0, NewDir);
      end;

      UpdateSelectedDrive(ANoteBook);
      UpdateFreeSpace(ANoteBook.Side);
      UpdatePrompt;

      {if (fspDirectAccess in Page.FileView.FileSource.GetProperties) then
        begin
          if gTermWindow and Assigned(Cons) then
            Cons.Terminal.SetCurrentDir(NewDir);
        end;}
    end;
end;

procedure TfrmMain.FileViewChangeFileSource(Sender: TFileView);
var
  ANoteBook : TFileViewNotebook;
  Page: TFileViewPage;
  sCaption : String;
begin
  if Sender.NotebookPage is TFileViewPage then
    begin
      Page := Sender.NotebookPage as TFileViewPage;
      ANoteBook := Page.Notebook;

      if Page.LockState = tlsNormal then // if not locked tab
        begin
          sCaption := GetLastDir(Page.FileView.CurrentPath);
          Page.UpdateCaption(sCaption);
        end;

      if Page.IsActive then
      begin
        ToggleFileSystemWatcher;

        UpdateSelectedDrive(ANoteBook);
        UpdateFreeSpace(ANoteBook.Side);
        UpdatePrompt;
      end;
    end;
end;

procedure TfrmMain.FileViewActivate(Sender: TFileView);
var
  Page: TFileViewPage;
begin
  if Sender.NotebookPage is TFileViewPage then
    begin
      Page := Sender.NotebookPage as TFileViewPage;
      PanelSelected := Page.Notebook.Side;
      UpdateSelectedDrive(Page.Notebook);
      UpdatePrompt;
      UpdateFreeSpace(Page.Notebook.Side);
    end;
end;

procedure TfrmMain.FileViewReload(Sender: TFileView);
var
  Page: TFileViewPage;
begin
  if Sender.NotebookPage is TFileViewPage then
    begin
      Page := Sender.NotebookPage as TFileViewPage;

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
  if gDriveBar2 and gDriveBar1 then
    CreateDiskPanel(dskLeft);
  if gDriveBar1 then
    CreateDiskPanel(dskRight);
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

    dskPanel.AddButtonX(Drive^.DisplayName, Drive^.Path, '', '', Drive^.DriveLabel, '', BitmapTmp);

    if Assigned(BitmapTmp) then
      FreeAndNil(BitmapTmp);
    {Set Buttons Transparent. Is need? }
    dskPanel.Buttons[I].Glyph.Transparent := True;
    dskPanel.Buttons[I].Transparent := True;
    {/Set Buttons Transparent}
    dskPanel.Buttons[I].Layout := blGlyphLeft;
  end; // for

  if not gDriveMenuButton then  {Add special buttons}
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
    OnBeforeChangeDirectory := @FileViewBeforeChangeDirectory;
    OnAfterChangeDirectory := @FileViewAfterChangeDirectory;
    OnChangeFileSource := @FileViewChangeFileSource;
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
            DebugLn('File view type not specified in configuration: ' + gConfig.GetPathFromNode(ViewNode) + '.');
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
          DebugLn('Invalid entry in configuration: ' + gConfig.GetPathFromNode(TabNode) + '.');

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

function TfrmMain.ExecCmd(Cmd: string; param:string=''): Boolean;
begin
  if Actions.Execute(Cmd, Param) <> uActs.cf_Error then
    Result:= True
  else
    Result:= ExecCmdFork(Format('"%s" %s', [Cmd, Param]));
end;

function TfrmMain.ExecCmdEx(Sender: TObject; NumberOfButton: Integer): Boolean;
var
  Cmd, Param, Path: String;
begin
  if Sender is TKASToolBar then
    with Sender as TKASToolBar do
    begin
      Cmd:= GetButtonX(NumberOfButton, CmdX);
      Param:= GetButtonX(NumberOfButton, ParamX);
      Path:= GetButtonX(NumberOfButton, PathX);
    end;
  if Sender is TKASBarMenu then
    with Sender as TKASBarMenu do
    begin
      Cmd:= BarFile.GetButtonX(NumberOfButton, CmdX);
      Param:= BarFile.GetButtonX(NumberOfButton, ParamX);
      Path:= BarFile.GetButtonX(NumberOfButton, PathX);
    end;
  Param:= ReplaceEnvVars(Param);
  if Actions.Execute(Cmd, Param) <> uActs.cf_Error then
    Result:= True
  else
    begin
      Cmd:= mbExpandFileName(Cmd);
      Path:= ReplaceEnvVars(Path);
      ReplaceExtCommand(Param, FrameLeft, FrameRight, ActiveFrame);
      ReplaceExtCommand(Path, FrameLeft, FrameRight, ActiveFrame);
      if Path <> '' then
        mbSetCurrentDir(Path);
      // Only add a space after command if there are parameters.
      if Length(Param) > 0 then
        Param := ' ' + Param;
      Result:= ExecCmdFork(Format('"%s"%s', [Cmd, Param]));
    end;
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

procedure TfrmMain.ToggleFileSystemWatcher;
var
  WatchFilter: TWatchFilter;
begin
  WatchFilter:= [];
  if (watch_file_name_change in gWatchDirs) then
    Include(WatchFilter, wfFileNameChange);
  if (watch_attributes_change in gWatchDirs) then
    Include(WatchFilter, wfAttributesChange);

  if (WatchFilter <> []) and (FrameLeft.FileSource.IsClass(TFileSystemFileSource)) then
    begin
      if not Assigned(LeftFrameWatcher) then
        begin
          LeftFrameWatcher:= TFileSystemWatcher.Create(nbLeft, FrameLeft.CurrentPath, WatchFilter);
          LeftFrameWatcher.OnWatcherNotifyEvent:= @FramePanelOnWatcherNotifyEvent;
          LeftFrameWatcher.Active:= True;
        end;
    end
  else
    begin
      if Assigned(LeftFrameWatcher) then
        FreeAndNil(LeftFrameWatcher);
    end;

  if (WatchFilter <> []) and (FrameRight.FileSource.IsClass(TFileSystemFileSource)) then
    begin
      if not Assigned(RightFrameWatcher) then
        begin
          RightFrameWatcher:= TFileSystemWatcher.Create(nbRight, FrameRight.CurrentPath, WatchFilter);
          RightFrameWatcher.OnWatcherNotifyEvent:= @FramePanelOnWatcherNotifyEvent;
          RightFrameWatcher.Active:= True;
        end;
    end
  else
    begin
      if Assigned(RightFrameWatcher) then
        FreeAndNil(RightFrameWatcher);
    end;
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
      if NoteBook.Page[I].LockState <> tlsPathResets then
        NoteBook.Page[I].UpdateCaption(GetLastDir(NoteBook.View[I].CurrentPath))
      else
        NoteBook.Page[I].UpdateCaption(GetLastDir(NoteBook.Page[I].LockPath));
      NoteBook.View[I].UpdateView;
    end;
  end;

var
  I: Integer;
  IniBarFile: TIniFileEx = nil;
begin
  (* Disk Panels *)
  UpdateDiskCount; // Update list of showed drives

  pnlLeftSyncSize.Visible := (gDriveBar1 and gDriveBar2);
  pnlRightSyncSize.Visible := gDriveBar1;

  UpdateDriveToolbarSelection(dskLeft, FrameLeft);
  UpdateDriveToolbarSelection(dskRight, FrameRight);
  UpdateDriveButtonSelection(btnLeftDrive, FrameLeft);
  UpdateDriveButtonSelection(btnRightDrive, FrameRight);

  pnlSyncSize.Visible := gDriveBar1;
  (*/ Disk Panels *)

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

  btnLeftDrive.Visible := gDriveMenuButton;
  btnLeftDrive.Flat := gInterfaceFlat;
  btnLeftRoot.Visible := gDriveMenuButton;
  btnLeftRoot.Flat := gInterfaceFlat;
  btnLeftUp.Visible := gDriveMenuButton;
  btnLeftUp.Flat := gInterfaceFlat;
  btnLeftHome.Visible := gDriveMenuButton;
  btnLeftHome.Flat := gInterfaceFlat;
  btnLeftDirectoryHotlist.Visible := gDriveMenuButton;
  btnLeftDirectoryHotlist.Flat := gInterfaceFlat;
  btnLeftEqualRight.Visible := gDriveMenuButton;
  btnLeftEqualRight.Flat:= gInterfaceFlat;
  lblLeftDriveInfo.Visible:= gDriveFreeSpace;
  pbxLeftDrive.Visible := gDriveInd;
  pnlLeftTools.Visible:= gDriveMenuButton or gDriveFreeSpace or gDriveInd;

  btnRightDrive.Visible := gDriveMenuButton;
  btnRightDrive.Flat := gInterfaceFlat;
  btnRightRoot.Visible := gDriveMenuButton;
  btnRightRoot.Flat := gInterfaceFlat;
  btnRightUp.Visible := gDriveMenuButton;
  btnRightUp.Flat := gInterfaceFlat;
  btnRightHome.Visible := gDriveMenuButton;;
  btnRightHome.Flat := gInterfaceFlat;
  btnRightDirectoryHotlist.Visible := gDriveMenuButton;
  btnRightDirectoryHotlist.Flat := gInterfaceFlat;
  btnRightEqualLeft.Visible := gDriveMenuButton;
  btnRightEqualLeft.Flat:= gInterfaceFlat;
  lblRightDriveInfo.Visible:= gDriveFreeSpace;
  pbxRightDrive.Visible := gDriveInd;
  pnlRightTools.Visible:= gDriveMenuButton or gDriveFreeSpace or gDriveInd;

  //Indicator of free spase position
  if gDriveMenuButton=false then
    begin
      lblRightDriveInfo.Align:=alTop;
      lblLeftDriveInfo.Align:=alTop;
    end
  else
    begin
      lblRightDriveInfo.Align:=alNone;
      lblLeftDriveInfo.Align:=alNone;
    end;

  // Tabs
  UpdateNoteBook(nbLeft);
  UpdateNoteBook(nbRight);

  // Function keys
  pnlKeys.Visible := gKeyButtons;
  if gKeyButtons then
  begin
    pnlKeys.Height := Canvas.TextHeight('Wg') + 4;
    for I := 0 to pnlKeys.ControlCount - 1 do
      if pnlKeys.Controls[I] is TSpeedButton then
        (pnlKeys.Controls[I] as TSpeedButton).Flat := gInterfaceFlat;
  end;

  // Command line
  pnlCommand.Visible := gCmdLine;
  edtCommand.Tag := 0;
  ToggleConsole;

  // Log windows
  seLogWindow.Visible := gLogWindow or (not miLogHide.Enabled);
  LogSplitter.Visible := gLogWindow or (not miLogHide.Enabled);
  seLogWindow.Font.Name := gEditorFontName;

  ToggleFileSystemWatcher;
  ShowTrayIcon(gAlwaysShowTrayIcon);

  UpdateFreeSpace(fpLeft);
  UpdateFreeSpace(fpRight);
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
end;

procedure TfrmMain.edtCommandEnter(Sender: TObject);
begin
  EnableHotkeys(False);

  // Which actions should be active in the command line.
  Actions.EnableAction('AddPathToCmdLine', True);
  Actions.EnableAction('AddFilenameToCmdLine', True);
  Actions.EnableAction('AddPathAndFilenameToCmdLine', True);
  Actions.EnableAction('ShowCmdLineHistory', True);
end;

procedure TfrmMain.edtCommandExit(Sender: TObject);
begin
  edtCommand.Tag:= 0;

  // Hide command line if it was temporarily shown.
  if (not gCmdLine) and IsCommandLineVisible then
    pnlCommand.Hide;
end;

procedure TfrmMain.ConsoleSplitterChangeBounds(Sender: TObject);
begin
  nbConsole.Height := nbConsole.Height +
    // How much splitter was moved upwards.
    (pnlCommand.Top - ConsoleSplitter.Top - ConsoleSplitter.Height);
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

procedure TfrmMain.FramePanelOnWatcherNotifyEvent(Sender: TObject; NotifyData: PtrInt);
var
  sDrive,
  sWatchDirsExclude: String;
  FileView: TFileView;
begin
  // if not active and refresh only in foreground then exit
  if (watch_only_foreground in gWatchDirs) and (not Application.Active) then Exit;
  if not (Sender is TFileViewNotebook) then Exit;

  FileView := (Sender as TFileViewNotebook).ActiveView;
  if not Assigned(FileView) then Exit;

  // if current path in exclude list then exit
  if (watch_exclude_dirs in gWatchDirs) and (gWatchDirsExclude <> '') then
    begin
      sWatchDirsExclude:= gWatchDirsExclude;
      repeat
        sDrive:= Copy2SymbDel(sWatchDirsExclude, ';');
        if IsInPath(UTF8UpperCase(sDrive), UTF8UpperCase(FileView.CurrentPath), True) then
          Exit;
      until sWatchDirsExclude = '';
    end;

  FileView.Reload;
end;

procedure TfrmMain.OnUniqueInstanceMessage(Sender: TObject; Params: array of UTF8String; ParamCount: Integer);
var
  I: Integer;
begin
  WindowState:= lastWindowState;
  BringToFront;
  for I:= 0 to ParamCount - 1 do
    DebugLn(Params[I]);
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

function TfrmMain.ExecuteCommandFromEdit(sCmd: String; bRunInTerm: Boolean): Boolean;
var
  iIndex: Integer;
  sDir: String;
  Operation: TFileSourceExecuteOperation = nil;
  aFile: TFile = nil;
begin
  Result:= True;
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
              if Pos('~' + PathDelim, sDir) = 1 then
                sDir:= StringReplace(sDir, '~' + PathDelim, GetHomeDir, []);
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
              DebugLn(sDir);
              if gTermWindow and Assigned(Cons) then
                Cons.Terminal.SetCurrentDir(sDir);
            end;
        end
      else
        begin
          if edtCommand.Items.IndexOf(sCmd)=-1 then
            edtCommand.Items.Insert(0,sCmd);

          if gTermWindow and Assigned(Cons) then
            Cons.Terminal.Write_pty(sCmd + #13)
          else if bRunInTerm then
            ExecCmdFork(sCmd, True, gRunInTerm)
          else
            ExecCmdFork(sCmd);

          edtCommand.DroppedDown:= False;
          // only cMaxStringItems(see uGlobs.pas) is stored
          if edtCommand.Items.Count>cMaxStringItems then
            edtCommand.Items.Delete(edtCommand.Items.Count-1);
        end;
    end
  else
    begin
      aFile:= ActiveFrame.ActiveFile;
      if Assigned(aFile) then
        try
          aFile:= aFile.Clone;
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

// Save ShortCuts to config file
procedure TfrmMain.SaveShortCuts;
begin
  HotMan.Save(gpCfgDir + 'shortcuts.ini');
end;

// Load ShortCuts from config file
procedure TfrmMain.LoadShortCuts;
begin
  // ToDo Black list HotKey which can't use
  HotMan.Load(gpCfgDir + 'shortcuts.ini');
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

procedure TfrmMain.UpdateDriveToolbarSelection(DriveToolbar: TKAStoolBar; FileView: TFileView);
var
  i : Integer;
  ToolButtonPath : String;
  Path: String;
begin
  if not gDriveBar1 and not gDriveBar2 then
    Exit;

  Path := FileView.CurrentPath;

  for i := 0 to DriveToolbar.ButtonCount - 1 do
  begin
    ToolButtonPath := DriveToolbar.Commands[i];
    if IsInPath(UTF8UpperCase(ToolButtonPath), UTF8UpperCase(Path), True) then
    begin
      DriveToolbar.Buttons[i].Down := True;
      Exit;
    end;
  end;

  // Path not found in toolbar.

  DriveToolbar.UncheckAllButtons;
end;

procedure TfrmMain.UpdateDriveButtonSelection(DriveButton: TSpeedButton; FileView: TFileView);
var
  i : Integer;
  BitmapTmp: Graphics.TBitmap = nil;
  Drive: PDrive;
  Path: String;
  Found: Boolean = False;
begin
  if not gDriveMenuButton then
    Exit;

  Path := FileView.CurrentPath;

  for i := 0 to FDrivesListPopup.DrivesCount - 1 do
  begin
    Drive := DrivesList.Items[i];

    if IsInPath(UTF8UpperCase(Drive^.Path), UTF8UpperCase(Path), True) then
    begin
      DriveButton.Caption := Drive^.DisplayName;
      DriveButton.Tag := i;

      BitmapTmp := PixMapManager.GetDriveIcon(Drive, 22, DriveButton.Color);
      Found := True;
      Break;
    end;
  end;

  // Path not found in menu.
  if not Found then
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
  // делал по другому
  // WindowState:=lastWindowState; но при wsNormal
  // окно становится видимим но свернутим
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

procedure TfrmMain.EnableHotkeys(Enable: Boolean);
begin
// KeyPreview should be always enabled.
// We're enabling/disabling on a per-action level.

  Actions.EnableAllActions(Enable);

  // Actions that should always be enabled.
  Actions.EnableAction('Exit', True);
  Actions.EnableAction('Options', True);
  Actions.EnableAction('FileAssoc', True);
  Actions.EnableAction('HelpIndex', True);
  Actions.EnableAction('Keyboard', True);
  Actions.EnableAction('VisitHomePage', True);
  Actions.EnableAction('About', True);
  Actions.EnableAction('LeftOpenDrives', True);
  Actions.EnableAction('RightOpenDrives', True);
  Actions.EnableAction('RunTerm', True);
  Actions.EnableAction('Minimize', True);
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
var
  i: Integer;
begin
  // First stop all work in threads.
  for i := 0 to ANotebook.PageCount - 1 do
    ANotebook.View[i].StopBackgroundWork;
  // Then remove file views.
  ANotebook.RemoveAllPages;
end;

procedure TfrmMain.DriveListDriveSelected(Sender: TObject; ADriveIndex: Integer;
  APanel: TFilePanelSelect);
begin
  SetPanelDrive(APanel, DrivesList.Items[ADriveIndex]^.Path);
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
  // Скрываем прогрессбар если нет операций в фоне
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
          for i := 0 to OperationsManager.OperationsCount - 1 do
          begin
            Operation := OperationsManager.GetOperationByIndex(i);
            if Assigned(Operation) then
              begin
                sboxOperations.Invalidate; // force redraw
              end;
          end;
          // Делаем все видимым  если у хотя бы одной операции нет формы
          visiblePanel:= False;
           for i := 0 to  OperationsManager.OperationsCount - 1 do
           begin
             if OperationsManager.GetFormCreate(OperationsManager.GetHandleById(i)) = False then
               visiblePanel:= True;
           end;
           if visiblePanel = True then
             PanelAllProgress.Visible:= True
           else
             PanelAllProgress.Visible:= False ;
        end;

      if gProgInMenuBar = true then
        begin
          AllProgressPoint:= OperationsManager.AllProgressPoint;
          AllOpPct.Caption:=IntToStr(AllProgressPoint)+' %'; // Показываем в строке меню
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
        RemoveLastFileSource;
    end;

    if FileSourcesCount = 0 then
    begin
      // If not found, get a new filesystem file source.
      AddFileSource(TFileSystemFileSource.GetFileSource, aPath);
    end;
  end;
end;

procedure TfrmMain.SetPanelDrive(aPanel: TFilePanelSelect; aPath: UTF8String);
var
  aFileView, OtherFileView: TFileView;
begin
  if IsAvailable(aPath) then
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

    // Copy path opened in the other panel if the file source and drive match.
    if OtherFileView.FileSource.IsClass(TFileSystemFileSource) and
       (OtherFileView.FileSource.GetRootDir(OtherFileView.CurrentPath) = aPath) then
    begin
      aPath := OtherFileView.CurrentPath;
    end;

    SetFileSystemPath(aFileView, aPath);
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

initialization
 {$I fmain.lrs}
end.
