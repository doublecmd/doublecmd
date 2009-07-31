{
   Double Commander
   -------------------------------------------------------------------------
   Licence  : GNU GPL v 2.0
   Copyright (C) 2006-2009 Alexander Koblov (Alexx2000@mail.ru)

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
  Graphics, Forms, Menus, Controls, Dialogs, ComCtrls, StdCtrls, ExtCtrls, ActnList,
  Buttons, SysUtils, Classes, Grids, KASToolBar, SynEdit, KASBarMenu, KASBarFiles,
  uColumns, uFileList, LCLType, uCmdBox, uFileSystemWatcher,
  uFileView, uColumnsFileView, uFilePanelSelect,
  uFileSource, uFileViewNotebook, uFile, uFileSourceOperation,
  uOperationsManager
  {$IF NOT DEFINED(DARWIN)}
  , uTerminal
  {$ENDIF}
  {$IFDEF LCLQT}
  , qt4
  {$ENDIF}
  ;

const
  cHistoryFile='cmdhistory.txt';

type

  TDropPopupMenu = class(TPopupMenu)
  private
    FDropParams: TDropParams;

  public
    constructor Create(AOwner: TComponent); override;
    procedure PopUp(DropParams: TDropParams); reintroduce;
  end;


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
    actSortByColumn: TAction;
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
    actToggleLockDcaTab: TAction;
    actToggleLockTab: TAction;
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
    actOpenVFSList: TAction;
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
    CmdBox1: TCmdBox;
    dskLeft: TKAStoolBar;
    dskRight: TKAStoolBar;
    edtCommand: TComboBox;
    lblCommandPath: TLabel;
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
    mnuToggleLockTab: TMenuItem;
    mnuToggleLockDcaTab: TMenuItem;
    mnuRemoveAllTabs: TMenuItem;
    mnuRemoveTab: TMenuItem;
    miLine15: TMenuItem;
    mnuOpenDirInNewTab: TMenuItem;
    mnuNewTab: TMenuItem;
    miRemoveAllTabs: TMenuItem;
    miToggleLockDcaTab: TMenuItem;
    miToggleLockTab: TMenuItem;
    miRemoveTab: TMenuItem;
    miLine14: TMenuItem;
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
    nbConsole: TNotebook;
    Page1: TPage;
    Panel1: TPanel;
    MainSplitter: TPanel;
    pmButtonMenu: TKASBarMenu;
    lblRightDriveInfo: TLabel;
    lblLeftDriveInfo: TLabel;
    MainToolBar: TKASToolBar;
    mnuOpenVFSList: TMenuItem;
    mnuExtractFiles: TMenuItem;
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
    pmDrivesMenu: TPopupMenu;
    LogSplitter: TSplitter;
    pmColumnsMenu: TPopupMenu;
    pmDropMenu: TDropPopupMenu;
    pmTabMenu: TPopupMenu;
    pmTrayIconMenu: TPopupMenu;
    pmLogMenu: TPopupMenu;
    seLogWindow: TSynEdit;
    btnLeftTargetEqualSource: TSpeedButton;
    btnRightTargetEqualSource: TSpeedButton;
    Splitter1: TSplitter;
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
    tmHAL: TTimer;

    procedure actExecute(Sender: TObject);
    procedure btnLeftClick(Sender: TObject);
    procedure btnLeftDirectoryHotlistClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure btnRightDirectoryHotlistClick(Sender: TObject);
    procedure dskRightResize(Sender: TObject);
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
    procedure frmMainKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    procedure pnlKeysResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure edtCommandKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);

    procedure pnlLeftResize(Sender: TObject);
    procedure pnlLeftRightDblClick(Sender: TObject);
    procedure seLogWindowSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);

    function FramepnlFileBeforeChangeDirectory(Sender: TCustomPage; const NewDir : String): Boolean;
    procedure FramepnlFileAfterChangeDirectory(Sender: TCustomPage; const NewDir : String);
    procedure edtCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtCommandEnter(Sender: TObject);
    procedure edtCommandExit(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
    procedure FramePanelOnWatcherNotifyEvent(Sender: TObject; NotifyData: PtrInt);
    procedure tmHALTimer(Sender: TObject);
  private
    { Private declarations }
    PanelSelected: TFilePanelSelect;
    LeftFrameWatcher,
    RightFrameWatcher: TFileSystemWatcher;
    DrivesList : TList;
    MainSplitterHintWnd: THintWindow;
    HiddenToTray: Boolean;
    nbLeft, nbRight: TFileViewNotebook;

    // frost_asm begin
    // mainsplitter
    MainSplitterLeftMouseBtnDown:boolean;
    MainSplitterMouseDownX:integer;
    // lastWindowState
    lastWindowState:TWindowState;
    // frost_asm end

    function ExecuteCommandFromEdit(sCmd: String; bRunInTerm: Boolean): Boolean;
    procedure AddSpecialButtons(dskPanel: TKASToolBar);
    procedure HideToTray;
    procedure RestoreFromTray;
    procedure ShowTrayIcon(bShow: Boolean);

    {en
       Retrieves current window state, with a workaround for QT minimized state.
    }
    function GetWindowState: TWindowState;

    procedure OperationFinishedEvent(Operation: TFileSourceOperation; Event: TOperationManagerEvent);

  public
    procedure HandleActionHotKeys(var Key: Word; Shift: TShiftState);

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
    procedure CreatePopUpHotDir;
    procedure CreatePopUpDirHistory;
    procedure miHotAddClick(Sender: TObject);
    procedure miHotDeleteClick(Sender: TObject);
    procedure miHotConfClick(Sender: TObject);
    procedure RunRenameThread(srcFileList: TFileList; sDestPath: String; sDestMask: String);
    procedure RunCopyThread(srcFileList: TFileList; sDestPath: String; sDestMask: String;
                            bDropReadOnlyFlag: Boolean);
//    procedure RenameFile(srcFileList: TFileList; dstFramePanel: TFileView; sDestPath: String);
//    procedure CopyFile(srcFileList: TFileList; dstFramePanel: TFileView; sDestPath: String);
    procedure RenameFile(sDestPath:String); // this is for F6 and Shift+F6
    procedure CopyFile(sDestPath:String); //  this is for F5 and Shift+F5
    procedure GetDestinationPathAndMask(EnteredPath: String; BaseDir: String;
                                        out DestPath, DestMask: String);
    procedure SetNotActFrmByActFrm;
    procedure SetActiveFrame(panel: TFilePanelSelect);
    procedure UpdateDiskCount;
    procedure CreateDrivesMenu;
    procedure DrivesMenuClick(Sender: TObject);
    procedure CreateDiskPanel(dskPanel : TKASToolBar);
    function CreateFileView(sType: String; FileSource: TFileSource; Page: TFileViewPage): TFileView;
    function RemovePage(ANoteBook: TFileViewNotebook; iPageIndex:Integer): LongInt;
    procedure LoadTabs(ANoteBook: TFileViewNotebook);
    procedure SaveTabs(ANoteBook: TFileViewNotebook);
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
    function  IsCommandLineVisible: Boolean;
    procedure UpdateDriveToolbarSelection(DriveToolbar: TKAStoolBar; Path: String);
    procedure UpdateDriveButtonMenuSelection(DriveButton: TSpeedButton; Path: String);
    procedure UpdateSelectedDrive(ANoteBook: TFileViewNotebook);
    procedure EnableHotkeys(Enable: Boolean);
    procedure ExecuteCommandLine(bRunInTerm: Boolean);
    procedure UpdatePrompt;
    procedure ReLoadTabs(ANoteBook: TFileViewNotebook);
  published
    // For now we allow to write here.
    // Should be changed via event 'Active-file-view-changed' or something like that.
    property SelectedPanel:TFilePanelSelect read PanelSelected write PanelSelected;
    property LeftTabs: TFileViewNotebook read nbLeft;
    property RightTabs: TFileViewNotebook read nbRight;
  end;

var
  frmMain: TfrmMain;
{$IF NOT DEFINED(DARWIN)}
  Cons: TConsoleThread = nil;
{$ENDIF}

implementation

uses
  Clipbrd, LCLIntf, uTypes, fAbout, uGlobs, uLng, fOptions, fconfigtoolbar, fFileAssoc,
  uCopyThread, uWCXModule, uVFSTypes, Masks, fMkDir, fCopyDlg, fCompareFiles,
  fMoveDlg, uMoveThread, uShowMsg, uClassesEx, fFindDlg, uSpaceThread, fHotDir,
  fSymLink, fHardLink, uDCUtils, uLog, fMultiRename, uGlobsPaths, fMsg, fPackDlg,
  fExtractDlg, fLinker, fSplitter, LCLProc, uOSUtils, uOSForms, uPixMapManager,
  fColumnsSetConf, uDragDropEx, StrUtils, uKeyboard, WSExtCtrls, uFileSorting,
  uFileSystemFileSource, fViewOperations,
  uFileSourceOperationTypes, uFileSourceCopyOperation, uFileSystemCopyOperation,
  fFileOpDlg
  {$IFDEF LCLQT}
    , qtwidgets, qtobjects
  {$ENDIF}
  ;

{$IF DEFINED(LCLGTK2) or DEFINED(LCLQT)}
var
  LastActiveWindow: TCustomForm = nil;
{$ENDIF}

procedure TfrmMain.FormCreate(Sender: TObject);

  function CreateNotebook(aParent: TWinControl; aSide: TFilePanelSelect): TFileViewNotebook;
  begin
    Result := TFileViewNotebook.Create(aParent, aSide);

    Result.OnCloseTabClicked := @NotebookCloseTabClicked;
    Result.OnMouseDown := @nbPageMouseDown;
    Result.OnMouseUp := @nbPageMouseUp;
    Result.OnPageChanged := @nbPageChanged;
  end;

var
  slCommandHistory: TStringListEx;
  i: Integer;
begin
  nbLeft := CreateNotebook(pnlLeft, fpLeft);
  nbRight := CreateNotebook(pnlRight, fpRight);

  HiddenToTray := False;

  inherited;
  // frost_asm begin
  MainSplitterLeftMouseBtnDown:=false;
  // frost_asm end
  SetMyWndProc(Handle);
  Application.OnException := @AppException;

  if mbFileExists(gpIniDir+cHistoryFile) then
    begin
      slCommandHistory:= TStringListEx.Create;
      slCommandHistory.LoadFromFile(gpIniDir+cHistoryFile);
      edtCommand.Items.Assign(slCommandHistory);
      slCommandHistory.Free;
    end;

  LoadWindowState;

  // frost_asm begin
    lastWindowState:=WindowState;
  // frost_asm end

  // Tabs
  nbLeft.Options:=[nboHidePageListPopup];
  nbRight.Options:=[nboHidePageListPopup];

  actShowSysFiles.Checked:=uGlobs.gShowSystemFiles;

  AllowDropFiles := not uDragDropEx.IsExternalDraggingSupported;

  PanelSelected:=fpLeft;

  pnlNotebooks.Width:=Width div 2;

  //DebugLN('dskLeft.Width == ' + IntToStr(dskLeft.Width));
  //DebugLN('dskRight.Width == ' + IntToStr(dskRight.Width));

  DrivesList := nil;

  LeftFrameWatcher:= nil;
  RightFrameWatcher:= nil;

  pmButtonMenu.BarFile.ChangePath := gpExePath;
  pmButtonMenu.BarFile.EnvVar := '%commander_path%';

  // Use the same tooltips for left and right panel butttons.
  btnRightDirectoryHotlist.Hint := btnLeftDirectoryHotlist.Hint;
  btnRightHome.Hint := btnLeftHome.Hint;
  btnRightRoot.Hint := btnLeftRoot.Hint;
  btnRightTargetEqualSource.Hint := btnLeftTargetEqualSource.Hint;
  btnRightUp.Hint := btnLeftUp.Hint;


  { *HotKeys* }
  LoadShortCuts;

  HotMan.RegisterHotkeyManager(Self);
  HotMan.RegisterHotkeyManager(edtCommand);

  if HotMan.HotkeyList.Count=0 then LoadDefaultHotkeyBindings;
  // load shortcuts to action list for showing it in menu
  HotMan.LoadShortCutToActionList(ActionLst);

  for i:=0 to actionLst.ActionCount -1 do
    // Have to cast TContainedAction to TAction here, which may be unsafe.
    Actions.AddAction(TAction(actionLst[i]));
  { *HotKeys* }

  UpdateWindowView;
  //DebugLn('frmMain.FormCreate Done');

  OperationsManager.AddEventsListener([omevOperationFinished], @OperationFinishedEvent);
end;

procedure TfrmMain.btnLeftClick(Sender: TObject);
begin
  PanelButtonClick(Sender as TSpeedButton, FrameLeft, fpLeft);
end;

procedure TfrmMain.actExecute(Sender: TObject);
var cmd:string;
begin
  cmd:=(Sender as TAction).Name;
  cmd:='cm_'+copy(cmd,4,length(cmd)-3);
  try
    // 14.05.2009 - fix delete to trash from context menu;
    If (cmd = 'cm_Delete') and gUseTrash and mbCheckTrash then
     Actions.Execute(cmd,'recycle')
    else
    Actions.Execute(cmd);
  except
    on e : Exception do
      msgError(e.Message);
  end;
end;


procedure TfrmMain.btnLeftDirectoryHotlistClick(Sender: TObject);
Var P:TPoint;
begin
  inherited;
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
  inherited;
  SetActiveFrame(fpRight);
  CreatePopUpHotDir;// TODO: i thing in future this must call on create or change
  p := Classes.Point(btnRightDirectoryHotlist.Left,btnRightDirectoryHotlist.Height);
  p := pnlRightTools.ClientToScreen(p);
  pmHotList.PopUp(P.x,P.y);
end;

procedure TfrmMain.dskRightResize(Sender: TObject);
begin
  pnlSyncSize.Height:= dskRight.Height + pnlDisk.BevelWidth * 2;
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
  with Button do
  begin
{
   // Should those buttons only be in columns view?
   // or on every view?
    if Caption = '/' then
      SourceFrame.cdRootLevel
    else if Caption = '..' then
      SourceFrame.cdUpLevel
    else if Caption = '~' then
      SourceFrame.CurrentPath := GetHomeDir;
}
  end;

  SetActiveFrame(PanelSelect);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  IniBarFile: TIniFileEx;
  slCommandHistory: TStringListEx;
begin
  DebugLn('frmMain.Destroy');

  OperationsManager.RemoveEventsListener([omevOperationFinished], @OperationFinishedEvent);

  //ColSet.Free;

  if gSaveCmdLineHistory then
    begin
      try
        slCommandHistory:= TStringListEx.Create;
        slCommandHistory.Assign(edtCommand.Items);
        slCommandHistory.SaveToFile(gpIniDir+cHistoryFile);
      finally
        FreeThenNil(slCommandHistory);
      end;
    end;  
  {*Tool Bar*}
  try
    IniBarFile:= TIniFileEx.Create(gpIniDir + 'default.bar');
    MainToolBar.SaveToIniFile(IniBarFile);
  finally
    FreeThenNil(IniBarFile);
  end;
  {*Tool Bar*}

  DestroyDrivesList(DrivesList);
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
// This should not be needed for now as drag&drop should work through uDragDropEx.
{var
  FileView: TFileView;
  I: Integer;
  FileList: TFileList;
  FileNamesList: TStringList;
  Point: TPoint;
  DropParams: TDropParams;}
begin
{  FrameFilePanel:= nil;
  // drop on left panel
  if FindLCLControl(Mouse.CursorPos) = FrameLeft.dgPanel then
    FrameFilePanel:= FrameLeft;
  // drop on right panel
  if FindLCLControl(Mouse.CursorPos) = FrameRight.dgPanel then
    FrameFilePanel:= FrameRight;
  if not Assigned(FrameFilePanel) then Exit;

  // fill file list by files

  FileList:= TFileList.Create;
  FileNamesList := TStringList.Create;
  try
    for I:= Low(FileNames) to High(FileNames) do
      FileNamesList.Add(FileNames[I]);

    FileList.LoadFromFileNames(FileNamesList);
  except
    FreeAndNil(FileList);
    FreeAndNil(FileNamesList);
    Exit;
  end;

  FreeAndNil(FileNamesList); // don't need it anymore

  GetCursorPos(Point);

  try
    DropParams := TDropParams.Create(
        FileList,
        GetDropEffectByKeyAndMouse(GetKeyShiftState, mbLeft),
        Point, False,
        nil, FrameFilePanel);
  except
    FreeAndNil(FileList);
    Exit;
  end;

  FrameFilePanel.dgPanel.DropFiles(DropParams);}
end;

procedure TfrmMain.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  ModifierKeys: TShiftState;
begin
debugln('formutf8keypress');
{  // Either left or right panel has to be focused.
  if not FrameLeft.Focused and
     not FrameRight.Focused then
  begin
    Exit;
  end;
}
  if (edtCommand.Tag = 0) then
    begin
      // quick search by Letter only

      // Check for certain Ascii keys.
      if (Length(UTF8Key) = 1) and ((Ord(UTF8Key[1]) <= 32) or
         (UTF8Key[1] in ['+','-','*','/','\'])) then Exit;

      ModifierKeys := GetKeyShiftStateEx;

      if gQuickSearch and (gQuickSearchMode = []) and
         // Check only ssCtrl and ssAlt.
         (ModifierKeys * [ssCtrl, ssAlt] = gQuickSearchMode) then
        begin
          // Let the panel receive this message.
        end
      else if gCmdLine then  // If command line is enabled
        begin
          edtCommand.SetFocus;
          edtCommand.Text := edtCommand.Text + UTF8Key;
          edtCommand.SelStart := UTF8Length(edtCommand.Text) + 1;
          UTF8Key := '';
        end;
    end
end;

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  if (GetWindowState = wsMinimized) then
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
  if GetWindowState = wsMinimized then
  begin
    RestoreFromTray;
  end
  else
  begin
    MinimizeWindow;
    HideToTray;
  end;
end;

procedure TfrmMain.lblDriveInfoDblClick(Sender: TObject);
begin
  if (Sender as TLabel).Name = 'lblRightDriveInfo' then
      SetActiveFrame(fpRight)
  else if (Sender as TLabel).Name = 'lblLeftDriveInfo' then
      SetActiveFrame(fpLeft);
  Actions.cm_DirHotList('');
//  actDirHotList.Execute;
end;

procedure TfrmMain.MainToolBarDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  sFileName: String;
  IniBarFile: TIniFileEx;
begin
{
  // Should probably use Sender or Source here.

  if ActiveFrame.IsActiveItemValid then
  with ActiveFrame, ActiveFrame.GetActiveItem^ do
    begin
      sFileName := ActiveDir + sName;
      MainToolBar.AddButtonX('', sFileName, '', sPath, ExtractOnlyFileName(sName), '',  sFileName);
      try
        IniBarFile:= TIniFileEx.Create(gpIniDir + 'default.bar');
        MainToolBar.SaveToIniFile(IniBarFile);
      finally
        FreeThenNil(IniBarFile);
      end;
    end;
}
end;

procedure TfrmMain.MainToolBarDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
{
  if ActiveFrame.IsActiveItemValid then
    Accept := True
  else
    Accept := False;
}
end;

function TfrmMain.MainToolBarLoadButtonGlyph(sIconFileName: String;
  iIconSize: Integer; clBackColor: TColor): TBitmap;
begin
  Result := LoadBitmapFromFile(sIconFileName, iIconSize, clBackColor);
end;

procedure TfrmMain.DeleteClick(Sender: TObject);
begin
  if pmToolBar.Tag >= 0 then
  begin
    if msgYesNo(Format(rsMsgDelSel, [MainToolBar.Buttons[pmToolBar.Tag].Hint])) then
    begin
       MainToolBar.RemoveButton (pmToolBar.Tag);
       MainToolBar.SaveToFile(gpIniDir + 'default.bar');
    end;
  end;
end;

procedure TfrmMain.dskToolBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  sPath: String;
begin
  if Button <> mbRight then Exit;
  if Sender is TSpeedButton then
    with Sender as TSpeedButton do
    begin
      if Tag < DrivesList.Count then
        begin
          sPath:= PDrive(DrivesList.Items[Tag])^.Path;
          Actions.cm_DriveContextMenu(sPath);
        end;
    end;
end;

procedure TfrmMain.dskToolButtonClick(Sender: TObject; NumberOfButton: Integer);
var
  dskPanel : TKASToolBar;
  FileView : TFileView;
begin
  dskPanel := (Sender as TKASToolBar);

  if (dskPanel.Align = alLeft) or (not gDriveBar2 and (PanelSelected = fpLeft))  then
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
     if IsAvailable(dskPanel.Commands[NumberOfButton]) then
       begin
         if PanelSelected = fpRight then
           begin
             if IncludeTrailingPathDelimiter(ExtractFileDrive(FrameLeft.CurrentPath)) = dskPanel.Commands[NumberOfButton] then
               FileView.CurrentPath := FrameLeft.CurrentPath
             else
               FileView.CurrentPath := dskPanel.Commands[NumberOfButton];
           end
         else
           begin
             if IncludeTrailingPathDelimiter(ExtractFileDrive(FrameRight.CurrentPath)) = dskPanel.Commands[NumberOfButton] then
               FileView.CurrentPath := FrameRight.CurrentPath
             else
               FileView.CurrentPath := dskPanel.Commands[NumberOfButton];
           end;
       end
     else
       begin
         msgOK(rsMsgDiskNotAvail);
       end;
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

  try
    SaveWindowState;
    SaveShortCuts;
    SaveGlobs;
  except
  end;

{$IF NOT DEFINED(DARWIN)}
 if assigned(Cons) then
  Cons.Free;
{$ENDIF}
  Application.Terminate;
end;

procedure TfrmMain.frmMainKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//
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
  {$IFDEF LINUX}
  tmHAL.Enabled := True;
  {$ENDIF}
end;

procedure TfrmMain.mnuDropClick(Sender: TObject);
var
  DropParamsRef: TDropParams;
begin
  if (Sender is TMenuItem) and Assigned(pmDropMenu.FDropParams) then
    begin
      // Make a copy of the reference to parameters and clear FDropParams,
      // so that they're not destroyed if pmDropMenuClose is called while we're processing.
      DropParamsRef := pmDropMenu.FDropParams;
      pmDropMenu.FDropParams := nil; // release ownership

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

constructor TDropPopupMenu.Create(AOwner: TComponent);
begin
  FDropParams := nil;
  inherited;
end;

procedure TDropPopupMenu.PopUp(DropParams: TDropParams);
begin
  // Disposing of the params is handled in pmDropMenuClose or mnuDropClick.
  if Assigned(DropParams) then
  begin
    FDropParams := DropParams;
    inherited PopUp(DropParams.ScreenDropPoint.X,
                    DropParams.ScreenDropPoint.Y);
  end;
end;

procedure TfrmMain.pmDropMenuClose(Sender: TObject);
begin
  // Free drop parameters given to drop menu.
  if Assigned(pmDropMenu.FDropParams) then
    FreeAndNil(pmDropMenu.FDropParams);
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

  if MenuItem.Action = actRemoveTab then

    Actions.DoRemoveTab(NoteBook, pmTabMenu.Tag)

  else if MenuItem.Action = actToggleLockTab then

    Actions.DoToggleLockTab(NoteBook.Page[pmTabMenu.Tag])

  else if MenuItem.Action = actToggleLockDcaTab then

    Actions.DoToggleLockDcaTab(NoteBook.Page[pmTabMenu.Tag])

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
    if Page.LockState = tlsResettingPath then // if locked with directory change
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
    end;
  end;
end;

procedure TfrmMain.nbPageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  PopUpPoint: TPoint;
  NoteBook: TFileViewNotebook;
  TabNr: Integer;
  Index: Integer;
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

          // Check lock option items.
          Index := pmTabMenu.Items.IndexOf(miToggleLockTab);
          pmTabMenu.Items.Items[Index].Checked := (NoteBook.Page[TabNr].LockState = tlsLockedPath);
          Index := pmTabMenu.Items.IndexOf(miToggleLockDcaTab);
          pmTabMenu.Items.Items[Index].Checked := (NoteBook.Page[TabNr].LockState = tlsResettingPath);

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
  Result := LoadBitmapFromFile(sIconFileName, iIconSize, clBackColor);
end;

procedure TfrmMain.pmButtonMenuMenuButtonClick(Sender: TObject;
  NumberOfButton: Integer);
begin
    ExecCmdEx(Sender, NumberOfButton);
end;

procedure TfrmMain.pnlKeysResize(Sender: TObject);
var
  iWidth:Integer;
begin
  iWidth:=pnlKeys.Width div 8;
  btnF3.Left:=(pnlKeys.Width mod 8) div 2;
  btnF3.Width:=iWidth;

  btnF4.Left:=btnF3.Left+btnF3.Width;
  btnF4.Width:=iWidth;

  btnF5.Left:=btnF4.Left+btnF4.Width;
  btnF5.Width:=iWidth;

  btnF6.Left:=btnF5.Left+btnF5.Width;
  btnF6.Width:=iWidth;

  btnF7.Left:=btnF6.Left+btnF6.Width;
  btnF7.Width:=iWidth;

  btnF8.Left:=btnF7.Left+btnF7.Width;
  btnF8.Width:=iWidth;

  btnF9.Left:=btnF8.Left+btnF8.Width;
  btnF9.Width:=iWidth;

  btnF10.Left:=btnF9.Left+btnF9.Width;
  btnF10.Width:=iWidth;

end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  pnlLeft.Width:= (frmMain.Width div 2) - (MainSplitter.Width div 2);

  //DebugLN('pnlDisk.Width == ' + IntToStr(pnlDisk.Width));

  { Synchronize width of left and right disk panels }

  pnlDisk.Width := pnlSyncSize.Width - (pnlSyncSize.Width mod 2);

  dskLeft.Width := (pnlDisk.Width div 2) - pnlDisk.BevelWidth;

  //DebugLN('dskLeft.Width == ' + IntToStr(dskLeft.Width));
  //DebugLN('dskRight.Width == ' + IntToStr(dskRight.Width));

  dskLeft.Repaint;
  dskRight.Repaint;
End;


procedure TfrmMain.HandleActionHotKeys(var Key: Word; Shift: TShiftState);
begin
  // ---- 30.04.2009 - переписал для удаления в корзину. ----
  If ((Key = VK_F8) or (Key = VK_DELETE)) and
     ((not IsCommandLineVisible) or ((not edtCommand.Focused) and (edtCommand.Tag = 0))) then
   begin
    if gUseTrash and mbCheckTrash then // 14.05.2009 - additional check for various linux distributives.
     begin
      if Shift=[ssShift] then // если шифт - удаляем напрямую
       Actions.cm_Delete('')
      else Actions.cm_Delete('recycle'); // без шифта удаляем в корзину
     end
    else Actions.cm_Delete('');  // если корзина отключена в конфигурации, или (для линукс) нет программы gvsf-trash, то удалять напрямую.
    Key := 0;
    Exit;
   end;
  // ---------------------------------------------------------

  if Shift=[] then
  begin
    case Key of
     VK_APPS:
       begin
         Actions.cm_ContextMenu('');
         //actContextMenu.Execute;
         Key := 0;
         Exit;
       end;
    end; // case
  end; // Shift=[]

  if (Key=VK_Return) or (Key=VK_SELECT) then
  begin
    with ActiveFrame do
    begin
      if (Shift=[])or (Shift=[ssCaps]) then // 21.05.2009 - не учитываем CapsLock при перемещении по панелям
      begin
        if (not IsCommandLineVisible) or (edtCommand.Text='') then
        begin
          // Delegated to ActiveFrame.
        end
        else
        begin
          ExecuteCommandLine(False);
          Key:=0;
          Exit;
        end;
      end; //Shift=[] + 21.05.2009 - не учитываем CapsLock при перемещении по панелям

      // execute command line in terminal (Shift+Enter)
      if Shift=[ssShift] then
      begin
        if (not IsCommandLineVisible) or (edtCommand.Text='') then
        begin
          // Delegated to ActiveFrame.
        end
        else
          begin
            ExecuteCommandLine(True);
            Key := 0;
            Exit;
          end;
      end;
    end;
  end;  // handle ENTER with some modifier
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
var
  CmdText : UTF8String;
begin
debugln('formkeypress');
  // Either left or right panel has to be focused.
{
  if not FrameLeft.dgPanel.Focused and
     not FrameRight.dgPanel.Focused then
  begin
    Exit;
  end;
}

  if gCmdLine then  // If command line is enabled
  begin
    if Key=#27 then
      edtCommand.Text := '';

    if ((ord(key)>31) and (ord(key)<255)) or (ord(Key) = VK_BACK) then
    begin
      if ((Key='-') or (Key='*') or (Key='+') or (Key=' '))and (Trim(edtCommand.Text)='') then Exit;
      if (edtCommand.Tag = 0) then
      begin
        edtCommand.SetFocus; // handle first char of command line specially
        CmdText := edtCommand.Text;
        if ord(Key) = VK_BACK then  // backspace
        begin
          UTF8Delete(CmdText, UTF8Length(CmdText), 1);
          edtCommand.Text := CmdText;
        end
        else
          edtCommand.Text := CmdText + Key;

        edtCommand.SelStart := UTF8Length(edtCommand.Text) + 1;
        Key:=#0;
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
  WriteLn(stdErr,'Exception:',E.Message);
  WriteLn(stdErr,'Func:',BackTraceStrFunc(get_caller_frame(get_frame)));
  Dump_Stack(StdErr, get_caller_frame(get_frame));
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
begin
  inherited;
  glsHotDir.Add(ActiveFrame.CurrentPath);
//  pmHotList.Items.Add();
// OnClick:=HotDirSelected;
end;

procedure TfrmMain.miHotDeleteClick(Sender: TObject);
var i : integer;
begin
  i:= glsHotDir.IndexOf(ActiveFrame.CurrentPath);
  if i > 0 then glsHotDir.Delete(i);
end;

procedure TfrmMain.miHotConfClick(Sender: TObject);
begin
  inherited;
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
  mi:TMenuItem;
  i:Integer;
begin
  pmDirHistory.Items.Clear;

  // store only first gDirHistoryCount of DirHistory
  for i:=glsDirHistory.Count-1 downto 0 do
    if i>gDirHistoryCount then
      glsDirHistory.Delete(i)
    else
      Break;

  for i:=0 to glsDirHistory.Count-1 do
  begin
    mi:=TMenuItem.Create(pmDirHistory);
    mi.Caption:=glsDirHistory.Strings[i];
    mi.OnClick:=@HotDirSelected;
    pmDirHistory.Items.Add(mi);
  end;

end;


procedure TfrmMain.CreatePopUpHotDir;
var
  mi:TMenuItem;
  i:Integer;
begin
  // Create All popup menu
  pmHotList.Items.Clear;
  for i:=0 to glsHotDir.Count-1 do
  begin
    mi:=TMenuItem.Create(pmHotList);
    mi.Caption:=glsHotDir.Strings[i];
    mi.OnClick:=@HotDirSelected;
    pmHotList.Items.Add(mi);
  end;
  // now add delimiter
  mi:=TMenuItem.Create(pmHotList);
  mi.Caption:='-';
  pmHotList.Items.Add(mi);
  // now add ADD or DELETE item

  mi:=TMenuItem.Create(pmHotList);
  if glsHotDir.IndexOf(ActiveFrame.CurrentPath)>0 then
  begin
    mi.Caption:=Format(rsMsgPopUpHotDelete,[ActiveFrame.CurrentPath]);
    mi.OnClick:=@miHotDeleteClick;
  end
  else
  begin
    mi.Caption:=Format(rsMsgPopUpHotAdd,[ActiveFrame.CurrentPath]);
    mi.OnClick:=@miHotAddClick;
  end;
  pmHotList.Items.Add(mi);

  // now add configure item
  mi:=TMenuItem.Create(pmHotList);
  mi.Caption:=rsMsgPopUpHotCnf;
  mi.OnClick:=@miHotConfClick;
  pmHotList.Items.Add(mi);
end;

procedure TfrmMain.HotDirSelected(Sender:TObject);
var
  sDummy:String;
begin
 // this handler is used by HotDir and DirHistory
 // must extract & from Caption
  sDummy:=(Sender As TMenuItem).Caption;
  SDummy:=StringReplace(sDummy,'&','',[rfReplaceAll]);
  ActiveFrame.CurrentPath := sDummy;
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

// Frees srcFileList.
procedure TfrmMain.RunRenameThread(srcFileList: TFileList; sDestPath: String; sDestMask: String);
var
  MT: TMoveThread = nil;
begin
  try
    MT:= TMoveThread.Create(srcFileList);
    MT.sDstPath:= sDestPath;
    MT.sDstMask:= sDestMask;
    MT.Resume; // srcFileList is freed when thread finishes
  except
    if MT <> nil then
      MT.Free // frees srcFileList
    else
      FreeAndNil(srcFileList);
  end;
end;

// Frees srcFileList.
procedure TfrmMain.RunCopyThread(srcFileList: TFileList;
                                 sDestPath: String; sDestMask: String;
                                 bDropReadOnlyFlag: Boolean);
var
  CT: TCopyThread = nil;
begin
  try
    CT:= TCopyThread.Create(srcFileList);
    CT.sDstPath:= sDestPath;
    CT.sDstMask:= sDestMask;
    CT.bDropReadOnlyFlag:= bDropReadOnlyFlag;
    CT.Resume; // srcFileList is freed when thread finishes
  except
    if CT <> nil then
      CT.Free // frees srcFileList
    else
      FreeAndNil(srcFileList);
  end;
end;

(* Used for drag&drop move from external application *)
// Frees srcFileList automatically.
{procedure TfrmMain.RenameFile(srcFileList: TFileList; dstFramePanel: TFileView; sDestPath: String);
var
  sDstMaskTemp: String;
  sCopyQuest: String;
begin
  if (srcFileList.Count=1) and not (FPS_ISDIR(srcFileList.GetItem(0)^.iMode) or srcFileList.GetItem(0)^.bLinkIsDir) then
    begin
      sCopyQuest:= Format(rsMsgRenSel, [srcFileList.GetItem(0)^.sName]);
      sDestPath:= sDestPath + ExtractFileName(srcFileList.GetItem(0)^.sName);
    end
  else
    begin
      sCopyQuest:= Format(rsMsgRenFlDr, [srcFileList.Count]);
      sDestPath:= sDestPath + '*.*';
    end;

  with TfrmMoveDlg.Create(Application) do
  begin
    try
      edtDst.Text:= sDestPath;
      lblMoveSrc.Caption:= sCopyQuest;
      if ShowModal = mrCancel then
      begin
         FreeAndNil(srcFileList);
         Exit; // throught finally
      end;

      GetDestinationPathAndMask(edtDst.Text, dstFramePanel.CurrentPath, sDestPath, sDstMaskTemp);

    finally
      Free;
    end;
  end;

  (* Move files *)

  RunRenameThread(srcFileList, sDestPath, sDstMaskTemp);

end;

(* Used for drag&drop copy from external application *)
// Frees srcFileList automatically.
procedure TfrmMain.CopyFile(srcFileList: TFileList; dstFramePanel: TFileView; sDestPath: String);
var
  sCopyQuest,
  sDstMaskTemp: String;
  blDropReadOnlyFlag: Boolean;
begin
  (* Copy files between archive and real file system *)

  if  dstFramePanel.pnlFile.PanelMode = pmArchive then
    begin
      if not IsBlocked then
        begin
          if  (VFS_CAPS_COPYIN in dstFramePanel.pnlFile.VFS.VFSmodule.VFSCaps) then
            begin
              DebugLn('+++ Pack files to archive +++');
              ShowPackDlg(dstFramePanel.pnlFile.VFS, srcFileList, sDestPath, False);
            end
          else
            msgWarning(rsMsgErrNotSupported);
        end;
      Exit;
    end;

  if (srcFileList.Count=1) and not (FPS_ISDIR(srcFileList.GetItem(0)^.iMode) or srcFileList.GetItem(0)^.bLinkIsDir) then
    begin
      sCopyQuest:= Format(rsMsgCpSel, [srcFileList.GetItem(0)^.sName]);
      sDestPath:= sDestPath + ExtractFileName(srcFileList.GetItem(0)^.sName);
    end
  else
    begin
      sCopyQuest:= Format(rsMsgCpFlDr, [srcFileList.Count]);
      sDestPath:= sDestPath + '*.*';
    end;

  with TfrmCopyDlg.Create(Application) do
  begin
    try
      edtDst.Text:=sDestPath;
      lblCopySrc.Caption := sCopyQuest;
      cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;
      cbDropReadOnlyFlag.Visible := (dstFramePanel.pnlFile.PanelMode = pmDirectory);
      if ShowModal = mrCancel then
      begin
        FreeAndNil(srcFileList);
        Exit; // throught finally
      end;

      GetDestinationPathAndMask(edtDst.Text, dstFramePanel.CurrentPath, sDestPath, sDstMaskTemp);

      blDropReadOnlyFlag := cbDropReadOnlyFlag.Checked;
    finally
      Free;
    end;
  end; //with

  (* Copy files between VFS and real file system *)

  if dstFramePanel.pnlFile.PanelMode = pmVFS then
    begin
      if  (VFS_CAPS_COPYIN in dstFramePanel.pnlFile.VFS.VFSmodule.VFSCaps) then
        begin
          DebugLn('+++ Copy files to VFS +++');
          dstFramePanel.pnlFile.VFS.VFSmodule.VFSCopyInEx(srcFileList, sDestPath, 0);
        end
      else
        msgOK(rsMsgErrNotSupported);
      Exit;
    end;

  (* Copy files between real file system *)

  RunCopyThread(srcFileList, sDestPath, sDstMaskTemp, blDropReadOnlyFlag);
end;
}
procedure TfrmMain.RenameFile(sDestPath:String);
var
  fl:TFileList;
  sDstMaskTemp:String;
begin
  // Exit if no valid files selected.
{
  if ActiveFrame.SelectFileIfNoSelected(ActiveFrame.GetActiveItem) = False then Exit;
}

{
  if (sDestPath = '') and (ActiveFrame.pnlFile.GetSelectedCount = 1) then
    with ActiveFrame do
    begin
      if IsActiveItemValid then
        ShowRenameFileEdit(ActiveDir + GetActiveItem^.sName);
      Exit;
    end;
}

  fl := TFileList.Create; // free at Thread end by thread
  try
{
    fl.CurrentDirectory := ActiveFrame.CurrentPath;
    CopyListSelectedExpandNames(ActiveFrame.pnlFile.FileList,fl,ActiveFrame.CurrentPath);
}

    with TfrmMoveDlg.Create(Application) do
    begin
      try
        if (fl.Count = 1) and
           (not (FPS_ISDIR(fl.GetItem(0)^.iMode) or fl.GetItem(0)^.bLinkIsDir))
        then
          edtDst.Text := sDestPath + ExtractFileName(fl.GetItem(0)^.sName)
        else
          edtDst.Text := sDestPath + '*.*';

        //lblMoveSrc.Caption := GetFileDlgStr(rsMsgRenSel, rsMsgRenFlDr);
        if ShowModal=mrCancel then
        begin
          FreeAndNil(fl); // Free now, because the thread won't be run.
          Exit; // throught finally
        end;

        GetDestinationPathAndMask(edtDst.Text, ActiveFrame.CurrentPath, sDestPath, sDstMaskTemp);

      finally
{
        with ActiveFrame do
          UnSelectFileIfSelected(GetActiveItem);
}
        Free;
      end;
    end;
  
    (* Move files *)

    RunRenameThread(fl, sDestPath, sDstMaskTemp);

  except
    FreeAndNil(fl);
  end;
end;

procedure TfrmMain.CopyFile(sDestPath:String);
var
  sDstMaskTemp: String;
  blDropReadOnlyFlag : Boolean;
  TargetFileSource: TFileSource = nil;
  SourceFiles: TFiles = nil;
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
begin
  if not ((fsoCopyOut in ActiveFrame.FileSource.GetOperationsTypes) and
          (fsoCopyIn in NotActiveFrame.FileSource.GetOperationsTypes)) then
  begin
    msgWarning(rsMsgErrNotSupported);
    Exit;
  end;

  SourceFiles := ActiveFrame.SelectedFiles; // free at Thread end by thread
  try
    if SourceFiles.Count = 0 then
    begin
      FreeAndNil(SourceFiles);
      Exit;
    end;

    with TfrmCopyDlg.Create(Application) do
    begin
      try
        if (SourceFiles.Count = 1) and
           (not (SourceFiles[0].IsDirectory or SourceFiles[0].IsLinkToDirectory))
        then
          edtDst.Text := sDestPath + ExtractFileName(SourceFiles[0].Name)
        else
          edtDst.Text := sDestPath + '*.*';

        lblCopySrc.Caption := GetFileDlgStr(rsMsgCpSel, rsMsgCpFlDr, SourceFiles);
        cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;
        //cbDropReadOnlyFlag.Visible := (NotActiveFrame.pnlFile.PanelMode = pmDirectory);
        if ShowModal = mrCancel then
        begin
          FreeAndNil(SourceFiles); // Free now, because the thread won't be run.
          Exit ; // throught finally
        end;

        GetDestinationPathAndMask(edtDst.Text, SourceFiles.Path, sDestPath, sDstMaskTemp);

        blDropReadOnlyFlag := cbDropReadOnlyFlag.Checked;

      finally
        Free;
      end;
    end; //with

    // For now at least one must be FileSystem.
    if not (ActiveFrame.FileSource is TFileSystemFileSource or
            NotActiveFrame.FileSource is TFileSystemFileSource) then Exit;

    TargetFileSource := NotActiveFrame.FileSource.Clone;
    try
      Operation := ActiveFrame.FileSource.CreateCopyOutOperation(
                       TargetFileSource,
                       SourceFiles,
                       sDestPath,
                       sDstMaskTemp);

      if Assigned(Operation) then
      begin
        // Start operation.
        OperationHandle := OperationsManager.AddOperation(Operation, ossAutoQueue);

        ProgressDialog := TfrmFileOp.Create(OperationHandle);
        ProgressDialog.Show;
      end
      else
        msgWarning(rsMsgNotImplemented);

    except
      if Assigned(TargetFileSource) then
        FreeAndNil(TargetFileSource);
      raise;
    end;

  except
    FreeAndNil(SourceFiles);
  end;

end;

procedure TfrmMain.GetDestinationPathAndMask(EnteredPath: String; BaseDir: String;
                                             out DestPath, DestMask: String);
var
  AbsolutePath: String;
begin
  if GetPathType(EnteredPath) = ptAbsolute then
    AbsolutePath := EnteredPath
  else
    AbsolutePath := BaseDir + EnteredPath;

  DoDirSeparators(AbsolutePath);  // normalize path delimiters
  AbsolutePath := ExpandAbsolutePath(AbsolutePath);

  if AbsolutePath[Length(AbsolutePath)] = PathDelim then
  begin
    // If the entered path ends with a path delimiter
    // treat it as a path to a not yet existing directory
    // which should be created.
    DestPath := AbsolutePath;
    DestMask := '*.*';
  end
  else if mbDirectoryExists(AbsolutePath) then
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
  end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ModifierKeys: TShiftState;
  UTF8Char: TUTF8Char;
begin
debugln('formkeydown');
{
  // Either left or right panel has to be focused.
  if not FrameLeft.Focused and
     not FrameRight.Focused then
  begin
    Exit;
  end;
}

  // used for quick search by Ctrl+Alt+Letter and Alt+Letter
  if gQuickSearch and (edtCommand.Tag = 0) then
  begin
    ModifierKeys := GetKeyShiftStateEx;

    if ((gQuickSearchMode <> []) and
        // Check only Ctrl and Alt as quicksearch keys.
       (ModifierKeys * [ssCtrl, ssAlt] = gQuickSearchMode))
{$IFDEF MSWINDOWS}
    // Entering international characters with Ctrl+Alt on Windows.
    or ((gQuickSearchMode = []) and
       (ModifierKeys * [ssCtrl, ssAlt] = [ssCtrl, ssAlt]) and
       (ModifierKeys - [ssCtrl, ssAlt, ssShift, ssCaps] = []))
{$ENDIF}
    then
    begin
      UTF8Char := VirtualKeyToUTF8Char(Key, ModifierKeys - gQuickSearchMode);
      if UTF8Char <> '' then
      begin
        // Delegated to ActiveFrame.
        Exit;
      end;
    end;
  end;

  if Key=VK_TAB then
  begin
    Key:=0;
    case PanelSelected of
      fpLeft: SetActiveFrame(fpRight);
      fpRight: SetActiveFrame(fpLeft);
    end;
    Exit;
  end;

  // CTRL+PgUp
  if (Shift=[ssCtrl]) and (Key=VK_PRIOR) then
  begin
   // Delegate to ActiveFrame (actually should be via hotkey and appropriate command).
{
    ActiveFrame.pnlFile.cdUpLevel;
    Key:=0;

    Exit;
}
  end;

  // CTRL+PgDown
  if (Shift=[ssCtrl]) and (Key=VK_NEXT) then
  begin
   // Delegate to ActiveFrame (actually should be via hotkey and appropriate command).

    with ActiveFrame do
    begin
{
      if IsActiveItemValid then
        begin
          if FPS_ISDIR(pnlFile.GetActiveItem^.iMode) or (pnlFile.GetActiveItem^.bLinkIsDir) then
            pnlFile.cdDownLevel(pnlFile.GetActiveItem)
          else
            Actions.cm_OpenArchive('');
            //actOpenArchive.Execute;
        end;
}
    end;
    Key:=0;
    Exit;
  end;

  HandleActionHotKeys(Key, Shift);
  if Key = 0 then Exit;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
//  ActiveFrame.SetFocus;
//  DebugLn('Activate');
end;

procedure TfrmMain.SetNotActFrmByActFrm;
var
  pfr:PFileRecItem;
begin
  with ActiveFrame do
  begin
{
    pfr:=pnlFile.GetActiveItem;
    if not assigned(pfr) then Exit;
    if FPS_ISDIR(pfr^.iMode) and (not (pfr^.sName = '..')) then
    begin
      NotActiveFrame.pnlFile.CurrentPath := ActiveDir + pfr^.sName;
    end
    else
    begin
      NotActiveFrame.pnlFile.CurrentPath := ActiveDir;
    end;
}
  end;
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

function TfrmMain.FramepnlFileBeforeChangeDirectory(Sender: TCustomPage; const NewDir: String): Boolean;
var
  ANoteBook: TFileViewNotebook;
  Page, NewPage: TFileViewPage;
  Panel: TFilePanelSelect;
begin
  Result:= True;
  if Sender is TFileViewPage then
  begin
    Page := Sender as TFileViewPage;

    if Page.LockState = tlsLockedPath then
      begin
        Result:= False;  // do not change directory in this tab

        ANoteBook := Page.Notebook;

        // Create same type
        NewPage := ANoteBook.AddPage;
        Page.FileView.Clone(NewPage);
        NewPage.FileView.CurrentPath := NewDir;
        NewPage.MakeActive;
      end;
  end;
end;

procedure TfrmMain.FramepnlFileAfterChangeDirectory(Sender: TCustomPage; const NewDir: String);
var
  ANoteBook : TFileViewNotebook;
  Page: TFileViewPage;
  sCaption : String;
begin
  if Sender is TFileViewPage then
    begin
      Page := Sender as TFileViewPage;
      ANoteBook := Page.Notebook;
      if Page.LockState = tlsNormal then // if not locked tab
        begin
          sCaption := GetLastDir(ExcludeTrailingPathDelimiter(NewDir));
          Page.UpdateCaption(sCaption);
        end;

      // update file system watcher directory
      if (ANoteBook = nbLeft) and Assigned(LeftFrameWatcher) then
        LeftFrameWatcher.WatchPath:= NewDir
      else if (ANoteBook = nbRight) and Assigned(RightFrameWatcher) then
        RightFrameWatcher.WatchPath:= NewDir;

      UpdateSelectedDrive(ANoteBook);
    end;
end;

procedure TfrmMain.SetActiveFrame(panel: TFilePanelSelect);
begin
  PanelSelected:=panel;
  ActiveFrame.SetFocus;
  {$IF NOT DEFINED(DARWIN)}
  if gTermWindow and Assigned(Cons) then
    Cons.Terminal.Write_pty(' cd "'+ActiveFrame.CurrentPath+'"'+#13+#10);
  {$ENDIF}
end;

procedure TfrmMain.UpdateDiskCount;
var
  I: Integer;
begin
  DestroyDrivesList(DrivesList);
  DrivesList := GetAllDrives;
  { Delete drives that in drives black list }
  for I:= DrivesList.Count - 1 downto 0 do
    begin
      if MatchesMaskList(PDrive(DrivesList.Items[I])^.Name, gDriveBlackList) then
      begin
        if Assigned(DrivesList.Items[i]) then
          Dispose(PDrive(DrivesList.Items[i]));
        DrivesList.Delete(I);
      end;
    end;  

  // create drives drop down menu
  CreateDrivesMenu;

  // create drives left/right panels
  if gDriveBar2 and gDriveBar1 then
    CreateDiskPanel(dskLeft);
  if gDriveBar1 then
    CreateDiskPanel(dskRight);
end;

procedure TfrmMain.CreateDrivesMenu;
var
  I, Count : Integer;
  Drive : PDrive;
  miTmp : TMenuItem;
  BitmapTmp: Graphics.TBitmap;
begin
  pmDrivesMenu.Items.Clear;
  Count := DrivesList.Count - 1;
  for I := 0 to Count do
    begin
      Drive := PDrive(DrivesList.Items[I]);
      with Drive^ do
      begin
        miTmp := TMenuItem.Create(pmDrivesMenu);
        miTmp.Tag := I;
        miTmp.Caption := Name;
        miTmp.ShortCut:= TextToShortCut(Name[1]);
        
        // get disk icon
        BitmapTmp := PixMapManager.GetDriveIcon(Drive, 16, clMenu);
        miTmp.Bitmap := BitmapTmp;
        if Assigned(BitmapTmp) then
          FreeAndNil(BitmapTmp);

        miTmp.RadioItem := True;
        miTmp.AutoCheck := True;
        miTmp.GroupIndex := 1;
        miTmp.OnClick := @DrivesMenuClick;
        pmDrivesMenu.Items.Add(miTmp);
      end;  // with
    end; // for
end;

procedure TfrmMain.DrivesMenuClick(Sender: TObject);
var
  Drive: PDrive;
begin
  with Sender as TMenuItem do
  begin
    Drive := PDrive(DrivesList.Items[(Sender as TMenuItem).Tag]);
    if IsAvailable(Drive^.Path) then
       begin
         case pmDrivesMenu.Tag of
         0:
           begin
             FrameLeft.CurrentPath := Drive^.Path;
             SetActiveFrame(fpLeft);
           end;
         1:
           begin
             FrameRight.CurrentPath := Drive^.Path;
             SetActiveFrame(fpRight);
           end;
         end;  // case
       end
     else
       begin
         msgOK(rsMsgDiskNotAvail);
       end;
  end;
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
  Drive := PDrive(DrivesList.Items[I]);
  with Drive^ do
    begin
      // get drive icon
      BitmapTmp := PixMapManager.GetDriveIcon(Drive, dskPanel.GlyphSize, clBtnFace);

      dskPanel.AddButtonX(Name, Path, '', '', Path, '', BitmapTmp);

      if Assigned(BitmapTmp) then
        FreeAndNil(BitmapTmp);
      {Set Buttons Transparent. Is need? }
      dskPanel.Buttons[I].Glyph.Transparent := True;
      dskPanel.Buttons[I].Transparent := True;
      {/Set Buttons Transparent}
      dskPanel.Buttons[I].Layout := blGlyphLeft;
    end; // with
  end; // for

  if not gDriveMenuButton then  {Add special buttons}
    AddSpecialButtons(dskPanel);
end;

function TfrmMain.CreateFileView(sType: String; FileSource: TFileSource;
                                 Page: TFileViewPage): TFileView;
var
  Panel: TFilePanelSelect;
begin
  // This function should be changed to a separate TFileView factory.

  if sType = 'columns' then
  begin
    Result := TColumnsFileView.Create(Page, FileSource);
  end
  else
  begin
    Result := nil;
    Exit;
  end;

  with Result do
  begin
    OnBeforeChangeDirectory := @FramepnlFileBeforeChangeDirectory;
    OnAfterChangeDirectory := @FramepnlFileAfterChangeDirectory;
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
  DebugLn('FSetCol='+inttostr(colset.Items.Count));

  for i:=0 to ANoteBook.PageCount-1 do
  begin
    with ANoteBook.View[I] do
    begin
{
      DebugLn('ActiveColmRET'+Inttostr(I)+'='+ActiveColm);
      if ColSet.Items.IndexOf(ActiveColm)=-1 then
        if ColSet.Items.Count>0 then
          ActiveColm:=ColSet.Items[0]
        else
          ActiveColm:='Default';

      Colset.GetColumnSet(ActiveColm).Load(gini,ActiveColm);
      SetColWidths;
      UpdateColumnsView;
}
    end;
  end;
end;

procedure TfrmMain.LoadTabs(ANoteBook: TFileViewNotebook);
var
  I, J: Integer;
  sIndex,
  TabsSection: String;
  sCurrentDir,
  sPath, sColumnSet,
  sCaption: String;
  iActiveTab: Integer;
  Page: TFileViewPage;
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
            sCaption:= GetLastDir(ExcludeTrailingPathDelimiter(sPath));
        end
      else
        begin // find exists directory
          repeat
            sPath:= GetParentDir(sPath);
            if sPath = EmptyStr then
              sPath:= sCurrentDir;
          until mbDirectoryExists(sPath);
          sCaption:= GetLastDir(ExcludeTrailingPathDelimiter(sPath));
        end;

      Page := ANoteBook.AddPage;

      if not Assigned(CreateFileView('columns', TFileSystemFileSource.Create(sPath), Page)) then
      begin
        ANoteBook.RemovePage(Page);
        continue;
      end;

      Page.LockState := TTabLockState(gIni.ReadInteger(TabsSection, sIndex + '_options', 0));
      if Page.LockState = tlsResettingPath then // if locked tab with directory change
        Page.LockPath := sPath;

      Page.UpdateCaption(sCaption);
      Page.FileView.LoadConfiguration(TabsSection, StrToInt(sIndex));

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

procedure TfrmMain.SaveTabs(ANoteBook: TFileViewNotebook);
var
  I, J, Count: Integer;
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

      if Page.LockState = tlsResettingPath then // if locked tab with directory change
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

(* Execute internal or external command *)

function TfrmMain.ExecCmd(Cmd: string; param:string=''): Boolean;
begin
  if Actions.Execute(Cmd, Param)>-1 then
    Result:= True
  else
    Result:= ExecCmdFork(Format('"%s" %s', [Cmd, Param]));
end;

function TfrmMain.ExecCmdEx(Sender: TObject; NumberOfButton: Integer): Boolean;
var
  Cmd, Param: String;
begin
  if Sender is TKASToolBar then
    with Sender as TKASToolBar do
    begin
      Cmd:= GetButtonX(NumberOfButton, CmdX);
      Param:= GetButtonX(NumberOfButton, ParamX);
    end;
  if Sender is TKASBarMenu then
    with Sender as TKASBarMenu do
    begin
      Cmd:= BarFile.GetButtonX(NumberOfButton, CmdX);
      Param:= BarFile.GetButtonX(NumberOfButton, ParamX);
    end;
  Param:= mbExpandFileName(Param);
  if Actions.Execute(Cmd, Param) > -1 then
    Result:= True
  else
    begin
      Cmd:= mbExpandFileName(Cmd);
      Result:= ExecCmdFork(Format('"%s" %s', [Cmd, Param]));
    end;
end;

procedure TfrmMain.ToggleConsole;
begin
{$IF NOT DEFINED(DARWIN)}
  if gTermWindow then
    begin
      if not Assigned(Cons) then
        begin
          Cons:= CreateConsoleThread;
          Cons.ColsCount:= 80;
          Cons.RowsCount:= CmdBox1.LineCount;
          Cons.CmdBox:= CmdBox1;
          Cons.Resume;
        end;
    end
  else
    begin
      if Assigned(Cons) then
        FreeAndNil(Cons);
    end;
  pnlCommand.AutoSize:= True;
  nbConsole.Visible:= gTermWindow;
  Splitter1.Visible:= gTermWindow;
  pnlCommand.AutoSize:= not gTermWindow;
{$ELSE} // temporarily while console not implemented under Mac OS X
  pnlCommand.AutoSize:= True;
  nbConsole.Visible:= False;
  Splitter1.Visible:= False;
{$ENDIF}
end;

procedure TfrmMain.ToggleFileSystemWatcher;
var
  WatchFilter: TWatchFilter;
begin
  if gWatchDirs <> [] then
    begin
      WatchFilter:= [];
      if (watch_file_name_change in gWatchDirs) then
        Include(WatchFilter, wfFileNameChange);
      if (watch_attributes_change in gWatchDirs) then
        Include(WatchFilter, wfAttributesChange);
      if not Assigned(LeftFrameWatcher) then
        begin
          LeftFrameWatcher:= TFileSystemWatcher.Create(nbLeft, FrameLeft.CurrentPath, WatchFilter);
          LeftFrameWatcher.OnWatcherNotifyEvent:= @FramePanelOnWatcherNotifyEvent;
          LeftFrameWatcher.Active:= True;
        end;
      if not Assigned(RightFrameWatcher) then
        begin
          RightFrameWatcher:= TFileSystemWatcher.Create(nbRight, FrameRight.CurrentPath, WatchFilter);
          RightFrameWatcher.OnWatcherNotifyEvent:= @FramePanelOnWatcherNotifyEvent;
          RightFrameWatcher.Active:= True;
        end;
    end
  else
    begin
      if Assigned(LeftFrameWatcher) then
        FreeAndNil(LeftFrameWatcher);
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

    NoteBook.MultilineTabs := tb_multiple_lines in gDirTabOptions;

    for I := 0 to NoteBook.PageCount - 1 do  //  change on all tabs
    begin
      NoteBook[I].UpdateView;
    end;
  end;

var
  I: Integer;
  IniBarFile: TIniFileEx;
  iDelta: Integer;
begin
  (* Disk Panels *)
  UpdateDiskCount; // Update list of showed drives

  dskLeft.Visible := (gDriveBar1 and gDriveBar2);
  dskRight.Visible := gDriveBar1;

  UpdateDriveToolbarSelection(dskLeft, FrameLeft.CurrentPath);
  UpdateDriveToolbarSelection(dskRight, FrameRight.CurrentPath);
  UpdateDriveButtonMenuSelection(btnLeftDrive, FrameLeft.CurrentPath);
  UpdateDriveButtonMenuSelection(btnRightDrive, FrameRight.CurrentPath);

  pnlSyncSize.Visible := gDriveBar1;
  (*/ Disk Panels *)

  (*Tool Bar*)
  MainToolBar.Visible:= gButtonBar;
  if gButtonBar then
    begin
      MainToolBar.Flat:= gToolBarFlat;
      if gToolBarSmallIcons then
        begin
          MainToolBar.GlyphSize:= gToolBarIconSize;
          iDelta:= 0;
        end
      else
        begin
          MainToolBar.GlyphSize:= gToolBarButtonSize;
          iDelta:= 4;
        end;
      MainToolBar.ButtonHeight:= gToolBarButtonSize + iDelta;
      MainToolBar.ButtonWidth:= gToolBarButtonSize + iDelta;
      MainToolBar.ChangePath:= gpExePath;
      MainToolBar.EnvVar:= '%commander_path%';
      try
        IniBarFile:= TIniFileEx.Create(gpIniDir + 'default.bar', fmOpenRead or fmShareDenyNone);
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
  btnLeftTargetEqualSource.Visible := gDriveMenuButton;
  btnLeftTargetEqualSource.Flat:= gInterfaceFlat;

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
  btnRightTargetEqualSource.Visible := gDriveMenuButton;
  btnRightTargetEqualSource.Flat:= gInterfaceFlat;

  // Tabs
  UpdateNoteBook(nbLeft);
  UpdateNoteBook(nbRight);

  for I := 0 to pnlKeys.ControlCount - 1 do  // function keys
    if pnlKeys.Controls[I] is TSpeedButton then
      (pnlKeys.Controls[I] as TSpeedButton).Flat := gInterfaceFlat;
        
  pnlCommand.Visible := gCmdLine;
  edtCommand.Tag := 0;
  pnlKeys.Visible := gKeyButtons;
  LogSplitter.Visible := gLogWindow or (not miLogHide.Enabled);
  seLogWindow.Visible := gLogWindow or (not miLogHide.Enabled);
  seLogWindow.Font.Name := gEditorFontName;
  ToggleConsole;
  ToggleFileSystemWatcher;
  ShowTrayIcon(gAlwaysShowTrayIcon);
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
          edtCommand.Text := '';
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

procedure TfrmMain.tbEditClick(Sender: TObject);
begin
  ShowConfigToolbar(pmToolBar.Tag);
end;

procedure TfrmMain.FramePanelOnWatcherNotifyEvent(Sender: TObject; NotifyData: PtrInt);
var
  sDrive,
  sWatchDirsExclude: String;
  FileView: TFileView;
begin
  // if not active and refresh only in foreground then exit
  if (not Focused) and (watch_only_foreground in gWatchDirs) then Exit;
  if not (Sender is TFileViewNotebook) then Exit;

  FileView := (Sender as TFileViewNotebook).ActiveView;
  if not Assigned(FileView) then Exit;

  // if current path in exclude list then exit
  if gWatchDirsExclude <> '' then
    begin
      sWatchDirsExclude:= gWatchDirsExclude;
      repeat
        sDrive:= Copy2SymbDel(sWatchDirsExclude, ';');
        if Pos(sDrive, FileView.CurrentPath) = 1 then Exit;
      until sWatchDirsExclude = '';
    end;
//  FileView.RefreshPanel((watch_total_number_files in gWatchDirs), (watch_free_disk_space in gWatchDirs));
end;

procedure TfrmMain.tmHALTimer(Sender: TObject);
begin
{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  if CheckHalMsg then
    UpdateDiskCount;
{$ENDIF}
end;

function TfrmMain.ExecuteCommandFromEdit(sCmd: String; bRunInTerm: Boolean): Boolean;
var
  iIndex:Integer;
  sDir:String;
begin
  Result:=True;
  iIndex:=pos('cd ',sCmd);
  if iIndex=1 then
  begin
    sDir:=Trim(Copy(sCmd, iIndex+3, length(sCmd)));
    sDir:=IncludeTrailingBackslash(sDir);
    if Pos('~' + PathDelim, sDir) = 1 then
      sDir:= StringReplace(sDir, '~' + PathDelim, GetHomeDir, []);
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
{$IF NOT DEFINED(DARWIN)}
      if gTermWindow and Assigned(Cons) then
        Cons.Terminal.Write_pty(' cd "'+sDir+'"'+#13#10);
{$ENDIF}
    end;
  end
  else
  begin
    if edtCommand.Items.IndexOf(sCmd)=-1 then
      edtCommand.Items.Insert(0,sCmd);

{$IF NOT DEFINED(DARWIN)}
    if gTermWindow and Assigned(Cons) then
      Cons.Terminal.Write_pty(sCmd+#13#10)
    else
{$ENDIF}
    if bRunInTerm then
      ExecCmdFork(sCmd, True, gRunInTerm)
    else
      ExecCmdFork(sCmd);

    edtCommand.DroppedDown:=False;
    // only cMaxStringItems(see uGlobs.pas) is stored
    if edtCommand.Items.Count>cMaxStringItems then
      edtCommand.Items.Delete(edtCommand.Items.Count-1);
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
begin
  (* Load window bounds and state*)
  Left := gIni.ReadInteger('Configuration', 'Main.Left', Left);
  Top := gIni.ReadInteger('Configuration', 'Main.Top', Top);
  Width :=  gIni.ReadInteger('Configuration', 'Main.Width', Width);
  Height :=  gIni.ReadInteger('Configuration', 'Main.Height', Height);
  if gIni.ReadBool('Configuration', 'maximized', True) then
    Self.WindowState := wsMaximized;

  (* Load all tabs *)
  LoadTabs(nbLeft);
  LoadTabs(nbRight);
end;

procedure TfrmMain.SaveWindowState;
var
  x: Integer;
begin
  try
    (* Save  columns widths *)
    // Save columns settings for all colsets.
  {
      for x:=0 to ColSet.GetColumnSet(ActiveColm).ColumnsCount - 1 do
        ColSet.GetColumnSet(ActiveColm).SetColumnWidth(x, dgPanel.ColWidths[x]);
      ColSet.GetColumnSet(ActiveColm).Save(gIni);
  }

    (* Save all tabs *)
    SaveTabs(nbLeft);
    SaveTabs(nbRight);
    (* Save window bounds and state*)
    gIni.WriteInteger('Configuration', 'Main.Left', Left);
    gIni.WriteInteger('Configuration', 'Main.Top', Top);
    gIni.WriteInteger('Configuration', 'Main.Width', Width);
    gIni.WriteInteger('Configuration', 'Main.Height', Height);
    gIni.WriteBool('Configuration', 'maximized', (WindowState = wsMaximized));
  except
  end;
end;

// Save ShortCuts to config file
procedure TfrmMain.SaveShortCuts;
begin
  HotMan.Save(gpIniDir + 'shortcuts.ini');
end;

// Load ShortCuts from config file
procedure TfrmMain.LoadShortCuts;
begin
  // ToDo Black list HotKey which can't use
  HotMan.Load(gpIniDir + 'shortcuts.ini');
end;

function TfrmMain.IsCommandLineVisible: Boolean;
begin
  Result := (edtCommand.Visible and pnlCommand.Visible);
end;

procedure TfrmMain.UpdateDriveToolbarSelection(DriveToolbar: TKAStoolBar; Path: String);
var
  i : Integer;
  ToolButtonPath : String;
begin
  for i := 0 to DriveToolbar.ButtonCount - 1 do
  begin
    ToolButtonPath := DriveToolbar.Buttons[i].Hint;
    if IsInPath(UTF8UpperCase(ToolButtonPath), UTF8UpperCase(Path), True) then
    begin
      DriveToolbar.Buttons[i].Down := True;
      Exit;
    end;
  end;

  // Path not found in toolbar.

  DriveToolbar.UncheckAllButtons;
end;

procedure TfrmMain.UpdateDriveButtonMenuSelection(DriveButton: TSpeedButton; Path: String);
var
  i : Integer;
  BitmapTmp: Graphics.TBitmap;
  Drive: PDrive;
begin
  for i := 0 to pmDrivesMenu.Items.Count - 1 do
  begin
    Drive := PDrive(DrivesList.Items[pmDrivesMenu.Items[i].Tag]);

    if IsInPath(UTF8UpperCase(Drive^.Path), UTF8UpperCase(Path), True) then
    begin
      if gDriveMenuButton then
      begin
        DriveButton.Caption := Drive^.Name;

        BitmapTmp := PixMapManager.GetDriveIcon(Drive,
                                                DriveButton.Height - 2,
                                                DriveButton.Color);
        DriveButton.Glyph := BitmapTmp;

        if Assigned(BitmapTmp) then
          FreeAndNil(BitmapTmp);

        DriveButton.Width := DriveButton.Glyph.Width
                           + DriveButton.Canvas.TextWidth(DriveButton.Caption) + 16;
      end;

      pmDrivesMenu.Items[i].Checked := True;

      Exit;
    end;
  end;

  // Path not found in menu.

  DriveButton.Caption := '';

  BitmapTmp := PixMapManager.GetDefaultDriveIcon(DriveButton.Height - 2,
                                                 DriveButton.Color);
  DriveButton.Glyph := BitmapTmp;

  if Assigned(BitmapTmp) then
    FreeAndNil(BitmapTmp);

  for i := 0 to pmDrivesMenu.Items.Count - 1 do
    pmDrivesMenu.Items[i].Checked := False;
end;

procedure TfrmMain.UpdateSelectedDrive(ANoteBook: TFileViewNotebook);
var
  Path : String;
  FileView: TFileView;
begin
  FileView := ANoteBook.ActiveView;
  if Assigned(FileView) then
  begin
    Path := FileView.CurrentPath;

    // Change left drive toolbar for left drive button.
    if (ANoteBook = nbLeft) then
    begin
      UpdateDriveToolbarSelection(dskLeft, Path);
      UpdateDriveButtonMenuSelection(btnLeftDrive, Path);

      // If only one drive toolbar is displayed then also change it.
      if gDriveBar1 and not gDriveBar2 then
        // dskRight is the main toolbar.
        UpdateDriveToolbarSelection(dskRight, Path);
    end
    // Change right drive toolbar for right drive button
    else if (ANoteBook = nbRight) then
    begin
      UpdateDriveToolbarSelection(dskRight, Path);
      UpdateDriveButtonMenuSelection(btnRightDrive, Path);
    end;
  end;
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
  if bShow <> MainTrayIcon.Visible then
  begin
    MainTrayIcon.Visible := bShow;
  end;
end;

function TfrmMain.GetWindowState: TWindowState;
{$IFDEF LCLQT}
var
  WindowStates: QtWindowStates;
{$ENDIF}
begin
{$IFDEF LCLQT}
  // On QT reported window state can be maximized and minimized at the same time
  // (meaning a minimized window which should be in a maximized state when shown again).
  WindowStates := QWidget_windowState(TQtWidget(Self.Handle).Widget);
  if (WindowStates and QtWindowMinimized) <> 0 then
    Result := wsMinimized
  else
{$ENDIF}
  Result := Self.WindowState;
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
const PTLen=40;
var
  FreeSize,
  TotalSize : Int64;
begin
  with lblCommandPath do
  begin
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

  edtCommand.Left := lblCommandPath.Width + 5;
  edtCommand.Width := TControl(edtCommand.Parent).Width - edtCommand.Left;
end;

procedure TfrmMain.OperationFinishedEvent(Operation: TFileSourceOperation; Event: TOperationManagerEvent);

  procedure UpdateTabs(Notebook: TFileViewNotebook);
  var
    i: Integer;
    aFileView: TFileView;
    aChangedFileSource: TObject;
  begin
    // Reload all views which file source type and address match.
    for i := 0 to Notebook.PageCount - 1 do
    begin
      aFileView := Notebook.View[i];
      aChangedFileSource := Operation.ChangedFileSource;
      if Assigned(aChangedFileSource) and
         // Same type of file source and same address point to the same file source.
         (aFileView.FileSource.ClassType = aChangedFileSource.ClassType) and
         (aFileView.FileSource.CurrentAddress = (aChangedFileSource as TFileSource).CurrentAddress) then
      begin
        aFileView.Reload;
      end;
    end;
  end;

begin
  UpdateTabs(LeftTabs);
  UpdateTabs(RightTabs);
end;

initialization
 {$I fmain.lrs}
end.
