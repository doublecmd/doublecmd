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
  Graphics, Forms, Menus, Controls, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls,ActnList,Buttons, uFileSystemWatcher,
  SysUtils, Classes,  {uFilePanel,} framePanel, {FileCtrl,} Grids,
  KASToolBar, SynEdit, KASBarMenu,KASBarFiles,uColumns, uFileList, LCLType,uCmdBox{$IFDEF UNIX},uterm{$ENDIF};

const
  cHistoryFile='cmdhistory.txt';

type

  TDropPopupMenu = class(TPopupMenu)
  private
    FDropParams: TDropParams;

  public
    constructor Create(AOwner: TComponent); override;
    procedure PopUp(DropParams: TDropParams);
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
    MenuItem4: TMenuItem;
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
    nbLeft: TNotebook;
    nbRight: TNotebook;
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
    procedure DeleteClick(Sender: TObject);
    procedure dskToolBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dskRightChangeLineCount(AddSize: Integer);
    procedure dskToolButtonClick(Sender: TObject; NumberOfButton: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormDataEvent(Data: PtrInt);
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
    procedure FrameEditExit(Sender: TObject);
    procedure FrameedtSearchExit(Sender: TObject);

    procedure FramedgPanelEnter(Sender: TObject);
    procedure framedgPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlLeftResize(Sender: TObject);
    procedure pnlLeftRightDblClick(Sender: TObject);
    procedure seLogWindowSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure ShowPathEdit;
    procedure FramelblLPathClick(Sender: TObject);
    procedure FramelblLPathMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function FramepnlFileBeforeChangeDirectory(Sender: TObject; const NewDir : String): Boolean;
    procedure FramepnlFileAfterChangeDirectory(Sender: TObject; const NewDir : String);
    procedure edtCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtCommandExit(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
    procedure FramePanelOnWatcherNotifyEvent(Sender: TObject; NotifyEvent: TWatchFilter);
    procedure tmHALTimer(Sender: TObject);
  private
    { Private declarations }
    PanelSelected:TFilePanelSelect;
    LeftFrameWatcher,
    RightFrameWatcher: TFileSystemWatcher;
    DrivesList : TList;
    MainSplitterHintWnd: THintWindow;

    // frost_asm begin
    // mainsplitter
    MainSplitterLeftMouseBtnDown:boolean;
    MainSplitterMouseDownX:integer;
    // lastWindowState
    lastWindowState:TWindowState;
    // frost_asm end
    procedure ColumnsMenuClick(Sender: TObject);
    function ExecuteCommandFromEdit(sCmd: String; bRunInTerm: Boolean): Boolean;
    procedure AddSpecialButtons(dskPanel: TKASToolBar);
    procedure ReLoadTabs(ANoteBook: TNoteBook);
  public
//    frameLeft, frameRight:TFrameFilePanel;
    
    function HandleActionHotKeys(var Key: Word; Shift: TShiftState):Boolean; // handled
    
    Function ActiveFrame:TFrameFilePanel;  // get Active frame
    Function NotActiveFrame:TFrameFilePanel; // get NotActive frame :)
    function FrameLeft:TFrameFilePanel;
    function FrameRight:TFrameFilePanel;
    Function IsAltPanel:Boolean;
    procedure AppException(Sender: TObject; E: Exception);
    //check selected count and generate correct msg, parameters is lng indexs
    Function GetFileDlgStr(sLngOne, sLngMulti : String):String;
    procedure HotDirSelected(Sender:TObject);
    procedure CreatePopUpHotDir;
    procedure CreatePopUpDirHistory;
    procedure miHotAddClick(Sender: TObject);
    procedure miHotDeleteClick(Sender: TObject);
    procedure miHotConfClick(Sender: TObject);
    procedure CalculateSpace(bDisplayMessage:Boolean);
    procedure RunRenameThread(srcFileList: TFileList; sDestPath: String; sDestMask: String);
    procedure RunCopyThread(srcFileList: TFileList; sDestPath: String; sDestMask: String;
                            bDropReadOnlyFlag: Boolean);
    procedure RenameFile(srcFileList: TFileList; dstFramePanel: TFrameFilePanel; destPath: String);
    procedure CopyFile(srcFileList: TFileList; dstFramePanel: TFrameFilePanel; destPath: String);
    procedure RenameFile(sDestPath:String); // this is for F6 and Shift+F6
    procedure CopyFile(sDestPath:String); //  this is for F5 and Shift+F5
    procedure ShowRenameFileEdit(const sFileName:String);
    procedure SetNotActFrmByActFrm;
    procedure SetActiveFrame(panel: TFilePanelSelect);
    procedure UpdateDiskCount;
    procedure CreateDrivesMenu;
    procedure DrivesMenuClick(Sender: TObject);
    procedure CreateDiskPanel(dskPanel : TKASToolBar);
    procedure CreatePanel(AOwner:TWinControl; APanel:TFilePanelSelect; sPath : String);
    function AddPage(ANoteBook:TNoteBook; bSetActive: Boolean = True):TPage;
    function RemovePage(ANoteBook:TNoteBook; iPageIndex:Integer): LongInt;
    procedure LoadTabs(ANoteBook:TNoteBook);
    procedure SaveTabs(ANoteBook:TNoteBook);
    procedure SetMultilineTabs(ANoteBook: TNoteBook; Multiline: Boolean);
    function ExecCmd(Cmd:string; param:string='') : Boolean;
    function ExecCmdEx(Sender: TObject; NumberOfButton:Integer) : Boolean;
    procedure ToggleConsole;
    procedure ToggleFileSystemWatcher;
    procedure UpdateWindowView;
    procedure LoadWindowState;
    procedure SaveWindowState;
    procedure SaveShortCuts;
    procedure LoadShortCuts;
    function  IsCommandLineVisible: Boolean;
  published
    property SelectedPanel:TFilePanelSelect read PanelSelected;
  end;
var
  frmMain: TfrmMain;
{$IFDEF UNIX}
  Cons: TConThread = nil;
{$ENDIF}

implementation

uses
  Clipbrd, LCLIntf, uTypes, fAbout, uGlobs, uLng, fOptions,{ fViewer,}fconfigtoolbar, fFileAssoc,
  uCopyThread, uDeleteThread, uVFSUtil, uWCXModule, uVFSTypes, Masks, uFileOp,
  fMkDir, fCopyDlg, fCompareFiles,{ fEditor,} fMoveDlg, uMoveThread, uShowMsg, uClassesEx,
  fFindDlg, uSpaceThread, fHotDir, fSymLink, fHardLink, uDCUtils, uLog, uWipeThread,
  fMultiRename, uShowForm, uGlobsPaths, fFileOpDlg, fMsg, fPackDlg, fExtractDlg,
  fLinker, fSplitter, uFileProcs, LCLProc, uOSUtils, uOSForms, uPixMapManager,
  fColumnsSetConf, uDragDropEx, StrUtils, uKeyboard, WSExtCtrls;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  IniBarFile: TIniFileEx;
  slCommandHistory: TStringListEx;
  I : Integer;
begin
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
  nbLeft.Options:=[nboShowCloseButtons];
  nbRight.Options:=[nboShowCloseButtons];

  SetMultilineTabs(nbLeft, Boolean(gDirTabOptions and tb_multiple_lines));
  SetMultilineTabs(nbRight, Boolean(gDirTabOptions and tb_multiple_lines));


  actShowSysFiles.Checked:=uGlobs.gShowSystemFiles;

  AllowDropFiles := not uDragDropEx.IsExternalDraggingSupported;

  PanelSelected:=fpLeft;

  pnlNotebooks.Width:=Width div 2;

  //DebugLN('dskLeft.Width == ' + IntToStr(dskLeft.Width));
  //DebugLN('dskRight.Width == ' + IntToStr(dskRight.Width));
  DrivesList := GetAllDrives;

  { Delete drives that in drives black list }
  for I:= DrivesList.Count - 1 downto 0 do
    begin
      if MatchesMaskList(PDrive(DrivesList.Items[I])^.Name, gDriveBlackList) then
        DrivesList.Delete(I);
    end;

  CreateDrivesMenu;

  (*Create Disk Panels*)
  dskLeft.Visible :=  gDriveBar2;
  if gDriveBar2 and gDriveBar1 then
    CreateDiskPanel(dskLeft);

  dskRight.Visible := gDriveBar1;
  if gDriveBar1 then
    CreateDiskPanel(dskRight);

  pnlSyncSize.Visible := gDriveBar1;
  (*/Create Disk Panels*)

  (*Tool Bar*)
  if gButtonBar then
    begin
      MainToolBar.FlatButtons := gToolBarFlat;
      MainToolBar.ButtonGlyphSize := gToolBarIconSize;
      MainToolBar.ChangePath := gpExePath;
      MainToolBar.EnvVar := '%commander_path%';
      try
        IniBarFile:= TIniFileEx.Create(gpIniDir + 'default.bar', fmOpenRead);
        MainToolBar.LoadFromIniFile(IniBarFile);
      finally
        FreeThenNil(IniBarFile);
      end;
    end;
  (*Tool Bar*)

  pmButtonMenu.BarFile.ChangePath := gpExePath;
  pmButtonMenu.BarFile.EnvVar := '%commander_path%';

  LoadShortCuts;

  {Load some options from layout page}

  for I := 0 to pnlKeys.ControlCount - 1 do  // function keys
    if pnlKeys.Controls[I] is TSpeedButton then
      (pnlKeys.Controls[I] as TSpeedButton).Flat := gInterfaceFlat;

  MainToolBar.Visible := gButtonBar;

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
  btnRightTargetEqualSource.Flat:= gInterfaceFlat;

  pnlCommand.Visible := gCmdLine;
  pnlKeys.Visible := gKeyButtons;
  LogSplitter.Visible := gLogWindow;
  seLogWindow.Visible := gLogWindow;

  seLogWindow.Font.Name := gEditorFontName;

  pmColumnsMenu.Items.Clear;
  //DebugLn('frmMain.FormCreate Done');
  
  HotMan.RegisterHotkeyManager(Self);
  HotMan.RegisterHotkeyManager(edtCommand);

  if HotMan.HotkeyList.Count=0 then LoadDefaultHotkeyBindings;
  // load shortcuts to action list for showing it in menu
  HotMan.LoadShortCutToActionList(ActionLst);
  // toggle console window
  ToggleConsole;
  // toggle file system watcher
  LeftFrameWatcher:= nil;
  RightFrameWatcher:= nil;
  ToggleFileSystemWatcher;
end;

procedure TfrmMain.btnLeftClick(Sender: TObject);
begin
  with Sender as TSpeedButton do
  begin
    if Caption = '/' then
      FrameLeft.pnlFile.cdRootLevel
    else if Caption = '..' then
      FrameLeft.pnlFile.cdUpLevel
    else if Caption = '~' then
    begin
      FrameLeft.pnlFile.ActiveDir := GetHomeDir;
      FrameLeft.pnlFile.LoadPanel;
    end;
  end;

  SetActiveFrame(fpLeft);
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
  with Sender as TSpeedButton do
  begin
    if Caption = '/' then
      FrameRight.pnlFile.ActiveDir := ExtractFileDrive(FrameRight.pnlFile.ActiveDir);
    if Caption = '..' then
      FrameRight.pnlFile.cdUpLevel;
    if Caption = '~' then
      FrameRight.pnlFile.ActiveDir := GetHomeDir;
  end;
  FrameRight.pnlFile.LoadPanel;

  SetActiveFrame(fpRight);
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

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  IniBarFile: TIniFileEx;
  slCommandHistory: TStringListEx;
begin
  DebugLn('frmMain.Destroy');

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

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  FrameFilePanel: TFrameFilePanel;
  I: Integer;
  FileList: TFileList;
  FileNamesList: TStringList;
  Point: TPoint;
  DropParams: TDropParams;
begin
  FrameFilePanel:= nil;
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

  FrameFilePanel.dgPanel.DropFiles(DropParams);
end;

procedure TfrmMain.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  ModifierKeys: TShiftState;
begin
  if (not edtCommand.Focused) and (edtCommand.Tag = 0) then
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
          // Make upper case if either caps-lock is toggled or shift pressed.
          if (ssCaps in ModifierKeys) xor (ssShift in ModifierKeys) then
            UTF8Key := UTF8UpperCase(UTF8Key)
          else
            UTF8Key := UTF8LowerCase(UTF8Key);

          ActiveFrame.ShowAltPanel(UTF8Key);
          UTF8Key:= '';
          KeyPreview:= False;
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

procedure TfrmMain.FormDataEvent(Data: PtrInt);
begin
  MainTrayIcon.Visible:= False;
end;

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  // запоминаєм состояние окна перед минимизацией для
  // дальнейшего восстановления после разворачивания из трея
  if WindowState <> wsMinimized then
     lastWindowState:=WindowState;

  if gTrayIcon and (WindowState = wsMinimized) and (not MainTrayIcon.Visible) then
    begin
      Hide;
      MainTrayIcon.Visible:= True;
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
  // делал по другому
  // WindowState:=lastWindowState; но при wsNormal
  // окно становится видимим но свернутим
  if lastWindowState=wsMaximized then  WindowState:=wsMaximized;
  ShowOnTop;
  Application.QueueAsyncCall(@FormDataEvent, 0);
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
  if ActiveFrame.IsActiveItemValid then
  with ActiveFrame, ActiveFrame.GetActiveItem^ do
    begin
      sFileName := ActiveDir + sName;
      MainToolBar.AddButton('', sFileName, ExtractOnlyFileName(sName), sFileName);
      MainToolBar.AddX(sFileName, sFileName, '', sPath, ExtractOnlyFileName(sName));
      try
        IniBarFile:= TIniFileEx.Create(gpIniDir + 'default.bar');
        MainToolBar.SaveToIniFile(IniBarFile);
      finally
        FreeThenNil(IniBarFile);
      end;
    end;
end;

procedure TfrmMain.MainToolBarDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if ActiveFrame.IsActiveItemValid then
    Accept := True
  else
    Accept := False;
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

procedure TfrmMain.dskRightChangeLineCount(AddSize: Integer);
begin
  pnlSyncSize.Height := pnlSyncSize.Height + AddSize;
end;

procedure TfrmMain.dskToolButtonClick(Sender: TObject; NumberOfButton: Integer);
var
  Command : String;
  dskPanel : TKASToolBar;
  FrameFilePanel : TFrameFilePanel;
  btnDrive : TSpeedButton;
begin
  dskPanel := (Sender as TKASToolBar);

  if (dskPanel.Align = alLeft) or (not gDriveBar2 and (PanelSelected = fpLeft))  then
    begin
      FrameFilePanel := FrameLeft;
      btnDrive := btnLeftDrive;
      PanelSelected := fpLeft;
    end
  else
    begin
      FrameFilePanel := FrameRight;
      btnDrive := btnRightDrive;
      PanelSelected := fpRight;
    end;
    

  if dskPanel.Buttons[NumberOfButton].GroupIndex = 0 then
     begin
     Command := dskPanel.Commands[NumberOfButton];
     if Command = '/' then
        FrameFilePanel.pnlFile.ActiveDir := ExtractFileDrive(FrameFilePanel.pnlFile.ActiveDir);
     if Command = '..' then
        FrameFilePanel.pnlFile.cdUpLevel;
     if Command = '~' then
        FrameFilePanel.pnlFile.ActiveDir := GetHomeDir;
     end
  else
   begin
     if IsAvailable(dskPanel.Commands[NumberOfButton]) then
       begin
         if PanelSelected = fpRight then
           begin
             if IncludeTrailingPathDelimiter(ExtractFileDrive(FrameLeft.pnlFile.ActiveDir)) = dskPanel.Commands[NumberOfButton] then
               FrameFilePanel.pnlFile.ActiveDir := FrameLeft.pnlFile.ActiveDir
             else
               FrameFilePanel.pnlFile.ActiveDir := dskPanel.Commands[NumberOfButton];
           end
         else
           begin
             if IncludeTrailingPathDelimiter(ExtractFileDrive(FrameRight.pnlFile.ActiveDir)) = dskPanel.Commands[NumberOfButton] then
               FrameFilePanel.pnlFile.ActiveDir := FrameRight.pnlFile.ActiveDir
             else
               FrameFilePanel.pnlFile.ActiveDir := dskPanel.Commands[NumberOfButton];
           end;

         dskPanel.Tag := NumberOfButton;
         btnDrive.Tag:= NumberOfButton;

         btnDrive.Glyph := PixMapManager.GetDriveIcon(PDrive(DrivesList[NumberOfButton]), btnDrive.Height - 4, btnDrive.Color);
         btnDrive.Caption := dskRight.Buttons[NumberOfButton].Caption;
       end
     else
       begin
         dskPanel.Buttons[dskPanel.Tag].Down := True;
         msgOK(rsMsgDiskNotAvail);
       end;
  end;
  FrameFilePanel.pnlFile.LoadPanel;

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
  try
    SaveWindowState;
    SaveShortCuts;
    SaveGlobs;
  except
  end;

{$IFDEF UNIX}
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
  SetActiveFrame(fpLeft);
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
  SourceFileName, TargetFileName: String;
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
            TargetPanel.dgPanel.DoDragDropOperation(ddoMove, DropParamsRef);
          end
        else if (Sender as TMenuItem).Name = 'miCopy' then
          begin
            TargetPanel.dgPanel.DoDragDropOperation(ddoCopy, DropParamsRef);
          end
        else if (Sender as TMenuItem).Name = 'miSymLink' then
          begin
            TargetPanel.dgPanel.DoDragDropOperation(ddoSymLink, DropParamsRef);
          end
        else if (Sender as TMenuItem).Name = 'miHardLink' then
          begin
            TargetPanel.dgPanel.DoDragDropOperation(ddoHardLink, DropParamsRef);
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
begin
  Cmd:= (Sender as TMenuItem).Action.Name;
  Cmd:= 'cm_' + Copy(Cmd, 4, Length(Cmd) - 3);
  Actions.Execute(Cmd, pmTabMenu.Parent.Name);
end;

procedure TfrmMain.nbPageAfterMouseDown(Data: PtrInt);
begin
  with TNoteBook(Pointer(Data)) do
  begin
    if (Name = 'nbLeft') and (FrameLeft <> nil) then
      begin
        if PanelSelected = fpLeft then // same panel
          FrameLeft.SetFocus
        else if Boolean(gDirTabOptions and tb_activate_panel_on_click) then
          SetActiveFrame(fpLeft)
        else
          FrameRight.SetFocus;
      end;
    if (Name = 'nbRight') and (FrameRight <> nil) then
      begin
        if PanelSelected = fpRight then // same panel
          FrameRight.SetFocus
        else if Boolean(gDirTabOptions and tb_activate_panel_on_click) then
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
begin
  with Sender as TNoteBook do
  begin
    if Page[PageIndex].Tag = 2 then // if locked with directory change
      begin
        ActiveFrame.pnlFile.ActiveDir:= Page[PageIndex].Hint;
        ActiveFrame.LoadPanel;
      end;

    if (Name = 'nbLeft') and (FrameLeft <> nil) then
      begin
        FrameLeft.pnlFile.UpdatePrompt;
        if Assigned(LeftFrameWatcher) and (LeftFrameWatcher.WatchPath <> FrameLeft.ActiveDir) then
          LeftFrameWatcher.WatchPath:= FrameLeft.ActiveDir;
      end;
    if (Name = 'nbRight') and (FrameRight <> nil) then
      begin
        FrameRight.pnlFile.UpdatePrompt;
        if Assigned(RightFrameWatcher) and (RightFrameWatcher.WatchPath <> FrameRight.ActiveDir) then
          RightFrameWatcher.WatchPath:= FrameRight.ActiveDir;    
      end;
  end;
end;

procedure TfrmMain.nbPageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  PopUpPoint: TPoint;
begin
  {$IFDEF LCLGTK2}
  if Button = mbMiddle then
  {$ELSE}
  if Button = mbRight then
  {$ENDIF}
    begin
      PopUpPoint:= (Sender as TNoteBook).ClientToScreen(Point(X, Y));
      pmTabMenu.Parent:= (Sender as TNoteBook);
      pmTabMenu.PopUp(PopUpPoint.x, PopUpPoint.y);
    end;
end;

procedure TfrmMain.NoteBookCloseTabClicked(Sender: TObject);
begin
  with (Sender As TPage) do
  begin
    RemovePage(Parent as TNoteBook, PageIndex);
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


function TfrmMain.HandleActionHotKeys(var Key: Word; Shift: TShiftState):Boolean; // handled
var
  pfri : PFileRecItem;
begin
  Result:=True;
  // ---- 30.04.2009 - переписал для удаления в корзину. ----
  If (Key = VK_F8) or (Key = VK_DELETE) then
   begin
    if ((not edtCommand.Focused) and (edtCommand.Tag = 0)) or (Key = VK_F8) then
     begin
      if gUseTrash and mbCheckTrash then // 14.05.2009 - additional check for various linux distributives.
       begin
        if Shift=[ssShift] then // если шифт - удаляем напрямую
         Actions.cm_Delete('')
        else Actions.cm_Delete('recycle'); // без шифта удаляем в корзину
       end
      else Actions.cm_Delete('');  // если корзина отключена в конфигурации, или (для линукс) нет программы gvsf-trash, то удалять напрямую.
      Exit;
     end;
   end;
  // ---------------------------------------------------------

  if Shift=[] then
  begin
    case Key of
     VK_APPS:
       begin
         Actions.cm_ContextMenu('');
         //actContextMenu.Execute;
         Exit;
       end;
    end; // case
  end; // Shift=[]

  if (Key=VK_Return) or (Key=VK_SELECT) then
  begin
    Key:=0;
    with ActiveFrame do
    begin
      if (Shift=[])or (Shift=[ssCaps]) then // 21.05.2009 - не учитываем CapsLock при перемещении по панелям
      begin
        if edtCommand.Focused and (edtCommand.Text='') then
        begin
          ActiveFrame.SetFocus;
        end
        else if (not IsCommandLineVisible) or (edtCommand.Text='') then
        begin
          // Only if there are items in the panel.
          if not IsEmpty then
          begin
            if pnlFile.PanelMode = pmDirectory then
              Screen.Cursor:=crHourGlass;
            try
              pnlFile.ChooseFile(pnlFile.GetActiveItem);
              UpDatelblInfo;
            finally
              dgPanel.Invalidate;
              Screen.Cursor:=crDefault;
            end;
          end;
          Exit;
        end
        else
        begin
          // execute command line
          mbSetCurrentDir(ActiveDir);
          ExecuteCommandFromEdit(edtCommand.Text, False);
          ClearCmdLine;
          RefreshPanel;
          ActiveFrame.SetFocus;
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
          // workaround for GTK
          // edtCommandExit is not always called when losing focus
          edtCommandExit(Self);
{$ENDIF}
          Exit;
        end;
      end; //Shift=[] + 21.05.2009 - не учитываем CapsLock при перемещении по панелям

      // execute active file or command line in terminal (Shift+Enter)
      if Shift=[ssShift] then
      begin
        mbSetCurrentDir(ActiveDir);
        if (not IsCommandLineVisible) or (edtCommand.Text='') then
        begin
          if IsActiveItemValid then
          begin
            ExecCmdFork(ActiveDir + pnlFile.GetActiveItem^.sName, True, gRunInTerm);
          end;
        end
        else
          begin
            // execute command line
            ExecuteCommandFromEdit(edtCommand.Text, True);
            ClearCmdLine;
            RefreshPanel;
            ActiveFrame.SetFocus;
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
            // workaround for GTK,
            // edtCommandExit is not always called when losing focus
            edtCommandExit(Self);
{$ENDIF}
          end;
        Exit;
      end;
      // ctrl enter
      if Shift=[ssCtrl] then
      begin
        if IsActiveItemValid then
          edtCmdLine.Text:=edtCmdLine.Text+pnlFile.GetActiveItem^.sName+' ';
        Exit;
      end;
      // ctrl+shift+enter
      if Shift=[ssShift,ssCtrl] then
      begin
        if Assigned(GetActiveItem) then
        begin
          if (pnlFile.GetActiveItem^.sName = '..') then
          begin
            edtCmdLine.Text:=edtCmdLine.Text+(pnlFile.ActiveDir) + ' ';
          end
          else
          begin
            edtCmdLine.Text:=edtCmdLine.Text+(pnlFile.ActiveDir) + pnlFile.GetActiveItem^.sName+' ';
          end;
        end;
        Exit;
      end;
    end;
  end;  // handle ENTER with some modifier

  if Shift=[ssCtrl] then
  begin
    // handle ctrl+right
    if (Key=VK_Right) then
    begin
      if (PanelSelected = fpLeft) then
        SetNotActFrmByActFrm;
      Exit;
    end;
    // handle ctrl+left
    if (Key=VK_Left) then
    begin
      if (PanelSelected = fpRight) then
        SetNotActFrmByActFrm;
      Exit;
    end;
  end; // Shift=[ssCtrl]
  
  // not handled
  Result:=False;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
var
  CmdText : UTF8String;
begin
  if gCmdLine then  // If command line is enabled
  begin
    if Key=#27 then
      ActiveFrame.ClearCmdLine;
    if ((ord(key)>31) and (ord(key)<255)) or (ord(Key) = VK_BACK) then
    begin
      if ((Key='-') or (Key='*') or (Key='+') or (Key=' '))and (Trim(edtCommand.Text)='') then Exit;
      if (not edtCommand.Focused) and (edtCommand.Tag = 0) then
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

Function TfrmMain.ActiveFrame:TFrameFilePanel;
begin

  case PanelSelected of
    fpLeft:
      Result:=FrameLeft;
    fpRight:
      Result:=FrameRight;
  else
    assert(false,'Bad active frame');
  end;
end;

Function TfrmMain.NotActiveFrame:TFrameFilePanel;
begin
  case PanelSelected of
    fpRight: Result:=FrameLeft;
    fpLeft: Result:=FrameRight;
  else
    assert(false,'Bad active frame');
    Result:=FrameLeft;// only for compilator warning;
  end;
end;

function TfrmMain.FrameLeft: TFrameFilePanel;
begin
//  DebugLn(nbLeft.Page[nbLeft.PageIndex].Components[0].ClassName);
  Result:=TFrameFilePanel(nbLeft.Page[nbLeft.PageIndex].Components[0]);
end;

function TfrmMain.FrameRight: TFrameFilePanel;
begin
//  DebugLn(nbRight.Page[nbRight.PageIndex].Components[0].ClassName);
  Result:=TFrameFilePanel(nbRight.Page[nbRight.PageIndex].Components[0]);
  
//  Result:=TFrameFilePanel(nbRight.Page[0].Components[0]);

end;

Function TfrmMain.IsAltPanel:Boolean;
begin
  Result:= frameLeft.pnAltSearch.Visible or
           FrameRight.pnAltSearch.Visible;
end;


procedure TfrmMain.AppException(Sender: TObject; E: Exception);
begin
  WriteLn(stdErr,'Exception:',E.Message);
  WriteLn(stdErr,'Func:',BackTraceStrFunc(get_caller_frame(get_frame)));
  Dump_Stack(StdErr, get_caller_frame(get_frame));
end;


Function TfrmMain.GetFileDlgStr(sLngOne, sLngMulti:String):String;
var
  iSelCnt:Integer;
begin
  with ActiveFrame do
  begin
    iSelCnt:=pnlFile.GetSelectedCount;
    if iSelCnt=0 then Abort;
    if iSelCnt >1 then
      Result:=Format(sLngMulti, [iSelCnt])
    else
      Result:=Format(sLngOne, [pnlFile.GetFirstSelectedItem^.sName])
  end;
end;


procedure TfrmMain.miHotAddClick(Sender: TObject);
begin
  inherited;
  glsHotDir.Add(ActiveFrame.ActiveDir);
//  pmHotList.Items.Add();
// OnClick:=HotDirSelected;
end;

procedure TfrmMain.miHotDeleteClick(Sender: TObject);
var i : integer;
begin
  i:= glsHotDir.IndexOf(ActiveFrame.ActiveDir);
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
  if glsHotDir.IndexOf(ActiveFrame.ActiveDir)>0 then
  begin
    mi.Caption:=Format(rsMsgPopUpHotDelete,[ActiveFrame.ActiveDir]);
    mi.OnClick:=@miHotDeleteClick;
  end
  else
  begin
    mi.Caption:=Format(rsMsgPopUpHotAdd,[ActiveFrame.ActiveDir]);
    mi.OnClick:=@miHotAddClick;
  end;
  pmHotList.Items.Add(mi);

  // now add configure item
  mi:=TMenuItem.Create(pmHotList);
  mi.Caption:=rsMsgPopUpHotCnf;
  mi.OnClick:=@miHotConfClick;
  pmHotList.Items.Add(mi);
//  KeyPreview:=False;
end;

procedure TfrmMain.HotDirSelected(Sender:TObject);
var
  sDummy:String;
begin
 // this handler is used by HotDir and DirHistory
 // must extract & from Caption
  sDummy:=(Sender As TMenuItem).Caption;
  SDummy:=StringReplace(sDummy,'&','',[rfReplaceAll]);
  ActiveFrame.pnlFile.ActiveDir:=sDummy;
  ActiveFrame.LoadPanel;

  KeyPreview:=True;
  with ActiveFrame.dgPanel do
  begin
    if RowCount>0 then
     Row:=1;
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

procedure TfrmMain.CalculateSpace(bDisplayMessage:Boolean);
var
  fl:TFileList;
  p:TFileRecItem;
begin
  fl:= TFileList.Create; // free at Thread end by thread
  with ActiveFrame do
  begin
    if not bDisplayMessage then  // Calculate by <Space> key
      begin
        if not IsActiveItemValid then
        begin
          FreeAndNil(fl);
          Exit;
        end;

        p:= GetActiveItem^;
        p.sNameNoExt:= p.sName; //dstname
        p.sName:= ActiveDir+p.sName;
        p.sPath:= '';

        fl.AddItem(@p);
      end
    else
      begin
        if SelectFileIfNoSelected(GetActiveItem) = False then
        begin
          FreeAndNil(fl);
          Exit;
        end;

        CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir);
      end;
  end;

  try
  with TSpaceThread.Create(fl, bDisplayMessage) do
    begin
      if not bDisplayMessage then
        Screen.Cursor:= crHourGlass;

      // start thread
      Resume;

      if not bDisplayMessage then
        begin
          WaitFor;
          Screen.Cursor:= crDefault;
        end;

      with ActiveFrame.GetActiveItem^ do
      begin
        if (bDisplayMessage = False) then
          iDirSize:= FilesSize;
        ActiveFrame.pnlFile.LastActive:= sName;
      end;

      if not bDisplayMessage then
        Free;
    end;
  finally
    with ActiveFrame do
    begin
      Screen.Cursor:= crDefault;
//      UnMarkAll;
      pnlFile.UpdatePanel;
    end;
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
procedure TfrmMain.RenameFile(srcFileList: TFileList; dstFramePanel: TFrameFilePanel; destPath: String);
var
  sDestPath,
  sDstMaskTemp: String;
  sCopyQuest: String;
begin
  if (srcFileList.Count=1) and not (FPS_ISDIR(srcFileList.GetItem(0)^.iMode) or srcFileList.GetItem(0)^.bLinkIsDir) then
    begin
      sCopyQuest:= Format(rsMsgRenSel, [srcFileList.GetItem(0)^.sName]);
      sDestPath:= destPath + ExtractFileName(srcFileList.GetItem(0)^.sName);
    end
  else
    begin
      sCopyQuest:= Format(rsMsgRenFlDr, [srcFileList.Count]);
      sDestPath:= destPath + '*.*';
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
      sDestPath:= ExtractFilePath(edtDst.Text);
      sDstMaskTemp:= ExtractFileName(edtDst.Text);
    finally
      Free;
    end;
  end;

  (* Move files *)

  RunRenameThread(srcFileList, sDestPath, sDstMaskTemp);

end;

(* Used for drag&drop copy from external application *)
// Frees srcFileList automatically.
procedure TfrmMain.CopyFile(srcFileList: TFileList; dstFramePanel: TFrameFilePanel; destPath: String);
var
  sCopyQuest,
  sDestPath,
  sDstMaskTemp: String;
  blDropReadOnlyFlag: Boolean;
begin
  if (srcFileList.Count=1) and not (FPS_ISDIR(srcFileList.GetItem(0)^.iMode) or srcFileList.GetItem(0)^.bLinkIsDir) then
    begin
      sCopyQuest:= Format(rsMsgCpSel, [srcFileList.GetItem(0)^.sName]);
      sDestPath:= destPath + ExtractFileName(srcFileList.GetItem(0)^.sName);
    end
  else
    begin
      sCopyQuest:= Format(rsMsgCpFlDr, [srcFileList.Count]);
      sDestPath:= destPath + '*.*';
    end;

  (* Copy files between archive and real file system *)

  if  dstFramePanel.pnlFile.PanelMode = pmArchive then
    begin
      if not IsBlocked then
        begin
          if  (VFS_CAPS_COPYIN in dstFramePanel.pnlFile.VFS.VFSmodule.VFSCaps) then
            begin
              DebugLn('+++ Pack files to archive +++');
              sDestPath:=ExtractFilePath(sDestPath);
              ShowPackDlg(dstFramePanel.pnlFile.VFS, srcFileList, sDestPath, False);
            end
          else
            msgWarning(rsMsgErrNotSupported);
        end;
      Exit;
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
      sDestPath:=ExtractFilePath(edtDst.Text);
      sDstMaskTemp:=ExtractFileName(edtDst.Text);
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

procedure TfrmMain.RenameFile(sDestPath:String);
var
  fl:TFileList;
  sDstMaskTemp:String;
  sCopyQuest:String;
begin
  // Exit if no valid files selected.
  if ActiveFrame.SelectFileIfNoSelected(ActiveFrame.GetActiveItem) = False then Exit;

  if (ActiveFrame.pnlFile.GetSelectedCount=1) then
  with ActiveFrame.pnlFile do
  begin
    if sDestPath='' then
      begin
        if ActiveFrame.IsActiveItemValid then
          ShowRenameFileEdit(ActiveDir + GetActiveItem^.sName);
        Exit;
      end
    else
      begin
        if FPS_ISDIR(GetFirstSelectedItem^.iMode) then
          sDestPath:=sDestPath + '*.*'
        else
          sDestPath:=sDestPath + ExtractFileName(GetFirstSelectedItem^.sName);
      end;
  end
  else
    sDestPath:=sDestPath+'*.*';

  try
    fl:= TFileList.Create; // free at Thread end by thread
    fl.CurrentDirectory := ActiveFrame.ActiveDir;
    CopyListSelectedExpandNames(ActiveFrame.pnlFile.FileList,fl,ActiveFrame.ActiveDir);

    with TfrmMoveDlg.Create(Application) do
    begin
      try
        edtDst.Text:=sDestPath;
        sCopyQuest:=GetFileDlgStr(rsMsgRenSel, rsMsgRenFlDr);
        lblMoveSrc.Caption:=sCopyQuest;
        if ShowModal=mrCancel then
        begin
          FreeAndNil(fl); // Free now, because the thread won't be run.
          Exit; // throught finally
        end;

        sCopyQuest := ExtractFilePath(edtDst.Text);
        if Length(sCopyQuest) <> 0 then
          sDestPath:= sCopyQuest
        else
          begin
            sCopyQuest:= ActiveFrame.pnlFile.GetActiveItem^.sPath;
            if Length(sCopyQuest) <> 0 then
              sDestPath:= sCopyQuest
            else
              sDestPath:= ActiveFrame.pnlFile.ActiveDir;
          end;
        sDstMaskTemp:=ExtractFileName(edtDst.Text);
      finally
        with ActiveFrame do
	  UnSelectFileIfSelected(GetActiveItem);
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
  fl:TFileList;
  sDstMaskTemp:String;
  sCopyQuest:String;
  blDropReadOnlyFlag : Boolean;
begin
  if (ActiveFrame.pnlFile.PanelMode in [pmVFS, pmArchive]) then
  begin
    if (NotActiveFrame.pnlFile.PanelMode in [pmVFS, pmArchive]) then
    begin
      // At least one panel must be real file system.
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    if not (VFS_CAPS_COPYOUT in ActiveFrame.pnlFile.VFS.VFSmodule.VFSCaps) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;
  end;

  if (NotActiveFrame.pnlFile.PanelMode in [pmVFS, pmArchive]) then
  begin
    if not (VFS_CAPS_COPYIN in NotActiveFrame.pnlFile.VFS.VFSmodule.VFSCaps) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;
  end;


  // Exit if no valid files selected.
  if ActiveFrame.SelectFileIfNoSelected(ActiveFrame.GetActiveItem) = False then Exit;

  fl:=TFileList.Create; // free at Thread end by thread
  fl.CurrentDirectory := ActiveFrame.ActiveDir;
  try
    CopyListSelectedExpandNames(ActiveFrame.pnlFile.FileList,fl,ActiveFrame.ActiveDir);

    if (ActiveFrame.pnlFile.GetSelectedCount=1) and not (FPS_ISDIR(ActiveFrame.pnlFile.GetFirstSelectedItem^.iMode) or ActiveFrame.pnlFile.GetFirstSelectedItem^.bLinkIsDir) then
      sDestPath:=sDestPath + ExtractFileName(ActiveFrame.pnlFile.GetFirstSelectedItem^.sName)
    else
      sDestPath:=sDestPath + '*.*';

    (* Copy files between archive and real file system *)
  
    (* Check active panel *)
    if ActiveFrame.pnlFile.PanelMode = pmArchive then
      begin
        if not IsBlocked then
          begin
            DebugLn('+++ Extract files from archive +++');
            ShowExtractDlg(ActiveFrame, fl, ExtractFilePath(sDestPath));
            NotActiveFrame.RefreshPanel;
          end
          else
            FreeAndNil(fl);
        Exit;
      end;

    (* Check not active panel *)
    if  NotActiveFrame.pnlFile.PanelMode = pmArchive then
      begin
        if not IsBlocked then
          begin
            DebugLn('+++ Pack files to archive +++');
            sDestPath:=ExtractFilePath(sDestPath);
            ShowPackDlg(NotActiveFrame.pnlFile.VFS, fl, sDestPath, False);
          end
          else
            FreeAndNil(fl);
        Exit;
      end;
                                                        
    with TfrmCopyDlg.Create(Application) do
    begin
      try
        edtDst.Text:=sDestPath;
        sCopyQuest:=GetFileDlgStr(rsMsgCpSel, rsMsgCpFlDr);
        lblCopySrc.Caption := sCopyQuest;
        cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;
        cbDropReadOnlyFlag.Visible := (NotActiveFrame.pnlFile.PanelMode = pmDirectory);
        if ShowModal=mrCancel then
        begin
          FreeAndNil(fl); // Free now, because the thread won't be run.
          Exit ; // throught finally
        end;

        sCopyQuest := ExtractFilePath(edtDst.Text);
        if Length(sCopyQuest) <> 0 then
          sDestPath:= sCopyQuest
        else
          begin
            sCopyQuest:= ActiveFrame.pnlFile.GetActiveItem^.sPath;
            if Length(sCopyQuest) <> 0 then
              sDestPath:= sCopyQuest
            else
              sDestPath:= ActiveFrame.pnlFile.ActiveDir;
          end;
        sDstMaskTemp:=ExtractFileName(edtDst.Text);
        blDropReadOnlyFlag := cbDropReadOnlyFlag.Checked;

      finally
        with ActiveFrame do
          UnSelectFileIfSelected(GetActiveItem);

        Free;
      end;
    end; //with

    (* Copy files between VFS and real file system *)
  
    (* Check not active panel *)
    if NotActiveFrame.pnlFile.PanelMode = pmVFS then
      begin
        DebugLn('+++ Copy files to VFS +++');
        NotActiveFrame.pnlFile.VFS.VFSmodule.VFSCopyInEx(fl, sDestPath, 0);
        Exit;
      end;

    (* Check active panel *)
    if ActiveFrame.pnlFile.PanelMode = pmVFS then
      begin
        DebugLn('+++ Copy files from VFS +++');
        ActiveFrame.pnlFile.VFS.VFSmodule.VFSCopyOutEx(fl, sDestPath, 0);
        Exit;
      end;

    (* Copy files between real file system *)

    RunCopyThread(fl, sDestPath, sDstMaskTemp, blDropReadOnlyFlag);

  except
    FreeAndNil(fl);
  end;
end;


procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ModifierKeys: TShiftState;
  UTF8Char: TUTF8Char;
begin
  // used for quick search by Ctrl+Alt+Letter and Alt+Letter
  if gQuickSearch and (not edtCommand.Focused) and (edtCommand.Tag = 0) then
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
        ActiveFrame.ShowAltPanel(UTF8Char);
        Key := 0;
        KeyPreview := False;
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
    ActiveFrame.pnlFile.cdUpLevel;
    Key:=0;

    Exit;
  end;

  // CTRL+PgDown
  if (Shift=[ssCtrl]) and (Key=VK_NEXT) then
  begin
    with ActiveFrame do
    begin
      if IsActiveItemValid then
        begin
          if FPS_ISDIR(pnlFile.GetActiveItem^.iMode) or (pnlFile.GetActiveItem^.bLinkIsDir) then
            pnlFile.cdDownLevel(pnlFile.GetActiveItem)
          else
            Actions.cm_OpenArchive('');
            //actOpenArchive.Execute;
        end;
    end;
    Key:=0;
    Exit;
  end;

  // cursors keys in Lynx like mode
  if (Shift=[]) and (Key=VK_LEFT) and gLynxLike and (edtCommand.Text='') then
  begin
    ActiveFrame.pnlFile.cdUpLevel;
    Key:=0;
    Exit;
  end;

  if (Shift=[]) and (Key=VK_RIGHT) and gLynxLike and (edtCommand.Text='') then
  begin
    with ActiveFrame do
    begin
      if Assigned(GetActiveItem) then
        pnlFile.ChooseFile(pnlFile.GetActiveItem,true);
    end;
    Key:=0;
    Exit;
  end;

//  DebugLn(Key);
  if HandleActionHotKeys(Key, Shift) Then
  begin
    Key:=0;;
    Exit;
  end;
//  DebugLn(Key);

  if (shift=[ssCtrl]) and (Key=VK_Up) then
  begin
    Key:=0;
    Actions.cm_OpenDirInNewTab('');
    //actOpenDirInNewTab.Execute;
    Exit;
  end;

  if (shift=[ssCtrl]) and (Key=VK_Down) then
  begin
    Key:=0;
    Actions.cm_ShowCmdLineHistory('');
    //actShowCmdLineHistory.Execute;
    Exit;
  end;

  // handle Space key
  if (Shift=[]) and (Key=VK_Space) and
     ((not IsCommandLineVisible) or (edtCommand.Text='')) then
  begin
    with ActiveFrame do
    begin
      if not IsEmpty then
      begin
        if IsActiveItemValid then
        begin
          if FPS_ISDIR(pnlFile.GetActiveItem^.iMode) then
            CalculateSpace(False);
          SelectFile(GetActiveItem);
        end;
        if gSpaceMovesDown then
          dgPanel.Row:= dgPanel.Row + 1;
        dgPanel.Invalidate;
        MakeSelectedVisible;
      end;
    end;
    Key := 0;
    Exit;
  end;

  if (Shift=[]) and (Key=VK_BACK) and
     ((not IsCommandLineVisible) or (edtCommand.Text='')) then
  begin
    if (not edtCommand.Focused) and (edtCommand.Tag = 0) then
    begin
      with ActiveFrame do
      begin
        pnlFile.cdUpLevel;
        RedrawGrid;
      end;
      Key := 0;
    end;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  KeyPreview:=True;
//  ActiveFrame.SetFocus;
//  DebugLn('Activate');
end;

procedure TfrmMain.FrameEditExit(Sender: TObject);
begin
// handler for both edits
//  DebugLn('On exit');
  KeyPreview:=True;
  (Sender as TEdit).Visible:=False;
end;

procedure TfrmMain.FrameedtSearchExit(Sender: TObject);
begin
  // sometimes must be search panel closed this way
  TFrameFilePanel(TEdit(Sender).Parent.Parent).CloseAltPanel;
  TFrameFilePanel(TEdit(Sender).Parent.Parent).RedrawGrid;
  KeyPreview:=True;
end;

procedure TfrmMain.ShowRenameFileEdit(const sFileName:String);
begin
  KeyPreview:=False;
  With ActiveFrame do
  begin
    edtRename.OnExit:=@FrameEditExit;
    edtRename.Width:=dgPanel.ColWidths[0]+dgPanel.ColWidths[1]-16;
    edtRename.Top:= (dgPanel.CellRect(0,dgPanel.Row).Top-2);
    if gShowIcons then
      edtRename.Left:= gIconsSize + 3
    else
      edtRename.Left:= 2;
    edtRename.Height:=dgpanel.DefaultRowHeight+4;
    edtRename.Hint:=sFileName;
    edtRename.Text:=ExtractFileName(sFileName);
    edtRename.Visible:=True;
    edtRename.SetFocus;
    if gRenameSelOnlyName then
      begin
        {$IFDEF LCLGTK2}
        edtRename.SelStart:=1;
        {$ENDIF}
        edtRename.SelStart:=0;
        edtRename.SelLength:= UTF8Length(edtRename.Text)-UTF8Length(ExtractFileExt(edtRename.Text));
      end
    else
      edtRename.SelectAll;
  end;
end;


procedure TfrmMain.SetNotActFrmByActFrm;
var
  pfr:PFileRecItem;
begin
  with ActiveFrame do
  begin
    pfr:=pnlFile.GetActiveItem;
    if not assigned(pfr) then Exit;
    if FPS_ISDIR(pfr^.iMode) and (not (pfr^.sName = '..')) then
    begin
      NotActiveFrame.pnlFile.ActiveDir := ActiveDir + pfr^.sName;
    end
    else
    begin
      NotActiveFrame.pnlFile.ActiveDir := ActiveDir;
    end;
    NotActiveFrame.LoadPanel;
  end;
end;


procedure TfrmMain.FramedgPanelEnter(Sender: TObject);
begin
  if (Sender is TDrawGrid) then
  begin;
    with TFrameFilePanel(TDrawGrid(Sender).Parent) do
    begin
      PanelSelected:=PanelSelect;
      dgPanelEnter(Sender);
    end;
  end;
end;

procedure TfrmMain.ColumnsMenuClick(Sender: TObject);
var
  frmColumnsSetConf: TfColumnsSetConf;
  FrameFilePanel: TFrameFilePanel;
  Index: Integer;
begin
  FrameFilePanel:= (pmColumnsMenu.Parent as TFrameFilePanel);
  Case (Sender as TMenuItem).Tag of
    1000: //This
          begin
            Application.CreateForm(TfColumnsSetConf, frmColumnsSetConf);
            {EDIT Set}
            frmColumnsSetConf.edtNameofColumnsSet.Text:=ColSet.GetColumnSet(FrameFilePanel.ActiveColm).CurrentColumnsSetName;
            Index:=ColSet.Items.IndexOf(FrameFilePanel.ActiveColm);
            frmColumnsSetConf.lbNrOfColumnsSet.Caption:=IntToStr(1 + Index);
            frmColumnsSetConf.Tag:=Index;
            frmColumnsSetConf.ColumnClass.Clear;
            frmColumnsSetConf.ColumnClass.Load(gIni,FrameFilePanel.ActiveColm);
            {EDIT Set}
            frmColumnsSetConf.ShowModal;

            FreeAndNil(frmColumnsSetConf);
            //TODO: Reload current columns in panels
            ReLoadTabs(nbLeft);
            ReLoadTabs(nbRight);
          end;
    1001: //All columns
          begin
            Actions.cm_Options('15');
            ReLoadTabs(nbLeft);
            ReLoadTabs(nbRight);
          end;

  else
    begin
      FrameFilePanel.ActiveColm:=ColSet.Items[(Sender as TMenuItem).Tag];
      FrameFilePanel.SetColWidths;
      FrameFilePanel.UpdateColumnsView;
//      ActiveFrame.dgPanel.ColCount:=ColSet.GetColumnSet(ActiveFrame.ActiveColm).ColumnsCount;

//      if ColSet.GetColumnSet(ActiveFrame.ActiveColm).ColumnsCount>0 then
 //      for x:=0 to ColSet.GetColumnSet(ActiveFrame.ActiveColm).ColumnsCount-1 do
   //     ActiveFrame.dgPanel.ColWidths[x]:=ColSet.GetColumnSet(ActiveFrame.ActiveColm).GetColumnWidth(x);
    end;

  end;
end;

{ Show context or columns menu on right click }
{ Is called manually from TDrawGridEx.MouseUp }
procedure TfrmMain.framedgPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var  I : Integer; Point:TPoint; MI:TMenuItem;
begin

  if (Sender is TDrawGridEx) and (Button = mbRight) then
    begin
      { If right click on header }
      if (Y < (Sender as TDrawGridEx).GetHeaderHeight) then
        begin

          //Load Columns into menu
          pmColumnsMenu.Items.Clear;
          pmColumnsMenu.Parent:= (Sender as TDrawGridEx).Parent;
          if ColSet.Items.Count>0 then
            begin
              For I:=0 to ColSet.Items.Count-1 do
                begin
                  MI:=TMenuItem.Create(pmColumnsMenu);
                  MI.Tag:=I;
                  MI.Caption:=ColSet.Items[I];
                  MI.OnClick:=@ColumnsMenuClick;
                  pmColumnsMenu.Items.Add(MI);
                end;
            end;

         //-
    	    MI:=TMenuItem.Create(pmColumnsMenu);
    	    MI.Caption:='-';
    	    pmColumnsMenu.Items.Add(MI);
    	   //Configure this custom columns
    	    MI:=TMenuItem.Create(pmColumnsMenu);
    	    MI.Tag:=1000;
    	    MI.Caption:=rsMenuConfigureThisCustomColumn;
    	    MI.OnClick:=@ColumnsMenuClick;
    	    pmColumnsMenu.Items.Add(MI);
    	   //Configure custom columns
    	    MI:=TMenuItem.Create(pmColumnsMenu);
    	    MI.Tag:=1001;
    	    MI.Caption:=rsMenuConfigureCustomColumns;
    	    MI.OnClick:=@ColumnsMenuClick;
    	    pmColumnsMenu.Items.Add(MI);
        
          Point:=(Sender as TDrawGrid).ClientToScreen(Classes.Point(0,0));
          Point.Y:=Point.Y+(Sender as TDrawGridEx).GetHeaderHeight;
          Point.X:=Point.X+X-50;
          pmColumnsMenu.PopUp(Point.X,Point.Y);
        end

      { If right click on file/directory }
      else if (Y < (Sender as TDrawGridEx).GridHeight)
           and ((gMouseSelectionButton<>1) or not gMouseSelectionEnabled) then
        begin
          Actions.cm_ContextMenu('OnMouseClick');
          //actContextMenu.Execute;
        end;
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
begin
  if Sender is TPanel then
    with (Sender as TPanel) do
    begin
      if Name = 'pnlLeft' then
        Actions.cm_NewTab('nbLeft')
      else if Name = 'pnlRight' then
        Actions.cm_NewTab('nbRight');
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

procedure TfrmMain.ShowPathEdit;
begin
  KeyPreview:=False;
  with ActiveFrame do
  begin
    edtPath.OnExit:=@FrameEditExit;
    with lblLPath do
      edtPath.SetBounds(Left, Top, Width, Height);
    edtPath.Text := ActiveDir;
    edtPath.Visible := True;
    edtPath.SetFocus;
  end;
end;

procedure TfrmMain.FramelblLPathClick(Sender: TObject);
begin
//  DebugLn(TControl(Sender).Parent.Parent.ClassName);
  SetActiveFrame(TFrameFilePanel(TControl(Sender).Parent.Parent).PanelSelect);
  Actions.cm_DirHistory('');
  //actDirHistory.Execute;
end;

procedure TfrmMain.FramelblLPathMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
  mbMiddle:
    begin
      SetActiveFrame(TFrameFilePanel(TControl(Sender).Parent.Parent).PanelSelect);
      Actions.cm_DirHotList('');
      //actDirHotList.Execute;
    end;
  mbRight:
    begin
      SetActiveFrame(TFrameFilePanel(TControl(Sender).Parent.Parent).PanelSelect);
      ShowPathEdit;
    end;
  end;
end;

function TfrmMain.FramepnlFileBeforeChangeDirectory(Sender: TObject; const NewDir: String): Boolean;
begin
  Result:= True;
  if Sender is TPage then
    with Sender as TPage do
      if Tag = 1 then
        begin
          CreatePanel(AddPage(TNoteBook((Sender as TPage).Parent)), ActiveFrame.PanelSelect, NewDir);
          ActiveFrame.SetFocus;
          Result:= False;
        end;
end;

procedure TfrmMain.FramepnlFileAfterChangeDirectory(Sender: TObject; const NewDir: String);
var
  ANoteBook : TNoteBook;
  sCaption : String;
begin
  if Sender is TPage then
    begin
      ANoteBook := (Sender as TPage).Parent as TNoteBook;
      if (Sender as TPage).Tag = 0 then // if not locked tab
        begin
          sCaption := GetLastDir(ExcludeTrailingPathDelimiter(NewDir));
          if Boolean(gDirTabOptions and tb_text_length_limit) and (Length(sCaption) > gDirTabLimit) then
            ANoteBook.Page[(Sender as TPage).PageIndex].Caption:= Copy(sCaption, 1, gDirTabLimit) + '...'
          else
            ANoteBook.Page[(Sender as TPage).PageIndex].Caption := sCaption;
        end;
      // update file system watcher directory
      if (ANoteBook.Name = 'nbLeft') and Assigned(LeftFrameWatcher) then
        LeftFrameWatcher.WatchPath:= NewDir
      else if (ANoteBook.Name = 'nbRight') and Assigned(RightFrameWatcher) then
        RightFrameWatcher.WatchPath:= NewDir;
    end;
end;

procedure TfrmMain.SetActiveFrame(panel: TFilePanelSelect);
begin
  PanelSelected:=panel;
  ActiveFrame.SetFocus;
  NotActiveFrame.dgPanelExit(self);
  {$IFDEF unix}
  if gTermWindow and Assigned(Cons) then
    Cons.Terminal.Write_pty(' cd "'+ActiveFrame.ActiveDir+'"'+#13+#10);
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
        DrivesList.Delete(I);
    end;  
  // create drives drop down menu
  CreateDrivesMenu;
  // delete all disk buttons
  dskRight.DeleteAllToolButtons;
  dskLeft.DeleteAllToolButtons;
  // and add new
  CreateDiskPanel(dskLeft);
  CreateDiskPanel(dskRight);
end;

procedure TfrmMain.CreateDrivesMenu;
var
  I, Count : Integer;
  Drive : PDrive;
  miTmp : TMenuItem;
begin
  pmDrivesMenu.Items.Clear;
  btnLeftDrive.Caption := '';
  btnRightDrive.Caption := '';
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
        miTmp.Hint := Path;
        
        if gDriveMenuButton then
          begin
            if Pos(LowerCase(Path), LowerCase(FrameLeft.pnlFile.ActiveDir)) = 1 then
              begin
                btnLeftDrive.Glyph := PixMapManager.GetDriveIcon(Drive, btnLeftDrive.Height - 4, btnLeftDrive.Color);
                btnLeftDrive.Caption := Name;
                btnLeftDrive.Width := btnLeftDrive.Glyph.Width + btnLeftDrive.Canvas.TextWidth(btnLeftDrive.Caption) + 16;
                btnLeftDrive.Tag:= I;
                dskLeft.Tag := I;
              end;
            if Pos(LowerCase(Path), LowerCase(FrameRight.pnlFile.ActiveDir)) = 1 then
              begin
                btnRightDrive.Glyph := PixMapManager.GetDriveIcon(Drive, btnRightDrive.Height - 4, btnRightDrive.Color);
                btnRightDrive.Caption := Name;
                btnRightDrive.Width := btnRightDrive.Glyph.Width + btnRightDrive.Canvas.TextWidth(btnRightDrive.Caption) + 16;
                btnRightDrive.Tag:= I;
                dskRight.Tag := I;
              end;
          end;
        
        // get disk icon
        miTmp.Bitmap := PixMapManager.GetDriveIcon(Drive, 16, clMenu);
        miTmp.RadioItem := True;
        miTmp.AutoCheck := True;
        miTmp.GroupIndex := 1;
        miTmp.OnClick := @DrivesMenuClick;
        pmDrivesMenu.Items.Add(miTmp);
      end;  // with
    end; // for
end;

procedure TfrmMain.DrivesMenuClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    if IsAvailable(Hint) then
       begin
         case pmDrivesMenu.Tag of
         0:
           begin
             FrameLeft.pnlFile.ActiveDir := Hint;
             if gDriveBar2 and gDriveBar1 then
               dskLeft.Buttons[Tag].Down := True;
             dskLeft.Tag := Tag;
             FrameLeft.pnlFile.LoadPanel;
             SetActiveFrame(fpLeft);
             btnLeftDrive.Glyph := PixMapManager.GetDriveIcon(PDrive(DrivesList[Tag]), btnLeftDrive.Height - 4, btnLeftDrive.Color);
             btnLeftDrive.Caption := Caption;
             btnLeftDrive.Width := btnLeftDrive.Glyph.Width + btnLeftDrive.Canvas.TextWidth(btnLeftDrive.Caption) + 16;
             btnLeftDrive.Tag:= Tag;
           end;
         1:
           begin
             FrameRight.pnlFile.ActiveDir := Hint;
             if gDriveBar1 then
               dskRight.Buttons[Tag].Down := True;
             dskRight.Tag := Tag;
             FrameRight.pnlFile.LoadPanel;
             SetActiveFrame(fpRight);
             btnRightDrive.Glyph := PixMapManager.GetDriveIcon(PDrive(DrivesList[Tag]), btnRightDrive.Height - 4, btnRightDrive.Color);
             btnRightDrive.Caption := Caption;
             btnRightDrive.Width := btnRightDrive.Glyph.Width + btnRightDrive.Canvas.TextWidth(btnRightDrive.Caption) + 16;
             btnRightDrive.Tag:= Tag;
           end;
         end;  // case
       end
     else
       begin
         pmDrivesMenu.Items[dskLeft.Tag].Checked := True;
         msgOK(rsMsgDiskNotAvail);
       end;
  end;
end;

procedure TfrmMain.AddSpecialButtons(dskPanel: TKASToolBar);
var
  btnIndex : Integer;
begin
  (*root button*)
  btnIndex := dskPanel.AddButton('/', '/', 'root', '');
  dskPanel.Buttons[btnIndex].GroupIndex := 0;
  (*up button*)
  btnIndex := dskPanel.AddButton('..', '..', 'Up', '');
  dskPanel.Buttons[btnIndex].GroupIndex := 0;
  (*home button*)
  btnIndex := dskPanel.AddButton('~', '~', 'Home', '');
  dskPanel.Buttons[btnIndex].GroupIndex := 0;
end;

procedure TfrmMain.CreateDiskPanel(dskPanel: TKASToolBar);
var
  I, Count : Integer;
  Drive : PDrive;
begin
//dskPanel.InitBounds; // Update information

  dskPanel.FlatButtons := gDriveBarFlat;
  Count := DrivesList.Count - 1;

  for I := 0 to Count do
  begin
  Drive := PDrive(DrivesList.Items[I]);
  with Drive^ do
    begin
      dskPanel.AddButton(Name, Path, Path, '');
      {Set chosen drive}
      if dskPanel.Align = alLeft then
        begin
          if Pos(Path, FrameLeft.pnlFile.ActiveDir) = 1 then
            begin
              dskPanel.Buttons[I].Down := True;
              dskPanel.Tag := I;
            end;
        end
      else
        begin
          if Pos(Path, FrameRight.pnlFile.ActiveDir) = 1 then
            begin
              dskPanel.Buttons[I].Down := True;
              dskPanel.Tag := I;
            end;
        end;
      {/Set chosen drive}

      // get drive icon
      dskPanel.Buttons[I].Glyph := PixMapManager.GetDriveIcon(Drive, dskPanel.ButtonGlyphSize, dskPanel.Buttons[I].Color);
      {Set Buttons Transparent. Is need? }
      dskPanel.Buttons[I].Glyph.Transparent := True;
      dskPanel.Buttons[I].Transparent := True;
      {/Set Buttons Transparent}
      dskPanel.Buttons[I].Layout := blGlyphLeft;
      // save in tag button index
      dskPanel.Buttons[I].Tag:= I;
    end; // with
  end; // for

  if not gDriveMenuButton then  {Add special buttons}
    AddSpecialButtons(dskPanel);
end;

procedure TfrmMain.CreatePanel(AOwner: TWinControl; APanel:TFilePanelSelect; sPath : String);
var
  lblDriveInfo : TLabel;
begin
  case APanel of
  fpLeft:
    lblDriveInfo := lblLeftDriveInfo;
  fpRight:
    lblDriveInfo := lblRightDriveInfo;
  end;
  
  with TFrameFilePanel.Create(AOwner, lblDriveInfo, lblCommandPath, edtCommand) do
  begin
    edtCmdLine:=edtCommand;
    PanelSelect:=APanel;
    Init;
    ReAlign;
    pnlFile.OnBeforeChangeDirectory := @FramepnlFileBeforeChangeDirectory;
    pnlFile.OnAfterChangeDirectory := @FramepnlFileAfterChangeDirectory;
    if not mbDirectoryExists(sPath) then
      sPath:= mbGetCurrentDir;
    pnlFile.ActiveDir := sPath;
    pnlFile.LoadPanel;
    GridVertLine:= gGridVertLine;
    GridHorzLine:= gGridHorzLine;
    UpDatelblInfo;
    dgPanel.Color := gBackColor;
    pnlHeader.Visible := gCurDir;
    pnlFooter.Visible := gStatusBar;
    lblLPath.OnClick:=@FramelblLPathClick;
    lblLPath.OnMouseUp := @FramelblLPathMouseUp;
    edtPath.OnExit:=@FrameEditExit;
    edtRename.OnExit:=@FrameEditExit;
    edtSearch.OnExit:=@FrameedtSearchExit;
    
    dgPanel.OnEnter:=@framedgPanelEnter;
  end;

end;

function TfrmMain.AddPage(ANoteBook: TNoteBook; bSetActive: Boolean):TPage;
var
  x:Integer;
begin
  x:=ANotebook.PageCount;

  ANoteBook.Pages.Add(IntToStr(x));
  if bSetActive then
    ANoteBook.ActivePage:= IntToStr(x);
  Result:=ANoteBook.Page[x];

  ANoteBook.ShowTabs:= ((ANoteBook.PageCount > 1) or Boolean(gDirTabOptions and tb_always_visible)) and gDirectoryTabs;
end;

function TfrmMain.RemovePage(ANoteBook: TNoteBook; iPageIndex:Integer): LongInt;
begin
  Result:= -1;
  if ANoteBook.PageCount > 1 then
  begin
    if ANoteBook.Page[iPageIndex].Tag > 0 then
      case msgYesNoCancel(Format(rsMsgCloseLockedTab, [ANoteBook.Page[iPageIndex].Caption])) of
        mmrNo:
          Exit(1);
        mmrCancel, mmrNone:
          Exit(2);
      end;

    ANoteBook.Pages.Delete(iPageIndex);

    if (nboMultiLine in ANoteBook.Options) and
       ANoteBook.ClientRectNeedsInterfaceUpdate then
    begin
      // The height of the tabs (nr of lines) has changed.
      // Recalculate size of each page.
      ANoteBook.InvalidateClientRectCache(False);
      ANoteBook.ReAlign;
    end;

    Result:= 0;
  end;
  ANoteBook.ShowTabs:= ((ANoteBook.PageCount > 1) or Boolean(gDirTabOptions and tb_always_visible)) and gDirectoryTabs;
end;

procedure TfrmMain.ReLoadTabs(ANoteBook: TNoteBook);
var
  I : Integer;
begin
     DebugLn('FSetCol='+inttostr(colset.Items.Count));
     
     for i:=0 to ANoteBook.PageCount-1 do
        begin
          with TFrameFilePanel(ANoteBook.Page[I].Components[0]) do
           begin
           DebugLn('ActiveColmRET'+Inttostr(I)+'='+ActiveColm);
              if ColSet.Items.IndexOf(ActiveColm)=-1 then
                if ColSet.Items.Count>0 then
                  ActiveColm:=ColSet.Items[0]
                else
                  ActiveColm:='Default';
              Colset.GetColumnSet(ActiveColm).Load(gini,ActiveColm);
              SetColWidths;
              UpdateColumnsView;
           end;
        end;
end;


procedure TfrmMain.LoadTabs(ANoteBook: TNoteBook);
var
  I: Integer;
  sIndex,
  TabsSection: String;
  fpsPanel: TFilePanelSelect;
  sPath, sColumnSet,
  sCaption: String;
  iActiveTab: Integer;
begin
  if ANoteBook.Name = 'nbLeft' then
    begin
      TabsSection:= 'lefttabs';
      fpsPanel:= fpLeft;
    end
  else
    begin
      TabsSection:= 'righttabs';
      fpsPanel:= fpRight;
    end;
  I:= 0;
  sIndex:= '0';
  // create one tab in any way
  sPath:= mbGetCurrentDir; // default path
  sPath:= gIni.ReadString(TabsSection, sIndex + '_path', sPath);
   while True do
    begin
      sCaption:= gIni.ReadString(TabsSection, sIndex + '_caption', '');
      CreatePanel(AddPage(ANoteBook), fpsPanel, sPath);

      ANoteBook.Page[ANoteBook.PageCount - 1].Tag:= gIni.ReadInteger(TabsSection, sIndex + '_options', 0);
      if ANoteBook.Page[ANoteBook.PageCount - 1].Tag = 2 then // if locked tab with directory change
        ANoteBook.Page[ANoteBook.PageCount - 1].Hint:= sPath; // save in hint real path

      if sCaption <> '' then
        if Boolean(gDirTabOptions and tb_text_length_limit) and (Length(sCaption) > gDirTabLimit) then
          ANoteBook.Page[ANoteBook.PageCount - 1].Caption:= Copy(sCaption, 1, gDirTabLimit) + '...'
        else
          ANoteBook.Page[ANoteBook.PageCount - 1].Caption:= sCaption;
          
      sColumnSet:= gIni.ReadString(TabsSection, sIndex + '_columnsset', 'Default');
      with TFrameFilePanel(ANoteBook.Page[ANoteBook.PageCount - 1].Components[0]) do
        begin
          ActiveColm:= sColumnSet;
          SetColWidths;
          UpdateColumnsView;
        end;

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
    if iActiveTab < ANoteBook.PageCount then
      ANoteBook.PageIndex := iActiveTab;
end;

procedure TfrmMain.SaveTabs(ANoteBook: TNoteBook);
var
  I, Count: Integer;
  sIndex,
  TabsSection: String;
  sPath,sColumnSet : String;
begin
  if ANoteBook.Name = 'nbLeft' then
    TabsSection := 'lefttabs'
  else
    TabsSection := 'righttabs';

  // delete tabs section
  gIni.EraseSection(TabsSection);
  // get page count
  Count:= ANoteBook.PageCount - 1;
  for I:= 0 to Count do
    begin
      // get page index in string representation
      sIndex:= IntToStr(I);

      if ANoteBook.Page[I].Tag = 2 then // if locked tab with directory change
        sPath:= ANoteBook.Page[I].Hint // get path from hint
      else
        sPath:= TFrameFilePanel(ANoteBook.Page[I].Components[0]).ActiveDir;
      gIni.WriteString(TabsSection, sIndex + '_path', sPath);
      gIni.WriteString(TabsSection, sIndex + '_caption', ANoteBook.Page[I].Caption);
      gIni.WriteInteger(TabsSection, sIndex + '_options', ANoteBook.Page[I].Tag);

      sColumnSet:= TFrameFilePanel(ANoteBook.Page[I].Components[0]).ActiveColm;
      gIni.WriteString(TabsSection, sIndex + '_columnsset', sColumnSet);
    end;
  gIni.WriteInteger(TabsSection, 'activetab', ANoteBook.PageIndex);
end;

procedure TfrmMain.SetMultilineTabs(ANoteBook: TNoteBook; Multiline: Boolean);
begin
  if (nbcMultiline in ANoteBook.GetCapabilities) and
      // If different then current setting
     (Multiline <> (nboMultiline in ANoteBook.Options)) then
  begin
    if Multiline then
      ANoteBook.Options := ANoteBook.Options + [nboMultiLine]
    else
      ANoteBook.Options := ANoteBook.Options - [nboMultiLine];

    // Workaround: nboMultiline property is currently not updated by LCL.
    // Force update and realign all pages.

    TWSCustomNotebookClass(ANoteBook.WidgetSetClass).UpdateProperties(ANoteBook);

    if ANoteBook.ClientRectNeedsInterfaceUpdate then
    begin
      // Change sizes of pages, because multiline tabs may
      // take up different amount of space than single line.
      ANoteBook.InvalidateClientRectCache(True);
      ANoteBook.ReAlign;
    end;
  end;
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
  if Actions.Execute(Cmd,Param) > -1 then
    Result:= True
  else
    Result:= ExecCmdFork(Format('"%s" %s', [Cmd, Param]));
end;

procedure TfrmMain.ToggleConsole;
begin
{$IFDEF UNIX}
  if gTermWindow then
    begin
      if not Assigned(Cons) then
        begin
          Cons:= TConThread.Create;
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
{$ELSE} // temporarily while console not implemented under Windows
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
          LeftFrameWatcher:= TFileSystemWatcher.Create(nbLeft, FrameLeft.ActiveDir, WatchFilter);
          LeftFrameWatcher.OnWatcherNotifyEvent:= @FramePanelOnWatcherNotifyEvent;
          LeftFrameWatcher.Active:= True;
        end;
      if not Assigned(RightFrameWatcher) then
        begin
          RightFrameWatcher:= TFileSystemWatcher.Create(nbRight, FrameRight.ActiveDir, WatchFilter);
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
var
  I : Integer;
begin
  (* Update list of showed drives *)
  UpdateDiskCount;
  (* Disk Panels *)
  if (dskRight.FlatButtons <> gDriveBarFlat) then  //  if change
    begin
      dskLeft.FlatButtons := gDriveBarFlat;
      dskRight.FlatButtons := gDriveBarFlat;
    end;
  
  if (dskLeft.Visible <> gDriveBar2) or (dskRight.Visible <> gDriveBar1) then  // if change
    begin
      if gDriveBar1 and gDriveBar2 then  // left visible only then right visible
        begin
          CreateDiskPanel(dskLeft);
          dskLeft.Visible := gDriveBar2;
        end
      else
        begin
          dskLeft.Visible := gDriveBar2;
          dskLeft.DeleteAllToolButtons;
        end;

    end;

  if dskRight.Visible <> gDriveBar1 then  // if change
    begin
      if gDriveBar1 then
        begin
          CreateDiskPanel(dskRight);
          dskRight.Visible := gDriveBar1;
        end
      else
        begin
          dskRight.Visible := gDriveBar1;
          dskRight.DeleteAllToolButtons;
        end;
    end;

  pnlSyncSize.Visible := gDriveBar1;
  (*/ Disk Panels *)

  (*Tool Bar*)
  if MainToolBar.FlatButtons <> gToolBarFlat then
    MainToolBar.FlatButtons := gToolBarFlat;
  if MainToolBar.Visible <> gButtonBar then  // if change
    begin
      if gButtonBar then
        begin
          MainToolBar.ChangePath := gpExePath;
          MainToolBar.EnvVar := '%commander_path%';
          MainToolBar.LoadFromFile(gpIniDir + 'default.bar');
          MainToolBar.Visible := gButtonBar;
        end
      else
        begin
          MainToolBar.Visible := gButtonBar;
          MainToolBar.DeleteAllToolButtons;
        end;
    end;
  (*Tool Bar*)

  {Load some options from layout page}
  if btnLeftDrive.Visible <> gDriveMenuButton then
    if gDriveMenuButton then
      begin
        if gDriveBar1 and gDriveBar2 then // left visible only then right visible
          for I := 1 to 3 do
            dskLeft.RemoveButton(dskLeft.ButtonCount - 1);
        if gDriveBar1 then
          for I := 1 to 3 do
            dskRight.RemoveButton(dskRight.ButtonCount - 1);
      end
    else
      begin
        if gDriveBar1 and gDriveBar2 then  // left visible only then right visible
          AddSpecialButtons(dskLeft);
        if gDriveBar1 then
          AddSpecialButtons(dskRight);
      end;
  if btnLeftDrive.Flat <> gInterfaceFlat then
    begin
      btnLeftDrive.Flat := gInterfaceFlat;
      btnLeftRoot.Flat := gInterfaceFlat;
      btnLeftUp.Flat := gInterfaceFlat;
      btnLeftHome.Flat := gInterfaceFlat;
      btnLeftDirectoryHotlist.Flat := gInterfaceFlat;
      btnLeftTargetEqualSource.Flat:= gInterfaceFlat;
      
      btnRightDrive.Flat := gInterfaceFlat;
      btnRightRoot.Flat := gInterfaceFlat;
      btnRightUp.Flat := gInterfaceFlat;
      btnRightHome.Flat := gInterfaceFlat;
      btnRightDirectoryHotlist.Flat := gInterfaceFlat;
      btnRightTargetEqualSource.Flat:= gInterfaceFlat;
    end;

  // Tabs
  nbLeft.ShowTabs := ((nbLeft.PageCount > 1) or Boolean(gDirTabOptions and tb_always_visible)) and gDirectoryTabs;
  nbRight.ShowTabs := ((nbRight.PageCount > 1) or Boolean(gDirTabOptions and tb_always_visible)) and gDirectoryTabs;

  SetMultilineTabs(nbLeft, Boolean(gDirTabOptions and tb_multiple_lines));
  SetMultilineTabs(nbRight, Boolean(gDirTabOptions and tb_multiple_lines));

  for I := 0 to nbLeft.PageCount - 1 do  //  change on all tabs
    with nbLeft.Page[I].Controls[0] as TFrameFilePanel do
    begin
      pnlHeader.Visible := gCurDir;  // Current directory
      pnlFooter.Visible := gStatusBar;  // Status bar
      GridVertLine:= gGridVertLine;
      GridHorzLine:= gGridHorzLine;
      if gShowIcons then
        pnlFile.FileList.UpdateFileInformation(pnlFile.PanelMode);
      pnlFile.UpdatePanel;
    end;

  for I := 0 to nbRight.PageCount - 1 do  //  change on all tabs
    with nbRight.Page[I].Controls[0] as TFrameFilePanel do
    begin
      pnlHeader.Visible := gCurDir;  // Current directory
      pnlFooter.Visible := gStatusBar;  // Status bar
      GridVertLine:= gGridVertLine;
      GridHorzLine:= gGridHorzLine;
      if gShowIcons then
        pnlFile.FileList.UpdateFileInformation(pnlFile.PanelMode);
      pnlFile.UpdatePanel;
    end;

  for I := 0 to pnlKeys.ControlCount - 1 do  // function keys
    if pnlKeys.Controls[I] is TSpeedButton then
      (pnlKeys.Controls[I] as TSpeedButton).Flat := gInterfaceFlat;
        
  btnLeftDrive.Visible := gDriveMenuButton;
  btnLeftRoot.Visible := gDriveMenuButton;
  btnLeftUp.Visible := gDriveMenuButton;
  btnLeftHome.Visible := gDriveMenuButton;

  btnRightDrive.Visible := gDriveMenuButton;
  btnRightRoot.Visible := gDriveMenuButton;
  btnRightUp.Visible := gDriveMenuButton;
  btnRightHome.Visible := gDriveMenuButton;

  pnlCommand.Visible := gCmdLine;
  edtCommand.Tag := 0;
  pnlKeys.Visible := gKeyButtons;
  LogSplitter.Visible := gLogWindow;
  seLogWindow.Visible := gLogWindow;
  ToggleConsole;
  ToggleFileSystemWatcher;
end;

procedure TfrmMain.edtCommandKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not edtCommand.DroppedDown and ((Key=VK_UP) or (Key=VK_DOWN)) then
    begin
      ActiveFrame.SetFocus;
      Key:= 0;
    end
  else if (edtCommand.DroppedDown) and (Key in [VK_RETURN, VK_ESCAPE]) then
    begin
      edtCommand.DroppedDown := False;
      Key := 0;
    end
  else if Key = VK_ESCAPE then
    begin
      edtCommand.Text := '';
      Key := 0;
    end
  else
    edtCommand.Tag:= 1;
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

procedure TfrmMain.FramePanelOnWatcherNotifyEvent(Sender: TObject; NotifyEvent: TWatchFilter);
var
  sDrive,
  sWatchDirsExclude: String;
  FrameFilePanel: TFrameFilePanel;
begin
  // if not active and refresh only in foreground then exit
  if (not Focused) and (watch_only_foreground in gWatchDirs) then Exit;
  if not (Sender is TNotebook) then Exit;
  with Sender as TNotebook do
    FrameFilePanel:= TFrameFilePanel(Page[PageIndex].Components[0]);
  // if current path in exclude list then exit
  if gWatchDirsExclude <> '' then
    begin
      sWatchDirsExclude:= gWatchDirsExclude;
      repeat
        sDrive:= Copy2SymbDel(sWatchDirsExclude, ';');
        if Pos(sDrive, FrameFilePanel.ActiveDir) = 1 then Exit;
      until sWatchDirsExclude = '';
    end;
  FrameFilePanel.RefreshPanel((watch_total_number_files in gWatchDirs), (watch_free_disk_space in gWatchDirs));
end;

procedure TfrmMain.tmHALTimer(Sender: TObject);
begin
{$IFDEF UNIX}
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
      with ActiveFrame.pnlFile do
      begin
        sDir:= mbGetCurrentDir;
        ActiveDir:= sDir;
        DebugLn(sDir);
{$IFDEF UNIX}
        if gTermWindow and Assigned(Cons) then
          Cons.Terminal.Write_pty(' cd "'+sDir+'"'+#13#10);
{$ENDIF}
      end;
    end;
  end
  else
  begin
    if edtCommand.Items.IndexOf(sCmd)=-1 then
      edtCommand.Items.Insert(0,sCmd);

{$IFDEF unix}
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
    with FrameLeft do
    begin
      for x:=0 to ColSet.GetColumnSet(ActiveColm).ColumnsCount - 1 do
        ColSet.GetColumnSet(ActiveColm).SetColumnWidth(x, dgPanel.ColWidths[x]);
      ColSet.GetColumnSet(ActiveColm).Save(gIni);
    end;
  
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

initialization
 {$I fmain.lrs}
end.
