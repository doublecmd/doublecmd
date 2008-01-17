{
Double Commander
----------------------------
Licence  : GNU GPL v 2.0
Copyright (C) 2006-2008 Alexander Koblov (Alexx2000@mail.ru)

Main Dialog window

contributors:

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

}

unit fMain;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  Graphics, Forms, Menus, Controls, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls,ActnList,Buttons,
  SysUtils, Classes,  {uFilePanel,} framePanel, {FileCtrl,} Grids,
  KASToolBar, IniFiles;

const
  cHistoryFile='cmdhistory.txt';

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actExtractFiles: TAction;
    actAddPathToCmdLine: TAction;
    actFocusCmdLine: TAction;
    actContextMenu: TAction;
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
    dskLeft: TKAStoolBar;
    dskRight: TKAStoolBar;
    lblRightDriveInfo: TLabel;
    lblLeftDriveInfo: TLabel;
    MainToolBar: TKASToolBar;
    MenuItem1: TMenuItem;
    mnuExtractFiles: TMenuItem;
    nbLeft: TNotebook;
    nbRight: TNotebook;
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
    tbDelete: TMenuItem;
    tbEdit: TMenuItem;
    mnuMain: TMainMenu;
    pnlNotebooks: TPanel;
    pnlSyncSize: TPanel;
    pnlCommand: TPanel;
    lblCommandPath: TLabel;
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
    edtCommand: TComboBox;
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
    MainSplitter: TSplitter;
    procedure actAddPathToCmdLineExecute(Sender: TObject);
    procedure actContextMenuExecute(Sender: TObject);
    procedure actExtractFilesExecute(Sender: TObject);
    procedure actFocusCmdLineExecute(Sender: TObject);
    procedure actLeftOpenDrivesExecute(Sender: TObject);
    procedure actOpenVFSListExecute(Sender: TObject);
    procedure actPackFilesExecute(Sender: TObject);
    procedure actRightOpenDrivesExecute(Sender: TObject);
    procedure actTransferLeftExecute(Sender: TObject);
    procedure actTransferRightExecute(Sender: TObject);
    procedure btnLeftClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure dskRightChangeLineCount(AddSize: Integer);
    procedure dskToolButtonClick(Sender: TObject; NumberOfButton: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblDriveInfoDblClick(Sender: TObject);
    function MainToolBarLoadButtonGlyph(sIconFileName: String;
      iIconSize: Integer; clBackColor: TColor): TBitmap;
    procedure MainToolBarMouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainToolBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
    procedure actExitExecute(Sender: TObject);
    procedure actNewTabExecute(Sender: TObject);
    procedure actRemoveTabExecute(Sender: TObject);
    procedure frmMainClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frmMainKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure frmMainShow(Sender: TObject);
    procedure mnuHelpClick(Sender: TObject);
    procedure nbPageChanged(Sender: TObject);
    procedure NotebookCloseTabClicked(Sender: TObject);
    procedure pnlKeysResize(Sender: TObject);
    procedure actViewExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actRenameExecute(Sender: TObject);
    procedure actMakeDirExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actShowSysFilesExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure actCompareContentsExecute(Sender: TObject);
    procedure actShowMenuExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actMarkInvertExecute(Sender: TObject);
    procedure actMarkMarkAllExecute(Sender: TObject);
    procedure actMarkUnmarkAllExecute(Sender: TObject);
    procedure actDirHotListExecute(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    procedure actDelete2Execute(Sender: TObject);
    procedure edtCommandKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actMarkPlusExecute(Sender: TObject);
    procedure actMarkMinusExecute(Sender: TObject);
    procedure actSymLinkExecute(Sender: TObject);
    procedure actHardLinkExecute(Sender: TObject);
    procedure actReverseOrderExecute(Sender: TObject);
    procedure actSortByNameExecute(Sender: TObject);
    procedure actSortByExtExecute(Sender: TObject);
    procedure actSortBySizeExecute(Sender: TObject);
    procedure actSortByDateExecute(Sender: TObject);
    procedure actSortByAttrExecute(Sender: TObject);
    procedure actMultiRenameExecute(Sender: TObject);
    procedure actCopySamePanelExecute(Sender: TObject);
    procedure actRenameOnlyExecute(Sender: TObject);
    procedure actEditNewExecute(Sender: TObject);
    procedure actDirHistoryExecute(Sender: TObject);
    procedure actShowCmdLineHistoryExecute(Sender: TObject);
    procedure actRunTermExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FrameEditExit(Sender: TObject);
    procedure FrameedtSearchExit(Sender: TObject);

    procedure actCalculateSpaceExecute(Sender: TObject);
    procedure actFilePropertiesExecute(Sender: TObject);
    procedure FramedgPanelEnter(Sender: TObject);
    procedure framedgPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowPathEdit;
    procedure FramelblLPathClick(Sender: TObject);
    procedure FramelblLPathMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FramepnlFileChangeDirectory(Sender: TObject; const NewDir : String);
    procedure edtCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actFileLinkerExecute(Sender: TObject);
    procedure actFileSpliterExecute(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
  private
    { Private declarations }
    PanelSelected:TFilePanelSelect;
    bAltPress:Boolean;
    DrivesList : TList;
    IsPanelsCreated : Boolean;
    
    function ExecuteCommandFromEdit(sCmd:String):Boolean;
    procedure AddSpecialButtons(dskPanel: TKASToolBar);
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
    function AddPage(ANoteBook:TNoteBook):TPage;
    procedure RemovePage(ANoteBook:TNoteBook; iPageIndex:Integer);
    procedure LoadTabs(ANoteBook:TNoteBook);
    procedure SaveTabs(ANoteBook:TNoteBook);
    function ExecCmd(Cmd : String) : Boolean;
    procedure UpdateWindowView;
    procedure SaveShortCuts;
    procedure LoadShortCuts;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uTypes, fAbout, uGlobs, uLng, fOptions,{ fViewer,}fconfigtoolbar,
  uCopyThread, uFileList, uDeleteThread, uVFSUtil, uWCXModule, uVFSTypes,
  fMkDir, fCopyDlg, fCompareFiles,{ fEditor,} fMoveDlg, uMoveThread, uShowMsg,
  fFindDlg, uSpaceThread, fHotDir, fSymLink, fHardLink, uDCUtils,
  fMultiRename, uShowForm, uGlobsPaths, fFileOpDlg, fMsg, fPackDlg, fExtractDlg,
  fLinker, fSplitter, uFileProcs, lclType, LCLProc, uOSUtils, uOSForms, uPixMapManager;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  inherited;
  SetMyWndProc(Handle);
  Application.OnException := @AppException;


  if FileExists(gpIniDir+cHistoryFile) then
    edtCommand.Items.LoadFromFile(gpIniDir+cHistoryFile);
//  DebugLn('frmMain.FormCreate Done');
  IsPanelsCreated := False;
end;


(* Pack files in archive *)
procedure TfrmMain.actPackFilesExecute(Sender: TObject);
var
  fl : TFileList;
begin
  if not IsBlocked then
    begin
      fl:=TFileList.Create;
      with ActiveFrame do
        begin
          SelectFileIfNoSelected(GetActiveItem);
          CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir);

          fl.CurrentDirectory := ActiveDir;
        end;
      try
        ShowPackDlg(NotActiveFrame.pnlFile.VFS, fl, NotActiveFrame.ActiveDir);
      finally
        frameLeft.RefreshPanel;
        frameRight.RefreshPanel;
      end;
    end;  // IsBlocked
end;

procedure TfrmMain.actRightOpenDrivesExecute(Sender: TObject);
var
  p : TPoint;
begin
  pmDrivesMenu.Tag := 1;  // indicate that is right panel menu
  p := Classes.Point(btnRightDrive.Left,btnRightDrive.Height);
  p := pnlRightTools.ClientToScreen(p);
  pmDrivesMenu.Items[dskRight.Tag].Checked := True;
  pmDrivesMenu.PopUp(p.x, p.y);
end;

procedure TfrmMain.actTransferLeftExecute(Sender: TObject);
begin
  if (PanelSelected = fpRight) then
    SetNotActFrmByActFrm;
end;

procedure TfrmMain.actTransferRightExecute(Sender: TObject);
begin
  if (PanelSelected = fpLeft) then
    SetNotActFrmByActFrm;
end;

procedure TfrmMain.btnLeftClick(Sender: TObject);
begin
  with Sender as TSpeedButton do
  begin
    if Caption = '/' then
      FrameLeft.pnlFile.ActiveDir := ExtractFileDrive(FrameLeft.pnlFile.ActiveDir);
    if Caption = '..' then
      FrameLeft.pnlFile.cdUpLevel;
    if Caption = '~' then
      FrameLeft.pnlFile.ActiveDir := GetHomeDir;
  end;
  FrameLeft.pnlFile.LoadPanel;

  SetActiveFrame(fpLeft);
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

procedure TfrmMain.actExtractFilesExecute(Sender: TObject);
var
  fl : TFileList;
begin
  if not IsBlocked then
    begin
      fl:=TFileList.Create;
      with ActiveFrame do
        begin
          SelectFileIfNoSelected(GetActiveItem);
          CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir);

          fl.CurrentDirectory := ActiveDir;
        end;
      try
        ShowExtractDlg(ActiveFrame, fl, NotActiveFrame.ActiveDir);
      finally
        frameLeft.RefreshPanel;
        frameRight.RefreshPanel;
      end;
    end;  // IsBlocked
end;

procedure TfrmMain.actFocusCmdLineExecute(Sender: TObject);
begin
  edtCommand.SetFocus;
end;

procedure TfrmMain.actAddPathToCmdLineExecute(Sender: TObject);
begin
  with ActiveFrame do
    begin
      edtCmdLine.Text := edtCmdLine.Text + (pnlFile.ActiveDir);
    end;
end;

procedure TfrmMain.actContextMenuExecute(Sender: TObject);
var
  fl : TFileList;
begin
  with ActiveFrame do
    begin
      if pnlFile.PanelMode in [pmArchive, pmVFS] then
        begin
          msgError(rsMsgErrNotSupported);
          UnMarkAll;
          Exit;
        end;
        
      fl := TFileList.Create;
      SelectFileIfNoSelected(GetActiveItem);
      CopyListSelectedExpandNames(pnlFile.FileList, fl, ActiveDir, False);
    end;
  ShowContextMenu(Handle, fl, Mouse.CursorPos.x, Mouse.CursorPos.y);
  ActiveFrame.UnMarkAll;
end;

procedure TfrmMain.actLeftOpenDrivesExecute(Sender: TObject);
var
  p : TPoint;
begin
  pmDrivesMenu.Tag := 0;  // indicate that is left panel menu
  p := Classes.Point(btnLeftDrive.Left,btnLeftDrive.Height);
  p := pnlLeftTools.ClientToScreen(p);
  pmDrivesMenu.Items[dskLeft.Tag].Checked := True;
  pmDrivesMenu.PopUp(p.x, p.y);
end;

procedure TfrmMain.actOpenVFSListExecute(Sender: TObject);
begin
  ActiveFrame.pnlFile.LoadVFSListInPanel;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DebugLn('frmMain.Destroy');
  edtCommand.Items.SaveToFile(gpIniDir+cHistoryFile);
  {*Tool Bar*}
  MainToolBar.SaveToFile(gpIniDir + 'default.bar');
  {*Tool Bar*}
end;

procedure TfrmMain.lblDriveInfoDblClick(Sender: TObject);
begin
  if (Sender as TLabel).Name = 'lblRightDriveInfo' then
      SetActiveFrame(fpRight)
  else if (Sender as TLabel).Name = 'lblLeftDriveInfo' then
      SetActiveFrame(fpLeft);
  actDirHotList.Execute;
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
   if msgYesNo('You really want delete button?') then
      begin
         MainToolBar.RemoveButton (pmToolBar.Tag);
         MainToolBar.SaveToFile(gpIniDir + 'default.bar');
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
         FrameFilePanel.pnlFile.ActiveDir := dskPanel.Commands[NumberOfButton];
         dskPanel.Tag := NumberOfButton;
         if gDriveMenuButton then  //  if show drive button
           begin
             btnDrive.Glyph := PixMapManager.GetDriveIcon(PDrive(DrivesList[NumberOfButton]), btnDrive.Height - 4, btnDrive.Color);
             btnDrive.Caption := dskRight.Buttons[NumberOfButton].Caption;
           end;
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
  ExecCmd(MainToolBar.Commands[NumberOfButton]);
  DebugLn(MainToolBar.Commands[NumberOfButton]);
end;


procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.actNewTabExecute(Sender: TObject);
begin
  case PanelSelected of
  fpLeft:
     CreatePanel(AddPage(nbLeft), fpLeft, ActiveFrame.ActiveDir);
  fpRight:
     CreatePanel(AddPage(nbRight), fpRight, ActiveFrame.ActiveDir);
  end;
end;

procedure TfrmMain.actRemoveTabExecute(Sender: TObject);
begin
  case PanelSelected of
  fpLeft:
     RemovePage(nbLeft, nbLeft.PageIndex);
  fpRight:
     RemovePage(nbRight, nbRight.PageIndex);
  end;
end;

procedure TfrmMain.frmMainClose(Sender: TObject; var CloseAction: TCloseAction);
var
  x:Integer;
begin
  for x:=0 to 4 do
    gColumnSize[x]:=FrameLeft.dgPanel.ColWidths[x];
  
  SaveGlobs; // must be first
  (* Save all tabs *)
  SaveTabs(nbLeft);
  SaveTabs(nbRight);
  
  gIni.WriteInteger('Configuration', 'Main.Left', Left);
  gIni.WriteInteger('Configuration', 'Main.Top', Top);
  gIni.WriteInteger('Configuration', 'Main.Width', Width);
  gIni.WriteInteger('Configuration', 'Main.Height', Height);
  gIni.WriteBool('Configuration', 'maximized', (WindowState = wsMaximized));
end;

procedure TfrmMain.frmMainKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//
end;

procedure TfrmMain.frmMainShow(Sender: TObject);
var
  I : Integer;
begin
   DebugLn('frmMainShow');
  (* If panels already created then refresh their and exit *)
  if IsPanelsCreated then
    begin
      FrameLeft.RefreshPanel;
      FrameRight.RefreshPanel;
      Exit;
    end;


  Left := gIni.ReadInteger('Configuration', 'Main.Left', Left);
  Top := gIni.ReadInteger('Configuration', 'Main.Top', Top);
  Width :=  gIni.ReadInteger('Configuration', 'Main.Width', Width);
  Height :=  gIni.ReadInteger('Configuration', 'Main.Height', Height);
  if gIni.ReadBool('Configuration', 'maximized', True) then
    Self.WindowState := wsMaximized;

  
  LoadTabs(nbLeft);
  LoadTabs(nbRight);
  
  nbLeft.Options:=[nboShowCloseButtons];
  nbRight.Options:=[nboShowCloseButtons];
  actShowSysFiles.Checked:=uGlobs.gShowSystemFiles;
  
  PanelSelected:=fpLeft;

  SetActiveFrame(fpLeft);

  pnlNotebooks.Width:=Width div 2;

  //DebugLN('dskLeft.Width == ' + IntToStr(dskLeft.Width));
  //DebugLN('dskRight.Width == ' + IntToStr(dskRight.Width));
  DrivesList := GetAllDrives;
  
  CreateDrivesMenu;
  
  (*Create Disk Panels*)
  dskLeft.Visible :=  gDriveBar2;
  if gDriveBar2 then
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
	  if FileExists(gpIniDir + 'default.bar') then
        MainToolBar.LoadFromFile(gpIniDir + 'default.bar')	  
	  else
        MainToolBar.LoadFromFile(gpCfgDir + 'default.bar');
    end;
  (*Tool Bar*)
  
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

  btnRightDrive.Visible := gDriveMenuButton;
  btnRightDrive.Flat := gInterfaceFlat;
  btnRightRoot.Visible := gDriveMenuButton;
  btnRightRoot.Flat := gInterfaceFlat;
  btnRightUp.Visible := gDriveMenuButton;
  btnRightUp.Flat := gInterfaceFlat;
  btnRightHome.Visible := gDriveMenuButton;;
  btnRightHome.Flat := gInterfaceFlat;

  pnlCommand.Visible := gCmdLine;
  pnlKeys.Visible := gKeyButtons;

  IsPanelsCreated := True;
end;

procedure TfrmMain.mnuHelpClick(Sender: TObject);
begin

end;

procedure TfrmMain.nbPageChanged(Sender: TObject);
begin
  with Sender as TNoteBook do
  begin
    if (Name = 'nbLeft') and (FrameLeft <> nil) then
      FrameLeft.pnlFile.UpdatePrompt;
    if (Name = 'nbRight') and (FrameRight <> nil) then
      FrameRight.pnlFile.UpdatePrompt;
  end;
end;

procedure TfrmMain.NoteBookCloseTabClicked(Sender: TObject);
begin
  With (Sender As TPage) do
  begin
    RemovePage(Parent as TNoteBook, PageIndex);
  end;
end;

procedure TfrmMain.pnlKeysResize(Sender: TObject);
var
  iWidth:Integer;
begin
  iWidth:=pnlKeys.Width div 8;
  btnF3.Left:=1;
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

procedure TfrmMain.actViewExecute(Sender: TObject);
var
  sl:TStringList;
  i:Integer;
  fr:PFileRecItem;
  VFSFileList : TFileList;
  sFileName,
  sFilePath,
  sTempDir : String;
begin
  with ActiveFrame do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    sl:=TStringList.Create;
    try
      for i:=0 to pnlFile.FileList.Count-1 do
      begin
        fr:=pnlFile.GetFileItemPtr(i);
        if fr^.bSelected and not (FPS_ISDIR(fr^.iMode)) then
        begin
          (* If in Virtual File System *)
          if pnlFile.PanelMode in [pmArchive, pmVFS] then
            begin
              VFSFileList := TFileList.Create;
              VFSFileList.CurrentDirectory := ActiveDir;
              sFileName := ActiveDir + fr^.sName;
              New(fr);
              fr^.sName := sFileName;
              VFSFileList.AddItem(fr);
              sTempDir := GetTempDir;
              {if }pnlFile.VFS.VFSmodule.VFSCopyOut(VFSFileList, sTempDir, 0);{ then}
                begin
                 sl.Add(sTempDir + ExtractDirLevel(ActiveDir, fr^.sName));
                 ShowViewerByGlobList(sl, True);
                 Dispose(fr);
                 Exit;
                end;
            end;
          sFileName := fr^.sName;
          sFilePath := ActiveDir;
          sl.Add(GetSplitFileName(sFileName, sFilePath));
          DebugLn('View.Add: ', sFilePath + sFileName);
        end;
      end;
      if sl.Count>0 then
        ShowViewerByGlobList(sl)
      else
        begin
          fr := pnlFile.GetActiveItem;
          if FPS_ISDIR(fr^.iMode) then
            begin
              Screen.Cursor:=crHourGlass;
              try
                pnlFile.ChooseFile(fr);
                UpDatelblInfo;
              finally
                dgPanel.Invalidate;
                Screen.Cursor:=crDefault;
              end;
            end
        end;
    finally
      if pnlFile.PanelMode = pmDirectory then
        FreeAndNil(sl);
      ActiveFrame.UnMarkAll;
    end;
  end;
end;

procedure TfrmMain.actEditExecute(Sender: TObject);
var
//  sl:TStringList;
  i:Integer;
  fr:PFileRecItem;
  sFileName,
  sFilePath : String;
begin
  with ActiveFrame do
  begin
    if pnlFile.PanelMode in [pmArchive, pmVFS] then
      begin
        msgError(rsMsgErrNotSupported);
        UnMarkAll;
        Exit;
      end;
    SelectFileIfNoSelected(GetActiveItem);
    try
    // in this time we only one file process
      for i:=0 to pnlFile.FileList.Count-1 do
      begin
      fr:=pnlFile.GetFileItemPtr(i);
      if fr^.bSelected and not (FPS_ISDIR(fr^.iMode)) then
        begin
          sFileName := fr^.sName;
          sFilePath := ActiveDir;
          ShowEditorByGlob(GetSplitFileName(sFileName, sFilePath));
          Break;
        end;
      end;
    finally
      ActiveFrame.UnMarkAll;
    end;
  end;
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  CopyFile(NotActiveFrame.ActiveDir);
end;

procedure TfrmMain.actRenameExecute(Sender: TObject);
begin
  RenameFile(NotActiveFrame.ActiveDir);
end;

procedure TfrmMain.actMakeDirExecute(Sender: TObject);
var
  sPath:String;
begin
  with ActiveFrame do
  begin
    try
      if  pnlFile.PanelMode in [pmArchive, pmVFS] then // if in VFS
        begin
          if not (VFS_CAPS_MKDIR in pnlFile.VFS.VFSModule.VFSCaps) then
            begin
              msgOK(rsMsgErrNotSupported);
              Exit;
            end;
        end; // in VFS
        
      sPath:=ActiveDir;
      if not ShowMkDir(sPath) then Exit;
      if (sPath='') then Exit;
      
      { Create directory in VFS }
        if  ActiveFrame.pnlFile.PanelMode in [pmArchive, pmVFS] then
        begin
          DebugLN('+++ Create directory in VFS +++');
          ActiveFrame.pnlFile.VFS.VFSmodule.VFSMkDir(ActiveDir + sPath);
          ActiveFrame.RefreshPanel;
          Exit;
        end;

      { Create directory }

      if (DirectoryExists(ActiveDir+sPath)) then
      begin
        msgError(Format(rsMsgErrDirExists,[ActiveDir+sPath]));
        pnlFile.LastActive:=sPath;
        pnlFile.LoadPanel;
      end
      else
      begin
        if not ForceDirectory(ActiveDir+sPath) then
          msgError(Format(rsMsgErrForceDir,[ActiveDir+sPath]))
        else
        begin
          pnlFile.LastActive:=sPath;
          pnlFile.LoadPanel;
        end;
      end;
    finally
      ActiveFrame.SetFocus;
    end;
  end;
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
var
  fl:TFileList;
  DT : TDeleteThread;
begin
  with ActiveFrame do
  begin
    if  pnlFile.PanelMode in [pmArchive, pmVFS] then // if in VFS
      begin
        if not (VFS_CAPS_DELETE in pnlFile.VFS.VFSModule.VFSCaps) then
          begin
            msgOK(rsMsgErrNotSupported);
            Exit;
          end;
      end; // in VFS
      
    SelectFileIfNoSelected(GetActiveItem);
  end;
  
  case msgYesNoCancel(GetFileDlgStr(rsMsgDelSel,rsMsgDelFlDr)) of
    mmrNo:
      begin
        ActiveFrame.UnMarkAll;
        Exit;
      end;
    mmrCancel:
      begin
        Exit;
      end;
  end;

  fl:=TFileList.Create; // free at Thread end by thread
  try
    CopyListSelectedExpandNames(ActiveFrame.pnlFile.FileList,fl,ActiveFrame.ActiveDir);
    
    
    (* Delete files from VFS *)
    if  ActiveFrame.pnlFile.PanelMode in [pmArchive, pmVFS] then // if in VFS
      begin
        DebugLN('+++ Delete files +++');
        ActiveFrame.pnlFile.VFS.VFSmodule.VFSDelete(fl);
        Exit;
      end;
      
    (* Delete files *)
    begin
     if not Assigned(frmFileOp) then
       frmFileOp:= TfrmFileOp.Create(Application);
     try
       DT := TDeleteThread.Create(fl);
       DT.FFileOpDlg := frmFileOp;
       DT.sDstPath:=NotActiveFrame.ActiveDir;
       //DT.sDstMask:=sDstMaskTemp;
       frmFileOp.Thread := TThread(DT);
       frmFileOp.Show;
       DT.Resume;
     except
       DT.Free;
     end;
    end;

  except
    FreeAndNil(frmFileOp);
  end;
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

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TfrmMain.actShowSysFilesExecute(Sender: TObject);
begin
  uGlobs.gShowSystemFiles:=not uGlobs.gShowSystemFiles;
  actShowSysFiles.Checked:=uGlobs.gShowSystemFiles;
// we don't want any not visited files selected
  if not uGlobs.gShowSystemFiles then
  begin
    frameLeft.pnlFile.MarkAllFiles(False);
    frameRight.pnlFile.MarkAllFiles(False);
  end;
//repaint both panels
  FrameLeft.pnlFile.UpdatePanel;
  FrameRight.pnlFile.UpdatePanel;
end;

function TfrmMain.HandleActionHotKeys(var Key: Word; Shift: TShiftState):Boolean; // handled
var
  pfri : PFileRecItem;
begin
  Result:=True;
  if Shift=[] then
  begin
    case Key of
     VK_F8, VK_DELETE:
       begin
{         Key:=0;
         ActiveFrame.ClearCmdLine; // hack delete key
}
         if (not edtCommand.Focused) or (Key = VK_F8) then
         begin
           actDelete.Execute;
           Exit;
         end;
       end;

     VK_APPS:
       begin
         actContextMenu.Execute;
         Exit;
       end;
    end; // case
  end; // Shift=[]

  if (Key=VK_Return) or (Key=VK_SELECT) then
  begin
    Key:=0;
    with ActiveFrame do
    begin
      if Shift=[] then
      begin
        if (edtCommand.Text='') then
        begin
          Screen.Cursor:=crHourGlass;
          try
            pnlFile.ChooseFile(pnlFile.GetActiveItem);
            UpDatelblInfo;
          finally
            dgPanel.Invalidate;
            Screen.Cursor:=crDefault;
          end;
          Exit;
        end
        else
        begin
          // execute command line
          ChDir(ActiveDir);
          ExecuteCommandFromEdit(edtCommand.Text);
          ClearCmdLine;
          RefreshPanel;
          Exit;
        end;
      end; //Shift=[]

      // execute active file in terminal (Shift+Enter)
      if Shift=[ssShift] then
      begin
        Chdir(ActiveDir);
        pnlFile.ExecuteFile(pnlFile.GetActiveItem^.sName, True);
        Exit;
      end;
      // ctrl enter
      if Shift=[ssCtrl] then
      begin
        edtCmdLine.Text:=edtCmdLine.Text+pnlFile.GetActiveItem^.sName+' ';
        Exit;
      end;
      // ctrl+shift+enter
      if Shift=[ssShift,ssCtrl] then
      begin
        if (pnlFile.GetActiveItem^.sName = '..') then
        begin
          edtCmdLine.Text:=edtCmdLine.Text+(pnlFile.ActiveDir) + ' ';
        end
        else
        begin
          edtCmdLine.Text:=edtCmdLine.Text+(pnlFile.ActiveDir) + pnlFile.GetActiveItem^.sName+' ';
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

    if (Key=VK_X) then
    begin
      if not edtCommand.Focused then
      begin
        actRunTerm.Execute;
        Exit;
      end;
    end;
  end; // Shift=[ssCtrl]
  
  // not handled
  Result:=False;
end;

procedure TfrmMain.actOptionsExecute(Sender: TObject);
begin
  inherited;
  with TfrmOptions.Create(Application) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;  
  end;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
//  DebugLn('KeyPress:',Key);
  if Key=#27 then
    ActiveFrame.ClearCmdLine;
  if (ord(key)>31) and (ord(key)<255) then
  begin
    if ((Key='-') or (Key='*') or (Key='+') or (Key=' '))and (Trim(edtCommand.Text)='') then Exit;
    if not edtCommand.Focused then
    begin
      edtCommand.SetFocus; // handle first char of command line specially
      with ActiveFrame do
        begin
          edtCommand.Text:=edtCmdLine.Text+Key;
          edtCommand.SelStart := Length(edtCommand.Text) + 1;
        end;
      Key:=#0;
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

procedure TfrmMain.actCompareContentsExecute(Sender: TObject);
var
  sFile1, sFile2:String;
begin
  inherited;

  with frameLeft do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    sFile1:=ActiveDir+pnlFile.GetActiveItem^.sName;
  end; // frameLeft;

  with frameRight do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    sFile2:=ActiveDir+pnlFile.GetActiveItem^.sName;
  end; // frameright;
  if gUseExtDiff then
    begin
      ExecCmdFork(Format('"%s" "%s" "%s"', [gExtDiff, sFile1, sFile2]));
      Exit;
    end;
  try
    ShowCmpFiles(sFile1, sFile2);
  finally
    frameLeft.UnMarkAll;
    FrameRight.UnMarkAll;
  end;
end;

procedure TfrmMain.AppException(Sender: TObject; E: Exception);
begin
  WriteLN(stdErr,'Exception:',E.Message);
  WriteLN(stdErr,'Func:',BackTraceStrFunc(get_caller_frame(get_frame)));
  Dump_Stack(StdErr, get_caller_frame(get_frame));
end;

procedure TfrmMain.actShowMenuExecute(Sender: TObject);
begin
  //gtk_menu_item_select(PGtkMenuItem(mnuFiles.Handle));
end;

procedure TfrmMain.actRefreshExecute(Sender: TObject);
begin
  inherited;
  ActiveFrame.RefreshPanel;
end;

Function TfrmMain.GetFileDlgStr(sLngOne, sLngMulti:String):String;
var
  iSelCnt:Integer;
begin
  with ActiveFrame do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    iSelCnt:=pnlFile.GetSelectedCount;
    if iSelCnt=0 then Abort;
    if iSelCnt >1 then
      Result:=Format(sLngMulti, [iSelCnt])
    else
      Result:=Format(sLngOne, [pnlFile.GetActiveItem^.sName])
  end;
end;

procedure TfrmMain.actMarkInvertExecute(Sender: TObject);
begin
  inherited;
  ActiveFrame.InvertAllFiles;
end;

procedure TfrmMain.actMarkMarkAllExecute(Sender: TObject);
begin
  inherited;
  ActiveFrame.MarkAll;
end;

procedure TfrmMain.actMarkUnmarkAllExecute(Sender: TObject);
begin
  inherited;
  ActiveFrame.UnMarkAll;
end;

procedure TfrmMain.actDirHotListExecute(Sender: TObject);
var
  p:TPoint;
begin
  inherited;
  CreatePopUpHotDir;// TODO: i thing in future this must call on create or change
  p:=ActiveFrame.dgPanel.ClientToScreen(Classes.Point(0,0));
  pmHotList.Popup(p.X,p.Y);
end;

procedure TfrmMain.actSearchExecute(Sender: TObject);
begin
  inherited;
  DebugLn('ShowFindDlg');
  ShowFindDlg(ActiveFrame.ActiveDir);
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

procedure TfrmMain.actDelete2Execute(Sender: TObject);
begin
  inherited;
  actDelete.Execute;
end;

procedure TfrmMain.edtCommandKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (key=VK_Down) and (Shift=[ssCtrl]) and (edtCommand.Items.Count>0) then
  begin
    Key:=0;
    edtCommand.DroppedDown:=True;
  end;
end;

procedure TfrmMain.CalculateSpace(bDisplayMessage:Boolean);
var
  fl:TFileList;
  p:TFileRecItem;
begin
  fl:=TFileList.Create; // free at Thread end by thread
  with ActiveFrame do
  begin
    p:=GetActiveItem^;
    p.sNameNoExt:=p.sName; //dstname
    p.sName:=ActiveDir+p.sName;
    p.sPath:='';

    fl.AddItem(@p);

//    CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir);
  end;
//  sDstPathTemp:=NotActiveFrame.ActiveDir;
  try
  with TSpaceThread.Create(fl) do
    begin
      Screen.Cursor:=crHourGlass;
      Resume;
      WaitFor;
      Screen.Cursor:=crDefault;

      if (bDisplayMessage = True) then
        ShowMessage(Format(rsSpaceMsg,[FilesCount, DirCount, FilesSize]));

      with ActiveFrame.GetActiveItem^ do
      begin
        if (bDisplayMessage = False) then
          iDirSize:=FilesSize;
        ActiveFrame.pnlFile.LastActive:=sName;
      end;
      Free;
    end;
  finally
    with ActiveFrame do
    begin
      Screen.Cursor:=crDefault;
//      UnMarkAll;
      pnlFile.UpdatePanel;
    end;
  end;
end;


procedure TfrmMain.actMarkPlusExecute(Sender: TObject);
begin
  ActiveFrame.MarkPlus;
end;

procedure TfrmMain.actMarkMinusExecute(Sender: TObject);
begin
  ActiveFrame.MarkMinus;
end;


procedure TfrmMain.actSymLinkExecute(Sender: TObject);
var
  sFile1, sFile2:String;
begin
  inherited;
  try
    with ActiveFrame do
    begin
      SelectFileIfNoSelected(GetActiveItem);
      sFile2 := pnlFile.GetActiveItem^.sName;
      sFile1 := ActiveDir + sFile2;
      sFile2 := NotActiveFrame.ActiveDir + sFile2;
    end;

    ShowSymLinkForm(sFile1, sFile2);

  finally
    FrameLeft.RefreshPanel;
    FrameRight.RefreshPanel;
    ActiveFrame.SetFocus;
  end;
end;

procedure TfrmMain.actHardLinkExecute(Sender: TObject);
var
  sFile1, sFile2:String;
begin
  inherited;
  try
    with ActiveFrame do
    begin
      SelectFileIfNoSelected(GetActiveItem);
      sFile2 := pnlFile.GetActiveItem^.sName;
      sFile1 := ActiveDir + sFile2;
      sFile2 := NotActiveFrame.ActiveDir + sFile2;
    end;
    
    ShowHardLinkForm(sFile1, sFile2);

  finally
    frameLeft.RefreshPanel;
    FrameRight.RefreshPanel;
    ActiveFrame.SetFocus;
  end;
end;

procedure TfrmMain.actReverseOrderExecute(Sender: TObject);
begin
  inherited;
  with ActiveFrame do
  begin
    pnlFile.SortDirection:= not pnlFile.SortDirection;
    pnlFile.Sort;
  end;
end;

procedure TfrmMain.actSortByNameExecute(Sender: TObject);
begin
  inherited;
  with ActiveFrame do
  begin
    pnlFile.SortByCol(0);
  end;
end;

procedure TfrmMain.actSortByExtExecute(Sender: TObject);
begin
  inherited;
  with ActiveFrame do
  begin
    pnlFile.SortByCol(1);
  end;
end;

procedure TfrmMain.actSortBySizeExecute(Sender: TObject);
begin
  inherited;
  with ActiveFrame do
  begin
    pnlFile.SortByCol(2);
  end;
end;

procedure TfrmMain.actSortByDateExecute(Sender: TObject);
begin
  inherited;
  with ActiveFrame do
  begin
    pnlFile.SortByCol(3);
  end;
end;

procedure TfrmMain.actSortByAttrExecute(Sender: TObject);
begin
  inherited;
  with ActiveFrame do
  begin
    pnlFile.SortByCol(4);
  end;
end;

procedure TfrmMain.actMultiRenameExecute(Sender: TObject);
var
  sl:TStringList;
  i:Integer;
begin
  with ActiveFrame do
  begin
    SelectFileIfNoSelected(GetActiveItem);

    sl:=TStringList.Create;
    try
      for i:=0 to pnlFile.FileList.Count-1 do
        if pnlFile.GetFileItem(i).bSelected then
          sl.Add(ActiveDir+pnlFile.GetFileItem(i).sName);
      if sl.Count>0 then
        ShowMultiRenameForm(sl);
    finally
      FreeAndNil(sl);
      FrameLeft.RefreshPanel;
      FrameRight.RefreshPanel;
      ActiveFrame.SetFocus;
    end;
  end;
end;

procedure TfrmMain.RenameFile(sDestPath:String);
var
  fl:TFileList;
  sDstMaskTemp:String;
  sCopyQuest:String;
  MT : TMoveThread;
begin
  fl:=TFileList.Create; // free at Thread end by thread
  sCopyQuest:=GetFileDlgStr(rsMsgRenSel, rsMsgRenFlDr);
  CopyListSelectedExpandNames(ActiveFrame.pnlFile.FileList,fl,ActiveFrame.ActiveDir);


  if (ActiveFrame.pnlFile.GetSelectedCount=1) then
  begin
    if sDestPath='' then
    begin
      ShowRenameFileEdit(ActiveFrame.pnlFile.GetActiveItem^.sName);
      Exit;
    end
    else
    begin
      if FPS_ISDIR(ActiveFrame.pnlFile.GetActiveItem^.iMode) then
        sDestPath:=sDestPath + '*.*'
      else
        sDestPath:=sDestPath + ExtractFileName(ActiveFrame.pnlFile.GetActiveItem^.sName);
    end;
  end
  else
    sDestPath:=sDestPath+'*.*';
  with TfrmMoveDlg.Create(Application) do
  begin
    try
      edtDst.Text:=sDestPath;
      lblMoveSrc.Caption:=sCopyQuest;
      if ShowModal=mrCancel then   Exit ; // throught finally
{        ActiveFrame.UnMarkAll;
        Exit;}
      sDestPath := ExtractFilePath(edtDst.Text);
      sDstMaskTemp:=ExtractFileName(edtDst.Text);
    finally
      Free;
    end;
  end;
  
(*Move files*)

try
  begin
   if not Assigned(frmFileOp) then
     frmFileOp:= TfrmFileOp.Create(Application);
   try
     MT := TMoveThread.Create(fl);
     MT.FFileOpDlg := frmFileOp;
     MT.sDstPath:=sDestPath;
     MT.sDstMask:=sDstMaskTemp;
     frmFileOp.Thread := TThread(MT);
     frmFileOp.Show;
     MT.Resume;
   except
     MT.Free;
   end;
  end;

except
    //FreeAndNil(frmFileOp);
end;
end;

procedure TfrmMain.CopyFile(sDestPath:String);
var
  fl:TFileList;
  sDstMaskTemp:String;
  sCopyQuest:String;
  CT : TCopyThread;
  blDropReadOnlyFlag : Boolean;
begin

  if (ActiveFrame.pnlFile.PanelMode in [pmVFS, pmArchive]) and
     (NotActiveFrame.pnlFile.PanelMode in [pmVFS, pmArchive]) then
    begin
      ShowMessage(rsMsgErrNotSupported);
      Exit;
    end;

  fl:=TFileList.Create; // free at Thread end by thread
  sCopyQuest:=GetFileDlgStr(rsMsgCpSel, rsMsgCpFlDr);

  CopyListSelectedExpandNames(ActiveFrame.pnlFile.FileList,fl,ActiveFrame.ActiveDir);

  CopyListSelectedExpandNames(ActiveFrame.pnlFile.FileList,fl,ActiveFrame.ActiveDir);

  if (ActiveFrame.pnlFile.GetSelectedCount=1) and not (FPS_ISDIR(ActiveFrame.pnlFile.GetActiveItem^.iMode) or ActiveFrame.pnlFile.GetActiveItem^.bLinkIsDir) then
    sDestPath:=sDestPath + ExtractFileName(ActiveFrame.pnlFile.GetActiveItem^.sName)
  else
    sDestPath:=sDestPath + '*.*';

  (* Copy files between archive and real file system *)
  
  (* Check active panel *)
  if  ActiveFrame.pnlFile.PanelMode = pmArchive then
    begin
      if not IsBlocked then
        begin
          DebugLN('+++ Extract files from archive +++');
          fl.CurrentDirectory := ActiveFrame.ActiveDir;
          ShowExtractDlg(ActiveFrame, fl, ExtractFilePath(sDestPath));
          NotActiveFrame.RefreshPanel;
        end;
      Exit;
    end;

  (* Check not active panel *)
  if  NotActiveFrame.pnlFile.PanelMode = pmArchive then
    begin
      if not IsBlocked then
        begin
          if  (VFS_CAPS_COPYIN in NotActiveFrame.pnlFile.VFS.VFSmodule.VFSCaps) then
            begin
              DebugLN('+++ Pack files to archive +++');
              fl.CurrentDirectory := ActiveFrame.ActiveDir;
              sDestPath:=ExtractFilePath(sDestPath);
              ShowPackDlg(NotActiveFrame.pnlFile.VFS, fl, sDestPath, False);
            end
          else
            msgOK(rsMsgErrNotSupported);
        end;
      Exit;
    end;
                                                        
  with TfrmCopyDlg.Create(Application) do
  begin
    try
      edtDst.Text:=sDestPath;
      lblCopySrc.Caption := sCopyQuest;
      cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;
      cbDropReadOnlyFlag.Visible := (NotActiveFrame.pnlFile.PanelMode = pmDirectory);
      if ShowModal=mrCancel then
        Exit ; // throught finally
      sDestPath:=ExtractFilePath(edtDst.Text);
      sDstMaskTemp:=ExtractFileName(edtDst.Text);
      blDropReadOnlyFlag := cbDropReadOnlyFlag.Checked;

    finally
      Free;
    end;
  end; //with

  (* Copy files between VFS and real file system *)
  
  (* Check not active panel *)
  if NotActiveFrame.pnlFile.PanelMode = pmVFS then
    begin
      if  (VFS_CAPS_COPYIN in NotActiveFrame.pnlFile.VFS.VFSmodule.VFSCaps) then
        begin
          DebugLN('+++ Copy files to VFS +++');
          fl.CurrentDirectory := ActiveFrame.ActiveDir;
          NotActiveFrame.pnlFile.VFS.VFSmodule.VFSCopyInEx(fl, sDestPath, 0);
        end
      else
        msgOK(rsMsgErrNotSupported);
      Exit;
    end;

  (* Check active panel *)
  if  ActiveFrame.pnlFile.PanelMode = pmVFS then
    begin
      if  (VFS_CAPS_COPYOUT in ActiveFrame.pnlFile.VFS.VFSmodule.VFSCaps) then
        begin
          DebugLN('+++ Copy files from VFS +++');
          fl.CurrentDirectory := ActiveFrame.ActiveDir;
          ActiveFrame.pnlFile.VFS.VFSmodule.VFSCopyOutEx(fl, sDestPath, 0);
        end
      else
        msgOK(rsMsgErrNotSupported);
      Exit;
    end;

  (* Copy files between real file system *)

  if not Assigned(frmFileOp) then
    frmFileOp:= TfrmFileOp.Create(Application);
    try
      CT := TCopyThread.Create(fl);
      CT.FFileOpDlg := frmFileOp;
      CT.sDstPath:=sDestPath;
      CT.sDstMask:=sDstMaskTemp;
      CT.bDropReadOnlyFlag := blDropReadOnlyFlag;

      frmFileOp.Thread := TThread(CT);
      frmFileOp.Show;
      CT.Resume;
    except
      CT.Free;
    end;
end;

procedure TfrmMain.actCopySamePanelExecute(Sender: TObject);
begin
  CopyFile('');
end;

procedure TfrmMain.actRenameOnlyExecute(Sender: TObject);
begin
  RenameFile('');
end;

procedure TfrmMain.actEditNewExecute(Sender: TObject);
var
  sNewFile:String;
  f:TextFile;
begin
  sNewFile:=ActiveFrame.ActiveDir + rsEditNewFile;
  if not InputQuery(rsEditNewOpen, rsEditNewFileName, sNewFile) then Exit;
  if not FileExists(sNewFile) then
  begin
    assignFile(f,sNewFile);
    try
      rewrite(f);
    finally
      CloseFile(f);
    end;
  end;
  try
    ShowEditorByGlob(sNewFile);
  finally
    frameLeft.RefreshPanel;
    frameRight.RefreshPanel;
  end;
end;

procedure TfrmMain.actDirHistoryExecute(Sender: TObject);
var
  p:TPoint;
begin
  inherited;
  CreatePopUpDirHistory;
  p:=ActiveFrame.dgPanel.ClientToScreen(Classes.Point(0,0));
  pmDirHistory.Popup(p.X,p.Y);
end;

procedure TfrmMain.actShowCmdLineHistoryExecute(Sender: TObject);
begin
  inherited;
  if (edtCommand.Items.Count>0) then
    edtCommand.DroppedDown:=True;
end;

procedure TfrmMain.actRunTermExecute(Sender: TObject);
begin
  if not edtCommand.Focused then
    begin
      SetCurrentDir(ActiveFrame.ActiveDir);
      ExecCmdFork(gRunTerm);
    end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;

//  DebugLn('Key down:',Key);
  if Key=18 then // is the ALT?
  begin
    ActiveFrame.ShowAltPanel;
    Key:=0;
    KeyPreview:=False;
    Exit;
  end;
  
  if Key=9 then  // TAB
  begin
    Key:=0;
    case PanelSelected of
      fpLeft: SetActiveFrame(fpRight);
      fpRight: SetActiveFrame(fpLeft);
    end;
    Exit;
  end;

  bAltPress:=(Shift=[ssAlt]);

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
      if pnlFile.GetActiveItem^.sName='..' then Exit;
        pnlFile.cdDownLevel(pnlFile.GetActiveItem);
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
      if pnlFile.GetActiveItem^.sName='..' then Exit;
        pnlFile.ChooseFile(pnlFile.GetActiveItem);
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

  bAltPress:=False;

  if (shift=[ssCtrl]) and (Key=VK_Down) then
  begin
    Key:=0;
    actShowCmdLineHistory.Execute;
    Exit;
  end;

  // handle Space key
  if (Shift=[]) and (Key=VK_Space) and (edtCommand.Text='') and (edtCommand.Text='') then
  begin
    with ActiveFrame do
    begin
//      if not AnySelected then Exit;
      if FPS_ISDIR(pnlFile.GetActiveItem^.iMode) then
        CalculateSpace(False);
      SelectFile(GetActiveItem);
      dgPanel.Invalidate;
      MakeSelectedVisible;
    end;
    Exit;
  end;

  if (Shift=[]) and (Key=VK_BACK) and (edtCommand.Text='') then
  begin
    if edtCommand.Focused then Exit;
    with ActiveFrame do
    begin
      pnlFile.cdUpLevel;
      RedrawGrid;
    end;
    Exit;
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
  TPanel(TEdit(Sender).Parent).Visible:=False;
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
    edtRename.Left:=16;
    edtRename.Height:=dgpanel.DefaultRowHeight+4;
    edtRename.Hint:=sFileName;
    edtRename.Text:=ExtractFileName(sFileName);
    edtRename.Visible:=True;
    edtRename.SelectAll;
    edtRename.SetFocus;
  end;
end;

procedure TfrmMain.actCalculateSpaceExecute(Sender: TObject);
begin
  inherited;
  with ActiveFrame do
  begin
    if FPS_ISDIR(pnlFile.GetActiveItem^.iMode) then
      CalculateSpace(True);
    // I don't know what to do if the item is file or something else
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

procedure TfrmMain.actFilePropertiesExecute(Sender: TObject);
begin
  inherited;
  try
    with ActiveFrame do
    begin
      SelectFileIfNoSelected(GetActiveItem);
      ShowFilePropertiesDialog(pnlFile.FileList, ActiveDir);
    end;
  finally
    frameLeft.RefreshPanel;
    frameRight.RefreshPanel;
    ActiveFrame.SetFocus;
  end
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

{ Show context menu on right click }
procedure TfrmMain.framedgPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    begin
      actContextMenu.Execute;
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
  actDirHistory.Execute;
end;

procedure TfrmMain.FramelblLPathMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
  mbMiddle:
    begin
      SetActiveFrame(TFrameFilePanel(TControl(Sender).Parent.Parent).PanelSelect);
      actDirHotList.Execute;
    end;
  mbRight:
    begin
      SetActiveFrame(TFrameFilePanel(TControl(Sender).Parent.Parent).PanelSelect);
      ShowPathEdit;
    end;
  end;
end;

procedure TfrmMain.FramepnlFileChangeDirectory(Sender: TObject; const NewDir: String);
var
  ANoteBook : TNoteBook;
  sCaption : String;
begin
  if Sender is TPage then
    begin
      ANoteBook := (Sender as TPage).Parent as TNoteBook;
      sCaption := GetLastDir(ExcludeTrailingPathDelimiter(NewDir));
      if Boolean(gDirTabOptions and tb_text_length_limit) and (Length(sCaption) > gDirTabLimit) then
        ANoteBook.Page[ANoteBook.PageIndex].Caption:= Copy(sCaption, 1, gDirTabLimit) + '...'
      else
        ANoteBook.Page[ANoteBook.PageIndex].Caption := sCaption;
    end;
end;

procedure TfrmMain.SetActiveFrame(panel: TFilePanelSelect);
begin
  PanelSelected:=panel;
  ActiveFrame.SetFocus;
  NotActiveFrame.dgPanelExit(self);
end;

procedure TfrmMain.UpdateDiskCount;
begin
  DrivesList.Clear;
  DrivesList := GetAllDrives;
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
  Count := DrivesList.Count - 1;
  for I := 0 to Count do
    begin
      Drive := PDrive(DrivesList.Items[I]);
      with Drive^ do
      begin
        miTmp := TMenuItem.Create(pmDrivesMenu);
        miTmp.Tag := I;
        miTmp.Caption := Name;
        miTmp.Hint := Path;
        
        if gDriveMenuButton then
          begin
            if Pos(Path, FrameLeft.pnlFile.ActiveDir) = 1 then
              begin
                btnLeftDrive.Glyph := PixMapManager.GetDriveIcon(Drive, btnLeftDrive.Height - 4, btnLeftDrive.Color);
                btnLeftDrive.Caption := Name;
                btnLeftDrive.Width := btnLeftDrive.Glyph.Width + btnLeftDrive.Canvas.TextWidth(btnLeftDrive.Caption) + 16;
                dskLeft.Tag := I;
              end;
            if Pos(Path, FrameRight.pnlFile.ActiveDir) = 1 then
              begin
                btnRightDrive.Glyph := PixMapManager.GetDriveIcon(Drive, btnRightDrive.Height - 4, btnRightDrive.Color);
                btnRightDrive.Caption := Name;
                btnRightDrive.Width := btnRightDrive.Glyph.Width + btnRightDrive.Canvas.TextWidth(btnRightDrive.Caption) + 16;
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
             if gDriveBar2 then
               dskLeft.Buttons[Tag].Down := True;
             dskLeft.Tag := Tag;
             FrameLeft.pnlFile.LoadPanel;
             SetActiveFrame(fpLeft);
             btnLeftDrive.Glyph := PixMapManager.GetDriveIcon(PDrive(DrivesList[Tag]), btnLeftDrive.Height - 4, btnLeftDrive.Color);
             btnLeftDrive.Caption := Caption;
             btnLeftDrive.Width := btnLeftDrive.Glyph.Width + btnLeftDrive.Canvas.TextWidth(btnLeftDrive.Caption) + 16;
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
    pnlFile.OnChangeDirectory := @FramepnlFileChangeDirectory;
    if not DirectoryExists(sPath) then
      GetDir(0, sPath);
    pnlFile.ActiveDir := sPath;
    pnlFile.LoadPanel;
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
    dgPanel.OnMouseUp := @framedgPanelMouseUp;

  end;

end;

function TfrmMain.AddPage(ANoteBook: TNoteBook):TPage;
var
  x:Integer;
begin
  x:=ANotebook.PageCount;

  ANoteBook.Pages.Add(IntToStr(x));
  ANoteBook.ActivePage:= IntToStr(x);
  Result:=ANoteBook.Page[x];

  ANoteBook.ShowTabs:= ((ANoteBook.PageCount > 1) or Boolean(gDirTabOptions and tb_always_visible)) and gDirectoryTabs;
  if Boolean(gDirTabOptions and tb_multiple_lines) then
    ANoteBook.Options := ANoteBook.Options + [nboMultiLine];
end;

procedure TfrmMain.RemovePage(ANoteBook: TNoteBook; iPageIndex:Integer);
begin
  if ANoteBook.PageCount>1 then
  begin
{    With ANoteBook.Page[iPageIndex] do
    begin
    if ComponentCount>0 then // must be true, but
    // component 0 is TFrameFilePanel
      Components[0].Free;
    end;}
    ANoteBook.Pages.Delete(iPageIndex);
  end;
  ANoteBook.ShowTabs:= ((ANoteBook.PageCount > 1) or Boolean(gDirTabOptions and tb_always_visible)) and gDirectoryTabs;
end;

procedure TfrmMain.LoadTabs(ANoteBook: TNoteBook);
var
  I : Integer;
  sIndex,
  TabsSection, Section: String;
  fpsPanel : TFilePanelSelect;
  sPath,
  sCaption, sActiveCaption : String;
  iActiveTab : Integer;
begin
  if ANoteBook.Name = 'nbLeft' then
    begin
      TabsSection := 'lefttabs';
      Section := 'left';
      fpsPanel := fpLeft;
    end
  else
    begin
      TabsSection := 'righttabs';
      Section := 'right';
      fpsPanel := fpRight;
    end;
  I := 0;
  sIndex := '0';

  { Read active tab index and caption }
  iActiveTab := gIni.ReadInteger(TabsSection, 'activetab', 0);
  sActiveCaption := gIni.ReadString(Section, 'activecaption', '');

   while True do
    begin
      if I = iActiveTab then
        begin
          sPath := gIni.ReadString(Section, 'path', '');
          CreatePanel(AddPage(ANoteBook), fpsPanel, sPath);
          if sActiveCaption <> '' then
            if Boolean(gDirTabOptions and tb_text_length_limit) and (Length(sActiveCaption) > gDirTabLimit) then
              ANoteBook.Page[ANoteBook.PageCount - 1].Caption := Copy(sActiveCaption, 1, gDirTabLimit) + '...'
            else
              ANoteBook.Page[ANoteBook.PageCount - 1].Caption := sActiveCaption;
        end;
      sPath := gIni.ReadString(TabsSection, sIndex + '_path', '');
      if sPath = '' then Break;
      sCaption := gIni.ReadString(TabsSection, sIndex + '_caption', '');
      CreatePanel(AddPage(ANoteBook), fpsPanel, sPath);
      if sCaption <> '' then
        if Boolean(gDirTabOptions and tb_text_length_limit) and (Length(sCaption) > gDirTabLimit) then
          ANoteBook.Page[ANoteBook.PageCount - 1].Caption := Copy(sCaption, 1, gDirTabLimit) + '...'
        else
          ANoteBook.Page[ANoteBook.PageCount - 1].Caption := sCaption;
      inc(I);
      sIndex := IntToStr(I);
    end;
    // set active tab
    ANoteBook.PageIndex := iActiveTab;
end;

procedure TfrmMain.SaveTabs(ANoteBook: TNoteBook);
var
  I, Count, J : Integer;
  sIndex,
  TabsSection, Section : String;
  sPath : String;
begin
  if ANoteBook.Name = 'nbLeft' then
    begin
      TabsSection := 'lefttabs';
      Section := 'left';
    end
  else
    begin
      TabsSection := 'righttabs';
      Section := 'right';
    end;

  gIni.EraseSection(TabsSection);
  
  I := 0;
  J := 0;
  Count := ANoteBook.PageCount - 1;
  repeat
    sIndex := IntToStr(I - J);

    if I = ANoteBook.PageIndex then
      begin
        gIni.WriteInteger(TabsSection, 'activetab', I);
        gIni.WriteString(TabsSection, 'activecaption', ANoteBook.ActivePage);
        if I < Count then
          begin
            inc(I);
            J := 1;
          end
        else
          Break;
      end;
    sPath := TFrameFilePanel(ANoteBook.Page[I].Components[0]).ActiveDir;
    gIni.WriteString(TabsSection, sIndex + '_path', sPath);
    gIni.WriteString(TabsSection, sIndex + '_caption', ANoteBook.Page[I].Caption);
    inc(I);
  until (I > Count);
  sPath := TFrameFilePanel(ANoteBook.ActivePageComponent.Components[0]).ActiveDir;
  gIni.WriteString(Section, 'path', sPath);
end;

(* Execute internal or external command *)

function TfrmMain.ExecCmd(Cmd: String): Boolean;
begin
  if actionLst.ActionByName(Cmd) <> nil then
    Result := actionLst.ActionByName(Cmd).Execute
  else
    Result := ExecCmdFork(Format('"%s"', [Cmd]));
end;

procedure TfrmMain.UpdateWindowView;
var
  I : Integer;
begin
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
          if FileExists(gpIniDir + 'default.bar') then
            MainToolBar.LoadFromFile(gpIniDir + 'default.bar')
	      else
            MainToolBar.LoadFromFile(gpCfgDir + 'default.bar');
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
      
      btnRightDrive.Flat := gInterfaceFlat;
      btnRightRoot.Flat := gInterfaceFlat;
      btnRightUp.Flat := gInterfaceFlat;
      btnRightHome.Flat := gInterfaceFlat;
    end;

  for I := 0 to nbLeft.PageCount - 1 do  //  change on all tabs
    with nbLeft.Page[I].Controls[0] as TFrameFilePanel do
    begin
      pnlHeader.Visible := gCurDir;  // Current directory
      pnlFooter.Visible := gStatusBar;  // Status bar
      if gShowIcons then
        pnlFile.FileList.UpdateFileInformation(pnlFile.PanelMode);
    end;

  for I := 0 to nbRight.PageCount - 1 do  //  change on all tabs
    with nbRight.Page[I].Controls[0] as TFrameFilePanel do
    begin
      pnlHeader.Visible := gCurDir;  // Current directory
      pnlFooter.Visible := gStatusBar;  // Status bar
      if gShowIcons then
        pnlFile.FileList.UpdateFileInformation(pnlFile.PanelMode);
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
  pnlKeys.Visible := gKeyButtons;
  nbLeft.ShowTabs := ((nbLeft.PageCount > 1) or Boolean(gDirTabOptions and tb_always_visible)) and gDirectoryTabs;
  nbRight.ShowTabs := ((nbRight.PageCount > 1) or Boolean(gDirTabOptions and tb_always_visible)) and gDirectoryTabs;
end;

procedure TfrmMain.edtCommandKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not edtCommand.DroppedDown and ((Key=VK_UP) or (Key=VK_DOWN)) then
  begin
    ActiveFrame.SetFocus;
    Key:=0;
  end;
end;


procedure TfrmMain.actFileLinkerExecute(Sender: TObject);
var
  sl:TStringList;
  i:Integer;
begin

  with ActiveFrame do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    sl:=TStringList.Create;
    try
      for i:=0 to pnlFile.FileList.Count-1 do
        if pnlFile.GetFileItem(i).bSelected then
          sl.Add(ActiveDir+pnlFile.GetFileItem(i).sName);
      if sl.Count>1 then
        ShowLinkerFilesForm(sl);
    finally
      FreeAndNil(sl);
      FrameLeft.RefreshPanel;
      FrameRight.RefreshPanel;
      ActiveFrame.SetFocus;
    end;
  end;
end;

procedure TfrmMain.actFileSpliterExecute(Sender: TObject);
var
  sl:TStringList;
  i:Integer;
begin
  with ActiveFrame do
  begin
    SelectFileIfNoSelected(GetActiveItem);

    sl:=TStringList.Create;
    try
      for i:=0 to pnlFile.FileList.Count-1 do
        if pnlFile.GetFileItem(i).bSelected then
          sl.Add(ActiveDir+pnlFile.GetFileItem(i).sName);
      if sl.Count>0 then
        ShowSplitterFileForm(sl);
    finally
      FreeAndNil(sl);
      FrameLeft.RefreshPanel;
      FrameRight.RefreshPanel;
      ActiveFrame.SetFocus;
    end;
  end;
end;

procedure TfrmMain.tbEditClick(Sender: TObject);
begin
  ShowConfigToolbar(pmToolBar.Tag);
end;


function TfrmMain.ExecuteCommandFromEdit(sCmd: String): Boolean;
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
    DebugLn('Chdir to:',sDir);
    if not SetCurrentDir(sDir) then
    begin
      msgError(Format(rsMsgChDirFailed, [sDir]));
    end
    else
    begin
      with ActiveFrame.pnlFile do
      begin
        GetDir(0,sDir);
        ActiveDir:=sDir;
        DebugLn(sDir);
      end;
    end;
  end
  else
  begin
    if edtCommand.Items.IndexOf(sCmd)=-1 then
      edtCommand.Items.Insert(0,sCmd);
    ActiveFrame.pnlFile.ExecuteFile(sCmd, True);
    edtCommand.DroppedDown:=False;
    // only cMaxStringItems(see uGlobs.pas) is stored
    if edtCommand.Items.Count>cMaxStringItems then
      edtCommand.Items.Delete(edtCommand.Items.Count-1);
  end;
end;

// Save ShortCuts to config file
procedure TfrmMain.SaveShortCuts;
var
  i, count: Integer;
  ini: TIniFile;
begin
  ini:=TIniFile.Create(gpIniDir + 'shortcuts.ini');
  try
    count := actionLst.ActionCount;
    for i := 0 to count-1 do
      with actionLst.Actions[i] as TAction do
        ini.WriteString('SHORTCUTS', Name, ShortCutToText(ShortCut));
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

// Load ShortCuts from config file
procedure TfrmMain.LoadShortCuts;
var
  i, j, count: Integer;
  ini: TIniFile;
  vAction: TAction;
  vShortCut: TShortCut;
begin
  // ToDo Black list HotKey which can't use
  ini:=TIniFile.Create(gpIniDir + 'shortcuts.ini');
  try
    count := actionLst.ActionCount;
    for i := 0 to count-1 do
    begin
      vAction := actionLst.Actions[i] as TAction;
      vShortCut := TextToShortCut(ini.ReadString('SHORTCUTS',
        vAction.Name, ShortCutToText(vAction.ShortCut)));
      if (ShortCutToText(vShortCut) <> ShortCutToText(vAction.ShortCut)) and (ShortCutToText(vShortCut) <> '') then
      begin
        for j := 0 to count-1 do
        if (ShortCutToText(TAction(actionLst.Actions[j]).ShortCut) = ShortCutToText(vShortCut)) then
           TAction(actionLst.Actions[j]).ShortCut := TextToShortCut('');
        vAction.ShortCut := vShortCut;
      end; // if
    end; // for i
  finally
    ini.Free;
  end;
end;

initialization
 {$I fmain.lrs}
end.
