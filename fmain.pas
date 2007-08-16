{
Double Commander
----------------------------
Licence  : GNU GPL v 2.0
Copyright (C) 2006-2007 Alexander Koblov (Alexx2000@mail.ru)

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
  SysUtils, Classes,  {uFilePanel,} fLngForm, framePanel, {FileCtrl,} Grids,
  KASToolBar, IniFiles;

const
  cHistoryFile='cmdhistory.txt';

type

  { TfrmMain }

  TfrmMain = class(TfrmLng)
    actChMod: TAction;
    actChown: TAction;
    actExtractFiles: TAction;
    actPackFiles: TAction;
    actRemoveTab: TAction;
    actNewTab: TAction;
    dskLeft: TKAStoolBar;
    dskRight: TKAStoolBar;
    MainToolBar: TKASToolBar;
    mnuExtractFiles: TMenuItem;
    pnlDisk: TPanel;
    tbDelete: TMenuItem;
    tbEdit: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mnuMain: TMainMenu;
    nbLeft: TNotebook;
    nbRight: TNotebook;
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
    pnlKeys: TPanel;
    btnF3: TSpeedButton;
    btnF4: TSpeedButton;
    btnF5: TSpeedButton;
    btnF6: TSpeedButton;
    btnF7: TSpeedButton;
    btnF8: TSpeedButton;
    btnF10: TSpeedButton;
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
    btnF9: TSpeedButton;
    actShowMenu: TAction;
    actRefresh: TAction;
    actSearch: TAction;
    actDirHotList: TAction;
    actMarkMarkAll: TAction;
    actMarkInvert: TAction;
    actMarkUnmarkAll: TAction;
    pmHotList: TPopupMenu;
    actDelete2: TAction;
    actPathToCmdLine: TAction;
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
    actShiftF5: TAction;
    actShiftF6: TAction;
    actShiftF4: TAction;
    actDirHistory: TAction;
    pmDirHistory: TPopupMenu;
    actCtrlF8: TAction;
    actRunTerm: TAction;
    miLine9: TMenuItem;
    miRunTerm: TMenuItem;
    actCalculateSpace: TAction;
    actFileProperties:  TAction;
    actFileLinker: TAction;
    actFileSpliter: TAction;
    pmToolBar: TPopupMenu;
    MainSplitter: TSplitter;
    procedure actExtractFilesExecute(Sender: TObject);
    procedure actPackFilesExecute(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure dskRightChangeLineCount(AddSize: Integer);
    procedure dskLeftToolButtonClick(NumberOfButton: Integer);
    procedure dskRightToolButtonClick(NumberOfButton: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MainToolBarMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainToolBarToolButtonClick(NumberOfButton : Integer);
    procedure actExitExecute(Sender: TObject);
    procedure actNewTabExecute(Sender: TObject);
    procedure actRemoveTabExecute(Sender: TObject);
    procedure frmMainClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frmMainKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure frmMainShow(Sender: TObject);
    procedure mnuHelpClick(Sender: TObject);
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
    procedure actPathToCmdLineExecute(Sender: TObject);
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
    procedure actShiftF5Execute(Sender: TObject);
    procedure actShiftF6Execute(Sender: TObject);
    procedure actShiftF4Execute(Sender: TObject);
    procedure actDirHistoryExecute(Sender: TObject);
    procedure actCtrlF8Execute(Sender: TObject);
    procedure actRunTermExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FrameRightedtRenameExit(Sender: TObject);
    procedure FrameedtSearchExit(Sender: TObject);

    procedure actCalculateSpaceExecute(Sender: TObject);
    procedure actFilePropertiesExecute(Sender: TObject);
    procedure FramedgPanelEnter(Sender: TObject);
    procedure framedgPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FramelblLPathClick(Sender: TObject);
    procedure FrameHeaderDblClick(Sender: TObject);
    procedure FramelblLPathMouseDown(Sender: TObject; Button: TMouseButton;
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
    IsPanelsCreated : Boolean;
    
    function ExecuteCommandFromEdit(sCmd:String):Boolean;
  public
//    frameLeft, frameRight:TFrameFilePanel;
    
    procedure LoadLng; override;
    
    function HandleActionHotKeys(var Key: Word; Shift: TShiftState):Boolean; // handled
    
    Function ActiveFrame:TFrameFilePanel;  // get Active frame
    Function NotActiveFrame:TFrameFilePanel; // get NotActive frame :)
    function FrameLeft:TFrameFilePanel;
    function FrameRight:TFrameFilePanel;
    Function IsAltPanel:Boolean;
    procedure AppException(Sender: TObject; E: Exception);
    //check selected count and generate correct msg, parameters is lng indexs
    Function GetFileDlgStr(iLngOne, iLngMulti:Integer):String;
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
    procedure CreateDiskPanel(dskPanel : TKASToolBar);
    procedure CreatePanel(AOwner:TWinControl; APanel:TFilePanelSelect; sPath : String);
    function AddPage(ANoteBook:TNoteBook):TPage;
    procedure RemovePage(ANoteBook:TNoteBook; iPageIndex:Integer);
    procedure LoadTabs(ANoteBook:TNoteBook);
    procedure SaveTabs(ANoteBook:TNoteBook);
    function ExecCmd(Cmd : String) : Boolean;
    procedure SaveShortCuts;
    procedure LoadShortCuts;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uTypes, fAbout, uGlobs, uLng, fOptions,{ fViewer,}fbtnchangedlg, fconfigtoolbar,
  uCopyThread, uFileList, uDeleteThread,
  fMkDir, fCopyDlg, fCompareFiles,{ fEditor,} fMoveDlg, uMoveThread, uShowMsg,
  fFindDlg, uSpaceThread, fHotDir, fSymLink, fHardLink,
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
  fl:=TFileList.Create;
  with ActiveFrame do
    begin
      SelectFileIfNoSelected(GetActiveItem);
      CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir);

      fl.CurrentDirectory := ActiveDir;
    end;
  try
    ShowPackFilesForm(NotActiveFrame.pnlFile.VFS, fl, NotActiveFrame.ActiveDir);
  finally
    frameLeft.RefreshPanel;
    frameRight.RefreshPanel;
  end;

end;

procedure TfrmMain.actExtractFilesExecute(Sender: TObject);
var
  fl : TFileList;
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

end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DebugLn('frmMain.Destroy');
  edtCommand.Items.SaveToFile(gpIniDir+cHistoryFile);
  {*Tool Bar*}
  MainToolBar.SaveToFile(gpIniDir + 'default.bar');
  {*Tool Bar*}
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

procedure TfrmMain.dskLeftToolButtonClick(NumberOfButton: Integer);
var
  Command : String;
begin
  if dskLeft.Buttons[NumberOfButton].GroupIndex = 0 then
     begin
     Command := dskLeft.Commands[NumberOfButton];
     if Command = '/' then
        FrameLeft.pnlFile.ActiveDir := ExtractFileDrive(FrameLeft.pnlFile.ActiveDir);
     if Command = '..' then
        FrameLeft.pnlFile.cdUpLevel;
     if Command = '~' then
        FrameLeft.pnlFile.ActiveDir := GetHomeDir;
     end
  else
  begin
     if IsAvailable(dskLeft.Commands[NumberOfButton]) then
       begin
         FrameLeft.pnlFile.ActiveDir := dskLeft.Commands[NumberOfButton];
         dskLeft.Tag := NumberOfButton;
       end
     else
       begin
         dskLeft.Buttons[dskLeft.Tag].Down := True;
         msgOK(lngGetString(clngMsgDiskNotAvail));
       end;
  end;
  FrameLeft.pnlFile.LoadPanel;

  SetActiveFrame(fpLeft);
end;

procedure TfrmMain.dskRightToolButtonClick(NumberOfButton: Integer);
var
  Command : String;
begin
  if dskRight.Buttons[NumberOfButton].GroupIndex = 0 then
     begin
     Command := dskRight.Commands[NumberOfButton];
     if Command = '/' then
        FrameRight.pnlFile.ActiveDir := ExtractFileDrive(FrameRight.pnlFile.ActiveDir);
     if Command = '..' then
        FrameRight.pnlFile.cdUpLevel;
     if Command = '~' then
        FrameRight.pnlFile.ActiveDir := GetHomeDir;
     end
  else
   begin
     if IsAvailable(dskRight.Commands[NumberOfButton]) then
       begin
         FrameRight.pnlFile.ActiveDir := dskRight.Commands[NumberOfButton];
         dskRight.Tag := NumberOfButton;
       end
     else
       begin
         dskRight.Buttons[dskRight.Tag].Down := True;
         msgOK(lngGetString(clngMsgDiskNotAvail));
       end;
  end;
  FrameRight.pnlFile.LoadPanel;

  SetActiveFrame(fpRight);
end;


procedure TfrmMain.MainToolBarMouseDown(Sender: TOBject; Button: TMouseButton;
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

procedure TfrmMain.MainToolBarToolButtonClick(NumberOfButton : Integer);
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
  
  (*Create Disk Panels*)
  CreateDiskPanel(dskLeft);
  CreateDiskPanel(dskRight);
  (*/Create Disk Panels*)

  (*Tool Bar*)
  MainToolBar.ChangePath := gpExePath;
  MainToolBar.EnvVar := '%commander_path%';
  MainToolBar.LoadFromFile(gpIniDir + 'default.bar');
  (*Tool Bar*)
  
  LoadShortCuts;
  
  IsPanelsCreated := True;
end;

procedure TfrmMain.mnuHelpClick(Sender: TObject);
begin

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
              VFSFileList.AddItem(fr);
              sTempDir := GetTempDir;
              {if }pnlFile.VFS.VFSmodule.VFSCopyOut(VFSFileList, sTempDir, 0);{ then}
                begin
                 sl.Add(sTempDir + fr^.sName);
                 ShowViewerByGlobList(sl, True);
                 Exit;
                end;
            end;
          sl.Add(ActiveDir+fr^.sName);
          DebugLn('View.Add:',ActiveDir+fr^.sName);
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
begin
  with ActiveFrame do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    try
    // in this time we only one file process
      for i:=0 to pnlFile.FileList.Count-1 do
      begin
      fr:=pnlFile.GetFileItemPtr(i);
      if fr^.bSelected and not (FPS_ISDIR(fr^.iMode)) then
        begin
          ShowEditorByGlob(ActiveDir+fr^.sName);
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
      sPath:=ActiveDir;
      if not ShowMkDir(sPath) then Exit;
      if (sPath='') then Exit;
      
      { Create directory in VFS }
        if  ActiveFrame.pnlFile.PanelMode in [pmArchive, pmVFS] then
        begin
          DebugLN('+++ Create directory in VFS +++');
          ActiveFrame.pnlFile.VFS.VFSmodule.VFSMkDir(ActiveDir + sPath);
          ActiveFrame.RefreshPanel;
        end
      else
      { /Create directory in VFS }

      if (DirectoryExists(ActiveDir+sPath)) then
      begin
        msgError(Format(lngGetString(clngMsgErrDirExists),[ActiveDir+sPath]));
        pnlFile.LastActive:=sPath;
        pnlFile.LoadPanel;
      end
      else
      begin
        if not ForceDirectory(ActiveDir+sPath) then
          msgError(Format(lngGetString(clngMsgErrForceDir),[ActiveDir+sPath]))
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
    SelectFileIfNoSelected(GetActiveItem);

  case msgYesNoCancel(GetFileDlgStr(clngMsgDelSel,clngMsgDelFlDr)) of
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
    
    
    (* Delete files from archive *)
    if  ActiveFrame.pnlFile.PanelMode in [pmArchive, pmVFS] then
      begin
        DebugLN('+++ Delete files +++');
        ActiveFrame.pnlFile.VFS.VFSmodule.VFSDelete(fl);
      end
    else
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
  nbLeft.Width:= (frmMain.Width div 2) - (MainSplitter.Width div 2);

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

procedure TfrmMain.LoadLng;
begin
//actions
  actExit.Caption:= lngGetString(clngActExit);
  actView.Caption:=  lngGetString(clngActView);
  actEdit.Caption:=  lngGetString(clngActEdit);
  actCopy.Caption:=  lngGetString(clngActCopy);
  actRename.Caption:=  lngGetString(clngActRename);
  actMakeDir.Caption:=  lngGetString(clngActMkDir);
  actDelete.Caption:=  lngGetString(clngActDelte);
  actOptions.Caption:=   lngGetString(clngMnuCnfOpt);
  actCompareContents.Caption:= lngGetString(clngMnuFileCmpCnt);
  actShowMenu.Caption:=  lngGetString(clngActMenu);
  actRefresh.Caption:=  lngGetString(clngMnuShwReRead);
  actSearch.Caption:= lngGetString(clngMnuCmdSearch);
  actDirHotList.Caption:= lngGetString(clngMnuCmdHotDir);
  actMarkMarkAll.Caption:=     lngGetString(clngMnuMarkSelAll);
  actMarkUnmarkAll.Caption :=lngGetString(clngMnuMarkUnSelAll);
  actShowSysFiles.Caption:=     lngGetString(clngMnuFileShowSys);
  actCalculateSpace.Caption:=     lngGetString(clngMnuFileCalc);

  actMarkInvert.Caption:=     lngGetString(clngMnuMarkInvSel);
  actMarkPlus.Caption:=   lngGetString(clngMnuMarkSelGr);
  actMarkMinus.Caption:=  lngGetString(clngMnuMarkUnSelGr);

  actHardLink.Caption:=     lngGetString(clngMnuFileLink);
  actSymLink.Caption:=     lngGetString(clngMnuFileSymLink);
  actReverseOrder.Caption:= lngGetString(clngMnuShwRevOrd);
  actMultiRename.Caption:= lngGetString(clngActMultiRename);

  actRunTerm.Caption:= lngGetString(clngActRunTerm);

  actFileProperties.Caption:=     lngGetString(clngMnuFileProp);
  
// Menu
// File
  mnuFiles.Caption:=   lngGetString(clngMnuFile);
  mnuPackFiles.Caption:= 'Pack Files...'; //TODO localize
  mnuFilesSplit.Caption:=     lngGetString(clngMnuFileSplit);
  mnuFilesCombine.Caption:=     lngGetString(clngMnuFileCombine);


//Mark
  mnuMark.Caption:=   lngGetString(clngMnuMark);
  mnuMarkCmpDir.Caption:=     lngGetString(clngMnuMarkCmpDir);

//Commands
  mnuCmd.Caption:=        lngGetString(clngMnuCmd);

  mnuCmdSwapSourceTarget.Caption:=    lngGetString(clngMnuCmdSrcTrg);
  mnuCmdTargetIsSource.Caption:=   lngGetString(clngMnuCmdSrcEkvTrg);

//Show
  mnuShow.Caption:=      lngGetString(clngMnuShw);
  mnuShowName.Caption:=  lngGetString(clngMnuShwName);
  mnuShowExtension.Caption:=  lngGetString(clngMnuShwExt);
  mnuShowSize.Caption:=  lngGetString(clngMnuShwSize);
  mnuShowTime.Caption:=  lngGetString(clngMnuShwDate);
  mnuShowAttrib.Caption:=  lngGetString(clngMnuShwAttr);

//Configuration
  mnuConfig.Caption:=  lngGetString(clngMnuCnf);

//Help

  mnuHelp.Caption:=lngGetString(clngMnuHlp);
  mnuHelpAbout.Caption:=  lngGetString(clngMnuHlpAbout);
// Other
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
     VK_F9:
       begin
         actShowMenu.Execute;
         Exit;
       end;

     VK_APPS:
       begin
         pfri := ActiveFrame.GetActiveItem;
         pfri^.sPath := ActiveFrame.ActiveDir;
         ShowContextMenu(Handle, pfri, Mouse.CursorPos.x, Mouse.CursorPos.y);
         Exit;
       end;
     
   end;
  end;



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
        pnlFile.ExecuteFile('./'+pnlFile.GetActiveItem^.sName, True);
        Exit;
      end;
      // alt enter
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



  if Shift=[ssAlt] then
  begin



  end;

  if Shift=[ssShift] then
  begin
    if (Key=VK_F2) then
    begin
      edtCommand.SetFocus;
      Exit;
    end;

    {Kylix:
    this strange: KEY_15 is at real KEY_5
     and KEY_16 is a KEY_6, Why?
     it's a bug or feature? :-(
    }
    if ((Key=VK_F4) {or (Key=VK_F14)}) then
    begin
      actShiftF4.Execute;
      Exit;
    end;

    if ((Key=VK_F5){ or (Key=VK_F15)}) then
    begin
      actShiftF5.Execute;
      Exit;
    end;

    if ((Key=VK_F6) {or (Key=VK_F16)})then
    begin
      actShiftF6.Execute;
      Exit;
    end;
  end;

  if Shift=[ssCtrl] then
  begin

    // handle ctrl+q
    if (Key=VK_Q) then
    begin
      actExit.Execute;
      Exit;
    end;

    if (Key=VK_D) then
    begin
      actDirHotList.Execute;
      Exit;
    end;

    if (Key=VK_H) then
    begin
      actDirHistory.Execute;
      Exit;
    end;
    if (Key=VK_R) then
    begin
      actRefresh.Execute;
      Exit;
    end;
{
   // handle Ctrl+Enter
    if ((Key=VK_Return) or (Key=VK_SELECT)) and (edtCommand.Text='') then
    begin
      actCalculateSpace.Execute;
      Exit;
    end;
}
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

    if (Key=VK_T) then
    begin
      actNewTab.Execute;
      Exit;
    end;
    if (Key=VK_W) then
    begin
      actRemoveTab.Execute;
      Exit;
    end;

    
    if (Key=VK_P) then
    begin
      with ActiveFrame do
      begin
        edtCmdLine.Text:=edtCmdLine.Text+(pnlFile.ActiveDir);
      end;
      Exit;
    end;

  end;
  
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
      ExecCmdFork(Format(gExtDiff,[sFile1, sFile2]));
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

Function TfrmMain.GetFileDlgStr(iLngOne, iLngMulti:Integer):String;
var
  iSelCnt:Integer;
begin
  with ActiveFrame do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    iSelCnt:=pnlFile.GetSelectedCount;
    if iSelCnt=0 then Abort;
    if iSelCnt >1 then
      Result:=Format(lngGetString(iLngMulti),[iSelCnt])
    else
      Result:=Format(lngGetString(iLngOne),[pnlFile.GetActiveItem^.sName])
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
    mi.Caption:=Format(lngGetString(clngMsgPopUpHotDelete),[ActiveFrame.ActiveDir]);
    mi.OnClick:=@miHotDeleteClick;
  end
  else
  begin
    mi.Caption:=Format(lngGetString(clngMsgPopUpHotAdd),[ActiveFrame.ActiveDir]);
    mi.OnClick:=@miHotAddClick;
  end;
  pmHotList.Items.Add(mi);

  // now add configure item
  mi:=TMenuItem.Create(pmHotList);
  mi.Caption:=lngGetString(clngMsgPopUpHotCnf);
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

procedure TfrmMain.actPathToCmdLineExecute(Sender: TObject);
begin
  inherited;
  with ActiveFrame do
  begin
    edtCmdLine.Text:=edtCmdLine.Text+ActiveDir;
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
        ShowMessage(Format(lngGetString(clngSpaceMsg),[FilesCount, DirCount, FilesSize]));

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
      sFile1:=ActiveDir+pnlFile.GetActiveItem^.sName;
    end;

    with NotActiveFrame do
    begin
      SelectFileIfNoSelected(GetActiveItem);
      sFile2:=ActiveDir+pnlFile.GetActiveItem^.sName;
    end;
    ShowSymLinkForm(sFile1, sFile2);

  finally
    frameLeft.RefreshPanel;
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
      sFile1:=ActiveDir+pnlFile.GetActiveItem^.sName;
    end;

    with NotActiveFrame do
    begin
      SelectFileIfNoSelected(GetActiveItem);
      sFile2:=ActiveDir+pnlFile.GetActiveItem^.sName;
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
  sCopyQuest:=GetFileDlgStr(clngMsgRenSel,clngMsgRenFlDr);
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
        sDestPath:=sDestPath+'*.*'
      else
        sDestPath:=sDestPath+ActiveFrame.pnlFile.GetActiveItem^.sName;
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
  fl:=TFileList.Create; // free at Thread end by thread
  sCopyQuest:=GetFileDlgStr(clngMsgCpSel,clngMsgCpFlDr);

  CopyListSelectedExpandNames(ActiveFrame.pnlFile.FileList,fl,ActiveFrame.ActiveDir);

  CopyListSelectedExpandNames(ActiveFrame.pnlFile.FileList,fl,ActiveFrame.ActiveDir);

  if (ActiveFrame.pnlFile.GetSelectedCount=1) and not (FPS_ISDIR(ActiveFrame.pnlFile.GetActiveItem^.iMode) or ActiveFrame.pnlFile.GetActiveItem^.bLinkIsDir) then
    sDestPath:=sDestPath+ActiveFrame.pnlFile.GetActiveItem^.sName
  else
    sDestPath:=sDestPath+'*.*';


  (* Extract files from archive *)
  if  ActiveFrame.pnlFile.PanelMode = pmArchive then
    begin
      DebugLN('+++ Extract files from archive +++');
      fl.CurrentDirectory := ActiveFrame.ActiveDir;
      ShowExtractDlg(ActiveFrame, fl, ExtractFilePath(sDestPath));
      NotActiveFrame.RefreshPanel;
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

  (* Check not active panel *)
  if  NotActiveFrame.pnlFile.PanelMode in [pmArchive, pmVFS] then
    begin
      DebugLN('+++ Copy files to VFS +++');
      fl.CurrentDirectory := ActiveFrame.ActiveDir;
      NotActiveFrame.pnlFile.VFS.VFSmodule.VFSCopyInEx(fl, sDestPath, 0);
      Exit;
    end;

  (* Check active panel *)
  try
    (*Copy files from VFS*)
    if  ActiveFrame.pnlFile.PanelMode = pmVFS then
      begin
        DebugLN('+++ Copy files from VFS +++');
        fl.CurrentDirectory := ActiveFrame.ActiveDir;
        ActiveFrame.pnlFile.VFS.VFSmodule.VFSCopyOutEx(fl, sDestPath, 0);
        NotActiveFrame.RefreshPanel;
      end
    else
    (*Copy files*)
      begin
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

  except
    //frmFileOp.Free;
  end;

end;

procedure TfrmMain.actShiftF5Execute(Sender: TObject);
begin
  CopyFile('');
end;

procedure TfrmMain.actShiftF6Execute(Sender: TObject);
begin
  RenameFile('');
end;

procedure TfrmMain.actShiftF4Execute(Sender: TObject);
var
  sNewFile:String;
  f:TextFile;
begin
  sNewFile:=ActiveFrame.ActiveDir+lngGetString(clngShiftF4file);
  if not InputQuery(lngGetString(clngShiftF4Open),lngGetString(clngShiftF4FileName),sNewFile) then Exit;
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

procedure TfrmMain.actCtrlF8Execute(Sender: TObject);
begin
  inherited;
  if (edtCommand.Items.Count>0) then
    edtCommand.DroppedDown:=True;
end;

procedure TfrmMain.actRunTermExecute(Sender: TObject);
begin
  ExecCmdFork(gRunTerm);
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
  
  if Key=9 then
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
      pnlFile.cdDownLevel(pnlFile.GetActiveItem);
    end;
    Key:=0;
    Exit;
  end;

{  // this is hack for ShowModal bug
  with ActiveFrame do
  begin
    if bDialogShowed then
    begin
      Key:=0;
      bDialogShowed:=False;
      Exit;
    end;
  end;}
//  DebugLn(Key);
  if HandleActionHotKeys(Key, Shift) Then
  begin
    Key:=0;;
    Exit;
  end;
//  DebugLn(Key);

{  if bAltPress and (shift=[ssAlt]) and (key=VK_Alt) and not IsAltPanel then
  begin
//    qt.QMenuBar_activateItemAt(mnuMain.Handle, 0);
    bAltPress:=False;
    Exit;
  end;}
  bAltPress:=False;

  if (shift=[ssCtrl]) and (Key=VK_Down) then
  begin
    Key:=0;
    actCtrlF8.Execute;
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

procedure TfrmMain.FrameRightedtRenameExit(Sender: TObject);
begin
// handler for both edits
//  DebugLn('On exit');
  KeyPreview:=True;
  ActiveFrame.edtRename.Visible:=False;
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
    edtRename.OnExit:=@FrameRightedtRenameExit;
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
procedure TfrmMain.framedgPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pfri : PFileRecItem;
begin
  if Button = mbRight then
    begin
     pfri := ActiveFrame.GetActiveItem;
     pfri^.sPath := ActiveFrame.ActiveDir;
     ShowContextMenu(Handle, pfri, Mouse.CursorPos.x, Mouse.CursorPos.y);
    end;
end;


procedure TfrmMain.FramelblLPathClick(Sender: TObject);
begin
//  DebugLn(TControl(Sender).Parent.Parent.ClassName);
  SetActiveFrame(TFrameFilePanel(TControl(Sender).Parent.Parent).PanelSelect);
  actDirHistory.Execute;
end;

procedure TfrmMain.FrameHeaderDblClick(Sender: TObject);
begin
  SetActiveFrame(TFrameFilePanel(TControl(Sender).Parent).PanelSelect);
  actDirHotList.Execute;
end;

procedure TfrmMain.FramelblLPathMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbMiddle  then
    begin
      SetActiveFrame(TFrameFilePanel(TControl(Sender).Parent.Parent).PanelSelect);
      actDirHotList.Execute;
    end;
end;

procedure TfrmMain.FramepnlFileChangeDirectory(Sender: TObject; const NewDir: String);
var
  ANoteBook : TNoteBook;
begin
  if Sender is TPage then
    begin
      ANoteBook := (Sender as TPage).Parent as TNoteBook;
      ANoteBook.Page[ANoteBook.PageIndex].Caption := NewDir;
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
  // delete all disk buttons
  dskRight.DeleteAllToolButtons;
  dskLeft.DeleteAllToolButtons;
  // and add new
  CreateDiskPanel(dskLeft);
  CreateDiskPanel(dskRight);
end;

procedure TfrmMain.CreateDiskPanel(dskPanel: TKASToolBar);
var
Drives : TList;
I, Count, btnIndex : Integer;
Drive : PDrive;
ButtonIcon : TBitMap;
begin
//dskPanel.InitBounds; // Update information
  Drives := GetAllDrives;
  Count := Drives.Count - 1;

  for I := 0 to Count do
  begin
  Drive := PDrive(Drives.Items[I]);
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

      //**********************************
      if gIconsSize > 16 then
        begin
          ButtonIcon:= TBitMap.Create;
          with ButtonIcon do
          begin
            Width:=dskPanel.Buttons[I].Height;
            Height:=Width;

            Canvas.Brush.Color := dskPanel.Buttons[I].Color;
            Canvas.FillRect(Canvas.ClipRect);
            Canvas.StretchDraw(Canvas.ClipRect,PixMapManager.GetBitmap(DriveIcon, dskPanel.Buttons[I].Color));
            dskPanel.Buttons[I].Glyph := ButtonIcon;
          end;
        end
      else
      //**********************************
      dskPanel.Buttons[I].Glyph := PixMapManager.GetBitmap(DriveIcon, dskPanel.Buttons[I].Color);
      {Set Buttons Transparent. Is need? }
      dskPanel.Buttons[I].Glyph.Transparent := True;
      dskPanel.Buttons[I].Transparent := True;
      {/Set Buttons Transparent}
      dskPanel.Buttons[I].Layout := blGlyphLeft;
    end; // with
  end; // for
{Add special buttons}
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

procedure TfrmMain.CreatePanel(AOwner: TWinControl; APanel:TFilePanelSelect; sPath : String);
begin
  with TFrameFilePanel.Create(AOwner, lblCommandPath, edtCommand) do
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
    lblLPath.OnClick:=@FramelblLPathClick;
    lblLPath.OnMouseDown := @FramelblLPathMouseDown;
    edtRename.OnExit:=@FrameRightedtRenameExit;
    edtSearch.OnExit:=@FrameedtSearchExit;
    
    
    dgPanel.OnEnter:=@framedgPanelEnter;
    dgPanel.OnMouseDown := @framedgPanelMouseDown;
    pnlHeader.OnDblClick := @FrameHeaderDblClick;

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

  ANoteBook.ShowTabs:= (ANoteBook.PageCount > 1) or Boolean(gDirTabOptions and tb_always_visible);
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
  ANoteBook.ShowTabs:= (ANoteBook.PageCount > 1) or Boolean(gDirTabOptions and tb_always_visible);
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
  if ANoteBook.Align = alLeft then
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
            ANoteBook.Page[ANoteBook.PageCount - 1].Caption := sActiveCaption;
        end;
      sPath := gIni.ReadString(TabsSection, sIndex + '_path', '');
      if sPath = '' then Break;
      sCaption := gIni.ReadString(TabsSection, sIndex + '_caption', '');
      CreatePanel(AddPage(ANoteBook), fpsPanel, sPath);
      if sCaption <> '' then
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
  if ANoteBook.Align = alLeft then
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
    Result := (ExecCmdFork(Cmd) = 0);
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
if pmToolBar.Tag >= 0 then
begin
ShowOneBtnChangeDlg(pmToolBar.Tag);
end
else
begin
ShowConfigToolbar;
end;
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
      msgError(Format('ChDir to [%s] failed!',[sDir]));
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
