{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains all DC actions

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Copyright (C) 2008-2009  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uActs;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, Dialogs, typinfo, ExtCtrls, StringHashList, ActnList,
  uFileView, uFileViewNotebook;
  
  
const cf_Null=0;
      cf_Error=-1;

  type

  TCommandFunc = procedure(param: string) of object;

  PActionState = ^TActionState;
  TActionState = record
    Enabled: Boolean;  //<en Whether the action is enabled (through hotkeys).
    Action: TAction;   //<en If a TAction is assigned to a named action,
                       //    it is cached here.
  end;

  { TActs }

  TActs=class
  private
   FCmdList:TStrings;
   function GetList:TStrings;
   procedure EnableAction(ActionState: PActionState; Enabled: Boolean);
   class function Methods(AClass:TClass) : TStringList;
  public
   FActionsState: TStringHashList;
   constructor Create;
   destructor Destroy;override;
   function Execute(Cmd: string; param:string =''): integer;
   function GetIndex(Cmd: string): integer;
   function GetCategoriesList(const List:TStrings):integer;
   function GetCommandsByCategory(Category:string; const List:TStrings):integer;
   function GetCommandCaption(sCommand: String; bAmpersand: Boolean = False): UTF8String;

   {en
      Adds a named action to a list of possible actions.
      @param(ActionName
             Name of the action _without_ any prefixes ('cm_' or 'act').)
   }
   procedure AddAction(ActionName: String);
   {en
      Adds a TAction to a list of possible actions.
   }
   procedure AddAction(Action: TAction);
   {en
      Enables/disables an action.
      @param(ActionName
             Name of the action _without_ any prefixes ('cm_' or 'act').)
      @param(Enable
             Whether to enable or disable the action.)
   }
   procedure EnableAction(ActionName: String; Enable: Boolean);
   {en
      Checks if the action is enabled.
      @param(ActionName
             Name of the action _without_ any prefixes ('cm_' or 'act').)
   }
   function  IsActionEnabled(ActionName: String): Boolean;
   procedure EnableAllActions(Enable: Boolean);

   //---------------------
   // The Do... functions are cm_... functions' counterparts which are to be
   // executed directly from the code with specific - especially non-string
   // - arguments (instead of calling cm_... functions, in which case
   // parameters would have to be converted to and from strings).
   //
   procedure DoRemoveTab(Notebook: TFileViewNotebook; PageIndex: Integer);
   procedure DoToggleLockTab(Tab: TFileViewPage);
   procedure DoToggleLockDcaTab(Tab: TFileViewPage);
   procedure DoCopySelectedFileNamesToClipboard(FileView: TFileView; FullNames: Boolean);
   procedure DoNewTab(Notebook: TFileViewNotebook);
   //---------------------

  published

   //--------------------------------------------------------------------------
   // The cm_... functions are 'user' functions which can be assigned to toolbar
   // button, hotkey, menu item, scripts, etc. Those functions take a string
   // parameter(s) set by the user.
   //
   //--------------------------------------------------------------------------
   // Only published functions and procedures can by found by MethodAddress
   //--------------------------------------------------------------------------
   //
   // All commands can be split into three groups:
   // 1. Global commands intended for the main application (cm_VisitHomePage,
   //    cm_About, cm_Exit, ...).
   //
   // 2. Commands intended for file views (cm_QuickSearch, cm_SortByColumn, ...).
   //    Those commands are simply redirected to the currently active file view by calling:
   //       frmMain.ActiveFrame.ExecuteCommand(CommandName, param);
   //    If they are supported by the given file view they are executed there.
   //
   //    If in future there will be a need to pass specific parameters to the
   //    commands, i.e. not string, they should be implemented by creating
   //    an interface for each command, and each file view implementing those
   //    interfaces which commands it supports.
   //
   // 3. Commands intended for file sources (cm_Copy, cm_Rename, cm_MakeDir).
   //    The file operations will mostly work only for non-virtual file sources.
   //
   procedure cm_AddPathToCmdLine(param: string='');
   procedure cm_AddFilenameToCmdLine(param: string='');
   procedure cm_AddPathAndFilenameToCmdLine(param: string='');
   procedure cm_ContextMenu(param: string='');
   procedure cm_DriveContextMenu(param: string='');
   procedure cm_CopyFullNamesToClip(param: string='');
   procedure cm_Exchange(param:string='');
   procedure cm_LeftOpenDrives(param:string='');
   procedure cm_OpenArchive(param:string='');
   procedure cm_OpenDirInNewTab(param:string='');
   procedure cm_Open(param:string='');
   procedure cm_OpenVFSList(param:string='');
   procedure cm_TargetEqualSource(param:string='');
   procedure cm_LeftEqualRight(param:string='');
   procedure cm_RightEqualLeft(param:string='');
   procedure cm_PackFiles(param: string='');
   procedure cm_ExtractFiles(param: string='');
   procedure cm_QuickSearch(param: string='');
   procedure cm_RightOpenDrives(param: string='');
   procedure cm_ShowButtonMenu(param: string='');
   procedure cm_TransferLeft(param: string='');
   procedure cm_TransferRight(param: string='');
   procedure cm_Minimize(param: string='');
   procedure cm_Wipe(param: string='');
   procedure cm_Exit(param: string='');
   procedure cm_NewTab(param: string='');
   procedure cm_RemoveTab(param: string='');
   procedure cm_RemoveAllTabs(param: string='');
   procedure cm_NextTab(param: string='');
   procedure cm_PrevTab(param: string='');
   procedure cm_ToggleLockTab(param: string='');
   procedure cm_ToggleLockDcaTab(param: string='');
   procedure cm_Copy(param: string='');
   procedure cm_Delete(param: string='');
   procedure cm_CheckSumCalc(param:string);
   procedure cm_CheckSumVerify(param:string);
   procedure cm_Edit(param: string='');
   procedure cm_MakeDir(param: string='');
   procedure cm_Rename(param: string='');
   procedure cm_View(param: string='');
   procedure cm_CopyNamesToClip(param: string='');
   procedure cm_FocusCmdLine(param: string='');
   procedure cm_FileAssoc(param: string='');
   procedure cm_HelpIndex(param: string='');
   procedure cm_Keyboard(param: string='');
   procedure cm_VisitHomePage(param: string='');
   procedure cm_About(param: string='');
   procedure cm_ShowSysFiles(param: string='');
   procedure cm_Options(param: string='');
   procedure cm_CompareContents(param: string='');
   procedure cm_Refresh(param: string='');
   procedure cm_ShowMenu(param: string='');
   procedure cm_DirHotList(param: string='');
   procedure cm_MarkInvert(param: string='');
   procedure cm_MarkMarkAll(param: string='');
   procedure cm_MarkUnmarkAll(param: string='');
   procedure cm_MarkPlus(param: string='');
   procedure cm_MarkMinus(param: string='');
   procedure cm_MarkCurrentExtension(param: string='');
   procedure cm_UnmarkCurrentExtension(param: string='');
   procedure cm_Search(param: string='');
   procedure cm_HardLink(param: string='');
   procedure cm_MultiRename(param: string='');
   procedure cm_ReverseOrder(param: string='');
   procedure cm_SortByAttr(param: string='');
   procedure cm_SortByDate(param: string='');
   procedure cm_SortByExt(param: string='');
   procedure cm_SortByName(param: string='');
   procedure cm_SortBySize(param: string='');
   procedure cm_SortByColumn(param: string='');
   procedure cm_SymLink(param: string='');
   procedure cm_CopySamePanel(param: string='');
   procedure cm_DirHistory(param: string='');
   procedure cm_EditNew(param: string='');
   procedure cm_RenameOnly(param: string='');
   procedure cm_RunTerm(param: string='');
   procedure cm_ShowCmdLineHistory(param: string='');
   procedure cm_CalculateSpace(param: string='');
   procedure cm_CountDirContent(param: string='');
   procedure cm_FileProperties(param: string='');
   procedure cm_FileLinker(param: string='');
   procedure cm_FileSpliter(param: string='');
   procedure cm_PanelsSplitterPerPos(param: string='');
   procedure cm_EditComment(param: string='');
   procedure cm_CopyToClipboard(param: string='');
   procedure cm_CutToClipboard(param: string='');
   procedure cm_PasteFromClipboard(param: string='');
   procedure cm_ChangeDirToRoot(param: string='');
   procedure cm_ChangeDir(param: string='');
   procedure cm_ClearLogWindow(param: string='');
   procedure cm_ClearLogFile(param: string='');

   //---------------------
   {   procedure SomeFunction (param:string; var Result:integer);
   procedure SomeProcedure(param:string);
   procedure Mess(param:string);}
   //---------------------
   property CommandList:TStrings read FCmdList; //be careful with these list's objects.
  end;

implementation

uses uLng,fMain,uGlobs,uFileList,uTypes,uShowMsg,uOSForms,Controls,
     Clipbrd,uOSUtils,uWCXmodule,fPackDlg,uWipeThread,
     fFileOpDlg,forms,uVFSutil,uShowForm,uDCUtils,uLog,uVFSTypes,
     fMkDir,LCLProc,uFileProcs,uDeleteThread,fFileAssoc,fExtractDlg,fAbout,
     fOptions,fCompareFiles,fFindDlg,fSymLink,fHardLink,fMultiRename, uHash,
     uSpaceThread,fLinker,fSplitter,uGlobsPaths, uClassesEx, fDescrEdit,
     HelpIntfs, dmHelpManager, uShellExecute, uClipboard, uCheckSumThread, fCheckSumCalc,
     uFileSorting, uFilePanelSelect, uFile, uFileSystemFileSource,
     uFileSystemCopyOperation, uOperationsManager, uFileSourceOperationTypes,
     uFileSourceOperation, uFileSystemDeleteOperation, uFileSystemWipeOperation,
     uFileSourceOperationMessageBoxesUI;

{ TActs }

class function TActs.Methods(AClass:TClass): TStringList;
//------------------------------------------------------
    type
       pmethodnamerec = ^tmethodnamerec;
       tmethodnamerec = packed record
          name : pshortstring;
          addr : pointer;
       end;

       tmethodnametable = packed record
         count : dword;
         entries : tmethodnamerec; // first entry
         // subsequent tmethodnamerec records follow
       end;

       pmethodnametable =  ^tmethodnametable;

var
 methodtable : pmethodnametable;
 i : dword;
 vmt : tclass;
 pentry: pmethodnamerec;

begin
   Result:=TStringList.Create;
   vmt:=AClass;
   while assigned(vmt) do
     begin
        methodtable:=pmethodnametable((Pointer(vmt)+vmtMethodTable)^);
        if assigned(methodtable) then
        begin
          pentry := @methodtable^.entries;
          for i:=0 to methodtable^.count-1 do
            Result.AddObject(pentry[i].name^,Tobject(AClass));
        end;
        vmt:=pclass(pointer(vmt)+vmtParent)^;
     end;
end;
//------------------------------------------------------


function TActs.GetList: TStrings;
begin
  Result:=Methods(Self.ClassType);
end;


constructor TActs.Create;
var
  i: Integer;
begin
  FCmdList:=GetList;
  FActionsState:=TStringHashList.Create(False); // not case-sensitive

  for i:=0 to FCmdList.Count - 1 do
    AddAction(Copy(FCmdList.Strings[i], 4,
                   Length(FCmdList.Strings[i]) - 3));
end;

destructor TActs.Destroy;
var
  i: Integer;
begin
  if Assigned(FCmdList) then FreeAndNil(FCmdList);
  if Assigned(FActionsState) then
  begin
    for i := 0 to FActionsState.Count - 1 do
      if Assigned(FActionsState.List[i]^.Data) then
        Dispose(PActionState(FActionsState.List[i]^.Data));

    FreeAndNil(FActionsState);
  end;
  inherited Destroy;
end;

function TActs.Execute(Cmd: string; param:string =''): integer;
var t:TMethod; ind:integer;
begin
    Result:=cf_Error;
    ind:={FCmdList.IndexOf(cmd);}GetIndex(Cmd);
    if ind=-1 then exit;
    if not Assigned(FCmdList.Objects[ind]) then exit;
    t.Data:=TClass(FCmdList.Objects[ind]).ClassInfo;
    t.Code:=TClass(FCmdList.Objects[ind]).MethodAddress(cmd);
    if Assigned(t.code) then
    begin
     Result:=cf_Null;
     TCommandFunc(t)(param);
    end;
end;


function TActs.GetIndex(Cmd: string): integer;
//------------------------------------------------------
    Function DoCompareText(const s1,s2 : string) : PtrInt;
      begin
        result:=CompareText(upcase(s1),upcase(s2));
      end;
    //---------------------
begin
  Result:=0;
  with FCmdList do
  begin
    While (Result<Count) and (DoCompareText(Strings[Result],Cmd)<>0) do Result:=Result+1;
    if Result=Count then Result:=-1;
  end;
end;

function TActs.GetCategoriesList(const List: TStrings): integer;
var s:string; i,p:integer;
begin
  List.Clear;
  for i:=0 to CommandList.Count-1 do
    begin
     s:=CommandList[i];
     //find forms
     if Pos('cm_',s)>0 then
       begin
         s:=copy(s,4,length(s)-3);
         p:=pos('_',s);
         if p>0 then
           begin
             s:=copy(s,1,p-1);
             if list.IndexOf(s)=-1 then
               List.Add(s);
           end
         else
           begin
            if list.IndexOf('Main')=-1 then
              List.Add('Main');
           end;
       end;
    end;
  result:=List.Count;
end;

function TActs.GetCommandsByCategory(Category: string; const List: TStrings
  ): integer;
var i:integer; s:string;
begin
  List.Clear;
  if Category='Main' then
    begin
    for i:=0 to CommandList.Count-1 do
      begin
       s:=CommandList[i];
       delete(s,1,3);
       if pos('_',s)=0 then
         List.Add(CommandList[i]);
      end;
    end
  else
    begin
      s:='cm_'+Category+'_';
      for i:=0 to CommandList.Count-1 do
       if pos(s,CommandList[i])>0 then
         List.Add(CommandList[i]);
    end;

  Result:=List.Count;
end;

function TActs.GetCommandCaption(sCommand: String; bAmpersand: Boolean): UTF8String;
//< find Comment for command
// command=caption of action assigned to command
var
  myAct: TContainedAction;
  lstr: String;
begin
  Result:= '';
  with frmMain.actionLst do
  begin
    lstr:= Copy(sCommand, 4, Length(sCommand) - 3);// get action name
    myAct:= ActionByName('act' + lstr); // get action
    if (myAct <> nil) and (myAct is TAction) then // if action exist and action is TAction. its Need?
      begin
        lstr:= (myAct as TAction).Caption; //copy caption
        if not bAmpersand then
          while pos('&', lstr) <> 0 do Delete(lstr, Pos('&', lstr), 1); //delete all ampersand
        Result:= lstr;
      end;
  end;
end;

procedure TActs.AddAction(ActionName: String);
var
  ActionState: PActionState;
begin
  if FActionsState.Find(ActionName) = -1 then
  begin
    New(ActionState);

    if Assigned(ActionState) then
    try
      ActionState^.Enabled := True;
      ActionState^.Action := nil;

      FActionsState.Add(ActionName, ActionState);
    except
      Dispose(ActionState);
    end;
  end;
end;

procedure TActs.AddAction(Action: TAction);
var
  ActionState: PActionState;
  ActionNameWithoutPrefix: string;
  Index: Integer;
begin
  ActionNameWithoutPrefix := Copy(Action.Name, 4, Length(Action.Name) - 3);

  Index := FActionsState.Find(ActionNameWithoutPrefix);
  if Index = -1 then
  begin
    New(ActionState);

    if Assigned(ActionState) then
    try
      ActionState^.Enabled := True;
      ActionState^.Action := Action;

      FActionsState.Add(ActionNameWithoutPrefix, ActionState);
    except
      Dispose(ActionState);
    end;
  end
  else
  begin
    // Action already exists. Update TAction reference.
    PActionState(FActionsState.List[Index]^.Data)^.Action := Action;
  end;
end;

procedure TActs.EnableAction(ActionState: PActionState; Enabled: Boolean);
begin
  if Assigned(ActionState) then
  begin
    ActionState^.Enabled := Enabled;
    if Assigned(ActionState^.Action) then
      ActionState^.Action.Enabled := Enabled;
  end;
end;

procedure TActs.EnableAction(ActionName: String; Enable: Boolean);
var
  ActionState: PActionState;
begin
  ActionState := FActionsState[ActionName];
  if Assigned(ActionState) then
    EnableAction(ActionState, Enable)
  else
    raise Exception.Create('Invalid Action Name: ' + ActionName);
end;

function TActs.IsActionEnabled(ActionName: String): Boolean;
var
  ActionState: PActionState;
begin
  ActionState := FActionsState[ActionName];
  if Assigned(ActionState) then
    Result := ActionState^.Enabled
  else
    raise Exception.Create('Invalid Action Name: ' + ActionName);
end;

procedure TActs.EnableAllActions(Enable: Boolean);
var
  i: Integer;
begin
  for i := 0 to FActionsState.Count - 1 do
    EnableAction(PActionState(FActionsState.List[i]^.Data), Enable);
end;

//------------------------------------------------------

procedure TActs.DoRemoveTab(Notebook: TFileViewNotebook; PageIndex: Integer);
begin
  with frmMain do
  begin
    RemovePage(Notebook, PageIndex);
    ActiveFrame.SetFocus;
  end;
end;

procedure TActs.DoToggleLockTab(Tab: TFileViewPage);
begin
  with frmMain do
  begin
    if Tab.LockState <> tlsLockedPath then  // lock
      begin
        Tab.LockState := tlsLockedPath;
      end
    else // unlock
      begin
        Tab.LockState := tlsNormal;
      end;

    ActiveFrame.SetFocus;
  end;
end;

procedure TActs.DoToggleLockDcaTab(Tab: TFileViewPage);
begin
  with frmMain do
  begin
    if Tab.LockState <> tlsResettingPath then  // lock
      begin
        Tab.LockState := tlsResettingPath;
        Tab.LockPath := ActiveFrame.CurrentPath;
      end
    else // unlock
      begin
        Tab.LockState := tlsNormal;
        Tab.LockPath := '';
      end;

    ActiveFrame.SetFocus;
  end;
end;

procedure TActs.DoCopySelectedFileNamesToClipboard(FileView: TFileView; FullNames: Boolean);
var
  I: Integer;
  sl: TStringList = nil;
  SelectedFiles: TFiles = nil;
  PathToAdd: String;
begin
  SelectedFiles := FileView.SelectedFiles;
  try
    if SelectedFiles.Count > 0 then
    begin
      sl := TStringList.Create;
      for I := 0 to SelectedFiles.Count - 1 do
      begin
        if FullNames then
          PathToAdd := FileView.CurrentAddress
                     + FileView.CurrentPath
        else
          PathToAdd := '';

        sl.Add(PathToAdd + SelectedFiles[I].Name);
      end;

      Clipboard.Clear;   // prevent multiple formats in Clipboard (specially synedit)
      Clipboard.AsText := sl.Text;
    end;

  finally
    if Assigned(sl) then
      FreeAndNil(sl);
    if Assigned(SelectedFiles) then
      FreeAndNil(SelectedFiles);
  end;
end;

procedure TActs.DoNewTab(Notebook: TFileViewNotebook);
var
  NewPage: TFileViewPage;
begin
  NewPage := Notebook.AddPage;
  Notebook.ActiveView.Clone(NewPage);
  NewPage.MakeActive;
  NewPage.UpdateCaption(GetLastDir(ExcludeTrailingPathDelimiter(NewPage.FileView.CurrentPath)));
end;

//------------------------------------------------------
//Published methods
//------------------------------------------------------

procedure TActs.cm_AddPathToCmdLine(param:string);
var
  OldPosition: Integer;
begin
  with frmMain do
    begin
      OldPosition := edtCommand.SelStart;
      edtCommand.Text := edtCommand.Text + (ActiveFrame.CurrentPath);
      edtCommand.SelStart := OldPosition + Length(ActiveFrame.CurrentPath);
    end;
end;

procedure TActs.cm_AddFilenameToCmdLine(param: string='');
var
  AddedString: String;
  OldPosition: Integer;
  aFile: TFile;
begin
  with frmMain do
    begin
      aFile := ActiveFrame.ActiveFile;
      if Assigned(aFile) then
      begin
        OldPosition := edtCommand.SelStart;
        AddedString := aFile.Name + ' ';
        edtCommand.Text := edtCommand.Text + AddedString;
        edtCommand.SelStart := OldPosition + Length(AddedString);
      end;
    end;
end;

procedure TActs.cm_AddPathAndFilenameToCmdLine(param: string='');
var
  AddedString: String;
  OldPosition: Integer;
  aFile: TFile;
begin
  with frmMain do
    begin
      aFile := ActiveFrame.ActiveFile;
      if Assigned(aFile) then
      begin
        AddedString := ActiveFrame.CurrentPath;

        if aFile.Name = '..' then
        begin
          AddedString := AddedString + ' ';
        end
        else
        begin
          AddedString := AddedString + aFile.Name + ' ';
        end;

        OldPosition := edtCommand.SelStart;
        edtCommand.Text := edtCommand.Text + AddedString;
        edtCommand.SelStart := OldPosition + Length(AddedString);
      end;
    end;
end;

procedure TActs.cm_ContextMenu(param:string);
var
  fl: TFileList;
  Point: TPoint;
  Rect: TRect;
  SelectedFiles: TFiles;
begin
  //frmMain.ActiveFrame.ExecuteCommand('cm_ContextMenu', param);

  // Temporarily work for Filesystem only.
  with frmMain do
  begin
    if not (ActiveFrame.FileSource is TFileSystemFileSource) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    SelectedFiles := ActiveFrame.SelectedFiles;
    try
      if SelectedFiles.Count > 0 then
      begin
        if param  = 'OnMouseClick' then
          ShowContextMenu(frmMain, SelectedFiles, Mouse.CursorPos.x, Mouse.CursorPos.y)
        else
          begin
            Point.X := 0;
            Point.Y := 0;
            {Rect:= dgPanel.CellRect(0, dgPanel.Row);
            Point.X:= Rect.Left + ((Rect.Right - Rect.Left) div 2);
            Point.Y:= Rect.Top + ((Rect.Bottom - Rect.Top) div 2);
            Point:= dgPanel.ClientToScreen(Point);}
            ShowContextMenu(frmMain, SelectedFiles, Point.X, Point.Y)
          end;
      end;

      SelectedFiles := nil;  // freed by ShowContextMenu

    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end;
end;

procedure TActs.cm_DriveContextMenu(param: string);
begin
  if param <> '' then
    begin
      ShowDriveContextMenu(frmMain, param, Mouse.CursorPos.x, Mouse.CursorPos.y);
    end;
end;

procedure TActs.cm_CopyFullNamesToClip(param:string);
begin
  DoCopySelectedFileNamesToClipboard(frmMain.ActiveFrame, True);
end;

procedure TActs.cm_CopyNamesToClip(param:string);
begin
  DoCopySelectedFileNamesToClipboard(frmMain.ActiveFrame, False);
end;

//------------------------------------------------------
procedure TActs.cm_Exchange(param:string);
var
  sDir: String;
begin
  sDir:= FrmMain.ActiveFrame.CurrentPath;
  FrmMain.ActiveFrame.CurrentPath:= FrmMain.NotActiveFrame.CurrentPath;
  FrmMain.NotActiveFrame.CurrentPath:= sDir;
end;

procedure TActs.cm_LeftOpenDrives(param:string);
var
  p : TPoint;
begin
 with FrmMain do
 begin
  pmDrivesMenu.Tag := 0;  // indicate that is left panel menu
  p := Classes.Point(btnLeftDrive.Left,btnLeftDrive.Height);
  p := pnlLeftTools.ClientToScreen(p);
  UpdateDriveButtonMenuSelection(btnLeftDrive, FrameLeft.CurrentPath);
  pmDrivesMenu.PopUp(p.x, p.y);
 end;
end;

procedure TActs.cm_OpenDirInNewTab(param:string);
var
  NewPage: TFileViewPage;
  NewPath: String;
  aFile: TFile;
begin
  with FrmMain do
  begin
    aFile := ActiveFrame.ActiveFile;
    if Assigned(aFile) and aFile.IsNameValid and
       (aFile.IsDirectory or aFile.IsLinkToDirectory) then
    begin
      NewPath := ActiveFrame.CurrentPath + aFile.Name;
      NewPage := ActiveNotebook.AddPage;
      ActiveFrame.Clone(NewPage);
      NewPage.FileView.CurrentPath := NewPath;
      if tb_open_new_in_foreground in gDirTabOptions then
        NewPage.MakeActive;
    end;
  end;
end;

procedure TActs.cm_TargetEqualSource(param:string);
var
  NewPage: TFileViewPage;
begin
  with frmMain do
  begin
    NotActiveNotebook.ActivePage.FileView := nil;
    ActiveFrame.Clone(NotActiveNotebook.ActivePage);
    NotActiveNotebook.ActivePage.UpdateCaption(GetLastDir(
      ExcludeTrailingPathDelimiter(NotActiveNotebook.ActivePage.FileView.CurrentPath)));
  end;
end;

procedure TActs.cm_LeftEqualRight(param: string);
var
  NewPage: TFileViewPage;
begin
  with frmMain do
  begin
    LeftTabs.ActivePage.FileView := nil;
    FrameRight.Clone(LeftTabs.ActivePage);
    LeftTabs.ActivePage.UpdateCaption(GetLastDir(ExcludeTrailingPathDelimiter(LeftTabs.ActivePage.FileView.CurrentPath)));
  end;
end;

procedure TActs.cm_RightEqualLeft(param: string);
var
  NewPage: TFileViewPage;
begin
  with frmMain do
  begin
    RightTabs.ActivePage.FileView := nil;
    FrameLeft.Clone(RightTabs.ActivePage);
    RightTabs.ActivePage.UpdateCaption(GetLastDir(ExcludeTrailingPathDelimiter(RightTabs.ActivePage.FileView.CurrentPath)));
  end;
end;

procedure TActs.cm_OpenArchive(param:string);
begin
{
  with frmMain.ActiveFrame.pnlFile do
  begin
    if IsItemValid(GetActiveItem) then
      TryOpenArchive(GetActiveItem);
  end;
}
end;

procedure TActs.cm_Open(param:string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_Open', param);
end;

procedure TActs.cm_OpenVFSList(param:string);
begin
{
  FrmMain.ActiveFrame.pnlFile.LoadVFSListInPanel;
}
end;

//------------------------------------------------------
(* Pack files in archive *)
procedure TActs.cm_PackFiles(param:string);
var
  fl : TFileList;
  Result: Boolean;
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_PackFiles', param);
{
with frmMain do
begin
  Result:= False;
  if not IsBlocked then
    begin
      with FrmMain.ActiveFrame do
        begin
          if SelectFileIfNoSelected(GetActiveItem) = False then Exit;

          fl:=TFileList.Create;
          CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir);

          fl.CurrentDirectory := ActiveDir;
        end;
      try
        Result:= ShowPackDlg(FrmMain.NotActiveFrame.pnlFile.VFS, fl, FrmMain.NotActiveFrame.CurrentPath);
      finally
        if Result then
          begin
            frameLeft.RefreshPanel;
            frameRight.RefreshPanel;
          end
        else
          begin
            with FrmMain.ActiveFrame do
              UnSelectFileIfSelected(GetActiveItem);
          end;
      end;
    end;  // IsBlocked
 end;
}
end;

procedure TActs.cm_ExtractFiles(param:string);
var
  fl : TFileList;
  Result: Boolean;
begin
// This command will probably be removed?
// Because extracting files will be simply copying from archive file source.
{
with frmMain do
begin
  Result:= False;
  if not IsBlocked then
    begin
      with ActiveFrame do
        begin
          if SelectFileIfNoSelected(GetActiveItem) = False then Exit;

          fl:=TFileList.Create;
          CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir);

          fl.CurrentDirectory := ActiveDir;
        end;
      try
        Result:= ShowExtractDlg(ActiveFrame, fl, NotActiveFrame.CurrentPath);
      finally
        if Result then
          begin
            frameLeft.RefreshPanel;
            frameRight.RefreshPanel;
          end
        else
          begin
            with ActiveFrame do
	      UnSelectFileIfSelected(GetActiveItem);
          end;
      end;
    end;  // IsBlocked
end;
}
end;

procedure TActs.cm_QuickSearch(param:string);
begin
  FrmMain.ActiveFrame.ExecuteCommand('cm_QuickSearch', param);
end;

procedure TActs.cm_RightOpenDrives(param:string);
var
  p : TPoint;
begin
  with frmMain do
  begin
    pmDrivesMenu.Tag := 1;  // indicate that is right panel menu
    p := Classes.Point(btnRightDrive.Left,btnRightDrive.Height);
    p := pnlRightTools.ClientToScreen(p);
    UpdateDriveButtonMenuSelection(btnRightDrive, FrameRight.CurrentPath);
    pmDrivesMenu.PopUp(p.x, p.y);
  end;
end;

procedure TActs.cm_ShowButtonMenu(param:string);
var
  Point: TPoint;
  IniFile: TIniFileEx;
begin
  with frmMain do
  begin
    if Pos(PathDelim, param) <> 0 then
      IniFile:= TIniFileEx.Create(GetCmdDirFromEnvVar(param))
    else
      IniFile:= TIniFileEx.Create(gpIniDir + param);
    pmButtonMenu.LoadFromIniFile(IniFile);
    IniFile.Free;
    Point:=MainToolBar.ClientToScreen(Classes.Point(0,0));
    Point.Y:=Point.Y+MainToolbar.Height;
    Point.X:=mouse.CursorPos.X-60;
    pmButtonMenu.PopUp(Point.x,Point.Y);
  end;
end;

procedure TActs.cm_TransferLeft(param:string);
begin
  if (frmMain.SelectedPanel = fpRight) then
    frmMain.SetNotActFrmByActFrm;
end;

procedure TActs.cm_TransferRight(param:string);
begin
  if (frmMain.SelectedPanel = fpLeft) then
    frmMain.SetNotActFrmByActFrm;
end;

procedure TActs.cm_Minimize(param:string);
begin
	FrmMain.MinimizeWindow;
end;

procedure TActs.cm_Wipe(param:string);
var
  theFilesToWipe: TFiles;
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
begin
  with frmMain.ActiveFrame do
  begin
    if not (fsoWipe in FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    // ------------------------------------------------------

    theFilesToWipe := SelectedFiles; // free at Thread end by thread

    if Assigned(theFilesToWipe) then
    try
      if theFilesToWipe.Count = 0 then
        Exit;

      if not msgYesNo(frmMain.GetFileDlgStr(rsMsgWipeSel, rsMsgWipeFlDr, theFilesToWipe)) then
        Exit;

      Operation := FileSource.CreateWipeOperation(theFilesToWipe);

      if Assigned(Operation) then
      begin
        // Start operation.
        OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);

        ProgressDialog := TfrmFileOp.Create(OperationHandle);
        ProgressDialog.Show;
      end
      else
      begin
        msgWarning(rsMsgNotImplemented);
      end;

    finally
      if Assigned(theFilesToWipe) then
        FreeAndNil(theFilesToWipe);
    end;
  end;
end;

procedure TActs.cm_Exit(param:string);
begin
  frmMain.Close; // application.Terminate not save settings.
end;

procedure TActs.cm_NewTab(param:string);
var
  PanelSelected: TFilePanelSelect;
begin
  DoNewTab(frmMain.ActiveNotebook);
end;

procedure TActs.cm_RemoveTab(param:string);
begin
  with frmMain do
    DoRemoveTab(ActiveNotebook, ActiveNotebook.PageIndex);
end;

procedure TActs.cm_RemoveAllTabs(param: string);
var
  I: Integer;
  ANotebook: TFileViewNotebook;
begin
  with frmMain do
  begin
    if (tb_confirm_close_all in gDirTabOptions) then
      if not msgYesNo(rsMsgCloseAllInActiveTabs) then Exit;

    if param = 'LeftTabs' then
      ANotebook := LeftTabs
    else if param = 'RightTabs' then
      ANotebook := RightTabs
    else
      ANotebook := ActiveNotebook;

    for I := ANotebook.PageCount - 1 downto 0 do
      if I <> ANotebook.PageIndex then
        case RemovePage(ANotebook, I) of
          1: Continue; // skip tab
          2: Break;    // cancel operation
        end;

    ActiveFrame.SetFocus;
  end;
end;

procedure TActs.cm_NextTab(param: string);
begin
  frmMain.ActiveNotebook.ActivateNextTab;
end;

procedure TActs.cm_PrevTab(param: string);
begin
  frmMain.ActiveNotebook.ActivatePrevTab;
end;

procedure TActs.cm_ToggleLockTab(param: string);
var
  nbNoteBook: TFileViewNotebook;
begin
  with frmMain do
  begin
    if param = 'LeftTabs' then
      nbNoteBook := LeftTabs
    else if param = 'RightTabs' then
      nbNoteBook := RightTabs
    else
      nbNoteBook := ActiveNotebook;

    DoToggleLockTab(nbNoteBook.ActivePage);
  end;
end;

procedure TActs.cm_ToggleLockDcaTab(param: string);
var
  nbNoteBook: TFileViewNotebook;
begin
  with frmMain do
  begin
    if param = 'LeftTabs' then
      nbNoteBook := LeftTabs
    else if param = 'RightTabs' then
      nbNoteBook := RightTabs
    else
      nbNoteBook := ActiveNotebook;

    DoToggleLockDcaTab(nbNoteBook.ActivePage);
  end;
end;

//------------------------------------------------------

procedure TActs.cm_View(param:string);
var
  sl: TStringList = nil;
  i: Integer;
  sViewCmd,
  sFileName,
  sFilePath: String;
  bDeleteAfterView: Boolean;
  SelectedFiles: TFiles = nil;
  aFile: TFile;
begin
  with frmMain do
  // For now only works for FileSystem.
  if ActiveFrame.FileSource is TFileSystemFileSource then
  begin
    sl := TStringList.Create;
    try
      SelectedFiles := ActiveFrame.SelectedFiles;

      for i := 0 to SelectedFiles.Count - 1 do
      begin
        aFile := SelectedFiles[i];

        if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
        begin
          if (log_info in gLogOptions) then
            logWrite('View.Add: ' + ActiveFrame.CurrentPath + aFile.Name, lmtInfo);

          //now test if exists View command in doublecmd.ext :)
          sViewCmd:= gExts.GetExtActionCmd(aFile, 'view');

{
 // Use TemporaryFileSystem for this when it's done.

          case pnlFile.PanelMode of
          pmArchive, pmVFS: // if in Virtual File System
            begin
              VFSFileList:= TFileList.Create;
              VFSFileList.CurrentDirectory := ActiveDir;
              sFileName:= ActiveDir + fr^.sName;
              New(fr);
              fr^.sName:= sFileName;
              fr^.iMode:= 0;
              fr^.sPath:= GetTempFolder;
              VFSFileList.AddItem(fr);
              {if }pnlFile.VFS.VFSmodule.VFSCopyOut(VFSFileList, fr^.sPath, 0);{ then}
                begin
                  if (sViewCmd<>'') then
                    begin
                      ReplaceExtCommand(sViewCmd, fr, pnlFile.CurrentPath);
                      ProcessExtCommand(sViewCmd, pnlFile.CurrentPath);
                    end
                  else
                    begin
                      sl.Add(fr^.sPath + ExtractDirLevel(ActiveDir, fr^.sName));
                      bDeleteAfterView:= True;
                    end;
                 Dispose(fr);
                end;
            end;
}
          if (sViewCmd<>'') then
            begin
              ReplaceExtCommand(sViewCmd, aFile);
              ProcessExtCommand(sViewCmd, ActiveFrame.CurrentPath);
            end
          else
            begin
              sFileName := aFile.Name;
              sFilePath := ActiveFrame.CurrentPath; // aFile.Path;
              sl.Add(GetSplitFileName(sFileName, sFilePath));
              bDeleteAfterView:= False;
            end;
        end; // if selected
      end; // for

      // if sl has files then view it
      if sl.Count > 0 then
        ShowViewerByGlobList(sl, bDeleteAfterView)
      else
        begin
          // Enter directories using View command.
          aFile := ActiveFrame.ActiveFile;
          if Assigned(aFile) and (aFile.IsDirectory or
                                  aFile.IsLinkToDirectory) then
            begin
              ActiveFrame.ExecuteCommand('cm_Open');
              // or change ActiveFrame.CurrentPath directly?
              // or use GoDownLevel (GoToSubDir(aFile)) command or similar?
            end
        end;

    finally
      if Assigned(sl) then
        FreeAndNil(sl);
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end
  else
    msgWarning(rsMsgNotImplemented);
end;

procedure TActs.cm_Edit(param:string);
var
  i: Integer;
  sEditCmd,
  sFileName,
  sFilePath : String;
  aFile: TFile;
  SelectedFiles: TFiles;
begin
  with frmMain do
  // For now only works for FileSystem.
  if ActiveFrame.FileSource is TFileSystemFileSource then
  begin
    SelectedFiles := ActiveFrame.SelectedFiles;
    try
      for i := 0 to SelectedFiles.Count - 1 do
      begin
        aFile := SelectedFiles[i];

        // For now we only process one file.
        if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
        begin
          //now test if exists View command in doublecmd.ext :)
          sEditCmd:= gExts.GetExtActionCmd(aFile, 'edit');

          if (sEditCmd<>'') then
            begin
              ReplaceExtCommand(sEditCmd, aFile);
              ProcessExtCommand(sEditCmd, ActiveFrame.CurrentPath);
            end
          else
            begin
              sFileName := aFile.Name;
              sFilePath := ActiveFrame.CurrentPath;
              ShowEditorByGlob(GetSplitFileName(sFileName, sFilePath));
            end;
          Break;
        end;
      end;

    finally
      if Assigned(SelectedFiles) then
        FreeAndNil(SelectedFiles);
    end;
  end
  else
    msgWarning(rsMsgNotImplemented);
end;

procedure TActs.cm_Copy(param:string);
begin
  // Selection validation in CopyFile.
  frmMain.CopyFile(frmMain.NotActiveFrame.CurrentPath);
end;

procedure TActs.cm_Rename(param:string);
begin
{
  File source operation.

  // Selection validation in RenameFile.
  frmMain.RenameFile(frmMain.NotActiveFrame.CurrentPath);
}
end;

procedure TActs.cm_MakeDir(param:string);
var
  sPath:String;
  Operation: TFileSourceOperation;
  UI: TFileSourceOperationMessageBoxesUI = nil;
begin
  with frmMain do
  begin
    if not (fsoCreateDirectory in ActiveFrame.FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    sPath := ActiveFrame.ActiveFile.Name;         // 21.05.2009 - pass name from cursor to makedir form
    if not frmMkDir.ShowMkDir(sPath) then Exit;   // show makedir dialog
    if (sPath='') then Exit;

    Operation := ActiveFrame.FileSource.CreateCreateDirectoryOperation(sPath);
    if Assigned(Operation) then
    begin
      try
        // Call directly - not through operations manager.
        UI := TFileSourceOperationMessageBoxesUI.Create;
        Operation.AddUserInterface(UI);
        Operation.Execute;
        ActiveFrame.Reload;
        //sPath := ExtractFileName(ExcludeTrailingPathDelimiter(sPath));
        //ActiveFrame.Select(sPath);
      finally
        FreeAndNil(Operation);
        FreeAndNil(UI);
      end;
    end;
  end;
end;

procedure TActs.cm_Delete(param:string);
var
  theFilesToDelete: TFiles;
  // 12.05.2009 - if delete to trash, then show another messages
  MsgDelSel, MsgDelFlDr : string;
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
begin
  with frmMain.ActiveFrame do
  begin
    if not (fsoDelete in FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit;
    end;

    // 12.05.2009
    // Showing delete dialog: to trash or to /dev/null :)
    If (param = 'recycle') then
     begin
      MsgDelSel := rsMsgDelSelT;
      MsgDelFlDr := rsMsgDelFlDrT;
     end
    else
     begin
      MsgDelSel := rsMsgDelSel;
      MsgDelFlDr := rsMsgDelFlDr;
     end;

    // ------------------------------------------------------

    theFilesToDelete := SelectedFiles; // free at Thread end by thread

    if Assigned(theFilesToDelete) then
    try
      if theFilesToDelete.Count = 0 then
        Exit;

      if not msgYesNo(frmMain.GetFileDlgStr(MsgDelSel, MsgDelFlDr, theFilesToDelete)) then
        Exit;

      Operation := FileSource.CreateDeleteOperation(theFilesToDelete);

      if Assigned(Operation) then
      begin
        // Special case for filesystem - 'recycle' parameter.
        if Operation is TFileSystemDeleteOperation then
          with Operation as TFileSystemDeleteOperation do
          begin
            // 30.04.2009 - передаем параметр корзины в поток.
            if param = 'recycle' then
              Recycle := True
          end;

        // Start operation.
        OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);

        ProgressDialog := TfrmFileOp.Create(OperationHandle);
        ProgressDialog.Show;
      end
      else
      begin
        msgWarning(rsMsgNotImplemented);
      end;

    finally
      if Assigned(theFilesToDelete) then
        FreeAndNil(theFilesToDelete);
    end;
  end;
end;

procedure TActs.cm_CheckSumCalc(param:string);
var
  fl: TFileList;
  I: Integer;
  bSeparateFile: Boolean;
  HashAlgorithm: THashAlgorithm;
  sFileName: UTF8String;
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_CheckSumCalc', param);
{
  with frmMain.ActiveFrame do
  begin
    if  pnlFile.PanelMode <> pmDirectory then Exit;

    if SelectFileIfNoSelected(GetActiveItem) = False then Exit;

    bSeparateFile:= False;
    with pnlFile.FileList do
    for I:= Count - 1 downto 0 do // find files in selection
      if GetItem(I)^.bSelected and not FPS_ISDIR(GetItem(I)^.iMode) then
        begin
          bSeparateFile:= True;
          Break;
        end;
    if not bSeparateFile then // if selected only directories
      begin
        msgError(rsMsgNoFilesSelected);
        Exit;
      end;

    if pnlFile.GetSelectedCount > 1 then
      sFileName:= ActiveDir + MakeFileName(ActiveDir, 'checksum')
    else
      sFileName:= ActiveDir + GetActiveItem^.sName;

    if not ShowCalcCheckSum(sFileName, bSeparateFile, HashAlgorithm) then Exit;

    fl:= TFileList.Create; // free at thread end by thread
    fl.CurrentDirectory := ActiveDir;
    try
      CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir, True, True);

      // calculate check sum
      with TCheckSumThread.Create(fl) do
      try
        sDstPath:= ActiveDir;
        sDstMask:= sFileName;
        CheckSumOp:= checksum_calc;
        OneFile:= not bSeparateFile;
        Algorithm:= HashAlgorithm;
        Resume;
      except
        Free;
      end;

    except
      FreeAndNil(fl);
    end;
  end;
}
end;

procedure TActs.cm_CheckSumVerify(param:string);
var
  fl: TFileList;
  I: Integer;
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_CheckSumVerify', param);
  with frmMain.ActiveFrame do
  begin
{
    if  pnlFile.PanelMode <> pmDirectory then Exit;

    if SelectFileIfNoSelected(GetActiveItem) = False then Exit;

    with pnlFile.FileList do
    for I:= Count - 1 downto 0 do
      if GetItem(I)^.bSelected and (mbCompareText(GetItem(I)^.sExt, '.md5') <> 0) and
         (mbCompareText(GetItem(I)^.sExt, '.sha') <> 0) then
        begin
          msgError(rsMsgSelectOnlyCheckSumFiles);
          Exit;
        end;

    fl:= TFileList.Create; // free at thread end by thread
    fl.CurrentDirectory := ActiveDir;
    try
      CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir, True, True);

      // verify check sum
      with TCheckSumThread.Create(fl) do
      try
        sDstPath:= ActiveDir;
        CheckSumOp:= checksum_verify;
        Resume;
      except
        Free;
      end;

    except
      FreeAndNil(fl);
    end;
}
  end;
end;

procedure TActs.cm_FocusCmdLine(param:string);
begin
  // Show temporarily command line on user request.
  if (not gCmdLine) and (frmMain.IsCommandLineVisible = False) then
  begin
    frmMain.pnlCommand.Show;
  end;

  frmMain.edtCommand.SetFocus;
end;

procedure TActs.cm_FileAssoc(param:string);
begin
  ShowFileAssocDlg;
end;

procedure TActs.cm_HelpIndex(param: string='');
begin
  ShowHelpOrErrorForKeyword('', gHelpLang+'/index.html');
end;

procedure TActs.cm_Keyboard(param: string='');
begin
  ShowHelpOrErrorForKeyword('', gHelpLang+'/shortcuts.html');
end;

procedure TActs.cm_VisitHomePage(param: string='');
var
  ErrMsg: String;
begin
  dmHelpMgr.HTMLHelpDatabase.ShowURL('http://doublecmd.sourceforge.net','Double Commander Web Site', ErrMsg);
end;

procedure TActs.cm_About(param:string);
begin
  ShowAboutBox;
end;

procedure TActs.cm_ShowSysFiles(param:string);
begin
with frmMain do
begin
  uGlobs.gShowSystemFiles:=not uGlobs.gShowSystemFiles;
  actShowSysFiles.Checked:=uGlobs.gShowSystemFiles;
//repaint both panels
  FrameLeft.Reload;
  FrameRight.Reload;
end;
end;

procedure TActs.cm_Options(param:string);
var ind:integer;
begin
  inherited;
  with TfrmOptions.Create(Application) do
  begin
    try
      if TryStrToInt(param,ind) then
      Tag:=Ind;// else Tag:=0;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TActs.cm_CompareContents(param:string);
var
  FilesToCompare: TStringList = nil;
  DirsToCompare: TStringList = nil;

  procedure AddItem(const pItem: PFileRecItem; Directory: String);
  begin
    if not FPS_ISDIR(pItem^.iMode) then
      FilesToCompare.Add(Directory + pItem^.sName)
    else
      DirsToCompare.Add(Directory + pItem^.sName);
  end;

  function FormatCommand(CompareList: TStringList): String;
  var
    i : Integer;
  begin
    Result := '"' + gExtDiff + '"';
    for i := 0 to CompareList.Count - 1 do
      Result := Result + ' "' + CompareList.Strings[i] + '"';
  end;

var
  i : Integer;
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_CompareContents', param);
{
  with frmMain do
  begin
    try
      FilesToCompare := TStringList.Create;
      DirsToCompare := TStringList.Create;

      if param = 'dir' then
      begin
        if (not FrameLeft.IsEmpty) and (not FrameRight.IsEmpty) then
        begin
          DirsToCompare.Add(FrameLeft.CurrentPath);
          DirsToCompare.Add(FrameRight.CurrentPath);
        end;
      end
      else if ActiveFrame.pnlFile.SelectedCount = 1 then
      begin
        if NotActiveFrame.pnlFile.SelectedCount = 1 then
        begin
          { compare single selected files in both panels }

          AddItem(ActiveFrame.pnlFile.GetFirstSelectedItem, ActiveFrame.CurrentPath);
          AddItem(NotActiveFrame.pnlFile.GetFirstSelectedItem, NotActiveFrame.CurrentPath);
        end
        else
        begin
          // Only one file selected in active panel.
          MsgWarning(rsMsgInvalidSelection);
          Exit;
        end;
      end
      else if ActiveFrame.pnlFile.SelectedCount > 1 then
      begin
        { compare all selected files in active frame }

        for i := 0 to ActiveFrame.pnlFile.FileList.Count - 1 do
          if ActiveFrame.pnlFile.FileList.GetItem(i)^.bSelected then
            AddItem(ActiveFrame.pnlFile.FileList.GetItem(i), ActiveFrame.CurrentPath);
      end
      else if FrameLeft.IsActiveItemValid and FrameRight.IsActiveItemValid then
      begin
        { no files selected in the active panel }
        { compare ActiveItems in both panels }

        AddItem(FrameLeft.pnlFile.GetActiveItem, FrameLeft.CurrentPath);
        AddItem(FrameRight.pnlFile.GetActiveItem, FrameRight.CurrentPath);
      end;

      if ((FilesToCompare.Count > 0) and (DirsToCompare.Count > 0))
      or ((FilesToCompare.Count = 1) or (DirsToCompare.Count = 1)) then
      begin
         // Either files or directories must be selected and more than one.
         MsgWarning(rsMsgInvalidSelection)
      end
      else if FilesToCompare.Count > 0 then
      begin
        if gUseExtDiff then
          ExecCmdFork(FormatCommand(FilesToCompare))
        else if FilesToCompare.Count = 2 then
          ShowCmpFiles(FilesToCompare.Strings[0], FilesToCompare.Strings[1])
        else
          MsgWarning(rsMsgTooManyFilesSelected);
      end
      else if DirsToCompare.Count > 0 then
      begin
        if gUseExtDiff then
          ExecCmdFork(FormatCommand(DirsToCompare))
        else
          MsgWarning(rsMsgNotImplemented);
      end
      else
        msgWarning(rsMsgNoFilesSelected);

    finally
      if Assigned(FilesToCompare) then
        FreeAndNil(FilesToCompare);
      if Assigned(DirsToCompare) then
        FreeAndNil(DirsToCompare);
    end;
  end;
}
end;


procedure TActs.cm_ShowMenu(param:string);
begin
  // Show main menu on demand when hidden?
end;

procedure TActs.cm_Refresh(param:string);
begin
  frmMain.ActiveFrame.Reload;
end;

//------------------------------------------------------
  
procedure TActs.cm_MarkInvert(param:string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkInvert', param);
end;

procedure TActs.cm_MarkMarkAll(param:string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkMarkAll', param);
end;

procedure TActs.cm_MarkUnmarkAll(param:string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkUnmarkAll', param);
end;

procedure TActs.cm_MarkPlus(param:string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkPlus', param);
end;

procedure TActs.cm_MarkMinus(param:string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkMinus', param);
end;

procedure TActs.cm_MarkCurrentExtension(param: string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_MarkCurrentExtension', param);
end;

procedure TActs.cm_UnmarkCurrentExtension(param: string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_UnmarkCurrentExtension', param);
end;

//------------------------------------------------------

procedure TActs.cm_DirHotList(param:string);
var
  p:TPoint;
begin
  frmMain.CreatePopUpHotDir;// TODO: i thing in future this must call on create or change
  p:=frmMain.ActiveFrame.ClientToScreen(Classes.Point(0,0));
  frmMain.pmHotList.Popup(p.X,p.Y);
end;

procedure TActs.cm_Search(param:string);
begin
  DebugLn('ShowFindDlg');
  ShowFindDlg(frmMain.ActiveFrame.CurrentPath);
end;


//------------------------------------------------------

procedure TActs.cm_SymLink(param:string);
var
  sFile1, sFile2:String;
  Result: Boolean;
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_SymLink', param);
{
with frmMain do
begin
  Result := False;
  try
    with ActiveFrame do
    begin
      if SelectFileIfNoSelected(GetActiveItem) = False then Exit; // through finally

      sFile2 := pnlFile.GetActiveItem^.sName;
      sFile1 := ActiveDir + sFile2;
      if param <> '' then
        sFile2 := param + sFile2
      else
        sFile2 := NotActiveFrame.CurrentPath + sFile2;
    end;

    Result:= ShowSymLinkForm(sFile1, sFile2);

  finally
    if Result then
      begin
        frameLeft.RefreshPanel;
        frameRight.RefreshPanel;
      end
    else
      begin
        with ActiveFrame do
          UnSelectFileIfSelected(GetActiveItem);
      end;
    ActiveFrame.SetFocus;
  end;
end;
}
end;

procedure TActs.cm_HardLink(param:string);
var
  sFile1, sFile2:String;
  Result: Boolean;
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_HardLink', param);
{
with frmMain do
begin
  inherited;
  Result := False;
  try
    with ActiveFrame do
    begin
      if SelectFileIfNoSelected(GetActiveItem) = False then Exit;

      sFile2 := pnlFile.GetActiveItem^.sName;
      sFile1 := ActiveDir + sFile2;
      if param <> '' then
        sFile2 := param + sFile2
      else
        sFile2 := NotActiveFrame.CurrentPath + sFile2;
    end;

    Result:= ShowHardLinkForm(sFile1, sFile2);

  finally
    if Result then
      begin
        frameLeft.RefreshPanel;
        frameRight.RefreshPanel;
      end
    else
      begin
        with ActiveFrame do
	  UnSelectFileIfSelected(GetActiveItem);
      end;
    ActiveFrame.SetFocus;
  end;
  end;
}
end;

procedure TActs.cm_ReverseOrder(param:string);
begin
// This action is used?
{
  inherited;
  with frmMain.ActiveFrame do
  begin
    pnlFile.SortDirection := ReverseSortDirection(pnlFile.SortDirection);
    pnlFile.Sort;
    RefreshPanel;
  end;
}
end;

procedure TActs.cm_SortByName(param:string);
begin
{
  inherited;
  with frmMain.ActiveFrame do
  begin
    if pnlFile.SortColumn = 0 then
      pnlFile.SortDirection := ReverseSortDirection(pnlFile.SortDirection)
    else
      pnlFile.SortDirection := sdAscending;
    pnlFile.SortByCol(0);
    RefreshPanel;
  end;
}
end;

procedure TActs.cm_SortByExt(param:string);
begin
{
  inherited;
  with frmMain.ActiveFrame do
  begin
    if pnlFile.SortColumn = 1 then
      pnlFile.SortDirection := ReverseSortDirection(pnlFile.SortDirection)
    else
      pnlFile.SortDirection := sdAscending;
    pnlFile.SortByCol(1);
    RefreshPanel;
  end;
}
end;

procedure TActs.cm_SortBySize(param:string);
begin
{
  inherited;
  with frmMain.ActiveFrame do
  begin
    if pnlFile.SortColumn = 2 then
      pnlFile.SortDirection := ReverseSortDirection(pnlFile.SortDirection)
    else
      pnlFile.SortDirection := sdAscending;
    pnlFile.SortByCol(2);
    RefreshPanel;
  end;
}
end;

procedure TActs.cm_SortByDate(param:string);
begin
{
  inherited;
  with frmMain.ActiveFrame do
  begin
    if pnlFile.SortColumn = 3 then
      pnlFile.SortDirection := ReverseSortDirection(pnlFile.SortDirection)
    else
      pnlFile.SortDirection := sdAscending;
    pnlFile.SortByCol(3);
    RefreshPanel;
  end;
}
end;

procedure TActs.cm_SortByAttr(param:string);
begin
{
  inherited;
  with frmMain.ActiveFrame do
  begin
    if pnlFile.SortColumn = 4 then
      pnlFile.SortDirection := ReverseSortDirection(pnlFile.SortDirection)
    else
      pnlFile.SortDirection := sdAscending;
    pnlFile.SortByCol(4);
    RefreshPanel;
  end;
}
end;

// Parameters:
// Number of the column to sort by.
procedure TActs.cm_SortByColumn(param: string='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_SortByColumn', param);
end;

procedure TActs.cm_MultiRename(param:string);
var
  fl: TFileList;
  I: Integer;
begin
{
// MultiRename should also use TFileSource
// as well as selection of files from the view.

with frmMain do
begin
  with ActiveFrame do
  begin
    if SelectFileIfNoSelected(GetActiveItem) = False then Exit;

    fl:= TFileList.Create;  // ShowMultiRenameForm frees 'fl'.
    try
      for I:= 0 to pnlFile.FileList.Count-1 do
        if pnlFile.GetFileItem(I).bSelected then
          fl.AddItem(pnlFile.GetFileItemPtr(I));
      ShowMultiRenameForm(fl);
    except
      FreeAndNil(fl);
    end;
  end;
end;
}
end;

//------------------------------------------------------

procedure TActs.cm_CopySamePanel(param:string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_CopySamePanel', param);
  // Selection validation in CopyFile.
  frmMain.CopyFile('');
end;

procedure TActs.cm_RenameOnly(param:string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_RenameOnly', param);
  // Selection validation in RenameFile.
  frmMain.RenameFile('');
end;

procedure TActs.cm_EditNew(param:string);
var
  sNewFile: String;
  hFile: Integer = 0;
  aFile: TFile;
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_EditNew', param);

  // For now only works for FileSystem.

  with frmMain do
  if ActiveFrame.FileSource is TFileSystemFileSource then
  begin
    aFile := ActiveFrame.ActiveFile;
    if Assigned(aFile) and aFile.IsNameValid then
      sNewFile:= aFile.Name
    else
      sNewFile:= rsEditNewFile;

    if not InputQuery(rsEditNewOpen, rsEditNewFileName, sNewFile) then Exit;

    // If user entered only a filename prepend it with current directory.
    if ExtractFilePath(sNewFile) = '' then
      sNewFile:= ActiveFrame.CurrentPath + sNewFile;

    if not mbFileExists(sNewFile) then
      try
        hFile:= mbFileCreate(sNewFile);
      finally
        if hFile > 0 then
          FileClose(hFile);
      end;

    try
      ShowEditorByGlob(sNewFile);

    finally
      frameLeft.Reload;
      frameRight.Reload;
    end;
  end
  else
    msgWarning(rsMsgNotImplemented);
end;

procedure TActs.cm_DirHistory(param:string);
var
  p:TPoint;
begin
  inherited;
  frmMain.CreatePopUpDirHistory;
  p:=frmMain.ActiveFrame.ClientToScreen(Classes.Point(0,0));
  frmMain.pmDirHistory.Popup(p.X,p.Y);
end;

procedure TActs.cm_ShowCmdLineHistory(param:string);
begin
  with frmMain do
  begin
    if IsCommandLineVisible then
    begin
      edtCommand.SetFocus;
      if edtCommand.Items.Count>0 then
        edtCommand.DroppedDown:=True;
    end;
  end;
end;

procedure TActs.cm_RunTerm(param:string);
begin
  if not frmMain.edtCommand.Focused then
    begin
      mbSetCurrentDir(frmMain.ActiveFrame.CurrentPath);
      ExecCmdFork(gRunTerm);
    end;
end;

procedure TActs.cm_CalculateSpace(param:string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_CalculateSpace', param);
end;

procedure TActs.cm_CountDirContent(param: string);
var
  I: Integer;
  dstFileList: TFileList;
  p: TFileRecItem;
  LastSelection: String;
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_CountDirContent', param);
{
  with frmMain, ActiveFrame do
  begin
    if pnlFile.PanelMode <> pmDirectory then Exit;
    Screen.Cursor:= crHourGlass;

    if Assigned(pnlFile.GetActiveItem) then
      LastSelection := pnlFile.GetActiveItem^.sName
    else
      LastSelection := '';

    for I:= 0 to pnlFile.FileList.Count - 1 do
      begin
        p:= pnlFile.FileList.GetItem(I)^;
        if (not FPS_ISDIR(p.iMode)) or (p.sName = '..') then Continue;
        p.sNameNoExt:= p.sName;
        p.sName:= ActiveDir + p.sNameNoExt;
        p.sPath:= '';
        //DebugLn(p.sName);
        dstFileList:= TFileList.Create; // free at Thread end by thread
        dstFileList.CurrentDirectory:= ActiveDir;
        dstFileList.AddItem(@p);
        Application.ProcessMessages;
        with TSpaceThread.Create(dstFileList, False) do
        begin
          // start thread
          Resume;
          // wait while calculating
          WaitFor;
          // set up directory size
          pnlFile.FileList.GetItem(I)^.iDirSize:= FilesSize;
          // free space thread
          Free;
          // update panel
          pnlFile.Select(LastSelection);
          pnlFile.UpdatePanel;
          RedrawGrid;
       end; // with
      end; // for

    pnlFile.Select(LastSelection);

  end; // with
  Screen.Cursor:= crDefault;
}
end;

procedure TActs.cm_FileProperties(param:string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_FileProperties', param);
{
  Command for file source.

  with frmMain do
  begin
    with ActiveFrame do
    begin
      case pnlFile.PanelMode of
        pmVFS:
          msgWarning(rsMsgErrNotSupported);

        pmArchive:
          if IsActiveItemValid then
            pnlFile.VFS.VFSmodule.VFSRun(GetActiveItem^.sName);

        pmDirectory:
          try
            if SelectFileIfNoSelected(GetActiveItem) = True then
              ShowFilePropertiesDialog(pnlFile.FileList, ActiveDir);
          finally
            frameLeft.RefreshPanel;
            frameRight.RefreshPanel;
            ActiveFrame.SetFocus;
          end;
      end;
    end;
  end;
}
end;

procedure TActs.cm_FileLinker(param:string);
var
  sl:TStringList;
  i:Integer;
begin
{
  with frmMain do
  begin
    with ActiveFrame do
    begin
      if SelectFileIfNoSelected(GetActiveItem) = False then Exit;
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
}
end;

procedure TActs.cm_FileSpliter(param:string);
var
  sl:TStringList;
  i:Integer;
  Result: Boolean;
begin
{
  with frmMain do
  begin
    with ActiveFrame do
    begin
      if SelectFileIfNoSelected(GetActiveItem) = False then Exit;

      sl:=TStringList.Create;
      try
        for i:=0 to pnlFile.FileList.Count-1 do
          if pnlFile.GetFileItem(i).bSelected then
            sl.Add(ActiveDir+pnlFile.GetFileItem(i).sName);
        if sl.Count>0 then
          Result:= ShowSplitterFileForm(sl);
      finally
        FreeAndNil(sl);
        if Result then
          begin
            frameLeft.RefreshPanel;
            frameRight.RefreshPanel;
          end
        else
          begin
            UnSelectFileIfSelected(GetActiveItem);
          end;
        ActiveFrame.SetFocus;
      end;
    end; // with
  end;
}
end;

procedure TActs.cm_PanelsSplitterPerPos(param: string);
var i:integer;
begin
  with frmMain do
  begin
   if TryStrToInt(param,i) then
     begin
       pnlLeft.Width:= (pnlNoteBooks.Width-MainSplitter.Width) * i div 100;
     end;
  end;
end;

procedure TActs.cm_EditComment(param: string);
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_EditComment', param);
{
  with frmMain.ActiveFrame do
  begin
    if IsActiveItemValid then
      ShowDescrEditDlg(ActiveDir + pnlFile.GetActiveItem^.sName);
  end;
}
end;

function SendToClipboard(ClipboardMode: uClipboard.TClipboardOperation):Boolean;
var
  sl: TStringList = nil;
  i : Integer;
  theSelectedFiles: TFiles = nil;
begin
  // Will probably work only for file system.

  // For now we check file source type here.

  Result := False;

  with frmMain.ActiveFrame do
  if FileSource is TFileSystemFileSource then
  begin
    sl := TStringList.Create;
    try
      theSelectedFiles := SelectedFiles;

      for i := 0 to theSelectedFiles.Count - 1 do
        sl.Add(CurrentPath + theSelectedFiles[i].Name);

      case ClipboardMode of
        uClipboard.ClipboardCut:
            Result := uClipboard.CutToClipboard(sl);

        uClipboard.ClipboardCopy:
            Result := uClipboard.CopyToClipboard(sl);
      end;

    finally
      if Assigned(sl) then
        FreeAndNil(sl);
      if Assigned(theSelectedFiles) then
        FreeAndNil(theSelectedFiles);
    end;
  end
  else
    msgWarning(rsMsgErrNotSupported);
end;

procedure TActs.cm_CopyToClipboard(param: string='');
begin
  SendToClipboard(ClipboardCopy);
end;

procedure TActs.cm_CutToClipboard(param: string='');
begin
  SendToClipboard(ClipboardCut);
end;

procedure TActs.cm_PasteFromClipboard(param: string='');
var
  ClipboardOp: TClipboardOperation;
  filenamesList: TStringList;
  FileList: TFileList;
begin
{
  with frmMain do
  begin
    if PasteFromClipboard(ClipboardOp, filenamesList) = True then
    try
      // fill file list with files
      FileList:= TFileList.Create;
      FileList.LoadFromFileNames(fileNamesList);

      { If panel is in Archive of VFS mode - show dialog for the user to confirm. }
      { Otherwise just start the operation thread. }
      case ClipboardOp of

        uClipboard.ClipboardCut:
        begin
          if ActiveFrame.pnlFile.PanelMode in [pmArchive, pmVFS] then
            RenameFile(FileList, ActiveFrame, ActiveFrame.CurrentPath)
          else if ActiveFrame.pnlFile.PanelMode = pmDirectory then
            RunRenameThread(FileList, ActiveFrame.CurrentPath, '*.*');
        end;

        uClipboard.ClipboardCopy:
        begin
          if ActiveFrame.pnlFile.PanelMode in [pmArchive, pmVFS] then
            CopyFile(FileList, ActiveFrame, ActiveFrame.CurrentPath)
          else if ActiveFrame.pnlFile.PanelMode = pmDirectory then
            RunCopyThread(FileList, ActiveFrame.CurrentPath, '*.*', False);
        end

        else
          FreeAndNil(FileList);
      end;

    finally
      FreeAndNil(fileNamesList);
    end;
  end;
}
end;

procedure TActs.cm_ChangeDirToRoot(param: string='');
begin
  frmMain.ActiveFrame.ExecuteCommand('cm_ChangeDirToRoot', param);
{
  FrmMain.ActiveFrame.pnlFile.cdRootLevel;
}
end;

// Parameters:
// Full path to a directory.
procedure TActs.cm_ChangeDir(param: string='');
begin
  FrmMain.ActiveFrame.CurrentPath := param;
end;

procedure TActs.cm_ClearLogWindow(param: string);
begin
  frmMain.seLogWindow.Lines.Clear;
end;

procedure TActs.cm_ClearLogFile(param: string);
begin
  mbDeleteFile(gLogFileName);
end;

end.

