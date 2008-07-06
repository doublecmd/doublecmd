unit uActs;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils,Dialogs,typinfo;
  
  
const cf_Null=0;
      cf_Error=-1;
  
  type

   TIntFunc=procedure(param:string; var Result:integer) of object;

  { TActs }

  TActs=class
  private
   FCmdList:TStrings;


   function GetList:TStrings;
   function Methods(AClass:TClass) : TStringList;
  public
   constructor Create;
   destructor Destroy;override;
   function Execute(Cmd: string; param:string =''): integer;
   function GetIndex(Cmd: string): integer;
   function GetCategoriesList(var List:TStrings):integer;
   function GetCommandsByCategory(Category:string; var List:TStrings):integer;
  published

  //Only published functions and procedures can by found by MethodAddress
  //---------------------
   procedure cm_AddPathToCmdLine(param: string='');
   procedure cm_ContextMenu(param: string='');
   procedure cm_CopyFullNamesToClip(param: string='');
   procedure cm_Exchange(param:string='');
   procedure cm_LeftOpenDrives(param:string='');
   procedure cm_OpenArchive(param:string='');
   procedure cm_OpenDirInNewTab(param:string='');
   procedure cm_Open(param:string='');
   procedure cm_OpenVFSList(param:string='');
   procedure cm_TargetEqualSource(param:string='');
   procedure cm_PackFiles(param: string='');
   procedure cm_QuickSearch(param: string='');
   procedure cm_RightOpenDrives(param: string='');
   procedure cm_ShowButtonMenu(param: string='');
   procedure cm_TransferLeft(param: string='');
   procedure cm_TransferRight(param: string='');
   procedure cm_Wipe(param: string='');
   procedure cm_Exit(param: string='');
   procedure cm_NewTab(param: string='');
   procedure cm_RemoveTab(param: string='');
   procedure cm_NextTab(param: string='');
   procedure cm_PrevTab(param: string='');
   procedure cm_Copy(param: string='');
   procedure cm_Delete(param: string='');
   procedure cm_Edit(param: string='');
   procedure cm_MakeDir(param: string='');
   procedure cm_Rename(param: string='');
   procedure cm_View(param: string='');
   procedure cm_CopyNamesToClip(param: string='');
   procedure cm_FocusCmdLine(param: string='');
   procedure cm_FileAssoc(param: string='');
   procedure cm_ExtractFiles(param: string='');
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
   procedure cm_Search(param: string='');
   procedure cm_HardLink(param: string='');
   procedure cm_MarkMinus(param: string='');
   procedure cm_MarkPlus(param: string='');
   procedure cm_MultiRename(param: string='');
   procedure cm_ReverseOrder(param: string='');
   procedure cm_SortByAttr(param: string='');
   procedure cm_SortByDate(param: string='');
   procedure cm_SortByExt(param: string='');
   procedure cm_SortByName(param: string='');
   procedure cm_SortBySize(param: string='');
   procedure cm_SymLink(param: string='');
   procedure cm_CopySamePanel(param: string='');
   procedure cm_DirHistory(param: string='');
   procedure cm_EditNew(param: string='');
   procedure cm_RenameOnly(param: string='');
   procedure cm_RunTerm(param: string='');
   procedure cm_ShowCmdLineHistory(param: string='');
   procedure cm_CalculateSpace(param: string='');
   procedure cm_FileProperties(param: string='');
   procedure cm_FileLinker(param: string='');
   procedure cm_FileSpliter(param: string='');
   procedure cm_PanelsSplitterPerPos(param: string='');

   //---------------------
   {   procedure SomeFunction (param:string; var Result:integer);
   procedure SomeProcedure(param:string);
   procedure Mess(param:string);}
   //---------------------
   property CommandList:TStrings read FCmdList; //be careful with these list's objects.
  end;

implementation

uses uLng,fMain,uGlobs,uFileList,uTypes,uShowMsg,uOSForms,Controls,
     Clipbrd,uOSUtils,framePanel,uWCXmodule,fPackDlg,uWipeThread,uFileOp,
     uFileOpThread,fFileOpDlg,forms,uVFSutil,uShowForm,uDCUtils,uLog,uVFSTypes,
     fMkDir,LCLProc,uFileProcs,uDeleteThread,fFileAssoc,fExtractDlg,fAbout,
     fOptions,fCompareFiles,fFindDlg,fSymLink,fHardLink,fMultiRename,
     uSpaceThread,fLinker,fSplitter,uGlobsPaths ;

{ TActs }

function TActs.Methods(AClass:TClass): TStringList;
//------------------------------------------------------
    type
       tmethodnamerec = packed record
          name : pshortstring;
          addr : pointer;
       end;

       tmethodnametable = packed record
         count : dword;
         entries : packed array[0..0] of tmethodnamerec;
       end;

       pmethodnametable =  ^tmethodnametable;

var
 methodtable : pmethodnametable;
 i : dword;
 vmt : tclass;

begin
   Result:=TStringList.Create;
   vmt:=AClass;
   while assigned(vmt) do
     begin
        methodtable:=pmethodnametable((Pointer(vmt)+vmtMethodTable)^);
        if assigned(methodtable) then
          for i:=0 to methodtable^.count-1 do
            Result.AddObject(methodtable^.entries[i].name^,Tobject(AClass));
        vmt:=pclass(pointer(vmt)+vmtParent)^;
     end;
end;
//------------------------------------------------------


function TActs.GetList: TStrings;
begin
  Result:=Methods(Self.ClassType);
end;


constructor TActs.Create;
begin
  FCmdList:=GetList;
end;

destructor TActs.Destroy;
begin
  if Assigned(FCmdList) then FreeAndNil(FCmdList);
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
     TIntFunc(t)(param,Result);
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

function TActs.GetCategoriesList(var List: TStrings): integer;
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

function TActs.GetCommandsByCategory(Category: string; var List: TStrings
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

//------------------------------------------------------
//Published methods
//------------------------------------------------------

procedure TActs.cm_AddPathToCmdLine(param:string);
begin
  with frmMain.ActiveFrame do
    begin
      edtCmdLine.Text := edtCmdLine.Text + (pnlFile.ActiveDir);
    end;
end;

procedure TActs.cm_ContextMenu(param:string);
var
  fl : TFileList;
begin
with frmMain do
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
end;

procedure TActs.cm_CopyFullNamesToClip(param:string);
var
  I: Integer;
  sl: TStringList;
begin
  sl:= TStringList.Create;
  with frmmain.ActiveFrame do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    for I:=0 to pnlFile.FileList.Count - 1 do
      if pnlFile.FileList.GetItem(I)^.bSelected then
        sl.Add(ActiveDir + pnlFile.FileList.GetItem(I)^.sName);
    Clipboard.Clear;   // prevent multiple formats in Clipboard (specially synedit)
    Clipboard.AsText:= sl.Text;
    UnMarkAll;
  end;
  FreeAndNil(sl);
end;

procedure TActs.cm_CopyNamesToClip(param:string);
var
  I: Integer;
  sl: TStringList;
begin
  sl:= TStringList.Create;
  with frmMain.ActiveFrame do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    for I:=0 to pnlFile.FileList.Count - 1 do
      if pnlFile.FileList.GetItem(I)^.bSelected then
        sl.Add(pnlFile.FileList.GetItem(I)^.sName);
    Clipboard.Clear;   // prevent multiple formats in Clipboard (specially synedit)
    Clipboard.AsText:= sl.Text;
    UnMarkAll;
  end;
  FreeAndNil(sl);
end;

//------------------------------------------------------
procedure TActs.cm_Exchange(param:string);
var
  sDir: String;
begin
  sDir:= FrmMain.ActiveFrame.pnlFile.ActiveDir;
  FrmMain.ActiveFrame.pnlFile.ActiveDir:= FrmMain.NotActiveFrame.pnlFile.ActiveDir;
  FrmMain.NotActiveFrame.pnlFile.ActiveDir:= sDir;
  FrmMain.ActiveFrame.RefreshPanel;
  FrmMain.NotActiveFrame.RefreshPanel;
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
  pmDrivesMenu.Items[dskLeft.Tag].Checked := True;
  pmDrivesMenu.PopUp(p.x, p.y);
 end;
end;

procedure TActs.cm_OpenDirInNewTab(param:string);
var
  sDir: String;
  bSetActive: Boolean;
begin
  with FrmMain.ActiveFrame do
  begin
    if fpS_ISDIR(pnlFile.GetActiveItem^.iMode) then
      sDir:= FrmMain.ActiveFrame.ActiveDir + pnlFile.GetActiveItem^.sName
    else
      sDir:= FrmMain.ActiveFrame.ActiveDir;
  end;

  bSetActive:= Boolean(gDirTabOptions and tb_open_new_in_foreground);
  with FrmMain do
  begin
    case SelectedPanel of
    fpLeft:
       CreatePanel(AddPage(nbLeft, bSetActive), fpLeft, sDir);
    fpRight:
       CreatePanel(AddPage(nbRight, bSetActive), fpRight, sDir);
    end;
  end;
end;

procedure TActs.cm_TargetEqualSource(param:string);
begin
  FrmMain.NotActiveFrame.pnlFile.ActiveDir:= FrmMain.ActiveFrame.pnlFile.ActiveDir;
  FrmMain.NotActiveFrame.RefreshPanel;
end;

procedure TActs.cm_OpenArchive(param:string);
begin
  FrmMain.ActiveFrame.pnlFile.TryOpenArchive(FrmMain.ActiveFrame.GetActiveItem);
end;

procedure TActs.cm_Open(param:string);
begin
  FrmMain.ActiveFrame.pnlFile.ChooseFile(FrmMain.ActiveFrame.GetActiveItem);
end;

procedure TActs.cm_OpenVFSList(param:string);
begin
  FrmMain.ActiveFrame.pnlFile.LoadVFSListInPanel;
end;

//------------------------------------------------------
(* Pack files in archive *)
procedure TActs.cm_PackFiles(param:string);
var
  fl : TFileList;
  Result: Boolean;
begin
with frmMain do
begin
  Result:= False;
  if not IsBlocked then
    begin
      fl:=TFileList.Create;
      with FrmMain.ActiveFrame do
        begin
          SelectFileIfNoSelected(GetActiveItem);
          CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir);

          fl.CurrentDirectory := ActiveDir;
        end;
      try
        Result:= ShowPackDlg(FrmMain.NotActiveFrame.pnlFile.VFS, fl, FrmMain.NotActiveFrame.ActiveDir);
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
end;

procedure TActs.cm_QuickSearch(param:string);
begin
  FrmMain.ActiveFrame.ShowAltPanel;
  FrmMain.KeyPreview := False;
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
    pmDrivesMenu.Items[dskRight.Tag].Checked := True;
    pmDrivesMenu.PopUp(p.x, p.y);
  end;
end;

procedure TActs.cm_ShowButtonMenu(param:string);
var  Point:TPoint;
begin
  with frmMain do
  begin
    pmButtonMenu.LoadBarFile(gpIniDir + param);
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

procedure TActs.cm_Wipe(param:string);
var
  fl:TFileList;
  WT : TWipeThread;
begin
  with FrmMain.ActiveFrame do
  begin
    if  pnlFile.PanelMode in [pmArchive, pmVFS] then // if in VFS
      begin
        msgOK(rsMsgErrNotSupported);
        Exit;
      end; // in VFS

    SelectFileIfNoSelected(GetActiveItem);
  end;

  case msgYesNoCancel(frmMain.GetFileDlgStr(rsMsgDelSel,rsMsgDelFlDr)) of
    mmrNo:
      begin
        FrmMain.ActiveFrame.UnMarkAll;
        Exit;
      end;
    mmrCancel:
      begin
	with FrmMain.ActiveFrame do
	  UnSelectFileIfSelected(GetActiveItem);
        Exit;
      end;
  end;

  fl:=TFileList.Create; // free at Thread end by thread
  try
    CopyListSelectedExpandNames(FrmMain.ActiveFrame.pnlFile.FileList,fl,FrmMain.ActiveFrame.ActiveDir);

    (* Wipe files *)
     if not Assigned(frmFileOp) then
       frmFileOp:= TfrmFileOp.Create(Application);
     try
       WT := TWipeThread.Create(fl);
       WT.FFileOpDlg:= frmFileOp;
       WT.sDstPath:= FrmMain.NotActiveFrame.ActiveDir;
       //DT.sDstMask:=sDstMaskTemp;
       frmFileOp.Thread:= TThread(WT);
       frmFileOp.Show;
       WT.Resume;
     except
       WT.Free;
     end;

  except
    FreeAndNil(frmFileOp);
  end;
end;

procedure TActs.cm_Exit(param:string);
begin
  frmMain.Close; // application.Terminate not save settings.
end;

procedure TActs.cm_NewTab(param:string);
begin
  with frmMain do
  begin
    case SelectedPanel of
    fpLeft:
       CreatePanel(AddPage(nbLeft), fpLeft, ActiveFrame.ActiveDir);
    fpRight:
       CreatePanel(AddPage(nbRight), fpRight, ActiveFrame.ActiveDir);
    end;
  end;
end;

procedure TActs.cm_RemoveTab(param:string);
begin
  with frmMain do
  begin
    case SelectedPanel of
    fpLeft:
       RemovePage(nbLeft, nbLeft.PageIndex);
    fpRight:
       RemovePage(nbRight, nbRight.PageIndex);
    end;
  end;
end;

procedure TActs.cm_NextTab(param: string);
begin
with frmMain do
  begin
    case SelectedPanel of
    fpLeft: begin
               if nbLeft.PageIndex=nbLeft.PageCount-1 then
                 nbLeft.PageIndex:=0
               else
                 nbLeft.PageIndex:=nbLeft.PageIndex+1;
            end;

    fpRight: begin
               if nbRight.PageIndex=nbRight.PageCount-1 then
                 nbRight.PageIndex:=0
               else
                 nbRight.PageIndex:=nbRight.PageIndex+1;
             end;
    end;
   ActiveFrame.SetFocus;
  end;
end;

procedure TActs.cm_PrevTab(param: string);
begin
with frmMain do
  begin
    case SelectedPanel of
    fpLeft: begin
               if nbLeft.PageIndex=0 then
                 nbLeft.PageIndex:=nbLeft.PageCount-1
               else
                 nbLeft.PageIndex:=nbLeft.PageIndex-1;
            end;

    fpRight: begin
               if nbRight.PageIndex=0 then
                 nbRight.PageIndex:=nbRight.PageCount-1
               else
                 nbRight.PageIndex:=nbRight.PageIndex-1;
             end;
    end;
  ActiveFrame.SetFocus;
  end;
end;

//------------------------------------------------------

procedure TActs.cm_View(param:string);
var
  sl:TStringList;
  i:Integer;
  fr:PFileRecItem;
  VFSFileList : TFileList;
  sFileName,
  sFilePath,
  sTempDir : String;
begin
with frmMain do
begin
  with ActiveFrame do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    sl:=TStringList.Create;
    try
      for i:=0 to pnlFile.FileList.Count-1 do
      begin
        fr:=pnlFile.GetFileItemPtr(i);
        if fr^.bSelected and not (FPS_ISDIR(fr^.iMode) or fr^.bLinkIsDir) then
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
          if (log_info in gLogOptions) then
            logWrite('View.Add: ' + sFilePath + sFileName, lmtInfo);
        end;
      end;
      if sl.Count>0 then
        ShowViewerByGlobList(sl)
      else
        begin
          fr := pnlFile.GetActiveItem;
          if (FPS_ISDIR(fr^.iMode) or fr^.bLinkIsDir) then
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
end;

procedure TActs.cm_Edit(param:string);
var
//  sl:TStringList;
  i:Integer;
  fr:PFileRecItem;
  sFileName,
  sFilePath : String;
begin
  with frmMain do
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
end;

procedure TActs.cm_Copy(param:string);
begin
  frmMain.CopyFile(frmMain.NotActiveFrame.ActiveDir);
end;

procedure TActs.cm_Rename(param:string);
begin
  frmMain.RenameFile(frmMain.NotActiveFrame.ActiveDir);
end;

procedure TActs.cm_MakeDir(param:string);
var
  sPath:String;
begin
with frmMain do
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

      if (mbDirectoryExists(ActiveDir+sPath)) then
      begin
        msgError(Format(rsMsgErrDirExists,[ActiveDir+sPath]));
        pnlFile.LastActive:=sPath;
        pnlFile.LoadPanel;
      end
      else
      begin
        if not ForceDirectory(ActiveDir+sPath) then
          begin
            // write log
            if (log_dir_op in gLogOptions) and (log_errors in gLogOptions) then
              logWrite(Format(rsMsgLogError+rsMsgLogMkDir, [ActiveDir+sPath]), lmtError);

            // Standart error modal dialog
            msgError(Format(rsMsgErrForceDir,[ActiveDir+sPath]))
          end
        else
        begin
          // write log
          if (log_dir_op in gLogOptions) and (log_success in gLogOptions) then
            logWrite(Format(rsMsgLogSuccess+rsMsgLogMkDir,[ActiveDir+sPath]), lmtSuccess);

          pnlFile.LastActive:=sPath;
          pnlFile.LoadPanel;
        end;
      end;
    finally
      ActiveFrame.SetFocus;
    end;
  end;
end;
end;

procedure TActs.cm_Delete(param:string);
var
  fl:TFileList;
  DT : TDeleteThread;
begin
with frmMain do
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
	with ActiveFrame do
	  UnSelectFileIfSelected(GetActiveItem);
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
end;

procedure TActs.cm_FocusCmdLine(param:string);
begin
  frmMain.edtCommand.SetFocus;
end;

procedure TActs.cm_FileAssoc(param:string);
begin
  ShowFileAssocDlg;
end;

procedure TActs.cm_ExtractFiles(param:string);
var
  fl : TFileList;
  Result: Boolean;
begin
with frmMain do
begin
  Result:= False;
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
        Result:= ShowExtractDlg(ActiveFrame, fl, NotActiveFrame.ActiveDir);
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
  sFile1, sFile2:String;
begin
  inherited;
with frmMain do
begin
  with FrameLeft do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    with pnlFile.GetActiveItem^ do
    begin
      if not FPS_ISDIR(iMode) then
        sFile1 := ActiveDir + sName
      else
        begin
          MsgOk(rsMsgErrNoFiles);
          FrameLeft.UnMarkAll;
          Exit;
        end;
    end;
  end; // FrameLeft;

  with FrameRight do
  begin
    SelectFileIfNoSelected(GetActiveItem);
    with pnlFile.GetActiveItem^ do
    begin
      if not FPS_ISDIR(iMode) then
        sFile2 := ActiveDir + sName
      else
        begin
          MsgOk(rsMsgErrNoFiles);
          FrameRight.UnMarkAll;
          Exit;
        end;
    end;
  end; // Frameright;

  try
    if gUseExtDiff then
      begin
        ExecCmdFork(Format('"%s" "%s" "%s"', [gExtDiff, sFile1, sFile2]));
        Exit;
      end;

    ShowCmpFiles(sFile1, sFile2);
  finally
    FrameLeft.UnMarkAll;
    FrameRight.UnMarkAll;
  end;
end;
end;


procedure TActs.cm_ShowMenu(param:string);
begin
  //gtk_menu_item_select(PGtkMenuItem(mnuFiles.Handle));
end;

procedure TActs.cm_Refresh(param:string);
begin
  inherited;
  frmMain.ActiveFrame.RefreshPanel;
end;

  //------------------------------------------------------
  
procedure TActs.cm_MarkInvert(param:string);
begin
  inherited;
  frmMain.ActiveFrame.InvertAllFiles;
end;

procedure TActs.cm_MarkMarkAll(param:string);
begin
  inherited;
  frmMain.ActiveFrame.MarkAll;
end;

procedure TActs.cm_MarkUnmarkAll(param:string);
begin
  inherited;
  frmMain.ActiveFrame.UnMarkAll;
end;

procedure TActs.cm_DirHotList(param:string);
var
  p:TPoint;
begin
  inherited;
  frmMain.CreatePopUpHotDir;// TODO: i thing in future this must call on create or change
  p:=frmMain.ActiveFrame.dgPanel.ClientToScreen(Classes.Point(0,0));
  frmMain.pmHotList.Popup(p.X,p.Y);
end;

procedure TActs.cm_Search(param:string);
begin
  inherited;
  DebugLn('ShowFindDlg');
  ShowFindDlg(frmMain.ActiveFrame.ActiveDir);
end;


//------------------------------------------------------

procedure TActs.cm_MarkPlus(param:string);
begin
  frmMain.ActiveFrame.MarkPlus;
end;

procedure TActs.cm_MarkMinus(param:string);
begin
  frmMain.ActiveFrame.MarkMinus;
end;


procedure TActs.cm_SymLink(param:string);
var
  sFile1, sFile2:String;
  Result: Boolean;
begin
  inherited;
with frmMain do
begin
  try
    with ActiveFrame do
    begin
      SelectFileIfNoSelected(GetActiveItem);
      sFile2 := pnlFile.GetActiveItem^.sName;
      sFile1 := ActiveDir + sFile2;
      if param <> '' then
        sFile2 := param + sFile2
      else
        sFile2 := NotActiveFrame.ActiveDir + sFile2;
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
end;

procedure TActs.cm_HardLink(param:string);
var
  sFile1, sFile2:String;
  Result: Boolean;
begin
with frmMain do
begin
  inherited;
  try
    with ActiveFrame do
    begin
      SelectFileIfNoSelected(GetActiveItem);
      sFile2 := pnlFile.GetActiveItem^.sName;
      sFile1 := ActiveDir + sFile2;
      if param <> '' then
        sFile2 := param + sFile2
      else
        sFile2 := NotActiveFrame.ActiveDir + sFile2;
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
end;

procedure TActs.cm_ReverseOrder(param:string);
begin
  inherited;
  with frmMain.ActiveFrame do
  begin
    pnlFile.SortDirection:= not pnlFile.SortDirection;
    pnlFile.Sort;
    RefreshPanel;
  end;
end;

procedure TActs.cm_SortByName(param:string);
begin
  inherited;
  with frmMain.ActiveFrame do
  begin
    pnlFile.SortByCol(0);
    RefreshPanel;
  end;
end;

procedure TActs.cm_SortByExt(param:string);
begin
  inherited;
  with frmMain.ActiveFrame do
  begin
    pnlFile.SortByCol(1);
    RefreshPanel;
  end;
end;

procedure TActs.cm_SortBySize(param:string);
begin
  inherited;
  with frmMain.ActiveFrame do
  begin
    pnlFile.SortByCol(2);
    RefreshPanel;
  end;
end;

procedure TActs.cm_SortByDate(param:string);
begin
  inherited;
  with frmMain.ActiveFrame do
  begin
    pnlFile.SortByCol(3);
    RefreshPanel;
  end;
end;

procedure TActs.cm_SortByAttr(param:string);
begin
  inherited;
  with frmMain.ActiveFrame do
  begin
    pnlFile.SortByCol(4);
    RefreshPanel;
  end;
end;

procedure TActs.cm_MultiRename(param:string);
var
  sl:TStringList;
  i:Integer;
  Result: Boolean;
begin
with frmMain do
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
        Result:= ShowMultiRenameForm(sl);
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
  end;
end;
end;

//------------------------------------------------------

procedure TActs.cm_CopySamePanel(param:string);
begin
  frmMain.CopyFile('');
end;

procedure TActs.cm_RenameOnly(param:string);
begin
  frmMain.RenameFile('');
end;

procedure TActs.cm_EditNew(param:string);
var
  sNewFile: String;
  hFile: Integer;
begin
with frmMain do
begin
  sNewFile:=ActiveFrame.ActiveDir + rsEditNewFile;
  if not InputQuery(rsEditNewOpen, rsEditNewFileName, sNewFile) then Exit;
  if not mbFileExists(sNewFile) then
    try
      hFile:= mbFileCreate(sNewFile);
    finally
      FileClose(hFile);
    end;
  try
    ShowEditorByGlob(sNewFile);
  finally
    frameLeft.RefreshPanel;
    frameRight.RefreshPanel;
  end;
end;
end;

procedure TActs.cm_DirHistory(param:string);
var
  p:TPoint;
begin
  inherited;
  frmMain.CreatePopUpDirHistory;
  p:=frmMain.ActiveFrame.dgPanel.ClientToScreen(Classes.Point(0,0));
  frmMain.pmDirHistory.Popup(p.X,p.Y);
end;

procedure TActs.cm_ShowCmdLineHistory(param:string);
begin
  inherited;
  if (frmMain.edtCommand.Items.Count>0) then
    frmMain.edtCommand.DroppedDown:=True;
end;

procedure TActs.cm_RunTerm(param:string);
begin
  if not frmMain.edtCommand.Focused then
    begin
      mbSetCurrentDir(frmMain.ActiveFrame.ActiveDir);
      ExecCmdFork(gRunTerm);
    end;
end;

procedure TActs.cm_CalculateSpace(param:string);
begin
  inherited;
  with frmMain.ActiveFrame do
  begin
    if FPS_ISDIR(pnlFile.GetActiveItem^.iMode) then
      frmMain.CalculateSpace(True);
    // I don't know what to do if the item is file or something else
  end;
end;

procedure TActs.cm_FileProperties(param:string);
begin
  inherited;
  with frmMain do
  begin
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
end;



procedure TActs.cm_FileLinker(param:string);
var
  sl:TStringList;
  i:Integer;
begin
  with frmMain do
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
end;

procedure TActs.cm_FileSpliter(param:string);
var
  sl:TStringList;
  i:Integer;
  Result: Boolean;
begin
  with frmMain do
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



end.

