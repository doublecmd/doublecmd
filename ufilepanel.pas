{
   Seksi Commander
   ----------------------------
   Implementing of storing Files and main file operation

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2006-2009 Alexander Koblov (Alexx2000@mail.ru)
   
   Vitaly Zotov (vitalyzotov@mail.ru)
}

unit uFilePanel;

{$mode objfpc}{$H+}

interface
uses
  StdCtrls, Grids, uFileList, uTypes, uPathHistory, Classes, uVFS;

type
  TOnBeforeChangeDirectory = function (Sender: TObject; const NewDir : String): Boolean of object;
  TOnAfterChangeDirectory = procedure (Sender: TObject; const NewDir : String) of object;

  { TFilePanel }

  TFilePanel=Class
  private
    fOwner : TObject;
    fFileList:TFileList;
    fVFS : TVFS;
    flblPath:TLabel;
    fPanel:TDrawGrid;
    fSortCol:Integer;
    fSortDirect:Boolean;
    fPrevActiveDir: String;
    fActiveDir:String;
    fLastActive:String;

    fPanelMode: TPanelMode; // file, archive or VFS?
    fPathHistory: TPathHistory;
    fRefList:TList;  // list of ptr (showed in grid) to FileRecItem
    fVFSmoduleList : TStringList; // list of VFS modules (used in sub archive)


    fFilesInDir:Integer; //must call UpdateCountStatus first
    fFilesSelected:Integer; //must call UpdateCountStatus first
    fSizeInDir:Int64; //must call UpdateCountStatus first
    fSizeSelected:Int64; //must call UpdateCountStatus first
    flblCurPath:TLabel; // label before Command line
    flblFree:TLabel;
    fedtCommand:TComboBox; // only for place correction after Chdir
    FOnBeforeChangeDirectory : TOnBeforeChangeDirectory;
    FOnAfterChangeDirectory : TOnAfterChangeDirectory;
  public
//    iLastDrawnIndex  :Integer; // fucking dirty hack (OnDrawItem

    constructor Create(AOwner : TObject; APanel:TDrawGrid; AlblPath: TLabel; AlblCurPath, AlblFree:TLabel; AedtCommand:TComboBox);
    Destructor Destroy; override;
    procedure LoadPanel;
    procedure LoadPanelVFS(frp:PFileRecItem);
    procedure LoadVFSListInPanel;
    procedure SortByCol(iCol:Integer);
    procedure Sort;
    procedure UpdatePanel;
//    procedure ChDir(sDir:String);
    procedure TryOpenArchive(pfri:PFileRecItem);
    procedure ChooseFile(pfri:PFileRecItem); // main input node
    function GetFileItem(iIndex:Integer):TFileRecItem;
    function GetFileItemPtr(iIndex:Integer):PFileRecItem;
    function GetReferenceItemPtr(iIndex:Integer):PFileRecItem;
    function GetActiveItem:PFileRecItem;
    function GetSelectedCount:Integer;
    procedure InvertFileSection(frp:PFileRecItem);
    procedure MarkAllFiles(bMarked:Boolean);
    procedure MarkFile(frp:PFileRecItem; bMarked:Boolean);
    procedure InvertAllFiles;
    procedure UpdateCountStatus;
    procedure cdUpLevel;
    procedure cdDownLevel(frp:PFileRecItem);
    procedure MarkGroup(const sMask:String; bSelect:Boolean); // second parametr is switch sel/uns
    procedure UpdatePrompt;
    procedure SetActiveDir(const AValue:String);
    function GetActiveDir:String;
    { Returns True if there are no files shown in the panel. }
    function IsEmpty:Boolean;
    { Returns True if item is not nil and not '..'.
      May be extended to include other conditions. }
    function IsItemValid(frp:PFileRecItem):Boolean;
    property OnBeforeChangeDirectory : TOnBeforeChangeDirectory read FOnBeforeChangeDirectory write FOnBeforeChangeDirectory;
    property OnAfterChangeDirectory : TOnAfterChangeDirectory read FOnAfterChangeDirectory write FOnAfterChangeDirectory;

  published
    property SortDirection:Boolean read fSortDirect write fSortDirect; // maybe write method
    property SortColumn : Integer read fSortCol write SortByCol;
    property ActiveDir:String read GetActiveDir write SetActiveDir;
    property LastActive:String read fLastActive write fLastActive;
    property FileList: TFileList read fFileList write fFileList;
    property SelectedCount:Integer read GetSelectedCount;
    property FilesInDir:Integer read fFilesInDir;
    property FilesSelected:Integer read fFilesSelected;
    property SizeInDir:Int64 read fSizeInDir;
    property SizeSelected:Int64 read fSizeSelected;
    property PanelMode : TPanelMode read fPanelMode;
    property VFS : TVFS read fVFS;
  end;

implementation

uses
  LCLProc, SysUtils, Masks, uFileOp, uGlobs, uVFSutil,
  uShowMsg, Controls, uLng, uShowForm, uVFSmodule, uDCUtils,
  uOSUtils,fMain, uShellExecute;

constructor TFilePanel.Create(AOwner : TObject; APanel:TDrawGrid; AlblPath: TLabel; AlblCurPath, AlblFree:TLabel; AedtCommand:TComboBox);
begin
  fOwner := AOwner;
  fPanel:=APanel;
  fRefList:=TList.Create;
  fVFS := TVFS.Create;
  flblPath:=AlblPath;
  flblCurPath:=AlblCurPath;
  flblFree:=AlblFree;
  fedtCommand:=AedtCommand;
  fFileList:=TFileList.Create;
  GetDir(0,fActiveDir);
  fActiveDir:=ExtractFilePath(fActiveDir);
  fPathHistory:=TPathHistory.Create;
  fPanelMode:=pmDirectory;
  fVFSmoduleList := TStringList.Create;
//  LastActive:='';
//  iLastDrawnIndex:=-1;
end;

Destructor TFilePanel.Destroy;
begin
  if assigned(fFileList) then
    FreeAndNil(fFileList);
  if assigned(fVFS) then
    FreeAndNil(fVFS);
  if assigned(fPathHistory) then
    FreeAndNil(fPathHistory);
  if assigned(fRefList) then
    FreeAndNil(fRefList);
  if Assigned(fVFSmoduleList) then
    FreeAndNil(fVFSmoduleList);
end;


procedure TFilePanel.UpdatePanel;
var
  i:Integer;
  pfri:PFileRecItem;
  bAnyRow:Boolean;
begin
  case fPanelMode of
    pmDirectory:
      flblPath.Caption:=' '+MinimizeFilePath(ActiveDir, flblPath.Canvas, flblPath.Width);
    pmArchive,
    pmVFS:
      flblPath.Caption:=' ' + ActiveDir;
  else
    Raise Exception.Create('fix me:UpdatePanel:bad panelmode');
  end;

  if fPanel.FixedRows <> Integer(gTabHeader) then
    begin
      fPanel.FixedRows:= Integer(gTabHeader);
      if gTabHeader then
        fPanel.RowHeights[0]:= fPanel.Tag
      else
        fPanel.RowHeights[0]:= gIconsSize;
    end;

  bAnyRow:=fPanel.Row>=0;
  fRefList.Clear;
  for i:=0 to fFileList.Count-1 do
  begin
    pfri:=fFileList.GetItem(i);
    with pfri^ do
    begin
      if (not gShowSystemFiles) and bSysFile then Continue;
      fRefList.Add(pfri);
    end;
  end;

  fPanel.RowCount:=fRefList.Count+fPanel.FixedRows; // one is header
  UpdatePrompt;
  if bAnyRow then
  begin
    if (LastActive<>'') then // find correct cursor position in Panel (drawgrid)
    begin
      for i:=0 to fRefList.Count-1 do
      begin
        with GetReferenceItemPtr(i)^ do
          if pos(LastActive, sName)=1 then
          begin
            fPanel.Row:= i+Integer(gTabHeader);
            Break;
          end;
      end;
    end
    else
      fPanel.Row:=0;
    if (fPanel.Row<0)then
      fPanel.Row:=0;
  end;    
//  fPanel.Selected.MakeVisible;}
  UpdateCountStatus;
end;

procedure TFilePanel.LoadPanelVFS(frp:PFileRecItem);
var
  sDir:String;
  sDummy:String;
  VFSFileList : TFileList;
  sTempDir,
  sFileName : String;
  I, iIndex : Integer;
begin
  with frp^ do
  begin
    if (fPanelMode in [pmArchive, pmVFS]) then
     begin
      if sName = '..' then
        begin
          fActiveDir := fVFS.ArcFullName + sPath;

          //DebugLn('UpDir = ' + sPath);

          if not fVFS.cdUpLevel(frp, fFileList) then
            begin
              if fVFSmoduleList.Count <> 0 then  // if in sub archive then return in parent VFS
                begin
                  mbDeleteFile(fVFS.ArcFullName);
                  I := fVFSmoduleList.Count - 1;
                  fVFS.VFSmodule := TVFSmodule(fVFSmoduleList.Objects[I]); // load VFS module
                  fVFS.ArcFullName := fVFSmoduleList.Names[I]; // load archive name
                  sTempDir := fVFSmoduleList.ValueFromIndex[I]; // load archive subdirectory
                  fVFSmoduleList.Delete(I);
                  fVFS.VFSmodule.VFSList(sTempDir, fFileList);
                  case fVFS.VFSType of
                    vtWCX: fPanelMode := pmArchive;
                    vtWFX: fPanelMode := pmVFS;
                  end;
                  fActiveDir := fVFS.ArcFullName + sTempDir;
                end
              else  // exit from VFS
                begin
                  case fPanelMode of
                  pmVFS:
                    LoadVFSListInPanel;
                  pmArchive:
                    begin
                      fPanelMode := pmDirectory;
                      fActiveDir := ExtractFilePath(fVFS.ArcFullName);
                      mbSetCurrentDir(fActiveDir);
                      if Assigned(FOnAfterChangeDirectory) then
                        FOnAfterChangeDirectory(fOwner, fActiveDir);
                      LoadFilesbyDir(fActiveDir, fFileList);
                    end;
                  end; // case
                end;
            end;
        end
      else // is directory
      if FPS_ISDIR(iMode) then
        begin
          fActiveDir := fVFS.ArcFullName + sPath + sName + DirectorySeparator;
          fVFS.cdDownLevel(frp, fFileList);
        end
      else // Is file
        begin
          if fVFS.FindModule(sName, False) then // if archive
            begin
              iIndex := fVFSmoduleList.AddObject(fVFS.ArcFullName + '=' + sPath, fVFS.VFSmodule);

              //DebugLn('sPath ==' + sPath);

              VFSFileList := TFileList.Create;
              VFSFileList.CurrentDirectory := ActiveDir;

              //DebugLn('ActiveDir == ' + ActiveDir);

              sName := ActiveDir + sName;
              sFileName := sName;
              VFSFileList.AddItem(frp);
              sTempDir := GetTempFolder;
              {if }fVFS.VFSmodule.VFSCopyOut(VFSFileList, sTempDir, 0);{ then}
                begin
                 if not fVFS.LoadAndOpen(sTempDir + ExtractDirLevel(ActiveDir, sFileName)) then
                   begin
                     // restore old plugin module and delete it from list
                     fVFS.VFSmodule := TVFSmodule(fVFSmoduleList.Objects[iIndex]);
                     fVFS.ArcFullName := fVFSmoduleList.Names[iIndex];
                     fVFSmoduleList.Delete(iIndex);
                     Exit;
                   end;
                 //DebugLn('sTempDir + sName == ' + sTempDir + sName);

                 fVFS.VFSmodule.VFSList(PathDelim, fFileList);
                 fPanelMode:=pmArchive;
                 fActiveDir := fVFS.ArcFullName + DirectorySeparator;

                end;
            end
          else
            fVFS.VFSmodule.VFSRun(sName);
        end;
     end
    else // Is not in VFS
       begin
           fVFS.VFSmodule.VFSList(PathDelim, fFileList);
           case fVFS.VFSType of
             vtWCX: fPanelMode := pmArchive;
             vtWFX: fPanelMode := pmVFS;
           end;
           fActiveDir := fVFS.ArcFullName + DirectorySeparator;
       end;
    if gShowIcons then
      fFileList.UpdateFileInformation(fPanelMode);
    Sort;
    Exit;
  end;
end;

procedure TFilePanel.LoadVFSListInPanel;
begin
  if fVFS.LoadVFSList(fFileList) then
    begin
      fPanelMode := pmDirectory;
      fActiveDir := PathDelim;
      if gShowIcons then
        fFileList.UpdateFileInformation(PanelMode);
      Sort;
    end;
end;


procedure TFilePanel.LoadPanel;
begin
//  DebugLn('TFilePanel.LoadPanel');
  if fPanelMode in [pmArchive, pmVFS] then
    fPanelMode := pmDirectory;

  if Assigned(FOnBeforeChangeDirectory) then
    if not FOnBeforeChangeDirectory(fOwner, ActiveDir) then
      begin
        fActiveDir:= fPrevActiveDir;
        Exit;
      end;

  if not mbSetCurrentDir(ActiveDir) then
    begin
      GetDir(0,fActiveDir);
      IncludeTrailingBackslash(fActiveDir);
      Exit;   // chdir failed
    end;
  if Assigned(FOnAfterChangeDirectory) then
    FOnAfterChangeDirectory(fOwner, fActiveDir);
  LoadFilesbyDir(fActiveDir, fFileList);

  if gShowIcons then
    fFileList.UpdateFileInformation(fPanelMode);
  Sort; // and Update panel
  fPanel.Invalidate;
//  DebugLn('TFilePanel.LoadPanel DONE');
end;


procedure TFilePanel.SortByCol(iCol:Integer);
begin
  fSortCol:=iCol;
  Sort;
end;

procedure TFilePanel.Sort;
begin
  fFileList.Sort(fSortCol, fSortDirect, gCaseSensitiveSort);
  UpDatePanel;
end;

function TFilePanel.GetFileItem(iIndex:Integer):TFileRecItem;
begin
  Result:=fFilelist.GetItem(iIndex)^;
end;

function TFilePanel.GetFileItemPtr(iIndex:Integer):PFileRecItem;
begin
  Result:=fFilelist.GetItem(iIndex);
end;


procedure TFilePanel.InvertFileSection(frp:PFileRecItem);
begin
  if Assigned(frp) then
    MarkFile(frp, not frp^.bSelected);
end;

procedure TFilePanel.InvertAllFiles;
var
  i:Integer;
begin
  for i:=0 to fFileList.Count-1 do
    InvertFileSection(fFileList.GetItem(i));
end;

procedure TFilePanel.TryOpenArchive(pfri:PFileRecItem);
var
  VFSFileList : TFileList;
  sTempDir,
  sFileName : String;
  iIndex : Integer;
begin
  with pfri^ do
    if fPanelMode = pmDirectory then // in real file system
      begin
        if fVFS.TryFindModule(sPath + sName) then
          begin
            fVFS.VFSmodule.VFSList(PathDelim, fFileList);
            fPanelMode := pmArchive;
            fActiveDir := fVFS.ArcFullName + DirectorySeparator;
          end;
      end
    else  // in Virtual File System
      begin
        iIndex := fVFSmoduleList.AddObject(fVFS.ArcFullName + '=' + sPath, fVFS.VFSmodule);

        //DebugLn('sPath ==' + sPath);

        VFSFileList := TFileList.Create;
        VFSFileList.CurrentDirectory := ActiveDir;

        //DebugLn('ActiveDir == ' + ActiveDir);

        sName := ActiveDir + sName;
        sFileName := sName;

        //DebugLn('sFileName = ', sFileName);

        VFSFileList.AddItem(pfri);
        sTempDir := GetTempFolder;
        {if }fVFS.VFSmodule.VFSCopyOut(VFSFileList, sTempDir, 0);{ then}
          begin
            //DebugLn('sTempDir + sName == ' + sTempDir + ExtractDirLevel(ActiveDir, sFileName));
            if not fVFS.TryFindModule(sTempDir + ExtractDirLevel(ActiveDir, sFileName)) then
              begin
                // restore old plugin module and delete it from list
                fVFS.VFSmodule := TVFSmodule(fVFSmoduleList.Objects[iIndex]);
                fVFSmoduleList.Delete(iIndex);
                Exit;
              end;

            fVFS.VFSmodule.VFSList(PathDelim, fFileList);
            fPanelMode:=pmArchive;
            fActiveDir := fVFS.ArcFullName + DirectorySeparator;
          end;
      end; // in VFS
  if gShowIcons then
    fFileList.UpdateFileInformation(fPanelMode);
  Sort;
end;

procedure TFilePanel.ChooseFile(pfri:PFileRecItem);
var
  sOpenCmd:String;
begin
// main file input point for decision
//  DebugLn(pfri^.sName);

  with pfri^ do
  begin
    if (sName='..') then
    begin
      cdUpLevel;
      Exit;
    end;

    if (fPanelMode=pmVFS) or ((sModeStr = 'wfx') and fVFS.FindModule(sPath + sName)) then
    begin
      LastActive:= '';
      LoadPanelVFS(pfri);
      Exit;
    end;
    if (fPanelMode=pmArchive) or (not FPS_ISDIR(iMode) and fVFS.FindModule(sPath + sName)) then
    begin
      LastActive:= '';
      LoadPanelVFS(pfri);
      Exit;
    end;

    if FPS_ISDIR(iMode) or bLinkIsDir then // deeper and deeper
    begin
      cdDownLevel(pfri);
      Exit;
    end;
    //now test if exists Open command in doublecmd.ext :)
    sOpenCmd:=gExts.GetExtActionCmd(lowercase(ExtractFileExt(sName)),'open');
    if (sOpenCmd<>'') then
    begin
      if Pos('{!VFS}',sOpenCmd)>0 then
      begin
        if fVFS.FindModule(sName) then
        begin
          LoadPanelVFS(pfri);
          Exit;
        end;
      end;
      LastActive:=sName;

      ReplaceExtCommand(sOpenCmd, pfri, ActiveDir);
      if ProcessExtCommand(sOpenCmd, ActiveDir) then
        Exit;
    end;
    // and at the end try to open by system
    mbSetCurrentDir(ActiveDir);
    LastActive:= sName;
    ShellExecute(sName);
    LoadPanel;
  end;
end;

procedure TFilePanel.MarkAllFiles(bMarked:Boolean);
var
  i:Integer;

begin
  for i:=0 to fFileList.Count-1 do
  begin
    MarkFile(fFileList.GetItem(i), bMarked);
  end;
end;

procedure TFilePanel.MarkFile(frp:PFileRecItem; bMarked:Boolean);
begin
  if IsItemValid(frp) then
  begin
    if not gShowSystemFiles and (frp^.bSysFile) then
      begin
// system files is always not selected if not showed
        frp^.bSelected:=False
      end
    else
      begin
        frp^.bSelected:=bMarked;
      end;
  end;
end;

function TFilePanel.GetSelectedCount:Integer;
var
  i:Integer;
begin
  Result:=0;
  for i:=0 to fFileList.Count-1 do
    if fFileList.GetItem(i)^.bSelected then
      inc(Result);
end;

procedure TFilePanel.UpdateCountStatus;
var
  i:Integer;
begin
  fFilesInDir:=0;
  fFilesSelected:=0;
  fSizeInDir:=0;
  fSizeSelected:=0;
  for i:=0 to fFileList.Count-1 do
  begin
    with fFileList.GetItem(i)^ do
    begin
//      if S_ISDIR(fMode) then Continue;
      if sName='..' then Continue;
      if bSelected then
      begin
        inc(fFilesSelected);
        if not FPS_ISDIR(iMode) then
          fSizeSelected:=Cardinal(fSizeSelected)+iSize
        else
          if iDirSize<>0 then
            fSizeSelected:=Cardinal(fSizeSelected)+iDirSize;
      end;
      inc(fFilesInDir);
      if not FPS_ISDIR(iMode) then
        fSizeInDir:=Cardinal(fSizeInDir)+iSize
      else
        if iDirSize<>0 then
          fSizeInDir:=Cardinal(fSizeInDir)+iDirSize;
    end;
  end;
end;

procedure TFilePanel.cdUpLevel;
var
  i:Integer;
  bPathFound:Boolean;
begin
  if fPanelMode = pmDirectory then
    begin
      bPathFound:=False;
      fActiveDir:=ExcludeTrailingPathDelimiter(fActiveDir);
      for i:=length(fActiveDir) downto 1 do
        begin
          if fActiveDir[i] = DirectorySeparator then
            begin
              LastActive:=Copy(fActiveDir,i+1,length(fActiveDir)-i+1);
              fActiveDir:=Copy(fActiveDir,1, i);
              {$IFDEF unix}
              if gTermWindow and Assigned(Cons) then
                Cons.Terminal.Write_pty('cd "'+fActiveDir+'"'+#13#10);
              {$ENDIF}
              bPathFound:=True;
              Break;
            end;
        end;
    
      if glsDirHistory.IndexOf(ActiveDir)=-1 then
        glsDirHistory.Insert(0,ActiveDir);

      LoadPanel;
    end
  else // if VFS
    begin
      LastActive:= ExtractFileName(ExcludeTrailingPathDelimiter(fActiveDir));
      LoadPanelVFS(fFileList.GetItem(0)); // get '..' item
      fPanel.Invalidate;
    end;
end;


procedure TFilePanel.cdDownLevel(frp:PFileRecItem);
begin
  if fPanelMode = pmDirectory then
    begin
      with frp^ do
      begin
        ActiveDir:=ActiveDir+sName+DirectorySeparator;
        {$IFDEF unix}
        if gTermWindow and Assigned(Cons) then
          Cons.Terminal.Write_pty('cd "'+ActiveDir+'"'+#13#10);
        {$ENDIF}
        LastActive:='';
        if glsDirHistory.IndexOf(ActiveDir)=-1 then
          glsDirHistory.Insert(0,ActiveDir);
      end; // with frp^
      
      LoadPanel;
    end
  else // if VFS
    begin
      LastActive:='';
      LoadPanelVFS(frp);
      fPanel.Invalidate;
    end;
end;

function TFilePanel.GetActiveItem:PFileRecItem;
begin
  Result:= nil;
  if IsEmpty then Exit; // No files in the panel.
  if fPanel.Row < fPanel.FixedRows then
    fPanel.Row:= fPanel.FixedRows;
//  DebugLn(fPanel.Row, ' ', fRefList.Count);
  if fPanel.Row > fRefList.Count then
     fPanel.Row:= fPanel.FixedRows;
  Result:= fRefList.Items[fPanel.Row-fPanel.FixedRows]; // minus fixed header
end;

procedure TFilePanel.MarkGroup(const sMask:String; bSelect:Boolean);
var
  i:Integer;
  frp:PFileRecItem;
begin
    for i:=0 to fFileList.Count-1 do
    begin
      frp:=fFileList.GetItem(i);
      if (frp^.sName='..') then Continue;
      if MatchesMaskList(frp^.sName, sMask) then
        frp^.bSelected := bSelect;
    end;
end;

procedure TFilePanel.UpdatePrompt;
const PTLen=40;
var
  FreeSize,
  TotalSize : Int64;
begin
  with flblCurPath do
  begin
    AutoSize:=False;
    if length(ActiveDir)>PTLen then
      Caption:='['+copy(ActiveDir,length(ActiveDir)-PTLen,PTLen)+']$:'
    else
    Caption:='['+ActiveDir+']$:';
    AutoSize:=True;
    Left:=1;
  end;
  
  fedtCommand.Left:=flblCurPath.Width+5;
  fedtCommand.Width:=TControl(fedtCommand.Parent).Width-fedtCommand.Left;
  if fPanelMode=pmDirectory then
  begin
    GetDiskFreeSpace(fActiveDir, FreeSize, TotalSize);
    flblFree.Caption := Format(rsFreeMsg,[cnvFormatFileSize(FreeSize),cnvFormatFileSize(TotalSize)]);
  end
  else
  //TODO
    flblFree.Caption:=Format(rsFreeMsg,[cnvFormatFileSize(0),cnvFormatFileSize(0)]);
end;

procedure TFilePanel.SetActiveDir(const AValue:String);
begin
  fPrevActiveDir:= fActiveDir;
  fActiveDir:= IncludeTrailingBackslash(AValue);
end;

function TFilePanel.GetActiveDir:String;
begin
  Result:= IncludeTrailingBackslash(fActiveDir);
end;

function TFilePanel.GetReferenceItemPtr(iIndex:Integer):PFileRecItem;
begin
  Result:= nil;
  if iIndex >= fRefList.Count then Exit;
  Result:= PFileRecItem(fRefList.Items[iIndex]);
end;

function TFilePanel.IsEmpty:Boolean;
begin
  Result := (fRefList.Count = 0);
end;

function TFilePanel.IsItemValid(frp:PFileRecItem):Boolean;
begin
  if Assigned(frp) and (frp^.sName <> '..') then
    Result := True
  else
    Result := False;
end;

end.

