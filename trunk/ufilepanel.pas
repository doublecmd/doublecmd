{
Seksi Commander
----------------------------
Implementing of storing Files and main file operation

Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:

Copyright (C) 2006-2007 Alexander Koblov (Alexx2000@mail.ru)


}
unit uFilePanel;

{$mode objfpc}{$H+}

interface
uses
  StdCtrls, Grids, uFileList, uTypes, uPathHistory, Classes, uVFS;

type
  TOnChangeDirectory = procedure (Sender: TObject; const NewDir : String) of object;

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
    FOnChangeDirectory : TOnChangeDirectory;
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
    procedure ChooseFile(pfri:PFileRecItem); // main input node
    procedure ExecuteFile(const sName:String; bTerm:Boolean);
    function GetFileItem(iIndex:Integer):TFileRecItem;
    function GetFileItemPtr(iIndex:Integer):PFileRecItem;
    function GetReferenceItemPtr(iIndex:Integer):PFileRecItem;
    function GetActiveItem:PFileRecItem;
    function GetSelectedCount:Integer;
    procedure InvertFileSection(frp:PFileRecItem);
    procedure MarkAllFiles(bMarked:Boolean);
    procedure InvertAllFiles;
    procedure UpdateCountStatus;
    procedure cdUpLevel;
    procedure cdDownLevel(frp:PFileRecItem);
    procedure MarkGroup(const sMask:String; bSelect:Boolean); // second parametr is switch sel/uns
    procedure UpdatePrompt;
    function ProcessExtCommand(sCmd:String{; pfr:PFileRecItem}) : Boolean;
    procedure ReplaceExtCommand(var sCmd:String; pfr:PFileRecItem);
    procedure SetActiveDir(const AValue:String);
    function GetActiveDir:String;
    property OnChangeDirectory : TOnChangeDirectory read FOnChangeDirectory write FOnChangeDirectory;

  published
    property SortDirection:Boolean read fSortDirect write fSortDirect; // maybe write method
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
  SysUtils, uFileOp, uGlobs, uVFSutil,
  uShowMsg, Controls, uLng, uShowForm, uVFSmodule, uDCUtils,
  uOSUtils;

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
  //flblPath.Height := 14;
  //WriteLN('Path = ', flblPath.Caption);
//  writeln('fPanel.Row:',fPanel.Row);
//  writeln('TFilePanel:', fFileList.Count);
  bAnyRow:=fPanel.Row>=0;
  fRefList.Clear;
  for i:=0 to fFileList.Count-1 do
  begin
    pfri:=fFileList.GetItem(i);
    with pfri^ do
    begin
      if not gShowSystemFiles and (sName[1]='.') and (sName<>'..') then Continue;
      fRefList.Add(pfri);
    end;
  end;

  fPanel.RowCount:=fRefList.Count+1; // one is header
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
            fPanel.Row:=i+1;
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
  sTempDir : String;
  I : Integer;
begin
  with frp^ do
  begin
    if (fPanelMode in [pmArchive, pmVFS]) then
     begin
      if sName = '..' then
        begin
          fActiveDir := fVFS.ArcFullName + sPath;

          //WriteLN(output, 'UpDir = ' + sPath);

          if not fVFS.cdUpLevel(frp, fFileList) then
            begin
              if fVFSmoduleList.Count <> 0 then  // if in sub archive then return in parent VFS
                begin
                  DeleteFile(fVFS.ArcFullName);
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
                      ChDir(fActiveDir);
                      if Assigned(FOnChangeDirectory) then
                        FOnChangeDirectory(fOwner, fActiveDir);
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
              fVFSmoduleList.AddObject(fVFS.ArcFullName + '=' + sPath, fVFS.VFSmodule);

              //WriteLN('sPath ==' + sPath);

              VFSFileList := TFileList.Create;
              VFSFileList.CurrentDirectory := ActiveDir;

              //WriteLN('ActiveDir == ' + ActiveDir);

              sName := ActiveDir + sName;
              VFSFileList.AddItem(frp);
              sTempDir := GetTempDir;
              {if }fVFS.VFSmodule.VFSCopyOut(VFSFileList, sTempDir, 0);{ then}
                begin
                 fVFS.LoadAndOpen(sTempDir + ExtractDirLevel(ActiveDir, sName));

                 //WriteLN('sTempDir + sName == ' + sTempDir + sName);

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
      fFileList.UpdateFileInformation(PanelMode);
      Sort;
    end;
end;


procedure TFilePanel.LoadPanel;
begin
//  writeln('TFilePanel.LoadPanel');
  case fPanelMode of
  pmArchive,
  pmVFS:
    ;
    {VFS.VFSListItems(fPathHistory.GetLastPath,ActiveDir,fFileList);}
  else
    begin
      // classic filesystem
      if not SetCurrentDir(ActiveDir) then
      begin
        GetDir(0,fActiveDir);
        if fActiveDir<>DirectorySeparator then
          fActiveDir:=fActiveDir+DirectorySeparator;
        Exit;   // chdir failed
      end;
      if Assigned(FOnChangeDirectory) then
        FOnChangeDirectory(fOwner, fActiveDir);
      LoadFilesbyDir(fActiveDir, fFileList);
    end;
  end; // case
  fFileList.UpdateFileInformation(fPanelMode);
  Sort; // and Update panel
  fPanel.Invalidate;
//  writeln('TFilePanel.LoadPanel DONE');
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
  if not gShowSystemFiles and (frp^.sName[1]='.') then Exit;
  frp^.bSelected:=not frp^.bSelected;
end;

procedure TFilePanel.InvertAllFiles;
var
  i:Integer;
begin
  for i:=0 to fFileList.Count-1 do
    InvertFileSection(fFileList.GetItem(i));
end;


procedure TFilePanel.ChooseFile(pfri:PFileRecItem);
var
  sOpenCmd:String;
begin
// main file input point for decision
//  writeln(pfri^.sName);

  with pfri^ do
  begin
    if (fPanelMode=pmVFS) or ((sModeStr = 'wfx') and fVFS.FindModule(sPath + sName)) then
    begin
      LoadPanelVFS(pfri);
      Exit;
    end;
    if (fPanelMode=pmArchive) or (not FPS_ISDIR(iMode) and fVFS.FindModule(sPath + sName)) then
    begin
      LoadPanelVFS(pfri);
      Exit;
    end;
    if (sName='..') then
    begin
      cdUpLevel;
      Exit;
    end;
    
    if FPS_ISDIR(iMode) or bLinkIsDir then // deeper and deeper
    begin
      cdDownLevel(pfri);
      Exit;
    end;
    //now test if exists Open command in doublecmd.ext :)
    sOpenCmd:=gExts.GetCommandText(lowercase(ExtractFileExt(sName)),'open');
    if (sOpenCmd<>'') then
    begin
      if Pos('{!VFS}',sOpenCmd)>0 then
      begin
        if fVFS.FindModule(sName) then
//        if VFS.VFSGetScriptName(ExtractFileName(sName))<>'' then
        begin
          LoadPanelVFS(pfri);
          Exit;
        end;
      end;
      LastActive:=sName;

      ReplaceExtCommand(sOpenCmd, pfri);
      ProcessExtCommand(sOpenCmd{, frp});
      Exit;
    end;
    // and at the end try if it is executable
    if bExecutable then
      begin
        System.ChDir(ActiveDir);
        LastActive:=sName;
        ExecuteFile(sName, False);
//        ExecCmdFork('./'+sName);
        LoadPanel;
        Exit;
      end;
  end;
end;

procedure TFilePanel.ExecuteFile(const sName:String; bTerm:Boolean);
begin
  if bTerm then
//    ExecCmdFork(Format(gTerm,[sName+'|less']))
    ExecCmdFork(Format(gTerm,[sName+';echo ''Press Enter'';read']))
  else
    ExecCmdFork(sName);
end;

procedure TFilePanel.MarkAllFiles(bMarked:Boolean);
var
  i:Integer;
  fr:PFileRecItem;

begin
  for i:=0 to fFileList.Count-1 do
  begin
    fr:=fFileList.GetItem(i);
    if not gShowSystemFiles and (fr^.sName[1]='.') then
// system files is always not selected if not showed
      fr^.bSelected:=False
    else
      fr^.bSelected:=bMarked;
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
          fSizeSelected:=Cardinal(fSizeSelected)+iDirSize;
    end;
  end;
end;

procedure TFilePanel.cdUpLevel;
var
  i:Integer;
  bPathFound:Boolean;
begin
  bPathFound:=False;
  fActiveDir:=ExcludeTrailingPathDelimiter(fActiveDir);
  for i:=length(fActiveDir) downto 1 do
    begin
      if fActiveDir[i] = DirectorySeparator then
        begin
          LastActive:=Copy(fActiveDir,i+1,length(fActiveDir)-i+1);
          fActiveDir:=Copy(fActiveDir,1, i);
          bPathFound:=True;
          Break;
        end;
    end;
    
  if glsDirHistory.IndexOf(ActiveDir)=-1 then
    glsDirHistory.Insert(0,ActiveDir);

  LoadPanel;
end;


procedure TFilePanel.cdDownLevel(frp:PFileRecItem);
{var
  i:Integer;}
begin
  with frp^ do
  begin
    ActiveDir:=ActiveDir+sName+DirectorySeparator;
    LastActive:='';
    if glsDirHistory.IndexOf(ActiveDir)=-1 then
      glsDirHistory.Insert(0,ActiveDir);
  end; // with frp^
  LoadPanel;
end;

function TFilePanel.GetActiveItem:PFileRecItem;
begin
  Result:=nil;
  if fPanel.Row<1 then
    SysUtils.Abort;
//  writeln(fPanel.Row, ' ', fRefList.Count);
  if fPanel.Row>fRefList.Count then
    SysUtils.Abort;
  Result:=fRefList.Items[fPanel.Row-1]; // 1 is fixed header
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
      if G_ValidateWildText(frp^.sName, sMask) then
        frp^.bSelected := bSelect;
    end;
end;

procedure TFilePanel.UpdatePrompt;
var
  FreeSize,
  TotalSize : Int64;
begin
  with flblCurPath do
  begin
    AutoSize:=False;
    Caption:='['+ActiveDir+']$:';
    AutoSize:=True;
    Left:=1;
  end;
  
  fedtCommand.Left:=flblCurPath.Width+5;
  fedtCommand.Width:=TControl(fedtCommand.Parent).Width-fedtCommand.Left;
  if fPanelMode=pmDirectory then
  begin
    GetDiskFreeSpace(fActiveDir, FreeSize, TotalSize);
    flblFree.Caption := Format(lngGetString(clngFreeMsg),[cnvFormatFileSize(FreeSize),cnvFormatFileSize(TotalSize)]);
  end
  else
  //TODO
    flblFree.Caption:=Format(lngGetString(clngFreeMsg),[cnvFormatFileSize(0),cnvFormatFileSize(0)]);
end;

procedure TFilePanel.ReplaceExtCommand(var sCmd:String; pfr:PFileRecItem);
begin
  with pfr^ do
  begin
    sCmd:=StringReplace(sCmd,'%f',ExtractFileName(sName),[rfReplaceAll]);
    sCmd:=StringReplace(sCmd,'%d',ActiveDir,[rfReplaceAll]);
    sCmd:=Trim(StringReplace(sCmd,'%p',ActiveDir+ExtractFileName(sName),[rfReplaceAll]));
  end;
end;

function TFilePanel.ProcessExtCommand(sCmd:String{; pfr:PFileRecItem}) : Boolean;
begin
  Result := False;
  if Pos('{!SHELL}', sCmd)>0 then
  begin
    sCmd:=StringReplace(sCmd,'{!SHELL}','',[rfReplaceAll]);
    sCmd:=Format(gTerm,[sCmd]);
    Result := True;
  end;
  if Pos('{!EDITOR}',sCmd)>0 then
  begin
    sCmd:=StringReplace(sCmd,'{!EDITOR}','',[rfReplaceAll]);
    uShowForm.ShowEditorByGlob(sCmd);
    Result := True;
    Exit;
  end;
  if Pos('{!VIEWER}',sCmd)>0 then
  begin
    sCmd:=StringReplace(sCmd,'{!VIEWER}','',[rfReplaceAll]);
    uShowForm.ShowViewerByGlob(sCmd);
    Result := True;
    Exit;
  end;
  System.ChDir(ActiveDir);
end;

procedure TFilePanel.SetActiveDir(const AValue:String);
begin
  fActiveDir := IncludeTrailingBackslash(AValue);
end;

function TFilePanel.GetActiveDir:String;
begin
  Result := IncludeTrailingBackslash(fActiveDir);
end;

function TFilePanel.GetReferenceItemPtr(iIndex:Integer):PFileRecItem;
begin
  Result:=nil;
  if iIndex>=fRefList.Count then Exit;
  Result:=PFileRecItem(fRefList.Items[iIndex]);
end;
end.

