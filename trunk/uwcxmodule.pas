{
   Double commander
   -------------------------------------------------------------------------
   Archive File support - class for manage WCX plugins (Version 2.10)

   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)

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

unit uWCXmodule;

interface
uses
  uWCXprototypes, uWCXhead, uFileList, uTypes, dynlibs, Classes, uVFSModule,
  uVFSTypes, uVFSUtil, fFileOpDlg, Dialogs;

{$H+}
const
  OP_EXTRACT = 0;
  OP_PACK = 1;

Type
  TWCXModule = class;
  
  PHeaderData = ^THeaderData;
  
  { Packing/Unpacking thread }
  
   TWCXCopyThread = class(TThread)
   protected
     procedure Execute; override;
   public
     Operation : Integer;
     WCXModule : TWCXModule;
   end;
  
  
  { TWCXModule }

  TWCXModule = class (TVFSModule)
  private
    FArcFileList : TList;
    FFileList : TFileList;
    FFileMask : String;
    FFlags : Integer;
    FDstPath,
    fFolder : String;
    FFilesSize: Int64;
    FPercent : Double;
    CT : TWCXCopyThread;         // Packing/Unpacking thread
    FFileOpDlg: TfrmFileOp; // progress window
    procedure ShowErrorMessage;
    function WCXCopyOut : Boolean; {Extract files from archive}
    function WCXCopyIn : Boolean;  {Pack files in archive}

    procedure CopySelectedWithSubFolders(var flist:TFileList);
  protected
    // module's functions
  //**mandatory:
    OpenArchive : TOpenArchive;
    ReadHeader : TReadHeader;
    ProcessFile : TProcessFile;
    CloseArchive : TCloseArchive;
  //**optional:
    PackFiles : TPackFiles;
    DeleteFiles : TDeleteFiles;
    GetPackerCaps : TGetPackerCaps;
    ConfigurePacker : TConfigurePacker;
    SetChangeVolProc : TSetChangeVolProc;
    SetProcessDataProc : TSetProcessDataProc;
    StartMemPack : TStartMemPack;
    PackToMem : TPackToMem;
    DoneMemPack : TDoneMemPack;
    CanYouHandleThisFile : TCanYouHandleThisFile;
    PackSetDefaultParams : TPackSetDefaultParams;
    FModuleHandle:TLibHandle;  // Handle to .DLL or .so
    FArchiveName : String;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadModule(const sName:String):Boolean;override; {Load WCX plugin}
    procedure UnloadModule;override;                          {UnLoad WCX plugin}

    function VFSInit:Boolean;override;
    procedure VFSDestroy;override;
    function VFSCaps : TVFSCaps;override;

    function VFSConfigure(Parent: THandle):Boolean;override;
    function VFSOpen(const sName:String; bCanYouHandleThisFile : Boolean = False):Boolean;override;
    function VFSClose:Boolean;override;
    function VFSRefresh : Boolean;override;

    function VFSMkDir(const sDirName:String ):Boolean;override;{Create a directory}
    function VFSRmDir(const sDirName:String):Boolean;override; {Remove a directory}

    function VFSCopyOut(var flSrcList : TFileList; sDstPath:String; Flags: Integer):Boolean;override;{Extract files from archive}
    function VFSCopyIn(var flSrcList : TFileList; sDstName:String;  Flags : Integer):Boolean;override;{Pack files in archive}
    function VFSCopyOutEx(var flSrcList : TFileList; sDstPath:String; Flags: Integer):Boolean;override;{Extract files from archive in thread}
    function VFSCopyInEx(var flSrcList : TFileList; sDstName:String;  Flags : Integer):Boolean;override;{Pack files in archive in thread}

    function VFSRename(const sSrcName, sDstName:String):Boolean;override;{Rename or move file}
    function VFSRun(const sName:String):Boolean;override;
    function VFSDelete(var flNameList:TFileList):Boolean;override;{Delete files from archive}

    function VFSList(const sDir:String; var fl:TFileList ):Boolean;override;{Return the filelist of archive}
    function VFSMisc : Cardinal;override;
  end;

function IsBlocked : Boolean;

implementation
uses Forms, SysUtils, Masks, uFileOp, uGlobs, uLog, uOSUtils, LCLProc, uFileProcs, uDCUtils, uLng, Controls;

var
  WCXModule : TWCXModule;  // used in ProcessDataProc
  iResult : Integer;

constructor TWCXModule.Create;
begin
  FFilesSize:= 0;
  FPercent := 0;
end;

destructor TWCXModule.Destroy;
begin
  UnloadModule;
end;

function TWCXModule.LoadModule(const sName:String):Boolean;
var
  PackDefaultParamStruct : pPackDefaultParamStruct;
begin
  FModuleHandle := LoadLibrary(sName);
  Result := (FModuleHandle <> 0);
  if  FModuleHandle = 0 then exit;
  //DebugLN('FModuleHandle =', FModuleHandle);
  OpenArchive:= TOpenArchive(GetProcAddress(FModuleHandle,'OpenArchive'));
  @ReadHeader:= GetProcAddress(FModuleHandle,'ReadHeader');
  @ProcessFile:= GetProcAddress(FModuleHandle,'ProcessFile');
  @CloseArchive:= GetProcAddress(FModuleHandle,'CloseArchive');
  if ((@OpenArchive = nil)or(@ReadHeader = nil)or
   (@ProcessFile = nil)or(@CloseArchive = nil)) then
    begin
      OpenArchive := nil;
      ReadHeader:= nil;
      ProcessFile := nil;
      CloseArchive := nil;
      Result := False;
      Exit;
    end;
  @PackFiles:= GetProcAddress(FModuleHandle,'PackFiles');
  @DeleteFiles:= GetProcAddress(FModuleHandle,'DeleteFiles');
  @GetPackerCaps:= GetProcAddress(FModuleHandle,'GetPackerCaps');
  @ConfigurePacker:= GetProcAddress(FModuleHandle,'ConfigurePacker');
  @SetChangeVolProc:= GetProcAddress(FModuleHandle,'SetChangeVolProc');
  @SetProcessDataProc:= GetProcAddress(FModuleHandle,'SetProcessDataProc');
  @StartMemPack:= GetProcAddress(FModuleHandle,'StartMemPack');
  @PackToMem:= GetProcAddress(FModuleHandle,'PackToMem');
  @DoneMemPack:= GetProcAddress(FModuleHandle,'DoneMemPack');
  @CanYouHandleThisFile:= GetProcAddress(FModuleHandle,'CanYouHandleThisFile');
  @PackSetDefaultParams:= GetProcAddress(FModuleHandle,'PackSetDefaultParams');
  
  if Assigned(PackSetDefaultParams) then
    begin
      with PackDefaultParamStruct^ do
        begin
          Size := SizeOf(PackDefaultParamStruct^);
          PluginInterfaceVersionLow := 10;
          PluginInterfaceVersionHi := 2;
          DefaultIniName := '';
        end;
      PackSetDefaultParams(PackDefaultParamStruct);
    end;
end;

procedure TWCXModule.UnloadModule;
begin
  if FModuleHandle <> 0 then
    FreeLibrary(FModuleHandle);
  FModuleHandle := 0;
  @OpenArchive:= nil;
  @ReadHeader:= nil;
  @ProcessFile:= nil;
  @CloseArchive:= nil;
  @PackFiles:= nil;
  @DeleteFiles:= nil;
  @GetPackerCaps:= nil;
  @ConfigurePacker:= nil;
  @SetChangeVolProc:= nil;
  @SetProcessDataProc:= nil;
  @StartMemPack:= nil;
  @PackToMem:= nil;
  @DoneMemPack:= nil;
  @CanYouHandleThisFile:= nil;
  @PackSetDefaultParams:= nil;
end;

procedure  ShowErrorMsg(iErrorMsg : Integer);
var
  sErrorMsg : String;
begin
  case iErrorMsg of
  E_END_ARCHIVE    :   sErrorMsg := rsMsgErrEndArchive;
  E_NO_MEMORY      :   sErrorMsg := rsMsgErrNoMemory;
  E_BAD_DATA       :   sErrorMsg := rsMsgErrBadData;
  E_BAD_ARCHIVE    :   sErrorMsg := rsMsgErrBadArchive;
  E_UNKNOWN_FORMAT :   sErrorMsg := rsMsgErrUnknownFormat;
  E_EOPEN          :   sErrorMsg := rsMsgErrEOpen;
  E_ECREATE        :   sErrorMsg := rsMsgErrECreate;
  E_ECLOSE         :   sErrorMsg := rsMsgErrEClose;
  E_EREAD          :   sErrorMsg := rsMsgErrERead;
  E_EWRITE         :   sErrorMsg := rsMsgErrEWrite;
  E_SMALL_BUF      :   sErrorMsg := rsMsgErrSmallBuf;
  E_EABORTED       :   sErrorMsg := rsMsgErrEAborted;
  E_NO_FILES       :   sErrorMsg := rsMsgErrNoFiles;
  E_TOO_MANY_FILES :   sErrorMsg := rsMsgErrTooManyFiles;
  E_NOT_SUPPORTED  :   sErrorMsg := rsMsgErrNotSupported;
  end;

  // write log error
  if (log_arc_op in gLogOptions) and (log_errors in gLogOptions) then
    logWrite(rsMsgLogError + sErrorMsg, lmtError);

  // Standart error modal dialog
  ShowMessage(sErrorMsg);
end;

function ChangeVolProc(ArcName : Pchar; Mode:Longint):Longint; stdcall;
begin
  case Mode of
  PK_VOL_ASK:
    ArcName := PChar(InputBox ('Double Commander', rsMsgSelLocNextVol, ArcName));
  PK_VOL_NOTIFY:
    ShowMessage(rsMsgNextVolUnpack);
  end;
end;

function ProcessDataProc(FileName: PChar; Size: Integer): Integer; stdcall;
begin
  DebugLN('Working ' + FileName + ' Size = ' + IntToStr(Size));

  Result := 1;
  with WCXModule do
  begin
    if FFileOpDlg.ModalResult = mrCancel then // Cancel operation
      Result := 0;

    FFileOpDlg.sFileName := FileName;

    if not (Size < 0) then
      begin
        FPercent := FPercent + ((Size * 100) / FFilesSize);
        DebugLN('Percent = ' + IntToStr(Round(FPercent)));

        FFileOpDlg.iProgress1Pos := 100;
        FFileOpDlg.iProgress2Pos := Round(FPercent);
      end
    else // For plugins which unpack in CloseArchive
      if (Size >= -100) and (Size <= -1) then // first percent bar
        begin
          FFileOpDlg.iProgress1Pos := (Size * -1);
          DebugLN('Working ' + FileName + ' Percent1 = ' + IntToStr(FFileOpDlg.iProgress1Pos));
        end
      else if (Size >= -1100) and (Size <= -1000) then // second percent bar
        begin
          FFileOpDlg.iProgress2Pos := (Size * -1) - 1000;
          DebugLN('Working ' + FileName + ' Percent2 = ' + IntToStr(FFileOpDlg.iProgress2Pos));
        end;
        
        
    if Assigned(CT) then
      CT.Synchronize(FFileOpDlg.UpdateDlg)
    else
      begin
        FFileOpDlg.UpdateDlg;
        Application.ProcessMessages;
      end;
  end; //with
end;

procedure TWCXModule.ShowErrorMessage;
begin
  ShowErrorMsg(iResult);
end;

function TWCXModule.VFSInit: Boolean;
begin

end;

procedure TWCXModule.VFSDestroy;
begin

end;

function TWCXModule.VFSCaps: TVFSCaps;
begin
  Result := [];
  Include(Result, VFS_CAPS_COPYOUT);
  if Assigned(PackFiles) then
    Include(Result, VFS_CAPS_COPYIN);
  if Assigned(DeleteFiles) then
    Include(Result, VFS_CAPS_DELETE);
end;

function TWCXModule.VFSConfigure(Parent: THandle): Boolean;
begin
  if @ConfigurePacker <> nil then
    ConfigurePacker(Parent, FModuleHandle);
end;


function TWCXModule.VFSOpen(const sName: String; bCanYouHandleThisFile : Boolean = False): Boolean;
var
  ArcHandle : THandle;
  ArcFile : tOpenArchiveData;
  ArcHeader : THeaderData;
  HeaderData : PHeaderData;
  bHasDir : Boolean;
  sDirs : TStringList;
  I : Integer;
begin
  bHasDir := False;
  sDirs := TStringList.Create;
  
  FArchiveName := sName;
  DebugLN('FArchiveName = ' + FArchiveName);

  if not FileExists(FArchiveName) then
    begin
      Result := False;
      Exit;
    end;

  if bCanYouHandleThisFile and Assigned(CanYouHandleThisFile) then
    begin
      Result := CanYouHandleThisFile(PChar(sName));
      if not Result then Exit;
    end;

  DebugLN('Open Archive');

  (*Open Archive*)
  FillChar(ArcFile, SizeOf(ArcFile), #0);
  ArcFile.ArcName := PChar(sName);
  ArcFile.OpenMode := PK_OM_LIST;

  try
    ArcHandle := OpenArchive(ArcFile);
  except
    ArcHandle := 0;
  end;
  
  if ArcHandle = 0 then
    begin
      if not bCanYouHandleThisFile then
        ShowErrorMsg(ArcFile.OpenResult);
      Result := False;
      Exit;
    end;
  {
  WCXModule := Self;  // set WCXModule variable to current module
  SetChangeVolProc(ArcHandle, ChangeVolProc);
  SetProcessDataProc(ArcHandle, ProcessDataProc);
  }

  DebugLN('Get File List');
  (*Get File List*)
  FillChar(ArcHeader, SizeOf(ArcHeader), #0);
  FArcFileList := TList.Create;

  try
    while (ReadHeader(ArcHandle, ArcHeader) = 0) do
      begin
        New(HeaderData);
        HeaderData^ := ArcHeader;
        FArcFileList.Add(HeaderData);
        //****************************
        (* if plugin is not list a list of folders *)
        if not bHasDir then
          begin
            bHasDir := FPS_ISDIR(HeaderData^.FileAttr);
            GetDirs(String(HeaderData^.FileName), sDirs);
          end;
        //****************************
        FillChar(ArcHeader, SizeOf(ArcHeader), #0);
        // get next file
        iResult := ProcessFile(ArcHandle, PK_SKIP, nil, nil);

        //Check for errors
        if iResult <> 0 then
          ShowErrorMessage;
    end; // while
    
    (* if plugin is not list a list of folders *)
    if not bHasDir then
      begin
        for I := 0 to sDirs.Count - 1 do
          begin
            FillChar(ArcHeader, SizeOf(ArcHeader), #0);
            ArcHeader.FileName := sDirs.Strings[I];
            ArcHeader.FileAttr := faFolder;
            ArcHeader.FileTime := FileAge(FArchiveName);
            New(HeaderData);
            HeaderData^ := ArcHeader;
            FArcFileList.Add(HeaderData);
          end;
      end;
  finally
    sDirs.Free;
    CloseArchive(ArcHandle);
  end;
  Result := True;
end;

function TWCXModule.VFSClose: Boolean;
begin

end;

function TWCXModule.VFSRefresh: Boolean;
begin
  Result := VFSOpen(FArchiveName)
end;

function TWCXModule.VFSMkDir(const sDirName: String): Boolean;
begin

end;

function TWCXModule.VFSRmDir(const sDirName: String): Boolean;
begin

end;

function GetFileList(var fl:TFileList) : String;
var
  I, Count : Integer;
  FileList : String;
begin
  I := 1;
  Count := fl.Count - 1;
  FileList := fl.GetItem(0)^.sName;
  while I <= Count do
    begin
      FileList := FileList + #0 + fl.GetItem(I)^.sName;
      I := I + 1;
    end;
  FileList := FileList + #0#0;
  DebugLN('FileList := ' + FileList);
  Result := FileList;
end;

function TWCXModule.WCXCopyOut : Boolean;
var
  ArcHandle : THandle;
  ArcFile : tOpenArchiveData;
  ArcHeader : THeaderData;
  Extract : Boolean;
  Count, I : Integer;
  Folder : String;
begin
   FPercent := 0;

   FFileMask := ExtractFileName(FDstPath);
   if FFileMask = '' then FFileMask := '*';  // extract all selected files/folders
   FDstPath := ExtractFilePath(FDstPath);
   
   (* Get current folder in archive *)
   Folder := FFileList.CurrentDirectory; //LowDirLevel(FFileList.GetItem(0)^.sName);

   (* Get relative path *)
   IncludeFileInList(FArchiveName, Folder);

   FFolder := Folder;

   DebugLN('Folder = ' + Folder);

   //sDstPath := ExcludeTrailingPathDelimiter(sDstPath);

   CopySelectedWithSubFolders(FFileList);
   DebugLN('Extract file = ' + FArchiveName + DirectorySeparator + ArcHeader.FileName);


  Count := FFileList.Count;
  FillChar(ArcFile, SizeOf(ArcFile), #0);
  ArcFile.ArcName := PChar(FArchiveName);
  ArcFile.OpenMode := PK_OM_EXTRACT;
  ArcHandle := OpenArchive(ArcFile);

  if ArcHandle = 0 then
   begin
    if Assigned(CT) then
      begin
        iResult := ArcFile.OpenResult;
        CT.Synchronize(ShowErrorMessage);
      end
    else
      ShowErrorMsg(ArcFile.OpenResult);
    Result := False;
    Exit;
   end;

  WCXModule := Self;  // set WCXModule variable to current module
  SetChangeVolProc(ArcHandle, ChangeVolProc);
  SetProcessDataProc(ArcHandle, ProcessDataProc);


  FillChar(ArcHeader, SizeOf(ArcHeader), #0);
  while (ReadHeader(ArcHandle, ArcHeader) = 0) do
   begin

     if  FFileList.CheckFileName(ArcHeader.FileName) >= 0 then // Want To Extract This File
       begin
         DebugLN(FDstPath + ExtractDirLevel(Folder, PathDelim + ArcHeader.FileName));

         if (FFileMask <> '*.*') and (FFileMask <> '*') then
           ForceDirectory(ExtractFilePath(FDstPath + ExtractDirLevel(Folder, PathDelim + ArcHeader.FileName)));


         iResult := ProcessFile(ArcHandle, PK_EXTRACT, nil, PChar(FDstPath + ExtractDirLevel(Folder, PathDelim + ArcHeader.FileName)));

         //Check for errors
         if iResult <> 0 then
           begin
             if Assigned(CT) then
               begin
                 // write log error
                 if (log_arc_op in gLogOptions) and (log_errors in gLogOptions) then
                   logWrite(CT, Format(rsMsgLogError+rsMsgLogExtract, [FArchiveName + PathDelim + ArcHeader.FileName+' -> '+FDstPath+ExtractDirLevel(Folder, PathDelim + ArcHeader.FileName)]), lmtError);
                 // Standart error modal dialog
                 CT.Synchronize(ShowErrorMessage)
               end
             else
               begin
                 // write log error
                 if (log_arc_op in gLogOptions) and (log_errors in gLogOptions) then
                   logWrite(Format(rsMsgLogError+rsMsgLogExtract, [FArchiveName + PathDelim + ArcHeader.FileName+' -> '+FDstPath+ExtractDirLevel(Folder, PathDelim + ArcHeader.FileName)]), lmtError);
                 // Standart error modal dialog
                 ShowErrorMessage;
               end;
           end // Error
         else
           begin
             if Assigned(CT) then
               begin
                 // write log success
                 if (log_arc_op in gLogOptions) and (log_success in gLogOptions) then
                   logWrite(CT, Format(rsMsgLogSuccess+rsMsgLogExtract, [FArchiveName + PathDelim + ArcHeader.FileName+' -> '+FDstPath+ExtractDirLevel(Folder, PathDelim + ArcHeader.FileName)]), lmtSuccess);
               end
             else
               begin
                 // write log success
                 if (log_arc_op in gLogOptions) and (log_success in gLogOptions) then
                   logWrite(Format(rsMsgLogSuccess+rsMsgLogExtract, [FArchiveName + PathDelim + ArcHeader.FileName+' -> '+FDstPath+ExtractDirLevel(Folder, PathDelim + ArcHeader.FileName)]), lmtSuccess);
               end;
           end; // Success
           
       end // CheckFileName
     else // Skip
       begin
         iResult := ProcessFile(ArcHandle, PK_SKIP, nil, nil);

         //Check for errors
         if iResult <> 0 then
           if Assigned(CT) then
             CT.Synchronize(ShowErrorMessage)
           else
             ShowErrorMessage;
       end; // Skip
     FillChar(ArcHeader, SizeOf(ArcHeader), #0);
   end;
  CloseArchive(ArcHandle);
  Result := True;
end;

function TWCXModule.WCXCopyIn : Boolean;
var
  FileList, Folder, pDstPath : PChar;
  I : Integer;
begin
  DebugLN('VFSCopyIn =' + FArchiveName);
  FPercent := 0;
  New(FileList);
  New(Folder);
  FDstPath := ExtractDirLevel(FArchiveName + PathDelim, FDstPath);
  FDstPath := ExcludeTrailingPathDelimiter(FDstPath);

  if FDstPath = '' then
    pDstPath := nil
  else
    pDstPath := PChar(FDstPath);
    
  DebugLN('sDstPath == ' + FDstPath);

  (* Add in file list files from subfolders *)
  FillAndCount(FFileList, FFilesSize);

  DebugLN('Curr Dir := ' + FFileList.CurrentDirectory);
  Folder := PChar(FFileList.CurrentDirectory);
  

  (* Convert TFileList into PChar *)
  FileList := PChar(GetFileList(FFileList));

  WCXModule := Self;  // set WCXModule variable to current module
  SetChangeVolProc(0, ChangeVolProc);
  SetProcessDataProc(0, ProcessDataProc);

  iResult := PackFiles(PChar(FArchiveName), pDstPath, Folder, FileList, FFlags);

  //Check for errors
  if iResult <> 0 then
    begin
      if Assigned(CT) then
        begin
          // write log error
          if (log_arc_op in gLogOptions) and (log_errors in gLogOptions) then
            logWrite(CT, Format(rsMsgLogError+rsMsgLogPack, [FArchiveName]), lmtError);
          // Standart error modal dialog
          CT.Synchronize(ShowErrorMessage)
        end
      else
        begin
          // write log error
            if (log_arc_op in gLogOptions) and (log_errors in gLogOptions) then
              logWrite(Format(rsMsgLogError+rsMsgLogPack, [FArchiveName]), lmtError);
            // Standart error modal dialog
            ShowErrorMessage;
        end;
    end // Error
  else
    begin
      if Assigned(CT) then
        begin
          // write log success
          if (log_arc_op in gLogOptions) and (log_success in gLogOptions) then
            logWrite(CT, Format(rsMsgLogSuccess+rsMsgLogPack, [FArchiveName]), lmtSuccess);
        end
      else
        begin
          // write log success
          if (log_arc_op in gLogOptions) and (log_success in gLogOptions) then
            logWrite(Format(rsMsgLogSuccess+rsMsgLogPack, [FArchiveName]), lmtSuccess);
        end;
    end; // Success
end;

procedure TWCXModule.CopySelectedWithSubFolders(var flist:TFileList);

  procedure SelectFilesInSubfolders(var fl : TFileList; sDir : String);
  var
    fr : PFileRecItem;
    I, Count : Integer;
    CurrFileName : String;  // Current file name
  begin
    if (FFileMask = '*.*') or (FFileMask = '*') then
      ForceDirectory(FDstPath + ExtractDirLevel(FFolder, PathDelim + sDir));

    //DebugLN('ForceDirectory = ' + FDstPath + ExtractDirLevel(FFolder, PathDelim + sDir));

    Count := FArcFileList.Count - 1;
    for I := 0 to  Count do
     begin
       CurrFileName := PathDelim + PHeaderData(FArcFileList.Items[I])^.FileName;

       //DebugLN('sDir = ', sDir);
       //DebugLN('In folder = ' + CurrFileName);

       if not IncludeFileInList(sDir + PathDelim, CurrFileName) then
         Continue;

       if (FFileMask <> '*.*') and (FFileMask <> '*') and
          not FPS_ISDIR(PHeaderData(FArcFileList.Items[I])^.FileAttr) and
          not(MatchesMaskList(CurrFileName, FFileMask)) then
         Continue;

  //     DebugLN('In folder = ' + CurrFileName);

       New(fr);
       with fr^, PHeaderData(FArcFileList.Items[I])^  do
           begin
             sName := {FArchiveName + PathDelim +} FileName;
             iMode := FileAttr;
             if FPS_ISDIR(iMode) then
               begin
                 sExt:='';
                 //DebugLN('SelectFilesInSubfolders = ' + FileName);
                 if (FFileMask = '*.*') or (FFileMask = '*') then
                   fl.AddItem(fr);
                 SelectFilesInSubfolders(fl, FileName);
               end
             else
               begin
                 fl.AddItem(fr);
                 inc(FFilesSize, UnpSize);
               end;
          end; //with
     end;
  end;


var
  xIndex:Integer;
  fri:TFileRecItem;
  Newfl : TFileList;
  Count : Integer;
begin
  Newfl := TFileList.Create;
  Count := flist.Count-1;
  for xIndex:=0 to Count do
  begin
    fri:=flist.GetItem(xIndex)^;


    fri.sName := ExtractDirLevel(FArchiveName, fri.sName);

    if fri.sName[1] = PathDelim then
      Delete(fri.sName, 1, 1);

    if (FFileMask <> '*.*') and (FFileMask <> '*') and not(MatchesMaskList(fri.sName, FFileMask) or FPS_ISDIR(fri.iMode)) then
      Continue;
      

    DebugLN('Curr File = ' + fri.sName);

    if FPS_ISDIR(fri.iMode) then
      begin
        if (FFileMask = '*.*') or (FFileMask = '*') then
          Newfl.AddItem(@fri);
        SelectFilesInSubfolders(Newfl, fri.sName);
      end
    else
      begin
        Newfl.AddItem(@fri);
        inc(FFilesSize, fri.iSize);
      end;

  end; //for
  FreeAndNil(flist);
  flist := Newfl;
end;

{Extract files from archive}

function TWCXModule.VFSCopyOut(var flSrcList: TFileList; sDstPath: String;
  Flags: Integer): Boolean;
begin
  Result := True;
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := rsDlgExtract;

    FFileList := flSrcList;
    FDstPath := sDstPath;
    FFlags := Flags;

    CT := nil;
    WCXCopyOut;
    FFileOpDlg.Close;
    FFileOpDlg.Free;
    FFileOpDlg := nil;
  except
    FFileOpDlg := nil;
    Result := False;
  end;
end;

{Pack files}

function TWCXModule.VFSCopyIn(var flSrcList: TFileList; sDstName: String; Flags : Integer
  ): Boolean;
begin
  Result := True;
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := rsDlgPack;

    FFileList := flSrcList;
    FDstPath := sDstName;
    FFlags := Flags;

    CT := nil;
    WCXCopyIn;
    FFileOpDlg.Close;
    FFileOpDlg.Free;
    FFileOpDlg := nil;
  except
    FFileOpDlg := nil;
    Result := False;
  end;
end;

{Extract files from archive in thread}

function TWCXModule.VFSCopyOutEx(var flSrcList: TFileList; sDstPath: String;
  Flags: Integer): Boolean;
begin
  Result := True;
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := rsDlgExtract;
  
    FFileList := flSrcList;
    FDstPath := sDstPath;
  
    CT := TWCXCopyThread.Create(True);
    CT.FreeOnTerminate := True;
    CT.Operation := OP_EXTRACT;
    CT.WCXModule := Self;
    FFileOpDlg.Thread := TThread(CT);
    CT.Resume;
  except
    FFileOpDlg := nil;
    Result := False;
  end;
end;

{Pack files in thread}

function TWCXModule.VFSCopyInEx(var flSrcList: TFileList; sDstName: String; Flags : Integer
  ): Boolean;
begin
  Result := True;
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := rsDlgPack;
    
    FFileList := flSrcList;
    FDstPath := sDstName;
    FFlags := Flags;
  
    CT := TWCXCopyThread.Create(True);
    CT.FreeOnTerminate := True;
    CT.Operation := OP_PACK;
    CT.WCXModule := Self;
    FFileOpDlg.Thread := TThread(CT);
    CT.Resume;
  except
    FFileOpDlg := nil;
    Result := False;
  end;
end;

function TWCXModule.VFSRename(const sSrcName, sDstName: String): Boolean;
begin

end;

function TWCXModule.VFSRun(const sName: String): Boolean;
begin

end;

function TWCXModule.VFSDelete(var flNameList: TFileList): Boolean;
var
  Folder : String;
begin
  // DebugLN('Folder = ' + Folder);
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := rsDlgDel;

    CT := nil;

    WCXModule := Self;  // set WCXModule variable to current module
    SetChangeVolProc(0, ChangeVolProc);
    SetProcessDataProc(0, ProcessDataProc);

    CopySelectedWithSubFolders(flNameList);
   
    DeleteFiles(PChar(FArchiveName), PChar(GetFileList(flNameList)));
    
    FFileOpDlg.Close;
    FFileOpDlg.Free;
    FFileOpDlg := nil;
  except
    FFileOpDlg := nil;
    Result := False;
  end;
end;

function TWCXModule.VFSList(const sDir: String; var fl: TFileList): Boolean;
var
  fr : PFileRecItem;
  I, Count : Integer;
  CurrFileName : String;  // Current file name
begin
  fl.Clear;
  AddUpLevel(LowDirLevel(sDir), fl);
  
  DebugLN('LowDirLevel(sDir) = ' + LowDirLevel(sDir));
  
  Count := FArcFileList.Count - 1;
  for I := 0 to  Count do
   begin
     CurrFileName := PathDelim + PHeaderData(FArcFileList.Items[I])^.FileName;
     
     DebugLN(CurrFileName);
     
     if not IncludeFileInList(sDir, CurrFileName) then
       Continue;

     //DebugLN('In folder = ' + CurrFileName);

     New(fr);
     with fr^, PHeaderData(FArcFileList.Items[I])^  do
         begin
            sName := CurrFileName;
            iMode := FileAttr;
            sModeStr := AttrToStr(iMode);
            bLinkIsDir := False;
            bSelected := False;
            if FPS_ISDIR(iMode) then
              sExt:=''
            else
              sExt:=ExtractFileExt(CurrFileName);
            sNameNoExt:=Copy(CurrFileName,1,length(CurrFileName)-length(sExt));
            sPath := sDir;
            fTimeI := FileDateToDateTime(FileTime);
            sTime := DateToStr(fTimeI);
            iSize := UnpSize;
         end; //with
     fl.AddItem(fr);
   end;
end;

function TWCXModule.VFSMisc: Cardinal;
begin
  if Assigned(GetPackerCaps) then
    Result := GetPackerCaps
  else
    Result := 0;
end;

{ TWCXCopyThread }

procedure TWCXCopyThread.Execute;

begin
// main archive thread code started here
  try
    with WCXModule do
      begin
      case Operation of
        OP_EXTRACT:
          begin
            WCXCopyOut;
          end;
        OP_PACK:
          begin
            WCXCopyIn;
          end;
      end; //case
        Synchronize(FFileOpDlg.Close);
        Synchronize(FFileOpDlg.Free);
        FFileOpDlg := nil;
      end; //with
  except
    DebugLN('Error in "WCXCopyThread.Execute"');
  end;
end;

function IsBlocked : Boolean;
begin
  Result := Assigned(WCXModule);
  if Result then
    with WCXModule do
      begin
        Result := Assigned(FFileOpDlg);
        if Result then
          if Assigned(CT) then
            CT.Synchronize(FFileOpDlg.ShowOnTop)
          else
            FFileOpDlg.ShowOnTop;
      end;  // with
end;

end.
