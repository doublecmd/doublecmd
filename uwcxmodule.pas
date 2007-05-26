{
   Double commander
   -------------------------------------------------------------------------
   Archive File support - class for manage WCX plugins

   Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)

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
  uWCXprototypes, uWCXhead, uFileList, uTypes, dynlibs, Classes, uVFSModule, uVFSUtil, fFileOpDlg;

{$H+}
const
  OP_EXTRACT = 0;
  OP_PACK = 1;

Type
  TWCXModule = class;
  
  PHeaderData = ^THeaderData;
  
  { Packing/Unpacking thread }
  
   TArcThread = class(TThread)
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
    FFlags : Integer;
    FDstPath,
    fFolder : String;
    FFilesSize: Int64;
    FPercent : Double;
    AT : TArcThread;         // Packing/Unpacking thread
    FFileOpDlg: TfrmFileOp; // progress window
    FEmulate : Boolean;
    function WCXCopyOut : Boolean;{Extract files from archive}
    function WCXCopyIn : Boolean;{Pack files in archive}

    function ProcessDataProc(FileName:pchar;Size:longint):longint;stdcall;
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
    function VFSCaps :Integer;override;

    function VFSConfigure(Parent: THandle):Boolean;override;
    function VFSOpen(const sName:String):Boolean;override;
    function VFSClose:Boolean;override;

    function VFSMkDir(const sDirName:String ):Boolean;override;{Create a directory}
    function VFSRmDir(const sDirName:String):Boolean;override; {Remove a directory}

    function VFSCopyOut(var flSrcList : TFileList; sDstPath:String):Boolean;override;{Extract files from archive}
    function VFSCopyIn(var flSrcList : TFileList; sDstName:String;  Flags : Integer):Boolean;override;{Pack files in archive}
    function VFSRename(const sSrcName, sDstName:String):Boolean;override;{Rename or move file}
    function VFSRun(const sName:String):Boolean;override;
    function VFSDelete(var flNameList:TFileList):Boolean;override;{Delete files from archive}

    function VFSList(const sDir:String; var fl:TFileList ):Boolean;override;{Return the filelist of archive}
  end;

implementation
uses SysUtils, uFileOp, uOSUtils, LCLProc, uFileProcs, uDCUtils;

constructor TWCXModule.Create;
begin
  FFilesSize:= 0;
  FPercent := 0;
  FEmulate := True; // temporally
end;

destructor TWCXModule.Destroy;
begin
  UnloadModule;
end;

function TWCXModule.LoadModule(const sName:String):Boolean;
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

function TWCXModule.VFSInit: Boolean;
begin

end;

procedure TWCXModule.VFSDestroy;
begin

end;

function TWCXModule.VFSCaps : Integer;
begin
  if Assigned(GetPackerCaps) then
    Result := GetPackerCaps
  else
    Result := 0;
end;

function TWCXModule.VFSConfigure(Parent: THandle): Boolean;
begin
  if @ConfigurePacker <> nil then
    ConfigurePacker(Parent, FModuleHandle);
end;


function TWCXModule.VFSOpen(const sName: String): Boolean;
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
      exit;
    end;

  try

  DebugLN('Open Archive');

  (*Open Archive*)
  FillChar(ArcFile, SizeOf(ArcFile), #0);
  ArcFile.ArcName := PChar(sName);
  ArcFile.OpenMode := PK_OM_LIST;
  ArcHandle := OpenArchive(ArcFile);

  if ArcHandle = 0 then
    begin
      //Result := E_EOPEN
      Exit;
    end;

  if not FEmulate then
    SetProcessDataProc(ArcHandle, Pointer(ProcessDataProc));

  DebugLN('Get File List');
  (*Get File List*)
  FillChar(ArcHeader, SizeOf(ArcHeader), #0);
  FArcFileList := TList.Create;

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
     ProcessFile(ArcHandle, PK_SKIP, nil, nil);

    end;
    (* if plugin is not list a list of folders *)
    if not bHasDir then
      begin
        for I := 0 to sDirs.Count - 1 do
          begin
            FillChar(ArcHeader, SizeOf(ArcHeader), #0);
            ArcHeader.FileName := sDirs.Strings[I];
            ArcHeader.FileAttr := faDirectory;
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
end;

function TWCXModule.VFSClose: Boolean;
begin

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
   //FDstPath := sDstPath;


   (* Get current folder in archive *)
   Folder := LowDirLevel(FFileList.GetItem(0)^.sName);

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
    Result := False;
    Exit;
   end;
  if not FEmulate then
    SetProcessDataProc(ArcHandle, Pointer(ProcessDataProc));

  FillChar(ArcHeader, SizeOf(ArcHeader), #0);
  while (ReadHeader(ArcHandle, ArcHeader) = 0) do
   begin


        if  FFileList.CheckFileName(ArcHeader.FileName) >= 0 then
          begin
            DebugLN(FDstPath + ExtractDirLevel(Folder, ArcHeader.FileName));

            if FEmulate then
              begin
                FFileOpDlg.sFileName := FDstPath + ExtractDirLevel(Folder, PathDelim + ArcHeader.FileName);
                FFileOpDlg.iProgress1Pos := 0;
                if AT.Terminated then Exit;
                AT.Synchronize(FFileOpDlg.UpdateDlg);
              end;

            ProcessFile(ArcHandle, PK_EXTRACT, nil, PChar(FDstPath + ExtractDirLevel(Folder, PathDelim + ArcHeader.FileName)));
            //*************
            if FEmulate then
              begin
                if FFilesSize > 0 then
                  FPercent := FPercent + ((ArcHeader.PackSize * 100) / FFilesSize)
                else
                  FPercent := 100;
                  
                DebugLN('Percent = ' + IntToStr(Round(FPercent)));

                FFileOpDlg.iProgress1Pos := 100;
                FFileOpDlg.iProgress2Pos := Round(FPercent);

                AT.Synchronize(FFileOpDlg.UpdateDlg);
              end; // Emulate
            //*************
            end
        else
            ProcessFile(ArcHandle, PK_SKIP, nil, nil);
     FillChar(ArcHeader, SizeOf(ArcHeader), #0);
   end;
   CloseArchive(ArcHandle);
   Result := True;
end;

function TWCXModule.WCXCopyIn : Boolean;
var
  FileList, Folder : PChar;
  I : Integer;
begin
  DebugLN('VFSCopyIn =' + FArchiveName);
  FPercent := 0;
  New(FileList);
  New(Folder);

  (* Add in file list files from subfolders *)
  SelectFilesInSubFoldersInRFS(FFileList, FFilesSize);

  DebugLN('Curr Dir := ' + FFileList.CurrentDirectory);
  Folder := PChar(FFileList.CurrentDirectory);
  
  if not FEmulate then
  begin
  (* Convert TFileList into PChar *)
  FileList := PChar(GetFileList(FFileList));

  PackFiles(PChar(FArchiveName), nil{PChar(sDstName)}, Folder, FileList, FFlags);
  end
  else
    begin
      for I := 0 to FFileList.Count - 1 do
        begin

          FileList := PChar(FFileList.GetItem(I)^.sName + #0#0);

          FFileOpDlg.sFileName := FileList;
          FFileOpDlg.iProgress1Pos := 0;
          if AT.Terminated then Exit;
          AT.Synchronize(FFileOpDlg.UpdateDlg);
          
          PackFiles(PChar(FArchiveName), nil{PChar(sDstName)}, Folder, FileList, FFlags);

          if FFilesSize > 0 then
            FPercent := FPercent + ((FFileList.GetItem(I)^.iSize * 100) / FFilesSize)
          else
            FPercent := 100;
            
          DebugLN('Percent = ' + IntToStr(Round(FPercent)));

          FFileOpDlg.iProgress1Pos := 100;
          FFileOpDlg.iProgress2Pos := Round(FPercent);

          AT.Synchronize(FFileOpDlg.UpdateDlg);
        end;
    end;
end;

function TWCXModule.ProcessDataProc(FileName: pchar; Size: longint): longint;stdcall;
begin
  DebugLN('Working ' + FileName + ' Size = ' + IntToStr(Size));

  Result := 1;
  
  FPercent := FPercent + ((Size * 100) / FFilesSize);
  DebugLN('Percent = ' + IntToStr(Round(FPercent)));

  FFileOpDlg.iProgress1Pos := 100;
  FFileOpDlg.iProgress2Pos := Round(FPercent);

  AT.Synchronize(FFileOpDlg.UpdateDlg);
end;

procedure TWCXModule.CopySelectedWithSubFolders(var flist:TFileList);

  procedure SelectFilesInSubfolders(var fl : TFileList; sDir : String);
  var
    fr : PFileRecItem;
    I, Count : Integer;
    CurrFileName : String;  // Current file name
  begin


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
                 SelectFilesInSubfolders(fl, FileName);
               end
             else
               begin
                 inc(FFilesSize, PackSize);
               end;
          end; //with
       fl.AddItem(fr);
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

    Newfl.AddItem(@fri);
    DebugLN('Curr File = ' + fri.sName);

    if FPS_ISDIR(fri.iMode) then
      SelectFilesInSubfolders(Newfl, fri.sName)
    else
      begin
        inc(FFilesSize);
      end;

  end;
  flist.Free;
  flist := Newfl;
end;

{Extract files from archive in thread}

function TWCXModule.VFSCopyOut(var flSrcList: TFileList; sDstPath: String
  ): Boolean;
begin
  Result := True;
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := 'Extracting...'; //TODO: Localize
  
    FFileList := flSrcList;
    FDstPath := sDstPath;
  
    AT := TArcThread.Create(True);
    AT.FreeOnTerminate := True;
    AT.Operation := OP_EXTRACT;
    AT.WCXModule := Self;
    FFileOpDlg.Thread := TThread(AT);
    AT.Resume;
  except
    Result := False;
  end;
end;

{Pack files in thread}

function TWCXModule.VFSCopyIn(var flSrcList: TFileList; sDstName: String; Flags : Integer
  ): Boolean;
begin
  Result := True;
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := 'Packing...'; //TODO: Localize
    
    FFileList := flSrcList;
    FDstPath := sDstName;
    FFlags := Flags;
  
    AT := TArcThread.Create(True);
    AT.FreeOnTerminate := True;
    AT.Operation := OP_PACK;
    AT.WCXModule := Self;
    FFileOpDlg.Thread := TThread(AT);
    AT.Resume;
  except
    Result := False
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

   CopySelectedWithSubFolders(flNameList);
   
   DeleteFiles(PChar(FArchiveName), PChar(GetFileList(flNameList)));
   
  
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

{ TArcThread }

procedure TArcThread.Execute;

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
        FFileOpDlg.Close;
      end; //with
  except
    DebugLN('Error in "ArcThread.Execute"');
  end;
  end;
end.
