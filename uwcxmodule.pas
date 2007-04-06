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
  uWCXprototypes, uWCXhead, uFileList, uTypes, dynlibs, Classes, uVFSModule, uVFSUtil;

{$H+}
Type

  TWCXItem=packed record
    HeaderData : THeaderData;
    FileRecItem : TFileRecItem;
  end;
  PWCXItem = ^TWCXItem;

  PHeaderData = ^THeaderData;
  
  { TWCXModule }

  TWCXModule = class (TVFSModule)
  private
    FArcFileList : TList;
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
    function VFSCaps(const sExt:String):Integer;override;

    function VFSGetExts:String;override;
    function VFSOpen(const sName:String):Boolean;override;
    function VFSClose:Boolean;override;

    function VFSMkDir(const sDirName:String ):Boolean;override;{Create a directory}
    function VFSRmDir(const sDirName:String):Boolean;override; {Remove a directory}

    function VFSCopyOut(const flSrcList : TFileList; sDstPath:String):Boolean;override;{Extract files from archive}
    function VFSCopyIn(const flSrcList : TFileList; sDstName:String):Boolean;override;{Pack files in archive}
    function VFSRename(const sSrcName, sDstName:String):Boolean;override;{Rename or move file}
    function VFSRun(const sName:String):Boolean;override;
    function VFSDelete(const flNameList:TFileList):Boolean;override;{Delete files from archive}

    function VFSList(const sDir:String; var fl:TFileList ):Boolean;override;{Return the filelist of archive}
  end;

implementation
uses SysUtils, uFileOp, uOSUtils, LCLProc;

constructor TWCXModule.Create;
begin

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

function TWCXModule.VFSCaps(const sExt: String): Integer;
begin

end;

function TWCXModule.VFSGetExts: String;
begin

end;

function TWCXModule.VFSOpen(const sName: String): Boolean;
var
ArcHandle : THandle;
ArcFile : tOpenArchiveData;
ArcHeader : THeaderData;
HeaderData : PHeaderData;
begin
  FArchiveName := sName;
  DebugLN(sName);

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

  (*Get File List*)
  FillChar(ArcHeader, SizeOf(ArcHeader), #0);
  FArcFileList := TList.Create;

  while (ReadHeader(ArcHandle, ArcHeader) = 0) do
   begin
     New(HeaderData);
     HeaderData^ := ArcHeader;
     FArcFileList.Add(HeaderData);
     FillChar(ArcHeader, SizeOf(ArcHeader), #0);
     // get next file
     ProcessFile(ArcHandle, PK_SKIP, nil, nil);

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

{Extract files from archive}

function TWCXModule.VFSCopyOut(const flSrcList: TFileList; sDstPath: String
  ): Boolean;
var
ArcHandle : THandle;
ArcFile : tOpenArchiveData;
ArcHeader : THeaderData;
Extract : Boolean;
Count, I : Integer;
begin
  Count := flSrcList.Count;
  FillChar(ArcFile, SizeOf(ArcFile), #0);
  ArcFile.ArcName := PChar(FArchiveName);
  ArcFile.OpenMode := PK_OM_EXTRACT;
  ArcHandle := OpenArchive(ArcFile);

  if ArcHandle = 0 then
   begin
    Result := False;
    Exit;
   end;

  FillChar(ArcHeader, SizeOf(ArcHeader), #0);
  while (ReadHeader(ArcHandle, ArcHeader) = 0) do
   begin

        if  flSrcList.CheckFileName(FArchiveName + DirectorySeparator + ArcHeader.FileName) >= 0 then
            ProcessFile(ArcHandle, PK_EXTRACT, nil, PChar(sDstPath + ExtractFileName(ArcHeader.FileName)))
        else
            ProcessFile(ArcHandle, PK_SKIP, nil, nil);
     FillChar(ArcHeader, SizeOf(ArcHeader), #0);
   end;
   CloseArchive(ArcHandle);
   Result := True;
end;

function TWCXModule.VFSCopyIn(const flSrcList: TFileList; sDstName: String
  ): Boolean;
begin

end;

function TWCXModule.VFSRename(const sSrcName, sDstName: String): Boolean;
begin

end;

function TWCXModule.VFSRun(const sName: String): Boolean;
begin

end;

function TWCXModule.VFSDelete(const flNameList: TFileList): Boolean;
begin

end;

function TWCXModule.VFSList(const sDir: String; var fl: TFileList): Boolean;
var
  fr : PFileRecItem;
  I, Count : Integer;
  CurrFileName : String;  // Current file name
begin
  fl.Clear;
  AddUpLevel(LowDirLevel(sDir), fl);
  
  //DebugLN('LowDirLevel(sDir) = ' + LowDirLevel(sDir));
  
  Count := FArcFileList.Count - 1;
  for I := 0 to  Count do
   begin
     CurrFileName := PathDelim + PHeaderData(FArcFileList.Items[I])^.FileName;
     
     //DebugLN(CurrFileName);
     
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


end.
