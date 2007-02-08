{
 Double commander - Archive File support
 - class for manage WCX plugins

 (C) Alexander Koblov 2006, Alexx2000@mail.ru
 released under GNU GPL2

 constributors:

}

unit uWCXmodule;

interface
uses
  uWCXprototypes, uWCXhead, uFileList, uTypes, dynlibs, Classes;

{$H+}
Type

  TWCXItem=packed record
    HeaderData : THeaderData;
    FileRecItem : TFileRecItem;
  end;
  PWCXItem = ^TWCXItem;

  TWCXModule= Class
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
    destructor Destroy; override;
    function LoadModule(const sName:String):Boolean; {Load WCX plugin}
    procedure UnloadModule;                          {UnLoad WCX plugin}
    function WCXCaps(const sExt:String):Integer; {Get WCX plugin capabilities}
    function WCXOpen(const sName:String):TList; {Return the filelist of archive}
    function WCXCopyOut(const sExtractList : TFileList; sDstPath:String):Integer; {Extract files from archive}
    function WCXCopyIn(const sSrcName, sDstName:String):Integer;  {Pack files in archive}
    function WCXDelete(PackedFile: pchar;  DeleteList: pchar) : Integer;               {Delete files from archive}
  end;

implementation
uses SysUtils, uFileOp, uOSUtils;

destructor TWCXModule.Destroy;
begin
  UnloadModule;
end;

function TWCXModule.LoadModule(const sName:String):Boolean;
begin
  FModuleHandle := LoadLibrary(sName);
  Result := (FModuleHandle <> 0);
  if  FModuleHandle = 0 then exit;
  //WriteLN('FModuleHandle =', FModuleHandle);
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

function TWCXModule.WCXCaps(const sExt:String):Integer;
begin
  Result:= E_NOT_SUPPORTED;//FWCXCaps(FModuleGlobs, PChar(sExt));
end;

function TWCXModule.WCXOpen(const sName:String):TList;
var
ArcHandle : THandle;
ArcFile : tOpenArchiveData;
ArcHeader : THeaderData;
WCXItem : PWCXItem;
begin
FArchiveName := sName;
   WriteLN(sName);
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
  Result := TList.Create;
  //Result.Add(nil);
  while (ReadHeader(ArcHandle, ArcHeader) = 0) do
   begin
     New(WCXItem);
     with WCXItem^ do
         begin
            HeaderData := ArcHeader;
            FileRecItem.sName := DirectorySeparator + ArcHeader.FileName;
            FileRecItem.iMode := ArcHeader.FileAttr;
            FileRecItem.sModeStr := AttrToStr(FileRecItem.iMode);
            if FPS_ISDIR(FileRecItem.iMode) then
              FileRecItem.sExt:=''
            else
              FileRecItem.sExt:=ExtractFileExt(ArcHeader.FileName);
            FileRecItem.sNameNoExt:=Copy(ArcHeader.FileName,1,length(ArcHeader.FileName)-length(FileRecItem.sExt));
            FileRecItem.sPath := DirectorySeparator;
            FileRecItem.fTimeI := FileDateToDateTime(ArcHeader.FileTime);
            FileRecItem.sTime := DateToStr(FileRecItem.fTimeI);
            FileRecItem.iSize := ArcHeader.UnpSize;
            // Alexx2000 доделать позже
         end; //with
     Result.Add(WCXItem);
     FillChar(ArcHeader, SizeOf(ArcHeader), #0);
     // get next file
     ProcessFile(ArcHandle, PK_SKIP, nil, nil);
     WriteLN(PWCXItem(Result.Items[0])^.FileRecItem.sName);
end;
end;

{function TWCXModule.WCXClose:Integer;
begin
  Result:= CloseArchive (FArcHandle);
end; }

function TWCXModule.WCXCopyOut(const sExtractList : TFileList; sDstPath:String):Integer;
var
ArcHandle : THandle;
ArcFile : tOpenArchiveData;
ArcHeader : THeaderData;
Extract : Boolean;
Count, I : Integer;
begin
  Count := sExtractList.Count;
  FillChar(ArcFile, SizeOf(ArcFile), #0);
  ArcFile.ArcName := PChar(FArchiveName);
  ArcFile.OpenMode := PK_OM_EXTRACT;
  ArcHandle := OpenArchive(ArcFile);

  if ArcHandle = 0 then
   begin
    Result := E_EOPEN;
    Exit;
   end;

  FillChar(ArcHeader, SizeOf(ArcHeader), #0);
  while (ReadHeader(ArcHandle, ArcHeader) = 0) do
   begin

        if  sExtractList.CheckFileName(FArchiveName + DirectorySeparator + ArcHeader.FileName) >= 0 then
            ProcessFile(ArcHandle, PK_EXTRACT, nil, PChar(sDstPath + ExtractFileName(ArcHeader.FileName)))
        else
            ProcessFile(ArcHandle, PK_SKIP, nil, nil);
     FillChar(ArcHeader, SizeOf(ArcHeader), #0);
   end;
   CloseArchive(ArcHandle);
   Result := 0;
end;

function TWCXModule.WCXCopyIn(const sSrcName, sDstName:String):Integer;
begin
  Result:= E_NOT_SUPPORTED;
end;

function TWCXModule.WCXDelete(PackedFile: pchar;  DeleteList: pchar) : Integer;
begin
  if Assigned(DeleteFiles) then
    Result := E_NOT_SUPPORTED
  else
    Result:=DeleteFiles(PackedFile, DeleteList);
end;

end.
