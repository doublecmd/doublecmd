library SevenZipWcx;

uses
  CMem,
{$IFDEF UNIX}
  CThreads,
{$ENDIF}
  FPCAdds, SevenZipFunc, WcxPlugin, SevenZipAdv, SevenZipLng,
  SevenZipCodecs, SevenZipDlg;

function OpenArchive(var ArchiveData : tOpenArchiveData) : TArcHandle; winapi;
begin
  Result:= 0;
  ArchiveData.OpenResult:= E_NOT_SUPPORTED;
end;

function ReadHeader(hArcData : TArcHandle; var HeaderData: THeaderData) : Integer; winapi;
begin
  Result:= E_NOT_SUPPORTED;
end;

function ProcessFile (hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PAnsiChar) : Integer; winapi;
begin
  Result:= E_NOT_SUPPORTED;
end;

procedure SetChangeVolProc (hArcData : TArcHandle; pChangeVolProc : PChangeVolProc); winapi;
begin
end;

procedure SetProcessDataProc (hArcData : TArcHandle; pProcessDataProc : TProcessDataProc); winapi;
begin
end;

function PackFiles(PackedFile, SubPath, SrcPath, AddList: PAnsiChar; Flags: Integer): Integer; winapi;
begin
  Result:= E_NOT_SUPPORTED;
end;

function DeleteFiles(PackedFile, DeleteList: PAnsiChar): Integer; winapi;
begin
  Result:= E_NOT_SUPPORTED;
end;

function GetBackgroundFlags: Integer; winapi;
begin
  Result:= BACKGROUND_UNPACK or BACKGROUND_PACK;
end;

function GetPackerCaps : Integer; winapi;
begin
  Result:= PK_CAPS_NEW or PK_CAPS_DELETE or PK_CAPS_MODIFY or PK_CAPS_MULTIPLE or
           PK_CAPS_OPTIONS or PK_CAPS_BY_CONTENT or PK_CAPS_ENCRYPT;
end;

exports
  { Mandatory }
  OpenArchive,
  OpenArchiveW,
  ReadHeader,
  ReadHeaderExW,
  ProcessFile,
  ProcessFileW,
  CloseArchive,
  SetChangeVolProc,
  SetChangeVolProcW,
  SetProcessDataProc,
  SetProcessDataProcW,
  { Optional }
  PackFiles,
  PackFilesW,
  DeleteFiles,
  DeleteFilesW,
  GetPackerCaps,
  ConfigurePacker,
  GetBackgroundFlags,
  CanYouHandleThisFileW,
  { Extension }
  ExtensionInitialize
  ;

{$R *.res}

begin
end.

