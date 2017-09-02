library SevenZipWcx;

uses
  FPCAdds, SevenZipFunc, SevenZipDlg, WcxPlugin, SevenZipAdv, SevenZipLng,
  SevenZipCodecs;

function OpenArchive(var ArchiveData : tOpenArchiveData) : TArcHandle; stdcall;
begin
  Result:= 0;
  ArchiveData.OpenResult:= E_NOT_SUPPORTED;
end;

function ReadHeader(hArcData : TArcHandle; var HeaderData: THeaderData) : Integer; stdcall;
begin
  Result:= E_NOT_SUPPORTED;
end;

function ProcessFile (hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PAnsiChar) : Integer; stdcall;
begin
  Result:= E_NOT_SUPPORTED;
end;

procedure SetChangeVolProc (hArcData : TArcHandle; pChangeVolProc : PChangeVolProc); stdcall;
begin
end;

procedure SetProcessDataProc (hArcData : TArcHandle; pProcessDataProc : TProcessDataProc); stdcall;
begin
end;

function PackFiles(PackedFile, SubPath, SrcPath, AddList: PAnsiChar; Flags: Integer): Integer; stdcall;
begin
  Result:= E_NOT_SUPPORTED;
end;

function DeleteFiles(PackedFile, DeleteList: PAnsiChar): Integer; stdcall;
begin
  Result:= E_NOT_SUPPORTED;
end;

function GetBackgroundFlags: Integer; stdcall;
begin
  Result:= BACKGROUND_UNPACK or BACKGROUND_PACK;
end;

function GetPackerCaps : Integer; stdcall;
begin
  Result:= PK_CAPS_NEW or PK_CAPS_DELETE  or PK_CAPS_MODIFY
           or PK_CAPS_MULTIPLE or PK_CAPS_OPTIONS or PK_CAPS_ENCRYPT;
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
  PackSetDefaultParams,
  CanYouHandleThisFileW
  ;

{$R *.res}

begin
end.

