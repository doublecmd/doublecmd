library sample;

uses
  SysUtils,
  Classes,
  WcxPlugin;

function  GetPackerCaps : Integer; winapi; export;
begin
  Result:= PK_CAPS_MULTIPLE or PK_CAPS_BY_CONTENT or PK_CAPS_HIDE;
end;

function  OpenArchive(var ArchiveData : TOpenArchiveData) : TArcHandle; winapi; export;
var
  Handle: PBoolean absolute Result;
begin
  New(Handle);
  Handle^:= True;
  ArchiveData.OpenResult:= E_SUCCESS;
end;

function  CloseArchive(hArcData : TArcHandle) : Integer; winapi; export;
var
  Handle: PBoolean absolute hArcData;
begin
  Dispose(Handle);
  Result:= E_SUCCESS;
end;

function  ReadHeader(hArcData : TArcHandle; var HeaderData : THeaderData) : Integer; winapi; export;
var
  Handle: PBoolean absolute hArcData;
begin
  if Handle^ then
  begin
    Handle^:= False;
    Result:= E_SUCCESS;
    HeaderData.FileName:= 'Hello.txt';
  end
  else begin
    Result:= E_NO_FILES;
  end;
end;

function  ProcessFile(hArcData : TArcHandle; Operation : Integer; DestPath : PChar; DestName : PChar) : Integer; winapi; export;
begin
  Result:= E_SUCCESS;
end;

procedure SetProcessDataProc(hArcData : TArcHandle; ProcessDataProc : TProcessDataProc); winapi; export;
begin
end;

procedure SetChangeVolProc(hArcData : TArcHandle; ChangeVolProc : TChangeVolProc); winapi; export;
begin
end;

function  CanYouHandleThisFile(FileName: PAnsiChar): LongBool; winapi; export;
begin
  Result:= False;
end;

exports
  CloseArchive,
  GetPackerCaps,
  OpenArchive,
  ProcessFile,
  ReadHeader,
  SetChangeVolProc,
  SetProcessDataProc,
  CanYouHandleThisFile;

end.

