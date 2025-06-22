library Sample;

{$mode objfpc}{$H+}
{$include calling.inc}

uses
  Classes,
  Extension,
  SysUtils,
  WfxPlugin;

{$R *.res}

const
  FILE_NAME_1 = 'file1.wav';
  FILE_NAME_2 = 'file2.log';

var
  gPluginNr: integer;
  gProgressProc: tProgressProc;
  gLogProc: tLogProc;
  gRequestProc: tRequestProc;
  gStartupInfo: TExtensionStartupInfo;

  TestIcon: TMemoryStream;

function FsInit(PluginNr:integer;pProgressProc:tProgressProc;pLogProc:tLogProc;
                pRequestProc:tRequestProc):integer; dcpcall;
begin
  gPluginNr:= PluginNr;
  gProgressProc:= pProgressProc;
  gLogProc:= pLogProc;
  gRequestProc:= pRequestProc;
  Result:= 0;
end;

function FsFindFirst(path :pchar;var FindData:tWIN32FINDDATA):thandle; dcpcall;
var
  Handle: PInteger absolute Result;
begin
  FindData.dwFileAttributes:= 0;
  StrPCopy(FindData.cFileName, FILE_NAME_1);
  New(Handle);
  Handle^:= 1;
end;

function FsFindNext(Hdl:thandle;var FindData:tWIN32FINDDATA): BOOL; dcpcall;
var
  Handle: PInteger absolute Hdl;
begin
  Result:= (Handle^ < 2);
  if Result then
  begin
    StrPCopy(FindData.cFileName, FILE_NAME_2);
  end;
  Inc(Handle^);
end;

function FsFindClose(Hdl:thandle):integer; dcpcall;
var
  Handle: PInteger absolute Hdl;
begin
  Dispose(Handle);
  Result:= 0;
end;

function FsRenMovFile(OldName,NewName:pchar;Move,OverWrite:bool;
  RemoteInfo:pRemoteInfo):integer; dcpcall;
begin
  gRequestProc(gPluginNr, RT_MsgOK, OldName, NewName, nil, 0);
  Result:= FS_FILE_OK;
end;

function FsExecuteFile(MainWin:thandle;RemoteName,Verb:pchar):integer; dcpcall;
begin
  gRequestProc(gPluginNr, RT_MsgOK, RemoteName, Verb, nil, 0);
  Result:= FS_EXEC_OK;
end;

function FsExtractCustomIcon(RemoteName:pansichar;ExtractFlags:integer; TheIcon: PWfxIcon):integer; dcpcall;
begin
  if RemoteName = PathDelim + FILE_NAME_1 then
  begin
    Result:= FS_ICON_EXTRACTED;
    TheIcon^.Format:= FS_ICON_FORMAT_FILE;
    StrPLCopy(RemoteName, gStartupInfo.PluginDir + 'file1.png', MAX_PATH);
  end
  else if RemoteName = PathDelim + FILE_NAME_2 then
  begin
    Result:= FS_ICON_EXTRACTED;
    TheIcon^.Format:= FS_ICON_FORMAT_BINARY;
    TheIcon^.Data:= TestIcon.Memory;
    TheIcon^.Size:= TestIcon.Size;
    // Copy to RemoteName a unique name, so icon will be cached by DC
    StrPLCopy(RemoteName, '{2B5653E2-818D-4B35-8EDD-6CDE45C86B49}', MAX_PATH);
  end
  else begin
    Result:= FS_ICON_USEDEFAULT;
  end;
end;

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;
begin
  gStartupInfo:= StartupInfo^;
  TestIcon:= TMemoryStream.Create;
  TestIcon.LoadFromFile(gStartupInfo.PluginDir + 'file2.png');
end;

procedure ExtensionFinalize(Reserved: Pointer); dcpcall;
begin
  TestIcon.Free;
end;

exports
      // mandatory
      FsInit,
      FsFindFirst,
      FsFindNext,
      FsFindClose,
      // optional
      FsRenMovFile,
      FsExecuteFile,
      FsExtractCustomIcon,
      // extension
      ExtensionInitialize,
      ExtensionFinalize;

end.
