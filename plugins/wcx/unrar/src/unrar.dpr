library unrar;

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  FPCAdds, SysUtils, DynLibs, UnRARFunc, RarFunc;

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
  GetPackerCaps,
  PackFilesW,
  DeleteFilesW,
  ConfigurePacker,
  GetBackgroundFlags,
  PackSetDefaultParams,
  { Extension API }
  ExtensionInitialize;

{$R *.res}

begin
  ModuleHandle := LoadLibrary(_unrar);
{$IF DEFINED(LINUX)}
  if ModuleHandle = NilHandle then
    ModuleHandle := LoadLibrary(_unrar + '.5');
{$ENDIF}
  if ModuleHandle = NilHandle then
    ModuleHandle := LoadLibrary(GetEnvironmentVariable('COMMANDER_PATH') + PathDelim + _unrar);
  if ModuleHandle <> NilHandle then
  begin
    RAROpenArchiveEx := TRAROpenArchiveEx(GetProcAddress(ModuleHandle, 'RAROpenArchiveEx'));
    RARCloseArchive := TRARCloseArchive(GetProcAddress(ModuleHandle, 'RARCloseArchive'));
    RARReadHeaderEx := TRARReadHeaderEx(GetProcAddress(ModuleHandle, 'RARReadHeaderEx'));
    RARProcessFileW := TRARProcessFileW(GetProcAddress(ModuleHandle, 'RARProcessFileW'));
    RARSetCallback := TRARSetCallback(GetProcAddress(ModuleHandle, 'RARSetCallback'));
    RARSetPassword := TRARSetPassword(GetProcAddress(ModuleHandle, 'RARSetPassword'));
    RARGetDllVersion := TRARGetDllVersion(GetProcAddress(ModuleHandle, 'RARGetDllVersion'));
  end;
end.
