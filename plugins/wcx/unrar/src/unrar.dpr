library unrar;

uses
  DynLibs, UnRARFunc;

exports
  OpenArchive,
  OpenArchiveW,
  ReadHeader,
  ReadHeaderEx,
  ReadHeaderExW,
  ProcessFile,
  ProcessFileW,
  CloseArchive,
  SetChangeVolProc,
  SetChangeVolProcW,
  SetProcessDataProc,
  SetProcessDataProcW;

begin
  ModuleHandle := LoadLibrary(_unrar);
  if ModuleHandle > 0 then
    begin
      RAROpenArchive := TRAROpenArchive(GetProcAddress(ModuleHandle, 'RAROpenArchive'));
      RAROpenArchiveEx := TRAROpenArchiveEx(GetProcAddress(ModuleHandle, 'RAROpenArchiveEx'));
      RARCloseArchive := TRARCloseArchive(GetProcAddress(ModuleHandle, 'RARCloseArchive'));
      RARReadHeader := TRARReadHeader(GetProcAddress(ModuleHandle, 'RARReadHeader'));
      RARReadHeaderEx := TRARReadHeaderEx(GetProcAddress(ModuleHandle, 'RARReadHeaderEx'));
      RARProcessFile := TRARProcessFile(GetProcAddress(ModuleHandle, 'RARProcessFile'));
      RARProcessFileW := TRARProcessFileW(GetProcAddress(ModuleHandle, 'RARProcessFileW'));
      RARSetCallback := TRARSetCallback(GetProcAddress(ModuleHandle, 'RARSetCallback'));
      RARSetChangeVolProc := TRARSetChangeVolProc(GetProcAddress(ModuleHandle, 'RARSetChangeVolProc'));
      RARSetProcessDataProc := TRARSetProcessDataProc(GetProcAddress(ModuleHandle, 'RARSetProcessDataProc'));
      RARSetPassword := TRARSetPassword(GetProcAddress(ModuleHandle, 'RARSetPassword'));
      RARGetDllVersion := TRARGetDllVersion(GetProcAddress(ModuleHandle, 'RARGetDllVersion'));
    end;
end.
