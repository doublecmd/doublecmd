library unrar;

uses
  DynLibs, UnRARFunc;

{$E wcx}

exports
  { Mandatory }
  OpenArchive,
  ReadHeader,
  ProcessFile,
  CloseArchive,
  SetChangeVolProc,
  SetProcessDataProc;

begin
  ModuleHandle := LoadLibrary(_unrar);
  if ModuleHandle > 0 then
    begin
      RAROpenArchive := GetProcAddress(ModuleHandle, 'RAROpenArchive');
      RARReadHeader := GetProcAddress(ModuleHandle, 'RARReadHeader');
      RARProcessFile := GetProcAddress(ModuleHandle, 'RARProcessFile');
      RARCloseArchive := GetProcAddress(ModuleHandle, 'RARCloseArchive');
      RARSetCallback := GetProcAddress(ModuleHandle, 'RARSetCallback');
    end;
end.
