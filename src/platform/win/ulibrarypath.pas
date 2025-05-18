unit uLibraryPath;

{$mode delphi}

interface

implementation

uses
  Windows, SysUtils;

procedure Initialize;
var
  Handle: TLibHandle;
  FileName: UnicodeString;
  AddDllDirectory: function(NewDirectory: PCWSTR): Pointer; stdcall;
begin
  if (Win32MajorVersion > 5) then
  begin
    Handle:= GetModuleHandleW(Kernel32);
    @AddDllDirectory:= GetProcAddress(Handle, 'AddDllDirectory');
    if Assigned(AddDllDirectory) then
    begin
      SetLength(FileName, MaxSmallint + 1);
      SetLength(FileName, GetModuleFileNameW(0, PWideChar(FileName), MaxSmallint));
      AddDllDirectory(PWideChar(ExtractFilePath(FileName) + 'plugins\dll'));
    end;
  end;
end;

initialization
  Initialize;

end.

