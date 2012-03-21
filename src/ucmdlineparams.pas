unit uCmdLineParams;

{$mode objfpc}{$H+}

interface

procedure ProcessCommandLineParams;

implementation

uses
  {$IF DEFINED(NIGHTLY_BUILD)}
  uOSUtils,
  {$ENDIF}
  uDCUtils, uGlobsPaths, FileUtil;

procedure ProcessCommandLineParams;
var
  i: Integer;
  param: UTF8String;
begin
  // Param 0 is executable path.
  for i := 1 to Paramcount do
  begin
    param := ParamStrUTF8(i);
    if StrBegins(param, '--config-dir=') then
      gpCmdLineCfgDir := TrimQuotes(Copy(param, 1 + Length('--config-dir='), MaxInt));

    {$IF DEFINED(NIGHTLY_BUILD)}
    if StrBegins(param, '--no-console') then
      HideConsoleWindow;
    {$ENDIF}
  end;
end;

end.

