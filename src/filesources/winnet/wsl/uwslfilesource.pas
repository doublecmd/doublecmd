unit uWslFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  uFileSource, uFileSourceOperation, uWinNetFileSource;

type

  { TWslFileSource }

  TWslFileSource = class(TWinNetFileSource)
  public
    function GetParentDir(sPath : String): String; override;
    function IsPathAtRoot(Path: String): Boolean; override;
    function GetRootDir(sPath: String): String; override; overload;
    function GetRootDir: String; override; overload;

    class function Available: Boolean;
    class function IsSupportedPath(const Path: String): Boolean; override;
    class function GetMainIcon(out Path: String): Boolean; override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
  end;

implementation

uses
  LazUTF8, DCOSUtils, DCStrUtils, uMyWindows, uWslListOperation;

{ TWslFileSource }

function TWslFileSource.GetParentDir(sPath: String): String;
begin
  Result:= DCStrUtils.GetParentDir(sPath);
end;

function TWslFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Path:= IncludeTrailingBackslash(LowerCase(Path));
  Result:= SameStr(Path, '\\wsl$\') or SameStr(Path, '\\wsl.localhost\');
end;

function TWslFileSource.GetRootDir(sPath: String): String;
begin
  if (Win32BuildNumber >= 22000) then
    Result:= '\\wsl.localhost\'
  else begin
    Result:= '\\wsl$\';
  end;
end;

function TWslFileSource.GetRootDir: String;
begin
  Result:= GetRootDir(EmptyStr);
end;

class function TWslFileSource.Available: Boolean;
begin
  Result:= GetServiceStatus('LxssManager') <> 0;
end;

class function TWslFileSource.IsSupportedPath(const Path: String): Boolean;
var
  APath: String;
begin
  APath:= IncludeTrailingBackslash(LowerCase(Path));
  Result:= StrBegins(APath, '\\wsl$\') or StrBegins(APath, '\\wsl.localhost\');
end;

class function TWslFileSource.GetMainIcon(out Path: String): Boolean;
begin
  if IsWow64 then
    Path:= '%SystemRoot%\Sysnative\wsl.exe'
  else begin
    Path:= '%SystemRoot%\System32\wsl.exe';
  end;
  Result:= True;
end;

function TWslFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:= TWslListOperation.Create(TargetFileSource, TargetPath);
end;

end.
