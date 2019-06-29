program fpmake;

{$coperators on}
{$mode objfpc}{$H+}

uses
  fpmkunit, SysUtils, Classes;

const
  AllMyUnixOSes = AllUnixOSes - [Darwin];

const
  CommonComponents: array[1..9] of String =
  (
    'components\chsdet\chsdet.lpk',
    'components\CmdLine\cmdbox.lpk',
    'components\multithreadprocs\multithreadprocslaz.lpk',
    'components\dcpcrypt\dcpcrypt.lpk',
    'components\doublecmd\doublecmd_common.lpk',
    'components\KASToolBar\kascomp.lpk',
    'components\viewer\viewerpackage.lpk',
    'components\gifanim\pkg_gifanim.lpk',
    'components\synunihighlighter\synuni.lpk'
  );

  CommonPlugins: array[1..8] of String =
  (
    'plugins/wcx/deb/src/deb.lpi',
    'plugins/wcx/rpm/src/rpm.lpi',
    'plugins/wcx/unrar/src/unrar.lpi',
    'plugins/wcx/zip/src/Zip.lpi',
    'plugins/wdx/rpm_wdx/src/rpm_wdx.lpi',
    'plugins/wdx/deb_wdx/src/deb_wdx.lpi',
    'plugins/wdx/audioinfo/src/AudioInfo.lpi',
    'plugins/wfx/ftp/src/ftp.lpi'
  );

  UnixPlugins: array[1..4] of String =
  (
    'plugins/wcx/cpio/src/cpio.lpi',
    'plugins/wfx/samba/src/samba.lpi',
    'plugins/wlx/WlxMplayer/src/wlxMplayer.lpi',
    'plugins/dsx/DSXLocate/src/DSXLocate.lpi'
  );

  WindowsPlugins: array[1..1] of String =
  (
    'plugins/wcx/sevenzip/src/SevenZipWcx.lpi'
  );

  DeleteFiles: array[1..6] of String =
  (
    'doublecmd',
    'doublecmd.exe',
    'doublecmd.dbg',
    'doublecmd.zdli',
    'src\doublecmd.res',
    'tools\extractdwrflnfo'
  );

type

  { TDCDefaults }

  TDCDefaults = class(TFPCDefaults)
  private
    FWS: String;
  public
    procedure CompilerDefaults; override;
    property WS: String read FWS write FWS;
  end;

  { TDCBuildEngine }

  TDCBuildEngine = class(TBuildEngine)

  end;

  { TDCInstaller }

  TDCInstaller = class(TCustomInstaller)
  private
    FLazBuildParams: String;
  private
    procedure CleanDirectory(const Directory: String);
    procedure CleanComponents;
    procedure BuildComponents;
    procedure CleanPlugins;
    procedure BuildPlugins;
    procedure Clean; overload;
    procedure Build;
    function ReadOutputDirectory(const FileName: String): String;
    procedure CleanOutputDirectory(const FileName: String);
  protected
    procedure Clean(AllTargets: boolean); override;
  public
    constructor Create(AOwner : TComponent); override;
  end;

{ TDCDefaults }

procedure TDCDefaults.CompilerDefaults;
var
  AValue: String;
begin
  if Defaults.OS = osNone then
  begin
    AValue:= GetEnvironmentVariable('OS_TARGET');
    if Length(AValue) > 0 then Defaults.OS:= StringToOS(AValue);
  end;
  if Defaults.CPU = cpuNone then
  begin
    AValue:= GetEnvironmentVariable('CPU_TARGET');
    if Length(AValue) > 0 then Defaults.CPU:= StringToCPU(AValue);
  end;
  AValue:= GetEnvironmentVariable('LCL_PLATFORM');
  if Length(AValue) > 0 then (Defaults as TDCDefaults).FWS:= AValue;
  inherited CompilerDefaults;
  Compiler:= 'lazbuild';
end;

procedure TDCInstaller.BuildComponents;
var
  I: Integer;
  Args : String;
begin
  for I:= Low(CommonComponents) to High(CommonComponents) do
  begin
    Args:= SetDirSeparators(CommonComponents[I]) + FLazBuildParams;
    BuildEngine.ExecuteCommand(Defaults.Compiler, Args);
  end;
end;

procedure TDCInstaller.CleanPlugins;
var
  I: Integer;
begin
  for I:= Low(CommonPlugins) to High(CommonPlugins) do
  begin
    CleanOutputDirectory(CommonPlugins[I]);
  end;
end;

function TDCInstaller.ReadOutputDirectory(const FileName: String): String;
var
  I: Integer;
  AFile: TStringList;
begin
  try
    AFile:= TStringList.Create;
    try
      AFile.LoadFromFile(SetDirSeparators(FileName));
      I:= Pos('UnitOutputDirectory Value', AFile.Text);
      if I = 0 then Exit;
      Result:= Copy(AFile.Text, I + 27, MaxInt);
      I:= Pos('"', Result);
      if I = 0 then Exit;
      Result:= ExtractFilePath(FileName) + Copy(Result, 1, I - 1);
      Result:= StringReplace(Result, '$(TargetOS)', OSToString(Defaults.OS), [rfReplaceAll, rfIgnoreCase]);
      Result:= StringReplace(Result, '$(TargetCPU)', CPUToString(Defaults.CPU), [rfReplaceAll, rfIgnoreCase]);
      Result:= SetDirSeparators(Result);
    finally
      AFile.Free;
    end;
  except
    Result:= EmptyStr;
  end;
end;

procedure TDCInstaller.CleanOutputDirectory(const FileName: String);
begin
  CleanDirectory(ReadOutputDirectory(FileName));
end;

procedure TDCInstaller.Clean(AllTargets: boolean);
begin
  // Clean components
  CleanComponents;
  // Clean plugins
  CleanPlugins;
  // Clean Double Commander
  Clean;
end;

constructor TDCInstaller.Create(AOwner: TComponent);
begin
  Defaults:= TDCDefaults.Create;
  Defaults.IgnoreInvalidOptions:= True;
  inherited Create(AOwner);
end;

procedure TDCInstaller.BuildPlugins;
var
  I: Integer;
begin
  for I:= Low(CommonPlugins) to High(CommonPlugins) do
    BuildEngine.ExecuteCommand(Defaults.Compiler, SetDirSeparators(CommonPlugins[I]) + FLazBuildParams);

  if Defaults.OS in AllMyUnixOSes then
  begin
    for I:= Low(UnixPlugins) to High(UnixPlugins) do
      BuildEngine.ExecuteCommand(Defaults.Compiler, SetDirSeparators(UnixPlugins[I]) + FLazBuildParams);
  end;

  if Defaults.OS in AllWindowsOSes then
  begin
    for I:= Low(WindowsPlugins) to High(WindowsPlugins) do
      BuildEngine.ExecuteCommand(Defaults.Compiler, SetDirSeparators(WindowsPlugins[I]) + FLazBuildParams);
  end;
end;

procedure TDCInstaller.Clean;
const
  OutputPath = 'units' + PathDelim;
var
  I: Integer;
  Info : TSearchRec;
begin
  // Clean output directories
  if FindFirst(OutputPath + AllFilesMask, faAnyFile - faHidden, Info) = 0 then
  repeat
    if ((Info.Attr and faDirectory) = faDirectory) and (Info.Name <> '.') and (Info.Name <> '..') then
      CleanDirectory(OutputPath + Info.Name);
  until FindNext(Info) <> 0;
  FindClose(Info);
  TDCBuildEngine(BuildEngine).SysDeleteTree('tools' + PathDelim + 'lib');
  // Clean files
  for I:= Low(DeleteFiles) to High(DeleteFiles) do
    TDCBuildEngine(BuildEngine).SysDeleteFile(SetDirSeparators(DeleteFiles[I]));
  // Clean debug directory
  if Defaults.OS = Darwin then
  begin
    TDCBuildEngine(BuildEngine).SysDeleteTree('doublecmd.dSYM');
  end;
  // Clean fpmake output files
  TDCBuildEngine(BuildEngine).SysDeleteFile('fpmake.o');
end;

procedure TDCInstaller.Build;
begin
  // Build components
  BuildComponents;
  // Build plugins
  BuildPlugins;
  // Set default build mode
  if Pos('--bm=', FLazBuildParams) = 0 then
    FLazBuildParams+= ' --bm=beta';
  // Build Double Commander
  BuildEngine.ExecuteCommand(Defaults.Compiler, SetDirSeparators('src/doublecmd.lpi') + FLazBuildParams);
  if Pos('--bm=beta', FLazBuildParams) > 0 then
  begin
    // Build Dwarf LineInfo Extractor
    BuildEngine.ExecuteCommand(Defaults.Compiler, SetDirSeparators('tools/extractdwrflnfo.lpi'));
    // Extract debug line info
    if Defaults.OS = Darwin then
    begin
      BuildEngine.CmdRenameFile('doublecmd.dSYM/Contents/Resources/DWARF/doublecmd', 'doublecmd.dbg');
    end;
    BuildEngine.ExecuteCommand(SetDirSeparators('tools/extractdwrflnfo'), 'doublecmd.dbg');
  end;
end;

procedure TDCInstaller.CleanDirectory(const Directory: String);
var
  List: TStrings;
begin
  List:= TStringList.Create;
  try
    SearchFiles(IncludeTrailingPathDelimiter(Directory) + AllFilesMask, EmptyStr, False, List);
    BuildEngine.CmdDeleteFiles(List);
  finally
    List.Free;
  end;
end;

procedure TDCInstaller.CleanComponents;
var
  I: Integer;
begin
  for I:= Low(CommonComponents) to High(CommonComponents) do
  begin
    CleanOutputDirectory(CommonComponents[I]);
  end;
end;

var
  I: Integer;
  OptArg: String;
  BuildTarget: String;
begin
  AddCustomFpmakeCommandlineOption('bm', 'Override the project build mode.');
  AddCustomFpmakeCommandlineOption('ws', 'Override the project widgetset, e.g. gtk2 qt qt5 win32 cocoa.');

  with Installer(TDCInstaller) as TDCInstaller do
  begin
    CreateBuildEngine;

    BuildTarget:= ParamStr(1);
    FLazBuildParams:= ' --os=' + OSToString(Defaults.OS);
    FLazBuildParams+= ' --cpu=' + CPUToString(Defaults.CPU);

    if BuildTarget = 'clean' then
    begin
      Clean(True);
      Exit;
    end;

    for I:= 0 to ParamCount do WriteLn(ParamStr(I));

    OptArg:= GetCustomFpmakeCommandlineOptionValue('bm');
    if Length(OptArg) > 0 then begin
      FLazBuildParams+= ' --bm=' + OptArg;
    end;

    OptArg:= GetCustomFpmakeCommandlineOptionValue('ws');
    if Length(OptArg) > 0 then begin
      FLazBuildParams+= ' --ws=' + OptArg;
    end;

    if (Defaults.HaveOptions) then begin
      FLazBuildParams+= ' ' + TDCDefaults(Defaults).CmdLineOptions;
    end;

    if BuildEngine.Verbose then begin
      FLazBuildParams+= ' --verbose';
    end;

    if (Pos('--ws', FLazBuildParams) = 0) and (Length((Defaults as TDCDefaults).WS) > 0) then
      FLazBuildParams+= ' --ws=' + (Defaults as TDCDefaults).WS;

    WriteLn(Defaults.Compiler + ' ' + FLazBuildParams);

    if BuildTarget = 'components' then
      BuildComponents
    else if BuildTarget = 'plugins' then
      BuildPlugins
    else
      Build;

  end;
end.
