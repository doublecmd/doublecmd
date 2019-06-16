program fpmake;

{$coperators on}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, getopts, fpmkunit;

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

var
  FLazBuild: String = 'lazbuild';


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
end;

procedure TDCInstaller.BuildComponents;
var
  I: Integer;
  Args : String;
begin
  for I:= Low(CommonComponents) to High(CommonComponents) do
  begin
    Args:= SetDirSeparators(CommonComponents[I]) + FLazBuildParams;
    BuildEngine.ExecuteCommand(FLazBuild, Args);
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
    BuildEngine.ExecuteCommand(FLazBuild, SetDirSeparators(CommonPlugins[I]) + FLazBuildParams);

  if Defaults.OS in AllMyUnixOSes then
  begin
    for I:= Low(UnixPlugins) to High(UnixPlugins) do
      BuildEngine.ExecuteCommand(FLazBuild, SetDirSeparators(UnixPlugins[I]) + FLazBuildParams);
  end;

  if Defaults.OS in AllWindowsOSes then
  begin
    for I:= Low(WindowsPlugins) to High(WindowsPlugins) do
      BuildEngine.ExecuteCommand(FLazBuild, SetDirSeparators(WindowsPlugins[I]) + FLazBuildParams);
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
  TDCBuildEngine(BuildEngine).SysDeleteTree('lib');
  if Defaults.OS in AllUnixOSes then
  begin
    TDCBuildEngine(BuildEngine).SysDeleteFile('fpmake');
  end;
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
  BuildEngine.ExecuteCommand(FLazBuild, SetDirSeparators('src/doublecmd.lpi') + FLazBuildParams);
  if Pos('--bm=beta', FLazBuildParams) > 0 then
  begin
    // Build Dwarf LineInfo Extractor
    BuildEngine.ExecuteCommand(FLazBuild, SetDirSeparators('tools/extractdwrflnfo.lpi'));
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

var
  BuildTarget: String;
  Option: AnsiChar = #0;
  OptionIndex: LongInt = 0;
  Options: array[1..4] of TOption;
begin
  WriteLn('Run!!!');
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

  for I:= 0 to ParamCount do
  WriteLn(ParamStr(I));

  FillChar(Options, SizeOf(Options), 0);
  with Options[1] do
  begin
    Name:= 'bm';
    Has_arg:= 1;
  end;
  with Options[2] do
  begin
    Name:= 'ws';
    Has_arg:= 1;
  end;
  with Options[3] do
  begin
    Name:= 'options';
    Has_arg:= 1;
  end;
  with Options[4] do
  begin
    Name:= 'verbose';
  end;

  repeat
    try
      Option:= GetLongOpts('v', @Options[1], OptionIndex);
    except
      on E: Exception do
      begin
        WriteLn(E.Message);
        Exit;
      end;
    end;
    case Option of
      #0:
        begin
          case OptionIndex of
            1:
              begin
                FLazBuildParams+= ' --bm=' + OptArg;
                WriteLn(FLazBuildParams);
              end;
            2:
              begin
                FLazBuildParams+= ' --ws=' + OptArg;
                WriteLn(FLazBuildParams);
              end;
            3:
              begin
                FLazBuildParams+= ' ' + OptArg;
                WriteLn(FLazBuildParams);
              end;
            4:
              begin
                FLazBuildParams+= ' --verbose';
                WriteLn(FLazBuildParams);
              end;
          end;
        end;
      'v':
        begin
          FLazBuildParams+= ' --verbose';
          WriteLn(FLazBuildParams);
        end;
      '?', ':': WriteLn ('Error with opt : ', OptOpt);
    end; { case }
  until Option = EndOfOptions;

  if (Pos('--ws', FLazBuildParams) = 0) and (Length((Defaults as TDCDefaults).WS) > 0) then
    FLazBuildParams+= ' --ws=' + (Defaults as TDCDefaults).WS;

  if BuildTarget = 'components' then
    BuildComponents
  else if BuildTarget = 'plugins' then
    BuildPlugins
  else
    Build;

  end;
end.
