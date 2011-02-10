{ $threading on}
program doublecmd;
{%File 'doc/changelog.txt'}

{.$APPTYPE GUI}
uses
  {$IFDEF UNIX}
  cthreads,
  //cwstring,
  clocale,
  {$ENDIF}
  Interfaces,
  LCLProc,
  SysUtils,
  Forms,
  {$IFDEF NIGHTLY_BUILD}
  un_lineinfo,
  {$ENDIF}
  uGlobsPaths,
  uGlobs,
  fHackForm,
  fMain,
  fMkDir,
  dmHigh, dmHelpManager, dmCommonData,
  uShowMsg,
  uCryptProc,
  uPixMapManager,
  uKeyboard,
  uUniqueInstance,
  uDCVersion;

{$R *.res}

{$IFDEF HEAPTRC}
var
  LogPath: String;
{$ENDIF}

begin
  DebugLn('Starting Double Commander');

  {$IFDEF NIGHTLY_BUILD}
  InitLineInfo;
  {$ENDIF}

  {$IFDEF HEAPTRC}
  LogPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'logs';
  CreateDir(LogPath);
  SetHeapTraceOutput(LogPath + '/heaptrc-' + FormatDateTime('yyyy-mm-dd hh.mm.ss', Now) + '.log');
  {$ENDIF}

  Application.Title:= 'Double Commander';
  Application.Initialize;
  uDCVersion.InitializeVersionInfo;

  // Use only current directory separator
  AllowDirectorySeparators:= [DirectorySeparator];
  ThousandSeparator:= ' ';

  DebugLn('Double Commander ' + dcVersion);
  DebugLn('Revision: ' + dcRevision);
  DebugLn('Build: ' + dcBuildDate);
  DebugLn('Lazarus: ' + lazVersion + '-' + lazRevision);
  DebugLn('Free Pascal: ' + fpcVersion);
  DebugLn('Platform: ' + TargetCPU + '-' + TargetOS + '-' + TargetWS);
  DebugLn('System: ' + OSVersion);
  if WSVersion <> EmptyStr then
    DebugLn('Widgetset library: ' + WSVersion);
  DebugLn('This program is free software released under terms of GNU GPL 2');
  DebugLn('(C)opyright 2006-2011 Koblov Alexander (Alexx2000@mail.ru)');
  DebugLn('   and contributors (see about dialog)');

  LoadPaths; // must be first
  Application.ShowMainForm:= False;
  Application.CreateForm(TfrmHackForm, frmHackForm);
  if InitGlobs then
    if IsInstanceAllowed then
     begin
       InitPasswordStore;
       LoadPixMapManager;
       Application.CreateForm(TfrmMain, frmMain); // main form
       Application.CreateForm(TdmHighl, dmHighl); // highlighters
       Application.CreateForm(TdmComData, dmComData); // common data
       Application.CreateForm(TdmHelpManager, dmHelpMgr); // help manager
       Application.CreateForm(TfrmMkDir, frmMkDir);  // 21.05.2009 - makedir form

       // Calculate buttons width of message dialogs
       InitDialogButtonWidth;

       // Initializing keyboard module on GTK needs GTKProc.InitKeyboardTables
       // which is called by Application.Initialize. On QT needs the handle
       // of the main form created in Application.CreateForm above.
       uKeyboard.InitializeKeyboard;

       Application.Run;

       uKeyboard.CleanupKeyboard;
     end
    else
     begin
       DebugLn('Another instance of DC is already running. Exiting.');
     end;

  DebugLn('Finished Double Commander');
end.