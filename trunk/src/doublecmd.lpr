{ $threading on}
program doublecmd;
// uGlobs must be first in uses, uLng must be before any form;
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
  LResources,
  SysUtils,
  Forms,
  {$IFDEF NIGHTLY_BUILD}
  un_lineinfo,
  {$ENDIF}
  uGlobsPaths,
  uGlobs,
  fHackForm,
  fMain,
  fAbout,
  fMkDir,
  dmHigh, dmHelpManager, dmCommonData,
  uCryptProc,
  uPixMapManager,
  uKeyboard,
  uUniqueInstance,
  uDCVersion;

{$IFDEF WINDOWS}{$R doublecmd.rc}{$ENDIF}

begin
  {$I doublecmd.lrs}

  DebugLn('Starting Double Commander');

  {$IFDEF NIGHTLY_BUILD}
  InitLineInfo;
  {$ENDIF}

  Application.Title:= 'Double Commander';
  Application.Initialize;
  uDCVersion.InitializeVersionInfo;

  {$IFDEF UNIX}
  // File name under Unix can contain '\' symbol, so we can not use it as directory separator
  AllowDirectorySeparators:= [DirectorySeparator];
  {$ENDIF}
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
  DebugLn('(C)opyright 2006-2009 Koblov Alexander (Alexx2000@mail.ru)');
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