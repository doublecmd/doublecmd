{ $threading on}
program doublecmd;
{%File 'doc/changelog.txt'}

{.$APPTYPE GUI}
uses
  {$IFDEF MSWINDOWS}
  uElevation,
  {$IFDEF LCLQT5}
  uDarkStyle,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DARWIN}
  uAppleMagnifiedModeFix,
  {$ENDIF}
  {$IF DEFINED(WIN64) AND (FPC_FULLVERSION < 30000)}
  uExceptionHandlerFix,
  {$ENDIF}
  {$IFDEF UNIX}
  cthreads,
  {$IFNDEF HEAPTRC}
  cmem,
  {$ENDIF}
  cwstring,
  clocale,
  uElevation,
  {$IFDEF LINUX}
  uAppImage,
  {$ENDIF}
  {$IFDEF LCLGTK2}
  uOverlayScrollBarFix,
  gtk2,
  Gtk2Int,
  {$ENDIF}
  {$IF DEFINED(LCLQT5) and not DEFINED(DARWIN)}
  uQt5Workaround,
  {$ENDIF}
  {$ENDIF}
  DCConvertEncoding,
  Interfaces,
  {$IFDEF LCLGTK2}
  uGtk2FixCursorPos,
  {$ENDIF}
  {$IFDEF LCLWIN32}
  uDClass,
  uWin32WidgetSetFix,
  {$ENDIF}
  LCLProc,
  Classes,
  SysUtils,
  Forms,
  LCLVersion,
  Math,
  {$IF DEFINED(NIGHTLY_BUILD)}
  un_lineinfo,
  {$ENDIF}
  uGlobsPaths,
  uGlobs,
  fHackForm,
  fMain,
  uAccentsUtils,
  fMkDir,
  dmHigh, dmHelpManager, dmCommonData,
  uShowMsg,
  uCryptProc,
  uPixMapManager,
  uKeyboard,
  uUniqueInstance,
  uDCVersion,
  uCmdLineParams,
  uDebug,
  uOSUtils,
  uspecialdir,
  fstartingsplash,
  ulog,
  uVariableMenuSupport,
  uLng
  {$IFDEF MSWINDOWS}
  , uMyWindows
  {$ENDIF}
  {$IFDEF UNIX}
  , uMyUnix
  {$ENDIF}
  ;

{$IF NOT (DEFINED(DARWIN) AND DEFINED(LCLQT))}
{$R *.res}
{$ENDIF}

{$IF DEFINED(MSWINDOWS)}
{$R doublecmd.manifest.rc}
{$ENDIF}

{$IFDEF HEAPTRC}
var
  LogPath: String;
{$ENDIF}

begin
  DCDebug('Starting Double Commander');

  // Initialize random number generator
  Randomize;

  // Disable invalid floating point operation exception
  SetExceptionMask(GetExceptionMask + [exInvalidOp]);

  {$IF DEFINED(NIGHTLY_BUILD)}
  InitLineInfo;
  AddLineInfoPath(ExtractFileDir(ParamStr(0)));
  {$ENDIF}

  {$IFDEF HEAPTRC}
  LogPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'logs';
  CreateDir(LogPath);
  SetHeapTraceOutput(LogPath + '/heaptrc-' + FormatDateTime('yyyy-mm-dd hh.mm.ss', Now) + '.log');
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  uMyWindows.InitErrorMode;
  uMyWindows.FixCommandLineToUTF8;
  {$ENDIF}

  {$if lcl_fullversion >= 1070000}
  Application.Scaled:= True;
  {$endif}

  // Fix default BidiMode
  // see http://bugs.freepascal.org/view.php?id=22044
  Application.BidiMode:= bdLeftToRight;

  Application.Title:='Double Commander';
  Application.Initialize;
  uDCVersion.InitializeVersionInfo;
  // Initializing keyboard module on GTK needs GTKProc.InitKeyboardTables
  // which is called by Application.Initialize.
  uKeyboard.InitializeKeyboard;

{$IF DEFINED(MSWINDOWS) and DEFINED(LCLQT5)}
  ApplyDarkStyle;
{$ENDIF}

  // Use only current directory separator
  AllowDirectorySeparators:= [DirectorySeparator];
  {$IF lcl_fullversion >= 093100}
  // Disable because we set a few of our own format settings and we don't want
  // them to be changed. There's no way currently to react to Application.IntfSettingsChange.
  // If in future we move to a Unicode RTL this could be removed.
  {$PUSH}{$WARN SYMBOL_PLATFORM OFF}
  Application.UpdateFormatSettings := False;
  {$POP}
  {$ENDIF}
  DefaultFormatSettings.ThousandSeparator:= ' ';
  {$IFDEF UNIX}
  uMyUnix.FixDateTimeSeparators;
  {$ENDIF}
  FixDateNamesToUTF8;

  DCDebug('Double Commander ' + dcVersion);
  DCDebug('Revision: ' + dcRevision);
  DCDebug('Build: ' + dcBuildDate);
  DCDebug('Lazarus: ' + GetLazarusVersion);
  DCDebug('Free Pascal: ' + fpcVersion);
  DCDebug('Platform: ' + TargetCPU + '-' + TargetOS + '-' + TargetWS);
  DCDebug('System: ' + OSVersion);
  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  DCDebug('Desktop Environment: ' + DesktopName[DesktopEnv]);
  {$ENDIF}
  if WSVersion <> EmptyStr then
    DCDebug('Widgetset library: ' + WSVersion);
  DCDebug('This program is free software released under terms of GNU GPL 2');
  DCDebug('(C)opyright 2006-2019 Alexander Koblov (alexx2000@mail.ru)');
  DCDebug('   and contributors (see about dialog)');

  Application.ShowMainForm:= False;
  Application.CreateForm(TfrmHackForm, frmHackForm);

  ProcessCommandLineParams; // before load paths

  if not CommandLineParams.NoSplash then
  begin
    // Let's show the starting slash screen to confirm user application has been started
    Application.CreateForm(TfrmStartingSplash, frmStartingSplash);
    frmStartingSplash.Show;
  end;

  LoadInMemoryOurAccentLookupTableList; // Used for conversion of string to remove accents.
  LoadPaths; // before loading config
  LoadWindowsSpecialDir; // Load the list with special path. *Must* be located AFTER "LoadPaths" and BEFORE "InitGlobs"

  if InitGlobs then
    //-- NOTE: before, only IsInstanceAllowed was called, and all the magic on creation
    //         new instance or sending params to the existing server happened inside 
    //         IsInstanceAllowed() function as a side effect.
    //         Functions with side effects are generally bad, so,
    //         new function was added to explicitly initialize instance.
    InitInstance;
    if IsInstanceAllowed then
    begin
      if (log_start_shutdown in gLogOptions) then logWrite(rsMsgLogProgramStart + ' (' + GetCurrentUserName + '/' + GetComputerNetName + ')');

      InitPasswordStore;
      LoadPixMapManager;
      Application.CreateForm(TfrmMain, frmMain); // main form
      Application.CreateForm(TdmHighl, dmHighl); // highlighters
      Application.CreateForm(TdmComData, dmComData); // common data
      Application.CreateForm(TdmHelpManager, dmHelpMgr); // help manager
      Application.CreateForm(TfrmMkDir, frmMkDir);  // 21.05.2009 - makedir form

      {$IF DEFINED(LCLGTK2) AND (lcl_fullversion >= 093100)}
      // LCLGTK2 uses Application.MainForm as the clipboard widget, however our
      // MainForm is TfrmHackForm and it never gets realized. GTK2 doesn't
      // seem to allow a not realized widget to have clipboard ownership.
      // We switch to frmMain instead which will be realized at some point.
      GTK2WidgetSet.SetClipboardWidget(PGtkWidget(frmMain.Handle));
      {$ENDIF}

      // Hooking on QT needs the handle of the main form which is created
      // in Application.CreateForm above.
      uKeyboard.HookKeyboardLayoutChanged;

      if not CommandLineParams.NoSplash then
      begin
        // We may now remove the starting splash screen, most of the application has been started now
        frmStartingSplash.Close;
        frmStartingSplash.Release;
      end;

      Application.Run;

      if not UniqueInstance.isAnotherDCRunningWhileIamRunning then
        DeleteTempFolderDeletableAtTheEnd;

      FreeMemoryFromOurAccentLookupTableList;

      if (log_start_shutdown in gLogOptions) then logWrite(rsMsgLogProgramShutdown + ' (' + GetCurrentUserName + '/' + GetComputerNetName + ')');
    end
    else
    begin
      DCDebug('Another instance of DC is already running. Exiting.');
    end;

  uKeyboard.CleanupKeyboard;
  DCDebug('Finished Double Commander');
end.
