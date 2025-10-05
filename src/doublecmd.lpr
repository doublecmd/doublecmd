program doublecmd;

{$IF DEFINED(LCLGTK3)}
{$FATAL LCLGTK3 is not production ready}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}
  uElevation,
  {$IFDEF LCLQT5}
  uDarkStyle,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF UNIX}
  {$IFNDEF HEAPTRC}
  cmem,
  {$ENDIF}
  cthreads,
  {$IFDEF DARWIN}
  iosxwstr,
  iosxlocale,
  {$ELSE}
  cwstring,
  clocale,
  {$ENDIF}
  uElevation,
  {$IFDEF LINUX}
  uAppImage,
  {$ENDIF}
  {$IFDEF LCLGTK2}
  uOverlayScrollBarFix,
  gtk2,
  Gtk2Int,
  {$ENDIF}
  {$IF DEFINED(LCLQT5) or DEFINED(LCLQT6)}
  uQtWorkaround,
  {$ENDIF}
  {$ENDIF}
  uSystem,
  Interfaces,
  uMoveConfig,
  uEarlyConfig,
  DCConvertEncoding,
  {$IF DEFINED(MSWINDOWS)}
  uLibraryPath,
  {$ENDIF}
  {$IF DEFINED(LCLWIN32) and DEFINED(DARKWIN)}
  uWin32WidgetSetDark,
  {$ENDIF}
  {$IFDEF LCLGTK2}
  uGtk2FixCursorPos,
  {$ENDIF}
  {$IFDEF darwin}
  uAppleMagnifiedModeFix,
  uMyDarwin,
  uiCloudDriverConfig,
  {$ENDIF}
  {$IFDEF LCLWIN32}
  uDClass,
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
  {$IFDEF LclCocoa}
{$if NOT defined(DisableCocoaModernForm)}
  ,uCocoaModernFormConfig
{$endif}
  ,CocoaConfig
  {$ENDIF}
  ;

{$R *.res}

{$IF DEFINED(MSWINDOWS)}
{$SETPEOPTFLAGS $140}
{$R doublecmd.manifest.rc}
{$ENDIF}

{$IFDEF HEAPTRC}
var
  LogPath: String;
{$ENDIF}

begin
  // Initialize again
  uSystem.Initialize;

  DCDebug('Starting Double Commander');

  // Initialize random number generator
  Randomize;

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
  uMyWindows.FixCommandLineToUTF8;
  {$ENDIF}

  Application.Scaled:= True;

  // Fix default BidiMode
  // see http://bugs.freepascal.org/view.php?id=22044
  Application.BidiMode:= bdLeftToRight;

  Application.Title:='Double Commander';
  Application.Initialize;

{$IF DEFINED(DARWIN)}
  GetMacFormatSettings(DefaultFormatSettings);
  CocoaConfigGlobal.useIcon:= True;
  Application.Icon:= nil;
{$ENDIF}

  uDCVersion.InitializeVersionInfo;
  // Initializing keyboard module on GTK needs GTKProc.InitKeyboardTables
  // which is called by Application.Initialize.
  uKeyboard.InitializeKeyboard;

{$IF DEFINED(MSWINDOWS) and (DEFINED(LCLQT5) or DEFINED(DARKWIN))}
  ApplyDarkStyle;
{$ENDIF}

{$IF DEFINED(darwin)}
  FixMacFormatSettings;
  setMacOSAppearance( gAppMode );
{$ENDIF}

  // Use only current directory separator
  AllowDirectorySeparators:= [DirectorySeparator];
  // Disable because we set a few of our own format settings and we don't want
  // them to be changed. There's no way currently to react to Application.IntfSettingsChange.
  // If in future we move to a Unicode RTL this could be removed.
  {$PUSH}{$WARN SYMBOL_PLATFORM OFF}
  Application.UpdateFormatSettings := False;
  {$POP}
  if Ord(DefaultFormatSettings.ThousandSeparator) > $7F then
  begin
    DefaultFormatSettings.ThousandSeparator:= ' ';
  end;
  {$IFDEF UNIX}
  uMyUnix.FixDateTimeSeparators;
  {$ENDIF}
  FixDateNamesToUTF8;

  DCDebug(GetVersionInformation);
  DCDebug('This program is free software released under terms of GNU GPL 2');
  DCDebug(Copyright + LineEnding + '   and contributors (see about dialog)');

  Application.ShowMainForm:= False;
  Application.CreateForm(TfrmHackForm, frmHackForm);

  ProcessCommandLineParams; // before load paths

  if (gSplashForm) and (not CommandLineParams.NoSplash) then
  begin
    // Let's show the starting slash screen to confirm user application has been started
    Application.CreateForm(TfrmStartingSplash, frmStartingSplash);
  end;

  LoadInMemoryOurAccentLookupTableList; // Used for conversion of string to remove accents.
  LoadPaths; // before loading config
  LoadWindowsSpecialDir; // Load the list with special path. *Must* be located AFTER "LoadPaths" and BEFORE "InitGlobs"

  if InitGlobs then
  begin
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
{$IF DEFINED(DARWIN)}
      initCocoaModernFormConfig;
      iCloudDriverConfigUtil.load;
{$ENDIF}
      Application.CreateForm(TfrmMain, frmMain); // main form
      Application.CreateForm(TdmComData, dmComData); // common data
      Application.CreateForm(TdmHelpManager, dmHelpMgr); // help manager

      {$IF DEFINED(LCLGTK2)}
      // LCLGTK2 uses Application.MainForm as the clipboard widget, however our
      // MainForm is TfrmHackForm and it never gets realized. GTK2 doesn't
      // seem to allow a not realized widget to have clipboard ownership.
      // We switch to frmMain instead which will be realized at some point.
      GTK2WidgetSet.SetClipboardWidget(PGtkWidget(frmMain.Handle));
      {$ENDIF}

      // Hooking on QT needs the handle of the main form which is created
      // in Application.CreateForm above.
      uKeyboard.HookKeyboardLayoutChanged;

      frmMain.ShowOnTop;
      Application.ProcessMessages;
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
  end;
  uKeyboard.CleanupKeyboard;
  DCDebug('Finished Double Commander');
end.
