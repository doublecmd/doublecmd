{ $threading on}
program doublecmd;
// uGlobs must be first in uses, uLng must be before any form;
{%File 'doc/changelog.txt'}

{.$APPTYPE GUI}
uses
  {$IFDEF UNIX}
  cthreads,
  //cwstring,
  {$ENDIF}
  Interfaces,
  LCLProc,
  uGlobsPaths,
  uGlobs,
  uLng,
  SysUtils,
  Forms,
  fMain,
  fAbout,
  uFileList,
  uFilePanel,
  uFileOp,
  uTypes,
  framePanel,
  uFileOpThread,
  uFileProcs,
  fFileOpDlg,
  uCopyThread,
  uDeleteThread,
  fMkDir,
  uCompareFiles,
  uHighlighterProcs,
  fEditor,
  uMoveThread,
  fMsg,
  uSpaceThread,
  fHotDir,
  fHardLink,
  fFindView,
  uPathHistory,
  uExts,
  uLog,
  uShowForm,
  fEditSearch,
  uColorExt,
  fEditorConf,
  {$IFNDEF WIN32}
  uFindMmap,
  fFileProperties,
  uUsersGroups,
  {$ENDIF}
  fLinker,
  fCompareFiles,
  dmHigh,
  uPixMapManager, uVFS,
  KASComp, fbtnchangedlg, fconfigtoolbar, uWCXprototypes, uDCUtils,
  fLngForm, dmDialogs, fViewer, fOptions, fCopyDlg, fMoveDlg, fFindDlg,
  fSymLink, fMultiRename, fSplitter, fPackDlg, fExtractDlg;
  
{$IFDEF WIN32}
{$R XP.res}
{$ENDIF}

const
  buildDate = {$I %DATE%};

begin
  // AssignFile(output, 'c:\doublecmd.log');
  // Rewrite(output);

  Application.Title:='Double Commander';
  //  try
  Application.Initialize;
  ThousandSeparator:=' ';
  DebugLn('Double commander 0.2 alpha - Free Pascal');
  DebugLn('Build: ' + buildDate);
  DebugLn('This program is free software released under terms of GNU GPL 2');
  DebugLn('(C)opyright 2006-7 Koblov Alexander (Alexx2000@mail.ru)');
  DebugLn('  and contributors (see about dialog)');

  fAbout.buildDate := buildDate;

  LoadPaths;
  if LoadGlobs then
     begin
       LoadPixMapManager;
       Application.CreateForm(TfrmMain, frmMain); // main form
       Application.CreateForm(TdmHighl, dmHighl); // highlighters
       Application.CreateForm(TdmDlg, dmDlg); // dialogs
       Application.Run;
     end;
{  except
  on E:Exception do
    Writeln('Critical unhandled exception:', E.Message);
end}

end.
