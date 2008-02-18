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
  uFindMmap,
  {$IFDEF UNIX}
  fFileProperties,
  uUsersGroups,
  {$ENDIF}
  fLinker,
  fCompareFiles,
  dmHigh,
  uPixMapManager, uVFS, fFileAssoc,
  KASComp, fconfigtoolbar, uWCXprototypes, uDCUtils, uOSUtils,
  dmDialogs, fViewer, fOptions, fCopyDlg, fMoveDlg, fFindDlg,
  fSymLink, fMultiRename, fSplitter, fPackDlg, fExtractDlg;
  
{$IFDEF MSWINDOWS}
{$R XP.res}
{$ENDIF}

const
  buildDate = {$I %DATE%};

begin
  Application.Title:='Double Commander';
  Application.Initialize;
  ThousandSeparator:=' ';
  DebugLn('Double commander 0.3 alpha - Free Pascal');
  DebugLn('Build: ' + buildDate);
  DebugLn('This program is free software released under terms of GNU GPL 2');
  DebugLn('(C)opyright 2006-2008 Koblov Alexander (Alexx2000@mail.ru)');
  DebugLn('  and contributors (see about dialog)');

  fAbout.buildDate := buildDate;

  LoadPaths; // must be first
  if LoadGlobs then
     begin
       LoadPixMapManager;
       Application.CreateForm(TfrmMain, frmMain); // main form
       Application.CreateForm(TdmHighl, dmHighl); // highlighters
       Application.CreateForm(TdmDlg, dmDlg); // dialogs
       Application.Run;
     end;
end.
