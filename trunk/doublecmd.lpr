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
  fHackForm,
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
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  fLinker,
  fCompareFiles,
  dmHigh,
  uPixMapManager, uVFS, fFileAssoc,
  KASComp, fconfigtoolbar, uWCXprototypes, uDCUtils, uOSUtils,
  dmDialogs, fViewer, fOptions, fCopyDlg, fMoveDlg, fFindDlg,
  fSymLink, fMultiRename, fSplitter, fPackDlg, fExtractDlg, uDescr;
  
{$IFDEF MSWINDOWS}
{$R XP.res}
{$ENDIF}

const
  buildDate = {$I %DATE%};
  Version = '0.3.5 alpha';
begin
  {$IFDEF MSWINDOWS}
  Application.Icon.Handle:= LoadIcon(hInstance, 'DOUBLECMD');
  {$ENDIF}
  Application.Title:='Double Commander';
  Application.Initialize;
  ThousandSeparator:=' ';
  DebugLn(Format('Double commander %s - Free Pascal', [Version]));
  DebugLn('Build: ' + buildDate);
  DebugLn('This program is free software released under terms of GNU GPL 2');
  DebugLn('(C)opyright 2006-2008 Koblov Alexander (Alexx2000@mail.ru)');
  DebugLn('  and contributors (see about dialog)');

  fAbout.buildDate := buildDate;
  fAbout.Version:= Version;

  LoadPaths; // must be first
  if LoadGlobs then
     begin
       LoadPixMapManager;
       Application.CreateForm(TfrmHackForm, frmHackForm);
       Application.CreateForm(TfrmMain, frmMain); // main form
       Application.CreateForm(TdmHighl, dmHighl); // highlighters
       Application.CreateForm(TdmDlg, dmDlg); // dialogs
       Application.Run;
     end;
end.
