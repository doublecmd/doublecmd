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
  InterfaceBase,
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
  fLinker,
  fCompareFiles,
  dmHigh, dmHelpManager,
  uPixMapManager, uVFS, fFileAssoc,
  KASComp, fconfigtoolbar, uWCXprototypes, uDCUtils, uOSUtils,
  dmCommonData, fViewer, fOptions, fCopyDlg, fMoveDlg, fFindDlg,
  fSymLink, fMultiRename, fSplitter, fPackDlg, fExtractDlg, uDescr, fDescrEdit,
  LResources;
  
const
  dcBuildDate = {$I %DATE%};
  dcVersion = '0.4 alpha';
  lazVersion = {$I version.inc};
  fpcVersion = {$I %FPCVERSION%};
  TargetCPU = {$I %FPCTARGETCPU%};
  TargetOS = {$I %FPCTARGETOS%};

{$I revision.inc} // Lazarus revision number
{$I dcrevision.inc} // Double Commander revision number

{$IFDEF WINDOWS}{$R doublecmd.rc}{$ENDIF}

begin
  {$I doublecmd.lrs}
  Application.Title:= 'Double Commander';
  Application.Initialize;
  ThousandSeparator:= ' ';
  DebugLn('Double Commander ' + dcVersion);
  DebugLn('Revision: ' + dcRevision);
  DebugLn('Build: ' + dcBuildDate);
  DebugLn('Lazarus: ' + lazVersion + '-' + RevisionStr);
  DebugLn('Free Pascal: ' + fpcVersion);
  DebugLn('Platform: ' + TargetCPU + '-' + TargetOS + '-' + LCLPlatform[WidgetSet.LCLPlatform]);
  DebugLn('This program is free software released under terms of GNU GPL 2');
  DebugLn('(C)opyright 2006-2009 Koblov Alexander (Alexx2000@mail.ru)');
  DebugLn('   and contributors (see about dialog)');

  fAbout.dcBuildDate := dcBuildDate;
  fAbout.dcVersion:= dcVersion;
  fAbout.dcRevision:= dcRevision;
  fAbout.lazRevision:= RevisionStr;
  fAbout.fpcVersion:= fpcVersion;
  fAbout.TargetCPU:= TargetCPU;
  fAbout.TargetOS:= TargetOS;

  LoadPaths; // must be first
  Application.ShowMainForm:= False;
  Application.CreateForm(TfrmHackForm, frmHackForm);
  if LoadGlobs then
     begin
       LoadPixMapManager;
       Application.CreateForm(TfrmMain, frmMain); // main form
       Application.CreateForm(TdmHighl, dmHighl); // highlighters
       Application.CreateForm(TdmComData, dmComData); // common data
       Application.CreateForm(TdmHelpManager, dmHelpMgr); // help manager
       Application.Run;
     end;
end.
