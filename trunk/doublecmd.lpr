{ $threading on}
program doublecmd;
// uGlobs must be first in uses, uLng must be before any form;
{%File 'doc/changelog.txt'}

{.$APPTYPE GUI}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  uGlobsPaths,
  uGlobs,
  uLng,
  uIni,
  SysUtils,
  Forms,
  fMain,
  fAbout,
  uFileList,
  uFilePanel,
  uFileOp,
  uConstants,
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
  uFilter,
  fMsg,
  uSpaceThread,
  fHotDir,
  uConv,
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
  uPixMapManager, uVFS_,
  KASComp, fbtnchangedlg, fconfigtoolbar, uWCXprototypes;
  
{$IFDEF WIN32}
{$R XP.res}
{$ENDIF}

 begin
   Application.Title:='Double Commander';
//  try
  Application.Initialize;
  ThousandSeparator:=' ';
  writeln('Double commander 0.1 alpha - Free Pascal');
  writeln('This program is free software released under terms of GNU GPL 2');
  writeln('(C)opyright 2006-7 Koblov Alexander (Alexx2000@mail.ru)');
  writeln('  and contributors (see about dialog)');
  
  LoadPaths;
  LoadGlobs;
  LoadPixMapManager;
  Application.CreateForm(TfrmMain, frmMain); // main form
  Application.CreateForm(TdmHighl, dmHighl); // highlighters
  Application.Run;
{  except
  on E:Exception do
    Writeln('Critical unhandled exception:', E.Message);
end}

end.
