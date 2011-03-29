{
   Double Commander
   -------------------------------------------------------------------------
   Localization core unit

   Copyright (C) 2007-2009  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit uLng;

{$mode objfpc}{$H+}

interface
uses
  LResources;
resourcestring
  rsMsgNotDelete = 'Can not delete file %s';
  rsMsgErrRename = 'Cannot rename file %s to %s';
  rsMsgErrCannotCopyFile = 'Cannot copy file %s to %s';
  rsMsgFileExistsRwrt = 'File %s exists, overwrite?';
  rsMsgFileChangedSave = 'File %s changed, save?';
  rsMsgFolderExistsRwrt = 'Folder %s exists, overwrite?';
  rsMsgFileReadOnly = 'File %s is marked as read-only. Delete it?';
  rsMsgNewFile = 'New file';
  rsMsgDelFlDr = 'Delete %d selected files/directories?';
  rsMsgDelSel = 'Delete selected "%s"?';
  // 12.05.2009 - another message, when deleting to trash
  rsMsgDelFlDrT = 'Delete %d selected files/directories into trash can?';
  rsMsgDelSelT = 'Delete selected "%s" into trash can?';
  rsMsgDelToTrashForce = 'Can not delete "%s" to trash! Delete directly?';
  // ---
  rsMsgWipeFlDr = 'Wipe %d selected files/directories?';
  rsMsgWipeSel = 'Wipe selected "%s"?';
  rsMsgCpFlDr = 'Copy %d selected files/directories?';
  rsMsgCpSel = 'Copy selected "%s"?';
  rsMsgRenFlDr = 'Rename/move %d selected files/directories?';
  rsMsgRenSel = 'Rename/move selected "%s"?';
  rsMsgErrForceDir = 'Can not create directory %s!';
  rsMsgSelected = 'Selected %s of %s in %d of %d files';
  rsMsgPopUpHotAdd = '&Add %s';
  rsMsgPopUpHotCnf = '&Configure';
  rsMsgCloseLockedTab = 'This tab (%s) is locked! Close anyway?';
  rsSpaceMsg = 'Files: %d, Dirs: %d, Size: %s (%s bytes)';
  rsSelectDir = 'Select a directory';
  rsMarkPlus = 'Select mask';
  rsMarkMinus = 'Unselect mask';
  rsMaskInput = 'Input mask:';
  rsFreeMsg = 'Free %s from %s bytes';
  rsFreeMsgShort = '%s bytes free';
  rsMsgErrDirExists = 'Directory %s exists!';
  rsMsgPopUpHotDelete = '&Delete %s';
  rsMsgDiskNotAvail = 'Disk is not available';
  rsMsgChDirFailed = 'ChDir to [%s] failed!';
  rsMsgNoFreeSpaceCont = 'No enough free space on target drive, Continue?';
  rsMsgNoFreeSpaceRetry = 'No enough free space on target drive, Retry?';
  rsMsgSetVolumeLabel = 'Set volume label';
  rsMsgVolumeLabel = 'Volume label:';
  rsMsgRestartForApplyChanges = 'Please, restart Double Commander in order to apply changes';
  rsMsgEnterName = 'Enter name:';
  rsMsgEnterFileExt = 'Enter file extension:';
  rsMsgFollowSymlink = 'Follow symlink "%s"?';
  rsMsgFileSizeTooBig = 'The file size of "%s" is too big for destination file system!';
  rsMsgCloseAllInActiveTabs = 'Remove all inactive tabs?';
  rsMsgErrRegExpSyntax = 'Syntax error in regular expression!';
  rsMsgNoFilesSelected = 'No files selected.';
  rsMsgTooManyFilesSelected = 'Too many files selected.';
  rsMsgInvalidSelection = 'Invalid selection.';
  rsMsgNotImplemented = 'Not implemented.';
  rsMsgInvalidFilename = 'Invalid filename';
  rsMsgSelectOnlyCheckSumFiles = 'Please select only check sum files!';
  rsMsgPresetAlreadyExists = 'Preset "%s" already exists. Overwrite?';
  rsMsgManualAddHotDir = 'Manually add hot path';
  rsMsgManualEditHotDir = 'Manualy edit hot path';
  rsMsgManualHotDirQuery = 'Enter name and path (format name=path):';
  rsMsgVolumeSizeEnter = 'Please enter the volume size:';
  rsMsgArchiverCustomParams = 'Additional parameters for archiver command-line:';
  rsMsgMasterPassword = 'Master Password';
  rsMsgMasterPasswordEnter = 'Please enter the master password:';
  rsMsgPasswordEnter = 'Please enter the password:';
  rsMsgUserName = 'User name:';
  rsMsgPassword = 'Password:';
  rsMsgAccount = 'Account:';
  rsMsgUserNameFirewall = 'User name (Firewall):';
  rsMsgPasswordFirewall = 'Password (Firewall):';
  rsMsgTargetDir = 'Target path:';
  rsMsgURL = 'URL:';
  rsMsgLoadingFileList = 'Loading file list...';
  rsMsgErrSetAttribute = 'Can not set attributes for "%s"';
  rsMsgErrSetDateTime = 'Can not set date/time for "%s"';
  rsMsgErrDateNotSupported = 'Date %s is not supported';
  rsMsgErrSaveFile = 'Cannot save file';
  rsMsgErrCanNotConnect = 'Can not connect to server: "%s"';
  rsMsgFileOperationsActive = 'File operations active';
  rsMsgFileOperationsActiveLong = 'Some file operations have not yet finished. Closing Double Commander may result in data loss.';
  rsMsgConfirmQuit = 'Are you sure you want to quit?';
  rsMsgCanNotCopyMoveItSelf = 'You can not copy/move a file "%s" to itself!';
  // for context menu
  rsMnuActions = 'Actions';
  rsMnuView = 'View';
  rsMnuEdit = 'Edit';
  rsMnuOpenWith = 'Open with ...';
  rsMnuMount = 'Mount';
  rsMnuUmount = 'Unmount';
  rsMnuNoMedia = 'No media available';
  rsMnuEject = 'Eject';
  rsMnuSortBy = 'Sort by';
  rsMnuNew = 'New';
  // wcx module messages
  rsMsgSelLocNextVol = 'Please select location of next volume';
  rsMsgNextVolUnpack = 'Next volume will be unpacked';
  // wcx module errors messages
  rsMsgErrEndArchive = 'No more files in archive';
  rsMsgErrNoMemory = 'Not enough memory';
  rsMsgErrBadData = 'Data is bad';
  rsMsgErrBadArchive = 'CRC error in archive data';
  rsMsgErrUnknownFormat = 'Archive format unknown';
  rsMsgErrEOpen = 'Cannot open existing file';
  rsMsgErrECreate = 'Cannot create file';
  rsMsgErrEClose = 'Error closing file';
  rsMsgErrERead = 'Error reading from file';
  rsMsgErrEWrite = 'Error writing to file';
  rsMsgErrSmallBuf = 'Buffer too small';
  rsMsgErrEAborted = 'Function aborted by user';
  rsMsgErrNoFiles = 'No files found';
  rsMsgErrTooManyFiles = 'Too many files to pack';
  rsMsgErrNotSupported = 'Function not supported!';
  rsMsgErrInvalidLink = 'Invalid link';

  // Buttons.
  rsDlgButtonOK = '&OK';
  rsDlgButtonNo = '&No';
  rsDlgButtonYes = '&Yes';
  rsDlgButtonCancel = '&Cancel';
  rsDlgButtonNone = 'Non&e';
  rsDlgButtonAppend = 'A&ppend';
  rsDlgButtonCopyInto = 'Copy &Into';
  rsDlgButtonOverwrite = '&Overwrite';
  rsDlgButtonOverwriteAll = 'Overwrite &All';
  rsDlgButtonSkip = '&Skip';
  rsDlgButtonSkipAll = 'S&kip All';
  rsDlgButtonAll = 'A&ll';
  rsDlgButtonRetry = 'Re&try';
  rsDlgButtonAbort = 'Ab&ort';

  // Log file
  rsMsgLogSuccess = 'Done: ';
  rsMsgLogError = 'Error: ';
  rsMsgLogInfo = 'Info: ';
  rsMsgLogCopy = 'Copy file %s';
  rsMsgLogMove = 'Move file %s';
  rsMsgLogDelete = 'Delete file %s';
  rsMsgLogLink = 'Create link %s';
  rsMsgLogSymLink = 'Create symlink %s';
  rsMsgLogMkDir = 'Create directory %s';
  rsMsgLogRmDir = 'Remove directory %s';
  rsMsgLogPack = 'Pack to file %s';
  rsMsgLogExtract = 'Extract file %s';
  rsMsgLogTest = 'Test file integrity %s';

  rsQuickSearchPanel = 'Find:';
  rsQuickFilterPanel = 'Filter:';
  rsSearchResult = 'Search result';

  // File operations dialog
  rsDlgSpeedTime = 'Speed %s/s, time remaining %s';
  rsDlgDel = 'Delete file(s)';
  rsDlgWipe = 'Wipe file(s)';
  rsDlgSplit = 'Split';
  rsDlgCombine = 'Combine';
  rsDlgCp = 'Copy file(s)';
  rsDlgMv = 'Move file(s)';
  rsDlgPack = 'Packing...';
  rsDlgExtract = 'Extracting...';
  rsDlgTest = 'Testing...';
  rsDlgDeleting = 'Deleting';
  rsDlgCheckSumCalc = 'Calculate check sum...';
  rsDlgCheckSumVerify = 'Verify check sum...';
  rsDlgAllOpProgress = 'Progress of all operations %d %%';
  rsDlgAllOpComplete = 'All operations is complete';
  rsDlgUnknownOperation = 'Unknown operation';
  rsDlgOpStart = 'Start';
  rsDlgOpPause = 'Pause';
  rsDlgQueueIn = 'In';
  rsDlgQueueOut = 'Out';
  rsDlgOpCaption = 'Caption of operation';
  // Viewer
  rsViewAboutText = 'Internal Viewer of Double Commander.';
  rsViewNotFound = '%s not found!';
  rsViewEncoding = 'Encoding';
  // Editor
  rsEditAboutText = 'Internal Editor of Double Commander.';
  
  // Columns in file panel
  rsColName = 'Name';
  rsColExt = 'Ext';
  rsColSize = 'Size';
  rsColDate = 'Date';
  rsColAttr = 'Attr';
  // Tools
  rsToolViewer = 'Viewer';
  rsToolEditor = 'Editor';
  rsToolDiffer = 'Differ';
  // Configure custom columns dialog
  rsConfColDelete = 'Delete';
  rsConfColCaption = 'Caption';
  rsConfColWidth = 'Width';
  rsConfColAlign = 'Align';
  rsConfColFieldCont = 'Field contents';
  rsConfColMove='Move';
  rsConfColConfig='Config';
  rsConfCustHeader='Customize column';
  // File properties dialog
  rsPropsYes = 'Yes (%s)';
  rsPropsNo = 'No';
  rsPropsFolder = 'Directory';
  rsPropsFile = 'File';
  rsPropsSpChrDev = 'Special character device';
  rsPropsSpBlkDev = 'Special block device';
  rsPropsNmdPipe = 'Named pipe';
  rsPropsSymLink = 'Symbolic link';
  rsPropsSocket = 'Socket';
  rsPropsUnknownType = 'Unknown type';
  rsPropsErrChMod = 'Can not change access rights for "%s"';
  rsPropsErrChOwn = 'Can not change owner for "%s"';
  //Compare by content Dialog
  rsCompareDiffs = 'Number of changes:';
  // Find files dialog
  rsFindSearchFiles = 'Find files';
  rsFindDefineTemplate = 'Define template';
  rsFindScanning = 'Scanning';
  rsFindScanned = 'Scanned: %d';
  rsFindFound = 'Found: %d';
  rsFindWhereBeg = 'Begin at';
  rsFindDirNoEx = 'Directory %s does not exist!';
  rsFindDepthAll = 'all (unlimited depth)';
  rsFindDepthCurDir = 'current dir only';
  rsFindDepth = '%s level(s)';
  rsFindSaveTemplateCaption = 'Save search template';
  rsFindSaveTemplateTitle = 'Template name:';

  rsTimeUnitSecond = 'Second(s)';
  rsTimeUnitMinute = 'Minute(s)';
  rsTimeUnitHour = 'Hour(s)';
  rsTimeUnitDay = 'Day(s)';
  rsTimeUnitWeek = 'Week(s)';
  rsTimeUnitMonth = 'Month(s)';
  rsTimeUnitYear = 'Year(s)';
  rsSizeUnitBytes = 'Bytes';
  rsSizeUnitKBytes = 'Kilobytes';
  rsSizeUnitMBytes = 'Megabytes';
  rsSizeUnitGBytes = 'Gigabytes';
  rsSizeUnitTBytes = 'Terabytes';

  // Other
  rsCopyNameTemplate = 'Copy (%d) %s';
  // Symlink dialog
  rsSymErrCreate = 'Error creating symlink.';
  // Hardlink dialog
  rsHardErrCreate = 'Error creating hardlink.';
  // Linker dialog
  rsLinkMsgOK = 'Linker complete';
  // Splitter dialog
  rsSplitSelDir = 'Select directory:';
  rsSplitErrFileSize = 'Incorrect file size format!';
  rsSplitErrDirectory = 'Unable to create target directory!';
  rsSplitErrSplitFile = 'Unable to split the file!';
  rsSplitMsgManyParts = 'The number of parts is more than 100! Continue?';
  // MultiRename dialog
  rsMulRenFileNameStyleList = 'No change;UPPERCASE;lowercase;First char uppercase;' +
                              'First Char Of Every Word Uppercase;';
  // Edit
  rsEditNewFile = 'new.txt';
  rsEditNewOpen = 'Open file';
  rsEditNewFileName = 'Filename:';
  // Edit search
  rsEditSearchCaption = 'Search';
  rsEditSearchReplace ='Replace';
  rsEditSearchFrw = '&Forward';
  rsEditSearchBack = '&Backward';
  // Options dialog
  rsOptLanguage = 'Language';
  rsOptBehav = 'Behaviors';
  rsOptTools = 'Tools';
  rsOptFonts = 'Fonts';
  rsOptColors = 'Colors';
  rsOptFilePanels = 'File panels';
  rsOptFileTypes = 'File types';
  rsOptHotKeys = 'Hot keys';
  rsOptPlugins = 'Plugins';
  rsOptLayout = 'Layout';
  rsOptFileOp = 'File operations';
  rsOptFolderTabs = 'Folder tabs';
  rsOptLog = 'Log';
  rsOptConfig = 'Configuration';
  rsOptQuickSearch = 'Quick search/filter';
  rsOptColumns = 'Columns';
  rsOptMiscellaneous = 'Miscellaneous';
  rsOptAutoRefresh = 'Auto refresh';
  rsOptIcons = 'Icons';
  rsOptIgnoreList = 'Ignore list';
  rsOptArchivers = 'Archivers';
  //-------------------------------
  rsOptArchiveParam = 'Parameter';
  rsOptArchiveValue = 'Value';
  rsOptArchiveDelete = 'Delete:';
  rsOptArchiveTest = 'Test:';
  rsOptArchiveExtractWithoutPath = 'Extract without path:';
  rsOptArchiveSelfExtract = 'Create self extracting archive:';
  rsOptArchiveID = 'ID (optional):';
  rsOptArchiveIDPos = 'ID Position (optional):';
  rsOptArchiveIDSeekRange = 'ID Seek Range (optional):';
  rsOptArchivePasswordQuery = 'Password query string:';
  //-------------------------------
  rsOptEnterExt = 'Enter extension';
  rsOptAssocPluginWith = 'Associate plugin "%s" with:';
  rsOptEnable = 'Enable';
  rsOptDisable = 'Disable';
  rsOptMouseSelectionButton = 'Left button;Right button;';
  rsOptAutoSizeColumn = 'First;Last;';
  rsOptTabsPosition = 'Top;Bottom;';
  rsOptArchiveTypeName = 'Archive type name:';
  //-------------------------------
  rsOptExampleMark = 'Mark';
  rsOptExampleText = 'Text';
  rsOptExampleCursor = 'Cursor';
  rsOptExampleMarkCursor = 'Mark + Cursor';
  // Hotkeys
  rsOptHotkeysCommands = 'Commands';
  rsOptHotkeysComments = 'Comments';
  rsOptHotkeysHotkeys = 'Hotkeys';
  rsOptHotkeysHotkey = 'Hotkey';
  rsOptHotkeysParameters = 'Parameters';
  rsOptHotkeysShortCutUsed = 'Shortcut in use';
  rsOptHotkeysShortCutUsedText1 = 'Shortcut %s is already used for %s.';
  rsOptHotkeysShortCutUsedText2 = 'Change it to %s?';
  rsOptHotkeysUsedBy = 'used by';
  // Plugins
  rsOptPluginsActive = 'Active';
  rsOptPluginsName = 'Name';
  rsOptPluginsRegisteredFor = 'Registered for';
  rsOptPluginsFileName = 'File name';
  rsOptPluginAlreadyAssigned = 'Plugin %s is already assigned for the following extensions:';
  //-------------------------------
  rsOptSortMethod = 'Alphabetical, considering accents;Natural sorting: alphabetical and numbers';

  //Columns Menu
  rsMenuConfigureCustomColumns= 'Configure custom columns';
  rsMenuConfigureThisCustomColumn= 'Configure this custom columns view';

  // Operation states.
  rsOperNotStarted = 'Not started';
  rsOperStarting = 'Starting';
  rsOperRunning = 'Running';
  rsOperPausing = 'Pausing';
  rsOperPaused = 'Paused';
  rsOperWaitingForFeedback = 'Waiting for user response';
  rsOperWaitingForConnection = 'Waiting for access to file source';
  rsOperStopping = 'Stopping';
  rsOperStopped = 'Stopped';
  rsOperFinished = 'Finished';
  rsOperAborted = 'Aborted';
  rsOperManualStart = 'Manually started';
  rsOperAutoStart = 'Auto-started';
  rsOperQueue = 'Queued';

  // Operation starting options.
  rsOperStartStateManualStart = 'Manual start';
  rsOperStartStateAutoStart = 'Start';
  rsOperStartStateQueueFirst = 'Queue as first';
  rsOperStartStateQueueLast = 'Queue as last';

  // Unhandled error.
  rsUnhandledExceptionMessage =
     'Please report this error to the bug tracker with a description '
   + 'of what you were doing and the following file:%s'
   + 'Press %s to continue or %s to abort the program.';

function GetLanguageName(poFileName : String) : String;
procedure lngLoadLng(const sFileName:String);
procedure DoLoadLng;

implementation
uses
  Classes, SysUtils, StrUtils, GetText, Translations, uGlobs, uGlobsPaths, uTranslator,
  LCLProc, uFileProcs, uDCUtils, uOSUtils;

function GetLanguageName(poFileName : String) : String;
var
  poFile : Integer;
  sLine : String;
  iPos1,
  iPos2 : Integer;
begin
  poFile:= mbFileOpen(poFileName, fmOpenRead);
  // find first msgid line
  FileReadLn(poFile, sLine);
  while Pos('msgid', sLine) = 0 do
    FileReadLn(poFile, sLine);
  // read msgstr line
  FileReadLn(poFile, sLine);
  repeat
    FileReadLn(poFile, sLine);
    // find language name line
    if Pos('X-Poedit-Language:', sLine) <> 0 then
      begin
        iPos1 := Pos(':', sLine) + 2;
        iPos2 := Pos('\n', sLine) - 1;
        Result := Copy(sLine, iPos1,  (iPos2 - iPos1) + 1);
        FileClose(poFile);
        Exit;
      end;
  until Pos('msgid', sLine) = 1;
  FileClose(poFile);
  Result := 'Language name not found';
end;

procedure TranslateLCL(poFileName: String);
var
  UserLang, LCLLngDir: String;
  Lang, FallbackLang: String;
begin
  LCLLngDir:= gpLngDir + PathDelim + 'lcl' + PathDelim;
  if NumCountChars('.', poFileName) >= 2 then
    begin
      UserLang:= ExtractDelimited(2, poFileName, ['.']);
      poFileName:= LCLLngDir + Format('lclstrconsts.%s.po', [UserLang]);
      if not mbFileExists(poFileName) then
        begin
          GetLanguageIDs(Lang,FallbackLang);
          poFileName:= LCLLngDir + Format('lclstrconsts.%s.po', [Lang]);
        end;
      if not mbFileExists(poFileName) then
        poFileName:= LCLLngDir + Format('lclstrconsts.%s.po', [FallbackLang]);
      if mbFileExists(poFileName) then
          Translations.TranslateUnitResourceStrings('LCLStrConsts', poFileName);
    end;
end;

procedure lngLoadLng(const sFileName:String);
var
  Lang, FallbackLang : String;
begin
  { Localization }
  if sFileName = 'doublecmd.po' then Exit;  // default english interface

  gPOFileName := sFileName;
  if not mbFileExists(gpLngDir + gPOFileName) then
    begin
      gPOFileName := 'doublecmd.%s.po';
      GetLanguageIDs(Lang, FallbackLang);
      gPOFileName := Format(gPOFileName,[FallbackLang]);
    end;
  if not mbFileExists(gpLngDir + gPOFileName) then
    begin
      gPOFileName := Format(gPOFileName,[Lang]);
    end;
  if mbFileExists(gpLngDir + gPOFileName) then
    begin
      DebugLn('Loading lng file: ' + gpLngDir + gPOFileName);
      TranslateLCL(gPOFileName);
      Translations.TranslateUnitResourceStrings('uLng', gpLngDir + gPOFileName);
      LRSTranslator := TTranslator.Create(gpLngDir + gPOFileName);
    end;
end;

procedure DoLoadLng;
begin
  lngLoadLng(gPOFileName);
end;

finalization
  if Assigned(LRSTranslator) then
    FreeAndNil(LRSTranslator);

end.
