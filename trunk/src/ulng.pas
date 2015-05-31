{
   Double Commander
   -------------------------------------------------------------------------
   Localization core unit

   Copyright (C) 2007-2015 Alexander Koblov (alexx2000@mail.ru)

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
  // File operations.
  rsMsgNotDelete = 'Can not delete file %s';
  rsMsgCannotDeleteDirectory = 'Cannot delete directory %s';
  rsMsgErrDirExists = 'Directory %s exists!';
  rsMsgErrRename = 'Cannot rename file %s to %s';
  rsMsgErrCannotCopyFile = 'Cannot copy file %s to %s';
  rsMsgFileExistsOverwrite = 'Overwrite:';
  rsMsgFileExistsWithFile = 'With file:';
  rsMsgFileExistsFileInfo = '%s bytes, %s';
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
  rsMsgFileNotFound = 'File "%s" not found.';
  // ---
  rsMsgWipeFlDr = 'Wipe %d selected files/directories?';
  rsMsgWipeSel = 'Wipe selected "%s"?';
  rsMsgCpFlDr = 'Copy %d selected files/directories?';
  rsMsgCpSel = 'Copy selected "%s"?';
  rsMsgRenFlDr = 'Rename/move %d selected files/directories?';
  rsMsgRenSel = 'Rename/move selected "%s"?';
  rsMsgErrForceDir = 'Can not create directory %s!';
  rsMsgSelectedInfo = 'Selected: %s of %s, files: %d of %d, folders: %d of %d';
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
  rsMsgDefaultCustomActionName = 'Custom action';
  rsMsgSelectExecutableFile = 'Select executable file for';
  rsMsgWithActionWith = 'with';
  rsMsgFollowSymlink = 'Follow symlink "%s"?';
  rsMsgFileSizeTooBig = 'The file size of "%s" is too big for destination file system!';
  rsMsgCloseAllInActiveTabs = 'Remove all inactive tabs?';
  rsMsgErrRegExpSyntax = 'Syntax error in regular expression!';
  rsMsgNoFilesSelected = 'No files selected.';
  rsMsgTooManyFilesSelected = 'Too many files selected.';
  rsMsgInvalidSelection = 'Invalid selection.';
  rsMsgNotImplemented = 'Not implemented.';
  rsMsgInvalidFilename = 'Invalid filename';
  rsMsgInvalidPath = 'Invalid path';
  rsMsgInvalidPathLong = 'Path %s contains forbidden characters.';
  rsMsgSelectOnlyCheckSumFiles = 'Please select only checksum files!';
  rsMsgPresetAlreadyExists = 'Preset "%s" already exists. Overwrite?';
  rsMsgVolumeSizeEnter = 'Please enter the volume size:';
  rsMsgArchiverCustomParams = 'Additional parameters for archiver command-line:';
  rsMsgMasterPassword = 'Master Password';
  rsMsgMasterPasswordEnter = 'Please enter the master password:';
  rsMsgPasswordEnter = 'Please enter the password:';
  rsMsgPasswordVerify = 'Please re-enter the password for verification:';
  rsMsgPasswordDiff = 'Passwords are different!';
  rsMsgUserName = 'User name:';
  rsMsgPassword = 'Password:';
  rsMsgAccount = 'Account:';
  rsMsgUserNameFirewall = 'User name (Firewall):';
  rsMsgPasswordFirewall = 'Password (Firewall):';
  rsMsgTargetDir = 'Target path:';
  rsMsgURL = 'URL:';
  rsMsgLoadingFileList = 'Loading file list...';
  rsMsgNoFiles = 'No files';
  rsMsgErrSetAttribute = 'Can not set attributes for "%s"';
  rsMsgErrSetDateTime = 'Can not set date/time for "%s"';
  rsMsgErrSetOwnership = 'Can not set owner/group for "%s"';
  rsMsgErrDateNotSupported = 'Date %s is not supported';
  rsMsgErrSaveFile = 'Cannot save file';
  rsMsgErrCanNotConnect = 'Can not connect to server: "%s"';
  rsMsgErrSaveAssociation = 'Can not save association!';
  rsMsgFileOperationsActive = 'File operations active';
  rsMsgFileOperationsActiveLong = 'Some file operations have not yet finished. Closing Double Commander may result in data loss.';
  rsMsgConfirmQuit = 'Are you sure you want to quit?';
  rsMsgCanNotCopyMoveItSelf = 'You can not copy/move a file "%s" to itself!';
  rsMsgTabRenameCaption = 'Rename tab';
  rsMsgTabRenamePrompt = 'New tab name:';
  rsMsgInvalidPlugin = 'This is not a valid plugin!';
  rsMsgInvalidPluginArchitecture = 'This plugin is built for Double Commander for %s.%sIt can not work with Double Commander for %s!';
  rsMsgErrCreateFileDirectoryExists = 'There already exists a directory named "%s".';
  rsMsgDeletePartiallyCopied = 'Delete the partially copied file ?';
  rsMsgInvalidCommandLine = 'Error in command line';
  rsMsgInvalidQuoting = 'Invalid quoting';
  rsMsgErrorInContextMenuCommand = 'Error in context menu command';
  rsMsgErrorLoadingConfiguration = 'Error when loading configuration';
  rsMsgInvalidFormatOfConfigurationFile = 'Invalid format of configuration file';
  rsDefaultSuffixDroppedText = '_DroppedText';
  rsDefaultSuffixDroppedTextRichtextFilename = '_DroppedRichtext';
  rsDefaultSuffixDroppedTextHTMLFilename = '_DroppedHTMLtext';
  rsDefaultSuffixDroppedTextUnicodeUTF16Filename = '_DroppedUnicodeUTF16text';
  rsDefaultSuffixDroppedTextUnicodeUTF8Filename = '_DroppedUnicodeUTF8text';
  rsDefaultSuffixDroppedTextSimpleFilename = '_DroppedSimpleText';
  rsCaptionForAskingFilename = 'Enter filename, with extension, for dropped text';
  rsMsgPromptAskingFilename = 'Filename for dropped text:';
  rsCaptionForTextFormatToImport = 'Text format to import';
  rsMsgForTextFormatToImport = 'Select the text format to import';
  rsMsgDragAndDropModifiedWantToSave = 'Options regarding mouse drag && drop have been modified. Do you want to save before to exit?';
  rsMsgFileOperationsModifiedWantToSave = 'Options regarding file operations have been modified. Do you want to save before to exit?';
  rsMsgMiscellaneousModifiedWantToSave = 'Miscellaneous options have been modified. Do you want to save before to exit?';
  rsMsgUserDidNotSetExtension = '<NO EXT>';
  rsMsgUserDidNotSetName = '<NO NAME>';
  rsMsgProblemExecutingCommand = 'Problem executing command (%s)';

  //Hot Dir related
  rsMsgHotDirWhatToDelete = 'Do you want to delete all elements inside the sub-menu [%s]?'+#$0A+'Answering NO will delete only menu delimiters but will keep element inside sub-menu.';
  rsMsgHotDirAddThisDirectory = 'Add current dir: ';
  rsMsgHotDirAddSelectedDirectory = 'Add selected dir: ';
  rsMsgHotDirReAddSelectedDirectory = 'Re-Add selected dir: ';
  rsMsgHotDirReAddThisDirectory = 'Re-Add current dir: ';
  rsMsgHotDirAddSelectedDirectories = 'Add %d selected dirs';
  rsMsgHotDirConfigHotlist = 'Configuration of Directory Hotlist';
  rsMsgHotDirDeleteAllEntries = 'Are you sure you want to remove all entries of your Directory Hotlist? (There is no "undo" to this action!)';
  rsMsgHotDirName = 'Hotdir name';
  rsMsgHotDirPath = 'Hotdir path';
  rsMsgHotDirJustPath = 'Path';
  rsMsgHotDirTarget = 'Hotdir target';
  rsMsgHotDirSubMenuName = 'Submenu name';
  rsMsgHotDirModifiedWantToSave = 'Directory Hotlist has been modified. Do you want to save before to exit?';
  rsMsgHotDirSimpleName = 'Name:';
  rsMsgHotDirSimpleSeparator = '(separator)';
  rsMsgHotDirSimpleMenu = 'Menu name:';
  rsMsgHotDirSimpleEndOfMenu = '(end of sub menu)';
  rsMsgHotDirSimpleCommand = 'Command:';
  rsMsgHotDirCommandName = 'Do command';
  rsMsgHotDirCommandSample = 'cm_somthing';
  rsMsgHotDirDemoName = 'This is hot dir named ';
  rsMsgHotDirDemoPath = 'This will change active frame to the following path:';
  rsMsgHotDirDemoCommand = 'This will execute the following command:';
  rsMsgHotDirDemoTarget = 'And inactive frame would change to the following path:';
  rsMsgHotDirLocateHotlistFile = 'Locate ".hotlist" file to import';
  rsMsgHotDirLocatePreviousSave = 'Select location and filename of previously saved Directory Hotlist file';
  rsMsgHotDirWhereToSave = 'Enter location and filename where to save a Directory Hotlist file';
  rsMsgHotDirRestoreWhat = 'Enter location and filename of Directory Hotlist to restore';
  rsMsgHotDirImportall = 'Import all!';
  rsMsgHotDirImportSel = 'Import selected';
  rsMsgHotDirImportHotlist = 'Import Directory Hotlist - Select the entries you want to import';
  rsMsgHotDirExportall = 'Export all!';
  rsMsgHotDirExportSel = 'Export selected';
  rsMsgHotDirExportHotlist = 'Export Directory Hotlist - Select the entries you want to export';
  rsMsgHotDirNbNewEntries = 'Number of new entries: %d';
  rsMsgHotDirTotalExported = 'Total entries exported: ';
  rsMsgHotDirErrorExporting = 'Error exporting entries...';
  rsMsgHotDirNothingToExport = 'Nothing selected to export!';
  rsMsgHotDirTipSpecialDirBut = 'Some functions to select appropriate path relative, absolute, windows special folders, etc.';
  rsMsgHotDirTipOrderPath = 'Determine if you want the active frame to be sorted in a specified order after changing directory';
  rsMsgHotDirTipOrderTarget = 'Determine if you want the not active frame to be sorted in a specified order after changing directory';
  rsMsgHotDirTotalBackuped = 'Total entries saved: %d'+#$0A+#$0A+'Backup filename: %s';
  rsMsgHotDirErrorBackuping = 'Error backuping entries...';
  rsHotDirWarningAbortRestoreBackup = 'Warning! When restoring a .hotlist backup file, this will erase existing list to replace by the imported one.'+#$0A+#$0A+
                                      'Are you sure you want to proceed?';
  rsHotDirNothingToImport = 'Sorry, nothing detected to import!';

  //Special dir related
  rsMsgSpecialDirUseDC = 'Use Double Commander special path...';
  rsMsgSpecialDirUseTC = 'Use Windows special folder (TC)...';
  rsMsgSpecialDirUseOther = 'Use other Windows special folder...';
  rsMsgSpecialDirEnvVar = 'Use environment variable...';
  rsMsgSpecialDirMkDCRel = 'Make relative to Double Commander special path...';
  rsMsgSpecialDirMkTCTel = 'Make relative to Windows special folder (TC)...';
  rsMsgSpecialDirMkWnRel = 'Make relative to other Windows special folder...';
  rsMsgSpecialDirMkEnvRel = 'Make relative to environment variable...';
  rsMsgSpecialDirMkAbso = 'Make path absolute';
  rsMsgSpecialDirAddActi = 'Add path from active frame';
  rsMsgSpecialDirAddNonActi = 'Add path from inactive frame';
  rsMsgSpecialDirBrowsSel = 'Browse and use selected path';

  rsMsgSpecialDir = 'Special Dirs';
  rsMsgSpecialDirGotoDC = 'Go to Double Commander special path...';
  rsMsgSpecialDirGotoTC = 'Go to Windows special folder (TC)...';
  rsMsgSpecialDirGotoOther = 'Go to other Windows special folder...';
  rsMsgSpecialDirGotoEnvVar = 'Go to environment variable...';

  //Total Commander related message
  rsMsgLocateTCExecutable = 'Locate TC executable file (totalcmd.exe or totalcmd64.exe)';
  rsMsgLocateTCConfiguation = 'Locate TC configuration file (wincmd.ini)';
  rsDefaultImportedTCToolbarHint = 'Imported TC toolbar';
  rsDefaultImportedDCToolbarHint = 'Imported DC toolbar';
  rsFilenameExportedTCBarPrefix = 'Exported_from_DC';

  // for context menu
  rsMnuActions = 'Actions';
  rsMnuOpen = 'Open';
  rsMnuView = 'View';
  rsMnuEdit = 'Edit';
  rsMnuOpenWith = 'Open with';
  rsMnuOpenWithOther = 'Other...';
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
  // Vfs
  rsVfsNetwork = 'Network';

  // Buttons.
  rsDlgButtonOK = '&OK';
  rsDlgButtonNo = '&No';
  rsDlgButtonYes = '&Yes';
  rsDlgButtonCancel = '&Cancel';
  rsDlgButtonNone = 'Non&e';
  rsDlgButtonAppend = 'A&ppend';
  rsDlgButtonResume = '&Resume';
  rsDlgButtonRename = 'R&ename';
  rsDlgButtonCopyInto = 'Copy &Into';
  rsDlgButtonCopyIntoAll = 'Copy Into &All';
  rsDlgButtonOverwrite = '&Overwrite';
  rsDlgButtonOverwriteAll = 'Overwrite &All';
  rsDlgButtonOverwriteOlder = 'Overwrite All Ol&der';
  rsDlgButtonOverwriteSmaller = 'Overwrite All S&maller';
  rsDlgButtonOverwriteLarger = 'Overwrite All &Larger';
  rsDlgButtonAutoRenameSource = 'A&uto-rename source files';
  rsDlgButtonSkip = '&Skip';
  rsDlgButtonSkipAll = 'S&kip All';
  rsDlgButtonIgnoreAll = 'I&gnore All';
  rsDlgButtonAll = 'A&ll';
  rsDlgButtonRetry = 'Re&try';
  rsDlgButtonAbort = 'Ab&ort';
  rsDlgButtonOther = 'Ot&her';

  rsDlgButtonContinue = '&Continue';
  rsDlgButtonExitProgram = 'E&xit program';

  // Log file
  rsMsgLogSuccess = 'Done: ';
  rsMsgLogError = 'Error: ';
  rsMsgLogInfo = 'Info: ';
  rsMsgLogCopy = 'Copy file %s';
  rsMsgLogMove = 'Move file %s';
  rsMsgLogDelete = 'Delete file %s';
  rsMsgLogWipe = 'Wipe file %s';
  rsMsgLogLink = 'Create link %s';
  rsMsgLogSymLink = 'Create symlink %s';
  rsMsgLogMkDir = 'Create directory %s';
  rsMsgLogRmDir = 'Remove directory %s';
  rsMsgLogWipeDir = 'Wipe directory %s';
  rsMsgLogPack = 'Pack to file %s';
  rsMsgLogExtract = 'Extract file %s';
  rsMsgLogTest = 'Test file integrity %s';
  rsMsgLogExtCmdLaunch = 'Launch external';
  rsMsgLogExtCmdResult = 'Result external';
  rsMsgLogOptionsModifiedWantToSave = 'Options regarding log have been modified.'+#$0A+#$0A+'Do you want to save them before to exit?';

  rsSearchResult = 'Search result';
  rsShowHelpFor = '&Show help for %s';
  rsClipboardContainsInvalidToolbarData = 'Clipboard doesn''t contain any valid toolbar data.';

  //Terminal
  rsMsgTerminalOptionsModifiedWantToSave = 'Options regarding terminal have been modified.'+#$0A+#$0A+'Do you want to save them before to exit?';

  //Panel Color Configuration
  rsMsgFilePanelColorModifiedWantToSave = 'Options regarding panel color have been modified.'+#$0A+#$0A+'Do you want to save them before to exit?';
  rsMsgPanelPreview = 'Below is a preview. You may move cursor and select files to get immediately an actual look and feel of the various settings.';

  // File operations dialog
  rsDlgAllOpComplete = 'All operations completed';
  rsDlgAllOpProgress = 'Progress of all operations %d %%';
  rsDlgCalcStatistics = 'Calculate files and folders';
  rsDlgCheckSumCalc = 'Calculate checksum...';
  rsDlgCheckSumVerify = 'Verify checksum...';
  rsDlgCp = 'Copy file(s)';
  rsDlgDel = 'Delete file(s)';
  rsDlgMv = 'Move file(s)';
  rsDlgOpPause = 'Pau&se';
  rsDlgOpStart = '&Start';
  rsDlgQueue = 'Queue';
  rsDlgSpeed = 'Speed %s/s';
  rsDlgSpeedTime = 'Speed %s/s, time remaining %s';
  rsDlgSplit = 'Split';
  rsDlgWipe = 'Wipe file(s)';

  // File operations
  rsFileOpFileExistsOptions = 'Ask;Overwrite;Overwrite Older;Skip';
  rsFileOpDirectoryExistsOptions = 'Ask;Overwrite;Copy into;Skip';
  rsFileOpSetPropertyErrorOptions = 'Ask;Don''t set anymore;Ignore errors';

  // Viewer
  rsViewAboutText = 'Internal Viewer of Double Commander.';
  rsViewNotFound = '%s not found!';
  rsViewEncoding = 'Encoding';
  // Editor
  rsEditGotoLineTitle = 'Goto Line';
  rsEditGotoLineQuery = 'Goto line:';
  rsEditAboutText = 'Internal Editor of Double Commander.';
  // Editor Highlighters
  rsSynLangPlainText = 'Plain text';
  rsSynDefaultText   = 'Default text';
  
  // Columns in file panel
  rsColName = 'Name';
  rsColExt = 'Ext';
  rsColSize = 'Size';
  rsColDate = 'Date';
  rsColAttr = 'Attr';

  // Filter status in file panel
  rsFilterStatus = 'FILTER';
  rsSearchStatus = 'SEARCH';

  // Cancel operations in file panel
  rsCancelFilter = 'Cancel Quick Filter';
  rsCancelOperation = 'Cancel Current Operation';

  // File function names
  rsFuncName = 'Name';
  rsFuncExt = 'Extension';
  rsFuncSize = 'Size';
  rsFuncAttr = 'Attributes';
  rsFuncPath = 'Path';
  rsFuncGroup = 'Group';
  rsFuncOwner = 'Owner';
  rsFuncMTime = 'Modification date/time';
  rsFuncCTime = 'Creation date/time';
  rsFuncATime = 'Access date/time';
  rsFuncLinkTo = 'Link to';
  rsFuncNameNoExt = 'Name without extension';
  rsFuncType = 'Type';
  rsFuncComment = 'Comment';
  rsFuncCompressedSize = 'Compressed size';

  // Tools
  rsToolViewer = 'Viewer';
  rsToolEditor = 'Editor';
  rsToolDiffer = 'Differ';
  rsToolTerminal = 'Terminal';
  rsToolErrorOpeningViewer = 'Error opening viewer';
  rsToolErrorOpeningEditor = 'Error opening editor';
  rsToolErrorOpeningDiffer = 'Error opening differ';
  rsToolErrorOpeningTerminal = 'Error opening terminal';
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
  rsPropsContains = 'Files: %d, folders: %d';
  rsPropsErrChMod = 'Can not change access rights for "%s"';
  rsPropsErrChOwn = 'Can not change owner for "%s"';
  // Compare by content Dialog
  rsDiffMatches = ' Matches: ';
  rsDiffModifies = ' Modifies: ';
  rsDiffAdds = ' Adds: ';
  rsDiffDeletes = ' Deletes: ';
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
  rsSearchTemplateUnnamed = '<unnamed template>';

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
  // CheckSumCalcVerify dialog
  rsCheckSumVerifyTitle = 'Verify checksum';
  rsCheckSumVerifyText = 'Enter checksum and select algorithm:';
  // CheckSumVerify dialog
  rsCheckSumVerifyGeneral = 'General:';
  rsCheckSumVerifyTotal = 'Total:';
  rsCheckSumVerifySuccess = 'Success:';
  rsCheckSumVerifyMissing = 'Missing:';
  rsCheckSumVerifyBroken = 'Broken:';
  rsCheckSumVerifyReadError = 'Read error:';
  // Drive status
  rsDriveNoMedia = '<no media>';
  rsDriveNoLabel = '<no label>';

  // Edit
  rsEditNewFile = 'new.txt';
  rsEditNewOpen = 'Open file';
  rsEditNewFileName = 'Filename:';

  // Edit search
  rsEditSearchCaption = 'Search';
  rsEditSearchReplace ='Replace';
  rsEditSearchFrw = '&Forward';
  rsEditSearchBack = '&Backward';
  rsZeroReplacement = 'No replacement took place.';
  rsXReplacements = 'Number of replacement: %d';

  // Options editors
  rsOptionsEditorArchivers = 'Archivers';
  rsOptionsEditorAutoRefresh = 'Auto refresh';
  rsOptionsEditorBehavior = 'Behaviors';
  rsOptionsEditorColors = 'Colors';
  rsOptionsEditorBriefView = 'Brief';
  rsOptionsEditorColumnsView = 'Columns';
  rsOptionsEditorCustomColumns = 'Custom columns';
  rsOptionsEditorConfiguration = 'Configuration';
  rsOptionsEditorDragAndDrop = 'Drag & drop';
  rsOptionsEditorDrivesListButton = 'Drives list button';
  rsOptionsEditorFileOperations = 'File operations';
  rsOptionsEditorFilePanels = 'File panels';
  rsOptionsEditorFileTypes = 'File types';
  rsOptionsEditorFilesViews = 'Files views';
  rsOptionsEditorFolderTabs = 'Folder tabs';
  rsOptionsEditorFonts = 'Fonts';
  rsOptionsEditorHighlighters = 'Highlighters';
  rsOptionsEditorHotKeys = 'Hot keys';
  rsOptionsEditorIcons = 'Icons';
  rsOptionsEditorIgnoreList = 'Ignore list';
  rsOptionsEditorKeyboard = 'Keys';
  rsOptionsEditorLanguage = 'Language';
  rsOptionsEditorLayout = 'Layout';
  rsOptionsEditorLog = 'Log';
  rsOptionsEditorMiscellaneous = 'Miscellaneous';
  rsOptionsEditorMouse = 'Mouse';
  rsOptionsEditorPlugins = 'Plugins';
  rsOptionsEditorQuickSearch = 'Quick search/filter';
  rsOptionsEditorTerminal = 'Terminal';
  rsOptionsEditorToolbar = 'Toolbar';
  rsOptionsEditorToolbarComplement = 'Complements';
  rsOptionsEditorTools = 'Tools';
  rsOptionsEditorTooltips = 'Tooltips';
  rsOptionsEditorFileAssoc = 'File associations';
  rsOptionsEditorFileAssicExtra = 'File associations extra';
  rsOptionsEditorDirectoryHotlist = 'Directory Hotlist';
  //-------------------------------
  rsOptConfigSortOrder = 'Classic, legacy order;Alphabetic order (but language still first)';
  rsOptDifferFramePosition = 'Active frame panel on left, inactive on right (legacy);Left frame panel on left, right on right';
  //-------------------------------
  rsOptArchiveParam = 'Parameter';
  rsOptArchiveValue = 'Value';
  rsOptArchiveDelete = 'Delete:';
  rsOptArchiveTest = 'Test:';
  rsOptArchiveExtractWithoutPath = 'Extract without path:';
  rsOptArchiveSelfExtract = 'Create self extracting archive:';
  rsOptArchiveID = 'ID:';
  rsOptArchiveIDPos = 'ID Position:';
  rsOptArchiveIDSeekRange = 'ID Seek Range:';
  rsOptArchivePasswordQuery = 'Password query string:';
  rsOptArchiveFormMode = 'Format parsing mode:';
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
  rsOptHotkeysAddDeleteShortcutLong =
    'Shortcut %s for cm_Delete will be registered, so it can be used to reverse this setting.';
  rsOptHotkeysAddShortcutButton = 'Add shortcut';
  rsOptHotkeysCannotSetShortcut = 'Cannot set shortcut';
  rsOptHotkeysChangeShortcut = 'Change shortcut';
  rsOptHotkeysDeleteTrashCanOverrides =
    'Shortcut %s for cm_Delete has a parameter that overrides this setting. Do you want to change this parameter to use the global setting?';
  rsOptHotkeysDeleteTrashCanParameterExists =
    'Shortcut %s for cm_Delete needs to have a parameter changed to match shortcut %s. Do you want to change it?';
  rsOptHotkeysSetDeleteShortcut = 'Set shortcut to delete file';
  rsOptHotkeysShortcutForDeleteAlreadyAssigned =
    'For this setting to work with shortcut %s, shortcut %s must be assigned to cm_Delete but it is already assigned to %s. Do you want to change it?';
  rsOptHotkeysShortcutForDeleteIsSequence =
    'Shortcut %s for cm_Delete is a sequence shortcut for which a hotkey with reversed Shift cannot be assigned. This setting might not work.';
  rsOptHotkeysCommand = 'Command';
  rsOptHotkeysDescription = 'Description';
  rsOptHotkeysFixParameter = 'Fix parameter';
  rsOptHotkeysHotkeys = 'Hotkeys';
  rsOptHotkeysHotkey = 'Hotkey';
  rsOptHotkeysNoHotkey = '<none>';
  rsOptHotkeysParameters = 'Parameters';
  rsOptHotkeysShortCutUsed = 'Shortcut in use';
  rsOptHotkeysShortCutUsedText1 = 'Shortcut %s is already used.';
  rsOptHotkeysShortCutUsedText2 = 'Change it to %s?';
  rsOptHotkeysUsedBy = 'used for %s in %s';
  rsOptHotkeysUsedWithDifferentParams = 'used for this command but with different parameters';
  rsOptHotkeysAddHotkey = 'Add hotkey for %s';
  rsOptHotkeysEditHotkey = 'Edit hotkey for %s';
  rsHotkeyCategoryMain = 'Main';
  rsHotkeyCategoryViewer = 'Viewer';
  rsHotkeyCategoryDiffer = 'Differ';
  rsHotkeyCategoryCopyMoveDialog = 'Copy/Move Dialog';
  rsHotkeyCategoryEditCommentDialog = 'Edit Comment Dialog';
  rsCmdCategoryListInOrder='All;Active Panel;Left Panel;Right Panel;File Operations;Configuration;Network;Miscellaneous;Parallel Port;Print;Mark;Security;Clipboard;FTP;Navigation;Help;Window;Command Line;Tools;View;User;Tabs;Sorting;Log';
  rsCmdKindOfSort='Legacy sorted;A-Z sorted';
  rsMsgThisIsNowInClipboard = '"%s" is now in the clipboard';
  rsSimpleWordAll = 'All';
  rsSimpleWordCategory = 'Category';
  rsSimpleWordFilename = 'Filename';
  rsSimpleWordParameter = 'Param';
  rsSimpleWordWorkDir = 'WorkDir';
  rsSimpleWordResult = 'Result';
  rsSimpleWordPreview = 'Preview';
  rsSimpleWordColumnSingular = 'Column';
  // Plugins
  rsOptPluginsActive = 'Active';
  rsOptPluginsName = 'Name';
  rsOptPluginsRegisteredFor = 'Registered for';
  rsOptPluginsFileName = 'File name';
  rsOptPluginAlreadyAssigned = 'Plugin %s is already assigned for the following extensions:';
  //-------------------------------
  rsOptSortMethod = 'Alphabetical, considering accents;Natural sorting: alphabetical and numbers';
  rsOptSortCaseSens = 'not case sensitive;according to locale settings (aAbBcC);first upper then lower case (ABCabc)';
  rsOptSortFolderMode = 'sort by name and show first;sort like files and show first;sort like files';
  rsOptNewFilesPosition = 'at the top of the file list;after directories (if directories are sorted before files);at sorted position;at the bottom of the file list';
  rsOptUpdatedFilesPosition = 'don''t change position;use the same setting as for new files;to sorted position';
  rsOptFileOperationsProgressKind = 'separate window;minimized separate window;operations panel';
  rsOptTypeOfDuplicatedRename = 'DC legacy - Copy (x) filename.ext;Windows - filename (x).ext;Other - filename(x).ext';
  rsOptFileSizeFormat = 'float;B;K;M;G';
  // Keyboard
  rsOptLetters = 'None;Command Line;Quick Search;Quick Filter';
  // Directory hotlist
  rsOptAddFromMainPanel = 'Add at beginning;Add at the end;Smart add';
  //File Associations
  rsMsgTitleExtNotInFileType = 'Extension of selected file is not in any recognized file types';
  rsMsgSekectFileType = 'Select to which file type to add extension "%s"';
  rsMsgCreateANewFileType = '< Create a new file type "%s files" >';
  rsMsgEnterNewFileTypeName = 'Enter name of new file type to create for extension "%s"';
  rsMsgEnterCustomAction = 'Enter custom action name:';
  rsSimpleWordFiles = 'files';
  rsMsgFileAssociationsModifiedWantToSave = 'File extension associations have been modified.'+#$0A+#$0A+'Do you want to save before to exit?';
  rsMsgFileAssociationsExtraModifiedWantToSave = 'File extension associations extra options have been modified.'+#$0A+#$0A+'Do you want to save before to exit?';
  rsViewWithInternalViewer = 'with internal viewer';
  rsEditWithInternalEditor = 'with internal editor';
  rsViewWithExternalViewer = 'with external viewer';
  rsEditWithExternalEditor = 'with external editor';
  rsExecuteViaShell = 'Execute via shell';
  rsExecuteViaTerminalClose = 'Execute via terminal and close';
  rsExecuteViaTerminalStayOpen = 'Execute via terminal and stay open';
  rsConfigurationFileAssociation = 'Configure file association';
  //Variables
  rsConfirmExecution = 'Confirming command line and parameters';
  rsVarHelpWith = 'Help with "%" variables';
  rsVarOtherExamples = 'Other example of what''s possible';

  rsVarOnlyFilename = 'Only filename';
  rsVarPath = 'Path, with ending delimiter';
  rsVarFullPath = 'Complete filename (path+filename)';
  rsVarFilenameNoExt = 'Just filename, no extension';
  rsVarOnlyExtension = 'Only file extension';
  rsVarRelativePathAndFilename = 'Filename with relative path';
  rsVarCurrentPath = 'Path of panel';
  rsVarListFilename = 'Temporary filename of list of filenames';
  rsVarListFullFilename = 'Temporary filename of list of complete filenames (path+filename)';
  rsVarListRelativeFilename = 'Temporary filename of list of filenames with relative path';

  rsVarSourcePanel = 'Active panel (source)';
  rsVarTargetPanel = 'Inactive panel (target)';
  rsVarLeftPanel = 'Left panel';
  rsVarRightPanel = 'Right panel';
  rsVarBothPanelLeftToRight = 'Both panels, from left to right';
  rsVarBothPanelActiveToInactive = 'Both panels, from active to inactive';

  rsVarShowCommandPrior = 'Show command prior execute';
  rsVarPercentSign = 'Return the percent sign';
  rsVarPercentChangeToPound =  'From here to the end of the line, the percent-variable indicator is the "#" sign';
  rsVarPoundChangeToPercent =  'From here to the end of the line, the percent-variable indicator is back the "%" sign';
  rsVarWillNotBeQuoted =  'Filenames will not be quoted from here';
  rsVarWillBeQuoted =  'Filenames will be quoted from here (default)';
  rsVarWillNotHaveEndingDelimiter =  'Paths will not have ending delimiter (default)';
  rsVarWillHaveEndingDelimiter =  'Paths will have ending delimiter';
  rsVarWillNotDoInTerminal =  'Command will be done in terminal, closed at the end';
  rsVarWillDoInTerminal =  'Command will be done in terminal, remaining opened at the end';
  rsVarSimpleMessage = '%[Simple message]';
  rsVarSimpleShowMessage =  'Will show a simple message';
  rsVarPromptUserForParam = '%[Prompt user for param;Default value proposed]';
  rsVarInputParam =  'Will request request user to enter a parameter with a default suggested value';
  rsVarPrependElement = 'Prepend each name with "-a " or what you want';
  rsVarEncloseElement = 'Enclose each name in brackets or what you want';
  rsVarSecondElementRightPanel = 'Full path of second selected file in right panel';

  // Quick Search/Filter
  rsOptSearchItems = '&Files;Di&rectories;Files a&nd Directories';
  rsOptSearchCase = '&Sensitive;&Insensitive';
  rsOptSearchOpt = '&Hide filter panel when not focused;Keep saving setting modifications for next session';

  // Toolbar
  rsOptToolbarButtonType = 'S&eparator;Inte&rnal command;E&xternal command;Men&u';
  rsImportToolbarProblem = 'Cannot find reference to default bar file';
  rsMsgToolbarSaved = 'Saved!'+#$0A+'Toolbar filename: %s';
  rsMsgToolbarErrorNotSaved = 'Error! Problem saving toolbar!';
  rsMsgTCToolbarWhereToSave = 'Enter location and filename where to save a TC Toolbar file';
  rsMsgDCToolbarWhereToSave = 'Enter location and filename where to save a DC Toolbar file';
  rsMsgToolbarRestoreWhat = 'Enter location and filename of Toolbar to restore';
  rsMsgToolbarLocateTCToolbarFile = 'Locate ".BAR" file to import';
  rsMsgToolbarLocateDCToolbarFile = 'Locate ".toolbar" file to import';
  rsMsgTCToolbarNotFound = 'Error! Cannot find the desired wanted TC toolbar output folder:'+#$0A+'%s';
  rsMsgTCConfigNotFound = 'Error! Cannot find the TC configuration file:'+#$0A+'%s';
  rsMsgTCExecutableNotFound = 'Error! Cannot find the TC configuration executable:'+#$0A+'%s';
  rsMsgTCisRunning = 'Error! TC is still running but it should be closed for this operation.'+#$0A+'Close it and press OK or press CANCEL to abort.';
  rsMsgToolbarModifiedWantToSave = 'Toolbar has been modified. Do you want to save before to exit?';

  //Columns Menu
  rsMenuConfigureCustomColumns= 'Configure custom columns';
  rsMenuConfigureThisCustomColumn= 'Configure this custom columns view';
  rsMenuConfigureEnterCustomColumnName = 'Enter new custom columns name';
  rsMenuConfigureColumnsSaveToChange = 'To change current editing colmuns view, either SAVE, COPY or DELETE current editing one';
  rsMenuConfigureColumnsAlreadyExists = 'A columns view with that name already exists.';
  rsMsgColumnsModifiedWantToSave = 'Options regarding columns view have been modified.'+#$0A+#$0A+'Do you want to save them before to exit?';

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
  rsOperQueued = 'Queued';

  // Operations descriptions.
  rsOperCalculatingCheckSum = 'Calculating checksum';
  rsOperCalculatingCheckSumIn = 'Calculating checksum in "%s"';
  rsOperCalculatingCheckSumOf = 'Calculating checksum of "%s"';
  rsOperCalculatingStatictics = 'Calculating';
  rsOperCalculatingStatisticsIn = 'Calculating "%s"';
  rsOperCombining = 'Joining';
  rsOperCombiningFromTo = 'Joining files in "%s" to "%s"';
  rsOperCopying = 'Copying';
  rsOperCopyingFromTo = 'Copying from "%s" to "%s"';
  rsOperCopyingSomethingTo = 'Copying "%s" to "%s"';
  rsOperCreatingDirectory = 'Creating directory';
  rsOperCreatingSomeDirectory = 'Creating directory "%s"';
  rsOperDeleting = 'Deleting';
  rsOperDeletingIn = 'Deleting in "%s"';
  rsOperDeletingSomething = 'Deleting "%s"';
  rsOperExecuting = 'Executing';
  rsOperExecutingSomething = 'Executing "%s"';
  rsOperExtracting = 'Extracting';
  rsOperExtractingFromTo = 'Extracting from "%s" to "%s"';
  rsOperListing = 'Listing';
  rsOperListingIn = 'Listing "%s"';
  rsOperMoving = 'Moving';
  rsOperMovingFromTo = 'Moving from "%s" to "%s"';
  rsOperMovingSomethingTo = 'Moving "%s" to "%s"';
  rsOperPacking = 'Packing';
  rsOperPackingFromTo = 'Packing from "%s" to "%s"';
  rsOperPackingSomethingTo = 'Packing "%s" to "%s"';
  rsOperSettingProperty = 'Setting property';
  rsOperSettingPropertyIn = 'Setting property in "%s"';
  rsOperSettingPropertyOf = 'Setting property of "%s"';
  rsOperSplitting = 'Splitting';
  rsOperSplittingFromTo = 'Splitting "%s" to "%s"';
  rsOperTesting = 'Testing';
  rsOperTestingSomething = 'Testing "%s"';
  rsOperTestingIn = 'Testing in "%s"';
  rsOperVerifyingCheckSum = 'Verifying checksum';
  rsOperVerifyingCheckSumIn = 'Verifying checksum in "%s"';
  rsOperVerifyingCheckSumOf = 'Verifying checksum of "%s"';
  rsOperWiping = 'Wiping';
  rsOperWipingIn = 'Wiping in "%s"';
  rsOperWipingSomething = 'Wiping "%s"';
  rsOperWorking = 'Working'; // Generic description for unknown operation

  //Split/Combine operation special message
  rsMsgBadCRC32 = 'Bad CRC32 for resulting file:'+#$0A+'"%s"'+#$0A+#$0A+'Do you want to keep the resulting corrupted file anyway?';
  rsMsgProvideThisFile = 'Please, make this file available. Retry?';
  rsMsgIncorrectFilelength = 'Incorrect resulting filelength for file : "%s"';
  rsMSgUndeterminedNumberOfFile = 'Undetermined';
  rsMsgInsertNextDisk = 'Please insert next disk or something similar.'+#$0A+#$0A+'It is to allow writing this file:'+#$0A+'"%s"'+#$0A+''+#$0A+'Number of bytes still to write: %d';
  msgTryToLocateCRCFile = 'This file cannot be found and could help to validate final combination of files:'+#$0A+'%s'+#$0A+#$0A+'Could you make it available and press "OK" when ready,'+#$0A+'or press "CANCEL" to continue without it?';

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
  uDebug, uFileProcs, DCOSUtils, DCStrUtils;

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
    if Pos('X-Native-Language:', sLine) <> 0 then
      begin
        iPos1 := Pos(':', sLine) + 2;
        iPos2 := Pos('\n', sLine) - 1;
        Result := Copy(sLine, iPos1,  (iPos2 - iPos1) + 1);
        FileClose(poFile);
        Exit;
      end;
  until Pos('msgid', sLine) = 1;
  FileClose(poFile);
  Result := 'Unknown';
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
      DCDebug('Loading lng file: ' + gpLngDir + gPOFileName);
      LRSTranslator := TTranslator.Create(gpLngDir + gPOFileName);
      TranslateLCL(gPOFileName);
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
