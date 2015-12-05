{
    Double Commander
    -------------------------------------------------------------------------
    Creates Total Commander fake window (some plugins don't work without it)

    Copyright (C) 2009-2015 Alexander Koblov (alexx2000@mail.ru)

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

{
Equivalence of some abreviation here:

TC = Total Commander
DC = Double Commander
}

unit uTotalCommander;

{$MODE DELPHI}

{.$DEFINE DEBUG}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Windows, Classes,

  //DC
  uFile, uFileView, uFormCommands, KASToolItems, KASToolBar;
const
  TCCONFIG_MAINBAR_NOTPRESENT = ':-<#/?*+*?\#>-:';

  TCCONFIG_BUTTONBAR_SECTION = 'Buttonbar';
  TCCONFIG_BUTTONBAR_COUNT = 'Buttoncount';

  TCCONFIG_BUTTONHEIGHT = 'Buttonheight';
  TCCONFIG_BUTTON_PREFIX = 'button';
  TCCONFIG_ICONIC_PREFIX = 'iconic';
  TCCONFIG_CMD_PREFIX = 'cmd';
  TCCONFIG_STARTINGPATH_PREFIX = 'path';
  TCCONFIG_HINT_PREFIX = 'menu';
  TCCONFIG_PARAM_PREFIX = 'param';

var
  sTotalCommanderMainbarFilename: string = TCCONFIG_MAINBAR_NOTPRESENT;

procedure CreateTotalCommanderWindow(hWindow: HWND);
function ConvertTCStringToString(TCString: ansistring): string;
function ConvertStringToTCString(sString: string): ansistring;
function ReplaceDCEnvVars(const sText: string): string;
function ReplaceTCEnvVars(const sText: string): string;
function areWeInSituationToPlayWithTCFiles: boolean;
function GetTCEquivalentCommandToDCCommand(DCCommand: string; var TCIndexOfCommand: integer): string;
function GetTCIconFromDCIconAndCreateIfNecessary(const DCIcon: string): string;
function GetTCEquivalentCommandIconToDCCommandIcon(DCIcon: string; TCIndexOfCommand: integer): string;
procedure ExportDCToolbarsToTC(Toolbar: TKASToolbar; Barfilename: string; FlushExistingContent, FlagNeedToUpdateConfigIni: boolean);
procedure ImportTCToolbarsToDC(Barfilename: string; UpperToolItem: TKASToolItem; Toolbar: TKASToolbar; WhereToImport: integer; FCurrentButton: TKASToolButton; FFormCommands: IFormCommands);

implementation

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, LCLVersion, Forms, JwaDbt, SysUtils, LCLProc,

  //DC
  uFileProcs, uOSUtils, fOptionsMisc, uKASToolItemsExtended,
  DCClassesUtf8, DCOSUtils, uDebug, DCStrUtils, uPixMapManager, uShowMsg,
  uDCUtils, uLng, uGlobs, uGlobsPaths, DCConvertEncoding;
type
  { TTCommandEquivalence }
  TTCommandEquivalence = record
    TCCommand: string;
    TCIcon: longint;
    DCCommand: string;
  end;

const
  NUMBEROFCOMMANDS = 458;

  COMMANDS_LIST_TC: array[1..NUMBEROFCOMMANDS] of TTCommandEquivalence =
    (
    (TCCommand: 'cm_SrcComments'; TCIcon: 21; DCCommand: ''), //Source: Show comments
    (TCCommand: 'cm_SrcShort'; TCIcon: 3; DCCommand: 'cm_BriefView'), //Source: Only file names
    (TCCommand: 'cm_SrcLong'; TCIcon: 4; DCCommand: 'cm_ColumnsView'), //Source: All file details
    (TCCommand: 'cm_SrcTree'; TCIcon: 2; DCCommand: ''), //Source: Directory tree
    (TCCommand: 'cm_SrcQuickview'; TCIcon: 22; DCCommand: 'cm_QuickView'), //Source: Quick view panel
    (TCCommand: 'cm_VerticalPanels'; TCIcon: 23; DCCommand: 'cm_HorizontalFilePanels'), //File windows above each other
    (TCCommand: 'cm_SrcQuickInternalOnly'; TCIcon: 22; DCCommand: ''), //Source: Quick view, no plugins
    (TCCommand: 'cm_SrcHideQuickview'; TCIcon: 22; DCCommand: ''), //Source: Quick view panel off
    (TCCommand: 'cm_SrcExecs'; TCIcon: 12; DCCommand: ''), //Source: Only programs
    (TCCommand: 'cm_SrcAllFiles'; TCIcon: 13; DCCommand: ''), //Source: All files
    (TCCommand: 'cm_SrcUserSpec'; TCIcon: 24; DCCommand: ''), //Source: Last selected
    (TCCommand: 'cm_SrcUserDef'; TCIcon: 25; DCCommand: ''), //Source: Select user type
    (TCCommand: 'cm_SrcByName'; TCIcon: 5; DCCommand: 'cm_SortByName'), //Source: Sort by name
    (TCCommand: 'cm_SrcByExt'; TCIcon: 6; DCCommand: 'cm_SortByExt'), //Source: Sort by extension
    (TCCommand: 'cm_SrcBySize'; TCIcon: 8; DCCommand: 'cm_SortBySize'), //Source: Sort by size
    (TCCommand: 'cm_SrcByDateTime'; TCIcon: 7; DCCommand: 'cm_SortByDate'), //Source: Sort by date
    (TCCommand: 'cm_SrcUnsorted'; TCIcon: 9; DCCommand: ''), //Source: Unsorted
    (TCCommand: 'cm_SrcNegOrder'; TCIcon: 10; DCCommand: 'cm_ReverseOrder'), //Source: Reversed order
    (TCCommand: 'cm_SrcOpenDrives'; TCIcon: -1; DCCommand: 'cm_SrcOpenDrives'), //Source: Open drive list
    (TCCommand: 'cm_SrcThumbs'; TCIcon: 26; DCCommand: 'cm_ThumbnailsView'), //Source: Thumbnail view
    (TCCommand: 'cm_SrcCustomViewMenu'; TCIcon: 52; DCCommand: ''), //Source: Custom view menu
    (TCCommand: 'cm_SrcPathFocus'; TCIcon: -1; DCCommand: ''), //Source: Put focus on path
    (TCCommand: 'cm_LeftComments'; TCIcon: 21; DCCommand: ''), //Left: Show comments
    (TCCommand: 'cm_LeftShort'; TCIcon: 3; DCCommand: 'cm_LeftBriefView'), //Left: Only file names
    (TCCommand: 'cm_LeftLong'; TCIcon: 4; DCCommand: 'cm_LeftColumnsView'), //Left: All file details
    (TCCommand: 'cm_LeftTree'; TCIcon: 2; DCCommand: ''), //Left: Directory tree
    (TCCommand: 'cm_LeftQuickview'; TCIcon: 22; DCCommand: ''), //Left: Quick view panel
    (TCCommand: 'cm_LeftQuickInternalOnly'; TCIcon: 22; DCCommand: ''), //Left: Quick view, no plugins
    (TCCommand: 'cm_LeftHideQuickview'; TCIcon: 22; DCCommand: ''), //Left: Quick view panel off
    (TCCommand: 'cm_LeftExecs'; TCIcon: 12; DCCommand: ''), //Left: Only programs
    (TCCommand: 'cm_LeftAllFiles'; TCIcon: 13; DCCommand: ''), //Left: All files
    (TCCommand: 'cm_LeftUserSpec'; TCIcon: 24; DCCommand: ''), //Left: Last selected
    (TCCommand: 'cm_LeftUserDef'; TCIcon: 25; DCCommand: ''), //Left: Select user type
    (TCCommand: 'cm_LeftByName'; TCIcon: 5; DCCommand: 'cm_LeftSortByName'), //Left: Sort by name
    (TCCommand: 'cm_LeftByExt'; TCIcon: 6; DCCommand: 'cm_LeftSortByExt'), //Left: Sort by extension
    (TCCommand: 'cm_LeftBySize'; TCIcon: 8; DCCommand: 'cm_LeftSortBySize'), //Left: Sort by size
    (TCCommand: 'cm_LeftByDateTime'; TCIcon: 7; DCCommand: 'cm_LeftSortByDate'), //Left: Sort by date
    (TCCommand: 'cm_LeftUnsorted'; TCIcon: 9; DCCommand: ''), //Left: Unsorted
    (TCCommand: 'cm_LeftNegOrder'; TCIcon: 10; DCCommand: 'cm_LeftReverseOrder'), //Left: Reversed order
    (TCCommand: 'cm_LeftOpenDrives'; TCIcon: -1; DCCommand: 'cm_LeftOpenDrives'), //Left: Open drive list
    (TCCommand: 'cm_LeftPathFocus'; TCIcon: -1; DCCommand: ''), //Left: Put focus on path
    (TCCommand: 'cm_LeftDirBranch'; TCIcon: 50; DCCommand: 'cm_LeftFlatView'), //Left: Branch view
    (TCCommand: 'cm_LeftDirBranchSel'; TCIcon: 50; DCCommand: ''), //Left: branch view, only selected
    (TCCommand: 'cm_LeftThumbs'; TCIcon: 26; DCCommand: 'cm_LeftThumbView'), //Left: Thumbnail view
    (TCCommand: 'cm_LeftCustomViewMenu'; TCIcon: 52; DCCommand: ''), //Left: Custom view menu
    (TCCommand: 'cm_RightComments'; TCIcon: 21; DCCommand: ''), //Right: Show comments
    (TCCommand: 'cm_RightShort'; TCIcon: 3; DCCommand: 'cm_RightBriefView'), //Right: Only file names
    (TCCommand: 'cm_RightLong'; TCIcon: 4; DCCommand: 'cm_RightColumnsView'), //Right: All file details
    (TCCommand: 'cm_RightTree'; TCIcon: 2; DCCommand: ''), //Right: Directory tree
    (TCCommand: 'cm_RightQuickview'; TCIcon: 22; DCCommand: ''), //Right: Quick view panel
    (TCCommand: 'cm_RightQuickInternalOnly'; TCIcon: 22; DCCommand: ''), //Right: Quick view, no plugins
    (TCCommand: 'cm_RightHideQuickview'; TCIcon: 22; DCCommand: ''), //Right: Quick view panel off
    (TCCommand: 'cm_RightExecs'; TCIcon: 12; DCCommand: ''), //Right: Only programs
    (TCCommand: 'cm_RightAllFiles'; TCIcon: 13; DCCommand: ''), //Right: All files
    (TCCommand: 'cm_RightUserSpec'; TCIcon: 24; DCCommand: ''), //Right: Last selected
    (TCCommand: 'cm_RightUserDef'; TCIcon: 25; DCCommand: ''), //Right: Select user type
    (TCCommand: 'cm_RightByName'; TCIcon: 5; DCCommand: ''), //Right: Sort by name
    (TCCommand: 'cm_RightByExt'; TCIcon: 6; DCCommand: 'cm_RightSortByName'), //Right: Sort by extension
    (TCCommand: 'cm_RightBySize'; TCIcon: 8; DCCommand: 'cm_RightSortByExt'), //Right: Sort by size
    (TCCommand: 'cm_RightByDateTime'; TCIcon: 7; DCCommand: 'cm_RightSortBySize'), //Right: Sort by date
    (TCCommand: 'cm_RightUnsorted'; TCIcon: 9; DCCommand: 'cm_RightSortByDate'), //Right: Unsorted
    (TCCommand: 'cm_RightNegOrder'; TCIcon: 10; DCCommand: 'cm_RightReverseOrder'), //Right: Reversed order
    (TCCommand: 'cm_RightOpenDrives'; TCIcon: -1; DCCommand: 'cm_RightOpenDrives'), //Right: Open drive list
    (TCCommand: 'cm_RightPathFocus'; TCIcon: -1; DCCommand: ''), //Right: Put focus on path
    (TCCommand: 'cm_RightDirBranch'; TCIcon: 50; DCCommand: 'cm_RightFlatView'), //Right: branch view
    (TCCommand: 'cm_RightDirBranchSel'; TCIcon: 50; DCCommand: ''), //Right: branch view, only selected
    (TCCommand: 'cm_RightThumbs'; TCIcon: 26; DCCommand: 'cm_RightThumbView'), //Right: Thumbnail view
    (TCCommand: 'cm_RightCustomViewMenu'; TCIcon: 52; DCCommand: ''), //Right: Custom view menu
    (TCCommand: 'cm_List'; TCIcon: 27; DCCommand: 'cm_View'), //View with Lister
    (TCCommand: 'cm_ListInternalOnly'; TCIcon: 27; DCCommand: 'cm_view'), //Lister without plugins/multimedia
    (TCCommand: 'cm_Edit'; TCIcon: 28; DCCommand: 'cm_Edit'), //Edit (Notepad)
    (TCCommand: 'cm_Copy'; TCIcon: 62; DCCommand: 'cm_Copy'), //Copy files
    (TCCommand: 'cm_CopySamepanel'; TCIcon: 62; DCCommand: 'cm_CopySamePanel'), //Copy within panel
    (TCCommand: 'cm_CopyOtherpanel'; TCIcon: 62; DCCommand: ''), //Copy to other
    (TCCommand: 'cm_RenMov'; TCIcon: 63; DCCommand: 'cm_Rename'), //Rename/Move files
    (TCCommand: 'cm_MkDir'; TCIcon: 29; DCCommand: 'cm_MakeDir'), //Make directory
    (TCCommand: 'cm_Delete'; TCIcon: 64; DCCommand: 'cm_Delete'), //Delete files
    (TCCommand: 'cm_TestArchive'; TCIcon: 60; DCCommand: 'cm_TestArchive'), //Test selected archives
    (TCCommand: 'cm_PackFiles'; TCIcon: 30; DCCommand: 'cm_PackFiles'), //Pack files
    (TCCommand: 'cm_UnpackFiles'; TCIcon: 31; DCCommand: 'cm_ExtractFiles'), //Unpack all
    (TCCommand: 'cm_RenameOnly'; TCIcon: 32; DCCommand: 'cm_RenameOnly'), //Rename (Shift+F6)
    (TCCommand: 'cm_RenameSingleFile'; TCIcon: 32; DCCommand: ''), //Rename file under cursor
    (TCCommand: 'cm_MoveOnly'; TCIcon: 63; DCCommand: ''), //Move (F6)
    (TCCommand: 'cm_Properties'; TCIcon: -1; DCCommand: 'cm_FileProperties'), //Properties dialog
    (TCCommand: 'cm_CreateShortcut'; TCIcon: 65; DCCommand: ''), //Create a shortcut
    (TCCommand: 'cm_Return'; TCIcon: -1; DCCommand: 'cm_Open'), //Simulate: Return pressed
    (TCCommand: 'cm_OpenAsUser'; TCIcon: -1; DCCommand: ''), //Open program under cursor as different user
    (TCCommand: 'cm_Split'; TCIcon: 68; DCCommand: 'cm_FileSpliter'), //Split file into pieces
    (TCCommand: 'cm_Combine'; TCIcon: 69; DCCommand: 'cm_FileLinker'), //Combine partial files
    (TCCommand: 'cm_Encode'; TCIcon: 66; DCCommand: ''), //Encode MIME/UUE/XXE
    (TCCommand: 'cm_Decode'; TCIcon: 67; DCCommand: ''), //Decode MIME/UUE/XXE/BinHex
    (TCCommand: 'cm_CRCcreate'; TCIcon: -1; DCCommand: 'cm_CheckSumCalc'), //Create CRC checksums
    (TCCommand: 'cm_CRCcheck'; TCIcon: 61; DCCommand: 'cm_CheckSumVerify'), //Verify CRC checksums
    (TCCommand: 'cm_SetAttrib'; TCIcon: 33; DCCommand: 'cm_SetFileProperties'), //Change attributes
    (TCCommand: 'cm_Config'; TCIcon: 34; DCCommand: 'cm_Options'), //Conf: Layout (first page)
    (TCCommand: 'cm_DisplayConfig'; TCIcon: 34; DCCommand: ''), //Conf: Display
    (TCCommand: 'cm_IconConfig'; TCIcon: 34; DCCommand: ''), //Conf: Icons
    (TCCommand: 'cm_FontConfig'; TCIcon: 34; DCCommand: ''), //Conf: Font
    (TCCommand: 'cm_ColorConfig'; TCIcon: 34; DCCommand: ''), //Conf: Colors
    (TCCommand: 'cm_ConfTabChange'; TCIcon: 34; DCCommand: ''), //Conf: Tabstops
    (TCCommand: 'cm_DirTabsConfig'; TCIcon: 34; DCCommand: ''), //Conf: Directory tabs
    (TCCommand: 'cm_CustomColumnConfig'; TCIcon: 56; DCCommand: ''), //Conf: Custom colums
    (TCCommand: 'cm_CustomColumnDlg'; TCIcon: 56; DCCommand: ''), //Change current custom columns
    (TCCommand: 'cm_LanguageConfig'; TCIcon: 34; DCCommand: ''), //Conf: Language
    (TCCommand: 'cm_Config2'; TCIcon: 34; DCCommand: ''), //Conf: Operation
    (TCCommand: 'cm_EditConfig'; TCIcon: 34; DCCommand: ''), //Conf: Viewer/Editor
    (TCCommand: 'cm_CopyConfig'; TCIcon: 34; DCCommand: ''), //Conf: Copy/Delete
    (TCCommand: 'cm_RefreshConfig'; TCIcon: 34; DCCommand: ''), //Conf: Refresh file lists
    (TCCommand: 'cm_QuickSearchConfig'; TCIcon: 34; DCCommand: ''), //Conf: Quick Search
    (TCCommand: 'cm_FtpConfig'; TCIcon: 34; DCCommand: ''), //FTP options
    (TCCommand: 'cm_PluginsConfig'; TCIcon: 34; DCCommand: ''), //Conf: Plugins
    (TCCommand: 'cm_ThumbnailsConfig'; TCIcon: 34; DCCommand: ''), //Conf: Thumbnails
    (TCCommand: 'cm_LogConfig'; TCIcon: 34; DCCommand: ''), //Conf: Log file
    (TCCommand: 'cm_IgnoreConfig'; TCIcon: 34; DCCommand: ''), //Conf: Ignore list
    (TCCommand: 'cm_PackerConfig'; TCIcon: 34; DCCommand: ''), //Conf: Packer
    (TCCommand: 'cm_ZipPackerConfig'; TCIcon: 34; DCCommand: ''), //Conf: ZIP packer
    (TCCommand: 'cm_Confirmation'; TCIcon: 34; DCCommand: ''), //Conf: Misc, Confirmation
    (TCCommand: 'cm_ConfigSavePos'; TCIcon: -1; DCCommand: ''), //Conf: Save position
    (TCCommand: 'cm_ButtonConfig'; TCIcon: 14; DCCommand: 'cm_ConfigToolbars'), //Conf: Button bar
    (TCCommand: 'cm_ConfigSaveSettings'; TCIcon: -1; DCCommand: ''), //Save current paths etc.
    (TCCommand: 'cm_ConfigChangeIniFiles'; TCIcon: -1; DCCommand: ''), //Open ini files in notepad
    (TCCommand: 'cm_ConfigSaveDirHistory'; TCIcon: -1; DCCommand: ''), //Save directory history
    (TCCommand: 'cm_ChangeStartMenu'; TCIcon: 34; DCCommand: ''), //Change Start menu
    (TCCommand: 'cm_NetConnect'; TCIcon: 53; DCCommand: 'cm_NetworkConnect'), //Network connections
    (TCCommand: 'cm_NetDisconnect'; TCIcon: 54; DCCommand: 'cm_NetworkDisconnect'), //Disconnect network drives
    (TCCommand: 'cm_NetShareDir'; TCIcon: -1; DCCommand: ''), //Share directory
    (TCCommand: 'cm_NetUnshareDir'; TCIcon: -1; DCCommand: ''), //Unshare directory
    (TCCommand: 'cm_AdministerServer'; TCIcon: -1; DCCommand: ''), //Connect to admin share to open \\server\c$ etc.
    (TCCommand: 'cm_ShowFileUser'; TCIcon: -1; DCCommand: ''), //Which remote user has opened a local file
    (TCCommand: 'cm_GetFileSpace'; TCIcon: -1; DCCommand: 'cm_CalculateSpace'), //Calculate space
    (TCCommand: 'cm_VolumeId'; TCIcon: -1; DCCommand: ''), //Volume label
    (TCCommand: 'cm_VersionInfo'; TCIcon: -1; DCCommand: ''), //Version information
    (TCCommand: 'cm_ExecuteDOS'; TCIcon: -1; DCCommand: 'cm_RunTerm'), //Open command prompt window
    (TCCommand: 'cm_CompareDirs'; TCIcon: 35; DCCommand: 'cm_CompareDirectories'), //Compare dirs
    (TCCommand: 'cm_CompareDirsWithSubdirs'; TCIcon: 35; DCCommand: 'cm_CompareDirectories'), //Also mark subdirs not present in other dir
    (TCCommand: 'cm_ContextMenu'; TCIcon: -1; DCCommand: 'cm_ContextMenu'), //Show context menu
    (TCCommand: 'cm_ContextMenuInternal'; TCIcon: -1; DCCommand: ''), //Show context menu for internal associations
    (TCCommand: 'cm_ContextMenuInternalCursor'; TCIcon: -1; DCCommand: ''), //Internal context menu for file under cursor
    (TCCommand: 'cm_ShowRemoteMenu'; TCIcon: -1; DCCommand: ''), //Context menu for Media Center remote control Play/Pause
    (TCCommand: 'cm_SyncChangeDir'; TCIcon: 75; DCCommand: ''), //Synchronous directory changing in both windows
    (TCCommand: 'cm_EditComment'; TCIcon: -1; DCCommand: 'cm_EditComment'), //Edit file comment
    (TCCommand: 'cm_FocusLeft'; TCIcon: -1; DCCommand: ''), //Focus on left file list
    (TCCommand: 'cm_FocusRight'; TCIcon: -1; DCCommand: ''), //Focus on right file list
    (TCCommand: 'cm_FocusCmdLine'; TCIcon: -1; DCCommand: 'cm_FocusCmdLine'), //Focus on command line
    (TCCommand: 'cm_FocusButtonBar'; TCIcon: -1; DCCommand: ''), //Focus on button bar
    (TCCommand: 'cm_CountDirContent'; TCIcon: 36; DCCommand: 'cm_CountDirContent'), //Calculate space occupied by subdirs in current dir
    (TCCommand: 'cm_UnloadPlugins'; TCIcon: -1; DCCommand: ''), //Unload all plugins
    (TCCommand: 'cm_DirMatch'; TCIcon: 35; DCCommand: ''), //Mark newer
    (TCCommand: 'cm_Exchange'; TCIcon: 37; DCCommand: 'cm_Exchange'), //Swap panels
    (TCCommand: 'cm_MatchSrc'; TCIcon: 86; DCCommand: 'cm_TargetEqualSource'), //target=Source
    (TCCommand: 'cm_ReloadSelThumbs'; TCIcon: -1; DCCommand: ''), //Re-load selected thumbnails
    (TCCommand: 'cm_DirectCableConnect'; TCIcon: -1; DCCommand: ''), //Connect to other PC by cable
    (TCCommand: 'cm_NTinstallDriver'; TCIcon: -1; DCCommand: ''), //Install parallel port driver on NT
    (TCCommand: 'cm_NTremoveDriver'; TCIcon: -1; DCCommand: ''), //Remove parallel port driver on NT
    (TCCommand: 'cm_PrintDir'; TCIcon: 38; DCCommand: ''), //Print current directory (with preview)
    (TCCommand: 'cm_PrintDirSub'; TCIcon: 38; DCCommand: ''), //Print dir with subdirs
    (TCCommand: 'cm_PrintFile'; TCIcon: 38; DCCommand: ''), //Print file
    (TCCommand: 'cm_SpreadSelection'; TCIcon: 39; DCCommand: 'cm_MarkPlus'), //Select group
    (TCCommand: 'cm_SelectBoth'; TCIcon: 72; DCCommand: ''), //Select group: files+folders
    (TCCommand: 'cm_SelectFiles'; TCIcon: 70; DCCommand: ''), //Select group: just files
    (TCCommand: 'cm_SelectFolders'; TCIcon: 71; DCCommand: ''), //Select group: just folders
    (TCCommand: 'cm_ShrinkSelection'; TCIcon: 40; DCCommand: 'cm_MarkMinus'), //Unselect group
    (TCCommand: 'cm_ClearFiles'; TCIcon: 40; DCCommand: ''), //Unselect group: just files
    (TCCommand: 'cm_ClearFolders'; TCIcon: 40; DCCommand: ''), //Unselect group: just folders
    (TCCommand: 'cm_ClearSelCfg'; TCIcon: 40; DCCommand: ''), //Unselect group (files or both, as configured)
    (TCCommand: 'cm_SelectAll'; TCIcon: 44; DCCommand: 'cm_MarkMarkAll'), //Select all (files or both, as configured)
    (TCCommand: 'cm_SelectAllBoth'; TCIcon: 44; DCCommand: ''), //Select both files+folders
    (TCCommand: 'cm_SelectAllFiles'; TCIcon: 44; DCCommand: ''), //Select all files
    (TCCommand: 'cm_SelectAllFolders'; TCIcon: 44; DCCommand: ''), //Select all folders
    (TCCommand: 'cm_ClearAll'; TCIcon: -1; DCCommand: 'cm_MarkUnmarkAll'), //Unselect all (files+folders)
    (TCCommand: 'cm_ClearAllFiles'; TCIcon: -1; DCCommand: ''), //Unselect all files
    (TCCommand: 'cm_ClearAllFolders'; TCIcon: -1; DCCommand: ''), //Unselect all folders
    (TCCommand: 'cm_ClearAllCfg'; TCIcon: -1; DCCommand: ''), //Unselect all (files or both, as configured)
    (TCCommand: 'cm_ExchangeSelection'; TCIcon: 11; DCCommand: 'cm_MarkInvert'), //Invert selection
    (TCCommand: 'cm_ExchangeSelBoth'; TCIcon: 11; DCCommand: ''), //Invert selection (files+folders)
    (TCCommand: 'cm_ExchangeSelFiles'; TCIcon: 11; DCCommand: ''), //Invert selection (files)
    (TCCommand: 'cm_ExchangeSelFolders'; TCIcon: 11; DCCommand: ''), //Invert selection (folders)
    (TCCommand: 'cm_SelectCurrentExtension'; TCIcon: 41; DCCommand: 'cm_MarkCurrentExtension'), //Select all files with same ext.
    (TCCommand: 'cm_UnselectCurrentExtension'; TCIcon: -1; DCCommand: 'cm_UnmarkCurrentExtension'), //Unselect all files with same ext.
    (TCCommand: 'cm_SelectCurrentName'; TCIcon: -1; DCCommand: ''), //Select all files with same name
    (TCCommand: 'cm_UnselectCurrentName'; TCIcon: -1; DCCommand: ''), //Unselect all files with same name
    (TCCommand: 'cm_SelectCurrentNameExt'; TCIcon: -1; DCCommand: ''), //Select all files with same name+ext.
    (TCCommand: 'cm_UnselectCurrentNameExt'; TCIcon: -1; DCCommand: ''), //Unselect all files with same name+ext.
    (TCCommand: 'cm_SelectCurrentPath'; TCIcon: 72; DCCommand: ''), //Select all in same path (for branch view+search)
    (TCCommand: 'cm_UnselectCurrentPath'; TCIcon: -1; DCCommand: ''), //Unselect all in same path
    (TCCommand: 'cm_RestoreSelection'; TCIcon: 42; DCCommand: 'cm_RestoreSelection'), //Selection before last operation
    (TCCommand: 'cm_SaveSelection'; TCIcon: 43; DCCommand: 'cm_SaveSelection'), //Temporarily save selection
    (TCCommand: 'cm_SaveSelectionToFile'; TCIcon: -1; DCCommand: 'cm_SaveSelectionToFile'), //Save file selection to file
    (TCCommand: 'cm_SaveSelectionToFileA'; TCIcon: -1; DCCommand: ''), //Save file selection to file (ANSI)
    (TCCommand: 'cm_SaveSelectionToFileW'; TCIcon: -1; DCCommand: ''), //Save file selection to file (Unicode)
    (TCCommand: 'cm_SaveDetailsToFile'; TCIcon: -1; DCCommand: ''), //Save all shown columns to file
    (TCCommand: 'cm_SaveDetailsToFileA'; TCIcon: -1; DCCommand: ''), //Save all shown columns to file (ANSI)
    (TCCommand: 'cm_SaveDetailsToFileW'; TCIcon: -1; DCCommand: ''), //Save all shown columns to file (Unicode)
    (TCCommand: 'cm_LoadSelectionFromFile'; TCIcon: -1; DCCommand: 'cm_LoadSelectionFromFile'), //Read file selection from file
    (TCCommand: 'cm_LoadSelectionFromClip'; TCIcon: -1; DCCommand: 'cm_LoadSelectionFromClip'), //Read file selection from clipboard
    (TCCommand: 'cm_EditPermissionInfo'; TCIcon: -1; DCCommand: ''), //Permissions dialog (NTFS)
    (TCCommand: 'cm_EditPersmissionInfo'; TCIcon: -1; DCCommand: ''), //Typo...
    (TCCommand: 'cm_EditAuditInfo'; TCIcon: -1; DCCommand: ''), //File auditing (NTFS)
    (TCCommand: 'cm_EditOwnerInfo'; TCIcon: -1; DCCommand: ''), //Take ownership (NTFS)
    (TCCommand: 'cm_CutToClipboard'; TCIcon: -1; DCCommand: 'cm_CutToClipboard'), //Cut selected files to clipboard
    (TCCommand: 'cm_CopyToClipboard'; TCIcon: -1; DCCommand: 'cm_CopyToClipboard'), //Copy selected files to clipboard
    (TCCommand: 'cm_PasteFromClipboard'; TCIcon: -1; DCCommand: 'cm_PasteFromClipboard'), //Paste from clipboard to current dir
    (TCCommand: 'cm_CopyNamesToClip'; TCIcon: 45; DCCommand: 'cm_CopyNamesToClip'), //Copy filenames to clipboard
    (TCCommand: 'cm_CopyFullNamesToClip'; TCIcon: 45; DCCommand: 'cm_CopyFullNamesToClip'), //Copy names with full path
    (TCCommand: 'cm_CopyNetNamesToClip'; TCIcon: 45; DCCommand: ''), //Copy names with UNC path
    (TCCommand: 'cm_CopySrcPathToClip'; TCIcon: 45; DCCommand: ''), //Copy source path to clipboard
    (TCCommand: 'cm_CopyTrgPathToClip'; TCIcon: 45; DCCommand: ''), //Copy target path to clipboard
    (TCCommand: 'cm_CopyFileDetailsToClip'; TCIcon: 59; DCCommand: 'cm_CopyFileDetailsToClip'), //Copy all shown columns
    (TCCommand: 'cm_CopyFpFileDetailsToClip'; TCIcon: 59; DCCommand: ''), //Copy all columns, with full path
    (TCCommand: 'cm_CopyNetFileDetailsToClip'; TCIcon: 59; DCCommand: ''), //Copy all columns, with UNC path
    (TCCommand: 'cm_FtpConnect'; TCIcon: 16; DCCommand: ''), //Connect to FTP
    (TCCommand: 'cm_FtpNew'; TCIcon: 17; DCCommand: ''), //New FTP connection
    (TCCommand: 'cm_FtpDisconnect'; TCIcon: -1; DCCommand: ''), //Disconnect from FTP
    (TCCommand: 'cm_FtpHiddenFiles'; TCIcon: -1; DCCommand: ''), //Show hidden FTP files
    (TCCommand: 'cm_FtpAbort'; TCIcon: -1; DCCommand: ''), //Abort current FTP command
    (TCCommand: 'cm_FtpResumeDownload'; TCIcon: -1; DCCommand: ''), //Resume aborted download
    (TCCommand: 'cm_FtpSelectTransferMode'; TCIcon: -1; DCCommand: ''), //Select Binary, ASCII or Auto mode
    (TCCommand: 'cm_FtpAddToList'; TCIcon: -1; DCCommand: ''), //Add selected files to download list
    (TCCommand: 'cm_FtpDownloadList'; TCIcon: -1; DCCommand: ''), //Download files in download list
    (TCCommand: 'cm_GotoPreviousDir'; TCIcon: 18; DCCommand: ''), //Go back
    (TCCommand: 'cm_GotoNextDir'; TCIcon: 19; DCCommand: ''), //Go forward
    (TCCommand: 'cm_DirectoryHistory'; TCIcon: -1; DCCommand: 'cm_DirHistory'), //History list
    (TCCommand: 'cm_GotoPreviousLocalDir'; TCIcon: 18; DCCommand: ''), //Go back, no ftp
    (TCCommand: 'cm_GotoNextLocalDir'; TCIcon: 19; DCCommand: ''), //Go forward, no ftp
    (TCCommand: 'cm_DirectoryHotlist'; TCIcon: -1; DCCommand: 'cm_DirHotList'), //Directory popup menu
    (TCCommand: 'cm_GoToRoot'; TCIcon: -1; DCCommand: 'cm_ChangeDirToRoot'), //Go to root directory
    (TCCommand: 'cm_GoToParent'; TCIcon: 15; DCCommand: 'cm_ChangeDirToParent'), //Go to parent directory
    (TCCommand: 'cm_GoToDir'; TCIcon: -1; DCCommand: ''), //Open dir or zip under cursor
    (TCCommand: 'cm_OpenDesktop'; TCIcon: -1; DCCommand: ''), //Desktop folder
    (TCCommand: 'cm_OpenDrives'; TCIcon: -1; DCCommand: ''), //My computer
    (TCCommand: 'cm_OpenControls'; TCIcon: 20; DCCommand: ''), //Control panel
    (TCCommand: 'cm_OpenFonts'; TCIcon: -1; DCCommand: ''), //Fonts folder
    (TCCommand: 'cm_OpenNetwork'; TCIcon: -1; DCCommand: ''), //Network neighborhood
    (TCCommand: 'cm_OpenPrinters'; TCIcon: -1; DCCommand: ''), //Printers folder
    (TCCommand: 'cm_OpenRecycled'; TCIcon: -1; DCCommand: ''), //Recycle bin
    (TCCommand: 'cm_CDtree'; TCIcon: 1; DCCommand: ''), //Popup directory tree
    (TCCommand: 'cm_TransferLeft'; TCIcon: -1; DCCommand: 'cm_TransferLeft'), //Transfer dir under cursor to left window
    (TCCommand: 'cm_TransferRight'; TCIcon: -1; DCCommand: 'cm_TransferRight'), //Transfer dir under cursor to right window
    (TCCommand: 'cm_EditPath'; TCIcon: -1; DCCommand: 'cm_EditPath'), //Edit path field above file list
    (TCCommand: 'cm_GoToFirstFile'; TCIcon: -1; DCCommand: 'cm_GoToFirstFile'), //Place cursor on first file in list
    (TCCommand: 'cm_GotoNextDrive'; TCIcon: -1; DCCommand: ''), //Go one drive up (C->D)
    (TCCommand: 'cm_GotoPreviousDrive'; TCIcon: -1; DCCommand: ''), //Go one drive down
    (TCCommand: 'cm_GotoNextSelected'; TCIcon: -1; DCCommand: ''), //Go to next selected file
    (TCCommand: 'cm_GotoPrevSelected'; TCIcon: -1; DCCommand: ''), //Go to previous selected file
    (TCCommand: 'cm_GotoDriveA'; TCIcon: -1; DCCommand: ''), //Switch to drive A
    (TCCommand: 'cm_GotoDriveC'; TCIcon: -1; DCCommand: ''), //Switch to drive C
    (TCCommand: 'cm_GotoDriveD'; TCIcon: -1; DCCommand: ''), //Switch to drive D
    (TCCommand: 'cm_GotoDriveE'; TCIcon: -1; DCCommand: ''), //Switch to drive E
    (TCCommand: 'cm_GotoDriveF'; TCIcon: -1; DCCommand: ''), //(etc, define your own if)
    (TCCommand: 'cm_GotoDriveZ'; TCIcon: -1; DCCommand: ''), //(you need more drives)
    (TCCommand: 'cm_HelpIndex'; TCIcon: 55; DCCommand: 'cm_HelpIndex'), //Help index
    (TCCommand: 'cm_Keyboard'; TCIcon: -1; DCCommand: 'cm_Keyboard'), //Keyboard help
    (TCCommand: 'cm_Register'; TCIcon: -1; DCCommand: ''), //Registration info
    (TCCommand: 'cm_VisitHomepage'; TCIcon: -1; DCCommand: 'cm_VisitHomePage'), //Visit http://www.ghisler.com/
    (TCCommand: 'cm_About'; TCIcon: -1; DCCommand: 'cm_About'), //Help/About Total Commander
    (TCCommand: 'cm_Exit'; TCIcon: -1; DCCommand: 'cm_Exit'), //Exit Total Commander
    (TCCommand: 'cm_Minimize'; TCIcon: -1; DCCommand: 'cm_Minimize'), //Minimize Total Commander
    (TCCommand: 'cm_Maximize'; TCIcon: -1; DCCommand: ''), //Maximize Total Commander
    (TCCommand: 'cm_Restore'; TCIcon: -1; DCCommand: ''), //Restore normal size
    (TCCommand: 'cm_ClearCmdLine'; TCIcon: -1; DCCommand: ''), //Clear command line
    (TCCommand: 'cm_NextCommand'; TCIcon: -1; DCCommand: 'cm_CmdLineNext'), //Next command line
    (TCCommand: 'cm_PrevCommand'; TCIcon: -1; DCCommand: 'cm_CmdLinePrev'), //Previous command line
    (TCCommand: 'cm_AddPathToCmdline'; TCIcon: -1; DCCommand: 'cm_AddPathToCmdLine'), //Copy path to command line
    (TCCommand: 'cm_MultiRenameFiles'; TCIcon: 46; DCCommand: 'cm_MultiRename'), //Rename multiple files
    (TCCommand: 'cm_SysInfo'; TCIcon: -1; DCCommand: ''), //System information
    (TCCommand: 'cm_OpenTransferManager'; TCIcon: 74; DCCommand: ''), //Background transfer manager
    (TCCommand: 'cm_SearchFor'; TCIcon: 47; DCCommand: 'cm_Search'), //Search for
    (TCCommand: 'cm_SearchStandalone'; TCIcon: 47; DCCommand: ''), //Search in separate process
    (TCCommand: 'cm_FileSync'; TCIcon: 48; DCCommand: 'cm_SyncDirs'), //Synchronize directories
    (TCCommand: 'cm_Associate'; TCIcon: -1; DCCommand: ''), //Associate
    (TCCommand: 'cm_InternalAssociate'; TCIcon: -1; DCCommand: 'cm_FileAssoc'), //Define internal associations
    (TCCommand: 'cm_CompareFilesByContent'; TCIcon: 49; DCCommand: 'cm_CompareContents'), //File comparison
    (TCCommand: 'cm_IntCompareFilesByContent'; TCIcon: 49; DCCommand: 'cm_CompareContents'), //Use internal compare tool
    (TCCommand: 'cm_CommandBrowser'; TCIcon: 82; DCCommand: ''), //Browse internal commands
    (TCCommand: 'cm_VisButtonbar'; TCIcon: -1; DCCommand: ''), //Show/hide button bar
    (TCCommand: 'cm_VisDriveButtons'; TCIcon: -1; DCCommand: ''), //Show/hide drive button bars
    (TCCommand: 'cm_VisTwoDriveButtons'; TCIcon: -1; DCCommand: ''), //Show/hide two drive bars
    (TCCommand: 'cm_VisFlatDriveButtons'; TCIcon: -1; DCCommand: ''), //Buttons: Flat/normal mode
    (TCCommand: 'cm_VisFlatInterface'; TCIcon: -1; DCCommand: ''), //Interface: Flat/normal mode
    (TCCommand: 'cm_VisDriveCombo'; TCIcon: -1; DCCommand: ''), //Show/hide drive combobox
    (TCCommand: 'cm_VisCurDir'; TCIcon: -1; DCCommand: ''), //Show/hide current directory
    (TCCommand: 'cm_VisBreadCrumbs'; TCIcon: -1; DCCommand: ''), //Show/hide Breadcrumb bar
    (TCCommand: 'cm_VisTabHeader'; TCIcon: -1; DCCommand: ''), //Show/hide tab header (sorting)
    (TCCommand: 'cm_VisStatusbar'; TCIcon: -1; DCCommand: ''), //Show/hide status bar
    (TCCommand: 'cm_VisCmdLine'; TCIcon: -1; DCCommand: ''), //Show/hide Command line
    (TCCommand: 'cm_VisKeyButtons'; TCIcon: -1; DCCommand: ''), //Show/hide function key buttons
    (TCCommand: 'cm_ShowHint'; TCIcon: -1; DCCommand: ''), //Show file tip window
    (TCCommand: 'cm_ShowQuickSearch'; TCIcon: -1; DCCommand: ''), //Show name search window
    (TCCommand: 'cm_SwitchLongNames'; TCIcon: -1; DCCommand: ''), //Turn long names on and off
    (TCCommand: 'cm_RereadSource'; TCIcon: 0; DCCommand: 'cm_Refresh'), //Reread source
    (TCCommand: 'cm_ShowOnlySelected'; TCIcon: 73; DCCommand: ''), //Hide files which aren't selected
    (TCCommand: 'cm_SwitchHidSys'; TCIcon: 79; DCCommand: ''), //Turn hidden/system files on and off
    (TCCommand: 'cm_SwitchHid'; TCIcon: 79; DCCommand: ''), //Turn hidden files on and off
    (TCCommand: 'cm_SwitchSys'; TCIcon: 79; DCCommand: 'cm_ShowSysFiles'), //Turn system files on and off
    (TCCommand: 'cm_Switch83Names'; TCIcon: -1; DCCommand: ''), //Turn 8.3 names lowercase on/off
    (TCCommand: 'cm_SwitchDirSort'; TCIcon: -1; DCCommand: ''), //Turn directory sorting by name on/off
    (TCCommand: 'cm_DirBranch'; TCIcon: 50; DCCommand: 'cm_FlatView'), //Show all files in current dir and all subdirs
    (TCCommand: 'cm_DirBranchSel'; TCIcon: 50; DCCommand: ''), //Show selected files, and all in selected subdirs
    (TCCommand: 'cm_50Percent'; TCIcon: -1; DCCommand: ''), //Window separator at 50%
    (TCCommand: 'cm_100Percent'; TCIcon: -1; DCCommand: ''), //Window separator at 100%
    (TCCommand: 'cm_VisDirTabs'; TCIcon: -1; DCCommand: ''), //Show/hide folder tabs
    (TCCommand: 'cm_VisXPThemeBackground'; TCIcon: -1; DCCommand: ''), //Show/hide XP theme background
    (TCCommand: 'cm_SwitchOverlayIcons'; TCIcon: -1; DCCommand: ''), //Switch icon overlays on/off
    (TCCommand: 'cm_VisHistHotButtons'; TCIcon: -1; DCCommand: ''), //Show/hide dir history+hotlist
    (TCCommand: 'cm_SwitchWatchDirs'; TCIcon: 80; DCCommand: ''), //Enable/disable WatchDirs auto-refresh temporarily
    (TCCommand: 'cm_SwitchIgnoreList'; TCIcon: 81; DCCommand: 'cm_SwitchIgnoreList'), //Enable/disable ignore list file to not show file names
    (TCCommand: 'cm_SwitchX64Redirection'; TCIcon: -1; DCCommand: ''), //64-bit Windows: Redirect 32-bit system32 dir off/on
    (TCCommand: 'cm_SeparateTreeOff'; TCIcon: 76; DCCommand: ''), //Disable separate tree panel
    (TCCommand: 'cm_SeparateTree1'; TCIcon: 77; DCCommand: ''), //One separate tree panel
    (TCCommand: 'cm_SeparateTree2'; TCIcon: 78; DCCommand: ''), //Two separate tree panels
    (TCCommand: 'cm_SwitchSeparateTree'; TCIcon: 51; DCCommand: ''), //Switch through tree panel options
    (TCCommand: 'cm_ToggleSeparateTree1'; TCIcon: 77; DCCommand: ''), //One separate tree panel on/off
    (TCCommand: 'cm_ToggleSeparateTree2'; TCIcon: 78; DCCommand: ''), //Two separate tree panels on/off
    (TCCommand: 'cm_UserMenu1'; TCIcon: -1; DCCommand: ''), //Start first menu item in Start menu
    (TCCommand: 'cm_UserMenu2'; TCIcon: -1; DCCommand: ''), //Second item
    (TCCommand: 'cm_UserMenu3'; TCIcon: -1; DCCommand: ''), //Third item
    (TCCommand: 'cm_UserMenu4'; TCIcon: -1; DCCommand: ''), //etc.
    (TCCommand: 'cm_UserMenu5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_UserMenu6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_UserMenu7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_UserMenu8'; TCIcon: -1; DCCommand: ''), //You can add more
    (TCCommand: 'cm_UserMenu9'; TCIcon: -1; DCCommand: ''), //custom user menu ids
    (TCCommand: 'cm_UserMenu10'; TCIcon: -1; DCCommand: ''), //in totalcmd.inc!
    (TCCommand: 'cm_OpenNewTab'; TCIcon: 83; DCCommand: 'cm_NewTab'), //Open new tab
    (TCCommand: 'cm_OpenNewTabBg'; TCIcon: 83; DCCommand: ''), //Open new tab in background
    (TCCommand: 'cm_OpenDirInNewTab'; TCIcon: -1; DCCommand: 'cm_OpenDirInNewTab'), //Open dir under cursor in tab
    (TCCommand: 'cm_OpenDirInNewTabOther'; TCIcon: -1; DCCommand: ''), //Open dir under cursor (other window)
    (TCCommand: 'cm_SwitchToNextTab'; TCIcon: -1; DCCommand: 'cm_NextTab'), //Switch to next Tab (as Ctrl+Tab)
    (TCCommand: 'cm_SwitchToPreviousTab'; TCIcon: -1; DCCommand: 'cm_PrevTab'), //Switch to previous Tab (Ctrl+Shift+Tab)
    (TCCommand: 'cm_CloseCurrentTab'; TCIcon: 84; DCCommand: 'cm_CloseTab'), //Close tab
    (TCCommand: 'cm_CloseAllTabs'; TCIcon: 85; DCCommand: 'cm_CloseAllTabs'), //Close all
    (TCCommand: 'cm_DirTabsShowMenu'; TCIcon: -1; DCCommand: ''), //Show tab menu
    (TCCommand: 'cm_ToggleLockCurrentTab'; TCIcon: -1; DCCommand: ''), //Turn on/off tab locking
    (TCCommand: 'cm_ToggleLockDcaCurrentTab'; TCIcon: -1; DCCommand: ''), //Same but with dir changes allowed
    (TCCommand: 'cm_ExchangeWithTabs'; TCIcon: 37; DCCommand: ''), //Swap all Tabs
    (TCCommand: 'cm_GoToLockedDir'; TCIcon: -1; DCCommand: ''), //Go to the base dir of locked tab
    (TCCommand: 'cm_SrcActivateTab1'; TCIcon: -1; DCCommand: ''), //Activate first tab
    (TCCommand: 'cm_SrcActivateTab2'; TCIcon: -1; DCCommand: ''), //Activate second tab
    (TCCommand: 'cm_SrcActivateTab3'; TCIcon: -1; DCCommand: ''), //(Source window)
    (TCCommand: 'cm_SrcActivateTab4'; TCIcon: -1; DCCommand: ''), //etc.
    (TCCommand: 'cm_SrcActivateTab5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcActivateTab6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcActivateTab7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcActivateTab8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcActivateTab9'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcActivateTab10'; TCIcon: -1; DCCommand: ''), //(up to 99 items)
    (TCCommand: 'cm_TrgActivateTab1'; TCIcon: -1; DCCommand: ''), //Activate first tab
    (TCCommand: 'cm_TrgActivateTab2'; TCIcon: -1; DCCommand: ''), //Activate second tab
    (TCCommand: 'cm_TrgActivateTab3'; TCIcon: -1; DCCommand: ''), //(Target window)
    (TCCommand: 'cm_TrgActivateTab4'; TCIcon: -1; DCCommand: ''), //etc.
    (TCCommand: 'cm_TrgActivateTab5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgActivateTab6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgActivateTab7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgActivateTab8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgActivateTab9'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgActivateTab10'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftActivateTab1'; TCIcon: -1; DCCommand: ''), //Activate first tab
    (TCCommand: 'cm_LeftActivateTab2'; TCIcon: -1; DCCommand: ''), //Activate second tab
    (TCCommand: 'cm_LeftActivateTab3'; TCIcon: -1; DCCommand: ''), //(Left window)
    (TCCommand: 'cm_LeftActivateTab4'; TCIcon: -1; DCCommand: ''), //etc.
    (TCCommand: 'cm_LeftActivateTab5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftActivateTab6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftActivateTab7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftActivateTab8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftActivateTab9'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftActivateTab10'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightActivateTab1'; TCIcon: -1; DCCommand: ''), //Activate first tab
    (TCCommand: 'cm_RightActivateTab2'; TCIcon: -1; DCCommand: ''), //Activate second tab
    (TCCommand: 'cm_RightActivateTab3'; TCIcon: -1; DCCommand: ''), //(Right window)
    (TCCommand: 'cm_RightActivateTab4'; TCIcon: -1; DCCommand: ''), //etc.
    (TCCommand: 'cm_RightActivateTab5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightActivateTab6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightActivateTab7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightActivateTab8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightActivateTab9'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightActivateTab10'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcSortByCol1'; TCIcon: -1; DCCommand: ''), // Sort by first column
    (TCCommand: 'cm_SrcSortByCol2'; TCIcon: -1; DCCommand: ''), // Sort by second column
    (TCCommand: 'cm_SrcSortByCol3'; TCIcon: -1; DCCommand: ''), // (source window)
    (TCCommand: 'cm_SrcSortByCol4'; TCIcon: -1; DCCommand: ''), // etc.
    (TCCommand: 'cm_SrcSortByCol5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcSortByCol6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcSortByCol7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcSortByCol8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcSortByCol9'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcSortByCol10'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcSortByCol99'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgSortByCol1'; TCIcon: -1; DCCommand: ''), // Sort by first column
    (TCCommand: 'cm_TrgSortByCol2'; TCIcon: -1; DCCommand: ''), // Sort by second column
    (TCCommand: 'cm_TrgSortByCol3'; TCIcon: -1; DCCommand: ''), // (target window)
    (TCCommand: 'cm_TrgSortByCol4'; TCIcon: -1; DCCommand: ''), // etc.
    (TCCommand: 'cm_TrgSortByCol5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgSortByCol6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgSortByCol7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgSortByCol8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgSortByCol9'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgSortByCol10'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_TrgSortByCol99'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftSortByCol1'; TCIcon: -1; DCCommand: ''), // Sort by first column
    (TCCommand: 'cm_LeftSortByCol2'; TCIcon: -1; DCCommand: ''), // Sort by second column
    (TCCommand: 'cm_LeftSortByCol3'; TCIcon: -1; DCCommand: ''), // (left window)
    (TCCommand: 'cm_LeftSortByCol4'; TCIcon: -1; DCCommand: ''), // etc.
    (TCCommand: 'cm_LeftSortByCol5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftSortByCol6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftSortByCol7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftSortByCol8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftSortByCol9'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftSortByCol10'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftSortByCol99'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightSortByCol1'; TCIcon: -1; DCCommand: ''), // Sort by first column
    (TCCommand: 'cm_RightSortByCol2'; TCIcon: -1; DCCommand: ''), // Sort by second column
    (TCCommand: 'cm_RightSortByCol3'; TCIcon: -1; DCCommand: ''), // (right window)
    (TCCommand: 'cm_RightSortByCol4'; TCIcon: -1; DCCommand: ''), // etc.
    (TCCommand: 'cm_RightSortByCol5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightSortByCol6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightSortByCol7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightSortByCol8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightSortByCol9'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightSortByCol10'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightSortByCol99'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcCustomView1'; TCIcon: -1; DCCommand: ''), // Source: Custom columns 1
    (TCCommand: 'cm_SrcCustomView2'; TCIcon: -1; DCCommand: ''), // (user defined columns)
    (TCCommand: 'cm_SrcCustomView3'; TCIcon: -1; DCCommand: ''), // etc.
    (TCCommand: 'cm_SrcCustomView4'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcCustomView5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcCustomView6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcCustomView7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcCustomView8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcCustomView9'; TCIcon: -1; DCCommand: ''), // etc. until 299
    (TCCommand: 'cm_LeftCustomView1'; TCIcon: -1; DCCommand: ''), // Left: Custom columns 1
    (TCCommand: 'cm_LeftCustomView2'; TCIcon: -1; DCCommand: ''), // (user defined columns)
    (TCCommand: 'cm_LeftCustomView3'; TCIcon: -1; DCCommand: ''), // etc.
    (TCCommand: 'cm_LeftCustomView4'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftCustomView5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftCustomView6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftCustomView7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftCustomView8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_LeftCustomView9'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightCustomView1'; TCIcon: -1; DCCommand: ''), // Right: Custom columns 1
    (TCCommand: 'cm_RightCustomView2'; TCIcon: -1; DCCommand: ''), // (user defined columns)
    (TCCommand: 'cm_RightCustomView3'; TCIcon: -1; DCCommand: ''), // etc.
    (TCCommand: 'cm_RightCustomView4'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightCustomView5'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightCustomView6'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightCustomView7'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightCustomView8'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_RightCustomView9'; TCIcon: -1; DCCommand: ''),
    (TCCommand: 'cm_SrcNextCustomView'; TCIcon: 52; DCCommand: ''), //  Source: Next custom view
    (TCCommand: 'cm_SrcPrevCustomView'; TCIcon: 52; DCCommand: ''), //  Source: Previous custom view
    (TCCommand: 'cm_TrgNextCustomView'; TCIcon: 52; DCCommand: ''), //  Target: Next custom view
    (TCCommand: 'cm_TrgPrevCustomView'; TCIcon: 52; DCCommand: ''), //  Target: Previous custom view
    (TCCommand: 'cm_LeftNextCustomView'; TCIcon: 52; DCCommand: ''), // Left:   Next custom view
    (TCCommand: 'cm_LeftPrevCustomView'; TCIcon: 52; DCCommand: ''), // Left:   Previous custom view
    (TCCommand: 'cm_RightNextCustomView'; TCIcon: 52; DCCommand: ''), //Right:  Next custom view
    (TCCommand: 'cm_RightPrevCustomView'; TCIcon: 52; DCCommand: ''), //Right:  Previous custom view
    (TCCommand: 'cm_LoadAllOnDemandFields'; TCIcon: -1; DCCommand: ''), //Load on demand fields for all files
    (TCCommand: 'cm_LoadSelOnDemandFields'; TCIcon: -1; DCCommand: ''), //Load on demand fields for selected files
    (TCCommand: 'cm_ContentStopLoadFields'; TCIcon: -1; DCCommand: '') //Stop loading on demand fields
    );

//DC commands unmatched for the moment
//------------------------------------
//cm_AddFilenameToCmdLine - Looks like TC can do it with a CTRL+ENTER but no cm_ command for this.
//cm_AddPathAndFilenameToCmdLine - Looks like TC can do it with a CTRL+SHIFT+ENTER but no cm_ command for this.
//cm_ChangeDir - Looks like TC can do it with "CD ..." bit no cm_ command for this.
//cm_ChangeDirToHome - Looks like there is no TC equivalent
//cm_ClearLogFile -
//cm_ClearLogWindow
//cm_ConfigDirHotList
//cm_CopyNoAsk
//cm_DebugShowCommandParameters
//cm_EditNew
//cm_GoToLastFile
//cm_HardLink
//cm_LeftEqualRight
//cm_LoadTabs
//cm_OpenArchive
//cm_OpenBar
//cm_OpenVirtualFileSystemList
//cm_OperationsViewer
//cm_PanelsSplitterPerPos
//cm_QuickFilter
//cm_QuickSearch
//cm_RenameNoAsk
//cm_RenameTab
//cm_RightEqualLeft
//cm_SaveTabs
//cm_SetTabOptionNormal
//cm_SetTabOptionPathLocked
//cm_SetTabOptionPathResets
//cm_SetTabOptionDirsInNewTab
//cm_ShellExecute
//cm_ShowButtonMenu
//cm_ShowCmdLineHistory
//cm_ShowMainMenu
//cm_SortByAttr
//cm_SymLink
//cm_UniversalSingleDirectSort
//cm_ViewHistory
//cm_ViewHistoryNext
//cm_ViewHistoryPrev
//cm_ViewLogFile
//cm_Wipe
//cm_WorkWithDirectoryHotlist

var
  wcFakeWndClass: TWndClassEx;
  //hMainWindow,
  {$IFDEF DEBUG}
  hFakeWindow: HWND;
  {$ENDIF}
  TCIconSize: integer = 32;
  TCNumberOfInstance: integer;
  TCListOfCreatedTCIconFilename: TStringList;

{ WindowProc }
function WindowProc(hWnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  {
    Resend message to DoubleCommander main window.

    Disabled currently, because it may interfere with LCL, especially since the fake
    TotalCmd window is also a main app window (WS_OVERLAPPEDWINDOW). May be enabled
    in future if any plugins need this, but following messages should be skipped
    because they are known to cause conflict:
    - WM_ACTIVATEAPP
      Confuses LCL about which main form (window) is currently active and
      it stops calling OnExit events for controls (see TWinControl.WMKillFocus).
  }
  //SendMessage(hMainWindow, uiMsg, wParam, lParam);

  {$IF (lcl_fullversion >= 1020000)}
  if (uiMsg = WM_DEVICECHANGE) and (wParam = DBT_DEVNODES_CHANGED) and (lParam = 0) then
  begin
    Screen.UpdateMonitors; // Refresh monitor list
    DCDebug('WM_DEVICECHANGE:DBT_DEVNODES_CHANGED');
  end;
  {$ENDIF}

  {$IFDEF DEBUG}
  WriteLn(uiMsg);
  {$ENDIF}
  Result := DefWindowProc(hWnd, uiMsg, wParam, lParam);
end;

{ CreateTotalCommanderWindow }
procedure CreateTotalCommanderWindow(hWindow: HWND);
begin
  //  hMainWindow:= hWindow;
  FillByte(wcFakeWndClass, SizeOf(wcFakeWndClass), 0);
  wcFakeWndClass.cbSize := SizeOf(wcFakeWndClass);
  wcFakeWndClass.Style := CS_HREDRAW or CS_VREDRAW;
  wcFakeWndClass.lpfnWndProc := @WindowProc;
  wcFakeWndClass.hInstance := hInstance;
  wcFakeWndClass.hbrBackground := Color_BtnFace + 12;
  wcFakeWndClass.lpszMenuName := nil;
  wcFakeWndClass.lpszClassName := 'TTOTAL_CMD';
  RegisterClassEx(wcFakeWndClass);
  // Create Total Commander fake window
  {$IFDEF DEBUG}
  hFakeWindow :=
  {$ENDIF}
    CreateWindowEx(0, 'TTOTAL_CMD', 'Double Commander', WS_OVERLAPPEDWINDOW, 100, 100, 300, 300, 0, 0, hInstance, nil);
  {$IFDEF DEBUG}
  // Show window (for debugging only)
  ShowWindow(hFakeWindow, SW_SHOW);
  {$ENDIF}
end;

// Test have been made with string from site http://stackoverflow.com/questions/478201/how-to-test-an-application-for-correct-encoding-e-g-utf-8
// Note: If you ever "think" to change or modify this routine, make sure to test the following:
// 1o) Make a directory with utf-8 special characters, a path like this: "Card-♠♣♥♦"
// 2o) Then, go with TC and add it as a favorite.
// 3o) Then, exit it to make sure it is saved in its wndcmd.ini file
// 4o) Then, go in the hotlist of DC and do an import from TC file
// 5o) Make sure the path you've created has really been imported and it's NOT written "cd Card-♠♣♥♦\" or things like that.
// 6o) Make sure you can also GO TO this folder from the popup menu of hotlist.
// 7o) After that, repeat the step one through six with a path called "français", or "Esta frase está en español" and really take the time to do it.
// 8o) Really take the time to do step 7 with the suggested two folder mentionned here.

// In its "wincmd", TC is using AnsiString for character that don't need UTF-8 string.
// He add the identifier "AnsiChar($EF) + AnsiChar($BB) + AnsiChar($BF)" at the beginning of each value that requires that the following needs to be interpret as UTF8 string.
// So we cannot systematically convert the string. Some are using code between 128 and 255 that needs to be interpert as what it was in ANSI.
// ALSO, lettings the $EF $BB $BF in the "string" make the string to be displayble "normally" in Lazarus, yes...
// ...but when it's time to do things like "pos(...", "copy(...", the $EF $BB $BF are there, taken into acocunt, even when doing a print of the string we don't see see them!

// Anyway. If you ever modify the following thinking it shouldn't be like this or there is a better way or whatever, please, take the time to do the test written after your modifications
function ConvertTCStringToString(TCString: ansistring): string;
begin
  Result := TCString;
  if length(Result) >= 3 then
  begin
    if ((TCString[1] = AnsiChar($EF)) and (TCString[2] = AnsiChar($BB)) and (TCString[3] = AnsiChar($BF))) then
    begin
      Result := copy(Result, 4, (length(Result) - 3));
    end
    else
    begin
      Result := CeAnsiToUtf8(Result);
    end;
  end;
end;

// TC is adding the "$EF $BB $BF" identifier if the string stored in its config file require to be interpret in uniccode.
// Adding it systematically, like we already tried before, doesn't work in 100% of the situation.
// For example, for raison that can't explain without its source code, if a toolbar filename is express with the "$EF $BB $BF" in the name,
// it will "basically work", but if it is defined to be shown as a drop menu, the little down triangle won't be shown in TC!!!
// So let's add the "$EF $BB $BF" only when it required.
function ConvertStringToTCString(sString: string): ansistring;
begin
  if CeUtf8ToAnsi(sString) = sString then
    Result := sString
  else
    Result := AnsiChar($EF) + AnsiChar($BB) + AnsiChar($BF) + sString;
end;

{ ReplaceDCEnvVars }
// Routine to replace %VARIABLE% of DC path by the actual absolute path
// This is useful when we "export" to TC related path to place them in absolute format this way TC refer them correctly after export.
function ReplaceDCEnvVars(const sText: string): string;
begin
  Result := StringReplace(sText, '%DC_CONFIG_PATH%', ExcludeTrailingPathDelimiter(gpCfgDir), [rfIgnoreCase]);
  Result := StringReplace(Result, '%COMMANDER_PATH%', ExcludeTrailingPathDelimiter(ExtractFilePath(gpExePath)), [rfIgnoreCase]);
end;

{ ReplaceTCEnvVars }
// Routine to replace %VARIABLE% of TC path by the actual absolute path
// This is useful when we "import" TC related path to place them in absolute format this way DC refer them correctly after import.
function ReplaceTCEnvVars(const sText: string): string;
begin
  Result := StringReplace(sText, '%COMMANDER_INI%', gTotalCommanderConfigFilename, [rfIgnoreCase]);
  Result := StringReplace(Result, '%COMMANDER_PATH%', ExcludeTrailingPathDelimiter(ExtractFilePath(gTotalCommanderExecutableFilename)), [rfIgnoreCase]);
  Result := StringReplace(Result, '%COMMANDER_EXE%', ExcludeTrailingPathDelimiter(ExtractFilePath(gTotalCommanderExecutableFilename)), [rfIgnoreCase]);
  Result := StringReplace(Result, '%COMMANDER_DRIVE%', ExcludeTrailingPathDelimiter(ExtractFileDrive(gTotalCommanderExecutableFilename)), [rfIgnoreCase]);
  if utf8pos(UTF8UpperCase('wcmicons.dll'), UTF8UpperCase(Result)) = 1 then
    Result := StringReplace(Result, 'wcmicons.dll', ExtractFilePath(gTotalCommanderExecutableFilename) + 'wcmicons.dll', [rfIgnoreCase]);
end;

{ GetTotalCommandeMainBarFilename }
// We'll return the TC main bar filename.
// At the same time, since we're in the config file, we'll determine the icon size for the button bar.
function GetTotalCommandeMainBarFilename: string;
var
  TCMainConfigFile: TIniFileEx;
begin
  Result := '';
  if mbFileExists(gTotalCommanderConfigFilename) then
  begin
    TCMainConfigFile := TIniFileEx.Create(gTotalCommanderConfigFilename);
    try
      Result :=
        ReplaceTCEnvVars(ConvertTCStringToString(TCMainConfigFile.ReadString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_MAINBAR_NOTPRESENT)));

      //While we're there, we'll get the button height.
      TCIconSize := TCMainConfigFile.ReadInteger(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTONHEIGHT, 32 + 5);
      TCIconSize := TCIconSize - 5; //Yeah... A magic -5...
    finally
      TCMainConfigFile.Free;
    end;

    //If the toolbar section is not present, TC attempts to load "DEFAULT.bar" from application path.
    //Let's see if there is such file there.
    if Result = TCCONFIG_MAINBAR_NOTPRESENT then
    begin
      if mbFileExists(IncludeTrailingPathDelimiter(ExtractFilePath(gTotalCommanderExecutableFilename)) + 'DEFAULT.bar') then
        Result := IncludeTrailingPathDelimiter(ExtractFilePath(gTotalCommanderExecutableFilename)) + 'DEFAULT.bar';
    end;
  end;
end;

{ EnumTaskWindowsProc }
// Routine used for the following "IsTotalCommanderSeemsRunning" routine.
function EnumTaskWindowsProc(Wnd: THandle; List: TStrings): boolean; stdcall;
var
  ClassName: PChar;
begin
  ClassName := Stralloc(100);
  GetClassName(Wnd, ClassName, 99);
  if ClassName = 'TTOTAL_CMD' then
    Inc(TCNumberOfInstance);
  Result := True;
  strDispose(ClassName);
end;

{ IsTotalCommanderSeemsRunning }
// Routine that we'll return TRUE if TC is currently running and false otherwise
function IsTotalCommanderSeemsRunning: boolean;
begin
  TCNumberOfInstance := 0;

  try
    EnumWindows(@EnumTaskWindowsProc, 0);
  finally
  end;

  Result := (TCNumberOfInstance > 1);
end;

{ areTCRelatedPathsAndFilesDetected }
//To consider to be cumfortable to work with TC related stuff we need to make sure:
//1o) We know where is the TC executable
//2o) We know where is the TC config file
//3o) We know where where is the file for the TC main toolbar
//4o) We know where the toolbar and associted icon COULD be stored (util when exporting the DC toolbar)
function areTCRelatedPathsAndFilesDetected: boolean;
begin
  Result := False;
  if mbFileExists(gTotalCommanderExecutableFilename) then
  begin
    if mbFileExists(gTotalCommanderConfigFilename) then
    begin
      sTotalCommanderMainbarFilename := GetTotalCommandeMainBarFilename;
      if mbFileExists(sTotalCommanderMainbarFilename) then
      begin
        if mbDirectoryExists(ExcludeTrailingPathDelimiter(gTotalCommanderToolbarPath)) then
        begin
          Result := True;
        end
        else
        begin
          MsgError(Format(rsMsgTCToolbarNotFound, [gTotalCommanderToolbarPath]));
        end;
      end
      else
      begin
        MsgError(Format(rsImportToolbarProblem, [sTotalCommanderMainbarFilename]));
      end;
    end
    else
    begin
      MsgError(Format(rsMsgTCConfigNotFound, [gTotalCommanderConfigFilename]));
    end;
  end
  else
  begin
    MsgError(Format(rsMsgTCExecutableNotFound, [gTotalCommanderExecutableFilename]));
  end;

  if not Result then
    BringUsToTCConfigurationPage;
end;

{ areWeInSituationToPlayWithTCFiles }
function areWeInSituationToPlayWithTCFiles: boolean;
var
  FlagCancelWaitingTCClose: TMyMsgResult;
  FlagTCIsRunning: boolean;
begin
  Result := False;

  if areTCRelatedPathsAndFilesDetected then
  begin
    repeat
      FlagTCIsRunning := IsTotalCommanderSeemsRunning;
      if FlagTCIsRunning then
        FlagCancelWaitingTCClose := MsgBox(rsMsgTCisRunning, [msmbOk, msmbCancel], msmbOk, msmbCancel);
    until (FlagTCIsRunning = False) or (FlagCancelWaitingTCClose = mmrCancel);

    Result := not FlagTCIsRunning;
  end;
end;

{ GetTCEquivalentCommandToDCCommand }
// From the given DC command, we'll return the equivalent TC command.
// If not found, we'll return the same DC command, at least.
function GetTCEquivalentCommandToDCCommand(DCCommand: string; var TCIndexOfCommand: integer): string;
var
  SearchingIndex: integer = 1;
begin
  Result := '';
  TCIndexOfCommand := -1;

  if DCCommand <> '' then
  begin
    DCCommand := UTF8LowerCase(DCCommand);

    //Let's see if we have an equivalent TC for our DC command.
    while (SearchingIndex <= NUMBEROFCOMMANDS) and (TCIndexOfCommand = -1) do
    begin
      if DCCommand = UTF8LowerCase(COMMANDS_LIST_TC[SearchingIndex].DCCommand) then
      begin
        Result := COMMANDS_LIST_TC[SearchingIndex].TCCommand;
        TCIndexOfCommand := SearchingIndex;
      end
      else
      begin
        Inc(SearchingIndex);
      end;
    end;

    if TCIndexOfCommand = -1 then
      Result := DCCommand;
  end;
end;

{ GetTCIconFromDCIconAndCreateIfNecessary }
// Will return the string to use for the icon for the tool bar button when doing an export to TC bar file.
// Will also create a .ICO file if we know the fiel can't be load by TC.
// Basically routine generate the same bitmap as what DC would generate to show.
// Then, we look from where it's coming from with "fromWhatItWasLoaded)".
// Depending of this, will simply return the same filename OR will create and icon for TC.
// This has been test with:

// fwbwlNotLoaded: NOT TESTED
// fwbwlIconThemeBitmap: Tested with 'cm_configdirhotlist', 'cm_dirhotlist', 'utilities-terminal', 'cm_markunmarkall', 'go-previous', 'go-next'
// fwbwlResourceFileExtracted: Tested with 'wcmicons.dll,3', 'MyOwnIcons.icl,12', 'doublecmd.exe', 'TOTALCMD64.EXE', 'HWorks32.exe'
// fwbwlGraphicFile: Test with 'UploadDispatcher.ico', 'Carlos.bmp'
// fwbwlGraphicFile switched to fwbwlGraphicFileNotSupportedByTC: Tested with 'Nop.png', 'cm_extractfiles.png', 'cm_about.png', a corrutped .png file
// fwbwlFileIconByExtension: Tested with 'ElementaryOS-32bits.vbox', 'backupsource.bat', 'Microsoft Word 2010.lnk', a corrupted .bmp file since DC at least attenmpt to by the extension which is nice!
// fwbwlFiDefaultIconID: Tested with "a missing unknown extension file", An empty icon string,

function GetTCIconFromDCIconAndCreateIfNecessary(const DCIcon: string): string;
var
  LocalBitmap: Graphics.TBitmap = nil;
  fromWhatItWasLoaded: TfromWhatBitmapWasLoaded;
  LocalIcon: Graphics.TIcon = nil;
  Suffix: string;
  needToBeConvertToIco: boolean = False;
begin
  Result := DCIcon; //In any case, by default at least, return the same thing as what we got in DC and good luck TC!

  //Get the bitmap of the icon and make sure to get "fromWhatItWasLoaded" to see from where it came from
  LocalBitmap := PixmapManager.LoadBitmapEnhanced(DCICon, TCIconSize, True, clBtnFace, @fromWhatItWasLoaded);
  try
    if ExtractFileExt(UTF8Lowercase(DCIcon)) = '.png' then
      fromWhatItWasLoaded := fwbwlGraphicFileNotSupportedByTC;

    case fromWhatItWasLoaded of
      fwbwlNotLoaded: needToBeConvertToIco := False;
      fwbwlIconThemeBitmap: needToBeConvertToIco := True;
      fwbwlResourceFileExtracted: needToBeConvertToIco := False;
      fwbwlGraphicFile: needToBeConvertToIco := False;
      fwbwlGraphicFileNotSupportedByTC: needToBeConvertToIco := True;
      fwbwlFileIconByExtension: needToBeConvertToIco := True;
      fwbwlFiDefaultIconID: needToBeConvertToIco := True;
    end;

    // If TC can't load the file, let's generate a .ICO file for it.
    // We use a .ICO so we can passed at least something with transparency.
    if needToBeConvertToIco then
    begin
      Result := RemoveFileExt(ExtractFilename(DCIcon));

      if Result = '' then
        Result := 'empty';

      Result := gTotalCommanderToolbarPath + PathDelim + Result;

      //Make sure to use a filename not already generated.
      Suffix := '';
      while TCListOfCreatedTCIconFilename.IndexOf(Result + Suffix + '.ico') <> -1 do
        Suffix := IntToStr(StrToIntDef(Suffix, 0) + 1);
      Result := Result + Suffix + '.ico';

      //.ICO conversion.
      LocalIcon := Graphics.TIcon.Create;
      try
        LocalIcon.Assign(LocalBitmap);
        LocalIcon.SaveToFile(Result);
        TCListOfCreatedTCIconFilename.Add(Result);
      finally
        LocalIcon.Free;
      end;
    end;

  finally
    LocalBitmap.Free;
  end;
end;

{ GetTCEquivalentCommandIconToDCCommandIcon }
// Different from the previous "GetTCIconFromDCIconAndCreateIfNecessary" routine because it concerns "commands".
// If TC has an icon in its "wcmicons.dll" file for the command, we'll use it.
// If not, we'll save a .ICO for it, no matter where it is comming from.
function GetTCEquivalentCommandIconToDCCommandIcon(DCIcon: string; TCIndexOfCommand: integer): string;
begin
  Result := '';

  if TCIndexOfCommand <> -1 then
  begin
    if COMMANDS_LIST_TC[TCIndexOfCommand].TCIcon <> -1 then
      Result := 'wcmicons.dll,' + IntToStr(COMMANDS_LIST_TC[TCIndexOfCommand].TCIcon);
  end;

  if Result = '' then
    Result := GetTCIconFromDCIconAndCreateIfNecessary(DCIcon);
end;

{ GetDCEquivalentCommandToTCCommand }
// From the given TC command, we'll return the equivalent DC command.
function GetDCEquivalentCommandToTCCommand(TCCommand: string; var TCIndexOfCommand: integer): string;
begin
  Result := 'nil';
  TCIndexOfCommand := 1;

  if TCCommand <> '' then
  begin
    TCCommand := UTF8LowerCase(TCCommand);

    //Let's see if we have an equivalent DC for the TC command.
    while (TCIndexOfCommand <= NUMBEROFCOMMANDS) and (Result = 'nil') do
    begin
      if TCCommand = UTF8LowerCase(COMMANDS_LIST_TC[TCIndexOfCommand].TCCommand) then
        Result := COMMANDS_LIST_TC[TCIndexOfCommand].DCCommand
      else
        Inc(TCIndexOfCommand);
    end;

  end;

  if (Result = '') or (Result = 'nil') then
  begin
    TCIndexOfCommand := -1;
    Result := TCCommand;
  end;
end;

{ ExportDCToolbarsToTC }
procedure ExportDCToolbarsToTC(Toolbar: TKASToolbar; Barfilename: string; FlushExistingContent, FlagNeedToUpdateConfigIni: boolean);
var
  TargetBarFilenamePrefix: string;
  TCToolBarIndex: integer;
  ExportationDateTime: TDateTime;

  procedure PossiblyRecursiveAddThisToolItemToConfigFile(ToolItem: TKASToolItem; TCBarConfigFile: TIniFileEx; TCIndexButton: integer);
  var
    sTCIndexButton: string;
    TCIndexOfCommand, IndexItem: integer;
    TCCommand, TCIcon: string;
    InnerTCBarConfigFilename: string;
    InnerTCBarConfigFile: TIniFileEx;
  begin
    sTCIndexButton := IntToStr(TCIndexButton);

    if ToolItem is TKASSeparatorItem then
    begin
      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTON_PREFIX + sTCIndexButton, '');
      TCBarConfigFile.WriteInteger(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_ICONIC_PREFIX + sTCIndexButton, 0);
    end;

    if ToolItem is TKASCommandItem then
    begin
      TCCommand := GetTCEquivalentCommandToDCCommand(TKASCommandItem(ToolItem).Command, TCIndexOfCommand);
      TCIcon := GetTCEquivalentCommandIconToDCCommandIcon(TKASCommandItem(ToolItem).Icon, TCIndexOfCommand);
      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_CMD_PREFIX + sTCIndexButton, ConvertStringToTCString(TCCommand));
      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTON_PREFIX + sTCIndexButton, ConvertStringToTCString(TCIcon));
      if (TKASCommandItem(ToolItem).Hint <> '') and (TCIndexOfCommand = -1) then //We'll write the hint *only* if command is not a recognized Total Commander command.
        TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_HINT_PREFIX + sTCIndexButton, ConvertStringToTCString(TKASCommandItem(ToolItem).Hint));
    end;

    if ToolItem is TKASProgramItem then
    begin
      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTON_PREFIX + sTCIndexButton, ConvertStringToTCString(GetTCIconFromDCIconAndCreateIfNecessary(TKASProgramItem(ToolItem).Icon)));
      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_CMD_PREFIX + sTCIndexButton, ConvertStringToTCString(mbExpandFileName(TKASProgramItem(ToolItem).Command)));
      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_STARTINGPATH_PREFIX + sTCIndexButton, ConvertStringToTCString(mbExpandFileName(TKASProgramItem(ToolItem).StartPath)));
      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_HINT_PREFIX + sTCIndexButton, ConvertStringToTCString(TKASProgramItem(ToolItem).Hint));
      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_PARAM_PREFIX + sTCIndexButton, ConvertStringToTCString(TKASProgramItem(ToolItem).Params));
    end;

    if ToolItem is TKASMenuItem then
    begin
      InnerTCBarConfigFilename := TargetBarFilenamePrefix + '_SubBar' + Format('%2.2d', [TCToolBarIndex]) + '_' + GetDateTimeInStrEZSortable(ExportationDateTime) + '.BAR';
      Inc(TCToolBarIndex);

      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_CMD_PREFIX + sTCIndexButton, ConvertStringToTCString(InnerTCBarConfigFilename));
      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTON_PREFIX + sTCIndexButton, ConvertStringToTCString(mbExpandFileName(TKASMenuItem(ToolItem).Icon)));
      TCBarConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_HINT_PREFIX + sTCIndexButton, ConvertStringToTCString(TKASMenuItem(ToolItem).Hint));
      TCBarConfigFile.WriteInteger(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_ICONIC_PREFIX + sTCIndexButton, 1);

      //Now we have to create the TC toolbar file to store the coming subbar.
      InnerTCBarConfigFile := TIniFileEx.Create(InnerTCBarConfigFilename, fmOpenWrite);
      try
        for IndexItem := 0 to pred(TKASMenuItem(ToolItem).SubItems.Count) do
          PossiblyRecursiveAddThisToolItemToConfigFile(TKASMenuItem(ToolItem).SubItems[IndexItem], InnerTCBarConfigFile, (IndexItem + 1));

        //*AFTER* all the buttons have been added, let's update for TC the number of buttons now present.
        InnerTCBarConfigFile.WriteInteger(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTONBAR_COUNT, TKASMenuItem(ToolItem).SubItems.Count);

        InnerTCBarConfigFile.UpdateFile;

      finally
        InnerTCBarConfigFile.Free;
      end;

    end;
  end;

var
  //Placed intentionnally *AFTER* above routine to make sure these variable names are not used in above possibly recursive routines.
  TCMainConfigFile, MainTCBarConfigFile: TIniFileEx;
  IndexButton, TCMainIndexButton: integer;

begin
  ExportationDateTime := now;

  TargetBarFilenamePrefix := IncludeTrailingPathDelimiter(gTotalCommanderToolbarPath) + rsFilenameExportedTCBarPrefix;
  TCToolBarIndex := 1;

  TCListOfCreatedTCIconFilename := TStringList.Create;
  TCListOfCreatedTCIconFilename.Sorted := True;
  TCListOfCreatedTCIconFilename.Clear;
  try
    //Let's create/append the .BAR file(s)!
    MainTCBarConfigFile := TIniFileEx.Create(Barfilename, fmOpenReadWrite);
    try
      if FlushExistingContent then
      begin
        MainTCBarConfigFile.EraseSection(TCCONFIG_BUTTONBAR_SECTION);
        TCMainIndexButton := 0;
      end
      else
      begin
        TCMainIndexButton := MainTCBarConfigFile.ReadInteger(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTONBAR_COUNT, 0);
      end;

      //Let's add the DC toolbar to the TC .BAR file.
      for IndexButton := 0 to pred(Toolbar.ButtonCount) do
      begin
        Inc(TCMainIndexButton);
        PossiblyRecursiveAddThisToolItemToConfigFile(Toolbar.Buttons[IndexButton].ToolItem, MainTCBarConfigFile, TCMainIndexButton);
      end;

      //*AFTER* all the buttons have been added, let's update for TC the number of buttons now present.
      MainTCBarConfigFile.WriteInteger(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTONBAR_COUNT, TCMainIndexButton);

      MainTCBarConfigFile.UpdateFile;

    finally
      MainTCBarConfigFile.Free;
    end;

  finally
    TCListOfCreatedTCIconFilename.Free;
  end;

  //If we've been asked to play in the Wincmd.ini file, let's make sure to save the main bar filename.
  if FlagNeedToUpdateConfigIni then
  begin
    TCMainConfigFile := TIniFileEx.Create(gTotalCommanderConfigFilename, fmOpenReadWrite);
    try
      //2014-11-27:It looks like, will with TC 8.50B12, the main bar file cannot have unicode in the name???
      //It "basically" works but have some annoying problem from here to thre.
      //So intentionnally, we don't use "ConvertStringToTCString(SaveDialog.Filename)"
      TCMainConfigFile.WriteString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTONBAR_SECTION, ansistring(Barfilename));

      TCMainConfigFile.UpdateFile;

    finally
      TCMainConfigFile.Free;
    end;
  end;
end;

{ ImportTCToolbarsToDC }
// Will import the TC toolbar file named "Barfilename" into either:
//   -a TKASToolbar referenced by "Toolbar" (when "WhereToImport"=IMPORT_TO_BAR)
//   -a subtoolbar of a "TKASToolItem" referenced by "UpperToolItem" (when "WhereToImport"=IMPORT_TO_ITEM)
// If the TC toolbar have buttons pointing other TC toolbar file, the routine will import them as well
//   and organize something similar in the tree structure of subtoolbar DC is using.
// Obviously to avoid keeps cycling in round if "Toolbar A points toolbar B and toolbar B points toolbar A",
//   this import routine will not re-importe a toolbar already imported.
procedure ImportTCToolbarsToDC(Barfilename: string; UpperToolItem: TKASToolItem; Toolbar: TKASToolbar; WhereToImport: integer; FCurrentButton: TKASToolButton; FFormCommands: IFormCommands);
const
  IMPORT_TO_BAR = 0;
  IMPORT_TO_ITEM = 1;
var
  TCToolbarFilenameList: TStringList; //To hold the TC toolbarfile already imported to don't re-import more than once a toolbar file already imported.
  TCIndexOfCommand: integer;

  procedure RecursiveIncorporateTCBarfile(Barfilename: string; UpperToolItem: TKASToolItem; Toolbar: TKASToolbar; WhereToImport: integer);
  var
    TCBarConfigFile: TIniFileEx;
    IndexButton: integer;
    sButtonName, sCmdName, sHintName, sParamValue, sStartingPath: string;
    SubToolItem: TKASToolItem = nil;

    procedure AddToolItem;
    begin
      case WhereToImport of
        IMPORT_TO_BAR: FCurrentButton := ToolBar.AddButton(SubToolItem);
        IMPORT_TO_ITEM: TKASMenuItem(UpperToolItem).SubItems.Add(SubToolItem);
      end;
    end;

  begin
    if mbFileExists(Barfilename) then
    begin
      TCBarConfigFile := TIniFileEx.Create(Barfilename);
      try
        IndexButton := 1;
        repeat
          sButtonName := ConvertTCStringToString(TCBarConfigFile.ReadString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTON_PREFIX + IntToStr(IndexButton), TCCONFIG_MAINBAR_NOTPRESENT));

          if sButtonName <> TCCONFIG_MAINBAR_NOTPRESENT then
          begin
            if sButtonName = '' then
            begin
              //We have a separator bar!
              SubToolItem := TKASSeparatorItem.Create;
              AddToolItem;
            end
            else
            begin
              sButtonName := ReplaceTCEnvVars(sButtonName);
              sCmdName := TrimQuotes(ReplaceTCEnvVars(ConvertTCStringToString(TCBarConfigFile.ReadString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_CMD_PREFIX + IntToStr(IndexButton), 'cmd_notimplement'))));
              sParamValue := ReplaceTCEnvVars(ConvertTCStringToString(TCBarConfigFile.ReadString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_PARAM_PREFIX + IntToStr(IndexButton), '')));
              sStartingPath := TrimQuotes(ReplaceTCEnvVars(ConvertTCStringToString(TCBarConfigFile.ReadString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_STARTINGPATH_PREFIX + IntToStr(IndexButton), ''))));
              sHintName := ConvertTCStringToString(TCBarConfigFile.ReadString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_HINT_PREFIX + IntToStr(IndexButton), ''));

              if pos('cm_', UTF8LowerCase(sCmdName)) = 1 then
              begin
                //We have an internal command!
                sCmdName := GetDCEquivalentCommandToTCCommand(sCmdName, TCIndexOfCommand);
                SubToolItem := TKASCommandItem.Create(FFormCommands);
                TKASCommandItem(SubToolItem).Command := sCmdName;
                if sHintName <> '' then
                  TKASCommandItem(SubToolItem).Hint := sHintName
                else
                  TKASCommandItem(SubToolItem).Hint := FFormCommands.GetCommandCaption(sCmdName, cctLong);
                if TCIndexOfCommand = -1 then
                  TKASCommandItem(SubToolItem).Icon := sButtonName
                else
                  TKASCommandItem(SubToolItem).Icon := UTF8LowerCase(TKASCommandItem(SubToolItem).Command);

                AddToolItem;
              end
              else
              begin
                if UTF8UpperCase(ExtractFileExt(sCmdName)) = '.BAR' then
                begin
                  //Since with TC we could have toolbar recursively pointing themselves, we need to make sure we'll not get lost cycling throught the same ones over and over.
                  if TCToolbarFilenameList.IndexOf(UTF8UpperCase(sCmdName)) = -1 then
                  begin
                    //We have a subtoolbar!
                    TCToolbarFilenameList.Add(UTF8UpperCase(sCmdName));
                    SubToolItem := TKASMenuItem.Create;
                    TKASMenuItem(SubToolItem).Icon := sButtonName;
                    if sHintName <> '' then
                      TKASMenuItem(SubToolItem).Hint := sHintName
                    else
                      TKASMenuItem(SubToolItem).Hint := 'Sub menu';
                    AddToolItem;
                    RecursiveIncorporateTCBarfile(sCmdName, SubToolItem, nil, IMPORT_TO_ITEM);
                  end;
                end
                else
                begin
                  //We have a "Program Item"
                  SubToolItem := TKASProgramItem.Create;
                  TKASProgramItem(SubToolItem).Icon := sButtonName;
                  TKASProgramItem(SubToolItem).Command := sCmdName;
                  TKASProgramItem(SubToolItem).Params := sParamValue;
                  TKASProgramItem(SubToolItem).StartPath := sStartingPath;
                  if sHintName <> '' then
                    TKASProgramItem(SubToolItem).Hint := sHintName
                  else
                    TKASProgramItem(SubToolItem).Hint := 'Program';
                  AddToolItem;
                end;
              end;
            end;
          end;
          Inc(IndexButton);
        until sButtonName = TCCONFIG_MAINBAR_NOTPRESENT;
      finally
        TCBarConfigFile.Free;
      end;
    end;
  end;

begin
  TCToolbarFilenameList := TStringList.Create;
  try
    RecursiveIncorporateTCBarfile(Barfilename, UpperToolItem, Toolbar, WhereToImport);
  finally
    TCToolbarFilenameList.Free;
  end;
end;

end.
