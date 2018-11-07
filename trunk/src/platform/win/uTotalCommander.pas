{
    Double Commander
    -------------------------------------------------------------------------
    Creates Total Commander fake window (some plugins don't work without it)

    Copyright (C) 2009-2018 Alexander Koblov (alexx2000@mail.ru)

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
  DCXmlConfig, uFormCommands, KASToolItems, KASToolBar;

const
  TCCONFIG_MAINBAR_NOTPRESENT = ':-<#/?*+*?\#>-:';

  TCCONFIG_BUTTONBAR_SECTION = 'Buttonbar';
  TCCONFIG_BUTTONBAR_COUNT = 'Buttoncount';
  TCCONFIG_DEFAULTBAR_FILENAME = 'DEFAULT.bar';

  TCCONFIG_BUTTONHEIGHT = 'Buttonheight';
  TCCONFIG_BUTTON_PREFIX = 'button';
  TCCONFIG_ICONIC_PREFIX = 'iconic';
  TCCONFIG_CMD_PREFIX = 'cmd';
  TCCONFIG_STARTINGPATH_PREFIX = 'path';
  TCCONFIG_HINT_PREFIX = 'menu';
  TCCONFIG_PARAM_PREFIX = 'param';

var
  sTotalCommanderMainbarFilename: string = TCCONFIG_MAINBAR_NOTPRESENT;

procedure CreateTotalCommanderWindow({%H-}hWindow: HWND);
function ConvertTCStringToString(TCString: ansistring): string;
function ConvertStringToTCString(sString: string): ansistring;
function ReplaceDCEnvVars(const sText: string): string;
function ReplaceTCEnvVars(const sText: string): string;
function areWeInSituationToPlayWithTCFiles: boolean;
function GetTCEquivalentCommandToDCCommand(DCCommand: string; var TCIndexOfCommand: integer): string;
function GetTCIconFromDCIconAndCreateIfNecessary(const DCIcon: string): string;
function GetTCEquivalentCommandIconToDCCommandIcon(DCIcon: string; TCIndexOfCommand: integer): string;
procedure ExportDCToolbarsToTC(Toolbar: TKASToolbar; Barfilename: string; FlushExistingContent, FlagNeedToUpdateConfigIni: boolean);
procedure ConvertTCToolbarToDCXmlConfig(sTCBarFilename: string; ADCXmlConfig:TXmlConfig);

implementation

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, LCLVersion, Forms, JwaDbt, SysUtils, LCLProc, LazUTF8,

  //DC
  fOptionsMisc, uKASToolItemsExtended,
  DCClassesUtf8, DCOSUtils, uDebug, DCStrUtils, uPixMapManager, uShowMsg,
  uDCUtils, uLng, uGlobs, uGlobsPaths, DCConvertEncoding, uMyWindows;

type
  { TTCommandEquivalence }
  TTCommandEquivalence = record
    TCCommand: string;
    TCIcon: longint;
    DCCommand: string;
    DCParameters: string;
  end;

const
  NUMBEROFCOMMANDS = 458;

  //jcf:format=off
  COMMANDS_LIST_TC: array[1..NUMBEROFCOMMANDS] of TTCommandEquivalence =
    (
    (TCCommand: 'cm_SrcComments';               TCIcon: 21; DCCommand: '';                          DCParameters: ''          ), //Source: Show comments
    (TCCommand: 'cm_SrcShort';                  TCIcon: 3;  DCCommand: 'cm_BriefView';              DCParameters: ''          ), //Source: Only file names
    (TCCommand: 'cm_SrcLong';                   TCIcon: 4;  DCCommand: 'cm_ColumnsView';            DCParameters: ''          ), //Source: All file details
    (TCCommand: 'cm_SrcTree';                   TCIcon: 2;  DCCommand: '';                          DCParameters: ''          ), //Source: Directory tree
    (TCCommand: 'cm_SrcQuickview';              TCIcon: 22; DCCommand: 'cm_QuickView';              DCParameters: ''          ), //Source: Quick view panel
    (TCCommand: 'cm_VerticalPanels';            TCIcon: 23; DCCommand: 'cm_HorizontalFilePanels';   DCParameters: ''          ), //File windows above each other
    (TCCommand: 'cm_SrcQuickInternalOnly';      TCIcon: 22; DCCommand: '';                          DCParameters: ''          ), //Source: Quick view, no plugins
    (TCCommand: 'cm_SrcHideQuickview';          TCIcon: 22; DCCommand: '';                          DCParameters: ''          ), //Source: Quick view panel off
    (TCCommand: 'cm_SrcExecs';                  TCIcon: 12; DCCommand: '';                          DCParameters: ''          ), //Source: Only programs
    (TCCommand: 'cm_SrcAllFiles';               TCIcon: 13; DCCommand: '';                          DCParameters: ''          ), //Source: All files
    (TCCommand: 'cm_SrcUserSpec';               TCIcon: 24; DCCommand: '';                          DCParameters: ''          ), //Source: Last selected
    (TCCommand: 'cm_SrcUserDef';                TCIcon: 25; DCCommand: '';                          DCParameters: ''          ), //Source: Select user type
    (TCCommand: 'cm_SrcByName';                 TCIcon: 5;  DCCommand: 'cm_SortByName';             DCParameters: ''          ), //Source: Sort by name
    (TCCommand: 'cm_SrcByExt';                  TCIcon: 6;  DCCommand: 'cm_SortByExt';              DCParameters: ''          ), //Source: Sort by extension
    (TCCommand: 'cm_SrcBySize';                 TCIcon: 8;  DCCommand: 'cm_SortBySize';             DCParameters: ''          ), //Source: Sort by size
    (TCCommand: 'cm_SrcByDateTime';             TCIcon: 7;  DCCommand: 'cm_SortByDate';             DCParameters: ''          ), //Source: Sort by date
    (TCCommand: 'cm_SrcUnsorted';               TCIcon: 9;  DCCommand: '';                          DCParameters: ''          ), //Source: Unsorted
    (TCCommand: 'cm_SrcNegOrder';               TCIcon: 10; DCCommand: 'cm_ReverseOrder';           DCParameters: ''          ), //Source: Reversed order
    (TCCommand: 'cm_SrcOpenDrives';             TCIcon: -1; DCCommand: 'cm_SrcOpenDrives';          DCParameters: ''          ), //Source: Open drive list
    (TCCommand: 'cm_SrcThumbs';                 TCIcon: 26; DCCommand: 'cm_ThumbnailsView';         DCParameters: ''          ), //Source: Thumbnail view
    (TCCommand: 'cm_SrcCustomViewMenu';         TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), //Source: Custom view menu
    (TCCommand: 'cm_SrcPathFocus';              TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Source: Put focus on path
    (TCCommand: 'cm_LeftComments';              TCIcon: 21; DCCommand: '';                          DCParameters: ''          ), //Left: Show comments
    (TCCommand: 'cm_LeftShort';                 TCIcon: 3;  DCCommand: 'cm_LeftBriefView';          DCParameters: ''          ), //Left: Only file names
    (TCCommand: 'cm_LeftLong';                  TCIcon: 4;  DCCommand: 'cm_LeftColumnsView';        DCParameters: ''          ), //Left: All file details
    (TCCommand: 'cm_LeftTree';                  TCIcon: 2;  DCCommand: '';                          DCParameters: ''          ), //Left: Directory tree
    (TCCommand: 'cm_LeftQuickview';             TCIcon: 22; DCCommand: '';                          DCParameters: ''          ), //Left: Quick view panel
    (TCCommand: 'cm_LeftQuickInternalOnly';     TCIcon: 22; DCCommand: '';                          DCParameters: ''          ), //Left: Quick view, no plugins
    (TCCommand: 'cm_LeftHideQuickview';         TCIcon: 22; DCCommand: '';                          DCParameters: ''          ), //Left: Quick view panel off
    (TCCommand: 'cm_LeftExecs';                 TCIcon: 12; DCCommand: '';                          DCParameters: ''          ), //Left: Only programs
    (TCCommand: 'cm_LeftAllFiles';              TCIcon: 13; DCCommand: '';                          DCParameters: ''          ), //Left: All files
    (TCCommand: 'cm_LeftUserSpec';              TCIcon: 24; DCCommand: '';                          DCParameters: ''          ), //Left: Last selected
    (TCCommand: 'cm_LeftUserDef';               TCIcon: 25; DCCommand: '';                          DCParameters: ''          ), //Left: Select user type
    (TCCommand: 'cm_LeftByName';                TCIcon: 5;  DCCommand: 'cm_LeftSortByName';         DCParameters: ''          ), //Left: Sort by name
    (TCCommand: 'cm_LeftByExt';                 TCIcon: 6;  DCCommand: 'cm_LeftSortByExt';          DCParameters: ''          ), //Left: Sort by extension
    (TCCommand: 'cm_LeftBySize';                TCIcon: 8;  DCCommand: 'cm_LeftSortBySize';         DCParameters: ''          ), //Left: Sort by size
    (TCCommand: 'cm_LeftByDateTime';            TCIcon: 7;  DCCommand: 'cm_LeftSortByDate';         DCParameters: ''          ), //Left: Sort by date
    (TCCommand: 'cm_LeftUnsorted';              TCIcon: 9;  DCCommand: '';                          DCParameters: ''          ), //Left: Unsorted
    (TCCommand: 'cm_LeftNegOrder';              TCIcon: 10; DCCommand: 'cm_LeftReverseOrder';       DCParameters: ''          ), //Left: Reversed order
    (TCCommand: 'cm_LeftOpenDrives';            TCIcon: -1; DCCommand: 'cm_LeftOpenDrives';         DCParameters: ''          ), //Left: Open drive list
    (TCCommand: 'cm_LeftPathFocus';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Left: Put focus on path
    (TCCommand: 'cm_LeftDirBranch';             TCIcon: 50; DCCommand: 'cm_LeftFlatView';           DCParameters: ''          ), //Left: Branch view
    (TCCommand: 'cm_LeftDirBranchSel';          TCIcon: 50; DCCommand: '';                          DCParameters: ''          ), //Left: branch view, only selected
    (TCCommand: 'cm_LeftThumbs';                TCIcon: 26; DCCommand: 'cm_LeftThumbView';          DCParameters: ''          ), //Left: Thumbnail view
    (TCCommand: 'cm_LeftCustomViewMenu';        TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), //Left: Custom view menu
    (TCCommand: 'cm_RightComments';             TCIcon: 21; DCCommand: '';                          DCParameters: ''          ), //Right: Show comments
    (TCCommand: 'cm_RightShort';                TCIcon: 3;  DCCommand: 'cm_RightBriefView';         DCParameters: ''          ), //Right: Only file names
    (TCCommand: 'cm_RightLong';                 TCIcon: 4;  DCCommand: 'cm_RightColumnsView';       DCParameters: ''          ), //Right: All file details
    (TCCommand: 'cm_RightTree';                 TCIcon: 2;  DCCommand: '';                          DCParameters: ''          ), //Right: Directory tree
    (TCCommand: 'cm_RightQuickview';            TCIcon: 22; DCCommand: '';                          DCParameters: ''          ), //Right: Quick view panel
    (TCCommand: 'cm_RightQuickInternalOnly';    TCIcon: 22; DCCommand: '';                          DCParameters: ''          ), //Right: Quick view, no plugins
    (TCCommand: 'cm_RightHideQuickview';        TCIcon: 22; DCCommand: '';                          DCParameters: ''          ), //Right: Quick view panel off
    (TCCommand: 'cm_RightExecs';                TCIcon: 12; DCCommand: '';                          DCParameters: ''          ), //Right: Only programs
    (TCCommand: 'cm_RightAllFiles';             TCIcon: 13; DCCommand: '';                          DCParameters: ''          ), //Right: All files
    (TCCommand: 'cm_RightUserSpec';             TCIcon: 24; DCCommand: '';                          DCParameters: ''          ), //Right: Last selected
    (TCCommand: 'cm_RightUserDef';              TCIcon: 25; DCCommand: '';                          DCParameters: ''          ), //Right: Select user type
    (TCCommand: 'cm_RightByName';               TCIcon: 5;  DCCommand: '';                          DCParameters: ''          ), //Right: Sort by name
    (TCCommand: 'cm_RightByExt';                TCIcon: 6;  DCCommand: 'cm_RightSortByName';        DCParameters: ''          ), //Right: Sort by extension
    (TCCommand: 'cm_RightBySize';               TCIcon: 8;  DCCommand: 'cm_RightSortByExt';         DCParameters: ''          ), //Right: Sort by size
    (TCCommand: 'cm_RightByDateTime';           TCIcon: 7;  DCCommand: 'cm_RightSortBySize';        DCParameters: ''          ), //Right: Sort by date
    (TCCommand: 'cm_RightUnsorted';             TCIcon: 9;  DCCommand: 'cm_RightSortByDate';        DCParameters: ''          ), //Right: Unsorted
    (TCCommand: 'cm_RightNegOrder';             TCIcon: 10; DCCommand: 'cm_RightReverseOrder';      DCParameters: ''          ), //Right: Reversed order
    (TCCommand: 'cm_RightOpenDrives';           TCIcon: -1; DCCommand: 'cm_RightOpenDrives';        DCParameters: ''          ), //Right: Open drive list
    (TCCommand: 'cm_RightPathFocus';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Right: Put focus on path
    (TCCommand: 'cm_RightDirBranch';            TCIcon: 50; DCCommand: 'cm_RightFlatView';          DCParameters: ''          ), //Right: branch view
    (TCCommand: 'cm_RightDirBranchSel';         TCIcon: 50; DCCommand: '';                          DCParameters: ''          ), //Right: branch view, only selected
    (TCCommand: 'cm_RightThumbs';               TCIcon: 26; DCCommand: 'cm_RightThumbView';         DCParameters: ''          ), //Right: Thumbnail view
    (TCCommand: 'cm_RightCustomViewMenu';       TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), //Right: Custom view menu
    (TCCommand: 'cm_List';                      TCIcon: 27; DCCommand: 'cm_View';                   DCParameters: ''          ), //View with Lister
    (TCCommand: 'cm_ListInternalOnly';          TCIcon: 27; DCCommand: 'cm_view';                   DCParameters: ''          ), //Lister without plugins/multimedia
    (TCCommand: 'cm_Edit';                      TCIcon: 28; DCCommand: 'cm_Edit';                   DCParameters: ''          ), //Edit (Notepad)
    (TCCommand: 'cm_Copy';                      TCIcon: 62; DCCommand: 'cm_Copy';                   DCParameters: ''          ), //Copy files
    (TCCommand: 'cm_CopySamepanel';             TCIcon: 62; DCCommand: 'cm_CopySamePanel';          DCParameters: ''          ), //Copy within panel
    (TCCommand: 'cm_CopyOtherpanel';            TCIcon: 62; DCCommand: '';                          DCParameters: ''          ), //Copy to other
    (TCCommand: 'cm_RenMov';                    TCIcon: 63; DCCommand: 'cm_Rename';                 DCParameters: ''          ), //Rename/Move files
    (TCCommand: 'cm_MkDir';                     TCIcon: 29; DCCommand: 'cm_MakeDir';                DCParameters: ''          ), //Make directory
    (TCCommand: 'cm_Delete';                    TCIcon: 64; DCCommand: 'cm_Delete';                 DCParameters: ''          ), //Delete files
    (TCCommand: 'cm_TestArchive';               TCIcon: 60; DCCommand: 'cm_TestArchive';            DCParameters: ''          ), //Test selected archives
    (TCCommand: 'cm_PackFiles';                 TCIcon: 30; DCCommand: 'cm_PackFiles';              DCParameters: ''          ), //Pack files
    (TCCommand: 'cm_UnpackFiles';               TCIcon: 31; DCCommand: 'cm_ExtractFiles';           DCParameters: ''          ), //Unpack all
    (TCCommand: 'cm_RenameOnly';                TCIcon: 32; DCCommand: 'cm_RenameOnly';             DCParameters: ''          ), //Rename (Shift+F6)
    (TCCommand: 'cm_RenameSingleFile';          TCIcon: 32; DCCommand: 'cm_RenameOnly';             DCParameters: ''          ), //Rename file under cursor
    (TCCommand: 'cm_MoveOnly';                  TCIcon: 63; DCCommand: '';                          DCParameters: ''          ), //Move (F6)
    (TCCommand: 'cm_Properties';                TCIcon: -1; DCCommand: 'cm_FileProperties';         DCParameters: ''          ), //Properties dialog
    (TCCommand: 'cm_CreateShortcut';            TCIcon: 65; DCCommand: '';                          DCParameters: ''          ), //Create a shortcut
    (TCCommand: 'cm_Return';                    TCIcon: -1; DCCommand: 'cm_Open';                   DCParameters: ''          ), //Simulate: Return pressed
    (TCCommand: 'cm_OpenAsUser';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Open program under cursor as different user
    (TCCommand: 'cm_Split';                     TCIcon: 68; DCCommand: 'cm_FileSpliter';            DCParameters: ''          ), //Split file into pieces
    (TCCommand: 'cm_Combine';                   TCIcon: 69; DCCommand: 'cm_FileLinker';             DCParameters: ''          ), //Combine partial files
    (TCCommand: 'cm_Encode';                    TCIcon: 66; DCCommand: '';                          DCParameters: ''          ), //Encode MIME/UUE/XXE
    (TCCommand: 'cm_Decode';                    TCIcon: 67; DCCommand: '';                          DCParameters: ''          ), //Decode MIME/UUE/XXE/BinHex
    (TCCommand: 'cm_CRCcreate';                 TCIcon: -1; DCCommand: 'cm_CheckSumCalc';           DCParameters: ''          ), //Create CRC checksums
    (TCCommand: 'cm_CRCcheck';                  TCIcon: 61; DCCommand: 'cm_CheckSumVerify';         DCParameters: ''          ), //Verify CRC checksums
    (TCCommand: 'cm_SetAttrib';                 TCIcon: 33; DCCommand: 'cm_SetFileProperties';      DCParameters: ''          ), //Change attributes
    (TCCommand: 'cm_Config';                    TCIcon: 34; DCCommand: 'cm_Options';                DCParameters: ''          ), //Conf: Layout (first page)
    (TCCommand: 'cm_DisplayConfig';             TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Display
    (TCCommand: 'cm_IconConfig';                TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Icons
    (TCCommand: 'cm_FontConfig';                TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Font
    (TCCommand: 'cm_ColorConfig';               TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Colors
    (TCCommand: 'cm_ConfTabChange';             TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Tabstops
    (TCCommand: 'cm_DirTabsConfig';             TCIcon: 34; DCCommand: 'cm_ConfigFolderTabs';       DCParameters: ''          ), //Conf: Directory tabs
    (TCCommand: 'cm_CustomColumnConfig';        TCIcon: 56; DCCommand: '';                          DCParameters: ''          ), //Conf: Custom colums
    (TCCommand: 'cm_CustomColumnDlg';           TCIcon: 56; DCCommand: '';                          DCParameters: ''          ), //Change current custom columns
    (TCCommand: 'cm_LanguageConfig';            TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Language
    (TCCommand: 'cm_Config2';                   TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Operation
    (TCCommand: 'cm_EditConfig';                TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Viewer/Editor
    (TCCommand: 'cm_CopyConfig';                TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Copy/Delete
    (TCCommand: 'cm_RefreshConfig';             TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Refresh file lists
    (TCCommand: 'cm_QuickSearchConfig';         TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Quick Search
    (TCCommand: 'cm_FtpConfig';                 TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //FTP options
    (TCCommand: 'cm_PluginsConfig';             TCIcon: 34; DCCommand: 'cm_ConfigPlugins';          DCParameters: ''          ), //Conf: Plugins
    (TCCommand: 'cm_ThumbnailsConfig';          TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Thumbnails
    (TCCommand: 'cm_LogConfig';                 TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Log file
    (TCCommand: 'cm_IgnoreConfig';              TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Ignore list
    (TCCommand: 'cm_PackerConfig';              TCIcon: 34; DCCommand: 'cm_ConfigArchivers';        DCParameters: ''          ), //Conf: Packer
    (TCCommand: 'cm_ZipPackerConfig';           TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: ZIP packer
    (TCCommand: 'cm_Confirmation';              TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Conf: Misc, Confirmation
    (TCCommand: 'cm_ConfigSavePos';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Conf: Save position
    (TCCommand: 'cm_ButtonConfig';              TCIcon: 14; DCCommand: 'cm_ConfigToolbars';         DCParameters: ''          ), //Conf: Button bar
    (TCCommand: 'cm_ConfigSaveSettings';        TCIcon: -1; DCCommand: 'cm_ConfigSaveSettings';     DCParameters: ''          ), //Save current paths etc.
    (TCCommand: 'cm_ConfigChangeIniFiles';      TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Open ini files in notepad
    (TCCommand: 'cm_ConfigSaveDirHistory';      TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Save directory history
    (TCCommand: 'cm_ChangeStartMenu';           TCIcon: 34; DCCommand: '';                          DCParameters: ''          ), //Change Start menu
    (TCCommand: 'cm_NetConnect';                TCIcon: 53; DCCommand: 'cm_NetworkConnect';         DCParameters: ''          ), //Network connections
    (TCCommand: 'cm_NetDisconnect';             TCIcon: 54; DCCommand: 'cm_NetworkDisconnect';      DCParameters: ''          ), //Disconnect network drives
    (TCCommand: 'cm_NetShareDir';               TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Share directory
    (TCCommand: 'cm_NetUnshareDir';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Unshare directory
    (TCCommand: 'cm_AdministerServer';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Connect to admin share to open \\server\c$ etc.
    (TCCommand: 'cm_ShowFileUser';              TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Which remote user has opened a local file
    (TCCommand: 'cm_GetFileSpace';              TCIcon: -1; DCCommand: 'cm_CalculateSpace';         DCParameters: ''          ), //Calculate space
    (TCCommand: 'cm_VolumeId';                  TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Volume label
    (TCCommand: 'cm_VersionInfo';               TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Version information
    (TCCommand: 'cm_ExecuteDOS';                TCIcon: -1; DCCommand: 'cm_RunTerm';                DCParameters: ''          ), //Open command prompt window
    (TCCommand: 'cm_CompareDirs';               TCIcon: 35; DCCommand: 'cm_CompareDirectories';     DCParameters: ''          ), //Compare dirs
    (TCCommand: 'cm_CompareDirsWithSubdirs';    TCIcon: 35; DCCommand: 'cm_CompareDirectories';     DCParameters: ''          ), //Also mark subdirs not present in other dir
    (TCCommand: 'cm_ContextMenu';               TCIcon: -1; DCCommand: 'cm_ContextMenu';            DCParameters: ''          ), //Show context menu
    (TCCommand: 'cm_ContextMenuInternal';       TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show context menu for internal associations
    (TCCommand: 'cm_ContextMenuInternalCursor'; TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Internal context menu for file under cursor
    (TCCommand: 'cm_ShowRemoteMenu';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Context menu for Media Center remote control Play/Pause
    (TCCommand: 'cm_SyncChangeDir';             TCIcon: 75; DCCommand: '';                          DCParameters: ''          ), //Synchronous directory changing in both windows
    (TCCommand: 'cm_EditComment';               TCIcon: -1; DCCommand: 'cm_EditComment';            DCParameters: ''          ), //Edit file comment
    (TCCommand: 'cm_FocusLeft';                 TCIcon: -1; DCCommand: 'cm_FocusSwap';              DCParameters: 'side=left' ), //Focus on left file list
    (TCCommand: 'cm_FocusRight';                TCIcon: -1; DCCommand: 'cm_FocusSwap';              DCParameters: 'side=right'), //Focus on right file list
    (TCCommand: 'cm_FocusCmdLine';              TCIcon: -1; DCCommand: 'cm_FocusCmdLine';           DCParameters: ''          ), //Focus on command line
    (TCCommand: 'cm_FocusButtonBar';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Focus on button bar
    (TCCommand: 'cm_CountDirContent';           TCIcon: 36; DCCommand: 'cm_CountDirContent';        DCParameters: ''          ), //Calculate space occupied by subdirs in current dir
    (TCCommand: 'cm_UnloadPlugins';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Unload all plugins
    (TCCommand: 'cm_DirMatch';                  TCIcon: 35; DCCommand: '';                          DCParameters: ''          ), //Mark newer
    (TCCommand: 'cm_Exchange';                  TCIcon: 37; DCCommand: 'cm_Exchange';               DCParameters: ''          ), //Swap panels
    (TCCommand: 'cm_MatchSrc';                  TCIcon: 86; DCCommand: 'cm_TargetEqualSource';      DCParameters: ''          ), //target=Source
    (TCCommand: 'cm_ReloadSelThumbs';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Re-load selected thumbnails
    (TCCommand: 'cm_DirectCableConnect';        TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Connect to other PC by cable
    (TCCommand: 'cm_NTinstallDriver';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Install parallel port driver on NT
    (TCCommand: 'cm_NTremoveDriver';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Remove parallel port driver on NT
    (TCCommand: 'cm_PrintDir';                  TCIcon: 38; DCCommand: '';                          DCParameters: ''          ), //Print current directory (with preview)
    (TCCommand: 'cm_PrintDirSub';               TCIcon: 38; DCCommand: '';                          DCParameters: ''          ), //Print dir with subdirs
    (TCCommand: 'cm_PrintFile';                 TCIcon: 38; DCCommand: '';                          DCParameters: ''          ), //Print file
    (TCCommand: 'cm_SpreadSelection';           TCIcon: 39; DCCommand: 'cm_MarkPlus';               DCParameters: ''          ), //Select group
    (TCCommand: 'cm_SelectBoth';                TCIcon: 72; DCCommand: 'cm_MarkPlus';               DCParameters: 'attr='     ), //Select group: files+folders
    (TCCommand: 'cm_SelectFiles';               TCIcon: 70; DCCommand: 'cm_MarkPlus';               DCParameters: 'attr=d-'   ), //Select group: just files
    (TCCommand: 'cm_SelectFolders';             TCIcon: 71; DCCommand: 'cm_MarkPlus';               DCParameters: 'attr=d+'   ), //Select group: just folders
    (TCCommand: 'cm_ShrinkSelection';           TCIcon: 40; DCCommand: 'cm_MarkMinus';              DCParameters: 'attr='     ), //Unselect group
    (TCCommand: 'cm_ClearFiles';                TCIcon: 40; DCCommand: 'cm_MarkMinus';              DCParameters: 'attr=d+'   ), //Unselect group: just files
    (TCCommand: 'cm_ClearFolders';              TCIcon: 40; DCCommand: 'cm_MarkMinus';              DCParameters: 'attr=d-'   ), //Unselect group: just folders
    (TCCommand: 'cm_ClearSelCfg';               TCIcon: 40; DCCommand: 'cm_MarkMinus';              DCParameters: ''          ), //Unselect group (files or both, as configured)
    (TCCommand: 'cm_SelectAll';                 TCIcon: 44; DCCommand: 'cm_MarkMarkAll';            DCParameters: ''          ), //Select all (files or both, as configured)
    (TCCommand: 'cm_SelectAllBoth';             TCIcon: 44; DCCommand: 'cm_MarkMarkAll';            DCParameters: 'attr='     ), //Select both files+folders
    (TCCommand: 'cm_SelectAllFiles';            TCIcon: 44; DCCommand: 'cm_MarkMarkAll';            DCParameters: 'attr=d-'   ), //Select all files
    (TCCommand: 'cm_SelectAllFolders';          TCIcon: 44; DCCommand: 'cm_MarkMarkAll';            DCParameters: 'attr=d+'   ), //Select all folders
    (TCCommand: 'cm_ClearAll';                  TCIcon: -1; DCCommand: 'cm_MarkUnmarkAll';          DCParameters: 'attr='     ), //Unselect all (files+folders)
    (TCCommand: 'cm_ClearAllFiles';             TCIcon: -1; DCCommand: 'cm_MarkUnmarkAll';          DCParameters: 'attr=d-'   ), //Unselect all files
    (TCCommand: 'cm_ClearAllFolders';           TCIcon: -1; DCCommand: 'cm_MarkUnmarkAll';          DCParameters: 'attr=d+'   ), //Unselect all folders
    (TCCommand: 'cm_ClearAllCfg';               TCIcon: -1; DCCommand: 'cm_MarkUnmarkAll';          DCParameters: ''          ), //Unselect all (files or both, as configured)
    (TCCommand: 'cm_ExchangeSelection';         TCIcon: 11; DCCommand: 'cm_MarkInvert';             DCParameters: ''          ), //Invert selection
    (TCCommand: 'cm_ExchangeSelBoth';           TCIcon: 11; DCCommand: 'cm_MarkInvert';             DCParameters: 'attr='     ), //Invert selection (files+folders)
    (TCCommand: 'cm_ExchangeSelFiles';          TCIcon: 11; DCCommand: 'cm_MarkInvert';             DCParameters: 'attr=d-'   ), //Invert selection (files)
    (TCCommand: 'cm_ExchangeSelFolders';        TCIcon: 11; DCCommand: 'cm_MarkInvert';             DCParameters: 'attr=d+'   ), //Invert selection (folders)
    (TCCommand: 'cm_SelectCurrentExtension';    TCIcon: 41; DCCommand: 'cm_MarkCurrentExtension';   DCParameters: ''          ), //Select all files with same ext.
    (TCCommand: 'cm_UnselectCurrentExtension';  TCIcon: -1; DCCommand: 'cm_UnmarkCurrentExtension'; DCParameters: ''          ), //Unselect all files with same ext.
    (TCCommand: 'cm_SelectCurrentName';         TCIcon: -1; DCCommand: 'cm_MarkCurrentName';        DCParameters: ''          ), //Select all files with same name
    (TCCommand: 'cm_UnselectCurrentName';       TCIcon: -1; DCCommand: 'cm_UnmarkCurrentName';      DCParameters: ''          ), //Unselect all files with same name
    (TCCommand: 'cm_SelectCurrentNameExt';      TCIcon: -1; DCCommand: 'cm_MarkCurrentNameExt';     DCParameters: ''          ), //Select all files with same name+ext.
    (TCCommand: 'cm_UnselectCurrentNameExt';    TCIcon: -1; DCCommand: 'cm_UnmarkCurrentNameExt';   DCParameters: ''          ), //Unselect all files with same name+ext.
    (TCCommand: 'cm_SelectCurrentPath';         TCIcon: 72; DCCommand: 'cm_MarkCurrentPath';        DCParameters: ''          ), //Select all in same path (for branch view+search)
    (TCCommand: 'cm_UnselectCurrentPath';       TCIcon: -1; DCCommand: 'cm_UnmarkCurrentPath';      DCParameters: ''          ), //Unselect all in same path
    (TCCommand: 'cm_RestoreSelection';          TCIcon: 42; DCCommand: 'cm_RestoreSelection';       DCParameters: ''          ), //Selection before last operation
    (TCCommand: 'cm_SaveSelection';             TCIcon: 43; DCCommand: 'cm_SaveSelection';          DCParameters: ''          ), //Temporarily save selection
    (TCCommand: 'cm_SaveSelectionToFile';       TCIcon: -1; DCCommand: 'cm_SaveSelectionToFile';    DCParameters: ''          ), //Save file selection to file
    (TCCommand: 'cm_SaveSelectionToFileA';      TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Save file selection to file (ANSI)
    (TCCommand: 'cm_SaveSelectionToFileW';      TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Save file selection to file (Unicode)
    (TCCommand: 'cm_SaveDetailsToFile';         TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Save all shown columns to file
    (TCCommand: 'cm_SaveDetailsToFileA';        TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Save all shown columns to file (ANSI)
    (TCCommand: 'cm_SaveDetailsToFileW';        TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Save all shown columns to file (Unicode)
    (TCCommand: 'cm_LoadSelectionFromFile';     TCIcon: -1; DCCommand: 'cm_LoadSelectionFromFile';  DCParameters: ''          ), //Read file selection from file
    (TCCommand: 'cm_LoadSelectionFromClip';     TCIcon: -1; DCCommand: 'cm_LoadSelectionFromClip';  DCParameters: ''          ), //Read file selection from clipboard
    (TCCommand: 'cm_EditPermissionInfo';        TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Permissions dialog (NTFS)
    (TCCommand: 'cm_EditPersmissionInfo';       TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Typo...
    (TCCommand: 'cm_EditAuditInfo';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //File auditing (NTFS)
    (TCCommand: 'cm_EditOwnerInfo';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Take ownership (NTFS)
    (TCCommand: 'cm_CutToClipboard';            TCIcon: -1; DCCommand: 'cm_CutToClipboard';         DCParameters: ''          ), //Cut selected files to clipboard
    (TCCommand: 'cm_CopyToClipboard';           TCIcon: -1; DCCommand: 'cm_CopyToClipboard';        DCParameters: ''          ), //Copy selected files to clipboard
    (TCCommand: 'cm_PasteFromClipboard';        TCIcon: -1; DCCommand: 'cm_PasteFromClipboard';     DCParameters: ''          ), //Paste from clipboard to current dir
    (TCCommand: 'cm_CopyNamesToClip';           TCIcon: 45; DCCommand: 'cm_CopyNamesToClip';        DCParameters: ''          ), //Copy filenames to clipboard
    (TCCommand: 'cm_CopyFullNamesToClip';       TCIcon: 45; DCCommand: 'cm_CopyFullNamesToClip';    DCParameters: ''          ), //Copy names with full path
    (TCCommand: 'cm_CopyNetNamesToClip';        TCIcon: 45; DCCommand: '';                          DCParameters: ''          ), //Copy names with UNC path
    (TCCommand: 'cm_CopySrcPathToClip';         TCIcon: 45; DCCommand: '';                          DCParameters: ''          ), //Copy source path to clipboard
    (TCCommand: 'cm_CopyTrgPathToClip';         TCIcon: 45; DCCommand: '';                          DCParameters: ''          ), //Copy target path to clipboard
    (TCCommand: 'cm_CopyFileDetailsToClip';     TCIcon: 59; DCCommand: 'cm_CopyFileDetailsToClip';  DCParameters: ''          ), //Copy all shown columns
    (TCCommand: 'cm_CopyFpFileDetailsToClip';   TCIcon: 59; DCCommand: '';                          DCParameters: ''          ), //Copy all columns, with full path
    (TCCommand: 'cm_CopyNetFileDetailsToClip';  TCIcon: 59; DCCommand: '';                          DCParameters: ''          ), //Copy all columns, with UNC path
    (TCCommand: 'cm_FtpConnect';                TCIcon: 16; DCCommand: '';                          DCParameters: ''          ), //Connect to FTP
    (TCCommand: 'cm_FtpNew';                    TCIcon: 17; DCCommand: '';                          DCParameters: ''          ), //New FTP connection
    (TCCommand: 'cm_FtpDisconnect';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Disconnect from FTP
    (TCCommand: 'cm_FtpHiddenFiles';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show hidden FTP files
    (TCCommand: 'cm_FtpAbort';                  TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Abort current FTP command
    (TCCommand: 'cm_FtpResumeDownload';         TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Resume aborted download
    (TCCommand: 'cm_FtpSelectTransferMode';     TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Select Binary, ASCII or Auto mode
    (TCCommand: 'cm_FtpAddToList';              TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Add selected files to download list
    (TCCommand: 'cm_FtpDownloadList';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Download files in download list
    (TCCommand: 'cm_GotoPreviousDir';           TCIcon: 18; DCCommand: '';                          DCParameters: ''          ), //Go back
    (TCCommand: 'cm_GotoNextDir';               TCIcon: 19; DCCommand: '';                          DCParameters: ''          ), //Go forward
    (TCCommand: 'cm_DirectoryHistory';          TCIcon: -1; DCCommand: 'cm_DirHistory';             DCParameters: ''          ), //History list
    (TCCommand: 'cm_GotoPreviousLocalDir';      TCIcon: 18; DCCommand: '';                          DCParameters: ''          ), //Go back, no ftp
    (TCCommand: 'cm_GotoNextLocalDir';          TCIcon: 19; DCCommand: '';                          DCParameters: ''          ), //Go forward, no ftp
    (TCCommand: 'cm_DirectoryHotlist';          TCIcon: -1; DCCommand: 'cm_DirHotList';             DCParameters: ''          ), //Directory popup menu
    (TCCommand: 'cm_GoToRoot';                  TCIcon: -1; DCCommand: 'cm_ChangeDirToRoot';        DCParameters: ''          ), //Go to root directory
    (TCCommand: 'cm_GoToParent';                TCIcon: 15; DCCommand: 'cm_ChangeDirToParent';      DCParameters: ''          ), //Go to parent directory
    (TCCommand: 'cm_GoToDir';                   TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Open dir or zip under cursor
    (TCCommand: 'cm_OpenDesktop';               TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Desktop folder
    (TCCommand: 'cm_OpenDrives';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //My computer
    (TCCommand: 'cm_OpenControls';              TCIcon: 20; DCCommand: '';                          DCParameters: ''          ), //Control panel
    (TCCommand: 'cm_OpenFonts';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Fonts folder
    (TCCommand: 'cm_OpenNetwork';               TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Network neighborhood
    (TCCommand: 'cm_OpenPrinters';              TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Printers folder
    (TCCommand: 'cm_OpenRecycled';              TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Recycle bin
    (TCCommand: 'cm_CDtree';                    TCIcon: 1;  DCCommand: '';                          DCParameters: ''          ), //Popup directory tree
    (TCCommand: 'cm_TransferLeft';              TCIcon: -1; DCCommand: 'cm_TransferLeft';           DCParameters: ''          ), //Transfer dir under cursor to left window
    (TCCommand: 'cm_TransferRight';             TCIcon: -1; DCCommand: 'cm_TransferRight';          DCParameters: ''          ), //Transfer dir under cursor to right window
    (TCCommand: 'cm_EditPath';                  TCIcon: -1; DCCommand: 'cm_EditPath';               DCParameters: ''          ), //Edit path field above file list
    (TCCommand: 'cm_GoToFirstFile';             TCIcon: -1; DCCommand: 'cm_GoToFirstFile';          DCParameters: ''          ), //Place cursor on first file in list
    (TCCommand: 'cm_GotoNextDrive';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Go one drive up (C->D)
    (TCCommand: 'cm_GotoPreviousDrive';         TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Go one drive down
    (TCCommand: 'cm_GotoNextSelected';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Go to next selected file
    (TCCommand: 'cm_GotoPrevSelected';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Go to previous selected file
    (TCCommand: 'cm_GotoDriveA';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Switch to drive A
    (TCCommand: 'cm_GotoDriveC';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Switch to drive C
    (TCCommand: 'cm_GotoDriveD';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Switch to drive D
    (TCCommand: 'cm_GotoDriveE';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Switch to drive E
    (TCCommand: 'cm_GotoDriveF';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //(etc, define your own if)
    (TCCommand: 'cm_GotoDriveZ';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //(you need more drives)
    (TCCommand: 'cm_HelpIndex';                 TCIcon: 55; DCCommand: 'cm_HelpIndex';              DCParameters: ''          ), //Help index
    (TCCommand: 'cm_Keyboard';                  TCIcon: -1; DCCommand: 'cm_Keyboard';               DCParameters: ''          ), //Keyboard help
    (TCCommand: 'cm_Register';                  TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Registration info
    (TCCommand: 'cm_VisitHomepage';             TCIcon: -1; DCCommand: 'cm_VisitHomePage';          DCParameters: ''          ), //Visit http://www.ghisler.com/
    (TCCommand: 'cm_About';                     TCIcon: -1; DCCommand: 'cm_About';                  DCParameters: ''          ), //Help/About Total Commander
    (TCCommand: 'cm_Exit';                      TCIcon: -1; DCCommand: 'cm_Exit';                   DCParameters: ''          ), //Exit Total Commander
    (TCCommand: 'cm_Minimize';                  TCIcon: -1; DCCommand: 'cm_Minimize';               DCParameters: ''          ), //Minimize Total Commander
    (TCCommand: 'cm_Maximize';                  TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Maximize Total Commander
    (TCCommand: 'cm_Restore';                   TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Restore normal size
    (TCCommand: 'cm_ClearCmdLine';              TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Clear command line
    (TCCommand: 'cm_NextCommand';               TCIcon: -1; DCCommand: 'cm_CmdLineNext';            DCParameters: ''          ), //Next command line
    (TCCommand: 'cm_PrevCommand';               TCIcon: -1; DCCommand: 'cm_CmdLinePrev';            DCParameters: ''          ), //Previous command line
    (TCCommand: 'cm_AddPathToCmdline';          TCIcon: -1; DCCommand: 'cm_AddPathToCmdLine';       DCParameters: ''          ), //Copy path to command line
    (TCCommand: 'cm_MultiRenameFiles';          TCIcon: 46; DCCommand: 'cm_MultiRename';            DCParameters: ''          ), //Rename multiple files
    (TCCommand: 'cm_SysInfo';                   TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //System information
    (TCCommand: 'cm_OpenTransferManager';       TCIcon: 74; DCCommand: '';                          DCParameters: ''          ), //Background transfer manager
    (TCCommand: 'cm_SearchFor';                 TCIcon: 47; DCCommand: 'cm_Search';                 DCParameters: ''          ), //Search for
    (TCCommand: 'cm_SearchStandalone';          TCIcon: 47; DCCommand: '';                          DCParameters: ''          ), //Search in separate process
    (TCCommand: 'cm_FileSync';                  TCIcon: 48; DCCommand: 'cm_SyncDirs';               DCParameters: ''          ), //Synchronize directories
    (TCCommand: 'cm_Associate';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Associate
    (TCCommand: 'cm_InternalAssociate';         TCIcon: -1; DCCommand: 'cm_FileAssoc';              DCParameters: ''          ), //Define internal associations
    (TCCommand: 'cm_CompareFilesByContent';     TCIcon: 49; DCCommand: 'cm_CompareContents';        DCParameters: ''          ), //File comparison
    (TCCommand: 'cm_IntCompareFilesByContent';  TCIcon: 49; DCCommand: 'cm_CompareContents';        DCParameters: ''          ), //Use internal compare tool
    (TCCommand: 'cm_CommandBrowser';            TCIcon: 82; DCCommand: '';                          DCParameters: ''          ), //Browse internal commands
    (TCCommand: 'cm_VisButtonbar';              TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide button bar
    (TCCommand: 'cm_VisDriveButtons';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide drive button bars
    (TCCommand: 'cm_VisTwoDriveButtons';        TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide two drive bars
    (TCCommand: 'cm_VisFlatDriveButtons';       TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Buttons: Flat/normal mode
    (TCCommand: 'cm_VisFlatInterface';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Interface: Flat/normal mode
    (TCCommand: 'cm_VisDriveCombo';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide drive combobox
    (TCCommand: 'cm_VisCurDir';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide current directory
    (TCCommand: 'cm_VisBreadCrumbs';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide Breadcrumb bar
    (TCCommand: 'cm_VisTabHeader';              TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide tab header (sorting)
    (TCCommand: 'cm_VisStatusbar';              TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide status bar
    (TCCommand: 'cm_VisCmdLine';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide Command line
    (TCCommand: 'cm_VisKeyButtons';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide function key buttons
    (TCCommand: 'cm_ShowHint';                  TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show file tip window
    (TCCommand: 'cm_ShowQuickSearch';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show name search window
    (TCCommand: 'cm_SwitchLongNames';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Turn long names on and off
    (TCCommand: 'cm_RereadSource';              TCIcon: 0;  DCCommand: 'cm_Refresh';                DCParameters: ''          ), //Reread source
    (TCCommand: 'cm_ShowOnlySelected';          TCIcon: 73; DCCommand: '';                          DCParameters: ''          ), //Hide files which aren't selected
    (TCCommand: 'cm_SwitchHidSys';              TCIcon: 79; DCCommand: '';                          DCParameters: ''          ), //Turn hidden/system files on and off
    (TCCommand: 'cm_SwitchHid';                 TCIcon: 79; DCCommand: '';                          DCParameters: ''          ), //Turn hidden files on and off
    (TCCommand: 'cm_SwitchSys';                 TCIcon: 79; DCCommand: 'cm_ShowSysFiles';           DCParameters: ''          ), //Turn system files on and off
    (TCCommand: 'cm_Switch83Names';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Turn 8.3 names lowercase on/off
    (TCCommand: 'cm_SwitchDirSort';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Turn directory sorting by name on/off
    (TCCommand: 'cm_DirBranch';                 TCIcon: 50; DCCommand: 'cm_FlatView';               DCParameters: ''          ), //Show all files in current dir and all subdirs
    (TCCommand: 'cm_DirBranchSel';              TCIcon: 50; DCCommand: '';                          DCParameters: ''          ), //Show selected files, and all in selected subdirs
    (TCCommand: 'cm_50Percent';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Window separator at 50%
    (TCCommand: 'cm_100Percent';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Window separator at 100%
    (TCCommand: 'cm_VisDirTabs';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide folder tabs
    (TCCommand: 'cm_VisXPThemeBackground';      TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide XP theme background
    (TCCommand: 'cm_SwitchOverlayIcons';        TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Switch icon overlays on/off
    (TCCommand: 'cm_VisHistHotButtons';         TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show/hide dir history+hotlist
    (TCCommand: 'cm_SwitchWatchDirs';           TCIcon: 80; DCCommand: '';                          DCParameters: ''          ), //Enable/disable WatchDirs auto-refresh temporarily
    (TCCommand: 'cm_SwitchIgnoreList';          TCIcon: 81; DCCommand: 'cm_SwitchIgnoreList';       DCParameters: ''          ), //Enable/disable ignore list file to not show file names
    (TCCommand: 'cm_SwitchX64Redirection';      TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //64-bit Windows: Redirect 32-bit system32 dir off/on
    (TCCommand: 'cm_SeparateTreeOff';           TCIcon: 76; DCCommand: '';                          DCParameters: ''          ), //Disable separate tree panel
    (TCCommand: 'cm_SeparateTree1';             TCIcon: 77; DCCommand: '';                          DCParameters: ''          ), //One separate tree panel
    (TCCommand: 'cm_SeparateTree2';             TCIcon: 78; DCCommand: '';                          DCParameters: ''          ), //Two separate tree panels
    (TCCommand: 'cm_SwitchSeparateTree';        TCIcon: 51; DCCommand: '';                          DCParameters: ''          ), //Switch through tree panel options
    (TCCommand: 'cm_ToggleSeparateTree1';       TCIcon: 77; DCCommand: '';                          DCParameters: ''          ), //One separate tree panel on/off
    (TCCommand: 'cm_ToggleSeparateTree2';       TCIcon: 78; DCCommand: '';                          DCParameters: ''          ), //Two separate tree panels on/off
    (TCCommand: 'cm_UserMenu1';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Start first menu item in Start menu
    (TCCommand: 'cm_UserMenu2';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Second item
    (TCCommand: 'cm_UserMenu3';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Third item
    (TCCommand: 'cm_UserMenu4';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //etc.
    (TCCommand: 'cm_UserMenu5';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_UserMenu6';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_UserMenu7';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_UserMenu8';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //You can add more
    (TCCommand: 'cm_UserMenu9';                 TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //custom user menu ids
    (TCCommand: 'cm_UserMenu10';                TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //in totalcmd.inc!
    (TCCommand: 'cm_OpenNewTab';                TCIcon: 83; DCCommand: 'cm_NewTab';                 DCParameters: ''          ), //Open new tab
    (TCCommand: 'cm_OpenNewTabBg';              TCIcon: 83; DCCommand: '';                          DCParameters: ''          ), //Open new tab in background
    (TCCommand: 'cm_OpenDirInNewTab';           TCIcon: -1; DCCommand: 'cm_OpenDirInNewTab';        DCParameters: ''          ), //Open dir under cursor in tab
    (TCCommand: 'cm_OpenDirInNewTabOther';      TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Open dir under cursor (other window)
    (TCCommand: 'cm_SwitchToNextTab';           TCIcon: -1; DCCommand: 'cm_NextTab';                DCParameters: ''          ), //Switch to next Tab (as Ctrl+Tab)
    (TCCommand: 'cm_SwitchToPreviousTab';       TCIcon: -1; DCCommand: 'cm_PrevTab';                DCParameters: ''          ), //Switch to previous Tab (Ctrl+Shift+Tab)
    (TCCommand: 'cm_CloseCurrentTab';           TCIcon: 84; DCCommand: 'cm_CloseTab';               DCParameters: ''          ), //Close tab
    (TCCommand: 'cm_CloseAllTabs';              TCIcon: 85; DCCommand: 'cm_CloseAllTabs';           DCParameters: ''          ), //Close all
    (TCCommand: 'cm_DirTabsShowMenu';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Show tab menu
    (TCCommand: 'cm_ToggleLockCurrentTab';      TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Turn on/off tab locking
    (TCCommand: 'cm_ToggleLockDcaCurrentTab';   TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Same but with dir changes allowed
    (TCCommand: 'cm_ExchangeWithTabs';          TCIcon: 37; DCCommand: '';                          DCParameters: ''          ), //Swap all Tabs
    (TCCommand: 'cm_GoToLockedDir';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Go to the base dir of locked tab
    (TCCommand: 'cm_SrcActivateTab1';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'index=1'   ), //Activate first tab
    (TCCommand: 'cm_SrcActivateTab2';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'index=2'   ), //Activate second tab
    (TCCommand: 'cm_SrcActivateTab3';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'index=3'   ), //(Source window)
    (TCCommand: 'cm_SrcActivateTab4';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'index=4'   ), //etc.
    (TCCommand: 'cm_SrcActivateTab5';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'index=5'   ),
    (TCCommand: 'cm_SrcActivateTab6';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'index=6'   ),
    (TCCommand: 'cm_SrcActivateTab7';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'index=7'   ),
    (TCCommand: 'cm_SrcActivateTab8';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'index=8'   ),
    (TCCommand: 'cm_SrcActivateTab9';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'index=9'   ),
    (TCCommand: 'cm_SrcActivateTab10';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'index=10'  ), //(up to 99 items)
    (TCCommand: 'cm_TrgActivateTab1';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=inactive;index=1'  ), //Activate first tab
    (TCCommand: 'cm_TrgActivateTab2';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=inactive;index=2'  ), //Activate second tab
    (TCCommand: 'cm_TrgActivateTab3';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=inactive;index=3'  ), //(Target window)
    (TCCommand: 'cm_TrgActivateTab4';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=inactive;index=4'  ), //etc.
    (TCCommand: 'cm_TrgActivateTab5';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=inactive;index=5'  ),
    (TCCommand: 'cm_TrgActivateTab6';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=inactive;index=6'  ),
    (TCCommand: 'cm_TrgActivateTab7';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=inactive;index=7'  ),
    (TCCommand: 'cm_TrgActivateTab8';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=inactive;index=8'  ),
    (TCCommand: 'cm_TrgActivateTab9';           TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=inactive;index=9'  ),
    (TCCommand: 'cm_TrgActivateTab10';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=inactive;index=10' ),
    (TCCommand: 'cm_LeftActivateTab1';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=left;index=1'      ), //Activate first tab
    (TCCommand: 'cm_LeftActivateTab2';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=left;index=2'      ), //Activate second tab
    (TCCommand: 'cm_LeftActivateTab3';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=left;index=3'      ), //(Left window)
    (TCCommand: 'cm_LeftActivateTab4';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=left;index=4'      ), //etc.
    (TCCommand: 'cm_LeftActivateTab5';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=left;index=5'      ),
    (TCCommand: 'cm_LeftActivateTab6';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=left;index=6'      ),
    (TCCommand: 'cm_LeftActivateTab7';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=left;index=7'      ),
    (TCCommand: 'cm_LeftActivateTab8';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=left;index=8'      ),
    (TCCommand: 'cm_LeftActivateTab9';          TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=left;index=9'      ),
    (TCCommand: 'cm_LeftActivateTab10';         TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=left;index=10'     ),
    (TCCommand: 'cm_RightActivateTab1';         TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=right;index=1'     ), //Activate first tab
    (TCCommand: 'cm_RightActivateTab2';         TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=right;index=2'     ), //Activate second tab
    (TCCommand: 'cm_RightActivateTab3';         TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=right;index=3'     ), //(Right window)
    (TCCommand: 'cm_RightActivateTab4';         TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=right;index=4'     ), //etc.
    (TCCommand: 'cm_RightActivateTab5';         TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=right;index=5'     ),
    (TCCommand: 'cm_RightActivateTab6';         TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=right;index=6'     ),
    (TCCommand: 'cm_RightActivateTab7';         TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=right;index=7'     ),
    (TCCommand: 'cm_RightActivateTab8';         TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=right;index=8'     ),
    (TCCommand: 'cm_RightActivateTab9';         TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=right;index=9'     ),
    (TCCommand: 'cm_RightActivateTab10';        TCIcon: -1; DCCommand: 'cm_ActivateTabByIndex';     DCParameters: 'side=right;index=10'    ),
    (TCCommand: 'cm_SrcSortByCol1';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Sort by first column
    (TCCommand: 'cm_SrcSortByCol2';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Sort by second column
    (TCCommand: 'cm_SrcSortByCol3';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // (source window)
    (TCCommand: 'cm_SrcSortByCol4';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // etc.
    (TCCommand: 'cm_SrcSortByCol5';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcSortByCol6';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcSortByCol7';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcSortByCol8';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcSortByCol9';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcSortByCol10';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcSortByCol99';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_TrgSortByCol1';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Sort by first column
    (TCCommand: 'cm_TrgSortByCol2';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Sort by second column
    (TCCommand: 'cm_TrgSortByCol3';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // (target window)
    (TCCommand: 'cm_TrgSortByCol4';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // etc.
    (TCCommand: 'cm_TrgSortByCol5';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_TrgSortByCol6';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_TrgSortByCol7';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_TrgSortByCol8';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_TrgSortByCol9';             TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_TrgSortByCol10';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_TrgSortByCol99';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftSortByCol1';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Sort by first column
    (TCCommand: 'cm_LeftSortByCol2';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Sort by second column
    (TCCommand: 'cm_LeftSortByCol3';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // (left window)
    (TCCommand: 'cm_LeftSortByCol4';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // etc.
    (TCCommand: 'cm_LeftSortByCol5';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftSortByCol6';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftSortByCol7';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftSortByCol8';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftSortByCol9';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftSortByCol10';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftSortByCol99';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightSortByCol1';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Sort by first column
    (TCCommand: 'cm_RightSortByCol2';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Sort by second column
    (TCCommand: 'cm_RightSortByCol3';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // (right window)
    (TCCommand: 'cm_RightSortByCol4';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // etc.
    (TCCommand: 'cm_RightSortByCol5';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightSortByCol6';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightSortByCol7';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightSortByCol8';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightSortByCol9';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightSortByCol10';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightSortByCol99';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcCustomView1';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Source: Custom columns 1
    (TCCommand: 'cm_SrcCustomView2';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // (user defined columns)
    (TCCommand: 'cm_SrcCustomView3';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // etc.
    (TCCommand: 'cm_SrcCustomView4';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcCustomView5';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcCustomView6';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcCustomView7';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcCustomView8';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcCustomView9';            TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // etc. until 299
    (TCCommand: 'cm_LeftCustomView1';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Left: Custom columns 1
    (TCCommand: 'cm_LeftCustomView2';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // (user defined columns)
    (TCCommand: 'cm_LeftCustomView3';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // etc.
    (TCCommand: 'cm_LeftCustomView4';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftCustomView5';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftCustomView6';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftCustomView7';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftCustomView8';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_LeftCustomView9';           TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightCustomView1';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // Right: Custom columns 1
    (TCCommand: 'cm_RightCustomView2';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // (user defined columns)
    (TCCommand: 'cm_RightCustomView3';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), // etc.
    (TCCommand: 'cm_RightCustomView4';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightCustomView5';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightCustomView6';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightCustomView7';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightCustomView8';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_RightCustomView9';          TCIcon: -1; DCCommand: '';                          DCParameters: ''          ),
    (TCCommand: 'cm_SrcNextCustomView';         TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), //  Source: Next custom view
    (TCCommand: 'cm_SrcPrevCustomView';         TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), //  Source: Previous custom view
    (TCCommand: 'cm_TrgNextCustomView';         TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), //  Target: Next custom view
    (TCCommand: 'cm_TrgPrevCustomView';         TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), //  Target: Previous custom view
    (TCCommand: 'cm_LeftNextCustomView';        TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), // Left:   Next custom view
    (TCCommand: 'cm_LeftPrevCustomView';        TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), // Left:   Previous custom view
    (TCCommand: 'cm_RightNextCustomView';       TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), //Right:  Next custom view
    (TCCommand: 'cm_RightPrevCustomView';       TCIcon: 52; DCCommand: '';                          DCParameters: ''          ), //Right:  Previous custom view
    (TCCommand: 'cm_LoadAllOnDemandFields';     TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Load on demand fields for all files
    (TCCommand: 'cm_LoadSelOnDemandFields';     TCIcon: -1; DCCommand: '';                          DCParameters: ''          ), //Load on demand fields for selected files
    (TCCommand: 'cm_ContentStopLoadFields';     TCIcon: -1; DCCommand: '';                          DCParameters: ''          )  //Stop loading on demand fields
    );
//jcf:format=on

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

procedure UpdateEnvironment;
var
  dwSize: DWORD;
  ASysPath: UnicodeString;
  AUserPath: UnicodeString;
  APath: UnicodeString = '';
begin
  // System environment
  if RegReadKey(HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Control\Session Manager\Environment', 'Path', ASysPath) then
  begin
    APath := ASysPath;
    if (Length(APath) > 0) and (APath[Length(APath)] <> PathSeparator) then APath += PathSeparator;
  end;
  // User environment
  if RegReadKey(HKEY_CURRENT_USER, 'Environment', 'Path', AUserPath) then
  begin
    APath := APath + AUserPath;
    if (Length(APath) > 0) and (APath[Length(APath)] <> PathSeparator) then APath += PathSeparator;
  end;
  // Update path environment variable
  if Length(APath) > 0 then
  begin
    SetLength(ASysPath, MaxSmallInt + 1);
    dwSize:= ExpandEnvironmentStringsW(PWideChar(APath), PWideChar(ASysPath), MaxSmallInt);
    if (dwSize = 0) or (dwSize > MaxSmallInt) then
      ASysPath:= APath
    else begin
      SetLength(ASysPath, dwSize - 1);
    end;
    SetEnvironmentVariableW('Path', PWideChar(ASysPath));
  end;
end;

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

  if (uiMsg = WM_SETTINGCHANGE) and (lParam <> 0) and (StrComp('Environment', {%H-}PAnsiChar(lParam)) = 0) then
  begin
    UpdateEnvironment;
    DCDebug('WM_SETTINGCHANGE:Environment');
  end;

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
var
  sAbsoluteTotalCommanderExecutableFilename, sAbsoluteTotalCommanderConfigFilename: string;
begin
  sAbsoluteTotalCommanderExecutableFilename := mbExpandFileName(gTotalCommanderExecutableFilename);
  sAbsoluteTotalCommanderConfigFilename := mbExpandFileName(gTotalCommanderConfigFilename);

  Result := StringReplace(sText, '%COMMANDER_INI%\..', ExcludeTrailingPathDelimiter(ExtractFilePath(sAbsoluteTotalCommanderConfigFilename)),[rfIgnoreCase]);
  Result := StringReplace(Result, '%COMMANDER_INI%', sAbsoluteTotalCommanderConfigFilename, [rfIgnoreCase]);
  Result := StringReplace(Result, '%COMMANDER_PATH%', ExcludeTrailingPathDelimiter(ExtractFilePath(sAbsoluteTotalCommanderExecutableFilename)), [rfIgnoreCase]);
  Result := StringReplace(Result, '%COMMANDER_EXE%', ExcludeTrailingPathDelimiter(ExtractFilePath(sAbsoluteTotalCommanderExecutableFilename)), [rfIgnoreCase]);
  Result := StringReplace(Result, '%COMMANDER_DRIVE%', ExcludeTrailingPathDelimiter(ExtractFileDrive(sAbsoluteTotalCommanderExecutableFilename)), [rfIgnoreCase]);
  if utf8pos(UTF8UpperCase('wcmicons.dll'), UTF8UpperCase(Result)) = 1 then
    Result := StringReplace(Result, 'wcmicons.dll', ExtractFilePath(sAbsoluteTotalCommanderExecutableFilename) + 'wcmicons.dll', [rfIgnoreCase]);
end;

{ GetTotalCommandeMainBarFilename }
// We'll return the TC main bar filename.
// At the same time, since we're in the config file, we'll determine the icon size for the button bar.
// TC attempts to save the "default.bar" file in the same location as the executable.
// When it can, it will be located there.
// If not, it will store it in the same location as the ini file.
// Obviously, if it's configured somewhere else by the user, its location will be stored into the ini file in the section "Buttonbar" under the variable "Buttonbar".
// So the flow to find it would be something like that:
//   1.Let's attempt to read it from "Buttonbar/Buttonbar" from the ini file. If it's there, we may quit searching and exit with that.
//   2.If it was not found, let's attempt to see if we have one in the same directory as the ini config file. If it's there, we may quit searching and exit with that.
//   3.If we still don't have one, let's check if it is in the same folder as the executable itself... And it will have to be there!
function GetTotalCommandeMainBarFilename: string;
var
  TCMainConfigFile: TIniFileEx;
begin
  Result := '';

  //1.Let's attempt to read it from configuration file.
  if mbFileExists(mbExpandFileName(gTotalCommanderConfigFilename)) then
  begin
    TCMainConfigFile := TIniFileEx.Create(mbExpandFileName(gTotalCommanderConfigFilename));
    try
      Result := mbExpandFileName(ReplaceTCEnvVars(ConvertTCStringToString(TCMainConfigFile.ReadString(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTONBAR_SECTION, Result))));
      //While we're there, we'll get the button height.
      TCIconSize := TCMainConfigFile.ReadInteger(TCCONFIG_BUTTONBAR_SECTION, TCCONFIG_BUTTONHEIGHT, 32 + 5);
      TCIconSize := TCIconSize - 5; //Yeah... A magic -5...
    finally
      TCMainConfigFile.Free;
    end;

    //2.If we have no result, let's let see if we have from one from the same location as the configuration file.
    if Result = '' then
      if FileExists(ExtractFilePath(mbExpandFileName(gTotalCommanderConfigFilename)) + TCCONFIG_DEFAULTBAR_FILENAME) then
        result := ExtractFilePath(mbExpandFileName(gTotalCommanderConfigFilename)) + TCCONFIG_DEFAULTBAR_FILENAME;
  end;

  //3.If we still did not find it, let's finally attempt to take it from the same location as the executable.
  if Result = '' then
    if FileExists(ExtractFilePath(mbExpandFileName(gTotalCommanderExecutableFilename)) + TCCONFIG_DEFAULTBAR_FILENAME) then
      result := ExtractFilePath(mbExpandFileName(gTotalCommanderExecutableFilename)) + TCCONFIG_DEFAULTBAR_FILENAME;
end;

{ EnumTaskWindowsProc }
// Routine used for the following "IsTotalCommanderSeemsRunning" routine.
function EnumTaskWindowsProc(Wnd: THandle; List{%H-}: TStrings): boolean; stdcall;
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
  if mbFileExists(mbExpandFileName(gTotalCommanderExecutableFilename)) then
  begin
    if mbFileExists(mbExpandFileName(gTotalCommanderConfigFilename)) then
    begin
      sTotalCommanderMainbarFilename := GetTotalCommandeMainBarFilename;
      if mbFileExists(sTotalCommanderMainbarFilename) then
      begin
        if mbDirectoryExists(ExcludeTrailingPathDelimiter(mbExpandFileName(gTotalCommanderToolbarPath))) then
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

      Result := IncludeTrailingPathDelimiter(mbExpandFileName(gTotalCommanderToolbarPath)) + Result;

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
function GetDCEquivalentCommandToTCCommand(TCCommand: string; var TCIndexOfCommand: integer; ListOfParameters: TStringList): string;
begin
  Result := 'nil';
  TCIndexOfCommand := 1;
  ListOfParameters.Clear;

  if TCCommand <> '' then
  begin
    TCCommand := UTF8LowerCase(TCCommand);

    //Let's see if we have an equivalent DC for the TC command.
    while (TCIndexOfCommand <= NUMBEROFCOMMANDS) and (Result = 'nil') do
    begin
      if TCCommand = UTF8LowerCase(COMMANDS_LIST_TC[TCIndexOfCommand].TCCommand) then
      begin
        Result := COMMANDS_LIST_TC[TCIndexOfCommand].DCCommand;
        ParseLineToList(COMMANDS_LIST_TC[TCIndexOfCommand].DCParameters, ListOfParameters);
      end
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
    TCIndexOfCommand: integer = -1;
    IndexItem: integer;
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

  TargetBarFilenamePrefix := IncludeTrailingPathDelimiter(mbExpandFilename(gTotalCommanderToolbarPath)) + rsFilenameExportedTCBarPrefix;
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
    TCMainConfigFile := TIniFileEx.Create(mbExpandFileName(gTotalCommanderConfigFilename), fmOpenReadWrite);
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

{ ConvertTCToolbarToDCXmlConfig }
// Will import the TC toolbar file named "sBarFilename" into either our "AToolbarConfig" XML structure.
// If the TC toolbar have buttons pointing other TC toolbar file, the routine will import them as well
//   and organize something similar in the tree structure of subtoolbar DC is using.
// Obviously to avoid keeps cycling in round if "Toolbar A points toolbar B and toolbar B points toolbar A",
//   this import routine will not re-importe a toolbar already imported.
procedure ConvertTCToolbarToDCXmlConfig(sTCBarFilename: string; ADCXmlConfig:TXmlConfig);
var
  TCToolbarFilenameList: TStringList; //To hold the TC toolbarfile already imported so we don't re-import more than once a toolbar file already imported.
  TCIndexOfCommand: integer;
  DCListOfParameters: TStringList;
  ToolBarNode, RowNode: TXmlNode;

  // WARNING: "RecursiveIncorporateTCBarfile" is recursive and may call itself!
  procedure RecursiveIncorporateTCBarfile(Barfilename: string; InsertionNode:TXmlNode);
  var
    TCBarConfigFile: TIniFileEx;
    IndexButton: integer;
    sButtonName, sCmdName, sHintName, sParamValue, sStartingPath: string;
    SubMenuNode, CommandNode, MenuItemsNode: TXmlNode;
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
              CommandNode := ADCXmlConfig.AddNode(InsertionNode, 'Separator');
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
                // We have an internal command!
                sCmdName := GetDCEquivalentCommandToTCCommand(sCmdName, TCIndexOfCommand, DCListOfParameters);

                if TCIndexOfCommand <> -1 then
                begin
                  // If we have an equivalent, we add it as the equivalent internal command.
                  CommandNode := ADCXmlConfig.AddNode(InsertionNode, 'Command');
                  ADCXmlConfig.AddValue(CommandNode, 'ID', GuidToString(DCGetNewGUID));
                  ADCXmlConfig.AddValue(CommandNode, 'Icon', UTF8LowerCase(sCmdName));
                  ADCXmlConfig.AddValue(CommandNode, 'Command', sCmdName);
                  ADCXmlConfig.AddValue(CommandNode, 'Hint', sHintName);
                end
                else
                begin
                  // If we don't have an equivalent, we add is as an external command and we will write info to mean it.
                  CommandNode := ADCXmlConfig.AddNode(InsertionNode, 'Program');
                  ADCXmlConfig.AddValue(CommandNode, 'ID', GuidToString(DCGetNewGUID));
                  ADCXmlConfig.AddValue(CommandNode, 'Icon', '???: '+sButtonName); // ???: will result into the question mark icon so it's easy for user to see that one did not work.
                  ADCXmlConfig.AddValue(CommandNode, 'Command', rsNoEquivalentInternalCommand + ' - ' + sCmdName);
                  ADCXmlConfig.AddValue(CommandNode, 'Params',  '');
                  ADCXmlConfig.AddValue(CommandNode, 'StartPath',  '');
                  ADCXmlConfig.AddValue(CommandNode, 'Hint', rsNoEquivalentInternalCommand);
                end;
              end
              else
              begin
                if UTF8UpperCase(ExtractFileExt(sCmdName)) = '.BAR' then
                begin
                  //Since with TC we could have toolbars recursively pointing themselves, we need to make sure we'll not get lost cycling throught the same ones over and over.
                  if TCToolbarFilenameList.IndexOf(UTF8UpperCase(sCmdName)) = -1 then
                  begin
                    //We have a subtoolbar!
                    TCToolbarFilenameList.Add(UTF8UpperCase(sCmdName));
                    SubMenuNode := ADCXmlConfig.AddNode(InsertionNode, 'Menu');
                    ADCXmlConfig.AddValue(SubMenuNode, 'ID', GuidToString(DCGetNewGUID));
                    if sHintName <> '' then
                      ADCXmlConfig.AddValue(SubMenuNode, 'Hint', sHintName)
                    else
                      ADCXmlConfig.AddValue(SubMenuNode, 'Hint', 'Sub menu');
                    ADCXmlConfig.AddValue(SubMenuNode, 'Icon', sButtonName);
                    MenuItemsNode := ADCXmlConfig.AddNode(SubMenuNode, 'MenuItems');
                    RecursiveIncorporateTCBarfile(sCmdName, MenuItemsNode);
                  end;
                end
                else
                begin
                  //We have a "Program Item"
                  CommandNode := ADCXmlConfig.AddNode(InsertionNode, 'Program');
                  ADCXmlConfig.AddValue(CommandNode, 'ID', GuidToString(DCGetNewGUID));
                  ADCXmlConfig.AddValue(CommandNode, 'Icon', sButtonName);
                  ADCXmlConfig.AddValue(CommandNode, 'Command', sCmdName);
                  ADCXmlConfig.AddValue(CommandNode, 'Params',  sParamValue);
                  ADCXmlConfig.AddValue(CommandNode, 'StartPath',  sStartingPath);
                  if sHintName <> '' then
                    ADCXmlConfig.AddValue(CommandNode, 'Hint', sHintName)
                  else
                    ADCXmlConfig.AddValue(CommandNode, 'Hint', 'Program');
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
    ToolBarNode := ADCXmlConfig.FindNode(ADCXmlConfig.RootNode, 'Toolbars/MainToolbar', True);
    ADCXmlConfig.ClearNode(ToolBarNode);
    RowNode := ADCXmlConfig.AddNode(ToolBarNode, 'Row');

    DCListOfParameters := TStringList.Create;
    try
      RecursiveIncorporateTCBarfile(sTCBarFilename, RowNode);
    finally
      DCListOfParameters.Free;
    end;
  finally
    TCToolbarFilenameList.Free;
  end;
end;

end.
