{
   Double Commander
   -------------------------------------------------------------------------
   Localization core unit

   Copyright (C) 2007  Koblov Alexander (Alexx2000@mail.ru)

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
  rsMsgFileExistsRwrt = 'File %s exists, overwrite?';
  rsMsgFileChangedSave = 'File %s changed, save?';
  rsMsgNewFile = 'New file';
  rsMsgDelFlDr = 'Delete %d selected files/directories?';
  rsMsgDelSel = 'Delete selected "%s"?';
  rsMsgCpFlDr = 'Copy %d selected files/directories?';
  rsMsgCpSel = 'Copy selected "%s"?';
  rsMsgRenFlDr = 'Rename/move %d selected files/directories?';
  rsMsgRenSel = 'Rename/move selected "%s"?';
  rsMsgErrForceDir = 'Can not create directory %s!';
  rsMsgSelected = 'Selected %s of %s in %d of %d files';
  rsMsgPopUpHotAdd = '&Add %s';
  rsMsgPopUpHotCnf = '&Configure';
  rsDlgButtons = '&OK;&No;&Yes;&Cancel;&None;&Append;&Rewrite;Rewrite &All;&Skip;S&kip All;All;';
  rsSpaceMsg = 'Files:%d, Dirs:%d, Size: %d bytes';
  rsSelectDir = 'Select a directory';
  rsMarkPlus = 'Select mask';
  rsMarkMinus = 'Unselect mask';
  rsMaskInput = 'Input mask:';
  rsFreeMsg = 'Free %s from %s bytes';
  rsMsgErrDirExists = 'Directory %s exists!';
  rsMsgPopUpHotDelete = '&Delete %s';
  rsMsgDiskNotAvail = 'Disk is not available';
  rsMsgChDirFailed = 'ChDir to [%s] failed!';
  rsMsgNoFreeSpaceCont = 'No enough free space on target drive, Continue?';
  rsMsgNoFreeSpaceRetry = 'No enough free space on target drive, Retry?';
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

  rsSavePosition = '&Save position';
  rsPositionSaved = 'Position saved.';
  //File operations dialog
  rsDlgDel = 'Delete file(s)';
  rsDlgCp = 'Copy file(s)';
  rsDlgMv = 'Move file(s)';
  rsDlgPack = 'Packing...';
  rsDlgExtract = 'Extracting...';
  // Viewer
  rsViewAboutText = 'Internal Viewer of Double Commander.';
  
  //Column in TListView
  rsColName = 'Name';
  rsColExt = 'Ext';
  rsColSize = 'Size';
  rsColDate = 'Date';
  rsColAttr = 'Attr';
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
  //Compare by content Dialog
  rsCompareDiffs = 'Number of changes:';
  // Find files dialog
  rsFindScaned = 'Scanned: %d';
  rsFindWhereBeg = 'Begin at';
  rsFindDirNoEx = 'Directory %s does not exist!';
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
  rsSplitMsgCreated = 'Created:';
  rsSplitMsgSize = 'size:';
  // Edit
  rsEditNewFile = 'new.txt';
  rsEditNewOpen = 'Open file';
  rsEditNewFileName = 'Filename:';
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

function GetLanguageName(poFileName : String) : String;
procedure lngLoadLng(const sFileName:String);
procedure DoLoadLng;

implementation
uses
  Classes, SysUtils, GetText, Translations, uGlobs, uGlobsPaths, uTranslator, LCLProc;

function GetLanguageName(poFileName : String) : String;
var
  poFile : TextFile;
  sLine : String;
  iPos1,
  iPos2 : Integer;
begin
  AssignFile(poFile, poFileName);
  Reset(poFile);
  // find first msgid line
  ReadLn(poFile, sLine);
  while Pos('msgid', sLine) = 0 do
    ReadLn(poFile, sLine);
  // read msgstr line
  ReadLn(poFile, sLine);
  repeat
    ReadLn(poFile, sLine);
    // find language name line
    if Pos('X-Poedit-Language:', sLine) <> 0 then
      begin
        iPos1 := Pos(':', sLine) + 2;
        iPos2 := Pos('\n', sLine) - 1;
        Result := Copy(sLine, iPos1,  (iPos2 - iPos1) + 1);
        CloseFile(poFile);
        Exit;
      end;
  until Pos('msgid', sLine) = 1;
  CloseFile(poFile);
  Result := 'Language name not found';
end;

procedure lngLoadLng(const sFileName:String);
var
  Lang, FallbackLang : String;
begin
  { Localization }
  if sFileName = 'doublecmd.po' then Exit;  // default english interface
  
  DebugLn('Loading lng file: ' + gpLngDir + sFileName);

  gPOFileName := sFileName;
  if not FileExists(gpLngDir + gPOFileName) then
    begin
      gPOFileName := 'doublecmd.%s.po';
      GetLanguageIDs(Lang, FallbackLang);
      gPOFileName := Format(gPOFileName,[FallbackLang]);
    end;
  if not FileExists(gpLngDir + gPOFileName) then
    begin
      gPOFileName := Format(gPOFileName,[Lang]);
    end;
  if FileExists(gpLngDir + gPOFileName) then
    begin
      Translations.TranslateUnitResourceStrings('uLng', gpLngDir + gPOFileName);
      LRSTranslator := TTranslator.Create(gpLngDir + gPOFileName);
    end;
end;

procedure DoLoadLng;
begin
  lngLoadLng(gPOFileName);
end;

end.
