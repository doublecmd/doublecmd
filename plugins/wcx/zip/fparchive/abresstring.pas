(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Roman Kassebaum
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* Abbrevia: AbResString.pas                             *}
{*********************************************************}
{* Abbrevia: Resource strings                            *}
{*********************************************************}

unit AbResString;

{$I AbDefine.inc}

interface

resourcestring
  AbErrZipInvalidS = 'Invalid file - not a PKZip file';
  AbZipVersionNeededS = 'Cannot extract file - newer version required';
  AbUnknownCompressionMethodS = 'Cannot extract file - unsupported compression method';
  AbNoExtractionMethodS = 'Cannot extract file - no extraction support provided';
  AbInvalidPasswordS = 'Cannot extract file - invalid password';
  AbNoInsertionMethodS = 'Cannot insert file - no insertion support provided';
  AbInvalidFactorS = 'Invalid Reduce Factor';
  AbDuplicateNameS = 'Cannot insert file - duplicates stored name';
  AbUnsupportedCompressionMethodS = 'Cannot insert file - unsupported compression method';
  AbUserAbortS = 'Process aborted by user';
  AbArchiveBusyS = 'Archive is busy - cannot process new requests';
  AbLastDiskRequestS = 'Insert the last disk in the spanned disk set';
  AbDiskRequestS = 'Insert floppy';
  AbImageRequestS = 'Image file name';
  AbBadSpanStreamS = 'Spanned archives must be opened as file streams';
  AbDiskNumRequestS = 'Insert disk number %d of the spanned disk set';
  AbImageNumRequestS = 'Insert span number %d of the spanned file set';
  AbNoOverwriteSpanStreamS = 'Cannot update an existing spanned disk set';
  AbNoSpannedSelfExtractS = 'Cannot make a self-extracting spanned disk set';
  AbBlankDiskS = 'Insert a blank floppy disk';
  AbStreamFullS = 'Stream write error';
  AbNoSuchDirectoryS = 'Directory does not exist';
  AbInflateBlockErrorS = 'Cannot inflate block';
  AbBadStreamTypeS = 'Invalid Stream';
  AbTruncateErrorS = 'Error truncating Zip File';
  AbZipBadCRCS = 'Failed CRC Check';
  AbZipBadStubS = 'Stub must be an executable';
  AbFileNotFoundS = 'File not found';
  AbInvalidLFHS = 'Invalid Local File Header entry';
  AbNoArchiveS = 'Archive does not exist - Filename is blank';
  AbReadErrorS = 'Error reading archive';
  AbInvalidIndexS = 'Invalid archive item index';
  AbInvalidThresholdS = 'Invalid archive size threshold';
  AbUnhandledFileTypeS = 'Unhandled Archive Type';
  AbSpanningNotSupportedS = 'Spanning not supported by this Archive type';
  AbLogCreateErrorS = 'Error creating Log File';
  AbMoveFileErrorS = 'Error Moving File %s to %s';
  AbFileSizeTooBigS = 'File size is too big for archive type';

  AbNoCabinetDllErrorS = 'Cannot load cabinet.dll';
  AbFCIFileOpenErrorS = 'FCI cannot open file';
  AbFCIFileReadErrorS = 'FCI cannot read file';
  AbFCIFileWriteErrorS = 'FCI cannot write file';
  AbFCIFileCloseErrorS = 'FCI close file error';
  AbFCIFileSeekErrorS = 'FCI file seek error';
  AbFCIFileDeleteErrorS = 'FCI file delete error';
  AbFCIAddFileErrorS = 'FCI cannot add file';
  AbFCICreateErrorS = 'FCI cannot create context';
  AbFCIFlushCabinetErrorS = 'FCI cannot flush cabinet';
  AbFCIFlushFolderErrorS = 'FCI cannot flush folder';
  AbFDICopyErrorS = 'FDI cannot enumerate files';
  AbFDICreateErrorS = 'FDI cannot create context';
  AbInvalidCabTemplateS = 'Invalid cab file template';
  AbInvalidCabFileS = 'Invalid file - not a cabinet file';

  AbZipStored = 'Stored';
  AbZipShrunk = 'Shrunk';
  AbZipReduced = 'Reduced';
  AbZipImploded = 'Imploded';
  AbZipTokenized = 'Tokenized';
  AbZipDeflated = 'Deflated';
  AbZipDeflate64 = 'Enhanced Deflation';
  AbZipDCLImploded = 'DCL Imploded';
  AbZipBzip2 = 'Bzip2';
  AbZipLZMA = 'LZMA';
  AbZipIBMTerse = 'IBM Terse';
  AbZipLZ77 = 'IBM LZ77';
  AbZipJPEG = 'JPEG';
  AbZipWavPack = 'WavPack';
  AbZipPPMd = 'PPMd';
  AbZipUnknown = 'Unknown (%d)';
  AbZipBestMethod = 'Best Method';

  AbVersionFormatS = 'Version %s';
  AbCompressedSizeFormatS = 'Compressed Size: %d';
  AbUncompressedSizeFormatS = 'Uncompressed Size: %d';
  AbCompressionMethodFormatS = 'Compression Method: %s';
  AbCompressionRatioFormatS = 'Compression Ratio: %2.0f%%';
  AbCRCFormatS = 'CRC: %x';
  AbReadOnlyS = 'r';
  AbHiddenS = 'h';
  AbSystemS = 's';
  AbArchivedS = 'a';
  AbEFAFormatS = 'External File Attributes: %s';
  AbIFAFormatS = 'File Type: %s';
  AbTextS = 'Text';
  AbBinaryS = 'Binary';
  AbEncryptionFormatS = 'Encryption: %s';
  AbEncryptedS = 'Encrypted';
  AbNotEncryptedS = 'Not Encrypted';
  AbUnknownS = 'Unknown';
  AbTimeStampFormatS = 'Time Stamp: %s';
  AbMadeByFormatS = 'Made by Version: %f';
  AbNeededFormatS = 'Version Needed to Extract: %f';
  AbCommentFormatS = 'Comment: %s';
  AbDefaultExtS = '*.zip';
  AbFilterS = 'PKZip Archives (*.zip)|*.zip|Self Extracting Archives (*.exe)|*.exe|All Files (*.*)|*.*';
  AbFileNameTitleS = 'Select File Name';

  AbOKS = 'OK';
  AbCancelS = 'Cancel';
  AbSelectDirectoryS = 'Select Directory';

  AbEnterPasswordS = 'Enter Password';
  AbPasswordS = '&Password';
  AbVerifyS = '&Verify';

  AbCabExtS = '*.cab';
  AbCabFilterS = 'Cabinet Archives (*.cab)|*.CAB|All Files (*.*)|*.*';
  AbLogExtS = '*.txt';
  AbLogFilterS = 'Text Files (*.txt)|*.TXT|All Files (*.*)|*.*';
  AbExeExtS = '*.exe';
  AbExeFilterS = 'Self-Extracting Zip Files (*.exe)|*.EXE|All Files (*.*)|*.*';

  AbVMSReadTooManyBytesS = 'VMS: request to read too many bytes [%d]';
  AbVMSInvalidOriginS = 'VMS: invalid origin %d, should be 0, 1, 2';
  AbVMSErrorOpenSwapS = 'VMS: Cannot open swap file %s';
  AbVMSSeekFailS = 'VMS: Failed to seek in swap file %s';
  AbVMSReadFailS = 'VMS: Failed to read %d bytes from swap file %s';
  AbVMSWriteFailS = 'VMS: Failed to write %d bytes to swap file %s';
  AbVMSWriteTooManyBytesS = 'VMS: request to write too many bytes [%d]';

  AbBBSReadTooManyBytesS = 'BBS: request to read too many bytes [%d]';
  AbBBSSeekOutsideBufferS = 'BBS: New position is outside the buffer';
  AbBBSInvalidOriginS = 'BBS: Invalid Origin value';
  AbBBSWriteTooManyBytesS = 'BBS: request to write too many bytes [%d]';

  AbSWSNotEndofStreamS = 'TabSlidingWindowStream.Write: Not at end of stream';
  AbSWSSeekFailedS = 'TabSlidingWindowStream.bsWriteChunk: seek failed';
  AbSWSWriteFailedS = 'TabSlidingWindowStream.bsWriteChunk: write failed';
  AbSWSInvalidOriginS = 'TabSlidingWindowStream.Seek: invalid origin';
  AbSWSInvalidNewOriginS = 'TabSlidingWindowStream.Seek: invalid new position';

  AbItemNameHeadingS = 'Name';
  AbPackedHeadingS = 'Packed';
  AbMethodHeadingS = 'Method';
  AbRatioHeadingS = 'Ratio (%)';
  AbCRCHeadingS = 'CRC32';
  AbFileAttrHeadingS = 'Attributes';
  AbFileFormatHeadingS = 'Format';
  AbEncryptionHeadingS = 'Encrypted';
  AbTimeStampHeadingS = 'Time Stamp';
  AbFileSizeHeadingS = 'Size';
  AbVersionMadeHeadingS = 'Version Made';
  AbVersionNeededHeadingS = 'Version Needed';
  AbPathHeadingS = 'Path';
  AbPartialHeadingS = 'Partial';
  AbExecutableHeadingS = 'Executable';
  AbFileTypeHeadingS = 'Type';
  AbLastModifiedHeadingS = 'Modified';

  AbCabMethod0S = 'None';
  AbCabMethod1S = 'MSZip';

  AbLtAddS = ' added ';
  AbLtDeleteS = ' deleted ';
  AbLtExtractS = ' extracted ';
  AbLtFreshenS = ' freshened ';
  AbLtMoveS = ' moved ';
  AbLtReplaceS = ' replaced ';
  AbLtStartS = ' logging ';

  AbGzipInvalidS                   = 'Invalid Gzip';
  AbGzipBadCRCS                    = 'Bad CRC';
  AbGzipBadFileSizeS               = 'Bad File Size';

  AbTarInvalidS                    = 'Invalid Tar';
  AbTarBadFileNameS                = 'File name too long';
  AbTarBadLinkNameS                = 'Symbolic link path too long';
  AbTarBadOpS                      = 'Unsupported Operation';

  AbUnhandledEntityS             = 'Unhandled Entity';

  { pre-defined "operating system" (really more FILE system) identifiers for the
    Gzip header }
  AbGzOsFat         = 'FAT File System (MS-DOS, OS/2, NT/Win32)';
  AbGzOsAmiga       = 'Amiga';
  AbGzOsVMS         = 'VMS (or OpenVMS)';
  AbGzOsUnix        = 'Unix';
  AbGzOsVM_CMS      = 'VM/CMS';
  AbGzOsAtari       = 'Atari TOS';
  AbGzOsHPFS        = 'HPFS File System (OS/2, NT)';
  AbGzOsMacintosh   = 'Macintosh';
  AbGzOsZ_System    = 'Z-System';
  AbGzOsCP_M        = 'CP/M';
  AbGzOsTOPS_20     = 'TOPS-20';
  AbGzOsNTFS        = 'NTFS File System (NT)';
  AbGzOsQDOS        = 'QDOS';
  AbGzOsAcornRISCOS = 'Acorn RISCOS';
  AbGzOsVFAT        = 'VFAT File System (Win95, NT)';
  AbGzOsMVS         = 'MVS';
  AbGzOsBeOS        = 'BeOS (BeBox or PowerMac)';
  AbGzOsTandem      = 'Tandem/NSK';
  AbGzOsTHEOS       = 'THEOS';
  AbGzOsunknown     = 'unknown';
  AbGzOsUndefined   = 'ID undefined by gzip';

{ Compound File specific error messages }
resourcestring
  AbCmpndIndexOutOfBounds   = 'Index out of bounds';
  AbCmpndBusyUpdating       = 'Compound file is busy updating';
  AbCmpndInvalidFile        = 'Invalid compound file';
  AbCmpndFileNotFound       = 'File/Directory not found';
  AbCmpndFolderNotEmpty     = 'Folder not empty';
  AbCmpndExceedsMaxFileSize = 'File size exceeds maximum allowable';



implementation

end.
