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
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* Abbrevia: Abconst.pas 3.05                            *}
{*********************************************************}
{* Abbrevia: Constants                                   *}
{*********************************************************}

unit AbConst;

interface

const
  AbVersion = '3.05';
  Ab_MessageLen = 255;
  Ab_CaptionLen = 80;
  AB_ZIPPATHDELIM = '/';

const
  AbZipVersionNeeded             = 1;
  AbUnknownCompressionMethod     = 2;
  AbNoExtractionMethod           = 3;
  AbInvalidPassword              = 4;
  AbNoInsertionMethod            = 5;
  AbInvalidFactor                = 6;
  AbDuplicateName                = 7;
  AbUnsupportedCompressionMethod = 8;
  AbUserAbort                    = 9;
  AbArchiveBusy                  = 10;
  AbDiskRequest                  = 11;
  AbLastDiskRequest              = 12;
  AbBadSpanStream                = 13;
  AbDiskNumRequest               = 14;
  AbNoOverwriteSpanStream        = 15;
  AbNoSpannedSelfExtract         = 16;
  AbBlankDisk                    = 17;
  AbStreamFull                   = 18;
  AbNoSuchDirectory              = 19;
  AbInflateBlockError            = 20;
  AbBadStreamType                = 21;
  AbTruncateError                = 22;
  AbZipBadCRC                    = 23;
  AbZipBadStub                   = 24;
  AbFileNotFound                 = 25;
  AbInvalidLFH                   = 26;
  AbNoArchive                    = 27;
  AbErrZipInvalid                = 28;
  AbReadError                    = 29;
  AbInvalidIndex                 = 30;
  AbInvalidThreshold             = 31;
  AbNthImageRequest              = 32;
  AbLastImageRequest             = 33;
  AbImageRequest                 = 34;

  AbUnhandledFileType            = 35;
  AbSpanningNotSupported         = 36;
  AbImageNumRequest              = 37;
  AbLogCreateError               = 38;

  AbBBSReadTooManyBytes          = 40;
  AbBBSSeekOutsideBuffer         = 41;
  AbBBSInvalidOrigin             = 42;
  AbBBSWriteTooManyBytes         = 43;

  AbNoCabinetDllError            = 50;
  AbFCIFileOpenError             = 51;
  AbFCIFileReadError             = 52;
  AbFCIFileWriteError            = 53;
  AbFCIFileCloseError            = 54;
  AbFCIFileSeekError             = 55;
  AbFCIFileDeleteError           = 56;
  AbFCIAddFileError              = 57;
  AbFCICreateError               = 58;
  AbFCIFlushCabinetError         = 59;
  AbFCIFlushFolderError          = 60;
  AbFDICopyError                 = 61;
  AbFDICreateError               = 62;
  AbInvalidCabTemplate           = 63;
  AbInvalidCabFile               = 64;

  AbSWSNotEndofStream            = 80;
  AbSWSSeekFailed                = 81;
  AbSWSWriteFailed               = 82;
  AbSWSInvalidOrigin             = 83;
  AbSWSInvalidNewOrigin          = 84;

  AbVersionFormat                = 100;
  AbMethod                       = 101; {base for later ids, don't add to array!}
  AbMethod0                      = 101;
  AbMethod1                      = 102;
  AbMethod2                      = 103;
  AbMethod3                      = 104;
  AbMethod4                      = 105;
  AbMethod5                      = 106;
  AbMethod6                      = 107;
  AbMethod7                      = 108;
  AbMethod8                      = 109;
  AbMethod9                      = 110;
  AbMethod10                      = 111;
  AbMethod11                      = 112;

  AbCompressedSizeFormat          = 113;
  AbUncompressedSizeFormat        = 114;
  AbCompressionMethodFormat       = 115;
  AbCompressionRatioFormat        = 116;
  AbCRCFormat                     = 117;
  AbReadOnly                      = 118;
  AbHidden                        = 119;
  AbSystem                        = 120;
  AbArchived                      = 121;
  AbEFAFormat                     = 122;
  AbIFAFormat                     = 123;
  AbText                          = 124;
  AbBinary                        = 125;
  AbEncryptionFormat              = 126;
  AbEncrypted                     = 127;
  AbNotEncrypted                  = 128;
  AbUnknown                       = 129;
  AbTimeStampFormat               = 130;
  AbMadeByFormat                  = 131;
  AbNeededFormat                  = 132;
  AbCommentFormat                 = 133;

  AbDefaultExt                    = 134;
  AbFilter                        = 135;
  AbFileNameTitle                 = 136;

  AbOK                            = 137;
  AbCancel                        = 138;
  AbSelectDirectory               = 139;

  AbEnterPassword                 = 140;
  AbPassword                      = 141;
  AbVerify                        = 142;

  AbCabExt                        = 150;
  AbCabFilter                     = 151;
  AbLogExt                        = 152;
  AbLogFilter                     = 153;
  AbExeExt                        = 154;
  AbExeFilter                     = 155;

  AbVMSReadTooManyBytes           = 200;
  AbVMSInvalidOrigin              = 201;
  AbVMSErrorOpenSwap              = 202;
  AbVMSSeekFail                   = 203;
  AbVMSReadFail                   = 204;
  AbVMSWriteFail                  = 205;
  AbVMSWriteTooManyBytes          = 206;

  AbDefColHeadings                = 250;
  AbItemNameHeading               = 250;
  AbPackedHeading                 = 251;
  AbMethodHeading                 = 252;
  AbRatioHeading                  = 253;
  AbCRCHeading                    = 254;
  AbFileAttrHeading               = 255;
  AbFileFormatHeading             = 256;
  AbEncryptionHeading             = 257;
  AbTimeStampHeading              = 258;
  AbFileSizeHeading               = 259;
  AbVersionMadeHeading            = 260;
  AbVersionNeededHeading          = 261;
  AbPathHeading                   = 262;
  AbPartialHeading                = 263;
  AbExecutableHeading             = 264;

  AbCabMethod0                    = 290;
  AbCabMethod1                    = 291;

  AbLtAdd                         = 310;
  AbLtDelete                      = 311;
  AbLtExtract                     = 312;
  AbLtFreshen                     = 313;
  AbLtMove                        = 314;
  AbLtReplace                     = 315;
  AbLtStart                       = 316;

  AbGZipInvalid                   = 400;
  AbGzipBadCRC                    = 401;
  AbGzipBadFileSize               = 402;

  AbBZipInvalid                   = 403;
  AbBzipBadCRC                    = 404;
  AbBzipBadFileSize               = 405;

  AbUnhandledEntity               = 513;

  AbInvalidHeader				  = $FFFF;

// Bzip2 Const

   AbBZ_X_MAGIC_1 = 10;
   AbBZ_X_MAGIC_2 = 11;
   AbBZ_X_MAGIC_3 = 12;
   AbBZ_X_MAGIC_4 = 13;

//   AbBZ_HDR_B = 42h;   /* 'B' */
//   AbBZ_HDR_Z = 5ah;   /* 'Z' */
//   AbBZ_HDR_h = 68h;   /* 'h' */
//   AbBZ_HDR_0 = 30h;   /* '0' */

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
  AbNthImageRequestS = 'Specify spanned image file # ';
  AbLastImageRequestS = 'Specify the last file name in the spanned image set';
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
  abMoveFileErrorS = 'Error Moving File %s to %s';

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

  AbMethod0S = 'Stored';
  AbMethod1S = 'Shrunk';
  AbMethod2S = 'Reduced';
  AbMethod3S = 'Reduced';
  AbMethod4S = 'Reduced';
  AbMethod5S = 'Reduced';
  AbMethod6S = 'Imploded';
  AbMethod7S = 'Tokenized';
  AbMethod8S = 'Deflated';
  AbMethod9S = 'Enhanced Deflation';
  AbMethod10S = 'DCL Imploded';
  AbMethod11S = 'Best Method';

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

  AbBzipInvalidS                   = 'Invalid Bzip';
  AbBzipBadCRCS                    = 'Bad CRC';
  AbBzipBadFileSizeS               = 'Bad File Size';

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
  AbGzOsunknown     = 'unknown';
  AbGzOsUndefined   = 'ID undefined by gzip';


{!!.03 - Moved from AbCompnd.inc }
{ Compound File specific error messages }
resourcestring
  AbCmpndIndexOutOfBounds   = 'Index out of bounds';
  AbCmpndBusyUpdating       = 'Compound file is busy updating';
  AbCmpndInvalidFile        = 'Invalid compound file';
  AbCmpndFileNotFound       = 'File/Directory not found';
  AbCmpndFolderNotEmpty     = 'Folder not empty';
  AbCmpndExceedsMaxFileSize = 'File size exceeds maximum allowable';
{!!.03 - End Moved }


type
  AbStrRec = record
    ID: Integer;
    Str: string;
  end;

const
  AbStrArray : array [0..145] of AbStrRec = (
    (ID: AbZipVersionNeeded; Str: AbZipVersionNeededS),
    (ID: AbUnknownCompressionMethod; Str: AbUnknownCompressionMethodS),
    (ID: AbNoExtractionMethod; Str: AbNoExtractionMethodS),
    (ID: AbInvalidPassword; Str: AbInvalidPasswordS),
    (ID: AbNoInsertionMethod; Str: AbNoInsertionMethodS),
    (ID: AbInvalidFactor; Str: AbInvalidFactorS),
    (ID: AbDuplicateName; Str: AbDuplicateNameS),
    (ID: AbUnsupportedCompressionMethod; Str: AbUnsupportedCompressionMethodS),
    (ID: AbUserAbort; Str: AbUserAbortS),
    (ID: AbArchiveBusy; Str: AbArchiveBusyS),
    (ID: AbDiskRequest; Str: AbDiskRequestS),
    (ID: AbLastDiskRequest; Str: AbLastDiskRequestS),
    (ID: AbBadSpanStream; Str: AbBadSpanStreamS),
    (ID: AbDiskNumRequest; Str: AbDiskNumRequestS),
    (ID: AbNoOverwriteSpanStream; Str: AbNoOverwriteSpanStreamS),
    (ID: AbNoSpannedSelfExtract; Str: AbNoSpannedSelfExtractS),
    (ID: AbBlankDisk; Str: AbBlankDiskS),
    (ID: AbStreamFull; Str: AbStreamFullS),
    (ID: AbNoSuchDirectory; Str: AbNoSuchDirectoryS),
    (ID: AbInflateBlockError; Str: AbInflateBlockErrorS),
    (ID: AbBadStreamType; Str: AbBadStreamTypeS),
    (ID: AbTruncateError; Str: AbTruncateErrorS),
    (ID: AbZipBadCRC; Str: AbZipBadCRCS),
    (ID: AbZipBadStub; Str: AbZipBadStubS),
    (ID: AbFileNotFound; Str: AbFileNotFoundS),
    (ID: AbInvalidLFH; Str: AbInvalidLFHS),
    (ID: AbNoArchive; Str: AbNoArchiveS),
    (ID: AbErrZipInvalid; Str: AbErrZipInvalidS),
    (ID: AbReadError; Str: AbReadErrorS),
    (ID: AbInvalidIndex; Str: AbInvalidIndexS),
    (ID: AbInvalidThreshold; Str: AbInvalidThresholdS),
    (ID: AbNthImageRequest; Str: AbNthImageRequestS),
    (ID: AbLastImageRequest; Str: AbLastImageRequestS),
    (ID: AbImageRequest; Str: AbImageRequestS),
    (ID: AbUnhandledFileType; Str: AbUnhandledFileTypeS),
    (ID: AbSpanningNotSupported; Str: AbSpanningNotSupportedS),
    (ID: AbLogCreateError; Str: AbLogCreateErrorS),

    (ID: AbImageNumRequest; Str: AbImageNumRequestS),

    (ID: AbBBSReadTooManyBytes; Str: AbBBSReadTooManyBytesS),
    (ID: AbBBSSeekOutsideBuffer; Str: AbBBSSeekOutsideBufferS),
    (ID: AbBBSInvalidOrigin; Str: AbBBSInvalidOriginS),
    (ID: AbBBSWriteTooManyBytes; Str: AbBBSWriteTooManyBytesS),

    (ID: AbNoCabinetDllError; Str: AbNoCabinetDllErrorS),
    (ID: AbFCIFileOpenError; Str: AbFCIFileOpenErrorS),
    (ID: AbFCIFileReadError; Str: AbFCIFileReadErrorS),
    (ID: AbFCIFileWriteError; Str: AbFCIFileWriteErrorS),
    (ID: AbFCIFileCloseError; Str: AbFCIFileCloseErrorS),
    (ID: AbFCIFileSeekError; Str: AbFCIFileSeekErrorS),
    (ID: AbFCIFileDeleteError; Str: AbFCIFileDeleteErrorS),
    (ID: AbFCIAddFileError; Str: AbFCIAddFileErrorS),
    (ID: AbFCICreateError; Str: AbFCICreateErrorS),
    (ID: AbFCIFlushCabinetError; Str: AbFCIFlushCabinetErrorS),
    (ID: AbFCIFlushFolderError; Str: AbFCIFlushFolderErrorS),
    (ID: AbFDICopyError; Str: AbFDICopyErrorS),
    (ID: AbFDICreateError; Str: AbFDICreateErrorS),
    (ID: AbInvalidCabTemplate; Str: AbInvalidCabTemplateS),
    (ID: AbInvalidCabFile; Str: AbInvalidCabFileS),

    (ID: AbSWSNotEndofStream; Str: AbSWSNotEndofStreamS),
    (ID: AbSWSSeekFailed; Str: AbSWSSeekFailedS),
    (ID: AbSWSWriteFailed; Str: AbSWSWriteFailedS),
    (ID: AbSWSInvalidOrigin; Str: AbSWSInvalidOriginS),
    (ID: AbSWSInvalidNewOrigin; Str: AbSWSInvalidNewOriginS),

    (ID: AbVersionFormat; Str: AbVersionFormatS),
    (ID: AbMethod0; Str: AbMethod0S),
    (ID: AbMethod1; Str: AbMethod1S),
    (ID: AbMethod2; Str: AbMethod2S),
    (ID: AbMethod3; Str: AbMethod3S),
    (ID: AbMethod4; Str: AbMethod4S),
    (ID: AbMethod5; Str: AbMethod5S),
    (ID: AbMethod6; Str: AbMethod6S),
    (ID: AbMethod7; Str: AbMethod7S),
    (ID: AbMethod8; Str: AbMethod8S),
    (ID: AbMethod9; Str: AbMethod9S),
    (ID: AbMethod10; Str: AbMethod10S),
    (ID: AbMethod11; Str: AbMethod11S),

    (ID: AbCompressedSizeFormat; Str: AbCompressedSizeFormatS),
    (ID: AbUncompressedSizeFormat; Str: AbUncompressedSizeFormatS),
    (ID: AbCompressionMethodFormat; Str: AbCompressionMethodFormatS),
    (ID: AbCompressionRatioFormat; Str: AbCompressionRatioFormatS),
    (ID: AbCRCFormat; Str: AbCRCFormatS),
    (ID: AbReadOnly; Str: AbReadOnlyS),
    (ID: AbHidden; Str: AbHiddenS),
    (ID: AbSystem; Str: AbSystemS),
    (ID: AbArchived; Str: AbArchivedS),
    (ID: AbEFAFormat; Str: AbEFAFormatS),
    (ID: AbIFAFormat; Str: AbIFAFormatS),
    (ID: AbText; Str: AbTextS),
    (ID: AbBinary; Str: AbBinaryS),
    (ID: AbEncryptionFormat; Str: AbEncryptionFormatS),
    (ID: AbEncrypted; Str: AbEncryptedS),
    (ID: AbNotEncrypted; Str: AbNotEncryptedS),
    (ID: AbUnknown; Str: AbUnknownS),
    (ID: AbTimeStampFormat; Str: AbTimeStampFormatS),
    (ID: AbMadeByFormat; Str: AbMadeByFormatS),
    (ID: AbNeededFormat; Str: AbNeededFormatS),
    (ID: AbCommentFormat; Str: AbCommentFormatS),

    (ID: AbDefaultExt; Str: AbDefaultExtS),
    (ID: AbFilter; Str: AbFilterS),
    (ID: AbFileNameTitle; Str: AbFileNameTitleS),

    (ID: AbOK; Str: AbOKS),
    (ID: AbCancel; Str: AbCancelS),
    (ID: AbSelectDirectory; Str: AbSelectDirectoryS),

    (ID: AbEnterPassword; Str: AbEnterPasswordS),
    (ID: AbPassword; Str: AbPasswordS),
    (ID: AbVerify; Str: AbVerifyS),

    (ID: AbCabExt; Str: AbCabExtS),
    (ID: AbCabFilter; Str: AbCabFilterS),
    (ID: AbLogExt; Str: AbLogExtS),
    (ID: AbLogFilter; Str: AbLogFilterS),
    (ID: AbExeExt; Str: AbExeExtS),
    (ID: AbExeFilter; Str: AbExeFilterS),

    (ID: AbVMSReadTooManyBytes; Str: AbVMSReadTooManyBytesS),
    (ID: AbVMSInvalidOrigin; Str: AbVMSInvalidOriginS),
    (ID: AbVMSErrorOpenSwap; Str: AbVMSErrorOpenSwapS),
    (ID: AbVMSSeekFail; Str: AbVMSSeekFailS),
    (ID: AbVMSReadFail; Str: AbVMSReadFailS),
    (ID: AbVMSWriteFail; Str: AbVMSWriteFailS),
    (ID: AbVMSWriteTooManyBytes; Str: AbVMSWriteTooManyBytesS),

    (ID: AbItemNameHeading; Str: AbItemNameHeadingS),
    (ID: AbPackedHeading; Str: AbPackedHeadingS),
    (ID: AbMethodHeading; Str: AbMethodHeadingS),
    (ID: AbRatioHeading; Str: AbRatioHeadingS),
    (ID: AbCRCHeading; Str: AbCRCHeadingS),
    (ID: AbFileAttrHeading; Str: AbFileAttrHeadingS),
    (ID: AbFileFormatHeading; Str: AbFileFormatHeadingS),
    (ID: AbEncryptionHeading; Str: AbEncryptionHeadingS),
    (ID: AbTimeStampHeading; Str: AbTimeStampHeadingS),
    (ID: AbFileSizeHeading; Str: AbFileSizeHeadingS),
    (ID: AbVersionMadeHeading; Str: AbVersionMadeHeadingS),
    (ID: AbVersionNeededHeading; Str: AbVersionNeededHeadingS),
    (ID: AbPathHeading; Str: AbPathHeadingS),
    (ID: AbPartialHeading; Str: AbPartialHeadingS),
    (ID: AbExecutableHeading; Str: AbExecutableHeadingS),

    (ID: AbCabMethod0; Str: AbCabMethod0S),
    (ID: AbCabMethod1; Str: AbCabMethod1S),

    (ID: AbLtAdd; Str: AbLtAddS),
    (ID: AbLtDelete; Str: AbLtDeleteS),
    (ID: AbLtExtract; Str: AbLtExtractS),
    (ID: AbLtFreshen; Str: AbLtFreshenS),
    (ID: AbLtMove; Str: AbLtMoveS),
    (ID: AbLtReplace; Str: AbLtReplaceS),
    (ID: AbLtStart; Str: AbLtStartS),

    (ID: AbGzipInvalid; Str: AbGzipInvalidS),
    (ID: AbGzipBadCRC; Str: AbGzipBadCRCS),
    (ID: AbGzipBadFileSize; Str: AbGzipBadFileSizeS),

    (ID: AbUnhandledEntity; Str: AbUnhandledEntityS)
  );

   BZ_MAX_ALPHA_SIZE = 253;
// Crc table for BZip2
    crctable32 : array [0..255] of LongWord  = (
   $00000000, $04c11db7, $09823b6e, $0d4326d9,
   $130476dc, $17c56b6b, $1a864db2, $1e475005,
   $2608edb8, $22c9f00f, $2f8ad6d6, $2b4bcb61,
   $350c9b64, $31cd86d3, $3c8ea00a, $384fbdbd,
   $4c11db70, $48d0c6c7, $4593e01e, $4152fda9,
   $5f15adac, $5bd4b01b, $569796c2, $52568b75,
   $6a1936c8, $6ed82b7f, $639b0da6, $675a1011,
   $791d4014, $7ddc5da3, $709f7b7a, $745e66cd,
   $9823b6e0, $9ce2ab57, $91a18d8e, $95609039,
   $8b27c03c, $8fe6dd8b, $82a5fb52, $8664e6e5,
   $be2b5b58, $baea46ef, $b7a96036, $b3687d81,
   $ad2f2d84, $a9ee3033, $a4ad16ea, $a06c0b5d,
   $d4326d90, $d0f37027, $ddb056fe, $d9714b49,
   $c7361b4c, $c3f706fb, $ceb42022, $ca753d95,
   $f23a8028, $f6fb9d9f, $fbb8bb46, $ff79a6f1,
   $e13ef6f4, $e5ffeb43, $e8bccd9a, $ec7dd02d,
   $34867077, $30476dc0, $3d044b19, $39c556ae,
   $278206ab, $23431b1c, $2e003dc5, $2ac12072,
   $128e9dcf, $164f8078, $1b0ca6a1, $1fcdbb16,
   $018aeb13, $054bf6a4, $0808d07d, $0cc9cdca,
   $7897ab07, $7c56b6b0, $71159069, $75d48dde,
   $6b93dddb, $6f52c06c, $6211e6b5, $66d0fb02,
   $5e9f46bf, $5a5e5b08, $571d7dd1, $53dc6066,
   $4d9b3063, $495a2dd4, $44190b0d, $40d816ba,
   $aca5c697, $a864db20, $a527fdf9, $a1e6e04e,
   $bfa1b04b, $bb60adfc, $b6238b25, $b2e29692,
   $8aad2b2f, $8e6c3698, $832f1041, $87ee0df6,
   $99a95df3, $9d684044, $902b669d, $94ea7b2a,
   $e0b41de7, $e4750050, $e9362689, $edf73b3e,
   $f3b06b3b, $f771768c, $fa325055, $fef34de2,
   $c6bcf05f, $c27dede8, $cf3ecb31, $cbffd686,
   $d5b88683, $d1799b34, $dc3abded, $d8fba05a,
   $690ce0ee, $6dcdfd59, $608edb80, $644fc637,
   $7a089632, $7ec98b85, $738aad5c, $774bb0eb,
   $4f040d56, $4bc510e1, $46863638, $42472b8f,
   $5c007b8a, $58c1663d, $558240e4, $51435d53,
   $251d3b9e, $21dc2629, $2c9f00f0, $285e1d47,
   $36194d42, $32d850f5, $3f9b762c, $3b5a6b9b,
   $0315d626, $07d4cb91, $0a97ed48, $0e56f0ff,
   $1011a0fa, $14d0bd4d, $19939b94, $1d528623,
   $f12f560e, $f5ee4bb9, $f8ad6d60, $fc6c70d7,
   $e22b20d2, $e6ea3d65, $eba91bbc, $ef68060b,
   $d727bbb6, $d3e6a601, $dea580d8, $da649d6f,
   $c423cd6a, $c0e2d0dd, $cda1f604, $c960ebb3,
   $bd3e8d7e, $b9ff90c9, $b4bcb610, $b07daba7,
   $ae3afba2, $aafbe615, $a7b8c0cc, $a379dd7b,
   $9b3660c6, $9ff77d71, $92b45ba8, $9675461f,
   $8832161a, $8cf30bad, $81b02d74, $857130c3,
   $5d8a9099, $594b8d2e, $5408abf7, $50c9b640,
   $4e8ee645, $4a4ffbf2, $470cdd2b, $43cdc09c,
   $7b827d21, $7f436096, $7200464f, $76c15bf8,
   $68860bfd, $6c47164a, $61043093, $65c52d24,
   $119b4be9, $155a565e, $18197087, $1cd86d30,
   $029f3d35, $065e2082, $0b1d065b, $0fdc1bec,
   $3793a651, $3352bbe6, $3e119d3f, $3ad08088,
   $2497d08d, $2056cd3a, $2d15ebe3, $29d4f654,
   $c5a92679, $c1683bce, $cc2b1d17, $c8ea00a0,
   $d6ad50a5, $d26c4d12, $df2f6bcb, $dbee767c,
   $e3a1cbc1, $e760d676, $ea23f0af, $eee2ed18,
   $f0a5bd1d, $f464a0aa, $f9278673, $fde69bc4,
   $89b8fd09, $8d79e0be, $803ac667, $84fbdbd0,
   $9abc8bd5, $9e7d9662, $933eb0bb, $97ffad0c,
   $afb010b1, $ab710d06, $a6322bdf, $a2f33668,
   $bcb4666d, $b8757bda, $b5365d03, $b1f740b4
);


function AbStrRes(Index : Integer) : string;

implementation

function AbStrRes(Index : Integer) : string;
var
  i : Integer;
begin
  for i := Low(AbStrArray) to High(AbStrArray) do
    if AbStrArray[i].ID = Index then
      Result := AbStrArray[i].Str;
end;


end.
