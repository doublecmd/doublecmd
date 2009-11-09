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

  AbUnhandledEntity               = 513;

  AbInvalidHeader                 = $FFFF;

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
