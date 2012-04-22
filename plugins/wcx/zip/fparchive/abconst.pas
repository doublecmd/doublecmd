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
{* Abbrevia: AbConst.pas                                 *}
{*********************************************************}
{* Abbrevia: Constants                                   *}
{*********************************************************}

unit AbConst;

{$I AbDefine.inc}

interface

const
  AbVersion = 5.0;
  AbVersionS = '5.0';
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
  AbBadSpanStream                = 11;
  AbNoOverwriteSpanStream        = 12;
  AbNoSpannedSelfExtract         = 13;
  AbStreamFull                   = 14;
  AbNoSuchDirectory              = 15;
  AbInflateBlockError            = 16;
  AbBadStreamType                = 17;
  AbTruncateError                = 18;
  AbZipBadCRC                    = 19;
  AbZipBadStub                   = 20;
  AbFileNotFound                 = 21;
  AbInvalidLFH                   = 22;
  AbNoArchive                    = 23;
  AbErrZipInvalid                = 24;
  AbReadError                    = 25;
  AbInvalidIndex                 = 26;
  AbInvalidThreshold             = 27;
  AbUnhandledFileType            = 28;
  AbSpanningNotSupported         = 29;

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

  AbVMSReadTooManyBytes           = 100;
  AbVMSInvalidOrigin              = 101;
  AbVMSErrorOpenSwap              = 102;
  AbVMSSeekFail                   = 103;
  AbVMSReadFail                   = 104;
  AbVMSWriteFail                  = 105;
  AbVMSWriteTooManyBytes          = 106;

  AbGZipInvalid                   = 200;
  AbGzipBadCRC                    = 201;
  AbGzipBadFileSize               = 202;

  AbTarInvalid                    = 220;
  AbTarBadFileName                = 221;
  AbTarBadLinkName                = 222;
  AbTarBadOp                      = 223;


function AbStrRes(Index : Integer) : string;

implementation

uses
  AbResString;

type
  AbStrRec = record
    ID: Integer;
    Str: string;
  end;

const
  AbStrArray : array [0..66] of AbStrRec = (
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
    (ID: AbBadSpanStream; Str: AbBadSpanStreamS),
    (ID: AbNoOverwriteSpanStream; Str: AbNoOverwriteSpanStreamS),
    (ID: AbNoSpannedSelfExtract; Str: AbNoSpannedSelfExtractS),
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
    (ID: AbUnhandledFileType; Str: AbUnhandledFileTypeS),
    (ID: AbSpanningNotSupported; Str: AbSpanningNotSupportedS),

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

    (ID: AbVMSReadTooManyBytes; Str: AbVMSReadTooManyBytesS),
    (ID: AbVMSInvalidOrigin; Str: AbVMSInvalidOriginS),
    (ID: AbVMSErrorOpenSwap; Str: AbVMSErrorOpenSwapS),
    (ID: AbVMSSeekFail; Str: AbVMSSeekFailS),
    (ID: AbVMSReadFail; Str: AbVMSReadFailS),
    (ID: AbVMSWriteFail; Str: AbVMSWriteFailS),
    (ID: AbVMSWriteTooManyBytes; Str: AbVMSWriteTooManyBytesS),

    (ID: AbGzipInvalid; Str: AbGzipInvalidS),
    (ID: AbGzipBadCRC; Str: AbGzipBadCRCS),
    (ID: AbGzipBadFileSize; Str: AbGzipBadFileSizeS),

    (ID: AbTarInvalid; Str: AbTarInvalidS),
    (ID: AbTarBadFileName; Str: AbTarBadFileNameS),
    (ID: AbTarBadLinkName; Str: AbTarBadLinkNameS),
    (ID: AbTarBadOp; Str: AbTarBadOpS)
  );

function AbStrRes(Index : Integer) : string;
var
  i : Integer;
begin
  for i := Low(AbStrArray) to High(AbStrArray) do
    if AbStrArray[i].ID = Index then
      Result := AbStrArray[i].Str;
end;

end.
