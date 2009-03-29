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
{* ABBREVIA: AbExcept.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: Exception classes                           *}
{*********************************************************}

{$I AbDefine.inc}

unit AbExcept;

interface

uses
  SysUtils,
  AbUtils;

type
  EAbException = class( Exception )
  public
    ErrorCode : Integer;
  end;

  EAbArchiveBusy = class( EAbException )
  public
    constructor Create;
  end;

  EAbBadStream = class( EAbException )
  protected
    FInnerException : Exception;
  public
    constructor Create;
    constructor CreateInner(aInnerException : Exception);
    property InnerException : Exception read FInnerException;
  end;

  EAbDuplicateName = class( EAbException )
  public
    constructor Create;
  end;

  EAbFileNotFound = class( EAbException )
  public
    constructor Create;
  end;

  EAbNoArchive = class( EAbException )
  public
    constructor Create;
  end;

  EAbUserAbort = class( EAbException )
  public
    constructor Create;
  end;

  EAbNoSuchDirectory = class( EAbException )
  public
    constructor Create;
  end;

  EAbUnhandledType = class( EAbException )
  public
    constructor Create;
  end;

  EAbSpanningNotSupported = class (EAbException)
  public
    constructor Create;
  end;

  EAbInvalidSpanningThreshold = class ( EAbException )
  public
    constructor Create;
  end;

  EAbZipException = class( EAbException ); {Zip exception}
  EAbCabException = class( EAbException ); {Cab exception}
  EAbTarException = class( EAbException ); {Tar Exception}
  EAbGzipException = class( EAbException); {GZip exception}
  EAbBzipException = class( EAbException); {BZip exception}


  EAbInvalidHeaderException = class(EAbException)
  public 
    constructor Create;
  end;

  EAbZipBadSpanStream = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipBadCRC = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipInflateBlock = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipInvalid = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbInvalidIndex = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipInvalidFactor = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipInvalidLFH = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipInvalidMethod = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipInvalidPassword = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipInvalidStub= class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipNoExtraction = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipNoInsertion = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipSpanOverwrite= class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipStreamFull = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipTruncate = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipUnsupported = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbZipVersion = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbReadError = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbTarBadFileName = class( EAbTarException )
  public
    constructor Create;
  end;

  EAbTarBadLinkName = class( EAbTarException )
  public
    constructor Create;
  end;

  EAbTarBadOp = class( EAbTarException )
  public
    constructor Create;
  end;

  EAbTarInvalid = class( EAbTarException )
  public
    constructor Create;
  end;

  EAbGzipBadCRC = class( EAbGZipException )
  public
    constructor Create;
  end;

  EAbGzipBadFileSize = class( EAbGZipException )
  public
    constructor Create;
  end;

  EAbGzipInvalid = class( EAbGZipException )
  public
    constructor Create;
  end;

  EAbBzipBadCRC = class( EAbBZipException )
  public
    constructor Create;
  end;

  EAbBzipBadFileSize = class( EAbBZipException )
  public
    constructor Create;
  end;

  EAbBzipInvalid = class( EAbBZipException )
  public
    constructor Create;
  end;

  EAbVMSReadTooManyBytes = class( EAbZipException )
  public
    constructor Create( Count : Integer; Dummy : Word );
  end;

  EAbVMSInvalidOrigin = class( EAbZipException )
  public
    constructor Create( Value : Integer; Dummy : Word );
  end;

  EAbVMSErrorOpenSwap = class( EAbZipException )
  public
    constructor Create( const Value : string );
  end;

  EAbVMSSeekFail = class( EAbZipException )
  public
    constructor Create( const Value : string );
  end;

  EAbVMSReadFail = class( EAbZipException )
  public
    constructor Create( Count : Integer; const Value : string );
  end;

  EAbVMSWriteFail = class( EAbZipException )
  public
    constructor Create( Count : Integer; const Value : string );
  end;

  EAbVMSWriteTooManyBytes = class( EAbZipException )
  public
    constructor Create( Count : Integer; Dummy : Word );
  end;

  EAbBBSReadTooManyBytes = class( EAbZipException )
  public
    constructor Create(Count : Integer; Dummy : Word);
  end;

  EAbBBSSeekOutsideBuffer = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbBBSInvalidOrigin = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbBBSWriteTooManyBytes = class( EAbZipException )
  public
    constructor Create(Count : Integer ; Dummy : Word);
  end;

  EAbSWSNotEndofStream = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbSWSSeekFailed = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbSWSWriteFailed = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbSWSInvalidOrigin = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbSWSInvalidNewOrigin = class( EAbZipException )
  public
    constructor Create;
  end;

  EAbNoCabinetDll = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFCIFileOpenError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFCIFileReadError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFCIFileWriteError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFCIFileCloseError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFCIFileSeekError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFCIFileDeleteError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFCIAddFileError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFCICreateError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFCIFlushCabinetError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFCIFlushFolderError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFDICopyError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFDICreateError = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbInvalidCabTemplate = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbInvalidCabFile = class( EAbCabException )
  public
    constructor Create;
  end;

  EAbFileTooLarge = class(EAbException)
  public
    constructor Create;
  end;

  procedure AbConvertException( const E : Exception;
                                var eClass : TAbErrorClass;
                                var eErrorCode : Integer );


implementation

uses
  Classes,
  AbConst;

constructor EAbArchiveBusy.Create;
begin
  inherited Create(AbStrRes(AbArchiveBusy));
  ErrorCode := AbArchiveBusy;
end;

constructor EAbBadStream.Create;
begin
  inherited Create(AbStrRes(AbBadStreamType));
  FInnerException := nil;
  ErrorCode := AbBadStreamType;
end;

constructor EAbBadStream.CreateInner(aInnerException: Exception);
begin
  inherited Create(AbStrRes(AbBadStreamType) + #13#10 + aInnerException.Message);
  FInnerException := aInnerException;
  ErrorCode := AbBadStreamType;
end;


constructor EAbDuplicateName.Create;
begin
  inherited Create(AbStrRes(AbDuplicateName));
  ErrorCode := AbDuplicateName;
end;

constructor EAbNoSuchDirectory.Create;
begin
  inherited Create(AbStrRes(AbNoSuchDirectory));
  ErrorCode := AbNoSuchDirectory;
end;

constructor EAbInvalidSpanningThreshold.Create;
begin
  inherited Create(AbStrRes(AbInvalidThreshold));
  ErrorCode := AbInvalidThreshold;
end;

constructor EAbFileNotFound.Create;
begin
  inherited Create(AbStrRes(AbFileNotFound));
  ErrorCode := AbFileNotFound;
end;

constructor EAbNoArchive.Create;
begin
  inherited Create(AbStrRes(AbNoArchive));
  ErrorCode := AbNoArchive;
end;

constructor EAbUserAbort.Create;
begin
  inherited Create(AbStrRes(AbUserAbort));
  ErrorCode := AbUserAbort;
end;

constructor EAbZipBadSpanStream.Create;
begin
  inherited Create(AbStrRes(AbBadSpanStream));
  ErrorCode := AbBadSpanStream;
end;

constructor EAbZipBadCRC.Create;
begin
  inherited Create(AbStrRes(AbZipBadCRC));
  ErrorCode := AbZipBadCRC;
end;

constructor EAbZipInflateBlock.Create;
begin
  inherited Create(AbStrRes(AbInflateBlockError));
  ErrorCode := AbInflateBlockError;
end;

constructor EAbZipInvalid.Create;
begin
  inherited Create(AbStrRes(AbErrZipInvalid));
  ErrorCode := AbErrZipInvalid;
end;

constructor EAbInvalidIndex.Create;
begin
  inherited Create(AbStrRes(AbInvalidIndex));
  ErrorCode := AbInvalidIndex;
end;

constructor EAbZipInvalidFactor.Create;
begin
  inherited Create(AbStrRes(AbInvalidFactor));
  ErrorCode := AbInvalidFactor;
end;

constructor EAbZipInvalidLFH.Create;
begin
  inherited Create(AbStrRes(AbInvalidLFH));
  ErrorCode := AbInvalidLFH;
end;

constructor EAbZipInvalidMethod.Create;
begin
  inherited Create(AbStrRes(AbUnknownCompressionMethod));
  ErrorCode := AbUnknownCompressionMethod;
end;

constructor EAbZipInvalidPassword.Create;
begin
  inherited Create(AbStrRes(AbInvalidPassword));
  ErrorCode := AbInvalidPassword;
end;

constructor EAbZipInvalidStub.Create;
begin
  inherited Create(AbStrRes(AbZipBadStub));
  ErrorCode := AbZipBadStub;
end;

constructor EAbZipNoExtraction.Create;
begin
  inherited Create(AbStrRes(AbNoExtractionMethod));
  ErrorCode := AbNoExtractionMethod;
end;

constructor EAbZipNoInsertion.Create;
begin
  inherited Create(AbStrRes(AbNoInsertionMethod));
  ErrorCode := AbNoInsertionMethod;
end;

constructor EAbZipSpanOverwrite.Create;
begin
  inherited Create(AbStrRes(AbNoOverwriteSpanStream));
  ErrorCode := AbNoOverwriteSpanStream;
end;

constructor EAbZipStreamFull.Create;
begin
  inherited Create(AbStrRes(AbStreamFull));
  ErrorCode := AbStreamFull;
end;

constructor EAbZipTruncate.Create;
begin
  inherited Create(AbStrRes(AbTruncateError));
  ErrorCode := AbTruncateError;
end;

constructor EAbZipUnsupported.Create;
begin
  inherited Create(AbStrRes(AbUnsupportedCompressionMethod));
  ErrorCode := AbUnsupportedCompressionMethod;
end;

constructor EAbZipVersion.Create;
begin
  inherited Create(AbStrRes(AbZipVersionNeeded));
  ErrorCode := AbZipVersionNeeded;
end;

constructor EAbReadError.Create;
begin
  inherited Create(AbStrRes(AbReadError));
  ErrorCode := AbReadError;
end;

constructor EAbTarBadFileName.Create;
begin
  inherited Create('Invalid file name (too long?)');
  ErrorCode := 0;
end;

constructor EAbTarBadLinkName.Create;
begin
  inherited Create('Invalid link name (too long?)');
  ErrorCode := 0;
end;

constructor EAbTarBadOp.Create;
begin
  inherited Create('Invalid operation requested');
  ErrorCode := 0;
end;

constructor EAbTarInvalid.Create;
begin
  inherited Create('Invalid TAR file');
  ErrorCode := 0;
end;

constructor EAbVMSReadTooManyBytes.Create( Count : Integer;
                                           Dummy : Word );
begin
  inherited Create(Format(AbStrRes(AbVMSReadTooManyBytes), [Count]));
  ErrorCode := AbVMSReadTooManyBytes;
end;

constructor EAbVMSInvalidOrigin.Create( Value : Integer;
                                        Dummy : Word );
begin
  inherited Create(Format(AbStrRes(AbVMSInvalidOrigin), [Value]));
  ErrorCode := AbVMSInvalidOrigin;
end;

constructor EAbBBSReadTooManyBytes.Create(Count : Integer; Dummy : Word);
begin
  inherited Create(Format(AbStrRes(AbBBSReadTooManyBytes), [Count]));
  ErrorCode := AbBBSReadTooManyBytes;
end;

constructor EAbBBSSeekOutsideBuffer.Create;
begin
  inherited Create(AbStrRes(AbBBSSeekOutsideBuffer));
  ErrorCode := AbBBSSeekOutsideBuffer;
end;

constructor EAbBBSInvalidOrigin.Create;
begin
  inherited Create(AbStrRes(AbBBSInvalidOrigin));
  ErrorCode := AbBBSInvalidOrigin;
end;

constructor EAbBBSWriteTooManyBytes.Create(Count : Integer; Dummy : Word);
begin
  inherited Create(Format(AbStrRes(AbBBSWriteTooManyBytes), [Count]));
  ErrorCode := AbBBSWriteTooManyBytes;
end;

constructor EAbVMSErrorOpenSwap.Create( const Value : string );
begin
  inherited Create(Format(AbStrRes(AbVMSErrorOpenSwap), [Value]));
  ErrorCode := AbVMSErrorOpenSwap;
end;

constructor EAbVMSSeekFail.Create( const Value : string );
begin
  inherited Create(Format(AbStrRes(AbVMSSeekFail), [Value]));
  ErrorCode := AbVMSSeekFail;
end;

constructor EAbVMSReadFail.Create( Count : Integer; const Value : string );
begin
  inherited Create(Format(AbStrRes(AbVMSReadFail), [Count, Value]));
  ErrorCode := AbVMSReadFail;
end;

constructor EAbVMSWriteFail.Create( Count : Integer; const Value : string );
begin
  inherited Create(Format(AbStrRes(AbVMSWriteFail), [Count, Value]));
  ErrorCode := AbVMSWriteFail;
end;

constructor EAbVMSWriteTooManyBytes.Create( Count : Integer;
                                            Dummy : Word );
begin
  inherited Create(Format(AbStrRes(AbVMSWriteTooManyBytes), [Count]));
  ErrorCode := AbVMSWriteTooManyBytes;
end;

constructor EAbSWSNotEndofStream.Create;
begin
  inherited Create(AbStrRes(AbSWSNotEndofStream));
  ErrorCode := AbSWSNotEndofStream;
end;

constructor EAbSWSSeekFailed.Create;
begin
  inherited Create(AbStrRes(AbSWSSeekFailed));
  ErrorCode := AbSWSSeekFailed;
end;

constructor EAbSWSWriteFailed.Create;
begin
  inherited Create(AbStrRes(AbSWSWriteFailed));
  ErrorCode := AbSWSWriteFailed;
end;

constructor EAbSWSInvalidOrigin.Create;
begin
  inherited Create(AbStrRes(AbSWSInvalidOrigin));
  ErrorCode := AbSWSInvalidOrigin;
end;

constructor EAbSWSInvalidNewOrigin.Create;
begin
  inherited Create(AbStrRes(AbSWSInvalidNewOrigin));
  ErrorCode := AbSWSInvalidNewOrigin;
end;

constructor EAbFCIFileOpenError.Create;
begin
  inherited Create(AbStrRes(AbFCIFileOpenError));
  ErrorCode := AbFCIFileOpenError;
end;

constructor EAbNoCabinetDll.Create;
begin
  inherited Create(AbStrRes(AbNoCabinetDllError));
  ErrorCode := AbNoCabinetDllError;
end;

constructor EAbFCIFileReadError.Create;
begin
  inherited Create(AbStrRes(AbFCIFileReadError));
  ErrorCode := AbFCIFileReadError;
end;

constructor EAbFCIFileWriteError.Create;
begin
  inherited Create(AbStrRes(AbFCIFileWriteError));
  ErrorCode := AbFCIFileWriteError;
end;

constructor EAbFCIFileCloseError.Create;
begin
  inherited Create(AbStrRes(AbFCIFileCloseError));
  ErrorCode := AbFCIFileCloseError;
end;

constructor EAbFCIFileSeekError.Create;
begin
  inherited Create(AbStrRes(AbFCIFileSeekError));
  ErrorCode := AbFCIFileSeekError;
end;

constructor EAbFCIFileDeleteError.Create;
begin
  inherited Create(AbStrRes(AbFCIFileDeleteError));
  ErrorCode := AbFCIFileDeleteError;
end;

constructor EAbFCIAddFileError.Create;
begin
  inherited Create(AbStrRes(AbFCIAddFileError));
  ErrorCode := AbFCIAddFileError;
end;

constructor EAbFCICreateError.Create;
begin
  inherited Create(AbStrRes(AbFCICreateError));
  ErrorCode := AbFCICreateError;
end;

constructor EAbFCIFlushCabinetError.Create;
begin
  inherited Create(AbStrRes(AbFCIFlushCabinetError));
  ErrorCode := AbFCIFlushCabinetError;
end;

constructor EAbFCIFlushFolderError.Create;
begin
  inherited Create(AbStrRes(AbFCIFlushFolderError));
  ErrorCode := AbFCIFlushFolderError;
end;

constructor EAbFDICopyError.Create;
begin
  inherited Create(AbStrRes(AbFDICopyError));
  ErrorCode := AbFDICopyError;
end;

constructor EAbFDICreateError.Create;
begin
  inherited Create(AbStrRes(AbFDICreateError));
  ErrorCode := AbFDICreateError;
end;

constructor EAbInvalidCabTemplate.Create;
begin
  inherited Create(AbStrRes(AbInvalidCabTemplate));
  ErrorCode := AbInvalidCabTemplate;
end;

constructor EAbInvalidCabFile.Create;
begin
  inherited Create(AbStrRes(AbInvalidCabFile));
  ErrorCode := AbInvalidCabFile;
end;

procedure AbConvertException( const E : Exception;
                              var eClass : TAbErrorClass;
                              var eErrorCode : Integer );
begin
  eClass := ecOther;
  eErrorCode := 0;
  if E is EAbException then begin
    eClass := ecAbbrevia;
    eErrorCode := (E as EAbException).ErrorCode;
  end
  else if E is EInOutError then begin
    eClass := ecInOutError;
    eErrorCode := (E as EInOutError).ErrorCode;
  end
  else if E is EFilerError then
    eClass := ecFilerError
  else if E is EFOpenError then
    eClass := ecFileOpenError
  else if E is EFCreateError then
    eClass := ecFileCreateError;
end;

{ EAbUnhandledType }

constructor EAbUnhandledType.Create;
begin
  inherited Create(AbStrRes(AbUnhandledFileType));
  ErrorCode := AbUnhandledFileType;
end;

{ EAbGzipBadCRC }

constructor EAbGzipBadCRC.Create;
begin
  inherited Create(AbStrRes(AbGzipBadCRC));
  ErrorCode := AbGzipBadCRC;
end;

{ EAbGzipBadFileSize }

constructor EAbGzipBadFileSize.Create;
begin
  inherited Create(AbStrRes(AbGzipBadFileSize));
  ErrorCode := AbGzipBadFileSize;
end;

{ EAbGzipInvalid }

constructor EAbGzipInvalid.Create;
begin
  inherited Create(AbStrRes(AbSpanningNotSupported));
  ErrorCode := AbSpanningNotSupported;

end;


{ EAbBzipBadCRC }

constructor EAbBzipBadCRC.Create;
begin
  inherited Create(AbStrRes(AbBzipBadCRC));
  ErrorCode := AbBzipBadCRC;
end;

{ EAbBzipBadFileSize }

constructor EAbBzipBadFileSize.Create;
begin
  inherited Create(AbStrRes(AbBzipBadFileSize));
  ErrorCode := AbBzipBadFileSize;
end;

{ EAbBzipInvalid }

constructor EAbBzipInvalid.Create;
begin
  inherited Create(AbStrRes(AbSpanningNotSupported));
  ErrorCode := AbSpanningNotSupported;

end;

{ EAbSpanningNotSupported }

constructor EAbSpanningNotSupported.Create;
begin
  inherited Create(AbStrRes(AbSpanningNotSupported));
  ErrorCode := AbSpanningNotSupported;
end;


{ EAbInvalidHeader }

constructor EAbInvalidHeaderException.Create;
begin
	inherited Create('Invalid Header');
  ErrorCode := AbInvalidHeader;
end;

{ EAbFileTooLarge }

constructor EAbFileTooLarge.Create;
begin
    {TODO Create const and fix wording}
    inherited Create('File size is too big for archive type'); 
end;

end.
