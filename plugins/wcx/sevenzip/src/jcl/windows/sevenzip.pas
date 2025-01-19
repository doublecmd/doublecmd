{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ interface of the 'sevenzip' (http://sourceforge.net/projects/sevenzip/) compression library      }
{ version 4.62, December 2th, 2008                                                                 }
{                                                                                                  }
{ Copyright (C) 1999-2008 Igor Pavlov                                                              }
{                                                                                                  }
{ GNU LGPL information                                                                             }
{ --------------------                                                                             }
{                                                                                                  }
{    This library is free software; you can redistribute it and/or modify it under the terms of    }
{    the GNU Lesser General Public License as published by the Free Software Foundation; either    }
{    version 2.1 of the License, or (at your option) any later version.                            }
{                                                                                                  }
{    This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;     }
{    without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.     }
{    See the GNU Lesser General Public License for more details.                                   }
{                                                                                                  }
{    You should have received a copy of the GNU Lesser General Public License along with this      }
{    library; if not, write to                                                                     }
{    the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   }
{                                                                                                  }
{ unRAR restriction                                                                                }
{ -----------------                                                                                }
{                                                                                                  }
{    The decompression engine for RAR archives was developed using source code of unRAR program.   }
{    All copyrights to original unRAR code are owned by Alexander Roshal.                          }
{                                                                                                  }
{    The license for original unRAR code has the following restriction:                            }
{                                                                                                  }
{      The unRAR sources cannot be used to re-create the RAR compression algorithm,                }
{      which is proprietary. Distribution of modified unRAR sources in separate form               }
{      or as a part of other software is permitted, provided that it is clearly                    }
{      stated in the documentation and source comments that the code may                           }
{      not be used to develop a RAR (WinRAR) compatible archiver.                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Translation 2007-2008 Florent Ouchet for the JEDI Code Library                                   }
{ Contributors:                                                                                    }
{   Uwe Schuster (uschuster)                                                                       }
{   Jan Goyvaerts (jgsoft)                                                                         }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit sevenzip;

{$mode delphi}

interface

{$DEFINE 7ZIP_LINKONREQUEST}

uses
  {$IFDEF HAS_UNITSCOPE}
  Winapi.ActiveX, Winapi.Windows,
  {$ELSE ~HAS_UNITSCOPE}
  ActiveX, Windows,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils,
  DCJclAlternative;

//DOM-IGNORE-BEGIN

// Guid.txt  
const
  CLSID_CCodec : TGUID = '{23170F69-40C1-2790-0000-000000000000}';
  CLSID_CCodecBCJ2    : TGUID = '{23170F69-40C1-2790-1B01-030300000000}'; // BCJ2 0303011B
  CLSID_CCodecBCJ     : TGUID = '{23170F69-40C1-2790-0301-030300000000}'; // BCJ  03030103
  CLSID_CCodecSWAP2   : TGUID = '{23170F69-40C1-2790-0203-030000000000}'; // swap2 020302
  CLSID_CCodecSWAP4   : TGUID = '{23170F69-40C1-2790-0403-020000000000}'; // swap4 020304
  CLSID_CCodecBPPC    : TGUID = '{23170F69-40C1-2790-0502-030300000000}'; // branch ppc 03030205
  CLSID_CCodecBIA64   : TGUID = '{23170F69-40C1-2790-0104-030300000000}'; // branch IA64 03030401
  CLSID_CCodecBARM    : TGUID = '{23170F69-40C1-2790-0105-030300000000}'; // branch ARM  03030501
  CLSID_CCodecBARMT   : TGUID = '{23170F69-40C1-2790-0107-030300000000}'; // branch ARM Thumb 03030701
  CLSID_CCodecBARMS   : TGUID = '{23170F69-40C1-2790-0508-030300000000}'; // branch ARM Sparc 03030805
  CLSID_CCodecBZIP    : TGUID = '{23170F69-40C1-2790-0202-040000000000}'; // bzip2 040202
  CLSID_CCodecCOPY    : TGUID = '{23170F69-40C1-2790-0000-000000000000}'; // copy 0
  CLSID_CCodecDEF64   : TGUID = '{23170F69-40C1-2790-0901-040000000000}'; // deflate64 040109
  CLSID_CCodecDEFNSIS : TGUID = '{23170F69-40C1-2790-0109-040000000000}'; // deflate nsis 040901
  CLSID_CCodecDEFREG  : TGUID = '{23170F69-40C1-2790-0801-040000000000}'; // deflate register 040108
  CLSID_CCodecLZMA    : TGUID = '{23170F69-40C1-2790-0101-030000000000}'; // lzma 030101
  CLSID_CCodecPPMD    : TGUID = '{23170F69-40C1-2790-0104-030000000000}'; // ppmd 030401
  CLSID_CCodecRAR1    : TGUID = '{23170F69-40C1-2790-0103-040000000000}'; // rar1 040301
  CLSID_CCodecRAR2    : TGUID = '{23170F69-40C1-2790-0203-040000000000}'; // rar2 040302
  CLSID_CCodecRAR3    : TGUID = '{23170F69-40C1-2790-0303-040000000000}'; // rar3 040303
  CLSID_CAESCodec     : TGUID = '{23170F69-40C1-2790-0107-F10600000000}'; // AES 06F10701

  CLSID_CArchiveHandler : TGUID = '{23170F69-40C1-278A-1000-000110000000}';
  CLSID_CFormatZip      : TGUID = '{23170F69-40C1-278A-1000-000110010000}';
  CLSID_CFormatBZ2      : TGUID = '{23170F69-40C1-278A-1000-000110020000}';
  CLSID_CFormatRar      : TGUID = '{23170F69-40C1-278A-1000-000110030000}';
  CLSID_CFormatArj      : TGUID = '{23170F69-40C1-278A-1000-000110040000}';
  CLSID_CFormatZ        : TGUID = '{23170F69-40C1-278A-1000-000110050000}';
  CLSID_CFormatLzh      : TGUID = '{23170F69-40C1-278A-1000-000110060000}';
  CLSID_CFormat7z       : TGUID = '{23170F69-40C1-278A-1000-000110070000}';
  CLSID_CFormatCab      : TGUID = '{23170F69-40C1-278A-1000-000110080000}';
  CLSID_CFormatNsis     : TGUID = '{23170F69-40C1-278A-1000-000110090000}';
  CLSID_CFormatLzma     : TGUID = '{23170F69-40C1-278A-1000-0001100A0000}';
  CLSID_CFormatLzma86   : TGUID = '{23170F69-40C1-278A-1000-0001100B0000}';
  CLSID_CFormatXz       : TGUID = '{23170F69-40C1-278A-1000-0001100C0000}';
  CLSID_CFormatPpmd     : TGUID = '{23170F69-40C1-278A-1000-0001100D0000}';
  CLSID_CFormatExt      : TGUID = '{23170F69-40C1-278A-1000-000110C70000}';
  CLSID_CFormatVMDK     : TGUID = '{23170F69-40C1-278A-1000-000110C80000}';
  CLSID_CFormatVDI      : TGUID = '{23170F69-40C1-278A-1000-000110C90000}';
  CLSID_CFormatQcow     : TGUID = '{23170F69-40C1-278A-1000-000110CA0000}';
  CLSID_CFormatGPT      : TGUID = '{23170F69-40C1-278A-1000-000110CB0000}';
  CLSID_CFormatRar5     : TGUID = '{23170F69-40C1-278A-1000-000110CC0000}';
  CLSID_CFormatIHex     : TGUID = '{23170F69-40C1-278A-1000-000110CD0000}';
  CLSID_CFormatHxs      : TGUID = '{23170F69-40C1-278A-1000-000110CE0000}';
  CLSID_CFormatTE       : TGUID = '{23170F69-40C1-278A-1000-000110CF0000}';
  CLSID_CFormatUEFIc    : TGUID = '{23170F69-40C1-278A-1000-000110D00000}';
  CLSID_CFormatUEFIs    : TGUID = '{23170F69-40C1-278A-1000-000110D10000}';
  CLSID_CFormatSquashFS : TGUID = '{23170F69-40C1-278A-1000-000110D20000}';
  CLSID_CFormatCramFS   : TGUID = '{23170F69-40C1-278A-1000-000110D30000}';
  CLSID_CFormatAPM      : TGUID = '{23170F69-40C1-278A-1000-000110D40000}';
  CLSID_CFormatMslz     : TGUID = '{23170F69-40C1-278A-1000-000110D50000}';
  CLSID_CFormatFlv      : TGUID = '{23170F69-40C1-278A-1000-000110D60000}';
  CLSID_CFormatSwf      : TGUID = '{23170F69-40C1-278A-1000-000110D70000}';
  CLSID_CFormatSwfc     : TGUID = '{23170F69-40C1-278A-1000-000110D80000}';
  CLSID_CFormatNtfs     : TGUID = '{23170F69-40C1-278A-1000-000110D90000}';
  CLSID_CFormatFat      : TGUID = '{23170F69-40C1-278A-1000-000110DA0000}';
  CLSID_CFormatMbr      : TGUID = '{23170F69-40C1-278A-1000-000110DB0000}';
  CLSID_CFormatVhd      : TGUID = '{23170F69-40C1-278A-1000-000110DC0000}';
  CLSID_CFormatPe       : TGUID = '{23170F69-40C1-278A-1000-000110DD0000}';
  CLSID_CFormatElf      : TGUID = '{23170F69-40C1-278A-1000-000110DE0000}';
  CLSID_CFormatMacho    : TGUID = '{23170F69-40C1-278A-1000-000110DF0000}';
  CLSID_CFormatUdf      : TGUID = '{23170F69-40C1-278A-1000-000110E00000}';
  CLSID_CFormatXar      : TGUID = '{23170F69-40C1-278A-1000-000110E10000}';
  CLSID_CFormatMub      : TGUID = '{23170F69-40C1-278A-1000-000110E20000}';
  CLSID_CFormatHfs      : TGUID = '{23170F69-40C1-278A-1000-000110E30000}';
  CLSID_CFormatDmg      : TGUID = '{23170F69-40C1-278A-1000-000110E40000}';
  CLSID_CFormatCompound : TGUID = '{23170F69-40C1-278A-1000-000110E50000}';
  CLSID_CFormatWim      : TGUID = '{23170F69-40C1-278A-1000-000110E60000}';
  CLSID_CFormatIso      : TGUID = '{23170F69-40C1-278A-1000-000110E70000}';
  //CLSID_CFormatBkf      : TGUID = '{23170F69-40C1-278A-1000-000110E80000}';  not in 4.57
  CLSID_CFormatChm      : TGUID = '{23170F69-40C1-278A-1000-000110E90000}';
  CLSID_CFormatSplit    : TGUID = '{23170F69-40C1-278A-1000-000110EA0000}';
  CLSID_CFormatRpm      : TGUID = '{23170F69-40C1-278A-1000-000110EB0000}';
  CLSID_CFormatDeb      : TGUID = '{23170F69-40C1-278A-1000-000110EC0000}';
  CLSID_CFormatCpio     : TGUID = '{23170F69-40C1-278A-1000-000110ED0000}';
  CLSID_CFormatTar      : TGUID = '{23170F69-40C1-278A-1000-000110EE0000}';
  CLSID_CFormatGZip     : TGUID = '{23170F69-40C1-278A-1000-000110EF0000}';

// IStream.h
type
  // "23170F69-40C1-278A-0000-000300xx0000"
  ISequentialInStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300010000}']
    function Read(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; winapi;
    {Out: if size != 0, return_value = S_OK and (*processedSize == 0),
     then there are no more bytes in stream.
     if (size > 0) && there are bytes in stream,
     this function must read at least 1 byte.
     This function is allowed to read less than number of remaining bytes in stream.
     You must call Read function in loop, if you need exact amount of data}
  end;

  ISequentialOutStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300020000}']
    function Write(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; winapi;
    {if (size > 0) this function must write at least 1 byte.
     This function is allowed to write less than "size".
     You must call Write function in loop, if you need to write exact amount of data}
  end;

  IInStream = interface(ISequentialInStream)
    ['{23170F69-40C1-278A-0000-000300030000}']
    function Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT; winapi;
  end;

  IOutStream = interface(ISequentialOutStream)
    ['{23170F69-40C1-278A-0000-000300040000}']
    function Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT; winapi;
    function SetSize(NewSize: Int64): HRESULT; winapi;
  end;

  IStreamGetSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300060000}']
    function GetSize(Size: PInt64): HRESULT; winapi;
  end;

  IOutStreamFlush = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300070000}']
    function Flush: HRESULT; winapi;
  end;

// PropID.h
const
  kpidNoProperty = 0;
  kpidMainSubfile = 1;
  kpidHandlerItemIndex = 2;
  kpidPath = 3;
  kpidName = 4;
  kpidExtension = 5;
  kpidIsFolder = 6 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kpidIsDir' {$ENDIF} {$ENDIF};
  kpidIsDir = 6;
  kpidSize = 7;
  kpidPackedSize = 8 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kpidPackSize' {$ENDIF} {$ENDIF};
  kpidPackSize = 8;
  kpidAttributes = 9 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kpidAttrib' {$ENDIF} {$ENDIF};
  kpidAttrib = 9;
  kpidCreationTime = 10 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kpidCTime' {$ENDIF} {$ENDIF};
  kpidCTime = 10;
  kpidLastAccessTime = 11 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kpidATime' {$ENDIF} {$ENDIF};
  kpidATime = 11;
  kpidLastWriteTime = 12 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kpidMTime' {$ENDIF} {$ENDIF};
  kpidMTime = 12;
  kpidSolid = 13;
  kpidCommented = 14;
  kpidEncrypted = 15;
  kpidSplitBefore = 16;
  kpidSplitAfter = 17;
  kpidDictionarySize = 18;
  kpidCRC = 19;
  kpidType = 20;
  kpidIsAnti = 21;
  kpidMethod = 22;
  kpidHostOS = 23;
  kpidFileSystem = 24;
  kpidUser = 25;
  kpidGroup = 26;
  kpidBlock = 27;
  kpidComment = 28;
  kpidPosition = 29;
  kpidPrefix = 30;
  kpidNumSubDirs = 31;
  kpidNumSubFiles = 32;
  kpidUnpackVer = 33;
  kpidVolume = 34;
  kpidIsVolume = 35;
  kpidOffset = 36;
  kpidLinks = 37;
  kpidNumBlocks = 38;
  kpidNumVolumes = 39;
  kpidTimeType = 40;
  kpidBit64 = 41;
  kpidBigEndian = 42;
  kpidCpu = 43;
  kpidPhySize = 44;
  kpidHeadersSize = 45;
  kpidChecksum = 46;
  kpidCharacts = 47;
  kpidVa = 48;
  kpidId = 49;
  kpidShortName = 50;
  kpidCreatorApp = 51;
  kpidSectorSize = 52;
  kpidPosixAttrib = 53;
  kpidLink = 54;

  kpidIsAltStream = 63;

  kpidTotalSize = $1100;
  kpidFreeSpace = $1101;
  kpidClusterSize = $1102;
  kpidVolumeName = $1103;

  kpidLocalName = $1200;
  kpidProvider = $1201;

  kpidUserDefined = $10000;

// HandlerOut.cpp

  kCopyMethodName = WideString('Copy');
  kLZMAMethodName = WideString('LZMA');
  kLZMA2MethodName = WideString('LZMA2');
  kBZip2MethodName = WideString('BZip2');
  kPpmdMethodName = WideString('PPMd');
  kDeflateMethodName = WideString('Deflate');
  kDeflate64MethodName = WideString('Deflate64');

  kAES128MethodName = WideString('AES128');
  kAES192MethodName = WideString('AES192');
  kAES256MethodName = WideString('AES256');
  kZipCryptoMethodName = WideString('ZIPCRYPTO');

// ICoder.h
type
  ICompressProgressInfo = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400040000}']
    function SetRatioInfo(InSize: PInt64; OutSize: PInt64): HRESULT; winapi;
  end;

  ICompressCoder = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400050000}']
    function Code(InStream: ISequentialInStream; OutStream: ISequentialOutStream;
      InSize, OutSize: PInt64; Progress: ICompressProgressInfo): HRESULT; winapi;
  end;

  PISequentialInStream = ^ISequentialInStream;
  PISequentialOutStream = ^ISequentialOutStream;

  ICompressCoder2 = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400180000}']
    function Code(InStreams: PISequentialInStream; InSizes: JclBase.PPInt64; NumInStreams: Cardinal;
      OutStreams: PISequentialOutStream; OutSizes: JclBase.PPInt64; NumOutStreams: Cardinal;
      Progress: ICompressProgressInfo): HRESULT; winapi;
  end;

const
  kDictionarySize = $400;
  kUsedMemorySize = $401;
  kOrder = $402;
  kBlockSize = $403;
  kPosStateBits = $440;
  kLitContextBits = $441;
  kLitPosBits = $442;
  kNumFastBytes = $450;
  kMatchFinder = $451;
  kMatchFinderCycles = $452;
  kNumPasses = $460;
  kAlgorithm = $470;
  kMultiThread = $480;
  kNumThreads = $481;
  kEndMarker = $490;

type
  ICompressSetCoderProperties = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400200000}']
    function SetCoderProperties(PropIDs: PPropID; Properties: PPropVariant;
      NumProperties: Cardinal): HRESULT; winapi;
  end;

  ICompressSetDecoderProperties2 = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400220000}']
    function SetDecoderProperties2(Data: PByte; Size: Cardinal): HRESULT; winapi;
  end;

  ICompressWriteCoderProperties = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400230000}']
    function WriteCoderProperties(OutStream: ISequentialOutStream): HRESULT; winapi;
  end;

  ICompressGetInStreamProcessedSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400240000}']
    function GetInStreamProcessedSize(Value: PInt64): HRESULT; winapi;
  end;

  ICompressSetCoderMt = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400250000}']
    function SetNumberOfThreads(NumThreads: Cardinal): HRESULT; winapi;
  end;

  ICompressGetSubStreamSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400300000}']
    function GetSubStreamSize(SubStream: Int64; out Value: Int64): HRESULT; winapi;
  end;

  ICompressSetInStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400310000}']
    function SetInStream(InStream: ISequentialInStream): HRESULT; winapi;
    function ReleaseInStream: HRESULT; winapi;
  end;

  ICompressSetOutStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400320000}']
    function SetOutStream(OutStream: ISequentialOutStream): HRESULT; winapi;
    function ReleaseOutStream: HRESULT; winapi;
  end;

  ICompressSetInStreamSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400330000}']
    function SetInStreamSize(InSize: PInt64): HRESULT; winapi;
  end;

  ICompressSetOutStreamSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400340000}']
    function SetOutStreamSize(OutSize: PInt64): HRESULT; winapi;
  end;

  ICompressFilter = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400400000}']
    function Init: HRESULT; winapi;
    function Filter(Data: PByte; Size: Cardinal): Cardinal; winapi;
    // Filter return outSize (UInt32)
    // if (outSize <= size): Filter have converted outSize bytes
    // if (outSize > size): Filter have not converted anything.
    //      and it needs at least outSize bytes to convert one block
    //      (it's for crypto block algorithms).
  end;

  ICompressCodecsInfo = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400600000}']
    function GetNumberOfMethods(NumMethods: PCardinal): HRESULT; winapi;
    function GetProperty(Index: Cardinal; PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
    function CreateDecoder(Index: Cardinal; IID: PGUID; out Decoder): HRESULT; winapi;
    function CreateEncoder(Index: Cardinal; IID: PGUID; out Coder): HRESULT; winapi;
  end;

  ISetCompressCodecsInfo = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400610000}']
    function SetCompressCodecsInfo(CompressCodecsInfo: ICompressCodecsInfo): HRESULT; winapi;
  end;

  ICryptoProperties = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400800000}']
    function SetKey(Data: PByte; Size: Cardinal): HRESULT; winapi;
    function SetInitVector(Data: PByte; Size: Cardinal): HRESULT; winapi;
  end;

  ICryptoSetPassword = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400900000}']
    function CryptoSetPassword(Data: PByte; Size: Cardinal): HRESULT; winapi;
  end;

  ICryptoSetCRC = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400A00000}']
    function CryptoSetCRC(crc: Cardinal): HRESULT; winapi;
  end;

const
  kID = 0;
  kName = 1;
  kDecoder = 2;
  kEncoder = 3;
  kInStreams = 4;
  kOutStreams = 5;
  kDescription = 6;
  kDecoderIsAssigned = 7;
  kEncoderIsAssigned = 8;

const
  // namespace NModuleInterfaceType
  (*
    virtual destructor in IUnknown:
    - no  : 7-Zip (Windows)
    - no  : 7-Zip (Linux) (v23) in default mode
    - yes : p7zip
    - yes : 7-Zip (Linux) before v23
    - yes : 7-Zip (Linux) (v23), if Z7_USE_VIRTUAL_DESTRUCTOR_IN_IUNKNOWN is defined
  *)
  k_IUnknown_VirtDestructor_No  = 0;
  k_IUnknown_VirtDestructor_Yes = 1;

  // namespace NModulePropID
  kInterfaceType = 0;  // VT_UI4
  kVersion       = 1;  // VT_UI4

// IProgress.h
type
  IProgress = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000000050000}']
    function SetTotal(Total: Int64): HRESULT; winapi;
    function SetCompleted(CompleteValue: PInt64): HRESULT; winapi;
  end;

// IArchive.h
const
  // file time type
  kWindows = 0;
  kUnix = 1;
  kDOS = 2;

  // archive
  kArchiveName = 0;
  kClassID = 1;
  kExtension = 2;
  kAddExtension = 3;
  kUpdate = 4;
  kKeepName = 5;
  kStartSignature = 6;
  kFinishSignature = 7;
  kAssociate = 8;

  // ask mode
  kExtract = 0;
  kTest = 1;
  kSkip = 2;

  // operation result
  kOK = 0;
  kUnSupportedMethod = 1;
  kDataError = 2;
  kCRCError = 3;

  kError = 1;

type
  // "23170F69-40C1-278A-0000-000600xx0000"
  IArchiveOpenCallback = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600100000}']
    function SetTotal(Files: PInt64; Bytes: PInt64): HRESULT; winapi;
    function SetCompleted(Files: PInt64; Bytes: PInt64): HRESULT; winapi;
  end;

  IArchiveExtractCallback = interface(IProgress)
    ['{23170F69-40C1-278A-0000-000600200000}']
    function GetStream(Index: Cardinal; out OutStream: ISequentialOutStream;
      askExtractMode: Cardinal): HRESULT; winapi;
    // GetStream OUT: S_OK - OK, S_FALSE - skeep this file
    function PrepareOperation(askExtractMode: Cardinal): HRESULT; winapi;
    function SetOperationResult(resultEOperationResult: Integer): HRESULT; winapi;
  end;

  IArchiveOpenVolumeCallback = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600300000}']
    function GetProperty(PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
    function GetStream(Name: PWideChar; out InStream: IInStream): HRESULT; winapi;
  end;

  IInArchiveGetStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600400000}']
    function GetStream(Index: Cardinal; out Stream: ISequentialInStream): HRESULT; winapi;
  end;

  IArchiveOpenSetSubArchiveName = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600500000}']
    function SetSubArchiveName(Name: PWideChar): HRESULT; winapi;
  end;

  IInArchive = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600600000}']
    function Open(Stream: IInStream; MaxCheckStartPosition: PInt64;
       OpenArchiveCallback: IArchiveOpenCallback): HRESULT; winapi;
    function Close: HRESULT; winapi;
    function GetNumberOfItems(NumItems: PCardinal): HRESULT; winapi;
    function GetProperty(Index: Cardinal; PropID: TPropID;
      var Value: TPropVariant): HRESULT; winapi;
    function Extract(Indices: PCardinal; NumItems: Cardinal;
      TestMode: Integer; ExtractCallback: IArchiveExtractCallback): HRESULT; winapi;
    // indices must be sorted
    // numItems = 0xFFFFFFFF means all files
    // testMode != 0 means "test files operation"
    function GetArchiveProperty(PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;

    function GetNumberOfProperties(NumProperties: PCardinal): HRESULT; winapi;
    function GetPropertyInfo(Index: Cardinal; out Name: TBStr; out PropID: TPropID;
      out VarType: TVarType): HRESULT; winapi;

    function GetNumberOfArchiveProperties(NumProperties: PCardinal): HRESULT; winapi;
    function GetArchivePropertyInfo(Index: Cardinal; out Name: TBStr; out PropID: TPropID;
      out VarType: TVarType): HRESULT; winapi;
  end;

  IArchiveUpdateCallback = interface(IProgress)
    ['{23170F69-40C1-278A-0000-000600800000}']
    function GetUpdateItemInfo(Index: Cardinal;
      NewData: PInteger;        // 1 - new data, 0 - old data
      NewProperties: PInteger;  // 1 - new properties, 0 - old properties
      IndexInArchive: PCardinal // -1 if there is no in archive, or if doesn't matter
      ): HRESULT; winapi;
    function GetProperty(Index: Cardinal; PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
    function GetStream(Index: Cardinal; out InStream: ISequentialInStream): HRESULT; winapi;
    function SetOperationResult(OperationResult: Integer): HRESULT; winapi;
  end;

  IArchiveUpdateCallback2 = interface(IArchiveUpdateCallback)
    ['{23170F69-40C1-278A-0000-000600820000}']
    function GetVolumeSize(Index: Cardinal; Size: PInt64): HRESULT; winapi;
    function GetVolumeStream(Index: Cardinal; out VolumeStream: ISequentialOutStream): HRESULT; winapi;
  end;

  IOutArchive = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600A00000}']
    function UpdateItems(OutStream: ISequentialOutStream; NumItems: Cardinal;
      UpdateCallback: IArchiveUpdateCallback): HRESULT; winapi;
    function GetFileTimeType(Type_: PCardinal): HRESULT; winapi;
  end;

  ISetProperties = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600030000}']
    function SetProperties(Names: PPWideChar; Values: PPropVariant; NumProperties: Integer): HRESULT; winapi;
  end;

// IPassword.h
type
  ICryptoGetTextPassword = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000500100000}']
    function CryptoGetTextPassword(password: PBStr): HRESULT; winapi;
  end;

  ICryptoGetTextPassword2 = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000500110000}']
    function CryptoGetTextPassword2(PasswordIsDefined: PInteger;
      Password: PBStr): HRESULT; winapi;
  end;

// ZipHandlerOut.cpp
const
  kDeflateAlgoX1 = 0 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kLzAlgoX1' {$ENDIF} {$ENDIF};
  kLzAlgoX1 = 0;
  kDeflateAlgoX5 = 1 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kLzAlgoX5' {$ENDIF} {$ENDIF};
  kLzAlgoX5 = 1;

  kDeflateNumPassesX1  = 1;
  kDeflateNumPassesX7  = 3;
  kDeflateNumPassesX9  = 10;

  kNumFastBytesX1 = 32 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kDeflateNumFastBytesX1' {$ENDIF} {$ENDIF};
  kDeflateNumFastBytesX1 = 32;
  kNumFastBytesX7 = 64 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kDeflateNumFastBytesX7' {$ENDIF} {$ENDIF};
  kDeflateNumFastBytesX7 = 64;
  kNumFastBytesX9 = 128 {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use kDeflateNumFastBytesX9' {$ENDIF} {$ENDIF};
  kDeflateNumFastBytesX9 = 128;

  kLzmaNumFastBytesX1 = 32;
  kLzmaNumFastBytesX7 = 64;

  kBZip2NumPassesX1 = 1;
  kBZip2NumPassesX7 = 2;
  kBZip2NumPassesX9 = 7;

  kBZip2DicSizeX1 = 100000;
  kBZip2DicSizeX3 = 500000;
  kBZip2DicSizeX5 = 900000;

// HandlerOut.cpp
const
  kLzmaAlgoX1 = 0;
  kLzmaAlgoX5 = 1;

  kLzmaDicSizeX1 = 1 shl 16;
  kLzmaDicSizeX3 = 1 shl 20;
  kLzmaDicSizeX5 = 1 shl 24;
  kLzmaDicSizeX7 = 1 shl 25;
  kLzmaDicSizeX9 = 1 shl 26;

  kLzmaFastBytesX1 = 32;
  kLzmaFastBytesX7 = 64;

  kPpmdMemSizeX1 = (1 shl 22);
  kPpmdMemSizeX5 = (1 shl 24);
  kPpmdMemSizeX7 = (1 shl 26);
  kPpmdMemSizeX9 = (192 shl 20);

  kPpmdOrderX1 = 4;
  kPpmdOrderX5 = 6;
  kPpmdOrderX7 = 16;
  kPpmdOrderX9 = 32;

  kDeflateFastBytesX1 = 32;
  kDeflateFastBytesX7 = 64;
  kDeflateFastBytesX9 = 128;

{$IFDEF 7ZIP_LINKONREQUEST}
type
  TCreateObjectFunc = function (ClsID: PGUID; IID: PGUID; out Obj): HRESULT; winapi;
  TGetHandlerProperty2 = function (FormatIndex: Cardinal; PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
  TGetHandlerProperty = function (PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
  TGetMethodProperty = function (CodecIndex: Cardinal; PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
  TGetModuleProp = function(PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
  TGetNumberOfFormatsFunc = function (NumFormats: PCardinal): HRESULT; winapi;
  TGetNumberOfMethodsFunc = function (NumMethods: PCardinal): HRESULT; winapi;
  TSetLargePageMode = function: HRESULT; winapi;

var
  CreateObject: TCreateObjectFunc = nil;
  GetHandlerProperty2: TGetHandlerProperty2 = nil;
  GetHandlerProperty: TGetHandlerProperty = nil;
  GetMethodProperty: TGetMethodProperty = nil;
  GetModuleProp: TGetModuleProp = nil;
  GetNumberOfFormats: TGetNumberOfFormatsFunc = nil;
  GetNumberOfMethods: TGetNumberOfMethodsFunc = nil;
  SetLargePageMode: TSetLargePageMode = nil;

{$IF DEFINED(UNIX)}
  SysAllocString: function(psz: pointer): TBStr; winapi;
  SysFreeString: procedure(bstr:pointer); winapi;
  SysStringByteLen: function(bstr:pointer): UINT; winapi;
{$ENDIF}

{$ELSE ~7ZIP_LINKONREQUEST}
function CreateObject(ClsID: PGUID; IID: PGUID; out Obj): HRESULT; winapi;
function GetHandlerProperty2(FormatIndex: Cardinal; PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
function GetHandlerProperty(PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
function GetMethodProperty(CodecIndex: Cardinal; PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
function GetNumberOfFormats(NumFormats: PCardinal): HRESULT; winapi;
function GetNumberOfMethods(NumMethods: PCardinal): HRESULT; winapi;
function SetLargePageMode: HRESULT; winapi;
{$ENDIF ~7ZIP_LINKONREQUEST}

//DOM-IGNORE-END

const
  SevenzipDefaultLibraryName = '7z.' + SharedSuffix;
  CreateObjectDefaultExportName = 'CreateObject';
  GetHandlerProperty2DefaultExportName = 'GetHandlerProperty2';
  GetHandlerPropertyDefaultExportName = 'GetHandlerProperty';
  GetMethodPropertyDefaultExportName = 'GetMethodProperty';
  GetModulePropDefaultExportName = 'GetModuleProp';
  GetNumberOfFormatsDefaultExportName = 'GetNumberOfFormats';
  GetNumberOfMethodsDefaultExportName = 'GetNumberOfMethods';
  SetLargePageModeDefaultExportName = 'SetLargePageMode';

{$IFDEF 7ZIP_LINKONREQUEST}
var
  SevenzipLibraryName: string = SevenzipDefaultLibraryName;
  CreateObjectExportName: string = CreateObjectDefaultExportName;
  GetHandlerProperty2ExportName: string = GetHandlerProperty2DefaultExportName;
  GetHandlerPropertyExportName: string = GetHandlerPropertyDefaultExportName;
  GetMethodPropertyExportName: string = GetMethodPropertyDefaultExportName;
  GetModulePropExportName: string = GetModulePropDefaultExportName;
  GetNumberOfFormatsExportName: string = GetNumberOfFormatsDefaultExportName;
  GetNumberOfMethodsExportName: string = GetNumberOfMethodsDefaultExportName;
  SetLargePageModeExportName: string = SetLargePageModeDefaultExportName;
  SevenzipLibraryHandle: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
{$ENDIF 7ZIP_LINKONREQUEST}

function Load7Zip: Boolean;
function Is7ZipLoaded: Boolean;
procedure Unload7Zip;

function CheckModule(GetProp: TGetModuleProp): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF 7ZIP_LINKDLL}
function CreateObject; external SevenzipDefaultLibraryName name CreateObjectDefaultExportName;
function GetHandlerProperty2; external SevenzipDefaultLibraryName name GetHandlerProperty2DefaultExportName;
function GetHandlerProperty; external SevenzipDefaultLibraryName name GetHandlerPropertyDefaultExportName;
function GetMethodProperty; external SevenzipDefaultLibraryName name GetMethodPropertyDefaultExportName;
function GetNumberOfFormats; external SevenzipDefaultLibraryName name GetNumberOfFormatsDefaultExportName;
function GetNumberOfMethods; external SevenzipDefaultLibraryName name GetNumberOfMethodsDefaultExportName;
function SetLargePageMode; external SevenzipDefaultLibraryName name SetLargePageModeDefaultExportName;
{$ENDIF 7ZIP_LINKDLL}

function Load7Zip: Boolean;
{$IFDEF 7ZIP_LINKONREQUEST}
begin
  Result := SevenzipLibraryHandle <> INVALID_MODULEHANDLE_VALUE;
  if Result then
    Exit;

  Result := JclSysUtils.LoadModule(SevenzipLibraryHandle, SevenzipLibraryName);
  if Result then
  begin
    @CreateObject := GetModuleSymbol(SevenzipLibraryHandle, CreateObjectExportName);
    @GetHandlerProperty2 := GetModuleSymbol(SevenzipLibraryHandle, GetHandlerProperty2ExportName);
    @GetHandlerProperty := GetModuleSymbol(SevenzipLibraryHandle, GetHandlerPropertyExportName);
    @GetMethodProperty := GetModuleSymbol(SevenzipLibraryHandle, GetMethodPropertyExportName);
    @GetModuleProp := GetModuleSymbol(SevenzipLibraryHandle, GetModulePropExportName);
    @GetNumberOfFormats := GetModuleSymbol(SevenzipLibraryHandle, GetNumberOfFormatsExportName);
    @GetNumberOfMethods := GetModuleSymbol(SevenzipLibraryHandle, GetNumberOfMethodsExportName);
    @SetLargePageMode := GetModuleSymbol(SevenzipLibraryHandle, SetLargePageModeExportName);

{$IF DEFINED(UNIX)}
    @SysAllocString:= GetProcAddress(SevenzipLibraryHandle, 'SysAllocString');
    @SysFreeString:= GetProcAddress(SevenzipLibraryHandle, 'SysFreeString');
    @SysStringByteLen:= GetProcAddress(SevenzipLibraryHandle, 'SysStringByteLen');
{$ENDIF}

    Result := Assigned(@CreateObject) and Assigned(@GetHandlerProperty2) and
      Assigned(@GetHandlerProperty) and Assigned(@GetMethodProperty) and
      Assigned(@GetNumberOfFormats) and Assigned(@GetNumberOfMethods) and
      Assigned(@SetLargePageMode)
{$IF DEFINED(UNIX)}
      and Assigned(@SysAllocString) and Assigned(@SysFreeString)
      and Assigned(@SysStringByteLen) and CheckModule(@GetModuleProp)
{$ENDIF}
      ;

    if not Result then Unload7Zip;
  end;
end;
{$ELSE ~7ZIP_LINKONREQUEST}
begin
  Result := True;
end;
{$ENDIF ~7ZIP_LINKONREQUEST}

function Is7ZipLoaded: Boolean;
begin
  {$IFDEF 7ZIP_LINKONREQUEST}
  Result := SevenzipLibraryHandle <> INVALID_MODULEHANDLE_VALUE;
  {$ELSE ~7ZIP_LINKONREQUEST}
  Result := True;
  {$ENDIF ~7ZIP_LINKONREQUEST}
end;

procedure Unload7Zip;
begin
  {$IFDEF 7ZIP_LINKONREQUEST}
  @CreateObject := nil;
  @GetHandlerProperty2 := nil;
  @GetHandlerProperty := nil;
  @GetMethodProperty := nil;
  @GetNumberOfFormats := nil;
  @GetNumberOfMethods := nil;
  @SetLargePageMode := nil;
  JclSysUtils.UnloadModule(SevenzipLibraryHandle);
  {$ENDIF 7ZIP_LINKONREQUEST}
end;

function CheckModule(GetProp: TGetModuleProp): Boolean;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= True;
end;
{$ELSE}
var
  This: UInt32;
  Value: TPropVariant;
begin
  if (@GetProp = nil) then
  begin
    Value.ulVal:= k_IUnknown_VirtDestructor_Yes;
  end
  else begin
    Value.vt:= VT_EMPTY;
    if GetProp(kInterfaceType, Value) <> S_OK then
      Exit(False);
    if (Value.vt <> VT_UI4) then Exit(False);
  end;
  This:= Ord(not IsEqualGUID(IUnknown, System.IUnknown));
  Result:= (Value.ulVal = This);
end;
{$ENDIF}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
