{
  Double Commander
  -------------------------------------------------------------------------
  SevenZip archiver plugin

  Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit DCJclResources;

{$mode delphi}

interface

resourcestring
  RsCompressionUnavailableProperty = 'This property is unavailable';
  RsCompressionNoNestedArchive     = 'Nested archive is not implemented';
  RsCompression7zWindows           = 'Windows';
  RsCompressionDuplicate           = 'Archive already contains the file %s';
  RsCompressionUnsupportedMethod   = 'Unsupported method';
  RsCompressionDataError           = 'Data error';
  RsCompressionCRCError            = 'CRC error';
  RsCompressionUnknownError        = 'Unknown error';
  RsCompressionWriteNotSupported   = 'Write operation is not supported';
  RsCompressionCompressingError    = 'This operation is not allowed during compression';
  RsCompressionDecompressingError  = 'This operation is not allowed during decompression';
  RsCompressionReplaceError        = 'Some volume could not be replaced while archive update';
  RsCompression7zReturnError       = '7-Zip: Error code (%.8x) - %s';
  RsCompression7zUnknownValueType  = '7-Zip: Property (%d) contains unknown value type (%d)';
  RsCompression7zLoadError         = '7-Zip: Cannot load library 7z.dll';
  RsCompression7zOutArchiveError   = '7-Zip: Unable to get out archive interface of %s class';
  RsCompression7zInArchiveError    = '7-Zip: Unable to get in archive interface of %s class';

  RsCompressionZipName             = 'ZIP format';
  RsCompressionZipExtensions       = '*.zip';
  RsCompressionBZip2Name           = 'BZIP2 format';
  RsCompressionBZip2Extensions     = '*.bz2;*.bzip2;*.tbz2;*.tbz';
  RsCompressionBZip2SubExtensions  = '.tbz2=.tar;.tbz=.tar';
  RsCompressionRarName             = 'RAR format';
  RsCompressionRarExtensions       = '*.rar;*.r00';
  RsCompressionArjName             = 'ARJ format';
  RsCompressionArjExtensions       = '*.arj';
  RsCompressionZName               = 'Z format';
  RsCompressionZExtensions         = '*.z;*.taz';
  RsCompressionZSubExtensions      = '.taz=.tar';
  RsCompressionLzhName             = 'LZH format';
  RsCompressionLzhExtensions       = '*.lzh;*.lha';
  RsCompression7zName              = '7z format';
  RsCompression7zExtensions        = '*.7z';
  RsCompressionCabName             = 'CAB format';
  RsCompressionCabExtensions       = '*.cab';
  RsCompressionNsisName            = 'NSIS format';
  RsCompressionNsisExtensions      = '*.nsis';
  RsCompressionLzmaName            = 'LZMA format';
  RsCompressionLzmaExtensions      = '*.lzma';
  RsCompressionLzma86Name          = 'LZMA86 format';
  RsCompressionLzma86Extensions    = '*.lzma86';
  RsCompressionXzName              = 'XZ format';
  RsCompressionXzExtensions        = '*.xz;*.txz';
  RsCompressionXzSubExtensions     = '.txz=.tar';
  RsCompressionPpmdName            = 'PPMD format';
  RsCompressionPpmdExtensions      = '*.ppmd';
  RsCompressionTEName              = 'TE format';
  RsCompressionTEExtensions        = '*.te';
  RsCompressionUEFIcName           = 'UEFIc format';
  RsCompressionUEFIcExtensions     = '*.scap';
  RsCompressionUEFIsName           = 'UEFIs format';
  RsCompressionUEFIsExtensions     = '*.uefif';
  RsCompressionSquashFSName        = 'SquashFS format';
  RsCompressionSquashFSExtensions  = '*.squashfs';
  RsCompressionCramFSName          = 'CramFS format';
  RsCompressionCramFSExtensions    = '*.cramfs';
  RsCompressionApmName             = 'APM format';
  RsCompressionApmExtensions       = '*.apm';
  RsCompressionMsLZName            = 'MsLZ format';
  RsCompressionMsLZExtensions      = '';
  RsCompressionFlvName             = 'FLV format';
  RsCompressionFlvExtensions       = '*.flv';
  RsCompressionSwfName             = 'SWF format';
  RsCompressionSwfExtensions       = '*.swf';
  RsCompressionSwfcName            = 'SWFC format';
  RsCompressionSwfcExtensions      = '*.swf';
  RsCompressionNtfsName            = 'NTFS format';
  RsCompressionNtfsExtensions      = '*.ntfs;*.img';
  RsCompressionFatName             = 'FAT format';
  RsCompressionFatExtensions       = '*.fat;*.img';
  RsCompressionMbrName             = 'MBR format';
  RsCompressionMbrExtensions       = '*.mbr';
  RsCompressionVhdName             = 'VHD format';
  RsCompressionVhdExtensions       = '*.vhd';
  RsCompressionVhdSubExtensions    = '.vhd=.mbr';
  RsCompressionPeName              = 'PE format';
  RsCompressionPeExtensions        = '*.exe;*.dll';
  RsCompressionElfName             = 'ELF format';
  RsCompressionElfExtensions       = '';
  RsCompressionMachoName           = 'MACH-O format';
  RsCompressionMachoExtensions     = '';
  RsCompressionUdfName             = 'UDF format';
  RsCompressionUdfExtensions       = '*.udf;*.iso;*.img';
  RsCompressionXarName             = 'XAR format';
  RsCompressionXarExtensions       = '*.xar;*.pkg';
  RsCompressionMubName             = 'MUB format';
  RsCompressionMubExtensions       = '*.mub';
  RsCompressionHfsName             = 'HFS format';
  RsCompressionHfsExtensions       = '*.hfs;*.hfsx';
  RsCompressionDmgName             = 'DMG format';
  RsCompressionDmgExtensions       = '*.dmg';
  RsCompressionCompoundName        = 'COMPOUND format';
  RsCompressionCompoundExtensions  = '*.msi;*.msp;*.doc;*.xls;*.ppt';
  RsCompressionWimName             = 'WIM format';
  RsCompressionWimExtensions       = '*.wim;*.swm';
  RsCompressionIsoName             = 'ISO format';
  RsCompressionIsoExtensions       = '*.iso;*.img';
  RsCompressionChmName             = 'CHM format';
  RsCompressionChmExtensions       = '*.chm;*.chw;*.chi;*.chq;*.hxs;*.hxi;*.hxr;*.hxq;*.hxw;*.lit';
  RsCompressionSplitName           = 'SPLIT format';
  RsCompressionSplitExtensions     = '*.001';
  RsCompressionRpmName             = 'RPM format';
  RsCompressionRpmExtensions       = '*.rpm';
  RsCompressionDebName             = 'DEB format';
  RsCompressionDebExtensions       = '*.deb';
  RsCompressionCpioName            = 'CPIO format';
  RsCompressionCpioExtensions      = '*.cpio';
  RsCompressionTarName             = 'TAR format';
  RsCompressionTarExtensions       = '*.tar';
  RsCompressionGZipName            = 'GZIP format';
  RsCompressionGZipExtensions      = '*.gz;*.gzip;*.tgz';
  RsCompressionGZipSubExtensions   = '.tgz=.tar';

implementation

end.

