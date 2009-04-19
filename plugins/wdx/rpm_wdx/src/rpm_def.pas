//***************************************************************
// This file is part of RPMWCX, a archiver plugin for
// Windows Commander.
// Copyright (C) 2000 Mandryka Yurij  e-mail:braingroup@hotmail.ru
//***************************************************************

//***************************************************************
// This code based on Christian Ghisler (support@ghisler.com) sources
//***************************************************************

{$A-,I-}
unit rpm_def;

{$MODE Delphi}

interface

uses
  Classes;

{$ifdef ver90}
type longword=longint;
{$endif}
{$ifdef ver100}
type longword=longint;
{$endif}

//values for TArchiveRec.headers (pseudo-files to show)
const
  HDR_INFO = 0;
  HDR_DATA = 1;

const
  MAX_PHANTOM_FILES = 13;

const
  RPM_MAGIC = $DBEEABED;

  RPM_TYPE_BINARY = 0; // binary package type
  RPM_TYPE_SOURCE = 1; // source package type

  RPMSIG_PGP262_1024 = 1;
  RPMSIG_MD5         = 3;
  RPMSIG_MD5_PGP     = 4;
  RPMSIG_HEADERSIG   = 5;

const
  RPMTAG_NAME         = 1000;
  RPMTAG_VERSION      = 1001;
  RPMTAG_RELEASE      = 1002;
  RPMTAG_SUMMARY      = 1004;
  RPMTAG_DESCRIPTION  = 1005;
  RPMTAG_BUILDTIME    = 1006;
  RPMTAG_DISTRIBUTION = 1010;
  RPMTAG_VENDOR       = 1011;
  RPMTAG_LICENSE      = 1014;
  RPMTAG_PACKAGER     = 1015;
  RPMTAG_GROUP        = 1016;
  RPMTAG_OS           = 1021;
  RPMTAG_ARCH         = 1022;
  RPMTAG_FILENAMES    = 1027;
  RPMTAG_FILEMTIMES   = 1034;
  RPMTAG_SOURCERPM    = 1044;
  RPMTAG_ARCHIVESIZE  = 1046;

type
  RPM_EntryInfo = record
    tag    : LongWord;
    etype  : LongWord;
    offset : LongWord;
    count  : LongWord;
  end;{EntryInfo}

type
  RPM_Lead = record
    magic          : LongWord;
    major_ver      : Byte;
    minor_ver      : Byte;
    rpmtype        : Word;
    archnum        : Word;
    name           : array[1..66] of Char;
    osnum          : Word;
    signature_type : Word;
    reserved       : array[1..16] of Char;
  end;{RPM_Lead}

  RPM_Header =record
    magic      : array [1..3] of byte;
    header_ver : Byte;
    reserved   : array [1..4] of Byte;
    count      : LongWord;
    data_size  : LongWord;
  end;{RPM_Header}

  RPM_InfoRec = record
    name         : AnsiString; // RPMTAG_NAME
    version      : AnsiString; // RPMTAG_VERSION
    release      : AnsiString; // RPMTAG_RELEASE
    summary      : AnsiString; // RPMTAG_SUMMARY
    description  : AnsiString; // RPMTAG_DESCRIPTION
    distribution : AnsiString; // RPMTAG_DISTRIBUTION
    buildtime    : LongWord;   // RPMTAG_BUILDTIME
    vendor       : AnsiString; // RPMTAG_VENDOR
    license      : AnsiString; // RPMTAG_LICENSE
    packager     : AnsiString; // RPMTAG_PACKAGER
    group        : AnsiString; // RPMTAG_GROUP
    os           : AnsiString; // RPMTAG_OS
    arch         : AnsiString; // RPMTAG_ARCH
    sourcerpm    : AnsiString; // RPMTAG_SOURCERPM
  end;{RPM_Info}

implementation

end.
