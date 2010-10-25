{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

Definitions of basic types.
This unit should depend on as little other units as possible.

contributors:


}

unit uTypes;

interface

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

{$IFDEF DARWIN}
uses
  MacOSAll;
{$ENDIF}

type
  TLibHandle = PtrInt;

  TOpenStringArray = array of String;

  TFileAttrs = Cardinal;     // file attributes type regardless of system

  TWinFileTime = QWord;      // NTFS time (UTC) (2 x DWORD)
  TDosFileTime = LongInt;    // MS-DOS time (local)

{$IFDEF MSWINDOWS}
  TFileTime = TWinFileTime;
{$ELSE}
  // Unix time (UTC).
  // Unix defines time_t as signed integer,
  // but we define it as unsigned because sign is not needed.
  {$IFDEF cpu64}
  TFileTime = QWord;
  {$ELSE}
  TFileTime = DWord;
  {$ENDIF}
{$ENDIF}

  PFileTime = ^TFileTime;
  PWinFileTime = ^TWinFileTime;

  TSearchRecEx = Record
    Time : TFileTime;  // modification time
    Size : Int64;
    Attr : TFileAttrs;
    Name : UTF8String;
    ExcludeAttr : TFileAttrs;
{$ifdef unix}
    FindHandle : Pointer;
{$else unix}
    FindHandle : THandle;
{$endif unix}
{$if defined(Win32) or defined(WinCE) or defined(Win64)}
    FindData : Windows.TWin32FindDataW;
{$endif}
{$ifdef netware_clib}
    FindData : TNetwareFindData;
{$endif}
{$ifdef netware_libc}
    FindData : TNetwareLibcFindData;
{$endif}
{$ifdef MacOS}
    FindData : TMacOSFindData;
{$endif}
  end;

// what is showed in panel
  TPanelMode= (pmDirectory, pmArchive, pmVFS);

// plugin types
  TPluginType = (ptDSX, ptWCX, ptWDX, ptWFX, ptWLX);

  //base structure for storing file informations
  TFileRecItem = record
//      iType       : Integer;
      bIsLink     : Boolean;
      bLinkIsDir  : Boolean;
      sLinkTo     : AnsiString;
      sName       : AnsiString;
      sNameNoExt  : AnsiString;
      sExt        : AnsiString;
      iSize       : Int64;
      fTimeI      : Double;
      sTime       : AnsiString;
      bExecutable : Boolean;
      sModeStr    : String[11];
      iMode       : Cardinal; // from stats
      bSysFile    : Boolean;
      iIconID     : Integer; // index ICO in imagelist
      sOwner      : AnsiString;
      sGroup      : AnsiString;
{mate}
      iOwner      : Cardinal;
      iGroup      : Cardinal;
{/mate}
      sPath       : AnsiString; // used only in block operation
      bSelected   : Boolean;
      iDirSize    : Int64;
  end;

  PFileRecItem=^TFileRecItem;

  TRange = record
    First: Integer;
    Last: Integer;
  end;

implementation

end.
