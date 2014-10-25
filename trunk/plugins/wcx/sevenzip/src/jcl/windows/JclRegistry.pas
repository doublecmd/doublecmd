{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclRegistry.pas.                                                            }
{                                                                                                  }
{ The Initial Developers of the Original Code are John C Molyneux, Marcel van Brakel and           }
{ Charlie Calvert. Portions created by these individuals are Copyright (C) of these individuals.   }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Stephane Fillon                                                                                }
{   Eric S.Fisher                                                                                  }
{   Peter Friese                                                                                   }
{   Andreas Hausladen (ahuser)                                                                     }
{   Manlio Laschena (manlio)                                                                       }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Petr Vones (pvones)                                                                            }
{   kogerbnz                                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains various utility routines to read and write registry values. Using these routines        }
{ prevents you from having to instantiate temporary TRegistry objects and since the routines       }
{ directly call the registry API they do not suffer from the resource overhead as TRegistry does.  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclRegistry;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows, System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Windows, Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclStrings;

type
  DelphiHKEY = {$IFDEF CPUX64}type Winapi.Windows.HKEY{$ELSE}Longword{$ENDIF CPUX64};
  {$HPPEMIT '// BCB users must typecast the HKEY values to DelphiHKEY or use the HK-values below.'}

  TExecKind = (ekMachineRun, ekMachineRunOnce, ekUserRun, ekUserRunOnce,
    ekServiceRun, ekServiceRunOnce);

  EJclRegistryError = class(EJclError);

{$IFDEF FPC}
const
  HKCR = DelphiHKEY($80000000);
  HKCU = DelphiHKEY($80000001);
  HKLM = DelphiHKEY($80000002);
  HKUS = DelphiHKEY($80000003);
  HKPD = DelphiHKEY($80000004);
  HKCC = DelphiHKEY($80000005);
  HKDD = DelphiHKEY($80000006);
{$ELSE ~FPC}
const
  HKCR = DelphiHKEY(HKEY_CLASSES_ROOT);
  HKCU = DelphiHKEY(HKEY_CURRENT_USER);
  HKLM = DelphiHKEY(HKEY_LOCAL_MACHINE);
  HKUS = DelphiHKEY(HKEY_USERS);
  HKPD = DelphiHKEY(HKEY_PERFORMANCE_DATA);
  HKCC = DelphiHKEY(HKEY_CURRENT_CONFIG);
  HKDD = DelphiHKEY(HKEY_DYN_DATA);
{$IFDEF CPU64}
{$NODEFINE DelphiHKEY}
{$NODEFINE HKCR}
{$NODEFINE HKCU}
{$NODEFINE HKLM}
{$NODEFINE HKUS}
{$NODEFINE HKPD}
{$NODEFINE HKCC}
{$NODEFINE HKDD}
{$HPPEMIT 'typedef HKEY DelphiHKEY;'}
{$HPPEMIT 'static const DelphiHKEY HKCR = HKEY_CLASSES_ROOT;'}
{$HPPEMIT 'static const DelphiHKEY HKCU = HKEY_CURRENT_USER;'}
{$HPPEMIT 'static const DelphiHKEY HKLM = HKEY_LOCAL_MACHINE;'}
{$HPPEMIT 'static const DelphiHKEY HKUS = HKEY_USERS;'}
{$HPPEMIT 'static const DelphiHKEY HKPD = HKEY_PERFORMANCE_DATA;'}
{$HPPEMIT 'static const DelphiHKEY HKCC = HKEY_CURRENT_CONFIG;'}
{$HPPEMIT 'static const DelphiHKEY HKDD = HKEY_DYN_DATA;'}
{$ENDIF CPU64}
{$ENDIF FPC}

function RootKeyName(const RootKey: THandle): string;
function RootKeyValue(const Name: string): THandle;

const
  RegKeyDelimiter = '\';

function RegCreateKey(const RootKey: DelphiHKEY; const Key: string): Longint; overload;
function RegCreateKey(const RootKey: DelphiHKEY; const Key, Value: string): Longint; overload;
function RegDeleteEntry(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
function RegDeleteKeyTree(const RootKey: DelphiHKEY; const Key: string): Boolean;

function RegGetDataSize(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataSize: Cardinal): Boolean;
function RegGetDataType(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataType: Cardinal): Boolean;
function RegReadBool(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
function RegReadBoolDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Boolean): Boolean;
function RegReadIntegerEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Integer; RaiseException: Boolean = False): Boolean;
function RegReadInteger(const RootKey: DelphiHKEY; const Key, Name: string): Integer;
function RegReadIntegerDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Integer): Integer;
function RegReadCardinalEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Cardinal; RaiseException: Boolean = False): Boolean;
function RegReadCardinal(const RootKey: DelphiHKEY; const Key, Name: string): Cardinal;
function RegReadCardinalDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Cardinal): Cardinal;
function RegReadDWORDEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: DWORD; RaiseException: Boolean = False): Boolean;
function RegReadDWORD(const RootKey: DelphiHKEY; const Key, Name: string): DWORD;
function RegReadDWORDDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: DWORD): DWORD;
function RegReadInt64Ex(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Int64; RaiseException: Boolean = False): Boolean;
function RegReadInt64(const RootKey: DelphiHKEY; const Key, Name: string): Int64;
function RegReadInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: Int64): Int64;
function RegReadUInt64Ex(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: UInt64; RaiseException: Boolean = False): Boolean;
function RegReadUInt64(const RootKey: DelphiHKEY; const Key, Name: string): UInt64;
function RegReadUInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: UInt64): UInt64;
function RegReadSingleEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Single; RaiseException: Boolean = False): Boolean;
function RegReadSingle(const RootKey: DelphiHKEY; const Key, Name: string): Single;
function RegReadSingleDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Single): Single;
function RegReadDoubleEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Double; RaiseException: Boolean = False): Boolean;
function RegReadDouble(const RootKey: DelphiHKEY; const Key, Name: string): Double;
function RegReadDoubleDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Double): Double;
function RegReadExtendedEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Extended; RaiseException: Boolean = False): Boolean;
function RegReadExtended(const RootKey: DelphiHKEY; const Key, Name: string): Extended;
function RegReadExtendedDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Extended): Extended;

function RegReadStringEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: string; RaiseException: Boolean = False): Boolean;
function RegReadString(const RootKey: DelphiHKEY; const Key, Name: string): string;
function RegReadStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: string): string;
function RegReadAnsiStringEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: AnsiString; RaiseException: Boolean = False): Boolean;
function RegReadAnsiString(const RootKey: DelphiHKEY; const Key, Name: string): AnsiString;
function RegReadAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: AnsiString): AnsiString;
function RegReadWideStringEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: WideString; RaiseException: Boolean = False): Boolean;
function RegReadWideString(const RootKey: DelphiHKEY; const Key, Name: string): WideString;
function RegReadWideStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: WideString): WideString;

function RegReadMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings;
  RaiseException: Boolean = False): Boolean; overload;
function RegReadMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; out RetValue: PMultiSz;
  RaiseException: Boolean = False): Boolean; overload;
procedure RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings); overload;
function RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PMultiSz; overload;
procedure RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TStrings); overload;
function RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: PMultiSz): PMultiSz; overload;

function RegReadAnsiMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; Value: TAnsiStrings;
  RaiseException: Boolean = False): Boolean; overload;
function RegReadAnsiMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; out RetValue: PAnsiMultiSz;
  RaiseException: Boolean = False): Boolean; overload;
procedure RegReadAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TAnsiStrings); overload;
function RegReadAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PAnsiMultiSz; overload;
procedure RegReadAnsiMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string;
  Value, Def: TAnsiStrings); overload;
function RegReadAnsiMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string;
  Def: PAnsiMultiSz): PAnsiMultiSz; overload;

function RegReadWideMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; Value: TWideStrings;
  RaiseException: Boolean = False): Boolean; overload;
function RegReadWideMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; out RetValue: PWideMultiSz;
  RaiseException: Boolean = False): Boolean; overload;
procedure RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TWideStrings); overload;
function RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PWideMultiSz; overload;
procedure RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string;
  Value, Def: TWideStrings); overload;
function RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string;
  Def: PWideMultiSz): PWideMultiSz; overload;

function RegReadBinaryEx(const RootKey: DelphiHKEY; const Key, Name: string; var Value; const ValueSize: Cardinal;
  out DataSize: Cardinal; RaiseException: Boolean = False): Boolean;
function RegReadBinary(const RootKey: DelphiHKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal): Cardinal;
function RegReadBinaryDef(const RootKey: DelphiHKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; Value: Boolean); overload;
procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Boolean); overload;
procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; Value: Integer); overload;
procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Integer); overload;
procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; Value: Cardinal); overload;
procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Cardinal); overload;
procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; Value: DWORD); overload;
procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: DWORD); overload;
procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: Int64); overload;
procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Int64); overload;
procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: UInt64); overload;
procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: UInt64); overload;
procedure RegWriteSingle(const RootKey: DelphiHKEY; const Key, Name: string; Value: Single); overload;
procedure RegWriteSingle(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Single); overload;
procedure RegWriteDouble(const RootKey: DelphiHKEY; const Key, Name: string; Value: Double); overload;
procedure RegWriteDouble(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Double); overload;
procedure RegWriteExtended(const RootKey: DelphiHKEY; const Key, Name: string; Value: Extended); overload;
procedure RegWriteExtended(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: Extended); overload;

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name, Value: string); overload;
procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: string); overload;
procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name: string; const Value: AnsiString); overload;
procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: AnsiString); overload;
procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; const Value: WideString); overload;
procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: WideString); overload;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PMultiSz); overload;
procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TStrings); overload;
procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: PMultiSz); overload;
procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: TStrings); overload;

procedure RegWriteAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PAnsiMultiSz); overload;
procedure RegWriteAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string;
  const Value: TAnsiStrings); overload;
procedure RegWriteAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: PAnsiMultiSz); overload;
procedure RegWriteAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: TAnsiStrings); overload;

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PWideMultiSz); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string;
  const Value: TWideStrings); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: PWideMultiSz); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: TWideStrings); overload;

procedure RegWriteBinary(const RootKey: DelphiHKEY; const Key, Name: string; const Value; const ValueSize: Cardinal);

function RegGetValueNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
function RegGetKeyNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
function RegGetValueNamesAndValues(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
function RegHasSubKeys(const RootKey: DelphiHKEY; const Key: string): Boolean;

function AllowRegKeyForEveryone(const RootKey: DelphiHKEY; Path: string): Boolean;

function RegAutoExecEnabled(const ExecKind: TExecKind; const Name: string; out CmdLine: string): Boolean;

{
From: Jean-Fabien Connault [cycocrew att worldnet dott fr]
Descr: Test whether a registry key exists as a subkey of RootKey
Used test cases:
procedure TForm1.Button1Click(Sender: TObject);
var
  RegKey: HKEY;
begin
  if RegOpenKeyEx(HKEY_CURRENT_USER, 'Software', 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    Assert(not RegKeyExists(RegKey, 'Microsoft\_Windows'));
    RegCloseKey(RegKey);
  end;
  if RegOpenKeyEx(HKEY_CURRENT_USER, 'Software', 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    Assert(RegKeyExists(RegKey, 'Microsoft\Windows'));;
    RegCloseKey(RegKey);
  end;
  Assert(RegKeyExists(HKEY_CURRENT_USER, ''));
  Assert(RegKeyExists(HKEY_CURRENT_USER, 'Software'));
  Assert(RegKeyExists(HKEY_CURRENT_USER, 'Software\Microsoft'));
  Assert(RegKeyExists(HKEY_CURRENT_USER, 'Software\Microsoft\Windows'));
  Assert(RegKeyExists(HKEY_CURRENT_USER, '\Software\Microsoft\Windows'));
  Assert(not RegKeyExists(HKEY_CURRENT_USER, '\Software\Microsoft2\Windows'));
end;
}
function RegKeyExists(const RootKey: DelphiHKEY; const Key: string): Boolean;
function RegValueExists(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;

function UnregisterAutoExec(ExecKind: TExecKind; const Name: string): Boolean;
function RegisterAutoExec(ExecKind: TExecKind; const Name, Cmdline: string): Boolean;

function RegSaveList(const RootKey: DelphiHKEY; const Key: string; const ListName: string;
  const Items: TStrings): Boolean;
function RegLoadList(const RootKey: DelphiHKEY; const Key: string; const ListName: string;
  const SaveTo: TStrings): Boolean;
function RegDelList(const RootKey: DelphiHKEY; const Key: string; const ListName: string): Boolean;

const
  HKCRLongName = 'HKEY_CLASSES_ROOT';
  HKCULongName = 'HKEY_CURRENT_USER';
  HKLMLongName = 'HKEY_LOCAL_MACHINE';
  HKUSLongName = 'HKEY_USERS';
  HKPDLongName = 'HKEY_PERFORMANCE_DATA';
  HKCCLongName = 'HKEY_CURRENT_CONFIG';
  HKDDLongName = 'HKEY_DYN_DATA';
  HKCRShortName = 'HKCR';
  HKCUShortName = 'HKCU';
  HKLMShortName = 'HKLM';
  HKUSShortName = 'HKUS';
  HKPDShortName = 'HKPD';
  HKCCShortName = 'HKCC';
  HKDDShortName = 'HKDD';

type
  TRootKey = record
    Key: DelphiHKEY;
    AnsiName: AnsiString;
    WideName: WideString;
  end;

const
  RootKeys: array [0..13] of TRootKey =
   (
    (Key: HKCR; AnsiName: HKCRLongName; WideName: HKCRLongName),
    (Key: HKCU; AnsiName: HKCULongName; WideName: HKCULongName),
    (Key: HKLM; AnsiName: HKLMLongName; WideName: HKLMLongName),
    (Key: HKUS; AnsiName: HKUSLongName; WideName: HKUSLongName),
    (Key: HKPD; AnsiName: HKPDLongName; WideName: HKPDLongName),
    (Key: HKCC; AnsiName: HKCCLongName; WideName: HKCCLongName),
    (Key: HKDD; AnsiName: HKDDLongName; WideName: HKDDLongName),
    (Key: HKCR; AnsiName: HKCRShortName; WideName: HKCRShortName),
    (Key: HKCU; AnsiName: HKCUShortName; WideName: HKCUShortName),
    (Key: HKLM; AnsiName: HKLMShortName; WideName: HKLMShortName),
    (Key: HKUS; AnsiName: HKUSShortName; WideName: HKUSShortName),
    (Key: HKPD; AnsiName: HKPDShortName; WideName: HKPDShortName),
    (Key: HKCC; AnsiName: HKCCShortName; WideName: HKCCShortName),
    (Key: HKDD; AnsiName: HKDDShortName; WideName: HKDDShortName)
   );

type
  { clRegWOW64Access allows the user to switch all registry functions to the 64 bit registry
    key on a 64bit system.

    OS/Application   32bit/32bit   64bit/32bit   64bit/64bit
    raDefault        Software      Wow6432Node   Software
    raNative         Software      Software      Software
    ra32Key          Software      Wow6432Node   Wow6432Node
    ra64Key          Software      Software      Software
  }
  TJclRegWOW64Access = (raDefault, raNative, ra32Key, ra64Key);

// cannot access variable JclRegWOW64Access from outside package
// so these helper functions can be used.
function RegGetWOW64AccessMode: TJclRegWOW64Access;
procedure RegSetWOW64AccessMode(Access: TJclRegWOW64Access);

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

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF FPC}
//  JwaAccCtrl,
  {$ELSE ~FPC}
  AccCtrl,
  JclSysUtils,
  {$ENDIF ~FPC}
  JclResources, JclWin32, JclSysInfo,
  JclAnsiStrings, JclWideStrings;

type
  TRegKind = REG_NONE..REG_QWORD;
  TRegKinds = set of TRegKind;

const
  cItems = 'Items';
  cRegBinKinds = [REG_SZ..REG_QWORD];  // all types

var
  CachedIsWindows64: Integer = -1;

threadvar
  JclRegWOW64Access: TJclRegWOW64Access {= raDefault};

function RegGetWOW64AccessMode: TJclRegWOW64Access;
begin
  Result := JclRegWOW64Access;
end;

procedure RegSetWOW64AccessMode(Access: TJclRegWOW64Access);
begin
  JclRegWOW64Access := Access;
end;

//=== Internal helper routines ===============================================

function GetWOW64AccessMode(samDesired: REGSAM): REGSAM;
const
  KEY_WOW64_32KEY = $0200;
  KEY_WOW64_64KEY = $0100;
  KEY_WOW64_RES = $0300;
  RegWOW64Accesses: array[Boolean, TJclRegWOW64Access] of HKEY = (
    (HKEY(0), HKEY(0), HKEY(0), HKEY(0)),
    (HKEY(0), KEY_WOW64_64KEY, KEY_WOW64_32KEY, KEY_WOW64_64KEY)
  );
begin
  Result := samDesired;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (samDesired and KEY_WOW64_RES = 0) then
  begin
    if CachedIsWindows64 = -1 then
      if IsWindows64 then
        CachedIsWindows64 := 1
      else
        CachedIsWindows64 := 0;

    Result := Result or RegWOW64Accesses[CachedIsWindows64 = 1, JclRegWOW64Access];
  end;
end;

function RootKeyName(const RootKey: THandle): string;
begin
  case RootKey of
    {$IFDEF DELPHI64_TEMPORARY}
    Integer(HKCR) : Result := HKCRLongName;
    Integer(HKCU) : Result := HKCULongName;
    Integer(HKLM) : Result := HKLMLongName;
    Integer(HKUS) : Result := HKUSLongName;
    Integer(HKPD) : Result := HKPDLongName;
    Integer(HKCC) : Result := HKCCLongName;
    Integer(HKDD) : Result := HKDDLongName;
    {$ELSE ~DELPHI64_TEMPORARY}
    HKCR : Result := HKCRLongName;
    HKCU : Result := HKCULongName;
    HKLM : Result := HKLMLongName;
    HKUS : Result := HKUSLongName;
    HKPD : Result := HKPDLongName;
    HKCC : Result := HKCCLongName;
    HKDD : Result := HKDDLongName;
    {$ENDIF ~DELPHI64_TEMPORARY}
  else
    Result := Format(HexFmt, [RootKey]);
  end;
end;

function RootKeyValue(const Name: string): THandle;
var
  Index: Integer;
begin
  for Index := Low(RootKeys) to High(RootKeys) do
    if string(RootKeys[Index].AnsiName) = Name then
  begin
    Result := RootKeys[Index].Key;
    Exit;
  end;
  raise EJclRegistryError.CreateResFmt(@RsInconsistentPath, [Name]);
end;

procedure ReadError(const RootKey: THandle; const Key: string);
begin
  raise EJclRegistryError.CreateResFmt(@RsUnableToOpenKeyRead, [RootKeyName(RootKey), Key]);
end;

procedure WriteError(const RootKey: THandle; const Key: string);
begin
  raise EJclRegistryError.CreateResFmt(@RsUnableToOpenKeyWrite, [RootKeyName(RootKey), Key]);
end;

procedure ValueError(const RootKey: THandle; const Key, Name: string);
begin
  raise EJclRegistryError.CreateResFmt(@RsUnableToAccessValue, [RootKeyName(RootKey), Key, Name]);
end;

procedure DataError(const RootKey: THandle; const Key, Name: string);
begin
  raise EJclRegistryError.CreateResFmt(@RsWrongDataType, [RootKeyName(RootKey), Key, Name]);
end;

function GetKeyAndPath(ExecKind: TExecKind; out Key: HKEY; out RegPath: string): Boolean;
begin
  Result := False;
  if (ExecKind in [ekServiceRun, ekServiceRunOnce]) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    Exit;
  if ExecKind in [ekMachineRun, ekMachineRunOnce, ekServiceRun, ekServiceRunOnce] then
    Key := HKEY_LOCAL_MACHINE
  else
    Key := HKEY_CURRENT_USER;
  RegPath := 'Software\Microsoft\Windows\CurrentVersion\';
  case ExecKind of
    ekMachineRun, ekUserRun:
      RegPath := RegPath + 'Run';
    ekMachineRunOnce, ekUserRunOnce:
      RegPath := RegPath + 'RunOnce';
    ekServiceRun:
      RegPath := RegPath + 'RunServices';
    ekServiceRunOnce:
      RegPath := RegPath + 'RunServicesOnce';
  end;
  Result := True;
end;

function RelativeKey(const RootKey: DelphiHKEY; Key: PAnsiChar): PAnsiChar; overload;
var
  I: Integer;
begin
  Result := Key;
  if Result^ = RegKeyDelimiter then
    Inc(Result);
  for I := Low(RootKeys) to High(RootKeys) do
    if StrPosA(Key, PAnsiChar(RootKeys[I].AnsiName + RegKeyDelimiter)) = Result then
    begin
      if RootKey <> RootKeys[I].Key then
        raise EJclRegistryError.CreateResFmt(@RsInconsistentPath, [Key])
      else
        Inc(Result, Length(RootKeys[I].AnsiName));
      Break;
    end;
end;

function RelativeKey(const RootKey: DelphiHKEY; Key: PWideChar): PWideChar; overload;
var
  I: Integer;
begin
  Result := Key;
  if Result^ = RegKeyDelimiter then
    Inc(Result);
  for I := Low(RootKeys) to High(RootKeys) do
    if StrPosW(Key, PWideChar(RootKeys[I].WideName + RegKeyDelimiter)) = Result then
    begin
      if RootKey <> RootKeys[I].Key then
        raise EJclRegistryError.CreateResFmt(@RsInconsistentPath, [Key])
      else
        Inc(Result, Length(RootKeys[I].WideName));
      Break;
    end;
end;

function InternalRegOpenKeyEx(Key: HKEY; SubKey: PWideChar;
  ulOptions: DWORD; samDesired: REGSAM; var RegKey: HKEY): Longint; overload;
var
  RelKey: AnsiString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := RegOpenKeyExW(Key, RelativeKey(Key, SubKey), ulOptions, GetWOW64AccessMode(samDesired), RegKey)
  else
  begin
    RelKey := AnsiString(WideString(RelativeKey(Key, SubKey)));
    Result := RegOpenKeyExA(Key, PAnsiChar(RelKey), ulOptions, samDesired, RegKey);
  end;
end;

function InternalRegOpenKeyEx(Key: HKEY; SubKey: PAnsiChar;
  ulOptions: DWORD; samDesired: REGSAM; var RegKey: HKEY): Longint; overload;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := RegOpenKeyExA(Key, RelativeKey(Key, SubKey), ulOptions, GetWOW64AccessMode(samDesired), RegKey)
  else
    Result := RegOpenKeyExA(Key, RelativeKey(Key, SubKey), ulOptions, samDesired, RegKey);
end;

function InternalRegQueryValueEx(Key: HKEY; ValueName: PWideChar;
  lpReserved: Pointer; lpType: PDWORD; lpData: Pointer; lpcbData: PDWORD): Longint;
var
  ValName: AnsiString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := RegQueryValueExW(Key, ValueName, lpReserved, lpType, lpData, lpcbData)
  else
  begin
    ValName := AnsiString(WideString(ValueName));
    Result := RegQueryValueExA(Key, PAnsiChar(ValName), lpReserved, lpType, lpData, lpcbData);
  end;
end;

function InternalRegSetValueEx(Key: HKEY; ValueName: PWideChar;
  Reserved: DWORD; dwType: DWORD; lpData: Pointer; cbData: DWORD): Longint; stdcall;
var
  ValName: AnsiString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := RegSetValueExW(Key, ValueName, Reserved, dwType, lpData, cbData)
  else
  begin
    ValName := AnsiString(WideString(ValueName));
    Result := RegSetValueExA(Key, PAnsiChar(ValName), Reserved, dwType, lpData, cbData);
  end;
end;

function InternalGetData(const RootKey: DelphiHKEY; const Key, Name: WideString;
  RegKinds: TRegKinds; ExpectedSize: DWORD;
  out DataType: DWORD; Data: Pointer; out DataSize: DWORD; RaiseException: Boolean): Boolean;
var
  RegKey: HKEY;
begin
  Result := True;
  DataType := REG_NONE;
  DataSize := 0;
  RegKey := 0;
  if InternalRegOpenKeyEx(RootKey, PWideChar(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    try
      if InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        if not (DataType in RegKinds) or (DataSize > ExpectedSize) then
          if RaiseException then
            DataError(RootKey, Key, Name)
          else
            Result := False;
        if InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, nil, Data, @DataSize) <> ERROR_SUCCESS then
          if RaiseException then
            ValueError(RootKey, Key, Name)
          else
            Result := False;
      end
      else
        if RaiseException then
          ValueError(RootKey, Key, Name)
        else
          Result := False;
    finally
      RegCloseKey(RegKey);
    end
  else
    if RaiseException then
      ReadError(RootKey, Key)
    else
      Result := False;
end;

function InternalGetAnsiString(const RootKey: DelphiHKEY; const Key, Name: WideString; MultiFlag: Boolean;
  out RetValue: AnsiString; RaiseException: Boolean): Boolean;
var
  RegKey: HKEY;
  DataType, DataSize: DWORD;
  TmpRet: WideString;
  DataLength: Integer;
  RegKinds: TRegKinds;
begin
  Result := True;
  DataType := REG_NONE;
  DataSize := 0;
  RetValue := '';
  RegKey := 0;
  if InternalRegOpenKeyEx(RootKey, PWideChar(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    try
      if InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        if MultiFlag then
          RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ]
        else
          RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ];
        if DataType in RegKinds then
        begin
          if Win32Platform = VER_PLATFORM_WIN32_NT then
          begin
            DataLength := DataSize div SizeOf(WideChar);
            SetLength(TmpRet, DataLength);
            Result := InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, nil, PWideChar(TmpRet), @DataSize) = ERROR_SUCCESS;
            if Result then
              RetValue := AnsiString(Copy(TmpRet, 1, DataLength - 1));
          end
          else
          begin
            DataLength := DataSize div SizeOf(AnsiChar);
            SetLength(RetValue, DataLength);
            Result := InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, nil, PAnsiChar(RetValue), @DataSize) = ERROR_SUCCESS;
            if Result then
              SetLength(RetValue, DataLength - 1);
          end;
          if not Result then
          begin
            RetValue := '';
            if RaiseException then
              ValueError(RootKey, Key, Name)
            else
              Result := False;
          end;
        end
        else
        begin
          RetValue := '';
          if RaiseException then
            DataError(RootKey, Key, Name)
          else
            Result := False;
        end;
      end
      else
        if RaiseException then
          ValueError(RootKey, Key, Name)
        else
          Result := False;
    finally
      RegCloseKey(RegKey);
    end
  else
    if RaiseException then
      ReadError(RootKey, Key)
    else
      Result := False;
end;

function InternalGetWideString(const RootKey: DelphiHKEY; const Key, Name: WideString; MultiFlag: Boolean;
  out RetValue: WideString; RaiseException: Boolean): Boolean;
var
  RegKey: HKEY;
  DataType, DataSize: DWORD;
  RegKinds: TRegKinds;
  DataLength: Integer;
begin
  Result := True;
  DataType := REG_NONE;
  DataSize := 0;
  RetValue := '';
  RegKey := 0;
  if InternalRegOpenKeyEx(RootKey, PWideChar(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    try
      if InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
          RegKinds := [REG_BINARY]
        else
        if MultiFlag then
          RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ]
        else
          RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ];
        if DataType in RegKinds then
        begin
          DataLength := DataSize div SizeOf(WideChar);
          SetLength(RetValue, DataLength);
          if InternalRegQueryValueEx(RegKey, PWideChar(Name), nil, nil, PWideChar(RetValue), @DataSize) = ERROR_SUCCESS then
            SetLength(RetValue, DataLength - 1)
          else
          begin
            RetValue := '';
            if RaiseException then
              ValueError(RootKey, Key, Name)
            else
              Result := False;
          end;
        end
        else
        begin
          RetValue := '';
          if RaiseException then
            DataError(RootKey, Key, Name)
          else
            Result := False;
        end;
      end
      else
        if RaiseException then
          ValueError(RootKey, Key, Name)
        else
          Result := False;
    finally
      RegCloseKey(RegKey);
    end
  else
    if RaiseException then
      ReadError(RootKey, Key)
    else
      Result := False;
end;

function InternalStrToFloat(const RootKey: DelphiHKEY; const Key, Name: string; out Success: Boolean;
  RaiseException: Boolean): Extended;
var
  {$IFDEF RTL150_UP}
  FS: TFormatSettings;
  {$ELSE ~RTL150_UP}
  OldSep: Char;
  {$ENDIF ~RTL150_UP}
begin
  {$IFDEF RTL150_UP}
  FS.ThousandSeparator := ',';
  FS.DecimalSeparator := '.';
  {$ELSE ~RTL150_UP}
  OldSep := DecimalSeparator;
  try
    DecimalSeparator := '.';
  {$ENDIF ~RTL150_UP}
    if RaiseException then
    begin
      Result := StrToFloat(RegReadString(RootKey, Key, Name){$IFDEF RTL150_UP}, FS{$ENDIF});
      Success := True;
    end
    else
      Success := TryStrToFloat(RegReadString(RootKey, Key, Name), Result{$IFDEF RTL150_UP}, FS{$ENDIF});
  {$IFNDEF RTL150_UP}
  finally
    DecimalSeparator := OldSep;
  end;
  {$ENDIF ~RTL150_UP}
end;

procedure InternalSetData(const RootKey: DelphiHKEY; const Key, Name: WideString;
  RegKind: TRegKind; Value: Pointer; ValueSize: Cardinal);
var
  RegKey: HKEY;
begin
  if not RegKeyExists(RootKey, Key) then
    RegCreateKey(RootKey, Key);
  RegKey := 0;
  if InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PWideChar(Key)), 0, KEY_WRITE, RegKey) = ERROR_SUCCESS then
    try
      if InternalRegSetValueEx(RegKey, PWideChar(Name), 0, RegKind, Value, ValueSize) <> ERROR_SUCCESS then
        WriteError(RootKey, Key);
    finally
      RegCloseKey(RegKey);
    end
  else
    WriteError(RootKey, Key);
end;

procedure InternalSetAnsiData(const RootKey: DelphiHKEY; const Key, Name: WideString;
  RegKind: TRegKind; Value: Pointer; ValueSize: Cardinal);
var
  Source: AnsiString;
  Dest: WideString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    // destination must be wide data
    SetLength(Source, ValueSize div SizeOf(AnsiChar));
    Move(Value^,Source[1],ValueSize * SizeOf(AnsiChar));
    Dest := WideString(Source);
    InternalSetData(RootKey, Key, Name, RegKind, PWideChar(Dest), SizeOf(WideChar) * ValueSize);
  end
  else
    InternalSetData(RootKey, Key, Name, RegKind, Value, ValueSize);
end;

procedure InternalSetWideData(const RootKey: DelphiHKEY; const Key, Name: string;
  RegKind: TRegKind; Value: Pointer; ValueSize: Cardinal);
begin
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (RegKind in [REG_SZ, REG_MULTI_SZ, REG_EXPAND_SZ]) then
    RegKind := REG_BINARY;
  InternalSetData(RootKey, Key, Name, RegKind, Value, ValueSize);
end;

//=== Registry ===============================================================

function RegCreateKey(const RootKey: DelphiHKEY; const Key: string): Longint;
var
  RegKey: HKEY;
begin
  RegKey := 0;
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.RegCreateKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, nil, 0,
    GetWOW64AccessMode(KEY_ALL_ACCESS), nil, RegKey, nil);
  if Result = ERROR_SUCCESS then
    RegCloseKey(RegKey);
end;

function RegCreateKey(const RootKey: DelphiHKEY; const Key, Value: string): Longint;
begin
  Result := RegSetValue(RootKey, RelativeKey(RootKey, PChar(Key)), REG_SZ, PChar(Value), Length(Value));
end;

function RegDeleteEntry(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
var
  RegKey: HKEY;
begin
  Result := False;
  RegKey := 0;
  if InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Result := RegDeleteValue(RegKey, PChar(Name)) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
    if not Result then
      ValueError(RootKey, Key, Name);
  end
  else
    WriteError(RootKey, Key);
end;

function RegDeleteKeyTree(const RootKey: DelphiHKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  MaxSubKeyLen: DWORD;
  KeyName: string;
begin
  RegKey := 0;
  Result := InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_ALL_ACCESS, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, @MaxSubKeyLen, nil, nil, nil, nil, nil, nil);
    if NumSubKeys <> 0 then
      for I := NumSubKeys - 1 downto 0 do
      begin
        Size := MaxSubKeyLen+1;
        SetLength(KeyName, Size);
        RegEnumKeyEx(RegKey, I, PChar(KeyName), Size, nil, nil, nil, nil);
        SetLength(KeyName, StrLen(PChar(KeyName)));
        Result := RegDeleteKeyTree(RootKey, Key + RegKeyDelimiter + KeyName);
        if not Result then
          Break;
      end;
    RegCloseKey(RegKey);
    if Result then
      Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.RegDeleteKey(RootKey, RelativeKey(RootKey, PChar(Key))) = ERROR_SUCCESS;
  end
  else
    WriteError(RootKey, Key);
end;

function RegGetDataSize(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataSize: Cardinal): Boolean;
var
  RegKey: HKEY;
begin
  DataSize := 0;
  RegKey := 0;
  Result := InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    Result := RegQueryValueEx(RegKey, PChar(Name), nil, nil, nil, @DataSize) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

function RegGetDataType(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataType: DWORD): Boolean;
var
  RegKey: HKEY;
begin
  DataType := REG_NONE;
  RegKey := 0;
  Result := InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    Result := RegQueryValueEx(RegKey, PChar(Name), nil, @DataType, nil, nil) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

function RegReadBool(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
begin
  Result := RegReadInteger(RootKey, Key, Name) <> 0;
end;

function RegReadBoolDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Boolean): Boolean;
begin
  Result := RegReadIntegerDef(RootKey, Key, Name, Ord(Def)) <> 0;
end;

function RegReadIntegerEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Integer; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  Ret: Int64;
begin
  Ret := 0;
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    if RaiseException then
    begin
      Ret := StrToInt64(RegReadString(RootKey, Key, Name));
      Result := True;
    end
    else
      Result := TryStrToInt64(RegReadString(RootKey, Key, Name), Ret)
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
      SizeOf(Ret), DataType, @Ret, DataSize, RaiseException);
  RetValue := Integer(Ret and $FFFFFFFF);
end;

function RegReadInteger(const RootKey: DelphiHKEY; const Key, Name: string): Integer;
begin
  RegReadIntegerEx(RootKey, Key, Name, Result, True);
end;

function RegReadIntegerDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Integer): Integer;
begin
  try
    if not RegReadIntegerEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadCardinalEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Cardinal; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  Ret: Int64;
begin
  Ret := 0;
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    if RaiseException then
    begin
      Ret := StrToInt64(RegReadString(RootKey, Key, Name));
      Result := True;
    end
    else
      Result := TryStrToInt64(RegReadString(RootKey, Key, Name), Ret)
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
      SizeOf(Ret), DataType, @Ret, DataSize, RaiseException);
  RetValue := Cardinal(Ret) and $FFFFFFFF;
end;

function RegReadCardinal(const RootKey: DelphiHKEY; const Key, Name: string): Cardinal;
begin
  RegReadCardinalEx(RootKey, Key, Name, Result, True);
end;

function RegReadCardinalDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Cardinal): Cardinal;
begin
  try
    if not RegReadCardinalEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadDWORDEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: DWORD; RaiseException: Boolean): Boolean;
begin
  Result := RegReadCardinalEx(RootKey, Key, Name, RetValue, RaiseException);
end;

function RegReadDWORD(const RootKey: DelphiHKEY; const Key, Name: string): DWORD;
begin
  Result := RegReadCardinal(RootKey, Key, Name);
end;

function RegReadDWORDDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: DWORD): DWORD;
begin
  Result := RegReadCardinalDef(RootKey, Key, Name, Def);
end;

function RegReadInt64Ex(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Int64; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  Data: array [0..1] of Integer;
  Ret: Int64;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
  begin
    // (rom) circumvents internal compiler error for D6
    if RaiseException then
    begin
      Ret := StrToInt64(RegReadString(RootKey, Key, Name));
      Result := True;
    end
    else
      Result := TryStrToInt64(RegReadString(RootKey, Key, Name), Ret);
    RetValue := Ret;
  end
  else
  begin
    Data[0] := 0;
    Data[1] := 0;
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
       SizeOf(Data), DataType, @Data, DataSize, RaiseException);
    // REG_BINARY is implicitly unsigned if DataSize < 8
    if DataType = REG_DWORD then
      // DWORDs get sign extended
      RetValue := Data[0]
    else
      Move(Data[0], RetValue, SizeOf(Data));
  end;
end;

function RegReadInt64(const RootKey: DelphiHKEY; const Key, Name: string): Int64;
begin
  RegReadInt64Ex(RootKey, Key, Name, Result, True);
end;

function RegReadInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: Int64): Int64;
begin
  try
    if not RegReadInt64Ex(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadUInt64Ex(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: UInt64; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
  Ret: Int64;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
  begin
    // (rom) circumvents internal compiler error for D6
    if RaiseException then
    begin
      Ret := StrToInt64(RegReadString(RootKey, Key, Name));
      Result := True;
    end
    else
      Result := TryStrToInt64(RegReadString(RootKey, Key, Name), Ret);
    RetValue := UInt64(Ret);
  end
  else
  begin
    // type cast required to circumvent internal error in D7
    RetValue := UInt64(0);
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
      SizeOf(RetValue), DataType, @RetValue, DataSize, RaiseException);
  end;
end;

function RegReadUInt64(const RootKey: DelphiHKEY; const Key, Name: string): UInt64;
begin
  RegReadUInt64Ex(RootKey, Key, Name, Result, True);
end;

function RegReadUInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: UInt64): UInt64;
begin
  try
    if not RegReadUInt64Ex(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadSingleEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Single; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RetValue := InternalStrToFloat(RootKey, Key, Name, Result, RaiseException)
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY],
      SizeOf(RetValue), DataType, @RetValue, DataSize, RaiseException);
end;

function RegReadSingle(const RootKey: DelphiHKEY; const Key, Name: string): Single;
begin
  RegReadSingleEx(RootKey, KEy, Name, Result, True);
end;

function RegReadSingleDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Single): Single;
begin
  try
    if not RegReadSingleEx(RootKey, KEy, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadDoubleEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Double; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RetValue := InternalStrToFloat(RootKey, Key, Name, Result, RaiseException)
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY],
      SizeOf(RetValue), DataType, @RetValue, DataSize, RaiseException);
end;

function RegReadDouble(const RootKey: DelphiHKEY; const Key, Name: string): Double;
begin
  RegReadDoubleEx(RootKey, Key, Name, Result, True);
end;

function RegReadDoubleDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Double): Double;
begin
  try
    if not RegReadDoubleEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadExtendedEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: Extended; RaiseException: Boolean): Boolean;
var
  DataType, DataSize: DWORD;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RetValue := InternalStrToFloat(RootKey, Key, Name, Result, RaiseException)
  else
    Result := InternalGetData(RootKey, Key, Name, [REG_BINARY],
      SizeOf(RetValue), DataType, @RetValue, DataSize, RaiseException);
end;

function RegReadExtended(const RootKey: DelphiHKEY; const Key, Name: string): Extended;
begin
  RegReadExtendedEx(RootKey, Key, Name, Result, True);
end;

function RegReadExtendedDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Extended): Extended;
begin
  try
    if not RegReadExtendedEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadStringEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: string; RaiseException: Boolean): Boolean;
{$IFDEF SUPPORTS_UNICODE}
var
  TmpRet: WideString;
begin
  Result := InternalGetWideString(RootKey, Key, Name, False, TmpRet, RaiseException);
  RetValue := string(TmpRet);
end;
{$ELSE ~SUPPORTS_UNICODE}
var
  TmpRet: AnsiString;
begin
  Result := InternalGetAnsiString(RootKey, Key, Name, False, TmpRet, RaiseException);
  RetValue := string(TmpRet);
end;
{$ENDIF ~SUPPORTS_UNICODE}

function RegReadString(const RootKey: DelphiHKEY; const Key, Name: string): string;
begin
  RegReadStringEx(RootKey, Key, Name, Result, True);
end;

function RegReadStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: string): string;
begin
  try
    if not RegReadStringEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadAnsiStringEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: AnsiString; RaiseException: Boolean): Boolean;
begin
  Result := InternalGetAnsiString(RootKey, Key, Name, False, RetValue, RaiseException);
end;

function RegReadAnsiString(const RootKey: DelphiHKEY; const Key, Name: string): AnsiString;
begin
  RegReadAnsiStringEx(RootKey, Key, Name, Result, True);
end;

function RegReadAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: AnsiString): AnsiString;
begin
  try
    if not RegReadAnsiStringEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadWideStringEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: WideString; RaiseException: Boolean): Boolean;
begin
  Result := InternalGetWideString(RootKey, Key, Name, False, RetValue, RaiseException);
end;

function RegReadWideString(const RootKey: DelphiHKEY; const Key, Name: string): WideString;
begin
  RegReadWideStringEx(RootKey, Key, Name, Result, True);
end;

function RegReadWideStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: WideString): WideString;
begin
  try
    if not RegReadWideStringEx(RootKey, Key, Name, Result, False) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function RegReadMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings;
  RaiseException: Boolean): Boolean;
{$IFDEF SUPPORTS_UNICODE}
var
  S: WideString;
begin
  Result := InternalGetWideString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    WideMultiSzToWideStrings(Value, PWideMultiSz(PChar(S)));
end;
{$ELSE ~SUPPORTS_UNICODE}
var
  S: AnsiString;
begin
  Result := InternalGetAnsiString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    JclStrings.MultiSzToStrings(Value, PAnsiMultiSz(PChar(S)));
end;
{$ENDIF ~SUPPORTS_UNICODE}

procedure RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings);
begin
  RegReadMultiSzEx(RootKey, Key, Name, Value, True);
end;

procedure RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TStrings);
begin
  try
    if not RegReadMultiSzEx(RootKey, Key, Name, Value, False) then
      Value.Assign(Def);
  except
    Value.Assign(Def);
  end;
end;

function RegReadMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: PMultiSz; RaiseException: Boolean): Boolean;
{$IFDEF SUPPORTS_UNICODE}
var
  S: WideString;
begin
  Result := InternalGetWideString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    // always returns a newly allocated PMultiSz
    RetValue := WideMultiSzDup(PWideMultiSz(S))
  else
    RetValue := nil;
end;
{$ELSE ~SUPPORTS_UNICODE}
var
  S: AnsiString;
begin
  Result := InternalGetAnsiString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    // always returns a newly allocated PMultiSz
    RetValue := JclAnsiStrings.MultiSzDup(PAnsiMultiSz(S))
  else
    RetValue := nil;
end;
{$ENDIF ~SUPPORTS_UNICODE}

function RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): JclStrings.PMultiSz;
begin
  RegReadMultiSzEx(RootKey, Key, Name, Result, True);
end;

function RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: JclStrings.PMultiSz): JclStrings.PMultiSz;
begin
  try
    if not RegReadMultiSzEx(RootKey, Key, Name, Result, False) then
      // always returns a newly allocated PMultiSz
      Result := JclStrings.MultiSzDup(Def);
  except
    // always returns a newly allocated PMultiSz
    Result := JclStrings.MultiSzDup(Def);
  end;
end;

function RegReadAnsiMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; Value: TAnsiStrings;
  RaiseException: Boolean): Boolean;
var
  S: AnsiString;
begin
  Result := InternalGetAnsiString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    JclAnsiStrings.MultiSzToStrings(Value, PAnsiMultiSz(S));
end;

procedure RegReadAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TAnsiStrings);
begin
  RegReadAnsiMultiSzEx(RootKey, Key, Name, Value, True);
end;

procedure RegReadAnsiMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TAnsiStrings);
begin
  try
    if not RegReadAnsiMultiSzEx(RootKey, Key, Name, Value, False) then
      Value.Assign(Def);
  except
    Value.Assign(Def);
  end;
end;

function RegReadAnsiMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: PAnsiMultiSz; RaiseException: Boolean): Boolean; overload;
var
  S: AnsiString;
begin
  RetValue := nil;
  Result := InternalGetAnsiString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    // always returns a newly allocated PMultiAnsiSz
    RetValue := JclAnsiStrings.MultiSzDup(PAnsiMultiSz(S));
end;

function RegReadAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PAnsiMultiSz;
begin
  RegReadAnsiMultiSzEx(RootKey, Key, Name, Result, True);
end;

function RegReadAnsiMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: PAnsiMultiSz): PAnsiMultiSz;
begin
  try
    if RegReadAnsiMultiSzEx(RootKey, Key, Name, Result, False) then
      // always returns a newly allocated PAnsiMultiSz
      Result := JclAnsiStrings.MultiSzDup(Def);
  except
    // always returns a newly allocated PAnsiMultiSz
    Result := JclAnsiStrings.MultiSzDup(Def);
  end;
end;

function RegReadWideMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string; Value: TWideStrings;
  RaiseException: Boolean): Boolean;
var
  S: WideString;
begin
  Result := InternalGetWideString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    JclWideStrings.MultiSzToStrings(Value, PWideMultiSz(S));
end;

procedure RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TWideStrings);
begin
  RegReadWideMultiSzEx(RootKey, Key, Name, Value, True);
end;

procedure RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TWideStrings);
begin
  try
    if not RegReadWideMultiSzEx(RootKey, Key, Name, Value, False) then
      Value.Assign(Def);
  except
    Value.Assign(Def);
  end;
end;

function RegReadWideMultiSzEx(const RootKey: DelphiHKEY; const Key, Name: string;
  out RetValue: PWideMultiSz; RaiseException: Boolean): Boolean; overload;
var
  S: WideString;
begin
  RetValue := nil;
  Result := InternalGetWideString(RootKey, Key, Name, True, S, RaiseException);
  if Result then
    // always returns a newly allocated PMultiWideSz
    RetValue := JclWideStrings.MultiSzDup(PWideMultiSz(S));
end;

function RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PWideMultiSz;
begin
  RegReadWideMultiSzEx(RootKey, Key, Name, Result, True);
end;

function RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: PWideMultiSz): PWideMultiSz;
begin
  try
    if RegReadWideMultiSzEx(RootKey, Key, Name, Result, False) then
      // always returns a newly allocated PWideMultiSz
      Result := JclWideStrings.MultiSzDup(Def);
  except
    // always returns a newly allocated PWideMultiSz
    Result := JclWideStrings.MultiSzDup(Def);
  end;
end;

function RegReadBinaryEx(const RootKey: DelphiHKEY; const Key, Name: string; var Value;
  const ValueSize: Cardinal; out DataSize: Cardinal; RaiseException: Boolean): Boolean;
var
  DataType: DWORD;
begin
  Result := InternalGetData(RootKey, Key, Name, cRegBinKinds, ValueSize, DataType, @Value, DataSize, RaiseException);
end;

function RegReadBinary(const RootKey: DelphiHKEY; const Key, Name: string; var Value;
  const ValueSize: Cardinal): Cardinal;
begin
  RegReadBinaryEx(RootKey, Key, Name, Value, ValueSize, Result, True);
end;

function RegReadBinaryDef(const RootKey: DelphiHKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;
begin
  try
    if not RegReadBinaryEx(RootKey, Key, Name, Value, ValueSize, Result, False) then
    begin
      FillChar(Value, ValueSize, Def);
      Result := ValueSize;
    end;
  except
    FillChar(Value, ValueSize, Def);
    Result := ValueSize;
  end;
end;

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; Value: Boolean);
begin
  RegWriteCardinal(RootKey, Key, Name, REG_DWORD, Cardinal(Ord(Value)));
end;

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Boolean);
begin
  RegWriteCardinal(RootKey, Key, Name, DataType, Cardinal(Ord(Value)));
end;

procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; Value: Integer);
begin
  RegWriteInteger(RootKey, Key, Name, REG_DWORD, Value);
end;

procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Integer);
var
  Val: Int64;
  Size: Integer;
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%d', [Value]))
  else
  if DataType in [REG_DWORD, REG_QWORD, REG_BINARY] then
  begin
    // sign extension
    Val := Value;
    if DataType = REG_QWORD then
      Size := SizeOf(Int64)
    else
      Size := SizeOf(Value);
    InternalSetData(RootKey, Key, Name, DataType, @Val, Size);
  end
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; Value: Cardinal);
begin
  RegWriteCardinal(RootKey, Key, Name, REG_DWORD, Cardinal(Value));
end;

procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Cardinal);
var
  Val: Int64;
  Size: Integer;
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%u', [Value]))
  else
  if DataType in [REG_DWORD, REG_QWORD, REG_BINARY] then
  begin
    // no sign extension
    Val := Value and $FFFFFFFF;
    if DataType = REG_QWORD then
      Size := SizeOf(Int64)
    else
      Size := SizeOf(Value);
    InternalSetData(RootKey, Key, Name, DataType, @Val, Size);
  end
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; Value: DWORD);
begin
  RegWriteCardinal(RootKey, Key, Name, REG_DWORD, Cardinal(Value));
end;

procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: DWORD);
begin
  RegWriteCardinal(RootKey, Key, Name, DataType, Cardinal(Value));
end;

procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: Int64);
begin
  RegWriteInt64(RootKey, Key, Name, REG_QWORD, Value);
end;

procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Int64);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%d', [Value]))
  else
    RegWriteUInt64(RootKey, Key, Name, DataType, UInt64(Value));
end;

procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: UInt64);
begin
  RegWriteUInt64(RootKey, Key, Name, REG_QWORD, Value);
end;

procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: UInt64);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%u', [Value]))
  else
  if DataType in [REG_QWORD, REG_BINARY] then
    InternalSetData(RootKey, Key, Name, DataType, @Value, SizeOf(Value))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteSingle(const RootKey: DelphiHKEY; const Key, Name: string; Value: Single);
begin
  RegWriteSingle(RootKey, Key, Name, REG_BINARY, Value);
end;

procedure RegWriteSingle(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Single);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%g', [Value]))
  else
  if DataType in [REG_BINARY] then
    InternalSetData(RootKey, Key, Name, DataType, @Value, SizeOf(Value))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteDouble(const RootKey: DelphiHKEY; const Key, Name: string; Value: Double);
begin
  RegWriteDouble(RootKey, Key, Name, REG_BINARY, Value);
end;

procedure RegWriteDouble(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Double);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%g', [Value]))
  else
  if DataType in [REG_BINARY] then
    InternalSetData(RootKey, Key, Name, DataType, @Value, SizeOf(Value))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteExtended(const RootKey: DelphiHKEY; const Key, Name: string; Value: Extended);
begin
  RegWriteExtended(RootKey, Key, Name, REG_BINARY, Value);
end;

procedure RegWriteExtended(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Extended);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%g', [Value]))
  else
  if DataType in [REG_BINARY] then
    InternalSetData(RootKey, Key, Name, DataType, @Value, SizeOf(Value))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name, Value: string);
begin
  RegWriteString(RootKey, Key, Name, REG_SZ, Value);
end;

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; const Value: string);
begin
  if DataType in [REG_BINARY, REG_SZ, REG_EXPAND_SZ] then
    {$IFDEF SUPPORTS_UNICODE}
    InternalSetWideData(RootKey, Key, Name, DataType, PChar(Value),
      (Length(Value) + 1) * SizeOf(Char))
    {$ELSE ~SUPPORTS_UNICODE}
    InternalSetAnsiData(RootKey, Key, Name, DataType, PChar(Value),
      (Length(Value) + 1) * SizeOf(Char))
    {$ENDIF ~SUPPORTS_UNICODE}
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name: string; const Value: AnsiString);
begin
  RegWriteAnsiString(RootKey, Key, Name, REG_SZ, Value);
end;

procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: AnsiString);
begin
  if DataType in [REG_BINARY, REG_SZ, REG_EXPAND_SZ] then
    InternalSetAnsiData(RootKey, Key, Name, DataType, PAnsiChar(Value),
      (Length(Value) + 1) * SizeOf(AnsiChar))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; const Value: WideString);
begin
  RegWriteWideString(RootKey, Key, Name, REG_SZ, Value);
end;

procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: WideString);
begin
  if DataType in [REG_BINARY, REG_SZ, REG_EXPAND_SZ] then
    InternalSetWideData(RootKey, Key, Name, DataType, PWideChar(Value),
      (Length(Value) + 1) * SizeOf(WideChar))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: JclStrings.PMultiSz);
begin
  RegWriteMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: JclStrings.PMultiSz);
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
    {$IFDEF SUPPORTS_UNICODE}
    InternalSetWideData(RootKey, Key, Name, DataType, Value,
      MultiSzLength(Value) * SizeOf(Char))
    {$ELSE ~SUPPORTS_UNICODE}
    InternalSetAnsiData(RootKey, Key, Name, DataType, Value,
      JclStrings.MultiSzLength(Value) * SizeOf(Char))
    {$ENDIF ~SUPPORTS_UNICODE}
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TStrings);
begin
  RegWriteMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: TStrings);
var
  Dest: JclStrings.PMultiSz;
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
  begin
    Dest := nil;
    JclStrings.StringsToMultiSz(Dest, Value);
    try
      RegWriteMultiSz(RootKey, Key, Name, DataType, Dest);
    finally
      JclStrings.FreeMultiSz(Dest);
    end;
  end
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PAnsiMultiSz);
begin
  RegWriteAnsiMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

procedure RegWriteAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: PAnsiMultiSz);
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
    InternalSetAnsiData(RootKey, Key, Name, DataType, Value,
      JclAnsiStrings.MultiSzLength(Value) * SizeOf(AnsiChar))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TAnsiStrings);
begin
  RegWriteAnsiMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

procedure RegWriteAnsiMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: TAnsiStrings);
var
  Dest: PAnsiMultiSz;
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
  begin
    Dest := nil;
    JclAnsiStrings.StringsToMultiSz(Dest, Value);
    try
      RegWriteAnsiMultiSz(RootKey, Key, Name, DataType, Dest);
    finally
      JclAnsiStrings.FreeMultiSz(Dest);
    end;
  end
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PWideMultiSz);
begin
  RegWriteWideMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  Value: PWideMultiSz);
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
    InternalSetWideData(RootKey, Key, Name, DataType, Value,
      JclWideStrings.MultiSzLength(Value) * SizeOf(WideChar))
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TWideStrings);
begin
  RegWriteWideMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal;
  const Value: TWideStrings);
var
  Dest: PWideMultiSz;
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
  begin
    Dest := nil;
    JclWideStrings.StringsToMultiSz(Dest, Value);
    try
      RegWriteWideMultiSz(RootKey, Key, Name, DataType, Dest);
    finally
      JclWideStrings.FreeMultiSz(Dest);
    end;
  end
  else
    DataError(RootKey, Key, Name);
end;

procedure RegWriteBinary(const RootKey: DelphiHKEY; const Key, Name: string; const Value; const ValueSize: Cardinal);
begin
  InternalSetData(RootKey, Key, Name, REG_BINARY, @Value, ValueSize);
end;

function UnregisterAutoExec(ExecKind: TExecKind; const Name: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  Result := GetKeyAndPath(ExecKind, Key, RegPath);
  if Result then
    Result := RegDeleteEntry(Key, RegPath, Name);
end;

function RegisterAutoExec(ExecKind: TExecKind; const Name, Cmdline: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  Result := GetKeyAndPath(ExecKind, Key, RegPath);
  if Result then
    RegWriteString(Key, RegPath, Name, Cmdline);
end;

function RegGetValueNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  NumSubValues: DWORD;
  MaxSubValueLen: DWORD;
  ValueName: string;
begin
  Result := False;
  List.BeginUpdate;
  try
    List.Clear;
    RegKey := 0;
    if InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    begin
      if RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, nil, nil,
        @NumSubValues, @MaxSubValueLen, nil, nil, nil) = ERROR_SUCCESS then
      begin
        SetLength(ValueName, MaxSubValueLen + 1);
        if NumSubValues <> 0 then
          for I := 0 to NumSubValues - 1 do
          begin
            Size := MaxSubValueLen + 1;
            RegEnumValue(RegKey, I, PChar(ValueName), Size, nil, nil, nil, nil);
            List.Add(PChar(ValueName));
          end;
        Result := True;
      end;
      RegCloseKey(RegKey);
    end
    else
      ReadError(RootKey, Key);
  finally
    List.EndUpdate;
  end;
end;

function RegGetKeyNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  MaxSubKeyLen: DWORD;
  KeyName: string;
begin
  Result := False;
  List.BeginUpdate;
  try
    List.Clear;
    RegKey := 0;
    if InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
    begin
      if RegQueryInfoKey(RegKey, nil, nil, nil,
        @NumSubKeys, @MaxSubKeyLen, nil, nil, nil, nil, nil, nil) = ERROR_SUCCESS then
      begin
        SetLength(KeyName, MaxSubKeyLen+1);
        if NumSubKeys <> 0 then
          for I := 0 to NumSubKeys-1 do
          begin
            Size := MaxSubKeyLen+1;
            RegEnumKeyEx(RegKey, I, PChar(KeyName), Size, nil, nil, nil, nil);
            List.Add(PChar(KeyName));
          end;
        Result := True;
      end;
      RegCloseKey(RegKey);
    end
    else
      ReadError(RootKey, Key);
  finally
    List.EndUpdate;
  end;
end;

function RegGetValueNamesAndValues(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
var
  I: Integer;
  TempList: TStringList;
  Name: string;
  DataType: DWORD;
begin
  List.BeginUpdate;
  TempList := TStringList.Create;
  try
    List.Clear;
    Result := RegKeyExists(RootKey, Key) and RegGetValueNames(RootKey, Key, TempList);
    if Result then
    begin
      for I := 0 to TempList.Count - 1 do
      begin
        Name := TempList[I];
        if RegGetDataType(RootKey, Key, Name, DataType) and
          ((DataType = REG_SZ) or (DataType = REG_EXPAND_SZ) or (DataType = REG_BINARY)) then
          List.Values[Name] := RegReadStringDef(RootKey, Key, Name, '');
      end;
    end;
  finally
    List.EndUpdate;
    TempList.Free;
  end;
end;

function RegHasSubKeys(const RootKey: DelphiHKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
  NumSubKeys: Integer;
begin
  Result := False;
  RegKey := 0;
  if InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, nil, nil, nil, nil, nil, nil, nil);
    Result := NumSubKeys <> 0;
    RegCloseKey(RegKey);
  end
  else
    ReadError(RootKey, Key);
end;

function RegKeyExists(const RootKey: DelphiHKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
begin
  RegKey := 0;
  Result := (InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS);
  if Result then
    RegCloseKey(RegKey);
end;

function RegValueExists(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
var
  RegKey: HKEY;
begin
  RegKey := 0;
  Result := (InternalRegOpenKeyEx(RootKey, RelativeKey(RootKey, PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS);
  if Result then
  begin
    Result := RegQueryValueEx(RegKey, PChar(Name), nil, nil, nil, nil) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

function RegSaveList(const RootKey: DelphiHKEY; const Key: string;
  const ListName: string; const Items: TStrings): Boolean;
var
  I: Integer;
  SubKey: string;
begin
  Result := False;
  SubKey := Key + RegKeyDelimiter + ListName;
  if RegCreateKey(RootKey, SubKey) = ERROR_SUCCESS then
  begin
    // Save Number of strings
    RegWriteInteger(RootKey, SubKey, cItems, Items.Count);
    for I := 1 to Items.Count do
      RegWriteString(RootKey, SubKey, IntToStr(I), Items[I-1]);
    Result := True;
  end;
end;

function RegLoadList(const RootKey: DelphiHKEY; const Key: string;
  const ListName: string; const SaveTo: TStrings): Boolean;
var
  I, N: Integer;
  SubKey: string;
begin
  SaveTo.BeginUpdate;
  try
    SaveTo.Clear;
    SubKey := Key + RegKeyDelimiter + ListName;
    N := RegReadIntegerDef(RootKey, SubKey, cItems, -1);
    for I := 1 to N do
      SaveTo.Add(RegReadString(RootKey, SubKey, IntToStr(I)));
    Result := N > 0;
  finally
    SaveTo.EndUpdate;
  end;
end;

function RegDelList(const RootKey: DelphiHKEY; const Key: string; const ListName: string): Boolean;
var
  I, N: Integer;
  SubKey: string;
begin
  Result := False;
  SubKey := Key + RegKeyDelimiter + ListName;
  N := RegReadIntegerDef(RootKey, SubKey, cItems, -1);
  if (N > 0) and RegDeleteEntry(RootKey, SubKey, cItems) then
    for I := 1 to N do
    begin
      Result := RegDeleteEntry(RootKey, SubKey, IntToStr(I));
      if not Result then
        Break;
    end;
end;

function AllowRegKeyForEveryone(const RootKey: DelphiHKEY; Path: string): Boolean;
var
  WidePath: PWideChar;
  Len: Integer;

// This is an ugly kludge until the x64 compiler allows 64bit constants in case statements
// http://qc.embarcadero.com/wc/qcmain.aspx?d=95499
const
  HKLM2 = Cardinal(HKLM);
  HKCU2 = Cardinal(HKCU);
  HKCR2 = Cardinal(HKCR);
  HKUS2 = Cardinal(HKUS);

begin
  Result := Win32Platform <> VER_PLATFORM_WIN32_NT;
  if not Result then // Win 2000/XP
  begin
    case Cardinal(RootKey) of
      HKLM2:
        Path := HKLMLongName + RegKeyDelimiter + RelativeKey(RootKey, PChar(Path));
      HKCU2:
        Path := HKCULongName + RegKeyDelimiter + RelativeKey(RootKey, PChar(Path));
      HKCR2:
        Path := HKCRLongName + RegKeyDelimiter + RelativeKey(RootKey, PChar(Path));
      HKUS2:
        Path := HKUSLongName + RegKeyDelimiter + RelativeKey(RootKey, PChar(Path));
    end;
    Len := (Length(Path) + 1) * SizeOf(WideChar);
    GetMem(WidePath, Len);
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PAnsiChar(AnsiString(Path)), -1, WidePath, Len);
    Result := RtdlSetNamedSecurityInfoW(WidePath, SE_REGISTRY_KEY,
      DACL_SECURITY_INFORMATION, nil, nil, nil, nil) = ERROR_SUCCESS;
    FreeMem(WidePath);
  end;
end;

function RegAutoExecEnabled(const ExecKind: TExecKind; const Name: string; out CmdLine: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  CmdLine := '';

  Result := GetKeyAndPath(ExecKind, Key, RegPath);
  if Result then
  begin
    try
      CmdLine := RegReadString(Key, RegPath, Name);
    except
      Result := False;
      CmdLine := '';
    end;
  end
  else
    CmdLine := '';
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

