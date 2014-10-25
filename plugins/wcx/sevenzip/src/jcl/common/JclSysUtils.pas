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
{ The Original Code is JclSysUtils.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Alexander Radchenko,                                                                           }
{   Andreas Hausladen (ahuser)                                                                     }
{   Anthony Steele                                                                                 }
{   Bernhard Berger                                                                                }
{   Heri Bender                                                                                    }
{   Jean-Fabien Connault (cycocrew)                                                                }
{   Jens Fudickar                                                                                  }
{   Jeroen Speldekamp                                                                              }
{   Marcel van Brakel                                                                              }
{   Peter Friese                                                                                   }
{   Petr Vones (pvones)                                                                            }
{   Python                                                                                         }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert R. Marsh                                                                                }
{   Robert Rossmair (rrossmair)                                                                    }
{   Rudy Velthuis                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{   Wayne Sherman                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Description: Various pointer and class related routines.                                         }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclSysUtils;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.SysUtils, System.Classes, System.TypInfo, System.SyncObjs,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, TypInfo, SyncObjs,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclSynch;

// memory initialization
// first parameter is "out" to make FPC happy with uninitialized values
procedure ResetMemory(out P; Size: Longint);

// Pointer manipulation
procedure GetAndFillMem(var P: Pointer; const Size: Integer; const Value: Byte);
procedure FreeMemAndNil(var P: Pointer);
function PCharOrNil(const S: string): PChar;
function PAnsiCharOrNil(const S: AnsiString): PAnsiChar;
{$IFDEF SUPPORTS_WIDESTRING}
function PWideCharOrNil(const W: WideString): PWideChar;
{$ENDIF SUPPORTS_WIDESTRING}

function SizeOfMem(const APointer: Pointer): Integer;

function WriteProtectedMemory(BaseAddress, Buffer: Pointer; Size: Cardinal;
  out WrittenBytes: Cardinal): Boolean;

// Guards
type
  ISafeGuard = interface
    function ReleaseItem: Pointer;
    function GetItem: Pointer;
    procedure FreeItem;
    property Item: Pointer read GetItem;
  end;

  IMultiSafeGuard = interface (IInterface)
    function AddItem(Item: Pointer): Pointer;
    procedure FreeItem(Index: Integer);
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
    function ReleaseItem(Index: Integer): Pointer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: Pointer read GetItem;
  end;

  TJclSafeGuard = class(TInterfacedObject, ISafeGuard)
  private
    FItem: Pointer;
  public
    constructor Create(Mem: Pointer);
    destructor Destroy; override;
    { ISafeGuard }
    function ReleaseItem: Pointer;
    function GetItem: Pointer;
    procedure FreeItem; virtual;
    property Item: Pointer read GetItem;
  end;

  TJclObjSafeGuard = class(TJclSafeGuard, ISafeGuard)
  public
    constructor Create(Obj: TObject);
    { ISafeGuard }
    procedure FreeItem; override;
  end;

  TJclMultiSafeGuard = class(TInterfacedObject, IMultiSafeGuard)
  private
    FItems: TList;
  public
    constructor Create;
    destructor Destroy; override;
    { IMultiSafeGuard }
    function AddItem(Item: Pointer): Pointer;
    procedure FreeItem(Index: Integer); virtual;
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
    function ReleaseItem(Index: Integer): Pointer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: Pointer read GetItem;
  end;

  TJclObjMultiSafeGuard = class(TJclMultiSafeGuard, IMultiSafeGuard)
  public
    { IMultiSafeGuard }
    procedure FreeItem(Index: Integer); override;
  end;

function Guard(Mem: Pointer; out SafeGuard: ISafeGuard): Pointer; overload;
function Guard(Obj: TObject; out SafeGuard: ISafeGuard): TObject; overload;

function Guard(Mem: Pointer; var SafeGuard: IMultiSafeGuard): Pointer; overload;
function Guard(Obj: TObject; var SafeGuard: IMultiSafeGuard): TObject; overload;

function GuardGetMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
function GuardAllocMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;

(*
{$IFDEF SUPPORTS_GENERICS}
type
  ISafeGuard<T: class> = interface
    function ReleaseItem: T;
    function GetItem: T;
    procedure FreeItem;
    property Item: T read GetItem;
  end;

  TSafeGuard<T: class> = class(TObject, ISafeGuard<T>)
  private
    FItem: T;
    function ReleaseItem: T;
    function GetItem: T;
    procedure FreeItem;

  constructor Create(Instance: T);
  destructor Destroy; override;
  public
    class function New(Instance: T): ISafeGuard<T>; static;
  end;
{$ENDIF SUPPORTS_GENERICS}
*)

{ Shared memory between processes functions }

// Functions for the shared memory owner
type
  ESharedMemError = class(EJclError);

{$IFDEF MSWINDOWS}

{ SharedGetMem return ERROR_ALREADY_EXISTS if the shared memory is already
  allocated, otherwise it returns 0.
  Throws ESharedMemError if the Name is invalid. }
function SharedGetMem(var P{: Pointer}; const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Integer;

{ SharedAllocMem calls SharedGetMem and then fills the memory with zero if
  it was not already allocated.
  Throws ESharedMemError if the Name is invalid. }
function SharedAllocMem(const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;

{ SharedFreeMem releases the shared memory if it was the last reference. }
function SharedFreeMem(var P{: Pointer}): Boolean;

// Functions for the shared memory user

{ SharedOpenMem returns True if the shared memory was already allocated by
  SharedGetMem or SharedAllocMem. Otherwise it returns False.
  Throws ESharedMemError if the Name is invalid. }

function SharedOpenMem(var P{: Pointer}; const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Boolean; overload;

{ SharedOpenMem return nil if the shared memory was not already allocated
  by SharedGetMem or SharedAllocMem.
  Throws ESharedMemError if the Name is invalid. }
function SharedOpenMem(const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer; overload;

{ SharedCloseMem releases the shared memory if it was the last reference. }
function SharedCloseMem(var P{: Pointer}): Boolean;

{$ENDIF MSWINDOWS}

// Binary search
function SearchSortedList(List: TList; SortFunc: TListSortCompare; Item: Pointer;
  Nearest: Boolean = False): Integer;

type
  TUntypedSearchCompare = function(Param: Pointer; ItemIndex: Integer; const Value): Integer;

function SearchSortedUntyped(Param: Pointer; ItemCount: Integer; SearchFunc: TUntypedSearchCompare;
  const Value; Nearest: Boolean = False): Integer;

// Dynamic array sort and search routines
type
  TDynArraySortCompare = function (Item1, Item2: Pointer): Integer;

procedure SortDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare);
// Usage: SortDynArray(Array, SizeOf(Array[0]), SortFunction);
function SearchDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare;
  ValuePtr: Pointer; Nearest: Boolean = False): SizeInt;
// Usage: SearchDynArray(Array, SizeOf(Array[0]), SortFunction, @SearchedValue);

{ Various compare functions for basic types }

function DynArrayCompareByte(Item1, Item2: Pointer): Integer;
function DynArrayCompareShortInt(Item1, Item2: Pointer): Integer;
function DynArrayCompareWord(Item1, Item2: Pointer): Integer;
function DynArrayCompareSmallInt(Item1, Item2: Pointer): Integer;
function DynArrayCompareInteger(Item1, Item2: Pointer): Integer;
function DynArrayCompareCardinal(Item1, Item2: Pointer): Integer;
function DynArrayCompareInt64(Item1, Item2: Pointer): Integer;

function DynArrayCompareSingle(Item1, Item2: Pointer): Integer;
function DynArrayCompareDouble(Item1, Item2: Pointer): Integer;
function DynArrayCompareExtended(Item1, Item2: Pointer): Integer;
function DynArrayCompareFloat(Item1, Item2: Pointer): Integer;

function DynArrayCompareAnsiString(Item1, Item2: Pointer): Integer;
function DynArrayCompareAnsiText(Item1, Item2: Pointer): Integer;
function DynArrayCompareWideString(Item1, Item2: Pointer): Integer;
function DynArrayCompareWideText(Item1, Item2: Pointer): Integer;
function DynArrayCompareString(Item1, Item2: Pointer): Integer;
function DynArrayCompareText(Item1, Item2: Pointer): Integer;

// Object lists
procedure ClearObjectList(List: TList);
procedure FreeObjectList(var List: TList);

// Reference memory stream
type
  TJclReferenceMemoryStream = class(TCustomMemoryStream)
  public
    constructor Create(const Ptr: Pointer; Size: Longint);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

// AutoPtr
type
  IAutoPtr = interface
    { Returns the object as pointer, so it is easier to assign it to a variable }
    function AsPointer: Pointer;
    { Returns the AutoPtr handled object }
    function AsObject: TObject;
    { Releases the object from the AutoPtr. The AutoPtr looses the control over
      the object. }
    function ReleaseObject: TObject;
  end;

  TJclAutoPtr = class(TInterfacedObject, IAutoPtr)
  private
    FValue: TObject;
  public
    constructor Create(AValue: TObject);
    destructor Destroy; override;
    { IAutoPtr }
    function AsPointer: Pointer;
    function AsObject: TObject;
    function ReleaseObject: TObject;
  end;

function CreateAutoPtr(Value: TObject): IAutoPtr;

// Replacement for the C ternary conditional operator ? :
function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Float): Float; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload;
{$IFDEF SUPPORTS_VARIANT}
function Iff(const Condition: Boolean; const TruePart, FalsePart: Variant): Variant; overload;
{$ENDIF SUPPORTS_VARIANT}

// Classes information and manipulation
type
  EJclVMTError = class(EJclError);

// Virtual Methods
{$IFNDEF FPC}
function GetVirtualMethodCount(AClass: TClass): Integer;
{$ENDIF ~FPC}
function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);

// Dynamic Methods
type
  TDynamicIndexList = array [0..MaxInt div 16] of Word;
  PDynamicIndexList = ^TDynamicIndexList;
  TDynamicAddressList = array [0..MaxInt div 16] of Pointer;
  PDynamicAddressList = ^TDynamicAddressList;

function GetDynamicMethodCount(AClass: TClass): Integer;
function GetDynamicIndexList(AClass: TClass): PDynamicIndexList;
function GetDynamicAddressList(AClass: TClass): PDynamicAddressList;
function HasDynamicMethod(AClass: TClass; Index: Integer): Boolean;
{$IFNDEF FPC}
function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer;
{$ENDIF ~FPC}

{ init table methods }

function GetInitTable(AClass: TClass): PTypeInfo;

{ field table methods }

type
  PFieldEntry = ^TFieldEntry;
  TFieldEntry = packed record
    OffSet: Integer;
    IDX: Word;
    Name: ShortString;
  end;

  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable = packed record
    Count: Smallint;
    Classes: array [0..8191] of ^TPersistentClass;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    EntryCount: Word;
    FieldClassTable: PFieldClassTable;
    FirstEntry: TFieldEntry;
   {Entries: array [1..65534] of TFieldEntry;}
  end;

function GetFieldTable(AClass: TClass): PFieldTable;

{ method table }

type
  PMethodEntry = ^TMethodEntry;
  TMethodEntry = packed record
    EntrySize: Word;
    Address: Pointer;
    Name: ShortString;
  end;

  PMethodTable = ^TMethodTable;
  TMethodTable = packed record
    Count: Word;
    FirstEntry: TMethodEntry;
   {Entries: array [1..65534] of TMethodEntry;}
  end;

function GetMethodTable(AClass: TClass): PMethodTable;
function GetMethodEntry(MethodTable: PMethodTable; Index: Integer): PMethodEntry;

// Class Parent
procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
function GetClassParent(AClass: TClass): TClass;

{$IFNDEF FPC}
function IsClass(Address: Pointer): Boolean;
function IsObject(Address: Pointer): Boolean;
{$ENDIF ~FPC}

function InheritsFromByName(AClass: TClass; const AClassName: string): Boolean;

// Interface information
function GetImplementorOfInterface(const I: IInterface): TObject;

// interfaced persistent
type
  TJclInterfacedPersistent = class(TInterfacedPersistent, IInterface)
  protected
    FOwnerInterface: IInterface;
    FRefCount: Integer;
  public
    procedure AfterConstruction; override;
    { IInterface }
    // function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

// Numeric formatting routines
type
  TDigitCount = 0..255;
  TDigitValue = -1..35;  // invalid, '0'..'9', 'A'..'Z'
  TNumericSystemBase = 2..Succ(High(TDigitValue));

  TJclNumericFormat = class(TObject)
  private
    FWantedPrecision: TDigitCount;
    FPrecision: TDigitCount;
    FNumberOfFractionalDigits: TDigitCount;
    FExpDivision: Integer;
    FDigitBlockSize: TDigitCount;
    FWidth: TDigitCount;
    FSignChars: array [Boolean] of Char;
    FBase: TNumericSystemBase;
    FFractionalPartSeparator: Char;
    FDigitBlockSeparator: Char;
    FShowPositiveSign: Boolean;
    FPaddingChar: Char;
    FMultiplier: string;
    function GetDigitValue(Digit: Char): Integer;
    function GetNegativeSign: Char;
    function GetPositiveSign: Char;
    procedure InvalidDigit(Digit: Char);
    procedure SetPrecision(const Value: TDigitCount);
    procedure SetBase(const Value: TNumericSystemBase);
    procedure SetNegativeSign(const Value: Char);
    procedure SetPositiveSign(const Value: Char);
    procedure SetExpDivision(const Value: Integer);
  protected
    function IntToStr(const Value: Int64; out FirstDigitPos: Integer): string; overload;
    function ShowSign(const Value: Float): Boolean; overload;
    function ShowSign(const Value: Int64): Boolean; overload;
    function SignChar(const Value: Float): Char; overload;
    function SignChar(const Value: Int64): Char; overload;
    property WantedPrecision: TDigitCount read FWantedPrecision;
  public
    constructor Create;
    function Digit(DigitValue: TDigitValue): Char;
    function DigitValue(Digit: Char): TDigitValue;
    function IsDigit(Value: Char): Boolean;
    function Sign(Value: Char): Integer;
    procedure GetMantissaExp(const Value: Float; out Mantissa: string; out Exponent: Integer);
    function FloatToHTML(const Value: Float): string;
    function IntToStr(const Value: Int64): string; overload;
    function FloatToStr(const Value: Float): string; overload;
    function StrToInt(const Value: string): Int64;
    property Base: TNumericSystemBase read FBase write SetBase;
    property Precision: TDigitCount read FPrecision write SetPrecision;
    property NumberOfFractionalDigits: TDigitCount read FNumberOfFractionalDigits write FNumberOfFractionalDigits;
    property ExponentDivision: Integer read FExpDivision write SetExpDivision;
    property DigitBlockSize: TDigitCount read FDigitBlockSize write FDigitBlockSize;
    property DigitBlockSeparator: Char read FDigitBlockSeparator write FDigitBlockSeparator;
    property FractionalPartSeparator: Char read FFractionalPartSeparator write FFractionalPartSeparator;
    property Multiplier: string read FMultiplier write FMultiplier;
    property PaddingChar: Char read FPaddingChar write FPaddingChar;
    property ShowPositiveSign: Boolean read FShowPositiveSign write FShowPositiveSign;
    property Width: TDigitCount read FWidth write FWidth;
    property NegativeSign: Char read GetNegativeSign write SetNegativeSign;
    property PositiveSign: Char read GetPositiveSign write SetPositiveSign;
  end;

function IntToStrZeroPad(Value, Count: Integer): string;

// Child processes
type
  // e.g. TStrings.Append
  TTextHandler = procedure(const Text: string) of object;
  TJclProcessPriority = (ppIdle, ppNormal, ppHigh, ppRealTime, ppBelowNormal, ppAboveNormal);

const
  ABORT_EXIT_CODE = {$IFDEF MSWINDOWS} ERROR_CANCELLED {$ELSE} 1223 {$ENDIF};

function Execute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  OutputLineCallback: TTextHandler; RawOutput: Boolean = False; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; var Output: string; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  var Output: string; RawOutput: Boolean = False; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;

function Execute(const CommandLine: string; OutputLineCallback, ErrorLineCallback: TTextHandler;
  RawOutput: Boolean = False; RawError: Boolean = False; AbortPtr: PBoolean = nil; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  OutputLineCallback, ErrorLineCallback: TTextHandler; RawOutput: Boolean = False; RawError: Boolean = False; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; var Output, Error: string;
  RawOutput: Boolean = False; RawError: Boolean = False; AbortPtr: PBoolean = nil; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  var Output, Error: string; RawOutput: Boolean = False; RawError: Boolean = False; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;

type
{$HPPEMIT 'namespace Jclsysutils'}
{$HPPEMIT '{'}
{$HPPEMIT '  // For some reason, the generator puts this interface after its first'}
{$HPPEMIT '  // usage, resulting in an unusable header file. We fix this by forward'}
{$HPPEMIT '  // declaring the interface.'}
{$HPPEMIT '  __interface IJclCommandLineTool;'}
(*$HPPEMIT '}'*)

  IJclCommandLineTool = interface
    ['{A0034B09-A074-D811-847D-0030849E4592}']
    function GetExeName: string;
    function GetOptions: TStrings;
    function GetOutput: string;
    function GetOutputCallback: TTextHandler;
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    property ExeName: string read GetExeName;
    property Options: TStrings read GetOptions;
    property OutputCallback: TTextHandler read GetOutputCallback write SetOutputCallback;
    property Output: string read GetOutput;
  end;

  EJclCommandLineToolError = class(EJclError);

  TJclCommandLineTool = class(TInterfacedObject, IJclCommandLineTool)
  private
    FExeName: string;
    FOptions: TStringList;
    FOutput: string;
    FOutputCallback: TTextHandler;
  public
    constructor Create(const AExeName: string);
    destructor Destroy; override;
    { IJclCommandLineTool }
    function GetExeName: string;
    function GetOptions: TStrings;
    function GetOutput: string;
    function GetOutputCallback: TTextHandler;
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    property ExeName: string read GetExeName;
    property Options: TStrings read GetOptions;
    property OutputCallback: TTextHandler read GetOutputCallback write SetOutputCallback;
    property Output: string read GetOutput;
  end;

// Console Utilities
function ReadKey: Char;

// Loading of modules (DLLs)
type
{$IFDEF MSWINDOWS}
  TModuleHandle = HINST;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  TModuleHandle = Pointer;
{$ENDIF LINUX}

const
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
procedure UnloadModule(var Module: TModuleHandle);
function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;

// Conversion Utilities
type
  EJclConversionError = class(EJclError);

function StrToBoolean(const S: string): Boolean;
function BooleanToStr(B: Boolean): string;
function IntToBool(I: Integer): Boolean;
function BoolToInt(B: Boolean): Integer;

function TryStrToUInt(const Value: string; out Res: Cardinal): Boolean;
function StrToUIntDef(const Value: string; const Default: Cardinal): Cardinal;
function StrToUInt(const Value: string): Cardinal;

const
  {$IFDEF MSWINDOWS}
  ListSeparator = ';';
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ListSeparator = ':';
  {$ENDIF LINUX}

// functions to handle items in a separated list of items
// add items at the end
procedure ListAddItems(var List: string; const Separator, Items: string);
// add items at the end if they are not present
procedure ListIncludeItems(var List: string; const Separator, Items: string);
// delete multiple items
procedure ListRemoveItems(var List: string; const Separator, Items: string);
// delete one item
procedure ListDelItem(var List: string; const Separator: string;
  const Index: Integer);
// return the number of item
function ListItemCount(const List, Separator: string): Integer;
// return the Nth item
function ListGetItem(const List, Separator: string;
  const Index: Integer): string;
// set the Nth item
procedure ListSetItem(var List: string; const Separator: string;
  const Index: Integer; const Value: string);
// return the index of an item
function ListItemIndex(const List, Separator, Item: string): Integer;

// RTL package information
function SystemTObjectInstance: TJclAddr;
function IsCompiledWithPackages: Boolean;

// GUID
function JclGUIDToString(const GUID: TGUID): string;
function JclStringToGUID(const S: string): TGUID;
function GUIDEquals(const GUID1, GUID2: TGUID): Boolean;

// thread safe support

type
  TJclIntfCriticalSection = class(TInterfacedObject, IInterface)
  private
    FCriticalSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    { IInterface }
    // function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

type
  {$IFDEF BORLAND}
  {$IFDEF COMPILER16_UP}
  TFileHandle = THandle;
  {$ELSE ~COMPILER16_UP}
  TFileHandle = Integer;
  {$ENDIF ~COMPILER16_UP}
  {$ELSE ~BORLAND}
  TFileHandle = THandle;
  {$ENDIF ~BORLAND}

  TJclSimpleLog = class (TObject)
  private
    FDateTimeFormatStr: String;
    FLogFileHandle: TFileHandle;
    FLogFileName: string;
    FLoggingActive: Boolean;
    FLogWasEmpty: Boolean;
    function GetLogOpen: Boolean;
  protected
    function CreateDefaultFileName: string;
  public
    constructor Create(const ALogFileName: string = '');
    destructor Destroy; override;
    procedure ClearLog;
    procedure CloseLog;
    procedure OpenLog;
    procedure Write(const Text: string; Indent: Integer = 0; KeepOpen: Boolean = true); overload;
    procedure Write(Strings: TStrings; Indent: Integer = 0; KeepOpen: Boolean = true); overload;
    //Writes a line to the log file. The current timestamp is written before the line.
    procedure TimeWrite(const Text: string; Indent: Integer = 0; KeepOpen: Boolean = true); overload;
    procedure TimeWrite(Strings: TStrings; Indent: Integer = 0; KeepOpen: Boolean = true); overload;
    procedure WriteStamp(SeparatorLen: Integer = 0; KeepOpen: Boolean = true);
    // DateTimeFormatStr property assumes the values described in "FormatDateTime Function" in Delphi Help
    property DateTimeFormatStr: String read FDateTimeFormatStr write FDateTimeFormatStr;
    property LogFileName: string read FLogFileName;
    //1 Property to activate / deactivate the logging
    property LoggingActive: Boolean read FLoggingActive write FLoggingActive default True;
    property LogOpen: Boolean read GetLogOpen;
  end;

type
  TJclFormatSettings = class
  private
    function GetCurrencyDecimals: Byte; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetCurrencyFormat: Byte; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetCurrencyString: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetDateSeparator: Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetDayNamesHighIndex: Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetDayNamesLowIndex: Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetDecimalSeparator: Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetListSeparator: Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetLongDateFormat: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetLongDayNames(AIndex: Integer): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetLongMonthNames(AIndex: Integer): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetLongTimeFormat: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetMonthNamesHighIndex: Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetMonthNamesLowIndex: Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetNegCurrFormat: Byte; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetShortDateFormat: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetShortDayNames(AIndex: Integer): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetShortMonthNames(AIndex: Integer): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetShortTimeFormat: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetThousandSeparator: Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetTimeAMString: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetTimePMString: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetTimeSeparator: Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function GetTwoDigitYearCenturyWindow: Word; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetCurrencyDecimals(AValue: Byte); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetCurrencyFormat(const AValue: Byte); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetCurrencyString(AValue: string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetDateSeparator(const AValue: Char); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetDecimalSeparator(AValue: Char); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetListSeparator(const AValue: Char); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetLongDateFormat(const AValue: string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetLongTimeFormat(const AValue: string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetNegCurrFormat(const AValue: Byte); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetShortDateFormat(AValue: string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetShortTimeFormat(const AValue: string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetThousandSeparator(AValue: Char); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetTimeAMString(const AValue: string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetTimePMString(const AValue: string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetTimeSeparator(const AValue: Char); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetTwoDigitYearCenturyWindow(const AValue: Word); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  public
    property CurrencyDecimals: Byte read GetCurrencyDecimals write SetCurrencyDecimals;
    property CurrencyFormat: Byte read GetCurrencyFormat write SetCurrencyFormat;
    property CurrencyString: string read GetCurrencyString write SetCurrencyString;
    property DateSeparator: Char read GetDateSeparator write SetDateSeparator;
    property DayNamesHighIndex: Integer read GetDayNamesHighIndex;
    property DayNamesLowIndex: Integer read GetDayNamesLowIndex;
    property DecimalSeparator: Char read GetDecimalSeparator write SetDecimalSeparator;
    property ListSeparator: Char read GetListSeparator write SetListSeparator;
    property LongDateFormat: string read GetLongDateFormat write SetLongDateFormat;
    property LongDayNames[AIndex: Integer]: string read GetLongDayNames;
    property LongMonthNames[AIndex: Integer]: string read GetLongMonthNames;
    property LongTimeFormat: string read GetLongTimeFormat write SetLongTimeFormat;
    property MonthNamesHighIndex: Integer read GetMonthNamesHighIndex;
    property MonthNamesLowIndex: Integer read GetMonthNamesLowIndex;
    property NegCurrFormat: Byte read GetNegCurrFormat write SetNegCurrFormat;
    property ShortDateFormat: string read GetShortDateFormat write SetShortDateFormat;
    property ShortDayNames[AIndex: Integer]: string read GetShortDayNames;
    property ShortMonthNames[AIndex: Integer]: string read GetShortMonthNames;
    property ShortTimeFormat: string read GetShortTimeFormat write SetShortTimeFormat;
    property ThousandSeparator: Char read GetThousandSeparator write SetThousandSeparator;
    property TimeAMString: string read GetTimeAMString write SetTimeAMString;
    property TimePMString: string read GetTimePMString write SetTimePMString;
    property TimeSeparator: Char read GetTimeSeparator write SetTimeSeparator;
    property TwoDigitYearCenturyWindow: Word read GetTwoDigitYearCenturyWindow write SetTwoDigitYearCenturyWindow;
  end;

var
  JclFormatSettings: TJclFormatSettings;

// Procedure to initialize the SimpleLog Variable
procedure InitSimpleLog(const ALogFileName: string = ''; AOpenLog: Boolean = true);

// Global Variable to make it easier for an application wide log handling.
// Must be initialized with InitSimpleLog before using
var
  SimpleLog : TJclSimpleLog;


// Validates if then variant value is null or is empty
function VarIsNullEmpty(const V: Variant): Boolean;
// Validates if then variant value is null or is empty or VarToStr is a blank string
function VarIsNullEmptyBlank(const V: Variant): Boolean;


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF MSWINDOWS}
  JclConsole,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNITSCOPE}
  System.Variants, System.Types, System.Contnrs,
  {$IFDEF HAS_UNIT_ANSISTRINGS}
  System.AnsiStrings,
  {$ENDIF HAS_UNIT_ANSISTRINGS}
  {$ELSE ~HAS_UNITSCOPE}
  Variants, Types, Contnrs,
  {$IFDEF HAS_UNIT_ANSISTRINGS}
  AnsiStrings,
  {$ENDIF HAS_UNIT_ANSISTRINGS}
  {$ENDIF ~HAS_UNITSCOPE}
  JclFileUtils, JclMath, JclResources, JclStrings,
  JclStringConversions, JclSysInfo, JclWin32;

// memory initialization
procedure ResetMemory(out P; Size: Longint);
begin
  if Size > 0 then
  begin
    Byte(P) := 0;
    FillChar(P, Size, 0);
  end;
end;

// Pointer manipulation
procedure GetAndFillMem(var P: Pointer; const Size: Integer; const Value: Byte);
begin
  GetMem(P, Size);
  FillChar(P^, Size, Value);
end;

procedure FreeMemAndNil(var P: Pointer);
var
  Q: Pointer;
begin
  Q := P;
  P := nil;
  FreeMem(Q);
end;

function PCharOrNil(const S: string): PChar;
begin
  Result := Pointer(S);
end;

function PAnsiCharOrNil(const S: AnsiString): PAnsiChar;
begin
  Result := Pointer(S);
end;

{$IFDEF SUPPORTS_WIDESTRING}

function PWideCharOrNil(const W: WideString): PWideChar;
begin
  Result := Pointer(W);
end;

{$ENDIF SUPPORTS_WIDESTRING}

{$IFDEF MSWINDOWS}
type
  PUsed = ^TUsed;
  TUsed = record
    SizeFlags: Integer;
  end;

const
  cThisUsedFlag = 2;
  cPrevFreeFlag = 1;
  cFillerFlag   = Integer($80000000);
  cFlags        = cThisUsedFlag or cPrevFreeFlag or cFillerFlag;

function SizeOfMem(const APointer: Pointer): Integer;
var
  U: PUsed;
begin
  if IsMemoryManagerSet then
    Result:= -1
  else
  begin
    Result := 0;
    if APointer <> nil then
    begin
      U := APointer;
      U := PUsed(TJclAddr(U) - SizeOf(TUsed));
      if (U.SizeFlags and cThisUsedFlag) <> 0 then
        Result := (U.SizeFlags) and (not cFlags - SizeOf(TUsed));
    end;
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
function SizeOfMem(const APointer: Pointer): Integer;
begin
  if IsMemoryManagerSet then
    Result:= -1
  else
  begin
    if APointer <> nil then
      Result := malloc_usable_size(APointer)
    else
      Result := 0;
  end;
end;
{$ENDIF LINUX}

function WriteProtectedMemory(BaseAddress, Buffer: Pointer;
  Size: Cardinal; out WrittenBytes: Cardinal): Boolean;
{$IFDEF MSWINDOWS}
var
  OldProtect, Dummy: Cardinal;
begin
  WrittenBytes := 0;
  if Size > 0 then
  begin
    // (outchy) VirtualProtect for DEP issues
    OldProtect := 0;
    Result := VirtualProtect(BaseAddress, Size, PAGE_EXECUTE_READWRITE, OldProtect);
    if Result then
    try
      Move(Buffer^, BaseAddress^, Size);
      WrittenBytes := Size;
      if OldProtect in [PAGE_EXECUTE, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE, PAGE_EXECUTE_WRITECOPY] then
        FlushInstructionCache(GetCurrentProcess, BaseAddress, Size);
    finally
      Dummy := 0;
      VirtualProtect(BaseAddress, Size, OldProtect, Dummy);
    end;
  end;
  Result := WrittenBytes = Size;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{ TODO -cHelp : Author: Andreas Hausladen }
{ TODO : Works so far, but causes app to hang on termination }
var
  AlignedAddress: Cardinal;
  PageSize, ProtectSize: Cardinal;
begin
  Result := False;
  WrittenBytes := 0;

  PageSize := Cardinal(getpagesize);
  AlignedAddress := Cardinal(BaseAddress) and not (PageSize - 1); // start memory page
  // get the number of needed memory pages
  ProtectSize := PageSize;
  while Cardinal(BaseAddress) + Size > AlignedAddress + ProtectSize do
    Inc(ProtectSize, PageSize);

  if mprotect(Pointer(AlignedAddress), ProtectSize,
    PROT_READ or PROT_WRITE or PROT_EXEC) = 0 then // obtain write access
  begin
    try
      Move(Buffer^, BaseAddress^, Size); // replace code
      Result := True;
      WrittenBytes := Size;
    finally
      // Is there any function that returns the current page protection?
//    mprotect(p, ProtectSize, PROT_READ or PROT_EXEC); // lock memory page
    end;
  end;
end;

procedure FlushInstructionCache;
{ TODO -cHelp : Author: Andreas Hausladen }
begin
  // do nothing
end;

{$ENDIF LINUX}

// Guards

//=== { TJclSafeGuard } ======================================================

constructor TJclSafeGuard.Create(Mem: Pointer);
begin
  inherited Create;
  FItem := Mem;
end;

destructor TJclSafeGuard.Destroy;
begin
  FreeItem;
  inherited Destroy;
end;

function TJclSafeGuard.ReleaseItem: Pointer;
begin
  Result := FItem;
  FItem := nil;
end;

function TJclSafeGuard.GetItem: Pointer;
begin
  Result := FItem;
end;

procedure TJclSafeGuard.FreeItem;
begin
  if FItem <> nil then
    FreeMem(FItem);
  FItem := nil;
end;

//=== { TJclObjSafeGuard } ===================================================

constructor TJclObjSafeGuard.Create(Obj: TObject);
begin
  inherited Create(Pointer(Obj));
end;

procedure TJclObjSafeGuard.FreeItem;
begin
  if FItem <> nil then
  begin
    TObject(FItem).Free;
    FItem := nil;
  end;
end;

//=== { TJclMultiSafeGuard } =================================================

constructor TJclMultiSafeGuard.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TJclMultiSafeGuard.Destroy;
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
    FreeItem(I);
  FItems.Free;
  inherited Destroy;
end;

function TJclMultiSafeGuard.AddItem(Item: Pointer): Pointer;
begin
  Result := Item;
  FItems.Add(Item);
end;

procedure TJclMultiSafeGuard.FreeItem(Index: Integer);
begin
  FreeMem(FItems[Index]);
  FItems.Delete(Index);
end;

function TJclMultiSafeGuard.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclMultiSafeGuard.GetItem(Index: Integer): Pointer;
begin
  Result := FItems[Index];
end;

function TJclMultiSafeGuard.ReleaseItem(Index: Integer): Pointer;
begin
  Result := FItems[Index];
  FItems.Delete(Index);
end;

function Guard(Mem: Pointer; var SafeGuard: IMultiSafeGuard): Pointer; overload;
begin
  if SafeGuard = nil then
    SafeGuard := TJclMultiSafeGuard.Create;
  Result := SafeGuard.AddItem(Mem);
end;

//=== { TJclObjMultiSafeGuard } ==============================================

procedure TJclObjMultiSafeGuard.FreeItem(Index: Integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

function Guard(Obj: TObject; var SafeGuard: IMultiSafeGuard): TObject; overload;
begin
  if SafeGuard = nil then
    SafeGuard := TJclObjMultiSafeGuard.Create;
  Result := SafeGuard.AddItem(Obj);
end;

function Guard(Mem: Pointer; out SafeGuard: ISafeGuard): Pointer; overload;
begin
  Result := Mem;
  SafeGuard := TJclSafeGuard.Create(Mem);
end;

function Guard(Obj: TObject; out SafeGuard: ISafeGuard): TObject; overload;
begin
  Result := Obj;
  SafeGuard := TJclObjSafeGuard.Create(Obj);
end;

function GuardGetMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
begin
  GetMem(Result, Size);
  Guard(Result, SafeGuard);
end;

function GuardAllocMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
begin
  Result := AllocMem(Size);
  Guard(Result, SafeGuard);
end;

{$IFDEF SUPPORTS_GENERICS_}
//=== { TSafeGuard<T> } ======================================================

constructor TSafeGuard<T>.Create(Instance: T);
begin
  inherited Create;
  FItem := Instance;
end;

destructor TSafeGuard<T>.Destroy;
begin
  FreeItem;
  inherited Destroy;
end;

function TSafeGuard<T>.ReleaseItem: T;
begin
  Result := FItem;
  FItem := nil;
end;

function TSafeGuard<T>.GetItem: T;
begin
  Result := FItem;
end;

procedure TSafeGuard<T>.FreeItem;
begin
  if FItem <> nil then
    FItem.Free;
  FItem := nil;
end;
{$ENDIF SUPPORTS_GENERICS_}

//=== Shared memory functions ================================================

type
  PMMFHandleListItem = ^TMMFHandleListItem;
  TMMFHandleListItem = record
    Next: PMMFHandleListItem;
    Memory: Pointer;
    Handle: THandle;
    Name: string;
    References: Integer;
  end;

  PMMFHandleList = PMMFHandleListItem;

var
  MMFHandleList: PMMFHandleList = nil;
  {$IFDEF THREADSAFE}
  MMFFinalized: Boolean = False;
  GlobalMMFHandleListCS: TJclIntfCriticalSection = nil;
  {$ENDIF THREADSAFE}

{$IFDEF THREADSAFE}
function GetAccessToHandleList: IInterface;
var
  OldValue: Pointer;
  CS: TJclIntfCriticalSection;
begin
  if not Assigned(GlobalMMFHandleListCS) and not MMFFinalized then
  begin
    CS := TJclIntfCriticalSection.Create;
    {$IFDEF RTL200_UP} // Delphi 2009+
    OldValue := InterlockedCompareExchangePointer(Pointer(GlobalMMFHandleListCS), Pointer(CS), nil);
    {$ELSE}
      {$IFDEF RTL160_UP} // Delphi 7-2007
    OldValue := Pointer(InterlockedCompareExchange(Longint(GlobalMMFHandleListCS), Longint(CS), 0));
      {$ELSE} // Delphi 5, 6
    OldValue := InterlockedCompareExchange(Pointer(GlobalMMFHandleListCS), Pointer(CS), nil);
      {$ENDIF RTL180_UP}
    {$ENDIF RTL185_UP}
    if OldValue <> nil then
      CS.Free;
  end;
  Result := GlobalMMFHandleListCS;
end;
{$ENDIF THREADSAFE}

{$IFDEF MSWINDOWS}

function SharedGetMem(var P{: Pointer}; const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Integer;
var
  FileMappingHandle: THandle;
  Iterate, NewListItem: PMMFHandleListItem;
  Protect: Cardinal;
  {$IFDEF THREADSAFE}
  HandleListAccess: IInterface;
  {$ENDIF THREADSAFE}
begin
  Result := 0;
  Pointer(P) := nil;

  if not JclCheckWinVersion(5, 0) and ((Name = '') or (Pos('\', Name) > 0)) then
    raise ESharedMemError.CreateResFmt(@RsInvalidMMFName, [Name]);

  {$IFDEF THREADSAFE}
  HandleListAccess := GetAccessToHandleList;
  {$ENDIF THREADSAFE}

  // search for same name
  Iterate := MMFHandleList;
  while Iterate <> nil do
  begin
    if CompareText(Iterate^.Name, Name) = 0 then
    begin
      Inc(Iterate^.References);
      Pointer(P) := Iterate^.Memory;
      Result := ERROR_ALREADY_EXISTS;
      Exit;
    end;
    Iterate := Iterate^.Next;
  end;

  // open file mapping
  FileMappingHandle := OpenFileMapping(DesiredAccess, False, PChar(Name));
  if FileMappingHandle = 0 then
  begin
    if Size = 0 then
      raise ESharedMemError.CreateResFmt(@RsInvalidMMFEmpty, [Name]);

    Protect := PAGE_READWRITE;
    if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (DesiredAccess = FILE_MAP_COPY) then
      Protect := PAGE_WRITECOPY;

    FileMappingHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, Protect,
      0, Size, PChar(Name));
  end
  else
    Result := ERROR_ALREADY_EXISTS;

  if GetLastError = ERROR_ALREADY_EXISTS then
    Result := ERROR_ALREADY_EXISTS
  else
  begin
    if FileMappingHandle = 0 then
      RaiseLastOSError;
  end;

  // map view
  Pointer(P) := MapViewOfFile(FileMappingHandle, DesiredAccess, 0, 0, Size);
  if Pointer(P) = nil then
  begin
    try
      RaiseLastOSError;
    except
      CloseHandle(FileMappingHandle);
      raise;
    end;
  end;

  // add list item to MMFHandleList
  New(NewListItem);
  NewListItem^.Name := Name;
  NewListItem^.Handle := FileMappingHandle;
  NewListItem^.Memory := Pointer(P);
  NewListItem^.References := 1;

  NewListItem^.Next := MMFHandleList;
  MMFHandleList := NewListItem;
end;

function SharedAllocMem(const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;
begin
  Result := nil;
  if (SharedGetMem(Result, Name, Size, DesiredAccess) <> ERROR_ALREADY_EXISTS) and
    ((DesiredAccess and (FILE_MAP_WRITE or FILE_MAP_COPY)) <> 0) and
    (Size > 0) and (Result <> nil) then
      ResetMemory(Pointer(Result)^, Size);
end;

function SharedFreeMem(var P{: Pointer}): Boolean;
var
  N, Iterate: PMMFHandleListItem;
  {$IFDEF THREADSAFE}
  HandleListAccess: IInterface;
  {$ENDIF THREADSAFE}
begin
  if Pointer(P) <> nil then
  begin
    Result := False;
    {$IFDEF THREADSAFE}
    HandleListAccess := GetAccessToHandleList;
    {$ENDIF THREADSAFE}
    Iterate := MMFHandleList;
    N := nil;
    while Iterate <> nil do
    begin
      if Iterate^.Memory = Pointer(P) then
      begin
        if Iterate^.References > 1 then
        begin
          Dec(Iterate^.References);
          Pointer(P) := nil;
          Result := True;
          Exit;
        end;

        UnmapViewOfFile(Iterate^.Memory);
        CloseHandle(Iterate^.Handle);

        if N = nil then
          MMFHandleList := Iterate^.Next
        else
          N^.Next := Iterate^.Next;

        Dispose(Iterate);
        Pointer(P) := nil;
        Result := True;
        Break;
      end;
      N := Iterate;
      Iterate := Iterate^.Next;
    end;
  end
  else
    Result := True;
end;

function SharedOpenMem(var P{: Pointer}; const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Boolean;
begin
  Result := SharedGetMem(P, Name, 0, DesiredAccess) = ERROR_ALREADY_EXISTS;
end;

function SharedOpenMem(const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;
begin
  Result := nil;
  SharedGetMem(Result, Name, 0, DesiredAccess);
end;

function SharedCloseMem(var P{: Pointer}): Boolean;
begin
  Result := SharedFreeMem(P);
end;

{$ENDIF MSWINDOWS}

//=== Binary search ==========================================================

function SearchSortedList(List: TList; SortFunc: TListSortCompare; Item: Pointer; Nearest: Boolean): Integer;
var
  L, H, I, C: Integer;
  B: Boolean;
begin
  Result := -1;
  if List <> nil then
  begin
    L := 0;
    H := List.Count - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := SortFunc(List.List{$IFNDEF RTL230_UP}^{$ENDIF !RTL230_UP}[I], Item);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else
    if Nearest and (H >= 0) then
      Result := H;
  end;
end;

function SearchSortedUntyped(Param: Pointer; ItemCount: Integer; SearchFunc: TUntypedSearchCompare;
  const Value; Nearest: Boolean): Integer;
var
  L, H, I, C: Integer;
  B: Boolean;
begin
  Result := -1;
  if ItemCount > 0 then
  begin
    L := 0;
    H := ItemCount - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := SearchFunc(Param, I, Value);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else
    if Nearest and (H >= 0) then
      Result := H;
  end;
end;

//=== Dynamic array sort and search routines =================================

procedure SortDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare);
var
  TempBuf: TDynByteArray;

  function ArrayItemPointer(Item: SizeInt): Pointer;
  begin
    Assert(Item >= 0);
    Result := Pointer(TJclAddr(ArrayPtr) + TJclAddr(Item * SizeInt(ElementSize)));
  end;

  procedure QuickSort(L, R: SizeInt);
  var
    I, J, T: SizeInt;
    P, IPtr, JPtr: Pointer;
    ElSize: Integer;
  begin
    ElSize := ElementSize;
    repeat
      I := L;
      J := R;
      P := ArrayItemPointer((L + R) shr 1);
      repeat
        IPtr := ArrayItemPointer(I);
        JPtr := ArrayItemPointer(J);
        while SortFunc(IPtr, P) < 0 do
        begin
          Inc(I);
          Inc(PByte(IPtr), ElSize);
        end;
        while SortFunc(JPtr, P) > 0 do
        begin
          Dec(J);
          Dec(PByte(JPtr), ElSize);
        end;
        if I <= J then
        begin
          if I <> J then
          begin
            case ElementSize of
              SizeOf(Byte):
                begin
                  T := PByte(IPtr)^;
                  PByte(IPtr)^ := PByte(JPtr)^;
                  PByte(JPtr)^ := T;
                end;
              SizeOf(Word):
                begin
                  T := PWord(IPtr)^;
                  PWord(IPtr)^ := PWord(JPtr)^;
                  PWord(JPtr)^ := T;
                end;
              SizeOf(Integer):
                begin
                  T := PInteger(IPtr)^;
                  PInteger(IPtr)^ := PInteger(JPtr)^;
                  PInteger(JPtr)^ := T;
                end;
            else
              Move(IPtr^, TempBuf[0], ElementSize);
              Move(JPtr^, IPtr^, ElementSize);
              Move(TempBuf[0], JPtr^, ElementSize);
            end;
          end;
          if P = IPtr then
            P := JPtr
          else
          if P = JPtr then
            P := IPtr;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if ArrayPtr <> nil then
  begin
    SetLength(TempBuf, ElementSize);
    QuickSort(0, PSizeInt(TJclAddr(ArrayPtr) - SizeOf(SizeInt))^ - 1);
  end;
end;

function SearchDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare;
  ValuePtr: Pointer; Nearest: Boolean): SizeInt;
var
  L, H, I, C: SizeInt;
  B: Boolean;
begin
  Result := -1;
  if ArrayPtr <> nil then
  begin
    L := 0;
    H := PSizeInt(TJclAddr(ArrayPtr) - SizeOf(SizeInt))^ - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := SortFunc(Pointer(TJclAddr(ArrayPtr) + TJclAddr(I * SizeInt(ElementSize))), ValuePtr);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else
    if Nearest and (H >= 0) then
      Result := H;
  end;
end;

{ Various compare functions for basic types }

function DynArrayCompareByte(Item1, Item2: Pointer): Integer;
begin
  Result := PByte(Item1)^ - PByte(Item2)^;
end;

function DynArrayCompareShortInt(Item1, Item2: Pointer): Integer;
begin
  Result := PShortInt(Item1)^ - PShortInt(Item2)^;
end;

function DynArrayCompareWord(Item1, Item2: Pointer): Integer;
begin
  Result := PWord(Item1)^ - PWord(Item2)^;
end;

function DynArrayCompareSmallInt(Item1, Item2: Pointer): Integer;
begin
  Result := PSmallInt(Item1)^ - PSmallInt(Item2)^;
end;

function DynArrayCompareInteger(Item1, Item2: Pointer): Integer;
begin
  if PInteger(Item1)^ < PInteger(Item2)^ then
    Result := -1
  else
  if PInteger(Item1)^ > PInteger(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareCardinal(Item1, Item2: Pointer): Integer;
begin
  if PCardinal(Item1)^ < PCardinal(Item2)^ then
    Result := -1
  else
  if PCardinal(Item1)^ > PCardinal(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareInt64(Item1, Item2: Pointer): Integer;
begin
  if PInt64(Item1)^ < PInt64(Item2)^ then
    Result := -1
  else
  if PInt64(Item1)^ > PInt64(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareSingle(Item1, Item2: Pointer): Integer;
begin
  if PSingle(Item1)^ < PSingle(Item2)^ then
    Result := -1
  else
  if PSingle(Item1)^ > PSingle(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareDouble(Item1, Item2: Pointer): Integer;
begin
  if PDouble(Item1)^ < PDouble(Item2)^ then
    Result := -1
  else
  if PDouble(Item1)^ > PDouble(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareExtended(Item1, Item2: Pointer): Integer;
begin
  if PExtended(Item1)^ < PExtended(Item2)^ then
    Result := -1
  else
  if PExtended(Item1)^ > PExtended(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareFloat(Item1, Item2: Pointer): Integer;
begin
  if PFloat(Item1)^ < PFloat(Item2)^ then
    Result := -1
  else
  if PFloat(Item1)^ > PFloat(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareAnsiString(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(PAnsiString(Item1)^, PAnsiString(Item2)^);
end;

function DynArrayCompareAnsiText(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(PAnsiString(Item1)^, PAnsiString(Item2)^);
end;

function DynArrayCompareWideString(Item1, Item2: Pointer): Integer;
begin
  Result := WideCompareStr(PWideString(Item1)^, PWideString(Item2)^);
end;

function DynArrayCompareWideText(Item1, Item2: Pointer): Integer;
begin
  Result := WideCompareText(PWideString(Item1)^, PWideString(Item2)^);
end;

function DynArrayCompareString(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(PString(Item1)^, PString(Item2)^);
end;

function DynArrayCompareText(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(PString(Item1)^, PString(Item2)^);
end;

//=== Object lists ===========================================================

procedure ClearObjectList(List: TList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := List.Count - 1 downto 0 do
    begin
      if List[I] <> nil then
      begin
        if TObject(List[I]) is TList then
        begin
          // recursively delete TList sublists
          ClearObjectList(TList(List[I]));
        end;
        TObject(List[I]).Free;
        if (not (List is TComponentList))
          and ((not(List is TObjectList)) or not TObjectList(List).OwnsObjects) then
          List[I] := nil;
      end;
    end;
    List.Clear;
  end;
end;

procedure FreeObjectList(var List: TList);
begin
  if List <> nil then
  begin
    ClearObjectList(List);
    FreeAndNil(List);
  end;
end;

//=== { TJclReferenceMemoryStream } ==========================================

constructor TJclReferenceMemoryStream.Create(const Ptr: Pointer; Size: Longint);
begin
  {$IFDEF MSWINDOWS}
  Assert(not IsBadReadPtr(Ptr, Size));
  {$ENDIF MSWINDOWS}
  inherited Create;
  SetPointer(Ptr, Size);
end;

function TJclReferenceMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EJclError.CreateRes(@RsCannotWriteRefStream);
end;

//=== { TJclAutoPtr } ========================================================

constructor TJclAutoPtr.Create(AValue: TObject);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TJclAutoPtr.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

function TJclAutoPtr.AsObject: TObject;
begin
  Result := FValue;
end;

function TJclAutoPtr.AsPointer: Pointer;
begin
  Result := FValue;
end;

function TJclAutoPtr.ReleaseObject: TObject;
begin
  Result := FValue;
  FValue := nil;
end;

function CreateAutoPtr(Value: TObject): IAutoPtr;
begin
  Result := TJclAutoPtr.Create(Value);
end;

//=== replacement for the C distfix operator ? : =============================

function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Float): Float;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

{$IFDEF SUPPORTS_VARIANT}
function Iff(const Condition: Boolean; const TruePart, FalsePart: Variant): Variant; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;
{$ENDIF SUPPORTS_VARIANT}

//=== Classes information and manipulation ===================================
// Virtual Methods
// Helper method

procedure SetVMTPointer(AClass: TClass; Offset: Integer; Value: Pointer);
var
  WrittenBytes: DWORD;
  PatchAddress: PPointer;
begin
  {$OVERFLOWCHECKS OFF}
  PatchAddress := Pointer(TJclAddr(AClass) + TJclAddr(Offset));
  {$IFDEF OVERFLOWCHECKS_ON}
  {$OVERFLOWCHECKS ON}
  {$ENDIF OVERFLOWCHECKS_ON}
  if not WriteProtectedMemory(PatchAddress, @Value, SizeOf(Value), WrittenBytes) then
    raise EJclVMTError.CreateResFmt(@RsVMTMemoryWriteError,
      [SysErrorMessage({$IFDEF FPC}GetLastOSError{$ELSE}GetLastError{$ENDIF})]);

  if WrittenBytes <> SizeOf(Pointer) then
    raise EJclVMTError.CreateResFmt(@RsVMTMemoryWriteError, [IntToStr(WrittenBytes)]);

  // make sure that everything keeps working in a dual processor setting
  // (outchy) done by WriteProtectedMemory
  // FlushInstructionCache{$IFDEF MSWINDOWS}(GetCurrentProcess, PatchAddress, SizeOf(Pointer)){$ENDIF};
end;

{$IFNDEF FPC}
function GetVirtualMethodCount(AClass: TClass): Integer;
type
  PINT_PTR = ^INT_PTR;
var
  BeginVMT: INT_PTR;
  EndVMT: INT_PTR;
  TablePointer: INT_PTR;
  I: Integer;
begin
  BeginVMT := INT_PTR(AClass);

  // Scan the offset entries in the class table for the various fields,
  // namely vmtIntfTable, vmtAutoTable, ..., vmtDynamicTable
  // The last entry is always the vmtClassName, so stop once we got there
  // After the last virtual method there is one of these entries.

  EndVMT := PINT_PTR(INT_PTR(AClass) + vmtClassName)^;
  // Set iterator to first item behind VMT table pointer
  I := vmtSelfPtr + SizeOf(Pointer);
  repeat
    TablePointer := PINT_PTR(INT_PTR(AClass) + I)^;
    if (TablePointer <> 0) and (TablePointer >= BeginVMT) and
       (TablePointer < EndVMT) then
      EndVMT := INT_PTR(TablePointer);
    Inc(I, SizeOf(Pointer));
  until I >= vmtClassName;

  Result := (EndVMT - BeginVMT) div SizeOf(Pointer);
end;
{$ENDIF ~FPC}

function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
begin
  {$OVERFLOWCHECKS OFF}
  Result := PPointer(TJclAddr(AClass) + TJclAddr(Index * SizeOf(Pointer)))^;
  {$IFDEF OVERFLOWCHECKS_ON}
  {$OVERFLOWCHECKS ON}
  {$ENDIF OVERFLOWCHECKS_ON}
end;

procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);
begin
  SetVMTPointer(AClass, Index * SizeOf(Pointer), Method);
end;

function GetDynamicMethodCount(AClass: TClass): Integer; assembler;
asm
        {$IFDEF CPU32}
        // --> RAX AClass
        // <-- EAX Result
        MOV     EAX, [EAX].vmtDynamicTable
        TEST    EAX, EAX
        JE      @@Exit
        MOVZX   EAX, WORD PTR [EAX]
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX AClass
        // <-- EAX Result
        MOV     RAX, [RCX].vmtDynamicTable
        TEST    RAX, RAX
        JE      @@Exit
        MOVZX   RAX, WORD PTR [RAX]
        {$ENDIF CPU64}
@@Exit:
end;

function GetDynamicIndexList(AClass: TClass): PDynamicIndexList; assembler;
asm
        {$IFDEF CPU32}
        // --> EAX AClass
        // <-- EAX Result
        MOV     EAX, [EAX].vmtDynamicTable
        ADD     EAX, 2
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX AClass
        // <-- RAX Result
        MOV     RAX, [RCX].vmtDynamicTable
        ADD     RAX, 2
        {$ENDIF CPU64}
end;

function GetDynamicAddressList(AClass: TClass): PDynamicAddressList; assembler;
asm
        {$IFDEF CPU32}
        // --> EAX AClass
        // <-- EAX Result
        MOV     EAX, [EAX].vmtDynamicTable
        MOVZX   EDX, Word ptr [EAX]
        ADD     EAX, EDX
        ADD     EAX, EDX
        ADD     EAX, 2
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX AClass
        // <-- RAX Result
        MOV     RAX, [RCX].vmtDynamicTable
        MOVZX   RDX, Word ptr [RAX]
        ADD     RAX, RDX
        ADD     RAX, RDX
        ADD     RAX, 2
        {$ENDIF CPU64}
end;

function HasDynamicMethod(AClass: TClass; Index: Integer): Boolean; assembler;
// Mainly copied from System.GetDynaMethod
asm
        {$IFDEF CPU32}
        // --> EAX AClass
        //     EDX Index
        // <-- AL  Result
        PUSH    EDI
        XCHG    EAX, EDX
        JMP     @@HaveVMT
@@OuterLoop:
        MOV     EDX, [EDX]
@@HaveVMT:
        MOV     EDI, [EDX].vmtDynamicTable
        TEST    EDI, EDI
        JE      @@Parent
        MOVZX   ECX, WORD PTR [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@Found
        POP     ECX
@@Parent:
        MOV     EDX,[EDX].vmtParent
        TEST    EDX,EDX
        JNE     @@OuterLoop
        MOV     EAX, 0
        JMP     @@Exit
@@Found:
        POP     EAX
        MOV     EAX, 1
@@Exit:
        POP     EDI
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX AClass
        //     EDX Index
        // <-- AL  Result
        MOV     EAX, EDX
        MOV     RDX, RCX
        JMP     @@HaveVMT
@@OuterLoop:
        MOV     RDX, [RDX]
@@HaveVMT:
        MOV     RDI, [RDX].vmtDynamicTable
        TEST    RDI, RDI
        JE      @@Parent
        MOVZX   RCX, WORD PTR [RDI]
        PUSH    RCX
        ADD     RDI,2
        REPNE   SCASW
        JE      @@Found
        POP     RCX
@@Parent:
        MOV     RDX,[RDX].vmtParent
        TEST    RDX,RDX
        JNE     @@OuterLoop
        MOV     RAX, 0
        JMP     @@Exit
@@Found:
        POP     RAX
        MOV     RAX, 1
@@Exit:
        {$ENDIF CPU64}
end;

{$IFNDEF FPC}
function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer; assembler;
asm
        CALL    System.@FindDynaClass
end;
{$ENDIF ~FPC}

//=== Interface Table ========================================================

function GetInitTable(AClass: TClass): PTypeInfo; assembler;
asm
        {$IFDEF CPU32}
        // --> EAX AClass
        // <-- EAX Result
        MOV     EAX, [EAX].vmtInitTable
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX AClass
        // <-- RAX Result
        MOV     RAX, [RCX].vmtInitTable
        {$ENDIF CPU64}
end;

function GetFieldTable(AClass: TClass): PFieldTable; assembler;
asm
        {$IFDEF CPU32}
        // --> EAX AClass
        // <-- EAX Result
        MOV     EAX, [EAX].vmtFieldTable
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX AClass
        // <-- RAX Result
        MOV     RAX, [RCX].vmtFieldTable
        {$ENDIF CPU64}
end;

function GetMethodTable(AClass: TClass): PMethodTable; assembler;
asm
        {$IFDEF CPU32}
        // --> EAX AClass
        // <-- EAX Result
        MOV     EAX, [EAX].vmtMethodTable
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX AClass
        // <-- RAX Result
        MOV     RAX, [RCX].vmtMethodTable
        {$ENDIF CPU64}
end;

function GetMethodEntry(MethodTable: PMethodTable; Index: Integer): PMethodEntry;
begin
  Result := Pointer(TJclAddr(MethodTable) + 2);
  for Index := Index downto 1 do
    Inc(TJclAddr(Result), Result^.EntrySize);
end;

//=== Class Parent methods ===================================================

procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
var
  WrittenBytes: DWORD;
  PatchAddress: Pointer;
begin
  {$OVERFLOWCHECKS OFF}
  PatchAddress := PPointer(TJclAddr(AClass) + TJclAddr(vmtParent))^;
  {$IFDEF OVERFLOWCHECKS_ON}
  {$OVERFLOWCHECKS ON}
  {$ENDIF OVERFLOWCHECKS_ON}
  if not WriteProtectedMemory(PatchAddress, @NewClassParent, SizeOf(Pointer), WrittenBytes) then
    raise EJclVMTError.CreateResFmt(@RsVMTMemoryWriteError,
      [SysErrorMessage({$IFDEF FPC}GetLastOSError{$ELSE}GetLastError{$ENDIF})]);
  if WrittenBytes <> SizeOf(Pointer) then
    raise EJclVMTError.CreateResFmt(@RsVMTMemoryWriteError, [IntToStr(WrittenBytes)]);
  // make sure that everything keeps working in a dual processor setting
  // (outchy) done by WriteProtectedMemory
  // FlushInstructionCache{$IFDEF MSWINDOWS}(GetCurrentProcess, PatchAddress, SizeOf(Pointer)){$ENDIF};
end;

function GetClassParent(AClass: TClass): TClass; assembler;
asm
        {$IFDEF CPU32}
        // --> EAX AClass
        // <-- EAX Result
        MOV     EAX, [EAX].vmtParent
        TEST    EAX, EAX
        JE      @@Exit
        MOV     EAX, [EAX]
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX AClass
        // <-- RAX Result
        MOV     RAX, [RCX].vmtParent
        TEST    RAX, RAX
        JE      @@Exit
        MOV     RAX, [RAX]
        {$ENDIF CPU64}
@@Exit:
end;

{$IFDEF BORLAND}
function IsClass(Address: Pointer): Boolean; assembler;
asm
        CMP     Address, Address.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;
{$ENDIF BORLAND}

{$IFDEF BORLAND}
function IsObject(Address: Pointer): Boolean; assembler;
asm
// or IsClass(Pointer(Address^));
        MOV     EAX, [Address]
        CMP     EAX, EAX.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;
{$ENDIF BORLAND}

function InheritsFromByName(AClass: TClass; const AClassName: string): Boolean;
begin
  while (AClass <> nil) and not AClass.ClassNameIs(AClassName) do
    AClass := AClass.ClassParent;
  Result := AClass <> nil;
end;

//=== Interface information ==================================================

function GetImplementorOfInterface(const I: IInterface): TObject;
{ TODO -cDOC : Original code by Hallvard Vassbotn }
{ TODO -cTesting : Check the implemetation for any further version of compiler }
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: Longint of
      AddByte: (AdjustmentByte: ShortInt);
      AddLong: (AdjustmentLong: Longint);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  try
    Result := Pointer(I);
    if Assigned(Result) then
    begin
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte:
          Inc(PByte(Result), QueryInterfaceThunk.AdjustmentByte);
        AddLong:
          Inc(PByte(Result), QueryInterfaceThunk.AdjustmentLong);
      else
        Result := nil;
      end;
    end;
  except
    Result := nil;
  end;
end;

//=== { TJclInterfacedPersistent } ===========================================

procedure TJclInterfacedPersistent.AfterConstruction;
begin
  inherited AfterConstruction;
  if GetOwner <> nil then
    GetOwner.GetInterface(IInterface, FOwnerInterface);
end;

function TJclInterfacedPersistent._AddRef: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef
  else
    Result := InterlockedIncrement(FRefCount);
end;

function TJclInterfacedPersistent._Release: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release
  else
  begin
    Result := InterlockedDecrement(FRefCount);
    if Result = 0 then
      Destroy;
  end;
end;

//=== Numeric formatting routines ============================================

function IntToStrZeroPad(Value, Count: Integer): string;
begin
  Result := IntToStr(Value);
  if Length(Result) < Count then
    Result := StrRepeatChar('0', Count - Length(Result)) + Result;
end;

//=== { TJclNumericFormat } ==================================================

{ TODO -cHelp : Author: Robert Rossmair }
{ Digit:         converts a digit value (number) to a digit (char)
  DigitValue:    converts a digit (char) into a number (digit value)
  IntToStr,
  FloatToStr,
  FloatToHTML:   converts a numeric value to a base <Base> numeric representation with formating options
  StrToIn:       converts a base <Base> numeric representation into an integer, if possible
  GetMantisseExponent: similar to AsString, but returns the Exponent separately as an integer
}
const
  {$IFDEF MATH_EXTENDED_PRECISION}
  BinaryPrecision = 64;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  BinaryPrecision = 53;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  BinaryPrecision = 24;
  {$ENDIF MATH_SINGLE_PRECISION}

constructor TJclNumericFormat.Create;
begin
  inherited Create;
  { TODO : Initialize, when possible, from locale info }
  FBase := 10;
  FExpDivision := 1;
  SetPrecision(6);
  FNumberOfFractionalDigits := BinaryPrecision;
  FSignChars[False] := '-';
  FSignChars[True] := '+';
  FPaddingChar := ' ';
  FMultiplier := '×';
  FFractionalPartSeparator := JclFormatSettings.DecimalSeparator;
  FDigitBlockSeparator := JclFormatSettings.ThousandSeparator;
end;

procedure TJclNumericFormat.InvalidDigit(Digit: Char);
begin
  raise EConvertError.CreateResFmt(@RsInvalidDigit, [Base, Digit]);
end;

function TJclNumericFormat.Digit(DigitValue: TDigitValue): Char;
begin
  Assert(DigitValue < Base, Format(LoadResString(@RsInvalidDigitValue), [Base, DigitValue]));
  if DigitValue > 9 then
    Result := Chr(Ord('A') + DigitValue - 10)
  else
    Result := Chr(Ord('0') + DigitValue);
end;

function TJclNumericFormat.GetDigitValue(Digit: Char): Integer;
begin
  Result := CharHex(Digit);
  if (Result = $FF) or (Result >= Base) then
    Result := -1;
end;

function TJclNumericFormat.DigitValue(Digit: Char): TDigitValue;
begin
  Result := GetDigitValue(Digit);
  if Result = -1 then
    InvalidDigit(Digit);
end;

function TJclNumericFormat.IsDigit(Value: Char): Boolean;
begin
  Result := GetDigitValue(Value) <> -1;
end;

function TJclNumericFormat.FloatToHTML(const Value: Float): string;
var
  Mantissa: string;
  Exponent: Integer;
begin
  GetMantissaExp(Value, Mantissa, Exponent);
  Result := Format('%s %s %d<sup>%d</sup>', [Mantissa, Multiplier, Base, Exponent]);
end;

procedure TJclNumericFormat.GetMantissaExp(const Value: Float;
  out Mantissa: string; out Exponent: Integer);
const
  {$IFDEF FPC}
  InfMantissa: array [Boolean] of string[4] = ('inf', '-inf');
  {$ElSE ~FPC}
  InfMantissa: array [Boolean] of string = ('inf', '-inf');
  {$ENDIF ~FPC}
var
  BlockDigits: TDigitCount;
  IntDigits, FracDigits: Integer;
  FirstDigitPos, Prec: Integer;
  I, J, N: Integer;
  K: Int64;
  X: Extended;
  HighDigit: Char;

  function GetDigit(X: Extended): Char;
  var
    N: Integer;
  begin
    N := Trunc(X);
    if N > 9 then
      Result := Chr(Ord('A') + N - 10)
    else
      Result := Chr(Ord('0') + N);
  end;

begin
  X := Abs(Value);

  if X > MaxFloatingPoint then
  begin
    Mantissa := InfMantissa[Value < 0];
    Exponent := 1;
    Exit;
  end
  else
  if X < MinFloatingPoint then
  begin
    Mantissa := Format('%.*f', [Precision, 0.0]);
    Exponent := 1;
    Exit;
  end;

  IntDigits := 1;
  Prec := Precision;

  Exponent := Trunc(LogBaseN(Base, X));
  if FExpDivision > 1 then
  begin
    N := Exponent mod FExpDivision;
    Dec(Exponent, N);
    Inc(IntDigits, N);
  end;
  X := X / Power(Base, Exponent);

  if X < 1.0 then
  begin
    Dec(Exponent, FExpDivision);
    X := X * PowerInt(Base, FExpDivision);
    Inc(IntDigits, FExpDivision - 1);
  end;

{ TODO : Here's a problem if X > High(Int64).
It *seems* to surface only if ExponentDivision > 12, but it
has not been investigated if ExponentDivision <= 12 is safe. }
  K := Trunc(X);
  if Value < 0 then
    K := -K;

  Mantissa := IntToStr(K, FirstDigitPos);

  FracDigits := Prec - IntDigits;
  if FracDigits > NumberOfFractionalDigits then
    FracDigits := NumberOfFractionalDigits;

  if FracDigits > 0 then
  begin
    J := Length(Mantissa) + 1;
    // allocate sufficient space for point + digits + digit block separators
    SetLength(Mantissa, FracDigits * 2 + J);
    Mantissa[J] := FractionalPartSeparator;
    I := J + 1;
    BlockDigits := 0;
    while FracDigits > 0 do
    begin
      if (BlockDigits > 0) and (BlockDigits = DigitBlockSize) then
      begin
        Mantissa[I] := DigitBlockSeparator;
        Inc(I);
        BlockDigits := 0;
      end;
      X := Frac(X) * Base;
      Mantissa[I] := GetDigit(X);
      Inc(I);
      Inc(BlockDigits);
      Dec(FracDigits);
    end;
    Mantissa[I] := #0;
    StrResetLength(Mantissa);
  end;

  if Frac(X) >= 0.5 then
  // round up
  begin
    HighDigit := Digit(Base - 1);
    for I := Length(Mantissa) downto 1 do
    begin
      if Mantissa[I] = HighDigit then
        if (I = FirstDigitPos) then
        begin
          Mantissa[I] := '1';
          Inc(Exponent);
          Break;
        end
        else
          Mantissa[I] := '0'
      else
      if (Mantissa[I] = DigitBlockSeparator) or (Mantissa[I] = FractionalPartSeparator) then
        Continue
      else
      begin
        if Mantissa[I] = '9' then
          Mantissa[I] := 'A'
        else
          Mantissa[I] := Succ(Mantissa[I]);
        Break;
      end;
    end;
  end;
end;

function TJclNumericFormat.FloatToStr(const Value: Float): string;
var
  Mantissa: string;
  Exponent: Integer;
begin
  GetMantissaExp(Value, Mantissa, Exponent);
  Result := Format('%s %s %d^%d', [Mantissa, Multiplier, Base, Exponent]);
end;

function TJclNumericFormat.IntToStr(const Value: Int64): string;
var
  FirstDigitPos: Integer;
begin
  Result := IntToStr(Value, FirstDigitPos);
end;

function TJclNumericFormat.IntToStr(const Value: Int64; out FirstDigitPos: Integer): string;
const
  MaxResultLen = 64 + 63 + 1; // max. digits + max. group separators + sign
var
  Remainder: Int64;
  I, N: Integer;
  Chars, Digits: Cardinal;
  LoopFinished, HasSign, SpacePadding: Boolean;
begin
  SpacePadding := PaddingChar = ' ';
  HasSign := ShowSign(Value);
  Chars := MaxResultLen;
  if Width > Chars then
    Chars := Width;
  Result := StrRepeatChar(' ', Chars);

  Remainder := Abs(Value);
  Digits := 0;

  Chars := 0;
  if HasSign then
    Chars := 1;

  I := MaxResultLen;

  while True do
  begin
    N := Remainder mod Base;
    Remainder := Remainder div Base;
    if N > 9 then
      Result[I] := Chr(Ord('A') + N - 10)
    else
      Result[I] := Chr(Ord('0') + N);
    Dec(I);
    Inc(Digits);
    Inc(Chars);
    if (Remainder = 0) and (SpacePadding or (Chars >= Width)) then
      Break;
    if (Digits = DigitBlockSize) then
    begin
      Inc(Chars);
      LoopFinished := (Remainder = 0) and (Chars = Width);
      if LoopFinished then
        Result[I] := ' '
      else
        Result[I] := DigitBlockSeparator;
      Dec(I);
      if LoopFinished then
        Break;
      Digits := 0;
    end;
  end;

  FirstDigitPos := I + 1;

  if HasSign then
    Result[I] := SignChar(Value)
  else
    Inc(I);
  N := MaxResultLen - Width + 1;
  if N < I then
    I := N;
  Result := Copy(Result, I, MaxResultLen);
  Dec(FirstDigitPos, I - 1);
end;

procedure TJclNumericFormat.SetBase(const Value: TNumericSystemBase);
begin
  FBase := Value;
  SetPrecision(FWantedPrecision);
end;

procedure TJclNumericFormat.SetExpDivision(const Value: Integer);
begin
  if Value <= 1 then
    FExpDivision := 1
  else
  // see TODO in GetMantissaExp
  if Value > 12 then
    FExpDivision := 12
  else
    FExpDivision := Value;
end;

procedure TJclNumericFormat.SetPrecision(const Value: TDigitCount);
begin
  FWantedPrecision := Value;
  // Do not display more digits than Float precision justifies
  if Base = 2 then
    FPrecision := BinaryPrecision
  else
    FPrecision := Trunc(BinaryPrecision / LogBase2(Base));
  if Value < FPrecision then
    FPrecision := Value;
end;

function TJclNumericFormat.Sign(Value: Char): Integer;
begin
  Result := 0;
  if Value = FSignChars[False] then
    Result := -1;
  if Value = FSignChars[True] then
    Result := +1;
end;

function TJclNumericFormat.StrToInt(const Value: string): Int64;
var
  I, N: Integer;
  C: Char;
begin
  Result := 0;
  I := 1;
  if (Length(Value) >= I)
    and ((Value[I] = '+') or (Value[I] = '-')) then
    Inc(I);
  for I := I to Length(Value) do
  begin
    C := Value[I];
    if C = DigitBlockSeparator then
      Continue
    else
    begin
      N := CharHex(C);
      if (N = $FF) or (N >= Base) then
        InvalidDigit(C);
      Result := Result * Base + N;
    end;
  end;
  if Value[1] = '-' then
    Result := -Result;
end;

function TJclNumericFormat.ShowSign(const Value: Float): Boolean;
begin
  Result := FShowPositiveSign or (Value < 0);
end;

function TJclNumericFormat.ShowSign(const Value: Int64): Boolean;
begin
  Result := FShowPositiveSign or (Value < 0);
end;

function TJclNumericFormat.SignChar(const Value: Float): Char;
begin
  Result := FSignChars[Value >= 0];
end;

function TJclNumericFormat.SignChar(const Value: Int64): Char;
begin
  Result := FSignChars[Value >= 0];
end;

function TJclNumericFormat.GetNegativeSign: Char;
begin
  Result := FSignChars[False];
end;

function TJclNumericFormat.GetPositiveSign: Char;
begin
  Result := FSignChars[True];
end;

procedure TJclNumericFormat.SetNegativeSign(const Value: Char);
begin
  FSignChars[False] := Value;
end;

procedure TJclNumericFormat.SetPositiveSign(const Value: Char);
begin
  FSignChars[True] := Value;
end;

//=== Child processes ========================================================

const
  BufferSize = 255;
type
  TBuffer = array [0..BufferSize] of AnsiChar;

  TPipeInfo = record
    PipeRead, PipeWrite: THandle;
    Buffer: TBuffer;
    Line: string;
    TextHandler: TTextHandler;
    RawOutput: Boolean;
    Event: TJclEvent;
  end;
  PPipeInfo = ^TPipeInfo;

// MuteCRTerminatedLines was "outsourced" from Win32ExecAndRedirectOutput

function InternalExecuteMuteCRTerminatedLines(const RawOutput: string): string;
const
  Delta = 1024;
var
  BufPos, OutPos, LfPos, EndPos: Integer;
  C: Char;
begin
  SetLength(Result, Length(RawOutput));
  OutPos := 1;
  LfPos := OutPos;
  EndPos := OutPos;
  for BufPos := 1 to Length(RawOutput) do
  begin
    if OutPos >= Length(Result)-2 then
      SetLength(Result, Length(Result) + Delta);
    C := RawOutput[BufPos];
    case C of
      NativeCarriageReturn:
        OutPos := LfPos;
      NativeLineFeed:
        begin
          OutPos := EndPos;
          Result[OutPos] := NativeCarriageReturn;
          Inc(OutPos);
          Result[OutPos] := C;
          Inc(OutPos);
          EndPos := OutPos;
          LfPos := OutPos;
        end;
    else
      Result[OutPos] := C;
      Inc(OutPos);
      EndPos := OutPos;
    end;
  end;
  SetLength(Result, OutPos - 1);
end;

procedure InternalExecuteProcessLine(const PipeInfo: TPipeInfo; LineEnd: Integer);
begin
  if PipeInfo.RawOutput or (PipeInfo.Line[LineEnd] <> NativeCarriageReturn) then
  begin
    while (LineEnd > 0) and CharIsReturn(PipeInfo.Line[LineEnd]) do
      Dec(LineEnd);
    PipeInfo.TextHandler(Copy(PipeInfo.Line, 1, LineEnd));
  end;
end;

procedure InternalExecuteProcessBuffer(var PipeInfo: TPipeInfo; PipeBytesRead: Cardinal);
var
  CR, LF: Integer;
begin
  PipeInfo.Buffer[PipeBytesRead] := #0;
  PipeInfo.Line := PipeInfo.Line + string(PipeInfo.Buffer);
  if Assigned(PipeInfo.TextHandler) then
  repeat
    CR := Pos(NativeCarriageReturn, PipeInfo.Line);
    if CR = Length(PipeInfo.Line) then
      CR := 0;        // line feed at CR + 1 might be missing
    LF := Pos(NativeLineFeed, PipeInfo.Line);
    if (CR > 0) and ((LF > CR + 1) or (LF = 0)) then
      LF := CR;       // accept CR as line end
    if LF > 0 then
    begin
      InternalExecuteProcessLine(PipeInfo, LF);
      Delete(PipeInfo.Line, 1, LF);
    end;
  until LF = 0;
end;

procedure InternalExecuteReadPipe(var PipeInfo: TPipeInfo; var Overlapped: TOverlapped);
var
  NullDWORD: ^DWORD; // XE4 broke PDWORD
  Res: DWORD;
begin
  NullDWORD := nil;
  if not ReadFile(PipeInfo.PipeRead, PipeInfo.Buffer[0], BufferSize, NullDWORD^, @Overlapped) then
  begin
    Res := GetLastError;
    case Res of
      ERROR_BROKEN_PIPE:
        begin
          CloseHandle(PipeInfo.PipeRead);
          PipeInfo.PipeRead := 0;
        end;
      ERROR_IO_PENDING:
        ;
    else
      {$IFDEF DELPHI11_UP}
      RaiseLastOSError(Res);
      {$ELSE}
      RaiseLastOSError;
      {$ENDIF DELPHI11_UP}
    end;
  end;
end;

procedure InternalExecuteHandlePipeEvent(var PipeInfo: TPipeInfo; var Overlapped: TOverlapped);
var
  PipeBytesRead: DWORD;
begin
  if GetOverlappedResult(PipeInfo.PipeRead, Overlapped, PipeBytesRead, False) then
  begin
    InternalExecuteProcessBuffer(PipeInfo, PipeBytesRead);
    // automatically launch the next read
    InternalExecuteReadPipe(PipeInfo, Overlapped);
  end
  else
  if GetLastError = ERROR_BROKEN_PIPE then
  begin
    CloseHandle(PipeInfo.PipeRead);
    PipeInfo.PipeRead := 0;
  end
  else
    RaiseLastOSError;
end;

procedure InternalExecuteFlushPipe(var PipeInfo: TPipeInfo; var Overlapped: TOverlapped);
var
  PipeBytesRead: DWORD;
begin
  CancelIo(PipeInfo.PipeRead);
  GetOverlappedResult(PipeInfo.PipeRead, Overlapped, PipeBytesRead, True);
  if PipeBytesRead > 0 then
    InternalExecuteProcessBuffer(PipeInfo, PipeBytesRead);
  while PeekNamedPipe(PipeInfo.PipeRead, nil, 0, nil, @PipeBytesRead, nil) and (PipeBytesRead > 0) do
  begin
    if PipeBytesRead > BufferSize then
      PipeBytesRead := BufferSize;
    if not ReadFile(PipeInfo.PipeRead, PipeInfo.Buffer[0], PipeBytesRead, PipeBytesRead, nil) then
      RaiseLastOSError;
    InternalExecuteProcessBuffer(PipeInfo, PipeBytesRead);
  end;
end;

var
  AsyncPipeCounter: Integer;

// CreateAsyncPipe creates a pipe that uses overlapped reading.
function CreateAsyncPipe(var hReadPipe, hWritePipe: THandle;
  lpPipeAttributes: PSecurityAttributes; nSize: DWORD): BOOL;
var
  PipeName: string;
  Error: DWORD;
  PipeReadHandle, PipeWriteHandle: THandle;
begin
  Result := False;

  if (@hReadPipe = nil) or (@hWritePipe = nil) then
  begin
    SetLastError(ERROR_INVALID_PARAMETER);
    Exit;
  end;

  if nSize = 0 then
    nSize := 4096;

  InterlockedIncrement(AsyncPipeCounter);
  // In some (not so) rare instances there is a race condition
  // where the counter is the same for two threads at the same 
  // time. This makes the CreateNamedPipe call below fail 
  // because of the limit set to 1 in the call.
  // So, to be sure this call succeeds, we put both the process
  // and thread id in the name of the pipe.
  // This was found to happen while simply starting 7 instances
  // of the same exe file in parallel.
  PipeName := Format('\\.\Pipe\AsyncAnonPipe.%.8x.%.8x.%.8x', [GetCurrentProcessId, GetCurrentThreadId, AsyncPipeCounter]);

  PipeReadHandle := CreateNamedPipe(PChar(PipeName), PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED,
      PIPE_TYPE_BYTE or PIPE_WAIT, 1, nSize, nSize, 120 * 1000, lpPipeAttributes);
  if PipeReadHandle = INVALID_HANDLE_VALUE then
    Exit;

  PipeWriteHandle := CreateFile(PChar(PipeName), GENERIC_WRITE, 0, lpPipeAttributes, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL {or FILE_FLAG_OVERLAPPED}, 0);
  if PipeWriteHandle = INVALID_HANDLE_VALUE then
  begin
    Error := GetLastError;
    CloseHandle(PipeReadHandle);
    SetLastError(Error);
    Exit;
  end;

  hReadPipe := PipeReadHandle;
  hWritePipe := PipeWriteHandle;

  Result := True;
end;

const
  BELOW_NORMAL_PRIORITY_CLASS = $00004000;
  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;

  ProcessPriorities: array [TJclProcessPriority] of DWORD =
    (IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS,
     BELOW_NORMAL_PRIORITY_CLASS, ABOVE_NORMAL_PRIORITY_CLASS);

function InternalExecute(CommandLine: string; AbortPtr: PBoolean; AbortEvent: TJclEvent;
  var Output: string; OutputLineCallback: TTextHandler; RawOutput: Boolean;
  MergeError: Boolean; var Error: string; ErrorLineCallback: TTextHandler; RawError: Boolean;
  ProcessPriority: TJclProcessPriority): Cardinal;
var
  OutPipeInfo, ErrorPipeInfo: TPipeInfo;
  Index: Cardinal;
{$IFDEF MSWINDOWS}
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  OutOverlapped, ErrorOverlapped: TOverlapped;
  ProcessEvent: TJclDispatcherObject;
  WaitEvents: array of TJclDispatcherObject;
  InternalAbort: Boolean;
  LastError: DWORD;
begin
  // hack to pass a null reference to the parameter lpNumberOfBytesRead of ReadFile
  Result := $FFFFFFFF;
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor := nil;
  SecurityAttr.bInheritHandle := True;

  ResetMemory(OutPipeInfo, SizeOf(OutPipeInfo));
  OutPipeInfo.TextHandler := OutputLineCallback;
  OutPipeInfo.RawOutput := RawOutput;
  if not CreateAsyncPipe(OutPipeInfo.PipeRead, OutPipeInfo.PipeWrite, @SecurityAttr, 0) then
  begin
    Result := GetLastError;
    Exit;
  end;
  OutPipeInfo.Event := TJclEvent.Create(@SecurityAttr, False {automatic reset}, False {not flagged}, '' {anonymous});
  ResetMemory(ErrorPipeInfo, SizeOf(ErrorPipeInfo));
  if not MergeError then
  begin
    ErrorPipeInfo.TextHandler := ErrorLineCallback;
    ErrorPipeInfo.RawOutput := RawError;
    if not CreateAsyncPipe(ErrorPipeInfo.PipeRead, ErrorPipeInfo.PipeWrite, @SecurityAttr, 0) then
    begin
      Result := GetLastError;
      CloseHandle(OutPipeInfo.PipeWrite);
      CloseHandle(OutPipeInfo.PipeRead);
      OutPipeInfo.Event.Free;
      Exit;
    end;
    ErrorPipeInfo.Event := TJclEvent.Create(@SecurityAttr, False {automatic reset}, False {not flagged}, '' {anonymous});
  end;

  ResetMemory(StartupInfo, SizeOf(TStartupInfo));
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  StartupInfo.hStdOutput := OutPipeInfo.PipeWrite;
  if MergeError then
    StartupInfo.hStdError := OutPipeInfo.PipeWrite
  else
    StartupInfo.hStdError := ErrorPipeInfo.PipeWrite;
  UniqueString(CommandLine); // CommandLine must be in a writable memory block
  ProcessInfo.dwProcessId := 0;
  ProcessEvent := nil;
  try
    if CreateProcess(nil, PChar(CommandLine), nil, nil, True, ProcessPriorities[ProcessPriority],
      nil, nil, StartupInfo, ProcessInfo) then
    begin
      try
        // init out and error events
        CloseHandle(OutPipeInfo.PipeWrite);
        OutPipeInfo.PipeWrite := 0;
        if not MergeError then
        begin
          CloseHandle(ErrorPipeInfo.PipeWrite);
          ErrorPipeInfo.PipeWrite := 0;
        end;
        InternalAbort := False;
        if AbortPtr <> nil then
          AbortPtr^ := {$IFDEF FPC}Byte({$ENDIF}False{$IFDEF FPC}){$ENDIF}
        else
          AbortPtr := @InternalAbort;
        // init the array of events to wait for
        ProcessEvent := TJclDispatcherObject.Attach(ProcessInfo.hProcess);
        SetLength(WaitEvents, 2);
        // add the process first
        WaitEvents[0] := ProcessEvent;
        // add the output event
        WaitEvents[1] := OutPipeInfo.Event;
        // add the error event
        if not MergeError then
        begin
          SetLength(WaitEvents, 3);
          WaitEvents[2] := ErrorPipeInfo.Event;
        end;
        // add the abort event if any
        if AbortEvent <> nil then
        begin
          AbortEvent.ResetEvent;
          Index := Length(WaitEvents);
          SetLength(WaitEvents, Index + 1);
          WaitEvents[Index] := AbortEvent;
        end;
        // init the asynchronous reads
        ResetMemory(OutOverlapped, SizeOf(OutOverlapped));
        OutOverlapped.hEvent := OutPipeInfo.Event.Handle;
        InternalExecuteReadPipe(OutPipeInfo, OutOverlapped);
        if not MergeError then
        begin
          ResetMemory(ErrorOverlapped, SizeOf(ErrorOverlapped));
          ErrorOverlapped.hEvent := ErrorPipeInfo.Event.Handle;
          InternalExecuteReadPipe(ErrorPipeInfo, ErrorOverlapped);
        end;
        // event based loop
        while not {$IFDEF FPC}Boolean({$ENDIF}AbortPtr^{$IFDEF FPC}){$ENDIF} do
        begin
          Index := WaitAlertableForMultipleObjects(WaitEvents, False, INFINITE);
          if Index = WAIT_OBJECT_0 then
            // the subprocess has ended
            Break
          else
          if Index = (WAIT_OBJECT_0 + 1) then
          begin
            // event on output
            InternalExecuteHandlePipeEvent(OutPipeInfo, OutOverlapped);
          end
          else
          if (Index = (WAIT_OBJECT_0 + 2)) and not MergeError then
          begin
            // event on error
            InternalExecuteHandlePipeEvent(ErrorPipeInfo, ErrorOverlapped);
          end
          else
          if ((Index = (WAIT_OBJECT_0 + 2)) and MergeError) or
             ((Index = (WAIT_OBJECT_0 + 3)) and not MergeError) then
            // event on abort
            AbortPtr^ := {$IFDEF FPC}Byte({$ENDIF}True{$IFDEF FPC}){$ENDIF}
          else
            {$IFDEF DELPHI11_UP}
            RaiseLastOSError(Index);
            {$ELSE}
            RaiseLastOSError;
            {$ENDIF DELPHI11_UP}
        end;
        if {$IFDEF FPC}Boolean({$ENDIF}AbortPtr^{$IFDEF FPC}){$ENDIF} then
          TerminateProcess(ProcessEvent.Handle, Cardinal(ABORT_EXIT_CODE));
        if (ProcessEvent.WaitForever = wrSignaled) and not GetExitCodeProcess(ProcessEvent.Handle, Result) then
          Result := $FFFFFFFF;
        CloseHandle(ProcessInfo.hThread);
        ProcessInfo.hThread := 0;
        if OutPipeInfo.PipeRead <> 0 then
          // read data remaining in output pipe
          InternalExecuteFlushPipe(OutPipeinfo, OutOverlapped);
        if not MergeError and (ErrorPipeInfo.PipeRead <> 0) then
          // read data remaining in error pipe
          InternalExecuteFlushPipe(ErrorPipeInfo, ErrorOverlapped);
      except
        // always terminate process in case of an exception.
        // This is especially useful when an exception occured in one of
        // the texthandler but only do it if the process actually started,
        // this prevents eating up the last error value by calling those
        // three functions with an invalid handle
        // Note that we don't do it in the finally block because these
        // calls would also then eat up the last error value which we tried
        // to avoid in the first place
        if ProcessInfo.hProcess <> 0 then
        begin
          TerminateProcess(ProcessInfo.hProcess, Cardinal(ABORT_EXIT_CODE));
          WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
          GetExitCodeProcess(ProcessInfo.hProcess, Result);
        end;

        raise;
      end;
    end;
  finally
    LastError := GetLastError;
    try
      if OutPipeInfo.PipeRead <> 0 then
        CloseHandle(OutPipeInfo.PipeRead);
      if OutPipeInfo.PipeWrite <> 0 then
        CloseHandle(OutPipeInfo.PipeWrite);
      if ErrorPipeInfo.PipeRead <> 0 then
        CloseHandle(ErrorPipeInfo.PipeRead);
      if ErrorPipeInfo.PipeWrite <> 0 then
        CloseHandle(ErrorPipeInfo.PipeWrite);
      if ProcessInfo.hThread <> 0 then
        CloseHandle(ProcessInfo.hThread);

      if Assigned(ProcessEvent) then
        ProcessEvent.Free // this calls CloseHandle(ProcessInfo.hProcess)
      else if ProcessInfo.hProcess <> 0 then
        CloseHandle(ProcessInfo.hProcess);
      OutPipeInfo.Event.Free;
      ErrorPipeInfo.Event.Free;
    finally
      SetLastError(LastError);
    end;
  end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
var
  PipeBytesRead: Cardinal;
  Pipe: PIOFile;
  Cmd: string;
begin
  Cmd := Format('%s 2>&1', [CommandLine]);
  Pipe := nil;
  try
    Pipe := Libc.popen(PChar(Cmd), 'r');
    { TODO : handle Abort }
    repeat
      PipeBytesRead := fread_unlocked(@OutBuffer, 1, BufferSize, Pipe);
      if PipeBytesRead > 0 then
        ProcessBuffer(OutBuffer, OutLine, PipeBytesRead);
    until PipeBytesRead = 0;
    Result := pclose(Pipe);
    Pipe := nil;
    wait(nil);
  finally
    if Pipe <> nil then
      pclose(Pipe);
    wait(nil);
  end;
{$ENDIF UNIX}
  if OutPipeInfo.Line <> '' then
    if Assigned(OutPipeInfo.TextHandler) then
      // output wasn't terminated by a line feed...
      // (shouldn't happen, but you never know)
      InternalExecuteProcessLine(OutPipeInfo, Length(OutPipeInfo.Line))
    else
      if RawOutput then
        Output := Output + OutPipeInfo.Line
      else
        Output := Output + InternalExecuteMuteCRTerminatedLines(OutPipeInfo.Line);
  if ErrorPipeInfo.Line <> '' then
    if Assigned(ErrorPipeInfo.TextHandler) then
      // error wasn't terminated by a line feed...
      // (shouldn't happen, but you never know)
      InternalExecuteProcessLine(ErrorPipeInfo, Length(ErrorPipeInfo.Line))
    else
      if RawError then
        Error := Error + ErrorPipeInfo.Line
      else
        Error := Error + InternalExecuteMuteCRTerminatedLines(ErrorPipeInfo.Line);
end;

{ TODO -cHelp :
RawOutput: Do not process isolated carriage returns (#13).
That is, for RawOutput = False, lines not terminated by a line feed (#10) are deleted from Output. }

function Execute(const CommandLine: string; var Output: string; RawOutput: Boolean;
  AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Error: string;
begin
  Error := '';
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, nil, RawOutput, True, Error, nil, False, ProcessPriority);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; var Output: string; RawOutput: Boolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Error: string;
begin
  Error := '';
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, nil, RawOutput, True, Error, nil, False, ProcessPriority);
end;

{ TODO -cHelp :
Author: Robert Rossmair
OutputLineCallback called once per line of output. }

function Execute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean;
  AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, OutputLineCallback, RawOutput, True, Error, nil, False, ProcessPriority);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; OutputLineCallback: TTextHandler; RawOutput: Boolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, OutputLineCallback, RawOutput, True, Error, nil, False, ProcessPriority);
end;

{ TODO -cHelp :
RawOutput: Do not process isolated carriage returns (#13).
That is, for RawOutput = False, lines not terminated by a line feed (#10) are deleted from Output. }

function Execute(const CommandLine: string; var Output, Error: string; RawOutput, RawError: Boolean;
  AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority): Cardinal;
begin
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, nil, RawOutput, False, Error, nil, RawError, ProcessPriority);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; var Output, Error: string;
  RawOutput, RawError: Boolean; ProcessPriority: TJclProcessPriority): Cardinal;
begin
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, nil, RawOutput, False, Error, nil, RawError, ProcessPriority);
end;

{ TODO -cHelp :
Author: Robert Rossmair
OutputLineCallback called once per line of output. }

function Execute(const CommandLine: string; OutputLineCallback, ErrorLineCallback: TTextHandler;
  RawOutput, RawError: Boolean; AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, OutputLineCallback, RawOutput, False, Error, ErrorLineCallback, RawError, ProcessPriority);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; OutputLineCallback, ErrorLineCallback: TTextHandler;
  RawOutput, RawError: Boolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, OutputLineCallback, RawOutput, False, Error, ErrorLineCallback, RawError, ProcessPriority);
end;

//=== { TJclCommandLineTool } ================================================

constructor TJclCommandLineTool.Create(const AExeName: string);
begin
  inherited Create;
  FOptions := TStringList.Create;
  FExeName := AExeName;
end;

destructor TJclCommandLineTool.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TJclCommandLineTool.AddPathOption(const Option, Path: string);
var
  S: string;
begin
  S := PathRemoveSeparator(Path);
  {$IFDEF MSWINDOWS}
  S := LowerCase(S); // file names are case insensitive
  {$ENDIF MSWINDOWS}
  S := Format('-%s%s', [Option, S]);
  // avoid duplicate entries (note that search is case sensitive)
  if GetOptions.IndexOf(S) = -1 then
    GetOptions.Add(S);
end;

function TJclCommandLineTool.Execute(const CommandLine: string): Boolean;
begin
  if Assigned(FOutputCallback) then
    Result := JclSysUtils.Execute(Format('"%s" %s', [ExeName, CommandLine]), FOutputCallback) = 0
  else
    Result := JclSysUtils.Execute(Format('"%s" %s', [ExeName, CommandLine]), FOutput) = 0;
end;

function TJclCommandLineTool.GetExeName: string;
begin
  Result := FExeName;
end;

function TJclCommandLineTool.GetOptions: TStrings;
begin
  Result := FOptions;
end;

function TJclCommandLineTool.GetOutput: string;
begin
  Result := FOutput;
end;

function TJclCommandLineTool.GetOutputCallback: TTextHandler;
begin
  Result := FOutputCallback;
end;

procedure TJclCommandLineTool.SetOutputCallback(const CallbackMethod: TTextHandler);
begin
  FOutputCallback := CallbackMethod;
end;

//=== Console Utilities ======================================================

function ReadKey: Char;
{$IFDEF MSWINDOWS}
{ TODO -cHelp : Contributor: Robert Rossmair }
var
  Console: TJclConsole;
  InputMode: TJclConsoleInputModes;
begin
  Console := TJclConsole.Default;
  InputMode := Console.Input.Mode;
  Console.Input.Mode := [imProcessed];
  Console.Input.Clear;
  Result := Char(Console.Input.GetEvent.Event.KeyEvent.AsciiChar);
  Console.Input.Mode := InputMode;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{ TODO -cHelp : Donator: Wayne Sherman }
var
  ReadFileDescriptor: TFDSet;
  TimeVal: TTimeVal;
  SaveTerminalSettings: TTermIos;
  RawTerminalSettings: TTermIos;
begin
  Result := #0;

  //Save Original Terminal Settings
  tcgetattr(stdin, SaveTerminalSettings);
  tcgetattr(stdin, RawTerminalSettings);

  //Put Terminal in RAW mode
  cfmakeraw(RawTerminalSettings);
  tcsetattr(stdin, TCSANOW, RawTerminalSettings);
  try
    //Setup file I/O descriptor for STDIN
    FD_ZERO(ReadFileDescriptor);
    FD_SET(stdin, ReadFileDescriptor);
    TimeVal.tv_sec := High(LongInt); //wait forever
    TimeVal.tv_usec := 0;

    //clear keyboard buffer first
    TCFlush(stdin, TCIFLUSH);

    //wait for a key to be pressed
    if select(1, @ReadFileDescriptor, nil, nil, @TimeVal) > 0 then
    begin
      //Now read the character
      Result := Char(getchar);
    end
    else
      raise EJclError.CreateRes(@RsReadKeyError);
  finally
    //Restore Original Terminal Settings
    tcsetattr(stdin, TCSANOW, SaveTerminalSettings);
  end;
end;
{$ENDIF UNIX}

//=== Loading of modules (DLLs) ==============================================

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
{$IFDEF MSWINDOWS}
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := SafeLoadLibrary(FileName);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := dlopen(PChar(FileName), RTLD_NOW);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF UNIX}

function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
{$IFDEF MSWINDOWS}
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := LoadLibraryEx(PChar(FileName), 0, Flags); // SafeLoadLibrary?
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := dlopen(PChar(FileName), Flags);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF UNIX}

procedure UnloadModule(var Module: TModuleHandle);
{$IFDEF MSWINDOWS}
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(Module);
  Module := INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
    dlclose(Pointer(Module));
  Module := INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF UNIX}

function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
{$IFDEF MSWINDOWS}
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := dlsym(Module, PChar(SymbolName));
end;
{$ENDIF UNIX}

function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
{$IFDEF MSWINDOWS}
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
  Accu := Accu and (Result <> nil);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := dlsym(Module, PChar(SymbolName));
  Accu := Accu and (Result <> nil);
end;
{$ENDIF UNIX}

function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Sym^, Buffer, Size);
end;

function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Buffer, Sym^, Size);
end;

//=== Conversion Utilities ===================================================

const
  DefaultTrueBoolStr  = 'True';  // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

  DefaultYesBoolStr   = 'Yes';   // DO NOT LOCALIZE
  DefaultNoBoolStr    = 'No';    // DO NOT LOCALIZE

function StrToBoolean(const S: string): Boolean;
var
  LowerCasedText: string;
begin
  { TODO : Possibility to add localized strings, like in Delphi 7 }
  { TODO : Lower case constants }
  LowerCasedText := LowerCase(S);
  Result := ((S = '1') or
    (LowerCasedText = LowerCase(DefaultTrueBoolStr)) or (LowerCasedText = LowerCase(DefaultYesBoolStr))) or
    (LowerCasedText = LowerCase(DefaultTrueBoolStr[1])) or (LowerCasedText = LowerCase(DefaultYesBoolStr[1]));
  if not Result then
  begin
    Result := not ((S = '0') or
      (LowerCasedText = LowerCase(DefaultFalseBoolStr)) or (LowerCasedText = LowerCase(DefaultNoBoolStr)) or
      (LowerCasedText = LowerCase(DefaultFalseBoolStr[1])) or (LowerCasedText = LowerCase(DefaultNoBoolStr[1])));
    if Result then
      raise EJclConversionError.CreateResFmt(@RsStringToBoolean, [S]);
  end;
end;

function BooleanToStr(B: Boolean): string;
begin
  if B then
    Result := DefaultTrueBoolStr
  else
    Result := DefaultFalseBoolStr;
end;

function IntToBool(I: Integer): Boolean;
begin
  Result := I <> 0;
end;

function BoolToInt(B: Boolean): Integer;
begin
  Result := Ord(B);
end;

function TryStrToUInt(const Value: string; out Res: Cardinal): Boolean;
var i6: Int64;
begin
  Result := false;
  if not TryStrToInt64(Value, i6) then exit;
  if ( i6 < Low(Res)) or ( i6 > High(Res)) then exit;

  Result := true;
  Res := i6;
end;

function StrToUIntDef(const Value: string; const Default: Cardinal): Cardinal;
begin
  if not TryStrToUInt(Value, Result)
     then Result := Default;
end;

function StrToUInt(const Value: string): Cardinal;
begin
  if not TryStrToUInt(Value, Result)
     then raise EConvertError.Create('"'+Value+'" is not within range of Cardinal data type');
end;

//=== RTL package information ================================================

function SystemTObjectInstance: TJclAddr;
begin
  Result := ModuleFromAddr(Pointer(System.TObject));
end;

function IsCompiledWithPackages: Boolean;
begin
  Result := SystemTObjectInstance <> HInstance;
end;

//=== GUID ===================================================================

function JclGUIDToString(const GUID: TGUID): string;
begin
  Result := Format('{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',
    [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2],
     GUID.D4[3], GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
end;

function JclStringToGUID(const S: string): TGUID;
begin
  if (Length(S) <> 38) or (S[1] <> '{') or (S[10] <> '-') or (S[15] <> '-') or
    (S[20] <> '-') or (S[25] <> '-') or (S[38] <> '}') then
    raise EJclConversionError.CreateResFmt(@RsInvalidGUIDString, [S]);

  Result.D1 := StrToInt('$' + Copy(S, 2, 8));
  Result.D2 := StrToInt('$' + Copy(S, 11, 4));
  Result.D3 := StrToInt('$' + Copy(S, 16, 4));
  Result.D4[0] := StrToInt('$' + Copy(S, 21, 2));
  Result.D4[1] := StrToInt('$' + Copy(S, 23, 2));
  Result.D4[2] := StrToInt('$' + Copy(S, 26, 2));
  Result.D4[3] := StrToInt('$' + Copy(S, 28, 2));
  Result.D4[4] := StrToInt('$' + Copy(S, 30, 2));
  Result.D4[5] := StrToInt('$' + Copy(S, 32, 2));
  Result.D4[6] := StrToInt('$' + Copy(S, 34, 2));
  Result.D4[7] := StrToInt('$' + Copy(S, 36, 2));
end;

function GUIDEquals(const GUID1, GUID2: TGUID): Boolean;
begin
  Result := (GUID1.D1 = GUID2.D1) and (GUID1.D2 = GUID2.D2) and (GUID1.D3 = GUID2.D3) and
    (GUID1.D4[0] = GUID2.D4[0]) and (GUID1.D4[1] = GUID2.D4[1]) and
    (GUID1.D4[2] = GUID2.D4[2]) and (GUID1.D4[3] = GUID2.D4[3]) and
    (GUID1.D4[4] = GUID2.D4[4]) and (GUID1.D4[5] = GUID2.D4[5]) and
    (GUID1.D4[6] = GUID2.D4[6]) and (GUID1.D4[7] = GUID2.D4[7]);
end;

// add items at the end
procedure ListAddItems(var List: string; const Separator, Items: string);
var
  StrList, NewItems: TStringList;
  Index: Integer;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList, False);

    NewItems := TStringList.Create;
    try
      StrToStrings(Items, Separator, NewItems);

      for Index := 0 to NewItems.Count - 1 do
        StrList.Add(NewItems.Strings[Index]);

      List := StringsToStr(StrList, Separator);
    finally
      NewItems.Free;
    end;
  finally
    StrList.Free;
  end;
end;

// add items at the end if they are not present
procedure ListIncludeItems(var List: string; const Separator, Items: string);
var
  StrList, NewItems: TStringList;
  Index: Integer;
  Item: string;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList, False);

    NewItems := TStringList.Create;
    try
      StrToStrings(Items, Separator, NewItems);

      for Index := 0 to NewItems.Count - 1 do
      begin
        Item := NewItems.Strings[Index];
        if StrList.IndexOf(Item) = -1 then
          StrList.Add(Item);
      end;

      List := StringsToStr(StrList, Separator);
    finally
      NewItems.Free;
    end;
  finally
    StrList.Free;
  end;
end;

// delete multiple items
procedure ListRemoveItems(var List: string; const Separator, Items: string);
var
  StrList, RemItems: TStringList;
  Index, Position: Integer;
  Item: string;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList, False);

    RemItems := TStringList.Create;
    try
      StrToStrings(Items, Separator, RemItems, False);

      for Index := 0 to RemItems.Count - 1 do
      begin
        Item := RemItems.Strings[Index];
        repeat
          Position := StrList.IndexOf(Item);
          if Position >= 0 then
            StrList.Delete(Position);
        until Position < 0;
      end;

      List := StringsToStr(StrList, Separator);
    finally
      RemItems.Free;
    end;
  finally
    StrList.Free;
  end;
end;

// delete one item
procedure ListDelItem(var List: string; const Separator: string; const Index: Integer);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList, False);

    StrList.Delete(Index);

    List := StringsToStr(StrList, Separator);
  finally
    StrList.Free;
  end;
end;

// return the number of item
function ListItemCount(const List, Separator: string): Integer;
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList, False);

    Result := StrList.Count;
  finally
    StrList.Free;
  end;
end;

// return the Nth item
function ListGetItem(const List, Separator: string; const Index: Integer): string;
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList, False);

    Result := StrList.Strings[Index];
  finally
    StrList.Free;
  end;
end;

// set the Nth item
procedure ListSetItem(var List: string; const Separator: string;
  const Index: Integer; const Value: string);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList, False);

    StrList.Strings[Index] := Value;

    List := StringsToStr(StrList, Separator);
  finally
    StrList.Free;
  end;
end;

// return the index of an item
function ListItemIndex(const List, Separator, Item: string): Integer;
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList, False);

    Result := StrList.IndexOf(Item);
  finally
    StrList.Free;
  end;
end;

//=== { TJclIntfCriticalSection } ============================================

constructor TJclIntfCriticalSection.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TJclIntfCriticalSection.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

function TJclIntfCriticalSection._AddRef: Integer;
begin
  FCriticalSection.Acquire;
  Result := -1;
end;

function TJclIntfCriticalSection._Release: Integer;
begin
  FCriticalSection.Release;
  Result := -1;
end;

//=== { TJclSimpleLog } ======================================================

{$IFDEF LINUX}
const
  INVALID_HANDLE_VALUE = 0;
{$ENDIF LINUX}

constructor TJclSimpleLog.Create(const ALogFileName: string = '');
begin
  if ALogFileName = '' then
    FLogFileName := CreateDefaultFileName
  else
    FLogFileName := ALogFileName;
  FLogFileHandle := TFileHandle(INVALID_HANDLE_VALUE);
  FLoggingActive := True;
end;

function TJclSimpleLog.CreateDefaultFileName: string;
begin
  Result := PathExtractFileDirFixed(ParamStr(0)) +
    PathExtractFileNameNoExt(ParamStr(0)) + '_Err.log';
end;

destructor TJclSimpleLog.Destroy;
begin
  CloseLog;
  inherited Destroy;
end;

procedure TJclSimpleLog.ClearLog;
var
  WasOpen: Boolean;
begin
  WasOpen := LogOpen;
  if WasOpen then
    CloseLog;
  if not FileExists(FlogFileName) then
    Exit;
  FLogFileHandle := FileCreate(FLogFileName);
  FLogWasEmpty := True;
  if Not WasOpen then
    CloseLog;
end;

procedure TJclSimpleLog.CloseLog;
begin
  if LogOpen then
  begin
    FileClose(FLogFileHandle);
    FLogFileHandle := TFileHandle(INVALID_HANDLE_VALUE);
    FLogWasEmpty := False;
  end;
end;

function TJclSimpleLog.GetLogOpen: Boolean;
begin
  Result := DWORD_PTR(FLogFileHandle) <> INVALID_HANDLE_VALUE;
end;

procedure TJclSimpleLog.OpenLog;
begin
  if not LogOpen then
  begin
    FLogFileHandle := FileOpen(FLogFileName, fmOpenWrite or fmShareDenyWrite);
    if LogOpen then
      FLogWasEmpty := FileSeek(FLogFileHandle, 0, soFromEnd) = 0
    else
    begin
      FLogFileHandle := FileCreate(FLogFileName);
      FLogWasEmpty := True;
      if LogOpen then
        FileWrite(FLogFileHandle, BOM_UTF8[0], Length(BOM_UTF8));
    end;
  end
  else
    FLogWasEmpty := False;
end;

procedure TJclSimpleLog.Write(const Text: string; Indent: Integer = 0; KeepOpen: Boolean = true);
var
  S: string;
  UTF8S: TUTF8String;
  SL: TStringList;
  I: Integer;
  WasOpen: Boolean;
begin
  if LoggingActive then
  begin
    WasOpen := LogOpen;
    if not WasOpen then
      OpenLog;
    if LogOpen then
    begin
      SL := TStringList.Create;
      try
        SL.Text := Text;
        for I := 0 to SL.Count - 1 do
        begin
          S := StringOfChar(' ', Indent) + StrEnsureSuffix(NativeLineBreak, TrimRight(SL[I]));
          UTF8S := StringToUTF8(S);
          FileWrite(FLogFileHandle, UTF8S[1], Length(UTF8S));
        end;
      finally
        SL.Free;
      end;
      // Keep the logfile Open when it was opened before and the KeepOpen is active
      if Not (WasOpen and KeepOpen) then
        CloseLog;
    end;
  end;
end;

procedure TJclSimpleLog.Write(Strings: TStrings; Indent: Integer = 0; KeepOpen: Boolean = true);
begin
  if Assigned(Strings) then
    Write(Strings.Text, Indent, KeepOpen);
end;

procedure TJclSimpleLog.TimeWrite(const Text: string; Indent: Integer = 0; KeepOpen: Boolean = true);
var
  S: string;
  UTF8S: TUTF8String;
  SL: TStringList;
  I: Integer;
  WasOpen: Boolean;
begin
  if LoggingActive then
  begin
    WasOpen := LogOpen;
    if not LogOpen then
      OpenLog;
    if LogOpen then
    begin
      SL := TStringList.Create;
      try
        SL.Text := Text;
        for I := 0 to SL.Count - 1 do
        begin
          if DateTimeFormatStr = '' then
            S := DateTimeToStr(Now)+' : '+StringOfChar(' ', Indent) + StrEnsureSuffix(NativeLineBreak, TrimRight(SL[I]))
          else
            S := FormatDateTime( DateTimeFormatStr, Now)+' : '+StringOfChar(' ', Indent) + StrEnsureSuffix(NativeLineBreak, TrimRight(SL[I]));
          UTF8S := StringToUTF8(S);
          FileWrite(FLogFileHandle, UTF8S[1], Length(UTF8S));
        end;
      finally
        SL.Free;
      end;
      if Not WasOpen and Not KeepOpen then
        CloseLog;
    end;
  end;
end;

procedure TJclSimpleLog.TimeWrite(Strings: TStrings; Indent: Integer = 0; KeepOpen: Boolean = true);
begin
  if Assigned(Strings) then
    TimeWrite(Strings.Text, Indent, KeepOpen);
end;

procedure TJclSimpleLog.WriteStamp(SeparatorLen: Integer = 0; KeepOpen: Boolean = true);
var
  WasOpen: Boolean;
begin
  if SeparatorLen <= 0 then
    SeparatorLen := 40;
  if LoggingActive then
  begin
    WasOpen := LogOpen;
    if not LogOpen then
    begin
      OpenLog;
      if LogOpen and not FLogWasEmpty then
        Write(NativeLineBreak);
    end;
    if LogOpen then
    begin
      Write(StrRepeat('=', SeparatorLen), 0, True);
      if DateTimeFormatStr = '' then
        Write(Format('= %-*s =', [SeparatorLen - 4, DateTimeToStr(Now)]), 0, True)
      else
        Write(Format('= %-*s =', [SeparatorLen - 4, FormatDateTime( DateTimeFormatStr, Now)]), 0, True);
      Write(StrRepeat('=', SeparatorLen), 0, True);
      if Not WasOpen and Not KeepOpen then
        CloseLog;
    end;
  end;
end;

procedure InitSimpleLog(const ALogFileName: string = ''; AOpenLog: Boolean = true);
begin
  if Assigned(SimpleLog) then
    FreeAndNil(SimpleLog);
  SimpleLog := TJclSimpleLog.Create(ALogFileName);
  if AOpenLog then
    SimpleLog.OpenLog;
end;

function TJclFormatSettings.GetCurrencyDecimals: Byte;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.CurrencyDecimals;
{$ELSE}
  Result := SysUtils.CurrencyDecimals;
{$ENDIF}
end;

function TJclFormatSettings.GetCurrencyFormat: Byte;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.CurrencyFormat;
{$ELSE}
  Result := SysUtils.CurrencyFormat;
{$ENDIF}
end;

function TJclFormatSettings.GetCurrencyString: string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.CurrencyString;
{$ELSE}
  Result := SysUtils.CurrencyString;
{$ENDIF}
end;

function TJclFormatSettings.GetDateSeparator: Char;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.DateSeparator;
{$ELSE}
  Result := SysUtils.DateSeparator;
{$ENDIF}
end;

function TJclFormatSettings.GetDayNamesHighIndex: Integer;
begin
{$IFDEF RTL220_UP}
  Result := High(FormatSettings.LongDayNames);
{$ELSE}
  Result := High(SysUtils.LongDayNames);
{$ENDIF}
end;

function TJclFormatSettings.GetDayNamesLowIndex: Integer;
begin
{$IFDEF RTL220_UP}
  Result := Low(FormatSettings.LongDayNames);
{$ELSE}
  Result := Low(SysUtils.LongDayNames);
{$ENDIF}
end;

function TJclFormatSettings.GetDecimalSeparator: Char;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.DecimalSeparator;
{$ELSE}
  Result := SysUtils.DecimalSeparator;
{$ENDIF}
end;

function TJclFormatSettings.GetListSeparator: Char;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.ListSeparator;
{$ELSE}
  Result := SysUtils.ListSeparator;
{$ENDIF}
end;

function TJclFormatSettings.GetLongDateFormat: string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.LongDateFormat;
{$ELSE}
  Result := SysUtils.LongDateFormat;
{$ENDIF}
end;

{ TJclFormatSettings }

function TJclFormatSettings.GetLongDayNames(AIndex: Integer): string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.LongDayNames[AIndex];
{$ELSE}
  Result := SysUtils.LongDayNames[AIndex];
{$ENDIF}
end;

function TJclFormatSettings.GetLongMonthNames(AIndex: Integer): string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.LongMonthNames[AIndex];
{$ELSE}
  Result := SysUtils.LongMonthNames[AIndex];
{$ENDIF}
end;

function TJclFormatSettings.GetLongTimeFormat: string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.LongTimeFormat;
{$ELSE}
  Result := SysUtils.LongTimeFormat;
{$ENDIF}
end;

function TJclFormatSettings.GetMonthNamesHighIndex: Integer;
begin
{$IFDEF RTL220_UP}
  Result := High(FormatSettings.LongMonthNames);
{$ELSE}
  Result := High(SysUtils.LongMonthNames);
{$ENDIF}
end;

function TJclFormatSettings.GetMonthNamesLowIndex: Integer;
begin
{$IFDEF RTL220_UP}
  Result := Low(FormatSettings.LongMonthNames);
{$ELSE}
  Result := Low(SysUtils.LongMonthNames);
{$ENDIF}
end;

function TJclFormatSettings.GetNegCurrFormat: Byte;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.NegCurrFormat;
{$ELSE}
  Result := SysUtils.NegCurrFormat;
{$ENDIF}
end;

function TJclFormatSettings.GetShortDateFormat: string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.ShortDateFormat;
{$ELSE}
  Result := SysUtils.ShortDateFormat;
{$ENDIF}
end;

function TJclFormatSettings.GetShortDayNames(AIndex: Integer): string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.ShortDayNames[AIndex];
{$ELSE}
  Result := SysUtils.ShortDayNames[AIndex];
{$ENDIF}
end;

function TJclFormatSettings.GetShortMonthNames(AIndex: Integer): string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.ShortMonthNames[AIndex];
{$ELSE}
  Result := SysUtils.ShortMonthNames[AIndex];
{$ENDIF}
end;

function TJclFormatSettings.GetShortTimeFormat: string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.ShortTimeFormat;
{$ELSE}
  Result := SysUtils.ShortTimeFormat;
{$ENDIF}
end;

function TJclFormatSettings.GetThousandSeparator: Char;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.ThousandSeparator;
{$ELSE}
  Result := SysUtils.ThousandSeparator;
{$ENDIF}
end;

function TJclFormatSettings.GetTimeAMString: string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.TimeAMString;
{$ELSE}
  Result := SysUtils.TimeAMString;
{$ENDIF}
end;

function TJclFormatSettings.GetTimePMString: string;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.TimePMString;
{$ELSE}
  Result := SysUtils.TimePMString;
{$ENDIF}
end;

function TJclFormatSettings.GetTimeSeparator: Char;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.TimeSeparator;
{$ELSE}
  Result := SysUtils.TimeSeparator;
{$ENDIF}
end;

function TJclFormatSettings.GetTwoDigitYearCenturyWindow: Word;
begin
{$IFDEF RTL220_UP}
  Result := FormatSettings.TwoDigitYearCenturyWindow;
{$ELSE}
  Result := SysUtils.TwoDigitYearCenturyWindow;
{$ENDIF}
end;

procedure TJclFormatSettings.SetCurrencyDecimals(AValue: Byte);
begin
{$IFDEF RTL220_UP}
  FormatSettings.CurrencyDecimals := AValue;
{$ELSE}
  SysUtils.CurrencyDecimals := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetCurrencyFormat(const AValue: Byte);
begin
{$IFDEF RTL220_UP}
  FormatSettings.CurrencyFormat := AValue;
{$ELSE}
  SysUtils.CurrencyFormat := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetCurrencyString(AValue: string);
begin
{$IFDEF RTL220_UP}
  FormatSettings.CurrencyString := AValue;
{$ELSE}
  SysUtils.CurrencyString := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetDateSeparator(const AValue: Char);
begin
{$IFDEF RTL220_UP}
  FormatSettings.DateSeparator := AValue;
{$ELSE}
  SysUtils.DateSeparator := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetDecimalSeparator(AValue: Char);
begin
{$IFDEF RTL220_UP}
  FormatSettings.DecimalSeparator := AValue;
{$ELSE}
  SysUtils.DecimalSeparator := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetListSeparator(const AValue: Char);
begin
{$IFDEF RTL220_UP}
  FormatSettings.ListSeparator := AValue;
{$ELSE}
  SysUtils.ListSeparator := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetLongDateFormat(const AValue: string);
begin
{$IFDEF RTL220_UP}
  FormatSettings.LongDateFormat := AValue;
{$ELSE}
  SysUtils.LongDateFormat := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetLongTimeFormat(const AValue: string);
begin
{$IFDEF RTL220_UP}
  FormatSettings.LongTimeFormat := AValue;
{$ELSE}
  SysUtils.LongTimeFormat := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetNegCurrFormat(const AValue: Byte);
begin
{$IFDEF RTL220_UP}
  FormatSettings.NegCurrFormat := AValue;
{$ELSE}
  SysUtils.NegCurrFormat := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetShortDateFormat(AValue: string);
begin
{$IFDEF RTL220_UP}
  FormatSettings.ShortDateFormat := AValue;
{$ELSE}
  SysUtils.ShortDateFormat := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetShortTimeFormat(const AValue: string);
begin
{$IFDEF RTL220_UP}
  FormatSettings.ShortTimeFormat := AValue;
{$ELSE}
  SysUtils.ShortTimeFormat := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetThousandSeparator(AValue: Char);
begin
{$IFDEF RTL220_UP}
  FormatSettings.TimeSeparator := AValue;
{$ELSE}
  SysUtils.TimeSeparator := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetTimeAMString(const AValue: string);
begin
{$IFDEF RTL220_UP}
  FormatSettings.TimeAMString := AValue;
{$ELSE}
  SysUtils.TimeAMString := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetTimePMString(const AValue: string);
begin
{$IFDEF RTL220_UP}
  FormatSettings.TimePMString := AValue;
{$ELSE}
  SysUtils.TimePMString := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetTimeSeparator(const AValue: Char);
begin
{$IFDEF RTL220_UP}
  FormatSettings.TimeSeparator := AValue;
{$ELSE}
  SysUtils.TimeSeparator := AValue;
{$ENDIF}
end;

procedure TJclFormatSettings.SetTwoDigitYearCenturyWindow(const AValue: Word);
begin
{$IFDEF RTL220_UP}
  FormatSettings.TwoDigitYearCenturyWindow:= AValue;
{$ELSE}
  SysUtils.TwoDigitYearCenturyWindow:= AValue;
{$ENDIF}
end;

function VarIsNullEmpty(const V: Variant): Boolean;
begin
  Result := VarIsNull(V) or VarIsEmpty(V);
end;

function VarIsNullEmptyBlank(const V: Variant): Boolean;
begin
  Result := VarIsNull(V) or VarIsEmpty(V) or (VarToStr(V) = '');
end;



initialization
  SimpleLog := nil;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  {$IFDEF THREADSAFE}
  // The user must release shared memory blocks himself. We don't clean up his
  // memory leaks and make it impossible to release the shared memory in other
  // unit's finalization blocks.
  MMFFinalized := True;
  FreeAndNil(GlobalMMFHandleListCS);
  {$ENDIF THREADSAFE}
  {$ENDIF MSWINDOWS}
  if Assigned(SimpleLog) then
    FreeAndNil(SimpleLog);
end.
