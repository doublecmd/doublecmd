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
{ The Original Code is JclSynch.pas.                                                               }
{                                                                                                  }
{ The Initial Developers of the Original Code are Marcel van Brakel and Azret Botash.              }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains various classes and support routines for implementing synchronisation in      }
{ multithreaded applications. This ranges from interlocked access to simple typed variables to     }
{ wrapper classes for synchronisation primitives provided by the operating system                  }
{ (critical section, semaphore, mutex etc). It also includes three user defined classes to         }
{ complement these.                                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclSynch;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows, JclWin32,
  {$ENDIF MSWINDOWS}
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows, JclWin32,
  {$ENDIF MSWINDOWS}
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

// Locked Integer manipulation
//
// Routines to manipulate simple typed variables in a thread safe manner
function LockedAdd(var Target: Integer; Value: Integer): Integer; overload;
function LockedCompareExchange(var Target: Integer; Exch, Comp: Integer): Integer; overload;
function LockedCompareExchange(var Target: TObject; Exch, Comp: TObject): TObject; overload;
function LockedCompareExchange(var Target: Pointer; Exch, Comp: Pointer): Pointer; overload;
function LockedDec(var Target: Integer): Integer; overload;
function LockedExchange(var Target: Integer; Value: Integer): Integer; overload;
function LockedExchangeAdd(var Target: Integer; Value: Integer): Integer; overload;
function LockedExchangeDec(var Target: Integer): Integer; overload;
function LockedExchangeInc(var Target: Integer): Integer; overload;
function LockedExchangeSub(var Target: Integer; Value: Integer): Integer; overload;
function LockedInc(var Target: Integer): Integer; overload;
function LockedSub(var Target: Integer; Value: Integer): Integer; overload;

{$IFDEF CPU64}
function LockedAdd(var Target: Int64; Value: Int64): Int64; overload;
function LockedCompareExchange(var Target: Int64; Exch, Comp: Int64): Int64; overload;
function LockedDec(var Target: Int64): Int64; overload;
function LockedExchange(var Target: Int64; Value: Int64): Int64; overload;
function LockedExchangeAdd(var Target: Int64; Value: Int64): Int64; overload;
function LockedExchangeDec(var Target: Int64): Int64; overload;
function LockedExchangeInc(var Target: Int64): Int64; overload;
function LockedExchangeSub(var Target: Int64; Value: Int64): Int64; overload;
function LockedInc(var Target: Int64): Int64; overload;
function LockedSub(var Target: Int64; Value: Int64): Int64; overload;

{$IFDEF BORLAND}
function LockedDec(var Target: NativeInt): NativeInt; overload;
function LockedInc(var Target: NativeInt): NativeInt; overload;
{$ENDIF BORLAND}
{$ENDIF CPU64}

// TJclDispatcherObject
//
// Base class for operating system provided synchronisation primitives
type
  TJclWaitResult = (wrAbandoned, wrError, wrIoCompletion, wrSignaled, wrTimeout);

  TJclWaitHandle = THandle;

  TJclDispatcherObject = class(TObject)
  private
    FExisted: Boolean;
    FHandle: TJclWaitHandle;
    FName: string;
  public
    constructor Attach(AHandle: TJclWaitHandle);
    destructor Destroy; override;
    //function MsgWaitFor(const TimeOut: Cardinal): TJclWaitResult; Mask: DWORD): TJclWaitResult;
    //function MsgWaitForEx(const TimeOut: Cardinal): TJclWaitResult; Mask: DWORD): TJclWaitResult;
    function SignalAndWait(const Obj: TJclDispatcherObject; TimeOut: Cardinal;
      Alertable: Boolean): TJclWaitResult;
    function WaitAlertable(const TimeOut: Cardinal): TJclWaitResult;
    function WaitFor(const TimeOut: Cardinal): TJclWaitResult;
    function WaitForever: TJclWaitResult;
    property Existed: Boolean read FExisted;
    property Handle: TJclWaitHandle read FHandle;
    property Name: string read FName;
  end;

// Wait functions
//
// Object enabled Wait functions (takes TJclDispatcher objects as parameter as
// opposed to handles) mostly for convenience
function WaitForMultipleObjects(const Objects: array of TJclDispatcherObject;
  WaitAll: Boolean; TimeOut: Cardinal): Cardinal;
function WaitAlertableForMultipleObjects(const Objects: array of TJclDispatcherObject;
  WaitAll: Boolean; TimeOut: Cardinal): Cardinal;

type
  TJclCriticalSection = class(TObject)
  private
    FCriticalSection: TRTLCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class procedure CreateAndEnter(var CS: TJclCriticalSection);
    procedure Enter;
    procedure Leave;
  end;

  TJclCriticalSectionEx = class(TJclCriticalSection)
  private
    FSpinCount: Cardinal;
    function GetSpinCount: Cardinal;
    procedure SetSpinCount(const Value: Cardinal);
  public
    constructor Create; override;
    constructor CreateEx(SpinCount: Cardinal; NoFailEnter: Boolean); virtual;
    class function GetSpinTimeOut: Cardinal;
    class procedure SetSpinTimeOut(const Value: Cardinal);
    function TryEnter: Boolean;
    property SpinCount: Cardinal read GetSpinCount write SetSpinCount;
  end;

  TJclEvent = class(TJclDispatcherObject)
  public
    constructor Create(SecAttr: PSecurityAttributes; Manual, Signaled: Boolean; const Name: string);
    constructor Open(Access: Cardinal; Inheritable: Boolean; const Name: string);
    function Pulse: Boolean;
    function ResetEvent: Boolean;
    function SetEvent: Boolean;
  end;

  TJclWaitableTimer = class(TJclDispatcherObject)
  private
    FResume: Boolean;
  public
    constructor Create(SecAttr: PSecurityAttributes; Manual: Boolean; const Name: string);
    constructor Open(Access: Cardinal; Inheritable: Boolean; const Name: string);
    function Cancel: Boolean;
    function SetTimer(const DueTime: Int64; Period: Longint; Resume: Boolean): Boolean;
    function SetTimerApc(const DueTime: Int64; Period: Longint; Resume: Boolean; Apc: TFNTimerAPCRoutine; Arg: Pointer): Boolean;
  end;

  TJclSemaphore = class(TJclDispatcherObject)
  public
    constructor Create(SecAttr: PSecurityAttributes; Initial, Maximum: Longint; const Name: string);
    constructor Open(Access: Cardinal; Inheritable: Boolean; const Name: string);
    function Release(ReleaseCount: Longint): Boolean;
    function ReleasePrev(ReleaseCount: Longint; var PrevCount: Longint): Boolean;
  end;

  TJclMutex = class(TJclDispatcherObject)
  public
    constructor Create(SecAttr: PSecurityAttributes; InitialOwner: Boolean;
      const Name: string);
    constructor Open(Access: Cardinal; Inheritable: Boolean; const Name: string);
    function Acquire(const TimeOut: Cardinal = INFINITE): Boolean;
    function Release: Boolean;
  end;

  POptexSharedInfo = ^TOptexSharedInfo;
  TOptexSharedInfo = record
    SpinCount: Integer;      // number of times to try and enter the optex before
                             // waiting on kernel event, 0 on single processor
    LockCount: Integer;      // count of enter attempts
    ThreadId: Longword;      // id of thread that owns the optex, 0 if free
    RecursionCount: Integer; // number of times the optex is owned, 0 if free
  end;

  TJclOptex = class(TObject)
  private
    FEvent: TJclEvent;
    FExisted: Boolean;
    FFileMapping: THandle;
    FName: string;
    FSharedInfo: POptexSharedInfo;
    function GetUniProcess: Boolean;
    function GetSpinCount: Integer;
    procedure SetSpinCount(Value: Integer);
  public
    constructor Create(const Name: string = ''; SpinCount: Integer = 4000);
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    function TryEnter: Boolean;
    property Existed: Boolean read FExisted;
    property Name: string read FName;
    property SpinCount: Integer read GetSpinCount write SetSpinCount;
    property UniProcess: Boolean read GetUniProcess;
  end;

  TMrewPreferred = (mpReaders, mpWriters, mpEqual);

  TMrewThreadInfo = record
    ThreadId: Longword;      // client-id of thread
    RecursionCount: Integer; // number of times a thread accessed the mrew
    Reader: Boolean;         // true if reader, false if writer
  end;
  TMrewThreadInfoArray = array of TMrewThreadInfo;

  TJclMultiReadExclusiveWrite = class(TObject)
  private
    FLock: TJclCriticalSection;
    FPreferred: TMrewPreferred;
    FSemReaders: TJclSemaphore;
    FSemWriters: TJclSemaphore;
    FState: Integer;
    FThreads: TMrewThreadInfoArray;
    FWaitingReaders: Integer;
    FWaitingWriters: Integer;
    procedure AddToThreadList(ThreadId: Longword; Reader: Boolean);
    procedure RemoveFromThreadList(Index: Integer);
    function FindThread(ThreadId: Longword): Integer;
    procedure ReleaseWaiters(WasReading: Boolean);
  protected
    procedure Release;
  public
    constructor Create(Preferred: TMrewPreferred);

    destructor Destroy; override;
    procedure BeginRead;
    procedure BeginWrite;
    procedure EndRead;
    procedure EndWrite;
  end;

  PMetSectSharedInfo = ^TMetSectSharedInfo;
  TMetSectSharedInfo = record
    Initialized: LongBool;    // Is the metered section initialized?
    SpinLock: Longint;        // Used to gain access to this structure
    ThreadsWaiting: Longint;  // Count of threads waiting
    AvailableCount: Longint;  // Available resource count
    MaximumCount: Longint;    // Maximum resource count
  end;

  PMeteredSection = ^TMeteredSection;
  TMeteredSection = record
    Event: THandle;           // Handle to a kernel event object
    FileMap: THandle;         // Handle to memory mapped file
    SharedInfo: PMetSectSharedInfo;
  end;

  TJclMeteredSection = class(TObject)
  private
    FMetSect: PMeteredSection;
    procedure CloseMeteredSection;
    function InitMeteredSection(InitialCount, MaxCount: Longint; const Name: string; OpenOnly: Boolean): Boolean;
    function CreateMetSectEvent(const Name: string; OpenOnly: Boolean): Boolean;
    function CreateMetSectFileView(InitialCount, MaxCount: Longint; const Name: string; OpenOnly: Boolean): Boolean;
  protected
    procedure AcquireLock;
    procedure ReleaseLock;
  public
    constructor Create(InitialCount, MaxCount: Longint; const Name: string);
    constructor Open(const Name: string);
    destructor Destroy; override;
    function Enter(TimeOut: Longword): TJclWaitResult;
    function Leave(ReleaseCount: Longint): Boolean; overload;
    function Leave(ReleaseCount: Longint; out PrevCount: Longint): Boolean; overload;
  end;

// Debugging
//
// Note that the following function and structure declarations are all offically
// undocumented and, except for QueryCriticalSection, require Windows NT since
// it is all part of the Windows NT Native API.
{ TODO -cTest : Test this structures }
type
  TEventInfo = record
    EventType: Longint;       // 0 = manual, otherwise auto
    Signaled: LongBool;       // true is signaled
  end;

  TMutexInfo = record
    SignalState: Longint;     // >0 = signaled, <0 = |SignalState| recurs. acquired
    Owned: ByteBool;          // owned by thread
    Abandoned: ByteBool;      // is abandoned?
  end;

  TSemaphoreCounts = record
    CurrentCount: Longint;    // current semaphore count
    MaximumCount: Longint;    // maximum semaphore count
  end;

  TTimerInfo = record
    Remaining: TLargeInteger; // 100ns intervals until signaled
    Signaled: ByteBool;       // is signaled?
  end;

function QueryCriticalSection(CS: TJclCriticalSection; var Info: TRTLCriticalSection): Boolean;
{ TODO -cTest : Test these 4 functions }
function QueryEvent(Handle: THandle; var Info: TEventInfo): Boolean;
function QueryMutex(Handle: THandle; var Info: TMutexInfo): Boolean;
function QuerySemaphore(Handle: THandle; var Info: TSemaphoreCounts): Boolean;
function QueryTimer(Handle: THandle; var Info: TTimerInfo): Boolean;

type
  // Exceptions
  EJclWin32HandleObjectError = class(EJclWin32Error);
  EJclDispatcherObjectError = class(EJclWin32Error);
  EJclCriticalSectionError = class(EJclWin32Error);
  EJclEventError = class(EJclWin32Error);
  EJclWaitableTimerError = class(EJclWin32Error);
  EJclSemaphoreError = class(EJclWin32Error);
  EJclMutexError = class(EJclWin32Error);
  EJclMeteredSectionError = class(EJclError);

function ValidateMutexName(const aName: string): string;


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
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclLogic, JclRegistry, JclResources,
  JclSysInfo, JclStrings;

const
  RegSessionManager = {HKLM\} 'SYSTEM\CurrentControlSet\Control\Session Manager';
  RegCritSecTimeout = {RegSessionManager\} 'CriticalSectionTimeout';

// Locked Integer manipulation
function LockedAdd(var Target: Integer; Value: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        //     EDX Value
        // <-- EAX Result
        MOV     ECX, EAX
        MOV     EAX, EDX
        LOCK XADD [ECX], EAX
        ADD     EAX, EDX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        //     EDX Value
        // <-- EAX Result
        MOV     EAX, EDX
        LOCK XADD [RCX], EAX
        ADD     EAX, EDX
        {$ENDIF CPU64}
end;

function LockedCompareExchange(var Target: Integer; Exch, Comp: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        //     EDX Exch
        //     ECX Comp
        // <-- EAX Result
        XCHG    EAX, ECX
        //     EAX Comp
        //     EDX Exch
        //     ECX Target
        LOCK CMPXCHG [ECX], EDX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        //     EDX Exch
        //     R8  Comp
        // <-- EAX Result
        MOV     RAX, R8
        //     RCX Target
        //     EDX Exch
        //     RAX Comp
        LOCK CMPXCHG [RCX], EDX
        {$ENDIF CPU64}
end;

function LockedCompareExchange(var Target: Pointer; Exch, Comp: Pointer): Pointer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        //     EDX Exch
        //     ECX Comp
        // <-- EAX Result
        XCHG    EAX, ECX
        //     EAX Comp
        //     EDX Exch
        //     ECX Target
        LOCK CMPXCHG [ECX], EDX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        //     RDX Exch
        //     R8  Comp
        // <-- RAX Result
        MOV     RAX, R8
        //     RCX Target
        //     RDX Exch
        //     RAX Comp
        LOCK CMPXCHG [RCX], RDX
        {$ENDIF CPU64}
end;

function LockedCompareExchange(var Target: TObject; Exch, Comp: TObject): TObject;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        //     EDX Exch
        //     ECX Comp
        // <-- EAX Result
        XCHG    EAX, ECX
        //     EAX Comp
        //     EDX Exch
        //     ECX Target
        LOCK CMPXCHG [ECX], EDX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        //     RDX Exch
        //     R8  Comp
        // <-- RAX Result
        MOV     RAX, R8
        // --> RCX Target
        //     RDX Exch
        //     RAX Comp
        LOCK CMPXCHG [RCX], RDX
        {$ENDIF CPU64}
end;

function LockedDec(var Target: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        // <-- EAX Result
        MOV     ECX, EAX
        MOV     EAX, -1
        LOCK XADD [ECX], EAX
        DEC     EAX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        // <-- EAX Result
        MOV     EAX, -1
        LOCK XADD [RCX], EAX
        DEC     EAX
        {$ENDIF CPU64}
end;

function LockedExchange(var Target: Integer; Value: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        //     EDX Value
        // <-- EAX Result
        MOV     ECX, EAX
        MOV     EAX, EDX
        //     ECX Target
        //     EAX Value
        LOCK XCHG [ECX], EAX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        //     EDX Value
        // <-- EAX Result
        MOV     EAX, EDX
        //     RCX Target
        //     EAX Value
        LOCK XCHG [RCX], EAX
        {$ENDIF CPU64}
end;

function LockedExchangeAdd(var Target: Integer; Value: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        //     EDX Value
        // <-- EAX Result
        MOV     ECX, EAX
        MOV     EAX, EDX
        //     ECX Target
        //     EAX Value
        LOCK XADD [ECX], EAX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        //     EDX Value
        // <-- EAX Result
        MOV     EAX, EDX
        //     RCX Target
        //     EAX Value
        LOCK XADD [RCX], EAX
        {$ENDIF CPU64}
end;

function LockedExchangeDec(var Target: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        // <-- EAX Result
        MOV     ECX, EAX
        MOV     EAX, -1
        LOCK XADD [ECX], EAX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        // <-- EAX Result
        MOV     EAX, -1
        LOCK XADD [RCX], EAX
        {$ENDIF CPU64}
end;

function LockedExchangeInc(var Target: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        // <-- EAX Result
        MOV     ECX, EAX
        MOV     EAX, 1
        LOCK XADD [ECX], EAX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        // <-- EAX Result
        MOV     EAX, 1
        LOCK XADD [RCX], EAX
        {$ENDIF CPU64}
end;

function LockedExchangeSub(var Target: Integer; Value: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        //     EDX Value
        // <-- EAX Result
        MOV     ECX, EAX
        NEG     EDX
        MOV     EAX, EDX
        //     ECX Target
        //     EAX -Value
        LOCK XADD [ECX], EAX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        //     EDX Value
        // <-- EAX Result
        NEG     EDX
        MOV     EAX, EDX
        //     RCX Target
        //     EAX -Value
        LOCK XADD [RCX], EAX
        {$ENDIF CPU64}
end;

function LockedInc(var Target: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        // <-- EAX Result
        MOV     ECX, EAX
        MOV     EAX, 1
        LOCK XADD [ECX], EAX
        INC     EAX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        // <-- EAX Result
        MOV     EAX, 1
        LOCK XADD [RCX], EAX
        INC     EAX
        {$ENDIF CPU64}
end;

function LockedSub(var Target: Integer; Value: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        //     EDX Value
        // <-- EAX Result
        MOV     ECX, EAX
        NEG     EDX
        MOV     EAX, EDX
        LOCK XADD [ECX], EAX
        ADD     EAX, EDX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        //     EDX Value
        // <-- EAX Result
        NEG     EDX
        MOV     EAX, EDX
        LOCK XADD [RCX], EAX
        ADD     EAX, EDX
        {$ENDIF CPU64}
end;

{$IFDEF CPU64}

// Locked Int64 manipulation
function LockedAdd(var Target: Int64; Value: Int64): Int64;
asm
        // --> RCX Target
        //     RDX Value
        // <-- RAX Result
        MOV     RAX, RDX
        LOCK XADD [RCX], RAX
        ADD     RAX, RDX
end;

function LockedCompareExchange(var Target: Int64; Exch, Comp: Int64): Int64;
asm
        // --> RCX Target
        //     RDX Exch
        //     R8  Comp
        // <-- RAX Result
        MOV     RAX, R8
        LOCK CMPXCHG [RCX], RDX
end;

function LockedDec(var Target: Int64): Int64;
asm
        // --> RCX Target
        // <-- RAX Result
        MOV     RAX, -1
        LOCK XADD [RCX], RAX
        DEC     RAX
end;

function LockedExchange(var Target: Int64; Value: Int64): Int64;
asm
        // --> RCX Target
        //     RDX Value
        // <-- RAX Result
        MOV     RAX, RDX
        LOCK XCHG [RCX], RAX
end;

function LockedExchangeAdd(var Target: Int64; Value: Int64): Int64;
asm
        // --> RCX Target
        //     RDX Value
        // <-- RAX Result
        MOV     RAX, RDX
        LOCK XADD [RCX], RAX
end;

function LockedExchangeDec(var Target: Int64): Int64;
asm
        // --> RCX Target
        // <-- RAX Result
        MOV     RAX, -1
        LOCK XADD [RCX], RAX
end;

function LockedExchangeInc(var Target: Int64): Int64;
asm
        // --> RCX Target
        // <-- RAX Result
        MOV     RAX, 1
        LOCK XADD [RCX], RAX
end;

function LockedExchangeSub(var Target: Int64; Value: Int64): Int64;
asm
        // --> RCX Target
        //     RDX Value
        // <-- RAX Result
        NEG     RDX
        MOV     RAX, RDX
        LOCK XADD [RCX], RAX
end;

function LockedInc(var Target: Int64): Int64;
asm
        // --> RCX Target
        // <-- RAX Result
        MOV     RAX, 1
        LOCK XADD [RCX], RAX
        INC     RAX
end;

function LockedSub(var Target: Int64; Value: Int64): Int64;
asm
        // --> RCX Target
        //     RDX Value
        // <-- RAX Result
        NEG     RDX
        MOV     RAX, RDX
        LOCK XADD [RCX], RAX
        ADD     RAX, RDX
end;

{$IFDEF BORLAND}

function LockedDec(var Target: NativeInt): NativeInt;
asm
        // --> RCX Target
        // <-- RAX Result
        MOV     RAX, -1
        LOCK XADD [RCX], RAX
        DEC     RAX
end;

function LockedInc(var Target: NativeInt): NativeInt;
asm
        // --> RCX Target
        // <-- RAX Result
        MOV     RAX, 1
        LOCK XADD [RCX], RAX
        INC     RAX
end;

{$ENDIF BORLAND}

{$ENDIF CPU64}

//=== { TJclDispatcherObject } ===============================================

function MapSignalResult(const Ret: DWORD): TJclWaitResult;
begin
  case Ret of
    WAIT_ABANDONED:
      Result := wrAbandoned;
    WAIT_OBJECT_0:
      Result := wrSignaled;
    WAIT_TIMEOUT:
      Result := wrTimeout;
    WAIT_IO_COMPLETION:
      Result := wrIoCompletion;
    WAIT_FAILED:
      Result := wrError;
  else
    Result := wrError;
  end;
end;

constructor TJclDispatcherObject.Attach(AHandle: TJclWaitHandle);
begin
  inherited Create;
  FExisted := True;
  FHandle := AHandle;
  FName := '';
end;

destructor TJclDispatcherObject.Destroy;
begin
  CloseHandle(FHandle);
  inherited Destroy;
end;

{ TODO: Use RTDL Version of SignalObjectAndWait }

function TJclDispatcherObject.SignalAndWait(const Obj: TJclDispatcherObject;
  TimeOut: Cardinal; Alertable: Boolean): TJclWaitResult;
begin
  // Note: Do not make this method virtual! It's only available on NT 4 up...
  Result := MapSignalResult(Cardinal({$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.SignalObjectAndWait(Obj.Handle, Handle, TimeOut, Alertable)));
end;

function TJclDispatcherObject.WaitAlertable(const TimeOut: Cardinal): TJclWaitResult;
begin
  Result := MapSignalResult({$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.WaitForSingleObjectEx(FHandle, TimeOut, True));
end;

function TJclDispatcherObject.WaitFor(const TimeOut: Cardinal): TJclWaitResult;
begin
  Result := MapSignalResult({$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.WaitForSingleObject(FHandle, TimeOut));
end;

function TJclDispatcherObject.WaitForever: TJclWaitResult;
begin
  Result := WaitFor(INFINITE);
end;

// Wait functions
function WaitForMultipleObjects(const Objects: array of TJclDispatcherObject;
  WaitAll: Boolean; TimeOut: Cardinal): Cardinal;
var
  Handles: array of TJclWaitHandle;
  I, Count: Integer;
begin
  Count := High(Objects) + 1;
  SetLength(Handles, Count);
  for I := 0 to Count - 1 do
    Handles[I] := Objects[I].Handle;
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.WaitForMultipleObjects(Count, @Handles[0], WaitAll, TimeOut);
end;

function WaitAlertableForMultipleObjects(const Objects: array of TJclDispatcherObject;
  WaitAll: Boolean; TimeOut: Cardinal): Cardinal;
var
  Handles: array of TJclWaitHandle;
  I, Count: Integer;
begin
  Count := High(Objects) + 1;
  SetLength(Handles, Count);
  for I := 0 to Count - 1 do
    Handles[I] := Objects[I].Handle;
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.WaitForMultipleObjectsEx(Count, @Handles[0], WaitAll, TimeOut, True);
end;

//=== { TJclCriticalSection } ================================================

constructor TJclCriticalSection.Create;
begin
  inherited Create;
  {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InitializeCriticalSection(FCriticalSection);
end;

destructor TJclCriticalSection.Destroy;
begin
  {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
end;

class procedure TJclCriticalSection.CreateAndEnter(var CS: TJclCriticalSection);
var
  NewCritSect: TJclCriticalSection;
begin
  NewCritSect := TJclCriticalSection.Create;
  if LockedCompareExchange(Pointer(CS), Pointer(NewCritSect), nil) <> nil then
  begin
    // LoadInProgress was <> nil -> no exchange took place, free the CS
    NewCritSect.Free;
  end;
  CS.Enter;
end;

procedure TJclCriticalSection.Enter;
begin
  {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.EnterCriticalSection(FCriticalSection);
end;

procedure TJclCriticalSection.Leave;
begin
  {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.LeaveCriticalSection(FCriticalSection);
end;

//== { TJclCriticalSectionEx } ===============================================

const
  DefaultCritSectSpinCount = 4000;

constructor TJclCriticalSectionEx.Create;
begin
  CreateEx(DefaultCritSectSpinCount, False);
end;

{ TODO: Use RTDL Version of InitializeCriticalSectionAndSpinCount }

constructor TJclCriticalSectionEx.CreateEx(SpinCount: Cardinal;
  NoFailEnter: Boolean);
begin
  FSpinCount := SpinCount;
  if NoFailEnter then
    SpinCount := SpinCount or Cardinal($80000000);

  if not InitializeCriticalSectionAndSpinCount(FCriticalSection, SpinCount) then
    raise EJclCriticalSectionError.CreateRes(@RsSynchInitCriticalSection);
end;

function TJclCriticalSectionEx.GetSpinCount: Cardinal;
begin
  // Spinning only makes sense on multiprocessor systems. On a single processor
  // system the thread would simply waste cycles while the owning thread is
  // suspended and thus cannot release the critical section.
  if ProcessorCount = 1 then
    Result := 0
  else
    Result := FSpinCount;
end;

class function TJclCriticalSectionEx.GetSpinTimeOut: Cardinal;
begin
  Result := Cardinal(RegReadInteger(HKEY_LOCAL_MACHINE, RegSessionManager,
    RegCritSecTimeout));
end;

{ TODO: Use RTLD version of SetCriticalSectionSpinCount }
procedure TJclCriticalSectionEx.SetSpinCount(const Value: Cardinal);
begin
  FSpinCount := SetCriticalSectionSpinCount(FCriticalSection, Value);
end;

class procedure TJclCriticalSectionEx.SetSpinTimeOut(const Value: Cardinal);
begin
  RegWriteInteger(HKEY_LOCAL_MACHINE, RegSessionManager, RegCritSecTimeout,
    Integer(Value));
end;

{ TODO: Use RTLD version of TryEnterCriticalSection }
function TJclCriticalSectionEx.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FCriticalSection);
end;

//== { TJclEvent } ===========================================================

constructor TJclEvent.Create(SecAttr: PSecurityAttributes; Manual, Signaled: Boolean; const Name: string);
begin
  inherited Create;
  FName := Name;
  FHandle := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CreateEvent(SecAttr, Manual, Signaled, PChar(FName));
  if FHandle = 0 then
    raise EJclEventError.CreateRes(@RsSynchCreateEvent);
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
end;

constructor TJclEvent.Open(Access: Cardinal; Inheritable: Boolean;
  const Name: string);
begin
  FName := Name;
  FExisted := True;
  FHandle := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.OpenEvent(Access, Inheritable, PChar(Name));
  if FHandle = 0 then
    raise EJclEventError.CreateRes(@RsSynchOpenEvent);
end;

function TJclEvent.Pulse: Boolean;
begin
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.PulseEvent(FHandle);
end;

function TJclEvent.ResetEvent: Boolean;
begin
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.ResetEvent(FHandle);
end;

function TJclEvent.SetEvent: Boolean;
begin
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.SetEvent(FHandle);
end;

//=== { TJclWaitableTimer } ==================================================

{ TODO: Use RTLD version of CreateWaitableTimer }
constructor TJclWaitableTimer.Create(SecAttr: PSecurityAttributes;
  Manual: Boolean; const Name: string);
begin
  FName := Name;
  FResume := False;
  FHandle := CreateWaitableTimer(SecAttr, Manual, PChar(Name));
  if FHandle = 0 then
    raise EJclWaitableTimerError.CreateRes(@RsSynchCreateWaitableTimer);
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
end;

{ TODO: Use RTLD version of CancelWaitableTimer }
function TJclWaitableTimer.Cancel: Boolean;
begin
  Result := CancelWaitableTimer(FHandle);
end;

{ TODO: Use RTLD version of OpenWaitableTimer }

constructor TJclWaitableTimer.Open(Access: Cardinal; Inheritable: Boolean;
  const Name: string);
begin
  FExisted := True;
  FName := Name;
  FResume := False;
  FHandle := OpenWaitableTimer(Access, Inheritable, PChar(Name));
  if FHandle = 0 then
    raise EJclWaitableTimerError.CreateRes(@RsSynchOpenWaitableTimer);
end;

{ TODO: Use RTLD version of SetWaitableTimer }
function TJclWaitableTimer.SetTimer(const DueTime: Int64; Period: Longint;
  Resume: Boolean): Boolean;
var
  DT: Int64;
begin
  DT := DueTime;
  FResume := Resume;
  Result := SetWaitableTimer(FHandle, DT, Period, nil, nil, FResume);
end;

{ TODO -cHelp : OS restrictions }
function TJclWaitableTimer.SetTimerApc(const DueTime: Int64; Period: Longint;
  Resume: Boolean; Apc: TFNTimerAPCRoutine; Arg: Pointer): Boolean;
var
  DT: Int64;
begin
  DT := DueTime;
  FResume := Resume;
  Result := RtdlSetWaitableTimer(FHandle, DT, Period, Apc, Arg, FResume);
  { TODO : Exception for Win9x, older WinNT? }
  // if not Result and (GetLastError = ERROR_CALL_NOT_IMPLEMENTED) then
  //   RaiseLastOSError;
end;

//== { TJclSemaphore } =======================================================

constructor TJclSemaphore.Create(SecAttr: PSecurityAttributes;
  Initial, Maximum: Integer; const Name: string);
begin
  Assert((Initial >= 0) and (Maximum > 0));
  FName := Name;
  FHandle := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CreateSemaphore(SecAttr, Initial, Maximum, PChar(Name));
  if FHandle = 0 then
    raise EJclSemaphoreError.CreateRes(@RsSynchCreateSemaphore);
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
end;

constructor TJclSemaphore.Open(Access: Cardinal; Inheritable: Boolean;
  const Name: string);
begin
  FName := Name;
  FExisted := True;
  FHandle := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.OpenSemaphore(Access, Inheritable, PChar(Name));
  if FHandle = 0 then
    raise EJclSemaphoreError.CreateRes(@RsSynchOpenSemaphore);
end;

function TJclSemaphore.ReleasePrev(ReleaseCount: Longint;
  var PrevCount: Longint): Boolean;
begin
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.ReleaseSemaphore(FHandle, ReleaseCount, @PrevCount);
end;

function TJclSemaphore.Release(ReleaseCount: Integer): Boolean;
begin
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.ReleaseSemaphore(FHandle, ReleaseCount, nil);
end;

//=== { TJclMutex } ==========================================================

function TJclMutex.Acquire(const TimeOut: Cardinal): Boolean;
begin
  Result := WaitFor(TimeOut) = wrSignaled;
end;

constructor TJclMutex.Create(SecAttr: PSecurityAttributes; InitialOwner: Boolean; const Name: string);
begin
  inherited Create;
  FName := Name;
  FHandle := JclWin32.CreateMutex(SecAttr, Ord(InitialOwner), PChar(Name));
  if FHandle = 0 then
    raise EJclMutexError.CreateRes(@RsSynchCreateMutex);
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
end;

constructor TJclMutex.Open(Access: Cardinal; Inheritable: Boolean; const Name: string);
begin
  inherited Create;
  FName := Name;
  FExisted := True;
  FHandle := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.OpenMutex(Access, Inheritable, PChar(Name));
  if FHandle = 0 then
    raise EJclMutexError.CreateRes(@RsSynchOpenMutex);
end;

function TJclMutex.Release: Boolean;
begin
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.ReleaseMutex(FHandle);
end;

//=== { TJclOptex } ==========================================================

constructor TJclOptex.Create(const Name: string; SpinCount: Integer);
begin
  FExisted := False;
  FName := Name;
  if Name = '' then
  begin
    // None shared optex, don't need filemapping, sharedinfo is local
    FFileMapping := 0;
    FEvent := TJclEvent.Create(nil, False, False, '');
    FSharedInfo := AllocMem(SizeOf(TOptexSharedInfo));
  end
  else
  begin
    // Shared optex, event protects access to sharedinfo. Creation of filemapping
    // doesn't need protection as it will automatically "open" instead of "create"
    // if another process already created it.
    FEvent := TJclEvent.Create(nil, False, False, 'Optex_Event_' + Name);
    FExisted := FEvent.Existed;
    FFileMapping := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE,
      0, SizeOf(TOptexSharedInfo), PChar('Optex_MMF_' + Name));
    Assert(FFileMapping <> 0);
    FSharedInfo := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.MapViewOfFile(FFileMapping, FILE_MAP_WRITE, 0, 0, 0);
    Assert(FSharedInfo <> nil);
  end;
  SetSpinCount(SpinCount);
end;

destructor TJclOptex.Destroy;
begin
  FreeAndNil(FEvent);
  if UniProcess then
    FreeMem(FSharedInfo)
  else
  begin
    {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.UnmapViewOfFile(FSharedInfo);
    {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CloseHandle(FFileMapping);
  end;
  inherited Destroy;
end;

procedure TJclOptex.Enter;
var
  ThreadId: Longword;
begin
  if TryEnter then
    Exit;
  ThreadId := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.GetCurrentThreadId;
  if {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedIncrement(FSharedInfo^.LockCount) = 1 then
  begin
    // Optex was unowned
    FSharedInfo^.ThreadId := ThreadId;
    FSharedInfo^.RecursionCount := 1;
  end
  else
  begin
    if FSharedInfo^.ThreadId = ThreadId then
    begin
      // We already owned it, increase ownership count
      Inc(FSharedInfo^.RecursionCount)
    end
    else
    begin
      // Optex is owner by someone else, wait for it to be released and then
      // immediately take ownership
      FEvent.WaitForever;
      FSharedInfo^.ThreadId := ThreadId;
      FSharedInfo^.RecursionCount := 1;
    end;
  end;
end;

function TJclOptex.GetSpinCount: Integer;
begin
  Result := FSharedInfo^.SpinCount;
end;

function TJclOptex.GetUniProcess: Boolean;
begin
  Result := FFileMapping = 0;
end;

procedure TJclOptex.Leave;
begin
  Dec(FSharedInfo^.RecursionCount);
  if FSharedInfo^.RecursionCount > 0 then
    {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedDecrement(FSharedInfo^.LockCount)
  else
  begin
    FSharedInfo^.ThreadId := 0;
    if {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedDecrement(FSharedInfo^.LockCount) > 0 then
      FEvent.SetEvent;
  end;
end;

procedure TJclOptex.SetSpinCount(Value: Integer);
begin
  if Value < 0 then
    Value := DefaultCritSectSpinCount;
  // Spinning only makes sense on multiprocessor systems
  if ProcessorCount > 1 then
    {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedExchange(Integer(FSharedInfo^.SpinCount), Value);
end;

function TJclOptex.TryEnter: Boolean;
var
  ThreadId: Longword;
  ThreadOwnsOptex: Boolean;
  SpinCount: Integer;
begin
  ThreadId := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.GetCurrentThreadId;
  SpinCount := FSharedInfo^.SpinCount;
  repeat
    //ThreadOwnsOptex := InterlockedCompareExchange(Pointer(FSharedInfo^.LockCount),
    //  Pointer(1), Pointer(0)) = Pointer(0); // not available on win95
    ThreadOwnsOptex := LockedCompareExchange(FSharedInfo^.LockCount, 1, 0) = 0;
    if ThreadOwnsOptex then
    begin
      // Optex was unowned
      FSharedInfo^.ThreadId := ThreadId;
      FSharedInfo^.RecursionCount := 1;
    end
    else
    begin
      if FSharedInfo^.ThreadId = ThreadId then
      begin
        // We already owned the Optex
        {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedIncrement(FSharedInfo^.LockCount);
        Inc(FSharedInfo^.RecursionCount);
        ThreadOwnsOptex := True;
      end;
    end;
    Dec(SpinCount);
  until ThreadOwnsOptex or (SpinCount <= 0);
  Result := ThreadOwnsOptex;
end;

//=== { TJclMultiReadExclusiveWrite } ========================================

constructor TJclMultiReadExclusiveWrite.Create(Preferred: TMrewPreferred);
begin
  inherited Create;
  FLock := TJclCriticalSection.Create;
  FPreferred := Preferred;
  FSemReaders := TJclSemaphore.Create(nil, 0, MaxInt, '');
  FSemWriters := TJclSemaphore.Create(nil, 0, MaxInt, '');
  SetLength(FThreads, 0);
  FState := 0;
  FWaitingReaders := 0;
  FWaitingWriters := 0;
end;

destructor TJclMultiReadExclusiveWrite.Destroy;
begin
  FreeAndNil(FSemReaders);
  FreeAndNil(FSemWriters);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TJclMultiReadExclusiveWrite.AddToThreadList(ThreadId: Longword;
  Reader: Boolean);
var
  L: Integer;
begin
  // Caller must own lock
  L := Length(FThreads);
  SetLength(FThreads, L + 1);
  FThreads[L].ThreadId := ThreadId;
  FThreads[L].RecursionCount := 1;
  FThreads[L].Reader := Reader;
end;

procedure TJclMultiReadExclusiveWrite.BeginRead;
var
  ThreadId: Longword;
  Index: Integer;
  MustWait: Boolean;
begin
  MustWait := False;
  ThreadId := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.GetCurrentThreadId;
  FLock.Enter;
  try
    Index := FindThread(ThreadId);
    if Index >= 0 then
    begin
      // Thread is on threadslist so it is already reading
      Inc(FThreads[Index].RecursionCount);
    end
    else
    begin
      // Request to read (first time)
      AddToThreadList(ThreadId, True);
      if FState >= 0 then
      begin
        // MREW is unowned or only readers. If there are no waiting writers or
        // readers are preferred then allow thread to continue, otherwise it must
        // wait it's turn
        if (FPreferred = mpReaders) or (FWaitingWriters = 0) then
          Inc(FState)
        else
        begin
          Inc(FWaitingReaders);
          MustWait := True;
        end;
      end
      else
      begin
        // MREW is owner by a writer, must wait
        Inc(FWaitingReaders);
        MustWait := True;
      end;
    end;
  finally
    FLock.Leave;
  end;
  if MustWait then
    FSemReaders.WaitForever;
end;

procedure TJclMultiReadExclusiveWrite.BeginWrite;
var
  ThreadId: Longword;
  Index: Integer;
  MustWait: Boolean;
begin
  MustWait := False;
  FLock.Enter;
  try
    ThreadId := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.GetCurrentThreadId;
    Index := FindThread(ThreadId);
    if Index < 0 then
    begin
      // Request to write (first time)
      AddToThreadList(ThreadId, False);
      if FState = 0 then
      begin
        // MREW is unowned so start writing
        FState := -1;
      end
      else
      begin
        // MREW is owner, must wait
        Inc(FWaitingWriters);
        MustWait := True;
      end;
    end
    else
    begin
      if FThreads[Index].Reader then
      begin
        // Request to write while reading
        Inc(FThreads[Index].RecursionCount);
        FThreads[Index].Reader := False;
        Dec(FState);
        if FState = 0 then
        begin
          // MREW is unowned so start writing
          FState := -1;
        end
        else
        begin
          // MREW is owned, must wait
          MustWait := True;
          Inc(FWaitingWriters);
        end;
      end
      else
        // Requesting to write while already writing
        Inc(FThreads[Index].RecursionCount);
    end;
  finally
    FLock.Leave;
  end;
  if MustWait then
    FSemWriters.WaitFor(INFINITE);
end;

procedure TJclMultiReadExclusiveWrite.EndRead;
begin
  Release;
end;

procedure TJclMultiReadExclusiveWrite.EndWrite;
begin
  Release;
end;

function TJclMultiReadExclusiveWrite.FindThread(ThreadId: Longword): Integer;
var
  I: Integer;
begin
  // Caller must lock
  Result := -1;
  for I := 0 to Length(FThreads) - 1 do
    if FThreads[I].ThreadId = ThreadId then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TJclMultiReadExclusiveWrite.Release;
var
  ThreadId: Longword;
  Index: Integer;
  WasReading: Boolean;
begin
  ThreadId := GetCurrentThreadId;
  FLock.Enter;
  try
    Index := FindThread(ThreadId);
    if Index >= 0 then
    begin
      Dec(FThreads[Index].RecursionCount);
      if FThreads[Index].RecursionCount = 0 then
      begin
        WasReading := FThreads[Index].Reader;
        if WasReading then
          Dec(FState)
        else
          FState := 0;
        RemoveFromThreadList(Index);
        if FState = 0 then
          ReleaseWaiters(WasReading);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TJclMultiReadExclusiveWrite.ReleaseWaiters(WasReading: Boolean);
var
  ToRelease: TMrewPreferred;
begin
  // Caller must Lock
  ToRelease := mpEqual;
  case FPreferred of
    mpReaders:
      if FWaitingReaders > 0 then
        ToRelease := mpReaders
      else
      if FWaitingWriters > 0 then
        ToRelease := mpWriters;
    mpWriters:
      if FWaitingWriters > 0 then
        ToRelease := mpWriters
      else
      if FWaitingReaders > 0 then
        ToRelease := mpReaders;
    mpEqual:
      if WasReading then
      begin
        if FWaitingWriters > 0 then
          ToRelease := mpWriters
        else
        if FWaitingReaders > 0 then
          ToRelease := mpReaders;
      end
      else
      begin
        if FWaitingReaders > 0 then
          ToRelease := mpReaders
        else
        if FWaitingWriters > 0 then
          ToRelease := mpWriters;
      end;
  end;
  case ToRelease of
    mpReaders:
      begin
        FState := FWaitingReaders;
        FWaitingReaders := 0;
        FSemReaders.Release(FState);
      end;
    mpWriters:
      begin
        FState := -1;
        Dec(FWaitingWriters);
        FSemWriters.Release(1);
      end;
    mpEqual:
      // no waiters
  end;
end;

procedure TJclMultiReadExclusiveWrite.RemoveFromThreadList(Index: Integer);
var
  L: Integer;
begin
  // Caller must Lock
  L := Length(FThreads);
  if Index < (L - 1) then
    Move(FThreads[Index + 1], FThreads[Index], SizeOf(TMrewThreadInfo) * (L - Index - 1));
  SetLength(FThreads, L - 1);
end;

//=== { TJclMeteredSection } =================================================

const
  MAX_METSECT_NAMELEN = 128;

constructor TJclMeteredSection.Create(InitialCount, MaxCount: Integer; const Name: string);
begin
  if (MaxCount < 1) or (InitialCount > MaxCount) or (InitialCount < 0) or
    (Length(Name) > MAX_METSECT_NAMELEN) then
    raise EJclMeteredSectionError.CreateRes(@RsMetSectInvalidParameter);
  FMetSect := PMeteredSection(AllocMem(SizeOf(TMeteredSection)));
  if FMetSect <> nil then
  begin
    if not InitMeteredSection(InitialCount, MaxCount, Name, False) then
    begin
      CloseMeteredSection;
      FMetSect := nil;
      raise EJclMeteredSectionError.CreateRes(@RsMetSectInitialize);
    end;
  end;
end;

constructor TJclMeteredSection.Open(const Name: string);
begin
  FMetSect := nil;
  if Name = '' then
    raise EJclMeteredSectionError.CreateRes(@RsMetSectNameEmpty);
  FMetSect := PMeteredSection(AllocMem(SizeOf(TMeteredSection)));
  Assert(FMetSect <> nil);
  if not InitMeteredSection(0, 0, Name, True) then
  begin
    CloseMeteredSection;
    FMetSect := nil;
    raise EJclMeteredSectionError.CreateRes(@RsMetSectInitialize);
  end;
end;

destructor TJclMeteredSection.Destroy;
begin
  CloseMeteredSection;
  inherited Destroy;
end;

procedure TJclMeteredSection.AcquireLock;
begin
  while {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedExchange(FMetSect^.SharedInfo^.SpinLock, 1) <> 0 do
    {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.Sleep(0);
end;

procedure TJclMeteredSection.CloseMeteredSection;
begin
  if FMetSect <> nil then
  begin
    if FMetSect^.SharedInfo <> nil then
      {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.UnmapViewOfFile(FMetSect^.SharedInfo);
    if FMetSect^.FileMap <> 0 then
      {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CloseHandle(FMetSect^.FileMap);
    if FMetSect^.Event <> 0 then
      {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CloseHandle(FMetSect^.Event);
    FreeMem(FMetSect);
  end;
end;

function TJclMeteredSection.CreateMetSectEvent(const Name: string; OpenOnly: Boolean): Boolean;
var
  FullName: string;
begin
  if Name = '' then
    FMetSect^.Event := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CreateEvent(nil, False, False, nil)
  else
  begin
    FullName :=  'JCL_MSECT_EVT_' + Name;
    if OpenOnly then
      FMetSect^.Event := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.OpenEvent(0, False, PChar(FullName))
    else
      FMetSect^.Event := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CreateEvent(nil, False, False, PChar(FullName));
  end;
  Result := FMetSect^.Event <> 0;
end;

function TJclMeteredSection.CreateMetSectFileView(InitialCount, MaxCount: Longint;
  const Name: string; OpenOnly: Boolean): Boolean;
var
  FullName: string;
  LastError: DWORD;
begin
  Result := False;
  if Name = '' then
    FMetSect^.FileMap := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, SizeOf(TMetSectSharedInfo), nil)
  else
  begin
    FullName := 'JCL_MSECT_MMF_' + Name;
    if OpenOnly then
      FMetSect^.FileMap := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.OpenFileMapping(0, False, PChar(FullName))
    else
      FMetSect^.FileMap := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, SizeOf(TMetSectSharedInfo), PChar(FullName));
  end;
  if FMetSect^.FileMap <> 0 then
  begin
    LastError := GetLastError;
    FMetSect^.SharedInfo := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.MapViewOfFile(FMetSect^.FileMap, FILE_MAP_WRITE, 0, 0, 0);
    if FMetSect^.SharedInfo <> nil then
    begin
      if LastError = ERROR_ALREADY_EXISTS then
        while not FMetSect^.SharedInfo^.Initialized do Sleep(0)
      else
      begin
        FMetSect^.SharedInfo^.SpinLock := 0;
        FMetSect^.SharedInfo^.ThreadsWaiting := 0;
        FMetSect^.SharedInfo^.AvailableCount := InitialCount;
        FMetSect^.SharedInfo^.MaximumCount := MaxCount;
        {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedExchange(Integer(FMetSect^.SharedInfo^.Initialized), 1);
      end;
      Result := True;
    end;
  end;
end;

function TJclMeteredSection.Enter(TimeOut: Longword): TJclWaitResult;
begin
  Result := wrSignaled;
  while Result = wrSignaled do
  begin
    AcquireLock;
    try
      if FMetSect^.SharedInfo^.AvailableCount >= 1 then
      begin
        Dec(FMetSect^.SharedInfo^.AvailableCount);
        Result := MapSignalResult(WAIT_OBJECT_0);
        Exit;
      end;
      Inc(FMetSect^.SharedInfo^.ThreadsWaiting);
      {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.ResetEvent(FMetSect^.Event);
    finally
      ReleaseLock;
    end;
    Result := MapSignalResult({$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.WaitForSingleObject(FMetSect^.Event, TimeOut));
  end;
end;

function TJclMeteredSection.InitMeteredSection(InitialCount, MaxCount: Longint;
  const Name: string; OpenOnly: Boolean): Boolean;
begin
  Result := False;
  if CreateMetSectEvent(Name, OpenOnly) then
    Result := CreateMetSectFileView(InitialCount, MaxCount, Name, OpenOnly);
end;

function TJclMeteredSection.Leave(ReleaseCount: Integer; out PrevCount: Integer): Boolean;
var
  Count: Integer;
begin
  Result := False;
  AcquireLock;
  try
    PrevCount := FMetSect^.SharedInfo^.AvailableCount;
    if (ReleaseCount < 0) or
      (FMetSect^.SharedInfo^.AvailableCount + ReleaseCount > FMetSect^.SharedInfo^.MaximumCount) then
    begin
      {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.SetLastError(ERROR_INVALID_PARAMETER);
      Exit;
    end;
    Inc(FMetSect^.SharedInfo^.AvailableCount, ReleaseCount);
    ReleaseCount := Min(ReleaseCount, FMetSect^.SharedInfo^.ThreadsWaiting);
    if FMetSect^.SharedInfo^.ThreadsWaiting > 0 then
    begin
      for Count := 0 to ReleaseCount - 1 do
      begin
        Dec(FMetSect^.SharedInfo^.ThreadsWaiting);
        {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.SetEvent(FMetSect^.Event);
      end;
    end;
  finally
    ReleaseLock;
  end;
  Result := True;
end;

function TJclMeteredSection.Leave(ReleaseCount: Integer): Boolean;
var
  Previous: Longint;
begin
  Result := Leave(ReleaseCount, Previous);
end;

procedure TJclMeteredSection.ReleaseLock;
begin
  {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedExchange(FMetSect^.SharedInfo^.SpinLock, 0);
end;

//=== Debugging ==============================================================

function QueryCriticalSection(CS: TJclCriticalSection; var Info: TRTLCriticalSection): Boolean;
begin
  Result := CS <> nil;
  if Result then
    Info := CS.FCriticalSection;
end;

// Native API functions
// http://undocumented.ntinternals.net/

{ TODO: RTLD version }

type
 TNtQueryProc = function (Handle: THandle; InfoClass: Byte; Info: Pointer;
     Len: Longint; ResLen: PLongint): Longint; stdcall;

var
  _QueryEvent: TNtQueryProc = nil;
  _QueryMutex: TNtQueryProc = nil;
  _QuerySemaphore: TNtQueryProc = nil;
  _QueryTimer: TNtQueryProc = nil;

function CallQueryProc(var P: TNtQueryProc; const Name: string; Handle: THandle;
  Info: Pointer; InfoSize: Longint): Boolean;
var
  NtDll: THandle;
  Status: Longint;
begin
  Result := False;
  if @P = nil then
  begin
    NtDll := GetModuleHandle(PChar('ntdll.dll'));
    if NtDll <> 0 then
      @P := GetProcAddress(NtDll, PChar(Name));
  end;
  if @P <> nil then
  begin
    Status := P(Handle, 0, Info, InfoSize, nil);
    Result := (Status and $80000000) = 0;
  end;
end;

function QueryEvent(Handle: THandle; var Info: TEventInfo): Boolean;
begin
  Result := CallQueryProc(_QueryEvent, 'NtQueryEvent', Handle, @Info, SizeOf(Info));
end;

function QueryMutex(Handle: THandle; var Info: TMutexInfo): Boolean;
begin
  Result := CallQueryProc(_QueryMutex, 'NtQueryMutex', Handle, @Info, SizeOf(Info));
end;

function QuerySemaphore(Handle: THandle; var Info: TSemaphoreCounts): Boolean;
begin
  Result := CallQueryProc(_QuerySemaphore, 'NtQuerySemaphore', Handle, @Info, SizeOf(Info));
end;

function QueryTimer(Handle: THandle; var Info: TTimerInfo): Boolean;
begin
  Result := CallQueryProc(_QueryTimer, 'NtQueryTimer', Handle, @Info, SizeOf(Info));
end;

function ValidateMutexName(const aName: string): string;
const cMutexMaxName = 200;
begin
  if Length(aName) > cMutexMaxName then
    Result := Copy (aName, Length(aName)-cMutexMaxName, cMutexMaxName)
  else
    Result := aName;
  Result := StrReplaceChar(Result, '\', '_');
end;



{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
