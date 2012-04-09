{
   Replaces Free Pascal exception handler

   Fixes bug:
        http://doublecmd.sourceforge.net/mantisbt/view.php?id=50

   Uses workaround from:
        http://bugs.freepascal.org/view.php?id=17280
        http://bugs.freepascal.org/view.php?id=12974
}

unit uExceptionHandlerFix;

{$mode delphi}

interface

implementation

uses
  Windows, SysUtils;


type
  M128A = record
    Low : QWord;
    High : Int64;
  end;

  PContext = ^TContext;
  TContext = record
    P1Home : QWord;
    P2Home : QWord;
    P3Home : QWord;
    P4Home : QWord;
    P5Home : QWord;
    P6Home : QWord;
    ContextFlags : DWord;
    MxCsr : DWord;
    SegCs : word;
    SegDs : word;
    SegEs : word;
    SegFs : word;
    SegGs : word;
    SegSs : word;
    EFlags : DWord;
    Dr0 : QWord;
    Dr1 : QWord;
    Dr2 : QWord;
    Dr3 : QWord;
    Dr6 : QWord;
    Dr7 : QWord;
    Rax : QWord;
    Rcx : QWord;
    Rdx : QWord;
    Rbx : QWord;
    Rsp : QWord;
    Rbp : QWord;
    Rsi : QWord;
    Rdi : QWord;
    R8 : QWord;
    R9 : QWord;
    R10 : QWord;
    R11 : QWord;
    R12 : QWord;
    R13 : QWord;
    R14 : QWord;
    R15 : QWord;
    Rip : QWord;
    Header : array[0..1] of M128A;
    Legacy : array[0..7] of M128A;
    Xmm0 : M128A;
    Xmm1 : M128A;
    Xmm2 : M128A;
    Xmm3 : M128A;
    Xmm4 : M128A;
    Xmm5 : M128A;
    Xmm6 : M128A;
    Xmm7 : M128A;
    Xmm8 : M128A;
    Xmm9 : M128A;
    Xmm10 : M128A;
    Xmm11 : M128A;
    Xmm12 : M128A;
    Xmm13 : M128A;
    Xmm14 : M128A;
    Xmm15 : M128A;
    VectorRegister : array[0..25] of M128A;
    VectorControl : QWord;
    DebugControl : QWord;
    LastBranchToRip : QWord;
    LastBranchFromRip : QWord;
    LastExceptionToRip : QWord;
    LastExceptionFromRip : QWord;
  end;

type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode   : DWord;
    ExceptionFlags  : DWord;
    ExceptionRecord : PExceptionRecord;
    ExceptionAddress : Pointer;
    NumberParameters : DWord;
    ExceptionInformation : array[0..EXCEPTION_MAXIMUM_PARAMETERS-1] of Pointer;
  end;

  PExceptionPointers = ^TExceptionPointers;
  TExceptionPointers = packed record
    ExceptionRecord   : PExceptionRecord;
    ContextRecord     : PContext;
  end;

type
  PVectoredExceptionNode = ^TVectoredExceptionNode;
  TVectoredExceptionNode = record
    m_pNextNode: PVectoredExceptionNode;
    m_pPreviousNode: PVectoredExceptionNode;
    m_Unknown: Pointer;
    m_pfnVectoredHandler: Pointer;
  end;

function AddVectoredExceptionHandler(FirstHandler: ULONG; VectoredHandler: Pointer): Pointer; stdcall;
  external 'kernel32.dll' name 'AddVectoredExceptionHandler';
function RemoveVectoredExceptionHandler(VectoredHandlerHandle: Pointer): ULONG; stdcall;
  external 'kernel32.dll' name 'RemoveVectoredExceptionHandler';  
function GetModuleHandleEx(dwFlags: DWORD; lpModuleName: Pointer; var hModule: THandle): BOOL; stdcall;
  external 'kernel32.dll' name 'GetModuleHandleExA';
function RtlEncodePointer(pfnVectoredHandler: Pointer): Pointer; stdcall;
  external 'ntdll' name 'RtlEncodePointer';

const
  GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT = 2;
  GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS       = 4;

// These entries are linked from FPC's RTL.
// If the RTL changes, the entries should be changed accordingly.
function syswin64_x86_64_exception_handler(excep : pointer) : Longint; external name 'SYSTEM_SYSWIN64_X86_64_EXCEPTION_HANDLER$PEXCEPTIONPOINTERS$$LONGINT';
var _fltused: int64 external name '_fltused';

// Test if the exception address resides in our program.
function CheckOurModule(p: Pointer): boolean;
var
  ModuleWithException: THandle;
  OurModule: THandle;
  Flags: DWORD;
begin
  Result := False;

  { It's necessary to keep refcount intact. }
  Flags := GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS or GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT;

  with PExceptionPointers(p)^.ExceptionRecord^ do
    Result := GetModuleHandleEx(Flags, ExceptionAddress, ModuleWithException) and
      GetModuleHandleEx(Flags, @CheckOurModule, OurModule) and (ModuleWithException = OurModule);
end;

function ProcessException(p: Pointer): longint; stdcall;
var
  _SS: PCardinal;
  Saved: TExceptionRecord;
begin
  Result := EXCEPTION_CONTINUE_SEARCH;

  if CheckOurModule(p) then
  begin
    Saved := PExceptionPointers(p)^.ExceptionRecord^;

    with PExceptionPointers(p)^.ExceptionRecord^ do
    begin
      // Dirty hack - in system.pp, private variable _SS is just after public _fltused. This might change in the future.
      _SS := @_fltused;
      inc(PBYTE(_SS), sizeof(int64));
      _SS^ := PExceptionPointers(p)^.ContextRecord^.SegSs;

      // Trying to unwind the stack in FPC's way - by walking the linked list of exception handers.
      Result := syswin64_x86_64_exception_handler(p);
    end;

    if Result <> 0 then
    begin
      // The FPC's unwind failed for some reason.
      // Restoring the Exception record, so the program's exception handlers can try to recover from the exception.
      PExceptionPointers(p)^.ExceptionRecord^ := Saved;

      // You can insert some kind of logging etc here.
      // ...
    end;
  end;
end;

var
  VectoredExceptionHandler: PVectoredExceptionNode = nil;

procedure InstallExceptionHandler;
var
  HandlerAddress: Pointer;
  Node: PVectoredExceptionNode;
begin
  // Get Free Pascal exception handler encoded address
  HandlerAddress := RtlEncodePointer(@syswin64_x86_64_exception_handler);
  VectoredExceptionHandler := AddVectoredExceptionHandler(1, @ProcessException);
  // Find Free Pascal exception handler and remove it
  Node:= VectoredExceptionHandler^.m_pNextNode;
  repeat
    if (Node^.m_pfnVectoredHandler = HandlerAddress) then
    begin
      RemoveVectoredExceptionHandler(Node);
      Break;
    end;
    Node := Node^.m_pNextNode;
  until (Node = nil);
end;

procedure UninstallExceptionHandler;
begin
  if Assigned(VectoredExceptionHandler) then
  begin
    RemoveVectoredExceptionHandler(VectoredExceptionHandler);
    VectoredExceptionHandler := nil;
  end;
end;

initialization
  InstallExceptionHandler;

finalization
  UninstallExceptionHandler;

end.
