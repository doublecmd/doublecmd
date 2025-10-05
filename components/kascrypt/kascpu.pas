{
   KAScrypt Cryptographic Component Library
   -------------------------------------------------------------------------
   Detect hardware features

   Copyright (C) 2018-2025 Alexander Koblov (alexx2000@mail.ru)

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

unit KAScpu;

interface

{$IF DEFINED(CPUAARCH64)}

function SHA256Support: Boolean;

{$ELSEIF DEFINED(CPUX86_64)}

function BMI2Support: LongBool;
function SSSE3Support: LongBool;
function SSE41Support: LongBool;

{$ENDIF}

implementation

{$IF DEFINED(CPUAARCH64)}

{$IF DEFINED(WIN64)}
uses
  Windows;
{$ELSEIF DEFINED(UNIX)}
uses
  InitC, CTypes;
{$ENDIF}

const
  HWCAP_SHA2 = 64;

var
  hwcaps: UInt64 = 0;

{$IF DEFINED(LINUX) OR DEFINED(ANDROID)}

const
  AT_HWCAP = 16;

function getauxval(type_: culong): culong; cdecl; external clib;

procedure Initialize;
begin
  hwcaps:= getauxval(AT_HWCAP);
end;
{$ELSEIF DEFINED(DARWIN)}

function sysctlbyname(const name: pansichar; oldp: pointer; oldlenp: pcsize_t;
                      const newp: pointer; newlen: csize_t): cint; cdecl; external clib;

procedure Initialize;
var
  value: cint;
  size: csize_t;
begin
  value:= 0;
  size:= SizeOf(value);
  if sysctlbyname('hw.optional.arm.FEAT_SHA256', @value, @size, nil, 0) = 0 then
  begin
    if (size > 0) and (value <> 0) then
    begin
      hwcaps:= hwcaps or HWCAP_SHA2;
    end;
  end;
end;

{$ELSEIF DEFINED(WIN64)}
const
  PF_ARM_V8_CRYPTO_INSTRUCTIONS_AVAILABLE = 30;

function IsProcessorFeaturePresent(ProcessorFeature: DWORD): BOOL; stdcall; external kernel32;

procedure Initialize;
begin
  if IsProcessorFeaturePresent(PF_ARM_V8_CRYPTO_INSTRUCTIONS_AVAILABLE) then
    hwcaps:= hwcaps or HWCAP_SHA2;
end;

{$ENDIF}

function SHA256Support: Boolean;
begin
  Result:= (hwcaps and HWCAP_SHA2) <> 0;
end;

initialization
  Initialize;

{$ELSEIF DEFINED(CPUX86_64)}

function BMI2Support: LongBool; assembler; nostackframe;
asm
  pushq %rbx
  movl $7,%eax
  movl $0,%ecx
  cpuid
  andl $256,%ebx
  movl %ebx,%eax
  popq %rbx
end;

function SSSE3Support: LongBool; assembler; nostackframe;
asm
  pushq %rbx
  movl $1,%eax
  cpuid
  andl $512,%ecx
  movl %ecx,%eax
  popq %rbx
end;

function SSE41Support: LongBool; assembler; nostackframe;
asm
  pushq %rbx
  movl $1,%eax
  cpuid
  andl $0x80000,%ecx
  movl %ecx,%eax
  popq %rbx
end;

{$ENDIF}

end.
