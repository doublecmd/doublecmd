{
   Double Commander
   -------------------------------------------------------------------------
   PCRE - Perl Compatible Regular Expressions

   Copyright (C) 2019 Alexander Koblov (alexx2000@mail.ru)

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

unit uRegExprU;

{$mode delphi}

interface

uses
  Classes, SysUtils, CTypes;

type

  { TRegExprU }

  TRegExprU = class
  private
    FCode: Pointer;
    FMatch: Pointer;
    FInput: PAnsiChar;
    FVector: pcsize_t;
    FVectorLength: cint;
    FExpression: String;
    FInputLength: UIntPtr;
    FOvector: array[Byte] of cint;
    procedure SetExpression(AValue: String);
    function GetMatchLen(Idx : integer): PtrInt;
    function GetMatchPos(Idx : integer): PtrInt;
  public
    destructor Destroy; override;
    class function Available: Boolean;
    function Exec(AOffset: UIntPtr): Boolean;
    procedure SetInputString(AInputString : PAnsiChar; ALength : UIntPtr);
  public
    property Expression : String read FExpression write SetExpression;
    property MatchPos [Idx : integer] : PtrInt read GetMatchPos;
    property MatchLen [Idx : integer] : PtrInt read GetMatchLen;
  end;

implementation

uses
  DynLibs;

// PCRE 2
const
  libpcre2 = {$IF DEFINED(MSWINDOWS)}
              'libpcre2-8.dll'
              {$ELSEIF DEFINED(DARWIN)}
              'libpcre2-8.dylib'
              {$ELSEIF DEFINED(UNIX)}
              'libpcre2-8.so.0'
              {$IFEND};

const
  PCRE2_CONFIG_UNICODE = 9;
  PCRE2_UTF            = $00080000;

var
  pcre2_compile: function(pattern: PAnsiChar; length: csize_t; options: cuint32; errorcode: pcint; erroroffset: pcsize_t; ccontext: Pointer): Pointer; cdecl;
  pcre2_code_free: procedure(code: Pointer); cdecl;
  pcre2_get_error_message: function(errorcode: cint; buffer: PAnsiChar; bufflen: csize_t): cint; cdecl;
  pcre2_match: function(code: Pointer; subject: PAnsiChar; length: csize_t; startoffset: csize_t; options: cuint32; match_data: Pointer; mcontext: Pointer): cint; cdecl;
  pcre2_get_ovector_pointer: function(match_data: Pointer): pcsize_t; cdecl;
  pcre2_match_data_create_from_pattern: function(code: Pointer; gcontext: Pointer): Pointer; cdecl;
  pcre2_match_data_free: procedure(match_data: Pointer); cdecl;
  pcre2_config: function(what: cuint32; where: pointer): cint; cdecl;

// PCRE 1
const
  libpcre  = {$IF DEFINED(MSWINDOWS)}
              'libpcre.dll'
              {$ELSEIF DEFINED(DARWIN)}
              'libpcre.dylib'
              {$ELSEIF DEFINED(UNIX)}
              'libpcre.so.1'
              {$IFEND};

const
  PCRE_CONFIG_UTF8     = 0;
  PCRE_UTF8            = $00000800;

var
  pcre_compile: function(pattern: PAnsiChar; options: cint; errptr: PPAnsiChar; erroffset: pcint; tableptr: PAnsiChar): pointer; cdecl;
  pcre_exec: function(code: pointer; extra: Pointer; subject: PAnsiChar; length: cint; startoffset: cint; options: cint; ovector: pcint; ovecsize: cint): cint; cdecl;
  pcre_free: procedure(code: pointer); cdecl;
  pcre_study: function(code: Pointer; options: cint; errptr: PPAnsiChar): Pointer; cdecl;
  pcre_free_study: procedure(extra: Pointer); cdecl;
  pcre_config: function(what: cint; where: pointer): cint; cdecl;

var
  pcre_new: Boolean;
  hLib: TLibHandle = NilHandle;

{ TRegExprU }

procedure TRegExprU.SetExpression(AValue: String);
var
  Message: String;
  error: PAnsiChar;
  errornumber: cint;
  erroroffset: cint;
begin
  FExpression:= AValue;

  if pcre_new then
  begin
    FCode := pcre2_compile(PAnsiChar(AValue), Length(AValue), PCRE2_UTF, @errornumber, @erroroffset, nil);
    if Assigned(FCode) then
      FMatch := pcre2_match_data_create_from_pattern(FCode, nil)
    else begin
      SetLength(Message, MAX_PATH + 1);
      pcre2_get_error_message(errornumber, PAnsiChar(Message), MAX_PATH);
      raise Exception.Create(Message);
    end;
  end
  else begin
    FCode := pcre_compile(PAnsiChar(AValue), PCRE_UTF8, @error, @erroroffset, nil);
    if Assigned(FCode) then
      FMatch:= pcre_study(FCode, 0, @error)
    else
      raise Exception.Create(StrPas(error));
  end;
end;

function TRegExprU.GetMatchLen(Idx : integer): PtrInt;
begin
  if (Idx < FVectorLength) then
  begin
    if pcre_new then
      Result := UIntPtr(FVector[Idx * 2 + 1]) - UIntPtr(FVector[Idx * 2])
    else
      Result := UIntPtr(FOvector[Idx * 2 + 1]) - UIntPtr(FOvector[Idx * 2]);
  end
  else
    Result:= 0;
end;

function TRegExprU.GetMatchPos(Idx : integer): PtrInt;
begin
  if (Idx < FVectorLength) then
  begin
    if pcre_new then
      Result := UIntPtr(FVector[Idx * 2])
    else
      Result := UIntPtr(FOvector[Idx * 2]);
  end
  else
    Result:= 0;
end;

destructor TRegExprU.Destroy;
begin
  if Assigned(FCode) then
  begin
    if pcre_new then
      pcre2_code_free(FCode)
    else
      pcre_free(FCode);
  end;
  if Assigned(FMatch) then
  begin
    if pcre_new then
      pcre2_match_data_free(FMatch)
    else
      pcre_free_study(FMatch);
  end;
  inherited Destroy;
end;

class function TRegExprU.Available: Boolean;
begin
  Result:= (hLib <> NilHandle);
end;

function TRegExprU.Exec(AOffset: UIntPtr): Boolean;
begin
  if pcre_new then
  begin
    FVectorLength:= pcre2_match(FCode, FInput, FInputLength,
                                AOffset, 0, FMatch, nil);

    Result:= (FVectorLength >= 0);

    if Result then
      FVector := pcre2_get_ovector_pointer(FMatch);
  end
  else begin
      FVectorLength := pcre_exec(FCode, FMatch, FInput, FInputLength,
                                 AOffset, 0, FOvector, SizeOf(FOvector));
      // The output vector wasn't big enough
      if (FVectorLength = 0) then FVectorLength:= SizeOf(FOvector) div 3;

      Result:= (FVectorLength >= 0);
  end;
end;

procedure TRegExprU.SetInputString(AInputString: PAnsiChar; ALength: UIntPtr);
begin
  FInput:= AInputString;
  FInputLength:= ALength;
end;

procedure Initialize;
var
  Where: IntPtr;
begin
  hLib:= LoadLibrary(libpcre2);
  if (hLib <> NilHandle) then
  begin
    pcre_new:= True;
    @pcre2_config:= GetProcAddress(hLib, 'pcre2_config_8');
    if (pcre2_config(PCRE2_CONFIG_UNICODE, @Where) = 0) and (Where <> 0) then
    begin
      @pcre2_compile:= GetProcAddress(hLib, 'pcre2_compile_8');
      @pcre2_code_free:= GetProcAddress(hLib, 'pcre2_code_free_8');
      @pcre2_get_error_message:= GetProcAddress(hLib, 'pcre2_get_error_message_8');
      @pcre2_match:= GetProcAddress(hLib, 'pcre2_match_8');
      @pcre2_get_ovector_pointer:= GetProcAddress(hLib, 'pcre2_get_ovector_pointer_8');
      @pcre2_match_data_create_from_pattern:= GetProcAddress(hLib, 'pcre2_match_data_create_from_pattern_8');
      @pcre2_match_data_free:= GetProcAddress(hLib, 'pcre2_match_data_free_8');
    end
    else begin
      FreeLibrary(hLib);
      hLib:= NilHandle;
    end;
  end
  else begin
    hLib:= LoadLibrary(libpcre);
    if (hLib <> NilHandle) then
    begin
      pcre_new:= False;
      @pcre_config:= GetProcAddress(hLib, 'pcre_config');
      if (pcre_config(PCRE_CONFIG_UTF8, @Where) = 0) and (Where <> 0) then
      begin
        @pcre_compile:= GetProcAddress(hLib, 'pcre_compile');
        @pcre_exec:= GetProcAddress(hLib, 'pcre_exec');
        @pcre_free:= GetProcAddress(hLib, 'pcre_free');
        @pcre_study:= GetProcAddress(hLib, 'pcre_study');
        @pcre_free_study:= GetProcAddress(hLib, 'pcre_free_study');
      end
      else begin
        FreeLibrary(hLib);
        hLib:= NilHandle;
      end;
    end;
  end;
end;

initialization
  Initialize;

end.

