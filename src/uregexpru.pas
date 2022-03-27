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
    procedure SetExpression(const AValue: String);
    function GetMatchLen(Idx : integer): PtrInt;
    function GetMatchPos(Idx : integer): PtrInt;
  public
    destructor Destroy; override;
    class function Available: Boolean;
    class function AvailableNew: Boolean;
    function Exec(AOffset: UIntPtr): Boolean;
    procedure SetInputString(AInputString : PAnsiChar; ALength : UIntPtr);
    function ReplaceAll(const Replacement: AnsiString; out Output: AnsiString): Boolean;
  public
    property Expression : String read FExpression write SetExpression;
    property MatchPos [Idx : integer] : PtrInt read GetMatchPos;
    property MatchLen [Idx : integer] : PtrInt read GetMatchLen;
  end;

function ExecRegExpr(const ARegExpr, AInputStr: String): Boolean;

implementation

uses
  DynLibs, DCOSUtils, uDebug;

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

  PCRE2_SUBSTITUTE_GLOBAL          = $00000100;
//PCRE2_SUBSTITUTE_EXTENDED        = $00000200;
  PCRE2_SUBSTITUTE_UNSET_EMPTY     = $00000400;
  PCRE2_SUBSTITUTE_UNKNOWN_UNSET   = $00000800;
  PCRE2_SUBSTITUTE_OVERFLOW_LENGTH = $00001000;

  PCRE2_ERROR_NOMEMORY = -48;

var
  pcre2_compile: function(pattern: PAnsiChar; length: csize_t; options: cuint32; errorcode: pcint; erroroffset: pcsize_t; ccontext: Pointer): Pointer; cdecl;
  pcre2_code_free: procedure(code: Pointer); cdecl;
  pcre2_get_error_message: function(errorcode: cint; buffer: PAnsiChar; bufflen: csize_t): cint; cdecl;
  pcre2_match: function(code: Pointer; subject: PAnsiChar; length: csize_t; startoffset: csize_t; options: cuint32; match_data: Pointer; mcontext: Pointer): cint; cdecl;
  pcre2_get_ovector_pointer: function(match_data: Pointer): pcsize_t; cdecl;
  pcre2_match_data_create_from_pattern: function(code: Pointer; gcontext: Pointer): Pointer; cdecl;
  pcre2_match_data_free: procedure(match_data: Pointer); cdecl;
  pcre2_config: function(what: cuint32; where: pointer): cint; cdecl;
  pcre2_substitute: function(code: Pointer; subject: PAnsiChar; length: csize_t; startoffset: csize_t;
    options: cuint32; match_data: Pointer; mcontext: Pointer;
    replacement: PAnsiChar; rlength: csize_t;
    outputbuffer: PAnsiChar; var outlength: csize_t): cint; cdecl;


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

procedure TRegExprU.SetExpression(const AValue: String);
var
  Message: String;
  error: PAnsiChar;
  errornumber: cint;
  erroroffset: cint;
  len: cint;
begin
  FExpression:= AValue;

  if pcre_new then
  begin
    FCode := pcre2_compile(PAnsiChar(AValue), Length(AValue), PCRE2_UTF, @errornumber, @erroroffset, nil);
    if Assigned(FCode) then
      FMatch := pcre2_match_data_create_from_pattern(FCode, nil)
    else begin
      SetLength(Message, MAX_PATH + 1);
      len := pcre2_get_error_message(errornumber, PAnsiChar(Message), MAX_PATH);
      if len < 0 then len := Length(PAnsiChar(Message)); // PCRE2_ERROR_NOMEMORY
      SetLength(Message, len);
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
      Result := UIntPtr(FVector[Idx * 2]) + 1
    else
      Result := UIntPtr(FOvector[Idx * 2]) + 1;
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

class function TRegExprU.AvailableNew: Boolean;
begin
  Result:= (hLib <> NilHandle) and pcre_new;
end;

function TRegExprU.Exec(AOffset: UIntPtr): Boolean;
begin
  Dec(AOffset);
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

function TRegExprU.ReplaceAll(const Replacement: AnsiString; out Output: AnsiString): Boolean;
var
  outlength: csize_t;
  options: cuint32;
  res: cint;
begin
  if not pcre_new then
  begin
    Output := '';
    Exit(False);
  end;

  if FInputLength = 0 then
  begin
    Output := '';
    Exit(True);
  end;

  options := PCRE2_SUBSTITUTE_OVERFLOW_LENGTH or PCRE2_SUBSTITUTE_UNKNOWN_UNSET or PCRE2_SUBSTITUTE_UNSET_EMPTY;
  //options := options or PCRE2_SUBSTITUTE_EXTENDED;
  options := options or PCRE2_SUBSTITUTE_GLOBAL;

  outlength := FInputLength * 2 + 1; // + space for #0
  if outlength < 2048 then outlength := 2048;
  SetLength(Output, outlength - 1);

  res := pcre2_substitute(FCode, FInput, FInputLength, 0, options, FMatch, nil,
    PAnsiChar(Replacement), Length(Replacement), PAnsiChar(Output), outlength);
  if res >= 0 then // if res = 0 then nothing found
    SetLength(Output, outlength)
  else if res = PCRE2_ERROR_NOMEMORY then
  begin
    SetLength(Output, outlength - 1);
    res := pcre2_substitute(FCode, FInput, FInputLength, 0, options, FMatch, nil,
      PAnsiChar(Replacement), Length(Replacement), PAnsiChar(Output), outlength);
  end;
  Result := res >= 0;
end;

function ExecRegExpr(const ARegExpr, AInputStr: String): Boolean;
var
  r: TRegExprU;
begin
  r := TRegExprU.Create;
  try
    r.Expression := ARegExpr;
    r.SetInputString(PChar(AInputStr), Length(AInputStr));
    Result := r.Exec(1);
  finally
    r.Free;
  end;
end;

procedure Initialize;
var
  Where: IntPtr;
begin
  hLib:= LoadLibrary(libpcre2);
  if (hLib <> NilHandle) then
  begin
    pcre_new:= True;
    try
      @pcre2_config:= SafeGetProcAddress(hLib, 'pcre2_config_8');
      if (pcre2_config(PCRE2_CONFIG_UNICODE, @Where) <> 0) or (Where = 0) then
        raise Exception.Create('pcre2_config(PCRE2_CONFIG_UNICODE)');
      @pcre2_compile:= SafeGetProcAddress(hLib, 'pcre2_compile_8');
      @pcre2_code_free:= SafeGetProcAddress(hLib, 'pcre2_code_free_8');
      @pcre2_get_error_message:= SafeGetProcAddress(hLib, 'pcre2_get_error_message_8');
      @pcre2_match:= SafeGetProcAddress(hLib, 'pcre2_match_8');
      @pcre2_get_ovector_pointer:= SafeGetProcAddress(hLib, 'pcre2_get_ovector_pointer_8');
      @pcre2_match_data_create_from_pattern:= SafeGetProcAddress(hLib, 'pcre2_match_data_create_from_pattern_8');
      @pcre2_match_data_free:= SafeGetProcAddress(hLib, 'pcre2_match_data_free_8');
      @pcre2_substitute:= SafeGetProcAddress(hLib, 'pcre2_substitute_8');
    except
      on E: Exception do
      begin
        FreeLibrary(hLib);
        hLib:= NilHandle;
        DCDebug(E.Message);
      end;
    end;
  end
  else begin
    hLib:= LoadLibrary(libpcre);
{$IF DEFINED(LINUX)}
    // Debian use another library name
    if (hLib = NilHandle) then
      hLib:= LoadLibrary('libpcre.so.3');
{$ENDIF}
    if (hLib <> NilHandle) then
    begin
      pcre_new:= False;
      try
        @pcre_config:= SafeGetProcAddress(hLib, 'pcre_config');
        if (pcre_config(PCRE_CONFIG_UTF8, @Where) <> 0) or (Where = 0) then
          raise Exception.Create('pcre_config(PCRE_CONFIG_UTF8)');
        @pcre_compile:= SafeGetProcAddress(hLib, 'pcre_compile');
        @pcre_exec:= SafeGetProcAddress(hLib, 'pcre_exec');
        @pcre_free:= PPointer(SafeGetProcAddress(hLib, 'pcre_free'))^;
        @pcre_study:= SafeGetProcAddress(hLib, 'pcre_study');
        @pcre_free_study:= SafeGetProcAddress(hLib, 'pcre_free_study');
      except
        on E: Exception do
        begin
          FreeLibrary(hLib);
          hLib:= NilHandle;
          DCDebug(E.Message);
        end;
      end;
    end;
  end;
end;

initialization
  Initialize;

end.

