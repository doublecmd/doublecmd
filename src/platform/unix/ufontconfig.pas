unit uFontConfig;

{$mode delphi}
{$packrecords c}

interface

uses
  Classes, SysUtils, CTypes;

type
  TFcBool = cint;
  PFcChar8 = PAnsiChar;

  PFcConfig = ^TFcConfig;
  TFcConfig = record end;

  PFcPattern = ^TFcPattern;
  TFcPattern = record end;

  TFcMatchKind = (FcMatchPattern, FcMatchFont, FcMatchScan);

  PFcResult = ^TFcResult;
  TFcResult = (FcResultMatch, FcResultNoMatch, FcResultTypeMismatch, FcResultNoId, FcResultOutOfMemory);

var
  FcStrFree: procedure(s: PFcChar8); cdecl;
  FcPatternDestroy: procedure(p: PFcPattern); cdecl;
  FcNameParse: function(name: PFcChar8): PFcPattern; cdecl;
  FcDefaultSubstitute: procedure(pattern: PFcPattern); cdecl;
  FcPatternFormat: function(pat: PFcPattern; format: PFcChar8): PFcChar8; cdecl;
  FcFontMatch: function(config: PFcConfig; p: PFcPattern; result: PFcResult): PFcPattern; cdecl;
  FcConfigSubstitute: function(config: PFcConfig; p: PFcPattern; kind: TFcMatchKind): TFcBool; cdecl;

function LoadFontConfigLib(const ALibName: String): Boolean;
procedure UnLoadFontConfigLib;

implementation

uses
  DCOSUtils, uDebug;

var
  hLib: TLibHandle;

function LoadFontConfigLib(const ALibName: String): Boolean;
begin
  hLib:= SafeLoadLibrary(ALibName);
  Result:= (hLib <> NilHandle);
  if Result then
  try
    FcStrFree:= SafeGetProcAddress(hLib, 'FcStrFree');
    FcNameParse:= SafeGetProcAddress(hLib, 'FcNameParse');
    FcFontMatch:= SafeGetProcAddress(hLib, 'FcFontMatch');
    FcPatternFormat:= SafeGetProcAddress(hLib, 'FcPatternFormat');
    FcPatternDestroy:= SafeGetProcAddress(hLib, 'FcPatternDestroy');
    FcConfigSubstitute:= SafeGetProcAddress(hLib, 'FcConfigSubstitute');
    FcDefaultSubstitute:= SafeGetProcAddress(hLib, 'FcDefaultSubstitute');
  except
    on E: Exception do
    begin
      Result:= False;
      DCDebug(E.Message);
      UnLoadFontConfigLib;
    end;
  end;
end;

procedure UnLoadFontConfigLib;
begin
  if (hLib <> NilHandle) then FreeLibrary(hLib);
end;

end.

