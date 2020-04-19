unit uShellFolder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, ShlObj, ActiveX, ComObj;

function GetDisplayName(AFolder: IShellFolder; PIDL: PItemIDList;
                        Flags: DWORD): String;

implementation

uses
  DCConvertEncoding;

function StrRetToString(PIDL: PItemIDList; StrRet: TStrRet; Free: Boolean = False): String;
var
  P: PWideChar;
  S: UnicodeString;
begin
  case StrRet.uType of
    STRRET_CSTR:
      Result := CeSysToUtf8(StrRet.cStr);
    STRRET_WSTR:
      begin
        if (StrRet.pOleStr = nil) then
          Result := EmptyStr
        else begin
          Result := UTF8Encode(UnicodeString(StrRet.pOleStr));
          if Free then CoTaskMemFree(StrRet.pOleStr);
        end;
      end;
    STRRET_OFFSET:
      begin
        P := PWideChar(@PIDL^.mkid.abID[StrRet.uOffset - SizeOf(PIDL^.mkid.cb)]);
        SetString(S, P, PIDL^.mkid.cb - StrRet.uOffset);
        Result:= UTF8Encode(S);
      end;
  end;
end;

function GetDisplayName(AFolder: IShellFolder; PIDL: PItemIDList;
                        Flags: DWORD): String;
var
  StrRet: TStrRet;
begin
  Result:= EmptyStr;
  StrRet:= Default(TStrRet);
  if Succeeded(AFolder.GetDisplayNameOf(PIDL, Flags, StrRet)) then
    Result := StrRetToString(PIDL, StrRet, True);
  if (Length(Result) = 0) and (Flags <> SHGDN_NORMAL) then
    Result := GetDisplayName(AFolder, PIDL, SHGDN_NORMAL);
end;

end.

