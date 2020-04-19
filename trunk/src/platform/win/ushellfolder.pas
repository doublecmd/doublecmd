unit uShellFolder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, ShlObj, ActiveX, ComObj;

function GetDisplayName(AFolder: IShellFolder; PIDL: PItemIDList;
                        Flags: DWORD): String;

implementation

uses
  DCConvertEncoding, uShlObjAdditional;

function StrRetToString(PIDL: PItemIDList; StrRet: TStrRet): String;
var
  S: array[0..MAX_PATH] of WideChar;
begin
  if StrRetToBufW(@StrRet, PIDL, S, MAX_PATH) <> S_OK then
    Result:= EmptyStr
  else
    Result:= UTF8Encode(UnicodeString(S));
end;

function GetDisplayName(AFolder: IShellFolder; PIDL: PItemIDList;
                        Flags: DWORD): String;
var
  StrRet: TStrRet;
begin
  Result:= EmptyStr;
  StrRet:= Default(TStrRet);
  if Succeeded(AFolder.GetDisplayNameOf(PIDL, Flags, StrRet)) then
    Result := StrRetToString(PIDL, StrRet);
  if (Length(Result) = 0) and (Flags <> SHGDN_NORMAL) then
    Result := GetDisplayName(AFolder, PIDL, SHGDN_NORMAL);
end;

end.

