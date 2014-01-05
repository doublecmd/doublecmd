{
   Double Commander
   -------------------------------------------------------------------------
   Simple key file implementation based on GKeyFile

   Copyright (C) 2014 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uKeyFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

const

  { Constants for handling freedesktop.org Desktop files }

  DESKTOP_GROUP = 'Desktop Entry';

  DESKTOP_KEY_CATEGORIES = 'Categories';
  DESKTOP_KEY_COMMENT = 'Comment';
  DESKTOP_KEY_EXEC = 'Exec';
  DESKTOP_KEY_ICON = 'Icon';
  DESKTOP_KEY_NAME = 'Name';
  DESKTOP_KEY_NO_DISPLAY = 'NoDisplay';
  DESKTOP_KEY_TERMINAL = 'Terminal';

type
  TGKeyFile = record end;
  PGKeyFile = ^TGKeyFile;

type

  { TKeyFile }

  TKeyFile = class(TCustomIniFile)
  private
    FGKeyFile: PGKeyFile;
  public
    constructor Create(const AFileName: String; AEscapeLineFeeds : Boolean = False); override;
    destructor Destroy; override;
  public
    function SectionExists(const Section: String): Boolean; override;
    function ReadBool(const Section, Ident: String; Default: Boolean): Boolean; override;
    function ReadString(const Section, Ident, Default: String): String; override;
    function ReadLocaleString(const Section, Ident, Default: String): String; virtual;
  end;

implementation

uses
  RtlConsts, GLib2;

type
  TGKeyFileFlags = (
    G_KEY_FILE_NONE              = 0,
    G_KEY_FILE_KEEP_COMMENTS     = 1 shl 0,
    G_KEY_FILE_KEEP_TRANSLATIONS = 1 shl 1
  );

function  g_key_file_new(): PGKeyFile; cdecl; external;
procedure g_key_file_free(key_file: PGKeyFile); cdecl; external;
function  g_key_file_load_from_file(key_file: PGKeyFile; const file_name: Pgchar;
                                    flags: TGKeyFileFlags; error: PPGError): gboolean; cdecl; external;
function  g_key_file_has_group(key_file: PGKeyFile; const group_name: Pgchar): gboolean; cdecl; external;
function  g_key_file_get_string(key_file: PGKeyFile; const group_name: Pgchar;
                                const key: Pgchar; error: PPGError): Pgchar; cdecl; external;
function  g_key_file_get_locale_string(key_file: PGKeyFile; const group_name: Pgchar;
                                       const key: Pgchar; const locale: Pgchar; error: PPGError): Pgchar; cdecl; external;
function  g_key_file_get_boolean(key_file: PGKeyFile; const group_name: Pgchar;
                                 const key: Pgchar; error: PPGError): gboolean; cdecl; external;

{ TKeyFile }

constructor TKeyFile.Create(const AFileName: String; AEscapeLineFeeds: Boolean);
begin
  FGKeyFile:= g_key_file_new();
  if not g_key_file_load_from_file(FGKeyFile, Pgchar(AFileName), G_KEY_FILE_NONE, nil) then
    raise EFOpenError.CreateFmt(SFOpenError, [AFileName]);
  inherited Create(AFileName, AEscapeLineFeeds);
  CaseSensitive:= True;
end;

destructor TKeyFile.Destroy;
begin
  inherited Destroy;
  g_key_file_free(FGKeyFile);
end;

function TKeyFile.SectionExists(const Section: String): Boolean;
begin
  Result:= g_key_file_has_group(FGKeyFile, Pgchar(Section));
end;

function TKeyFile.ReadBool(const Section, Ident: String; Default: Boolean): Boolean;
var
  AError: PGError = nil;
begin
  Result:= g_key_file_get_boolean(FGKeyFile, Pgchar(Section), Pgchar(Ident), @AError);
  if (AError <> nil) then
  begin
    Result:= Default;
    g_error_free(AError);
  end;
end;

function TKeyFile.ReadString(const Section, Ident, Default: String): String;
var
  AValue: Pgchar;
begin
  AValue:= g_key_file_get_string(FGKeyFile, Pgchar(Section), Pgchar(Ident), nil);
  if (AValue = nil) then
    Result:= Default
  else begin
    Result:= StrPas(AValue);
    g_free(AValue);
  end;
end;

function TKeyFile.ReadLocaleString(const Section, Ident, Default: String): String;
var
  AValue: Pgchar;
begin
  AValue:= g_key_file_get_locale_string(FGKeyFile, Pgchar(Section), Pgchar(Ident), nil, nil);
  if (AValue = nil) then
    Result:= Default
  else begin
    Result:= StrPas(AValue);
    g_free(AValue);
  end;
end;

end.

