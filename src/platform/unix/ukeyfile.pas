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
  Classes, SysUtils, IniFiles, DCBasicTypes, uGLib2;

const

  { Constants for handling freedesktop.org Desktop files }

  DESKTOP_GROUP = 'Desktop Entry';

  DESKTOP_KEY_CATEGORIES = 'Categories';
  DESKTOP_KEY_COMMENT = 'Comment';
  DESKTOP_KEY_EXEC = 'Exec';
  DESKTOP_KEY_ICON = 'Icon';
  DESKTOP_KEY_NAME = 'Name';
  DESKTOP_KEY_TYPE = 'Type';
  DESKTOP_KEY_TRY_EXEC = 'TryExec';
  DESKTOP_KEY_MIME_TYPE = 'MimeType';
  DESKTOP_KEY_NO_DISPLAY = 'NoDisplay';
  DESKTOP_KEY_TERMINAL = 'Terminal';
  DESKTOP_KEY_KDE_BUG = 'Path[$e]';

type

  { TKeyFile }

  TKeyFile = class(TCustomIniFile)
  private
    FGKeyFile: PGKeyFile;
  protected
    function LoadFromFile(const AFileName: String; out AMessage: String): Boolean; inline;
  public
    constructor Create(const AFileName: String; AEscapeLineFeeds : Boolean = False); override;
    destructor Destroy; override;
  public
    function SectionExists(const Section: String): Boolean; override;
    function ReadBool(const Section, Ident: String; Default: Boolean): Boolean; override;
    function ReadString(const Section, Ident, Default: String): String; override;
    function ReadLocaleString(const Section, Ident, Default: String): String; virtual;
    function ReadStringList(const Section, Ident: String): TDynamicStringArray; virtual;
  protected
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;
  end;

implementation

uses
  RtlConsts, DCStrUtils;

{ TKeyFile }

function TKeyFile.LoadFromFile(const AFileName: String; out AMessage: String): Boolean;
var
  AChar: Pgchar;
  ALength: gsize;
  AContents: Pgchar;
  AError: PGError = nil;

  function FormatMessage: String;
  begin
    if Assigned(AError) then
    begin
      Result:= StrPas(AError^.message);
      g_error_free(AError);
      AError:= nil;
    end;
  end;

begin
  Result:= g_key_file_load_from_file(FGKeyFile, Pgchar(AFileName), G_KEY_FILE_NONE, @AError);
  if not Result then
  begin
    AMessage:= FormatMessage;
    // KDE menu editor adds invalid "Path[$e]" key. GKeyFile cannot parse
    // such desktop files. We comment it before parsing to avoid this problem.
    if Pos(DESKTOP_KEY_KDE_BUG, AMessage) > 0 then
    begin
      Result:= g_file_get_contents(Pgchar(AFileName), @AContents, @ALength, @AError);
      if not Result then
        AMessage:= FormatMessage
      else try
        AChar:= g_strstr_len(AContents, ALength, DESKTOP_KEY_KDE_BUG);
        if Assigned(AChar) then AChar^:= '#';
        Result:= g_key_file_load_from_data(FGKeyFile, AContents, ALength, G_KEY_FILE_NONE, @AError);
        if not Result then AMessage:= FormatMessage;
      finally
        g_free(AContents);
      end;
    end;
  end;
end;

constructor TKeyFile.Create(const AFileName: String; AEscapeLineFeeds: Boolean);
var
  AMessage: String;
begin
  FGKeyFile:= g_key_file_new();
  if not LoadFromFile(AFileName, AMessage) then
    raise EFOpenError.CreateFmt(SFOpenErrorEx, [AFileName, AMessage]);
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
{$OPTIMIZATION OFF}
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
{$OPTIMIZATION DEFAULT}

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

function TKeyFile.ReadStringList(const Section, Ident: String): TDynamicStringArray;
var
  AValue: PPgchar;
  AIndex, ALength: gsize;
begin
  AValue:= g_key_file_get_string_list(FGKeyFile, Pgchar(Section), Pgchar(Ident), @ALength, nil);
  if Assigned(AValue) then
  begin
    SetLength(Result, ALength);
    for AIndex:= 0 to Pred(ALength) do
    begin
      Result[AIndex]:= StrPas(AValue[AIndex]);
    end;
    g_strfreev(AValue);
  end;
end;

procedure TKeyFile.WriteString(const Section, Ident, Value: String);
begin

end;

procedure TKeyFile.ReadSection(const Section: string; Strings: TStrings);
begin

end;

procedure TKeyFile.ReadSections(Strings: TStrings);
begin

end;

procedure TKeyFile.ReadSectionValues(const Section: string; Strings: TStrings);
begin

end;

procedure TKeyFile.EraseSection(const Section: string);
begin

end;

procedure TKeyFile.DeleteKey(const Section, Ident: String);
begin

end;

procedure TKeyFile.UpdateFile;
begin

end;

end.

