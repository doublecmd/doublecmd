{
    Double Commander
    -------------------------------------------------------------------------
    Help manager

    Copyright (C) 2008-2021 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit dmHelpManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, LazHelpHTML;

type

  { TdmHelpManager }

  TdmHelpManager = class(TDataModule)
    HTMLBrowserHelpViewer: THTMLBrowserHelpViewer;
    HTMLHelpDatabase: THTMLHelpDatabase;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

procedure ShowHelpForKeywordWithAnchor(const Keyword: String);

var
  dmHelpMgr: TdmHelpManager;

implementation

{$R *.lfm}

uses
  {$IFDEF MSWINDOWS}
  LCLIntf, uOSUtils, uFileProcs,
  {$ELSE}
  HelpIntfs,
  {$ENDIF}
  uGlobsPaths, uGlobs, DCStrUtils, DCOSUtils, StrUtils, DCClassesUtf8;

{$IF DEFINED(MSWINDOWS)}
procedure OpenURLWithAnchor(URL: String);
var
  hFile:THandle;
  TempoFilenameWithTheLink: String;
begin
  TempoFilenameWithTheLink:= GetTempFolderDeletableAtTheEnd + 'FileWithALink.html';
  hFile:= mbFileCreate(TempoFilenameWithTheLink);
  if hFile <> feInvalidHandle then
  try
    FileWriteLn(hFile,'<html>');
    FileWriteLn(hFile,'<head><meta http-equiv="refresh" content="0;url=' + URL + '" /></head>');
    // In case browser doesn't support auto-redirection, give a link to user.
    FileWriteLn(hFile,'<body><center><a href="' + URL + '">Click here</a> for help</center></body>');
    FileWriteLn(hFile,'</html>');
  finally
    FileClose(hFile);
  end;
  if mbFileExists(TempoFilenameWithTheLink) then OpenURL(TempoFilenameWithTheLink);
end;
{$ENDIF}

procedure ShowHelpForKeywordWithAnchor(const Keyword: String);
{$IF DEFINED(MSWINDOWS)}
begin
  OpenURLWithAnchor(dmHelpMgr.HTMLHelpDatabase.BaseURL + Keyword);
end;
{$ELSE}
begin
  ShowHelpOrErrorForKeyword('', Keyword);
end;
{$ENDIF}

{ TdmHelpManager }

procedure TdmHelpManager.DataModuleCreate(Sender: TObject);
{$IFDEF MSWindows}
var
  ABrowser, AParams: String;
{$ENDIF}
var
  ATranslations: TStringList;
begin
  if NumCountChars('.', gPOFileName) < 2 then
    gHelpLang:= 'en'
  else begin
    gHelpLang:= ExtractDelimited(2, gPOFileName, ['.']);
    if not mbDirectoryExists(gpExePath + 'doc' + PathDelim + gHelpLang) then
    begin
      ATranslations:= TStringListEx.Create;
      try
        ATranslations.LoadFromFile(gpExePath + 'doublecmd.help');
        if ATranslations.IndexOf(gHelpLang) < 0 then gHelpLang:= 'en';
      except
        gHelpLang:= 'en';
      end;
      ATranslations.Free;
    end;
  end;

  if mbDirectoryExists(gpExePath + 'doc' + PathDelim + gHelpLang) then
    HTMLHelpDatabase.BaseURL:= 'file://' + gpExePath + 'doc' + PathDelim + gHelpLang
  else begin
    HTMLHelpDatabase.BaseURL:= 'https://doublecmd.github.io/doc/' + gHelpLang;
  end;

  HTMLHelpDatabase.KeywordPrefix:= '/';

  {$IFDEF MSWindows}
  // Lazarus issue #0021637.
  if FindDefaultBrowser(ABrowser, AParams) then
  begin
    HTMLBrowserHelpViewer.BrowserPath := ABrowser;
    HTMLBrowserHelpViewer.BrowserParams := StringReplace(AParams, '%s', '"%s"', [rfReplaceAll]);
  end;
  {$ENDIF}
end;

end.

