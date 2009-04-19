{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains some functions for open files in associated applications.

    Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uShellExecute;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTypes;

procedure ReplaceExtCommand(var sCmd:String; pfr:PFileRecItem; ActiveDir: String);
function ProcessExtCommand(sCmd:String; ActiveDir: String): Boolean;
function ShellExecuteEx(sCmd, sFileName, sActiveDir: String): Boolean;

implementation

uses
  Process, UTF8Process, StrUtils, uDCUtils, uShowForm, uGlobs, uOSUtils;

procedure ReplaceExtCommand(var sCmd:String; pfr:PFileRecItem; ActiveDir: String);
var
  sDir: String;
  iStart,
  iCount: Integer;
  Process: TProcessUTF8;
begin
  with pfr^ do
  begin
    sDir:= IfThen(sPath<>'', sPath, ActiveDir);
    sCmd:= GetCmdDirFromEnvVar(sCmd);
    sCmd:= StringReplace(sCmd,'%f',QuoteStr(ExtractFileName(sName)),[rfReplaceAll]);
    sCmd:= StringReplace(sCmd,'%d',QuoteStr(sDir),[rfReplaceAll]);
    sCmd:= StringReplace(sCmd,'%p',QuoteStr(sDir+ExtractFileName(sName)),[rfReplaceAll]);
    sCmd:= Trim(sCmd);
    // get output from command between '<?' and '?>'
    if Pos('<?', sCmd) <> 0 then
      begin
        iStart:= Pos('<?', sCmd) + 2;
        iCount:= Pos('?>', sCmd) - iStart;
        sDir:= GetTempFolder + ExtractFileName(sName) + '.tmp';
        Process:= TProcessUTF8.Create(nil);
        Process.CommandLine:= Format(fmtRunInShell, [GetShell, Copy(sCmd, iStart, iCount) + ' > ' + sDir]);
        Process.Options:= [poNoConsole, poWaitOnExit];
        Process.Execute;
        Process.Free;
        sCmd:= Copy(sCmd, 1, iStart-3) + sDir;
//        DebugLn('"'+sCmd+'"');
      end;
  end;
end;

function ProcessExtCommand(sCmd:String; ActiveDir: String): Boolean;
var
  bTerm: Boolean;
begin
  Result:= False;
  bTerm:= False;
  if Pos('{!SHELL}', sCmd) > 0 then
  begin
    sCmd:= StringReplace(sCmd,'{!SHELL}','',[rfReplaceAll]);
    bTerm:= True;
  end;
  if Pos('{!EDITOR}',sCmd) > 0 then
  begin
    sCmd:= Trim(StringReplace(sCmd,'{!EDITOR}','',[rfReplaceAll]));
    uShowForm.ShowEditorByGlob(RemoveQuotation(sCmd));
    Result:= True;
    Exit;
  end;
  if Pos('{!VIEWER}',sCmd) > 0 then
  begin
    sCmd:= Trim(StringReplace(sCmd,'{!VIEWER}','',[rfReplaceAll]));
    uShowForm.ShowViewerByGlob(RemoveQuotation(sCmd));
    Result:= True;
    Exit;
  end;
  mbSetCurrentDir(ActiveDir);
  Result:= ExecCmdFork(sCmd, bTerm, gRunInTerm);
end;

function ShellExecuteEx(sCmd, sFileName, sActiveDir: String): Boolean;
var
  FileRecItem: TFileRecItem;
  sCommand: String;
begin
  Result:= False;
  FillChar(FileRecItem, SizeOf(FileRecItem), #0);
  with FileRecItem do
  begin
    sName:= ExtractFileName(sFileName);
    sPath:= ExtractFilePath(sFileName);
    sExt:= ExtractFileExt(sFileName);
    sCommand:= gExts.GetExtActionCmd(FileRecItem, sCmd);
  end;
  if sCommand <> '' then
    begin
      ReplaceExtCommand(sCommand, @FileRecItem, sActiveDir);
      Result:= ProcessExtCommand(sCommand, sActiveDir);
    end;
  if not Result then
    begin
      mbSetCurrentDir(sActiveDir);
      Result:= ShellExecute(sFileName);
    end;
end;

end.

