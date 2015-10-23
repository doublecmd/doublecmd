{
   Double Commander
   -------------------------------------------------------------------------
   K Desktop Environment integration unit

   Copyright (C) 2014-2015 Alexander Koblov (alexx2000@mail.ru)

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

unit uKde;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMyUnix;

function ShowOpenWithDialog(const FileList: TStringList): Boolean;

var
  UseKde: Boolean = False;

implementation

uses
  uDCUtils, uGlobs, uGlobsPaths, uOSUtils, uTrash, uPython;

var
  PythonScript: String = 'scripts/doublecmd-kde.py';

function ShowOpenWithDialog(const FileList: TStringList): Boolean;
var
  I: Integer;
  Args: String;
begin
  Args := ' openwith';
  for I := 0 to FileList.Count - 1 do
    Args+= ' ' + QuoteStr(FileList[I]);
  Result:= ExecCmdFork(PythonExe + ' ' + PythonScript + Args);
end;

function FileTrash(const FileName: String): Boolean;
begin
  Result:= fpSystemStatus('kioclient move ' + QuoteStr(FileName) + ' trash:/') = 0;
end;

procedure Initialize;
begin
  UseKde:= (DesktopEnv = DE_KDE);
  if UseKde then
  begin
    PythonScript:= gpExePath + PythonScript;
    if ExecutableInSystemPath('kioclient') then FileTrashUtf8:= @FileTrash;
    UseKde:= (fpSystemStatus(PythonExe + ' ' + PythonScript + ' > /dev/null 2>&1') = 0);
  end;
end;

initialization
  RegisterInitialization(@Initialize);

end.

