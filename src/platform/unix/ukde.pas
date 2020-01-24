{
   Double Commander
   -------------------------------------------------------------------------
   K Desktop Environment integration unit

   Copyright (C) 2014-2020 Alexander Koblov (alexx2000@mail.ru)

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

unit uKde;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMyUnix;

function KioOpen(const URL: String): Boolean;

var
  HasKdeOpen: Boolean = False;

implementation

uses
  uDCUtils, uGlobs, uOSUtils, uTrash;

var
  KdeVersion: String;
  KdeOpen: String = 'kioclient';

function KioOpen(const URL: String): Boolean;
begin
  Result:= ExecCmdFork(KdeOpen + ' exec ' + QuoteStr(URL));
end;

function FileTrash(const FileName: String): Boolean;
begin
  Result:= fpSystemStatus(KdeOpen + ' --noninteractive move ' +
                          QuoteStr(FileName) + ' trash:/') = 0;
end;

procedure Initialize;
begin
  if (DesktopEnv = DE_KDE) then
  begin
    KdeVersion:= GetEnvironmentVariable('KDE_SESSION_VERSION');
    if KdeVersion = '5' then KdeOpen:= 'kioclient5';
    HasKdeOpen:= ExecutableInSystemPath(KdeOpen);
    if HasKdeOpen then FileTrashUtf8:= @FileTrash;
  end;
end;

initialization
  RegisterInitialization(@Initialize);

end.

