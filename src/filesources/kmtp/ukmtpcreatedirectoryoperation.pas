{
   Double Commander
   -------------------------------------------------------------------------
   Create directory operation for KDE KMTP file source

   Copyright (C) 2026 Alexander Koblov (alexx2000@mail.ru)

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

unit uKmtpCreateDirectoryOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCreateDirectoryOperation,
  uFileSource;

type

  { TKmtpCreateDirectoryOperation }

  TKmtpCreateDirectoryOperation = class(TFileSourceCreateDirectoryOperation)
  public
    procedure MainExecute; override;
  end;

implementation

uses
  uGlobs, uLog, uLng;

{ TKmtpCreateDirectoryOperation }

procedure TKmtpCreateDirectoryOperation.MainExecute;
begin
  if (FileSource as IFileSource).CreateDirectory(AbsolutePath) then
  begin
    if (log_vfs_op in gLogOptions) and (log_success in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogSuccess + rsMsgLogMkDir, [AbsolutePath]), lmtSuccess);
  end
  else
  begin
    if (log_vfs_op in gLogOptions) and (log_errors in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogError + rsMsgLogMkDir, [AbsolutePath]), lmtError);
  end;
end;

end.
