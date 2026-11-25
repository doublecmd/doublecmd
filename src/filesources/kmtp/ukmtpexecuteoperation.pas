{
   Double Commander
   -------------------------------------------------------------------------
   Execute operation for KDE KMTP file source

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

unit uKmtpExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceExecuteOperation,
  uFileSource;

type

  { TKmtpExecuteOperation }

  TKmtpExecuteOperation = class(TFileSourceExecuteOperation)
  public
    procedure MainExecute; override;
  end;

implementation

uses
  fFileProperties, uFile;

{ TKmtpExecuteOperation }

procedure TKmtpExecuteOperation.MainExecute;
var
  AFiles: TFiles;
begin
  if Verb = 'properties' then
  begin
    AFiles := TFiles.Create(CurrentPath);
    try
      AFiles.Add(ExecutableFile.Clone);
      ShowFileProperties(FileSource as IFileSource, AFiles);
    finally
      AFiles.Free;
    end;
    FExecuteOperationResult := fseorSuccess;
    Exit;
  end;

  if not ExecutableFile.IsDirectory then
    FExecuteOperationResult := fseorYourSelf
  else
    FExecuteOperationResult := fseorError;
end;

end.
