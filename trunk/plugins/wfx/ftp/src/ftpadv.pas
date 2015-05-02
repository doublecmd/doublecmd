{
   Double commander
   -------------------------------------------------------------------------
   WFX plugin for working with File Transfer Protocol

   Copyright (C) 2009-2015 Alexander Koblov (alexx2000@mail.ru)

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

unit FtpAdv;

{$mode delphi}

interface

uses
  Classes, SysUtils, WfxPlugin, FtpSend;

type

  { EUserAbort }

  EUserAbort = class(Exception);

  { TFTPListRecEx }

  TFTPListRecEx = class(TFTPListRec)
  public
    procedure Assign(Value: TFTPListRec); override;
  end;

  { TFTPListEx }

  TFTPListEx = class(TFTPList)
  public
    procedure Assign(Value: TFTPList); override;
  end;

  { TProgressStream }

  TProgressStream = class(TFileStream)
  public
    DoneSize: Int64;
    FileSize: Int64;
    PluginNumber: Integer;
    ProgressProc: TProgressProc;
    RemoteName, LocalName: PAnsiChar;
  private
    procedure DoProgress(Result: Integer);
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

{ TFTPListRecEx }

procedure TFTPListRecEx.Assign(Value: TFTPListRec);
begin
  inherited Assign(Value);
  Permission:= Value.Permission;
end;

{ TFTPListEx }

procedure TFTPListEx.Assign(Value: TFTPList);
var
  flr: TFTPListRecEx;
  n: integer;
begin
  Clear;
  for n := 0 to Value.Count - 1 do
  begin
    flr := TFTPListRecEx.Create;
    flr.Assign(Value[n]);
    Flist.Add(flr);
  end;
  Lines.Assign(Value.Lines);
  Masks.Assign(Value.Masks);
  UnparsedLines.Assign(Value.UnparsedLines);
end;

{ TProgressStream }

procedure TProgressStream.DoProgress(Result: Integer);
var
  Percent: Int64;
begin
  DoneSize += Result;
  Percent:= DoneSize * 100 div FileSize;
  if ProgressProc(PluginNumber, LocalName, RemoteName, Percent) = 1 then
    raise EUserAbort.Create(EmptyStr);
end;

function TProgressStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:= inherited Read(Buffer, Count);
  if FileSize > 0 then DoProgress(Result);
end;

function TProgressStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:= inherited Write(Buffer, Count);
  if FileSize > 0 then DoProgress(Result);
end;

end.

