{
  Double Commander
  -------------------------------------------------------------------------
  SevenZip archiver plugin

  Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit DCJclCompression;

{$mode delphi}

interface

uses
  Classes, SysUtils, SevenZip;

type

  { TSfxSevenzipOutStream }

  TSfxSevenzipOutStream = class(TInterfacedObject, ISequentialOutStream,
    IOutStream, IUnknown)
  private
    FStream: TStream;
    FSfxLength: Int64;
    FSfxModule: String;
  public
    constructor Create(AStream: TStream; const ASfxModule: String);
    destructor Destroy; override;
    // ISequentialOutStream
    function Write(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; stdcall;
    // IOutStream
    function Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT; stdcall;
    function SetSize(NewSize: Int64): HRESULT; stdcall;
  end;

implementation

uses
  ActiveX, JwaWinError, LazUTF8Classes;

{ TSfxSevenzipOutStream }

constructor TSfxSevenzipOutStream.Create(AStream: TStream; const ASfxModule: String);
var
  SfxModule: TFileStreamUTF8;
begin
  inherited Create;

  FStream := AStream;
  FSfxModule := ASfxModule;

  SfxModule:= TFileStreamUTF8.Create(FSfxModule, fmOpenRead or fmShareDenyNone);
  try
    FStream.Seek(0, soBeginning);
    FSfxLength := FStream.CopyFrom(SfxModule, SfxModule.Size);
  finally
    SfxModule.Free;
  end;
end;

destructor TSfxSevenzipOutStream.Destroy;
begin
  FStream.Free;

  inherited Destroy;
end;

function TSfxSevenzipOutStream.Write(Data: Pointer; Size: Cardinal;
  ProcessedSize: PCardinal): HRESULT; stdcall;
var
  Processed: Cardinal;
begin
  if Assigned(FStream) then
  begin
    Result := S_OK;
    Processed := FStream.Write(Data^, Size);
    if Assigned(ProcessedSize) then
      ProcessedSize^ := Processed;
  end
  else
    Result := S_FALSE;
end;

function TSfxSevenzipOutStream.Seek(Offset: Int64; SeekOrigin: Cardinal;
  NewPosition: PInt64): HRESULT; stdcall;
var
  NewPos: Int64;
  NewOffset: Int64;
begin
  if Assigned(FStream) then
  begin
    Result := S_OK;
    if SeekOrigin <> STREAM_SEEK_SET then
      NewOffset := Offset
    else begin
      NewOffset := Offset + FSfxLength;
    end;
    // STREAM_SEEK_SET = 0 = soBeginning
    // STREAM_SEEK_CUR = 1 = soCurrent
    // STREAM_SEEK_END = 2 = soEnd
    NewPos := FStream.Seek(NewOffset, TSeekOrigin(SeekOrigin));
    if NewPos < FSfxLength then Exit(E_INVALIDARG);
    if Assigned(NewPosition) then
      NewPosition^ := NewPos - FSfxLength;
  end
  else
    Result := S_FALSE;
end;

function TSfxSevenzipOutStream.SetSize(NewSize: Int64): HRESULT; stdcall;
begin
  if Assigned(FStream) then
  begin
    Result := S_OK;
    FStream.Size := NewSize + FSfxLength;
  end
  else
    Result := S_FALSE;
end;

end.

