{
   Double Commander
   -------------------------------------------------------------------------
   Helper utilities and tree builder for KDE KMTP file source

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

unit uKmtpFileSourceUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFile, uFileSourceOperation, uFileSourceTreeBuilder,
  uKmtpFileSource, uKde;

type

  { TKmtpTreeBuilder }

  TKmtpTreeBuilder = class(TFileSourceTreeBuilder)
  private
    FKmtpFileSource: IKmtpFileSource;
  protected
    procedure AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode); override;
    procedure AddFilesInDirectory(srcPath: String; CurrentNode: TFileTreeNode); override;
  public
    constructor Create(AKmtpFileSource: IKmtpFileSource;
                       AskQuestionFunction: TAskQuestionFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction);
  end;

function SplitKmtpSegments(const APath: String): TStringArray;
function JoinKmtpStoragePath(const Segments: TStringArray; StartIndex: Integer): String;
function FindKmtpDeviceByName(const AName: String; out ADevice: TKmtpDeviceInfo): Boolean;
function FindKmtpStorageByDesc(const ADevicePath, ADesc: String; out AStorage: TKmtpStorageInfo): Boolean;

implementation

uses
  BaseUnix;

function SplitKmtpSegments(const APath: String): TStringArray;
var
  I, StartPos, Len: Integer;
  Count: Integer = 0;
  Part: String;
begin
  Result := nil;
  Len := Length(APath);
  I := 1;
  while I <= Len do
  begin
    while (I <= Len) and (APath[I] = '/') do
      Inc(I);
    if I > Len then
      Break;
    StartPos := I;
    while (I <= Len) and (APath[I] <> '/') do
      Inc(I);
    Part := Copy(APath, StartPos, I - StartPos);
    if (Part <> '') and (Part <> '.') then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := Part;
      Inc(Count);
    end;
  end;
end;

function JoinKmtpStoragePath(const Segments: TStringArray; StartIndex: Integer): String;
var
  I: Integer;
begin
  Result := '';
  for I := StartIndex to High(Segments) do
    Result := Result + '/' + Segments[I];
end;

function FindKmtpDeviceByName(const AName: String; out ADevice: TKmtpDeviceInfo): Boolean;
var
  Devices: TKmtpDeviceArray;
  I: Integer;
begin
  Result := False;
  ADevice := Default(TKmtpDeviceInfo);
  Devices := KmtpListDevices;
  for I := 0 to High(Devices) do
  begin
    if SameText(Devices[I].FriendlyName, AName) then
    begin
      ADevice := Devices[I];
      Exit(True);
    end;
  end;
end;

function FindKmtpStorageByDesc(const ADevicePath, ADesc: String; out AStorage: TKmtpStorageInfo): Boolean;
var
  Storages: TKmtpStorageArray;
  I: Integer;
begin
  Result := False;
  AStorage := Default(TKmtpStorageInfo);
  Storages := KmtpListStorages(ADevicePath);
  for I := 0 to High(Storages) do
  begin
    if SameText(Storages[I].Description, ADesc) then
    begin
      AStorage := Storages[I];
      Exit(True);
    end;
  end;
end;

{ TKmtpTreeBuilder }

constructor TKmtpTreeBuilder.Create(AKmtpFileSource: IKmtpFileSource;
  AskQuestionFunction: TAskQuestionFunction;
  CheckOperationStateFunction: TCheckOperationStateFunction);
begin
  inherited Create(AskQuestionFunction, CheckOperationStateFunction);
  FKmtpFileSource := AKmtpFileSource;
end;

procedure TKmtpTreeBuilder.AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode);
begin
  AddFile(aFile, CurrentNode);
end;

procedure TKmtpTreeBuilder.AddFilesInDirectory(srcPath: String; CurrentNode: TFileTreeNode);
var
  DevPath, StorPath, RelPath: String;
  Level, I, ExistsCode: Integer;
  Storages: TKmtpStorageArray;
  KmtpFiles: TKmtpFileArray;
  AFile: TFile;
begin
  if not FKmtpFileSource.ResolveTarget(srcPath, DevPath, StorPath, RelPath, Level) then
    Exit;

  if Level = 1 then
  begin
    Storages := KmtpListStorages(DevPath);
    for I := 0 to High(Storages) do
    begin
      CheckOperationState;
      AFile := TKmtpFileSource.CreateFile(srcPath);
      AFile.Name := Storages[I].Description;
      AFile.Size := 0;
      AFile.Attributes := S_IFDIR or $01ED;
      AddItem(AFile, CurrentNode);
    end;
  end
  else if Level >= 2 then
  begin
    if KmtpGetFilesAndFolders(StorPath, RelPath, KmtpFiles, ExistsCode) and (ExistsCode = 0) then
    begin
      for I := 0 to High(KmtpFiles) do
      begin
        CheckOperationState;
        if (KmtpFiles[I].FileName = '') or (KmtpFiles[I].FileName = '.') or (KmtpFiles[I].FileName = '..') then
          Continue;
        AFile := TKmtpFileSource.CreateFile(srcPath, KmtpFiles[I]);
        AddItem(AFile, CurrentNode);
      end;
    end;
  end;
end;

end.
