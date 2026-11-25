{
   Double Commander
   -------------------------------------------------------------------------
   KDE KMTP (org.kde.kmtpd5) virtual file source

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

unit uKmtpFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, URIParser, DCStrUtils, uFile, uFileProperty,
  uFileSource, uFileSourceProperty, uRealFileSource, uFileSourceOperation,
  uFileSourceOperationTypes, uKde;

type

  { IKmtpFileSource }

  IKmtpFileSource = interface(IRealFileSource)
    ['{8B7C2D4E-4A91-4B3C-9F1E-2C6D8E4A1B7F}']
    function ResolveTarget(const APath: String;
                           out ADevicePath, AStoragePath, ARelPath: String;
                           out ALevel: Integer): Boolean;
    function GetDeviceName: String;
  end;

  { TKmtpFileSource }

  TKmtpFileSource = class(TRealFileSource, IKmtpFileSource)
  private
    FDeviceName: String;
  protected
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;
  public
    constructor Create; override;
    constructor Create(const URI: TURI); override;

    class function IsSupportedPath(const Path: String): Boolean; override;
    class function CreateFile(const APath: String): TFile; override;
    class function CreateFile(const APath: String; const AInfo: TKmtpFileInfo): TFile;

    function ResolveTarget(const APath: String;
                           out ADevicePath, AStoragePath, ARelPath: String;
                           out ALevel: Integer): Boolean;
    function GetDeviceName: String;

    function GetPathType(sPath: String): TPathType; override;
    function GetFileSystem: String; override;
    function GetProperties: TFileSourceProperties; override;
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetRetrievableFileProperties: TFilePropertiesTypes; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    function FileSystemEntryExists(const Path: String; const Options: TFileSourceExistsOptions): TFileSourceExistsResult; override;
    function CreateDirectory(const Path: String): Boolean; override;
    function GetFreeSpace(Path: String; out FreeSize, TotalSize: Int64): Boolean; override;
    procedure Reload(const PathsToReload: TPathsArray); override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; override;
  end;

implementation

uses
  DCBasicTypes, DCDateTimeUtils, DCFileAttributes,
  uKmtpFileSourceUtil, uKmtpListOperation, uKmtpCopyInOperation,
  uKmtpCopyOutOperation, uKmtpDeleteOperation, uKmtpCreateDirectoryOperation,
  uKmtpExecuteOperation, uKmtpCalcStatisticsOperation, uKmtpSetFilePropertyOperation;

{ TKmtpFileSource }

constructor TKmtpFileSource.Create;
begin
  inherited Create;
  FDeviceName := '';
  FCurrentAddress := 'mtp://';
end;

constructor TKmtpFileSource.Create(const URI: TURI);
begin
  inherited Create(URI);
  FCurrentAddress := FURI.Protocol + '://' + FURI.Host;
  FDeviceName := DecodeMtpHost(FURI.Host);
end;

function TKmtpFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  Result := True;
end;

class function TKmtpFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  // GVfs formats MTP URIs as mtp://[usb:...]/ whereas KMTP uses mtp://<FriendlyName>/
  Result := StrBegins(Path, 'mtp://') and (not StrBegins(Path, 'mtp://[')) and IsKmtpActive;
end;

class function TKmtpFileSource.CreateFile(const APath: String): TFile;
begin
  Result := inherited CreateFile(APath);
  with Result do
  begin
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes;
    SizeProperty := TFileSizeProperty.Create;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
    TypeProperty := TFileTypeProperty.Create;
    CommentProperty := TFileCommentProperty.Create;
  end;
end;

class function TKmtpFileSource.CreateFile(const APath: String; const AInfo: TKmtpFileInfo): TFile;
begin
  Result := CreateFile(APath);
  Result.Name := AInfo.FileName;
  if AInfo.FileType = KMTP_MIME_DIRECTORY then
  begin
    Result.Size := 0;
    Result.Attributes := S_IFDIR or $01ED;
  end
  else
  begin
    Result.Size := Int64(AInfo.FileSize);
    Result.Attributes := S_IFREG or $01A4;
  end;
  if AInfo.ModDate > 0 then
    Result.ModificationTime := UnixFileTimeToDateTime(TUnixFileTime(AInfo.ModDate));
end;

function TKmtpFileSource.GetDeviceName: String;
begin
  Result := FDeviceName;
end;

function TKmtpFileSource.ResolveTarget(const APath: String;
  out ADevicePath, AStoragePath, ARelPath: String; out ALevel: Integer): Boolean;
var
  CleanPath: String;
  Segments: TStringArray;
  Device: TKmtpDeviceInfo;
  Storage: TKmtpStorageInfo;
  DevIdx: Integer;
begin
  Result := False;
  ADevicePath := '';
  AStoragePath := '';
  ARelPath := '';
  ALevel := 0;

  CleanPath := APath;
  if StrBegins(CleanPath, FCurrentAddress) then
    Delete(CleanPath, 1, Length(FCurrentAddress));

  Segments := SplitKmtpSegments(CleanPath);

  if FDeviceName <> '' then
  begin
    if not FindKmtpDeviceByName(FDeviceName, Device) then
      Exit(False);
    ADevicePath := Device.ObjectPath;
    if Length(Segments) = 0 then
    begin
      ALevel := 1;
      Exit(True);
    end;
    if not FindKmtpStorageByDesc(ADevicePath, Segments[0], Storage) then
      Exit(False);
    AStoragePath := Storage.ObjectPath;
    ARelPath := JoinKmtpStoragePath(Segments, 1);
    ALevel := 2;
    Result := True;
  end
  else
  begin
    if Length(Segments) = 0 then
    begin
      ALevel := 0;
      Exit(True);
    end;
    if not FindKmtpDeviceByName(Segments[0], Device) then
      Exit(False);
    ADevicePath := Device.ObjectPath;
    if Length(Segments) = 1 then
    begin
      ALevel := 1;
      Exit(True);
    end;
    DevIdx := 1;
    if not FindKmtpStorageByDesc(ADevicePath, Segments[DevIdx], Storage) then
      Exit(False);
    AStoragePath := Storage.ObjectPath;
    ARelPath := JoinKmtpStoragePath(Segments, DevIdx + 1);
    ALevel := 2;
    Result := True;
  end;
end;

function TKmtpFileSource.GetPathType(sPath: String): TPathType;
begin
  if Pos('://', sPath) > 0 then
    Result := ptAbsolute
  else
    Result := inherited GetPathType(sPath);
end;

function TKmtpFileSource.GetFileSystem: String;
begin
  if FDeviceName <> '' then
    Result := FDeviceName
  else
    Result := 'MTP';
end;

function TKmtpFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspVirtual, fspSynchronizable];
end;

function TKmtpFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := [fpName, fpSize, fpAttributes, fpModificationTime];
end;

function TKmtpFileSource.GetRetrievableFileProperties: TFilePropertiesTypes;
begin
  Result := [fpName, fpSize, fpAttributes, fpModificationTime];
end;

function TKmtpFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoCopyIn, fsoCopyOut, fsoDelete,
             fsoCreateDirectory, fsoExecute, fsoCalcStatistics, fsoSetFileProperty];
end;

function TKmtpFileSource.FileSystemEntryExists(const Path: String;
  const Options: TFileSourceExistsOptions): TFileSourceExistsResult;
var
  DevPath, StorPath, RelPath: String;
  Level: Integer;
  Meta: TKmtpFileInfo;
  Exists: Boolean;
begin
  Result := TFileSourceExistsResult.notExist;
  if Options = [] then
    Exit;

  if not ResolveTarget(Path, DevPath, StorPath, RelPath, Level) then
    Exit;

  if (Level < 2) or (RelPath = '') then
  begin
    if (Options = [TFileSourceExistsOption.needFile]) then
      Exit(TFileSourceExistsResult.notExist)
    else
      Exit(TFileSourceExistsResult.exists);
  end;

  if not KmtpGetFileMetadata(StorPath, RelPath, Meta) then
    Exit;

  if Options = [TFileSourceExistsOption.needFile] then
    Exists := (Meta.FileType <> KMTP_MIME_DIRECTORY)
  else if Options = [TFileSourceExistsOption.needDir] then
    Exists := (Meta.FileType = KMTP_MIME_DIRECTORY)
  else
    Exists := True;

  if Exists then
    Result := TFileSourceExistsResult.exists;
end;

function TKmtpFileSource.CreateDirectory(const Path: String): Boolean;
var
  CleanPath: String;
  Segments: TStringArray;
  Device: TKmtpDeviceInfo;
  Storage: TKmtpStorageInfo;
  CurRel, DevName, StorDesc: String;
  StartIdx, I: Integer;
  Meta: TKmtpFileInfo;
begin
  Result := False;
  CleanPath := Path;
  if StrBegins(CleanPath, FCurrentAddress) then
    Delete(CleanPath, 1, Length(FCurrentAddress));

  Segments := SplitKmtpSegments(CleanPath);
  if FDeviceName <> '' then
  begin
    if Length(Segments) < 2 then
      Exit(False);
    DevName := FDeviceName;
    StorDesc := Segments[0];
    StartIdx := 1;
  end
  else
  begin
    if Length(Segments) < 3 then
      Exit(False);
    DevName := Segments[0];
    StorDesc := Segments[1];
    StartIdx := 2;
  end;

  if not FindKmtpDeviceByName(DevName, Device) then
    Exit(False);
  if not FindKmtpStorageByDesc(Device.ObjectPath, StorDesc, Storage) then
    Exit(False);

  CurRel := '';
  for I := StartIdx to High(Segments) do
  begin
    CurRel := CurRel + '/' + Segments[I];
    if KmtpGetFileMetadata(Storage.ObjectPath, CurRel, Meta) then
    begin
      if Meta.FileType <> KMTP_MIME_DIRECTORY then
        Exit(False);
    end
    else
    begin
      if not KmtpCreateFolder(Storage.ObjectPath, CurRel) then
        Exit(False);
    end;
  end;
  Result := True;
end;

function TKmtpFileSource.GetFreeSpace(Path: String; out FreeSize, TotalSize: Int64): Boolean;
var
  DevPath, StorPath, RelPath: String;
  Level, I: Integer;
  FreeVal, TotalVal: UInt64;
  Storages: TKmtpStorageArray;
begin
  Result := False;
  FreeSize := 0;
  TotalSize := 0;
  if not ResolveTarget(Path, DevPath, StorPath, RelPath, Level) then
    Exit;

  if (Level >= 2) and (StorPath <> '') then
  begin
    Result := KmtpGetStorageSpace(StorPath, FreeVal, TotalVal);
    FreeSize := Int64(FreeVal);
    TotalSize := Int64(TotalVal);
  end
  else if (Level = 1) and (DevPath <> '') then
  begin
    Storages := KmtpListStorages(DevPath);
    for I := 0 to High(Storages) do
    begin
      Inc(FreeSize, Int64(Storages[I].FreeSpace));
      Inc(TotalSize, Int64(Storages[I].MaxCapacity));
    end;
    Result := (TotalSize > 0);
  end;
end;

procedure TKmtpFileSource.Reload(const PathsToReload: TPathsArray);
var
  Index: Integer;
  PathList: TPathsArray;
begin
  PathList := nil;
  SetLength(PathList, Length(PathsToReload));
  for Index := Low(PathsToReload) to High(PathsToReload) do
    PathList[Index] := StringReplace(PathsToReload[Index], FCurrentAddress, '', []);
  inherited Reload(PathList);
end;

function TKmtpFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result := TKmtpListOperation.Create(Self, TargetPath);
end;

function TKmtpFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
begin
  Result := TKmtpCopyInOperation.Create(SourceFileSource, Self, SourceFiles, TargetPath);
end;

function TKmtpFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
begin
  Result := TKmtpCopyOutOperation.Create(Self, TargetFileSource, SourceFiles, TargetPath);
end;

function TKmtpFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
begin
  Result := TKmtpDeleteOperation.Create(Self, FilesToDelete);
end;

function TKmtpFileSource.CreateCreateDirectoryOperation(BasePath: String;
  DirectoryPath: String): TFileSourceOperation;
begin
  Result := TKmtpCreateDirectoryOperation.Create(Self, BasePath, DirectoryPath);
end;

function TKmtpFileSource.CreateExecuteOperation(var ExecutableFile: TFile;
  BasePath, Verb: String): TFileSourceOperation;
begin
  Result := TKmtpExecuteOperation.Create(Self, ExecutableFile, BasePath, Verb);
end;

function TKmtpFileSource.CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation;
begin
  Result := TKmtpCalcStatisticsOperation.Create(Self, theFiles);
end;

function TKmtpFileSource.CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
  var theNewProperties: TFileProperties): TFileSourceOperation;
begin
  Result := TKmtpSetFilePropertyOperation.Create(Self, theTargetFiles, theNewProperties);
end;

end.
