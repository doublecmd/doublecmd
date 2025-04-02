unit uMacCloudUtil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  WfxPlugin,
  uMacCloudCore,
  uMiniUtil;

type

  ULONGLONG  = qword;

  ULARGE_INTEGER = record
     case byte of
       0: (LowPart : DWORD;
           HighPart : DWORD);
       1: (QuadPart : ULONGLONG);
  end;

  { TProgressCallback }

  TProgressCallback = class( ICloudProgressCallback )
  private
    _serverPath: pwidechar;
    _localPath: pwidechar;
    _totalBytes: Integer;
  public
    constructor Create(
      const serverPath: pwidechar;
      const localPath: pwidechar;
      const totalBytes: Integer );
    function progress( const accumulatedBytes: Integer ): Boolean;
  end;

  TMacCloudUtil = class
  public
    class function fileTimeToDateTime(AFileTime: FILETIME): TDateTime;
    class function dateTimeToFileTime(ADateTimeUTC: TDateTime): FILETIME;
    class procedure cloudFileToWinFindData( cloudFile: TCloudFile; var FindData:tWIN32FINDDATAW );
    class function exceptionToResult( const e: Exception ): Integer;
  end;

var
  macCloudPlugin: TMacCloudPlugin;

implementation

{ TProgressCallback }

constructor TProgressCallback.Create(
  const serverPath: pwidechar;
  const localPath: pwidechar;
  const totalBytes: Integer);
begin
  _serverPath:= serverPath;
  _localPath:= localPath;
  _totalBytes:= totalBytes;
end;

function TProgressCallback.progress(const accumulatedBytes: Integer): Boolean;
var
  percent: Integer;
  ret: Integer;
begin
  if _totalBytes > 0 then
    percent:= accumulatedBytes * 100 div _totalBytes
  else
    percent:= 50;
  ret:= macCloudPlugin.progress( _serverPath, _localPath, percent );
  Result:= (ret = 0);
end;

{ TMacCloudUtil }

class function TMacCloudUtil.FileTimeToDateTime(AFileTime: FILETIME): TDateTime;
var
  li: ULARGE_INTEGER;
const
  OA_ZERO_TICKS = UInt64(94353120000000000);
  TICKS_PER_DAY = UInt64(864000000000);
begin
  // Convert a FILETIME (which is UTC by definition), into a UTC TDateTime.

  // Copy FILETIME into LARGE_INTEGER to allow UInt64 access without alignment faults.
  li.LowPart := AFileTime.dwLowDateTime;
  li.HighPart := AFileTime.dwHighDateTime;
  Result := (Real(li.QuadPart) - OA_ZERO_TICKS) / TICKS_PER_DAY;
end;

class function TMacCloudUtil.DateTimeToFileTime(ADateTimeUTC: TDateTime): FILETIME;
var
  li: ULARGE_INTEGER;
const
  OA_ZERO_TICKS = UInt64(94353120000000000);
  TICKS_PER_DAY = UInt64(864000000000);
begin
  // Convert a UTC TDateTime into a FILETIME (which is UTC by definition).

  li.QuadPart := Round(ADateTimeUtc*TICKS_PER_DAY + OA_ZERO_TICKS);
  Result.dwLowDateTime := li.LowPart;
  Result.dwHighDateTime := li.HighPart;
end;

class function TMacCloudUtil.exceptionToResult( const e: Exception ): Integer;
begin
  TLogUtil.log( msgtype_importanterror, e.ClassName + ': ' + e.Message );

  if e is EAbort then
    Result:= FS_FILE_USERABORT
  else if e is EFileNotFoundException then
    Result:= FS_FILE_NOTFOUND
  else if e is EInOutError then
    Result:= FS_FILE_WRITEERROR
  else
    Result:= FS_FILE_NOTSUPPORTED;
end;

class procedure TMacCloudUtil.cloudFileToWinFindData( cloudFile: TCloudFile; var FindData:tWIN32FINDDATAW );
var
  li: ULARGE_INTEGER;
begin
  FillChar(FindData, SizeOf(FindData), 0);
  if cloudFile.isFolder then
    FindData.dwFileAttributes:= FILE_ATTRIBUTE_DIRECTORY
  else
    FindData.dwFileAttributes:= 0;
  li.QuadPart:= cloudFile.size;
  FindData.nFileSizeLow:= li.LowPart;
  FindData.nFileSizeHigh:= li.HighPart;
  FindData.ftCreationTime:= TMacCloudUtil.DateTimeToFileTime( cloudFile.creationTime );
  FindData.ftLastWriteTime:= TMacCloudUtil.DateTimeToFileTime( cloudFile.modificationTime );
  TStringUtil.stringToWidechars( FindData.cFileName, cloudFile.name, sizeof(FindData.cFileName) );
  cloudFile.Free;
end;

end.

