unit uMacCloudUtil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  WfxPlugin,
  uDropBoxClient,
  uMiniUtil;

type

  ULONGLONG  = qword;

  ULARGE_INTEGER = record
     case byte of
       0: (LowPart : DWORD;
           HighPart : DWORD);
       1: (QuadPart : ULONGLONG);
  end;

  { TMacCloudPlugin }

  TMacCloudPlugin = class
  strict private
    _pluginNumber: Integer;
    _progressProc: TProgressProcW;
    _logProc: TLogProcW;
    procedure pluginLogProc( const MsgType: Integer; const message: String );
  public
    constructor Create( const pluginNumber: Integer; const progressProc: TProgressProcW; const logProc: TLogProcW );
    function progress( const sourceName: pwidechar; const targetName: pwidechar; const percentDone: Integer ): Integer;
  public
    property pluginNumber: Integer read _pluginNumber;
  end;

  { TProgressCallback }

  TProgressCallback = class( IDropBoxProgressCallback )
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
    class function FileTimeToDateTime(AFileTime: FILETIME): TDateTime;
    class function DateTimeToFileTime(ADateTimeUTC: TDateTime): FILETIME;
    class procedure DbFileToWinFindData( dbFile: TDropBoxFile; var FindData:tWIN32FINDDATAW );
    class function exceptionToResult( const e: Exception ): Integer;
  end;

var
  macCloudPlugin: TMacCloudPlugin;

implementation

{ TMacCloudPlugin }

constructor TMacCloudPlugin.Create( const pluginNumber: Integer; const progressProc: TProgressProcW; const logProc: TLogProcW );
begin
  _pluginNumber:= pluginNumber;
  _progressProc:= progressProc;
  _logProc:= logProc;
  TLogUtil.setLogProc( @self.pluginLogProc );
end;

function TMacCloudPlugin.progress(const sourceName: pwidechar;
  const targetName: pwidechar; const percentDone: Integer): Integer;
begin
  Result:= _progressProc( _pluginNumber, sourceName, targetName, percentDone );
end;

procedure TMacCloudPlugin.pluginLogProc(const MsgType: Integer; const message: String);
var
  buffer: Array [0..1024*10-1] of widechar;
begin
  TStringUtil.stringToWidechars( buffer, 'MacCloud: ' + message, sizeof(buffer) );
  _logProc( _pluginNumber, MsgType, buffer );
end;

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

class procedure TMacCloudUtil.DbFileToWinFindData( dbFile: TDropBoxFile; var FindData:tWIN32FINDDATAW );
var
  li: ULARGE_INTEGER;
begin
  FillChar(FindData, SizeOf(FindData), 0);
  if dbFile.isFolder then
    FindData.dwFileAttributes:= FILE_ATTRIBUTE_DIRECTORY
  else
    FindData.dwFileAttributes:= 0;
  li.QuadPart:= dbFile.size;
  FindData.nFileSizeLow:= li.LowPart;
  FindData.nFileSizeHigh:= li.HighPart;
  FindData.ftCreationTime:= TMacCloudUtil.DateTimeToFileTime( dbFile.clientModified );
  FindData.ftLastWriteTime:= TMacCloudUtil.DateTimeToFileTime( dbFile.serverModified );
  TStringUtil.stringToWidechars( FindData.cFileName, dbFile.name, sizeof(FindData.cFileName) );
  dbFile.Free;
end;

end.

