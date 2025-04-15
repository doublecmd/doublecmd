unit uMacCloudUtil;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  WfxPlugin,
  CocoaAll, uMiniCocoa,
  uCloudDriver, uMacCloudCore,
  uMiniUtil;

type

  ULONGLONG  = qword;

  ULARGE_INTEGER = record
     case byte of
       0: (LowPart : DWORD;
           HighPart : DWORD);
       1: (QuadPart : ULONGLONG);
  end;

  { TCloudProgressCallback }

  TCloudProgressCallback = class( ICloudProgressCallback )
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

  { TCloudPathParser }

  TCloudPathParser = class
  private
    _connectionName: String;
    _driverPath: String;
  private
    function getConnection: TCloudConnection;
    function getDriver: TCloudDriver;
  public
    constructor Create( const path: String );
    property connection: TCloudConnection read getConnection;
    property driver: TCloudDriver read getDriver;
    property connectionName: String read _connectionName;
    property driverPath: String read _driverPath;
  end;

  { TMacCloudUtil }

  TMacCloudUtil = class
  public
    class function fileTimeToDateTime(AFileTime: FILETIME): TDateTime;
    class function dateTimeToFileTime(ADateTimeUTC: TDateTime): FILETIME;
    class procedure cloudFileToWinFindData( cloudFile: TCloudFile; var FindData:tWIN32FINDDATAW );
    class function exceptionToResult( const e: Exception ): Integer;
    class function driverMainIconPath( const driver: TCloudDriver ): String; overload;
    class function driverMainIconPath( const driver: TCloudDriverClass ): String; overload;
    class function driverMainIcon( const driver: TCloudDriver ): NSImage; overload;
    class function driverMainIcon( const driver: TCloudDriverClass ): NSImage; overload;
  end;

implementation

{ TCloudProgressCallback }

constructor TCloudProgressCallback.Create(
  const serverPath: pwidechar;
  const localPath: pwidechar;
  const totalBytes: Integer);
begin
  _serverPath:= serverPath;
  _localPath:= localPath;
  _totalBytes:= totalBytes;
end;

function TCloudProgressCallback.progress(const accumulatedBytes: Integer): Boolean;
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

{ TCloudPathParser }

function TCloudPathParser.getConnection: TCloudConnection;
begin
  Result:= cloudConnectionManager.get( _connectionName );
end;

function TCloudPathParser.getDriver: TCloudDriver;
begin
  Result:= self.connection.driver;
end;

constructor TCloudPathParser.Create(const path: String);
var
  i: Integer;
begin
  i:= path.IndexOf( PathDelim , 1 );
  if i < 0 then begin
    _connectionName:= path.Substring( 1 );
  end else begin
    _connectionName:= path.Substring( 1, i-1 );
    _driverPath:= path.Substring( i );
  end;
end;

{ TMacCloudUtil }

class function TMacCloudUtil.fileTimeToDateTime(AFileTime: FILETIME): TDateTime;
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

class function TMacCloudUtil.dateTimeToFileTime(ADateTimeUTC: TDateTime): FILETIME;
var
  li: ULARGE_INTEGER;
const
  OA_ZERO_TICKS = UInt64(94353120000000000);
  TICKS_PER_DAY = UInt64(864000000000);
begin
  // Convert a UTC TDateTime into a FILETIME (which is UTC by definition).
  if ADateTimeUTC = 0 then begin
    Result.dwLowDateTime:= $FFFFFFFE;
    Result.dwHighDateTime:= $FFFFFFFF;
  end else begin
    li.QuadPart:= Round(ADateTimeUtc*TICKS_PER_DAY + OA_ZERO_TICKS);
    Result.dwLowDateTime:= li.LowPart;
    Result.dwHighDateTime:= li.HighPart;
  end;
end;

class function TMacCloudUtil.exceptionToResult( const e: Exception ): Integer;
begin
  TLogUtil.logError( e.ClassName + ': ' + e.Message );

  if e is EAbort then
    Result:= FS_FILE_USERABORT
  else if e is EFileNotFoundException then
    Result:= FS_FILE_NOTFOUND
  else if e is EInOutError then
    Result:= FS_FILE_WRITEERROR
  else
    Result:= FS_FILE_NOTSUPPORTED;
end;

class function TMacCloudUtil.driverMainIconPath( const driver: TCloudDriver ): String;
begin
  Result:= TMacCloudUtil.driverMainIconPath( TCloudDriverClass(driver.ClassType) );
end;

class function TMacCloudUtil.driverMainIconPath( const driver: TCloudDriverClass ): String;
begin
  Result:= macCloudPlugin.pluginPath + 'drivers/' + driver.driverName + '/MainIcon.png';
end;

class function TMacCloudUtil.driverMainIcon( const driver: TCloudDriver ): NSImage;
begin
  Result:= TMacCloudUtil.driverMainIcon( TCloudDriverClass(driver.ClassType) );
end;

class function TMacCloudUtil.driverMainIcon( const driver: TCloudDriverClass ): NSImage;
var
  path: NSString;
begin
  path:= StringToNSString( TMacCloudUtil.driverMainIconPath(driver) );
  Result:= NSImage.alloc.initWithContentsOfFile( path );
  Result.autoRelease;
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
  FindData.ftCreationTime:= TMacCloudUtil.dateTimeToFileTime( cloudFile.creationTime );
  FindData.ftLastWriteTime:= TMacCloudUtil.dateTimeToFileTime( cloudFile.modificationTime );
  FindData.ftLastAccessTime:= TMacCloudUtil.dateTimeToFileTime( 0 );
  TStringUtil.stringToWidechars( FindData.cFileName, cloudFile.name, sizeof(FindData.cFileName) );
  cloudFile.Free;
end;

end.

