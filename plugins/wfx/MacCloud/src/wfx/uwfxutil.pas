unit uWFXUtil;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  WfxPlugin,
  CocoaAll, uMiniCocoa,
  uCloudDriver, uWFXPlugin,
  uMiniUtil;

type

  ULONGLONG  = qword;

  ULARGE_INTEGER = record
     case byte of
       0: (LowPart : DWORD;
           HighPart : DWORD);
       1: (QuadPart : ULONGLONG);
  end;

  { TWFXProgressCallback }

  TWFXProgressCallback = class( ICloudProgressCallback )
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

  { TWFXPathParser }

  TWFXPathParser = class
  private
    _connectionName: String;
    _driverPath: String;
  private
    function getConnection: TWFXConnection;
    function getDriver: TCloudDriver;
  public
    constructor Create( const path: String );
    property connection: TWFXConnection read getConnection;
    property driver: TCloudDriver read getDriver;
    property connectionName: String read _connectionName;
    property driverPath: String read _driverPath;
  end;

  { TWFXPluginUtil }

  TWFXPluginUtil = class
  public
    class function fileTimeToDateTime(AFileTime: FILETIME): TDateTime;
    class function dateTimeToFileTime(ADateTimeUTC: TDateTime): FILETIME;
    class procedure cloudFileToWinFindData( cloudFile: TCloudFile; var FindData:tWIN32FINDDATAW );
    class function exceptionToResult( const e: Exception ): Integer;
    class function driverDataPath( const driver: TCloudDriver ): String; overload;
    class function driverDataPath( const driver: TCloudDriverClass ): String; overload;
    class function driverMainIcon( const driver: TCloudDriver ): NSImage; overload;
    class function driverMainIcon( const driver: TCloudDriverClass ): NSImage; overload;
  end;

implementation

{ TWFXProgressCallback }

constructor TWFXProgressCallback.Create(
  const serverPath: pwidechar;
  const localPath: pwidechar;
  const totalBytes: Integer);
begin
  _serverPath:= serverPath;
  _localPath:= localPath;
  _totalBytes:= totalBytes;
end;

function TWFXProgressCallback.progress(const accumulatedBytes: Integer): Boolean;
var
  percent: Integer;
  ret: Integer;
begin
  if _totalBytes > 0 then
    percent:= accumulatedBytes * 100 div _totalBytes
  else
    percent:= 50;
  ret:= WFXMacCloudPlugin.progress( _serverPath, _localPath, percent );
  Result:= (ret = 0);
end;

{ TWFXPathParser }

function TWFXPathParser.getConnection: TWFXConnection;
begin
  Result:= WFXConnectionManager.get( _connectionName );
end;

function TWFXPathParser.getDriver: TCloudDriver;
begin
  Result:= self.connection.driver;
end;

constructor TWFXPathParser.Create(const path: String);
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

{ TWFXPluginUtil }

class function TWFXPluginUtil.fileTimeToDateTime(AFileTime: FILETIME): TDateTime;
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

class function TWFXPluginUtil.dateTimeToFileTime(ADateTimeUTC: TDateTime): FILETIME;
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

class function TWFXPluginUtil.exceptionToResult( const e: Exception ): Integer;
begin
  TLogUtil.logError( e.ClassName + ': ' + e.Message );

  if e is EAbort then
    Result:= FS_FILE_USERABORT
  else if e is EFileNotFoundException then
    Result:= FS_FILE_NOTFOUND
  else if e is EInOutError then
    Result:= FS_FILE_WRITEERROR
  else if e is ECloudDriverConflictException then
    Result:= FS_FILE_EXISTS
  else
    Result:= FS_FILE_NOTSUPPORTED;
end;

class function TWFXPluginUtil.driverDataPath( const driver: TCloudDriver ): String;
begin
  Result:= TWFXPluginUtil.driverDataPath( TCloudDriverClass(driver.ClassType) );
end;

class function TWFXPluginUtil.driverDataPath( const driver: TCloudDriverClass ): String;
begin
  Result:= WFXMacCloudPlugin.pluginPath + 'drivers/' + driver.driverName + '/';
end;

class function TWFXPluginUtil.driverMainIcon( const driver: TCloudDriver ): NSImage;
begin
  Result:= TWFXPluginUtil.driverMainIcon( TCloudDriverClass(driver.ClassType) );
end;

class function TWFXPluginUtil.driverMainIcon( const driver: TCloudDriverClass ): NSImage;
var
  path: NSString;
begin
  path:= StringToNSString( TWFXPluginUtil.driverDataPath(driver) + 'MainIcon.png' );
  Result:= NSImage.alloc.initWithContentsOfFile( path );
  Result.autoRelease;
end;

class procedure TWFXPluginUtil.cloudFileToWinFindData( cloudFile: TCloudFile; var FindData:tWIN32FINDDATAW );
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
  FindData.ftCreationTime:= TWFXPluginUtil.dateTimeToFileTime( cloudFile.creationTime );
  FindData.ftLastWriteTime:= TWFXPluginUtil.dateTimeToFileTime( cloudFile.modificationTime );
  FindData.ftLastAccessTime:= TWFXPluginUtil.dateTimeToFileTime( 0 );
  TStringUtil.stringToWidechars( FindData.cFileName, cloudFile.name, sizeof(FindData.cFileName) );
  cloudFile.Free;
end;

end.

