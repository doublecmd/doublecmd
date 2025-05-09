unit uCloudDriver;

{$mode ObjFPC}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils,
  uMiniHttpClient, uMiniUtil;

type

  { TCloudDriverResult }

  TCloudDriverResult = class
  public
    httpResult: TMiniHttpResult;
    resultMessage: String;
  end;

  { ECloudDriverException }

  ECloudDriverException = class( Exception );
  ECloudDriverAuthException = class( ECloudDriverException );
  ECloudDriverConflictException = class( ECloudDriverException );
  ECloudDriverQuotaException = class( ECloudDriverException );
  ECloudDriverPermissionException = class( ECloudDriverException );
  ECloudDriverRateLimitException = class( ECloudDriverException );
  ECloudDriverNetworkException = class( ECloudDriverException );

  TCloudDriverResultProcessFunc = procedure ( const cloudDriverResult: TCloudDriverResult );

  TCloudFile = class
  private
    _name: String;
    _size: QWord;
    _creationTime: TDateTime;
    _modificationTime: TDateTime;
    _isFolder: Boolean;
  public
    property name: String read _name write _name;
    property size: QWord read _size write _size;
    property creationTime: TDateTime read _creationTime write _creationTime;
    property modificationTime: TDateTime read _modificationTime write _modificationTime;
    property isFolder: Boolean read _isFolder write _isFolder;
  end;

  TCloudFiles = TFPList;

  ICloudProgressCallback = IMiniHttpDataCallback;

  TCloudDriverLister = class
  public
    procedure listFolderBegin; virtual; abstract;
    function  listFolderGetNextFile: TCloudFile; virtual; abstract;
    procedure listFolderEnd; virtual; abstract;
  end;

  TCloudDriverBase = class
  public
    function createLister( const path: String ): TCloudDriverLister; virtual; abstract;
  public
    procedure createFolder( const path: String ); virtual; abstract;
    procedure delete( const path: String; const isFolder: Boolean ); virtual; abstract;
    procedure copyOrMove( const fromPath: String; const toPath: String;
      const isFolder: Boolean; const needToMove: Boolean ); virtual; abstract;
  end;

  TCloudDriver = class( TCloudDriverBase )
  public
    class function driverName: String; virtual; abstract;
    class function createInstance: TCloudDriver; virtual; abstract;
  public
    function clone: TCloudDriver; virtual; abstract;
  public
    function authorize: Boolean; virtual; abstract;
    procedure unauthorize; virtual; abstract;
    function authorized: Boolean; virtual; abstract;
  public
    procedure download(
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback ); virtual; abstract;
    procedure upload(
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback ); virtual; abstract;
  end;

  TCloudDriverClass = class of TCloudDriver;

  TCloudDriverClasses = TFPList;

  ICloudDriverObserver = interface
    procedure driverUpdated( const driver: TCloudDriver );
  end;

  { TCloudDriverManager }

  TCloudDriverManager = class
  private
    _classes: TCloudDriverClasses;
    _observer: ICloudDriverObserver;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure register( const cloudDriverClass: TCloudDriverClass );
    function find( const name: String ): TCloudDriverClass;
    function createInstance( const name: String ): TCloudDriver;
    function createInstance( const index: Integer ): TCloudDriver;
  public
    procedure driverUpdated( const driver: TCloudDriver );
  public
    property driverClasses: TCloudDriverClasses read _classes;
    property observer: ICloudDriverObserver write _observer;
  end;

  { TCloudDriverAuthSession }

  TCloudDriverAuthSession = class
  strict protected
    _cloudDriver: TCloudDriver;
  public
    constructor Create( const driver: TCloudDriver );
    procedure setAuthHeader( const http: TMiniHttpClient ); virtual; abstract;
    function clone( const driver: TCloudDriver ): TCloudDriverAuthSession; virtual; abstract;
    property cloudDriver: TCloudDriver read _cloudDriver;
  end;

  { TCloudDriverListFolderSession }

  TCloudDriverListFolderSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _path: String;
    _files: TCloudFiles;
    _hasMore: Boolean;
  protected
    procedure listFolderFirst; virtual; abstract;
    procedure listFolderContinue; virtual; abstract;
  public
    constructor Create( const authSession: TCloudDriverAuthSession; const path: String ); virtual;
    destructor Destroy; override;
    function getNextFile: TCloudFile;
  end;

  TCloudDriverListFolderSessionClass = class of TCloudDriverListFolderSession;

  { TCloudDriverDownloadSession }

  TCloudDriverDownloadSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _serverPath: String;
    _localPath: String;
    _callback: ICloudProgressCallback;
  public
    constructor Create(
      const authSession: TCloudDriverAuthSession;
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback );
    procedure download; virtual; abstract;
  end;

  { TCloudDriverUploadSession }

  TCloudDriverUploadSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _serverPath: String;
    _localPath: String;
    _localFileSize: Integer;
    _callback: ICloudProgressCallback;
  public
    constructor Create(
      const authSession: TCloudDriverAuthSession;
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback );
    procedure upload; virtual; abstract;
  end;

  { TCloudDriverCreateFolderSession }

  TCloudDriverCreateFolderSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _path: String;
  public
    constructor Create( const authSession: TCloudDriverAuthSession; const path: String );
    procedure createFolder; virtual; abstract;
  end;

  { TCloudDriverDeleteSession }

  TCloudDriverDeleteSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _path: String;
    _isFolder: Boolean;
  public
    constructor Create( const authSession: TCloudDriverAuthSession;
      const path: String; const isFolder: Boolean );
    procedure delete; virtual; abstract;
  end;

  { TCloudDriverCopyMoveSession }

  TCloudDriverCopyMoveSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _fromPath: String;
    _toPath: String;
    _isFolder: Boolean;
  public
    constructor Create(
      const authSession: TCloudDriverAuthSession;
      const fromPath: String;
      const toPath: String;
      const isFolder: Boolean );
    procedure copyOrMove( const needToMove: Boolean ); virtual; abstract;
    procedure copy;
    procedure move;
  end;

  { TCloudDriverDefaultLister }

  TCloudDriverDefaultLister = class( TCloudDriverLister )
  private
    _session: TCloudDriverListFolderSession;
  public
    constructor Create( const session: TCloudDriverListFolderSession );
    destructor Destroy; override;
    procedure listFolderBegin; override;
    function  listFolderGetNextFile: TCloudFile; override;
    procedure listFolderEnd; override;
  end;

var
  cloudDriverManager: TCloudDriverManager;

implementation

{ TCloudDriverListFolderSession }

constructor TCloudDriverListFolderSession.Create(
  const authSession: TCloudDriverAuthSession; const path: String);
begin
  _authSession:= authSession;
  _files:= TCloudFiles.Create;
  _path:= path;
end;

destructor TCloudDriverListFolderSession.Destroy;
begin
  FreeAndNil( _files );
end;

function TCloudDriverListFolderSession.getNextFile: TCloudFile;
  function popFirst: TCloudFile;
  begin
    if _files.Count > 0 then begin
      Result:= TCloudFile( _files.First );
      _files.Delete( 0 );
    end else begin
      Result:= nil;
    end;
  end;

begin
  Result:= popFirst;
  if (Result=nil) and _hasMore then begin
    listFolderContinue;
    Result:= popFirst;
  end;
end;

{ TCloudDriverDownloadSession }

constructor TCloudDriverDownloadSession.Create(
  const authSession: TCloudDriverAuthSession;
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback );
begin
  _authSession:= authSession;
  _serverPath:= serverPath;
  _localPath:= localPath;
  _callback:= callback;
end;

{ TCloudDriverUploadSession }

constructor TCloudDriverUploadSession.Create(
  const authSession: TCloudDriverAuthSession;
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback );
begin
  _authSession:= authSession;
  _serverPath:= serverPath;
  _localPath:= localPath;
  _callback:= callback;
  _localFileSize:= TFileUtil.filesize( _localPath );
end;

{ TCloudDriverCreateFolderSession }

constructor TCloudDriverCreateFolderSession.Create(
  const authSession: TCloudDriverAuthSession; const path: String);
begin
  _authSession:= authSession;
  _path:= path;
end;

{ TCloudDriverDeleteSession }

constructor TCloudDriverDeleteSession.Create(
  const authSession: TCloudDriverAuthSession;
  const path: String; const isFolder: Boolean );
begin
  _authSession:= authSession;
  _path:= path;
  _isFolder:= isFolder;
end;

{ TCloudDriverCopyMoveSession }

constructor TCloudDriverCopyMoveSession.Create(
  const authSession: TCloudDriverAuthSession;
  const fromPath: String;
  const toPath: String;
  const isFolder: Boolean );
begin
  _authSession:= authSession;
  _fromPath:= fromPath;
  _toPath:= toPath;
  _isFolder:= isFolder;
end;

procedure TCloudDriverCopyMoveSession.copy;
begin
  copyOrMove( False );
end;

procedure TCloudDriverCopyMoveSession.move;
begin
  copyOrMove( True );
end;

{ TCloudDriverDefaultLister }

constructor TCloudDriverDefaultLister.Create( const session: TCloudDriverListFolderSession );
begin
  _session:= session;
end;

destructor TCloudDriverDefaultLister.Destroy;
begin
  FreeAndNil( _session );
end;

procedure TCloudDriverDefaultLister.listFolderBegin;
begin
  _session.listFolderFirst;
end;

function TCloudDriverDefaultLister.listFolderGetNextFile: TCloudFile;
begin
  Result:= _session.getNextFile;
end;

procedure TCloudDriverDefaultLister.listFolderEnd;
begin
  self.Free;
end;

{ TCloudDriverManager }

constructor TCloudDriverManager.Create;
begin
  _classes:= TCloudDriverClasses.Create;
end;

destructor TCloudDriverManager.Destroy;
begin
  _classes.Free;
end;

procedure TCloudDriverManager.register( const cloudDriverClass: TCloudDriverClass );
begin
  if _classes.IndexOf(cloudDriverClass) >= 0 then
    Exit;
  _classes.Add( cloudDriverClass );
end;

function TCloudDriverManager.find( const name: String ): TCloudDriverClass;
var
  i: Integer;
  cloudDriverClass: TCloudDriverClass;
begin
  for i:= 0 to _classes.Count - 1 do begin
    cloudDriverClass:= TCloudDriverClass( _classes[i] );
    if cloudDriverClass.driverName <> name then
      continue;
    Exit( cloudDriverClass );
  end;

  raise EArgumentException.Create( 'CloudDriver not found in TCloudDriverManager.find(): ' + name );
end;

function TCloudDriverManager.createInstance( const name: String ): TCloudDriver;
begin
  Result:= self.find(name).createInstance;
end;

function TCloudDriverManager.createInstance(const index: Integer): TCloudDriver;
begin
  Result:= TCloudDriverClass( self.driverClasses[index] ).createInstance;
end;

procedure TCloudDriverManager.driverUpdated(const driver: TCloudDriver);
begin
  if Assigned(_observer) then
    _observer.driverUpdated( driver );
end;

{ TCloudDriverAuthSession }

constructor TCloudDriverAuthSession.Create(const driver: TCloudDriver);
begin
  _cloudDriver:= driver;
end;

end.

