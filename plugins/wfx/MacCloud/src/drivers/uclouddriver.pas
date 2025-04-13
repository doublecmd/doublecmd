unit uCloudDriver;

{$mode ObjFPC}{$H+}
{$interfaces corba}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uMiniHttpClient;

type

  { TCloudDriverResult }

  TCloudDriverResult = class
  public
    httpResult: TMiniHttpResult;
    resultMessage: String;
  end;

  { ECloudDriverException }

  ECloudDriverException = class( Exception );
  ECloudDriverTokenException = class( ECloudDriverException );
  ECloudDriverConflictException = class( ECloudDriverException );
  ECloudDriverPermissionException = class( ECloudDriverException );
  ECloudDriverRateLimitException = class( ECloudDriverException );

  { TCloudDriverConfig }

  TCloudDriverConfig = class
  private
    _clientID: String;
    _listenURI: String;
  public
    constructor Create( const clientID: String; const listenURI: String );
    property clientID: String read _clientID;
    property listenURI: String read _listenURI;
  end;

  { TCloudDriverToken }

  TCloudDriverToken = class
  private
    _access: String;
    _refresh: String;
    _accessExpirationTime: NSTimeInterval;
  private
  public
    constructor Create;
    constructor Create( const access: String; const refresh: String; const accessExpirationTime: NSTimeInterval );
    function clone: TCloudDriverToken;
  public
    procedure setExpiration( const seconds: Integer );
    procedure invalid;
    property access: String read _access write _access;
    property refresh: String read _refresh write _refresh;
    property accessExpirationTime: NSTimeInterval read _accessExpirationTime write _accessExpirationTime;
    function isValidAccessToken: Boolean;
    function isValidFreshToken: Boolean;
  end;

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
    procedure delete( const path: String ); virtual; abstract;
    procedure copyOrMove( const fromPath: String; const toPath: String; const needToMove: Boolean ); virtual; abstract;
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
  public
    procedure driverUpdated( const driver: TCloudDriver );
  public
    property driverClasses: TCloudDriverClasses read _classes;
    property observer: ICloudDriverObserver write _observer;
  end;

var
  cloudDriverManager: TCloudDriverManager;

implementation

{ TCloudDriverConfig }

constructor TCloudDriverConfig.Create(
  const clientID: String;
  const listenURI: String );
begin
  _clientID:= clientID;
  _listenURI:= listenURI;
end;

{ TCloudDriverToken }

function TCloudDriverToken.isValidAccessToken: Boolean;
var
  now: NSDate;
begin
  Result:= False;
  if _access = EmptyStr then
    Exit;
  now:= NSDate.new;
  if now.timeIntervalSince1970 < _accessExpirationTime then
    Result:= True;
  now.release;
end;

function TCloudDriverToken.isValidFreshToken: Boolean;
begin
  Result:= _refresh <> EmptyStr;
end;

constructor TCloudDriverToken.Create;
begin
end;

constructor TCloudDriverToken.Create(const access: String; const refresh: String;
  const accessExpirationTime: NSTimeInterval);
begin
  _access:= access;
  _refresh:= refresh;
  _accessExpirationTime:= accessExpirationTime;
end;

function TCloudDriverToken.clone: TCloudDriverToken;
begin
  Result:= TCloudDriverToken.Create( _access, _refresh, _accessExpirationTime );
end;

procedure TCloudDriverToken.setExpiration(const seconds: Integer);
var
  now: NSDate;
  expirationDate: NSDate;
begin
  now:= NSDate.new;
  expirationDate:= now.dateByAddingTimeInterval( seconds - 300 );
  _accessExpirationTime:= expirationDate.timeIntervalSince1970;
  now.release;
end;

procedure TCloudDriverToken.invalid;
begin
  _access:= EmptyStr;
  _refresh:= EmptyStr;
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

procedure TCloudDriverManager.driverUpdated(const driver: TCloudDriver);
begin
  if Assigned(_observer) then
    _observer.driverUpdated( driver );
end;

end.

