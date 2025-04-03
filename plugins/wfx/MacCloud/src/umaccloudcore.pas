unit uMacCloudCore;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  WfxPlugin,
  uMiniHttpClient, uMiniUtil;

type

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

  ICloudProgressCallback = IMiniHttpDataCallback;

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

  TCloudFiles = specialize TList<TCloudFile>;

  TCloudListFolder = class
  public
    procedure listFolderBegin( const path: String ); virtual; abstract;
    function  listFolderGetNextFile: TCloudFile; virtual; abstract;
    procedure listFolderEnd; virtual; abstract;
  end;

  TCloudDriver = class( TCloudListFolder )
  public
    class function isMatched( const name: String ): Boolean; virtual; abstract;
    class function createInstance: TCloudDriver; virtual; abstract;
  public
    function authorize: Boolean; virtual; abstract;
  public
    procedure download(
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback ); virtual; abstract;
    procedure upload(
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback ); virtual; abstract;
  public
    procedure createFolder( const path: String ); virtual; abstract;
    procedure delete( const path: String ); virtual; abstract;
    procedure copyOrMove( const fromPath: String; const toPath: String; const needToMove: Boolean ); virtual; abstract;
  end;

  TCloudDriverClass = class of TCloudDriver;

  TCloudDriverClasses = specialize TList<TCloudDriverClass>;

  { TCloudDriverManager }

  TCloudDriverManager = class
  private
    classes: TCloudDriverClasses;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure register( const cloudDriverClass: TCloudDriverClass );
    function find( const name: String ): TCloudDriverClass;
    function createInstance( const name: String ): TCloudDriver;
  end;

  { TCloudConnection }

  TCloudConnection = class
  private
    _name: String;
    _driver: TCloudDriver;
  public
    constructor Create( const name: String; const driver: TCloudDriver );
    property name: String read _name;
    property driver: TCloudDriver read _driver;
  end;

  TCloudConnections = specialize TList<TCloudConnection>;

  { TCloudConnectionManager }

  TCloudConnectionManager = class
  private
    _connections : TCloudConnections;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure add( const connection: TCloudConnection );
    function get( const name: String ): TCloudConnection;
  end;

var
  cloudDriverManager: TCloudDriverManager;
  cloudConnectionManager: TCloudConnectionManager;

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

{ TCloudDriverManager }

constructor TCloudDriverManager.Create;
begin
  classes:= TCloudDriverClasses.Create;
end;

destructor TCloudDriverManager.Destroy;
begin
  classes.Free;
end;

procedure TCloudDriverManager.register( const cloudDriverClass: TCloudDriverClass );
begin
  classes.Add( cloudDriverClass );
end;

function TCloudDriverManager.find( const name: String ): TCloudDriverClass;
var
  i: Integer;
  cloudDriverClass: TCloudDriverClass;
begin
  for i:= 0 to classes.Count - 1 do begin
    cloudDriverClass:= classes[i];
    if NOT cloudDriverClass.isMatched(name) then
      continue;
    Exit( cloudDriverClass );
  end;

  raise EArgumentException.Create( 'error when calling TCloudDriverManager.find(' + name + ')' );
end;

function TCloudDriverManager.createInstance( const name: String ): TCloudDriver;
begin
  Result:= self.find(name).createInstance;
end;

{ TCloudConnection }

constructor TCloudConnection.Create( const name: String; const driver: TCloudDriver );
begin
  _name:= name;
  _driver:= driver;
end;

{ TCloudConnectionManager }

constructor TCloudConnectionManager.Create;
begin
  _connections:= TCloudConnections.Create.Create;
end;

destructor TCloudConnectionManager.Destroy;
begin
  FreeAndNil( _connections );
end;

procedure TCloudConnectionManager.add(const connection: TCloudConnection);
begin
  _connections.Add( connection );
end;

function TCloudConnectionManager.get(const name: String): TCloudConnection;
var
  i: Integer;
  connection: TCloudConnection;
begin
  for i:= 0 to _connections.Count - 1 do begin
    connection:= _connections[i];
    if connection.name <> name then
      continue;
    Exit( connection );
  end;

  raise EArgumentException.Create( 'error when calling TCloudDriverManager.find(' + name + ')' );
end;

initialization
  cloudDriverManager:= TCloudDriverManager.Create;
  cloudConnectionManager:= TCloudConnectionManager.Create;

finalization
  FreeAndNil( cloudConnectionManager );
  FreeAndNil( cloudDriverManager );

end.

