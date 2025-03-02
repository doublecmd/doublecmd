unit uMountedFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  Dialogs,
  uFile, uFileSource, uFileSourceManager,
  uFileSystemFileSource, uWcxArchiveFileSource,
  uFileSourceProperty, uFileSourceOperation, uFileSourceOperationTypes,
  uLng, uDCUtils, DCStrUtils;

type
  { IMountedFileSource }

  IMountedFileSource = interface(IFileSource)
    ['{47A3B4E6-5C40-A86A-E325-E4ABDC069B42}']

    procedure mount( const path: String; const mountPoint: String );
    procedure mount( const path: String );
  end;

  { TMountPoint }

  TMountPoint = class
  strict private
    _path: String;
    _point: String;
    _name: String;
  public
    constructor Create( const path: String; const point: String );
    property path: String read _path;
    property point: String read _point;
    property name: String read _name;
  end;

  TMountPoints = specialize TList<TMountPoint>;

  { TMountedFileSource }

  TMountedFileSource = class(TFileSystemFileSource, IMountedFileSource)
  private
    _mountPoints: TMountPoints;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure mount( const path: String; const point: String );
    procedure mount( const path: String );
    function getDefaultPointForPath( const path: String ): String; virtual;
    function getMountPointFromPath(const realPath: String): TMountPoint;
  protected
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;
    function GetCurrentWorkingDirectory: String; override;
  public
    function GetProcessor: TFileSourceProcessor; override;
    function GetRealPath(const APath: String): String; override;
    function GetVirtualPath(const APath: String): String; override;
    function GetFileName(aFile: TFile): String; override;

    function GetProperties: TFileSourceProperties; override;
    function GetParentDir(sPath : String): String; override;
    function GetRootDir(sPath : String): String; override;
    function IsPathAtRoot(Path: String): Boolean; override;
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
  public
    property mountPoints: TMountPoints read _mountPoints;
  end;

  { TMountedFileSourceProcessor }

  TMountedFileSourceProcessor = class( TFileSystemFileSourceProcessor )
  private
    procedure detectIfSupportOperation(var params: TFileSourceConsultParams);
    procedure consultCopyOperation(var params: TFileSourceConsultParams);
    procedure consultMoveOperation(var params: TFileSourceConsultParams);
    procedure resolveRealPath( var params: TFileSourceConsultParams );
    procedure calcTargetPath( var params: TFileSourceConsultParams );
  public
    procedure consultOperation(var params: TFileSourceConsultParams); override;
    procedure confirmOperation( var params: TFileSourceConsultParams ); override;
  end;

implementation

uses
  uMountedListOperation;

var
  mountedFileSourceProcessor: TMountedFileSourceProcessor;

{ TMountPoint }

constructor TMountPoint.Create(const path: String; const point: String);
begin
  _path:= path;
  _point:= point;
  _name:= ExcludeLeadingPathDelimiter(ExcludeTrailingPathDelimiter( _point ));
end;

{ TMountedFileSource }

constructor TMountedFileSource.Create;
begin
  inherited Create;
  _mountPoints:= TMountPoints.Create;
end;

destructor TMountedFileSource.Destroy;
begin
  FreeAndNil( _mountPoints );
  inherited Destroy;
end;

procedure TMountedFileSource.mount(const path: String; const point: String );
var
  realPath: String;
  realPoint: String;
  mountPoint: TMountPoint;
begin
  realPath:= IncludeTrailingPathDelimiter( uDCUtils.ReplaceTilde(path) );
  realPoint:= IncludeTrailingPathDelimiter( point );
  mountPoint:= TMountPoint.Create( realPath, realPoint );
  _mountPoints.Add( mountPoint );
end;

procedure TMountedFileSource.mount( const path: String );
var
  realPath: String;
  realPoint: String;
begin
  realPath:= uDCUtils.ReplaceTilde( path );
  realPoint:= self.getDefaultPointForPath( realPath );
  if realPoint.IsEmpty then
    raise ENotImplemented.Create( 'getDefaultPointForPath() not Implemented in ' + self.ClassName );
  realPoint:= PathDelim + realPoint + PathDelim;
  self.mount( realPath, realPoint );
end;

function TMountedFileSource.getDefaultPointForPath(const path: String): String;
begin
  Result:= String.Empty;
end;

function TMountedFileSource.getMountPointFromPath(const realPath: String): TMountPoint;
var
  mountPoint: TMountPoint;
  path: String;
begin
  Result:= nil;
  path:= IncludeTrailingPathDelimiter( realPath );
  for mountPoint in _mountPoints do begin
    if path.Equals(mountPoint.path) then begin
      Result:= mountPoint;
      Exit;
    end;
  end;
end;

function TMountedFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  Result:= True;
end;

function TMountedFileSource.GetCurrentWorkingDirectory: String;
begin
  Result:= '';
end;

function TMountedFileSource.GetProcessor: TFileSourceProcessor;
begin
  Result:= mountedFileSourceProcessor;
end;

function TMountedFileSource.GetRealPath( const APath: String ): String;
var
  mountPoint: TMountPoint;
  logicPath: String;
begin
  Result:= EmptyStr;
  logicPath:= APath.Substring( self.GetRootDir.Length - 1 );
  for mountPoint in _mountPoints do begin
    if logicPath.StartsWith(mountPoint.point) then begin
      Result:= mountPoint.path + logicPath.Substring(mountPoint.point.Length);
      Exit;
    end;
  end;
end;

function TMountedFileSource.GetVirtualPath( const APath: String ): String;
var
  mountPoint: TMountPoint;
  logicPath: String;
begin
  Result:= EmptyStr;
  logicPath:= IncludeTrailingPathDelimiter( APath );
  for mountPoint in _mountPoints do begin
    if logicPath.StartsWith(mountPoint.path) then begin
      Result:= self.GetRootDir() +
               ExcludeLeadingPathDelimiter(mountPoint.point) +
               APath.Substring(mountPoint.path.Length);
      Exit;
    end;
  end;
end;

function TMountedFileSource.GetFileName(aFile: TFile): String;
var
  mountPoint: TMountPoint;
begin
  mountPoint:= self.getMountPointFromPath( aFile.FullPath );
  if mountPoint <> nil then
    Result:= mountPoint.name
  else
    Result:= inherited;
end;

function TMountedFileSource.GetProperties: TFileSourceProperties;
begin
  Result:= inherited GetProperties;
  Result:= Result + [fspMounted];
end;

function TMountedFileSource.GetParentDir(sPath : String): String;
begin
  Result := DCStrUtils.GetParentDir(sPath);
end;

function TMountedFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + 'mount' + PathDelim;
end;

function TMountedFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Result:= ( IncludeTrailingPathDelimiter(Path)=self.GetRootDir );
end;

function TMountedFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result:= TMountedListOperation.Create( self, TargetPath );
end;

function TMountedFileSource.CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation;
var
  realPath: String;
begin
  realPath:= self.GetRealPath( BasePath );
  Result:= inherited CreateCreateDirectoryOperation(realPath, DirectoryPath);
end;

{ TMountedFileSourceProcessor }

procedure TMountedFileSourceProcessor.resolveRealPath( var params: TFileSourceConsultParams);
var
  mountedFS: TMountedFileSource;
  pathType: TPathType;
  targetPath: String;

  function calcBasePath: String;
  var
    realPath: String;
    mountPoint: TMountPoint;
  begin
    realPath:= params.files[0].FullPath;
    mountPoint:= mountedFS.getMountPointFromPath( realPath );
    if mountPoint <> nil then
      Result:= mountPoint.path
    else
      Result:= GetParentDir( realPath );
  end;

begin
  mountedFS:= params.currentFS as TMountedFileSource;
  targetPath:= params.targetPath;
  pathType:= GetPathType( targetPath );

  if ((params.phase=TFileSourceConsultPhase.source) and (pathType<>ptAbsolute)) or
     ((params.phase=TFileSourceConsultPhase.target) and (pathType=ptAbsolute)) then begin
    if pathType <> ptAbsolute then begin
      targetPath:= params.files.Path + targetPath;
      targetPath:= ExpandAbsolutePath( targetPath );
    end;
    params.resultTargetPath:= mountedFS.getRealPath( targetPath );
  end;

  if params.phase=TFileSourceConsultPhase.source then
    params.files.Path:= calcBasePath;
end;

procedure TMountedFileSourceProcessor.calcTargetPath(var params: TFileSourceConsultParams);
var
  mountedFS: TMountedFileSource;
  mountPoint: TMountPoint;
  realPath: String;
begin
  if params.phase<>TFileSourceConsultPhase.source then
    Exit;
  if NOT params.partnerFS.IsClass(TWcxArchiveFileSource) then
    Exit;

  mountedFS:= params.currentFS as TMountedFileSource;
  realPath:= params.files[0].FullPath;
  mountPoint:= mountedFS.getMountPointFromPath( realPath );
  if mountPoint = nil then
    Exit;

  params.targetPath:= IncludeTrailingPathDelimiter(params.targetPath) + mountPoint.name + PathDelim;
end;

procedure TMountedFileSourceProcessor.detectIfSupportOperation(
  var params: TFileSourceConsultParams);
var
  files: TFiles;
  i: Integer;
  path: String;
begin
  if params.phase<>TFileSourceConsultPhase.source then
    Exit;
  if NOT params.partnerFS.IsClass(TWcxArchiveFileSource) then
    Exit;

  files:= params.files;
  if files.Count = 1 then
    Exit;

  path:= files[0].Path;
  for i:=1 to files.Count-1 do begin
    if files[i].Path = path then
      continue;

    MessageDlg(
      rsMountedFileSourceCopyMultiFilesToWcxDlgTitle,
      rsMountedFileSourceCopyMultiFilesToWcxDlgMessage,
      mtInformation,
      [mbOK],
      0 );

    params.consultResult:= fscrCancel;
    params.handled:= True;
    Exit;
  end;
end;

procedure TMountedFileSourceProcessor.consultCopyOperation(var params: TFileSourceConsultParams);
begin
  detectIfSupportOperation( params );
  if params.handled then
    Exit;

  inherited consultOperation( params );
  self.calcTargetPath( params );
end;

procedure TMountedFileSourceProcessor.consultMoveOperation(var params: TFileSourceConsultParams);
begin
  params.consultResult:= fscrNotSupported;
  params.handled:= True;
  if params.phase=TFileSourceConsultPhase.source then
    Exit;
  if params.sourceFS.GetClass.ClassType <> TFileSystemFileSource then
    Exit;

  params.consultResult:= fscrSuccess;
end;

procedure TMountedFileSourceProcessor.consultOperation(
  var params: TFileSourceConsultParams);
begin
  case params.operationType of
    fsoCopy:
      consultCopyOperation( params );
    fsoMove:
      consultMoveOperation( params );
    else
      inherited consultOperation( params );
  end;
end;

procedure TMountedFileSourceProcessor.confirmOperation( var params: TFileSourceConsultParams );
begin
  inherited confirmOperation( params );
  case params.operationType of
    fsoCopy, fsoMove:
      self.resolveRealPath( params );
  end;
end;

initialization
  mountedFileSourceProcessor:= TMountedFileSourceProcessor.Create;

finalization
  FreeAndNil( mountedFileSourceProcessor );

end.

