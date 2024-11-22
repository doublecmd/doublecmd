unit uMountedFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  uFile, uFileSource, uFileSourceProperty, uFileSystemFileSource,
  uFileSourceOperation, uFileSourceOperationTypes,
  uDCUtils;

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
  public
    constructor Create( const path: String; const point: String );
    property path: String read _path;
    property point: String read _point;
  end;

  TMountPoints = specialize TList<TMountPoint>;

  { TMountedFileSource }

  TMountedFileSource = class(TFileSource, IMountedFileSource)
  private
    _mountPoints: TMountPoints;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure mount( const path: String; const point: String );
    procedure mount( const path: String );
    function getDefaultPointForPath( const path: String ): String; virtual;
  public
    class function CreateFile(const APath: String): TFile; override;
    function GetRootDir(sPath : String): String; override;
    function GetProperties: TFileSourceProperties; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
      var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
      override;
    function GetLocalName(var aFile: TFile): Boolean; override;

  public
    property mountPoints: TMountPoints read _mountPoints;
  end;

implementation

uses
  uMountedListOperation;

{ TMountPoint }

constructor TMountPoint.Create(const path: String; const point: String);
begin
  _path:= path;
  _point:= point;
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

class function TMountedFileSource.CreateFile(const APath: String): TFile;
begin
  Result:= TFileSystemFileSource.CreateFile( APath );
end;

function TMountedFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + 'mount' + PathDelim;
end;

function TMountedFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [
    fspDirectAccess, fspListFlatView, fspNoneParent, fspLinksToLocalFiles
{$IFDEF UNIX}
  , fspCaseSensitive
{$ENDIF}
  ];
end;

function TMountedFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList,
             fsoCopy,
             fsoCopyIn,
             fsoCopyOut,
             fsoMove,
             fsoDelete,
             fsoWipe,
             fsoSplit,
             fsoCombine,
             fsoCreateDirectory,
             fsoCalcChecksum,
             fsoCalcStatistics,
             fsoSetFileProperty,
             fsoExecute];
end;

function TMountedFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result:= TMountedListOperation.Create( self, TargetPath );
end;

function TMountedFileSource.CreateCopyOutOperation(
  TargetFileSource: IFileSource; var SourceFiles: TFiles; TargetPath: String
  ): TFileSourceOperation;
var
  fs: TFileSystemFileSource;
begin
  fs:= TFileSystemFileSource.create;
  Result:= fs.CreateCopyOutOperation( TargetFileSource, SourceFiles, TargetPath );
end;

function TMountedFileSource.GetLocalName(var aFile: TFile): Boolean;
begin
  Result:= True;
end;

end.

