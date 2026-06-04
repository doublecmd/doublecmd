unit uStashFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFile, uFileProperty, uFileSourceManager,
  uFileSourceProperty, uFileSourceOperation, uFileSourceOperationTypes,
  uFileSource, uVirtualFileSource, uFileSystemFileSource, uVfsModule,
  uFileSourceUtil, uDCUtils;

type

  { TStashFileSourceProcessor }

  TStashFileSourceProcessor = class( TDefaultFileSourceProcessor )
  private
    procedure consultCopyOperation( var params: TFileSourceConsultParams );
  public
    procedure consultOperation( var params: TFileSourceConsultParams ); override;
    procedure confirmOperation(var params: TFileSourceConsultParams); override;
  end;

  { TStashFileSource }

  TStashFileSource = class(TVirtualFileSource)
  private
    _fileSystemFS: IFileSystemFileSource;
  protected
    procedure onFileSystemEvent(var params: TFileSourceEventParams);
  public
    constructor Create; override; overload;
    destructor Destroy; override;
    function GetLocalName(var aFile: TFile): Boolean; override;
    class function GetMainIcon(out Path: String): Boolean; override;
    function needReload(const PathToReload: String; const PathToCheck: String): Boolean; override;

    function GetProcessor: TFileSourceProcessor; override;
    function GetRootDir(sPath : String): String; override;
    function GetProperties: TFileSourceProperties; override;
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetRetrievableFileProperties: TFilePropertiesTypes; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;
    class function CreateFile(const APath: String): TFile; override;
    procedure RetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes; const AVariantProperties: array of String); override;
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource; var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource; var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles; var theNewProperties: TFileProperties): TFileSourceOperation; override;
  end;

implementation

uses
  uStashFileSourceOperation;

var
  stashFileSourceProcessor: TFileSourceProcessor;

{ TStashFileSourceProcessor }

procedure TStashFileSourceProcessor.consultCopyOperation(
  var params: TFileSourceConsultParams);
var
  sourceFS: IFileSource;
  targetFS: IFileSource;

  procedure doSource;
  begin
    params.handled:= True;
    if NOT (fspDirectAccess in targetFS.Properties) and
       NOT (fsoCopyIn in targetFS.GetOperationsTypes) then begin
          params.consultResult:= fscrNotSupported;
          Exit;
    end;

    if fspDirectAccess in targetFS.Properties then begin
      params.resultOperationType:= fsoCopyOut;
      params.resultFS:= sourceFS;
    end else begin
      params.resultOperationType:= fsoCopyIn;
      params.resultFS:= targetFS;
    end;
    params.consultResult:= fscrSuccess;
  end;

  procedure doTarget;
  begin
    params.handled:= True;
    if NOT (fspDirectAccess in sourceFS.Properties) then begin
      params.consultResult:= fscrNotSupported;
      Exit;
    end;

    params.resultOperationType:= fsoCopyIn;
    params.resultFS:= targetFS;
    params.consultResult:= fscrSuccess;
  end;

begin
  sourceFS:= params.sourceFS;
  targetFS:= params.targetFS;

  if isCompatibleFileSourceForCopyOperation(sourceFS,targetFS) then begin
    params.consultResult:= fscrNotSupported;
    params.handled:= True;
    Exit;
  end;

  if params.phase=TFileSourceConsultPhase.source then
    doSource
  else
    doTarget;
end;

procedure TStashFileSourceProcessor.consultOperation(
  var params: TFileSourceConsultParams);
begin
  case params.operationType of
    fsoCopy:
      self.consultCopyOperation( params );
    else
      Inherited;
  end;
end;

procedure TStashFileSourceProcessor.confirmOperation(
  var params: TFileSourceConsultParams);
begin
  case params.operationType of
    fsoCopy: begin
      if (params.resultFS=params.partnerFS) then begin
        params.files.Path:= params.files[0].Path;
        params.handled:= True;
      end;
    end
    else
      Inherited;
  end;
end;

{ TStashFileSource }

procedure TStashFileSource.onFileSystemEvent(var params: TFileSourceEventParams);
begin
  self.Reload( params.paths );
end;

constructor TStashFileSource.Create;
begin
  Inherited Create;
  _fileSystemFS:= IFileSystemFileSource(FileSourceManager.Find(TFileSystemFileSource,EmptyStr));
  _fileSystemFS.AddEventListener( @self.onFileSystemEvent );
end;

destructor TStashFileSource.Destroy;
begin
  _fileSystemFS.RemoveEventListener( @self.onFileSystemEvent );
  inherited Destroy;
end;

function TStashFileSource.GetLocalName(var aFile: TFile): Boolean;
begin
  Result:= True;
end;

class function TStashFileSource.GetMainIcon(out Path: String): Boolean;
begin
  Path:= mbExpandFileName( '$COMMANDER_PATH/pixmaps/stuff/stash.png' );
  Result:= True;
end;

function TStashFileSource.needReload(
  const PathToReload: String;
  const PathToCheck: String): Boolean;
begin
  // todo: it should check the path in StashFilesBackend
  Result:= True;
end;

function TStashFileSource.GetProcessor: TFileSourceProcessor;
begin
  Result:= stashFileSourceProcessor;
end;

function TStashFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + 'Stash' + PathDelim;
end;

function TStashFileSource.GetProperties: TFileSourceProperties;
begin
  Result:= _fileSystemFS.Properties;
  Result+= [fspLinksToLocalFiles, fspDontChangePath];
end;

function TStashFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result:= _fileSystemFS.SupportedFileProperties;
end;

function TStashFileSource.GetRetrievableFileProperties: TFilePropertiesTypes;
begin
  Result:= _fileSystemFS.RetrievableFileProperties;
end;

function TStashFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result:= [fsoList, fsoCopyIn, fsoCopyOut, fsoSetFileProperty];
end;

class function TStashFileSource.CreateFile(const APath: String): TFile;
begin
  Result:= TFileSystemFileSource.CreateFile(APath);
end;

procedure TStashFileSource.RetrieveProperties(AFile: TFile;
  PropertiesToSet: TFilePropertiesTypes;
  const AVariantProperties: array of String);
begin
  _fileSystemFS.RetrieveProperties(AFile, PropertiesToSet, AVariantProperties);
end;

function TStashFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result:= TStashListOperation.Create(Self, TargetPath);
end;

function TStashFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
begin
  Result:= TStashCopyInOperation.Create(
              SourceFileSource,
              self,
              SourceFiles,
              TargetPath );
end;

function TStashFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
begin
  Result:= _fileSystemFS.CreateCopyOutOperation(
              TargetFileSource,
              SourceFiles,
              TargetPath );
end;

function TStashFileSource.CreateSetFilePropertyOperation(
  var theTargetFiles: TFiles; var theNewProperties: TFileProperties
  ): TFileSourceOperation;
begin
  Result:= _fileSystemFS.CreateSetFilePropertyOperation(
              theTargetFiles,
              theNewProperties );
end;

initialization
  stashFileSourceProcessor:= TStashFileSourceProcessor.Create;
  RegisterVirtualFileSource( 'Stash', TStashFileSource, True );

finalization
  FreeAndNil( stashFileSourceProcessor );

end.

