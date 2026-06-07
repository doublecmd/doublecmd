unit uStashFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, Menus,
  uFile, uFileProperty, uFileSourceManager,
  uFileSourceProperty, uFileSourceOperation, uFileSourceOperationTypes,
  uFileSource, uVirtualFileSource, uFileSystemFileSource, uVfsModule,
  uFileSourceUtil, uDCUtils,
  uStashFilesBackend
  {$IFDEF DARWIN}
  , uDarwinImage
  {$ENDIF}
  ;

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
  private
    procedure onFileSystemEvent(var params: TFileSourceEventParams);
    procedure onStashChanged(Sender: TObject);
    procedure reload; overload;
    procedure removeAction(Sender: TObject);
    procedure clearAction(Sender: TObject);
  public
    constructor Create; override; overload;
    destructor Destroy; override;
    class function GetFileSource: IFileSource; override;

    function GetLocalName(var aFile: TFile): Boolean; override;
    class function GetMainIcon(out Path: String): Boolean; override;
    function GetCustomIcon(const path: String; const iconSize: Integer): TBitmap; override; overload;
    function GetDisplayFileName(aFile: TFile): String; override;
    function needReload(const PathToReload: String; const PathToCheck: String): Boolean; override;

    function GetProcessor: TFileSourceProcessor; override;
    function GetRootDir(sPath : String): String; override;
    class function IsSupportedPath(const Path: String): Boolean; override;
    function GetProperties: TFileSourceProperties; override;
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetRetrievableFileProperties: TFilePropertiesTypes; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;
    class function CreateFile(const APath: String): TFile; override;
    procedure RetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes; const AVariantProperties: array of String); override;
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource; var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource; var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation; override;
    function CreateSplitOperation(var aSourceFile: TFile; aTargetPath: String): TFileSourceOperation; override;
    function CreateCombineOperation(var theSourceFiles: TFiles; aTargetFile: String): TFileSourceOperation; override;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles; var theNewProperties: TFileProperties): TFileSourceOperation; override;

    function QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean; override;
    procedure AddSearchPath( const startPath: String; paths: TStringList); override;
  end;

implementation

uses
  uStashFileSourceOperation;

const
  STASH_NAME   = 'Stash';
  STASH_SCHEME = 'stash://';

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

procedure TStashFileSource.onStashChanged(Sender: TObject);
begin
  TThread.Synchronize( nil, @self.reload );
end;

procedure TStashFileSource.reload;
begin
  self.Reload( self.GetRootDir );
end;

procedure TStashFileSource.removeAction(Sender: TObject);
var
  item: TMenuItem absolute Sender;
  files: TFiles;
begin
  files:= TFiles( item.Tag );
  stashFilesBackend.removePaths( files );
end;

procedure TStashFileSource.clearAction(Sender: TObject);
begin
  stashFilesBackend.clear;
end;

constructor TStashFileSource.Create;
begin
  Inherited Create;
  stashFilesBackend.setListener( @self.onStashChanged );
  FCurrentAddress:= STASH_SCHEME;
  _fileSystemFS:= IFileSystemFileSource(FileSourceManager.Find(TFileSystemFileSource,EmptyStr));
  _fileSystemFS.AddEventListener( @self.onFileSystemEvent );
end;

destructor TStashFileSource.Destroy;
begin
  stashFilesBackend.setListener( nil );
  _fileSystemFS.RemoveEventListener( @self.onFileSystemEvent );
  inherited Destroy;
end;

class function TStashFileSource.GetFileSource: IFileSource;
begin
  Result:= FileSourceManager.Find( TStashFileSource, STASH_SCHEME );
  if not Assigned(Result) then
    Result:= TStashFileSource.Create;
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

function TStashFileSource.GetCustomIcon(
  const path: String;
  const iconSize: Integer): TBitmap;
{$IFDEF DARWIN}
var
  iconPath: String;
begin
  self.GetMainIcon( iconPath );
  Result:= darwinImageCacheForPath.copyBitmapForFileContent( iconPath, iconSize, False );
end;
{$ELSE}
begin
  Result:= nil;
end;
{$ENDIF}

function TStashFileSource.GetDisplayFileName(aFile: TFile): String;
begin
  if aFile.FullPath = self.GetRootDir() then
    Result:= STASH_NAME
  else
    Result:= aFile.Name;
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
  Result:= PathDelim + STASH_NAME + PathDelim;
end;

class function TStashFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= Path.StartsWith( STASH_SCHEME );
end;

function TStashFileSource.GetProperties: TFileSourceProperties;
begin
  Result:= _fileSystemFS.Properties;
  Result+= [fspLinksToLocalFiles, fspDontChangePath, fspDontCreateDirectory];
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
  Result:= [fsoList,
            fsoCopyIn, fsoCopyOut,
            fsoDelete, fsoWipe,
            fsoSplit, fsoCombine,
            fsoCalcStatistics,
            fsoSetFileProperty];
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

function TStashFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
begin
  Result:= _fileSystemFS.CreateDeleteOperation( FilesToDelete );
end;

function TStashFileSource.CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation;
begin
  Result:= _fileSystemFS.CreateWipeOperation( FilesToWipe );
end;

function TStashFileSource.CreateSplitOperation(var aSourceFile: TFile;
  aTargetPath: String): TFileSourceOperation;
begin
  Result:= _fileSystemFS.CreateSplitOperation(aSourceFile, aTargetPath);
end;

function TStashFileSource.CreateCombineOperation(var theSourceFiles: TFiles;
  aTargetFile: String): TFileSourceOperation;
begin
  Result:= _fileSystemFS.CreateCombineOperation(theSourceFiles, aTargetFile);
end;

function TStashFileSource.CreateCalcStatisticsOperation(var theFiles: TFiles
  ): TFileSourceOperation;
begin
  Result:= _fileSystemFS.CreateCalcStatisticsOperation(theFiles);
end;

function TStashFileSource.CreateSetFilePropertyOperation(
  var theTargetFiles: TFiles; var theNewProperties: TFileProperties
  ): TFileSourceOperation;
begin
  Result:= _fileSystemFS.CreateSetFilePropertyOperation(
              theTargetFiles,
              theNewProperties );
end;

function TStashFileSource.QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean;
var
  index: Integer;

  function hasValidPath: Boolean;
  var
    testPath: String;
  begin
    Result:= False;
    if AFiles.Count = 0 then
      Exit;
    if AFiles.Count = 1 then begin
      testPath:= IncludeTrailingPathDelimiter( AFiles[0].FullPath );
      Result:= NOT self.IsPathAtRoot( testPath );
    end else begin
      Result:= True;
    end;
  end;

  procedure removeAddToStash;
  var
    item: TMenuItem;
  begin
    item:= AMenu.Items.Find( 'Add to Stash' );
    if Assigned(item) then
      AMenu.Items.Remove( item );
  end;

  procedure addRemoveStashItems;
  var
    item: TMenuItem;
  begin
    item:= TMenuItem.Create( AMenu );
    item.Caption:= 'Remove Stash Items';
    item.OnClick:= @self.removeAction;
    item.Tag:= PtrInt( AFiles );
    AMenu.Items.Insert(index, item);
    inc( index );
  end;

  procedure addEmptyStash;
  var
    item: TMenuItem;
  begin
    item:= TMenuItem.Create( AMenu );
    item.Caption:= 'Empty Stash';
    item.OnClick:= @self.clearAction;
    AMenu.Items.Insert(index, item);
    inc( index );
  end;

  procedure addSeperator;
  var
    item: TMenuItem;
  begin
    item:= TMenuItem.Create( AMenu );
    item.Caption:= '-';
    AMenu.Items.Insert(index, item);
  end;

begin
  Result:= False;
  index:= 0;

  removeAddToStash;

  if hasValidPath then
    addRemoveStashItems;

  if stashFilesBackend.count > 0 then
    addEmptyStash;

  if index > 0 then begin
    addSeperator;
    Result:= True;
  end;
end;

procedure TStashFileSource.AddSearchPath(
  const startPath: String;
  paths: TStringList );
var
  files: TFiles;
  i: Integer;
begin
  if paths.Count > 0 then
    Exit;
  files:= self.GetFiles( self.GetRootDir );
  for i:= 0 to files.Count-1 do
    paths.Add( files[i].FullPath );
  files.Free;
end;

initialization
  stashFileSourceProcessor:= TStashFileSourceProcessor.Create;
  RegisterVirtualFileSource( STASH_NAME, STASH_SCHEME, TStashFileSource, True );

finalization
  FreeAndNil( stashFileSourceProcessor );

end.

