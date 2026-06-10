unit uSearchResultFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics,
  uFile, uFileSource, uFileSourceManager, uMultiListFileSource,
  uFileSourceOperationTypes, uFileSourceOperation, uFileSourceProperty,
  uSysFolders
  {$IFDEF DARWIN}
  , uDarwinFile, uDarwinImage, uDCUtils
  {$ENDIF}
  ;

type

  ISearchResultFileSource = interface(IMultiListFileSource)
    ['{5076D4C2-3AB8-4029-9318-0AF115F7FDDD}']
  end;

  {en
     File source for search results.
  }

  { TSearchResultFileSource }

  TSearchResultFileSource = class(TMultiListFileSource, ISearchResultFileSource)
  protected
    _displayName: String;
  public
    constructor Create( const displayName: String );

    function GetLocalName(var aFile: TFile): Boolean; override;
    function GetRootDir(sPath : String): String; override;
    function GetRealPath(const path: String): String; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

    function GetCustomIcon(const path: String; const iconSize: Integer): TBitmap; override; overload;
    function GetDisplayFileName(aFile: TFile): String; override;

    function GetProcessor: TFileSourceProcessor; override;
    function GetProperties: TFileSourceProperties; override;
    class function CreateFile(const APath: String): TFile; override;
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;

    procedure AddSearchPath( const startPath: String; paths: TStringList); override;
  end;

implementation

uses
  uFileSystemFileSource, uSearchResultListOperation, uLng;

type

  { TSearchResultFileSourceProcessor }

  TSearchResultFileSourceProcessor = class( TDefaultFileSourceProcessor )
  private
    {
      when copying from SearchResult to TargetFileSource, the following needs to be considered:
      1. SearchResult only supports copying outwards, not copying to SearchResult.
      2. there are two types of SearchResult: one is created from a FileSource that
         supports DirectAccess, in which case the SearchResult supports DirectAccess;
         the other is created from a Wcx/Zip, in which case the SearchResult does not
         support DirectAccess.
      3. there are also two types of TargetFileSource: one that supports DirectAccess
         and one that does not (such as Wcx/Zip).
      therefore, there are a total of 4 combinations:
      1. both SearchResult and TargetFileSource support DirectAccess
         TargetFileSource CopyIn / no temp file
      2. SearchResult supports DirectAccess, but TargetFileSource does not
         TargetFileSource CopyIn / no temp file
      3. SearchResult does not support DirectAccess, but TargetFileSource does
         SearchResult CopyOut / no temp file
      4. Neither SearchResult nor TargetFileSource supports DirectAccess
         SearchResult CopyOut / TargetFileSource CopyIn / Temp file needed
    }
    procedure consultCopyOperation( var params: TFileSourceConsultParams );

    {
      when moving from SearchResult to TargetFileSource, the following needs to be considered:
      1. the SearchResult created from a Wcx/Zip doesn't support moving files.
         it's limited by DC's philosophy that doesn't support moving files from
         WCX/ZIP files to external.
      2. only the SearchResult created from FileSource that supports DirectAccess,
         such as FileSystemFileSource, supports moving files. it only supports moving outwards,
         not moving to SearchResult.
      3. only the TargetFileSource that supports DirectAccess supports moving files from
         SearchResult. it's also limited by DC's philosophy that doesn't support moving
         files to Wcx/Zip.
      4. therefore, only one type of movement is currently supported: moving from
         SearchResult that supports DirectAccess to TargetFileSource that supports DirectAccess.
    }
    procedure consultMoveOperation( var params: TFileSourceConsultParams );
  public
    procedure consultOperation( var params: TFileSourceConsultParams ); override;
    procedure confirmOperation(var params: TFileSourceConsultParams); override;
  end;

var
  searchResultFileSourceProcessor: TSearchResultFileSourceProcessor;

procedure TSearchResultFileSourceProcessor.consultCopyOperation(
  var params: TFileSourceConsultParams);
begin
  if params.phase = TFileSourceConsultPhase.target then
    Exit;

  if NOT (fsoCopyIn in params.partnerFS.GetOperationsTypes) then
    Exit;

  params.handled:= False;

  if fspDirectAccess in params.targetFS.Properties then begin
    params.resultFS:= params.partnerFS;
    params.resultOperationType:= fsoCopyIn;
    params.operationTemp:= False;
    params.consultResult:= fscrSuccess;
  end else begin
    Inherited consultOperation( params );
  end;
end;

procedure TSearchResultFileSourceProcessor.consultMoveOperation( var params: TFileSourceConsultParams);
begin
  if params.phase = TFileSourceConsultPhase.target then
    Exit;

  if NOT (fspDirectAccess in params.sourceFS.Properties) then
    Exit;

  if NOT (fspDirectAccess in params.targetFS.Properties) then
    Exit;

  params.resultFS:= params.partnerFS;
  params.resultOperationType:= fsoMove;
  params.operationTemp:= False;
  params.consultResult:= fscrSuccess;
  params.handled:= False;
end;

procedure TSearchResultFileSourceProcessor.consultOperation( var params: TFileSourceConsultParams);
begin
  params.consultResult:= fscrNotSupported;
  params.handled:= True;

  case params.operationType of
    fsoCopy:
      self.consultCopyOperation( params );
    fsoMove:
      self.consultMoveOperation( params );
  end;
end;

procedure TSearchResultFileSourceProcessor.confirmOperation(
  var params: TFileSourceConsultParams);
begin
  case params.operationType of
    fsoCopy: begin
      if fspDirectAccess in params.targetFS.Properties then
        Exit;
      params.files.Path:= params.files[0].Path;
      params.handled:= True;
    end
  end;
end;

constructor TSearchResultFileSource.Create(const displayName: String);
begin
  inherited Create;
  _displayName:= displayName;
end;

function TSearchResultFileSource.GetProcessor: TFileSourceProcessor;
begin
  Result:= searchResultFileSourceProcessor;
end;

function TSearchResultFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + rsSearchResult + PathDelim;
end;

function TSearchResultFileSource.GetRealPath(const path: String): String;
begin
  if self.IsPathAtRoot(path) then
    Result:= GetHomeDir
  else
    Result:= Path;
end;

function TSearchResultFileSource.GetProperties: TFileSourceProperties;
begin
  Result := inherited GetProperties;
  Result -= [fspNoneParent, fspListFlatView, fspSaveableLoadable];
  Result += [fspDontChangePath, fspDontCreateDirectory, fspImmutable, fspSearchable];
  if (fspDirectAccess in Result) then Result+= [fspLinksToLocalFiles];
end;

function TSearchResultFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  // Only Root dir allowed (for flat mode).
  Result := IsPathAtRoot(NewDir);
end;

class function TSearchResultFileSource.CreateFile(const APath: String): TFile;
begin
  Result:= TFileSystemFileSource.CreateFile(APath);
end;

function TSearchResultFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result := TSearchResultListOperation.Create(Self, TargetPath);
end;

function TSearchResultFileSource.GetLocalName(var aFile: TFile): Boolean;
begin
  if (fspLinksToLocalFiles in FileSource.Properties) then
    Result:= FileSource.GetLocalName(aFile)
  else
    Result:= True;
end;

function TSearchResultFileSource.GetCustomIcon(
  const path: String;
  const iconSize: Integer ): TBitmap;
{$IFDEF DARWIN}
const
  ICON_PATH = '$COMMANDER_PATH/pixmaps/macOS/magnifyingglass.png';
var
  iconPath: String;
begin
  iconPath:= mbExpandFileName( ICON_PATH );
  Result:= darwinImageCacheForPath.copyBitmapForFileContent( iconPath, iconSize, True );
end;
{$ELSE}
begin
  Result:= nil;
end;
{$ENDIF}

function TSearchResultFileSource.GetDisplayFileName(aFile: TFile): String;
begin
  if aFile.FullPath = self.GetRootDir() then
    Result:= _displayName
  else
    Result:= aFile.Name;
end;

procedure TSearchResultFileSource.AddSearchPath(
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
  searchResultFileSourceProcessor:= TSearchResultFileSourceProcessor.Create;

finalization
  FreeAndNil( searchResultFileSourceProcessor );

end.

