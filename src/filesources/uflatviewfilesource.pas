unit uFlatViewFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics,
  uFile, uFileSource, uFileSourceManager, uMultiListFileSource,
  uFileSourceOperationTypes, uFileSourceOperation, uFileSourceProperty
  {$IFDEF DARWIN}
  , uDarwinFile
  {$ENDIF}
  ;

type

  IFlatViewFileSource = interface(IMultiListFileSource)
    ['{1F423A05-91AD-CCE8-0329-8826437DF8D1}']
  end;

  { TFlatViewFileSource }

  TFlatViewFileSource = class(TMultiListFileSource, IFlatViewFileSource)
  protected
    _rootDir: String;
  public
    constructor Create(const rootDir: String);

    function GetProcessor: TFileSourceProcessor; override;

    function GetLocalName(var aFile: TFile): Boolean; override;
    function GetDisplayFileName(aFile: TFile): String; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

    function GetProperties: TFileSourceProperties; override;
    class function CreateFile(const APath: String): TFile; override;
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;

    function GetRootDir(sPath : String): String; override;

    procedure AddSearchPath( const startPath: String; paths: TStringList); override;
  end;

implementation

uses
  uFileSystemFileSource, uFlatViewListOperation;

type

  { TFlatViewFileSourceProcessor }

  TFlatViewFileSourceProcessor = class( TDefaultFileSourceProcessor )
  private
    {
      when copying from FlatView to TargetFileSource, the following needs to be considered:
      1. FlatView only supports copying outwards, not copying to FlatView.
      2. there are two types of FlatView: one is created from a FileSource that
         supports DirectAccess, in which case the FlatView supports DirectAccess;
         the other is created from a Wcx/Zip, in which case the FlatView does not
         support DirectAccess.
      3. there are also two types of TargetFileSource: one that supports DirectAccess
         and one that does not (such as Wcx/Zip).
      therefore, there are a total of 4 combinations:
      1. both FlatView and TargetFileSource support DirectAccess
         TargetFileSource CopyIn / no temp file
      2. FlatView supports DirectAccess, but TargetFileSource does not
         TargetFileSource CopyIn / no temp file
      3. FlatView does not support DirectAccess, but TargetFileSource does
         FlatView CopyOut / no temp file
      4. Neither FlatView nor TargetFileSource supports DirectAccess
         FlatView CopyOut / TargetFileSource CopyIn / Temp file needed
    }
    procedure consultCopyOperation( var params: TFileSourceConsultParams );

    {
      when moving from FlatView to TargetFileSource, the following needs to be considered:
      1. the FlatView created from a Wcx/Zip doesn't support moving files.
         it's limited by DC's philosophy that doesn't support moving files from
         WCX/ZIP files to external.
      2. only the FlatView created from FileSource that supports DirectAccess,
         such as FileSystemFileSource, supports moving files. it only supports moving outwards,
         not moving to FlatView.
      3. only the TargetFileSource that supports DirectAccess supports moving files from
         FlatView. it's also limited by DC's philosophy that doesn't support moving
         files to Wcx/Zip.
      4. therefore, only one type of movement is currently supported: moving from
         FlatView that supports DirectAccess to TargetFileSource that supports DirectAccess.
    }
    procedure consultMoveOperation( var params: TFileSourceConsultParams );
  public
    procedure consultOperation( var params: TFileSourceConsultParams ); override;
    procedure confirmOperation(var params: TFileSourceConsultParams); override;
  end;

var
  flatViewFileSourceProcessor: TFileSourceProcessor;

procedure TFlatViewFileSourceProcessor.consultCopyOperation(
  var params: TFileSourceConsultParams);
var
  targetFS: IFileSource;
begin
  if params.phase = TFileSourceConsultPhase.target then
    Exit;

  targetFS:= params.targetFS;

  if NOT (fspDirectAccess in targetFS.Properties) and
     NOT (fsoCopyIn in targetFS.GetOperationsTypes) then
        Exit;

  params.resultOperationType:= fsoCopyIn;
  params.resultFS:= targetFS;
  params.operationTemp:= False;
  params.consultResult:= fscrSuccess;
  params.handled:= False;
end;

procedure TFlatViewFileSourceProcessor.consultMoveOperation( var params: TFileSourceConsultParams);
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

procedure TFlatViewFileSourceProcessor.consultOperation( var params: TFileSourceConsultParams);
begin
  params.consultResult:= fscrNotSupported;
  params.handled:= True;

  case params.operationType of
    fsoCopy:
      self.consultCopyOperation( params );
    fsoMove:
      self.consultMoveOperation( params );
    else
      Inherited;
  end;
end;

procedure TFlatViewFileSourceProcessor.confirmOperation(
  var params: TFileSourceConsultParams);
begin
  params.files.setPathBaseOnAllFiles;
end;

{ TFlatViewFileSource }

constructor TFlatViewFileSource.Create(const rootDir: String);
begin
  Inherited Create;
  _rootDir:= IncludeTrailingPathDelimiter(rootDir);
end;

function TFlatViewFileSource.GetProcessor: TFileSourceProcessor;
begin
  Result:= flatViewFileSourceProcessor;
end;

function TFlatViewFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result := TFlatViewListOperation.Create(Self, TargetPath);
end;

function TFlatViewFileSource.GetProperties: TFileSourceProperties;
begin
  Result := inherited GetProperties;
  Result -= [fspNoneParent, fspListFlatView, fspSaveableLoadable, fspSearchable];
  Result += [fspDontChangePath, fspDontCreateDirectory, fspImmutable];
  if (fspDirectAccess in Result) then
    Result+= [fspLinksToLocalFiles, fspSearchable];
end;

class function TFlatViewFileSource.CreateFile(const APath: String): TFile;
begin
  Result:= TFileSystemFileSource.CreateFile(APath);
end;

function TFlatViewFileSource.GetLocalName(var aFile: TFile): Boolean;
begin
  if (fspLinksToLocalFiles in FileSource.Properties) then
    Result:= FileSource.GetLocalName(aFile)
  else
    Result:= True;
end;

function TFlatViewFileSource.GetDisplayFileName(aFile: TFile): String;
begin
  Result:= FileSource.GetDisplayFileName(aFile);
end;

function TFlatViewFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  // Only Root dir allowed (for flat mode).
  Result := IsPathAtRoot(NewDir);
end;

function TFlatViewFileSource.GetRootDir(sPath: String): String;
begin
  Result:= _rootDir;
end;

procedure TFlatViewFileSource.AddSearchPath(
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
  flatViewFileSourceProcessor:= TFlatViewFileSourceProcessor.Create;

finalization
  FreeAndNil( flatViewFileSourceProcessor );

end.

