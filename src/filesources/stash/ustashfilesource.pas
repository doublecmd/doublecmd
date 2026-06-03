unit uStashFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFile, uFileProperty, uFileSourceManager,
  uFileSourceProperty, uFileSourceOperation, uFileSourceOperationTypes,
  uFileSource, uVirtualFileSource, uFileSystemFileSource,
  uFileSourceUtil;

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
  public
    constructor Create; override; overload;

    function GetProcessor: TFileSourceProcessor; override;
    function GetRootDir(sPath : String): String; override;
    function GetProperties: TFileSourceProperties; override;
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetRetrievableFileProperties: TFilePropertiesTypes; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;
    class function CreateFile(const APath: String): TFile; override;
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource; var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource; var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation; override;
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

constructor TStashFileSource.Create;
begin
  Inherited Create;
  _fileSystemFS:= TFileSystemFileSource.Create;
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
  Result:= [fsoList, fsoCopyIn, fsoCopyOut];
end;

class function TStashFileSource.CreateFile(const APath: String): TFile;
begin
  Result:= TFileSystemFileSource.CreateFileFromFile(APath);
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

initialization
  stashFileSourceProcessor:= TStashFileSourceProcessor.Create;

finalization
  FreeAndNil( stashFileSourceProcessor );

end.

