unit uFileSourceManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource, uFileSourceOperationTypes, uFileSourceUtil,
  uDebug, DCStrUtils;

type
  { TFileSourceManager }

  TFileSourceManager = class
  private
    FFileSources: TFileSources;
  public
    // Only allow adding and removing to/from Manager by TFileSource constructor and destructor.
    procedure Add(aFileSource: IFileSource);
    procedure Remove(aFileSource: IFileSource);

    constructor Create;
    destructor Destroy; override;

    function Find(FileSourceClass: TClass; Address: String; CaseSensitive: Boolean = True): IFileSource;

    procedure consultOperation( var params: TFileSourceConsultParams );
    procedure confirmOperation( var params: TFileSourceConsultParams );
  end;

  { TDefaultFileSourceProcessor }

  TDefaultFileSourceProcessor = class( TFileSourceProcessor )
  private
    procedure consultCopyOperation( var params: TFileSourceConsultParams );
    procedure confirmCopyOperation( var params: TFileSourceConsultParams );
    procedure consultMoveOperation( var params: TFileSourceConsultParams );
  public
    procedure consultOperation( var params: TFileSourceConsultParams ); override;
    procedure confirmOperation( var params: TFileSourceConsultParams ); override;
  end;

var
  FileSourceManager: TFileSourceManager;

implementation

{ TFileSourceManager }

constructor TFileSourceManager.Create;
begin
  FFileSources := TFileSources.Create;
end;

destructor TFileSourceManager.Destroy;
var
  i: Integer;
begin
  if FFileSources.Count > 0 then
  begin
    DCDebug('Warning: Destroying manager with existing file sources!');

    for i := 0 to FFileSources.Count - 1 do
    begin
      // Restore the reference taken in TFileSource.Create before removing
      // all file sources from the list.
      FFileSources[i]._AddRef;
      // Free instance.
      FFileSources[i]:= nil;
    end;
  end;

  FreeAndNil(FFileSources);

  inherited Destroy;
end;

procedure TFileSourceManager.Add(aFileSource: IFileSource);
begin
  if FFileSources.IndexOf(aFileSource) < 0 then
  begin
    FFileSources.Add(aFileSource);
  end
  else
    DCDebug('Error: File source already exists in manager!');
end;

procedure TFileSourceManager.Remove(aFileSource: IFileSource);
begin
  FFileSources.Remove(aFileSource);
end;

function TFileSourceManager.Find(FileSourceClass: TClass; Address: String;
  CaseSensitive: Boolean): IFileSource;
var
  I: Integer;
  StrCmp: function(const S1, S2: String): Integer;
begin
  if CaseSensitive then
    StrCmp:= @CompareStr
  else begin
    StrCmp:= @CompareText;
  end;
  for I := 0 to FFileSources.Count - 1 do
  begin
    if (FFileSources[I].IsClass(FileSourceClass)) and
       (StrCmp(FFileSources[I].CurrentAddress, Address) = 0) then
    begin
      Result := FFileSources[I];
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TFileSourceManager.consultOperation( var params: TFileSourceConsultParams);
var
  fs: IFileSource;
  processor: TFileSourceProcessor;
begin
  params.consultResult:= fscrSuccess;
  params.resultOperationType:= params.operationType;
  params.resultTargetPath:= params.targetPath;
  params.handled:= False;

  fs:= params.sourceFS;
  params.currentFS:= fs;
  params.partnerFS:= params.targetFS;
  processor:= fs.GetProcessor;
  if processor <> nil then
    processor.consultOperation( params );

  if params.handled then
    Exit;

  if params.targetFS = nil then
    Exit;

  fs:= params.targetFS;
  params.currentFS:= fs;
  params.partnerFS:= params.sourceFS;
  processor:= fs.GetProcessor;
  if processor <> nil then
    processor.consultOperation( params );
end;

procedure TFileSourceManager.confirmOperation(var params: TFileSourceConsultParams);
var
  fs: IFileSource;
  processor: TFileSourceProcessor;
begin
  params.resultTargetPath:= params.targetPath;
  params.handled:= False;

  fs:= params.sourceFS;
  params.currentFS:= fs;
  params.partnerFS:= params.targetFS;
  processor:= fs.GetProcessor;
  if processor <> nil then
    processor.confirmOperation( params );

  if params.handled then
    Exit;

  if params.targetFS = nil then
    Exit;

  fs:= params.targetFS;
  params.currentFS:= fs;
  params.partnerFS:= params.sourceFS;
  processor:= fs.GetProcessor;
  if processor <> nil then
    processor.confirmOperation( params );
end;

{ TDefaultFileSourceProcessor }

procedure TDefaultFileSourceProcessor.consultCopyOperation( var params: TFileSourceConsultParams );
var
  sourceFS: IFileSource;
  targetFS: IFileSource;
begin
  if params.currentFS <> params.sourceFS then
    Exit;

  sourceFS:= params.sourceFS;
  targetFS:= params.targetFS;

  // If same file source and address
  if isCompatibleFileSourceForCopyOperation( sourceFS, targetFS ) then begin
    params.resultFS:= params.sourceFS;
  end else if (fsoCopyOut in sourceFS.GetOperationsTypes) and (fsoCopyIn in targetFS.GetOperationsTypes) then begin
    params.resultOperationType:= fsoCopyOut;
    params.operationTemp:= True;
    params.resultFS:= params.sourceFS;
  end else begin
    params.consultResult:= fscrNotSupported;
  end;
end;

procedure TDefaultFileSourceProcessor.confirmCopyOperation( var params: TFileSourceConsultParams );
begin
  if params.currentFS <> params.sourceFS then
    Exit;

  if NOT StrBegins(params.targetPath, '..') then
    Exit;

  params.resultOperationType:= fsoCopy;
  params.operationTemp:= False;
  params.resultFS:= params.sourceFS;
  params.handled:= True;
end;

procedure TDefaultFileSourceProcessor.consultMoveOperation( var params: TFileSourceConsultParams);
var
  sourceFS: IFileSource;
  targetFS: IFileSource;
begin
  if params.currentFS <> params.sourceFS then
    Exit;

  sourceFS:= params.sourceFS;
  targetFS:= params.targetFS;

  if (sourceFS.IsInterface(targetFS) or
      targetFS.IsInterface(sourceFS)) and
     (sourceFS.CurrentAddress = targetFS.CurrentAddress) and
     (fsoMove in sourceFS.GetOperationsTypes) and
     (fsoMove in targetFS.GetOperationsTypes) then
  begin
    params.consultResult:= fscrSuccess;
  end
  else if ((fsoCopyOut in sourceFS.GetOperationsTypes) and
           (fsoCopyIn in targetFS.GetOperationsTypes)) then
  begin
    params.consultResult:= fscrNotImplemented;
  end
  else
  begin
    params.consultResult:= fscrNotSupported;
  end;
end;

procedure TDefaultFileSourceProcessor.consultOperation( var params: TFileSourceConsultParams );
begin
  case params.operationType of
    fsoCopy:
      self.consultCopyOperation( params );
    fsoMove:
      self.consultMoveOperation( params );
  end;
end;

procedure TDefaultFileSourceProcessor.confirmOperation( var params: TFileSourceConsultParams );
begin
  case params.operationType of
    fsoCopy:
      self.confirmCopyOperation( params );
  end;
end;

initialization
  FileSourceManager := TFileSourceManager.Create;
  defaultFileSourceProcessor:= TDefaultFileSourceProcessor.Create;

finalization
  FreeAndNil(FileSourceManager);
  FreeAndNil(defaultFileSourceProcessor);

end.

