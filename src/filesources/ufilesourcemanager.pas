unit uFileSourceManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource, uFileSourceOperationTypes,
  uDebug;

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
    procedure consultBeforeOperate( var params: TFileSourceConsultParams );
  end;

  { TDefaultFileSourceProcessor }

  TDefaultFileSourceProcessor = class( TFileSourceProcessor )
    procedure consultBeforeOperate( var params: TFileSourceConsultParams ); override;
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

procedure TFileSourceManager.consultBeforeOperate( var params: TFileSourceConsultParams);
var
  fs: IFileSource;
  processor: TFileSourceProcessor;
begin
  params.consultResult:= fscrSuccess;
  params.handled:= False;

  fs:= params.sourceFS;
  params.currentFS:= fs;
  params.partnerFS:= params.targetFS;
  processor:= fs.GetProcessor;
  if processor <> nil then
    processor.consultBeforeOperate( params );

  if (params.consultResult<>fscrSuccess) or params.handled then
    Exit;

  if params.targetFS = nil then
    Exit;

  fs:= params.targetFS;
  params.currentFS:= fs;
  params.partnerFS:= params.sourceFS;
  processor:= fs.GetProcessor;
  if processor <> nil then
    processor.consultBeforeOperate( params );
end;

{ TDefaultFileSourceProcessor }

procedure TDefaultFileSourceProcessor.consultBeforeOperate( var params: TFileSourceConsultParams );
var
  sourceFS: IFileSource;
  targetFS: IFileSource;
begin
  if params.operationType <> fsoMove then
    Exit;

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

initialization
  FileSourceManager := TFileSourceManager.Create;
  defaultFileSourceProcessor:= TDefaultFileSourceProcessor.Create;

finalization
  FreeAndNil(FileSourceManager);
  FreeAndNil(defaultFileSourceProcessor);

end.

