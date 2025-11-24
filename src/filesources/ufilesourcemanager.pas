unit uFileSourceManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource, uFileSourceOperationTypes, uFileSourceUtil,
  uDebug, DCStrUtils;

function FileSourceManager_Find(FileSourceClass: TClass; Address: String; CaseSensitive: Boolean = True): IFileSource;

procedure FileSourceManager_consultOperation( var params: TFileSourceConsultParams );
procedure FileSourceManager_confirmOperation( var params: TFileSourceConsultParams );

type
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
  AllFileSourceObjects: TFPList;// item type is TFileSource
  AllFileSourceObjectsCriticalSection: TRTLCriticalSection;

implementation

function FileSourceManager_Find(FileSourceClass: TClass; Address: String;
  CaseSensitive: Boolean): IFileSource;
type
  TFileSourcePointer = ^TFileSource;
var
  StrCmp: function(const S1, S2: String): Integer;
  fl: TFileSourcePointer;
  i: Integer;
  f: TFileSource;
  SilentResult: Pointer absolute Result;// no auto reference counting on assign!
begin
  if CaseSensitive then
    StrCmp:= @CompareStr
  else begin
    StrCmp:= @CompareText;
  end;
  SilentResult:= nil;

  EnterCriticalSection(AllFileSourceObjectsCriticalSection);
  try
    fl:= TFileSourcePointer(AllFileSourceObjects.List);
    for i:= 0 to AllFileSourceObjects.Count-1 do
    begin
      f:= fl[i];
      // (f.RefCount<>0) check to avoid finding objects that:
      //   1. constructed (after AfterConstruction) but didn't yet gain a reference
      // or
      //   2. lost all references but not yet called BeforeDestruction
      if (f.RefCount<>0) and (f is FileSourceClass) and (StrCmp(f.CurrentAddress, Address)=0) then
      begin
        // inc(f.RefCount) instead of f._AddRef to not EnterCriticalSection(AllFileSourceObjectsCriticalSection) again
        inc(f.RefCount);
        // IFileSource(f) doesn't call anything
        SilentResult:= IFileSource(f);
        Break;
      end;
    end;
  finally
    LeaveCriticalSection(AllFileSourceObjectsCriticalSection);
  end;
end;

procedure FileSourceManager_consultOperation( var params: TFileSourceConsultParams);
var
  fs: IFileSource;
  processor: TFileSourceProcessor;
begin
  params.consultResult:= fscrSuccess;
  params.resultOperationType:= params.operationType;
  params.resultTargetPath:= params.targetPath;
  params.handled:= False;

  fs:= params.sourceFS;
  params.phase:= TFileSourceConsultPhase.source;
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
  params.phase:= TFileSourceConsultPhase.target;
  params.currentFS:= fs;
  params.partnerFS:= params.sourceFS;
  processor:= fs.GetProcessor;
  if processor <> nil then
    processor.consultOperation( params );
end;

procedure FileSourceManager_confirmOperation(var params: TFileSourceConsultParams);
var
  fs: IFileSource;
  processor: TFileSourceProcessor;
begin
  params.resultTargetPath:= params.targetPath;
  params.handled:= False;

  fs:= params.sourceFS;
  params.phase:= TFileSourceConsultPhase.source;
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
  params.phase:= TFileSourceConsultPhase.target;
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
  if params.phase<>TFileSourceConsultPhase.source then
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
  if params.phase<>TFileSourceConsultPhase.source then
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
  if params.phase<>TFileSourceConsultPhase.source then
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
  AllFileSourceObjects:= TFPList.Create;
  InitCriticalSection(AllFileSourceObjectsCriticalSection);

finalization
  if AllFileSourceObjects.Count <> 0 then
    DCDebug('Warning: AllFileSourceObjects has existing file sources!');

end.

