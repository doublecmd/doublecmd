unit uShellSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetFilePropertyOperation,
  uFileSource,
  uFile,
  uFileProperty,
  uShellFileSource,
  uShellFileOperation,
  uShellFileSourceUtil;

type

  { TShellSetFilePropertyOperation }

  TShellSetFilePropertyOperation = class(TFileSourceSetFilePropertyOperation)

  private
    FFileOp: IFileOperation;
    FCurrentFileIndex: Integer;
    FSourceFilesTree: TItemList;
    FShellFileSource: IShellFileSource;
    FStatistics: TFileSourceSetFilePropertyOperationStatistics;

    procedure ShowError(const sMessage: String);
  protected
    function SetNewProperty(aFile: TFile; aTemplateProperty: TFileProperty): TSetFilePropertyResult; override;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theTargetFiles: TFiles;
                       var theNewProperties: TFileProperties); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;

  end;

implementation

uses
  Windows, ActiveX, ShlObj, ComObj, DCConvertEncoding, uShlObjAdditional,
  uFileSourceOperationUI, uShellFolder, uGlobs, uLog, uLng;

procedure TShellSetFilePropertyOperation.ShowError(const sMessage: String);
begin
  if (log_errors in gLogOptions) then
  begin
    logWrite(Thread, sMessage, lmtError);
  end;

  if AskQuestion(sMessage, '', [fsourSkip, fsourAbort],
                 fsourSkip, fsourAbort) = fsourAbort then
  begin
    RaiseAbortOperation;
  end;
end;

constructor TShellSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
                                                      var theTargetFiles: TFiles;
                                                      var theNewProperties: TFileProperties);
begin
  FShellFileSource:= aTargetFileSource as IShellFileSource;
  FFileOp:= CreateComObject(CLSID_FileOperation) as IFileOperation;

  inherited Create(aTargetFileSource, theTargetFiles, theNewProperties);

  // Assign after calling inherited constructor.
  FSupportedProperties := [fpName];
end;

destructor TShellSetFilePropertyOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceFilesTree);
end;

procedure TShellSetFilePropertyOperation.Initialize;
var
  Index: Integer;
  AObject: PItemIDList;
begin
  FStatistics := RetrieveStatistics;

  FSourceFilesTree:= TItemList.Create;
  try
    for Index := 0 to TargetFiles.Count - 1 do
    begin
      AObject:= ILClone(TFileShellProperty(TargetFiles[Index].LinkProperty).Item);
      FSourceFilesTree.Add(AObject);
    end;
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

procedure TShellSetFilePropertyOperation.MainExecute;
var
  aFile: TFile;
  dwCookie: DWORD;
  aTemplateFile: TFile;
  CurrentFileIndex: Integer;
  ASink: TFileOperationProgressSink;
begin
  ASink:= TFileOperationProgressSink.Create(@FStatistics, @UpdateStatistics, @CheckOperationStateSafe);

  FFileOp.SetOperationFlags(FOF_SILENT or FOF_NOCONFIRMMKDIR);

  FFileOp.Advise(ASink, @dwCookie);

  for CurrentFileIndex := 0 to FSourceFilesTree.Count - 1 do
  begin
    FCurrentFileIndex:= CurrentFileIndex;
    AFile:= TargetFiles[FCurrentFileIndex];

    if Assigned(TemplateFiles) and (FCurrentFileIndex < TemplateFiles.Count) then
      aTemplateFile := TemplateFiles[FCurrentFileIndex]
    else
      aTemplateFile := nil;

    SetProperties(FCurrentFileIndex, aFile, aTemplateFile);

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      UpdateStatistics(FStatistics);
    end;

    CheckOperationState;
  end;

  FFileOp.Unadvise(dwCookie);
end;

function TShellSetFilePropertyOperation.SetNewProperty(aFile: TFile;
                                                       aTemplateProperty: TFileProperty): TSetFilePropertyResult;
var
  Res: HRESULT;
  PIDL: PItemIDList;
  AItem: IShellItem;
begin
  Result := sfprSuccess;

  PIDL:= PItemIDList(FSourceFilesTree[FCurrentFileIndex]);

  if Failed(SHCreateItemFromIDList(PIDL, IShellItem, AItem)) then
    Exit(sfprError);

  case aTemplateProperty.GetID of
    fpName:
      begin
        if (aTemplateProperty as TFileNameProperty).Value <> aFile.Name then
        begin
          if not Succeeded(FFileOp.RenameItem(AItem, PWideChar(CeUtf8ToUtf16((aTemplateProperty as TFileNameProperty).Value)), nil)) then
            Result := sfprError
          else begin
            Res:= FFileOp.PerformOperations();
            if Failed(Res) then
            begin
              if Res = COPYENGINE_E_USER_CANCELLED then
                RaiseAbortOperation
              else
                Result := sfprError
            end;
          end;
        end
        else
          Result := sfprSkipped;
      end
    else
      raise Exception.Create('Trying to set unsupported property');
  end;
end;

end.

