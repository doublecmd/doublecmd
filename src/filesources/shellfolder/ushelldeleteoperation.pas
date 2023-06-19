unit uShellDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Windows, ShlObj, ComObj,
  uFileSourceDeleteOperation,
  uShellFileSource,
  uFileSource,
  uShellFileOperation,
  uShellFileSourceUtil,
  uFileSourceOperationUI,
  uFile,
  uGlobs, uLog;

type

  { TShellDeleteOperation }

  TShellDeleteOperation = class(TFileSourceDeleteOperation)
  protected
    FFileOp: IFileOperation;
    FSourceFilesTree: TItemList;
    FShellFileSource: IShellFileSource;
    FStatistics: TFileSourceDeleteOperationStatistics;

    procedure ShowError(const sMessage: String);
  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToDelete: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  DCOSUtils, uLng, uShellFolder, uShlObjAdditional;

procedure TShellDeleteOperation.ShowError(const sMessage: String);
begin
  if (log_errors in gLogOptions) and (log_delete in gLogOptions) then
  begin
    logWrite(Thread, sMessage, lmtError);
  end;

  if AskQuestion(sMessage, '', [fsourSkip, fsourAbort],
                 fsourSkip, fsourAbort) = fsourAbort then
  begin
    RaiseAbortOperation;
  end;
end;

constructor TShellDeleteOperation.Create(aTargetFileSource: IFileSource;
                                             var theFilesToDelete: TFiles);
begin
  FShellFileSource:= aTargetFileSource as IShellFileSource;
  FFileOp:= CreateComObject(CLSID_FileOperation) as IFileOperation;
  inherited Create(aTargetFileSource, theFilesToDelete);
end;

destructor TShellDeleteOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceFilesTree);
end;

procedure TShellDeleteOperation.Initialize;
var
  Index: Integer;
  AObject: PItemIDList;
begin
  FStatistics := RetrieveStatistics;

  FSourceFilesTree:= TItemList.Create;

  try
    for Index := 0 to FilesToDelete.Count - 1 do
    begin
      AObject:= ILClone(TFileShellProperty(FilesToDelete[Index].LinkProperty).Item);
      FSourceFilesTree.Add(AObject);
    end;
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

procedure TShellDeleteOperation.MainExecute;
var
  Res: HRESULT;
  dwCookie: DWORD;
  siItemArray: IShellItemArray;
  ASink: TFileOperationProgressSink;
begin
  ASink:= TFileOperationProgressSink.Create(@FStatistics, @UpdateStatistics, @CheckOperationStateSafe);

  FFileOp.SetOperationFlags(FOF_SILENT or FOF_NOCONFIRMATION or FOF_NORECURSION);
  try
    FFileOp.Advise(ASink, @dwCookie);
    try
      OleCheck(SHCreateShellItemArrayFromIDLists(FSourceFilesTree.Count, PPItemIDList(FSourceFilesTree.List), siItemArray));
      OleCheck(FFileOp.DeleteItems(siItemArray));
      Res:= FFileOp.PerformOperations;
      if Failed(Res) then
      begin
        if Res = COPYENGINE_E_USER_CANCELLED then
          RaiseAbortOperation
        else
          OleError(Res);
      end;
    finally
      FFileOp.Unadvise(dwCookie);
    end;
  except
    on E: EOleError do ShowError(E.Message);
  end;
end;

end.

