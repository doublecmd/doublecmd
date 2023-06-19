unit uShellCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Windows, ShlObj, ComObj,
  uFileSourceOperation,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperationTypes,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI,
  uFile,
  uShellFileSource,
  uShellFileOperation,
  uShellFileSourceUtil;

type

  { TShellCopyOperation }

  TShellCopyOperation = class(TFileSourceCopyOperation)
  protected
    FFileOp: IFileOperation;
    FTargetFolder: IShellItem;
    FSourceFilesTree: TItemList;
    FShellFileSource: IShellFileSource;
    FStatistics: TFileSourceCopyOperationStatistics;

    procedure ShowError(const sMessage: String);
  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
  end;

  { TShellCopyInOperation }

  TShellCopyInOperation = class(TShellCopyOperation)
  protected
    function GetID: TFileSourceOperationType; override;
  public
    procedure Initialize; override;
  end;

  { TShellCopyOutOperation }

  TShellCopyOutOperation = class(TShellCopyOperation)
  protected
    function GetID: TFileSourceOperationType; override;
  end;

implementation

uses
  ActiveX, DCConvertEncoding, uFileSourceOperationUI, uShlObjAdditional,
  uShellFolder, uGlobs, uLog;

procedure TShellCopyOperation.ShowError(const sMessage: String);
begin
  if (log_cp_mv_ln in gLogOptions) and (log_errors in gLogOptions) then
  begin
    logWrite(Thread, sMessage, lmtError);
  end;

  if AskQuestion(sMessage, '', [fsourSkip, fsourAbort],
                 fsourSkip, fsourAbort) = fsourAbort then
  begin
    RaiseAbortOperation;
  end;
end;

constructor TShellCopyOperation.Create(aSourceFileSource: IFileSource;
                                            aTargetFileSource: IFileSource;
                                            var theSourceFiles: TFiles;
                                            aTargetPath: String);
begin
  case GetID of
    fsoCopy, fsoCopyOut:
      FShellFileSource:= aSourceFileSource as IShellFileSource;
    fsoCopyIn:
      FShellFileSource:= aTargetFileSource as IShellFileSource;
  end;
  FFileOp:= CreateComObject(CLSID_FileOperation) as IFileOperation;
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TShellCopyOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceFilesTree);
end;

procedure TShellCopyOperation.Initialize;
var
  Index: Integer;
  AObject: PItemIDList;
  AFolder: IShellFolder2;
begin
  FStatistics := RetrieveStatistics;

  FSourceFilesTree:= TItemList.Create;
  try
    for Index := 0 to SourceFiles.Count - 1 do
    begin
      AObject:= ILClone(TFileShellProperty(SourceFiles[Index].LinkProperty).Item);
      FSourceFilesTree.Add(AObject);
    end;
    case GetID of
      fsoCopy:
      begin
        OleCheck(FShellFileSource.FindFolder(TargetPath, AFolder));
        OleCheck(SHGetIDListFromObject(AFolder, AObject));
        try
          OleCheck(SHCreateItemFromIDList(AObject, IShellItem, FTargetFolder));
        finally
          CoTaskMemFree(AObject);
        end;
      end;
      fsoCopyOut:
        OleCheck(SHCreateItemFromParsingName(PWideChar(CeUtf8ToUtf16(TargetPath)), nil, IShellItem, FTargetFolder));
    end;
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

procedure TShellCopyOperation.MainExecute;
var
  Res: HRESULT;
  dwCookie: DWORD;
  siItemArray: IShellItemArray;
  ASink: TFileOperationProgressSink;
begin
  ASink:= TFileOperationProgressSink.Create(@FStatistics, @UpdateStatistics, @CheckOperationStateSafe);

  FFileOp.SetOperationFlags(FOF_SILENT or FOF_NOCONFIRMMKDIR);

  try
    FFileOp.Advise(ASink, @dwCookie);
    try
      OleCheck(SHCreateShellItemArrayFromIDLists(FSourceFilesTree.Count, PPItemIDList(FSourceFilesTree.List), siItemArray));
      OleCheck(FFileOp.CopyItems(siItemArray, FTargetFolder));
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

{ TShellCopyInOperation }

function TShellCopyInOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoCopyIn;
end;

procedure TShellCopyInOperation.Initialize;
var
  aFile: TFile;
  Index: Integer;
  AObject: PItemIDList;
  AFolder: IShellFolder2;
begin
  FStatistics := RetrieveStatistics;

  FSourceFilesTree:= TItemList.Create;
  try
    for Index := 0 to SourceFiles.Count - 1 do
    begin
      aFile := SourceFiles[Index];
      AObject:= ILCreateFromPathW(PWideChar(CeUtf8ToUtf16(aFile.FullPath)));
      FSourceFilesTree.Add(AObject);
    end;
    OleCheck(FShellFileSource.FindFolder(TargetPath, AFolder));
    OleCheck(SHGetIDListFromObject(AFolder, AObject));
    OleCheck(SHCreateItemFromIDList(AObject, IShellItem, FTargetFolder));
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

{ TShellCopyOutOperation }

function TShellCopyOutOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoCopyOut;
end;

end.

