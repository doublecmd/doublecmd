unit uShellFileSourceUtil;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  Windows, ActiveX, ShlObj, ComObj, ShlWAPI, ShellAPI,
  uShellFolder, uShellFileOperation, uFileSourceCopyOperation,
  uFileSourceDeleteOperation, uFileSourceSetFilePropertyOperation, uGlobs, uLog;

type

  { TItemList }

  TItemList = class(TFPList)
  public
    destructor Destroy; override;
  end;

  TUpdateCopyStatisticsFunction = procedure(var NewStatistics: TFileSourceCopyOperationStatistics) of object;
  TUpdateDeleteStatisticsFunction = procedure(var NewStatistics: TFileSourceDeleteOperationStatistics) of object;
  TUpdateSetFilePropertyStatisticsFunction = procedure(var NewStatistics: TFileSourceSetFilePropertyOperationStatistics) of object;

  { TFileOperationProgressSink }

  TFileOperationProgressSink = class(TInterfacedObject, IFileOperationProgressSink)
  private
    FCopyStatistics: PFileSourceCopyOperationStatistics;
    FUpdateCopyStatistics: TUpdateCopyStatisticsFunction;
    FDeleteStatistics: PFileSourceDeleteOperationStatistics;
    FUpdateDeleteStatistics: TUpdateDeleteStatisticsFunction;
    FUpdateSetFilePropertyStatistics: TUpdateSetFilePropertyStatisticsFunction;
    FSetFilePropertyStatistics: PFileSourceSetFilePropertyOperationStatistics;
  protected
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
  public
    constructor Create(AStatistics: PFileSourceCopyOperationStatistics; AUpdateStatistics: TUpdateCopyStatisticsFunction); reintroduce; overload;
    constructor Create(AStatistics: PFileSourceDeleteOperationStatistics; AUpdateStatistics: TUpdateDeleteStatisticsFunction); reintroduce; overload;
    constructor Create(AStatistics: PFileSourceSetFilePropertyOperationStatistics; AUpdateStatistics: TUpdateSetFilePropertyStatisticsFunction); reintroduce; overload;
  public
    function StartOperations: HResult; stdcall;
    function FinishOperations(hrResult: HResult): HResult; stdcall;
    function PreRenameItem(dwFlags: DWORD; psiItem: IShellItem; pszNewName: LPCWSTR): HResult; stdcall;
    function PostRenameItem(dwFlags: DWORD; psiItem: IShellItem; pszNewName: LPCWSTR; hrRename: HRESULT; psiNewlyCreated: IShellItem): HResult; stdcall;
    function PreMoveItem(dwFlags: DWORD; psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR): HResult; stdcall;
    function PostMoveItem(dwFlags: DWORD; psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR; hrMove: HRESULT; psiNewlyCreated: IShellItem): HResult; stdcall;
    function PreCopyItem(dwFlags: DWORD; psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR): HResult; stdcall;
    function PostCopyItem(dwFlags: DWORD; psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR; hrCopy: HRESULT; psiNewlyCreated: IShellItem): HResult; stdcall;
    function PreDeleteItem(dwFlags: DWORD; psiItem: IShellItem): HResult; stdcall;
    function PostDeleteItem(dwFlags: DWORD; psiItem: IShellItem; hrDelete: HRESULT; psiNewlyCreated: IShellItem): HResult; stdcall;
    function PreNewItem(dwFlags: DWORD; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR): HResult; stdcall;
    function PostNewItem(dwFlags: DWORD; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR; pszTemplateName: LPCWSTR; dwFileAttributes: DWORD; hrNew: HRESULT; psiNewItem: IShellItem): HResult; stdcall;
    function UpdateProgress(iWorkTotal: UINT; iWorkSoFar: UINT): HResult; stdcall;
    function ResetTimer: HResult; stdcall;
    function PauseTimer: HResult; stdcall;
    function ResumeTimer: HResult; stdcall;
  end;

procedure CheckObject(Result: HResult; const AName: String); inline;

var
  SHCreateItemWithParent: function(pidlParent: PCIDLIST_ABSOLUTE; psfParent: IShellFolder;
                                   pidl: PCUITEMID_CHILD; const riid: REFIID; out ppvItem): HRESULT; stdcall;

  SHGetIDListFromObject: function(punk: IUnknown; out ppidl): HRESULT; stdcall;

  SHCreateItemFromIDList: function(pidl: PItemIDList; const riid: REFIID; out ppv): HRESULT; stdcall;

  SHCreateItemFromParsingName: function(pszPath: LPCWSTR; const pbc: IBindCtx;
                                        const riid: TIID; out ppv): HRESULT; stdcall;

  SHCreateShellItemArray: function(pidlParent: PCIDLIST_ABSOLUTE; psf: IShellFolder;
                                   cidl: UINT; ppidl: PPItemIDList; out ppsiItemArray): HRESULT; stdcall;

  SHCreateShellItemArrayFromIDLists: function(cidl: UINT; rgpidl: PPItemIDList; out ppsiItemArray): HRESULT; stdcall;

implementation

uses
  DCOSUtils, DCConvertEncoding, uLng;

var
  AModule: HMODULE;

procedure CheckObject(Result: HResult; const AName: String);
begin
  if Failed(Result) then
  begin
    raise EOleError.Create(mbSysErrorMessage(Result) + LineEnding + AName);
  end;
end;

{ TItemList }

destructor TItemList.Destroy;
var
  AItem: PItemIDList;
begin
  for AItem in Self do
  begin
    CoTaskMemFree(AItem);
  end;
  inherited Destroy;
end;

{ TFileOperationProgressSink }

procedure TFileOperationProgressSink.LogMessage(sMessage: String;
  logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:
      if not (log_errors in gLogOptions) then Exit;
    lmtInfo:
      if not (log_info in gLogOptions) then Exit;
    lmtSuccess:
      if not (log_success in gLogOptions) then Exit;
  end;

  if logOptions <= gLogOptions then
  begin
    logWrite(nil, sMessage, logMsgType);
  end;
end;

constructor TFileOperationProgressSink.Create(
  AStatistics: PFileSourceCopyOperationStatistics;
  AUpdateStatistics: TUpdateCopyStatisticsFunction);
begin
  FCopyStatistics:= AStatistics;
  FUpdateCopyStatistics:= AUpdateStatistics;
end;

constructor TFileOperationProgressSink.Create(
  AStatistics: PFileSourceDeleteOperationStatistics;
  AUpdateStatistics: TUpdateDeleteStatisticsFunction);
begin
  FDeleteStatistics:= AStatistics;
  FUpdateDeleteStatistics:= AUpdateStatistics;
end;

constructor TFileOperationProgressSink.Create(
  AStatistics: PFileSourceSetFilePropertyOperationStatistics;
  AUpdateStatistics: TUpdateSetFilePropertyStatisticsFunction);
begin
  FSetFilePropertyStatistics:= AStatistics;
  FUpdateSetFilePropertyStatistics:= AUpdateStatistics;
end;

function TFileOperationProgressSink.StartOperations: HResult; stdcall;
begin
  Result:= S_OK;
end;

function TFileOperationProgressSink.FinishOperations(hrResult: HResult
  ): HResult; stdcall;
begin
  Result:= S_OK;
end;

function TFileOperationProgressSink.PreRenameItem(dwFlags: DWORD;
  psiItem: IShellItem; pszNewName: LPCWSTR): HResult; stdcall;
var
  AFileName: PWideChar;
begin
  if Succeeded(psiItem.GetDisplayName(SIGDN(SIGDN_DESKTOPABSOLUTEEDITING), @AFileName)) then
  begin
    FSetFilePropertyStatistics^.CurrentFile:= CeUtf16ToUtf8(AFileName);
    CoTaskMemFree(AFileName);
  end;
  Result:= S_OK;
end;

function TFileOperationProgressSink.PostRenameItem(dwFlags: DWORD;
  psiItem: IShellItem; pszNewName: LPCWSTR; hrRename: HRESULT;
  psiNewlyCreated: IShellItem): HResult; stdcall;
begin
  Result:= S_OK;
end;

function TFileOperationProgressSink.PreMoveItem(dwFlags: DWORD;
  psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR
  ): HResult; stdcall;
begin
  Result:= PreCopyItem(dwFlags, psiItem, psiDestinationFolder, pszNewName);
end;

function TFileOperationProgressSink.PostMoveItem(dwFlags: DWORD;
  psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR;
  hrMove: HRESULT; psiNewlyCreated: IShellItem): HResult; stdcall;
begin
  if (log_cp_mv_ln in gLogOptions) then
  begin
    with FCopyStatistics^ do
    begin
      if Succeeded(hrMove) then
      begin
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogMove, [CurrentFileFrom + ' -> ' + CurrentFileTo]),
                   [log_cp_mv_ln], lmtSuccess);
      end
      else begin
        LogMessage(Format(rsMsgLogError + rsMsgLogMove, [CurrentFileFrom + ' -> ' + CurrentFileTo]),
                   [log_cp_mv_ln], lmtError);
      end;
    end;
  end;
  Result:= S_OK;
end;

function TFileOperationProgressSink.PreCopyItem(dwFlags: DWORD;
  psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR
  ): HResult; stdcall;
var
  AFileName: PWideChar;
begin
  if Succeeded(psiItem.GetDisplayName(SIGDN(SIGDN_DESKTOPABSOLUTEEDITING), @AFileName)) then
  begin
    FCopyStatistics^.CurrentFileFrom:= CeUtf16ToUtf8(AFileName);
    CoTaskMemFree(AFileName);
  end;
  if Succeeded(psiDestinationFolder.GetDisplayName(SIGDN(SIGDN_DESKTOPABSOLUTEEDITING), @AFileName)) then
  begin
    with FCopyStatistics^ do
    begin
      CurrentFileTo:= CeUtf16ToUtf8(AFileName);
      CoTaskMemFree(AFileName);

      if Assigned(pszNewName) and (pszNewName^ <> #0) then
        CurrentFileTo:= CurrentFileTo + CeUtf16ToUtf8(pszNewName)
      else begin
        CurrentFileTo:= CurrentFileTo + ExtractFileName(CurrentFileFrom);
      end;
    end;
  end;
  FUpdateCopyStatistics(FCopyStatistics^);
  Result:= S_OK;
end;

function TFileOperationProgressSink.PostCopyItem(dwFlags: DWORD;
  psiItem: IShellItem; psiDestinationFolder: IShellItem; pszNewName: LPCWSTR;
  hrCopy: HRESULT; psiNewlyCreated: IShellItem): HResult; stdcall;
begin
  if (log_cp_mv_ln in gLogOptions) then
  begin
    with FCopyStatistics^ do
    begin
      if Succeeded(hrCopy) then
      begin
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogCopy, [CurrentFileFrom + ' -> ' + CurrentFileTo]),
                   [log_cp_mv_ln], lmtSuccess);
      end
      else begin
        LogMessage(Format(rsMsgLogError + rsMsgLogCopy, [CurrentFileFrom + ' -> ' + CurrentFileTo]),
                   [log_cp_mv_ln], lmtError);
      end;
    end;
  end;
  Result:= S_OK;
end;

function TFileOperationProgressSink.PreDeleteItem(dwFlags: DWORD;
  psiItem: IShellItem): HResult; stdcall;
var
  AFileName: PWideChar;
begin
  if Succeeded(psiItem.GetDisplayName(SIGDN(SIGDN_DESKTOPABSOLUTEEDITING), @AFileName)) then
  begin
    FDeleteStatistics^.CurrentFile:= CeUtf16ToUtf8(AFileName);
    CoTaskMemFree(AFileName);
  end;
  Result:= S_OK;
end;

function TFileOperationProgressSink.PostDeleteItem(dwFlags: DWORD;
  psiItem: IShellItem; hrDelete: HRESULT; psiNewlyCreated: IShellItem
  ): HResult; stdcall;
var
  AText: String;
  sfgaoAttribs: SFGAOF = 0;
begin
  if log_delete in gLogOptions then
  begin
    psiItem.GetAttributes(SFGAO_FOLDER, @sfgaoAttribs);
    if (sfgaoAttribs and SFGAO_FOLDER) = 0 then
      AText:= rsMsgLogDelete
    else begin
      AText:= rsMsgLogRmDir;
    end;
    with FDeleteStatistics^ do
    begin
      if Succeeded(hrDelete) then
      begin
        LogMessage(Format(rsMsgLogSuccess + AText, [CurrentFile]),
                   [log_delete], lmtSuccess);
      end
      else begin
        LogMessage(Format(rsMsgLogError + AText, [CurrentFile]),
                   [log_delete], lmtError);
      end;
    end;
  end;
  Result:= S_OK;
end;

function TFileOperationProgressSink.PreNewItem(dwFlags: DWORD;
  psiDestinationFolder: IShellItem; pszNewName: LPCWSTR): HResult; stdcall;
begin
  Result:= S_OK;
end;

function TFileOperationProgressSink.PostNewItem(dwFlags: DWORD;
  psiDestinationFolder: IShellItem; pszNewName: LPCWSTR;
  pszTemplateName: LPCWSTR; dwFileAttributes: DWORD; hrNew: HRESULT;
  psiNewItem: IShellItem): HResult; stdcall;
begin
  Result:= S_OK;
end;

function TFileOperationProgressSink.UpdateProgress(iWorkTotal: UINT;
  iWorkSoFar: UINT): HResult; stdcall;
begin
  if Assigned(FCopyStatistics) then
  begin
    FCopyStatistics^.TotalBytes:= iWorkTotal;
    FCopyStatistics^.DoneBytes:= iWorkSoFar;

    FUpdateCopyStatistics(FCopyStatistics^);
  end
  else if Assigned(FDeleteStatistics) then
  begin
    FDeleteStatistics^.TotalFiles:= iWorkTotal;
    FDeleteStatistics^.DoneFiles:= iWorkSoFar;

    FUpdateDeleteStatistics(FDeleteStatistics^);
  end
  else if Assigned(FSetFilePropertyStatistics) then
  begin
    FSetFilePropertyStatistics^.TotalFiles:= iWorkTotal;
    FSetFilePropertyStatistics^.DoneFiles:= iWorkSoFar;

    FUpdateSetFilePropertyStatistics(FSetFilePropertyStatistics^);
  end;

  Result:= S_OK;
end;

function TFileOperationProgressSink.ResetTimer: HResult; stdcall;
begin
  Result:= S_OK;
end;

function TFileOperationProgressSink.PauseTimer: HResult; stdcall;
begin
  Result:= S_OK;
end;

function TFileOperationProgressSink.ResumeTimer: HResult; stdcall;
begin
  Result:= S_OK;
end;

initialization
  if (Win32MajorVersion > 5) then
  begin
    AModule:= GetModuleHandleW(Shell32);
    @SHGetIDListFromObject:= GetProcAddress(AModule, 'SHGetIDListFromObject');
    @SHCreateItemFromIDList:= GetProcAddress(AModule, 'SHCreateItemFromIDList');
    @SHCreateItemWithParent:= GetProcAddress(AModule, 'SHCreateItemWithParent');
    @SHCreateShellItemArray:= GetProcAddress(AModule, 'SHCreateShellItemArray');
    @SHCreateItemFromParsingName:= GetProcAddress(AModule, 'SHCreateItemFromParsingName');
    @SHCreateShellItemArrayFromIDLists:= GetProcAddress(AModule, 'SHCreateShellItemArrayFromIDLists');
  end;
end.

