{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   showing editor or viewer by configuration dialog

   contributors:

   Copyright (C) 2006-2019 Alexander Koblov (alexx2000@mail.ru)
}


unit uShowForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, DCBasicTypes, uFileSource, uFileSourceOperation, uFile,
  uFileSourceCopyOperation;

type

  { TWaitData }

  TWaitData = class
  private
    procedure ShowOnTopAsync(Data: PtrInt);
  public
    procedure ShowOnTop(AForm: TCustomForm);
    procedure ShowWaitForm; virtual; abstract;
    procedure Done; virtual; abstract;
  end;

  { TViewerWaitData }

  TViewerWaitData = class(TWaitData)
  private
    FFileSource: IFileSource;
  public
    constructor Create(aFileSource: IFileSource);
    destructor Destroy; override;
    procedure ShowWaitForm; override;
    procedure Done; override;
  end;

  { TEditorWaitData }

  TEditorWaitData = class(TWaitData)
  public
    Files: TFiles;
    function GetFileList: TStringList;
  protected
    FileTimes: array of TFileTime;
    TargetPath: String;
    SourceFileSource: IFileSource;
    TargetFileSource: IFileSource;
    FModal: Boolean;
    function GetRelativeFileName(const FullPath: string): string;
    function GetRelativeFileNames: string;
    function GetFromPath: string;
  public
    constructor Create(aCopyOutOperation: TFileSourceCopyOperation; Modal: Boolean = False);
    destructor Destroy; override;
    procedure ShowWaitForm; override;
    procedure Done; override;
  protected
    procedure OnCopyInStateChanged(Operation: TFileSourceOperation;
                                   State: TFileSourceOperationState);
  end;

  TToolDataPreparedProc = procedure(const FileList: TStringList; WaitData: TWaitData; Modal: Boolean = False);

  // Callback may be called either asynchoronously or synchronously (for modal operations)
  // pdrInCallback is returned when FunctionToCall either will be called or was already called
  TPrepareDataResult = (pdrFailed, pdrSynchronous, pdrInCallback);

function PrepareData(FileSource: IFileSource; var SelectedFiles: TFiles;
                     FunctionToCall: TFileSourceOperationStateChangedNotify;
                     Modal: Boolean = False): TPrepareDataResult;

procedure PrepareToolData(FileSource: IFileSource; var SelectedFiles: TFiles;
                          FunctionToCall: TToolDataPreparedProc); overload;

procedure PrepareToolData(FileSource1: IFileSource; var SelectedFiles1: TFiles;
                          FileSource2: IFileSource; var SelectedFiles2: TFiles;
                          FunctionToCall: TToolDataPreparedProc); overload;

procedure PrepareToolData(FileSource1: IFileSource; File1: TFile;
                          FileSource2: IFileSource; File2: TFile;
                          FunctionToCall: TToolDataPreparedProc;
                          Modal: Boolean = False); overload;

procedure RunExtDiffer(CompareList: TStringList);

procedure ShowEditorByGlob(const sFileName: String);
procedure ShowEditorByGlob(WaitData: TEditorWaitData); overload;

procedure ShowDifferByGlob(const LeftName, RightName: String);
procedure ShowDifferByGlobList(const CompareList: TStringList; WaitData: TWaitData; Modal: Boolean = False);

procedure ShowViewerByGlob(const sFileName: String);
procedure ShowViewerByGlobList(const FilesToView: TStringList;
                               const aFileSource: IFileSource);

implementation

uses
  SysUtils, Process, DCProcessUtf8, Dialogs, LCLIntf,
  uShellExecute, uGlobs, uOSUtils, fEditor, fViewer, uDCUtils,
  uTempFileSystemFileSource, uLng, fDiffer, uDebug, DCOSUtils, uShowMsg,
  DCStrUtils, uFileSourceProperty, uWfxPluginCopyOutOperation,
  uFileSourceOperationOptions, uOperationsManager, uFileSourceOperationTypes,
  uMultiArchiveFileSource, fFileExecuteYourSelf, uFileProcs, uFileSystemFileSource;

type

  { TWaitDataDouble }

  TWaitDataDouble = class(TWaitData)
  private
    FWaitData1, FWaitData2: TEditorWaitData;
  public
    constructor Create(WaitData1: TEditorWaitData; WaitData2: TEditorWaitData);
    procedure ShowWaitForm; override;
    procedure Done; override;
    destructor Destroy; override;
  end;

  { TViewerWaitThread }

  TViewerWaitThread = class(TThread)
  private
    FFileList : TStringList;
    FFileSource: IFileSource;
  protected
    procedure Execute; override;
  public
    constructor Create(const FilesToView: TStringList; const aFileSource: IFileSource);
    destructor Destroy; override;
  end;

  { TExtToolWaitThread }

  TExtToolWaitThread = class(TThread)
  private
    FExternalTool: TExternalTool;
    FFileList: TStringList;
    FWaitData: TWaitData;
  private
    procedure RunEditDone;
    procedure ShowWaitForm;
  protected
    procedure Execute; override;
  public
    constructor Create(ExternalTool: TExternalTool;
                       const FileList: TStringList;
                       WaitData: TWaitData);
    destructor Destroy; override;
  end;

procedure RunExtTool(const ExtTool: TExternalToolOptions; sFileName: String);
var
  sCmd: String;
  sParams: String = '';
begin
  sCmd := ExtTool.Path;
  sParams := ExtTool.Parameters;
  // If there is %p already configured in the parameter, we assume user configured it the way he wants.
  // This might be in non-common case where there are parameters AFTER the filename to open.
  // If there is not %p, let's do thing like legacy was and let's add the filename received as parameter.
  if (Pos('%p', sParams) = 0) and (Pos('%f', sParams) = 0) then
  begin
    sParams := ConcatenateStrWithSpace(sParams, '%' + DLE);
    sParams := ConcatenateStrWithSpace(sParams, QuoteStr(sFileName));
  end;

  ProcessExtCommandFork(sCmd, sParams, '', nil, ExtTool.RunInTerminal, ExtTool.KeepTerminalOpen);
end;

procedure RunExtDiffer(CompareList: TStringList);
var
  i : Integer;
  sCmd: String;
  sParams:string='';
begin
  with gExternalTools[etDiffer] do
  begin
    sCmd := QuoteStr(ReplaceEnvVars(Path));
    if Parameters <> EmptyStr then begin
      sParams := sParams + ' ' + Parameters;
    end;
    sParams := ConcatenateStrWithSpace(sParams, '%' + DLE);
    for i := 0 to CompareList.Count - 1 do
      sParams := sParams + ' ' + QuoteStr(CompareList.Strings[i]);
    try
      ProcessExtCommandFork(sCmd, sParams, '', nil, RunInTerminal, KeepTerminalOpen);
    except
      on e: EInvalidCommandLine do
        MessageDlg(rsToolErrorOpeningDiffer,
          rsMsgInvalidCommandLine + ' (' + rsToolDiffer + '):' + LineEnding + e.Message,
          mtError, [mbOK], 0);
    end;
  end;
end;

procedure ShowEditorByGlob(const sFileName: String);
begin
  if gExternalTools[etEditor].Enabled then
  begin
    try
      RunExtTool(gExternalTools[etEditor], sFileName);
    except
      on e: EInvalidCommandLine do
        MessageDlg(rsToolErrorOpeningEditor,
          rsMsgInvalidCommandLine + ' (' + rsToolEditor + '):' + LineEnding + e.Message,
          mtError, [mbOK], 0);
    end;
  end
  else
    ShowEditor(sFileName);
end;

procedure ShowEditorByGlob(WaitData: TEditorWaitData);
var
  FileList: TStringList;
begin
  if gExternalTools[etEditor].Enabled then
  begin
    FileList := TStringList.Create;
    try
      FileList.Add(WaitData.Files[0].FullPath);
      with TExtToolWaitThread.Create(etEditor, FileList, WaitData) do Start;
    finally
      FileList.Free
    end;
  end
  else begin
    ShowEditor(WaitData.Files[0].FullPath, WaitData);
  end;
end;

procedure ShowViewerByGlob(const sFileName: String);
var
  sl:TStringList;
begin
  if gExternalTools[etViewer].Enabled then
  begin
    try
      RunExtTool(gExternalTools[etViewer], sFileName);
    except
      on e: EInvalidCommandLine do
        MessageDlg(rsToolErrorOpeningViewer,
          rsMsgInvalidCommandLine + ' (' + rsToolViewer + '):' + LineEnding + e.Message,
          mtError, [mbOK], 0);
    end;
  end
  else
  begin
    sl:=TStringList.Create;
    try
      sl.Add(sFileName);
      ShowViewer(sl);
    finally
      FreeAndNil(sl);
    end;
  end;
end;

procedure ShowDifferByGlob(const LeftName, RightName: String);
var
  sl: TStringList;
begin
  if gExternalTools[etDiffer].Enabled then
    begin
      sl:= TStringList.Create;
      try
        sl.add(LeftName);
        sl.add(RightName);
        RunExtDiffer(sl);
      finally
        sl.free;
      end;
    end
  else
    ShowDiffer(LeftName, RightName);
end;

procedure ShowDifferByGlobList(const CompareList: TStringList; WaitData: TWaitData; Modal: Boolean = False);
begin
  if gExternalTools[etDiffer].Enabled then
  begin
    if Assigned(WaitData) then
      with TExtToolWaitThread.Create(etDiffer, CompareList, WaitData) do Start
    else
      RunExtDiffer(CompareList);
  end
  else
    ShowDiffer(CompareList[0], CompareList[1], WaitData, Modal);
end;

procedure ShowViewerByGlobList(const FilesToView : TStringList;
                               const aFileSource: IFileSource);
var
  I : Integer;
  WaitThread : TViewerWaitThread;
begin
  if gExternalTools[etViewer].Enabled then
  begin
    DCDebug('ShowViewerByGlobList - Use ExtView');
    if aFileSource.IsClass(TTempFileSystemFileSource) then
      begin
        WaitThread := TViewerWaitThread.Create(FilesToView, aFileSource);
        WaitThread.Start;
      end
    else
    begin
      // TODO: If possible should run one instance of external viewer
      // with multiple file names as parameters.
      for i:=0 to FilesToView.Count-1 do
        RunExtTool(gExternalTools[etViewer], FilesToView.Strings[i]);
    end;
  end // gUseExtView
  else begin
    if aFileSource.IsClass(TTempFileSystemFileSource) then
      ShowViewer(FilesToView, TViewerWaitData.Create(aFileSource))
    else
      ShowViewer(FilesToView);
  end;
end;

{ TWaitData }

procedure TWaitData.ShowOnTopAsync(Data: PtrInt);
var
  Form: TCustomForm absolute Data;
begin
  Form.ShowOnTop;
end;

procedure TWaitData.ShowOnTop(AForm: TCustomForm);
var
  Data: PtrInt absolute AForm;
begin
  Application.QueueAsyncCall(@ShowOnTopAsync, Data);
end;

{ TViewerWaitData }

constructor TViewerWaitData.Create(aFileSource: IFileSource);
begin
  FFileSource:= aFileSource;
end;

destructor TViewerWaitData.Destroy;
begin
  inherited Destroy;
  FFileSource:= nil;
end;

procedure TViewerWaitData.ShowWaitForm;
begin

end;

procedure TViewerWaitData.Done;
begin

end;

{ TWaitDataDouble }

constructor TWaitDataDouble.Create(WaitData1: TEditorWaitData; WaitData2: TEditorWaitData);
begin
  FWaitData1 := WaitData1;
  FWaitData2 := WaitData2;
end;

procedure TWaitDataDouble.ShowWaitForm;
begin
  try
    if Assigned(FWaitData1) then
      FWaitData1.ShowWaitForm;
  finally
    if Assigned(FWaitData2) then
      FWaitData2.ShowWaitForm;
  end;
end;

procedure TWaitDataDouble.Done;
begin
  try
    if Assigned(FWaitData1) then
      FWaitData1.Done;
  finally
    FWaitData1 := nil;
    try
      if Assigned(FWaitData2) then
        FWaitData2.Done;
    finally
      FWaitData2 := nil;
      Free;
    end;
  end;
end;

destructor TWaitDataDouble.Destroy;
begin
  inherited Destroy;
  if Assigned(FWaitData1) then
    FWaitData1.Free;
  if Assigned(FWaitData2) then
    FWaitData2.Free;
end;

{ TEditorWaitData }

constructor TEditorWaitData.Create(aCopyOutOperation: TFileSourceCopyOperation; Modal: Boolean = False);
var
  I: Integer;
  aFileSource: ITempFileSystemFileSource;
begin
  aFileSource := aCopyOutOperation.TargetFileSource as ITempFileSystemFileSource;
  TargetPath := aCopyOutOperation.SourceFiles.Path;
  Files := aCopyOutOperation.SourceFiles.Clone;
  ChangeFileListRoot(aFileSource.FileSystemRoot, Files);
  SetLength(FileTimes, Files.Count);
  for I := 0 to Files.Count - 1 do
    FileTimes[I] := mbFileAge(Files[I].FullPath);
  // Special case for bzip2 like archivers which don't store file size
  if Files.Count = 1 then
    Files[0].Size  := mbFileSize(Files[0].FullPath);
  SourceFileSource := aFileSource;
  TargetFileSource := aCopyOutOperation.FileSource as IFileSource;
  FModal := Modal;
end;

destructor TEditorWaitData.Destroy;
begin
  inherited Destroy;
  Files.Free;
  SourceFileSource:= nil;
  TargetFileSource:= nil;
end;

function TEditorWaitData.GetRelativeFileName(const FullPath: string): string;
begin
  Result := ExtractDirLevel(IncludeTrailingPathDelimiter(Files.Path), FullPath);
end;

function TEditorWaitData.GetRelativeFileNames: string;
var
  I: Integer;
begin
  Result := GetRelativeFileName(Files[0].FullPath);
  for I := 1 to Files.Count - 1 do
    Result := Result + ', ' + GetRelativeFileName(Files[I].FullPath);
end;

function TEditorWaitData.GetFromPath: string;
begin
  if StrBegins(TargetPath, TargetFileSource.CurrentAddress) then
    Result := TargetPath // Workaround for TGioFileSource
  else
    Result := TargetFileSource.CurrentAddress + TargetPath;
end;

procedure TEditorWaitData.ShowWaitForm;
begin
  ShowFileEditExternal(GetRelativeFileNames, GetFromPath, Self, FModal);
end;

procedure TEditorWaitData.Done;
var
  Msg: string;
  I: Integer;
  Operation: TFileSourceCopyOperation;
  DoNotFreeYet: Boolean = False;
begin
  try
    for I := Files.Count - 1 downto 0 do
      if (mbFileAge(Files[I].FullPath) = FileTimes[I]) or
         not msgYesNo(Format(rsMsgCopyBackward, [GetRelativeFileName(Files[I].FullPath)]) + LineEnding + LineEnding + GetFromPath) then
        Files.Delete(I);

    // Files were modified
    if Files.Count > 0 then
    begin
      if (fsoCopyIn in TargetFileSource.GetOperationsTypes) and
         (not (TargetFileSource is TMultiArchiveFileSource)) then
      begin
        Operation:= TargetFileSource.CreateCopyInOperation(SourceFileSource, Files, TargetPath) as TFileSourceCopyOperation;
        // Copy files back
        if Assigned(Operation) then
        begin
          Operation.AddStateChangedListener([fsosStopped], @OnCopyInStateChanged);
          Operation.FileExistsOption:= fsoofeOverwrite;
          if FModal then
            OperationsManager.AddOperationModal(Operation)
          else
            OperationsManager.AddOperation(Operation);
          DoNotFreeYet:= True; // Will be free in operation
        end;
      end
      else
      begin
        Msg := rsMsgCouldNotCopyBackward + LineEnding;
        for I := 0 to Files.Count-1 do
          Msg := Msg + LineEnding + Files[I].FullPath;
        if msgYesNo(Msg) then
          (SourceFileSource as ITempFileSystemFileSource).DeleteOnDestroy:= False;
      end;
    end;
  finally
    if not DoNotFreeYet then
      Free;
  end;
end;

procedure TEditorWaitData.OnCopyInStateChanged(Operation: TFileSourceOperation;
                                               State: TFileSourceOperationState);
var
  I: Integer;
  Msg: string;
  aFileSource: ITempFileSystemFileSource;
  aCopyOperation: TFileSourceCopyOperation;
begin
  if (State = fsosStopped) then
  begin
    aCopyOperation := Operation as TFileSourceCopyOperation;
    aFileSource := aCopyOperation.SourceFileSource as ITempFileSystemFileSource;
    with aCopyOperation.RetrieveStatistics do
    begin
      if DoneFiles <> TotalFiles then
      begin
        Msg := rsMsgCouldNotCopyBackward + LineEnding;
        for I := 0 to aCopyOperation.SourceFiles.Count-1 do
          Msg := Msg + LineEnding + aCopyOperation.SourceFiles[I].FullPath;
        if msgYesNo(Operation.Thread, Msg) then
        begin
          aFileSource.DeleteOnDestroy:= False;
        end;
      end;
    end;
    Free;
  end;
end;

function TEditorWaitData.GetFileList: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Files.Count - 1 do
    Result.Add(Files[I].FullPath);
end;

{ TExtToolWaitThread }

procedure TExtToolWaitThread.RunEditDone;
begin
  FWaitData.Done;
end;

procedure TExtToolWaitThread.ShowWaitForm;
begin
  FWaitData.ShowWaitForm;
end;

procedure TExtToolWaitThread.Execute;
var
  I: Integer;
  StartTime: QWord;
  Process : TProcessUTF8;
  sCmd, sSecureEmptyStr: String;
begin
  try
    Process := TProcessUTF8.Create(nil);
    try
      with gExternalTools[FExternalTool] do
      begin
        sCmd := ReplaceEnvVars(Path);
        // TProcess arguments must be enclosed with double quotes and not escaped.
        if RunInTerminal then
        begin
          sCmd := QuoteStr(sCmd);
          if Parameters <> EmptyStr then
            sCmd := sCmd + ' ' + Parameters;
          for I := 0 to FFileList.Count - 1 do
            sCmd := sCmd + ' ' + QuoteStr(FFileList[I]);
          sSecureEmptyStr := EmptyStr; // Let's play safe and don't let EmptyStr being passed as "VAR" parameter of "FormatTerminal"
          FormatTerminal(sCmd, sSecureEmptyStr, False);
        end
        else
        begin
          sCmd := '"' + sCmd + '"';
          if Parameters <> EmptyStr then
            sCmd := sCmd + ' ' + Parameters;
          for I := 0 to FFileList.Count - 1 do
            sCmd := sCmd + ' "' + FFileList[I] + '"';
        end;
      end;

      Process.CommandLine := sCmd;
      Process.Options := [poWaitOnExit];
      StartTime:= GetTickCount64;
      Process.Execute;

      // If an editor closes within gEditWaitTime amount of milliseconds,
      // assume that it's a multiple document editor and show dialog where
      // user can confirm when editing has ended.
      if GetTickCount64 - StartTime < gEditWaitTime then
      begin
        Synchronize(@ShowWaitForm);
      end
      else begin
        Synchronize(@RunEditDone);
      end;

    finally
      Process.Free;
    end;
  except
    FWaitData.Free;
  end;
end;

constructor TExtToolWaitThread.Create(ExternalTool: TExternalTool;
                                      const FileList: TStringList;
                                      WaitData: TWaitData);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FExternalTool := ExternalTool;

  FFileList := TStringList.Create;
  // Make a copy of list elements.
  FFileList.Assign(FileList);

  FWaitData := WaitData;
end;

destructor TExtToolWaitThread.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

{ TViewerWaitThread }

constructor TViewerWaitThread.Create(const FilesToView: TStringList; const aFileSource: IFileSource);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FFileList := TStringList.Create;
  // Make a copy of list elements.
  FFileList.Assign(FilesToView);
  FFileSource := aFileSource;
end;

destructor TViewerWaitThread.Destroy;
begin
  if Assigned(FFileList) then
    FreeAndNil(FFileList);

  // Delete the temporary file source and all files inside.
  FFileSource := nil;

  inherited Destroy;
end;

procedure TViewerWaitThread.Execute;
var
  Process : TProcessUTF8;
  sCmd, sSecureEmptyStr: String;
begin
  Process := TProcessUTF8.Create(nil);

  with gExternalTools[etViewer] do
  begin
    sCmd := ReplaceEnvVars(Path);
    // TProcess arguments must be enclosed with double quotes and not escaped.
    if RunInTerminal then
    begin
      sCmd := QuoteStr(sCmd);
      if Parameters <> EmptyStr then
        sCmd := sCmd + ' ' + Parameters;
      sCmd := sCmd + ' ' + QuoteStr(FFileList.Strings[0]);
      sSecureEmptyStr := EmptyStr; //Let's play safe and don't let EmptyStr being passed as "VAR" parameter of "FormatTerminal"
      FormatTerminal(sCmd, sSecureEmptyStr, False);
    end
    else
    begin
      sCmd := '"' + sCmd + '"';
      if Parameters <> EmptyStr then
        sCmd := sCmd + ' ' + Parameters;
      sCmd := sCmd + ' "' + FFileList.Strings[0] + '"';
    end;
  end;

  Process.CommandLine := sCmd;
  Process.Options := [poWaitOnExit];
  Process.Execute;
  Process.Free;
end;

{ PrepareData }

function PrepareData(FileSource: IFileSource; var SelectedFiles: TFiles;
                     FunctionToCall: TFileSourceOperationStateChangedNotify;
                     Modal: Boolean = False): TPrepareDataResult;
var
  I: Integer;
  aFile: TFile;
  Directory: String;
  TempFiles: TFiles = nil;
  TempFileSource: ITempFileSystemFileSource = nil;
  Operation: TFileSourceOperation;
begin
  // If files are links to local files
  if (fspLinksToLocalFiles in FileSource.Properties) then
    begin
      for I := 0 to SelectedFiles.Count - 1 do
        begin
          aFile := SelectedFiles[I];
          FileSource.GetLocalName(aFile);
        end;
    end
  // If files not directly accessible copy them to temp file source.
  else if not (fspDirectAccess in FileSource.Properties) then
  begin
    if not (fsoCopyOut in FileSource.GetOperationsTypes) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit(pdrFailed);
    end;

    Directory := GetTempName(GetTempFolderDeletableAtTheEnd);
    if not mbForceDirectory(Directory) then
    begin
      MessageDlg(mbSysErrorMessage(GetLastOSError), mtError, [mbOK], 0);
      Exit(pdrFailed);
    end;

    TempFileSource := TTempFileSystemFileSource.Create(Directory);

    TempFiles := SelectedFiles.Clone;
    try
      Operation := FileSource.CreateCopyOutOperation(
                       TempFileSource,
                       TempFiles,
                       TempFileSource.FileSystemRoot);
      if Operation is TWfxPluginCopyOutOperation then
        (Operation as TWfxPluginCopyOutOperation).NeedsConnection := False; // use separate connection
    finally
      TempFiles.Free;
    end;

    if not Assigned(Operation) then
    begin
      msgWarning(rsMsgErrNotSupported);
      Exit(pdrFailed);
    end;

    Operation.AddStateChangedListener([fsosStopped], FunctionToCall);

    if Modal then
      OperationsManager.AddOperationModal(Operation)
    else
      OperationsManager.AddOperation(Operation);

    Exit(pdrInCallback);
  end;
  Exit(pdrSynchronous);
end;

{ TToolDataPreparator }

type
  TToolDataPreparator = class
  protected
    FFunc: TToolDataPreparedProc;
    FCallOnFail: Boolean;
    procedure OnCopyOutStateChanged(Operation: TFileSourceOperation;
                                    State: TFileSourceOperationState);
  public
    constructor Create(FunctionToCall: TToolDataPreparedProc; CallOnFail: Boolean = False);
    procedure Prepare(FileSource: IFileSource; var SelectedFiles: TFiles);
  end;

constructor TToolDataPreparator.Create(FunctionToCall: TToolDataPreparedProc; CallOnFail: Boolean = False);
begin
  FFunc := FunctionToCall;
  FCallOnFail := CallOnFail;
end;

procedure TToolDataPreparator.Prepare(FileSource: IFileSource; var SelectedFiles: TFiles);
var
  I: Integer;
  FileList: TStringList;
begin
  case PrepareData(FileSource, SelectedFiles, @OnCopyOutStateChanged) of
  pdrSynchronous:
    try
      FileList := TStringList.Create;
      for I := 0 to SelectedFiles.Count - 1 do
        FileList.Add(SelectedFiles[i].FullPath);
      FFunc(FileList, nil);
    finally
      Free;
    end;
  pdrFailed:
    try
      if FCallOnFail then
        FFunc(nil, nil);
    finally
      Free;
    end;
  end;
end;

procedure TToolDataPreparator.OnCopyOutStateChanged(
  Operation: TFileSourceOperation; State: TFileSourceOperationState);
var WaitData: TEditorWaitData;
begin
  if (State <> fsosStopped) then
    Exit;
  try
    if Operation.Result = fsorFinished then
    begin
      WaitData := TEditorWaitData.Create(Operation as TFileSourceCopyOperation);
      FFunc(WaitData.GetFileList, WaitData);
    end
    else
    begin
      if FCallOnFail then
        FFunc(nil, nil);
    end;
  finally
    Free;
  end;
end;

{ TToolDataPreparator2 }

type
  TToolDataPreparator2 = class
  protected
    FFunc: TToolDataPreparedProc;
    FCallOnFail: Boolean;
    FModal: Boolean;
    FFailed: Boolean;
    FFileList1: TStringList;
    FFileList2: TStringList;
    FPrepared1: Boolean;
    FPrepared2: Boolean;
    FWaitData1: TEditorWaitData;
    FWaitData2: TEditorWaitData;
    procedure OnCopyOutStateChanged1(Operation: TFileSourceOperation;
                                     State: TFileSourceOperationState);
    procedure OnCopyOutStateChanged2(Operation: TFileSourceOperation;
                                     State: TFileSourceOperationState);
    procedure TryFinish;
  public
    constructor Create(FunctionToCall: TToolDataPreparedProc; CallOnFail: Boolean = False);
    procedure Prepare(FileSource1: IFileSource; var SelectedFiles1: TFiles;
                      FileSource2: IFileSource; var SelectedFiles2: TFiles;
                      Modal: Boolean = False);
    destructor Destroy; override;
  end;

constructor TToolDataPreparator2.Create(FunctionToCall: TToolDataPreparedProc; CallOnFail: Boolean = False);
begin
  FFunc := FunctionToCall;
  FCallOnFail := CallOnFail;
end;

procedure TToolDataPreparator2.Prepare(FileSource1: IFileSource; var SelectedFiles1: TFiles;
                                       FileSource2: IFileSource; var SelectedFiles2: TFiles;
                                       Modal: Boolean = False);
var
  I: Integer;
begin
  FModal := Modal;

  case PrepareData(FileSource1, SelectedFiles1, @OnCopyOutStateChanged1, Modal) of
  pdrSynchronous:
    begin
      FFileList1 := TStringList.Create;
      for I := 0 to SelectedFiles1.Count - 1 do
        FFileList1.Add(SelectedFiles1[I].FullPath);
      FPrepared1 := True;
    end;
  pdrFailed:
    begin
      try
        if FCallOnFail then
          FFunc(nil, nil, FModal);
      finally
        Free;
      end;
      Exit;
    end;
  end;

  case PrepareData(FileSource2, SelectedFiles2, @OnCopyOutStateChanged2, Modal) of
  pdrSynchronous:
    begin
      FFileList2 := TStringList.Create;
      for I := 0 to SelectedFiles2.Count - 1 do
        FFileList2.Add(SelectedFiles2[I].FullPath);
      FPrepared2 := True;
      TryFinish;
    end;
  pdrFailed:
    begin
      FPrepared2 := True;
      FFailed := True;
      TryFinish;
    end;
  end;
end;

procedure TToolDataPreparator2.OnCopyOutStateChanged1(
  Operation: TFileSourceOperation; State: TFileSourceOperationState);
begin
  if (State <> fsosStopped) then
    Exit;
  FPrepared1 := True;
  if not FFailed then
  begin
    if Operation.Result = fsorFinished then
    begin
      FWaitData1 := TEditorWaitData.Create(Operation as TFileSourceCopyOperation, FModal);
      FFileList1 := FWaitData1.GetFileList;
    end
    else
    begin
      FFailed := True;
//      if not FPrepared2 and Assigned(FOperation2) then
//        FOperation2.Stop();
    end;
  end;
  TryFinish;
end;

procedure TToolDataPreparator2.OnCopyOutStateChanged2(
  Operation: TFileSourceOperation; State: TFileSourceOperationState);
begin
  if (State <> fsosStopped) then
    Exit;
  FPrepared2 := True;
  if not FFailed then
  begin
    if Operation.Result = fsorFinished then
    begin
      FWaitData2 := TEditorWaitData.Create(Operation as TFileSourceCopyOperation, FModal);
      FFileList2 := FWaitData2.GetFileList;
    end
    else
    begin
      FFailed := True;
//      if not FPrepared1 and Assigned(FOperation1) then
//        FOperation1.Stop();
    end;
  end;
  TryFinish;
end;

procedure TToolDataPreparator2.TryFinish;
var
  s: string;
  WaitData: TWaitDataDouble;
begin
  if FPrepared1 and FPrepared2 then
  try
    if FFailed then
    begin
      if FCallOnFail then
        FFunc(nil, nil, FModal);
      Exit;
    end;
    if Assigned(FFileList2) then
      for s in FFileList2 do
        FFileList1.Append(s);
    if Assigned(FWaitData1) or Assigned(FWaitData2) then
    begin
      WaitData := TWaitDataDouble.Create(FWaitData1, FWaitData2);
      FWaitData1 := nil;
      FWaitData2 := nil;
      FFunc(FFileList1, WaitData, FModal);
    end
    else
      FFunc(FFileList1, nil, FModal);
  finally
    Free;
  end;
end;

destructor TToolDataPreparator2.Destroy;
begin
  inherited Destroy;
  if Assigned(FFileList1) then
     FFileList1.Free;
  if Assigned(FFileList2) then
     FFileList2.Free;
  if Assigned(FWaitData1) then
     FWaitData1.Free;
  if Assigned(FWaitData2) then
     FWaitData2.Free;
end;

procedure PrepareToolData(FileSource: IFileSource; var SelectedFiles: TFiles;
                          FunctionToCall: TToolDataPreparedProc);
begin
  with TToolDataPreparator.Create(FunctionToCall) do
    Prepare(FileSource, SelectedFiles);
end;

procedure PrepareToolData(FileSource1: IFileSource; var SelectedFiles1: TFiles;
                          FileSource2: IFileSource; var SelectedFiles2: TFiles;
                          FunctionToCall: TToolDataPreparedProc);
begin
  with TToolDataPreparator2.Create(FunctionToCall) do
    Prepare(FileSource1, SelectedFiles1, FileSource2, SelectedFiles2);
end;

procedure PrepareToolData(FileSource1: IFileSource; File1: TFile;
                          FileSource2: IFileSource; File2: TFile;
                          FunctionToCall: TToolDataPreparedProc;
                          Modal: Boolean = False);
var Files1, Files2: TFiles;
begin
  Files1 := TFiles.Create(File1.Path);
  try
    Files1.Add(File1.Clone);
    Files2 := TFiles.Create(File2.Path);
    try
      Files2.Add(File2.Clone);
      with TToolDataPreparator2.Create(FunctionToCall) do
        Prepare(FileSource1, Files1, FileSource2, Files2, Modal);
    finally
      Files2.Free;
    end;
  finally
    Files1.Free;
  end;
end;

end.
