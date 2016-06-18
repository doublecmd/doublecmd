{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   showing editor or viewer by configuration dialog

   contributors:

   Copyright (C) 2006-2015 Alexander Koblov (alexx2000@mail.ru)
}


unit uShowForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, DCBasicTypes, uFileSource, uFileSourceOperation;

type

  { TEditorWaitData }

  TEditorWaitData = class
  public
    FileName: String;
    TargetPath: String;
    FileTime: TFileTime;
    SourceFileSource: IFileSource;
    TargetFileSource: IFileSource;
  public
    destructor Destroy; override;
  protected
    procedure OnCopyInStateChanged(Operation: TFileSourceOperation;
                                   State: TFileSourceOperationState);
  end;

procedure EditDone(WaitData: TEditorWaitData);
procedure RunExtDiffer(CompareList: TStringList);

procedure ShowEditorByGlob(const sFileName: String);
procedure ShowEditorByGlob(WaitData: TEditorWaitData); overload;

procedure ShowDifferByGlob(const LeftName, RightName: String);

procedure ShowViewerByGlob(const sFileName: String);
procedure ShowViewerByGlobList(const FilesToView: TStringList;
                               const aFileSource: IFileSource);

implementation

uses
  SysUtils, Process, DCProcessUtf8, Dialogs, LCLIntf,
  uShellExecute, uGlobs, uOSUtils, fEditor, fViewer, uDCUtils,
  uTempFileSystemFileSource, uLng, fDiffer, uDebug, DCOSUtils, uShowMsg,
  uFile, uFileSourceCopyOperation, uFileSystemFileSource,
  uFileSourceOperationOptions, uOperationsManager, uFileSourceOperationTypes,
  uWcxArchiveFileSource, uWfxPluginFileSource, fFileExecuteYourSelf;

type

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

  { TEditorWaitThread }

  TEditorWaitThread = class(TThread)
  private
    FWaitData: TEditorWaitData;
  private
    procedure ShowWaitForm;
  protected
    procedure Execute; override;
  public
    constructor Create(WaitData: TEditorWaitData);
  end;

procedure RunExtTool(const ExtTool: TExternalToolOptions; sFileName: String);
var
  sCmd: String;
  sParams:string='';
begin
  sCmd := ExtTool.Path;
  sParams := ExtTool.Parameters;
  //If there is %p already configured in the parameter, we assume user configured it the way he wants.
  //This might be in non-common case where there are paramters AFTER the filename to open.
  //If there is not %p, let's do thing like legacy was and let's add the filename received as paramter.
  if pos('%p',sParams)=0 then
    sParams := ConcatenateStrWithSpace(sParams,QuoteFilenameIfNecessary(sFileName));
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
    if Parameters <> EmptyStr then
      sParams := sParams + ' ' + Parameters;
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
begin
  if gExternalTools[etEditor].Enabled then
    with TEditorWaitThread.Create(WaitData) do Start
  else begin
    ShowEditor(WaitData);
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
  else
    ShowViewer(FilesToView, aFileSource);
end;

procedure EditDone(WaitData: TEditorWaitData);
var
  Files: TFiles;
  Operation: TFileSourceCopyOperation;
begin
  with WaitData do
  try
    // File was modified
    if mbFileAge(FileName) <> FileTime then
    begin
      if not msgYesNo(Format(rsMsgCopyBackward, [ExtractFileName(FileName)])) then Exit;
      if (fsoCopyIn in TargetFileSource.GetOperationsTypes) and
         ((TargetFileSource is TWcxArchiveFileSource) or (TargetFileSource is TWfxPluginFileSource)) then
      begin
        Files:= TFiles.Create(SourceFileSource.GetRootDir);
        Files.Add(TFileSystemFileSource.CreateFileFromFile(FileName));
        Operation:= TargetFileSource.CreateCopyInOperation(SourceFileSource, Files, TargetPath) as TFileSourceCopyOperation;
        // Copy file back
        if Assigned(Operation) then
        begin
          Operation.AddStateChangedListener([fsosStopped], @OnCopyInStateChanged);
          Operation.FileExistsOption:= fsoofeOverwrite;
          OperationsManager.AddOperation(Operation);
          WaitData:= nil; // Will be free in operation
        end;
      end
      else if msgYesNo(rsMsgCouldNotCopyBackward + LineEnding + FileName) then
      begin
        (SourceFileSource as ITempFileSystemFileSource).DeleteOnDestroy:= False;
      end;
    end;
  finally
    WaitData.Free;
  end;
end;

{ TEditorWaitData }

destructor TEditorWaitData.Destroy;
begin
  inherited Destroy;
  SourceFileSource:= nil;
  TargetFileSource:= nil;
end;

procedure TEditorWaitData.OnCopyInStateChanged(Operation: TFileSourceOperation;
                                               State: TFileSourceOperationState);
var
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
        if msgYesNo(Operation.Thread, rsMsgCouldNotCopyBackward + LineEnding + aCopyOperation.SourceFiles[0].FullPath) then
        begin
          aFileSource.DeleteOnDestroy:= False;
        end;
      end;
    end;
    Free;
  end;
end;

{ TEditorWaitThread }

procedure TEditorWaitThread.ShowWaitForm;
begin
  ShowFileEditExternal(FWaitData);
end;

procedure TEditorWaitThread.Execute;
var
  StartTime: QWord;
  Process : TProcessUTF8;
  sCmd, sSecureEmptyStr: String;
begin
  Process := TProcessUTF8.Create(nil);

  with gExternalTools[etEditor] do
  begin
    sCmd := ReplaceEnvVars(Path);
    // TProcess arguments must be enclosed with double quotes and not escaped.
    if RunInTerminal then
    begin
      sCmd := QuoteStr(sCmd);
      if Parameters <> EmptyStr then
        sCmd := sCmd + ' ' + Parameters;
      sCmd := sCmd + ' ' + QuoteStr(FWaitData.FileName);
      sSecureEmptyStr := EmptyStr; // Let's play safe and don't let EmptyStr being passed as "VAR" parameter of "FormatTerminal"
      FormatTerminal(sCmd, sSecureEmptyStr, False);
    end
    else
    begin
      sCmd := '"' + sCmd + '"';
      if Parameters <> EmptyStr then
        sCmd := sCmd + ' ' + Parameters;
      sCmd := sCmd + ' "' + FWaitData.FileName + '"';
    end;
  end;

  Process.CommandLine := sCmd;
  Process.Options := [poWaitOnExit];
  StartTime:= GetTickCount64;
  Process.Execute;
  Process.Free;

  if GetTickCount64 - StartTime < gEditWaitTime then
  begin
    Synchronize(@ShowWaitForm);
  end
  else begin
    EditDone(FWaitData);
  end;
end;

constructor TEditorWaitThread.Create(WaitData: TEditorWaitData);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FWaitData := WaitData;
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

end.
