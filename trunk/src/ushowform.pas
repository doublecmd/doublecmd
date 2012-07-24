{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   showing editor or viewer by configuration dialog

   contributors:

   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)
}


unit uShowForm;

interface

uses
  Classes, uFileSource;

type

  { TWaitThread }

  TWaitThread = class(TThread)
  private
    FFileList : TStringList;
    FFileSource: IFileSource;

  protected
    procedure Execute; override;

  public
    constructor Create(const FilesToView: TStringList; const aFileSource: IFileSource);
    destructor Destroy; override;
  end;

procedure ShowEditorByGlob(sFileName:String);
procedure ShowViewerByGlob(sFileName:String);
procedure ShowViewerByGlobList(const FilesToView: TStringList;
                               const aFileSource: IFileSource);


implementation

uses
  SysUtils, Process, UTF8Process, Dialogs,
  uGlobs, uOSUtils, fEditor, fViewer, uDCUtils, uTempFileSystemFileSource, uLng,
  uDebug, DCOSUtils;

procedure RunExtTool(const ExtTool: TExternalToolOptions; sFileName: String);
var
  CommandLine: String;
begin
  CommandLine := QuoteStr(ReplaceEnvVars(ExtTool.Path));
  if ExtTool.Parameters <> EmptyStr then
    CommandLine := CommandLine + ' ' + ExtTool.Parameters;
  CommandLine := CommandLine + ' ' + QuoteStr(sFileName);
  ExecCmdFork(CommandLine, ExtTool.RunInTerminal, '', ExtTool.KeepTerminalOpen);
end;

procedure ShowEditorByGlob(sFileName:String);
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

procedure ShowViewerByGlob(sFileName:String);
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

procedure ShowViewerByGlobList(const FilesToView : TStringList;
                               const aFileSource: IFileSource);
var
  I : Integer;
  WaitThread : TWaitThread;
begin
  if gExternalTools[etViewer].Enabled then
  begin
    DCDebug('ShowViewerByGlobList - Use ExtView');
    if aFileSource.IsClass(TTempFileSystemFileSource) then
      begin
        WaitThread := TWaitThread.Create(FilesToView, aFileSource);
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

{ TWaitThread }

constructor TWaitThread.Create(const FilesToView: TStringList; const aFileSource: IFileSource);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FFileList := TStringList.Create;
  // Make a copy of list elements.
  FFileList.Assign(FilesToView);
  FFileSource := aFileSource;
end;

destructor TWaitThread.Destroy;
begin
  if Assigned(FFileList) then
    FreeAndNil(FFileList);

  // Delete the temporary file source and all files inside.
  FFileSource := nil;

  inherited Destroy;
end;

procedure TWaitThread.Execute;
var
  I : Integer;
  Process : TProcessUTF8;
  sCmd: String;
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
      sCmd := FormatTerminal(sCmd, False);
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

  if not FFileSource.IsClass(TTempFileSystemFileSource) then
  begin
    (* Delete temp files after view *)
    for I := 0 to FFileList.Count - 1 do
      mbDeleteFile(FFileList.Strings[I]);
  end;
end;

end.
