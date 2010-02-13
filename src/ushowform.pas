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

Function ShowEditorByGlob(sFileName:String):Boolean;
Function ShowViewerByGlob(sFileName:String):Boolean;
Function ShowViewerByGlobList(const FilesToView: TStringList;
                              const aFileSource: IFileSource):Boolean;


implementation

uses
  SysUtils, Process, UTF8Process, LCLProc, uGlobs, uOSUtils, fEditor, fViewer,
  uDCUtils, uTempFileSystemFileSource;

function RunExtTool(const ExtTool: TExternalToolOptions; sFileName: String): String;
begin
  Result := QuoteStr(ExtTool.Path);
  if ExtTool.Parameters <> EmptyStr then
    Result := Result + ' ' + ExtTool.Parameters;
  Result := Result + ' ' + QuoteStr(sFileName);
  ExecCmdFork(Result, ExtTool.RunInTerminal, '', ExtTool.KeepTerminalOpen);
end;

function ShowEditorByGlob(sFileName:String):Boolean;
begin
  if gExternalTools[etEditor].Enabled then
    RunExtTool(gExternalTools[etEditor], sFileName)
  else
    ShowEditor(sFileName);
  Result:=True;   
end;

function ShowViewerByGlob(sFileName:String):Boolean;
var
  sl:TStringList;
begin
  if gExternalTools[etViewer].Enabled then
    RunExtTool(gExternalTools[etViewer], sFileName)
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
  Result:=True;
end;

function ShowViewerByGlobList(const FilesToView : TStringList;
                              const aFileSource: IFileSource):Boolean;
var
  I : Integer;
  WaitThread : TWaitThread;
begin
  if gExternalTools[etViewer].Enabled then
  begin
    DebugLN('ShowViewerByGlobList - Use ExtView ');
    if aFileSource.IsClass(TTempFileSystemFileSource) then
      begin
        WaitThread := TWaitThread.Create(FilesToView, aFileSource);
        WaitThread.Resume;
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
  Result:=True;
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
    // TProcess arguments must be enclosed with double quotes and not escaped.
    if RunInTerminal then
    begin
      sCmd := QuoteStr(Path);
      if Parameters <> EmptyStr then
        sCmd := sCmd + ' ' + Parameters;
      sCmd := sCmd + ' ' + QuoteStr(FFileList.Strings[0]);
      sCmd := FormatTerminal(sCmd, False);
    end
    else
    begin
      sCmd := '"' + Path + '"';
      if Parameters <> EmptyStr then
        sCmd := sCmd + ' ' + Parameters;
      sCmd := sCmd + ' "' + FFileList.Strings[0] + '"';
    end;
  end;

  Process.CommandLine := sCmd;
  Process.Options := [poWaitOnExit];
  Process.Execute;
  Process.Free;

  (* Delete temp files after view *)
  for I := 0 to FFileList.Count - 1 do
    mbDeleteFile(FFileList.Strings[I]);
end;

end.