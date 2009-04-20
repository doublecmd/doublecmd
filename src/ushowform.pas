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
  Classes;

type

  { TWaitThread }

  TWaitThread = class(TThread)
  private
    FFileList : TStringList;

  protected
    procedure Execute; override;

  public
    // Files in FilesToView list will be deleted after viewing.
    constructor Create(const FilesToView: TStringList);
    destructor Destroy; override;
  end;

Function ShowEditorByGlob(sFileName:String):Boolean;
Function ShowViewerByGlob(sFileName:String):Boolean;
Function ShowViewerByGlobList(const FilesToView: TStringList;
                              bDeleteAfterView : Boolean = False):Boolean;


implementation
uses
  SysUtils, Process, LCLProc, uGlobs, uOSUtils, fEditor, fViewer, uDCUtils;

const
  sCmdLine = '"%s" "%s"';

function ShowEditorByGlob(sFileName:String):Boolean;
begin
  TrimQuotes(sFileName);
  if gUseExtEdit then
    ExecCmdFork(Format(sCmdLine, [gExtEdit, sFileName]))
  else
    ShowEditor(sFileName);
  Result:=True;   
end;

function ShowViewerByGlob(sFileName:String):Boolean;
var
  sl:TStringList;
begin
  TrimQuotes(sFileName);
  if gUseExtView then
    ExecCmdFork(Format(sCmdLine, [gExtView, sFileName]))
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
                              bDeleteAfterView : Boolean = False):Boolean;
var
  I : Integer;
  WaitThread : TWaitThread;
begin
  if gUseExtView then
  begin
    DebugLN('ShowViewerByGlobList - Use ExtView ');
    if bDeleteAfterView then
      begin
        WaitThread := TWaitThread.Create(FilesToView);
        WaitThread.Resume;
      end
    else
     for i:=0 to FilesToView.Count-1 do
       ExecCmdFork(Format(sCmdLine, [gExtView, FilesToView.Strings[i]]))
  end // gUseExtView
  else
    ShowViewer(FilesToView, bDeleteAfterView);
  Result:=True;
end;

{ TWaitThread }

constructor TWaitThread.Create(const FilesToView: TStringList);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FFileList := TStringList.Create;
  // Make a copy of list elements.
  FFileList.Assign(FilesToView);
end;

destructor TWaitThread.Destroy;
begin
  if Assigned(FFileList) then
    FreeAndNil(FFileList);

  inherited Destroy;
end;

procedure TWaitThread.Execute;
var
  I : Integer;
  Process : TProcess;
begin
  Process := TProcess.Create(nil);
  Process.CommandLine := Format(sCmdLine, [gExtView, FFileList.Strings[0]]);
  Process.Options := [poWaitOnExit];
  Process.Execute;
  Process.Free;

  (* Delete temp files after view *)
  for I := 0 to FFileList.Count - 1 do
    mbDeleteFile(FFileList.Strings[I]);
end;

end.
