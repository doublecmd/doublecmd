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
  end;

Function ShowEditorByGlob(const sFileName:String):Boolean;
Function ShowViewerByGlob(const sFileName:String):Boolean;
Function ShowViewerByGlobList(list:TStringList; bDeleteAfterView : Boolean = False):Boolean;


implementation
uses
  SysUtils, Process, LCLProc,
  uGlobs, uOSUtils, fEditor, fViewer;

const
  sCmdLine = '"%s" "%s"';

function ShowEditorByGlob(const sFileName:String):Boolean;
begin
  if gUseExtEdit then
    ExecCmdFork(Format(sCmdLine, [gExtEdit, sFileName]))
  else
    ShowEditor(sFileName);
  Result:=True;   
end;

function ShowViewerByGlob(const sFileName:String):Boolean;
var
  sl:TStringList;
begin
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

function ShowViewerByGlobList(List : TStringList; bDeleteAfterView : Boolean = False):Boolean;
var
  I, Count:Integer;
  WaitThread : TWaitThread;
begin
  if gUseExtView then
  begin
    DebugLN('ShowViewerByGlobList - Use ExtView ');
    if bDeleteAfterView then
      begin
        WaitThread := TWaitThread.Create(True);
        WaitThread.FFileList := List;
        WaitThread.FreeOnTerminate := True;
        WaitThread.Resume;
      end
    else
     for i:=0 to list.Count-1 do
       ExecCmdFork(Format(sCmdLine, [gExtView, List.Strings[i]]))
  end // gUseExtView
  else
    ShowViewer(List, bDeleteAfterView);
  Result:=True;
end;

{ TWaitThread }

procedure TWaitThread.Execute;
var
  I, Count : Integer;
  Process : TProcess;
begin
  Process := TProcess.Create(nil);
  Process.CommandLine := Format(sCmdLine, [gExtView, FFileList.Strings[0]]);
  Process.Options := [poWaitOnExit];
  Process.Execute;
  Process.Free;
  (* Delete temp files after view *)
  Count := FFileList.Count - 1;
  for I := 0 to Count do
    mbDeleteFile(FFileList.Strings[I]);
end;

end.
