{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

showing editor or viewer by configuration dialog

contributors:

}


unit uShowForm;

interface
uses
  Classes;

Function ShowEditorByGlob(const sFileName:String):Boolean;
Function ShowViewerByGlob(const sFileName:String):Boolean;
Function ShowViewerByGlobList(list:TStringList; bDeleteAfterView : Boolean = False):Boolean;


implementation
uses
  SysUtils, Process,
  uGlobs, uOSUtils, fEditor, fViewer;

Function ShowEditorByGlob(const sFileName:String):Boolean;
begin
  if gUseExtEdit then
    ExecCmdFork(Format(gExtEdit,[sFileName]))
  else
    ShowEditor(sFileName);
  Result:=True;   
end;

Function ShowViewerByGlob(const sFileName:String):Boolean;
var
  sl:TStringList;
begin
  if gUseExtView then
    ExecCmdFork(Format(gExtView,[sFileName]))
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

Function ShowViewerByGlobList(list : TStringList; bDeleteAfterView : Boolean = False):Boolean;
var
  I, Count:Integer;
  Process : TProcess;
begin
  if gUseExtView then
  begin
    if bDeleteAfterView then
      begin
        Process := TProcess.Create(nil);
        Process.CommandLine := Format(gExtView,[List.Strings[0]]);
        Process.Options := [poWaitOnExit];
        Process.Execute;
    

        Count := list.Count - 1;

      //DebugLN('DeleteFile == ' + FileList.Strings[0]);

        for I := 0 to Count do
          DeleteFile(list.Strings[I]);
      end;
    //************
    writeln('ShowViewerByGlobList - Use ExtView ');
    for i:=0 to list.Count-1 do
      ExecCmdFork(Format(gExtView,[List.Strings[i]]))
  end
  else
    ShowViewer(list, bDeleteAfterView);
  Result:=True;
end;

end.
