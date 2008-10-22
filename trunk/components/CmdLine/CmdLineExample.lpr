program CmdLineExample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, wnmainform;

begin
  Application.Initialize;
  Application.CreateForm(TWMainForm, WMainForm);
  Application.Run;
end.

