library EverythingDsx;

{$mode objfpc}{$H+}

uses
  Classes, DsxPlugin, everything;

threadvar
  AddFileProc: TSAddFileProc;

procedure FoundCallback(FileName: PWideChar);
var
  S: String;
begin
  S:= UTF8Encode(UnicodeString(FileName));
  AddFileProc(0, PAnsiChar(S));
end;

function Init(dps: PDsxDefaultParamStruct; pAddFileProc: TSAddFileProc;
  pUpdateStatus: TSUpdateStatusProc): Integer; stdcall;

begin
  AddFileProc:= pAddFileProc;
end;

procedure StartSearch(FPluginNr: Integer; pSearchRecRec: PDsxSearchRecord); stdcall;
var
  Flags: Integer = 0;
begin
  if pSearchRecRec^.CaseSensitive then Flags:= Flags or EVERYTHING_IPC_MATCHCASE;
  Start(pSearchRecRec^.FileMask, Flags, @FoundCallback);
end;

procedure StopSearch(FPluginNr: Integer); stdcall;
begin

end;

procedure Finalize(FPluginNr: Integer); stdcall;
begin

end;

exports
  Init,
  StartSearch,
  StopSearch,
  Finalize;

begin
end.

