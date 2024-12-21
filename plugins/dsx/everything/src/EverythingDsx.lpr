library EverythingDsx;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Everything, DsxPlugin, DCConvertEncoding;

var
  List: TFPList;

threadvar
  AddFileProc: TSAddFileProc;

procedure FoundCallback(FileName: PWideChar);
var
  S: String;
begin
  S:= CeUtf16ToUtf8(UnicodeString(FileName));
  AddFileProc(0, PAnsiChar(S));
end;

function Init(dps: PDsxDefaultParamStruct; pAddFileProc: TSAddFileProc;
  pUpdateStatus: TSUpdateStatusProc): Integer; stdcall;
begin
  AddFileProc:= pAddFileProc;

  if (List = nil) then List:= TFPList.Create;

  Result:= List.Add(pAddFileProc);
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
  if Assigned(List) then
  begin
    if FPluginNr < List.Count then
    begin
      List.Delete(FPluginNr);
      if (List.Count = 0) then FreeAndNil(List);
    end;
  end;
end;

exports
  Init,
  StartSearch,
  StopSearch,
  Finalize;

begin
end.

