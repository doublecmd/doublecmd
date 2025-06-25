library EverythingDsx;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Everything, DsxPlugin, DCConvertEncoding;

var
  List: TFPList;
  AddFileProc: TSAddFileProc;

procedure FoundCallback(FileName: PWideChar; UserData: Pointer);
var
  S: String;
  FindData: TFindData absolute UserData;
begin
  S:= CeUtf16ToUtf8(UnicodeString(FileName));
  AddFileProc(FindData.PluginNumber, PAnsiChar(S));
end;

function Init(dps: PDsxDefaultParamStruct; pAddFileProc: TSAddFileProc;
  pUpdateStatus: TSUpdateStatusProc): Integer; stdcall;
var
  FindData: TFindData;
begin
  FindData:= TFindData.Create;

  if (List = nil) then
  begin
    List:= TFPList.Create;
    AddFileProc:= pAddFileProc;
  end;

  Result:= List.Add(FindData);

  FindData.PluginNumber:= Result;
  FindData.FoundCallback:= @FoundCallback;
end;

procedure StartSearch(FPluginNr: Integer; pSearchRecRec: PDsxSearchRecord); stdcall;
var
  Flags: Integer = 0;
begin
  if pSearchRecRec^.CaseSensitive then Flags:= Flags or EVERYTHING_IPC_MATCHCASE;
  Start(pSearchRecRec^.FileMask, Flags, TFindData(List[FPluginNr]));
end;

procedure StopSearch(FPluginNr: Integer); stdcall;
begin
  TFindData(List[FPluginNr]).Cancel:= True;
end;

procedure Finalize(FPluginNr: Integer); stdcall;
begin
  if Assigned(List) then
  begin
    if FPluginNr < List.Count then
    begin
      TFindData(List[FPluginNr]).Free;
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

