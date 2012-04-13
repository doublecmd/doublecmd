 {
    DSXLocate
    -------------------------------------------------------------------------
    This is DSX (Search) plugin for Double Commander.
    Plugin use locate and it's database for searching.

    Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
    The original class of TExProcess used in plugin was written by Anton Rjeshevsky.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 }


library DSXLocate;

{$mode objfpc}{$H+}
{$include calling.inc}

uses
  Classes, SysUtils, DsxPlugin, un_process;

var
  List: TStringList;
  LocatePath: String;

type

  { TPlugInfo }

  TPlugInfo = class
  private
    FProcess: TExProcess;
    FAddProc: TSAddFileProc;
    FUpdateProc: TSUpdateStatusProc;
    FSearchRec: TDsxSearchRecord;
    FilesScanned: Integer;
  public
    PluginNr: Integer;
    //---------------------
    constructor Create(Nr: Integer);
    procedure SetProcs(AddProc: TSAddFileProc; UpdateProc: TSUpdateStatusProc);
    procedure SetDefs(pSearchRec: PDsxSearchRecord);
    destructor Destroy; override;
    //---------------------
    procedure Start;
    procedure Stop;
    procedure OnReadLn(str: String);
  end;

constructor TPlugInfo.Create(Nr: Integer);
begin
  PluginNr := Nr;
  FProcess := nil;
end;

procedure TPlugInfo.SetProcs(AddProc: TSAddFileProc; UpdateProc: TSUpdateStatusProc);
begin
  FAddProc    := AddProc;
  FUpdateProc := UpdateProc;
end;

procedure TPlugInfo.SetDefs(pSearchRec: PDsxSearchRecord);
begin
  FSearchRec := pSearchRec^;
end;

destructor TPlugInfo.Destroy;
begin
  if Assigned(FProcess) then
    FreeAndNil(FProcess);
  inherited Destroy;
end;

procedure TPlugInfo.Start;
var
  sSearch: String;
begin
  FilesScanned := 0;
  if Assigned(FProcess) then
    FreeAndNil(FProcess);
  FProcess := TExProcess.Create;
  FProcess.OnReadLn := @OnReadLn;

  with FSearchRec do
  begin
    // TProcess doesn't support passing parameters other than quoted in "".
    // Adapt this code when this changes.
    sSearch := String(StartPath);
    if sSearch <> '' then
    begin
      // Search in given start path and in subdirectories.
      sSearch := '"' + IncludeTrailingPathDelimiter(sSearch) + String(FileMask) + '" ' +
                 '"' + IncludeTrailingPathDelimiter(sSearch) + '*' + PathDelim + String(FileMask) + '"';
    end
    else
      sSearch := '"' + String(FileMask) + '"';
  end;

  if LocatePath <> '' then
    FProcess.SetCmdLine(LocatePath + ' ' + sSearch);
  FProcess.Execute;
end;

procedure TPlugInfo.Stop;
begin
  if Assigned(FProcess) then
  begin
    FProcess.Stop;
    FreeAndNil(FProcess);
  end;
end;

procedure TPlugInfo.OnReadLn(str: String);
begin
  if str <> '' then
    Inc(FilesScanned);
  FAddProc(PluginNr, PChar(str));
  FUpdateProc(PluginNr, PChar(str), FilesScanned);
end;


{Main --------------------------------------------------------------------------------}

function Init(dps: PDsxDefaultParamStruct; pAddFileProc: TSAddFileProc;
  pUpdateStatus: TSUpdateStatusProc): Integer; dcpcall;
var
  i: Integer;
begin
  if not assigned(List) then
    List := TStringList.Create;
  I := List.Count;
  List.AddObject(IntToStr(I), TPlugInfo.Create(I));
  TPlugInfo(List.Objects[I]).SetProcs(pAddFileProc, pUpdateStatus);
  Result := I;
end;

procedure StartSearch(FPluginNr: Integer; pSearchRecRec: PDsxSearchRecord); dcpcall;
begin
  TPlugInfo(List.Objects[FPluginNr]).SetDefs(pSearchRecRec);
  TPlugInfo(List.Objects[FPluginNr]).Start;
end;

procedure StopSearch(FPluginNr: Integer); dcpcall;
begin
  TPlugInfo(List.Objects[FPluginNr]).Stop;
end;

procedure Finalize(FPluginNr: Integer); dcpcall;
begin
  if not Assigned(List) then
    exit;
  if (FPluginNr > List.Count) or (FPluginNr < 0) or (List.Count = 0) then
    exit;

  //Destroy PlugInfo Item №
  TPlugInfo(List.Objects[FPluginNr]).Free;
  List.Delete(FPluginNr);
  if List.Count = 0 then
    FreeAndNil(List);
end;

exports
  Init,
  StartSearch,
  StopSearch,
  Finalize;

type
  Tx = class
    procedure OnReadLnWhich(str: String);
  end;

procedure Tx.OnReadLnWhich(str: String);
begin
  if str <> '' then
  begin
    LocatePath := str;
    //WriteLn('PLUGIN: locate found in '+str);
  end;
end;

var
  Pr: TExProcess;
  x:  TX;

{$R *.res}

begin
  pr := TExProcess.Create('which locate');
  x  := Tx.Create;
  pr.OnReadLn := @x.OnReadLnWhich;
  pr.Execute;
  pr.Free;
  x.Free;
  {$IFDEF UNIX}
  if LocatePath = '' then
    Writeln('DSXLocate: Locate utility not found.');
  {$ENDIF}
end.

