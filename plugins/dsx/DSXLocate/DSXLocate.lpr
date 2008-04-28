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

uses
  Classes,sysutils,udsxplugin,un_process;

var List:TStringList;
    LocatePath:String;

type

       { TPlugInfo }

       TPlugInfo = class
         private
          FProcess:TExProcess;
          FAddProc:TSAddFileProc;
          FUpdateProc:TSUpdateStatusProc;
          FSearchAttr:TSearchAttrRecord;
          FStartPath:String;
          FilesScaned:integer;
         public
          PluginNr:integer;
         //---------------------
         constructor Create(Nr:integer);
         procedure SetProcs(AddProc:TSAddFileProc; UpdateProc:TSUpdateStatusProc);
         procedure SetDefs(SearchAttr:TSearchAttrRecord; StartPath:String);
         destructor Destroy; override;
         //---------------------
         procedure Start;
         procedure Stop;
         procedure OnReadLn(str: string);
       end;

constructor TPlugInfo.Create(Nr:integer);
begin
  PluginNr:=Nr;
end;

procedure TPlugInfo.SetProcs(AddProc: TSAddFileProc; UpdateProc: TSUpdateStatusProc);
begin
FAddProc:=AddProc;
FUpdateProc:=UpdateProc;
end;

procedure TPlugInfo.SetDefs(SearchAttr: TSearchAttrRecord; StartPath: String);
begin
FSearchAttr:=SearchAttr;
FStartPath:=StartPath;
end;


destructor TPlugInfo.Destroy;
begin
if Assigned(FProcess) then FreeAndNil(FProcess);
  inherited Destroy;
end;

procedure TPlugInfo.Start;
begin
  FilesScaned:=0;
  FProcess:=TExProcess.Create();
  FProcess.OnReadLn:=@OnReadLn;
  FProcess.SetCmdLine(LocatePath+' '+string(FSearchAttr.rFileMask));
  FProcess.Execute;
end;

procedure TPlugInfo.Stop;
begin
  FProcess.Stop;
  FreeAndNil(FProcess);
end;

procedure TPlugInfo.OnReadLn(str: string);
begin
  FilesScaned:=FilesScaned+1;
  FAddProc(PluginNr,PChar(str));
  FUpdateProc(PluginNr,PChar(str),FilesScaned);
end;


{Main --------------------------------------------------------------------------------}

function Init(dps:pDSXDefaultParamStruct; pAddFileProc:TSAddFileProc; pUpdateStatus:TSUpdateStatusProc):integer; stdcall;
var i:integer;
begin
  if not assigned(List) then List:=TStringList.Create;
  I:=List.Count;
  List.AddObject(IntToStr(I),TPlugInfo.Create(I));
  TPlugInfo(List.Objects[I]).SetProcs(pAddFileProc,pUpdateStatus);
end;

procedure StartSearch(FPluginNr:integer; StartPath:pchar; SearchAttrRec:TSearchAttrRecord); stdcall;
begin
  TPlugInfo(List.Objects[FPluginNr]).SetDefs(SearchAttrRec,string(StartPath));
  TPlugInfo(List.Objects[FPluginNr]).Start;
end;

procedure StopSearch(FPluginNr:integer); stdcall;
begin
TPlugInfo(List.Objects[FPluginNr]).Stop;
end;

procedure Finalize(FPluginNr:integer); stdcall;
begin
if not Assigned(List) then exit;
if (FPluginNr>List.Count) or (FPluginNr<0) or (List.Count=0) then exit;

//Destroy PlugInfo Item №
  TPlugInfo(List.Objects[FPluginNr]).Free;
  List.Delete(FPluginNr);
  if List.Count=0 then
    FreeAndNil(List);
end;


exports
       Init,
       StartSearch,
       StopSearch,
       Finalize;
       
type Tx=class
        procedure OnReadLnWhich(str: string);
       end;

procedure Tx.OnReadLnWhich(str: string);
begin
  if str<>'' then
   begin
     LocatePath:=str;
     //WriteLn('PLUGIN: locate found in '+str);
   end;
end;

var Pr:TExProcess; x:TX;
begin
pr:=TExProcess.Create('which locate');
x:=Tx.Create;
pr.OnReadLn:=@(x.OnReadLnWhich);
pr.Execute;
x.free;
end.

