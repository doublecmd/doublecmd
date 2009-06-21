{
   Double Commander
   -------------------------------------------------------------------------
   (DSX) Search plugin API implementation.
   DSX - Double commander Search eXtentions.
   
   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   
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


unit udsxmodule; 

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, dynlibs,LCLProc,uGlobs,udsxplugin,uClassesEx, uDCUtils, uOSUtils;

type


{Prototypes}
{Mandatory (must be implemented)}
TSInit=function (dps:pDSXDefaultParamStruct; pAddFileProc:TSAddFileProc; pUpdateStatus:TSUpdateStatusProc):integer; stdcall;
TSStartSearch= procedure(PluginNr:integer; StartPath:pchar; SearchAttrRec:TSearchAttrRecord); stdcall;
TSStopSearch=procedure (PluginNr:integer); stdcall;
TSFinalize=procedure (PluginNr:integer); stdcall;


{ TDsxModule }

TDsxModule=class
      protected
        SStartSearch:TSStartSearch;
        SStopSearch:TSStopSearch;
        SAddFileProc:TSAddFileProc;
        SUpdateStatusProc:TSUpdateStatusProc;
        SInit:TSInit;
        SFinalize:TSFinalize;
      private
        FPluginNr:integer;
        FModuleHandle:TLibHandle;  // Handle to .DLL or .so
        function GIsLoaded:boolean;
      public
        Name:string;
        FileName:string;
        Descr:string;
        //---------------------
        constructor Create;
        destructor Destroy; override;
        //---------------------
        function LoadModule:Boolean;
        procedure UnloadModule;
        //---------------------
        function CallInit(pAddFileProc:TSAddFileProc; pUpdateStatus:TSUpdateStatusProc):integer;
        procedure CallStartSearch(StartPath:pchar; SearchAttrRec:TSearchAttrRecord);
        procedure CallStopSearch;
        procedure CallFinalize;
        //---------------------
        property IsLoaded:boolean read GIsLoaded;
        property ModuleHandle:TLibHandle read FModuleHandle write FModuleHandle;
      end;
      
     { TDSXModuleList }

     TDSXModuleList = class
      private
        Flist:TStringList;
        function GetCount:integer;
      public
        //---------------------
        constructor Create;
        destructor Destroy; override;
        //---------------------
        procedure Clear;
        procedure Load(FileName:string);overload;
        procedure Load(Ini:TIniFileEx); overload;
        procedure Save(FileName:string);overload;
        procedure Save(Ini:TIniFileEx); overload;
        procedure DeleteItem(Index: integer);
        //---------------------
        function Add(Item:TDSXModule):integer;overload;
        function Add(FileName:string):integer;overload;
        function Add(AName,FileName,Descr:string):integer;overload;
        //---------------------
        function IsLoaded(AName:String):Boolean;overload;
        function IsLoaded(Index: integer):Boolean;overload;
        function LoadModule(AName:String):Boolean; overload;
        function LoadModule(Index: integer): Boolean; overload;
        //---------------------
        function GetDSXModule(Index:integer):TDSXModule;overload;
        function GetDSXModule(AName:string):TDSXModule;overload;
        //---------------------
        property Count:integer read GetCount;
      end;



implementation

uses
  uGlobsPaths;

const
  DsxIniFileName = 'dsx.ini';

{ TDsxModule }

function TDsxModule.GIsLoaded: boolean;
begin
  Result:=FModuleHandle<>0;
end;

constructor TDsxModule.Create;
begin

end;

destructor TDsxModule.Destroy;
begin
  if GIsLoaded then UnloadModule;
  inherited Destroy;
end;

function TDsxModule.LoadModule: Boolean;
begin
  FModuleHandle := mbLoadLibrary(Self.FileName);
  Result := (FModuleHandle <> 0);
  if  FModuleHandle = 0 then exit;

  SStopSearch:=TSStopSearch(GetProcAddress(FModuleHandle,'StopSearch'));
  SStartSearch:=TSStartSearch(GetProcAddress(FModuleHandle,'StartSearch'));
  SInit:=TSInit(GetProcAddress(FModuleHandle,'Init'));
  SFinalize:=TSFinalize(GetProcAddress(FModuleHandle,'Finalize'));
  
end;

procedure TDsxModule.UnloadModule;
begin
  if Assigned(SFinalize) then  SFinalize(FPluginNr);
     
  if FModuleHandle <> 0 then
      FreeLibrary(FModuleHandle);
    FModuleHandle := 0;

    SStartSearch:=nil;
    SStopSearch:=nil;
    SInit:=nil;
    SFinalize:=nil;
end;

function TDsxModule.CallInit(pAddFileProc: TSAddFileProc;
  pUpdateStatus: TSUpdateStatusProc):integer;
var dps:pDSXDefaultParamStruct;
begin
  if Assigned(SInit) then
    begin
      GetMem(dps,SizeOf(tDSXDefaultParamStruct));
      dps^.DefaultIniName:=gpIniDir + DsxIniFileName;
      dps^.PluginInterfaceVersionHi:=0;
      dps^.PluginInterfaceVersionLow:=10;
      dps^.size:=SizeOf(tDSXDefaultParamStruct);
      FPluginNr:=Sinit(dps,pAddFileProc,pUpdateStatus);
      Result:=FPluginNr;
      FreeMem(dps,SizeOf(tDSXDefaultParamStruct));
    end;
end;

procedure TDsxModule.CallStartSearch(StartPath: pchar;
  SearchAttrRec: TSearchAttrRecord);
begin
  if Assigned(SStartSearch) then
    begin
      SStartSearch(FPluginNr,StartPath,SearchAttrRec);
    end;
end;

procedure TDsxModule.CallStopSearch;
begin
  if Assigned(SStopSearch) then
     SStopSearch(FPluginNr);
end;

procedure TDsxModule.CallFinalize;
begin
  if Assigned(SFinalize) then
     SFinalize(FPluginNr);
end;

{ TDSXModuleList }

function TDSXModuleList.GetCount: integer;
begin
  if Assigned(Flist) then
  Result:=Flist.Count
 else Result:=0;
end;

constructor TDSXModuleList.Create;
begin
Flist:=TStringList.Create;
end;

destructor TDSXModuleList.Destroy;
begin
  while Flist.Count>0 do
   begin
     if assigned(TDSXModule(Flist.Objects[0])) then
       TDSXModule(Flist.Objects[0]).Free;
     Flist.Delete(0);
   end;
   FreeAndNil(Flist);

  inherited Destroy;
end;

procedure TDSXModuleList.Clear;
begin
  while Flist.Count>0 do
   begin
     TDSXModule(Flist.Objects[0]).Free;
     Flist.Delete(0);
   end;

end;

procedure TDSXModuleList.Load(FileName: string);
var Ini:TIniFileEx;
begin
  try
    Ini:=TIniFileEx.Create(FileName);
    Load(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TDSXModuleList.Load(Ini: TIniFileEx);
var xCount,I:integer;
    tmp:string;
begin
  Self.Clear;
  xCount:=Ini.ReadInteger('Search Plugins','PluginCount',0);
  if xCount=0 then Exit;

  For i:=0 to xCount-1 do
    begin
      tmp:=Ini.ReadString('Search Plugins','Plugin'+IntToStr(I+1)+'Name','');
      Flist.AddObject(UpCase(tmp),TDSXModule.Create);
      TDSXModule(Flist.Objects[I]).Name:=tmp;
      TDSXModule(Flist.Objects[I]).Descr:=Ini.ReadString('Search Plugins','Plugin'+IntToStr(I+1)+'Description','');
      TDSXModule(Flist.Objects[I]).FileName:=GetCmdDirFromEnvVar(Ini.ReadString('Search Plugins','Plugin'+IntToStr(I+1)+'Path',''));
    end;
end;

procedure TDSXModuleList.Save(FileName: string);
 var  Ini:TIniFileEx;
begin
  try
    Ini:=TIniFileEx.Create(FileName);
     Save(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TDSXModuleList.Save(Ini: TIniFileEx);
var i:integer;
begin
 Ini.EraseSection('Search Plugins');
 Ini.WriteInteger('Search Plugins','PluginCount',Flist.Count);
  For i:=0 to Flist.Count-1 do
    begin
      Ini.WriteString('Search Plugins','Plugin'+IntToStr(I+1)+'Name',TDSXModule(Flist.Objects[I]).Name);
      Ini.WriteString('Search Plugins','Plugin'+IntToStr(I+1)+'Description',TDSXModule(Flist.Objects[I]).Descr);
      Ini.WriteString('Search Plugins','Plugin'+IntToStr(I+1)+'Path',SetCmdDirAsEnvVar(TDSXModule(Flist.Objects[I]).FileName));
    end;
 end;

procedure TDSXModuleList.DeleteItem(Index: integer);
begin
  if (Index>-1) and (Index<Flist.Count) then
   begin
    TDSXModule(Flist.Objects[Index]).Free;
    Flist.Delete(Index);
   end;
end;

function TDSXModuleList.Add(Item: TDSXModule): integer;
begin
  Result:=Flist.AddObject(UpCase(item.Name),Item);
end;

function TDSXModuleList.Add(FileName: string): integer;
var s:string;
begin
    s:=ExtractFileName(FileName);
    if pos('.',s)>0 then
      delete(s,pos('.',s),length(s));
    Result:=Flist.AddObject(UpCase(s),TDSXModule.Create);
    TDSXModule(Flist.Objects[Result]).Name:=s;
    TDSXModule(Flist.Objects[Result]).FileName:=FileName;
end;

function TDSXModuleList.Add(AName, FileName, Descr: string): integer;
begin
      Result:=Flist.AddObject(UpCase(AName),TDSXModule.Create);
      TDSXModule(Flist.Objects[Result]).Name:=AName;
      TDSXModule(Flist.Objects[Result]).Descr:=Descr;
      TDSXModule(Flist.Objects[Result]).FileName:=FileName;
end;

function TDSXModuleList.IsLoaded(AName: String): Boolean;
var x:integer;
begin
  x:=Flist.IndexOf(AName);
  if x=-1 then Result:=false
  else
    begin
      Result:=GetDSXModule(x).IsLoaded;
    end;
end;

function TDSXModuleList.IsLoaded(Index: integer): Boolean;
begin
  Result:=GetDSXModule(Index).IsLoaded;
end;

function TDSXModuleList.LoadModule(AName: String): Boolean;
var x:integer;
begin
  x:=Flist.IndexOf(UpCase(AName));
  if x=-1 then Result:=false
  else
    begin
      Result:=GetDSXModule(x).LoadModule;
    end;
end;

function TDSXModuleList.LoadModule(Index: integer): Boolean;
begin
   Result:=GetDSXModule(Index).LoadModule;
end;

function TDSXModuleList.GetDSXModule(Index: integer): TDSXModule;
begin
  Result:=TDSXModule(Flist.Objects[Index]);
end;

function TDSXModuleList.GetDSXModule(AName: string): TDSXModule;
var tmp:integer;
begin
  tmp:=Flist.IndexOf(upcase(AName));
  if tmp>-1 then
  Result:=TDSXModule(Flist.Objects[tmp]);
end;

end.
