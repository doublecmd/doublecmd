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

unit uDsxModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs, LCLProc, DsxPlugin, DCClassesUtf8, uDCUtils,
  DCXmlConfig;

type

  { TDsxModule }

  TDsxModule = class
  protected
    SStartSearch: TSStartSearch;
    SStopSearch: TSStopSearch;
    SAddFileProc: TSAddFileProc;
    SUpdateStatusProc: TSUpdateStatusProc;
    SInit: TSInit;
    SFinalize: TSFinalize;
  private
    FPluginNr: integer;
    FModuleHandle: TLibHandle;  // Handle to .DLL or .so
    function GIsLoaded: boolean;
  public
    Name: string;
    FileName: string;
    Descr: string;
    //---------------------
    constructor Create;
    destructor Destroy; override;
    //---------------------
    function LoadModule: boolean;
    procedure UnloadModule;
    //---------------------
    function CallInit(pAddFileProc: TSAddFileProc; pUpdateStatus: TSUpdateStatusProc): integer;
    procedure CallStartSearch(SearchRec: TDsxSearchRecord);
    procedure CallStopSearch;
    procedure CallFinalize;
    //---------------------
    property IsLoaded: boolean read GIsLoaded;
    property ModuleHandle: TLibHandle read FModuleHandle write FModuleHandle;
  end;

  { TDSXModuleList }

  TDSXModuleList = class
  private
    Flist: TStringList;
    function GetCount: integer;
  public
    //---------------------
    constructor Create;
    destructor Destroy; override;
    //---------------------
    procedure Clear;
    procedure Load(Ini: TIniFileEx); overload;
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    procedure Save(Ini: TIniFileEx); overload;
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    procedure DeleteItem(Index: integer);
    //---------------------
    function Add(Item: TDSXModule): integer; overload;
    function Add(FileName: string): integer; overload;
    function Add(AName, FileName, Descr: string): integer; overload;
    //---------------------
    procedure Assign(OtherList: TDSXModuleList);
    //---------------------
    function IsLoaded(AName: string): boolean; overload;
    function IsLoaded(Index: integer): boolean; overload;
    function LoadModule(AName: string): boolean; overload;
    function LoadModule(Index: integer): boolean; overload;
    //---------------------
    function GetDSXModule(Index: integer): TDSXModule; overload;
    function GetDSXModule(AName: string): TDSXModule; overload;
    //---------------------
    property Count: integer read GetCount;
  end;


implementation

uses
  DCOSUtils, uDebug, uGlobs, uGlobsPaths;

const
  DsxIniFileName = 'dsx.ini';

{ TDsxModule }

function TDsxModule.GIsLoaded: boolean;
begin
  Result := FModuleHandle <> 0;
end;

constructor TDsxModule.Create;
begin
  FModuleHandle := 0;
  inherited Create;
end;

destructor TDsxModule.Destroy;
begin
  if GIsLoaded then
    UnloadModule;
  inherited Destroy;
end;

function TDsxModule.LoadModule: boolean;
begin
  FModuleHandle := mbLoadLibrary(Self.FileName);
  Result := (FModuleHandle <> 0);
  if FModuleHandle = 0 then
    exit;

  SStopSearch := TSStopSearch(GetProcAddress(FModuleHandle, 'StopSearch'));
  SStartSearch := TSStartSearch(GetProcAddress(FModuleHandle, 'StartSearch'));
  SInit := TSInit(GetProcAddress(FModuleHandle, 'Init'));
  SFinalize := TSFinalize(GetProcAddress(FModuleHandle, 'Finalize'));
end;

procedure TDsxModule.UnloadModule;
begin
  if Assigned(SFinalize) then
    SFinalize(FPluginNr);

{$IF (not DEFINED(LINUX)) or ((FPC_VERSION > 2) or ((FPC_VERSION=2) and (FPC_RELEASE >= 5)))}
  if FModuleHandle <> 0 then
    FreeLibrary(FModuleHandle);
{$ENDIF}
  FModuleHandle := 0;

  SStartSearch := nil;
  SStopSearch := nil;
  SInit := nil;
  SFinalize := nil;
end;

function TDsxModule.CallInit(pAddFileProc: TSAddFileProc; pUpdateStatus: TSUpdateStatusProc): integer;
var
  dps: TDsxDefaultParamStruct;
begin
  if Assigned(SInit) then
  begin
    dps.DefaultIniName := gpCfgDir + DsxIniFileName;
    dps.PluginInterfaceVersionHi := 0;
    dps.PluginInterfaceVersionLow := 10;
    dps.size  := SizeOf(TDsxDefaultParamStruct);
    FPluginNr := Sinit(@dps, pAddFileProc, pUpdateStatus);
    Result    := FPluginNr;
  end;
end;

procedure TDsxModule.CallStartSearch(SearchRec: TDsxSearchRecord);
begin
  if Assigned(SStartSearch) then
    SStartSearch(FPluginNr, @SearchRec);
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
    Result := Flist.Count
  else
    Result := 0;
end;

constructor TDSXModuleList.Create;
begin
  Flist := TStringList.Create;
end;

destructor TDSXModuleList.Destroy;
begin
  Clear;
  FreeAndNil(Flist);

  inherited Destroy;
end;

procedure TDSXModuleList.Clear;
begin
  while Flist.Count > 0 do
  begin
    TDSXModule(Flist.Objects[0]).Free;
    Flist.Delete(0);
  end;
end;

procedure TDSXModuleList.Load(Ini: TIniFileEx);
var
  xCount, I: integer;
  tmp: string;
begin
  Self.Clear;
  xCount := Ini.ReadInteger('Search Plugins', 'PluginCount', 0);
  if xCount = 0 then
    Exit;

  for i := 0 to xCount - 1 do
  begin
    tmp := Ini.ReadString('Search Plugins', 'Plugin' + IntToStr(I + 1) + 'Name', '');
    Flist.AddObject(UpCase(tmp), TDSXModule.Create);
    TDSXModule(Flist.Objects[I]).Name := tmp;
    TDSXModule(Flist.Objects[I]).Descr :=
      Ini.ReadString('Search Plugins', 'Plugin' + IntToStr(I + 1) + 'Description', '');
    TDSXModule(Flist.Objects[I]).FileName :=
      GetCmdDirFromEnvVar(Ini.ReadString('Search Plugins', 'Plugin' + IntToStr(I + 1) + 'Path', ''));
  end;
end;

procedure TDSXModuleList.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  AName, APath: String;
  ADsxModule: TDSXModule;
begin
  Clear;

  ANode := ANode.FindNode('DsxPlugins');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('DsxPlugin') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', AName) and
           AConfig.TryGetValue(ANode, 'Path', APath) then
        begin
          ADsxModule := TDsxModule.Create;
          Flist.AddObject(UpCase(AName), ADsxModule);
          ADsxModule.Name := AName;
          ADsxModule.FileName := GetCmdDirFromEnvVar(APath);
          ADsxModule.Descr := AConfig.GetValue(ANode, 'Description', '');
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TDSXModuleList.Save(Ini: TIniFileEx);
var
  i: integer;
begin
  Ini.EraseSection('Search Plugins');
  Ini.WriteInteger('Search Plugins', 'PluginCount', Flist.Count);
  for i := 0 to Flist.Count - 1 do
  begin
    Ini.WriteString('Search Plugins', 'Plugin' + IntToStr(I + 1) + 'Name',
      TDSXModule(Flist.Objects[I]).Name);
    Ini.WriteString('Search Plugins', 'Plugin' + IntToStr(I + 1) +
      'Description', TDSXModule(Flist.Objects[I]).Descr);
    Ini.WriteString('Search Plugins', 'Plugin' + IntToStr(I + 1) + 'Path',
      SetCmdDirAsEnvVar(TDSXModule(Flist.Objects[I]).FileName));
  end;
end;

procedure TDSXModuleList.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  i: Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, 'DsxPlugins', True);
  AConfig.ClearNode(ANode);

  for i := 0 to Flist.Count - 1 do
  begin
    SubNode := AConfig.AddNode(ANode, 'DsxPlugin');
    AConfig.AddValue(SubNode, 'Name', TDSXModule(Flist.Objects[I]).Name);
    AConfig.AddValue(SubNode, 'Path', SetCmdDirAsEnvVar(TDSXModule(Flist.Objects[I]).FileName));
    AConfig.AddValue(SubNode, 'Description', TDSXModule(Flist.Objects[I]).Descr);
  end;
end;

procedure TDSXModuleList.DeleteItem(Index: integer);
begin
  if (Index > -1) and (Index < Flist.Count) then
  begin
    TDSXModule(Flist.Objects[Index]).Free;
    Flist.Delete(Index);
  end;
end;

function TDSXModuleList.Add(Item: TDSXModule): integer;
begin
  Result := Flist.AddObject(UpCase(item.Name), Item);
end;

function TDSXModuleList.Add(FileName: string): integer;
var
  s: string;
begin
  s := ExtractFileName(FileName);
  if pos('.', s) > 0 then
    Delete(s, pos('.', s), length(s));
  Result := Flist.AddObject(UpCase(s), TDSXModule.Create);
  TDSXModule(Flist.Objects[Result]).Name := s;
  TDSXModule(Flist.Objects[Result]).FileName := FileName;
end;

function TDSXModuleList.Add(AName, FileName, Descr: string): integer;
begin
  Result := Flist.AddObject(UpCase(AName), TDSXModule.Create);
  TDSXModule(Flist.Objects[Result]).Name := AName;
  TDSXModule(Flist.Objects[Result]).Descr := Descr;
  TDSXModule(Flist.Objects[Result]).FileName := FileName;
end;

procedure TDSXModuleList.Assign(OtherList: TDSXModuleList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to OtherList.Flist.Count - 1 do
  begin
    with TDSXModule(OtherList.Flist.Objects[I]) do
      Add(Name, FileName, Descr);
  end;
end;

function TDSXModuleList.IsLoaded(AName: string): boolean;
var
  x: integer;
begin
  x := Flist.IndexOf(AName);
  if x = -1 then
    Result := False
  else
  begin
    Result := GetDSXModule(x).IsLoaded;
  end;
end;

function TDSXModuleList.IsLoaded(Index: integer): boolean;
begin
  Result := GetDSXModule(Index).IsLoaded;
end;

function TDSXModuleList.LoadModule(AName: string): boolean;
var
  x: integer;
begin
  x := Flist.IndexOf(UpCase(AName));
  if x = -1 then
    Result := False
  else
  begin
    Result := GetDSXModule(x).LoadModule;
  end;
end;

function TDSXModuleList.LoadModule(Index: integer): boolean;
begin
  Result := GetDSXModule(Index).LoadModule;
end;

function TDSXModuleList.GetDSXModule(Index: integer): TDSXModule;
begin
  Result := TDSXModule(Flist.Objects[Index]);
end;

function TDSXModuleList.GetDSXModule(AName: string): TDSXModule;
var
  tmp: integer;
begin
  tmp := Flist.IndexOf(upcase(AName));
  if tmp > -1 then
    Result := TDSXModule(Flist.Objects[tmp]);
end;

end.

