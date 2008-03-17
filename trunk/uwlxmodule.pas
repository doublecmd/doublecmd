{
   Double Commander
   -------------------------------------------------------------------------
   WLX-API implementation.
   (TC WLX-API v1.8)

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


unit uwlxmodule; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dynlibs,uDetectStr,uwlxprototypes,WLXPlugin,Inifiles,uDCUtils,uGlobs{,LCLProc};

type

{ TWLXModule }

     TWLXModule = class
      protected
        //a) Mandatory (must be implemented)
        ListLoad:TListLoad;
        //b) Optional (must NOT be implemented if unsupported!)
        ListLoadNext:TListLoadNext;
        ListCloseWindow:TListCloseWindow;
        ListGetDetectString:TListGetDetectString;
        ListSearchText:TListSearchText;
        ListSearchDialog:TListSearchDialog;
        ListSendCommand:TListSendCommand;
        ListPrint:TListPrint;
        ListNotificationReceived:TListNotificationReceived;
        ListSetDefaultParams:TListSetDefaultParams;
        ListGetPreviewBitmap:TListGetPreviewBitmap;
      private
        FModuleHandle:TLibHandle;  // Handle to .DLL or .so
        FForce:boolean;
        FParser:TParserControl;
        FPluginWindow:THandle;
        function GIsLoaded:boolean;
      public
        Name:string;
        FileName:string;
        DetectStr:string;
        pShowFlags:integer;
        //---------------------
        constructor Create;
        destructor Destroy; override;
        //---------------------
        function LoadModule:Boolean;
        procedure UnloadModule;
        //---------------------
        function CallListLoad(ParentWin:THandle; FileToLoad:string; ShowFlags:integer):THandle;
        function CallListLoadNext(ParentWin: THandle; FileToLoad: string; ShowFlags: integer): integer;
        function CallListGetDetectString:string;
        procedure CallListSetDefaultParams;
        procedure CallListCloseWindow;
        function CallListGetPreviewBitmap(FileToLoad: string; width, height: integer; contentbuf: string): hbitmap;
        function CallListNotificationReceived(Msg, wParam, lParam: integer): integer;
        function CallListPrint(FileToPrint, DefPrinter: string; PrintFlags: integer; var Margins: trect): integer;
        function CallListSearchDialog(FindNext: integer): integer;
        function CallListSearchText(SearchString: string; SearchParameter: integer): integer;
        function CallListSendCommand(Command, Parameter: integer): integer;
        //---------------------
        property IsLoaded:boolean read GIsLoaded;
        property ModuleHandle:TLibHandle read FModuleHandle write FModuleHandle;
        property Force:boolean read FForce write FForce;
        property PluginWindow:THandle read FPluginWindow;
      end;

      { TWLXModuleList }

      TWLXModuleList = class
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
        procedure Load(Ini:TiniFile); overload;
        procedure Save(FileName:string);overload;
        procedure Save(Ini:TIniFile); overload;
        procedure DeleteItem(Index: integer);
        //---------------------
        function Add(Item:TWLXModule):integer;overload;
        function Add(FileName:string):integer;overload;
        function Add(AName,FileName,DetectStr:string):integer;overload;

        function IsLoaded(AName:String):Boolean;overload;
        function IsLoaded(Index: integer):Boolean;overload;
        function LoadModule(AName:String):Boolean; overload;
        function LoadModule(Index: integer): Boolean; overload;

        function GetWLxModule(Index:integer):TWLXModule;overload;
        function GetWlxModule(AName:string):TWLXModule;overload;
        //---------------------
        //property WlxList:TStringList read Flist;
        property Count:integer read GetCount;
      end;



implementation

{ TWLXModule }

function TWLXModule.GIsLoaded: boolean;
begin
  Result:=FModuleHandle<>0;
end;

constructor TWLXModule.Create;
begin
FParser:=TParserControl.Create;
end;

destructor TWLXModule.Destroy;
begin
  if GIsLoaded then UnloadModule;
  if assigned(FParser) then
    FParser.Free;
    
  inherited Destroy;
end;

function TWLXModule.LoadModule: Boolean;
begin
//  DebugLn('WLXM LoadModule entered');
  FModuleHandle := LoadLibrary(Self.FileName);
  Result := (FModuleHandle <> 0);
  if  FModuleHandle = 0 then exit;

        ListLoad:=TListLoad(GetProcAddress(FModuleHandle,'ListLoad'));
        ListLoadNext:=TListLoadNext(GetProcAddress(FModuleHandle,'ListLoadNext'));
        ListCloseWindow:=TListCloseWindow(GetProcAddress(FModuleHandle,'ListCloseWindow'));
        ListGetDetectString:=TListGetDetectString(GetProcAddress(FModuleHandle,'ListGetDetectString'));
        ListSearchText:=TListSearchText(GetProcAddress(FModuleHandle,'ListSearchText'));
        ListSearchDialog:=TListSearchDialog(GetProcAddress(FModuleHandle,'ListSearchDialog'));
        ListSendCommand:=TListSendCommand(GetProcAddress(FModuleHandle,'ListSendCommand'));
        ListPrint:=TListPrint(GetProcAddress(FModuleHandle,'ListPrint'));
        ListNotificationReceived:=TListNotificationReceived(GetProcAddress(FModuleHandle,'ListNotificationReceived'));
        ListSetDefaultParams:=TListSetDefaultParams(GetProcAddress(FModuleHandle,'ListSetDefaultParams'));
        ListGetPreviewBitmap:=TListGetPreviewBitmap(GetProcAddress(FModuleHandle,'ListGetPreviewBitmap'));
//DebugLn('WLXM LoadModule Leaved');
end;

procedure TWLXModule.UnloadModule;
begin
//  DebugLn('Try to call ListCloseWindow');
   CallListCloseWindow;
//  DebugLn('Call ListCloseWindow succses');

  if FModuleHandle <> 0 then
      FreeLibrary(FModuleHandle);
    FModuleHandle := 0;

    ListLoad:=nil;
    ListLoadNext:=nil;
    ListCloseWindow:=nil;
    ListGetDetectString:=nil;
    ListSearchText:=nil;
    ListSearchDialog:=nil;
    ListSendCommand:=nil;
    ListPrint:=nil;
    ListNotificationReceived:=nil;
    ListSetDefaultParams:=nil;
    ListGetPreviewBitmap:=nil;
end;

function TWLXModule.CallListLoad(ParentWin: THandle; FileToLoad: string;
  ShowFlags: integer): THandle;
begin
  if not assigned(ListLoad) then exit; //To prevent crash.

  FPluginWindow:=ListLoad(ParentWin, pChar(FileToLoad), ShowFlags);
  Result:=FPluginWindow;
end;

function TWLXModule.CallListLoadNext(ParentWin: THandle;
  FileToLoad: string; ShowFlags: integer): integer;
begin
  if assigned(ListLoadNext) then
  Result:=ListLoadNext(ParentWin,FPluginWindow,PChar(FileToLoad),ShowFlags)
  //else Result:=LISTPLUGIN_ERROR;
end;

procedure TWLXModule.CallListCloseWindow;
begin
if not assigned(ListCloseWindow) then exit;
  try
    ListCloseWindow(FPluginWindow);
  finally
    FPluginWindow:=0;
  end;
end;

function TWLXModule.CallListGetDetectString: string;
var pc:Pchar;
begin
//DebugLn('GetDetectstr Entered');
  if assigned(ListGetDetectString) then
   begin
     GetMem(pc,MAX_PATH);
     ListGetDetectString(pc,MAX_PATH);
     Result:=StrPas(pc);
     FreeMem(pc);
   end
  else
    Result:='';
//DebugLn('GetDetectStr Leaved');
end;

function TWLXModule.CallListSearchText(SearchString: string;
  SearchParameter: integer): integer;
begin
    if Assigned(ListSearchText) then
      begin
        Result:=ListSearchText(FPluginWindow, PChar(SearchString), SearchParameter);
      end
    else Result:=LISTPLUGIN_ERROR;
end;

function TWLXModule.CallListSearchDialog(FindNext: integer
  ): integer;
begin
   if Assigned(ListSearchDialog) then
     begin
       Result:=ListSearchDialog(FPluginWindow, FindNext);
     end
   else Result:=LISTPLUGIN_ERROR;
end;

function TWLXModule.CallListSendCommand(Command,  Parameter: integer): integer;
begin
   if Assigned(ListSendCommand) then
     begin
       Result:=ListSendCommand(FPluginWindow, Command, Parameter);
     end
   else Result:=LISTPLUGIN_ERROR;
end;

function TWLXModule.CallListPrint(FileToPrint,  DefPrinter: string; PrintFlags: integer; var Margins: trect): integer;
begin
  if Assigned(ListPrint) then
    begin
      Result:=ListPrint(FPluginWindow, PChar(FileToPrint), PChar(DefPrinter), PrintFlags, Margins);
    end
  else Result:=LISTPLUGIN_ERROR;
end;

function TWLXModule.CallListNotificationReceived(Msg, wParam, lParam: integer): integer;
begin
  if Assigned(ListNotificationReceived) then
    begin
      Result:=ListNotificationReceived(FPluginWindow, Msg, wParam,lParam);
    end;
end;

procedure TWLXModule.CallListSetDefaultParams;
  var dps:pListDefaultParamStruct;
begin
  if not assigned(ListSetDefaultParams) then exit;

  GetMem(dps,SizeOf(tListDefaultParamStruct));
  dps^.DefaultIniName:=gini.FileName;
  dps^.PluginInterfaceVersionHi:=1;
  dps^.PluginInterfaceVersionLow:=80;
  dps^.size:=SizeOf(tListDefaultParamStruct);
  ListSetDefaultParams(dps);
  FreeMem(dps,SizeOf(tListDefaultParamStruct));
end;

function TWLXModule.CallListGetPreviewBitmap(FileToLoad: string; width,
  height: integer; contentbuf: string): hbitmap;
begin
  if Assigned(ListGetPreviewBitmap) then
      Result:=ListGetPreviewBitmap(PChar(FileToLoad), width, height, PChar(contentbuf), length(contentbuf));
end;

{ TWLXModuleList }

function TWLXModuleList.GetCount: integer;
begin
 if Assigned(Flist) then
  Result:=Flist.Count
 else Result:=0;
end;

constructor TWLXModuleList.Create;
begin
   Flist:=TStringList.Create;
end;

destructor TWLXModuleList.Destroy;
begin
  while Flist.Count>0 do
   begin
     if assigned(TWLXModule(Flist.Objects[0])) then
       TWLXModule(Flist.Objects[0]).Free;
     Flist.Delete(0);
   end;
   FreeAndNil(Flist);

  inherited Destroy;
end;

procedure TWLXModuleList.Clear;
begin
  while Flist.Count>0 do
   begin
     TWLXModule(Flist.Objects[0]).Free;
     Flist.Delete(0);
   end;
end;

procedure TWLXModuleList.Load(FileName: string);
var Ini:TIniFile;
begin
  try
    Ini:=TIniFile.Create(FileName);
    Load(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TWLXModuleList.Load(Ini: TiniFile);
var xCount,I:integer;
    tmp:string;
begin
  Self.Clear;
  xCount:=Ini.ReadInteger('Lister Plugins','PluginCount',0);
  if xCount=0 then Exit;

  For i:=0 to xCount-1 do
    begin
      tmp:=Ini.ReadString('Lister Plugins','Plugin'+IntToStr(I+1)+'Name','');
      Flist.AddObject(UpCase(tmp),TWLXModule.Create);
      TWLXModule(Flist.Objects[I]).Name:=tmp;
      TWLXModule(Flist.Objects[I]).DetectStr:=Ini.ReadString('Lister Plugins','Plugin'+IntToStr(I+1)+'Detect','');
      TWLXModule(Flist.Objects[I]).FileName:=GetCmdDirFromEnvVar(Ini.ReadString('Lister Plugins','Plugin'+IntToStr(I+1)+'Path',''));
    end;
end;

procedure TWLXModuleList.Save(FileName: string);
 var  Ini:TIniFile;
begin
  try
    Ini:=TIniFile.Create(FileName);
     Save(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TWLXModuleList.Save(Ini: TIniFile);
var i:integer;
begin
 Ini.EraseSection('Lister Plugins');
 Ini.WriteInteger('Lister Plugins','PluginCount',Flist.Count);
  For i:=0 to Flist.Count-1 do
    begin
      Ini.WriteString('Lister Plugins','Plugin'+IntToStr(I+1)+'Name',TWLXModule(Flist.Objects[I]).Name);
      Ini.WriteString('Lister Plugins','Plugin'+IntToStr(I+1)+'Detect',TWLXModule(Flist.Objects[I]).DetectStr);
      Ini.WriteString('Lister Plugins','Plugin'+IntToStr(I+1)+'Path',SetCmdDirAsEnvVar(TWLXModule(Flist.Objects[I]).FileName));
    end;
end;

procedure TWLXModuleList.DeleteItem(Index: integer);
begin
  if (Index>-1) and (Index<Flist.Count) then
   begin
    TWLXModule(Flist.Objects[Index]).Free;
    Flist.Delete(Index);
   end;
end;

function TWLXModuleList.Add(Item: TWLXModule): integer;
begin
  Result:=Flist.AddObject(UpCase(item.Name),Item);
end;

function TWLXModuleList.Add(FileName: string): integer;
var s:string;
begin
//    DebugLn('WLXLIST Add entered');
    s:=ExtractFileName(FileName);
    if pos('.',s)>0 then
      delete(s,pos('.',s),length(s));
    Result:=Flist.AddObject(UpCase(s),TWLXModule.Create);
    TWLXModule(Flist.Objects[Result]).Name:=s;
    TWLXModule(Flist.Objects[Result]).FileName:=FileName;
    if TWLXModule(Flist.Objects[Result]).LoadModule then
    begin
      TWLXModule(Flist.Objects[Result]).DetectStr:=TWLXModule(Flist.Objects[Result]).CallListGetDetectString;
      TWLXModule(Flist.Objects[Result]).UnloadModule;
    end;
//    DebugLn('WLXLIST ADD Leaved');
end;

function TWLXModuleList.Add(AName, FileName, DetectStr: string): integer;
begin
      Result:=Flist.AddObject(UpCase(AName),TWLXModule.Create);
      TWLXModule(Flist.Objects[Result]).Name:=AName;
      TWLXModule(Flist.Objects[Result]).DetectStr:=DetectStr;
      TWLXModule(Flist.Objects[Result]).FileName:=FileName;
end;

function TWLXModuleList.IsLoaded(AName: String): Boolean;
var x:integer;
begin
  x:=Flist.IndexOf(AName);
  if x=-1 then Result:=false
  else
    begin
      Result:=GetWLxModule(x).IsLoaded;
    end;
end;

function TWLXModuleList.IsLoaded(Index: integer): Boolean;
begin
  Result:=GetWLxModule(Index).IsLoaded;
end;

function TWLXModuleList.LoadModule(AName: String): Boolean;
var x:integer;
begin
  x:=Flist.IndexOf(UpCase(AName));
  if x=-1 then Result:=false
  else
    begin
      Result:=GetWlxModule(x).LoadModule;
    end;

end;

function TWLXModuleList.LoadModule(Index: integer): Boolean;
begin
   Result:=GetWlxModule(Index).LoadModule;
end;

function TWLXModuleList.GetWLxModule(Index: integer): TWLXModule;
begin
  Result:=TWLXModule(Flist.Objects[Index]);
end;

function TWLXModuleList.GetWlxModule(AName: string): TWLXModule;
var tmp:integer;
begin
  tmp:=Flist.IndexOf(upcase(AName));
  if tmp>-1 then
  Result:=TWLXModule(Flist.Objects[tmp]);
end;


end.

