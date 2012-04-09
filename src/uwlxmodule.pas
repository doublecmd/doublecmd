{
   Double Commander
   -------------------------------------------------------------------------
   WLX-API implementation.
   (TC WLX-API v1.8)

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)

   contributors:

   Copyright (C) 2009-2011  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, dynlibs, uDetectStr, uwlxprototypes, WLXPlugin,
  DCClassesUtf8, uDCUtils, LCLProc, LCLType, DCXmlConfig
  {$IFDEF LCLWIN32}
    , Windows
  {$ENDIF}
  {$IFDEF LCLGTK}
    ,gtk,glib,gdk
  {$ENDIF}
  {$IFDEF LCLGTK2}
    ,gtk2,glib2
  {$ENDIF}
  {$IFDEF LCLQT}
    ,qt4,qtwidgets
    // The Qt widgetset must be used to load plugins on qt
  {$ENDIF}
  ;

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
        //c) Unicode
        ListLoadW: TListLoadW;
        ListLoadNextW: TListLoadNextW;
        ListSearchTextW: TListSearchTextW;
        ListPrintW: TListPrintW;
        ListGetPreviewBitmapW: TListGetPreviewBitmapW;
      private
        FModuleHandle:TLibHandle;  // Handle to .DLL or .so
        FForce:boolean;
        FParser:TParserControl;
        FPluginWindow: HWND;
        function GetCanPrint: Boolean;
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
        function CallListLoad(ParentWin:HWND; FileToLoad:string; ShowFlags:integer): HWND;
        function CallListLoadNext(ParentWin: HWND; FileToLoad: string; ShowFlags: integer): integer;
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
//        function FileParamVSDetectStr(ptr:PFileRecItem):boolean; overload;
        function FileParamVSDetectStr(AFileName:String):boolean; //overload;
        //---------------------
        procedure ResizeWindow(aRect: TRect);
        //---------------------
        property IsLoaded:boolean read GIsLoaded;
        property ModuleHandle:TLibHandle read FModuleHandle write FModuleHandle;
        property Force:boolean read FForce write FForce;
        property PluginWindow: HWND read FPluginWindow;
        property CanPrint: Boolean read GetCanPrint;
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
        procedure Load(Ini:TIniFileEx); overload;
        procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
        procedure Save(Ini:TIniFileEx); overload;
        procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
        procedure DeleteItem(Index: integer);
        //---------------------
        function Add(Item:TWLXModule):integer;overload;
        function Add(FileName:string):integer;overload;
        function Add(AName,FileName,DetectStr:string):integer;overload;
        //---------------------
        procedure Assign(OtherList: TWLXModuleList);
        //---------------------
        function IsLoaded(AName:String):Boolean;overload;
        function IsLoaded(Index: integer):Boolean;overload;
        function LoadModule(AName:String):Boolean; overload;
        function LoadModule(Index: integer): Boolean; overload;
        //---------------------
        function GetWLxModule(Index:integer):TWLXModule;overload;
        function GetWlxModule(AName:string):TWLXModule;overload;
        //---------------------
        //---------------------
        //property WlxList:TStringList read Flist;
        property Count:integer read GetCount;
      end;

  Function WlxPrepareContainer(Ahandle:THandle; revert:boolean=false):boolean;

implementation

uses
  FileUtil, uDebug, DCOSUtils, uOSUtils, uGlobsPaths, uGlobs;

const
  WlxIniFileName = 'wlx.ini';

function WlxPrepareContainer(Ahandle: THandle; revert:boolean=false): boolean;
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  var lst:PGList;
{$ENDIF}
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
    if not revert then
      begin
        //Hide controls from our gtk container
        lst:=gtk_container_children(GTK_CONTAINER(PGtkwidget(AHandle)));
        if lst<>nil then
        begin
          gtk_widget_hide(PGtkWidget(lst^.data));
          Result:=true;
        end else Result:=false;
       Exit;
      end else
        begin
          //Show controls from our gtk container
          lst:=gtk_container_children(GTK_CONTAINER(PGtkwidget(AHandle)));
          if lst<>nil then
          begin
            gtk_widget_show(PGtkWidget(lst^.data));
            Result:=true;
          end else Result:=false;
         Exit;
        end;
{$ENDIF}
     Result:=true

end;

{ TWLXModule }

function TWLXModule.GIsLoaded: boolean;
begin
  Result:=FModuleHandle<>0;
end;

function TWLXModule.GetCanPrint: Boolean;
begin
  Result:= Assigned(ListPrint) or Assigned(ListPrintW);
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
//  DCDebug('WLXM LoadModule entered');
  FModuleHandle := mbLoadLibrary(Self.FileName);
  Result := (FModuleHandle <> 0);
  if  FModuleHandle = 0 then exit;
        { Mandatory }
        ListLoad:=TListLoad(GetProcAddress(FModuleHandle,'ListLoad'));
        { Optional }
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
        { Unicode }
        ListLoadW:= TListLoadW(GetProcAddress(FModuleHandle, 'ListLoadW'));
        ListLoadNextW:= TListLoadNextW(GetProcAddress(FModuleHandle, 'ListLoadNextW'));
        ListSearchTextW:= TListSearchTextW(GetProcAddress(FModuleHandle, 'ListSearchTextW'));
        ListPrintW:= TListPrintW(GetProcAddress(FModuleHandle, 'ListPrintW'));
        ListGetPreviewBitmapW:= TListGetPreviewBitmapW(GetProcAddress(FModuleHandle, 'ListGetPreviewBitmapW'));
//DCDebug('WLXM LoadModule Leaved');
end;

procedure TWLXModule.UnloadModule;
begin
{$IF (not DEFINED(LINUX)) or ((FPC_VERSION > 2) or ((FPC_VERSION=2) and (FPC_RELEASE >= 5)))}
  if FModuleHandle <> 0 then
    FreeLibrary(FModuleHandle);
{$ENDIF}
  FModuleHandle := 0;
  { Mandatory }
  ListLoad:=nil;
  { Optional }
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
  { Unicode }
  ListLoadW:= nil;
  ListLoadNextW:= nil;
  ListSearchTextW:= nil;
  ListPrintW:= nil;
  ListGetPreviewBitmapW:= nil;
end;

function TWLXModule.CallListLoad(ParentWin: HWND; FileToLoad: string;
  ShowFlags: integer): HWND;
begin
  {$IFDEF LCLQT}
  ParentWin:= HWND(TQtWidget(ParentWin).GetContainerWidget);
  {$ENDIF}

  if Assigned(ListLoadW) then
    FPluginWindow:= ListLoadW(ParentWin, PWideChar(UTF8Decode(FileToLoad)), ShowFlags)
  else if Assigned(ListLoad) then
    FPluginWindow:= ListLoad(ParentWin, PAnsiChar(UTF8ToSys(FileToLoad)), ShowFlags)
  else
    Exit(wlxInvalidHandle);

  Result:= FPluginWindow;
end;

function TWLXModule.CallListLoadNext(ParentWin: HWND;
  FileToLoad: string; ShowFlags: integer): integer;
begin
  {$IFDEF LCLQT}
  ParentWin:= HWND(TQtWidget(ParentWin).GetContainerWidget);
  {$ENDIF}

  if Assigned(ListLoadNextW) then
    Result:= ListLoadNextW(ParentWin, FPluginWindow, PWideChar(UTF8Decode(FileToLoad)), ShowFlags)
  else if Assigned(ListLoadNext) then
    Result:= ListLoadNext(ParentWin, FPluginWindow, PAnsiChar(UTF8ToSys(FileToLoad)), ShowFlags)
  else Result:= LISTPLUGIN_ERROR;
end;

procedure TWLXModule.CallListCloseWindow;
begin
  if not Assigned(ListCloseWindow) then Exit;
//  DCDebug('Try to call ListCloseWindow');
  try
    ListCloseWindow(FPluginWindow);
  finally
    FPluginWindow:=0;
  end;
//  DCDebug('Call ListCloseWindow success');
end;

function TWLXModule.CallListGetDetectString: string;
var pc:Pchar;
begin
//DCDebug('GetDetectstr Entered');
  if assigned(ListGetDetectString) then
   begin
     GetMem(pc,MAX_PATH);
     ListGetDetectString(pc,MAX_PATH);
     Result:=StrPas(pc);
     FreeMem(pc);
   end
  else
    Result:='';
//DCDebug('GetDetectStr Leaved');
end;

function TWLXModule.CallListSearchText(SearchString: string;
  SearchParameter: integer): integer;
begin
  if Assigned(ListSearchTextW) then
    Result:= ListSearchTextW(FPluginWindow, PWideChar(UTF8Decode(SearchString)), SearchParameter)
  else if Assigned(ListSearchText) then
    Result:= ListSearchText(FPluginWindow, PAnsiChar(UTF8ToSys(SearchString)), SearchParameter)
  else Result:= LISTPLUGIN_ERROR;
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

{function TWLXModule.FileParamVSDetectStr(ptr: PFileRecItem): boolean;
begin
  FParser.DetectStr:=Self.DetectStr;
  Result:=FParser.TestFileResult(ptr);
end;}

function TWLXModule.FileParamVSDetectStr(AFileName: String): boolean;
begin
  FParser.DetectStr:=Self.DetectStr;
  DCDebug('DetectStr = '+FParser.DetectStr);
  DCDebug('AFileName = '+AFileName);
  Result:=FParser.TestFileResult(AFileName);
end;

procedure TWLXModule.ResizeWindow(aRect: TRect);
begin
  //ToDo: Implement for other widgetsets
  {$IF DEFINED(LCLWIN32)}
  with aRect do
  MoveWindow(FPluginWindow, Left, Top, Right - Left, Bottom - Top, True);
  {$ENDIF}
end;

function TWLXModule.CallListPrint(FileToPrint,  DefPrinter: string; PrintFlags: integer; var Margins: trect): integer;
begin
  if Assigned(ListPrintW) then
    Result:= ListPrintW(FPluginWindow, PWideChar(UTF8Decode(FileToPrint)), PWideChar(UTF8Decode(DefPrinter)), PrintFlags, Margins)
  else if Assigned(ListPrint) then
    Result:= ListPrint(FPluginWindow, PAnsiChar(UTF8ToSys(FileToPrint)), PAnsiChar(UTF8ToSys(DefPrinter)), PrintFlags, Margins)
  else Result:= LISTPLUGIN_ERROR;
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
  dps^.DefaultIniName:= mbFileNameToSysEnc(gpCfgDir + WlxIniFileName);
  dps^.PluginInterfaceVersionHi:=1;
  dps^.PluginInterfaceVersionLow:=80;
  dps^.size:=SizeOf(tListDefaultParamStruct);
  ListSetDefaultParams(dps);
  FreeMem(dps,SizeOf(tListDefaultParamStruct));
end;

function TWLXModule.CallListGetPreviewBitmap(FileToLoad: string; width,
  height: integer; contentbuf: string): hbitmap;
begin
  if Assigned(ListGetPreviewBitmapW) then
      Result:= ListGetPreviewBitmapW(PWideChar(UTF8Decode(FileToLoad)), width, height, PChar(contentbuf), length(contentbuf))
  else if Assigned(ListGetPreviewBitmap) then
      Result:= ListGetPreviewBitmap(PAnsiChar(UTF8ToSys(FileToLoad)), width, height, PChar(contentbuf), length(contentbuf));
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
  Clear;
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

procedure TWLXModuleList.Load(Ini: TIniFileEx);
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

procedure TWLXModuleList.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  AName, APath: String;
  AWlxModule: TWLXModule;
begin
  Clear;

  ANode := ANode.FindNode('WlxPlugins');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('WlxPlugin') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', AName) and
           AConfig.TryGetValue(ANode, 'Path', APath) then
        begin
          AWlxModule := TWLXModule.Create;
          Flist.AddObject(UpCase(AName), AWlxModule);
          AWlxModule.Name := AName;
          AWlxModule.FileName := GetCmdDirFromEnvVar(APath);
          AWlxModule.DetectStr := AConfig.GetValue(ANode, 'DetectString', '');
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TWLXModuleList.Save(Ini: TIniFileEx);
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

procedure TWLXModuleList.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  i: Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, 'WlxPlugins', True);
  AConfig.ClearNode(ANode);

  for i := 0 to Flist.Count - 1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'WlxPlugin');
      AConfig.AddValue(SubNode, 'Name', TWLXModule(Flist.Objects[I]).Name);
      AConfig.AddValue(SubNode, 'Path', SetCmdDirAsEnvVar(TWLXModule(Flist.Objects[I]).FileName));
      AConfig.AddValue(SubNode, 'DetectString', TWLXModule(Flist.Objects[I]).DetectStr);
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
//    DCDebug('WLXLIST Add entered');
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
//    DCDebug('WLXLIST ADD Leaved');
end;

function TWLXModuleList.Add(AName, FileName, DetectStr: string): integer;
begin
      Result:=Flist.AddObject(UpCase(AName),TWLXModule.Create);
      TWLXModule(Flist.Objects[Result]).Name:=AName;
      TWLXModule(Flist.Objects[Result]).DetectStr:=DetectStr;
      TWLXModule(Flist.Objects[Result]).FileName:=FileName;
end;

procedure TWLXModuleList.Assign(OtherList: TWLXModuleList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to OtherList.Flist.Count - 1 do
  begin
    with TWLXModule(OtherList.Flist.Objects[I]) do
      Add(Name, FileName, DetectStr);
  end;
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

