{
   Double Commander
   -------------------------------------------------------------------------
   WDX-API implementation.
   (TC WDX-API v1.5)
   
   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   
   Some ideas were found in sources of WdxGuide by Alexey Torgashin
   and SuperWDX by Pavel Dubrovsky and Dmitry Vorotilin.

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


unit uWDXModule;

{$mode delphi}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
    Classes, SysUtils, uClassesEx,
    uWdxPrototypes, WdxPlugin,
    dynlibs, uDCUtils, uOSUtils,
    uDetectStr, lua, LCLProc, uFile, uXmlConfig;
  
type

      { TWdxField }

      TWdxField = class
        FName: String;
        FUnits: String;
        FType: Integer;
        function GetUnitIndex(UnitName: String): Integer;
      end;
      
     { TWDXModule }

      TWDXModule = class
      private
        function GetAName:string;  virtual;abstract;
        function GetAFileName:string;  virtual;abstract;
        function GetADetectStr:string;  virtual;abstract;
        procedure SetAName (AValue:string);  virtual;abstract;
        procedure SetAFileName (AValue:string);  virtual;abstract;
        procedure SetADetectStr(const AValue: string);  virtual;abstract;
      public
        //---------------------
        function LoadModule:Boolean;  virtual;abstract;
        procedure UnloadModule;  virtual;abstract;
        function IsLoaded:boolean;  virtual;abstract;
        //---------------------
        function FieldList:TStringlist; virtual;abstract;
        function WdxFieldType(n: integer): string;
        function GetFieldIndex(FieldName:string):integer;  virtual;abstract;
        function FileParamVSDetectStr(const aFile: TFile):boolean;  virtual;abstract;
        //------------------------------------------------------
        procedure CallContentGetSupportedField; virtual;abstract;
        procedure CallContentSetDefaultParams; virtual;abstract;
        procedure CallContentStopGetValue(FileName:string); virtual;abstract;
        //---------------------
        function CallContentGetDefaultSortOrder(FieldIndex:integer):boolean;  virtual;abstract;
        function CallContentGetDetectString:string; virtual;abstract;
        function CallContentGetValue(FileName: string; FieldName:String; UnitName: String; flags: integer):string; overload; virtual;abstract;
        function CallContentGetValue(FileName: string; FieldIndex, UnitIndex: integer; flags: integer):string;overload; virtual;abstract;
        function CallContentGetSupportedFieldFlags(FieldIndex:integer):integer; virtual;abstract;
        {ContentSetValue
         ContentEditValue
         ContentSendStateInformation}
        //------------------------------------------------------
        property Name:string read GetAName write SetAName;
        property FileName:string read GetAFileName write SetAFileName;
        property DetectStr:string read GetADetectStr write SetADetectStr;
        //---------------------
      end;


    { TPluginWDX }

      TPluginWDX = class(TWDXModule)
      private
        FFieldsList:TStringList;
        FModuleHandle:TLibHandle;  // Handle to .DLL or .so
        FForce:boolean;
        FParser:TParserControl;
        FName:string;
        FFileName:string;
        FDetectStr:string;

        function GetAName:string;    override;
        function GetAFileName:string; override;
        function GetADetectStr: string;  override;
        
        procedure SetAName (AValue:string); override;
        procedure SetAFileName (AValue:string); override;
        procedure SetADetectStr(const AValue: string);override;
      protected
        //a) Mandatory (must be implemented)
        ContentGetSupportedField:TContentGetSupportedField;
        ContentGetValue:TContentGetValue;
        //b) Optional (must NOT be implemented if unsupported!)
        ContentGetDetectString:TContentGetDetectString;
        ContentSetDefaultParams:TContentSetDefaultParams;
        ContentStopGetValue:TContentStopGetValue;
        ContentGetDefaultSortOrder:TContentGetDefaultSortOrder;
        ContentPluginUnloading:TContentPluginUnloading;
        ContentGetSupportedFieldFlags:TContentGetSupportedFieldFlags;
        ContentSetValue:TContentSetValue;
        ContentEditValue:TContentEditValue;
        ContentSendStateInformation:TContentSendStateInformation;
        //c) Unicode
        ContentGetValueW: TContentGetValueW;
        ContentStopGetValueW: TContentStopGetValueW;
        ContentSetValueW: TContentSetValueW;
        ContentSendStateInformationW: TContentSendStateInformationW;
      public
        //---------------------
        constructor Create;
        destructor Destroy; override;
        //---------------------
        function LoadModule:Boolean;override;
        procedure UnloadModule;override;
        function IsLoaded:boolean; override;
        //---------------------
        function FieldList:TStringlist;override;
        function GetFieldIndex(FieldName:string):integer;override;
        function FileParamVSDetectStr(const aFile: TFile):boolean;override;
        //------------------------------------------------------
        procedure CallContentGetSupportedField;override;
        procedure CallContentSetDefaultParams;override;
        procedure CallContentStopGetValue(FileName:string);override;
        //---------------------
        function CallContentGetDefaultSortOrder(FieldIndex:integer):boolean;override;
        function CallContentGetDetectString:string;override;
        function CallContentGetValue(FileName: string; FieldName:String; UnitName: String; flags: integer):string; overload;override;
        function CallContentGetValue(FileName: string; FieldIndex, UnitIndex: integer; flags: integer):string;overload;override;
        function CallContentGetSupportedFieldFlags(FieldIndex:integer):integer;override;
        {ContentSetValue
         ContentEditValue
         ContentSendStateInformation}
        //------------------------------------------------------
        property ModuleHandle:TLibHandle read FModuleHandle write FModuleHandle;
        property Force:boolean read FForce write FForce;
        property Name:string read GetAName write SetAName;
        property FileName:string read GetAFileName write SetAFileName;
        property DetectStr:string read GetADetectStr write SetADetectStr;

        //---------------------
      end;
      
     { TLuaWdx }

      TLuaWdx = class (TWdxModule)
      private
        L:Plua_State;
        FFieldsList:TStringList;
        FForce:boolean;
        FParser:TParserControl;
        FName:string;
        FFileName:string;
        FDetectStr:string;

        function GetAName:string;    override;
        function GetAFileName:string; override;
        function GetADetectStr: string;  override;

        procedure SetAName (AValue:string); override;
        procedure SetAFileName (AValue:string); override;
        procedure SetADetectStr(const AValue: string);override;

        function DoScript(AName: string): integer;
        function WdxLuaContentGetSupportedField(Index: integer; var xFieldName, xUnits: string): integer;
        procedure WdxLuaContentPluginUnloading;
      public
        constructor Create;
        destructor Destroy; override;
        //---------------------
        function LoadModule:Boolean; override;
        procedure UnloadModule; override;
        function IsLoaded:boolean; override;
        //---------------------
        function FieldList:TStringlist;override;
        function GetFieldIndex(FieldName:string):integer; override;
        function FileParamVSDetectStr(const aFile: TFile):boolean;   override;
        //------------------------------------------------------
        procedure CallContentGetSupportedField;override;
        procedure CallContentSetDefaultParams;override;
        procedure CallContentStopGetValue(FileName:string);override;
        //---------------------
        function CallContentGetDefaultSortOrder(FieldIndex:integer):boolean;override;
        function CallContentGetDetectString:string;override;
        function CallContentGetValue(FileName: string; FieldName:String; UnitName: String; flags: integer):string; overload;override;
        function CallContentGetValue(FileName: string; FieldIndex, UnitIndex: integer; flags: integer):string;overload;override;
        function CallContentGetSupportedFieldFlags(FieldIndex:integer):integer;override;
        //---------------------
        property Force:boolean read FForce write FForce;
        property Name:string read GetAName write SetAName;
        property FileName:string read GetAFileName write SetAFileName;
        property DetectStr:string read GetADetectStr write SetADetectStr;


      end;


      { TWDXModuleList }

      TWDXModuleList = class
      private
        Flist:TStringList;
        function GetCount:integer;
      public
        //---------------------
        constructor Create;
        destructor Destroy; override;
        //---------------------
        procedure Assign(Source: TWDXModuleList);
        procedure Clear;
        procedure Load(Ini:TIniFileEx); overload;
        procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
        procedure Save(Ini:TIniFileEx); overload;
        procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
        procedure DeleteItem(Index: integer);
        //---------------------
        function Add(Item:TWDXModule):integer;overload;
        function Add(FileName:string):integer;overload;
        function Add(AName,FileName,DetectStr:string):integer;overload;

        function IsLoaded(AName:String):Boolean;overload;
        function IsLoaded(Index: integer):Boolean;overload;
        function LoadModule(AName:String):Boolean; overload;
        function LoadModule(Index: integer): Boolean; overload;
        
        function GetWdxModule(Index:integer):TWDXModule;overload;
        function GetWdxModule(AName:string):TWDXModule;overload;
        //---------------------
        //property WdxList:TStringList read Flist;
        property Count:integer read GetCount;
      end;

implementation

uses
  StrUtils, uGlobs, uGlobsPaths, FileUtil;

const
  WdxIniFileName = 'wdx.ini';

{ TWDXModuleList }

function TWDXModuleList.GetCount: integer;
begin
if Assigned(Flist) then
  Result:=Flist.Count
 else Result:=0;
end;

constructor TWDXModuleList.Create;
begin
  Flist:=TStringList.Create;
end;

destructor TWDXModuleList.Destroy;
begin
  while Flist.Count>0 do
   begin
     TWDXModule(Flist.Objects[0]).Free;
     Flist.Delete(0);
   end;
   FreeAndNil(Flist);
   
  inherited Destroy;
end;

procedure TWDXModuleList.Assign(Source: TWDXModuleList);
begin
  if Assigned(Source) then
    begin
      Clear;
      FList.Assign(Source.FList);
      Source.FList.Clear;
    end;
end;

procedure TWDXModuleList.Clear;
begin
  while Flist.Count>0 do
   begin
    TWDXModule(Flist.Objects[0]).Free;
     Flist.Delete(0);
   end;
end;

procedure TWDXModuleList.Load(Ini: TIniFileEx);
var Count,I:integer;
    tmp,tp:string;
begin
  Self.Clear;
  Count:=Ini.ReadInteger('Content Plugins','PluginCount',0);
  if Count=0 then Exit;
  
  For i:=0 to Count-1 do
    begin
      tmp:=Ini.ReadString('Content Plugins','Plugin'+IntToStr(I+1)+'Name','');
      // читать FileName - читать расширение и создавать нужный обьект
      tp:=GetCmdDirFromEnvVar(Ini.ReadString('Content Plugins','Plugin'+IntToStr(I+1)+'Path',''));
      DebugLn('WDX:LOAD:'+tp);
      if upcase(ExtractFileExt(tp))='.WDX' then
        Flist.AddObject(UpCase(tmp),TPluginWDX.Create)
      else {иначе проверка на скрипт}
      if upcase(ExtractFileExt(tp))='.LUA' then
        Flist.AddObject(UpCase(tmp),TLuaWdx.Create);
        
      TWDXModule(Flist.Objects[I]).Name:=tmp;
      TWDXModule(Flist.Objects[I]).DetectStr:=Ini.ReadString('Content Plugins','Plugin'+IntToStr(I+1)+'Detect','');
      TWDXModule(Flist.Objects[I]).FileName:=tp;//GetCmdDirFromEnvVar(Ini.ReadString('Content Plugins','Plugin'+IntToStr(I+1)+'Path',''));
    end;

end;

procedure TWDXModuleList.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  AName, APath: String;
  AWdxModule: TWDXModule;
begin
  Self.Clear;

  ANode := ANode.FindNode('WdxPlugins');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('WdxPlugin') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', AName) and
           AConfig.TryGetValue(ANode, 'Path', APath) then
        begin
          // Create a correct object based on plugin file extension.
          APath := GetCmdDirFromEnvVar(APath);
          DebugLn('WDX: LOAD: ' + APath);
          if UpCase(ExtractFileExt(APath)) = '.WDX' then
            AWdxModule := TPluginWDX.Create
          else if UpCase(ExtractFileExt(APath)) = '.LUA' then
            AWdxModule := TLuaWdx.Create
          else
            raise Exception.Create('Invalid WDX plugin: ' + APath);

          AWdxModule.Name := AName;
          AWdxModule.FileName := APath;
          AWdxModule.DetectStr := AConfig.GetValue(ANode, 'DetectString', '');
          Flist.AddObject(UpCase(AName), AWdxModule);
        end
        else
          DebugLn('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TWDXModuleList.Save(Ini: TIniFileEx);
var i:integer;
begin
 Ini.EraseSection('Content Plugins');
 Ini.WriteInteger('Content Plugins','PluginCount',Flist.Count);
  For i:=0 to Flist.Count-1 do
    begin
      Ini.WriteString('Content Plugins','Plugin'+IntToStr(I+1)+'Name',TWDXModule(Flist.Objects[I]).Name);
      Ini.WriteString('Content Plugins','Plugin'+IntToStr(I+1)+'Detect',TWDXModule(Flist.Objects[I]).DetectStr);
      Ini.WriteString('Content Plugins','Plugin'+IntToStr(I+1)+'Path',SetCmdDirAsEnvVar(TWDXModule(Flist.Objects[I]).FileName));
    end;
end;

procedure TWDXModuleList.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  i: Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, 'WdxPlugins', True);
  AConfig.ClearNode(ANode);

  For i:=0 to Flist.Count-1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'WdxPlugin');
      AConfig.AddValue(SubNode, 'Name', TWDXModule(Flist.Objects[I]).Name);
      AConfig.AddValue(SubNode, 'Path', SetCmdDirAsEnvVar(TWDXModule(Flist.Objects[I]).FileName));
      AConfig.AddValue(SubNode, 'DetectString', TWDXModule(Flist.Objects[I]).DetectStr);
    end;
end;

procedure TWDXModuleList.DeleteItem(Index: integer);
begin
  if (Index>-1) and (Index<Flist.Count) then
   begin
   TWDXModule(Flist.Objects[Index]).Free;
   Flist.Delete(Index);
   end;
end;

function TWDXModuleList.Add(Item: TWDXModule): integer;
begin
  Result:=Flist.AddObject(UpCase(item.Name),Item);
end;

function TWDXModuleList.Add(FileName:string): integer;
var s:string;
begin
    s:=ExtractFileName(FileName);
    if pos('.',s)>0 then
      delete(s,pos('.',s),length(s));

      if upcase(ExtractFileExt(FileName))='.WDX' then
        Flist.AddObject(UpCase(s),TPluginWDX.Create)
      else {иначе проверка на скрипт}
      if upcase(ExtractFileExt(FileName))='.LUA' then
        Flist.AddObject(UpCase(s),TLuaWdx.Create);

    //Result:=Flist.AddObject(UpCase(s),TWDXModule.Create);
    
    TWDXModule(Flist.Objects[Result]).Name:=s;
    TWDXModule(Flist.Objects[Result]).FileName:=FileName;
    if TWDXModule(Flist.Objects[Result]).LoadModule then
    begin
      TWDXModule(Flist.Objects[Result]).DetectStr:=TWDXModule(Flist.Objects[Result]).CallContentGetDetectString;
      TWDXModule(Flist.Objects[Result]).UnloadModule;
    end;

end;

function TWDXModuleList.Add(AName, FileName, DetectStr: string): integer;
begin
      if upcase(ExtractFileExt(FileName))='.WDX' then
        Result:=Flist.AddObject(UpCase(AName),TPluginWDX.Create)
      else {иначе проверка на скрипт}
      if upcase(ExtractFileExt(FileName))='.LUA' then
        Result:=Flist.AddObject(UpCase(AName),TLuaWdx.Create);

//      Result:=Flist.AddObject(UpCase(AName),TWDXModule.Create);

      TWDXModule(Flist.Objects[Result]).Name:=AName;
      TWDXModule(Flist.Objects[Result]).DetectStr:=DetectStr;
      TWDXModule(Flist.Objects[Result]).FileName:=FileName;
end;

function TWDXModuleList.IsLoaded(AName: String): Boolean;
var x:integer;
begin
  x:=Flist.IndexOf(AName);
  if x=-1 then Result:=false
  else
    begin
      Result:=GetWdxModule(x).IsLoaded;
    end;
end;

function TWDXModuleList.IsLoaded(Index: integer): Boolean;
begin
   Result:=GetWdxModule(Index).IsLoaded;
end;

function TWDXModuleList.LoadModule(AName: String): Boolean;
var x:integer;
begin
  x:=Flist.IndexOf(UpCase(AName));
  if x=-1 then Result:=false
  else
    begin
      Result:=GetWdxModule(x).LoadModule;
    end;
end;

function TWDXModuleList.LoadModule(Index:integer): Boolean;
begin
   Result:=GetWdxModule(Index).LoadModule;
end;

function TWDXModuleList.GetWdxModule(Index: integer): TWDXModule;
begin
 if (Flist.Objects[Index] is TPluginWDX) then
   Result:=TPluginWDX(Flist.Objects[Index])
 else
  if (Flist.Objects[Index] is TLuaWdx) then
    Result:=TLuaWdx(Flist.Objects[Index]);
end;

function TWDXModuleList.GetWdxModule(AName: string): TWDXModule;
var tmp:integer;
begin
  tmp:=Flist.IndexOf(upcase(AName));
  if tmp>-1 then
  begin
     if (Flist.Objects[tmp] is TPluginWDX) then
       Result:=TPluginWDX(Flist.Objects[tmp])
     else
      if (Flist.Objects[tmp] is TLuaWdx) then
        Result:=TLuaWdx(Flist.Objects[tmp]);
  end;

end;

{ TPluginWDX }

function TPluginWDX.IsLoaded: boolean;
begin
  Result:=FModuleHandle<>0;
end;

function TPluginWDX.FieldList: TStringlist;
begin
  Result:=FFieldsList;
end;

function TPluginWDX.GetADetectStr: string;
begin
  Result:=FDetectStr;
end;

function TPluginWDX.GetAName: string;
begin
  Result:=FName;
end;

function TPluginWDX.GetAFileName: string;
begin
  Result:=FFileName;
end;

procedure TPluginWDX.SetADetectStr(const AValue: string);
begin
FDetectStr:=AValue;
end;

procedure TPluginWDX.SetAName(AValue: string);
begin
  FName:=AValue;
end;

procedure TPluginWDX.SetAFileName(AValue: string);
begin
  FFileName:=AValue;
end;

constructor TPluginWDX.Create;
begin
  FFieldsList:=TStringList.Create;
  FParser:=TParserControl.Create;
end;

destructor TPluginWDX.Destroy;
begin
  if assigned(FParser) then
    FParser.Free;

  if assigned(FFieldsList) then
  while FFieldsList.Count>0 do
    begin
      TWdxField(FFieldsList.Objects[0]).Free;
      FFieldsList.Delete(0);
    end;
  Self.UnloadModule;
  inherited Destroy;
end;

function TPluginWDX.LoadModule: Boolean;
begin
  FModuleHandle := mbLoadLibrary(Self.FileName);
  Result := (FModuleHandle <> 0);
  if  FModuleHandle = 0 then exit;
        { Mandatory }
        ContentGetSupportedField := TContentGetSupportedField (GetProcAddress(FModuleHandle,'ContentGetSupportedField'));
        ContentGetValue := TContentGetValue (GetProcAddress(FModuleHandle,'ContentGetValue'));
        { Optional (must NOT be implemented if unsupported!) }
        ContentGetDetectString := TContentGetDetectString (GetProcAddress(FModuleHandle,'ContentGetDetectString'));
        ContentSetDefaultParams := TContentSetDefaultParams (GetProcAddress(FModuleHandle,'ContentSetDefaultParams'));
        ContentStopGetValue := TContentStopGetValue (GetProcAddress(FModuleHandle,'ContentStopGetValue'));
        ContentGetDefaultSortOrder := TContentGetDefaultSortOrder (GetProcAddress(FModuleHandle,'ContentGetDefaultSortOrder'));
        ContentPluginUnloading := TContentPluginUnloading (GetProcAddress(FModuleHandle,'ContentPluginUnloading'));
        ContentGetSupportedFieldFlags := TContentGetSupportedFieldFlags (GetProcAddress(FModuleHandle,'ContentGetSupportedFieldFlags'));
        ContentSetValue := TContentSetValue (GetProcAddress(FModuleHandle,'ContentSetValue'));
        ContentEditValue := TContentEditValue (GetProcAddress(FModuleHandle,'ContentEditValue'));
        ContentSendStateInformation := TContentSendStateInformation (GetProcAddress(FModuleHandle,'ContentSendStateInformation'));
        { Unicode }
        ContentGetValueW := TContentGetValueW (GetProcAddress(FModuleHandle, 'ContentGetValueW'));
        ContentStopGetValueW := TContentStopGetValueW (GetProcAddress(FModuleHandle, 'ContentStopGetValueW'));
        ContentSetValueW := TContentSetValueW (GetProcAddress(FModuleHandle, 'ContentSetValueW'));
        ContentSendStateInformationW := TContentSendStateInformationW (GetProcAddress(FModuleHandle, 'ContentSendStateInformationW'));

    CallContentSetDefaultParams;
    CallContentGetSupportedField;
    if Length(Self.DetectStr)=0 then
      Self.DetectStr:=CallContentGetDetectString;
end;


procedure TPluginWDX.CallContentSetDefaultParams;
var dps:pContentDefaultParamStruct;
begin
   if assigned(ContentSetDefaultParams) then
   begin
       GetMem(dps,SizeOf(tContentDefaultParamStruct));
       dps.DefaultIniName:=gpCfgDir + WdxIniFileName;
       dps.PluginInterfaceVersionHi:=1;
       dps.PluginInterfaceVersionLow:=50;
       dps.size:=SizeOf(tContentDefaultParamStruct);
       ContentSetDefaultParams(dps);
       FreeMem(dps,SizeOf(tContentDefaultParamStruct));
   end;
end;

procedure TPluginWDX.CallContentStopGetValue(FileName: string);
begin
 if Assigned(ContentStopGetValueW) then
  ContentStopGetValueW(PWideChar(UTF8Decode(FileName)))
 else if Assigned(ContentStopGetValue) then
  ContentStopGetValue(PAnsiChar(UTF8ToSys(FileName)));
end;

function TPluginWDX.CallContentGetDefaultSortOrder(FieldIndex: integer
  ): boolean;
var x:integer;
begin
  if Assigned(ContentGetDefaultSortOrder) then
    begin
      x:=ContentGetDefaultSortOrder(FieldIndex);
      case x of
       1: Result:= false; //a..z 1..9
      -1: Result:= true;  //z..a 9..1
      end;
    end;

end;

procedure TPluginWDX.UnloadModule;
begin
  if assigned(ContentPluginUnloading) then ContentPluginUnloading;

  if FModuleHandle <> 0 then
    FreeLibrary(FModuleHandle);
  FModuleHandle := 0;

        { Mandatory }
        ContentGetSupportedField := nil;
        ContentGetValue := nil;
        { Optional (must NOT be implemented if unsupported!) }
        ContentGetDetectString := nil;
        ContentSetDefaultParams := nil;
        ContentStopGetValue := nil;
        ContentGetDefaultSortOrder := nil;
        ContentPluginUnloading := nil;
        ContentGetSupportedFieldFlags := nil;
        ContentSetValue := nil;
        ContentEditValue := nil;
        ContentSendStateInformation := nil;
        { Unicode }
        ContentGetValueW := nil;
        ContentStopGetValueW := nil;
        ContentSetValueW := nil;
        ContentSendStateInformationW := nil;
end;

procedure TPluginWDX.CallContentGetSupportedField;
var Index,
    maxlen,
    tmp,
    Rez:integer;
    xFieldName:PChar;
    xUnits:PChar;
    s:string;
begin
 if not assigned(ContentGetSupportedField) then exit;
 Index:=0;
 GetMem(xFieldName,MAX_PATH);
 GetMem(xUnits,MAX_PATH);
 maxlen:=MAX_PATH;
 repeat
   Rez:= ContentGetSupportedField(Index, xFieldName, xUnits , maxlen);
   if Rez<>ft_nomorefields then
     begin
       s:=xFieldName;
       tmp:=FFieldsList.AddObject(s,TWdxField.Create);
       TWdxField(FFieldsList.Objects[tmp]).FName:=xFieldName;
       TWdxField(FFieldsList.Objects[tmp]).FUnits:=xUnits;
       TWdxField(FFieldsList.Objects[tmp]).FType:=Rez;
     end;
   inc(Index);

 until Rez=ft_nomorefields;
 FreeMem(xFieldName);
 FreeMem(xUnits);
end;

function TPluginWDX.CallContentGetDetectString: string;
var
  pacDetectString: PAnsiChar;
begin
  if Assigned(ContentGetDetectString) then
   begin
     GetMem(pacDetectString, MAX_PATH);
     FillChar(pacDetectString^, MAX_PATH, #0);
     ContentGetDetectString(pacDetectString, MAX_PATH);
     Result:= StrPas(pacDetectString);
     FreeMem(pacDetectString);
   end
  else
    Result:='';
end;

function TPluginWDX.CallContentGetValue(FileName: string; FieldName: String;
  UnitName: String; flags: integer): string;
var
  FieldIndex,
  UnitIndex : Integer;
begin
  FieldIndex:= GetFieldIndex(FieldName);
  UnitIndex:= TWdxField(FieldList.Objects[FieldIndex]).GetUnitIndex(UnitName);
  result:=CallContentGetValue(FileName, FieldIndex, UnitIndex, flags);
end;

function TPluginWDX.CallContentGetValue(FileName: string; FieldIndex, UnitIndex: integer; flags: integer): string;
var Rez:integer;
    Buf: array[0..2*1024] of char;
    fnval: integer absolute buf;
    fnval64: Int64 absolute buf;
    ffval: Double absolute buf;
    fdate: TDateFormat absolute buf;
    ftime: TTimeFormat absolute buf;
    {$IF DEFINED(MSWINDOWS)}
    xtime: Windows.FILETIME absolute buf;
    {$ELSEIF DEFINED(UNIX)}
    dtime: TDateTime absolute buf;
    {$ENDIF}
    stime: TSystemTime;
begin
  if Assigned(ContentGetValueW) then
    Rez:= ContentGetValueW(PWideChar(UTF8Decode(FileName)),FieldIndex,UnitIndex,@Buf,SizeOf(buf),flags)
  else if Assigned(ContentGetValue) then
    Rez:= ContentGetValue(PAnsiChar(UTF8ToSys(FileName)),FieldIndex,UnitIndex,@Buf,SizeOf(buf),flags);

  case Rez of
    ft_fieldempty:       Result:='';
    ft_numeric_32:       Result:= IntToStr(fnval);
    ft_numeric_64:       Result:= IntToStr(fnval64);
    ft_numeric_floating: Result:= FloatToStr(ffval);
    ft_date:             Result:= Format('%2.2d.%2.2d.%4.4d', [fdate.wDay, fdate.wMonth, fdate.wYear]);
    ft_time:             Result:= Format('%2.2d:%2.2d:%2.2d', [ftime.wHour, ftime.wMinute, ftime.wSecond]);
    ft_datetime:         begin
                           {$IF DEFINED(MSWINDOWS)}
                            Windows.FileTimeToSystemTime(xtime, stime);
                            Result:= Format('%2.2d.%2.2d.%4.4d %2.2d:%2.2d:%2.2d',
                                    [stime.wDay, stime.wMonth, stime.wYear,
                                     stime.wHour, stime.wMinute, stime.wSecond]);
                           {$ELSEIF DEFINED(UNIX)}
                            DateTimeToSystemTime(dtime, stime);
                            Result:= Format('%2.2d.%2.2d.%4.4d %2.2d:%2.2d:%2.2d',
                                    [stime.Day, stime.Month, stime.Year,
                                     stime.Hour, stime.Minute, stime.Second]);
                           {$ENDIF}
                         end;

    ft_boolean:          if fnval=0 then Result:= 'FALSE' else Result:='TRUE';

    ft_multiplechoice,
    ft_string,
    ft_fulltext:         Result:= SysToUTF8(StrPas(Buf));
    ft_stringw:          Result:= UTF8Encode(WideString(Buf));
 //TODO: FT_DELAYED,ft_ondemand
  else Result:='';
  end;

end;

function TPluginWDX.CallContentGetSupportedFieldFlags(FieldIndex: integer
  ): integer;
begin
  if assigned(ContentGetSupportedFieldFlags) then
  Result:=ContentGetSupportedFieldFlags(FieldIndex);
end;


function TPluginWDX.GetFieldIndex(FieldName: string): integer;
begin
  Result:= FFieldsList.IndexOf(FieldName);
end;


function TPluginWDX.FileParamVSDetectStr(const aFile: TFile): boolean;
begin
  FParser.DetectStr:= Self.DetectStr;
  Result:= FParser.TestFileResult(aFile);
end;


{ TLuaWdx }

function TLuaWdx.GetAName: string;
begin
Result:=FName;
end;

function TLuaWdx.GetAFileName: string;
begin
  Result:=FFileName;
end;

function TLuaWdx.GetADetectStr: string;
begin
  Result:=FDetectStr;
end;

procedure TLuaWdx.SetAName(AValue: string);
begin
  FName:=AValue;
end;

procedure TLuaWdx.SetAFileName(AValue: string);
begin
 FFileName:= AValue;
end;

procedure TLuaWdx.SetADetectStr(const AValue: string);
begin
 FDetectStr:= AValue;
end;

function TLuaWdx.DoScript(AName: string): integer;
begin
  result:=LUA_ERRRUN;
  if not assigned(L) then exit;
  Result:=luaL_dofile(L,pchar(AName));
end;

constructor TLuaWdx.Create;
begin
 if not IsLuaLibLoaded then
  LoadLuaLib(gLuaLib); //Todo вынести загрузку либы в VmClass
  FFieldsList:=TStringList.Create;
  FParser:=TParserControl.Create;
end;

destructor TLuaWdx.Destroy;
begin
  if assigned(FParser) then
    FParser.Free;

  if assigned(FFieldsList) then
  while FFieldsList.Count>0 do
    begin
      TWdxField(FFieldsList.Objects[0]).Free;
      FFieldsList.Delete(0);
    end;

  Self.UnloadModule;

  //UnloadLuaLib;           //Todo вынести выгрузку либы в VmClass

  inherited Destroy;
end;

function TLuaWdx.LoadModule: Boolean;
begin
  result:=false;
  if not IsLuaLibLoaded then exit;

  L:=lua_open;
  if not Assigned(L) then exit;

  luaL_openlibs(L);

  if DoScript(Self.FFileName)=0 then
    result:=true
  else
    Result:=false;
    
    CallContentSetDefaultParams;
    CallContentGetSupportedField;
    if Length(Self.DetectStr)=0 then
      Self.DetectStr:=CallContentGetDetectString;
end;

procedure TLuaWdx.UnloadModule;
begin
WdxLuaContentPluginUnloading;

  if Assigned(L) then
  begin
    lua_close(L);
    L:=nil;
  end;
end;

function TLuaWdx.IsLoaded: boolean;
begin
  Result:=IsLuaLibLoaded and Assigned(Self.L);
end;

function TLuaWdx.FieldList: TStringlist;
begin
  Result:=FFieldsList;
end;

function TLuaWdx.GetFieldIndex(FieldName: string): integer;
begin
   Result:= FFieldsList.IndexOf(FieldName);
end;

function TLuaWdx.FileParamVSDetectStr(const aFile: TFile): boolean;
begin
  FParser.DetectStr:= Self.DetectStr;
  Result:= FParser.TestFileResult(aFile);
end;

function TLuaWdx.WdxLuaContentGetSupportedField(Index:integer; var xFieldName, xUnits:string): integer;
begin
  result:=ft_nomorefields;
  if not assigned(L) then exit;
  lua_getglobal(L,'ContentGetSupportedField');
  if not lua_isfunction(L,-1) then exit;
  lua_pushinteger(L,Index);
  lua_call(L,1,3);
  xFieldName:=lua_tostring(L,-3);
  xUnits:=lua_tostring(L,-2);
  Result:=lua_tointeger(L,-1);
  lua_pop(L,3);
end;

procedure TLuaWdx.WdxLuaContentPluginUnloading;
begin
if not assigned(L) then exit;
  lua_getglobal(L,'ContentPluginUnloading');
  if not lua_isfunction(L,-1) then exit;
  lua_call(L,0,0);
end;

procedure TLuaWdx.CallContentGetSupportedField;
var  Index,Rez,tmp:integer;
     xFieldName, xUnits:string;
begin
 Index:=0;
 repeat
   Rez:= WdxLuaContentGetSupportedField(Index, xFieldName, xUnits);
   DebugLn('WDX:CallGetSupFields:'+Inttostr(Rez));
   if Rez<>ft_nomorefields then
     begin
       tmp:=FFieldsList.AddObject(xFieldName,TWdxField.Create);
       TWdxField(FFieldsList.Objects[tmp]).FName:=xFieldName;
       TWdxField(FFieldsList.Objects[tmp]).FUnits:=xUnits;
       TWdxField(FFieldsList.Objects[tmp]).FType:=Rez;
     end;
   inc(Index);

 until Rez=ft_nomorefields;
end;

procedure TLuaWdx.CallContentSetDefaultParams;
begin
  if not assigned(L) then exit;
  lua_getglobal(L,'ContentSetDefaultParams');
  if not lua_isfunction(L,-1) then exit;
  lua_pushstring(L,pchar(gpCfgDir + WdxIniFileName));
  lua_pushinteger(L,1);
  lua_pushinteger(L,50);
  lua_call(L,3,0);
end;

procedure TLuaWdx.CallContentStopGetValue(FileName: string);
begin
  if not assigned(L) then exit;
  lua_getglobal(L,'ContentStopGetValue');
  if not lua_isfunction(L,-1) then exit;
  lua_pushstring(L,pchar(FileName));
  lua_call(L,1,0);
end;

function TLuaWdx.CallContentGetDefaultSortOrder(FieldIndex: integer): boolean;
var x:integer;
begin
 result:=false;
 if not assigned(L) then exit;

   lua_getglobal(L,'ContentGetDefaultSortOrder');
  if not lua_isfunction(L,-1) then exit;
  lua_pushinteger(L,FieldIndex);
  lua_call(L,1,1);

      x:=lua_tointeger(L,-1);
      case x of
       1: Result:= false; //a..z 1..9
      -1: Result:= true;  //z..a 9..1
      end;
lua_pop(L,1);
end;

function TLuaWdx.CallContentGetDetectString: string;
begin
 result:='';
 if not assigned(L) then exit;

  lua_getglobal(L,'ContentGetDetectString');
  if not lua_isfunction(L,-1) then exit;
  lua_call(L,0,1);
  Result:=lua_tostring(L,-1);
  lua_pop(L,1);
end;

function TLuaWdx.CallContentGetValue(FileName: string; FieldName: String;
  UnitName: String; flags: integer): string;
var
  FieldIndex,
  UnitIndex : Integer;
begin
  FieldIndex:= GetFieldIndex(FieldName);
  UnitIndex:= TWdxField(FieldList.Objects[FieldIndex]).GetUnitIndex(UnitName);
  result:=CallContentGetValue(FileName, FieldIndex, UnitIndex, flags);
end;

function TLuaWdx.CallContentGetValue(FileName: string; FieldIndex,
  UnitIndex: integer; flags: integer): string;
begin
 result:='';
 if not assigned(L) then exit;

  lua_getglobal(L,'ContentGetValue');
  if not lua_isfunction(L,-1) then exit;
  lua_pushstring(L,pchar(FileName));
  lua_pushinteger(L,FieldIndex);
  lua_pushinteger(L,UnitIndex);
  lua_pushinteger(L,flags);

  lua_call(L,4,1);
  Result:=lua_tostring(L,-1);
  lua_pop(L,1);

end;

function TLuaWdx.CallContentGetSupportedFieldFlags(FieldIndex: integer
  ): integer;
begin
 result:=0;
 if not assigned(L) then exit;

  lua_getglobal(L,'ContentGetSupportedFieldFlags');
  if not lua_isfunction(L,-1) then exit;
  lua_pushinteger(L,FieldIndex);

  lua_call(L,1,1);
  Result:=lua_tointeger(L,-1);
  lua_pop(L,1);

end;


{ TWDXModule }

function TWDXModule.WdxFieldType(n: integer): string;
 begin
   case n of
     FT_NUMERIC_32:              Result:= 'FT_NUMERIC_32';
     FT_NUMERIC_64:              Result:= 'FT_NUMERIC_64';
     FT_NUMERIC_FLOATING:        Result:= 'FT_NUMERIC_FLOATING';
     FT_DATE:                    Result:= 'FT_DATE';
     FT_TIME:                    Result:= 'FT_TIME';
     FT_DATETIME:                Result:= 'FT_DATETIME';
     FT_BOOLEAN:                 Result:= 'FT_BOOLEAN';
     FT_MULTIPLECHOICE:          Result:= 'FT_MULTIPLECHOICE';
     FT_STRING:                  Result:= 'FT_STRING';
     FT_FULLTEXT:                Result:= 'FT_FULLTEXT';
     FT_NOSUCHFIELD:             Result:= 'FT_NOSUCHFIELD';
     FT_FILEERROR:               Result:= 'FT_FILEERROR';
     FT_FIELDEMPTY:              Result:= 'FT_FIELDEMPTY';
     FT_DELAYED:                 Result:= 'FT_DELAYED';
     else Result:= '?';
   end;
end;

{ TWdxField }

function TWdxField.GetUnitIndex(UnitName: String): Integer;
var
  sUnits: String;
begin
 Result:= -1;
 sUnits:= FUnits;
 while sUnits <> EmptyStr do
 begin
   Inc(Result);
   if SameText(UnitName, Copy2SymbDel(sUnits, '|')) then
     Exit;
 end;
 Result:= 0;
end;

end.

