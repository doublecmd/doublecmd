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
    Classes, SysUtils,inifiles,uwdxprototypes,ucontplugin, dynlibs, uDCUtils, uOSUtils;
  
type

      { TWdxField }

      TWdxField = class
       FName:string;
       FUnits:string;
       FType:integer;
      end;

      { TWDXModule }

      TWDXModule = class
      private
        FFieldsList:TStringList;
        FModuleHandle:TLibHandle;  // Handle to .DLL or .so
        function GIsLoaded:boolean;
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
      public
        //---------------------
        Name:string;
        FileName:string;
        DetectStr:string;
        //---------------------
        constructor Create;
        destructor Destroy; override;
        //---------------------
        function LoadModule:Boolean;
        procedure UnloadModule;
        //---------------------
        function WdxFieldType(n: integer): string;
        function GetFieldIndex(FieldName:string):integer;
        //TODO: Detect string parser and useful functions
        function FileParamVSDetectStr(FileName:string):boolean;
        //------------------------------------------------------
        procedure CallContentGetSupportedField;
        procedure CallContentSetDefaultParams;
        procedure CallContentStopGetValue(FileName:string);
        //---------------------
        function CallContentGetDefaultSortOrder(FieldIndex:integer):boolean;
        function CallContentGetDetectString:string;
        function CallContentGetValue(FileName: string; FieldName:String; UnitIndex: integer; flags: integer):string; overload;
        function CallContentGetValue(FileName: string; FieldIndex, UnitIndex: integer; flags: integer):string;overload;
        function CallContentGetSupportedFieldFlags(FieldIndex:integer):integer;
        {ContentSetValue
         ContentEditValue
         ContentSendStateInformation}
        //------------------------------------------------------
        property ModuleHandle:TLibHandle read FModuleHandle write FModuleHandle;
        property FieldList:TStringlist read FFieldsList;
        property IsLoaded:boolean read GIsLoaded;
        //---------------------
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
        procedure Clear;
        procedure Load(FileName:string);overload;
        procedure Load(Ini:TiniFile); overload;
        procedure Save(FileName:string);overload;
        procedure Save(Ini:TIniFile); overload;
        procedure DeleteItem(Index: integer);
        //---------------------
        function Add(Item:TWDXModule):integer;overload;
        function Add(FileName:string):integer;overload;
        function Add(Name,FileName,DetectStr:string):integer;overload;

        function IsLoaded(Name:String):Boolean;overload;
        function IsLoaded(Index: integer):Boolean;overload;
        function LoadModule(Name:String):Boolean; overload;
        function LoadModule(Index: integer): Boolean; overload;
        
        function GetWdxModule(Index:integer):TWDXModule;overload;
        function GetWdxModule(Name:string):TWDXModule;overload;
        //---------------------
        //property WdxList:TStringList read Flist;
        property Count:integer read GetCount;
      end;

implementation

uses uGlobs;
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

procedure TWDXModuleList.Clear;
begin
  while Flist.Count>0 do
   begin
     TWDXModule(Flist.Objects[0]).Free;
     Flist.Delete(0);
   end;
end;

procedure TWDXModuleList.Load(FileName: string);
var Ini:TIniFile;
begin
  try
    Ini:=TIniFile.Create(FileName);
    Load(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TWDXModuleList.Load(Ini: TiniFile);
var Count,I:integer;
    tmp:string;
begin
  Self.Clear;
  Count:=Ini.ReadInteger('Content Plugins','PluginCount',0);
  if Count=0 then Exit;
  
  For i:=0 to Count-1 do
    begin
      tmp:=Ini.ReadString('Content Plugins','plugin'+IntToStr(I+1)+'Name','');
      Flist.AddObject(UpCase(tmp),TWDXModule.Create);
      TWDXModule(Flist.Objects[I]).Name:=tmp;
      TWDXModule(Flist.Objects[I]).DetectStr:=Ini.ReadString('Content Plugins','plugin'+IntToStr(I+1)+'Detect','');
      TWDXModule(Flist.Objects[I]).FileName:=GetCmdDirFromEnvVar(Ini.ReadString('Content Plugins','plugin'+IntToStr(I+1)+'Path',''));
    end;

end;

procedure TWDXModuleList.Save(FileName: string);
 var  Ini:TIniFile;
begin
  try
    Ini:=TIniFile.Create(FileName);
     Save(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TWDXModuleList.Save(Ini: TIniFile);
var i:integer;
begin
 Ini.EraseSection('Content Plugins');
 Ini.WriteInteger('Content Plugins','PluginCount',Flist.Count);
  For i:=0 to Flist.Count-1 do
    begin
      Ini.WriteString('Content Plugins','plugin'+IntToStr(I+1)+'Name',TWDXModule(Flist.Objects[I]).Name);
      Ini.WriteString('Content Plugins','plugin'+IntToStr(I+1)+'Detect',TWDXModule(Flist.Objects[I]).DetectStr);
      Ini.WriteString('Content Plugins','plugin'+IntToStr(I+1)+'Path',SetCmdDirAsEnvVar(TWDXModule(Flist.Objects[I]).FileName));
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
    Result:=Flist.AddObject(UpCase(s),TWDXModule.Create);
    TWDXModule(Flist.Objects[Result]).Name:=s;
    TWDXModule(Flist.Objects[Result]).FileName:=FileName;
    if TWDXModule(Flist.Objects[Result]).LoadModule then
    begin
      TWDXModule(Flist.Objects[Result]).DetectStr:=TWDXModule(Flist.Objects[Result]).CallContentGetDetectString;
      TWDXModule(Flist.Objects[Result]).UnloadModule;
    end;

end;

function TWDXModuleList.Add(Name, FileName, DetectStr: string): integer;
begin
      Result:=Flist.AddObject(UpCase(Name),TWDXModule.Create);
      TWDXModule(Flist.Objects[Result]).Name:=Name;
      TWDXModule(Flist.Objects[Result]).DetectStr:=DetectStr;
      TWDXModule(Flist.Objects[Result]).FileName:=FileName;
end;

function TWDXModuleList.IsLoaded(Name: String): Boolean;
var x:integer;
begin
  x:=Flist.IndexOf(Name);
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

function TWDXModuleList.LoadModule(Name: String): Boolean;
var x:integer;
begin
  x:=Flist.IndexOf(Name);
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
  Result:=TWDXModule(Flist.Objects[Index]);
end;

function TWDXModuleList.GetWdxModule(Name: string): TWDXModule;
var tmp:integer;
begin
  tmp:=Flist.IndexOf(Name);
  if tmp>-1 then
  Result:=TWDXModule(Flist.Objects[tmp]);
end;

{ TWDXModule }

function TWDXModule.GIsLoaded: boolean;
begin
  Result:=FModuleHandle<>0;
end;

constructor TWDXModule.Create;
begin
  FFieldsList:=TStringList.Create;
end;

destructor TWDXModule.Destroy;
begin
  if assigned(FFieldsList) then
  while FFieldsList.Count>0 do
    begin
      TWdxField(FFieldsList.Objects[0]).Free;
      FFieldsList.Delete(0);
    end;
  Self.UnloadModule;
  inherited Destroy;
end;

function TWDXModule.LoadModule: Boolean;
begin
  FModuleHandle := LoadLibrary(Self.FileName);
  Result := (FModuleHandle <> 0);
  if  FModuleHandle = 0 then exit;
        {Mandatory}
        ContentGetSupportedField := TContentGetSupportedField (GetProcAddress(FModuleHandle,'ContentGetSupportedField'));
        ContentGetValue := TContentGetValue (GetProcAddress(FModuleHandle,'ContentGetValue'));
        {Optional (must NOT be implemented if unsupported!)}
        ContentGetDetectString := TContentGetDetectString (GetProcAddress(FModuleHandle,'ContentGetDetectString'));
        ContentSetDefaultParams := TContentSetDefaultParams (GetProcAddress(FModuleHandle,'ContentSetDefaultParams'));
        ContentStopGetValue := TContentStopGetValue (GetProcAddress(FModuleHandle,'ContentStopGetValue'));
        ContentGetDefaultSortOrder := TContentGetDefaultSortOrder (GetProcAddress(FModuleHandle,'ContentGetDefaultSortOrder'));
        ContentPluginUnloading := TContentPluginUnloading (GetProcAddress(FModuleHandle,'ContentPluginUnloading'));
        ContentGetSupportedFieldFlags := TContentGetSupportedFieldFlags (GetProcAddress(FModuleHandle,'ContentGetSupportedFieldFlags'));
        ContentSetValue := TContentSetValue (GetProcAddress(FModuleHandle,'ContentSetValue'));
        ContentEditValue := TContentEditValue (GetProcAddress(FModuleHandle,'ContentEditValue'));
        ContentSendStateInformation := TContentSendStateInformation (GetProcAddress(FModuleHandle,'ContentSendStateInformation'));

    CallContentSetDefaultParams;
    CallContentGetSupportedField;
end;


procedure TWDXModule.CallContentSetDefaultParams;
var dps:pContentDefaultParamStruct;
begin
   if assigned(ContentSetDefaultParams) then
   begin
       GetMem(dps,SizeOf(tContentDefaultParamStruct));
       dps.DefaultIniName:=gini.FileName;
       dps.PluginInterfaceVersionHi:=1;
       dps.PluginInterfaceVersionLow:=50;
       dps.size:=SizeOf(tContentDefaultParamStruct);
       ContentSetDefaultParams(dps);
       FreeMem(dps,SizeOf(tContentDefaultParamStruct));
   end;
end;

procedure TWDXModule.CallContentStopGetValue(FileName: string);
begin
 if assigned(ContentStopGetValue) then
  ContentStopGetValue(PChar(FileName));
end;

function TWDXModule.CallContentGetDefaultSortOrder(FieldIndex: integer
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

procedure TWDXModule.UnloadModule;
begin
  if assigned(ContentPluginUnloading) then ContentPluginUnloading;

  if FModuleHandle <> 0 then
    FreeLibrary(FModuleHandle);
  FModuleHandle := 0;

        {Mandatory}
        ContentGetSupportedField := nil;
        ContentGetValue := nil;
        //b) Optional (must NOT be implemented if unsupported!)
        ContentGetDetectString := nil;
        ContentSetDefaultParams := nil;
        ContentStopGetValue := nil;
        ContentGetDefaultSortOrder := nil;
        ContentPluginUnloading := nil;
        ContentGetSupportedFieldFlags := nil;
        ContentSetValue := nil;
        ContentEditValue := nil;
        ContentSendStateInformation := nil;
end;

procedure TWDXModule.CallContentGetSupportedField;
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

function TWDXModule.CallContentGetDetectString: string;
begin
  if assigned(ContentGetDetectString) then
    ContentGetDetectString(PChar(Result),MAX_PATH)
  else
    Result:='';
end;

function TWDXModule.CallContentGetValue(FileName: string; FieldName: String;
  UnitIndex: integer; flags: integer): string;
begin
result:=CallContentGetValue(FileName, GetFieldIndex(FieldName), UnitIndex,flags);
end;

function TWDXModule.CallContentGetValue(FileName: string; FieldIndex, UnitIndex: integer; flags: integer): string;
var Rez:integer;
    Buf: array[0..2*1024] of char;
    fnval: integer absolute buf;
    fnval64: Int64 absolute buf;
    ffval: Double absolute buf;
    fdate: TDateFormat absolute buf;
    ftime: TTimeFormat absolute buf;
    xtime: TFileTime absolute buf;
    stime: TSystemTime;
    dtime: TDateTime absolute buf;
begin

  Rez:=ContentGetValue(PChar(FileName),FieldIndex,UnitIndex,@Buf,SizeOf(buf),flags);
  case Rez of
    ft_fieldempty:       Result:='';
    ft_numeric_32:       Result:= IntToStr(fnval);
    ft_numeric_64:       Result:= IntToStr(fnval64);
    ft_numeric_floating: Result:= FloatToStr(ffval);
    ft_date:             Result:= Format('%2.2d.%2.2d.%4.4d', [fdate.wDay, fdate.wMonth, fdate.wYear]);
    ft_time:             Result:= Format('%2.2d:%2.2d:%2.2d', [ftime.wHour, ftime.wMinute, ftime.wSecond]);
    ft_datetime:         begin
                           {$IFDEF MSWINDOWS}
                                    FileTimeToSystemTime(xtime, stime);
                            Result:= Format('%2.2d.%2.2d.%4.4d %2.2d:%2.2d:%2.2d',
                                    [stime.wDay, stime.wMonth, stime.wYear,
                                     stime.wHour, stime.wMinute, stime.wSecond]);
                           {$ENDIF}
                           {$IFDEF unix}
                           DateTimeToSystemTime(dtime, stime);
                            Result:= Format('%2.2d.%2.2d.%4.4d %2.2d:%2.2d:%2.2d',
                                    [stime.Day, stime.Month, stime.Year,
                                     stime.Hour, stime.Minute, stime.Second]);
                           {$ENDIF}
                         end;

    ft_boolean:          if fnval=0 then Result:= 'FALSE' else Result:='TRUE';

    ft_multiplechoice,
    ft_string,
    ft_fulltext:         Result:= StrPas(Buf);
 //TODO: FT_DELAYED,ft_ondemand
  else Result:='';
  end;

end;

function TWDXModule.CallContentGetSupportedFieldFlags(FieldIndex: integer
  ): integer;
begin
  if assigned(ContentGetSupportedFieldFlags) then
  Result:=ContentGetSupportedFieldFlags(FieldIndex);
end;

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

function TWDXModule.GetFieldIndex(FieldName: string): integer;
begin
  Result:= FFieldsList.IndexOf(FieldName);
end;


function TWDXModule.FileParamVSDetectStr(FileName: string): boolean;
begin

end;



end.

