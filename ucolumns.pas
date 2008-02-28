{
   Double Commander
   -------------------------------------------------------------------------
   Filepanel columns implementation unit

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


unit uColumns;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles, LCLProc, strutils, uTypes, uGlobs, uOSUtils, uDCUtils;

  type

 { TPanelColumnsType }
  TPanelColumn=class
  private
    function ActGetInfo(FuncS: string; ptr: PFileRecItem): string;
  //------------------------------------------------------
  public
    Title:string;
    FuncString:string;
    FuncList:TStringList;
    //String is function or simpletext;
    //TObject(integer)=indicator of function: 0 is simpletext; 1 is function;
    Width:integer;
    Align : TAlignment;
    //---------------------

    constructor Create;
    destructor Destroy; override;
    //---------------------
    function GetModFunctionName(str: string): string;
    function GetModFunctionParams(str: string): string;
    function GetModName(str: string): string;
    function GetModType(str: string): string;
    //---------------------
    function GetColumnResultString(ptr:PFileRecItem):string;
  //------------------------------------------------------
  end;

  { TPanelColumnsClass }

  TPanelColumnsClass =class
  //------------------------------------------------------
  private
   FList:TList;
   FCurrentColumnsFile:string;
  //------------------------------------------------------
  public
    constructor Create;
    destructor Destroy; override;
    //---------------------
    function GetColumnTitle(Index:integer):string;
    function GetColumnFuncString(Index:integer):string;
    function GetColumnWidth(Index:integer):Integer;
    function GetColumnAlign(Index:integer):TAlignment;

    function GetColumnItem(Index:integer):TPanelColumn;
    function GetCount:Integer;
    function Add(Item:TPanelColumn):integer;
    function Add(Title, FuncString:string; Width:integer;Align: TAlignment=taLeftJustify):integer; overload;
    //---------------------
    procedure SetColumnTitle(Index:integer;Title:string);
    procedure SetColumnFuncString(Index:integer;FuncString:string);
    procedure SetColumnWidth(Index,Width:integer);
    procedure SetColumnAlign(Index:Integer; Align: TAlignment);

    procedure Delete(Index:Integer);
    procedure Clear;
    procedure AddDefaultColumns;
    procedure Load(FileName:String);
    procedure Load(Ini:TIniFile);overload;
    procedure Save;
    procedure Save(FileName:string); overload;
    procedure Save(Ini:TIniFile); overload;
    //---------------------
    property ColumnsCount:Integer read GetCount;
    property CurrentColumnsFile:string read FCurrentColumnsFile;
  //------------------------------------------------------
  end;
  procedure FillListFromString(List: TStrings; FuncString: string);

  var IntList:TStringList;

implementation
uses uLng;

{ TPanelColumnsType }

function TPanelColumnsClass.GetColumnTitle(Index: integer): string;
begin
  if Index>Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Title;
end;

function TPanelColumnsClass.GetColumnFuncString(Index: integer): string;
begin
  if Index>Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).FuncString;
end;

function TPanelColumnsClass.GetColumnWidth(Index: integer): Integer;
begin
  if Index>Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Width;
end;

function TPanelColumnsClass.GetColumnAlign(Index: integer): TAlignment;
begin
  if Index>Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Align;
end;

function TPanelColumnsClass.GetColumnItem(Index: integer): TPanelColumn;
begin
  if Index>Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]);
end;

constructor TPanelColumnsClass.Create;
begin
FList:=TList.Create;
end;

procedure TPanelColumnsClass.Clear;
begin
    while Flist.Count>0 do
    begin
      TPanelColumn(Flist[0]).Free;
      FList.Delete(0);
    end;
    FCurrentColumnsFile:='';
end;

destructor TPanelColumnsClass.Destroy;
begin
  Self.Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;


function TPanelColumnsClass.GetCount: Integer;
begin
  Result:=FList.Count;
end;

function TPanelColumnsClass.Add(Item: TPanelColumn): integer;
begin
  Result:=FList.Add(Item);
end;

function TPanelColumnsClass.Add(Title, FuncString: string; Width:integer;Align: TAlignment): integer;
begin
 Result:=Flist.Add(TPanelColumn.Create);
 TPanelColumn(FList[Result]).Title:=Title;
 TPanelColumn(FList[Result]).FuncString:=FuncString;
 FillListFromString(TPanelColumn(FList[Result]).FuncList,FuncString);
 TPanelColumn(FList[Result]).Width:=Width;
 TPanelColumn(FList[Result]).Align:=Align;
end;

procedure TPanelColumnsClass.SetColumnTitle(Index: integer; Title: string);
begin
  if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).Title:=Title;
end;

procedure TPanelColumnsClass.SetColumnFuncString(Index: integer;
  FuncString: string);
begin
  if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).FuncString:=FuncString;
end;

procedure TPanelColumnsClass.SetColumnWidth(Index, Width: integer);
begin
  if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).Width:=Width;
end;

procedure TPanelColumnsClass.SetColumnAlign(Index: Integer; Align: TAlignment);
begin
  if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).Align:=Align;
end;


 //------------------------------------------------------
 procedure FillListFromString(List:TStrings; FuncString:string);
   var s,st:string; i:integer;
   begin

     s:=FuncString;
     if length(s)=0 then Exit;

     while pos('[',s)>0 do
      begin
        st:=Copy(s,1,pos('[',s)-1);
        if st<>'' then  List.AddObject(st,TObject(0));
        
        delete(s,1,pos('[',s));
        st:=Copy(s,1,pos(']',s)-1);
        if st<>'' then  List.AddObject(st,TObject(1));
        delete(s,1,pos(']',s));
      end;
        st:=Copy(s,1,length(s));
        if st<>'' then  List.AddObject(st,TObject(0));

   end;
 //------------------------------------------------------

procedure TPanelColumnsClass.AddDefaultColumns;
begin
  // file name
  Add(rsColName, '[DC().GETFILENAMENOEXT{}]', 225, taLeftJustify);
  // file ext
  Add(rsColExt, '[DC().GETFILEEXT{}]', 50, taLeftJustify);
  // file size
  Add(rsColSize, '[DC().GETFILESIZE{}]', 70, taRightJustify);
  // file date/time
  Add(rsColDate, '[DC().GETFILETIME{}]', 70, taRightJustify);
  // file attributes
  Add(rsColAttr, '[DC().GETFILEATTR{}]', 200, taLeftJustify);
end;

procedure TPanelColumnsClass.Load(FileName:string);
var Ini:TIniFile;
begin
  try
    Ini:=TIniFile.Create(FileName);
    Load(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TPanelColumnsClass.Load(Ini: TIniFile);
var Count,I:Integer;
begin
    Self.Clear;
    FCurrentColumnsFile:=Ini.FileName;
    Count:=Ini.ReadInteger('Columns','ColumnCount',0);
    //---------------------
    if Count=0 then
      begin
        AddDefaultColumns;
        Exit;
      end;
    //---------------------
    For I:=0 to Count-1 do
      begin
        Flist.Add(TPanelColumn.Create);
        TPanelColumn(FList[I]).Title:=Ini.ReadString('Columns','Column'+IntToStr(I+1)+'Title','');
         //---------------------
          TPanelColumn(FList[I]).FuncString:=Ini.ReadString('Columns','Column'+IntToStr(I+1)+'FuncsString','');
          FillListFromString(TPanelColumn(FList[I]).FuncList,Ini.ReadString('Columns','Column'+IntToStr(I+1)+'FuncsString',''));
          TPanelColumn(FList[I]).Width:=Ini.ReadInteger('Columns','Column'+IntToStr(I+1)+'Width',20);
          TPanelColumn(FList[I]).Align:=TAlignment(Ini.ReadInteger('Columns','Column'+IntToStr(I+1)+'Align',0));
         //---------------------
      end;
    //---------------------
end;

procedure TPanelColumnsClass.Save;
begin
 Save(CurrentColumnsFile);
end;

procedure TPanelColumnsClass.Save(FileName: string);
 var  Ini:TIniFile;
begin
  try
    Ini:=TIniFile.Create(FileName);
     Save(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TPanelColumnsClass.Save(Ini: TIniFile);
 var I:Integer;
begin
    Ini.EraseSection('Columns');
    Ini.WriteInteger('Columns','ColumnCount',FList.Count);
    For I:=0 to FList.Count-1 do
      begin
        Ini.WriteString('Columns','Column'+IntToStr(I+1)+'Title',TPanelColumn(FList[I]).Title);
        Ini.WriteString('Columns','Column'+IntToStr(I+1)+'FuncsString',TPanelColumn(FList[I]).FuncString);
        Ini.WriteInteger('Columns','Column'+IntToStr(I+1)+'Width', TPanelColumn(FList[I]).Width);
        Ini.WriteInteger('Columns','Column'+IntToStr(I+1)+'Align', Integer(TPanelColumn(FList[I]).Align));
      end;
end;

procedure TPanelColumnsClass.Delete(Index: Integer);
begin
  if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).Free;
  FList.Delete(Index);
end;

{ TPanelColumn }

constructor TPanelColumn.Create;
begin
  FuncList:=TStringList.Create;
end;

destructor TPanelColumn.Destroy;
begin
  FreeAndNil(FuncList);
  inherited Destroy;
end;

//Return type (Script or DC or Plugin etc)
function TPanelColumn.GetModType(str:string):string;
begin
  if pos('(',Str)>0 then
    Result:=Copy(Str,1,pos('(',Str)-1)
  else Result:='';
end;

//Return name in (). (SriptName or PluginName etc)
function TPanelColumn.GetModName(str:string):string;
var s:string;
begin
  s:=str;
  if pos('(',S)>0 then
    delete(s,1,pos('(',S))
  else Exit;

  if pos(')',s)>0 then
    Result:=Copy(s,1,pos(')',s)-1);
end;

//Return function name (ScriptFunction,PluginFunction etc)
function TPanelColumn.GetModFunctionName(str:string):string;
var s:string;
begin
s:=str;
  if pos('.',S)>0 then
    delete(s,1,pos('.',S))
  else Exit;

  if pos('{',S)>0 then
    Result:=Copy(s,1,pos('{',S)-1);
end;

//Return function parameters (хз как буду работать с параметрами и нах они нужны, но пусть будет)
function TPanelColumn.GetModFunctionParams(str:string):string;
var s:string;
begin
s:=str;
   if pos('{',S)>0 then
    delete(s,1,pos('{',S))
  else Exit;
  if pos(s,'}')>0 then
    Result:=Copy(s,1,pos(s,'}')-1);
end;


//'ThisIsSimpleTest [DC(Panel).GetFileName] [DC(Panel).GetFileExt]
//[Script(ScriptName).ScriptFunction{param1,param2...}]'
//[Plugin(Pluginname).Function{param...}]

function TPanelColumn.ActGetInfo(FuncS:string; ptr: PFileRecItem):string;
 //---------------------
  const
     //---------------------
     DC ='DC';
     Script='SCRIPT';
     Plugin='PLUGIN';
     //---------------------
 //---------------------
 var AType,AName,AFunc,AParam:string;
begin
//       DebugLn('Entered ActGetInfo');
//       DebugLn('FuncS='+FuncS);
       //---------------------
       AType:=upcase(GetModType(FuncS));
       AName:=upcase(GetModName(FuncS));
       AFunc:=upcase(GetModFunctionName(FuncS));
       AParam:=upcase(GetModFunctionParams(FuncS));
       //---------------------
      // DebugLn('AType='+AType+#13+#10+'AName='+AName+#13+#10+'AFunc='+AFunc+#13+#10+'AParam='+AParam);
        //Internal doublecmd function
        //------------------------------------------------------
        if AType=DC then
          begin
            case IntList.IndexOf(AFunc) of
              0: Result:=ptr^.sName;
              1: Result:=ptr^.sExt;
              2: begin
                   with ptr^ do
                     // counted dir size
                     if (FPS_ISDIR(iMode)) and (iDirSize<>0) then
                       Result:=cnvFormatFileSize(iDirSize)
                     else
                      begin
                        if FPS_ISDIR(iMode) then
                          Result:= '<DIR>'
                        else
                          Result:=cnvFormatFileSize(iSize);
                      end;
                  // end;
                 end;
              3: Result:=ptr^.sModeStr;
              4: Result:=ptr^.sPath;
              5: Result:=ptr^.sGroup;
              6: Result:=ptr^.sOwner;
              7: Result:=ptr^.sTime;
              8: Result:=ptr^.sLinkTo;
              9: Result:=ptr^.sNameNoExt;
        //     10: Result:=ptr^.
            end;
            Exit;
          end;
        //------------------------------------------------------

        //Script function (for future)
        //------------------------------------------------------
        if AType=Script then
          begin
          
          Exit;
          end;
        //------------------------------------------------------

        //Plugin function (for future)
        //------------------------------------------------------
        if AType=Plugin then
          begin
            if not WdxPlugins.IsLoaded(AName) then
              if not WdxPlugins.LoadModule(AName) then Exit;

//            DebugLN('AName:'+AName);
//            DebugLn('ptrFileName: '+ptr^.sPath+ptr^.sName);
//            DebugLn('AFunc:'+AFunc);
            
            Result:=WdxPlugins.GetWdxModule(AName).CallContentGetValue(ptr^.sPath+ptr^.sName,AFunc,0,0);
            Exit;
          end;
        //------------------------------------------------------
    //   DebugLn('Leaved ActGetInfo');
end;


function TPanelColumn.GetColumnResultString(ptr: PFileRecItem): string;
var i:integer; s:String;
begin

 s:='';
 if FuncList.Count=0 then exit;
 For i:=0 to FuncList.Count-1 do
   begin
     //Item is simpletext
     if Integer(FuncList.Objects[i])=0 then
       s:=s+FuncList[I]
     else
     //Item is function
       begin
         s:=s+ActGetInfo(FuncList[I],ptr);
       end;
    // DebugLn('Iter:'+Inttostr(i)+'   Rez: '+s+'   ptr.sname: '+ptr^.sName);
   end;
   Result:=s;
end;

initialization
 IntList:=TStringlist.Create;
{0} IntList.Add('GETFILENAME');
{1} IntList.Add('GETFILEEXT');
{2} IntList.Add('GETFILESIZE');
{3} IntList.Add('GETFILEATTR');
{4} IntList.Add('GETFILEPATH');
{5} IntList.Add('GETFILEGROUP');
{6} IntList.Add('GETFILEOWNER');
{7} IntList.Add('GETFILETIME');
{8} IntList.Add('GETFILELINKTO');
{9} IntList.Add('GETFILENAMENOEXT');


Finalization
 FreeAndNil(IntList);
end.


