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
  Classes, SysUtils, Inifiles, LCLProc, strutils, uTypes, uOSUtils, uDCUtils;

  type

 { TPanelColumnsType }
  TPanelColumn=class
  private
    function ActGetInfo(FuncS: string; ptr: PFileRecItem): string;
  //------------------------------------------------------
  public
    Title:string;
    FuncString:string;

    {String is function or simpletext;
    TObject(integer)=indicator of function: 0 is simpletext; 1 is function;}
    FuncList:TStringList;

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
   fSetName:string;
  //------------------------------------------------------
  public
    constructor Create;
    destructor Destroy; override;
    //---------------------
    function GetColumnTitle(Index:integer):string;
    function GetColumnFuncString(Index:integer):string;
    function GetColumnWidth(Index:integer):Integer;
    function GetColumnAlign(Index:integer):TAlignment;
    function GetColumnAlignString(Index:integer):string;
    function GetColumnItem(Index:integer):TPanelColumn;
    function GetColumnItemResultString(Index:integer; ptr:PFileRecItem):string;
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

    //---------------------
    procedure Load(FileName,SetName:String);overload;
    procedure Load(Ini:TIniFile; SetName:string);overload;
    //---------------------

    procedure Load(FileName:String);
    procedure Load(Ini:TIniFile);overload;

    //---------------------
    procedure Save(FileName,ASetName:string); overload;
    procedure Save(Ini:TIniFile;ASetName:string); overload;
    //---------------------

    procedure Save;
    procedure Save(FileName:string); overload;
    procedure Save(Ini:TIniFile); overload;
    //---------------------
    property ColumnsCount:Integer read GetCount;
    property CurrentColumnsFile:string read FCurrentColumnsFile;
    property CurrentColumnsSetName:string read fSetName write fSetName;
    property SetName:string read fSetName write fSetName;
    property Name:string read fSetName write fSetName;
  //------------------------------------------------------
  end;

  { TPanelColumnsList }

  TPanelColumnsList= class
  private
    fSet:TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    //---------------------
    procedure Clear;
    procedure Load(FileName:String);
    procedure Load(Ini:TIniFile);overload;
    procedure Save(FileName:string);
    procedure Save(Ini:TIniFile); overload;
    function Add(AName:string;Item:TPanelColumnsClass):integer;
    procedure Insert(AIndex: integer; AName: string; Item: TPanelColumnsClass);
    procedure DeleteColumnSet(ini:TInifile; SetName:string);
    procedure DeleteColumnSet(ini:TInifile; SetIndex:Integer); overload;
    procedure CopyColumnSet(ini:TInifile; SetName,NewSetName:string);
    function GetColumnSet(Index:Integer):TPanelColumnsClass;
    function GetColumnSet(Setname:string):TPanelColumnsClass;
    //---------------------
  published
    property Items:TStringList read fSet;
  end;


  procedure FillListFromString(List: TStrings; FuncString: string);
  function StrToAlign(str:string):TAlignment;

  var IntList:TStringList;

implementation
uses uLng, uGlobs;

function StrToAlign(str:string):TAlignment;
begin
if str='<-' then
  Result:=taLeftJustify
else
if str='->' then
  Result:=taRightJustify
else
if str='=' then
  Result:=taCenter;
end;

{ TPanelColumnsType }

function TPanelColumnsClass.GetColumnTitle(Index: integer): string;
begin
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Title;
end;

function TPanelColumnsClass.GetColumnFuncString(Index: integer): string;
begin
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).FuncString;
end;

function TPanelColumnsClass.GetColumnWidth(Index: integer): Integer;
begin
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Width;
end;

function TPanelColumnsClass.GetColumnAlign(Index: integer): TAlignment;
begin
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Align;
end;

function TPanelColumnsClass.GetColumnAlignString(Index: integer): string;
begin
  if Index>=Flist.Count then exit;
  case TPanelColumn(Flist[Index]).Align of
    taLeftJustify: Result:='<-';
    taRightJustify: Result:='->';
    taCenter :Result:='='
  end;
end;

function TPanelColumnsClass.GetColumnItem(Index: integer): TPanelColumn;
begin
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]);
end;

function TPanelColumnsClass.GetColumnItemResultString(Index: integer;
  ptr: PFileRecItem): string;
begin
  Result:='';
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).GetColumnResultString(ptr);
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
  SetName:='Default';
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


procedure TPanelColumnsClass.Load(FileName, SetName: String);
begin
  fSetName:=SetName;
  Load(FileName);
end;

procedure TPanelColumnsClass.Load(Ini: TIniFile; SetName: string);
begin
  fSetName:=SetName;
  Load(Ini);
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
    Count:=Ini.ReadInteger(fSetName,'ColumnCount',0);
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
        TPanelColumn(FList[I]).Title:=Ini.ReadString(fSetName,'Column'+IntToStr(I+1)+'Title','');
         //---------------------
          TPanelColumn(FList[I]).FuncString:=Ini.ReadString(fSetName,'Column'+IntToStr(I+1)+'FuncsString','');
          FillListFromString(TPanelColumn(FList[I]).FuncList,Ini.ReadString(fSetName,'Column'+IntToStr(I+1)+'FuncsString',''));
          TPanelColumn(FList[I]).Width:=Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'Width',20);
          TPanelColumn(FList[I]).Align:=TAlignment(Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'Align',0));
         //---------------------
      end;
    //---------------------
end;

procedure TPanelColumnsClass.Save(FileName, ASetName: string);
begin
  fSetName:=ASetName;
  Save(FileName);
end;

procedure TPanelColumnsClass.Save(Ini: TIniFile; ASetName: string);
begin
  fSetName:=ASetName;
  Save(Ini);
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
    if fSetName='' then Exit;
    Ini.EraseSection(fSetName);
    Ini.WriteInteger(fSetName,'ColumnCount',FList.Count);
    For I:=0 to FList.Count-1 do
      begin
        Ini.WriteString(fSetName,'Column'+IntToStr(I+1)+'Title',TPanelColumn(FList[I]).Title);
        Ini.WriteString(fSetName,'Column'+IntToStr(I+1)+'FuncsString',TPanelColumn(FList[I]).FuncString);
        Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'Width', TPanelColumn(FList[I]).Width);
        Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'Align', Integer(TPanelColumn(FList[I]).Align));
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
//            DebugLn('ptrFileName: '+ptr^.sPath+ptr^.sName);
            if WdxPlugins.GetWdxModule(AName).FileParamVSDetectStr(ptr) then
            begin
              Result:=WdxPlugins.GetWdxModule(AName).CallContentGetValue(ptr^.sPath+ptr^.sName,AFunc,0,0);
            end;
            Exit;
          end;
        //------------------------------------------------------
end;


function TPanelColumn.GetColumnResultString(ptr: PFileRecItem): string;
var i:integer; s:String;
begin

 s:='';
 Result:='';
 if not assigned(FuncList) then exit;
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
   end;
   Result:=s;
end;

{ TPanelColumnsList }

constructor TPanelColumnsList.Create;
begin
  FSet:=TStringList.Create;
end;

destructor TPanelColumnsList.Destroy;
var i:integer;
begin

  if assigned(FSet) then
    begin
     for i:=0 to Fset.Count-1 do
      TPanelColumnsClass(Fset.Objects[i]).Free;
      FreeAndNil(FSet);
    end;

  inherited Destroy;
end;

procedure TPanelColumnsList.Clear;
var i:integer;
begin
  for i:=0 to Fset.Count-1 do
    TPanelColumnsClass(Fset.Objects[i]).Free;
  Fset.Clear;
end;

procedure TPanelColumnsList.Load(FileName: String);
var Ini:TIniFile;
begin
  try
    Ini:=TIniFile.Create(FileName);
    Load(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TPanelColumnsList.Load(Ini: TIniFile);
var Count,I:Integer;
begin
    Self.Clear;
    Count:=Ini.ReadInteger('ColumnsSet','ColumnsSetCount',0);
    For I:=0 to Count-1 do
      begin
        fSet.AddObject(Ini.ReadString('ColumnsSet','ColumnsSet'+IntToStr(I+1)+'Name',''),TPanelColumnsClass.Create);
        TPanelColumnsClass(fSet.Objects[I]).Load(ini,fset[i]);
        DebugLn('FsetName='+Fset[i]);
      end;
      DebugLn('FsetCount='+inttostr(fset.Count));
end;

procedure TPanelColumnsList.Save(FileName: string);
 var  Ini:TIniFile;
begin
  try
    Ini:=TIniFile.Create(FileName);
     Save(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TPanelColumnsList.Save(Ini: TIniFile);
var I:integer;
begin
    Ini.EraseSection('ColumnsSet');
    Ini.WriteInteger('ColumnsSet','ColumnsSetCount',FSet.Count);
    For I:=0 to FSet.Count-1 do
      begin
        Ini.WriteString('ColumnsSet','ColumnsSet'+IntToStr(I+1)+'Name',FSet[i]);
        TPanelColumnsClass(Fset.Objects[i]).Save(ini,FSet[i]);
      end;
end;

function TPanelColumnsList.Add(AName:string;Item: TPanelColumnsClass): integer;
begin
  Result:=Fset.AddObject(AName,Item);
end;

procedure TPanelColumnsList.Insert(AIndex: integer; AName: string;
  Item: TPanelColumnsClass);
begin
  Fset.InsertObject(AIndex,AName,Item);
end;


procedure TPanelColumnsList.DeleteColumnSet(ini: TInifile; SetName: string);
var x:integer;
begin
    x:=fSet.IndexOf(SetName);
    if x<>-1 then
      DeleteColumnSet(ini,x);
end;

procedure TPanelColumnsList.DeleteColumnSet(ini: TInifile; SetIndex: Integer);
begin
    if (SetIndex>=Fset.Count) or (SetIndex<0) then exit;
    Ini.EraseSection(FSet[SetIndex]);
    TPanelColumnsClass(fSet.Objects[SetIndex]).Free;
    fSet.Delete(SetIndex);
end;

procedure TPanelColumnsList.CopyColumnSet(ini: TInifile; SetName,
  NewSetName: string);
var x,i:integer; st:TStringList;
begin
  x:=fSet.IndexOf(SetName);
  if x<>-1 then
    begin
{      try
        st:=TStringList.Create;
        ini.ReadSectionValues(SetName,st);
        for i:=0 to st.Count-1 do
          begin
            ini.WriteString(NewSetName,st.Names[i],st.Values[st.Names[i]]);
          end;
      finally
        st.Free;
      end;}
      fSet.AddObject(NewSetName,fset.Objects[x]);
    end;

end;

function TPanelColumnsList.GetColumnSet(Index: Integer): TPanelColumnsClass;
begin
//DebugLn('FsetCount='+inttostr(fset.Count));
 if (Index>-1) and (Index<Fset.Count) then
  Result:=TPanelColumnsClass(Fset.Objects[Index])
 else
 begin
  if fset.Count=0 then
   begin
    Fset.AddObject('Default',TPanelColumnsClass.Create);
    TPanelColumnsClass(Fset.Objects[0]).AddDefaultColumns;
   end;
   Result:=TPanelColumnsClass(Fset.Objects[0]);
 end;
end;

function TPanelColumnsList.GetColumnSet(Setname: string): TPanelColumnsClass;
begin
//DebugLn('FsetCount='+inttostr(fset.Count));
if fset.IndexOf(Setname)>-1 then
  Result:=TPanelColumnsClass(Fset.Objects[fset.IndexOf(Setname)])
   else
 begin
  if fset.Count=0 then
   begin
    Fset.AddObject('Default',TPanelColumnsClass.Create);
    TPanelColumnsClass(Fset.Objects[0]).AddDefaultColumns;
   end;
   Result:=TPanelColumnsClass(Fset.Objects[0]);
 end;
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


