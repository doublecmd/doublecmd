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
  Classes, SysUtils, uClassesEx, LCLProc, strutils, uTypes, uOSUtils, uDCUtils,Graphics;

  type

    { TColPrm }

    TColPrm = class
      FontName:string;
      FontSize:integer;
      Overcolor:boolean;
      TextColor,
      Background,
      Background2,
      MarkColor,
      CursorColor,
      CursorText:TColor;
     public
     constructor Create;
   end;

 { TPanelColumnsType }
  TPanelColumn=class
  private
    function ActGetInfo(FuncS: string; ptr: PFileRecItem): string;
  //------------------------------------------------------
  public
    //---------------------
    Title:string;
    FuncString:string;

    {String is function or simpletext;
    TObject(integer)=indicator of function: 0 is simpletext; 1 is function;}
    FuncList:TStringList;
    Width:integer;
    Align : TAlignment;
    //---------------------
    FontName:string;
    FontSize:integer;
    Overcolor:boolean;
    TextColor,
    Background,
    Background2,
    MarkColor,
    CursorColor,
    CursorText:TColor;
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
    function GetColumnTitle(const Index:Integer):string;
    function GetColumnFuncString(const Index:Integer):string;
    function GetColumnWidth(const Index:Integer):Integer;
    function GetColumnAlign(const Index:Integer):TAlignment;
    function GetColumnAlignString(const Index:Integer):string;
    //---------------------
    function GetColumnFontName(const Index:Integer):string;
    function GetColumnFontSize(const Index:Integer):integer;
    function GetColumnOvercolor(const Index:Integer):boolean;
    function GetColumnTextColor(const Index:Integer):TColor;
    function GetColumnBackground(const Index:Integer):TColor;
    function GetColumnBackground2(const Index:Integer):TColor;
    function GetColumnMarkColor(const Index:Integer):TColor;
    function GetColumnCursorColor(const Index:Integer):TColor;
    function GetColumnCursorText(const Index:Integer):TColor;
    //---------------------
    function GetColumnPrm(const Index:Integer):TColPrm;

    //---------------------
    function GetColumnItem(const Index:Integer):TPanelColumn;
    function GetColumnItemResultString(const Index:Integer;const  ptr:PFileRecItem):string;
    function GetCount:Integer;
    function Add(Item:TPanelColumn):integer;
    function Add(const Title, FuncString:string;const  Width:integer;const Align: TAlignment=taLeftJustify):integer; overload;
    //---------------------
    procedure SetColumnTitle(const Index:Integer; Title:string);
    procedure SetColumnFuncString(const Index:Integer; FuncString:string);
    procedure SetColumnWidth(Index,Width:integer);
    procedure SetColumnAlign(const Index:Integer; Align: TAlignment);
    //---------------------
    procedure SetColumnFontName(const Index:Integer; Value:string);
    procedure SetColumnFontSize(const Index:Integer; Value:integer);
    procedure SetColumnTextColor(const Index:Integer; Value:TColor);
    procedure SetColumnBackground(const Index:Integer; Value:TColor);
    procedure SetColumnBackground2(const Index:Integer; Value:TColor);
    procedure SetColumnMarkColor(const Index:Integer; Value:TColor);
    procedure SetColumnCursorColor(const Index:Integer; Value:TColor);
    procedure SetColumnCursorText(const Index:Integer; Value:TColor);
    procedure SetColumnOvercolor(const Index:Integer; Value:boolean);
    Procedure SetColumnPrm(const Index:Integer; Value:TColPrm);
    //---------------------
    procedure Delete(const Index:Integer);
    procedure Clear;
    procedure AddDefaultColumns;

    //---------------------
    procedure Load(FileName,SetName:String);overload;
    procedure Load(Ini:TIniFileEx; SetName:string);overload;
    //---------------------

    procedure Load(FileName:String);
    procedure Load(Ini:TIniFileEx);overload;

    //---------------------
    procedure Save(FileName,ASetName:string); overload;
    procedure Save(Ini:TIniFileEx;ASetName:string); overload;
    //---------------------

    procedure Save;
    procedure Save(FileName:string); overload;
    procedure Save(Ini:TIniFileEx); overload;
    //---------------------
    property ColumnsCount:Integer read GetCount;
    property Count:Integer read GetCount;
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
    function GetCount:integer;
  public
    constructor Create;
    destructor Destroy; override;
    //---------------------
    procedure Clear;
    procedure Load(FileName:String);
    procedure Load(Ini:TIniFileEx);overload;
    procedure Save(FileName:string);
    procedure Save(Ini:TIniFileEx); overload;
    function Add(AName:string;Item:TPanelColumnsClass):integer;
    procedure Insert(AIndex: integer; AName: string; Item: TPanelColumnsClass);
    procedure DeleteColumnSet(ini:TIniFileEx; SetName:string);
    procedure DeleteColumnSet(ini:TIniFileEx; SetIndex:Integer); overload;
    procedure CopyColumnSet(ini:TIniFileEx; SetName,NewSetName:string);
    function GetColumnSet(const Index:Integer):TPanelColumnsClass;
    function GetColumnSet(Setname:string):TPanelColumnsClass;

    //---------------------
  published
    property Items:TStringList read fSet;
    property Count:integer read GetCount;
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

function TPanelColumnsClass.GetColumnTitle(const Index: integer): string;
begin
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Title;
end;

function TPanelColumnsClass.GetColumnFuncString(const Index: integer): string;
begin
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).FuncString;
end;

function TPanelColumnsClass.GetColumnWidth(const Index: integer): Integer;
begin
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Width;
end;

function TPanelColumnsClass.GetColumnAlign(const Index: integer): TAlignment;
begin
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Align;
end;

function TPanelColumnsClass.GetColumnAlignString(const Index: integer): string;
begin
  if Index>=Flist.Count then exit;
  case TPanelColumn(Flist[Index]).Align of
    taLeftJustify: Result:='<-';
    taRightJustify: Result:='->';
    taCenter :Result:='='
  end;
end;

function TPanelColumnsClass.GetColumnFontName(const Index: integer): string;
begin
if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).FontName;
end;

function TPanelColumnsClass.GetColumnFontSize(const Index: integer): integer;
begin
if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).FontSize;
end;

function TPanelColumnsClass.GetColumnOvercolor(const Index: integer): boolean;
begin
if Index>=Flist.Count then
begin
  result:=true;
  exit;
end;
  Result:=TPanelColumn(Flist[Index]).Overcolor;
end;

function TPanelColumnsClass.GetColumnTextColor(const Index: integer): TColor;
begin
if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).TextColor;
end;

function TPanelColumnsClass.GetColumnBackground(const Index: integer): TColor;
begin
if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Background;
end;

function TPanelColumnsClass.GetColumnBackground2(const Index: integer): TColor;
begin
if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).Background2;
end;

function TPanelColumnsClass.GetColumnMarkColor(const Index: integer): TColor;
begin
if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).MarkColor;
end;

function TPanelColumnsClass.GetColumnCursorColor(const Index: integer): TColor;
begin
if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).CursorColor;
end;

function TPanelColumnsClass.GetColumnCursorText(const Index: integer): TColor;
begin
if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]).CursorText;
end;

function TPanelColumnsClass.GetColumnPrm(const Index: integer): TColPrm;
begin
if Index>=Flist.Count then exit;
Result:=TColPrm.Create;
Result.Overcolor:=GetColumnOvercolor(Index);
Result.Background:=GetColumnBackground(Index);
Result.Background2:=GetColumnBackground2(Index);
Result.CursorColor:=GetColumnCursorColor(Index);
Result.CursorText:=GetColumnCursorText(Index);
Result.FontName:=GetColumnFontName(Index);
Result.FontSize:=GetColumnFontSize(Index);
Result.MarkColor:=GetColumnMarkColor(Index);
Result.TextColor:=GetColumnTextColor(Index);
end;

function TPanelColumnsClass.GetColumnItem(const Index: integer): TPanelColumn;
begin
  if Index>=Flist.Count then exit;
  Result:=TPanelColumn(Flist[Index]);
end;

function TPanelColumnsClass.GetColumnItemResultString(const Index: integer;
  const ptr: PFileRecItem): string;
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

function TPanelColumnsClass.Add(const Title, FuncString: string; const Width:integer;const Align: TAlignment): integer;
begin
 Result:=Flist.Add(TPanelColumn.Create);
 TPanelColumn(FList[Result]).Title:=Title;
 TPanelColumn(FList[Result]).FuncString:=FuncString;
 FillListFromString(TPanelColumn(FList[Result]).FuncList,FuncString);
 TPanelColumn(FList[Result]).Width:=Width;
 TPanelColumn(FList[Result]).Align:=Align;
 
  TPanelColumn(Flist[Result]).FontName:=gFontName;
  TPanelColumn(Flist[Result]).FontSize:=gFontSize;
  TPanelColumn(Flist[Result]).TextColor:=gForeColor;
  TPanelColumn(Flist[Result]).Background:=gBackColor;
  TPanelColumn(Flist[Result]).Background2:=gBackColor2;
  TPanelColumn(Flist[Result]).MarkColor:=gMarkColor;
  TPanelColumn(Flist[Result]).CursorColor:=gCursorColor;
  TPanelColumn(Flist[Result]).CursorText:=gCursorText;
end;

procedure TPanelColumnsClass.SetColumnTitle(const Index: integer; Title: string);
begin
  if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).Title:=Title;
end;

procedure TPanelColumnsClass.SetColumnFuncString(const Index: integer;
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

procedure TPanelColumnsClass.SetColumnAlign(const Index: Integer; Align: TAlignment);
begin
  if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).Align:=Align;
end;

procedure TPanelColumnsClass.SetColumnFontName(const Index: integer; Value: string);
begin
if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).FontName:=Value;
end;

procedure TPanelColumnsClass.SetColumnFontSize(const Index: integer; Value: integer);
begin
if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).FontSize:=Value;
end;

procedure TPanelColumnsClass.SetColumnTextColor(const Index: integer; Value: TColor);
begin
if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).TextColor:=Value;
end;

procedure TPanelColumnsClass.SetColumnBackground(const Index: integer; Value: TColor
  );
begin
if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).Background:=Value;
end;

procedure TPanelColumnsClass.SetColumnBackground2(const Index: integer; Value: TColor
  );
begin
if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).Background2:=Value;
end;

procedure TPanelColumnsClass.SetColumnMarkColor(const Index: integer; Value: TColor);
begin
if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).MarkColor:=Value;
end;

procedure TPanelColumnsClass.SetColumnCursorColor(const Index: integer; Value: TColor
  );
begin
if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).CursorColor:=Value;
end;

procedure TPanelColumnsClass.SetColumnCursorText(const Index: integer; Value: TColor
  );
begin
if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).CursorText:=Value;
end;

procedure TPanelColumnsClass.SetColumnOvercolor(const Index: integer; Value: boolean);
begin
if Index>Flist.Count then exit;
  TPanelColumn(Flist[Index]).Overcolor:=Value;
end;

procedure TPanelColumnsClass.SetColumnPrm(const Index: integer; Value: TColPrm);
begin
 if Index>=Flist.Count then exit;
SetColumnBackground(Index, Value.Background);
SetColumnBackground2(Index, Value.Background2);
SetColumnCursorColor(Index, Value.CursorColor);
SetColumnCursorText(Index, Value.CursorText);
SetColumnFontName(Index, Value.FontName);
SetColumnFontSize(Index, Value.FontSize);
SetColumnMarkColor(Index, Value.MarkColor);
SetColumnTextColor(Index, Value.TextColor);
SetColumnOvercolor(Index,Value.Overcolor);

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

procedure TPanelColumnsClass.Load(Ini: TIniFileEx; SetName: string);
begin
  fSetName:=SetName;
  Load(Ini);
end;

procedure TPanelColumnsClass.Load(FileName:string);
var Ini:TIniFileEx;
begin
  try
    Ini:=TIniFileEx.Create(FileName);
    Load(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TPanelColumnsClass.Load(Ini: TIniFileEx);
var aCount,I:Integer;
begin
    Self.Clear;
    FCurrentColumnsFile:=Ini.FileName;
    aCount:=Ini.ReadInteger(fSetName,'ColumnCount',0);
    //---------------------
    if aCount=0 then
      begin
        AddDefaultColumns;
        Exit;
      end;
    //---------------------
    For I:=0 to aCount-1 do
      begin
        Flist.Add(TPanelColumn.Create);
        TPanelColumn(FList[I]).Title:=Ini.ReadString(fSetName,'Column'+IntToStr(I+1)+'Title','');
         //---------------------
          TPanelColumn(FList[I]).FuncString:=Ini.ReadString(fSetName,'Column'+IntToStr(I+1)+'FuncsString','');
          FillListFromString(TPanelColumn(FList[I]).FuncList,Ini.ReadString(fSetName,'Column'+IntToStr(I+1)+'FuncsString',''));
          TPanelColumn(FList[I]).Width:=Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'Width',50);
          TPanelColumn(FList[I]).Align:=TAlignment(Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'Align',0));
         //---------------------

          TPanelColumn(FList[I]).FontName:=Ini.ReadString(fSetName,'Column'+IntToStr(I+1)+'FontName',gFontName);
          TPanelColumn(FList[I]).FontSize:=Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'FontSize',gFontSize);
          TPanelColumn(FList[I]).Overcolor:=Ini.ReadBool(fSetName,'Column'+IntToStr(I+1)+'Overcolor',true );
                    
          TPanelColumn(FList[I]).TextColor:=Tcolor(Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'TextColor',gForeColor));
          TPanelColumn(FList[I]).Background:=Tcolor(Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'Background',gBackColor ));
          TPanelColumn(FList[I]).Background2:=Tcolor(Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'Background2',gBackColor2 ));
          TPanelColumn(FList[I]).MarkColor:=Tcolor(Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'MarkColor',gMarkColor ));
          TPanelColumn(FList[I]).CursorColor:=Tcolor(Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'CursorColor',gCursorColor ));
          TPanelColumn(FList[I]).CursorText:=Tcolor(Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'CursorText',gCursorText ));

         //---------------------
      end;
    //---------------------
end;

procedure TPanelColumnsClass.Save(FileName, ASetName: string);
begin
  fSetName:=ASetName;
  Save(FileName);
end;

procedure TPanelColumnsClass.Save(Ini: TIniFileEx; ASetName: string);
begin
  fSetName:=ASetName;
  Save(Ini);
end;

procedure TPanelColumnsClass.Save;
begin
 Save(CurrentColumnsFile);
end;

procedure TPanelColumnsClass.Save(FileName: string);
 var  Ini:TIniFileEx;
begin
  try
    Ini:=TIniFileEx.Create(FileName);
     Save(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TPanelColumnsClass.Save(Ini: TIniFileEx);
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
        //---------------------
        Ini.WriteString(fSetName,'Column'+IntToStr(I+1)+'FontName',TPanelColumn(FList[I]).FontName);
        Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'FontSize', TPanelColumn(FList[I]).FontSize);
        Ini.WriteBool(fSetName,'Column'+IntToStr(I+1)+'Overcolor', TPanelColumn(FList[I]).Overcolor);

        if TPanelColumn(FList[I]).TextColor <>clNone then
        Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'TextColor', TPanelColumn(FList[I]).TextColor);
        if TPanelColumn(FList[I]).Background <>clNone then
        Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'Background', TPanelColumn(FList[I]).Background);
        if TPanelColumn(FList[I]).Background2 <>clNone then
        Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'Background2', TPanelColumn(FList[I]).Background2);
        if TPanelColumn(FList[I]).MarkColor <>clNone then
        Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'MarkColor', TPanelColumn(FList[I]).MarkColor);
        if TPanelColumn(FList[I]).CursorColor <>clNone then
        Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'CursorColor', TPanelColumn(FList[I]).CursorColor);
        if TPanelColumn(FList[I]).CursorText <>clNone then
        Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'CursorText', TPanelColumn(FList[I]).CursorText);

      end;
end;

procedure TPanelColumnsClass.Delete(const Index: Integer);
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

        //Plugin function
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
     if PtrInt(FuncList.Objects[i])=0 then
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

function TPanelColumnsList.GetCount: integer;
begin
  Result:=fSet.Count;
end;

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
var Ini:TIniFileEx;
begin
  try
    Ini:=TIniFileEx.Create(FileName);
    Load(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TPanelColumnsList.Load(Ini: TIniFileEx);
var aCount,I:Integer;
begin
    Self.Clear;
    aCount:=Ini.ReadInteger('ColumnsSet','ColumnsSetCount',0);
    For I:=0 to aCount-1 do
      begin
        fSet.AddObject(Ini.ReadString('ColumnsSet','ColumnsSet'+IntToStr(I+1)+'Name',''),TPanelColumnsClass.Create);
        TPanelColumnsClass(fSet.Objects[I]).Load(ini,fset[i]);
        DebugLn('FsetName='+Fset[i]);
      end;
      DebugLn('FsetCount='+inttostr(fset.Count));
end;

procedure TPanelColumnsList.Save(FileName: string);
 var  Ini:TIniFileEx;
begin
  try
    Ini:=TIniFileEx.Create(FileName);
     Save(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TPanelColumnsList.Save(Ini: TIniFileEx);
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


procedure TPanelColumnsList.DeleteColumnSet(ini: TIniFileEx; SetName: string);
var x:integer;
begin
    x:=fSet.IndexOf(SetName);
    if x<>-1 then
      DeleteColumnSet(ini,x);
end;

procedure TPanelColumnsList.DeleteColumnSet(ini: TIniFileEx; SetIndex: Integer);
begin
    if (SetIndex>=Fset.Count) or (SetIndex<0) then exit;
    Ini.EraseSection(FSet[SetIndex]);
    TPanelColumnsClass(fSet.Objects[SetIndex]).Free;
    fSet.Delete(SetIndex);
end;

procedure TPanelColumnsList.CopyColumnSet(ini: TIniFileEx; SetName,
  NewSetName: string);
var x,i:integer; st:TStringList;
begin
  x:=fSet.IndexOf(SetName);
  if x<>-1 then
    begin
      try
        st:=TStringList.Create;
        ini.ReadSectionValues(SetName,st);
        for i:=0 to st.Count-1 do
          begin
            ini.WriteString(NewSetName,st.Names[i],st.Values[st.Names[i]]);
          end;
      finally
        st.Free;
      end;
      fSet.AddObject(NewSetName,TPanelColumnsClass.Create);
      TPanelColumnsClass(fset.Objects[fset.Count-1]).Name:=NewSetName;
      TPanelColumnsClass(fset.Objects[fset.Count-1]).Load(ini,NewSetName);
    end;

end;

function TPanelColumnsList.GetColumnSet(const Index: Integer): TPanelColumnsClass;
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


{ TColPrm }

constructor TColPrm.Create;
begin
Self.Overcolor:=true;
Self.FontName:=gFontName;
Self.FontSize:=gFontSize;
Self.TextColor:=gForeColor;
Self.Background:=gBackColor;
Self.Background2:=gBackColor2;
Self.MarkColor:=gMarkColor;
Self.CursorColor:=gCursorColor;
Self.CursorText:=gCursorText;
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


