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
  Classes, SysUtils, uClassesEx, LCLProc, Graphics, uFile, uFileSource,
  uXmlConfig, uFileFunctions;

type

  { TColPrm }

  TColPrm = class
    FontName: String;
    FontSize: Integer;
    FontStyle: TFontStyles;
    Overcolor: Boolean;
    TextColor,
    Background,
    Background2,
    MarkColor,
    CursorColor,
    CursorText: TColor;
  public
    constructor Create;
  end;

  { TPanelColumnsType }

  TPanelColumn = class
  public
    //---------------------
    Title: String;
    FuncString: String;

    {String is function or simpletext;
    TObject(integer)=indicator of function: 0 is simpletext; 1 is function;}
    FuncList: TStringList;
    Width: Integer;
    Align: TAlignment;
    //---------------------
    FontName: String;
    FontSize: Integer;
    FontStyle: TFontStyles;
    Overcolor: Boolean;
    TextColor,
    Background,
    Background2,
    MarkColor,
    CursorColor,
    CursorText: TColor;
    //---------------------

    constructor Create;
    destructor Destroy; override;

    //---------------------
    function GetColumnResultString(AFile: TFile; const AFileSource: IFileSource): String;
    {en
       Converts string functions in the column into their integer values,
       so that they don't have to be compared by string during sorting.
       Call this before sorting then pass result to Compare in the sorting loop.
    }
    function GetColumnFunctions: TFileFunctions;
    //------------------------------------------------------
  end;

  { TPanelColumnsClass }

  TPanelColumnsClass = class
    //------------------------------------------------------
  private
    FList: TList;
    fSetName: String;

    // Global settings for columns view.
    FCustomView: Boolean;
    FCursorBorder: Boolean;
    FCursorBorderColor: TColor;
    //------------------------------------------------------
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const OtherColumnsClass: TPanelColumnsClass);
    //---------------------
    function GetColumnTitle(const Index: Integer): String;
    function GetColumnFuncString(const Index: Integer): String;
    function GetColumnWidth(const Index: Integer): Integer;
    function GetColumnAlign(const Index: Integer): TAlignment;
    function GetColumnAlignString(const Index: Integer): String;
    //---------------------
    function GetColumnFontName(const Index: Integer): String;
    function GetColumnFontSize(const Index: Integer): Integer;
    function GetColumnFontStyle(const Index: Integer): TFontStyles;
    function GetColumnOvercolor(const Index: Integer): Boolean;
    function GetColumnTextColor(const Index: Integer): TColor;
    function GetColumnBackground(const Index: Integer): TColor;
    function GetColumnBackground2(const Index: Integer): TColor;
    function GetColumnMarkColor(const Index: Integer): TColor;
    function GetColumnCursorColor(const Index: Integer): TColor;
    function GetColumnCursorText(const Index: Integer): TColor;
    //---------------------
    function GetColumnPrm(const Index: Integer): TColPrm;
    //---------------------
    function GetCursorBorder: Boolean;
    function GetCursorBorderColor: TColor;
    //---------------------
    function GetColumnItem(const Index: Integer): TPanelColumn;
    function GetColumnItemResultString(const Index: Integer;
      const AFile: TFile; const AFileSource: IFileSource): String;
    function GetCount: Integer;
    function Add(Item: TPanelColumn): Integer;
    function Add(const Title, FuncString: String; const Width: Integer;
      const Align: TAlignment = taLeftJustify): Integer; overload;
    //---------------------
    procedure SetColumnTitle(const Index: Integer; Title: String);
    procedure SetColumnFuncString(const Index: Integer; FuncString: String);
    procedure SetColumnWidth(Index, Width: Integer);
    procedure SetColumnAlign(const Index: Integer; Align: TAlignment);
    //---------------------
    procedure SetColumnFontName(const Index: Integer; Value: String);
    procedure SetColumnFontSize(const Index: Integer; Value: Integer);
    procedure SetColumnFontStyle(const Index: Integer; Value: TFontStyles);
    procedure SetColumnTextColor(const Index: Integer; Value: TColor);
    procedure SetColumnBackground(const Index: Integer; Value: TColor);
    procedure SetColumnBackground2(const Index: Integer; Value: TColor);
    procedure SetColumnMarkColor(const Index: Integer; Value: TColor);
    procedure SetColumnCursorColor(const Index: Integer; Value: TColor);
    procedure SetColumnCursorText(const Index: Integer; Value: TColor);
    procedure SetColumnOvercolor(const Index: Integer; Value: Boolean);
    procedure SetColumnPrm(const Index: Integer; Value: TColPrm);
    //---------------------
    procedure SetCursorBorder(ShowBorder: Boolean);
    procedure SetCursorBorderColor(Color: TColor);
    //---------------------
    procedure Delete(const Index: Integer);
    procedure Clear;
    procedure AddDefaultColumns;
    //---------------------
    procedure Load(Ini: TIniFileEx; SetName: String); overload;
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    //---------------------
    procedure Save(Ini: TIniFileEx; ASetName: String); overload;
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    //---------------------
    property ColumnsCount: Integer read GetCount;
    property Count: Integer read GetCount;
    property CustomView: Boolean read FCustomView write FCustomView;
    property CurrentColumnsSetName: String read fSetName write fSetName;
    property SetName: String read fSetName write fSetName;
    property Name: String read fSetName write fSetName;
    //------------------------------------------------------
  end;

  { TPanelColumnsList }

  TPanelColumnsList = class
  private
    fSet: TStringList;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    //---------------------
    procedure Clear;
    procedure Load(Ini: TIniFileEx); overload;
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    procedure Save(Ini: TIniFileEx); overload;
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    function Add(AName: String; Item: TPanelColumnsClass): Integer;
    procedure Insert(AIndex: Integer; AName: String; Item: TPanelColumnsClass);
    procedure DeleteColumnSet(SetName: String);
    procedure DeleteColumnSet(SetIndex: Integer); overload;
    procedure CopyColumnSet(SetName, NewSetName: String);
    function GetColumnSet(const Index: Integer): TPanelColumnsClass;
    function GetColumnSet(Setname: String): TPanelColumnsClass;
    //---------------------
    property Items: TStringList read fSet;
    property Count: Integer read GetCount;
  end;

  function StrToAlign(str: String): TAlignment;

implementation

uses
  uLng, uGlobs;

function StrToAlign(str: String): TAlignment;
begin
  if str = '<-' then
    Result := taLeftJustify
  else
  if str = '->' then
    Result := taRightJustify
  else
  if str = '=' then
    Result := taCenter;
end;

procedure FillListFromString(List: TStrings; FuncString: String);
var
  s, st: String;
begin
  s := FuncString;
  if Length(s) = 0 then
    Exit;

  while pos('[', s) > 0 do
  begin
    st := Copy(s, 1, pos('[', s) - 1);
    if st <> '' then
      List.AddObject(st, TObject(0));
    Delete(s, 1, pos('[', s));
    st := Copy(s, 1, pos(']', s) - 1);
    if st <> '' then
      List.AddObject(st, TObject(1));
    Delete(s, 1, pos(']', s));
  end;

  st := Copy(s, 1, length(s));
  if st <> '' then
    List.AddObject(st, TObject(0));
end;

{ TPanelColumnsType }

function TPanelColumnsClass.GetColumnTitle(const Index: Integer): String;
begin
  if Index >= Flist.Count then
    Exit(EmptyStr);
  Result := TPanelColumn(Flist[Index]).Title;
end;

function TPanelColumnsClass.GetColumnFuncString(const Index: Integer): String;
begin
  if Index >= Flist.Count then
    Exit(EmptyStr);
  Result := TPanelColumn(Flist[Index]).FuncString;
end;

function TPanelColumnsClass.GetColumnWidth(const Index: Integer): Integer;
begin
  if Index >= Flist.Count then
    Exit(0);
  Result := TPanelColumn(Flist[Index]).Width;
end;

function TPanelColumnsClass.GetColumnAlign(const Index: Integer): TAlignment;
begin
  if Index >= Flist.Count then
    Exit(taLeftJustify);
  Result := TPanelColumn(Flist[Index]).Align;
end;

function TPanelColumnsClass.GetColumnAlignString(const Index: Integer): String;
begin
  if Index >= Flist.Count then
    Exit(EmptyStr);

  case TPanelColumn(Flist[Index]).Align of
    taLeftJustify:  Result := '<-';
    taRightJustify: Result := '->';
    taCenter:       Result := '=';
  end;
end;

function TPanelColumnsClass.GetColumnFontName(const Index: Integer): String;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).FontName
  else
    Result := gFontName;
end;

function TPanelColumnsClass.GetColumnFontSize(const Index: Integer): Integer;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).FontSize
  else
    Result := gFontSize;
end;

function TPanelColumnsClass.GetColumnFontStyle(const Index: Integer): TFontStyles;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).FontStyle
  else
    Result := gFontStyle;
end;

function TPanelColumnsClass.GetColumnOvercolor(const Index: Integer): Boolean;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).Overcolor
  else
    Result := True;
end;

function TPanelColumnsClass.GetColumnTextColor(const Index: Integer): TColor;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).TextColor
  else
    Result := gForeColor;
end;

function TPanelColumnsClass.GetColumnBackground(const Index: Integer): TColor;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).Background
  else
    Result := gBackColor;
end;

function TPanelColumnsClass.GetColumnBackground2(const Index: Integer): TColor;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).Background2
  else
    Result := gBackColor2;
end;

function TPanelColumnsClass.GetColumnMarkColor(const Index: Integer): TColor;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).MarkColor
  else
    Result := gMarkColor;
end;

function TPanelColumnsClass.GetColumnCursorColor(const Index: Integer): TColor;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).CursorColor
  else
    Result := gCursorColor;
end;

function TPanelColumnsClass.GetColumnCursorText(const Index: Integer): TColor;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).CursorText
  else
    Result := gCursorText;
end;

function TPanelColumnsClass.GetColumnPrm(const Index: Integer): TColPrm;
begin
  if Index >= Flist.Count then
    Exit(nil);

  Result := TColPrm.Create;
  Result.Overcolor := GetColumnOvercolor(Index);
  Result.Background := GetColumnBackground(Index);
  Result.Background2 := GetColumnBackground2(Index);
  Result.CursorColor := GetColumnCursorColor(Index);
  Result.CursorText := GetColumnCursorText(Index);
  Result.FontName := GetColumnFontName(Index);
  Result.FontSize := GetColumnFontSize(Index);
  Result.FontStyle := GetColumnFontStyle(Index);
  Result.MarkColor := GetColumnMarkColor(Index);
  Result.TextColor := GetColumnTextColor(Index);
end;

function TPanelColumnsClass.GetCursorBorder: Boolean;
begin
  Result := FCursorBorder;
end;

function TPanelColumnsClass.GetCursorBorderColor: TColor;
begin
  Result := FCursorBorderColor;
end;

function TPanelColumnsClass.GetColumnItem(const Index: Integer): TPanelColumn;
begin
  if Index >= Flist.Count then
    Exit(nil);
  Result := TPanelColumn(Flist[Index]);
end;

function TPanelColumnsClass.GetColumnItemResultString(const Index: Integer;
  const AFile: TFile; const AFileSource: IFileSource): String;
begin
  if Index >= Flist.Count then
    Exit(EmptyStr);
  Result := TPanelColumn(Flist[Index]).GetColumnResultString(AFile, AFileSource);
end;

constructor TPanelColumnsClass.Create;
begin
  FList := TList.Create;
end;

procedure TPanelColumnsClass.Clear;
begin
  while Flist.Count > 0 do
  begin
    TPanelColumn(Flist[0]).Free;
    FList.Delete(0);
  end;
end;

destructor TPanelColumnsClass.Destroy;
begin
  Self.Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TPanelColumnsClass.Assign(const OtherColumnsClass: TPanelColumnsClass);
var
  OldColumn, NewColumn: TPanelColumn;
  i: Integer;
begin
  Clear;

  if not Assigned(OtherColumnsClass) then
    Exit;

  Name := OtherColumnsClass.Name;
  FCustomView := OtherColumnsClass.FCustomView;
  FCursorBorder := OtherColumnsClass.FCursorBorder;
  FCursorBorderColor := OtherColumnsClass.FCursorBorderColor;

  for i := 0 to OtherColumnsClass.ColumnsCount - 1 do
  begin
    OldColumn := OtherColumnsClass.GetColumnItem(i);
    NewColumn := TPanelColumn.Create;
    Add(NewColumn);

    NewColumn.Title       := OldColumn.Title;
    NewColumn.FuncString  := OldColumn.FuncString;
    FillListFromString(NewColumn.FuncList, NewColumn.FuncString);
    NewColumn.Width       := OldColumn.Width;
    NewColumn.Align       := OldColumn.Align;
    NewColumn.FontName    := OldColumn.FontName;
    NewColumn.FontSize    := OldColumn.FontSize;
    NewColumn.FontStyle   := OldColumn.FontStyle;
    NewColumn.Overcolor   := OldColumn.Overcolor;
    NewColumn.TextColor   := OldColumn.TextColor;
    NewColumn.Background  := OldColumn.Background;
    NewColumn.Background2 := OldColumn.Background2;
    NewColumn.MarkColor   := OldColumn.MarkColor;
    NewColumn.CursorColor := OldColumn.CursorColor;
    NewColumn.CursorText  := OldColumn.CursorText;
  end;
end;

function TPanelColumnsClass.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TPanelColumnsClass.Add(Item: TPanelColumn): Integer;
begin
  Result := FList.Add(Item);
end;

function TPanelColumnsClass.Add(const Title, FuncString: String;
  const Width: Integer; const Align: TAlignment): Integer;
var
  AColumn: TPanelColumn;
begin
  AColumn := TPanelColumn.Create;
  Result := FList.Add(AColumn);

  AColumn.Title       := Title;
  AColumn.FuncString  := FuncString;
  FillListFromString(AColumn.FuncList, FuncString);
  AColumn.Width       := Width;
  AColumn.Align       := Align;
  AColumn.FontName    := gFontName;
  AColumn.FontSize    := gFontSize;
  AColumn.FontStyle   := gFontStyle;
  AColumn.Overcolor   := True;
  AColumn.TextColor   := gForeColor;
  AColumn.Background  := gBackColor;
  AColumn.Background2 := gBackColor2;
  AColumn.MarkColor   := gMarkColor;
  AColumn.CursorColor := gCursorColor;
  AColumn.CursorText  := gCursorText;
end;

procedure TPanelColumnsClass.SetColumnTitle(const Index: Integer; Title: String);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).Title := Title;
end;

procedure TPanelColumnsClass.SetColumnFuncString(const Index: Integer; FuncString: String);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).FuncString := FuncString;
end;

procedure TPanelColumnsClass.SetColumnWidth(Index, Width: Integer);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).Width := Width;
end;

procedure TPanelColumnsClass.SetColumnAlign(const Index: Integer; Align: TAlignment);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).Align := Align;
end;

procedure TPanelColumnsClass.SetColumnFontName(const Index: Integer; Value: String);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).FontName := Value;
end;

procedure TPanelColumnsClass.SetColumnFontSize(const Index: Integer; Value: Integer);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).FontSize := Value;
end;

procedure TPanelColumnsClass.SetColumnFontStyle(const Index: Integer; Value: TFontStyles);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).FontStyle := Value;
end;

procedure TPanelColumnsClass.SetColumnTextColor(const Index: Integer; Value: TColor);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).TextColor := Value;
end;

procedure TPanelColumnsClass.SetColumnBackground(const Index: Integer; Value: TColor);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).Background := Value;
end;

procedure TPanelColumnsClass.SetColumnBackground2(const Index: Integer; Value: TColor);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).Background2 := Value;
end;

procedure TPanelColumnsClass.SetColumnMarkColor(const Index: Integer; Value: TColor);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).MarkColor := Value;
end;

procedure TPanelColumnsClass.SetColumnCursorColor(const Index: Integer; Value: TColor);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).CursorColor := Value;
end;

procedure TPanelColumnsClass.SetColumnCursorText(const Index: Integer; Value: TColor);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).CursorText := Value;
end;

procedure TPanelColumnsClass.SetColumnOvercolor(const Index: Integer; Value: Boolean);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).Overcolor := Value;
end;

procedure TPanelColumnsClass.SetColumnPrm(const Index: Integer; Value: TColPrm);
begin
  if Index >= Flist.Count then
    Exit;
  SetColumnBackground(Index, Value.Background);
  SetColumnBackground2(Index, Value.Background2);
  SetColumnCursorColor(Index, Value.CursorColor);
  SetColumnCursorText(Index, Value.CursorText);
  SetColumnFontName(Index, Value.FontName);
  SetColumnFontSize(Index, Value.FontSize);
  SetColumnFontStyle(Index, Value.FontStyle);
  SetColumnMarkColor(Index, Value.MarkColor);
  SetColumnTextColor(Index, Value.TextColor);
  SetColumnOvercolor(Index, Value.Overcolor);
end;

procedure TPanelColumnsClass.SetCursorBorder(ShowBorder: Boolean);
begin
  FCursorBorder := ShowBorder;
end;

procedure TPanelColumnsClass.SetCursorBorderColor(Color: TColor);
begin
  FCursorBorderColor := Color;
end;

procedure TPanelColumnsClass.AddDefaultColumns;
var
  DCFunc: String;
begin
  SetName := 'Default';
  DCFunc := '[' + sFuncTypeDC + '().%s{}]';
  // file name
  Add(rsColName, Format(DCFunc, [TFileFunctionStrings[fsfNameNoExtension]]), 250, taLeftJustify);
  // file ext
  Add(rsColExt, Format(DCFunc, [TFileFunctionStrings[fsfExtension]]), 50, taLeftJustify);
  // file size
  Add(rsColSize, Format(DCFunc, [TFileFunctionStrings[fsfSize]]), 70, taRightJustify);
  // file date/time
  Add(rsColDate, Format(DCFunc, [TFileFunctionStrings[fsfModificationTime]]), 70, taRightJustify);
  // file attributes
  Add(rsColAttr, Format(DCFunc, [TFileFunctionStrings[fsfAttr]]), 175, taLeftJustify);
end;

procedure TPanelColumnsClass.Load(Ini: TIniFileEx; SetName: String);
var
  aCount, I: Integer;
begin
  fSetName := SetName;
  Self.Clear;
  aCount := Ini.ReadInteger(fSetName, 'ColumnCount', 0);
  //---------------------
  if aCount = 0 then
    begin
      AddDefaultColumns;
    end
  else
    for I := 0 to aCount - 1 do
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
        TPanelColumn(FList[I]).FontStyle:=TFontStyles(Ini.ReadInteger(fSetName,'Column'+IntToStr(I+1)+'FontStyle',Integer(gFontStyle)));
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
  FCustomView := Ini.ReadBool(fSetName, 'CustomView', False);
  SetCursorBorder(Ini.ReadBool(fSetName, 'CursorBorder', False));
  SetCursorBorderColor(TColor(Ini.ReadInteger(fSetName, 'CursorBorderColor', gCursorColor)));
end;

procedure TPanelColumnsClass.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  AColumn: TPanelColumn;
  SubNode: TXmlNode;
begin
  FCustomView := AConfig.GetValue(ANode, 'CustomView', False);
  SetCursorBorder(AConfig.GetAttr(ANode, 'CursorBorder/Enabled', False));
  SetCursorBorderColor(TColor(AConfig.GetValue(ANode, 'CursorBorder/Color', gCursorColor)));

  Clear;

  SubNode := ANode.FindNode('Columns');
  if Assigned(SubNode) then
  begin
    SubNode := SubNode.FirstChild;
    while Assigned(SubNode) do
    begin
      if SubNode.CompareName('Column') = 0 then
      begin
        AColumn := TPanelColumn.Create;
        FList.Add(AColumn);

        AColumn.Title := AConfig.GetValue(SubNode, 'Title', '');
        AColumn.FuncString := AConfig.GetValue(SubNode, 'FuncString', '');
        FillListFromString(AColumn.FuncList, AColumn.FuncString);
        AColumn.Width := AConfig.GetValue(SubNode, 'Width', 50);
        AColumn.Align := TAlignment(AConfig.GetValue(SubNode, 'Align', Integer(0)));
        AConfig.GetFont(SubNode, 'Font', AColumn.FontName, AColumn.FontSize, Integer(AColumn.FontStyle),
                        gFontName, gFontSize, Integer(gFontStyle));
        AColumn.Overcolor := AConfig.GetValue(SubNode, 'Overcolor', True);
        AColumn.TextColor := TColor(AConfig.GetValue(SubNode, 'TextColor', gForeColor));
        AColumn.Background := TColor(AConfig.GetValue(SubNode, 'Background', gBackColor));
        AColumn.Background2 := TColor(AConfig.GetValue(SubNode, 'Background2', gBackColor2));
        AColumn.MarkColor := TColor(AConfig.GetValue(SubNode, 'MarkColor', gMarkColor));
        AColumn.CursorColor := TColor(AConfig.GetValue(SubNode, 'CursorColor', gCursorColor));
        AColumn.CursorText := TColor(AConfig.GetValue(SubNode, 'CursorText', gCursorText));
      end;
      SubNode := SubNode.NextSibling;
    end;
  end;

  if Count = 0 then
    AddDefaultColumns;
end;

procedure TPanelColumnsClass.Save(Ini: TIniFileEx; ASetName: String);
var
  I: Integer;
begin
  fSetName := ASetName;
  if fSetName = '' then
    Exit;
  Ini.EraseSection(fSetName);
  Ini.WriteInteger(fSetName, 'ColumnCount', FList.Count);
  for I := 0 to FList.Count - 1 do
    begin
      Ini.WriteString(fSetName,'Column'+IntToStr(I+1)+'Title',TPanelColumn(FList[I]).Title);
      Ini.WriteString(fSetName,'Column'+IntToStr(I+1)+'FuncsString',TPanelColumn(FList[I]).FuncString);
      Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'Width', TPanelColumn(FList[I]).Width);
      Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'Align', Integer(TPanelColumn(FList[I]).Align));
      //---------------------
      Ini.WriteString(fSetName,'Column'+IntToStr(I+1)+'FontName',TPanelColumn(FList[I]).FontName);
      Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'FontSize', TPanelColumn(FList[I]).FontSize);
      Ini.WriteInteger(fSetName,'Column'+IntToStr(I+1)+'FontStyle', Integer(TPanelColumn(FList[I]).FontStyle));
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

  Ini.WriteBool(fSetName, 'CustomView', FCustomView);
  Ini.WriteBool(fSetName, 'CursorBorder', GetCursorBorder);
  if GetCursorBorderColor <> clNone then
    Ini.WriteInteger(fSetName, 'CursorBorderColor', GetCursorBorderColor);
end;

procedure TPanelColumnsClass.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I: Integer;
  SubNode: TXmlNode;
  AColumn: TPanelColumn;
begin
  AConfig.SetValue(ANode, 'CustomView', FCustomView);
  AConfig.SetAttr(ANode, 'CursorBorder/Enabled', GetCursorBorder);
  if GetCursorBorderColor <> clNone then
    AConfig.SetValue(ANode, 'CursorBorder/Color', GetCursorBorderColor);

  ANode := AConfig.FindNode(ANode, 'Columns', True);
  AConfig.ClearNode(ANode);

  for I := 0 to FList.Count - 1 do
    begin
      AColumn := TPanelColumn(FList[I]);
      SubNode := AConfig.AddNode(ANode, 'Column');

      AConfig.AddValue(SubNode, 'Title', AColumn.Title);
      AConfig.AddValue(SubNode, 'FuncString', AColumn.FuncString);
      AConfig.AddValue(SubNode, 'Width', AColumn.Width);
      AConfig.AddValue(SubNode, 'Align', Integer(AColumn.Align));
      AConfig.SetFont(SubNode, 'Font', AColumn.FontName,
                      AColumn.FontSize, Integer(AColumn.FontStyle));
      AConfig.AddValue(SubNode, 'Overcolor', AColumn.Overcolor);

      if AColumn.TextColor <> clNone then
        AConfig.AddValue(SubNode, 'TextColor', AColumn.TextColor);
      if AColumn.Background <> clNone then
        AConfig.AddValue(SubNode, 'Background', AColumn.Background);
      if AColumn.Background2 <> clNone then
        AConfig.AddValue(SubNode, 'Background2', AColumn.Background2);
      if AColumn.MarkColor <> clNone then
        AConfig.AddValue(SubNode, 'MarkColor', AColumn.MarkColor);
      if AColumn.CursorColor <> clNone then
        AConfig.AddValue(SubNode, 'CursorColor', AColumn.CursorColor);
      if AColumn.CursorText <> clNone then
        AConfig.AddValue(SubNode, 'CursorText', AColumn.CursorText);
    end;
end;

procedure TPanelColumnsClass.Delete(const Index: Integer);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).Free;
  FList.Delete(Index);
end;

{ TPanelColumn }

constructor TPanelColumn.Create;
begin
  FuncList := TStringList.Create;
end;

destructor TPanelColumn.Destroy;
begin
  FreeAndNil(FuncList);
  inherited Destroy;
end;

function TPanelColumn.GetColumnFunctions: TFileFunctions;
var
  FuncCount: Integer = 0;
  i: Integer;
begin
  if Assigned(FuncList) and (FuncList.Count > 0) then
  begin
    SetLength(Result, FuncList.Count); // Start with all strings.

    for i := 0 to FuncList.Count - 1 do
    begin
      // Don't need to compare simple text, only functions.
      if PtrInt(FuncList.Objects[i]) = 1 then
        begin
          Result[FuncCount] := GetFileFunctionByName(FuncList.Strings[i]);

          // If the function was found, save it's number.
          if Result[FuncCount] <> fsfInvalid then
            FuncCount := FuncCount + 1;
        end;
    end;

    SetLength(Result, FuncCount); // Set the actual functions count.
  end
  else
    SetLength(Result, 0);
end;

function TPanelColumn.GetColumnResultString(AFile: TFile;
  const AFileSource: IFileSource): String;
var
  i: Integer;
  s: String;
begin
  s      := '';
  Result := '';
  if (not Assigned(FuncList)) or (FuncList.Count = 0) then
    Exit;
  for i := 0 to FuncList.Count - 1 do
    begin
      //Item is simpletext
      if PtrInt(FuncList.Objects[i]) = 0 then
        s := s + FuncList[I]
      else
      //Item is function
        begin
          s := s + FormatFileFunction(FuncList[I], AFile, AFileSource);
        end;
    end;
  Result := s;
end;

{ TPanelColumnsList }

function TPanelColumnsList.GetCount: Integer;
begin
  Result := fSet.Count;
end;

constructor TPanelColumnsList.Create;
begin
  FSet := TStringList.Create;
end;

destructor TPanelColumnsList.Destroy;
var
  i: Integer;
begin
  if Assigned(FSet) then
  begin
    for i := 0 to Fset.Count - 1 do
      TPanelColumnsClass(Fset.Objects[i]).Free;
    FreeAndNil(FSet);
  end;

  inherited Destroy;
end;

procedure TPanelColumnsList.Clear;
var
  i: Integer;
begin
  for i := 0 to Fset.Count - 1 do
    TPanelColumnsClass(Fset.Objects[i]).Free;
  Fset.Clear;
end;

procedure TPanelColumnsList.Load(Ini: TIniFileEx);
var
  aCount, I: Integer;
begin
  Self.Clear;
  aCount := Ini.ReadInteger('ColumnsSet', 'ColumnsSetCount', 0);
  for I := 0 to aCount - 1 do
    begin
      fSet.AddObject(Ini.ReadString('ColumnsSet', 'ColumnsSet' +
        IntToStr(I + 1) + 'Name', ''), TPanelColumnsClass.Create);
      TPanelColumnsClass(fSet.Objects[I]).Load(ini, fset[i]);
      DebugLn('FsetName=' + Fset[i]);
    end;
  DebugLn('FsetCount=' + IntToStr(fset.Count));
end;

procedure TPanelColumnsList.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  AName: String;
  AnObject: TPanelColumnsClass;
begin
  Clear;

  ANode := ANode.FindNode('ColumnsSets');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('ColumnsSet') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', AName) then
        begin
          AnObject := TPanelColumnsClass.Create;
          fSet.AddObject(AName, AnObject);
          AnObject.Name := AName;
          AnObject.Load(AConfig, ANode);
        end
        else
          DebugLn('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TPanelColumnsList.Save(Ini: TIniFileEx);
var
  I: Integer;
begin
  Ini.EraseSection('ColumnsSet');
  Ini.WriteInteger('ColumnsSet', 'ColumnsSetCount', FSet.Count);
  for I := 0 to FSet.Count - 1 do
    begin
      Ini.WriteString('ColumnsSet', 'ColumnsSet' + IntToStr(I + 1) + 'Name', FSet[i]);
      TPanelColumnsClass(Fset.Objects[i]).Save(ini, FSet[i]);
    end;
end;

procedure TPanelColumnsList.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I:       Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, 'ColumnsSets', True);
  AConfig.ClearNode(ANode);

  for I := 0 to FSet.Count - 1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'ColumnsSet');
      AConfig.AddValue(SubNode, 'Name', FSet[I]);
      TPanelColumnsClass(Fset.Objects[I]).Save(AConfig, SubNode);
    end;
end;

function TPanelColumnsList.Add(AName: String; Item: TPanelColumnsClass): Integer;
begin
  Result := Fset.AddObject(AName, Item);
end;

procedure TPanelColumnsList.Insert(AIndex: Integer; AName: String;
  Item: TPanelColumnsClass);
begin
  Fset.InsertObject(AIndex, AName, Item);
end;

procedure TPanelColumnsList.DeleteColumnSet(SetName: String);
begin
  DeleteColumnSet(fSet.IndexOf(SetName));
end;

procedure TPanelColumnsList.DeleteColumnSet(SetIndex: Integer);
begin
  if (SetIndex >= Fset.Count) or (SetIndex < 0) then
    Exit;
  TPanelColumnsClass(fSet.Objects[SetIndex]).Free;
  fSet.Delete(SetIndex);
end;

procedure TPanelColumnsList.CopyColumnSet(SetName, NewSetName: String);
var
  OldSetIndex, NewSetIndex: Integer;
  OldSet, NewSet: TPanelColumnsClass;
begin
  OldSetIndex := fSet.IndexOf(SetName);
  if OldSetIndex <> -1 then
    begin
      OldSet := TPanelColumnsClass(fSet.Objects[OldSetIndex]);
      NewSetIndex := fSet.IndexOf(NewSetName);
      if NewSetIndex <> -1 then
        NewSet := TPanelColumnsClass(fSet.Objects[NewSetIndex])
      else
        begin
          NewSet := TPanelColumnsClass.Create;
          fSet.AddObject(NewSetName, NewSet);
        end;

      NewSet.Assign(OldSet);
      // Set new name.
      NewSet.Name := NewSetName;
    end;
end;

function TPanelColumnsList.GetColumnSet(const Index: Integer): TPanelColumnsClass;
begin
  //DebugLn('FsetCount='+inttostr(fset.Count));
  if (Index > -1) and (Index < Fset.Count) then
    Result := TPanelColumnsClass(Fset.Objects[Index])
  else
  begin
    if fset.Count = 0 then
    begin
      Fset.AddObject('Default', TPanelColumnsClass.Create);
      TPanelColumnsClass(Fset.Objects[0]).AddDefaultColumns;
    end;
    Result := TPanelColumnsClass(Fset.Objects[0]);
  end;
end;

function TPanelColumnsList.GetColumnSet(Setname: String): TPanelColumnsClass;
begin
  //DebugLn('FsetCount='+inttostr(fset.Count));
  if fset.IndexOf(Setname) > -1 then
    Result := TPanelColumnsClass(Fset.Objects[fset.IndexOf(Setname)])
  else
  begin
    if fset.Count = 0 then
    begin
      Fset.AddObject('Default', TPanelColumnsClass.Create);
      TPanelColumnsClass(Fset.Objects[0]).AddDefaultColumns;
    end;
    Result := TPanelColumnsClass(Fset.Objects[0]);
  end;
end;


{ TColPrm }

constructor TColPrm.Create;
begin
  Self.Overcolor   := True;
  Self.FontName    := gFontName;
  Self.FontSize    := gFontSize;
  Self.TextColor   := gForeColor;
  Self.Background  := gBackColor;
  Self.Background2 := gBackColor2;
  Self.MarkColor   := gMarkColor;
  Self.CursorColor := gCursorColor;
  Self.CursorText  := gCursorText;
end;

end.

