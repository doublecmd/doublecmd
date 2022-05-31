{
   Double Commander
   -------------------------------------------------------------------------
   Filepanel columns implementation unit

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Copyright (C) 2015  Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}


unit uColumns;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCClassesUtf8, Graphics, uFile, uFileSource,
  DCXmlConfig, DCBasicTypes, uFileFunctions;

const
  FS_GENERAL = '<General>';

type

  { TColPrm }

  TColPrm = class
    FontName: String;
    FontSize: Integer;
    FontStyle: TFontStyles;
    TextColor,
    Background,
    Background2,
    MarkColor,
    CursorColor,
    CursorText,
    InactiveCursorColor,
    InactiveMarkColor: TColor;
    UseInvertedSelection: Boolean;
    UseInactiveSelColor: Boolean;
    Overcolor: Boolean;
  public
    constructor Create;
  end;

  { TPanelColumnsType }

  TPanelColumn = class
  private
    FFuncString: String;

    procedure SetFuncString(NewValue: String);

    function GetColumnResultString(AFile: TFile; const AFileSource: IFileSource): String;

  public
    //---------------------
    Title: String;

    {String is function or simpletext;
    TObject(integer)=indicator of function: 0 is simpletext; 1 is function;}
    FuncList: TStringList;
    Width: Integer;
    Align: TAlignment;
    //---------------------
    FontName: String;
    FontSize: Integer;
    FontStyle: TFontStyles;
    TextColor,
    Background,
    Background2,
    MarkColor,
    CursorColor,
    CursorText,
    InactiveCursorColor,
    InactiveMarkColor: TColor;
    BorderFrameWidth :integer;
    UseInvertedSelection: Boolean;
    UseInactiveSelColor: Boolean;
    Overcolor: Boolean;
    //---------------------

    constructor Create;
    destructor Destroy; override;

    //------------------------------------------------------
    property FuncString: String read FFuncString write SetFuncString;
  end;

  { TPanelColumnsClass }

  TPanelColumnsClass = class
    //------------------------------------------------------
  private
    FList: TList;
    fSetName: String;

    // Global settings for columns view.
    FFileSystem: String;
    FCustomView: Boolean;
    FCursorBorder: Boolean;
    FCursorBorderColor: TColor;
    FUseFrameCursor: Boolean;
    //------------------------------------------------------
    function GetCursorBorder: boolean;
    function GetCursorBorderColor: TColor;
    function GetUseFrameCursor: Boolean;
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
    function GetColumnFontQuality(const Index: Integer): TFontQuality;
    function GetColumnTextColor(const Index: Integer): TColor;
    function GetColumnBackground(const Index: Integer): TColor;
    function GetColumnBackground2(const Index: Integer): TColor;
    function GetColumnMarkColor(const Index: Integer): TColor;
    function GetColumnCursorColor(const Index: Integer): TColor;
    function GetColumnCursorText(const Index: Integer): TColor;
    function GetColumnInactiveCursorColor(const Index: Integer): TColor;
    function GetColumnInactiveMarkColor(const Index: Integer): TColor;
    function GetColumnUseInvertedSelection(const Index: Integer): Boolean;
    function GetColumnUseInactiveSelColor(const Index: Integer): Boolean;
    function GetColumnOvercolor(const Index: Integer): Boolean;
    function GetColumnBorderFrameWidth(const Index: Integer):integer;

    //---------------------
    function GetColumnPrm(const Index: Integer): TColPrm;
    //--------------------------------------------------------------------------
    function GetColumnsVariants: TDynamicStringArray;
    {en
       Converts string functions in the column into their integer values,
       so that they don't have to be compared by string during sorting.
       Call this before sorting then pass result to Compare in the sorting loop.
    }
    function GetColumnFunctions(const Index: Integer): TFileFunctions;
    function GetColumnItemResultString(const Index: Integer;
      const AFile: TFile; const AFileSource: IFileSource): String;
    //--------------------------------------------------------------------------
    function GetColumnItem(const Index: Integer): TPanelColumn;
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
    procedure SetColumnInactiveCursorColor(const Index: Integer; Value: TColor);
    procedure SetColumnInactiveMarkColor(const Index: Integer; Value: TColor);
    procedure SetColumnUseInvertedSelection(const Index: Integer; Value: Boolean);
    procedure SetColumnUseInactiveSelColor(const Index: Integer; Value: Boolean);
    procedure SetColumnOvercolor(const Index: Integer; Value: Boolean);
    //---------------------
    procedure SetColumnPrm(const Index: Integer; Value: TColPrm);
    //---------------------
    procedure Delete(const Index: Integer);
    procedure Clear;
    procedure AddDefaultColumns;
    procedure AddDefaultEverything;
    //---------------------
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    //---------------------
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    //---------------------
    function GetSignature(Seed:dword=$000000):dword;
    property ColumnsCount: Integer read GetCount;
    property Count: Integer read GetCount;
    property CustomView: Boolean read FCustomView write FCustomView;
    property CurrentColumnsSetName: String read fSetName write fSetName;
    property SetName: String read fSetName write fSetName;
    property Name: String read fSetName write fSetName;
    property FileSystem: String read FFileSystem write FFileSystem;
    property UseCursorBorder: boolean read GetCursorBorder write FCursorBorder;
    property CursorBorderColor: TColor read GetCursorBorderColor write FCursorBorderColor;
    property UseFrameCursor: boolean read GetUseFrameCursor write FUseFrameCursor;
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
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    function Add(Item: TPanelColumnsClass): Integer;
    procedure Insert(AIndex: Integer; Item: TPanelColumnsClass);
    procedure DeleteColumnSet(SetName: String);
    procedure DeleteColumnSet(SetIndex: Integer); overload;
    procedure CopyColumnSet(SetName, NewSetName: String);
    function GetColumnSet(const Index: Integer): TPanelColumnsClass;
    function GetColumnSet(Setname: String): TPanelColumnsClass;
    function GetColumnSet(const AName, FileSystem: String): TPanelColumnsClass;
    //---------------------
    property Items: TStringList read fSet;
    property Count: Integer read GetCount;
  end;

  function StrToAlign(str: String): TAlignment;

implementation

uses
  LCLType, Forms, crc, DCStrUtils, uDebug, uLng, uGlobs;

var
  DefaultTitleHash: LongWord = 0;

procedure UpdateDefaultTitleHash;
var
  Title: String = '';
begin
  DefaultTitleHash:= CRC32(0, nil, 0);
  Title:= rsColName + rsColExt + rsColSize + rsColDate + rsColAttr;
  DefaultTitleHash:= CRC32(DefaultTitleHash, Pointer(Title), Length(Title));
end;

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
    Result := gFonts[dcfMain].Name;
end;

function TPanelColumnsClass.GetColumnFontSize(const Index: Integer): Integer;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).FontSize
  else
    Result := gFonts[dcfMain].Size;
end;

function TPanelColumnsClass.GetColumnFontStyle(const Index: Integer): TFontStyles;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).FontStyle
  else
    Result := gFonts[dcfMain].Style;
end;

function TPanelColumnsClass.GetColumnFontQuality(const Index: Integer): TFontQuality;
begin
  Result := gFonts[dcfMain].Quality;
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

function TPanelColumnsClass.GetColumnInactiveCursorColor(const Index: Integer): TColor;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).InactiveCursorColor
  else
    Result := gInactiveCursorColor;
end;

function TPanelColumnsClass.GetColumnInactiveMarkColor(const Index: Integer): TColor;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).InactiveMarkColor
  else
    Result := gInactiveMarkColor;
end;

function TPanelColumnsClass.GetColumnUseInvertedSelection(const Index: Integer): Boolean;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).UseInvertedSelection
  else
    Result := gUseInvertedSelection;
end;

function TPanelColumnsClass.GetColumnUseInactiveSelColor(const Index: Integer): Boolean;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).UseInactiveSelColor
  else
    Result := gUseInactiveSelColor;
end;

function TPanelColumnsClass.GetColumnOvercolor(const Index: Integer): Boolean;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).Overcolor
  else
    Result := gAllowOverColor;
end;

function TPanelColumnsClass.GetColumnBorderFrameWidth(const Index: Integer): integer;
begin
  if FCustomView and (Index < Flist.Count) then
    Result := TPanelColumn(Flist[Index]).BorderFrameWidth
  else
    Result := gBorderFrameWidth;
end;

function TPanelColumnsClass.GetColumnPrm(const Index: Integer): TColPrm;
begin
  if Index >= Flist.Count then
    Exit(nil);

  Result := TColPrm.Create;
  Result.FontName := GetColumnFontName(Index);
  Result.FontSize := GetColumnFontSize(Index);
  Result.FontStyle := GetColumnFontStyle(Index);
  Result.TextColor := GetColumnTextColor(Index);
  Result.Background := GetColumnBackground(Index);
  Result.Background2 := GetColumnBackground2(Index);
  Result.MarkColor := GetColumnMarkColor(Index);
  Result.CursorColor := GetColumnCursorColor(Index);
  Result.CursorText := GetColumnCursorText(Index);
  Result.InactiveCursorColor := GetColumnInactiveCursorColor(Index);
  Result.InactiveMarkColor := GetColumnMarkColor(Index);
  Result.UseInvertedSelection:= GetColumnUseInvertedSelection(Index);
  Result.UseInactiveSelColor:= GetColumnUseInactiveSelColor(Index);
  Result.Overcolor := GetColumnOvercolor(Index);
end;

function TPanelColumnsClass.GetColumnsVariants: TDynamicStringArray;
var
  I, J: Integer;
begin
  for J:= 0 to Flist.Count - 1 do
  begin
    with TPanelColumn(Flist[J]) do
    begin
      if Assigned(FuncList) and (FuncList.Count > 0) then
      begin
        for I := 0 to FuncList.Count - 1 do
        begin
          // Don't need to compare simple text, only functions.
          if PtrInt(FuncList.Objects[I]) = 1 then
          begin
            if GetFileFunctionByName(FuncList.Strings[I]) = fsfVariant then
              AddString(Result, FuncList.Strings[I]);
          end;
        end;
      end;
    end;
  end;
end;

function TPanelColumnsClass.GetColumnItem(const Index: Integer): TPanelColumn;
begin
  if Index >= Flist.Count then
    Exit(nil);
  Result := TPanelColumn(Flist[Index]);
end;

function TPanelColumnsClass.GetColumnFunctions(const Index: Integer): TFileFunctions;
var
  FuncCount: Integer = 0;
  i, J: Integer;
  Value: TFileFunction;
  VariantIndex: Integer = 0;
begin
  for J:= 0 to Index do
  with TPanelColumn(Flist[J]) do
  begin
    if Assigned(FuncList) and (FuncList.Count > 0) then
    begin
      SetLength(Result, FuncList.Count); // Start with all strings.

      for i := 0 to FuncList.Count - 1 do
      begin
        // Don't need to compare simple text, only functions.
        if PtrInt(FuncList.Objects[i]) = 1 then
          begin
            Value := GetFileFunctionByName(FuncList.Strings[i]);

            if Value = fsfVariant then
            begin
              Value := TFileFunction(Ord(fsfVariant) + VariantIndex);
              Inc(VariantIndex);
            end;

            if (J = Index) then
            begin
              Result[FuncCount] := Value;

              // If the function was found, save it's number.
              if Result[FuncCount] <> fsfInvalid then
                FuncCount := FuncCount + 1;
            end;
          end;
      end;

      SetLength(Result, FuncCount); // Set the actual functions count.
    end
    else
      SetLength(Result, 0);
  end;
end;

function TPanelColumnsClass.GetColumnItemResultString(const Index: Integer;
  const AFile: TFile; const AFileSource: IFileSource): String;
begin
  if Index >= Flist.Count then
    Exit(EmptyStr);
  Result := TPanelColumn(Flist[Index]).GetColumnResultString(AFile, AFileSource);
end;

function TPanelColumnsClass.GetUseFrameCursor: Boolean;
begin
  if FCustomView then
    Result := FUseFrameCursor
  else
    Result := gUseFrameCursor;
end;

function TPanelColumnsClass.GetCursorBorder: boolean;
begin
  if FCustomView then
    Result := FCursorBorder
  else
    Result := gUseCursorBorder;
end;

function TPanelColumnsClass.GetCursorBorderColor: TColor;
begin
  if FCustomView then
    Result := FCursorBorderColor
  else
    Result := gCursorBorderColor;
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
  FFileSystem := OtherColumnsClass.FFileSystem;
  FCustomView := OtherColumnsClass.FCustomView;
  FCursorBorder := OtherColumnsClass.FCursorBorder;
  FCursorBorderColor := OtherColumnsClass.FCursorBorderColor;
  FUseFrameCursor := OtherColumnsClass.FUseFrameCursor;

  for i := 0 to OtherColumnsClass.ColumnsCount - 1 do
  begin
    OldColumn := OtherColumnsClass.GetColumnItem(i);
    NewColumn := TPanelColumn.Create;
    Add(NewColumn);

    NewColumn.Title       := OldColumn.Title;
    NewColumn.FuncString  := OldColumn.FuncString;
    NewColumn.Width       := OldColumn.Width;
    NewColumn.Align       := OldColumn.Align;
    NewColumn.FontName    := OldColumn.FontName;
    NewColumn.FontSize    := OldColumn.FontSize;
    NewColumn.FontStyle   := OldColumn.FontStyle;
    NewColumn.TextColor   := OldColumn.TextColor;
    NewColumn.Background  := OldColumn.Background;
    NewColumn.Background2 := OldColumn.Background2;
    NewColumn.MarkColor   := OldColumn.MarkColor;
    NewColumn.CursorColor := OldColumn.CursorColor;
    NewColumn.CursorText  := OldColumn.CursorText;
    NewColumn.InactiveCursorColor := OldColumn.InactiveCursorColor;
    NewColumn.InactiveMarkColor := OldColumn.InactiveMarkColor;
    NewColumn.UseInvertedSelection := OldColumn.UseInvertedSelection;
    NewColumn.UseInactiveSelColor := OldColumn.UseInactiveSelColor;
    NewColumn.Overcolor := OldColumn.Overcolor;
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
  AColumn.Width       := Width;
  AColumn.Align       := Align;
  AColumn.FontName    := gFonts[dcfMain].Name;
  AColumn.FontSize    := gFonts[dcfMain].Size;
  AColumn.FontStyle   := gFonts[dcfMain].Style;
  AColumn.TextColor   := gForeColor;
  AColumn.Background  := gBackColor;
  AColumn.Background2 := gBackColor2;
  AColumn.MarkColor   := gMarkColor;
  AColumn.CursorColor := gCursorColor;
  AColumn.CursorText  := gCursorText;
  AColumn.InactiveCursorColor := gInactiveCursorColor;
  AColumn.InactiveMarkColor := gInactiveMarkColor;
  AColumn.UseInvertedSelection := gUseInvertedSelection;
  AColumn.UseInactiveSelColor := gUseInactiveSelColor;
  AColumn.Overcolor := gAllowOverColor;
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

procedure TPanelColumnsClass.SetColumnInactiveCursorColor(const Index: Integer; Value: TColor);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).InactiveCursorColor := Value;
end;

procedure TPanelColumnsClass.SetColumnInactiveMarkColor(const Index: Integer; Value: TColor);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).InactiveMarkColor := Value;
end;

procedure TPanelColumnsClass.SetColumnUseInvertedSelection(const Index: Integer; Value: Boolean);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).UseInvertedSelection := Value;
end;

procedure TPanelColumnsClass.SetColumnUseInactiveSelColor(const Index: Integer; Value: Boolean);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).UseInactiveSelColor := Value;
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
  SetColumnFontName(Index, Value.FontName);
  SetColumnFontSize(Index, Value.FontSize);
  SetColumnFontStyle(Index, Value.FontStyle);
  SetColumnTextColor(Index, Value.TextColor);
  SetColumnBackground(Index, Value.Background);
  SetColumnBackground2(Index, Value.Background2);
  SetColumnMarkColor(Index, Value.MarkColor);
  SetColumnCursorColor(Index, Value.CursorColor);
  SetColumnCursorText(Index, Value.CursorText);
  SetColumnInactiveCursorColor(Index, Value.InactiveCursorColor);
  SetColumnInactiveMarkColor(Index, Value.InactiveMarkColor);
  SetColumnUseInvertedSelection(Index, Value.UseInvertedSelection);
  SetColumnUseInactiveSelColor(Index, Value.UseInactiveSelColor);
  SetColumnOvercolor(Index, Value.Overcolor);
end;

procedure TPanelColumnsClass.AddDefaultColumns;
var
  DCFunc: String;
begin
  SetName := 'Default';
  FFileSystem := FS_GENERAL;
  DCFunc := '[' + sFuncTypeDC + '().%s{}]';
  // file name
  Add(rsColName, Format(DCFunc, [TFileFunctionStrings[fsfNameNoExtension]]), 250, taLeftJustify);
  // file ext
  Add(rsColExt, Format(DCFunc, [TFileFunctionStrings[fsfExtension]]), 50, taLeftJustify);
  // file size
  Add(rsColSize, Format(DCFunc, [TFileFunctionStrings[fsfSize]]), 70, taRightJustify);
  // file date/time
  Add(rsColDate, Format(DCFunc, [TFileFunctionStrings[fsfModificationTime]]), 140, taRightJustify);
  // file attributes
  Add(rsColAttr, Format(DCFunc, [TFileFunctionStrings[fsfAttr]]), 100, taLeftJustify);
  // Default title hash
  UpdateDefaultTitleHash;
end;

procedure TPanelColumnsClass.AddDefaultEverything;
begin
  AddDefaultColumns;
  FCustomView := False;
  FCursorBorder := gUseCursorBorder;
  FCursorBorderColor := gCursorBorderColor;
  FUseFrameCursor := gUseFrameCursor;
end;

procedure TPanelColumnsClass.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  Title: String;
  Hash: LongWord;
  SubNode: TXmlNode;
  Quality: Integer = 0;
  AColumn: TPanelColumn;
  APixelsPerInch: Integer;
begin
  FCustomView := AConfig.GetValue(ANode, 'CustomView', False);
  FFileSystem := AConfig.GetValue(ANode, 'FileSystem', FS_GENERAL);
  APixelsPerInch:= AConfig.GetValue(ANode, 'PixelsPerInch', Screen.PixelsPerInch);
  FCursorBorder := AConfig.GetAttr(ANode, 'CursorBorder/Enabled', gUseCursorBorder);
  FCursorBorderColor := TColor(AConfig.GetValue(ANode, 'CursorBorder/Color', gCursorBorderColor));
  FUseFrameCursor := AConfig.GetAttr(ANode, 'UseFrameCursor', gUseFrameCursor);

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
        AColumn.Width := AConfig.GetValue(SubNode, 'Width', 50);
        AColumn.Width := MulDiv(AColumn.Width, Screen.PixelsPerInch, APixelsPerInch);
        AColumn.Align := TAlignment(AConfig.GetValue(SubNode, 'Align', Integer(0)));
        AConfig.GetFont(SubNode, 'Font', AColumn.FontName, AColumn.FontSize, Integer(AColumn.FontStyle), Quality,
                        gFonts[dcfMain].Name, gFonts[dcfMain].Size, Integer(gFonts[dcfMain].Style), Quality);
        AColumn.TextColor := TColor(AConfig.GetValue(SubNode, 'TextColor', gForeColor));
        AColumn.Background := TColor(AConfig.GetValue(SubNode, 'Background', gBackColor));
        AColumn.Background2 := TColor(AConfig.GetValue(SubNode, 'Background2', gBackColor2));
        AColumn.MarkColor := TColor(AConfig.GetValue(SubNode, 'MarkColor', gMarkColor));
        AColumn.CursorColor := TColor(AConfig.GetValue(SubNode, 'CursorColor', gCursorColor));
        AColumn.CursorText := TColor(AConfig.GetValue(SubNode, 'CursorText', gCursorText));
        AColumn.InactiveCursorColor := TColor(AConfig.GetValue(SubNode, 'InactiveCursorColor', gInactiveCursorColor));
        AColumn.InactiveMarkColor := TColor(AConfig.GetValue(SubNode, 'InactiveMarkColor', gInactiveMarkColor));
        AColumn.UseInvertedSelection := AConfig.GetValue(SubNode, 'UseInvertedSelection', gUseInvertedSelection);
        AColumn.UseInactiveSelColor := AConfig.GetValue(SubNode, 'UseInactiveSelColor', gUseInactiveSelColor);
        AColumn.Overcolor := AConfig.GetValue(SubNode, 'Overcolor', True);
      end;
      SubNode := SubNode.NextSibling;
    end;
  end;

  if Count = 0 then
    AddDefaultColumns
  else begin
    Title:= EmptyStr;
    for Quality:= 0 to Count - 1 do
    begin
      Title += TPanelColumn(Flist[Quality]).Title;
    end;
    Hash:= CRC32(0, nil, 0);
    Hash:= CRC32(Hash, Pointer(Title), Length(Title));
    if Hash = DefaultTitleHash then
    begin
      SetColumnTitle(0, rsColName);
      SetColumnTitle(1, rsColExt);
      SetColumnTitle(2, rsColSize);
      SetColumnTitle(3, rsColDate);
      SetColumnTitle(4, rsColAttr);
      // Default title hash
      UpdateDefaultTitleHash;
    end;
  end;
end;

procedure TPanelColumnsClass.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I: Integer;
  SubNode: TXmlNode;
  AColumn: TPanelColumn;
begin
  AConfig.SetValue(ANode, 'CustomView', FCustomView);
  AConfig.SetValue(ANode, 'FileSystem', FFileSystem);
  AConfig.SetValue(ANode, 'PixelsPerInch', Screen.PixelsPerInch);
  AConfig.SetAttr(ANode, 'CursorBorder/Enabled', FCursorBorder);
  if FCursorBorderColor <> clNone then
    AConfig.SetValue(ANode, 'CursorBorder/Color', FCursorBorderColor);
  AConfig.SetAttr(ANode, 'UseFrameCursor', FUseFrameCursor);

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
                      AColumn.FontSize, Integer(AColumn.FontStyle), 0);

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
      if AColumn.InactiveCursorColor <> clNone then
        AConfig.AddValue(SubNode, 'InactiveCursorColor', AColumn.InactiveCursorColor);
      if AColumn.InactiveMarkColor <> clNone then
        AConfig.AddValue(SubNode, 'InactiveMarkColor', AColumn.InactiveMarkColor);

      AConfig.AddValue(SubNode, 'UseInvertedSelection', AColumn.UseInvertedSelection);
      AConfig.AddValue(SubNode, 'UseInactiveSelColor', AColumn.UseInactiveSelColor);
      AConfig.AddValue(SubNode, 'Overcolor', AColumn.Overcolor);
    end;
end;

procedure TPanelColumnsClass.Delete(const Index: Integer);
begin
  if Index > Flist.Count then
    Exit;
  TPanelColumn(Flist[Index]).Free;
  FList.Delete(Index);
end;

function TPanelColumnsClass.GetSignature(Seed:dword=$000000):dword;
  procedure ProgressSignatureWithThisString(sSomething:string);
  begin
    if length(sSomething) > 0 then
      Result := crc32(Result, @sSomething[1], length(sSomething));
  end;

var
  iPanelColumnIndex: integer;
  iFunction: integer;
begin
  result:=Seed;

  for iPanelColumnIndex := 0 to pred(Count) do
  begin
    with TPanelColumn(Flist[iPanelColumnIndex]) do
    begin
      ProgressSignatureWithThisString(Title);
      for iFunction:=0 to pred(FuncList.Count) do ProgressSignatureWithThisString(FuncList.Strings[iFunction]);
      Result := crc32(Result, @Width, sizeof(Width));
      Result := crc32(Result, @Align, sizeof(Align));
      if FCustomView then
      begin
        ProgressSignatureWithThisString(FontName);
        Result := crc32(Result, @FontSize, sizeof(FontSize));
        Result := crc32(Result, @FontStyle, sizeof(FontStyle));
        Result := crc32(Result, @TextColor, sizeof(TextColor));
        Result := crc32(Result, @Background, sizeof(Background));
        Result := crc32(Result, @Background2, sizeof(Background2));
        Result := crc32(Result, @MarkColor, sizeof(MarkColor));
        Result := crc32(Result, @CursorColor, sizeof(CursorColor));
        Result := crc32(Result, @CursorText, sizeof(CursorText));
        Result := crc32(Result, @InactiveCursorColor, sizeof(InactiveCursorColor));
        Result := crc32(Result, @InactiveMarkColor, sizeof(InactiveMarkColor));
        Result := crc32(Result, @UseInvertedSelection, sizeof(UseInvertedSelection));
        Result := crc32(Result, @UseInactiveSelColor, sizeof(UseInactiveSelColor));
        Result := crc32(Result, @Overcolor, sizeof(Overcolor));
      end;
    end;
  end;

  ProgressSignatureWithThisString(fSetName);
  Result := crc32(Result, @FCustomView, sizeof(FCustomView));
  Result := crc32(Result, @FCursorBorder, sizeof(FCursorBorder));
  Result := crc32(Result, @FCursorBorderColor, sizeof(FCursorBorderColor));
  Result := crc32(Result, @FUseFrameCursor, sizeof(FUseFrameCursor));
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


function TPanelColumn.GetColumnResultString(AFile: TFile; const AFileSource: IFileSource): String;
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

procedure TPanelColumn.SetFuncString(NewValue: String);
  procedure FillListFromString(List: TStrings; FuncS: String);
  var
    p: Integer;
  begin
    while True do
    begin
      p := pos('[', FuncS);
      if p = 0 then
        Break
      else if p > 1 then
        List.AddObject(Copy(FuncS, 1, p - 1), TObject(0));
      Delete(FuncS, 1, p);

      p := pos(']', FuncS);
      if p = 0 then
        Break
      else if p > 1 then
        List.AddObject(Copy(FuncS, 1, p - 1), TObject(1));
      Delete(FuncS, 1, p);
    end;

    if FuncS <> '' then
      List.AddObject(FuncS, TObject(0));
  end;

begin
  FFuncString := NewValue;
  FillListFromString(FuncList, NewValue);
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

procedure TPanelColumnsList.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  AName: String;
  AnObject: TPanelColumnsClass;
begin
  Clear;

  ANode := ANode.FindNode('ColumnsSets');
  if Assigned(ANode) then
  begin
    DefaultTitleHash := AConfig.GetAttr(ANode, 'DefaultTitleHash', Int64(0));
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
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TPanelColumnsList.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I:       Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, 'ColumnsSets', True);
  AConfig.ClearNode(ANode);
  AConfig.SetAttr(ANode, 'DefaultTitleHash', Int64(DefaultTitleHash));
  for I := 0 to FSet.Count - 1 do
  begin
    SubNode := AConfig.AddNode(ANode, 'ColumnsSet');
    AConfig.AddValue(SubNode, 'Name', FSet[I]);
    TPanelColumnsClass(Fset.Objects[I]).Save(AConfig, SubNode);
  end;
end;

function TPanelColumnsList.Add(Item: TPanelColumnsClass): Integer;
begin
  Result := Fset.AddObject(Item.Name, Item);
end;

procedure TPanelColumnsList.Insert(AIndex: Integer; Item: TPanelColumnsClass);
begin
  Fset.InsertObject(AIndex, Item.Name, Item);
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
  //DCDebug('FsetCount='+inttostr(fset.Count));
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
var
  Index: Integer;
begin
  Index:= fset.IndexOf(Setname);
  if Index > -1 then
    Result := TPanelColumnsClass(Fset.Objects[Index])
  else begin
    if fset.Count = 0 then
    begin
      Fset.AddObject('Default', TPanelColumnsClass.Create);
      TPanelColumnsClass(Fset.Objects[0]).AddDefaultColumns;
    end;
    Result := TPanelColumnsClass(Fset.Objects[0]);
  end;
end;

function TPanelColumnsList.GetColumnSet(const AName, FileSystem: String): TPanelColumnsClass;
var
  Index: Integer;
begin
  if (FileSystem = EmptyStr) or SameText(FileSystem, FS_GENERAL) then
    Result:= GetColumnSet(AName)
  else begin
    for Index:= 0 to Fset.Count - 1 do
    begin
      if SameText(AName, fset[Index]) and SameText(FileSystem, TPanelColumnsClass(Fset.Objects[Index]).FileSystem) then
      begin
        Exit(TPanelColumnsClass(Fset.Objects[Index]));
      end;
    end;
    Result:= nil;
  end;
end;

{ TColPrm }

constructor TColPrm.Create;
begin
  Self.FontName    := gFonts[dcfMain].Name;
  Self.FontSize    := gFonts[dcfMain].Size;
  Self.FontStyle   := gFonts[dcfMain].Style;
  Self.TextColor   := gForeColor;
  Self.Background  := gBackColor;
  Self.Background2 := gBackColor2;
  Self.MarkColor   := gMarkColor;
  Self.CursorColor := gCursorColor;
  Self.CursorText  := gCursorText;
  Self.InactiveCursorColor := gInactiveCursorColor;
  Self.InactiveMarkColor   := gInactiveMarkColor;
  Self.UseInvertedSelection:= gUseInvertedSelection;
  Self.UseInactiveSelColor:= gUseInactiveSelColor;
  Self.Overcolor   := gAllowOverColor;
end;

end.

