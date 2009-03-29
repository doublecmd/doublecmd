(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbView.pas 3.05                             *}
{*********************************************************}
{* ABBREVIA: Base archive viewer component (VCL)         *}
{*   See AbQView.pas for the CLX header                  *}
{*********************************************************}

unit AbView;


{$I AbDefine.inc}

interface

uses
  SysUtils,
  Classes,
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
  ShellApi,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
  Types,
{$ENDIF}
{$IFDEF UsingCLX }
  Qt,
  QControls,
  QGraphics,
  QForms,
  QGrids,
{$ELSE}
  Controls,
  Graphics,
  Forms,
  Grids,
{$ENDIF}
  AbExcept,
  AbUtils,
  AbConst,
  AbArcTyp,
  AbZipTyp;

type
  TAbViewAttribute =
    (vaItemName, vaPacked, vaMethod, vaRatio, vaCRC,
     vaFileAttributes, vaFileType, vaEncryption, vaTimeStamp,
     vaFileSize, vaVersionMade, vaVersionNeeded, vaPath);
  TAbViewAttributes = set of TAbViewAttribute;

  TAbDisplayOption =
    (doAlternateColors, doColLines, doColMove, doColSizing, doMultiSelect,
     doRowLines, doShowIcons, doThumbTrack, doTrackActiveRow);
  TAbDisplayOptions = set of TAbDisplayOption;

  TAbSortAttribute =
    (saItemName, saPacked,  saPath, saRatio, saTimeStamp, saFileSize);
  TAbSortAttributes = set of TAbSortAttribute;

const
  AbDefColWidth = 150;
  AbDefRowHeight = 24;
  AbHeaderRow = 0;
  AbDefSelColor = clHighlight;
  AbDefSelTextColor = clHighlightText;
  AbDefHighColor = clAqua;
  AbDefHighTextColor = clRed;
  AbDefDelColor = clYellow;
  AbDefDelTextColor = clNavy;


{ ===== TAbColors ========================================================== }
type
  TAbColors = class(TPersistent)
  protected {private}
    FSelected : TColor;
    FSelectedText : TColor;
    FAlternate : TColor;
    FAlternateText : TColor;
    FDeleted : TColor;
    FDeletedText : TColor;
    FUpdating  : Boolean;
    FOnChange  : TNotifyEvent;
    procedure DoOnChange;
    procedure SetSelected(Value : TColor);
    procedure SetSelectedText(Value : TColor);
    procedure SetAlternate(Value : TColor);
    procedure SetAlternateText(Value : TColor);
    procedure SetDeleted(Value : TColor);
    procedure SetDeletedText(Value : TColor);
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property OnChange : TNotifyEvent
      read FOnChange
      write FOnChange;
  published
    property Selected : TColor
      read FSelected
      write SetSelected;
    property SelectedText : TColor
      read FSelectedText
      write SetSelectedText;
    property Alternate : TColor
      read FAlternate
      write SetAlternate;
    property AlternateText : TColor
      read FAlternateText
      write SetAlternateText;
    property Deleted : TColor
      read FDeleted
      write SetDeleted;
    property DeletedText : TColor
      read FDeletedText
      write SetDeletedText;
  end;


{ ===== TAbSelList ========================================================= }
type
  TAbSelList = class
  protected {private}
    FList  : TList;
    FCurrent : Longint;
  public {methods}
    constructor Create;
    destructor Destroy;
      override;
    procedure Clear;
    procedure Deselect(Index : Longint);
    function IsSelected(Index : Longint) : Boolean;
    procedure Select(Index : Longint);
    procedure SelectAll(Count : Longint);
    function SelCount : Longint;
    procedure Toggle(Index : Longint);
    function FindFirst : Longint;
    function FindNext : Longint;
  end;


{ ===== TAbRowMap ========================================================== }
type
  TAbRowMap = class
  protected {private}
    FRows : TList;
    FInvRows : TList;
    FSortAscending : Boolean;
    function GetRow(RowNum : Longint) : Longint;
    function GetInvRow(RowNum : Longint) : Longint;
    procedure SortOnItemName(ItemList : TAbArchiveList);
    procedure SortOnItemDir(ItemList : TAbArchiveList);
  public {methods}
    constructor Create;
    destructor Destroy;
      override;
    procedure Clear;
    procedure Init(RowCount : Longint);
    procedure SortBy(Attr : TAbSortAttribute; ItemList : TAbArchiveList);
  public {properties}
    property Rows[RowNum : Longint] : Longint
      read GetRow; default;
    property InvRows[RowNum : Longint] : Longint
      read GetInvRow;
    property SortAscending : Boolean
      read FSortAscending;
  end;


{ ===== TAbBaseViewer ==================================================== }
type
  TAbColHeadings = class(TStringList)
    end;
  TAbSortedEvent =
    procedure (Sender : TObject; Attr : TAbViewAttribute) of object;
  TAbDrawSortArrowEvent =
    procedure (Sender : TObject; Column : Integer; Ascending: Boolean;
      Cnv: TCanvas; Rect : TRect) of object;

  TAbBaseViewer = class(TCustomGrid)
  protected {private}
    FAllowInvalidate : Boolean;
    FAttributes : TAbViewAttributes;
    FDisplayOptions : TAbDisplayOptions;
    FSortAttributes : TAbSortAttributes;
    FColMap : array[TAbViewAttribute] of Integer;
    FColSizing : Boolean;
    FColMoving : Boolean;                                             {!!.02}
    FHeadings : TAbColHeadings;
    FItemList : TAbArchiveList;
    FRowMap : TAbRowMap;
    FFileName : string;
    FFontSize : Integer;
    FItemIndex : Longint;
    FColors : TAbColors;
    FMethodStrings : array [TAbZipCompressionMethod] of string;
    FButtonDown : Boolean;
    FIcons : TStringList;
    FSelList : TAbSelList;
    FMultiSelecting : Boolean;
    FShiftState : TShiftState;
    FSortCol : Integer;
    RowAnchor : Longint;
    ViewMouseCoord : TGridCoord;

    FOnChange : TNotifyEvent;
    FOnClick : TNotifyEvent;
    FOnDblClick : TNotifyEvent;
    FOnSorted : TAbSortedEvent;
    FOnDrawSortArrow : TAbDrawSortArrowEvent;

    function AttrToSortAttribute(Attr : TAbViewAttribute;
      var SortAttr : TAbSortAttribute) : Boolean;
    function AttrToStr(Attr : TAbViewAttribute; aItem : TAbArchiveItem) : string;
    function ColMap(ColNum : Integer) : Integer;
    procedure ColorsChange(Sender : TObject);
    procedure DrawHeaderButton(ACol : Integer; Text : PChar);
    procedure DrawSortArrow;
    function DrawTextFormat(Attr : TAbViewAttribute; var Rect : TRect) : Word;
    function GetCount : Longint;
    function GetActiveRow : Longint;
    function GetHeaderRowHeight : Integer;
{$IFDEF MSWINDOWS}
    function GetIcon(const ItemName : string) : HIcon;
{$ENDIF}
{$IFDEF UsingClx}
  { no file type icons in Clx }
{$ENDIF}
    function GetSelCount : Longint;
    function GetSelected(RowNum : Longint) : Boolean;
    function GetVersion : string;
    procedure InitColMap;
    procedure InitMethodStrings;
    procedure InvalidateRow(ARow: Longint);
    procedure MoveColumn(FromCol, ToCol : Integer);
    procedure RefreshCell(ARow, ACol: Longint);
    procedure RefreshRow(ARow: Longint);
    procedure SetActiveRow(RowNum : Longint);
    procedure SetAttributes(Value : TAbViewAttributes);
    procedure SetDisplayOptions(Value : TAbDisplayOptions);
    procedure SetSortAttributes(Value : TAbSortAttributes);
    procedure SetHeaderRowHeight(Value : Integer);
    procedure SetHeadings(Value: TAbColHeadings);
    procedure SetSelected(RowNum : Longint; Value : Boolean);
    procedure SetVersion(const Value : string);
    function UpdateColCount(Attributes : TAbViewAttributes) : Integer;
{$IFDEF UsingCLX}
    procedure FontChanged; override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
{$ELSE}
    procedure WMSize(var Msg: TWMSize);
      message WM_SIZE;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd);
      message WM_ERASEBKGND;
    procedure CMFontChanged(var Message: TMessage);
      message CM_FONTCHANGED;
{$ENDIF UsingCLX}

  protected {overridden methods}
    procedure Click;
      override;
    procedure DblClick;
      override;
    procedure KeyDown(var Key: Word; Shift: TShiftState);
      override;
    procedure KeyUp(var Key: Word; Shift: TShiftState);
      override;
    procedure Loaded;
      override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y : Integer);
      override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
      override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint);
      override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
      override;
    procedure TopLeftChanged;
      override;

  protected {virtual methods}
    procedure DoChange(Sender : TObject);
      virtual;
    procedure DoLoad(Sender : TObject);
      virtual;
    procedure DoSorted(Attr : TAbViewAttribute);
      virtual;

  protected {properties}
    property Attributes : TAbViewAttributes
      read FAttributes
      write SetAttributes;
    property DisplayOptions : TAbDisplayOptions
      read FDisplayOptions
      write SetDisplayOptions;
    property HeaderRowHeight : Integer
      read GetHeaderRowHeight
      write SetHeaderRowHeight;
    property Headings : TAbColHeadings
      read FHeadings
      write SetHeadings;
    property ItemList : TAbArchiveList
      read FItemList
      write FItemList;
    property SortAttributes : TAbSortAttributes
      read FSortAttributes
      write SetSortAttributes;
    property Version : string
      read GetVersion
      write SetVersion
      stored False;

  protected {events}
    property OnChange : TNotifyEvent
      read FOnChange
      write FOnChange;
    property OnClick : TNotifyEvent
      read FOnClick
      write FOnClick;
    property OnDblClick : TNotifyEvent
      read FOnDblClick
      write FOnDblClick;
    property OnSorted :  TAbSortedEvent
      read FOnSorted
      write FOnSorted;
    property OnDrawSortArrow : TAbDrawSortArrowEvent
      read FOnDrawSortArrow
      write FOnDrawSortArrow;

  public {methods}
    constructor Create(AOwner : TComponent);
      override;
    destructor  Destroy;
      override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ClearSelections;
    procedure SelectAll;

  public {run-time properties}
    property ActiveRow : Longint
      read GetActiveRow  write SetActiveRow;
    property Colors : TAbColors
      read FColors write FColors;
    property Count : Longint
      read GetCount;
    property SelCount : Longint
      read GetSelCount;
    property Selected[RowNum : Longint] : Boolean
      read GetSelected write SetSelected;

    property ColWidths;
    property RowHeights;
  published  
    property OnDragDrop;
    property OnDragOver;
  end;


implementation

{ ===== TAbColors ========================================================== }
procedure TAbColors.BeginUpdate;
begin
  FUpdating := True;
end;
{ -------------------------------------------------------------------------- }
procedure TAbColors.EndUpdate;
begin
  FUpdating := False;
  DoOnChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbColors.DoOnChange;
begin
  if not FUpdating and Assigned(FOnChange) then
    FOnChange(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbColors.SetSelected(Value : TColor);
begin
  if (Value <> FSelected) then begin
    FSelected := Value;
    DoOnChange;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbColors.SetSelectedText(Value : TColor);
begin
  if (Value <> FSelectedText) then begin
    FSelectedText := Value;
    DoOnChange;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbColors.SetAlternate(Value : TColor);
begin
  if (Value <> FAlternate) then begin
    FAlternate := Value;
    DoOnChange;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbColors.SetAlternateText(Value : TColor);
begin
  if (Value <> FAlternateText) then begin
    FAlternateText := Value;
    DoOnChange;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbColors.SetDeleted(Value : TColor);
begin
  if (Value <> FDeleted) then begin
    FDeleted := Value;
    DoOnChange;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbColors.SetDeletedText(Value : TColor);
begin
  if (Value <> FDeletedText) then begin
    FDeletedText := Value;
    DoOnChange;
  end;
end;


{ ===== TAbSelList ========================================================= }
constructor TAbSelList.Create;
begin
  FList := TList.Create;
  FCurrent := -1;
end;
{ -------------------------------------------------------------------------- }
destructor TAbSelList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbSelList.Clear;
begin
  FList.Clear;
  FCurrent := -1;
end;
{ -------------------------------------------------------------------------- }
procedure TAbSelList.Select(Index: Longint);
begin
  if FList.IndexOf(Pointer(Index)) < 0 then
    FList.Add(Pointer(Index));
end;
{ -------------------------------------------------------------------------- }
procedure TAbSelList.Deselect(Index: Longint);
var
  i : Longint;
begin
  i := FList.IndexOf(Pointer(Index));
  if (i >= 0) then
    FList.Delete(i);
end;
{ -------------------------------------------------------------------------- }
function TAbSelList.IsSelected(Index : Longint) : Boolean;
begin
  Result := FList.IndexOf(Pointer(Index)) >= 0;
end;
{ -------------------------------------------------------------------------- }
procedure TAbSelList.Toggle(Index: Longint);
begin
  if IsSelected(Index) then
    Deselect(Index)
  else
    Select(Index);
end;
{ -------------------------------------------------------------------------- }
function TAbSelList.SelCount : Longint;
begin
  Result := FList.Count;
end;
{ -------------------------------------------------------------------------- }
procedure TAbSelList.SelectAll(Count : Longint);
var
  i : Longint;
begin
  for i := 0 to Pred(Count) do
    Select(i);
end;
{ -------------------------------------------------------------------------- }
function TAbSelList.FindFirst : Longint;
begin
  FCurrent := -1;
  if (FList.Count > 0) then
    Result := FindNext
  else
    Result := -1;
end;
{ -------------------------------------------------------------------------- }
function TAbSelList.FindNext : Longint;
begin
  if (FList.Count > 0) and (FCurrent < Pred(FList.Count)) then begin
    Inc(FCurrent);
    Result := Longint(FList[FCurrent]);
  end else
    Result := -1;
end;


{ ===== TAbRowMap ========================================================== }
procedure TAbRowMap.Clear;
begin
  FRows.Clear;
  FInvRows.Clear;
end;
{ -------------------------------------------------------------------------- }
function TAbRowMap.GetRow(RowNum : Longint) : Longint;
begin
  if (RowNum >= 0) and (RowNum < FRows.Count) then
    Result := Longint(FRows[RowNum])
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
function TAbRowMap.GetInvRow(RowNum : Longint) : Longint;
begin
  if (RowNum >= 0) and (RowNum < FInvRows.Count) then
    Result := Longint(FInvRows[RowNum])
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
constructor TAbRowMap.Create;
begin
  inherited Create;
  FRows := TList.Create;
  FInvRows := TList.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbRowMap.Init(RowCount : Longint);
var
  i : Longint;
begin
  Clear;
  if (RowCount > 0) then
    for i := 0 to Pred(RowCount) do begin
      FRows.Add(Pointer(i));
      FInvRows.Add(Pointer(i));
    end;
end;
{ -------------------------------------------------------------------------- }
destructor TAbRowMap.Destroy;
begin
  FRows.Free;
  FInvRows.Free;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbRowMap.SortBy(Attr : TAbSortAttribute; ItemList : TAbArchiveList);
type
  PSortRec = ^TSortRec;
  TSortRec = record
    Val : Double;
    Index : Longint;
  end;
var
  i, LI : Longint;
  SL : TList;
  RowCount : Longint;
  P : PSortRec;
  DT : TDateTime;
  aItem : TAbArchiveItem;

  procedure QuickSort(SL : TList; L, R: Integer);
  var
    i, j: Integer;
    P: PSortRec;
  begin
    i := L;
    j := R;
    P := SL[(L + R) shr 1];
    repeat
      while PSortRec(SL[i])^.Val < P^.Val do Inc(i);
      while PSortRec(SL[j])^.Val > P^.Val do Dec(j);
      if (i <= j) then
      begin
        SL.Exchange(i, j);
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if L < j then QuickSort(SL, L, j);
    if i < R then QuickSort(SL, i, R);
  end;

begin
  if (ItemList.Count <= 0) then
    Exit;
  case Attr of
    saItemName : SortOnItemName(ItemList);
    saPath : SortOnItemDir(ItemList);
    else begin
      RowCount := ItemList.Count;
      SL := TList.Create;
      try {SL}
        SL.Capacity := RowCount;
        for i := 0 to Pred(RowCount) do begin
          GetMem(P, SizeOf(TSortRec));
          aItem := TAbArchiveItem(ItemList.Items[i]);
          case Attr of
            saPacked   : P^.Val := aItem.CompressedSize;
            saRatio :
              begin
                if (aItem is TAbZipItem) then
                  P^.Val := TAbZipItem(aItem).CompressionRatio
                else
                  P^.Val := 0;
              end;
            saFileSize : P^.Val := aItem.UnCompressedSize;
            saTimeStamp : begin
              LI := LongInt(aItem.LastModFileDate) shl 16 +
                aItem.LastModFileTime;
              DT := FileDateToDateTime(LI);
              P^.Val := Double(DT);
             end;
          end;
          P^.Index := i;
          SL.Add(P);
        end;
        QuickSort(SL, 0, Pred(SL.Count));
        for i := 0 to Pred(SL.Count) do begin
          if FSortAscending then
            P := SL[i]
          else
            P := SL[Pred(SL.Count) - i];
          FRows[i] := Pointer(P^.Index)
        end;
      finally {SL}
        while (SL.Count > 0) do begin
          FreeMem(SL[0], Sizeof(TSortRec));
          SL.Delete(0);
        end;
        SL.Free;
      end; {SL}
    end;
  end;

  FSortAscending := not FSortAscending;
  for i := 0 to Pred(ItemList.Count) do
    FInvRows[Rows[i]] := Pointer(i);
end;
{ -------------------------------------------------------------------------- }
procedure TAbRowMap.SortOnItemName(ItemList : TAbArchiveList);
var
  i, RowCount : Longint;
  SL : TStringList;
  FN : string;
begin
  RowCount := ItemList.Count;
  SL := TStringList.Create;
  try {SL}
    for i := 0 to Pred(RowCount) do begin
      FN := TAbArchiveItem(ItemList.Items[i]).Filename;
      AbUnFixName(FN);
      SL.AddObject(ExtractFilename(FN), Pointer(i));
    end;
    SL.Sort;
    for i := 0 to Pred(RowCount) do begin
      if FSortAscending then
        FRows[i] := SL.Objects[i]
      else
        FRows[i] := SL.Objects[Pred(RowCount) - i];
    end;
  finally {SL}
    SL.Free;
  end; {SL}
end;

{ -------------------------------------------------------------------------- }
procedure TAbRowMap.SortOnItemDir(ItemList : TAbArchiveList);
var
  i, RowCount : Longint;
  SL : TStringList;
  FN : string;
begin
  RowCount := ItemList.Count;
  SL := TStringList.Create;
  try {SL}
    for i := 0 to Pred(RowCount) do begin
      FN := TAbArchiveItem(ItemList.Items[i]).DiskPath;
      AbUnFixName(FN);
      SL.AddObject(ExtractFilePath(FN), Pointer(i));
    end;
    SL.Sort;
    for i := 0 to Pred(RowCount) do begin
      if FSortAscending then
        FRows[i] := SL.Objects[i]
      else
        FRows[i] := SL.Objects[Pred(RowCount) - i];
    end;
  finally {SL}
    SL.Free;
  end; {SL}
end;

{===== TAbBaseViewer ===============================================}
constructor TAbBaseViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FItemList := TAbArchiveList.Create;

  RowCount := 2;
  FixedCols := 0;
  FixedRows := 1;   {Header Row}
  FSortCol := -1;
  Color := clWindow;
  FColors := TAbColors.Create;
  FColors.OnChange := ColorsChange;
  FColors.Selected := AbDefSelColor;
  FColors.SelectedText := AbDefSelTextColor;
  FColors.Alternate := AbDefHighColor;
  FColors.AlternateText := AbDefHighTextColor;
  FColors.Deleted := AbDefDelColor;
  FColors.DeletedText := AbDefDelTextColor;
  DefaultColWidth := AbDefColWidth;
  DefaultRowHeight := AbDefRowHeight;
  DefaultDrawing := False;
  ParentColor := False;
{$IFNDEF UsingCLX}
  ParentCtl3D := True;
{$ENDIF}
  ParentFont := True;
  ParentShowHint := True;
  FHeadings := TAbColHeadings.Create;
  InitColMap;
  FColSizing := False;
  FColMoving := False;
  FAllowInvalidate := True;
  FRowMap := TAbRowMap.Create;
  FIcons := TStringList.Create;
  FSelList := TAbSelList.Create;
  Attributes :=  [vaItemname, vaPacked, vaTimeStamp, vaFileSize, vaPath];
  SetDisplayOptions([doColSizing]);
  Visible := True;
end;
{ -------------------------------------------------------------------------- }
destructor TAbBaseViewer.Destroy;
begin
  FRowMap.Free;
  FHeadings.Free;
  FColors.Free;
  FIcons.Free;
  FSelList.Free;
  FItemList.Free;                                                        
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseViewer.AttrToSortAttribute(Attr : TAbViewAttribute;
  var SortAttr : TAbSortAttribute) : Boolean;
begin
  Result := True;
  case Attr of
    vaItemName  : SortAttr := saItemName;
    vaPacked    : SortAttr := saPacked;
    vaFileSize  : SortAttr := saFileSize;
    vaRatio     : SortAttr := saRatio;
    vaTimeStamp : SortAttr := saTimeStamp;
    vaPath      : SortAttr := saPath;
  else
    Result := False;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseViewer.AttrToStr(Attr : TAbViewAttribute;
  aItem : TAbArchiveItem) : string;
var
  FN : string;
  LI : Longint;
begin
  Result := '';
  if Attr in [vaItemName, vaPath] then begin
    FN := aItem.Filename;
    AbUnFixName(FN);
  end;

  {first take care of common attributes}
  with aItem do case Attr of
    vaCRC :
      Result := IntToHex(CRC32, 8);
    vaItemname :
      Result := ExtractFilename(FN);
    vaPacked :
      Result := IntToStr(CompressedSize);
    vaFileSize :
      Result := IntToStr(UncompressedSize);
    vaFileAttributes :
      begin
{$IFNDEF LINUX}
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
        if (faReadOnly and ExternalFileAttributes) = faReadOnly then
          Result := Result + AbStrRes(AbReadOnly);
        if (faHidden and ExternalFileAttributes) = faHidden then
          Result := Result + AbStrRes(AbHidden);
        if (faSysFile and ExternalFileAttributes) = faSysFile then
          Result := Result + AbStrRes(AbSystem);
        if (faArchive and ExternalFileAttributes) = faArchive then
          Result := Result + AbStrRes(AbArchived);
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
{$ENDIF LINUX}
      end;
    vaEncryption :
      if IsEncrypted then
        Result := AbStrRes(AbEncrypted);
    vaTimeStamp :
      if (LastModFileDate + LastModFileTime = 0) then
        Result := AbStrRes(AbUnknown)
      else begin
        LI := LongInt(LastModFileDate) shl 16 + LastModFileTime;
        Result := DateTimeToStr(FileDateToDateTime(LI));
      end;
    vaPath :
      Result := DiskPath;
  end;

  {now handle the zip specific attributes}
  if (aItem is TAbZipItem) then with TAbZipItem(aItem) do case Attr of
    vaFileType :
      if (InternalFileAttributes = 1) then
        Result := AbStrRes(AbText)
      else
        Result := AbStrRes(AbBinary);
    vaMethod :
      Result := FMethodStrings[CompressionMethod];
    vaRatio :
      Result := IntToStr(Round(CompressionRatio));
    vaVersionMade :
      Result := IntToStr(Round(Lo(VersionMadeBy)/ 10.0));
    vaVersionNeeded :
      Result := IntToStr(Round(Lo(VersionNeededToExtract)/ 10.0));
  end;
{$IFDEF LINUX}
  Result := ' ' + Result;
{$ENDIF}
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.BeginUpdate;
begin
  FAllowInvalidate := False;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.EndUpdate;
begin
  FAllowInvalidate := True;
  Refresh;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.ClearSelections;
var
  i : Longint;
begin
  if (FSelList.SelCount > 0) then begin
    i := FSelList.FindFirst;
    repeat
      InvalidateRow(FRowMap.InvRows[i]+1);
      i := FSelList.FindNext;
    until (i < 0);
    FSelList.Clear;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.Click;
  {Here is the logic for MultiSelect}
var
  i : Longint;
begin
  inherited Click;
  if Assigned(FItemList) and (FItemList.Count > 0) then begin
    if (ssCtrl in FShiftState) and (doMultiSelect in FDisplayOptions) then
      Selected[ActiveRow] := not Selected[ActiveRow]
    else begin
      if not ((ssShift in FShiftState) and
             (doMultiSelect in FDisplayOptions)) then begin
        ClearSelections;
        Selected[ActiveRow] := True;
      end else begin
        ClearSelections;
        if (RowAnchor < ActiveRow) then
          for i := RowAnchor to ActiveRow do
            Selected[i] := True
        else
          for i := ActiveRow to RowAnchor do
            Selected[i] := True;
      end;
    end;
    Update;
    if Assigned(FOnClick) then
      FOnClick(Self);
  end;
end;
{ -------------------------------------------------------------------------- }
{$IFDEF UsingCLX}
procedure TAbBaseViewer.FontChanged;
{$ELSE}
procedure TAbBaseViewer.CMFontChanged(var Message: TMessage);
{$ENDIF}
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    Canvas.Font := Font;
    DefaultRowHeight := Canvas.TextHeight('W') + 2;
    HeaderRowHeight := Canvas.TextHeight('W') + 4;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseViewer.ColMap(ColNum : Integer) : Integer;
begin
  Result := FColMap[TAbViewAttribute(ColNum)];
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.ColorsChange(Sender : TObject);
begin
  Invalidate;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.ColumnMoved(FromIndex, ToIndex : Longint);
begin
  MoveColumn(FromIndex, ToIndex);
  Invalidate;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.DblClick;
  {Dont pass along the event if double click in header}
begin
  inherited DblClick;
  if (ViewMouseCoord.Y <> abHeaderRow) then
    if Assigned(FItemList) and (FItemList.Count > 0) then
      if Assigned(FOnDblClick) then
        FOnDblClick(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.DoChange;
begin
  RowCount := 2;   {HeaderRow + 1}
  FSelList.Clear;
  if Assigned(FItemList) then begin
    FRowMap.Init(FItemList.Count);
    if (FItemList.Count > 0) then
      RowCount := FItemList.Count + 1
    else begin
{      RefreshRow(1);}
      FSortCol := -1;
    end;
  end;
  if FAllowInvalidate then
    Refresh;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.DoLoad;
begin
  FIcons.Clear;
  FSelList.Clear;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.DoSorted(Attr : TAbViewAttribute);
begin
  DrawSortArrow;
  if Assigned(FOnSorted) then
    FOnSorted(Self, Attr);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
const
  cStrSize = 255;
var
  s : array[0..cStrSize] of Char;                                   
  aItem : TAbArchiveItem;
  TxtRect : TRect;
  Attr : TAbViewAttribute;
  DTFormat : Word;
{$IFNDEF UsingClx}
  H : Integer;
  Icon : HIcon;
{$ENDIF}
begin
{$IFDEF LINUX}
  if not DefaultDrawing then
    DefaultDrawing := true;
{$ENDIF}

  fillchar( s, succ( cStrSize ), $00 );
  Canvas.Font := Font;
  if (ARow = AbHeaderRow) then begin
    StrPCopy(s, FHeadings[ColMap(ACol)]);
    DrawHeaderButton(ACol, s)
  end else if not FAllowInvalidate then  {waiting for EndUpdate}
    Exit
  else with Canvas do begin
    if not (doColLines in DisplayOptions) then
      ARect.Right := ARect.Right + 1;
    Brush.Color := clWindow;
    if (not Assigned(FItemList)) or (FItemList.Count = 0) then begin
      Canvas.FillRect(ARect);
      Exit;
    end;

    aItem := FItemList.Items[FRowMap[ARow-1]];
    Attr := TAbViewAttribute(ColMap(ACol));

    StrPCopy(s, AttrToStr(Attr, aItem));
    if FSelList.IsSelected(FRowMap[ARow-1]) then begin
      Brush.Color := FColors.Selected;
      Font.Color  := FColors.SelectedText;
    end else if aItem.Action = aaDelete then begin
      Brush.Color := FColors.Deleted;
      Font.Color  := FColors.DeletedText;
    end else if ((doAlternateColors in FDisplayOptions) and
    not Odd(ARow)) then begin
      Brush.Color := FColors.Alternate;
      Font.Color  := FColors.AlternateText;
    end;
    TxtRect := ARect;
{$IFNDEF UsingCLX}
    Icon := 0;
    if (Attr = vaItemName) then
      Icon := GetIcon(aItem.Filename);
    if (Icon <> 0) then begin
      H := ARect.Bottom - ARect.Top;
      Canvas.FillRect(ARect);
      Canvas.Brush.Style := bsClear;
      DrawIconEx(Canvas.Handle, ARect.Left+1, ARect.Top+1, Icon,
        H - 2, H - 2, 0, 0, DI_NORMAL);
      TxtRect.Left := TxtRect.Left + H;
    end else begin
      Canvas.FillRect(ARect);
      Canvas.Brush.Style := bsClear;
    end;
{$ELSE}
    Canvas.FillRect( CellRect( ACol, ARow ));
{$ENDIF}

    DTFormat := DrawTextFormat(Attr, TxtRect);
{$IFNDEF UsingCLX}
    DrawText(Canvas.Handle, s, -1, TxtRect, DTFormat);
{$ELSE}
    Canvas.TextRect(TxtRect, TxtRect.Left, TxtRect.Top, s, DTFormat);
{$ENDIF}
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.DrawHeaderButton(ACol : Integer; Text : PChar);
var
  ARect : TRect;
  DTFormat : Word;
begin
  ARect := CellRect(ACol, 0);
  with Canvas do begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnface;
    FillRect(ARect);
    if FButtonDown then
      Pen.Color := clHighlight
    else
      Pen.Color := clBtnShadow;
      MoveTo(ARect.Left, ARect.Bottom - 1);
      LineTo(ARect.Right - 1, ARect.Bottom - 1);
      LineTo(ARect.Right - 1, ARect.Top -1);
    if FButtonDown then
      Pen.Color := clBtnShadow
    else
      Pen.Color := clBtnHighlight;
    MoveTo(ARect.Left, ARect.Bottom - 2);
    LineTo(ARect.Left, ARect.Top);
    LineTo(ARect.Right - 1, ARect.Top);
    Brush.Style := bsClear;
  end;
  ARect.Right := ARect.Left + ColWidths[ACol];
  if FSortCol = ACol then
    ARect.Right := ARect.Right - 5 - (2 * (ARect.Bottom - ARect.Top) div 10);
{$IFDEF UsingCLX}
  { prefix is off by default in Qt}
  DTFormat := Integer(AlignmentFlags_AlignVCenter) or
              Integer(AlignmentFlags_SingleLine) or
              Integer(AlignmentFlags_AlignHCenter);
{$ELSE}
  DTFormat := DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_CENTER;
{$ENDIF}
  if FButtonDown then
    ARect := Rect(ARect.Left+1, ARect.Top+1, ARect.Right, ARect.Bottom);
{$IFDEF UsingCLX}
  Canvas.TextRect(ARect, ARect.Left, ARect.Top, Text, DTFormat);
{$ELSE}
  DrawText(Canvas.Handle, Text, -1, ARect, DTFormat);
{$ENDIF}
  if FSortCol = ACol then
    DrawSortArrow;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.DrawSortArrow;
var
  ARect : TRect;
  SavedColor : TColor;
begin
  if (FSortCol > -1) then begin
    { set up Rect for the OnDrawSortArrow event }
    ARect := CellRect(FSortCol, 0);
    ARect.Top := (ARect.Bottom - ARect.Top) div 10;
    ARect.Bottom := ARect.Bottom - ARect.Top;
    ARect.Right := ARect.Left + ColWidths[FSortCol] - 5;
    ARect.Left := ARect.Right - ((ARect.Bottom - ARect.Top));

    if Assigned(FOnDrawSortArrow) then begin
      FOnDrawSortArrow(Self, FSortCol, FRowMap.SortAscending, Canvas, ARect);
      Exit;
    end;
    { make ARect smaller for our own drawing }
    inc(ARect.Left, 10);
    inc(ARect.Top, 5);
    dec(ARect.Bottom, 5);

    with Canvas do begin
      Pen.Color := clBtnShadow;

      SavedColor := Brush.Color;
      Brush.Color := clBtnFace;
      with ARect do
        if FRowMap.SortAscending then begin
           Polygon([Point(((Right-Left)div 2)+Left, Bottom), Point(Right, Top),
             Point(Left, Top)]);
{$IFNDEF UsingCLX}
          if Ctl3D then begin
            Pen.Color := clBtnHighlight;
            MoveTo(((Right-Left)div 2)+Left, Bottom);
            LineTo(Right, Top);
          end;
{$ENDIF}
        end else begin
          Polygon([Point(((Right-Left)div 2)+Left, Top), Point(Right, Bottom),
            Point(Left, Bottom)]);
{$IFNDEF UsingCLX}
          if Ctl3D then begin
            Pen.Color := clBtnHighlight;
            MoveTo(((Right-Left)div 2)+Left, Top);
            LineTo(Right, Bottom);
            LineTo(Left, Bottom);
            Pen.Color := clBtnShadow;
            LineTo(((Right-Left)div 2)+Left, Top);
          end;
{$ENDIF}
        end;
      Brush.Color := SavedColor;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseViewer.DrawTextFormat(Attr : TAbViewAttribute;
  var Rect : TRect) : Word;
begin
{$IFDEF MSWINDOWS}
  Result := DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
  case Attr of
    vaItemname       : Result := Result or DT_LEFT;
    vaPacked         : Result := Result or DT_RIGHT;
    vaFileSize       : Result := Result or DT_RIGHT;
    vaMethod         : Result := Result or DT_CENTER;
    vaRatio          : Result := Result or DT_CENTER;
    vaCRC            : Result := Result or DT_CENTER;
    vaFileAttributes : Result := Result or DT_CENTER;
    vaFileType       : Result := Result or DT_CENTER;
    vaEncryption     : Result := Result or DT_CENTER;
    vaTimeStamp      : Result := Result or DT_LEFT;
    vaVersionMade    : Result := Result or DT_CENTER;
    vaVersionNeeded  : Result := Result or DT_CENTER;
    vaPath           : Result := Result or DT_LEFT;
  end;
  if (Result and 3) = DT_LEFT then
    OffsetRect(Rect, 5, 0)
  else if (Result and 3) = DT_RIGHT then
    OffsetRect(Rect, -5, 0);
{$ENDIF}
{$IFDEF LINUX}
  Result := Integer(AlignmentFlags_AlignVCenter) or
            Integer(AlignmentFlags_SingleLine);
  case Attr of
    vaItemname       : Result := Result or Integer(AlignmentFlags_AlignLeft);
    vaPacked         : Result := Result or Integer(AlignmentFlags_AlignRight);
    vaFileSize       : Result := Result or Integer(AlignmentFlags_AlignRight);
    vaMethod         : Result := Result or Integer(AlignmentFlags_AlignCenter);
    vaRatio          : Result := Result or Integer(AlignmentFlags_AlignCenter);
    vaCRC            : Result := Result or Integer(AlignmentFlags_AlignCenter);
    vaFileAttributes : Result := Result or Integer(AlignmentFlags_AlignCenter);
    vaFileType       : Result := Result or Integer(AlignmentFlags_AlignCenter);
    vaEncryption     : Result := Result or Integer(AlignmentFlags_AlignCenter);
    vaTimeStamp      : Result := Result or Integer(AlignmentFlags_AlignLeft);
    vaVersionMade    : Result := Result or Integer(AlignmentFlags_AlignCenter);
    vaVersionNeeded  : Result := Result or Integer(AlignmentFlags_AlignCenter);
    vaPath           : Result := Result or Integer(AlignmentFlags_AlignLeft);
  end;
{$ENDIF}
end;

{ -------------------------------------------------------------------------- }
function TAbBaseViewer.GetActiveRow : Longint;
begin
  Result := Row - 1;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseViewer.GetCount : Longint;
begin
  if Assigned(FItemList) then
    Result := FItemList.Count
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseViewer.GetHeaderRowHeight : Integer;
begin
  Result := RowHeights[AbHeaderRow];
end;
{ -------------------------------------------------------------------------- }
{$IFDEF MSWINDOWS}
function TAbBaseViewer.GetIcon(const ItemName : string) : HIcon;
var
  i : Longint;
  s : array[0..255] of Char;
  t : string;
  sfi : SHFILEINFO;
begin
  Result := 0;
  if not (doShowIcons in FDisplayOptions) then
    Exit;
  t := ExtractFileExt(ItemName);
  i := FIcons.IndexOf(t);
  if (i > -1) then
    Result := HIcon(FIcons.Objects[i])
  else begin
    strPcopy(s,t);
    SHGetFileInfo( s, FILE_ATTRIBUTE_NORMAL,
                  sfi, sizeof(sfi), SHGFI_ICON or SHGFI_USEFILEATTRIBUTES);
    result := sfi.hIcon;
    FIcons.AddObject(t, Pointer(Result));
  end;
end;

{$ENDIF}
{$IFDEF UsingCLX }
  { no file type icons in CLX }
{$ENDIF}
{ -------------------------------------------------------------------------- }
function TAbBaseViewer.GetSelCount : Longint;
begin
  Result := FSelList.SelCount;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseViewer.GetSelected(RowNum : Longint) : Boolean;
begin
  if Assigned(FItemList) then
    Result := FSelList.IsSelected(FRowMap[RowNum])
  else
    Result := False;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseViewer.GetVersion : string;
begin
  Result := AbVersion;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.InitColMap;
var
  i : TAbViewAttribute;
begin
  FHeadings.Clear;
  for i := Low(TAbViewAttribute) to High(TAbViewAttribute) do begin
    FHeadings.Add(AbStrRes(AbDefColHeadings + Ord(i)));
    FColMap[i] := Ord(i);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.InitMethodStrings;
var
  i : TAbZipCompressionMethod;
begin
  for i := Low(TAbZipCompressionMethod) to High(TAbZipCompressionMethod) do
    FMethodStrings[i] := AbStrRes(AbMethod + Ord(i));
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.InvalidateRow(ARow: Longint);
var
  Rect: TRect;
begin
  if not HandleAllocated then
    Exit;
  if ((ARow < TopRow) or (ARow > TopRow + VisibleRowCount)) and (ARow <> 0) then
    Exit;
  Rect := CellRect(0, ARow);
  Rect.Right := ClientWidth;
{$IFDEF UsingCLX}
  InvalidateRect(Rect, False);
{$ELSE}
  InvalidateRect(Handle, @Rect, True);
{$ENDIF}
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FShiftState := Shift;
  inherited KeyDown(Key, Shift);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FShiftState := Shift;
  inherited KeyUp(Key, Shift);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.Loaded;
begin
  inherited Loaded;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y : Integer);

  function GetMinLen(Col: Integer): Word;
  var
    I, L : Integer;
    s : String;
    aItem : TAbArchiveItem;
    Attr : TAbViewAttribute;
    Sorted : Boolean;
  begin
    Attr := TAbViewAttribute(ColMap(Col));
    Result := Canvas.TextWidth(FHeadings[ColMap(Col)]);
    case Attr of
      vaItemName : Sorted := saItemName in FSortAttributes;
      vaPacked   : Sorted := saPacked in FSortAttributes;
      vaRatio    : Sorted := saRatio in FSortAttributes;
      vaTimeStamp: Sorted := saTimeStamp in FSortAttributes;
      vaFileSize : Sorted := saFileSize in FSortAttributes;
      vaPath     : Sorted := saPath in FSortAttributes;
      else Sorted := False;
    end;
    if Sorted then
      Result := Result + RowHeights[0] + 16
    else
      Result := Result + 8;

    if Assigned(FItemList) then
    for I := 0 to (FItemList.Count-1) do begin
      aItem := FItemList.Items[I];
      S := AttrToStr(Attr, aItem);
      L := Canvas.TextWidth(S) + 8;
      if (doShowIcons in FDisplayOptions) and (Attr = vaItemName) then
        inc(L, RowHeights[I]);
      if L > Result then
        Result := L;
    end;
  end;

var
  ACol : Longint;
  ARow : Longint;
  Rect : TRect;
begin
  ViewMouseCoord := MouseCoord(X, Y);
  inherited MouseDown(Button, Shift, X, Y);

  FShiftState := Shift;
  { handle double clicks on header row dividers }
  if (ssDouble in FShiftState) and (ViewMouseCoord.Y = AbHeaderRow) then begin
    FColSizing := True;
    Rect := CellRect(ViewMouseCoord.X, ViewMouseCoord.Y);
    Rect.Left := Rect.Right - 3;
    if PtInRect(Rect, Point(X, Y)) then begin
      ColWidths[MouseCoord(Rect.Left, Y).X] :=
        GetMinLen(MouseCoord(Rect.Left, Y).X)
    end
    else begin
      Rect := CellRect(ViewMouseCoord.X, ViewMouseCoord.Y);
      Rect.Right := Rect.Left + 4;
      if PtInRect(Rect, Point(X, Y)) then
        ColWidths[MouseCoord(Rect.Left, Y).X-1] :=
          GetMinLen(MouseCoord(Rect.Left, Y).X-1);
    end;
  end;

  { if grid is being resized }
  if (FGridState = gsColSizing) then begin
    FColSizing := True;
    Exit;  {dont press button when resizing column}
  end;

  { refresh the headers}
  if Assigned(FItemList) then
    if (FItemList.Count > 0) then begin
      ARow := ViewMouseCoord.Y;
      ACol := ViewMouseCoord.X;
      if (ARow = abHeaderRow) then begin
        {if not (doColMove in FDisplayOptions) then}
        if not (doColMove in FDisplayOptions) and not FColSizing then
          FButtonDown := True;
        RefreshCell(0, ACol);
      end else if not (ssShift in Shift) then
        RowAnchor := ActiveRow;
    end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol   : Longint;
  ARow   : Longint;
  Attr   : TAbViewAttribute;
  SortAttribute : TAbSortAttribute;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if csDesigning in ComponentState then Exit;

  FShiftState := Shift;
  FButtonDown := False;

  if FColSizing then begin
    Refresh;
    FColSizing := False;
  end else
    if Assigned(FItemList) then
      if (FItemList.Count > 0) then begin
        ARow := ViewMouseCoord.Y;
        ACol := ViewMouseCoord.X;
        if (ARow = abHeaderRow) then begin
          Attr := TAbViewAttribute(ColMap(ACol));
          if not FColMoving and                                       {!!.02}
            {not (doColMove in FDisplayOptions) and}                  {!!.02}
            AttrToSortAttribute(Attr, SortAttribute) and
            (SortAttribute in FSortAttributes) then begin
            FSortCol := ACol;
            FItemIndex := FRowMap[Row-1];
            FRowMap.SortBy(SortAttribute, FItemList);
            FButtonDown := False;
            RefreshCell(0, ACol);
            if (doTrackActiveRow in FDisplayOptions) then
              Row := FRowMap.InvRows[FItemIndex] + 1;
            Refresh;
            DoSorted(Attr);
          end else begin
            FButtonDown := False;
            RefreshCell(0, ACol);
          end;
        end else
          Paint;
      end;

  FColMoving := False;                                                {!!.02}
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if (FGridState = gsColMoving) then                                  {!!.02}
    FColMoving := True;                                               {!!.02}
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.MoveColumn(FromCol, ToCol : Integer);
var
  temp, i : Integer;
begin
  Temp := ColMap(FromCol);
  if (FromCol < ToCol) then begin
    for i := (FromCol + 1) to ToCol do
      FColMap[TAbViewAttribute(i-1)] := FColMap[TAbViewAttribute(i)]; {Shift left}
  end else begin
    for i := (FromCol - 1) downto ToCol do
      FColMap[TAbViewAttribute(i+1)] := FColMap[TAbViewAttribute(i)]; {Shift right}
  end;
  FColMap[TAbViewAttribute(ToCol)] := Temp;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.RefreshCell(ARow, ACol: Longint);
var
  Rect: TRect;
begin
  if not HandleAllocated then
    Exit;
  Rect := CellRect(ACol, ARow);
{$IFDEF UsingCLX}
  InvalidateRect(Rect, False);
{$ELSE}
  InvalidateRect(Handle, @Rect, False);
{$ENDIF}
  Update;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.RefreshRow(ARow: Longint);
begin
  InvalidateRow(ARow);
  Update;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.SelectAll;
begin
  if Assigned(FItemList) then
    if (FItemList.Count > 0) then begin
      FSelList.SelectAll(FItemList.Count);
      Invalidate;
    end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.SetActiveRow(RowNum : Longint);
begin
  if Assigned(FItemList) then
    if (RowNum >= 0) and (RowNum < FItemList.Count) then
       Row := RowNum + 1;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.SetAttributes(Value : TAbViewAttributes);
begin
  FAttributes := Value;
  ColCount := UpdateColCount(FAttributes);
  DoChange(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.SetDisplayOptions(Value : TAbDisplayOptions);
  {maps DisplayOptions to TGridOptions}
begin
  FDisplayOptions := Value;
  Options := [goFixedVertLine, goFixedHorzLine, goRowSelect];

  if (doColLines in Value) then
    Options := Options + [goVertLine];
  if (doColMove in Value) then
    Options := Options + [goColMoving];
  if (doColSizing in Value) then
    Options := Options + [goColSizing];
  if (doRowLines in Value) then
    Options := Options + [goHorzLine];
  if (doThumbTrack in Value) then
    Options := Options + [goThumbTracking];
  DoChange(nil);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.SetHeaderRowHeight(Value : Integer);
begin
  RowHeights[abHeaderRow] := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.SetHeadings(Value: TAbColHeadings);
begin
  Headings.Assign(Value);
  Refresh;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.SetSortAttributes(Value : TAbSortAttributes);
begin
  FSortAttributes := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.SetSelected(RowNum : Longint; Value: Boolean);
begin
  if Assigned(FItemList) then
    case Value of
      True  : FSelList.Select(FRowMap[RowNum]);
      False : FSelList.Deselect(FRowMap[RowNum]);
    end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.SetVersion(const Value : string);
begin
  {NOP}
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseViewer.TopLeftChanged;
begin
  if FAllowInvalidate then
    Invalidate;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseViewer.UpdateColCount(Attributes : TAbViewAttributes) : Integer;
var
  i : TAbViewAttribute;
begin
  Result := 0;
  for i := Low(TAbViewAttribute) to High(TAbViewAttribute) do begin
    if (i in Attributes) then begin
      FColMap[TAbViewAttribute(Result)] := Ord(i);
      Inc(Result);
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
{$IFDEF UsingCLX}
procedure TAbBaseViewer.SizeChanged(OldColCount, OldRowCount: Longint);
begin
  inherited SizeChanged(OldColCount, OldRowCount);
  Refresh;
end;
{$ELSE}
procedure TAbBaseViewer.WMSize(var Msg: TWMSize);
begin
  inherited;
  Refresh;
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
{$IFNDEF UsingCLX}
procedure TAbBaseViewer.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := -1;
end;
{$ENDIF}

end.

