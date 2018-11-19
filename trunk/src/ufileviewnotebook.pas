{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains TFileViewPage and TFileViewNotebook objects.

   Copyright (C) 2016-2018 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uFileViewNotebook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, LMessages,
  LCLType, Forms,
  uFileView, uFilePanelSelect, DCXmlConfig;

type

  TTabLockState = (
    tlsNormal,           //<en Default state.
    tlsPathLocked,       //<en Path changes are not allowed.
    tlsPathResets,       //<en Path is reset when activating the tab.
    tlsDirsInNewTab);    //<en Path change opens a new tab.

  TFileViewNotebook = class;

  { TFileViewPage }

  TFileViewPage = class(TWinControl)
  private
    FLockState: TTabLockState;
    FLockPath: String;          //<en Path on which tab is locked
    FOnActivate: TNotifyEvent;
    FCurrentTitle: String;
    FPermanentTitle: String;
    FBackupViewClass: TFileViewClass;

    procedure AssignPage(OtherPage: TFileViewPage);
    procedure AssignProperties(OtherPage: TFileViewPage);
    {en
       Retrieves the file view on this page.
    }
    function GetFileView: TFileView;
    {en
       Retrieves notebook on which this page is.
    }
    function GetNotebook: TFileViewNotebook;
    function GetPageIndex: Integer;
    {en
       Frees current file view and assigns a new one.
    }
    procedure SetFileView(aFileView: TFileView);
    procedure SetLockState(NewLockState: TTabLockState);
    procedure SetPermanentTitle(AValue: String);

    procedure DoActivate;

  protected
    procedure PaintWindow(DC: HDC); override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  public
    constructor Create(TheOwner: TComponent); override;

    function IsActive: Boolean;
    procedure MakeActive;
    procedure UpdateTitle;

    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);

    property PageIndex: Integer read GetPageIndex;
    property LockState: TTabLockState read FLockState write SetLockState;
    property LockPath: String read FLockPath write FLockPath;
    property FileView: TFileView read GetFileView write SetFileView;
    property Notebook: TFileViewNotebook read GetNotebook;
    property PermanentTitle: String read FPermanentTitle write SetPermanentTitle;
    property CurrentTitle: String read FCurrentTitle;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property BackupViewClass: TFileViewClass read FBackupViewClass write FBackupViewClass;
  end;

  { TFileViewPageControl }

  TFileViewPageControl = class(TPageControl)
  private
    FStartDrag: Boolean;
    FDraggedPageIndex: Integer;
    FHintPageIndex: Integer;
    FLastMouseDownTime: TDateTime;
    FLastMouseDownPageIndex: Integer;
    {$IFDEF MSWINDOWS}
    FRowCount: Integer;
    {$ENDIF}
    function GetNoteBook: TFileViewNotebook;
  private
    procedure DragOverEvent(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DragDropEvent(Sender, Source: TObject; X, Y: Integer);

  protected
    procedure CreateHandle; override;
    procedure TabControlBoundsChange(Data: PtrInt);
  protected
    procedure DoChange; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ParentControl: TWinControl); reintroduce;

    procedure DoCloseTabClicked(APage: TCustomPage); override;

    {$IFDEF MSWINDOWS}
    {en
       Removes the rectangle of the pages contents from erasing background to reduce flickering.
       This is not needed on non-Windows because EraseBackground is not used there.
    }
    procedure EraseBackground(DC: HDC); override;
    procedure WndProc(var Message: TLMessage); override;
    {$ENDIF}

    property Notebook: TFileViewNotebook read GetNoteBook;
  end;

  { TFileViewNotebook }

  TFileViewNotebook = class(TCustomControl)
  private
    FOnPageChanged: TNotifyEvent;
    FNotebookSide: TFilePanelSelect;
    FOnCloseTabClicked: TNotifyEvent;
    FPageControl: TFileViewPageControl;

    function GetActivePage: TFileViewPage;
    function GetActiveView: TFileView;
    function GetFileViewOnPage(Index: Integer): TFileView;

    function GetShowTabs: Boolean;
    function GetMultiLine: Boolean;
    function GetPageIndex: Integer;
    function GetPageCount: Integer;
    function GetTabPosition: TTabPosition;
    function GetOptions: TCTabControlOptions;
    function GetLastMouseDownPageIndex: Integer;
    function GetPage(Index: Integer): TFileViewPage; reintroduce;

    procedure SetShowTabs(AValue: Boolean);
    procedure SetPageIndex(AValue: Integer);
    procedure SetMultiLine(AValue: Boolean);
    procedure SetTabPosition(AValue: TTabPosition);
    procedure SetOptions(AValue: TCTabControlOptions);

  protected
    procedure DoChange;
    procedure UpdatePagePosition(AIndex, ASpacing: Integer);
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(ParentControl: TWinControl;
                       NotebookSide: TFilePanelSelect); reintroduce;

    function AddPage: TFileViewPage;
    function IndexOfPageAt(P: TPoint): Integer;
    function GetCapabilities: TCTabControlCapabilities;
    function InsertPage(Index: Integer): TFileViewPage; reintroduce;
    function NewEmptyPage: TFileViewPage;
    function NewPage(CloneFromPage: TFileViewPage): TFileViewPage;
    function NewPage(CloneFromView: TFileView): TFileViewPage;
    procedure DeletePage(Index: Integer);
    procedure RemovePage(Index: Integer); reintroduce;
    procedure RemovePage(var APage: TFileViewPage);
    procedure DestroyAllPages;
    procedure ActivatePrevTab;
    procedure ActivateNextTab;
    procedure ActivateTabByIndex(Index: Integer);

    property ActivePage: TFileViewPage read GetActivePage;
    property ActiveView: TFileView read GetActiveView;
    property DoubleClickPageIndex: Integer read GetLastMouseDownPageIndex;
    property Page[Index: Integer]: TFileViewPage read GetPage;
    property View[Index: Integer]: TFileView read GetFileViewOnPage; default;
    property Side: TFilePanelSelect read FNotebookSide;

    property PageCount: Integer read GetPageCount;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;

    property ShowTabs: Boolean read GetShowTabs write SetShowTabs;
    property MultiLine: Boolean read GetMultiLine write SetMultiLine;
    property Options: TCTabControlOptions read GetOptions write SetOptions;
    property TabPosition: TTabPosition read GetTabPosition write SetTabPosition;

    property OnChange: TNotifyEvent read FOnPageChanged write FOnPageChanged;
    property OnCloseTabClicked: TNotifyEvent read FOnCloseTabClicked write FOnCloseTabClicked;

  published
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
  end;

implementation

uses
  Math,
  LCLIntf,
  LazUTF8,
  DCStrUtils,
  uGlobs,
  uColumnsFileView,
  uArchiveFileSource
  {$IF DEFINED(LCLGTK2)}
  , Glib2, Gtk2, Gtk2Proc, Gtk2Def
  {$ENDIF}
  {$IF DEFINED(MSWINDOWS)}
  , win32proc, Windows, Messages
  {$ENDIF}
  ;

// -- TFileViewPage -----------------------------------------------------------

procedure TFileViewPage.AssignPage(OtherPage: TFileViewPage);
begin
  AssignProperties(OtherPage);
  SetFileView(nil); // Remove previous view.
  OtherPage.FileView.Clone(Self);
end;

procedure TFileViewPage.AssignProperties(OtherPage: TFileViewPage);
begin
  FLockState      := OtherPage.FLockState;
  FLockPath       := OtherPage.FLockPath;
  FCurrentTitle   := OtherPage.FCurrentTitle;
  FPermanentTitle := OtherPage.FPermanentTitle;
end;

constructor TFileViewPage.Create(TheOwner: TComponent);
begin
  FLockState := tlsNormal;
  FBackupViewClass := TColumnsFileView;
  inherited Create(TheOwner);

  ControlStyle := ControlStyle + [csAcceptsControls, csDesignFixedBounds, csNoDesignVisible, csNoFocus];

  // Height and width depends on parent, align to client rect
  Caption := '';
  Visible := False;
end;

procedure TFileViewPage.RealSetText(const AValue: TCaption);
begin
  inherited RealSetText(AValue);
  Notebook.FPageControl.Pages[PageIndex].Caption:= AValue;
end;

function TFileViewPage.IsActive: Boolean;
begin
  Result := Assigned(Notebook) and (Notebook.PageIndex = PageIndex);
end;

procedure TFileViewPage.LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
begin
  FLockState := TTabLockState(AConfig.GetValue(ANode, 'Options', Integer(tlsNormal)));
  FLockPath := AConfig.GetValue(ANode, 'LockPath', '');
  FPermanentTitle := AConfig.GetValue(ANode, 'Title', '');
end;

procedure TFileViewPage.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
begin
  AConfig.AddValueDef(ANode, 'Options', Integer(FLockState), Integer(tlsNormal));
  AConfig.AddValueDef(ANode, 'LockPath', FLockPath, '');
  AConfig.AddValueDef(ANode, 'Title', FPermanentTitle, '');
end;

procedure TFileViewPage.MakeActive;
var
  aFileView: TFileView;
begin
  if Assigned(Notebook) then
  begin
    Notebook.PageIndex := PageIndex;

    aFileView := FileView;
    if Assigned(aFileView) then
      aFileView.SetFocus;
  end;
end;

procedure TFileViewPage.PaintWindow(DC: HDC);
begin
  // Don't paint anything.
end;

procedure TFileViewPage.UpdateTitle;
  {$IFDEF MSWINDOWS}
  function LocalGetDriveName(A:string):string;
  begin
    result:=LowerCase(ExtractFileDrive(A));
    if length(result)>2 then // Server path name are shown simply like \: in TC so let's do the same for those who get used to that.
      result:='\:'
    else
      if Lowercase(A) = (result+DirectorySeparator) then
        result:=''; //To avoid to get "c:C:" :-)
  end;
 {$ENDIF}

var
  NewCaption: String;
begin
  if Assigned(FileView) then
  begin
    if FPermanentTitle <> '' then
    begin
      NewCaption := FPermanentTitle;
      FCurrentTitle := FPermanentTitle;
    end
    else
    begin
      if (FileView.FileSource is TArchiveFileSource) and
         (FileView.FileSource.IsPathAtRoot(FileView.CurrentPath)) then
        begin
          with (FileView.FileSource as TArchiveFileSource) do
            NewCaption := ExtractFileName(ArchiveFileName);
        end
      else
        begin
          NewCaption := FileView.CurrentPath;
          if NewCaption <> '' then
            NewCaption := GetLastDir(NewCaption);
        end;
      FCurrentTitle := NewCaption;
    end;

    {$IFDEF MSWINDOWS}
    if tb_show_drive_letter in gDirTabOptions then
    begin
      if (FileView.FileSource is TArchiveFileSource) then
        with (FileView.FileSource as TArchiveFileSource) do NewCaption := LocalGetDriveName(ArchiveFileName) + NewCaption
      else
        NewCaption := LocalGetDriveName(FileView.CurrentPath) + NewCaption;
    end;
    {$ENDIF}

    if (FLockState in [tlsPathLocked, tlsPathResets, tlsDirsInNewTab]) and
       (tb_show_asterisk_for_locked in gDirTabOptions) then
      NewCaption := '*' + NewCaption;

    if (tb_text_length_limit in gDirTabOptions) and (UTF8Length(NewCaption) > gDirTabLimit) then
      NewCaption := UTF8Copy(NewCaption, 1, gDirTabLimit) + '...';

{$IF DEFINED(LCLGTK2)}
    Caption := NewCaption;
{$ELSE}
    Caption := StringReplace(NewCaption, '&', '&&', [rfReplaceAll]);
{$ENDIF}
  end;
end;

procedure TFileViewPage.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

function TFileViewPage.GetFileView: TFileView;
begin
  if ComponentCount > 0 then
    Result := TFileView(Components[0])
  else
    Result := nil;
end;

procedure TFileViewPage.SetFileView(aFileView: TFileView);
var
  aComponent: TComponent;
begin
  if ComponentCount > 0 then
  begin
    aComponent := Components[0];
    aComponent.Free;
  end;

  if Assigned(aFileView) then
  begin
    aFileView.Parent := Self;
  end;
end;

function TFileViewPage.GetNotebook: TFileViewNotebook;
begin
  Result := Owner as TFileViewNotebook;
end;

function TFileViewPage.GetPageIndex: Integer;
var
  Index: Integer;
begin
  if Assigned(Notebook) then
  begin
    for Index:= 0 to Notebook.PageCount - 1 do
    begin
      if (Notebook.GetPage(Index) = Self) then
        Exit(Index);
    end;
  end;
  Result := -1;
end;

procedure TFileViewPage.SetLockState(NewLockState: TTabLockState);
begin
  if FLockState = NewLockState then Exit;
  if NewLockState in [tlsPathLocked, tlsPathResets, tlsDirsInNewTab] then
    begin
      LockPath := FileView.CurrentPath;
      if (FLockState <> tlsNormal) or (Length(FPermanentTitle) = 0) then
        FPermanentTitle := GetLastDir(LockPath);
    end
  else
    begin
      LockPath := '';
      if not (tb_keep_renamed_when_back_normal in gDirTabOptions) then
        FPermanentTitle := '';
    end;
  FLockState := NewLockState;
  UpdateTitle;
end;

procedure TFileViewPage.SetPermanentTitle(AValue: String);
begin
  if FPermanentTitle = AValue then Exit;
  FPermanentTitle := AValue;
  UpdateTitle;
end;

procedure TFileViewPage.DoActivate;
begin
  if Assigned(FOnActivate) then
    FOnActivate(Self);
end;

{ TFileViewPageControl }

function TFileViewPageControl.GetNoteBook: TFileViewNotebook;
begin
  Result:= TFileViewNotebook(Parent);
end;

procedure TFileViewPageControl.DragOverEvent(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  ATabIndex: Integer;
begin
  if (Source is TFileViewPageControl) and (Sender is TFileViewPageControl) then
  begin
    ATabIndex := IndexOfPageAt(Classes.Point(X, Y));
    Accept := (Source <> Sender) or
              ((ATabIndex <> -1) and (ATabIndex <> FDraggedPageIndex));
  end
  else
    Accept := False;
end;

procedure TFileViewPageControl.DragDropEvent(Sender, Source: TObject; X, Y: Integer);
var
  ATabIndex: Integer;
  ANewPage, DraggedPage: TFileViewPage;
  SourcePageControl: TFileViewPageControl;
begin
  if (Source is TFileViewPageControl) and (Sender is TFileViewPageControl) then
  begin
    ATabIndex := IndexOfPageAt(Classes.Point(X, Y));

    if Source = Sender then
    begin
      // Move within the same panel.
      if ATabIndex <> -1 then
        Tabs.Move(FDraggedPageIndex, ATabIndex);
    end
    else
    begin
      // Move page between panels.
      SourcePageControl:= TFileViewPageControl(Source);
      DraggedPage := SourcePageControl.Notebook.Page[SourcePageControl.FDraggedPageIndex];

      if ATabIndex = -1 then
        ATabIndex := PageCount;

      // Create a clone of the page in the panel.
      ANewPage := Notebook.InsertPage(ATabIndex);
      ANewPage.AssignPage(DraggedPage);
      ANewPage.MakeActive;

      if (ssShift in GetKeyShiftState) and (SourcePageControl.Notebook.PageCount > 1) then
      begin
        // Remove page from source panel.
        SourcePageControl.Notebook.RemovePage(DraggedPage);
      end;
    end;
  end;
end;

procedure TFileViewPageControl.CreateHandle;
begin
  inherited CreateHandle;
  TabControlBoundsChange(0);
end;

procedure TFileViewPageControl.TabControlBoundsChange(Data: PtrInt);
var
  AIndex: Integer;
  ASpacing: Integer;
begin
  if PageIndex >= 0 then
  begin
    if not Visible then
      ASpacing:= 0
    else begin
      case TabPosition of
        tpTop:    ASpacing:= (Page[PageIndex].ClientOrigin.Y - Notebook.ClientOrigin.Y);
        tpBottom: ASpacing:= (Notebook.ClientOrigin.Y + Notebook.Height) - (Page[PageIndex].ClientOrigin.Y + Page[PageIndex].Height);
      end;
    end;

    for AIndex:= 0 to PageCount - 1 do
    begin
      Notebook.UpdatePagePosition(AIndex, ASpacing);
    end;
  end;

  Invalidate;
end;

procedure TFileViewPageControl.DoChange;
begin
  inherited DoChange;
  Notebook.DoChange;
end;

procedure TFileViewPageControl.DblClick;
begin
  inherited DblClick;
  if Assigned(Notebook.OnDblClick) then
    Notebook.OnDblClick(Notebook);
end;

procedure TFileViewPageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint;
{$IF DEFINED(LCLGTK2)}
var
  ArrowWidth: Integer;
  arrow_spacing: gint = 0;
  scroll_arrow_hlength: gint = 16;
{$ENDIF}
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Assigned(Notebook.OnMouseDown) then
  begin
    APoint:= ClientToParent(Classes.Point(X, Y));
    Notebook.OnMouseDown(Notebook, Button, Shift, APoint.X, APoint.Y);
  end;

  if Button = mbLeft then
  begin
    FDraggedPageIndex := IndexOfPageAt(Classes.Point(X, Y));
    FStartDrag := (FDraggedPageIndex <> -1);
  end;
  // Emulate double click
  if (Button = mbLeft) and Assigned(Notebook.OnDblClick) then
    begin
      if ((Now - FLastMouseDownTime) > ((1/86400)*(GetDoubleClickTime/1000))) then
        begin
          FLastMouseDownTime:= Now;
          FLastMouseDownPageIndex:= FDraggedPageIndex;
        end
      else if (FDraggedPageIndex = FLastMouseDownPageIndex) then
        begin
          {$IF DEFINED(LCLGTK2)}
          gtk_widget_style_get({%H-}PGtkWidget(Self.Handle),
                               'arrow-spacing', @arrow_spacing,
                               'scroll-arrow-hlength', @scroll_arrow_hlength,
                               nil);
          ArrowWidth:= arrow_spacing + scroll_arrow_hlength;
          if (X > ArrowWidth) and (X < ClientWidth - ArrowWidth) then
          {$ENDIF}
          Notebook.DblClick;
          FStartDrag:= False;
          FLastMouseDownTime:= 0;
          FLastMouseDownPageIndex:= -1;
        end;
    end;
end;

procedure TFileViewPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ATabIndex: Integer;
begin
  inherited MouseMove(Shift, X, Y);

  if ShowHint then
  begin
    ATabIndex := IndexOfPageAt(Classes.Point(X, Y));
    if (ATabIndex >= 0) and (ATabIndex <> FHintPageIndex) then
    begin
      FHintPageIndex := ATabIndex;
      Application.CancelHint;
      if (ATabIndex <> PageIndex) and (Length(Notebook.Page[ATabIndex].LockPath) <> 0) then
        Hint := Notebook.Page[ATabIndex].LockPath
      else
        Hint := Notebook.View[ATabIndex].CurrentPath;
    end;
  end;

  if FStartDrag then
  begin
    FStartDrag := False;
    BeginDrag(False);
  end;
end;

procedure TFileViewPageControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Assigned(Notebook.OnMouseUp) then
  begin
    APoint:= ClientToParent(Classes.Point(X, Y));
    Notebook.OnMouseUp(Notebook, Button, Shift, APoint.X, APoint.Y);
  end;

  FStartDrag := False;
end;

constructor TFileViewPageControl.Create(ParentControl: TWinControl);
begin
  inherited Create(ParentControl);
  ControlStyle := ControlStyle + [csNoFocus];

  Align := alClient;
  TabStop := False;
  ShowHint := True;
  Parent := ParentControl;

  FHintPageIndex := -1;
  FStartDrag := False;

  {$IFDEF MSWINDOWS}
  // The pages contents are removed from drawing background in EraseBackground.
  // But double buffering could be enabled to eliminate flickering of drawing
  // the tabs buttons themselves. But currently there's a bug where the buffer
  // bitmap is temporarily drawn in different position, probably at (0,0) and
  // not where pages contents start (after applying TCM_ADJUSTRECT).
  //DoubleBuffered := True;
  {$ENDIF}

  OnDragOver := @DragOverEvent;
  OnDragDrop := @DragDropEvent;

  TabControlBoundsChange(0);
end;

procedure TFileViewPageControl.DoCloseTabClicked(APage: TCustomPage);
begin
  inherited DoCloseTabClicked(APage);
  if Assigned(Notebook.OnCloseTabClicked) then
    Notebook.OnCloseTabClicked(Notebook.Page[APage.PageIndex]);
end;

{$IFDEF MSWINDOWS}
procedure TFileViewPageControl.EraseBackground(DC: HDC);
var
  ARect: TRect;
  SaveIndex: Integer;
  Clip: Integer;
begin
  if HandleAllocated and (DC <> 0) then
  begin
    ARect := Classes.Rect(0, 0, Width, Height);
    Windows.TabCtrl_AdjustRect(Handle, False, ARect);
    SaveIndex := SaveDC(DC);
    Clip := ExcludeClipRect(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    if Clip <> NullRegion then
    begin
      ARect := Classes.Rect(0, 0, Width, Height);
      FillRect(DC, ARect, HBRUSH(Brush.Reference.Handle));
    end;
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TFileViewPageControl.WndProc(var Message: TLMessage);
var
  ARowCount: Integer;
  ARect: PRect absolute Message.LParam;
begin
  inherited WndProc(Message);
  if Message.Msg = TCM_ADJUSTRECT then
  begin
    if Message.WParam = 0 then
      ARect^.Left := ARect^.Left - 2
    else begin
      ARect^.Left := ARect^.Left + 2;
    end;
    if MultiLine then
    begin
      ARowCount := SendMessage(Handle, TCM_GETROWCOUNT, 0, 0);
      if (FRowCount <> ARowCount) then
      begin
        FRowCount:= ARowCount;
        PostMessage(Handle, WM_USER, 0, 0);
      end;
    end;
  end
  else if Message.Msg = WM_USER then
  begin
    TabControlBoundsChange(FRowCount);
  end;
end;
{$ENDIF}

// -- TFileViewNotebook -------------------------------------------------------

constructor TFileViewNotebook.Create(ParentControl: TWinControl;
                                     NotebookSide: TFilePanelSelect);
begin
  inherited Create(ParentControl);
  ControlStyle := ControlStyle + [csNoFocus];

  FPageControl:= TFileViewPageControl.Create(Self);

  Parent := ParentControl;
  TabStop := False;
  ShowHint := True;

  FNotebookSide := NotebookSide;
end;

function TFileViewNotebook.GetActivePage: TFileViewPage;
begin
  if PageIndex <> -1 then
    Result := GetPage(PageIndex)
  else
    Result := nil;
end;

function TFileViewNotebook.GetActiveView: TFileView;
var
  APage: TFileViewPage;
begin
  APage := GetActivePage;
  if Assigned(APage) then
    Result := APage.FileView
  else
    Result := nil;
end;

function TFileViewNotebook.GetFileViewOnPage(Index: Integer): TFileView;
var
  APage: TFileViewPage;
begin
  APage := GetPage(Index);
  Result := APage.FileView;
end;

function TFileViewNotebook.GetLastMouseDownPageIndex: Integer;
begin
  Result:= FPageControl.FLastMouseDownPageIndex;
end;

function TFileViewNotebook.GetMultiLine: Boolean;
begin
  Result:= FPageControl.MultiLine;
end;

function TFileViewNotebook.GetOptions: TCTabControlOptions;
begin
  Result:= FPageControl.Options;
end;

function TFileViewNotebook.GetPage(Index: Integer): TFileViewPage;
var
  APage: PtrInt absolute Result;
begin
  APage:= FPageControl.Page[Index].Tag;
end;

function TFileViewNotebook.AddPage: TFileViewPage;
begin
  Result := InsertPage(PageCount);
end;

function TFileViewNotebook.InsertPage(Index: Integer): TFileViewPage;
var
  ATag: PtrInt absolute Result;
begin
  Result:= TFileViewPage.Create(Self);

  FPageControl.Tabs.Insert(Index, '');

  FPageControl.Page[Index].Tag:= ATag;

  Result.Parent:= Self;

  Result.BringToFront;

  Result.AnchorAsAlign(alClient, 0);

  Result.Visible:= (PageIndex = Index);

  ShowTabs:= ((PageCount > 1) or (tb_always_visible in gDirTabOptions)) and gDirectoryTabs;

  FPageControl.TabControlBoundsChange(0);
end;

function TFileViewNotebook.NewEmptyPage: TFileViewPage;
begin
  if tb_open_new_near_current in gDirTabOptions then
    Result := InsertPage(PageIndex + 1)
  else
    Result := InsertPage(PageCount);
end;

function TFileViewNotebook.NewPage(CloneFromPage: TFileViewPage): TFileViewPage;
begin
  if Assigned(CloneFromPage) then
  begin
    Result := NewEmptyPage;
    Result.AssignPage(CloneFromPage);
  end
  else
    Result := nil;
end;

function TFileViewNotebook.NewPage(CloneFromView: TFileView): TFileViewPage;
begin
  if Assigned(CloneFromView) then
  begin
    Result := NewEmptyPage;
    CloneFromView.Clone(Result);
  end
  else
    Result := nil;
end;

procedure TFileViewNotebook.DeletePage(Index: Integer);
var
  APage: TFileViewPage;
begin
  APage:= GetPage(Index);
  FPageControl.Pages[Index].Free;
  APage.Free;
end;

procedure TFileViewNotebook.RemovePage(Index: Integer);
begin
{$IFDEF LCLGTK2}
  // If removing currently active page, switch to another page first.
  // Otherwise there can be no page selected.
  if (PageIndex = Index) and (PageCount > 1) then
  begin
    if Index = PageCount - 1 then
      Page[Index - 1].MakeActive
    else
      Page[Index + 1].MakeActive;
  end;
{$ENDIF}

  DeletePage(Index);

  ShowTabs:= ((PageCount > 1) or (tb_always_visible in gDirTabOptions)) and gDirectoryTabs;

{$IFNDEF LCLGTK2}
  // Force-activate current page.
  if PageIndex <> -1 then
    Page[PageIndex].MakeActive;
{$ENDIF}
end;

procedure TFileViewNotebook.RemovePage(var APage: TFileViewPage);
begin
  RemovePage(APage.PageIndex);
  APage := nil;
end;

procedure TFileViewNotebook.DestroyAllPages;
begin
  while PageCount > 0 do DeletePage(0);
end;

procedure TFileViewNotebook.ActivatePrevTab;
begin
  if PageIndex = 0 then
    Page[PageCount - 1].MakeActive
  else
    Page[PageIndex - 1].MakeActive;
end;

procedure TFileViewNotebook.ActivateNextTab;
begin
  if PageIndex = PageCount - 1 then
    Page[0].MakeActive
  else
    Page[PageIndex + 1].MakeActive;
end;

function TFileViewNotebook.GetCapabilities: TCTabControlCapabilities;
begin
  Result:= FPageControl.GetCapabilities;
end;

function TFileViewNotebook.IndexOfPageAt(P: TPoint): Integer;
begin
  P:= ClientToScreen(P);
  Result:= FPageControl.IndexOfPageAt(FPageControl.ScreenToClient(P));
end;

function TFileViewNotebook.GetPageCount: Integer;
begin
  Result:= FPageControl.PageCount;
end;

function TFileViewNotebook.GetShowTabs: Boolean;
begin
  Result:= FPageControl.Visible;
end;

function TFileViewNotebook.GetPageIndex: Integer;
begin
  Result:= FPageControl.PageIndex;
end;

function TFileViewNotebook.GetTabPosition: TTabPosition;
begin
  Result:= FPageControl.TabPosition;
end;

procedure TFileViewNotebook.SetMultiLine(AValue: Boolean);
begin
  FPageControl.MultiLine:= AValue;
  Application.QueueAsyncCall(@FPageControl.TabControlBoundsChange, 0);
end;

procedure TFileViewNotebook.SetOptions(AValue: TCTabControlOptions);
begin
  FPageControl.Options:= AValue;
  Application.QueueAsyncCall(@FPageControl.TabControlBoundsChange, 0);
end;

procedure TFileViewNotebook.SetShowTabs(AValue: Boolean);
begin
  if (FPageControl.Visible <> AValue) then
  begin
    FPageControl.Visible:= AValue;
    Application.QueueAsyncCall(@FPageControl.TabControlBoundsChange, 0);
  end;
end;

procedure TFileViewNotebook.SetPageIndex(AValue: Integer);
begin
  FPageControl.PageIndex:= AValue;
end;

procedure TFileViewNotebook.SetTabPosition(AValue: TTabPosition);
begin
  if FPageControl.TabPosition <> AValue then
  begin
    FPageControl.TabPosition:= AValue;
{$IF DEFINED(LCLWIN32) or DEFINED(LCLCARBON)}
    // Fix Z-order, it's wrong when only one tab
    if PageCount = 1 then RecreateWnd(Self);
{$ENDIF}
    Application.QueueAsyncCall(@FPageControl.TabControlBoundsChange, 0);
  end;
end;

procedure TFileViewNotebook.DoChange;
var
  Index: Integer;
  APage: TFileViewPage;
begin
  if Assigned(FOnPageChanged) then
    FOnPageChanged(Self);

  for Index:= 0 to PageCount - 1 do
  begin
    APage:= GetPage(Index);
    if Assigned(APage) then
    begin
      if Index <> PageIndex then
        APage.Hide
      else begin
        APage.Show;
      end;
    end;
  end;

  ActivePage.DoActivate;
end;

procedure TFileViewNotebook.UpdatePagePosition(AIndex, ASpacing: Integer);
begin
  with Page[AIndex] do
  begin
    case FPageControl.TabPosition of
      tpTop:
        begin
          BorderSpacing.Bottom:= 0;
          BorderSpacing.Top:= ASpacing;
        end;
      tpBottom:
        begin
          BorderSpacing.Top:= 0;
          BorderSpacing.Bottom:= ASpacing;
        end;
    end;
    BringToFront;
  end;
end;

procedure TFileViewNotebook.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  FPageControl.TabControlBoundsChange(0);
end;

procedure TFileViewNotebook.ActivateTabByIndex(Index: Integer);
begin
  if Index < -1 then
    Exit;
  if Index = -1 then
    Page[PageCount - 1].MakeActive
  else if PageCount >= Index + 1 then
    Page[Index].MakeActive;
end;

procedure TFileViewNotebook.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPageControl.FLastMouseDownPageIndex:= -1;
  inherited MouseDown(Button, Shift, X, Y);
end;

end.

