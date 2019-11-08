{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains TFileViewPage and TFileViewNotebook objects.

   Copyright (C) 2016-2019 Alexander Koblov (alexx2000@mail.ru)

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

  TFileViewPage = class(TTabSheet)
  private
    FLockState: TTabLockState;
    FLockPath: String;          //<en Path on which tab is locked
    FOnActivate: TNotifyEvent;
    FCurrentTitle: String;
    FPermanentTitle: String;
    FBackupColumnSet: String;
    FOnChangeFileView: TNotifyEvent;
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
    {en
       Frees current file view and assigns a new one.
    }
    procedure SetFileView(aFileView: TFileView);
    procedure SetLockState(NewLockState: TTabLockState);
    procedure SetPermanentTitle(AValue: String);

    procedure DoActivate;

  protected
    procedure PaintWindow(DC: HDC); override;
{$IF DEFINED(MSWINDOWS)}
    procedure RealSetText(const AValue: TCaption); override;
{$ENDIF}
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  public
    constructor Create(TheOwner: TComponent); override;

    function IsActive: Boolean;
    procedure MakeActive;
    procedure UpdateTitle;

    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);

    property LockState: TTabLockState read FLockState write SetLockState;
    property LockPath: String read FLockPath write FLockPath;
    property FileView: TFileView read GetFileView write SetFileView;
    property Notebook: TFileViewNotebook read GetNotebook;
    property PermanentTitle: String read FPermanentTitle write SetPermanentTitle;
    property CurrentTitle: String read FCurrentTitle;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property BackupColumnSet: String read FBackupColumnSet write FBackupColumnSet;
    property BackupViewClass: TFileViewClass read FBackupViewClass write FBackupViewClass;
    property OnChangeFileView: TNotifyEvent read FOnChangeFileView write FOnChangeFileView;
  end;

  { TFileViewNotebook }

  TFileViewNotebook = class(TPageControl)
  private
    FNotebookSide: TFilePanelSelect;
    FStartDrag: Boolean;
    FDraggedPageIndex: Integer;
    FHintPageIndex: Integer;
    FLastMouseDownTime: TDateTime;
    FLastMouseDownPageIndex: Integer;

    function GetActivePage: TFileViewPage;
    function GetActiveView: TFileView;
    function GetFileViewOnPage(Index: Integer): TFileView;
    function GetPage(Index: Integer): TFileViewPage; reintroduce;

  protected
    procedure DoChange; override;
    function GetPageClass: TCustomPageClass; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  public
    constructor Create(ParentControl: TWinControl;
                       NotebookSide: TFilePanelSelect); reintroduce;
{$IFDEF MSWINDOWS}
    {en
       Removes the rectangle of the pages contents from erasing background to reduce flickering.
       This is not needed on non-Windows because EraseBackground is not used there.
    }
    procedure EraseBackground(DC: HDC); override;
    procedure WndProc(var Message: TLMessage); override;
{$ENDIF}
    function AddPage: TFileViewPage;
    function InsertPage(Index: Integer): TFileViewPage; reintroduce;
    function NewEmptyPage: TFileViewPage;
    function NewPage(CloneFromPage: TFileViewPage): TFileViewPage;
    function NewPage(CloneFromView: TFileView): TFileViewPage;
    procedure RemovePage(Index: Integer); reintroduce;
    procedure RemovePage(var aPage: TFileViewPage);
    procedure DestroyAllPages;
    procedure ActivatePrevTab;
    procedure ActivateNextTab;
    procedure ActivateTabByIndex(Index: Integer);
    function IndexOfPageAt(P: TPoint): Integer; override;

    procedure DragDrop(Source: TObject; X,Y: Integer); override;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState;
                       var Accept: Boolean); override;

    property ActivePage: TFileViewPage read GetActivePage;
    property ActiveView: TFileView read GetActiveView;
    property DoubleClickPageIndex: Integer read FLastMouseDownPageIndex;
    property Page[Index: Integer]: TFileViewPage read GetPage;
    property View[Index: Integer]: TFileView read GetFileViewOnPage; default;
    property Side: TFilePanelSelect read FNotebookSide;

  published
    property OnDblClick;
    property OnChange;
    property OnMouseDown;
    property OnMouseUp;
  end;

implementation

uses
  LCLIntf,
  LazUTF8,
  DCStrUtils,
  uGlobs,
  uColumnsFileView,
  uArchiveFileSource
  {$IF DEFINED(LCLGTK2)}
  , Glib2, Gtk2
  {$ENDIF}
  {$IF DEFINED(MSWINDOWS)}
  , Win32Proc, Windows, Messages
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
end;

{$IF DEFINED(MSWINDOWS)}
procedure TFileViewPage.RealSetText(const AValue: TCaption);
begin
  inherited RealSetText(AValue);
  if HandleAllocated then
    LCLControlSizeNeedsUpdate(Parent, True);
end;
{$ENDIF}

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
    if Assigned(FOnChangeFileView) then
      FOnChangeFileView(aFileView);
  end;
end;

function TFileViewPage.GetNotebook: TFileViewNotebook;
begin
  Result := Parent as TFileViewNotebook;
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

// -- TFileViewNotebook -------------------------------------------------------

constructor TFileViewNotebook.Create(ParentControl: TWinControl;
                                     NotebookSide: TFilePanelSelect);
begin
  inherited Create(ParentControl);
  ControlStyle := ControlStyle + [csNoFocus];

  Parent := ParentControl;
  TabStop := False;
  ShowHint := True;

  FHintPageIndex := -1;
  FNotebookSide := NotebookSide;
  FStartDrag := False;

  {$IFDEF MSWINDOWS}
  // The pages contents are removed from drawing background in EraseBackground.
  // But double buffering could be enabled to eliminate flickering of drawing
  // the tabs buttons themselves. But currently there's a bug where the buffer
  // bitmap is temporarily drawn in different position, probably at (0,0) and
  // not where pages contents start (after applying TCM_ADJUSTRECT).
  //DoubleBuffered := True;
  {$ENDIF}
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

function TFileViewNotebook.GetPage(Index: Integer): TFileViewPage;
begin
  Result := TFileViewPage(CustomPage(Index));
end;

function TFileViewNotebook.AddPage: TFileViewPage;
begin
  Result := InsertPage(PageCount);
end;

function TFileViewNotebook.InsertPage(Index: Integer): TFileViewPage;
begin
  Tabs.Insert(Index, '');
  Result := GetPage(Index);
  ShowTabs:= ((PageCount > 1) or (tb_always_visible in gDirTabOptions)) and gDirectoryTabs;
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

  Page[Index].Free;

  ShowTabs:= ((PageCount > 1) or (tb_always_visible in gDirTabOptions)) and gDirectoryTabs;

{$IFNDEF LCLGTK2}
  // Force-activate current page.
  if PageIndex <> -1 then
    Page[PageIndex].MakeActive;
{$ENDIF}
end;

procedure TFileViewNotebook.RemovePage(var aPage: TFileViewPage);
begin
  RemovePage(aPage.PageIndex);
  aPage := nil;
end;

procedure TFileViewNotebook.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  inherited WMEraseBkgnd(Message);
  // Always set as handled otherwise if not handled Windows will draw background
  // with hbrBackground brush of the window class. This might cause flickering
  // because later background will be again be erased but with TControl.Brush.
  // This is not actually needed on non-Windows because WMEraseBkgnd is not used there.
  Message.Result := 1;
end;

procedure TFileViewNotebook.DestroyAllPages;
var
   tPage:TFileViewPage;
begin
  while PageCount > 0 do
  begin
    tPage:=Page[0];
    if tPage<>nil then FreeAndNil(tPage);
  end;
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

procedure TFileViewNotebook.ActivateTabByIndex(Index: Integer);
begin
  if Index < -1 then
    Exit;
  if Index = -1 then
    Page[PageCount - 1].MakeActive
  else if PageCount >= Index + 1 then
    Page[Index].MakeActive;
end;

function TFileViewNotebook.IndexOfPageAt(P: TPoint): Integer;
begin
  Result:= inherited IndexOfPageAt(P);
  if (Result >= PageCount) then Result:= -1;
end;

procedure TFileViewNotebook.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{$IF DEFINED(LCLGTK2)}
var
  ArrowWidth: Integer;
  arrow_spacing: gint = 0;
  scroll_arrow_hlength: gint = 16;
{$ENDIF}
begin
  inherited;

  if Button = mbLeft then
  begin
    FDraggedPageIndex := IndexOfPageAt(Classes.Point(X, Y));
    FStartDrag := (FDraggedPageIndex <> -1);
  end;
  // Emulate double click
  if (Button = mbLeft) and Assigned(OnDblClick) then
    begin
      if ((Now - FLastMouseDownTime) > ((1/86400)*(GetDoubleClickTime/1000))) then
        begin
          FLastMouseDownTime:= Now;
          FLastMouseDownPageIndex:= FDraggedPageIndex;
        end
      else if (FDraggedPageIndex = FLastMouseDownPageIndex) then
        begin
          {$IF DEFINED(LCLGTK2)}
          gtk_widget_style_get(PGtkWidget(Self.Handle),
                               'arrow-spacing', @arrow_spacing,
                               'scroll-arrow-hlength', @scroll_arrow_hlength,
                               nil);
          ArrowWidth:= arrow_spacing + scroll_arrow_hlength;
          if (X > ArrowWidth) and (X < ClientWidth - ArrowWidth) then
          {$ENDIF}
          OnDblClick(Self);
          FStartDrag:= False;
          FLastMouseDownTime:= 0;
          FLastMouseDownPageIndex:= -1;
        end;
    end;
end;

procedure TFileViewNotebook.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ATabIndex: Integer;
begin
  inherited;

  if ShowHint then
  begin
    ATabIndex := IndexOfPageAt(Classes.Point(X, Y));
    if (ATabIndex >= 0) and (ATabIndex <> FHintPageIndex) then
    begin
      FHintPageIndex := ATabIndex;
      Application.CancelHint;
      if (ATabIndex <> PageIndex) and (Length(Page[ATabIndex].LockPath) <> 0) then
        Hint := Page[ATabIndex].LockPath
      else
        Hint := View[ATabIndex].CurrentPath;
    end;
  end;

  if FStartDrag then
  begin
    FStartDrag := False;
    BeginDrag(False);
  end;
end;

procedure TFileViewNotebook.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FStartDrag := False;
end;

procedure TFileViewNotebook.DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
var
  ATabIndex: Integer;
begin
  if (Source is TFileViewNotebook) then
  begin
    ATabIndex := IndexOfPageAt(Classes.Point(X, Y));
    Accept := (Source <> Self) or
              ((ATabIndex <> -1) and (ATabIndex <> FDraggedPageIndex));
  end
  else begin
    inherited DragOver(Source, X, Y, State, Accept);
  end;
end;

{$IFDEF MSWINDOWS}
procedure TFileViewNotebook.EraseBackground(DC: HDC);
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

procedure TFileViewNotebook.WndProc(var Message: TLMessage);
begin
  inherited WndProc(Message);
  if Message.Msg = TCM_ADJUSTRECT then
  begin
    if Message.WParam = 0 then
      PRect(Message.LParam)^.Left := PRect(Message.LParam)^.Left - 2
    else begin
      PRect(Message.LParam)^.Left := PRect(Message.LParam)^.Left + 2;
    end;
  end;
end;
{$ENDIF}

procedure TFileViewNotebook.DragDrop(Source: TObject; X,Y: Integer);
var
  ATabIndex: Integer;
  SourceNotebook: TFileViewNotebook;
  ANewPage, DraggedPage: TFileViewPage;
begin
  if (Source is TFileViewNotebook) then
  begin
    SourceNotebook := TFileViewNotebook(Source);
    ATabIndex := IndexOfPageAt(Classes.Point(X, Y));

    if Source = Self then
    begin
      // Move within the same panel.
      if ATabIndex <> -1 then
        Tabs.Move(FDraggedPageIndex, ATabIndex);
    end
    else if (SourceNotebook.FDraggedPageIndex < SourceNotebook.PageCount) then
    begin
      // Move page between panels.
      DraggedPage := SourceNotebook.Page[SourceNotebook.FDraggedPageIndex];

      if ATabIndex = -1 then
        ATabIndex := PageCount;

      // Create a clone of the page in the panel.
      ANewPage := InsertPage(ATabIndex);
      ANewPage.AssignPage(DraggedPage);
      ANewPage.MakeActive;

      if (ssShift in GetKeyShiftState) and (SourceNotebook.PageCount > 1) then
      begin
        // Remove page from source panel.
        SourceNotebook.RemovePage(DraggedPage);
      end;
    end;
  end
  else begin
    inherited DragDrop(Source, X, Y);
  end;
end;

procedure TFileViewNotebook.DoChange;
begin
  inherited DoChange;
  ActivePage.DoActivate;
end;

function TFileViewNotebook.GetPageClass: TCustomPageClass;
begin
  Result:= TFileViewPage;
end;

end.

