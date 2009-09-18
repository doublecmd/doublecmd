unit uFileViewNotebook; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,
  uFileView, uFilePanelSelect;

type

  TTabLockState = (tlsNormal,          //<en Default state.
                   tlsLockedPath,      //<en Path changes are not allowed.
                   tlsResettingPath);  //<en Path is reset when activating page.

  TFileViewNotebook = class;

  { TFileViewPage }

  TFileViewPage = class(TCustomPage)
  private
    FLockState: TTabLockState;
    FLockPath: String;          //<en Path on which tab is locked

    {en
       Shows or removes the '*' indicator of a locked tab.
    }
    procedure UpdateTabLockState;
    {en
       Retrieves the file view on this page.
    }
    function GetFileView: TFileView;
    {en
       Frees current file view and assigns a new one.
    }
    procedure SetFileView(aFileView: TFileView);
    {en
       Retrieves notebook on which this page is.
    }
    function GetNotebook: TFileViewNotebook;

    procedure SetLockState(NewLockState: TTabLockState);

  public
    constructor Create(TheOwner: TComponent); override;

    procedure MakeActive;
    procedure UpdateCaption(NewCaption: String);

    property LockState: TTabLockState read FLockState write SetLockState;
    property LockPath: String read FLockPath write FLockPath;
    property FileView: TFileView read GetFileView write SetFileView;
    property Notebook: TFileViewNotebook read GetNotebook;

  end;

  { TFileViewNotebook }

  TFileViewNotebook = class(TCustomNotebook)
  private
    FNotebookSide: TFilePanelSelect;

    function GetActivePage: TFileViewPage;
    function GetActiveView: TFileView;
    function GetFileViewOnPage(Index: Integer): TFileView;
    function GetPage(Index: Integer): TFileViewPage;

    procedure SetMultilineTabs(Multiline: Boolean);

  public
    constructor Create(ParentControl: TWinControl;
                       NotebookSide: TFilePanelSelect); reintroduce;

    function AddPage(aCaption: String = ''): TFileViewPage;
    procedure RemovePage(Index: Integer);
    procedure RemovePage(var aPage: TFileViewPage);
    procedure ActivatePrevTab;
    procedure ActivateNextTab;

    property ActivePage: TFileViewPage read GetActivePage;
    property ActiveView: TFileView read GetActiveView;
    property Page[Index: Integer]: TFileViewPage read GetPage;
    property View[Index: Integer]: TFileView read GetFileViewOnPage; default;
    property Side: TFilePanelSelect read FNotebookSide;

  published
    property OnMouseDown;
    property OnMouseUp;
    property MultilineTabs: Boolean write SetMultilineTabs;
  end;

implementation

uses
  WSExtCtrls,
  fMain, uGlobs;

// -- TFileViewPage -----------------------------------------------------------

constructor TFileViewPage.Create(TheOwner: TComponent);
begin
  FLockState := tlsNormal;
  inherited Create(TheOwner);
end;

procedure TFileViewPage.MakeActive;
var
  aFileView: TFileView;
begin
  Notebook.PageIndex := PageIndex;

  aFileView := FileView;
  if Assigned(aFileView) then
    aFileView.SetFocus;
end;

procedure TFileViewPage.UpdateCaption(NewCaption: String);
begin
  if NewCaption <> '' then
  begin
    if (tb_text_length_limit in gDirTabOptions) and (Length(NewCaption) > gDirTabLimit) then
      Caption := Copy(NewCaption, 1, gDirTabLimit) + '...'
    else
      Caption := NewCaption;
  end;
end;

procedure TFileViewPage.UpdateTabLockState;
var
  NewCaption: String;
begin
  if Caption[1] = '*' then
    NewCaption := Copy(Caption, 2, Length(Caption) - 1)
  else
    NewCaption := Caption;

  if (FLockState <> tlsNormal) and (tb_show_asterisk_for_locked in gDirTabOptions) then
    Caption := '*' + NewCaption
  else
    Caption := NewCaption;
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
  while ComponentCount > 0 do
  begin
    aComponent := Components[0];
    RemoveComponent(aComponent);
    aComponent.Free;
  end;

  if Assigned(aFileView) then
  begin
    InsertComponent(aFileView);
    aFileView.Parent := Self;
  end;
end;

function TFileViewPage.GetNotebook: TFileViewNotebook;
begin
  Result := Parent as TFileViewNotebook;
end;

procedure TFileViewPage.SetLockState(NewLockState: TTabLockState);
begin
  FLockState := NewLockState;
  UpdateTabLockState;
end;

// -- TFileViewNotebook -------------------------------------------------------

constructor TFileViewNotebook.Create(ParentControl: TWinControl;
                                     NotebookSide: TFilePanelSelect);
begin
  PageClass := TFileViewPage;
  inherited Create(ParentControl);

  Parent := ParentControl;
  Align := alClient;
  TabStop := False;

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

function TFileViewNotebook.GetPage(Index: Integer): TFileViewPage;
begin
  Result := TFileViewPage(CustomPage(Index));
end;

procedure TFileViewNotebook.SetMultilineTabs(Multiline: Boolean);
begin
  if (nbcMultiline in GetCapabilities) and
      // If different then current setting
     (Multiline <> (nboMultiline in Options)) then
  begin
    if Multiline then
      Options := Options + [nboMultiLine]
    else
      Options := Options - [nboMultiLine];

    // Workaround: nboMultiline property is currently not updated by LCL.
    // Force update and realign all pages.

    TWSCustomNotebookClass(Self.WidgetSetClass).UpdateProperties(Self);

    if ClientRectNeedsInterfaceUpdate then
    begin
      // Change sizes of pages, because multiline tabs may
      // take up different amount of space than single line.
      InvalidateClientRectCache(True);
      ReAlign;
    end;
  end;
end;

function TFileViewNotebook.AddPage(aCaption: String): TFileViewPage;
var
  PageNr: Integer;
begin
  if aCaption = '' then
    aCaption := IntToStr(PageCount);

  PageNr := Pages.Add(aCaption);
  Result := GetPage(PageNr);

  ShowTabs:= ((PageCount > 1) or (tb_always_visible in gDirTabOptions)) and gDirectoryTabs;
end;

procedure TFileViewNotebook.RemovePage(Index: Integer);
begin
{$IFDEF LCLGTK2}
  // If removing currently active page, switch to another page first.
  // Otherwise there can be no page selected.
  if PageIndex = Index then
    ActivateNextTab;
{$ENDIF}

  Pages.Delete(Index);

  if (nboMultiLine in Options) and
     ClientRectNeedsInterfaceUpdate then
  begin
    // The height of the tabs (nr of lines) has changed.
    // Recalculate size of each page.
    InvalidateClientRectCache(False);
    ReAlign;
  end;

  ShowTabs:= ((PageCount > 1) or (tb_always_visible in gDirTabOptions)) and gDirectoryTabs;
end;

procedure TFileViewNotebook.RemovePage(var aPage: TFileViewPage);
begin
  RemovePage(aPage.PageIndex);
  aPage := nil;
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

end.

