unit uFileViewNotebook; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,
  uFileView;

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
       Retrieves notebook on which this page is.
    }
    function GetNotebook: TFileViewNotebook;

    procedure SetLockState(NewLockState: TTabLockState);

  public
    constructor Create(TheOwner: TComponent); override;

    property LockState: TTabLockState read FLockState write SetLockState;
    property LockPath: String read FLockPath write FLockPath;
    property FileView: TFileView read GetFileView;
    property Notebook: TFileViewNotebook read GetNotebook;

  end;

  { TFileViewNotebook }

  TFileViewNotebook = class(TCustomNotebook)
  private
    function GetActivePage: TFileViewPage;
    function GetActiveView: TFileView;
    function GetFileViewOnPage(Index: Integer): TFileView;
    function GetPage(Index: Integer): TFileViewPage;

    procedure SetMultilineTabs(Multiline: Boolean);

    procedure CloseTabClickedEvent(Sender: TObject);

  public
    constructor Create(ParentControl: TWinControl); reintroduce;

    function AddPage: TFileViewPage;

    property ActivePage: TFileViewPage read GetActivePage;
    property ActiveView: TFileView read GetActiveView;
    property Page[Index: Integer]: TFileViewPage read GetPage;
    property View[Index: Integer]: TFileView read GetFileViewOnPage; default;

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

constructor TFileViewNotebook.Create(ParentControl: TWinControl);
begin
  PageClass := TFileViewPage;
  inherited Create(ParentControl);

  Parent := ParentControl;
  Align := alClient;
  TabStop := False;

  OnCloseTabClicked := @CloseTabClickedEvent;
  OnMouseDown := @frmMain.nbPageMouseDown;
  OnMouseUp := @frmMain.nbPageMouseUp;
  OnPageChanged := @frmMain.nbPageChanged;
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

procedure TFileViewNotebook.CloseTabClickedEvent(Sender: TObject);
begin
  with (Sender As TFileViewPage) do
  if PageIndex <> -1 then
  begin
    //RemovePage(Parent as TFileViewNotebook, PageIndex);
  end;
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

function TFileViewNotebook.AddPage: TFileViewPage;
var
  x: Integer;
begin
  x := PageCount;

  Pages.Add(IntToStr(x));

//  if bSetActive then
//    ANoteBook.ActivePage := IntToStr(x);

  Result := TFileViewPage(CustomPage(x));

  ShowTabs:= ((PageCount > 1) or (tb_always_visible in gDirTabOptions)) and gDirectoryTabs;
end;

end.

