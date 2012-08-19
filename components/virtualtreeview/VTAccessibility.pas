unit VTAccessibility;

// This unit implements iAccessible interfaces for the VirtualTree visual components
// and the currently focused node.
//
// Written by Marco Zehe. (c) 2007

interface

uses Windows, Classes, ActiveX, oleacc, VirtualTrees, VTAccessibilityFactory, Controls;

type
  TVirtualTreeAccessibility = class(TInterfacedObject, IDispatch, IAccessible)
  private
    FVirtualTree: TVirtualStringTree;
  public
    { IAccessibility }
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult; stdcall;
    function Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant;
                              out pidTopic: Integer): HResult; stdcall;
    function Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult; stdcall;
    function Get_accFocus(out pvarChild: OleVariant): HResult; stdcall;
    function Get_accSelection(out pvarChildren: OleVariant): HResult; stdcall;
    function Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer;
                         out pcyHeight: Integer; varChild: OleVariant): HResult; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult; stdcall;
    function accDoDefaultAction(varChild: OleVariant): HResult; stdcall;
    function Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
    function Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult; stdcall;
    {IDispatch}
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount: Integer; LocaleID: Integer; DispIDs: Pointer): HRESULT; stdcall;
    function GetTypeInfo(Index: Integer; LocaleID: Integer;
      out TypeInfo): HRESULT; stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult: Pointer; ExcepInfo: Pointer;
      ArgErr: Pointer): HRESULT; stdcall;
    constructor Create(VirtualTree: TVirtualStringTree);
  end;

  TVirtualTreeItemAccessibility = class(TVirtualTreeAccessibility, IAccessible)
    public
    { IAccessibility }
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer;
      out pyTop: Integer; out pcxWidth: Integer;
      out pcyHeight: Integer; varChild: OleVariant): HResult; stdcall;
    constructor Create(VirtualTree: TVirtualStringTree);
  end;

  TVTMultiColumnItemAccessibility = class(TVirtualTreeItemAccessibility, IAccessible)
    private
    function GetItemDescription(varChild: OleVariant; out pszDescription: WideString; IncludeMainColumn: boolean): HResult; stdcall;
    public
    { IAccessibility }
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
  end;

  TVTDefaultAccessibleProvider = class(TInterfacedObject, IVTAccessibleProvider)
    function CreateIAccessible(ATree: TBaseVirtualTree): IAccessible;
  end;

  TVTDefaultAccessibleItemProvider = class(TInterfacedObject, IVTAccessibleProvider)
    function CreateIAccessible(ATree: TBaseVirtualTree): IAccessible;
  end;

  TVTMultiColumnAccessibleItemProvider = class(TInterfacedObject, IVTAccessibleProvider)
    function CreateIAccessible(ATree: TBaseVirtualTree): IAccessible;
  end;

implementation

uses Variants, SysUtils, Types, Forms;

{ TVirtualTreeAccessibility }
//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeAccessibility.accDoDefaultAction(varChild: OleVariant): HResult;
// a default action is not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;
//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeAccessibility.accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult;
// returns the iAccessible object at the given point, if applicable.
var
  Pt: TPoint;
  HitInfo: THitInfo;
begin
  Result := S_FALSE;
  if FVirtualTree <> nil then
  begin
//    VariantInit(pvarChild);
//    TVarData(pvarChild).VType := VT_I4;
    Pt := fVirtualTree.ScreenToClient(Point(xLeft, yTop));
    if fVirtualTree.FocusedNode <> nil then
    begin
      fVirtualTree.GetHitTestInfoAt(xLeft, yTop, false, HitInfo);
      if FVirtualTree.FocusedNode = HitInfo.HitNode then
      begin
        pvarChild := FVirtualTree.AccessibleItem;
        Result := S_OK;
        exit;
      end;
    end;
    if PtInRect(FVirtualTree.BoundsRect, Pt) then
    begin
      pvarChild := CHILDID_SELF;
      Result := S_OK;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.accLocation(out pxLeft: Integer;
  out pyTop: Integer; out pcxWidth: Integer;
  out pcyHeight: Integer; varChild: OleVariant): HResult;
// returns the location of the VirtualStringTree object.
var
  P: TPoint;
begin
  Result := S_FALSE;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree <> nil then
    begin
      P := FVirtualTree.ClientToScreen(FVirtualTree.ClientRect.TopLeft);
      pxLeft := P.X;
      pyTop := P.Y;
      pcxWidth := FVirtualTree.Width;
      pcyHeight := FVirtualTree.Height;
      Result := S_OK;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.accNavigate(navDir: Integer; varStart: OleVariant;
  out pvarEndUpAt: OleVariant): HResult;
// This is not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accSelection(out pvarChildren: OleVariant): HResult;
// returns the selected child ID, if any.
begin
  Result := s_false;
  if FVirtualTree <> nil then
    if fVirtualTree.FocusedNode <> nil then
    begin
      pvarChildren := 1;
      result := s_OK;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------
constructor TVirtualTreeAccessibility.Create(VirtualTree: TVirtualStringTree);
// assigns the parent and current fields, and lets the control's iAccessible object know its address.
begin
  fVirtualTree := VirtualTree;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.GetIDsOfNames(const IID: TGUID;
  Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT;
// Not supported.
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HRESULT;
// not supported.
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.GetTypeInfoCount(
  out Count: Integer): HRESULT;
// not supported.
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult;
// returns the iAccessible child, whicfh represents the focused item.
begin
  if varChild = CHILDID_SELF then
  begin
    ppdispChild := FVirtualTree.AccessibleItem;
    Result := S_OK;
  end
  else
    Result := E_INVALIDARG
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accChildCount(out pcountChildren: Integer): HResult;
// Returns the number 1 for the one child: The focused item.
begin
  pcountChildren := 1;
  Result := S_OK;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult;
// Not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult;
// returns the hint of the control, if assigned.
begin
  pszDescription := '';
  Result := S_FALSE;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree <> nil then
      pszDescription := GetLongHint(fVirtualTree.Hint);
  end;
  if Length(pszDescription) > 0 then
    Result := S_OK;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accFocus(out pvarChild: OleVariant): HResult;
// returns the child ID of 1, if assigned.
begin
  Result := s_false;
  if fVirtualTree <> nil then
  begin
    if FVirtualTree.FocusedNode <> nil then
    begin
      pvarChild := fVirtualTree.AccessibleItem;
      result := s_OK;
    end
    else begin
      pvarChild := childid_self;
      result := S_OK;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult;
// Not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant;
                          out pidTopic: Integer): HResult;
// Returns the HelpContext ID, if present. 
begin
  pszHelpFile := '';
  pidTopic := 0;
  Result := S_OK;
  if varChild = CHILDID_SELF then
    if FVirtualTree <> nil then
    begin
      pszHelpFile := Application.HelpFile;
      pidTopic := FVirtualTree.HelpContext;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult;
// Not supported.
begin
  pszKeyboardShortcut := '';
  Result := S_FALSE;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accName(varChild: OleVariant; out pszName: WideString): HResult;
// if set, returns the new published AccessibleName property.
// otherwise, returns the default text.
begin
  pszName := '';
  Result := S_FALSE;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree <> nil then
    begin
      if FVirtualTree.AccessibleName <> '' then
        pszName := FVirtualTree.AccessibleName
      else
        PSZName := FVirtualTree.DefaultText;
      result := S_OK;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accParent(out ppdispParent: IDispatch): HResult;
// Returns false, the tree itself does not have a parent.
begin
  ppdispParent := nil;
  Result := S_FALSE;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult;
// tells MSAA that it is a TreeView.
begin
  Result := S_OK;
//  VariantInit(pvarRole);
//  TVarData(pvarRole).VType := VT_I4;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree <> nil then
      pvarRole := ROLE_SYSTEM_OUTLINE
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.accSelect(flagsSelect: Integer; varChild: OleVariant): HResult;
// since we're not supporting more than one item, this is not supported currently.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult;
// returns the state of the control.
const
  IsEnabled: array[Boolean] of Integer = (STATE_SYSTEM_UNAVAILABLE, 0);
  HasPopup: array[Boolean] of Integer = (0, STATE_SYSTEM_HASPOPUP);
  IsVisible: array[Boolean] of Integer = (STATE_SYSTEM_INVISIBLE, 0);
begin
  Result := S_OK;
//  VariantInit(pvarState);
//  TVarData(pvarState).VType := VT_I4;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree <> nil then
    begin
      pvarState := STATE_SYSTEM_FOCUSED or STATE_SYSTEM_FOCUSABLE or STATE_SYSTEM_HOTTRACKED;
      pvarState := pvarState or IsVisible[FVirtualTree.Visible];
      pvarState := pvarState or IsEnabled[FVirtualTree.Enabled];
    end
    else
      Result := E_INVALIDARG;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult;
// the TreeView control itself does not have a value, returning false here.
begin
  pszValue := '';
  Result := S_FALSE;//DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HRESULT;
// not supported.
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
// not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeAccessibility.Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult;
// not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND
end;

{ TVirtualTreeItemAccessibility }

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeItemAccessibility.accLocation(out pxLeft, pyTop, pcxWidth,
  pcyHeight: Integer; varChild: OleVariant): HResult;
// returns the location of the current accessible item.
var
  P: TPoint;
  DisplayRect: TRect;
begin
  Result := S_FALSE;
  if varChild = CHILDID_SELF then
  begin
   if FVirtualTree.FocusedNode <> nil then
    begin
      DisplayRect := FVirtualTree.GetDisplayRect(FVirtualTree.FocusedNode, -1, TRUE, FALSE);
      P := FVirtualTree.ClientToScreen(DisplayRect.TopLeft);
      pxLeft := P.X;
      pyTop := P.Y;
      pcxWidth := DisplayRect.Right - DisplayRect.Left;
      pcyHeight := DisplayRect.Bottom - DisplayRect.Top;
      Result := S_OK;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
constructor TVirtualTreeItemAccessibility.Create(VirtualTree: TVirtualStringTree);
// sets up the parent/child relationship.
begin
  fVirtualTree := VirtualTree;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeItemAccessibility.Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult;
// the item does not have children. Returning false.
begin
  ppdispChild := nil;
  Result := S_FALSE;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeItemAccessibility.Get_accChildCount(out pcountChildren: Integer): HResult;
// the item itself does not have children, returning 0.
begin
  pcountChildren := 0;
  Result := S_OK;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeItemAccessibility.Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult;
// not supported for an item.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeItemAccessibility.Get_accName(varChild: OleVariant; out pszName: WideString): HResult;
// the name is the node's caption.
begin
  pszName := '';
  Result := S_FALSE;
  if varChild = childid_self then
  begin
    if FVirtualTree <> nil then
      if FVirtualTree.FocusedNode <> nil then
      begin
        pszName := FVirtualTree.Text[FVirtualTree.FocusedNode, FVirtualTree.Header.MainColumn];
        result := S_OK;
      end
      else begin
        PSZName := FVirtualTree.DefaultText;
        result := S_OK;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeItemAccessibility.Get_accParent(out ppdispParent: IDispatch): HResult;
// tells MSAA that the VritualStringTree is its parent.
begin
  result := S_FALSE;
  if FVirtualTree <> nil then
  begin
    ppdispParent := FVirtualTree.Accessible;
    Result := S_OK;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeItemAccessibility.Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult;
// tells MSAA that it is a TreeView item as opposed to the TreeView itself.
begin
  Result := S_OK;
//  VariantInit(pvarRole);
//  TVarData(pvarRole).VType := VT_I4;
  if varChild = childid_self then
  begin
    if FVirtualTree <> nil then
      pvarRole := ROLE_SYSTEM_OUTLINEITEM
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeItemAccessibility.Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult;
// Tells MSAA the state the item is in.
const
  IsEnabled: array[Boolean] of Integer = (STATE_SYSTEM_UNAVAILABLE, 0);
  HasPopup: array[Boolean] of Integer = (0, STATE_SYSTEM_HASPOPUP);
  IsVisible: array[Boolean] of Integer = (STATE_SYSTEM_INVISIBLE, 0);
  IsChecked: array[Boolean] of Integer = (0, STATE_SYSTEM_CHECKED);
  IsExpanded: array[Boolean] of Integer = (0, STATE_SYSTEM_EXPANDED);
  IsCollapsed: array[Boolean] of Integer = (0, STATE_SYSTEM_COLLAPSED);
begin
  Result := S_OK;
//  VariantInit(pvarState);
//  TVarData(pvarState).VType := VT_I4;
  if varChild = childid_self then
  begin
    if FVirtualTree <> nil then
    begin
      pvarState := STATE_SYSTEM_FOCUSED or STATE_SYSTEM_FOCUSABLE or STATE_SYSTEM_HOTTRACKED;
      pvarState := pvarState or IsVisible[FVirtualTree.Visible];
      pvarState := pvarState or IsEnabled[FVirtualTree.Enabled];
      if fVirtualTree.FocusedNode <> nil then
      begin
        pvarState := pvarState or IsChecked[csCheckedNormal = FVirtualTree.FocusedNode.CheckState];
        pvarState := pvarState or IsExpanded[VSExpanded in FVirtualTree.FocusedNode.States];
        if not (vsExpanded in FVirtualTree.FocusedNode.States) then
          pvarState:= PvarState or IsCollapsed[vsHasChildren in FVirtualTree.FocusedNode.States];
     end;
    end
    else
      Result := E_INVALIDARG;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TVirtualTreeItemAccessibility.Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult;
// for a TreeView item, the value is the nesting level number, 0-based.
begin
  pszValue := '';
  Result := S_FALSE;//DISP_E_MEMBERNOTFOUND;
  if varChild = childid_self then
    if FVirtualTree <> nil then
      if FVirtualTree.FocusedNode <> nil then
      begin
        PSZValue := IntToStr(FVirtualTree.GetNodeLevel(FVirtualTree.FocusedNode));
        result := S_OK;
      end;
end;

{ TVTMultiColumnItemAccessibility }

function TVTMultiColumnItemAccessibility.GetItemDescription(
  varChild: OleVariant; out pszDescription: WideString;
  IncludeMainColumn: boolean): HResult;
var
  I: Integer;
  sTemp: WideString;
begin
  pszDescription := '';
  Result := S_FALSE;
  if varChild = childid_self then
  begin
    if FVirtualTree <> nil then
      if FVirtualTree.FocusedNode <> nil then
      begin
        if IncludeMainColumn then
          pszDescription := FVirtualTree.Text[FVirtualTree.FocusedNode, FVirtualTree.Header.MainColumn]
           +'; ';
        for I := 0 to FVirtualTree.Header.Columns.Count - 1 do
          if FVirtualTree.Header.MainColumn <> I then
          begin
            sTemp := FVirtualTree.Text[FVirtualTree.FocusedNode, I];
            if sTemp <> '' then
              pszDescription := pszDescription
               +FVirtualTree.Header.Columns[I].Text
               +': '
               +sTemp
               +'; ';
          end;
          if pszDescription <> '' then
            if pszDescription[Length(pszDescription)-1] = ';' then
              Delete(pszDescription, length(pszDescription)-1, 2);
        result := S_OK;
      end
      else begin
        PSZDescription := FVirtualTree.DefaultText;
        result := S_OK;
      end;
  end;
end;

function TVTMultiColumnItemAccessibility.Get_accDescription(
  varChild: OleVariant; out pszDescription: WideString): HResult;
begin
  result := GetItemDescription(varChild, pszDescription, false)
end;

function TVTMultiColumnItemAccessibility.Get_accName(varChild: OleVariant;
  out pszName: WideString): HResult;
begin
  result := GetItemDescription(varChild, pszName, true)
end;

{ TVTDefaultAccessibleProvider }

function TVTDefaultAccessibleProvider.CreateIAccessible(
  ATree: TBaseVirtualTree): IAccessible;
begin
  result := TVirtualTreeAccessibility.Create(TVirtualStringTree(ATree));
end;

{ TVTDefaultAccessibleItemProvider }

function TVTDefaultAccessibleItemProvider.CreateIAccessible(
  ATree: TBaseVirtualTree): IAccessible;
begin
  result := TVirtualTreeItemAccessibility.Create(TVirtualStringTree(ATree));
end;

{ TVTMultiColumnAccessibleItemProvider }

function TVTMultiColumnAccessibleItemProvider.CreateIAccessible(
  ATree: TBaseVirtualTree): IAccessible;
begin
  result := nil;
  if TVirtualStringTree(ATree).Header.UseColumns then
    result := TVTMultiColumnItemAccessibility.Create(TVirtualStringTree(ATree));
end;

var
  IDefaultAccessibleProvider: TVTDefaultAccessibleProvider;
  IDefaultAccessibleItemProvider: TVTDefaultAccessibleItemProvider;
  IMultiColumnAccessibleProvider: TVTMultiColumnAccessibleItemProvider;

initialization
  if VTAccessibleFactory = nil then
    VTAccessibleFactory := TVTAccessibilityFactory.Create;
  if IDefaultAccessibleProvider = nil then
  begin
    IDefaultAccessibleProvider := TVTDefaultAccessibleProvider.Create;
    VTAccessibleFactory.RegisterAccessibleProvider(IDefaultAccessibleProvider);
  end;
  if IDefaultAccessibleItemProvider = nil then
  begin
    IDefaultAccessibleItemProvider := TVTDefaultAccessibleItemProvider.Create;
    VTAccessibleFactory.RegisterAccessibleProvider(IDefaultAccessibleItemProvider);
  end;
  if IMultiColumnAccessibleProvider = nil then
  begin
    IMultiColumnAccessibleProvider := TVTMultiColumnAccessibleItemProvider.Create;
    VTAccessibleFactory.RegisterAccessibleProvider(IMultiColumnAccessibleProvider);
  end;
finalization
  if VTAccessibleFactory <> nil then
  begin
    VTAccessibleFactory.UnRegisterAccessibleProvider(IMultiColumnAccessibleProvider);
    IMultiColumnAccessibleProvider := nil;
      VTAccessibleFactory.UnRegisterAccessibleProvider(IDefaultAccessibleItemProvider);
    IDefaultAccessibleItemProvider := nil;
    VTAccessibleFactory.UnRegisterAccessibleProvider(IDefaultAccessibleProvider);
    IDefaultAccessibleProvider := nil;
  end;

end.

