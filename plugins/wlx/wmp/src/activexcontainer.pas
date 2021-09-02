unit activexcontainer;

{$mode delphi}{$H+}

{ Visual ActiveX container.

  Copyright (C) 2011 Ludo Brands

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

interface

uses
  Classes, SysUtils, Windows, ActiveX, ComObj;

type
  //from OCIDL.h
  PPointF = ^TPointF;
  tagPOINTF = record
    x: Single;
    y: Single;
  end;
  TPointF = tagPOINTF;
  POINTF = TPointF;


  IOleControlSite = interface
    ['{B196B289-BAB4-101A-B69C-00AA00341D07}']
    function OnControlInfoChanged: HResult; stdcall;
    function LockInPlaceActive(fLock: BOOL): HResult; stdcall;
    function GetExtendedControl(out disp: IDispatch): HResult; stdcall;
    function TransformCoords(var ptlHimetric: TPoint; var ptfContainer: TPointF;
      flags: Longint): HResult; stdcall;
    function TranslateAccelerator(msg: PMsg; grfModifiers: Longint): HResult;
       stdcall;
    function OnFocus(fGotFocus: BOOL): HResult; stdcall;
    function ShowPropertyFrame: HResult; stdcall;
  end;

  IPropertyNotifySink = interface
    ['{9BFBBC02-EFF1-101A-84ED-00AA00341D07}']
    function OnChanged(dispid: TDispID): HResult; stdcall;
    function OnRequestEdit(dispid: TDispID): HResult; stdcall;
  end;

  ISimpleFrameSite = interface
    ['{742B0E01-14E6-101B-914E-00AA00300CAB}']
    function PreMessageFilter(wnd: HWnd; msg, wp, lp: Integer;
      out res: Integer; out Cookie: Longint): HResult;
      stdcall;
    function PostMessageFilter(wnd: HWnd; msg, wp, lp: Integer;
      out res: Integer; Cookie: Longint): HResult;
      stdcall;
  end;


  TStatusTextEvent = procedure(Sender: TObject; Status:string) of object;

  { TActiveXContainer }

  TActiveXContainer = class(TComponent, IUnknown, IOleClientSite,
    IOleControlSite, IOleInPlaceSite, IOleInPlaceFrame, IDispatch)
    private
      FHandle: HWND;
      FPixelsPerInchX,
      FPixelsPerInchY: Integer;
      FActive: boolean;
      FAttached: boolean;
      FClassName: string;
      FOleObject: IDispatch;
      FOnStatusText: TStatusTextEvent;
      FPrevWndProc:windows.WNDPROC;
      Function GetvObject:variant;
      //IOleClientSite
      Function SaveObject: HResult;StdCall;
      Function GetMoniker(dwAssign: Longint; dwWhichMoniker: Longint;OUT mk: IMoniker):HResult;StdCall;
      Function GetContainer(OUT container: IOleContainer):HResult;StdCall;
      procedure SetActive(AValue: boolean);
      procedure SetClassName(AValue: string);
      procedure SetHandle(AValue: HWND);
      procedure SetOleObject(AValue: IDispatch);
      Function ShowObject:HResult;StdCall;
      Function OnShowWindow(fShow: BOOL):HResult;StdCall;
      Function RequestNewObjectLayout:HResult;StdCall;
      //IOleControlSite
      function OnControlInfoChanged: HResult; stdcall;
      function LockInPlaceActive(fLock: BOOL): HResult; stdcall;
      function GetExtendedControl(out disp: IDispatch): HResult; stdcall;
      function TransformCoords(var ptlHimetric: TPoint; var ptfContainer: TPointF;
        flags: Longint): HResult; stdcall;
      function TranslateAccelerator(msg: PMsg; grfModifiers: Longint): HResult;overload;
         stdcall;
      function OnFocus(fGotFocus: BOOL): HResult; stdcall;
      function ShowPropertyFrame: HResult; stdcall;
      //IOleInPlaceSite
      function CanInPlaceActivate : HResult;stdcall;
      function OnInPlaceActivate : HResult;stdcall;
      function OnUIActivate : HResult;stdcall;
      function GetWindowContext(out ppframe:IOleInPlaceFrame;out ppdoc:IOleInPlaceUIWindow;lprcposrect:LPRECT;lprccliprect:LPRECT;lpframeinfo:LPOLEINPLACEFRAMEINFO):hresult; stdcall;
      function Scroll(scrollExtant:TSIZE):hresult; stdcall;
      function OnUIDeactivate(fUndoable:BOOL):hresult; stdcall;
      function OnInPlaceDeactivate :hresult; stdcall;
      function DiscardUndoState :hresult; stdcall;
      function DeactivateAndUndo :hresult; stdcall;
      function OnPosRectChange(lprcPosRect:LPRect):hresult; stdcall;
      //IOleWindow
      function GetWindow(out wnd: HWnd): HResult; stdcall;
      function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
       //IOleInPlaceFrame
      function InsertMenus(hmenuShared: HMenu; var menuWidths: TOleMenuGroupWidths): HResult;StdCall;
      function SetMenu(hmenuShared: HMenu; holemenu: HMenu; hwndActiveObject: HWnd): HResult;StdCall;
      function RemoveMenus(hmenuShared: HMenu): HResult;StdCall;
      function SetStatusText(pszStatusText: POleStr): HResult;StdCall;
      function EnableModeless(fEnable: BOOL): HResult;StdCall;
      function TranslateAccelerator(var msg: TMsg; wID: Word): HResult;StdCall;overload;
      //IOleInPlaceUIWindow
      function GetBorder(out rectBorder: TRect):HResult;StdCall;
      function RequestBorderSpace(const borderwidths: TRect):HResult;StdCall;
      function SetBorderSpace(const borderwidths: TRect):HResult;StdCall;
      function SetActiveObject(const activeObject: IOleInPlaceActiveObject;pszObjName: POleStr):HResult;StdCall;
      //IDispatch
      function GetTypeInfoCount(out count : longint) : HResult;stdcall;
      function GetTypeInfo(Index,LocaleID : longint;
        out TypeInfo): HResult;stdcall;
      function GetIDsOfNames(const iid: TGUID; names: Pointer;
        NameCount, LocaleID: LongInt; DispIDs: Pointer) : HResult;stdcall;
      function Invoke(DispID: LongInt;const iid : TGUID;
            LocaleID : longint; Flags: Word;var params;
            VarResult,ExcepInfo,ArgErr : pointer) : HResult;stdcall;
      //internal
      procedure Attach;
      procedure Detach;
   public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
      //VT_DISPATCH variant used for late binding
      property vObject:Variant read GetvObject;
   published
      //ActiveX object is automatically created from classname and destroyed when set
      property OleClassName:string read FClassName write SetClassName;
      {IDispatch interface for ActiveX object. Overrides classname. Set ComServer
      when you create and destroy the object yourself, fe. using CoClass.
      When Active, returns the IDispatch for the object.
      }
      property ComServer:IDispatch read FOleObject write SetOleObject;
      {When set, binds ActiveX component to control.
      When cleared, detaches the component from the control
      If Classname is provided the ActiveX component will also be created and destroyed
      automatically.}
      property Active:boolean read FActive write SetActive;
      property Handle: HWND read FHandle write SetHandle;
    end;

implementation

{$ifdef wince}
const
  GWLP_USERDATA=GWL_USERDATA;

function GetWindowLongPtrW(hWnd:HWND; nIndex:longint):LONG;
begin
  result:=GetWindowLongW(hWnd, nIndex);
end;

function SetWindowLongPtrW(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG;
begin
  result:=SetWindowLongW(hWnd, nIndex, dwNewLong);
end;

function SetWindowLongPtr(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG;
begin
  result:=SetWindowLongW(hWnd, nIndex, dwNewLong);
end;
{$endif wince}

function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam;
  lParam: LParam): LRESULT; stdcall;
  var
    bounds:TRect;
    DC: HDC;
    size:TPOINT;
    AXC:TActiveXContainer;
begin
  AXC:=TActiveXContainer(GetWindowLongPtrW( Ahwnd, GWLP_USERDATA));
  case uMsg of
    WM_DESTROY:AXC.Detach;
    WM_SIZE:
      begin
      size.x:=(LOWORD(lparam)*2540) div AXC.FPixelsPerInchX;
      size.y:=(HIWORD(lparam)*2540) div AXC.FPixelsPerInchY;
      MoveWindow(AXC.FHandle, 0, 0, LOWORD(lparam), HIWORD(lparam), True);
      olecheck((AXC.ComServer as IOleObject).SetExtent(DVASPECT_CONTENT,size));
      GetClientRect(AXC.FHandle, @bounds);
      olecheck((AXC.ComServer as IOleInPlaceObject).SetObjectRects(@bounds,@bounds));
      end;
    WM_PAINT:
       begin
       DC:=GetDC(AXC.handle);
       GetClientRect(AXC.FHandle, @bounds);
       olecheck((AXC.ComServer as IViewObject).Draw(DVASPECT_CONTENT,0,nil,nil,0,DC,@bounds,@bounds,nil,0));
       ReleaseDC(AXC.handle,DC);
      end;
  end;
  result:=CallWindowProc(AXC.FPrevWndProc,Ahwnd, uMsg, WParam, LParam);
end;

{ TActiveXContainer }

function TActiveXContainer.GetvObject: variant;
begin
  result:=FOleObject;
end;

function TActiveXContainer.SaveObject: HResult; StdCall;
begin
  Result := S_OK;
end;

function TActiveXContainer.GetMoniker(dwAssign: Longint; dwWhichMoniker: Longint; out
  mk: IMoniker): HResult; StdCall;
begin
  mk := nil;
  Result := E_NOTIMPL;
end;

function TActiveXContainer.GetContainer(out container: IOleContainer): HResult;
  StdCall;
begin
  container := nil;
  Result := E_NOINTERFACE;
end;

procedure TActiveXContainer.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  if AValue then
    begin
      if (FClassName='') and not assigned(ComServer) then
        raise exception.Create('OleClassName and ComServer not assigned.');
      if not assigned(FOleObject) then
        FOleObject:=CreateOleObject(FClassName);
      Attach;
    end
  else
    begin
    Detach;
    if FClassName<>'' then //destroy com object
      FOleObject:=nil;
    end;
  FActive:=AValue;
end;

procedure TActiveXContainer.SetClassName(AValue: string);
begin
  if (FClassName=AValue) or FActive then Exit;
  FClassName:=AValue;
end;

procedure TActiveXContainer.SetHandle(AValue: HWND);
var
  DC: HDC;
begin
  if FHandle=AValue then Exit;
  FHandle:=AValue;

  DC:= GetDC(FHandle);
  FPixelsPerInchX := GetDeviceCaps(DC, LOGPIXELSX);
  FPixelsPerInchY := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(FHandle, DC);
end;

procedure TActiveXContainer.SetOleObject(AValue: IDispatch);
begin
  if (FOleObject=AValue) or FActive then Exit;
  FOleObject:=AValue;
end;

function TActiveXContainer.ShowObject: HResult; StdCall;
begin
  Result := S_OK;
end;

function TActiveXContainer.OnShowWindow(fShow: BOOL): HResult; StdCall;
begin
  Result := S_OK;
end;

function TActiveXContainer.RequestNewObjectLayout: HResult; StdCall;
begin
  Result := S_OK;
end;

function TActiveXContainer.OnControlInfoChanged: HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.LockInPlaceActive(fLock: BOOL): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.GetExtendedControl(out disp: IDispatch): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.TransformCoords(var ptlHimetric: TPoint;
  var ptfContainer: TPointF; flags: Longint): HResult; stdcall;
begin
  if flags and 4 <> 0 then  //XFORMCOORDS_HIMETRICTOCONTAINER=4
    begin
    ptfContainer.X := (ptlHimetric.X * FPixelsPerInchX) div 2540;
    ptfContainer.Y := (ptlHimetric.Y * FPixelsPerInchY) div 2540;
    end
  else if assigned(@ptlHimetric) and (flags and 8 <> 0) then  //XFORMCOORDS_CONTAINERTOHIMETRIC = 8
    begin
    ptlHimetric.X := Integer(Round(ptfContainer.X * 2540 / FPixelsPerInchX));
    ptlHimetric.Y := Integer(Round(ptfContainer.Y * 2540 / FPixelsPerInchY));
    end;
  Result := S_OK;
end;

function TActiveXContainer.TranslateAccelerator(msg: PMsg; grfModifiers: Longint
  ): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.OnFocus(fGotFocus: BOOL): HResult; stdcall;
begin
  Result := S_OK;
end;

function TActiveXContainer.ShowPropertyFrame: HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.CanInPlaceActivate: HResult;stdcall;
begin
  Result := S_OK;
end;

function TActiveXContainer.OnInPlaceActivate: HResult;stdcall;
begin
  Result := S_OK;
end;

function TActiveXContainer.OnUIActivate: HResult; stdcall;
begin
  Result := S_OK;
end;

function TActiveXContainer.GetWindowContext(out ppframe: IOleInPlaceFrame; out
  ppdoc: IOleInPlaceUIWindow; lprcposrect: LPRECT; lprccliprect: LPRECT;
  lpframeinfo: LPOLEINPLACEFRAMEINFO): hresult; stdcall;
begin
    if assigned (ppframe) then
      ppframe := Self as IOleInPlaceFrame;
    if assigned(ppdoc) then
      ppdoc:= nil;
    if assigned(lpframeinfo) then
      begin
      lpframeinfo.fMDIApp := False;
      lpframeinfo.cAccelEntries := 0;
      lpframeinfo.haccel := 0;
      lpframeinfo.hwndFrame := Handle;
      end;

    if assigned (lprcPosRect) then
      GetClientRect(FHandle, lprcPosRect);
    if assigned (lprcClipRect) then
      GetClientRect(FHandle, lprcClipRect);

    Result := S_OK;
end;

function TActiveXContainer.Scroll(scrollExtant: TSIZE): hresult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.OnUIDeactivate(fUndoable: BOOL): hresult; stdcall;
begin
  Result := S_OK;
end;

function TActiveXContainer.OnInPlaceDeactivate: hresult; stdcall;
begin
  Result := S_OK;
end;

function TActiveXContainer.DiscardUndoState: hresult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.DeactivateAndUndo: hresult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.OnPosRectChange(lprcPosRect: LPRect): hresult; stdcall;
begin
  Result := S_OK;
end;

function TActiveXContainer.GetWindow(out wnd: HWnd): HResult; stdcall;
begin
  wnd:=Handle;
  Result := S_OK;
end;

function TActiveXContainer.ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.InsertMenus(hmenuShared: HMenu;
  var menuWidths: TOleMenuGroupWidths): HResult; StdCall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.SetMenu(hmenuShared: HMenu; holemenu: HMenu;
  hwndActiveObject: HWnd): HResult; StdCall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.RemoveMenus(hmenuShared: HMenu): HResult; StdCall;
begin
  Result := S_OK;
end;

function TActiveXContainer.SetStatusText(pszStatusText: POleStr): HResult; StdCall;
begin
  if assigned(FOnStatusText) then
    FOnStatusText(Self,utf8encode(WideString(pszStatusText)));
  Result := S_OK;
end;

function TActiveXContainer.EnableModeless(fEnable: BOOL): HResult; StdCall;
begin
  Result := S_OK;
end;

function TActiveXContainer.TranslateAccelerator(var msg: TMsg; wID: Word): HResult;
  StdCall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.GetBorder(out rectBorder: TRect): HResult; StdCall;
begin
  Result := INPLACE_E_NOTOOLSPACE;
end;

function TActiveXContainer.RequestBorderSpace(const borderwidths: TRect): HResult;
  StdCall;
begin
  Result := INPLACE_E_NOTOOLSPACE;
end;

function TActiveXContainer.SetBorderSpace(const borderwidths: TRect): HResult;
  StdCall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.SetActiveObject(
  const activeObject: IOleInPlaceActiveObject; pszObjName: POleStr): HResult;
  StdCall;
begin
  Result := S_OK;
end;

function TActiveXContainer.GetTypeInfoCount(out count: longint): HResult;
  stdcall;
begin
  Count := 0;
  Result := S_OK;
end;

function TActiveXContainer.GetTypeInfo(Index, LocaleID: longint; out TypeInfo
  ): HResult; stdcall;
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TActiveXContainer.GetIDsOfNames(const iid: TGUID; names: Pointer;
  NameCount, LocaleID: LongInt; DispIDs: Pointer): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TActiveXContainer.Invoke(DispID: LongInt; const iid: TGUID;
  LocaleID: longint; Flags: Word; var params; VarResult, ExcepInfo,
  ArgErr: pointer): HResult; stdcall;
const
  DISPID_AMBIENT_BACKCOLOR         = -701;
  DISPID_AMBIENT_DISPLAYNAME       = -702;
  DISPID_AMBIENT_FONT              = -703;
  DISPID_AMBIENT_FORECOLOR         = -704;
  DISPID_AMBIENT_LOCALEID          = -705;
  DISPID_AMBIENT_MESSAGEREFLECT    = -706;
  DISPID_AMBIENT_USERMODE          = -709;
  DISPID_AMBIENT_UIDEAD            = -710;
  DISPID_AMBIENT_SHOWGRABHANDLES   = -711;
  DISPID_AMBIENT_SHOWHATCHING      = -712;
  DISPID_AMBIENT_SUPPORTSMNEMONICS = -714;
  DISPID_AMBIENT_AUTOCLIP          = -715;
begin
  if (Flags and DISPATCH_PROPERTYGET <> 0) and (VarResult <> nil) then
  begin
    Result := S_OK;
    case DispID of
//    DISPID_AMBIENT_BACKCOLOR:
//        PVariant(VarResult)^ := Color;
      DISPID_AMBIENT_DISPLAYNAME:
        PVariant(VarResult)^ := OleVariant(Name);
      DISPID_AMBIENT_FONT:
        PVariant(VarResult)^ :=nil;
//      DISPID_AMBIENT_FORECOLOR:
//        PVariant(VarResult)^ := Font.Color;
      DISPID_AMBIENT_LOCALEID:
        PVariant(VarResult)^ := Integer(GetUserDefaultLCID);
      DISPID_AMBIENT_MESSAGEREFLECT:
        PVariant(VarResult)^ := False;
      DISPID_AMBIENT_USERMODE:
        PVariant(VarResult)^ := not (csDesigning in ComponentState);
      DISPID_AMBIENT_UIDEAD:
        PVariant(VarResult)^ := csDesigning in ComponentState;
      DISPID_AMBIENT_SHOWGRABHANDLES:
        PVariant(VarResult)^ := False;
      DISPID_AMBIENT_SHOWHATCHING:
        PVariant(VarResult)^ := False;
      DISPID_AMBIENT_SUPPORTSMNEMONICS:
        PVariant(VarResult)^ := True;
      DISPID_AMBIENT_AUTOCLIP:
        PVariant(VarResult)^ := True;
    else
      Result := DISP_E_MEMBERNOTFOUND;
    end;
  end else
    Result := DISP_E_MEMBERNOTFOUND;
end;

procedure TActiveXContainer.Attach;
var
  size:TPOINT;
  ARect: TRect;
begin
  SetWindowLongPtr(Handle,GWLP_USERDATA, PtrInt(Self));
  FPrevWndProc:=Windows.WNDPROC(SetWindowLongPtr(Handle,GWL_WNDPROC,PtrInt(@WndCallback)));
  FAttached:=true;
  olecheck((FOleObject as IOleObject).SetClientSite(Self as IOleClientSite));
  olecheck((FOleObject as IOleObject).SetHostNames(PWideChar(name),PWideChar(name)));
  GetClientRect(FHandle, ARect);
  size.x:=(ARect.Width*2540) div FPixelsPerInchX;
  size.y:=(ARect.Height*2540) div FPixelsPerInchY;
  olecheck((FOleObject as IOleObject).SetExtent(DVASPECT_CONTENT,size));
  olecheck((FOleObject as IOleObject).DoVerb(OLEIVERB_INPLACEACTIVATE,nil,Self as IOleClientSite,0,Handle,ARect));
end;

procedure TActiveXContainer.Detach;
const
  OLECLOSE_NOSAVE = 1;
begin
  if FAttached then
    begin
    SetWindowLongPtr(Handle,GWL_WNDPROC,PtrInt(@FPrevWndProc));
    SetWindowLongPtr(Handle,GWLP_USERDATA, 0);
    end;
  if assigned(FOleObject) then
    begin
    olecheck((FOleObject as IOleObject).SetClientSite(nil));
    olecheck((FOleObject as IOleObject).Close(OLECLOSE_NOSAVE));
    end;
end;

constructor TActiveXContainer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TActiveXContainer.Destroy;
begin
  Active:=false; //destroys com object if created by Self
  inherited Destroy;
end;

end.

