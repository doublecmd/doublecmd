unit dwTaskbarList;

{$mode delphi}{$H+}

interface

uses
  Messages
  , Windows
  ;

const
  CLSID_TaskbarList: TGUID = '{56FDF344-FD6D-11D0-958A-006097C9A090}';
  CLSID_TaskbarList2: TGUID = '{602D4995-B13A-429B-A66E-1935E44F4317}';
  CLSID_TaskbarList3: TGUID = '{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}';

const
  THBF_ENABLED = $0000;  
  THBF_DISABLED = $0001;
  THBF_DISMISSONCLICK = $0002;
  THBF_NOBACKGROUND = $0004;
  THBF_HIDDEN = $0008;

const
  THB_BITMAP = $0001;
  THB_ICON = $0002;
  THB_TOOLTIP = $0004;
  THB_FLAGS = $0008;

const
  THBN_CLICKED = $1800;

const
  TBPF_NOPROGRESS	= $00;
	TBPF_INDETERMINATE = $01;
	TBPF_NORMAL	= $02;
	TBPF_ERROR= $04;
	TBPF_PAUSED	= $08;

const
  TBATF_USEMDITHUMBNAIL: DWORD = $00000001;
  TBATF_USEMDILIVEPREVIEW: DWORD = $00000002;
  
const
  WM_DWMSENDICONICTHUMBNAIL = $0323;
  WM_DWMSENDICONICLIVEPREVIEWBITMAP = $0326;

type
  TTipString = array[0..259] of WideChar;
  PTipString = ^TTipString;
  tagTHUMBBUTTON = packed record
    dwMask
      : DWORD;
    iId
    , iBitmap
      : UINT;
    hIcon
      : HICON;
    szTip
      : TTipString;
    dwFlags
      : DWORD;
  end;
  THUMBBUTTON = tagTHUMBBUTTON;
  THUMBBUTTONLIST = ^THUMBBUTTON;
  TThumbButton = THUMBBUTTON;
  TThumbButtonList = array of TThumbButton;

type
  ITaskbarList = interface
    ['{56FDF342-FD6D-11D0-958A-006097C9A090}']
    procedure HrInit; safecall;
    procedure AddTab(hwnd: HWND); safecall;
    procedure DeleteTab(hwnd: HWND); safecall;
    procedure ActivateTab(hwnd: HWND); safecall;
    procedure SetActiveAlt(hwnd: HWND); safecall;
  end;

  ITaskbarList2 = interface(ITaskbarList)
    ['{602D4995-B13A-429B-A66E-1935E44F4317}']
    procedure MarkFullscreenWindow(hwnd: HWND; fFullscreen: Bool); safecall;
  end;

  ITaskbarList3 = interface(ITaskbarList2)
    ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    procedure SetProgressValue(hwnd: HWND; ullCompleted, ullTotal: ULONGLONG); safecall;
    procedure SetProgressState(hwnd: HWND; tbpFlags: DWORD); safecall;
    procedure RegisterTab(hwndTab: HWND; hwndMDI: HWND); safecall;
    procedure UnregisterTab(hwndTab: HWND); safecall;
    procedure SetTabOrder(hwndTab: HWND; hwndInsertBefore: HWND); safecall;
    procedure SetTabActive(hwndTab: HWND; hwndMDI: HWND; tbatFlags: DWORD); safecall;
    procedure ThumbBarAddButtons(hwnd: HWND; cButtons: UINT; Button: THUMBBUTTONLIST); safecall;
    procedure ThumbBarUpdateButtons(hwnd: HWND; cButtons: UINT; pButton: THUMBBUTTONLIST); safecall;
    procedure ThumbBarSetImageList(hwnd: HWND; himl: HIMAGELIST); safecall;
    procedure SetOverlayIcon(hwnd: HWND; hIcon: HICON; pszDescription: LPCWSTR); safecall;
    procedure SetThumbnailTooltip(hwnd: HWND; pszTip: LPCWSTR); safecall;
    procedure SetThumbnailClip(hwnd: HWND; prcClip: PRect); safecall;
  end;

const
  DWM_SIT_DISPLAYFRAME 		= $00000001; // Display a window frame around the provided bitmap
  DWMWA_FORCE_ICONIC_REPRESENTATION = 7;	// [set] Force this window to display iconic thumbnails.
  DWMWA_HAS_ICONIC_BITMAP			= 10;	// [set] Indicates an available bitmap when there is no better thumbnail representation.
  DWMWA_DISALLOW_PEEK				= 11;	// [set] Don't invoke Peek on the window.

type
  TWMDwmSendIconicLivePreviewBitmap = TWMNoParams;

  TWMDwmSendIconicThumbnail = packed record
    Msg
      : Cardinal;
    Unused
      : Integer;
    Height
    , Width
      : Word;
    Result
      : LongInt;
  end;

function DwmInvalidateIconicBitmaps(hwnd: HWND): HRESULT;
function DwmSetIconicLivePreviewBitmap(hwnd: HWND; hbmp: HBITMAP; pptClient: PPoint; dwSITFlags: DWORD): HRESULT;
function DwmSetIconicThumbnail(hWnd: HWND; hBmp: HBITMAP; dwSITFlags: DWORD): HRESULT;
function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT;

function PrintWindow(hwnd: HWND; hdcBlt: HDC; nFlags: UINT): BOOL;

implementation

var
  hDWMAPI: HMODULE;
  _DwmInvalidateIconicBitmaps: function(hwnd: HWND): HRESULT; stdcall;
  _DwmSetIconicThumbnail: function(hWnd: HWND; hBmp: HBITMAP; dwSITFlags: DWORD): HRESULT; stdcall;
  _DwmSetIconicLivePreviewBitmap: function(hwnd: HWND; hbmp: HBITMAP; pptClient: PPoint; dwSITFlags: DWORD): HRESULT; stdcall;
  _DwmSetWindowAttribute: function(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  _PrintWindow: function(hwnd: HWND; hdcBlt: HDC; nFlags: UINT): BOOL; stdcall;

procedure InitDwmApi;
begin
  if hDWMAPI = 0 then
  begin
	  hDWMAPI := LoadLibrary('DWMAPI.DLL');
  	if hDWMAPI = 0 then
    begin
      hDWMAPI := THandle(-1);
    end
  	else
    begin
      _DwmInvalidateIconicBitmaps := GetProcAddress(hDWMAPI, 'DwmInvalidateIconicBitmaps');
      _DwmSetIconicLivePreviewBitmap := GetProcAddress(hDWMAPI, 'DwmSetIconicLivePreviewBitmap');
      _DwmSetIconicThumbnail := GetProcAddress(hDWMAPI, 'DwmSetIconicThumbnail');
      _DwmSetWindowAttribute := GetProcAddress(hDWMAPI, 'DwmSetWindowAttribute');
		end;
	end;
end;

function DwmInvalidateIconicBitmaps(hwnd: HWND): HRESULT;
begin
  InitDwmApi;
  if Assigned(_DwmInvalidateIconicBitmaps) then
    Result := _DwmInvalidateIconicBitmaps(hwnd)
  else
    Result := E_NOTIMPL;
end;

function DwmSetIconicLivePreviewBitmap(hwnd: HWND; hbmp: HBITMAP; pptClient: PPoint; dwSITFlags: DWORD): HRESULT;
begin
  InitDwmApi;
  if Assigned(_DwmSetIconicLivePreviewBitmap) then
    Result := _DwmSetIconicLivePreviewBitmap(hwnd, hbmp, pptClient, dwSITFlags)
  else
    Result := E_NOTIMPL;
end;

function DwmSetIconicThumbnail(hWnd: HWND; hBmp: HBITMAP; dwSITFlags: DWORD): HRESULT;
begin
  InitDwmApi;
  if Assigned(_DwmSetIconicThumbnail) then
    Result := _DwmSetIconicThumbnail(hWnd, hBmp, dwSITFlags)
  else
    Result := E_NOTIMPL;
end;

function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT;
begin
  InitDwmApi;
  if Assigned(_DwmSetWindowAttribute) then
    Result := _DwmSetWindowAttribute(hwnd, dwAttribute, pvAttribute, cbAttribute)
  else
    Result := E_NOTIMPL;
end;

{-----------------------------------------------}

function PrintWindow(hwnd: HWND; hdcBlt: HDC; nFlags: UINT): BOOL;
begin
  if Assigned(_PrintWindow) then
  begin
    Result := _PrintWindow(hwnd, hdcBlt, nFlags);
  end
  else
  begin
  	_PrintWindow := GetProcAddress(GetModuleHandle('user32.dll'), 'PrintWindow');
	  Result := Assigned(_PrintWindow) and _PrintWindow(hwnd, hdcBlt, nFlags);
	end;
end;

end.
