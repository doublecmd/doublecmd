unit uWin32WidgetSetFix;

{$mode objfpc}{$H+}

interface

implementation

uses
  Classes, SysUtils, Win32Int, WSLCLClasses, Forms, Windows, Win32Proc,
  Controls, LCLType, Win32WSComCtrls, ComCtrls, LMessages, LCLMessageGlue;

type

  { TWin32WSCustomTabControlEx }

  TWin32WSCustomTabControlEx = class(TWin32WSCustomTabControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

{ TWin32WSCustomTabControlEx }

function ChangeTabPage(TabControlHandle: HWND): Integer;
var
  TabControl: TCustomTabControl;
  PageIndex: Integer;
  PageHandle: HWND;
begin
  TabControl := GetWin32WindowInfo(TabControlHandle)^.WinControl as TCustomTabControl;

  if (TabControl.PageIndex <> -1) then
    Windows.ShowWindow(TabControl.CustomPage(TabControl.PageIndex).Handle, SW_HIDE);

  PageIndex := Windows.SendMessage(TabControlHandle, TCM_GETCURSEL, 0, 0);
  PageIndex := TabControl.TabToPageIndex(PageIndex);

  if (TabControl is TTabControl) then
    Exit(PageIndex);

  if PageIndex = -1 then
    Exit(PageIndex);

  PageHandle := TabControl.CustomPage(PageIndex).Handle;
  Windows.SetWindowPos(PageHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  Windows.RedrawWindow(PageHandle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_ERASE);
  Result := PageIndex;
end;

function TabControlParentMsgHandler(const AWinControl: TWinControl; Window: HWnd;
      Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam;
      var MsgResult: Windows.LResult; var WinProcess: Boolean): Boolean;
var
  NMHdr: PNMHDR;
  LMNotify: TLMNotify;
begin
  Result := False;
  if Msg = WM_NOTIFY then
  begin
    NMHdr := PNMHDR(LParam);
    with NMHdr^ do
      case code of
        TCN_SELCHANGE:
          begin
            Result := True;
            idFrom := ChangeTabPage(HWndFrom);
            with LMNotify Do
            begin
              Msg := LM_NOTIFY;
              IDCtrl := WParam;
              NMHdr := PNMHDR(LParam);
              Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
            end;
            DeliverMessage(AWinControl, LMNotify);
            TabControlFocusNewControl(AWinControl as TCustomTabControl, idFrom);
            MsgResult := LMNotify.Result;
          end;
        TCN_SELCHANGING:
          begin
            Result := True;
            with LMNotify Do
            begin
              Msg := LM_NOTIFY;
              IDCtrl := WParam;
              NMHdr := PNMHDR(LParam);
              Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
            end;
            DeliverMessage(AWinControl, LMNotify);
            MsgResult := LMNotify.Result;
          end;
      end;
  end;
end;

class function TWin32WSCustomTabControlEx.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  WindowInfo: PWin32WindowInfo;
begin
  Result:= inherited CreateHandle(AWinControl, AParams);
  WindowInfo:= GetWin32WindowInfo(Result);
  WindowInfo^.ParentMsgHandler := @TabControlParentMsgHandler;
end;

procedure Initialize;
begin
  // Replace TCustomTabControl widgetset class
  // Fix blinking: https://bugs.freepascal.org/view.php?id=22080
  RegisterCustomTabControl;
  RegisterWSComponent(TCustomTabControl, TWin32WSCustomTabControlEx);
end;

initialization
  Initialize;

end.

