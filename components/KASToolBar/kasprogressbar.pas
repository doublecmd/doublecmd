{
   Double Commander Components
   -------------------------------------------------------------------------
   Extended ProgressBar class

   Copyright (C) 2010  Przemyslaw Nagay (cobines@gmail.com)
   Copyright (C) 2011-2018  Alexander Koblov (alexx2000@mail.ru)

   Windows 7 implementation based on "Windows 7 Component Library"
   by Daniel Wischnewski (http://www.gumpi.com/blog)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit KASProgressBar;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls
  {$IFDEF LCLWIN32}
  , InterfaceBase, ComObj, LMessages, Windows, Themes, dwTaskbarList
  {$ENDIF}
  {$IFDEF LCLGTK2}
  , Gtk2
  {$ENDIF}
  {$IFDEF LCLQT}
  , qt4, qtwidgets
  {$ENDIF}
  {$IFDEF LCLQT5}
  , qt5, qtwidgets
  {$ENDIF}
  ;

type

  { TKASProgressBar }

  TKASProgressBar = class(TProgressBar)
  private
    FShowInTaskbar: Boolean;
    {$IFDEF LCLWIN32}
    FBarText: String;
    FTaskBarEntryHandle: HWND;
    FTaskbarList: ITaskbarList;
    FTaskbarList3: ITaskbarList3;
    {$ENDIF}
  protected
    {$IFDEF LCLWIN32}
    procedure InitializeWnd; override;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    {$ENDIF}
    procedure DoOnResize; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetProgress(CurrentValue: Int64; MaxValue: Int64; BarText: String = '');
  published
    property ShowInTaskbar: Boolean read FShowInTaskbar write FShowInTaskbar default False;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KASComponents',[TKASProgressBar]);
end;

{ TKASProgressBar }

{$IFDEF LCLWIN32}
procedure TKASProgressBar.InitializeWnd;
var
  aOwnerForm: TWinControl;
begin
  inherited InitializeWnd;
  if CheckWin32Version(6, 1) then
  begin
    aOwnerForm:= GetParentForm(Self);
    if Assigned(aOwnerForm) and (aOwnerForm <> Application.MainForm) then
      FTaskBarEntryHandle := aOwnerForm.Handle
    else
      FTaskBarEntryHandle := Widgetset.AppHandle;
  end;
end;

procedure TKASProgressBar.WMPaint(var Msg: TLMPaint);
var
  OldFont: HFONT;
  OldBkMode: Integer;
  Details: TThemedElementDetails;
begin
  inherited WMPaint(Msg);

  if BarShowText and (CheckWin32Version(8) and ThemeServices.ThemesEnabled) then
  begin
    OldBkMode:= SetBkMode(Msg.DC, TRANSPARENT);
    Details:= ThemeServices.GetElementDetails(tpBar);
    OldFont:= SelectObject(Msg.DC, Font.Reference.Handle);
    ThemeServices.DrawText(Msg.DC, Details, FBarText, Msg.PaintStruct^.rcPaint, DT_SINGLELINE or DT_CENTER or DT_VCENTER, 0);
    SelectObject(Msg.DC, OldFont);
    SetBkMode(Msg.DC, OldBkMode);
  end;
end;
{$ENDIF}

procedure TKASProgressBar.DoOnResize;
begin
  inherited;
  Max := Width;
end;

constructor TKASProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {$IFDEF LCLWIN32}
  FTaskbarList3 := nil;
  FTaskBarEntryHandle := INVALID_HANDLE_VALUE;
  // Works only under Windows 7 and higher
  if CheckWin32Version(6, 1) then
  try
    FTaskbarList := ITaskbarList(CreateComObject(CLSID_TaskbarList));
    FTaskbarList.HrInit;
    FTaskbarList.QueryInterface(CLSID_TaskbarList3, FTaskbarList3);
  except
    FTaskbarList3 := nil;
  end;
  {$ENDIF}

  {$IFDEF LCLGTK2}
  // Have to disable LCLGTK2 default progress bar text
  // set in TGtk2WSProgressBar.UpdateProgressBarText.
  BarShowText := False;
  {$ENDIF}
end;

procedure TKASProgressBar.SetProgress(CurrentValue: Int64; MaxValue: Int64;
  BarText: String);
{$IFDEF LCLGTK2}
var
  wText: String;
{$ENDIF}
{$IF DEFINED(LCLQT) OR DEFINED(LCLQT5)}
var
  wText: WideString;
{$ENDIF}
begin
  if MaxValue <> 0 then
    Position := Round(CurrentValue * Max / MaxValue)
  else
    Position := 0;

{$IFDEF LCLWIN32}
  if BarShowText then
  begin
    if MaxValue = 0 then
      FBarText := BarText
    else if BarText = '' then
      FBarText :=  FloatToStrF((CurrentValue / MaxValue) * 100, ffFixed, 0, 0) + '%'
    else
      FBarText := BarText + ' (' + FloatToStrF((CurrentValue / MaxValue) * 100, ffFixed, 0, 0) + '%)';
  end;

  if FShowInTaskbar and (FTaskBarEntryHandle <> INVALID_HANDLE_VALUE) and Assigned(FTaskbarList3) then
  begin
    FTaskbarList3.SetProgressValue(FTaskBarEntryHandle, Position, Max);
  end;
{$ENDIF}

{$IFDEF LCLGTK2}
{
  %v - the current progress value.
  %l - the lower bound for the progress value.
  %u - the upper bound for the progress value.
  %p - the current progress percentage.
}
  if BarText <> '' then
    wText := BarText + ' (%p%%)'
  else
    wText := '%p%%';
  gtk_progress_set_format_string(PGtkProgress(Self.Handle), PChar(wText));
  // Have to reset 'show_text' every time because LCLGTK2 will set it according to BarShowText.
  gtk_progress_set_show_text(PGtkProgress(Self.Handle), True);
{$ENDIF}
{$IF DEFINED(LCLQT) OR DEFINED(LCLQT5)}
{
  %p - is replaced by the percentage completed.
  %v - is replaced by the current value.
  %m - is replaced by the total number of steps.
}
  if BarText <> '' then
    wText := WideString(BarText) + ' (%p%)'
  else
    wText := '%p%';
  QProgressBar_setFormat(QProgressBarH(TQtProgressBar(Self.Handle).Widget), @wText);
  //QProgressBar_setTextVisible(QProgressBarH(TQtProgressBar(Self.Handle).Widget), True);
{$ENDIF}
end;

end.
