{
   Double Commander Components
   -------------------------------------------------------------------------
   Extended ProgressBar class

   Copyright (C) 2010  Przemyslaw Nagay (cobines@gmail.com)
   Copyright (C) 2011  Koblov Alexander (Alexx2000@mail.ru)

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
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}

unit KASProgressBar;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls
  {$IFDEF LCLWIN32}
  , ComObj, dwTaskbarList
  {$ENDIF}
  {$IFDEF LCLGTK2}
  , Gtk2
  {$ENDIF}
  {$IFDEF LCLQT}
  , qt4, qtwidgets
  {$ENDIF}
  ;

type

  { TKASProgressBar }

  TKASProgressBar = class(TProgressBar)
  private
    FShowInTaskbar: Boolean;
    {$IFDEF LCLWIN32}
    FOwnerForm: TWinControl;
    FTaskbarList: ITaskbarList;
    FTaskbarList2: ITaskbarList2;
    FTaskbarList3: ITaskbarList3;
    {$ENDIF}
  protected
    {$IFDEF LCLWIN32}
    procedure InitializeWnd; override;
    {$ENDIF}
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
begin
  inherited InitializeWnd;
  FOwnerForm:= GetParentForm(Self);
end;
{$ENDIF}

constructor TKASProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {$IFDEF LCLWIN32}
  // Works only under Windows 7
  if (Win32MajorVersion >= 6) and (Win32MinorVersion >= 1) then
  begin
    FTaskbarList := ITaskbarList(CreateComObject(CLSID_TaskbarList));
    if FTaskbarList = nil then
      begin
        FTaskbarList2 := nil;
        FTaskbarList3 := nil;
      end
    else
      begin
        FTaskbarList.HrInit;

        FTaskbarList.QueryInterface(CLSID_TaskbarList2, FTaskbarList2);
        FTaskbarList.QueryInterface(CLSID_TaskbarList3, FTaskbarList3);
      end;
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
{$IFDEF LCLQT}
var
  wText: WideString;
{$ENDIF}
begin
  if MaxValue <> 0 then
    Position := (CurrentValue * 100) div MaxValue
  else
    Position := 0;

{$IFDEF LCLWIN32}
  if FShowInTaskbar and Assigned(FOwnerForm) and Assigned(FTaskbarList3) then
  begin
    FTaskbarList3.SetProgressValue(FOwnerForm.Handle, Position, Max);
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
{$IFDEF LCLQT}
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
