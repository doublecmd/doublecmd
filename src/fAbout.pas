{
    Double Commander
    -------------------------------------------------------------------------
    About dialog

    Copyright (C) 2006-2024 Alexander Koblov (alexx2000@mail.ru)

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

unit fAbout;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Forms, Controls, StdCtrls, ExtCtrls, Buttons,
  SysUtils, Classes, LCLType, LMessages;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TBitBtn;
    btnCopyToClipboard: TButton;
    imgLogo: TImage;
    lblCommit: TLabel;
    lblWidgetsetVer: TLabel;
    lblPlatform: TLabel;
    lblOperatingSystem: TLabel;
    lblRevision: TLabel;
    lblHomePageAddress: TLabel;
    lblHomePage: TLabel;
    lblFreePascalVer: TLabel;
    lblTitle: TLabel;
    lblLazarusVer: TLabel;
    lblBuild: TLabel;
    lblVersion: TLabel;
    pnlText: TPanel;
    memInfo: TMemo;
    pnlInfo: TPanel;
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblHomePageAddressClick(Sender: TObject);
    procedure lblHomePageAddressMouseEnter(Sender: TObject);
    procedure lblHomePageAddressMouseLeave(Sender: TObject);
    procedure frmAboutShow(Sender: TObject);
  private
    FMouseLeave, FMouseEnter: TColor;
  protected
    procedure UpdateStyle;
    procedure CMThemeChanged(var Message: TLMessage); message CM_THEMECHANGED;
  public
    { Public declarations }
  end;


procedure ShowAboutBox(TheOwner: TComponent);

implementation

{$R *.lfm}

uses
  Clipbrd, dmHelpManager, uDCVersion, uClipboard, uOSForms;

const
  cIndention = LineEnding + #32#32;
  cAboutMsg =
    'This program is free software under GNU GPL 2 license, see COPYING.txt file.' + LineEnding + LineEnding +
    'Active developers: '+ cIndention +
    'Alexander Koblov (alexx2000@mail.ru) - author, core developer' + cIndention +
    'Rich Chang (rich2014.git@outlook.com) - developer' + LineEnding + LineEnding +
    'Former developers: ' + cIndention +
    'Denis Bisson (denis.bisson@denisbisson.org) - developer' + cIndention +
    'Przemysław Nagay (cobines@gmail.com) - core developer' + cIndention +
    'Dmitry Kolomiets (B4rr4cuda@rambler.ru) - developer' + cIndention +
    'Radek Cervinka (radek.cervinka@centrum.cz) - author of Seksi Commander' + LineEnding + LineEnding +
    'Contributors:' + cIndention +
    'Tolstov Igor (attid@yandex.ru)' + cIndention +
    'Anton Panferov (ast.a_s@mail.ru)' + cIndention +
    'Rustem Rakhimov (dok_rust@bk.ru)' + cIndention +
    'Moroz Serhiy (frost.asm@gmail.com)' + cIndention +
    'Vitaly Zotov (vitalyzotov@mail.ru)' + cIndention +
    'Zolotov Alex (zolotov-alex@shamangrad.net)' + cIndention +
    'Peter Cernoch (pcernoch@volny.cz) - author PFM' + cIndention +
    'Pavel Letko (letcuv@centrum.cz) - multirename, split, linker' + cIndention +
    'Jiri Karasek (jkarasek@centrum.cz)' + cIndention +
    'Vladimir Pilny (vladimir@pilny.com)' + cIndention +
    'Vaclav Juza (vaclavjuza@seznam.cz)' + cIndention +
    'Martin Matusu (xmat@volny.cz) - chown, chgrp' + cIndention +
    'Radek Polak - some viewer fixes' + cIndention +
    'Dmytro Zheludko (doublecmd@zheludko.mail.ua)' + cIndention +
    'Andryei Gudyak - main icon' + cIndention +
    'translators (see details in language files)  ' + LineEnding + LineEnding +
    'Double Commander uses icons from:' + LineEnding +
    '- Tango Icon Library (http://tango.freedesktop.org/Tango_Icon_Library)' + LineEnding +
    '- Silk icon set 1.3 by Mark James (http://www.famfamfam.com/lab/icons/silk/)' + LineEnding +
    '- Elementary icon theme 2.7.1 (https://github.com/elementary/icons)' + LineEnding +
    '- Adwaita Icon Theme (https://gitlab.gnome.org/GNOME/adwaita-icon-theme)' + LineEnding +
    '- Farm-Fresh Web Icons (https://www.fatcow.com/free-icons)' + LineEnding +
    '- Oxygen icon theme (https://invent.kde.org/frameworks/oxygen-icons)' + LineEnding + LineEnding +
    'Big thanks to Lazarus and Free Pascal Team!';

procedure ShowAboutBox(TheOwner: TComponent);
begin
  with TfrmAbout.Create(TheOwner) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmAbout.lblHomePageAddressMouseLeave(Sender: TObject);
begin
  with Sender as TLabel do
  begin
    Font.Style:= [];
    Font.Color:= FMouseLeave;
    Cursor:= crDefault;
  end;
end;

procedure TfrmAbout.lblHomePageAddressMouseEnter(Sender: TObject);
begin
  with Sender as TLabel do
  begin
    Font.Style:= [fsUnderLine];
    Font.Color:= FMouseEnter;
    Cursor:= crHandPoint;
  end;
end;

procedure TfrmAbout.lblHomePageAddressClick(Sender: TObject);
var
  ErrMsg: String;
begin
  dmHelpMgr.HTMLHelpDatabase.ShowURL('https://doublecmd.sourceforge.io','Double Commander Web Site', ErrMsg);
end;

procedure TfrmAbout.btnCopyToClipboardClick(Sender: TObject);
var
  StrInfo: String;
begin
  StrInfo := Format('Double Commander' + LineEnding +
                    'Version: %s' + LineEnding +
                    'Revision: %s' + LineEnding +
                    'Commit: %s' + LineEnding +
                    'Build date: %s' + LineEnding +
                    'Lazarus: %s' + LineEnding +
                    'FPC: %s' + LineEnding +
                    'Platform: %s' + LineEnding +
                    'OS version: %s' + LineEnding,
                    [dcVersion, dcRevision, dcCommit, dcBuildDate,
                    lazVersion, fpcVersion,
                    TargetCPU + '-' + TargetOS + '-' + TargetWS,
                    OSVersion]);
  if WSVersion <> EmptyStr then
    StrInfo := StrInfo + LineEnding + 'Widgetset library: ' + WSVersion;
  ClipboardSetText(StrInfo);
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  UpdateStyle;
  lblTitle.Font.Color:= FMouseEnter;
  lblHomePageAddress.Font.Color:= FMouseLeave;
end;

procedure TfrmAbout.frmAboutShow(Sender: TObject);
begin
  memInfo.Lines.Text         := cAboutMsg;
  memInfo.CaretPos           := Classes.Point(0, 0);

  lblVersion.Caption         := lblVersion.Caption + #32 + dcVersion;
  lblRevision.Caption        := lblRevision.Caption + #32 + dcRevision;
  lblCommit.Caption          := lblCommit.Caption + #32 + dcCommit;
  lblBuild.Caption           := lblBuild.Caption + #32 + dcBuildDate;
  lblLazarusVer.Caption      := lblLazarusVer.Caption + #32 + lazVersion;
  lblFreePascalVer.Caption   := lblFreePascalVer.Caption + #32 + fpcVersion;
  lblPlatform.Caption        := TargetCPU + '-' + TargetOS + '-' + TargetWS;
  lblOperatingSystem.Caption := OSVersion;
  lblWidgetsetVer.Caption    := WSVersion;

  Constraints.MinHeight      := Height;
  btnClose.Anchors           := [akLeft, akBottom];
end;

procedure TfrmAbout.UpdateStyle;
begin
  if DarkStyle then
  begin
    FMouseLeave:= RGBToColor(97, 155, 192);
    FMouseEnter:= RGBToColor(192, 102, 97);
  end
  else begin
    FMouseLeave:= clBlue;
    FMouseEnter:= clRed;
  end;
end;

procedure TfrmAbout.CMThemeChanged(var Message: TLMessage);
begin
  UpdateStyle;
end;

end.
