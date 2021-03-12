{
   Seksi Commander
   ----------------------------
   Implementing of About dialog

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)

}

unit fAbout;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Forms, Controls, StdCtrls, ExtCtrls, Buttons,
  SysUtils, Classes, LCLType;

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
    pnlLogo: TPanel;
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure lblHomePageAddressClick(Sender: TObject);
    procedure lblHomePageAddressMouseEnter(Sender: TObject);
    procedure lblHomePageAddressMouseLeave(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure frmAboutShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure ShowAboutBox;

implementation

{$R *.lfm}

uses
  Clipbrd, dmHelpManager, uDCVersion, uClipboard;

const
  cIndention = LineEnding + #32#32;
  cAboutMsg =
    'This program is free software under GNU GPL 2 license, see COPYING.txt file.' + LineEnding + LineEnding +
    'Active developers: '+ cIndention +
    'Alexander Koblov (alexx2000@mail.ru) - author, core developer' + cIndention +
    'Denis Bisson (denis.bisson@denisbisson.org) - developer' + LineEnding + LineEnding +
    'Former developers: ' + cIndention +
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
    '- Farm-Fresh Web Icons (https://www.fatcow.com/free-icons)' + LineEnding +
    '- Oxygen icon theme (http://oxygen-icons.org)' + LineEnding + LineEnding +
    'Big thanks to Lazarus and Free Pascal Team!';

procedure ShowAboutBox;
begin
  with TfrmAbout.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmAbout.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.lblHomePageAddressMouseLeave(Sender: TObject);
begin
  with Sender as TLabel do
  begin
    Font.Style:= [];
    Font.Color:= clBlue;
    Cursor:= crDefault;
  end;
end;

procedure TfrmAbout.lblHomePageAddressMouseEnter(Sender: TObject);
begin
  with Sender as TLabel do
  begin
    Font.Style:= [fsUnderLine];
    Font.Color:= clRed;
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
                    [dcVersion, dcRevision, GetCommitTime, dcBuildDate,
                    GetLazarusVersion, fpcVersion,
                    TargetCPU + '-' + TargetOS + '-' + TargetWS,
                    OSVersion]);
  if WSVersion <> EmptyStr then
    StrInfo := StrInfo + LineEnding + 'Widgetset library: ' + WSVersion;
  ClipboardSetText(StrInfo);
end;

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Escape) then
   Close;
end;

procedure TfrmAbout.frmAboutShow(Sender: TObject);
begin
  memInfo.Lines.Text         := cAboutMsg;
  memInfo.CaretPos           := Classes.Point(0, 0);

  lblVersion.Caption         := lblVersion.Caption + #32 + dcVersion;
  lblRevision.Caption        := lblRevision.Caption + #32 + dcRevision;
  lblCommit.Caption          := lblCommit.Caption + #32 + GetCommitTime;
  lblBuild.Caption           := lblBuild.Caption + #32 + dcBuildDate;
  lblLazarusVer.Caption      := lblLazarusVer.Caption + #32 + GetLazarusVersion;
  lblFreePascalVer.Caption   := lblFreePascalVer.Caption + #32 + fpcVersion;
  lblPlatform.Caption        := TargetCPU + '-' + TargetOS + '-' + TargetWS;
  lblOperatingSystem.Caption := OSVersion;
  lblWidgetsetVer.Caption    := WSVersion;

  lblBuild.Visible           := (dcCommit = 0);
  lblCommit.Visible          := (dcCommit > 0);
end;

end.
