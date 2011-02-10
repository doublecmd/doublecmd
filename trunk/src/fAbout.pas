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
  Graphics, Forms, Controls,  StdCtrls, ExtCtrls,
  SysUtils, Classes, LCLType;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    imgLogo: TImage;
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
    OKButton: TButton;
    Panel1: TPanel;
    memInfo: TMemo;
    pnlLogo: TPanel;
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
  dmHelpManager, uDCVersion;

const
  cIndention = LineEnding + #32#32;
  cAboutMsg =
    'This program is free software under GNU GPL 2 license, see COPYING file' + LineEnding +
    'Active developers: '+ cIndention +
    'Alexander Koblov (Alexx2000@mail.ru) - author, core developer' + cIndention +
    'Przemysław Nagay (cobines@gmail.com) - core developer' + LineEnding +
    'Recent developers: ' + cIndention +
    'Dmitry Kolomiets (B4rr4cuda@rambler.ru) - developer' + cIndention +
    'Radek Cervinka (radek.cervinka@centrum.cz) - author of Seksi Commander' + LineEnding +
    'Contributors:' + cIndention +
    'Tolstov Igor (attid@yandex.ru)' + cIndention +
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
    'translators (see detail in lng files)  ' + LineEnding + LineEnding +
    'In program used icons from:' + LineEnding +
    '- Tango Icon Library (http://tango.freedesktop.org/Tango_Icon_Library)' + LineEnding +
    '- Silk icon set 1.3 by Mark James (http://www.famfamfam.com/lab/icons/silk/)' + LineEnding +
    '- Oxygen icon theme (http://oxygen-icons.org)' + LineEnding + LineEnding +
    'Big thanks to Lazarus and FreePascal Team';

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
  dmHelpMgr.HTMLHelpDatabase.ShowURL('http://doublecmd.sourceforge.net','Double Commander Web Site', ErrMsg);
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
  lblVersion.Caption         := Format(lblVersion.Caption, [dcVersion]);
  lblRevision.Caption        := lblRevision.Caption + #32 + dcRevision;
  lblBuild.Caption           := lblBuild.Caption + #32 + dcBuildDate;
  lblLazarusVer.Caption      := lblLazarusVer.Caption + #32 + lazVersion + '-' + lazRevision;
  lblFreePascalVer.Caption   := lblFreePascalVer.Caption + #32 + fpcVersion;
  lblPlatform.Caption        := TargetCPU + '-' + TargetOS + '-' + TargetWS;
  lblOperatingSystem.Caption := OSVersion;
  lblWidgetsetVer.Caption    := WSVersion;
end;

end.
