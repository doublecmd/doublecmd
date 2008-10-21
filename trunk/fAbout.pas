{
Seksi Commander
----------------------------
Implementing of About dialog
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

}
unit fAbout;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  Graphics, Forms, Controls,  StdCtrls, ExtCtrls, ActnList,Buttons,
  SysUtils, Classes, lcltype;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    imgLogo: TImage;
    lblTitle: TLabel;
    lblLazarusVer: TLabel;
    lblBuild: TLabel;
    lblVersion: TLabel;
    OKButton: TButton;
    Panel1: TPanel;
    memInfo: TMemo;
    pnlLogo: TPanel;
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

var
  buildDate,
  Version: String;
  
implementation
{uses
  uConstants;}

const
  LazarusVersionStr = {$I version.inc};
  cAboutMsg =
    'This program is free software under GNU GPL 2 license, see COPYING file'+LineEnding+
    'Authors: '+ LineEnding +
    'Alexander Koblov (Alexx2000@mail.ru)' + LineEnding +
    'Radek Cervinka (radek.cervinka@centrum.cz) - author of Seksi Commander'+LineEnding+
    'Contributors:'+LineEnding+
    'Dmitry Kolomiets (B4rr4cuda@rambler.ru)'+LineEnding+
    'Tolstov Igor (attid@yandex.ru)'+LineEnding+
    'Zolotov Alex (zolotov-alex@shamangrad.net)'+LineEnding+
    'Peter Cernoch (pcernoch@volny.cz) - author PFM'+LineEnding+
    'Pavel Letko (letcuv@centrum.cz) - multirename, split, linker'+LineEnding+
    'Jiri Karasek (jkarasek@centrum.cz)'+LineEnding+
    'Vladimir Pilny (vladimir@pilny.com)'+LineEnding+
    'Vaclav Juza (vaclavjuza@seznam.cz)'+LineEnding+
    'Martin Matusu (xmat@volny.cz) - chown, chgrp'+LineEnding+
    'Radek Polak - some viewer fixes'+LineEnding+
    'translators (see detail in lng files)  '+LineEnding+LineEnding+
    'In program used icons from:'+LineEnding+
    '- Tango Icon Library (http://tango.freedesktop.org/Tango_Icon_Library)'+LineEnding+
    '- Silk icon set 1.3 by Mark James (http://www.famfamfam.com/lab/icons/silk/)'+LineEnding+LineEnding+
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

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Escape) then
   Close;
end;

procedure TfrmAbout.frmAboutShow(Sender: TObject);
begin
  memInfo.Lines.Text := cAboutMsg;
  memInfo.CaretPos:= Classes.Point(0, 0);
  lblVersion.Caption:= Format(lblVersion.Caption, [Version]);
  lblBuild.Caption := lblBuild.Caption + #32 + buildDate;
  lblLazarusVer.Caption := lblLazarusVer.Caption + #32 + LazarusVersionStr;
end;

initialization
 {$I fAbout.lrs}

end.
