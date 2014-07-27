unit fstartingsplash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmStartingSplash }

  TfrmStartingSplash = class(TForm)
    imgLogo: TImage;
    lblBuild: TLabel;
    lblFreePascalVer: TLabel;
    lblLazarusVer: TLabel;
    lblOperatingSystem: TLabel;
    lblPlatform: TLabel;
    lblRevision: TLabel;
    lblTitle: TLabel;
    lblVersion: TLabel;
    lblWidgetsetVer: TLabel;
    pnlInfo: TPanel;
    pnlLogo: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure pnlInfoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmStartingSplash: TfrmStartingSplash;

implementation

{$R *.lfm}

uses
  uOSUtils, uDCUtils, uDCVersion;

{ TfrmStartingSplash }

procedure TfrmStartingSplash.FormCreate(Sender: TObject);
begin
  lblVersion.Caption         := lblVersion.Caption + #32 + dcVersion;
  lblRevision.Caption        := lblRevision.Caption + #32 + dcRevision;
  lblBuild.Caption           := lblBuild.Caption + #32 + dcBuildDate;
  lblLazarusVer.Caption      := lblLazarusVer.Caption + #32 + lazVersion + '-' + lazRevision;
  lblFreePascalVer.Caption   := lblFreePascalVer.Caption + #32 + fpcVersion;
  lblPlatform.Caption        := TargetCPU + '-' + TargetOS + '-' + TargetWS;
  lblOperatingSystem.Caption := OSVersion;
  lblWidgetsetVer.Caption    := WSVersion;
end;

procedure TfrmStartingSplash.pnlInfoClick(Sender: TObject);
begin

end;

end.

