unit uQuickViewPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, fViewer,
  uFileViewNotebook, uFileSource;

type

  { TQuickViewPanel }

  TQuickViewPanel = class(TPanel)
  private
    FFirstFile: Boolean;
    FFileViewPage: TFileViewPage;
    FFileSource: IFileSource;
    FViewer: TfrmViewer;
  public
    constructor Create(TheOwner: TComponent; aParent: TFileViewPage); reintroduce;
    destructor Destroy; override;
    procedure CreateViewer(aFileSource: IFileSource);
    procedure LoadFile(const aFileName: UTF8String);
  end;

procedure QuickViewShow(aFileViewPage: TFileViewPage; const aFileSource: IFileSource);
procedure QuickViewLoadFile(const aFileName: UTF8String; const aFileSource: IFileSource);
procedure QuickViewClose;

var
  QuickViewPanel: TQuickViewPanel;

implementation

uses
  LCLProc, Forms, Controls;

procedure QuickViewShow(aFileViewPage: TFileViewPage; const aFileSource: IFileSource);
begin
  QuickViewPanel:= TQuickViewPanel.Create(Application, aFileViewPage);
  QuickViewPanel.CreateViewer(aFileSource);
end;

procedure QuickViewLoadFile(const aFileName: UTF8String; const aFileSource: IFileSource);
begin
  {
  if (not QuickViewPanel.FFileSource.IsInterface(aFileSource)) then
    begin
      QuickViewPanel.FViewer.Close;
      QuickViewPanel.CreateViewer(aFileSource);
    end;
  }
  QuickViewPanel.LoadFile(aFileName);
end;

procedure QuickViewClose;
begin
  FreeThenNil(QuickViewPanel);
end;

{ TQuickViewPanel }

constructor TQuickViewPanel.Create(TheOwner: TComponent; aParent: TFileViewPage);
begin
  inherited Create(TheOwner);
  Parent:= aParent;
  Align:= alClient;
  FFileViewPage:= aParent;
  FFileSource:= nil;
  FViewer:= nil;
end;

destructor TQuickViewPanel.Destroy;
begin
  FFileViewPage.FileView.Visible:= True;
  FreeThenNil(FViewer);
  inherited Destroy;
end;

procedure TQuickViewPanel.CreateViewer(aFileSource: IFileSource);
begin
  FViewer:= TfrmViewer.Create(Self, aFileSource);
  FViewer.Parent:= Self;
  FViewer.BorderStyle:= bsNone;
  FViewer.Menu:= nil;
  FViewer.Align:= alClient;
  FViewer.QuickView:= True;
  FFirstFile:= True;
  FFileViewPage.FileView.Visible:= False;
end;

procedure TQuickViewPanel.LoadFile(const aFileName: UTF8String);
begin
  if FFirstFile then
    begin
      FFirstFile:= False;
      FViewer.LoadFile(aFileName);
      FViewer.Show;
    end
  else
    begin
      FViewer.LoadNextFile(aFileName);
    end;
end;

end.

