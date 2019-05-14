{
   Double Commander
   -------------------------------------------------------------------------
   Quick view panel

   Copyright (C) 2009-2019 Alexander Koblov (alexx2000@mail.ru)

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

unit uQuickViewPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, fViewer,
  uFileViewNotebook, uFile, uFileSource, uFileView;

type

  { TQuickViewPanel }

  TQuickViewPanel = class(TPanel)
  private
    FFirstFile: Boolean;
    FFileViewPage: TFileViewPage;
    FFileView: TFileView;
    FFileSource: IFileSource;
    FViewer: TfrmViewer;
    FFileName: String;
  private
    procedure LoadFile(const aFileName: String);
    procedure OnChangeFileView(Sender: TObject);
    procedure CreateViewer(aFileView: TFileView);
    procedure FileViewChangeActiveFile(Sender: TFileView; const aFile : TFile);
  public
    constructor Create(TheOwner: TComponent; aParent: TFileViewPage); reintroduce;
    destructor Destroy; override;
  end;

procedure QuickViewShow(aFileViewPage: TFileViewPage; aFileView: TFileView);
procedure QuickViewClose;

var
  QuickViewPanel: TQuickViewPanel;

implementation

uses
  LCLProc, Forms, Controls, uTempFileSystemFileSource,
  uFileSourceProperty, uFileSourceOperation, uFileSourceOperationTypes;

procedure QuickViewShow(aFileViewPage: TFileViewPage; aFileView: TFileView);
var
  aFile: TFile = nil;
begin
  QuickViewPanel:= TQuickViewPanel.Create(Application, aFileViewPage);
  QuickViewPanel.CreateViewer(aFileView);
  aFile := aFileView.CloneActiveFile;
  try
    QuickViewPanel.FileViewChangeActiveFile(aFileView, aFile);
  finally
    FreeAndNil(aFile);
  end;
end;

procedure QuickViewClose;
begin
  FreeAndNil(QuickViewPanel);
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
  FFileView.OnChangeActiveFile:= nil;
  TFileViewPage(FFileView.NotebookPage).OnChangeFileView:= nil;
  FViewer.ExitPluginMode;
  FFileViewPage.FileView.Visible:= True;
  FreeThenNil(FViewer);
  FFileSource:= nil;
  FFileView.SetFocus;
  inherited Destroy;
end;

procedure TQuickViewPanel.CreateViewer(aFileView: TFileView);
begin
  FViewer:= TfrmViewer.Create(Self, nil, True);
  FViewer.Parent:= Self;
  FViewer.BorderStyle:= bsNone;
  FViewer.Align:= alClient;
  FFirstFile:= True;
  FFileView:= aFileView;
  FFileSource:= aFileView.FileSource;
  FFileViewPage.FileView.Visible:= False;
  FFileView.OnChangeActiveFile:= @FileViewChangeActiveFile;
  TFileViewPage(FFileView.NotebookPage).OnChangeFileView:= @OnChangeFileView;
end;

procedure TQuickViewPanel.LoadFile(const aFileName: String);
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
  // Viewer can steal focus, so restore it
  if not FFileView.Focused then FFileView.SetFocus;
end;

procedure TQuickViewPanel.OnChangeFileView(Sender: TObject);
begin
  FFileView:= TFileView(Sender);
  FFileView.OnChangeActiveFile:= @FileViewChangeActiveFile;
end;

procedure TQuickViewPanel.FileViewChangeActiveFile(Sender: TFileView; const aFile: TFile);
var
  ActiveFile: TFile = nil;
  TempFiles: TFiles = nil;
  TempFileSource: ITempFileSystemFileSource = nil;
  Operation: TFileSourceOperation = nil;
begin
  if not (Assigned(aFile) and (aFile.Name <> '..')) then Exit;
  try
    // If files are links to local files
    if (fspLinksToLocalFiles in Sender.FileSource.Properties) then
      begin
        if aFile.IsDirectory or aFile.IsLinkToDirectory then Exit;
        FFileSource := Sender.FileSource;
        ActiveFile:= aFile.Clone;
        if not FFileSource.GetLocalName(ActiveFile) then Exit;
      end
    // If files not directly accessible copy them to temp file source.
    else if not (fspDirectAccess in Sender.FileSource.Properties) then
      begin
        if aFile.IsDirectory or SameText(FFileName, aFile.Name) then Exit;
        if not (fsoCopyOut in Sender.FileSource.GetOperationsTypes) then Exit;

       ActiveFile:= aFile.Clone;
       TempFiles:= TFiles.Create(Sender.CurrentPath);
       TempFiles.Add(aFile.Clone);

       if FFileSource.IsClass(TTempFileSystemFileSource) then
         TempFileSource := (FFileSource as ITempFileSystemFileSource)
       else
         TempFileSource := TTempFileSystemFileSource.GetFileSource;

       Operation := Sender.FileSource.CreateCopyOutOperation(
                        TempFileSource,
                        TempFiles,
                        TempFileSource.FileSystemRoot);

       if not Assigned(Operation) then Exit;

       Operation.Execute;
       FreeAndNil(Operation);

       FFileName:= ActiveFile.Name;
       FFileSource := TempFileSource;
       ActiveFile.Path:= TempFileSource.FileSystemRoot;
     end
   else
     begin
       // We can use the file source directly.
       FFileSource := Sender.FileSource;
       ActiveFile:= aFile.Clone;
     end;

  LoadFile(ActiveFile.FullPath);

  finally
    FreeAndNil(TempFiles);
    FreeAndNil(ActiveFile);
  end;
end;

end.

