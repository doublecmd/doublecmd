{
   Double Commander
   -------------------------------------------------------------------------
   Quick view panel

   Copyright (C) 2009-2022 Alexander Koblov (alexx2000@mail.ru)

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
  Classes, SysUtils, Controls, ExtCtrls, fViewer,
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

    function handleLinksToLocal( const Sender:TFileView; const aFile:TFile; var fullPath:String; var showMsg:String ): Boolean;
    function handleNotDirect( const Sender:TFileView; const aFile:TFile; var fullPath:String; var showMsg:String ): Boolean;
    function handleDirect( const Sender:TFileView; const aFile:TFile; var fullPath:String; var showMsg:String ): Boolean;
  protected
     procedure DoOnShowHint(HintInfo: PHintInfo) override;
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
  LCLProc, Forms, fMain, uTempFileSystemFileSource, uLng,
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
  frmMain.actQuickView.Checked:= True;
end;

procedure QuickViewClose;
begin
  if Assigned(QuickViewPanel) then
  begin
    FreeAndNil(QuickViewPanel);
    frmMain.actQuickView.Checked:= False;
  end;
end;

{ TQuickViewPanel }

procedure TQuickViewPanel.DoOnShowHint(HintInfo: PHintInfo);
begin
  HintInfo^.HintStr:= '';
end;

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
  FViewer.ExitQuickView;
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
  FViewer.ShowHint:= false;
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
var
  lastFocusedControl: TWinControl;
begin
  lastFocusedControl:= TCustomForm(self.GetTopParent).ActiveControl;
  if (not FFirstFile) then
  begin
    FViewer.LoadNextFile(aFileName);
  end
  else begin
    FFirstFile:= False;
    Caption:= EmptyStr;
    FViewer.LoadFile(aFileName);
    FViewer.Show;
  end;
  // Viewer can steal focus, so restore it
  if Assigned(lastFocusedControl) then lastFocusedControl.SetFocus;
end;

procedure TQuickViewPanel.OnChangeFileView(Sender: TObject);
begin
  FFileView:= TFileView(Sender);
  FFileView.OnChangeActiveFile:= @FileViewChangeActiveFile;
end;

procedure TQuickViewPanel.FileViewChangeActiveFile(Sender: TFileView; const aFile: TFile);
var
  fullPath: String;
  showMsg: String;
begin
  fullPath:= EmptyStr;
  showMsg:= EmptyStr;
  try
    if not Assigned(aFile) then
      raise EAbort.Create(rsMsgErrNotSupported);

    if not handleLinksToLocal(Sender, aFile, fullPath, showMsg) then
      if not handleNotDirect(Sender, aFile, fullPath, showMsg) then
        handleDirect(Sender, aFile, fullPath, showMsg);

    if fullPath.IsEmpty() and ShowMsg.IsEmpty() then
      showMsg:= rsMsgErrNotSupported;
  except
    on E: EAbort do
    begin
      showMsg:= E.Message;
    end;
  end;

  if not fullPath.IsEmpty() then begin
    LoadFile( fullPath );
  end else begin
    FViewer.Hide;
    FFirstFile:= True;
    Caption:= showMsg;
    FViewer.LoadFile(EmptyStr);
  end;
end;

// return true if it should handle it, otherwise return false
// If files are links to local files
// for example: results from searching
function TQuickViewPanel.handleLinksToLocal( const Sender:TFileView; const aFile:TFile; var fullPath:String; var showMsg:String ): Boolean;
var
  ActiveFile: TFile = nil;
begin
  if not (fspLinksToLocalFiles in Sender.FileSource.Properties) then exit(false);
  Result:= true;
  if not aFile.IsNameValid then exit;

  FFileSource := Sender.FileSource;
  ActiveFile:= aFile.Clone;

  try
    if not FFileSource.GetLocalName(ActiveFile) then exit;
    fullPath:= ActiveFile.FullPath;
  finally
    FreeAndNil(ActiveFile);
  end;
end;

// return true if it should handle it, otherwise return false
// If files not directly accessible copy them to temp file source.
// for examples: ftp
function TQuickViewPanel.handleNotDirect( const Sender:TFileView; const aFile:TFile; var fullPath:String; var showMsg:String ): Boolean;
var
  ActiveFile: TFile = nil;
  TempFiles: TFiles = nil;
  Operation: TFileSourceOperation = nil;
  TempFileSource: ITempFileSystemFileSource = nil;
begin
  if (fspDirectAccess in Sender.FileSource.Properties) then exit(false);
  Result:= true;
  if SameText(FFileName, aFile.Name) then exit;
  FFileName:= aFile.Name;

  if aFile.IsDirectory or aFile.IsLinkToDirectory then exit;
  if not (fsoCopyOut in Sender.FileSource.GetOperationsTypes) then exit;

  ActiveFile:= aFile.Clone;
  TempFiles:= TFiles.Create(ActiveFile.Path);
  TempFiles.Add(aFile.Clone);

  try
    if FFileSource.IsClass(TTempFileSystemFileSource) then
      TempFileSource := (FFileSource as ITempFileSystemFileSource)
    else
      TempFileSource := TTempFileSystemFileSource.GetFileSource;

    Operation := Sender.FileSource.CreateCopyOutOperation(
                     TempFileSource,
                     TempFiles,
                     TempFileSource.FileSystemRoot);

    if not Assigned(Operation) then exit;

    Sender.Enabled:= False;
    try
      Operation.Execute;
    finally
      FreeAndNil(Operation);
      Sender.Enabled:= True;
    end;

    FFileSource := TempFileSource;
    ActiveFile.Path:= TempFileSource.FileSystemRoot;
    fullPath:= ActiveFile.FullPath;
  finally
    FreeAndNil(TempFiles);
    FreeAndNil(ActiveFile);
  end;
end;

// return true if it should handle it, otherwise return false
// for examples: file system
function TQuickViewPanel.handleDirect( const Sender:TFileView; const aFile:TFile; var fullPath:String; var showMsg:String ): Boolean;
var
  parentDir: String;
begin
  Result:= true;
  FFileSource:= Sender.FileSource;

  if aFile.IsNameValid then begin
    fullPath:= aFile.FullPath;
  end else begin
    parentDir:= FFileSource.GetParentDir( aFile.Path );
    if FFileSource.IsPathAtRoot(parentDir) then
      showMsg:= rsPropsFolder + ': ' + parentDir
    else
      fullPath:= parentDir;
  end;
end;

end.

