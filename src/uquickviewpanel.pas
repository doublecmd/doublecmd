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

{$IF DEFINED(DARWIN)}
  {$DEFINE NATIVE_QUICK_LOOK}
{$ENDIF}

unit uQuickViewPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls,
  uFileViewNotebook, uFile, uFileSource, uFileView;

type

  { IQuickViewPanel }

  // use class instead of interface because of mechanism of destructor
  IQuickViewPanel = class(TPanel)
  public
    procedure setFile(aFileView: TFileView; const aFile : TFile) virtual abstract;
  end;

procedure QuickViewShow(aFileViewPage: TFileViewPage; aFileView: TFileView);
procedure QuickViewClose;

var
  QuickViewPanel: IQuickViewPanel;

implementation

uses
  LCLProc, Forms, Controls, fMain, uTempFileSystemFileSource, uLng,
  uFileSourceProperty, uFileSourceOperation, uFileSourceOperationTypes,
  {$IF NOT DEFINED(NATIVE_QUICK_LOOK)}
  fViewer
  {$ELSE}
  uQuickLook
  {$ENDIF}
  ;

type

  { TBaseQuickViewPanel }

  TBaseQuickViewPanel = class(IQuickViewPanel)
  private
    FFileViewPage: TFileViewPage;
    FFileView: TFileView;
    FFileSource: IFileSource;
    FFileName: String;
  private
    procedure RaiseExit;
    procedure OnChangeFileView(Sender: TObject);
  protected
    procedure LoadFile(const aFileName: String) virtual abstract;
    procedure onSetFileException() virtual abstract;
  public
    procedure setFile(aFileView: TFileView; const aFile : TFile); override;
    constructor Create(TheOwner: TComponent; aParent: TFileViewPage; aFileView: TFileView); reintroduce;
    destructor Destroy; override;
  end;

  { TQuickViewPanel }

  {$IF NOT DEFINED(NATIVE_QUICK_LOOK)}

  TQuickViewPanel = class(TBaseQuickViewPanel)
  private
    FFirstFile: Boolean;
    FViewer: TfrmViewer;
  private
    procedure CreateViewer();
  protected
    procedure LoadFile(const aFileName: String) override;
    procedure onSetFileException() override;
  public
    constructor Create(TheOwner: TComponent; aParent: TFileViewPage; aFileView: TFileView); reintroduce;
    destructor Destroy; override;
  end;

  {$ELSE}

  TQuickViewPanel = class(TBaseQuickViewPanel)
  private
    QLControl: TDarwinQLControl;
  protected
    procedure LoadFile(const aFileName: String) override;
    procedure onSetFileException() override;
  public
    constructor Create(TheOwner: TComponent; aParent: TFileViewPage; aFileView: TFileView); reintroduce;
    destructor Destroy; override;
  end;

  {$ENDIF}

procedure QuickViewShow(aFileViewPage: TFileViewPage; aFileView: TFileView);
var
  aFile: TFile = nil;
begin
  QuickViewPanel:= TQuickViewPanel.Create(Application, aFileViewPage, aFileView);
  aFile := aFileView.CloneActiveFile;
  try
    QuickViewPanel.setFile(aFileView, aFile);
  finally
    FreeAndNil(aFile);
  end;
  frmMain.actQuickView.Checked:= True;
end;

procedure QuickViewClose;
begin
  FreeAndNil(QuickViewPanel);
  frmMain.actQuickView.Checked:= False;
end;

{ TBaseQuickViewPanel }

constructor TBaseQuickViewPanel.Create(TheOwner: TComponent; aParent: TFileViewPage; aFileView: TFileView );
begin
  inherited Create(TheOwner);
  Parent:= aParent;
  Align:= alClient;
  FFileViewPage:= aParent;

  FFileView:= aFileView;
  FFileSource:= FFileView.FileSource;
  FFileViewPage.FileView.Visible:= False;
  FFileView.OnChangeActiveFile:= @setFile;
  TFileViewPage(FFileView.NotebookPage).OnChangeFileView:= @OnChangeFileView;
end;

destructor TBaseQuickViewPanel.Destroy;
begin
  FFileView.OnChangeActiveFile:= nil;
  TFileViewPage(FFileView.NotebookPage).OnChangeFileView:= nil;
  FFileViewPage.FileView.Visible:= True;
  FFileSource:= nil;
  FFileView.SetFocus;
  inherited Destroy;
end;

procedure TBaseQuickViewPanel.RaiseExit;
begin
  raise EAbort.Create(rsSimpleWordFailedExcla);
end;

procedure TBaseQuickViewPanel.OnChangeFileView(Sender: TObject);
begin
  FFileView:= TFileView(Sender);
  FFileView.OnChangeActiveFile:= @setFile;
end;

procedure TBaseQuickViewPanel.setFile(aFileView: TFileView; const aFile: TFile);
var
  ActiveFile: TFile = nil;
  TempFiles: TFiles = nil;
  Operation: TFileSourceOperation = nil;
  TempFileSource: ITempFileSystemFileSource = nil;
begin
  try
    if not (Assigned(aFile) and aFile.IsNameValid) then
      raise EAbort.Create(rsMsgErrNotSupported);

    try
      // If files are links to local files
      if (fspLinksToLocalFiles in aFileView.FileSource.Properties) then
      begin
        if aFile.IsDirectory or aFile.IsLinkToDirectory then RaiseExit;
        FFileSource := aFileView.FileSource;
        ActiveFile:= aFile.Clone;
        if not FFileSource.GetLocalName(ActiveFile) then RaiseExit;
      end
      // If files not directly accessible copy them to temp file source.
      else if not (fspDirectAccess in aFileView.FileSource.Properties) then
      begin
        if aFile.IsDirectory or SameText(FFileName, aFile.Name) then RaiseExit;
        if not (fsoCopyOut in aFileView.FileSource.GetOperationsTypes) then RaiseExit;

        ActiveFile:= aFile.Clone;
        TempFiles:= TFiles.Create(ActiveFile.Path);
        TempFiles.Add(aFile.Clone);

        if FFileSource.IsClass(TTempFileSystemFileSource) then
          TempFileSource := (FFileSource as ITempFileSystemFileSource)
        else
          TempFileSource := TTempFileSystemFileSource.GetFileSource;

        Operation := aFileView.FileSource.CreateCopyOutOperation(
                         TempFileSource,
                         TempFiles,
                         TempFileSource.FileSystemRoot);

        if not Assigned(Operation) then RaiseExit;

        aFileView.Enabled:= False;
        try
          Operation.Execute;
        finally
          FreeAndNil(Operation);
          aFileView.Enabled:= True;
        end;

        FFileName:= ActiveFile.Name;
        FFileSource := TempFileSource;
        ActiveFile.Path:= TempFileSource.FileSystemRoot;
      end
      else begin
        // We can use the file source directly.
        FFileSource := aFileView.FileSource;
        ActiveFile:= aFile.Clone;
      end;

      LoadFile(ActiveFile.FullPath);

    finally
      FreeAndNil(TempFiles);
      FreeAndNil(ActiveFile);
    end;
  except
    on E: EAbort do
    begin
      Caption:= E.Message;
      onSetFileException();
    end;
  end;
end;

{ TQuickViewPanel }

{$IF NOT DEFINED(NATIVE_QUICK_LOOK)}

constructor TQuickViewPanel.Create(TheOwner: TComponent; aParent: TFileViewPage; aFileView: TFileView);
begin
  inherited Create(TheOwner, aParent, aFileView );
  FFirstFile:= True;
  CreateViewer();
end;

destructor TQuickViewPanel.Destroy;
begin
  FViewer.ExitQuickView;
  FreeThenNil(FViewer);
  inherited Destroy;
end;

procedure TQuickViewPanel.CreateViewer();
begin
  FViewer:= TfrmViewer.Create(Self, nil, True);
  FViewer.Parent:= Self;
  FViewer.BorderStyle:= bsNone;
  FViewer.Align:= alClient;
end;

procedure TQuickViewPanel.LoadFile(const aFileName: String);
begin
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
  if not FFileView.Focused then FFileView.SetFocus;
end;

procedure TQuickViewPanel.onSetFileException();
begin
  FFirstFile:= True;
  FViewer.Hide;
  FViewer.LoadFile(EmptyStr);
end;

{$ELSE}

constructor TQuickViewPanel.Create(TheOwner: TComponent; aParent: TFileViewPage; aFileView: TFileView);
begin
  inherited Create(TheOwner, aParent, aFileView );
  QLControl:= TDarwinQLControl.Create( self );
  QLControl.open;
end;

destructor TQuickViewPanel.Destroy;
begin
  FreeAndNil( QLControl );
  inherited Destroy;
end;

procedure TQuickViewPanel.LoadFile(const aFileName: String);
begin
  QLControl.filepath:= aFileName;
end;

procedure TQuickViewPanel.onSetFileException();
begin
end;

{$ENDIF}

end.

