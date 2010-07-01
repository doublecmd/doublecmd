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
    FFileSource: IFileSource;
    FViewer: TfrmViewer;
    FFileName: UTF8String;
  public
    constructor Create(TheOwner: TComponent; aParent: TFileViewPage); reintroduce;
    destructor Destroy; override;
    procedure CreateViewer(aFileSource: IFileSource);
    procedure LoadFile(const aFileName: UTF8String);
    procedure FileViewChangeActiveFile(Sender: TFileView; const aFile : TFile);
  end;

procedure QuickViewShow(aFileViewPage: TFileViewPage; const aFileSource: IFileSource);
procedure QuickViewClose;

var
  QuickViewPanel: TQuickViewPanel;

implementation

uses
  LCLProc, Forms, Controls, uTempFileSystemFileSource,
  uFileSourceProperty, uFileSourceOperation, uFileSourceOperationTypes;

procedure QuickViewShow(aFileViewPage: TFileViewPage; const aFileSource: IFileSource);
begin
  QuickViewPanel:= TQuickViewPanel.Create(Application, aFileViewPage);
  QuickViewPanel.CreateViewer(aFileSource);
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
  FViewer.ExitPluginMode;
  FFileViewPage.FileView.Visible:= True;
  FreeThenNil(FViewer);
  FFileSource:= nil;
  inherited Destroy;
end;

procedure TQuickViewPanel.CreateViewer(aFileSource: IFileSource);
begin
  FViewer:= TfrmViewer.Create(Self, nil);
  FViewer.Parent:= Self;
  FViewer.BorderStyle:= bsNone;
  FViewer.Menu:= nil;
  FViewer.Align:= alClient;
  FViewer.QuickView:= True;
  FFirstFile:= True;
  FFileSource:= aFileSource;
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
    FreeThenNil(TempFiles);
    FreeThenNil(ActiveFile);
  end;
end;

end.

