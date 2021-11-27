{
   Double Commander
   -------------------------------------------------------------------------
   Generic file view containing default panels (header, footer, etc.)

   Copyright (C) 2012-2018  Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2012  Przemyslaw Nagay (cobines@gmail.com)

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

unit uFileViewWithPanels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls,
  uFileView,
  uFileViewHeader,
  uFileSource;

type

  { TFileViewWithPanels }

  TFileViewWithPanels = class(TFileView)
  protected
    FSelectedCount: Integer;
    lblInfo: TLabel;

    pnlFooter: TPanel;
    pnlHeader: TFileViewHeader;

    procedure AfterChangePath; override;
    procedure CreateDefault(AOwner: TWinControl); override;
    procedure DisplayFileListChanged; override;
    procedure DoActiveChanged; override;
    procedure DoSelectionChanged; override;
    procedure ShowPathEdit;
    procedure DoUpdateView; override;
    procedure UpdateFlatFileName; virtual;
    procedure UpdateInfoPanel; virtual;

  public
    property Header:TFileViewHeader read pnlHeader;

    function AddFileSource(aFileSource: IFileSource; aPath: String): Boolean; override;
    function RemoveCurrentFileSource: Boolean; override;

  published
    procedure cm_EditPath(const {%H-}Params: array of string);
  end;

implementation

uses
  DCStrUtils, uFile, uGlobs, uLng, uFileProperty, uFileViewWorker, uDCUtils;

{ TFileViewWithPanels }

function TFileViewWithPanels.AddFileSource(aFileSource: IFileSource;
  aPath: String): Boolean;
begin
  Result:= inherited AddFileSource(aFileSource, aPath);
  if Result then pnlHeader.UpdateAddressLabel;
end;

procedure TFileViewWithPanels.AfterChangePath;
begin
  inherited AfterChangePath;
  if FileSourcesCount > 0 then
    pnlHeader.UpdatePathLabel;
end;

procedure TFileViewWithPanels.cm_EditPath(const Params: array of string);
begin
  ShowPathEdit;
end;

procedure TFileViewWithPanels.CreateDefault(AOwner: TWinControl);
begin
  inherited CreateDefault(AOwner);

  pnlHeader := TFileViewHeader.Create(Self, Self);

  pnlFooter                := TPanel.Create(Self);
  pnlFooter.Parent         := Self;
  pnlFooter.Align          := alBottom;
  pnlFooter.BevelInner     := bvNone;
  pnlFooter.BevelOuter     := bvNone;
  pnlFooter.AutoSize       := True;
  pnlFooter.DoubleBuffered := True;

  lblInfo          := TLabel.Create(pnlFooter);
  lblInfo.Parent   := pnlFooter;
  lblInfo.AutoSize := False;
  lblInfo.Align    := alClient;

  {$IF DEFINED(LCLGTK2)}
  // Workaround: "Layout and line"
  // http://doublecmd.sourceforge.net/mantisbt/view.php?id=573
  pnlFooter.Visible := False;
  {$ELSE}
  lblInfo.Height    := lblInfo.Canvas.TextHeight('Wg');
  {$ENDIF}

  {$IFDEF LCLCARBON}
  // Under Carbon AutoSize don't work without it
  pnlHeader.ClientHeight:= 0;
  pnlFooter.ClientHeight:= 0;
  {$ENDIF}
end;

procedure TFileViewWithPanels.DisplayFileListChanged;
begin
  inherited DisplayFileListChanged;
  UpdateInfoPanel;
end;

procedure TFileViewWithPanels.DoActiveChanged;
begin
  inherited DoActiveChanged;
  pnlHeader.SetActive(Active);
end;

procedure TFileViewWithPanels.DoSelectionChanged;
begin
  inherited DoSelectionChanged;
  UpdateInfoPanel;
end;

procedure TFileViewWithPanels.DoUpdateView;
begin
  inherited DoUpdateView;
  pnlHeader.Visible := gCurDir;     // Current directory
  pnlFooter.Visible := gStatusBar;  // Status bar
  pnlHeader.UpdateFont;
  pnlHeader.UpdateAddressLabel;
  pnlHeader.UpdatePathLabel;
end;

function TFileViewWithPanels.RemoveCurrentFileSource: Boolean;
begin
  Result:= inherited RemoveCurrentFileSource;
  if Result and (FileSourcesCount > 0) then
    pnlHeader.UpdateAddressLabel;
end;

procedure TFileViewWithPanels.ShowPathEdit;
begin
  pnlHeader.ShowPathEdit;
end;

procedure TFileViewWithPanels.UpdateFlatFileName;
var
  AFile: TFile;
begin
  AFile:= CloneActiveFile;
  if Assigned(AFile) then
  try
    lblInfo.Caption := MinimizeFilePath(ExtractDirLevel(CurrentPath, AFile.FullPath),
                                        lblInfo.Canvas, lblInfo.Width);
  finally
    AFile.Free;
  end;
end;

procedure TFileViewWithPanels.UpdateInfoPanel;
var
  i: Integer;
  FilesInDir, FilesSelected, FolderInDir, FolderSelected: Integer;
  SizeInDir, SizeSelected: Int64;
  SizeProperty: TFileSizeProperty;
begin
  FSelectedCount := 0;
  if GetCurrentWorkType = fvwtCreate then
  begin
    lblInfo.Caption := rsMsgLoadingFileList;
  end
  else if not Assigned(FAllDisplayFiles) or (FAllDisplayFiles.Count = 0) then
  begin
    lblInfo.Caption := rsMsgNoFiles;
  end
  else if Assigned(FileSource) then
  begin
    FilesInDir     := 0;
    FilesSelected  := 0;
    SizeInDir      := 0;
    SizeSelected   := 0;
    FolderInDir    := 0;
    FolderSelected := 0;

    for i := 0 to FFiles.Count - 1 do
    begin
      with FFiles[i] do
      begin
        if FSFile.Name = '..' then Continue;
        if FSFile.IsDirectory then
          inc(FolderInDir)
        else
          inc(FilesInDir);
        if Selected then
        begin
          if FSFile.IsDirectory then
            inc(FolderSelected)
          else
            inc(FilesSelected);
        end;

        // Count size if Size property exists.
        if fpSize in FSFile.AssignedProperties then
        begin
          SizeProperty := FSFile.SizeProperty;

          if Selected then
            SizeSelected := SizeSelected + SizeProperty.Value;

          SizeInDir := SizeInDir + SizeProperty.Value;
        end;
      end;
    end;

    FSelectedCount := FilesSelected + FolderSelected;

    if FlatView and (FSelectedCount = 0) then
      UpdateFlatFileName
    else
      lblInfo.Caption := Format(rsMsgSelectedInfo,
                                [cnvFormatFileSize(SizeSelected, uoscFooter),
                                 cnvFormatFileSize(SizeInDir, uoscFooter),
                                 FilesSelected,
                                 FilesInDir,
                                 FolderSelected,
                                 FolderInDir]);
  end
  else if not (csDestroying in ComponentState) then
    lblInfo.Caption := '';
end;

end.

