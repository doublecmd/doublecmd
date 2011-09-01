{
   Double Commander
   -------------------------------------------------------------------------
   Archivers options page

   Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit fOptionsArchivers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, ComCtrls, EditBtn, Grids, Buttons,
  fOptionsFrame;

type

  { TfrmOptionsArchivers }

  TfrmOptionsArchivers = class(TOptionsEditor)
    btnAutoConfig: TBitBtn;
    btnMultiArcAdd: TBitBtn;
    btnMultiArcApply: TBitBtn;
    btnMultiArcDelete: TBitBtn;
    btnMultiArcRename: TBitBtn;
    chkMultiArcDebug: TCheckBox;
    chkMultiArcEnabled: TCheckBox;
    chkMultiArcOutput: TCheckBox;
    edtArchiveAdd: TEdit;
    edtArchiveExtension: TEdit;
    edtArchiveExtract: TEdit;
    edtArchiveList: TEdit;
    edtArchiveListEnd: TEdit;
    edtArchiveListStart: TEdit;
    edtDescription: TEdit;
    fneArchiver: TFileNameEdit;
    gbArchiverOptions: TGroupBox;
    lblArchiveAdd: TLabel;
    lblArchiveExtension: TLabel;
    lblArchiveExtract: TLabel;
    lblArchiveList: TLabel;
    lblArchiveListEnd: TLabel;
    lblArchiveListFormat: TLabel;
    lblArchiveListStart: TLabel;
    lblArchiver: TLabel;
    lblDescription: TLabel;
    lbxMultiArc: TListBox;
    memArchiveListFormat: TMemo;
    pcArchiverCommands: TPageControl;
    pnlArchiverCommands: TPanel;
    pnlMultiArcButtons: TPanel;
    splMultiArc: TSplitter;
    stgArchiverCommands: TStringGrid;
    tbArchiverAdditional: TTabSheet;
    tbArchiverGeneral: TTabSheet;
    procedure btnAutoConfigClick(Sender: TObject);
    procedure btnMultiArcAddClick(Sender: TObject);
    procedure btnMultiArcApplyClick(Sender: TObject);
    procedure btnMultiArcDeleteClick(Sender: TObject);
    procedure btnMultiArcRenameClick(Sender: TObject);
    procedure chkMultiArcEnabledChange(Sender: TObject);
    procedure lbxMultiArcSelectionChange(Sender: TObject; User: boolean);
    procedure stgArchiverCommandsPrepareCanvas(Sender: TObject; aCol,
      aRow: Integer; aState: TGridDrawState);
    procedure tbArchiverAdditionalShow(Sender: TObject);
  private
    procedure FillArchiverList;
  protected
    procedure Init; override;
  public
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end; 

implementation

{$R *.lfm}

uses
  Dialogs, uGlobs, uLng, uMultiArc;

const
  stgArchiveTitle                = 0;
  stgArchiveDelete               = 1;
  stgArchiveTest                 = 2;
  stgArchiveExtractWithoutPath   = 3;
  stgArchiveSelfExtract          = 4;
  stgArchiveID                   = 5;
  stgArchiveIDPos                = 6;
  stgArchiveIDSeekRange          = 7;
  stgArchivePasswordQuery        = 8;
  stgArchiveFormMode             = 9;

{ TfrmOptionsArchivers }

procedure TfrmOptionsArchivers.btnAutoConfigClick(Sender: TObject);
begin
  gMultiArcList.AutoConfigure;
  lbxMultiArcSelectionChange(lbxMultiArc, True);
end;

procedure TfrmOptionsArchivers.btnMultiArcAddClick(Sender: TObject);
var
  sName: UTF8String;
  MultiArcItem: TMultiArcItem;
begin
  if InputQuery(Caption, rsOptArchiveTypeName, sName) then
    begin
      MultiArcItem:= TMultiArcItem.Create;
      lbxMultiArc.Items.AddObject(sName, MultiArcItem);
      gMultiArcList.Add(sName, MultiArcItem);
      lbxMultiArc.ItemIndex:= lbxMultiArc.Count - 1;
      pcArchiverCommands.Enabled:= (lbxMultiArc.Count <> 0);
      chkMultiArcEnabled.Enabled:= (lbxMultiArc.Count <> 0);
    end;
end;

procedure TfrmOptionsArchivers.btnMultiArcApplyClick(Sender: TObject);
begin
  if lbxMultiArc.ItemIndex < 0 then Exit;
  with TMultiArcItem(lbxMultiArc.Items.Objects[lbxMultiArc.ItemIndex]), stgArchiverCommands do
  begin
    FDescription:= edtDescription.Text;
    FArchiver:= fneArchiver.FileName;
    FExtension:= edtArchiveExtension.Text;
    FList:= edtArchiveList.Text;
    FStart:= edtArchiveListStart.Text;
    FEnd:= edtArchiveListEnd.Text;
    FFormat.Assign(memArchiveListFormat.Lines);
    FExtract:= edtArchiveExtract.Text;
    FAdd:= edtArchiveAdd.Text;
    FDelete:= Cells[1, stgArchiveDelete];
    FTest:= Cells[1, stgArchiveTest];
    FExtractWithoutPath:= Cells[1, stgArchiveExtractWithoutPath];
    FAddSelfExtract:= Cells[1, stgArchiveSelfExtract];
    FID:= Cells[1, stgArchiveID];
    FIDPos:= Cells[1, stgArchiveIDPos];
    FIDSeekRange:= Cells[1, stgArchiveIDSeekRange];
    FPasswordQuery:= Cells[1, stgArchivePasswordQuery];
    FFormMode:= StrToIntDef(Cells[1, stgArchiveFormMode], 0);
    FOutput:= chkMultiArcOutput.Checked;
    FDebug:= chkMultiArcDebug.Checked;
  end;
end;

procedure TfrmOptionsArchivers.btnMultiArcDeleteClick(Sender: TObject);
var
  I: Integer;
begin
  if lbxMultiArc.ItemIndex < 0 then Exit;
  I:= lbxMultiArc.ItemIndex;
  lbxMultiArc.Items.Delete(I);
  gMultiArcList.Delete(I);
  lbxMultiArc.ItemIndex:= lbxMultiArc.Count - 1;
  pcArchiverCommands.Enabled:= (lbxMultiArc.Count <> 0);
  chkMultiArcEnabled.Enabled:= (lbxMultiArc.Count <> 0);
end;

procedure TfrmOptionsArchivers.btnMultiArcRenameClick(Sender: TObject);
var
  sNewName: UTF8String;
begin
  if lbxMultiArc.ItemIndex < 0 then Exit;
  sNewName:= lbxMultiArc.Items[lbxMultiArc.ItemIndex];
  if InputQuery(Caption, rsOptArchiveTypeName, sNewName) then
    begin
      lbxMultiArc.Items[lbxMultiArc.ItemIndex]:= sNewName;
      gMultiArcList.Names[lbxMultiArc.ItemIndex]:= sNewName;
    end;
end;

procedure TfrmOptionsArchivers.chkMultiArcEnabledChange(Sender: TObject);
begin
  if lbxMultiArc.ItemIndex < 0 then Exit;
  with TMultiArcItem(lbxMultiArc.Items.Objects[lbxMultiArc.ItemIndex]) do
  FEnabled:= chkMultiArcEnabled.Checked;
end;

procedure TfrmOptionsArchivers.lbxMultiArcSelectionChange(Sender: TObject; User: boolean);
begin
  if lbxMultiArc.ItemIndex < 0 then
  with stgArchiverCommands do
    begin
      edtDescription.Text:= EmptyStr;
      fneArchiver.FileName:= EmptyStr;
      edtArchiveExtension.Text:= EmptyStr;
      edtArchiveList.Text:= EmptyStr;
      edtArchiveListStart.Text:= EmptyStr;
      edtArchiveListEnd.Text:= EmptyStr;
      memArchiveListFormat.Lines.Clear;
      edtArchiveExtract.Text:= EmptyStr;
      edtArchiveAdd.Text:= EmptyStr;
      Cells[1, stgArchiveDelete]:= EmptyStr;
      Cells[1, stgArchiveTest]:= EmptyStr;
      Cells[1, stgArchiveExtractWithoutPath]:= EmptyStr;
      Cells[1, stgArchiveSelfExtract]:= EmptyStr;
      Cells[1, stgArchiveID]:= EmptyStr;
      Cells[1, stgArchiveIDPos]:= EmptyStr;
      Cells[1, stgArchiveIDSeekRange]:= EmptyStr;
      Cells[1, stgArchivePasswordQuery]:= EmptyStr;
      Cells[1, stgArchiveFormMode]:= EmptyStr;
      chkMultiArcOutput.Checked:= False;
      chkMultiArcDebug.Checked:= False;
      chkMultiArcEnabled.Checked:= False;
      pcArchiverCommands.Enabled:= (lbxMultiArc.Count <> 0);
      chkMultiArcEnabled.Enabled:= (lbxMultiArc.Count <> 0);
    end
  else
    with TMultiArcItem(lbxMultiArc.Items.Objects[lbxMultiArc.ItemIndex]), stgArchiverCommands  do
    begin
      edtDescription.Text:= FDescription;
      fneArchiver.FileName:= FArchiver;
      edtArchiveExtension.Text:= FExtension;
      edtArchiveList.Text:= FList;
      edtArchiveListStart.Text:= FStart;
      edtArchiveListEnd.Text:= FEnd;
      memArchiveListFormat.Lines.Assign(FFormat);
      edtArchiveExtract.Text:= FExtract;
      edtArchiveAdd.Text:= FAdd;
      Cells[1, stgArchiveDelete]:= FDelete;
      Cells[1, stgArchiveTest]:= FTest;
      Cells[1, stgArchiveExtractWithoutPath]:= FExtractWithoutPath;
      Cells[1, stgArchiveSelfExtract]:= FAddSelfExtract;
      Cells[1, stgArchiveID]:= FID;
      Cells[1, stgArchiveIDPos]:= FIDPos;
      Cells[1, stgArchiveIDSeekRange]:= FIDSeekRange;
      Cells[1, stgArchivePasswordQuery]:= FPasswordQuery;
      Cells[1, stgArchiveFormMode]:= IntToStr(FFormMode);
      chkMultiArcOutput.Checked:= FOutput;
      chkMultiArcDebug.Checked:= FDebug;
      chkMultiArcEnabled.Checked:= FEnabled;
  end;
end;

procedure TfrmOptionsArchivers.stgArchiverCommandsPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  if aRow = 0 then
    stgArchiverCommands.Canvas.Brush.Color:= stgArchiverCommands.FixedColor
  else
    stgArchiverCommands.Canvas.Brush.Color:= stgArchiverCommands.Color;
end;

procedure TfrmOptionsArchivers.tbArchiverAdditionalShow(Sender: TObject);
var
  I, J: LongInt;
  iWidth: LongInt = 0;
begin
  for I:= 0 to stgArchiverCommands.RowCount - 1 do
  begin
    J:= stgArchiverCommands.Canvas.TextWidth(stgArchiverCommands.Cells[0, I]);
    if J > iWidth then iWidth:= J;
  end;
  stgArchiverCommands.ColWidths[0]:= iWidth + 12;
end;

procedure TfrmOptionsArchivers.FillArchiverList;
var
  I: Integer;
begin
  for I:= 0 to gMultiArcList.Count - 1 do
    lbxMultiArc.Items.AddObject(gMultiArcList.Names[I], gMultiArcList[I]);
  pcArchiverCommands.Enabled:= (lbxMultiArc.Count <> 0);
  chkMultiArcEnabled.Enabled:= (lbxMultiArc.Count <> 0);
  if lbxMultiArc.Count > 0 then
    lbxMultiArc.ItemIndex:= 0;
end;

procedure TfrmOptionsArchivers.Init;
begin
  // Localize additional archiver commands.
  stgArchiverCommands.Cells[0, stgArchiveTitle] := rsOptArchiveParam;
  stgArchiverCommands.Cells[1, stgArchiveTitle] := rsOptArchiveValue;
  stgArchiverCommands.Cells[0, stgArchiveDelete] := rsOptArchiveDelete;
  stgArchiverCommands.Cells[0, stgArchiveTest] := rsOptArchiveTest;
  stgArchiverCommands.Cells[0, stgArchiveExtractWithoutPath] := rsOptArchiveExtractWithoutPath;
  stgArchiverCommands.Cells[0, stgArchiveSelfExtract] := rsOptArchiveSelfExtract;
  stgArchiverCommands.Cells[0, stgArchiveID] := rsOptArchiveID;
  stgArchiverCommands.Cells[0, stgArchiveIDPos] := rsOptArchiveIDPos;
  stgArchiverCommands.Cells[0, stgArchiveIDSeekRange] := rsOptArchiveIDSeekRange;
  stgArchiverCommands.Cells[0, stgArchivePasswordQuery] := rsOptArchivePasswordQuery;
  stgArchiverCommands.Cells[0, stgArchiveFormMode] := rsOptArchiveFormMode;
end;

procedure TfrmOptionsArchivers.Load;
begin
  FillArchiverList;
end;

function TfrmOptionsArchivers.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
end;

initialization
  RegisterOptionsEditor(optedArchivers, TfrmOptionsArchivers);

end.

