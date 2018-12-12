{
   Double Commander
   -------------------------------------------------------------------------
   Take selected files and put them together to form one single file.

   Copyright (C) 2018 Alexander Koblov (alexx2000@mail.ru)

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

   Original comment:
   ----------------------------
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : Pavel Letko (letcuv@centrum.cz)
   File combiner
   contributors:
   Radek Cervinka
}

unit fLinker;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  SysUtils, Classes, Forms, Dialogs, StdCtrls,
  //DC
  fButtonForm, uFileSource, uFile;

type
  { TfrmLinker }
  TfrmLinker = class(TfrmButtonForm)
    lblFileName: TLabel;
    lstFile: TListBox;
    gbSaveTo: TGroupBox;
    edSave: TEdit;
    btnSave: TButton;
    grbxControl: TGroupBox;
    spbtnUp: TButton;
    spbtnDown: TButton;
    spbtnRem: TButton;
    dlgSaveAll: TSaveDialog;
    procedure spbtnUpClick(Sender: TObject);
    procedure spbtnDownClick(Sender: TObject);
    procedure spbtnRemClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

{ ShowLinkerFilesForm:
  "TMainCommands.cm_FileLinker" function from "uMainCommands.pas" is calling this routine.}
function ShowLinkerFilesForm(aFileSource: IFileSource; aFiles: TFiles; TargetPath: String): Boolean;

{ DoDynamicFilesLinking:
  "TMainCommands.cm_FileLinker" function from "uMainCommands.pas" is calling this routine.}
procedure DoDynamicFilesLinking(aFileSource: IFileSource; aFiles: TFiles; TargetPath, aFirstFilenameOfSeries: String);

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  LCLProc, Controls,
  //DC
  DCStrUtils, uLng, uFileProcs, uOperationsManager, uFileSourceCombineOperation,
  uGlobs;

{ ShowLinkerFilesForm:
  "TMainCommands.cm_FileLinker" function from "uMainCommands.pas" is calling this routine.}
function ShowLinkerFilesForm(aFileSource: IFileSource; aFiles: TFiles; TargetPath: String): Boolean;
var
  I: Integer;
  xFiles: TFiles = nil;
  Operation: TFileSourceCombineOperation = nil;
begin
  with TfrmLinker.Create(Application) do
  begin
    try
      // Fill file list box
      for I:= 0 to aFiles.Count - 1 do
      with lstFile.Items do
      begin
        AddObject(aFiles[I].Name, aFiles[I]);
      end;

      // Use first file name without extension as target file name
      edSave.Text:= TargetPath + aFiles[0].NameNoExt;

      // Show form
      Result:= (ShowModal = mrOk);

      if Result then
      begin
        if mbForceDirectory(ExtractFileDir(edSave.Text)) then
        try
          // Fill file list with new file order
          xFiles:= TFiles.Create(aFiles.Path);
          for I:= 0 to lstFile.Count - 1 do
          with lstFile.Items do
          begin
            xFiles.Add(TFile(Objects[I]).Clone);
          end;
          Operation:= aFileSource.CreateCombineOperation(xFiles, edSave.Text) as TFileSourceCombineOperation;
          OperationsManager.AddOperation(Operation, QueueIdentifier, False);
        finally
          FreeThenNil(xFiles);
        end;
      end;
    finally
      Free;
    end;
  end;
end;

{ DoDynamicFilesLinking:
  "TMainCommands.cm_FileLinker" function from "uMainCommands.pas" is calling this routine.}
procedure DoDynamicFilesLinking(aFileSource: IFileSource; aFiles: TFiles; TargetPath, aFirstFilenameOfSeries: String);
var
  xFiles: TFiles = nil;
  Operation: TFileSourceCombineOperation = nil;
begin
  xFiles:= TFiles.Create(aFiles.Path);
  try
    //Fill file list with new file order
    xFiles.Add(aFiles[0].Clone);
    Operation:= aFileSource.CreateCombineOperation(xFiles, TargetPath + aFiles[0].NameNoExt) as TFileSourceCombineOperation;
    Operation.RequireDynamicMode:=TRUE;
    OperationsManager.AddOperation(Operation);
  finally
    FreeThenNil(xFiles);
  end;
end;

{ TfrmLinker.spbtnDownClick }
procedure TfrmLinker.spbtnDownClick(Sender: TObject);
var
  iSelected: Integer;
begin
  with lstFile do
  begin
    if ItemIndex < 0 then Exit;
    if ItemIndex = Items.Count - 1 then Exit;
    iSelected:= ItemIndex;
    Items.Move(iSelected, iSelected + 1);
    ItemIndex:= iSelected + 1;
  end;
end;

{ TfrmLinker.spbtnUpClick }
procedure TfrmLinker.spbtnUpClick(Sender: TObject);
var
  iSelected: Integer;
begin
  with lstFile do
  begin
    if ItemIndex < 1 then Exit;
    iSelected:= ItemIndex;
    Items.Move(iSelected, iSelected - 1);
    ItemIndex:= iSelected - 1;
  end;
end;

{ TfrmLinker.spbtnRemClick }
procedure TfrmLinker.spbtnRemClick(Sender: TObject);
begin
  with lstFile do
  begin
    if ItemIndex > -1 then Items.Delete(ItemIndex);
  end;
end;

{ TfrmLinker.btnSaveClick }
procedure TfrmLinker.btnSaveClick(Sender: TObject);
begin
  dlgSaveAll.InitialDir:= ExtractFileDir(edSave.Text);
  dlgSaveAll.FileName:= ExtractFileName(edSave.Text);
  if dlgSaveAll.Execute then edSave.Text:= dlgSaveAll.FileName;
end;

{TfrmLinker.FormCreate }
procedure TfrmLinker.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self); // Initialize property storage
  dlgSaveAll.Filter := ParseLineToFileFilter([rsFilterAnyFiles, '*.*']);
end;

end.
