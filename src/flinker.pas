{
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
  SysUtils, Classes, Forms, Dialogs, StdCtrls,
  uFileSource,
  uFile;

type

  { TfrmLinker }

  TfrmLinker = class(TForm)
    lblFileName: TLabel;
    lstFile: TListBox;
    gbSaveTo: TGroupBox;
    edSave: TEdit;
    btnSave: TButton;
    grbxControl: TGroupBox;
    btnOK: TButton;
    btnExit: TButton;
    spbtnUp: TButton;
    spbtnDown: TButton;
    spbtnDel: TButton;
    dlgSaveAll: TSaveDialog;
    procedure spbtnUpClick(Sender: TObject);
    procedure spbtnDownClick(Sender: TObject);
    procedure spbtnDelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ShowLinkerFilesForm(aFileSource: IFileSource; aFiles: TFiles; TargetPath: UTF8String): Boolean;

implementation

{$R *.lfm}

uses
  LCLProc, Controls, uFileProcs, uOperationsManager,
  uFileSourceCombineOperation;

function ShowLinkerFilesForm(aFileSource: IFileSource; aFiles: TFiles; TargetPath: UTF8String): Boolean;
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
          OperationsManager.AddOperation(Operation);
        finally
          FreeThenNil(xFiles);
        end;
      end;
    finally
      Free;
    end;
  end;
end;

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

procedure TfrmLinker.spbtnDelClick(Sender: TObject);
begin
  with lstFile do
  begin
    if ItemIndex > -1 then
      Items.Delete(ItemIndex);
  end;
end;

procedure TfrmLinker.btnSaveClick(Sender: TObject);
begin
  dlgSaveAll.InitialDir:= ExtractFileDir(edSave.Text);
  dlgSaveAll.FileName:= ExtractFileName(edSave.Text);

  if dlgSaveAll.Execute then
    edSave.Text:= dlgSaveAll.FileName;
end;

end.
