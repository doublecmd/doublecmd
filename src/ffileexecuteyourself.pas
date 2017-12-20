{
   Double Commander
   -------------------------------------------------------------------------
   Copy out, execute and delete files from non FileSystemFileSource

   Copyright (C) 2010-2016 Alexander Koblov (alexx2000@mail.ru)

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

unit fFileExecuteYourSelf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons,
  uFile, uFileSource, uFileView, uOSForms, uShowForm;

type

  { TfrmFileExecuteYourSelf }

  TfrmFileExecuteYourSelf = class(TAloneForm)
    btnClose: TBitBtn;
    lblFromPath: TLabel;
    lblFileName: TLabel;
    lblFromPathValue: TLabel;
    lblFileNameValue: TLabel;
    lblPrompt: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FFileSource: IFileSource;
    FWaitData: TWaitData;
  public
    constructor Create(TheOwner: TComponent; aFileSource: IFileSource; const FileName, FromPath: String); reintroduce;
    destructor Destroy; override;
  end; 

  procedure ShowFileEditExternal(const FileName, FromPath: string; aWaitData: TWaitData);
  function ShowFileExecuteYourSelf(aFileView: TFileView; aFile: TFile; bWithAll: Boolean): Boolean;

implementation

{$R *.lfm}

uses
  LCLProc, uTempFileSystemFileSource, uFileSourceOperation, uShellExecute, DCOSUtils;

procedure ShowFileEditExternal(const FileName, FromPath: string; aWaitData: TWaitData);
begin
  // Create wait window
  with TfrmFileExecuteYourSelf.Create(Application, nil, FileName, FromPath) do
  begin
    FWaitData:= aWaitData;
    // Show wait window
    Visible := True;
  end;
end;

function ShowFileExecuteYourSelf(aFileView: TFileView; aFile: TFile; bWithAll: Boolean): Boolean;
var
  TempFiles: TFiles = nil;
  TempFileSource: ITempFileSystemFileSource = nil;
  Operation: TFileSourceOperation = nil;
  CurrentDir,
  FileName: String;
begin
  Result:= False;
  try
    TempFileSource:= TTempFileSystemFileSource.GetFileSource;
    if bWithAll then
      begin
        FileName:= TempFileSource.FileSystemRoot + aFile.FullPath;
        TempFiles:= aFileView.FileSource.GetFiles(aFileView.FileSource.GetRootDir);
      end
    else
      begin
        FileName:= TempFileSource.FileSystemRoot + aFile.Name;
        TempFiles:= TFiles.Create(aFileView.CurrentPath);
        TempFiles.Add(aFile.Clone);
      end;
    Operation := aFileView.FileSource.CreateCopyOutOperation(
                            TempFileSource,
                            TempFiles,
                            TempFileSource.FileSystemRoot);

    if not Assigned(Operation) then Exit;
    // Execute operation
    Operation.Execute;
    // Create wait window
    with TfrmFileExecuteYourSelf.Create(Application, TempFileSource, aFile.Name, aFileView.CurrentAddress + aFileView.CurrentPath) do
    begin
      // Show wait window
      Show;
      // Save current directory
      CurrentDir:= mbGetCurrentDir;
      Result:= ShellExecuteEx('open', FileName, TempFileSource.FileSystemRoot + aFile.Path);
      // Restore current directory
      mbSetCurrentDir(CurrentDir);
      // If file can not be opened then close wait window
      if not Result then Close;
    end;
  finally
    FreeThenNil(Operation);
    FreeThenNil(TempFiles);
  end;
end;

{ TfrmFileExecuteYourSelf }

procedure TfrmFileExecuteYourSelf.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmFileExecuteYourSelf.FormCreate(Sender: TObject);
begin
  // Workaround: TWinControl.WMSize loop detected
  // http://doublecmd.sourceforge.net/mantisbt/view.php?id=1378
  Constraints.MaxWidth:= Screen.Width;
  Constraints.MaxHeight:= Screen.Height;
end;

constructor TfrmFileExecuteYourSelf.Create(TheOwner: TComponent;
  aFileSource: IFileSource; const FileName, FromPath: String);
begin
  inherited Create(TheOwner);
  FFileSource:= aFileSource;
  lblFileNameValue.Caption:= FileName;
  lblFromPathValue.Caption:= FromPath;
end;

destructor TfrmFileExecuteYourSelf.Destroy;
begin
  // Delete the temporary file source and all files inside.
  FFileSource:= nil;
  inherited Destroy;
  if Assigned(FWaitData) then FWaitData.Done;
end;

end.

