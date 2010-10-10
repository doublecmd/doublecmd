{
   Double Commander
   -------------------------------------------------------------------------
   File unpacking window

   Copyright (C) 2007-2010  Koblov Alexander (Alexx2000@mail.ru)

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

unit fExtractDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, EditBtn,
  uFile, uFileSource;

type

  { TfrmExtractDlg }

  TfrmExtractDlg = class(TForm)
    edtExtractTo: TDirectoryEdit;
    lblExtractTo : TLabel;
    lblFileMask : TLabel;
    cbFileMask : TComboBox;
    cbExtractPath : TCheckBox;
    cbOverwrite : TCheckBox;
    cbInSeparateFolder : TCheckBox;
    btnOK : TButton;
    btnCancel : TButton;
    btnHelp : TButton;
  private
    { private declarations }
  public
    { public declarations }
  end; 

// Frees 'SourceFiles'.
procedure ShowExtractDlg(SourceFileSource: IFileSource;
                         var SourceFiles: TFiles;
                         TargetFileSource: IFileSource;
                         sDestPath: String);

implementation

uses
  uGlobs, uDCUtils, uShowMsg, uLng,
  uArchiveFileSource,
  uFileSourceOperation,
  uFileSystemFileSource,
  uArchiveFileSourceUtil,
  uFileSourceOperationTypes,
  uOperationsManager,
  fFileOpDlg;
  
procedure ShowExtractDlg(SourceFileSource: IFileSource;
                         var SourceFiles: TFiles;
                         TargetFileSource: IFileSource;
                         sDestPath: String);
var
  I : Integer;
  FilesToExtract: TFiles;
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
  ArchiveFileSource: IArchiveFileSource;
  extractDialog: TfrmExtractDlg;
  Result: Boolean;
  sTmpPath: String;
begin
  if not TargetFileSource.IsClass(TFileSystemFileSource) then
  begin
    msgWarning(rsMsgErrNotSupported);
    Exit;
  end;

  extractDialog := TfrmExtractDlg.Create(nil);
  if Assigned(extractDialog) then
  try
    with extractDialog do
      begin
        edtExtractTo.Text := sDestPath;

        if SourceFileSource.IsClass(TArchiveFileSource) then
          cbInSeparateFolder.Visible := False;
        cbFileMask.Items.Assign(glsMaskHistory);

        Result:= (ShowModal = mrOK);

        if Result then
          begin
            if glsMaskHistory.IndexOf(cbFileMask.Text) < 0 then
              glsMaskHistory.Add(cbFileMask.Text);

            sDestPath := edtExtractTo.Text;

            // if in archive
            if SourceFileSource.IsClass(TArchiveFileSource) then
              begin
                if fsoCopyOut in SourceFileSource.GetOperationsTypes then
                begin
                  // if destination path is null then extract to path there archive is located
                  if Length(sDestPath) = 0 then
                     sDestPath:= ExtractFilePath((SourceFileSource as IArchiveFileSource).ArchiveFileName)
                  else
                     sDestPath:= IncludeTrailingPathDelimiter(sDestPath);

                  Operation := SourceFileSource.CreateCopyOutOperation(
                                 TargetFileSource,
                                 SourceFiles,
                                 sDestPath);

                  if Assigned(Operation) then
                  begin
                    // Start operation.
                    OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);

                    ProgressDialog := TfrmFileOp.Create(OperationHandle);
                    ProgressDialog.Show;
                  end
                  else
                    msgWarning(rsMsgNotImplemented);
                end
                else
                  msgWarning(rsMsgErrNotSupported);
              end
            else
            // if filesystem
            if SourceFileSource.IsClass(TFileSystemFileSource) then
            begin
              for I := 0 to SourceFiles.Count - 1 do // extract all selected archives
              begin
                // Check if there is a ArchiveFileSource for possible archive.
                ArchiveFileSource := GetArchiveFileSource(SourceFileSource, SourceFiles[i]);

                if Assigned(ArchiveFileSource) then
                begin
                  // Check if List and CopyOut are supported.
                  if [fsoList, fsoCopyOut] * ArchiveFileSource.GetOperationsTypes = [fsoList, fsoCopyOut] then
                  begin
                    // Get files to extract.
                    FilesToExtract := ArchiveFileSource.GetFiles(ArchiveFileSource.GetRootDir);

                    if Assigned(FilesToExtract) then
                    try
                      // if destination path is null then extract to path there archive is located
                      if Length(sDestPath) = 0 then
                        sTmpPath:= ExtractFilePath(ArchiveFileSource.ArchiveFileName)
                      else
                        sTmpPath:= IncludeTrailingPathDelimiter(sDestPath);

                      // if each archive in separate folder
                      if cbInSeparateFolder.Checked then
                        begin
                          sTmpPath := sTmpPath +
                                      ExtractOnlyFileName(ArchiveFileSource.ArchiveFileName) +
                                      PathDelim;
                        end;

                      // extract all files
                      Operation := ArchiveFileSource.CreateCopyOutOperation(
                                     TargetFileSource,
                                     FilesToExtract,
                                     sTmpPath);

                      if Assigned(Operation) then
                      begin
                        // Start operation.
                        OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);

                        ProgressDialog := TfrmFileOp.Create(OperationHandle);
                        ProgressDialog.Show;
                      end
                      else
                        msgWarning(rsMsgNotImplemented);

                    finally
                      if Assigned(FilesToExtract) then
                        FreeAndNil(FilesToExtract);
                    end;
                  end
                  else
                    msgWarning(rsMsgErrNotSupported);

                end;

                // Short pause, so that all operations are not spawned at once.
                Sleep(100);
              end; // for
            end
            else
              msgWarning(rsMsgErrNotSupported);

          end; // if Result
      end;

  finally
    if Assigned(extractDialog) then
      FreeAndNil(extractDialog);
    if Assigned(SourceFiles) then
      FreeAndNil(SourceFiles);
  end;
end;

initialization
  {$I fextractdlg.lrs}

end.

