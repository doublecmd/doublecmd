unit uArchiveFileSourceUtil;

{$mode delphi}{$H+}

interface

uses
 Classes, SysUtils,
 uFileView,
 uFile,
 uArchiveFileSource;

function GetArchiveFileSource(anArchiveFileName: String): IArchiveFileSource;

function TestArchive(aFileView: TFileView; aFiles: TFiles): Boolean;

implementation

uses
  fFileOpDlg,
  uShowMsg,
  uLng,
  uFileSource,
  uWcxArchiveFileSource,
  uMultiArchiveFileSource,
  uFileSystemFileSource,
  uFileSourceOperation,
  uFileSourceTestArchiveOperation,
  uFileSourceOperationTypes,
  uOperationsManager;

function GetArchiveFileSource(anArchiveFileName: String): IArchiveFileSource;
begin
  Result:= nil;

  // Check if there is a registered WCX plugin for possible archive.
  Result := FileSourceManager.Find(TWcxArchiveFileSource, anArchiveFileName) as IArchiveFileSource;
  if not Assigned(Result) then
    Result := TWcxArchiveFileSource.CreateByArchiveName(anArchiveFileName);

  // Check if there is a registered MultiArc addon for possible archive.
  if not Assigned(Result) then
    begin
      Result := FileSourceManager.Find(TMultiArchiveFileSource, anArchiveFileName) as IArchiveFileSource;
      if not Assigned(Result) then
        Result := TMultiArchiveFileSource.CreateByArchiveName(anArchiveFileName);
    end;
end;

function TestArchive(aFileView: TFileView; aFiles: TFiles): Boolean;
var
  I: Integer;
  FilesToTest: TFiles = nil;
  Operation: TFileSourceOperation = nil;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp = nil;
  ArchiveFileSource: IArchiveFileSource;
begin
  try
    // if in archive
    if aFileView.FileSource.IsClass(TArchiveFileSource) then
      begin
       FilesToTest := aFiles.Clone;
       if fsoTestArchive in aFileView.FileSource.GetOperationsTypes then
         begin
           Operation := aFileView.FileSource.CreateTestArchiveOperation(FilesToTest);

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
      if aFileView.FileSource.IsClass(TFileSystemFileSource) then
        begin
          for I := 0 to aFiles.Count - 1 do // test all selected archives
            begin
              // Check if there is a ArchiveFileSource for possible archive.
              ArchiveFileSource := GetArchiveFileSource(aFiles[i].FullPath);

              if Assigned(ArchiveFileSource) then
                begin
                  // Check if List and fsoTestArchive are supported.
                  if [fsoList, fsoTestArchive] * ArchiveFileSource.GetOperationsTypes = [fsoList, fsoTestArchive] then
                    begin
                      // Get files to test.
                      FilesToTest := ArchiveFileSource.GetFiles(ArchiveFileSource.GetRootDir);

                      if Assigned(FilesToTest) then
                      try
                        // test all files
                        Operation := ArchiveFileSource.CreateTestArchiveOperation(FilesToTest);

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
                        if Assigned(FilesToTest) then
                          FreeAndNil(FilesToTest);
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
  finally

  end;
end;

end.

