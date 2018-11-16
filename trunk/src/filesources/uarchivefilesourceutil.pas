unit uArchiveFileSourceUtil;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,
 uFileView,
 uFile,
 uArchiveFileSource,
 uFileSource;

function GetArchiveFileSource(SourceFileSource: IFileSource;
                              ArchiveFile: TFile;
                              ArchiveType: String;
                              ArchiveSign: Boolean;
                              IncludeHidden: Boolean): IArchiveFileSource;

procedure TestArchive(aFileView: TFileView; aFiles: TFiles);

function FileIsArchive(const FileName: String): Boolean;

procedure FillAndCount(Files: TFiles; out NewFiles: TFiles;
                              out FilesCount: Int64; out FilesSize: Int64);

implementation

uses
  uFindEx,
  uShowMsg,
  uLng,
  DCStrUtils,
  uFileSourceProperty,
  uWcxArchiveFileSource,
  uMultiArchiveFileSource,
  uFileSystemFileSource,
  uTempFileSystemFileSource,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uOperationsManager;

// Only for direct access file sources.
function GetArchiveFileSourceDirect(SourceFileSource: IFileSource;
                                    ArchiveFileName: String;
                                    ArchiveType: String;
                                    ArchiveSign: Boolean;
                                    IncludeHidden: Boolean): IArchiveFileSource;
begin
  if not (fspDirectAccess in SourceFileSource.Properties) then
    Exit(nil);

  if (ArchiveType = EmptyStr) and (ArchiveSign = False) then
  begin
    ArchiveType := ExtractOnlyFileExt(ArchiveFileName);
  end;

  // Check if there is a registered WCX plugin for possible archive.
  Result := FileSourceManager.Find(TWcxArchiveFileSource, ArchiveFileName) as IArchiveFileSource;
  if not Assigned(Result) then
  begin
    if ArchiveSign then
      Result := TWcxArchiveFileSource.CreateByArchiveSign(SourceFileSource, ArchiveFileName)
    else
      Result := TWcxArchiveFileSource.CreateByArchiveType(SourceFileSource, ArchiveFileName, ArchiveType, IncludeHidden);
  end;
  // Check if there is a registered MultiArc addon for possible archive.
  if not Assigned(Result) then
  begin
    Result := FileSourceManager.Find(TMultiArchiveFileSource, ArchiveFileName) as IArchiveFileSource;
    if not Assigned(Result) then
    begin
      if ArchiveSign then
        Result := TMultiArchiveFileSource.CreateByArchiveSign(SourceFileSource, ArchiveFileName)
      else
        Result := TMultiArchiveFileSource.CreateByArchiveType(SourceFileSource, ArchiveFileName, ArchiveType);
    end;
  end;
end;

function GetArchiveFileSource(SourceFileSource: IFileSource;
                              ArchiveFile: TFile;
                              ArchiveType: String;
                              ArchiveSign: Boolean;
                              IncludeHidden: Boolean): IArchiveFileSource;
var
  TempFS: ITempFileSystemFileSource = nil;
  Operation: TFileSourceOperation = nil;
  Files: TFiles = nil;
  LocalArchiveFile: TFile;
begin
  if fspDirectAccess in SourceFileSource.Properties then
  begin
    Result := GetArchiveFileSourceDirect(SourceFileSource, ArchiveFile.FullPath, ArchiveType, ArchiveSign, IncludeHidden);
    Exit;
  end;

  Result := nil;

  if fspLinksToLocalFiles in SourceFileSource.Properties then
  begin
    LocalArchiveFile := ArchiveFile.Clone;
    try
      if SourceFileSource.GetLocalName(LocalArchiveFile) then
      begin
        TempFS := TTempFileSystemFileSource.Create(LocalArchiveFile.Path);
        // Source FileSource manages the files, not the TempFileSource.
        TempFS.DeleteOnDestroy := False;
        // The files on temp file source are valid as long as source FileSource is valid.
        TempFS.ParentFileSource := SourceFileSource;
        Result := GetArchiveFileSourceDirect(TempFS, LocalArchiveFile.FullPath, ArchiveType, ArchiveSign, IncludeHidden);
        // If not successful will try to get files through CopyOut below.
      end;
    finally
      FreeAndNil(LocalArchiveFile);
    end;
  end;

  if (not Assigned(Result)) and
     (fsoCopyOut in SourceFileSource.GetOperationsTypes) then
  begin
    if (ArchiveType = EmptyStr) and (ArchiveSign = False) then
    begin
      ArchiveType := ArchiveFile.Extension;
    end;

    // If checking by extension we don't have to unpack files yet.
    // First check if there is a registered plugin for the archive extension.
    if (not ArchiveSign) and
       (not (TWcxArchiveFileSource.CheckPluginByExt(ArchiveType) or
             TMultiArchiveFileSource.CheckAddonByExt(ArchiveType))) then
    begin
      // No registered handlers for the archive extension.
      Exit;
    end;
    // else either there is a handler for the archive extension
    //      or we have to unpack files first to check
    //      (if creating file source by archive signature).

    try
      TempFS := TTempFileSystemFileSource.Create;
      Files := TFiles.Create(ArchiveFile.Path);
      Files.Add(ArchiveFile.Clone);

      Operation := SourceFileSource.CreateCopyOutOperation(TempFS, Files, TempFS.FilesystemRoot);

      if Assigned(Operation) then
      begin
        Operation.Execute;

        if Operation.Result = fsorFinished then
        begin
          Result := GetArchiveFileSourceDirect(
                      TempFS,
                      IncludeTrailingPathDelimiter(TempFS.FilesystemRoot) + ArchiveFile.Name,
                      ArchiveType,
                      ArchiveSign,
                      IncludeHidden);
        end;
      end;

    finally
      TempFS := nil;
      if Assigned(Files) then
        FreeAndNil(Files);
      if Assigned(Operation) then
        FreeAndNil(Operation);
    end;
  end;
end;

procedure TestArchive(aFileView: TFileView; aFiles: TFiles);
var
  I: Integer;
  FilesToTest: TFiles = nil;
  Operation: TFileSourceOperation = nil;
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
               OperationsManager.AddOperation(Operation);
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
            try
              // Check if there is a ArchiveFileSource for possible archive.
              ArchiveFileSource := GetArchiveFileSource(aFileView.FileSource, aFiles[i], EmptyStr, False, True);

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
                            OperationsManager.AddOperation(Operation);
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
            except
              on E: Exception do msgError(E.Message + LineEnding + aFiles[i].FullPath);
            end; // for
        end
      else
        msgWarning(rsMsgErrNotSupported);
  finally

  end;
end;

function FileIsArchive(const FileName: String): Boolean;
var
  ArchiveType: String;
begin
  ArchiveType:= ExtractOnlyFileExt(FileName);
  Result:= TWcxArchiveFileSource.CheckPluginByExt(ArchiveType) or
           TMultiArchiveFileSource.CheckAddonByExt(ArchiveType);
end;

procedure FillAndCount(Files: TFiles; out NewFiles: TFiles;
                              out FilesCount: Int64; out FilesSize: Int64);

  procedure FillAndCountRec(const srcPath: String);
  var
    J: Integer;
    aFile: TFile;
    sr: TSearchRecEx;
    aFolders: TStringList;
  begin
    aFolders:= TStringList.Create;
    if FindFirstEx(srcPath + '*', 0, sr) = 0 then
    begin
      repeat
        if (sr.Name='.') or (sr.Name='..') then Continue;
        aFile := TFileSystemFileSource.CreateFile(srcPath, @sr);

        if aFile.IsDirectory and (not aFile.IsLinkToDirectory) then
        begin
          aFolders.AddObject(srcPath + sr.Name + DirectorySeparator, aFile);
        end
        else
        begin
          Inc(FilesCount);
          NewFiles.Add(aFile);
          FilesSize:= FilesSize + aFile.Size;
        end;
      until FindNextEx(sr) <> 0;
    end;
    // Process directories
    for J := 0 to aFolders.Count - 1 do
    begin
      NewFiles.Add(TFile(aFolders.Objects[J]));
      FillAndCountRec(aFolders[J]);  // go down to directory
    end;
    FindCloseEx(sr);
    aFolders.Free;
  end;

var
  I: Integer;
  aFile: TFile;
  aFolderList: TStringList;
begin
  FilesSize:= 0;
  FilesCount:= 0;
  aFolderList:= TStringList.Create;
  NewFiles := TFiles.Create(Files.Path);
  for I := 0 to Files.Count - 1 do
  begin
    aFile := Files[I];
    if aFile.IsDirectory and (not aFile.IsLinkToDirectory) then
    begin
      aFolderList.AddObject(aFile.Path + aFile.Name + DirectorySeparator, aFile.Clone);
    end
    else
    begin
      Inc(FilesCount);
      NewFiles.Add(aFile.Clone);
      FilesSize:= FilesSize + aFile.Size; // in first level we know file size -> use it
    end;
  end;
  // Process directories
  for I := 0 to aFolderList.Count - 1 do
  begin
    NewFiles.Add(TFile(aFolderList.Objects[I]));
    FillAndCountRec(aFolderList[I]);  // recursive browse child dir
  end;
  aFolderList.Free;
end;

end.

