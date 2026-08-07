unit uArchiveCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DCStrUtils,
  uFileSourceOperation,
  uFileSourceCopyOperation,
  uArchiveFileSource, uArchiveFileSourceUtil,
  uWcxArchiveFileSource, uWCXModule, WcxPlugin,
  uFileSystemFileSource,
  uFileSource,
  uTarWriter,
  uFile;

type
  TExtractFlag = (efSmartExtract);
  TExtractFlags = set of TExtractFlag;

  { TArchiveCopyInOperation }

  TArchiveCopyInOperation = class(TFileSourceCopyInOperation)
  private
    function doTarFiles(const files: TFiles): Integer;
  protected
    FStatistics: TFileSourceCopyOperationStatistics; // Local copy of statistics
    FTarWriter: TTarWriter;
    FPackingFlags: Integer; // Packing flags passed to plugin
    FFullFilesTree: TFiles; // Full list of files (recursive), only according to SourceFiles
    FCreateNew: Boolean;  // Create new archive
    FTarBefore: Boolean;  // Create TAR archive first
    FTarFileName: String; // Temporary TAR archive name
    FSetNewestFileTime: Boolean;

    function tarFiles: Boolean;
    function Tar(const archiveFS: IArchiveFileSource; var success: Boolean): Boolean;
    procedure DoReloadFileSources; override;
  public
    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;
    property CreateNew: Boolean read FCreateNew write FCreateNew;
    property NewestFileTime: Boolean read FSetNewestFileTime write FSetNewestFileTime;
  end;

  { TArchiveCopyOutOperation }

  TArchiveCopyOutOperation = class(TFileSourceCopyOutOperation)
  protected
    FExtractMask: String;
    FExtractFlags: TExtractFlags;
  public
    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;
    property ExtractMask: String read FExtractMask write FExtractMask;
    property ExtractFlags: TExtractFlags read FExtractFlags write FExtractFlags;
  end;

implementation

uses
  uArchiveFileSource, uArchiveFileSourceUtil, uGlobs,
  uLng;

{ TArchiveCopyInOperation }

function TArchiveCopyInOperation.doTarFiles(const files: TFiles): Integer;
var
  success: Boolean;
  currentFullFiles: TFiles = nil;
  uselessTotalFiles: Int64;
  uselessTotalBytes: Int64;
begin
  Result:= -1;
  try
    if Assigned(FFullFilesTree) then begin
      success:= FTarWriter.TarFiles(FFullFilesTree, FStatistics);
    end else begin
      uArchiveFileSourceUtil.FillAndCount(files,
                   currentFullFiles,
                   uselessTotalFiles,
                   uselessTotalBytes);    // gets full list of files (recursive)
      success:= FTarWriter.TarFiles(currentFullFiles, FStatistics);
    end;
    if success then
      Result:= 0;
  finally
    currentFullFiles.Free;
  end;
end;

function TArchiveCopyInOperation.tarFiles: Boolean;
var
  tarBeginResult: Boolean;
  resultCode: Integer;
begin
  Result:= False;
  tarBeginResult:= FTarWriter.TarBegin( FStatistics );
  if tarBeginResult then begin
    resultCode:= -1;
    try
      resultCode:= ProcessFilesWithMultiRootPath( SourceFiles, @self.doTarFiles );
    finally
      Result:= FTarWriter.TarEnd( FStatistics, resultCode=0 );
    end;
  end;
end;

// Result = True means that TarAndZip is processed by WCX or fail
function TArchiveCopyInOperation.Tar(
  const archiveFS: IArchiveFileSource;
  var success: Boolean ): Boolean;
var
  wcxFS: IWcxArchiveFileSource Absolute archiveFS;
  wcxModule: TWCXModule;
  archiveFileName: String;

  function needWcxModule: Boolean;
  begin
    Result:= False;
    if NOT (archiveFS is IWcxArchiveFileSource) then
      Exit;
    if NOT Assigned(wcxFS.WcxModule.PackToMem) then
      Exit;
    if (wcxFS.PluginCapabilities and PK_CAPS_MEMPACK) = 0 then
      Exit;
    Result:= True;
  end;

begin
  success:= False;
  Result:= needWcxModule;

  archiveFileName:= archiveFS.ArchiveFileName;
  if Result then begin
    wcxModule:= wcxFS.WcxModule;
  end else begin
    archiveFileName:= RemoveFileExt(archiveFileName);
    FTarFileName:= archiveFileName;
    wcxModule:= nil;
  end;

  FTarWriter:= TTarWriter.Create(
    archiveFileName,
    @AskQuestion,
    @RaiseAbortOperation,
    @CheckOperationState,
    @UpdateStatistics,
    wcxModule );

  try
    success:= TarFiles();
    if success then begin
      if Result = False then begin
        // Result = False means that Tar is processed internally
        // Fill file list with tar archive file
        SourceFiles.Clear;
        SourceFiles.Path:= ExtractFilePath(FTarFileName);
        SourceFiles.Add(TFileSystemFileSource.CreateFileFromFile(FTarFileName));
        // SourceFiles changed, FFullFilesTree becomes meaningless
        FreeAndNil(FFullFilesTree);
      end;
    end else begin
      // tar fail, Exit MainExecute()
      Result:= True;
    end;
  finally
    FreeAndNil(FTarWriter);
  end;
end;

procedure TArchiveCopyInOperation.DoReloadFileSources;
var
  ArchiveFileSource: IArchiveFileSource;
begin
  if not FCreateNew then inherited DoReloadFileSources;
  if FSetNewestFileTime or gSetNewestFileTime then
  begin
    ArchiveFileSource:= TargetFileSource as IArchiveFileSource;
    SetNewestFileTime(ArchiveFileSource.ArchiveFileName, ArchiveFileSource.GetFiles(PathDelim));
  end;
end;

function TArchiveCopyInOperation.GetDescription(Details: TFileSourceOperationDescriptionDetails): String;
begin
  case Details of
    fsoddJobAndTarget:
    begin
      if SourceFiles.Count = 1 then
        Result := Format(rsOperPackingSomethingTo, [SourceFiles[0].Name, TargetFileSource.CurrentAddress])
      else
        Result := Format(rsOperPackingFromTo, [SourceFiles.Path, TargetFileSource.CurrentAddress]);
    end;
    else
      Result := rsOperPacking;
  end;
end;

{ TArchiveCopyOutOperation }

function TArchiveCopyOutOperation.GetDescription(Details: TFileSourceOperationDescriptionDetails): String;
begin
  case Details of
    fsoddJobAndTarget:
      Result := Format(rsOperExtractingFromTo, [SourceFileSource.CurrentAddress, TargetPath]);
    else
      Result := rsOperExtracting;
  end;
end;

end.

