unit uArchiveCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceCopyOperation,
  uArchiveFileSourceUtil,
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

    function tarFiles: Boolean;
    procedure DoReloadFileSources; override;
  public
    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;
    property CreateNew: Boolean read FCreateNew write FCreateNew;
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

procedure TArchiveCopyInOperation.DoReloadFileSources;
begin
  if not FCreateNew then inherited DoReloadFileSources;
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

