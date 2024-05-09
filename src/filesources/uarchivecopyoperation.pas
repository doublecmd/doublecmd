unit uArchiveCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceCopyOperation,
  uFileSource,
  uFile;

type
  TExtractFlag = (efSmartExtract);
  TExtractFlags = set of TExtractFlag;

  { TArchiveCopyInOperation }

  TArchiveCopyInOperation = class(TFileSourceCopyInOperation)
  protected
    FStatistics: TFileSourceCopyOperationStatistics; // Local copy of statistics
    FPackingFlags: Integer; // Packing flags passed to plugin
    FFullFilesTree: TFiles; // Full list of files (recursive)
    FCreateNew: Boolean;  // Create new archive
    FTarBefore: Boolean;  // Create TAR archive first
    FTarFileName: String; // Temporary TAR archive name

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

