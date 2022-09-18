unit uArchiveCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
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
    property CreateNew: Boolean read FCreateNew write FCreateNew;
  end;

  { TArchiveCopyOutOperation }

  TArchiveCopyOutOperation = class(TFileSourceCopyOutOperation)
  protected
    FExtractMask: String;
    FExtractFlags: TExtractFlags;
  public
    property ExtractMask: String read FExtractMask write FExtractMask;
    property ExtractFlags: TExtractFlags read FExtractFlags write FExtractFlags;
  end;

implementation

{ TArchiveCopyInOperation }

procedure TArchiveCopyInOperation.DoReloadFileSources;
begin
  if not FCreateNew then inherited DoReloadFileSources;
end;

end.

