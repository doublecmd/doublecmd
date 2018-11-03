unit uVfsExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSource,
  uFileSourceExecuteOperation,
  uVfsFileSource;

type

  { TVfsExecuteOperation }

  TVfsExecuteOperation = class(TFileSourceExecuteOperation)
  private
    FVfsFileSource: IVfsFileSource;
  public
    {en
       @param(aTargetFileSource
              File source where the file should be executed.)
       @param(aExecutableFile
              File that should be executed.)
       @param(aCurrentPath
              Path of the file source where the execution should take place.)
    }
    constructor Create(aTargetFileSource: IFileSource;
                       var aExecutableFile: TFile;
                       aCurrentPath,
                       aVerb: String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  Forms, uWfxModule, uDCUtils, uGlobs;

constructor TVfsExecuteOperation.Create(
                aTargetFileSource: IFileSource;
                var aExecutableFile: TFile;
                aCurrentPath,
                aVerb: String);
begin
  FVfsFileSource := aTargetFileSource as IVfsFileSource;
  inherited Create(aTargetFileSource, aExecutableFile, aCurrentPath, aVerb);
end;

procedure TVfsExecuteOperation.Initialize;
begin

end;

procedure TVfsExecuteOperation.MainExecute;
var
  Index: Integer;
  WfxModule: TWfxModule;
begin
  FExecuteOperationResult:= fseorSuccess;
  if SameText(Verb, 'properties') then
  with FVfsFileSource do
  begin
    Index:= VfsFileList.FindFirstEnabledByName(RelativePath);
    if Index >= 0 then
    begin
      WfxModule:= gWFXPlugins.LoadModule(VfsFileList.FileName[Index]);
      if Assigned(WfxModule) then
      begin
        WfxModule.VFSInit;
        WfxModule.VFSConfigure(Application.MainForm.Tag);
      end;
    end;
  end;
end;

procedure TVfsExecuteOperation.Finalize;
begin

end;

end.

