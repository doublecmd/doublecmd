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
                       aExecutableFile: TFile;
                       aCurrentPath,
                       aVerb: UTF8String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  uWfxModule, uDCUtils;

constructor TVfsExecuteOperation.Create(
                aTargetFileSource: IFileSource;
                aExecutableFile: TFile;
                aCurrentPath,
                aVerb: UTF8String);
begin
  FVfsFileSource := aTargetFileSource as IVfsFileSource;
  inherited Create(aTargetFileSource, aExecutableFile, aCurrentPath, aVerb);
end;

procedure TVfsExecuteOperation.Initialize;
begin

end;

procedure TVfsExecuteOperation.MainExecute;
var
  sFileName: UTF8String;
  WfxModule: TWfxModule = nil;
begin
  FExecuteOperationResult:= fseorSuccess;
  if SameText(Verb, 'properties') then
    with FVfsFileSource do
    begin
      sFileName:= VfsFileList.Values[RelativePath];
      if sFileName <> EmptyStr then
        try
          sFileName:= GetCmdDirFromEnvVar(sFileName);
          WfxModule:= TWfxModule.Create;
          if WfxModule.LoadModule(sFileName) then
            begin
              WfxModule.VFSInit(0);
              WfxModule.VFSConfigure(0);
              WfxModule.UnloadModule;
            end;
        finally
          if Assigned(WfxModule) then
            FreeAndNil(WfxModule);
        end;
    end;
end;

procedure TVfsExecuteOperation.Finalize;
begin

end;

end.

