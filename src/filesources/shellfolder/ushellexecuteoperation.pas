unit uShellExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSource,
  uShellFileSource,
  uFileSourceExecuteOperation;

type

  { TShellExecuteOperation }

  TShellExecuteOperation = class(TFileSourceExecuteOperation)
  private
    FShellFileSource: IShellFileSource;
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

    procedure MainExecute; override;
  end;

implementation

uses
  Windows, ActiveX, ComObj, ShlObj, ShellAPI, DCOSUtils,
  DCConvertEncoding, fMain;

constructor TShellExecuteOperation.Create(aTargetFileSource: IFileSource;
  var aExecutableFile: TFile; aCurrentPath, aVerb: String);
begin
  FShellFileSource := aTargetFileSource as IShellFileSource;
  inherited Create(aTargetFileSource, aExecutableFile, aCurrentPath, aVerb);
end;

procedure TShellExecuteOperation.MainExecute;
var
  PIDL: PItemIDList;
  Menu: IContextMenu;
  AFolder: IShellFolder2;
  cmici: TCMInvokeCommandInfo;
  AExecInfo: TShellExecuteInfoW;
begin
  if Verb = 'properties' then
  try
    OleCheck(FShellFileSource.FindFolder(CurrentPath, AFolder));
    OleCheck(FShellFileSource.FindObject(AFolder, ExecutableFile.Name, PIDL));
    OleCheck(AFolder.GetUIObjectOf(frmMain.Handle, 1, PIDL, IID_IContextMenu, nil, Menu));
    if Assigned(Menu) then
    begin
      cmici:= Default(TCMInvokeCommandInfo);
      with cmici do
      begin
        cbSize := SizeOf(TCMInvokeCommandInfo);
        hwnd := frmMain.Handle;
        lpVerb := PAnsiChar(Verb);
        nShow := SW_SHOWNORMAL;
      end;
      OleCheck(Menu.InvokeCommand(cmici));
    end;
  except
    FExecuteOperationResult:= fseorError;
  end
  else if FShellFileSource.IsPathAtRoot(CurrentPath) then
  begin
    FResultString:= ExecutableFile.LinkProperty.LinkTo;
    FExecuteOperationResult:= fseorSymLink;
  end
  else begin
    AExecInfo:= Default(TShellExecuteInfoW);
    AExecInfo.cbSize:= SizeOf(TShellExecuteInfoW);

    if Failed(FShellFileSource.FindObject(AbsolutePath, AExecInfo.lpIDList)) then
      FExecuteOperationResult:= fseorError
    else begin
      AExecInfo.fMask:= SEE_MASK_IDLIST;

      if ShellExecuteExW(@AExecInfo) then
        FExecuteOperationResult:= fseorSuccess
      else begin
        FExecuteOperationResult:= fseorError;
      end;

      CoTaskMemFree(AExecInfo.lpIDList);
    end;
  end;
end;

end.

