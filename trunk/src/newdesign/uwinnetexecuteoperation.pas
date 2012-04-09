unit uWinNetExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSource,
  uWinNetFileSource,
  uFileSourceExecuteOperation;

type

  { TWinNetExecuteOperation }

  TWinNetExecuteOperation = class(TFileSourceExecuteOperation)
  private
    FWinNetFileSource: IWinNetFileSource;
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
                       aVerb: UTF8String); override;

    procedure MainExecute; override;
  end;

implementation

uses
  Windows, JwaWinNetWk, DCStrUtils, DCOSUtils;

constructor TWinNetExecuteOperation.Create(aTargetFileSource: IFileSource;
  var aExecutableFile: TFile; aCurrentPath, aVerb: UTF8String);
begin
  FWinNetFileSource := aTargetFileSource as IWinNetFileSource;
  inherited Create(aTargetFileSource, aExecutableFile, aCurrentPath, aVerb);
end;

procedure TWinNetExecuteOperation.MainExecute;
var
  nFile: TNetResourceW;
  lpBuffer: array [0..4095] of Byte;
  ResInfo: TNetResourceW absolute lpBuffer;
  pszSystem: PWideChar;
  dwBufferSize: DWORD;
  dwResult: DWORD;
  FileName: WideString;
begin
  FExecuteOperationResult:= fseorError;
  FResultString:= IncludeFrontPathDelimiter(ExecutableFile.FullPath);
  // Workstation/Server
  if Pos('\\', FResultString) = 1 then
    begin
       FileName:= UTF8Decode(FResultString);
       with FWinNetFileSource do
       try
         dwBufferSize:= SizeOf(lpBuffer);
         FillChar(nFile, SizeOf(TNetResource), #0);
         nFile.dwScope:= RESOURCE_GLOBALNET;
         nFile.dwType:= RESOURCETYPE_ANY;
         nFile.lpRemoteName:= PWideChar(FileName);
         nFile.lpProvider:= PWideChar(ProviderName);

         dwResult:= WNetAddConnection2W(nFile, nil, nil, CONNECT_INTERACTIVE);
         if (dwResult <> NO_ERROR) then Exit;

         dwResult:= WNetGetResourceInformationW(nFile, @lpBuffer, dwBufferSize, pszSystem);
         if (dwResult <> NO_ERROR) then Exit;

         if (ResInfo.dwType = RESOURCETYPE_PRINT) then
         begin
           if (ShellExecuteW(0, 'open', ResInfo.lpRemoteName, nil, nil, SW_SHOW) > 32) then
             FExecuteOperationResult:= fseorSuccess;
           Exit;
         end;
       finally
         if (dwResult <> NO_ERROR) then
           FResultString:= mbSysErrorMessage(dwResult);
       end;
    end;
  FExecuteOperationResult:= fseorSymLink;
end;

end.

