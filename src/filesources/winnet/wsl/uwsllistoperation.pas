unit uWslListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSystemListOperation,
  uWinNetFileSource,
  uFileSource;

type

  { TWslListOperation }

  TWslListOperation = class(TFileSystemListOperation)
  private
    FWinNetFileSource: IWinNetFileSource;
  private
    procedure LinuxEnum;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  LazUTF8, uFile, Windows, uShowMsg, DCOSUtils, uMyWindows, ShlObj, ComObj,
  ActiveX, DCConvertEncoding, uShellFolder, uShlObjAdditional;

procedure TWslListOperation.LinuxEnum;
var
  AFile: TFile;
  pchEaten: ULONG;
  APath: UnicodeString;
  NumIDs: LongWord = 0;
  AFolder: IShellFolder;
  dwAttributes: ULONG = 0;
  EnumIDList: IEnumIDList;
  DesktopFolder: IShellFolder;
  PIDL, NetworkPIDL: PItemIDList;
begin
  try
    OleCheckUTF8(SHGetDesktopFolder(DesktopFolder));
    APath:= CeUtf8ToUtf16(ExcludeTrailingPathDelimiter(Path));
    OleCheckUTF8(DeskTopFolder.ParseDisplayName(0, nil, PWideChar(APath), pchEaten, NetworkPIDL, dwAttributes));
    try
      OleCheckUTF8(DesktopFolder.BindToObject(NetworkPIDL, nil, IID_IShellFolder, Pointer(AFolder)));
      OleCheckUTF8(AFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN, EnumIDList));

      while EnumIDList.Next(1, PIDL, NumIDs) = S_OK do
      try
        CheckOperationState;

        aFile:= TWinNetFileSource.CreateFile(Path);
        aFile.Attributes:= FILE_ATTRIBUTE_DIRECTORY;
        AFile.FullPath:= GetDisplayName(AFolder, PIDL, SHGDN_FORPARSING or SHGDN_FORADDRESSBAR);

        FFiles.Add(AFile);
      finally
        CoTaskMemFree(PIDL);
      end;
    finally
      CoTaskMemFree(NetworkPIDL);
    end;
  except
    on E: Exception do msgError(Thread, E.Message);
  end;
end;

constructor TWslListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FWinNetFileSource := aFileSource as IWinNetFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TWslListOperation.MainExecute;
begin
  FFiles.Clear;
  with FWinNetFileSource do
  begin
    if IsNetworkPath(Path) then
      LinuxEnum
    else begin
      inherited MainExecute;
    end;
  end;
end;

end.

