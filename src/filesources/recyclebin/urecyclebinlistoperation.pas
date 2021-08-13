unit uRecycleBinListOperation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSystemListOperation,
  uRecycleBinFileSource,
  uFileSource;

type

  { TRecycleBinListOperation }

  TRecycleBinListOperation = class(TFileSystemListOperation)
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  Windows, ShlObj, ComObj, JwaShlGuid, Variants, DCOSUtils, DCDateTimeUtils,
  uFile, uShellFolder, uShlObjAdditional, uShowMsg;

const
  SID_DISPLACED = '{9B174B33-40FF-11d2-A27E-00C04FC30871}';
  SCID_OriginalLocation: TSHColumnID = ( fmtid: SID_DISPLACED; pid: PID_DISPLACED_FROM );
  SCID_DateDeleted:      TSHColumnID = ( fmtid: SID_DISPLACED; pid: PID_DISPLACED_DATE );

{ TRecycleBinListOperation }

constructor TRecycleBinListOperation.Create(aFileSource: IFileSource;
  aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  inherited Create(aFileSource, aPath);
end;

procedure TRecycleBinListOperation.MainExecute;
var
  AFile: TFile;
  NumIDs: LongWord = 0;
  AFolder: IShellFolder2;
  EnumIDList: IEnumIDList;
  Attr: TFileAttributeData;
  DesktopFolder: IShellFolder;
  PIDL, TrashPIDL: PItemIDList;
begin
  FFiles.Clear;
  try
    OleCheckUTF8(SHGetDesktopFolder(DesktopFolder));
    OleCheckUTF8(SHGetFolderLocation(0, CSIDL_BITBUCKET, 0, 0, {%H-}TrashPIDL));
    OleCheckUTF8(DesktopFolder.BindToObject(TrashPIDL, nil, IID_IShellFolder2, Pointer(AFolder)));
    OleCheckUTF8(AFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN, EnumIDList));

    while EnumIDList.Next(1, PIDL, NumIDs) = S_OK do
    begin
      CheckOperationState;

      aFile:= TRecycleBinFileSource.CreateFile(Path);
      AFile.FullPath:= GetDisplayName(AFolder, PIDL, SHGDN_NORMAL);
      AFile.LinkProperty.LinkTo:= GetDisplayName(AFolder, PIDL, SHGDN_FORPARSING);

      if mbFileGetAttr(AFile.LinkProperty.LinkTo, Attr) then
      begin
        AFile.Size:= Attr.Size;
        AFile.Attributes:= Attr.Attr;
        AFile.CreationTime:= WinFileTimeToDateTime(Attr.PlatformTime);
        AFile.LastAccessTime:= WinFileTimeToDateTime(Attr.LastAccessTime);
        AFile.ModificationTime:= WinFileTimeToDateTime(Attr.LastWriteTime);
        AFile.CommentProperty.Value:= GetDetails(AFolder, PIDL, SCID_OriginalLocation);
        AFile.ChangeTime:= VariantTimeToDateTime(VarToDateTime(GetDetails(AFolder, PIDL, SCID_DateDeleted)));
      end;

      FFiles.Add(AFile);
    end;
  except
    on E: Exception do msgError(Thread, E.Message);
  end;
end;

end.

