unit uShellListOperation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Windows, ShlObj, ComObj,
  uFileSourceListOperation,
  uShellFileSource,
  uFileSource;

type

  { TShellListOperation }

  TShellListOperation = class(TFileSourceListOperation)
  private
    FShellFileSource: IShellFileSource;
    procedure ListFolder(AFolder: IShellFolder2; grfFlags: DWORD);
    procedure ListDrives;
    procedure ListDirectory;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  ActiveX, Variants, DCOSUtils, DCDateTimeUtils, ShellAPI,
  uFile, uShellFolder, uShlObjAdditional, uShowMsg, uShellFileSourceUtil;

{ TShellListOperation }

procedure TShellListOperation.ListFolder(AFolder: IShellFolder2; grfFlags: DWORD);
const
  SFGAOF_DEFAULT = SFGAO_STORAGE or SFGAO_HIDDEN or SFGAO_FOLDER;
var
  AFile: TFile;
  PIDL: PItemIDList;
  AValue: OleVariant;
  rgfInOut: LongWord;
  AParent: PItemIDList;
  NumIDs: LongWord = 0;
  EnumIDList: IEnumIDList;
begin
  OleCheckUTF8(SHGetIDListFromObject(AFolder, AParent));
  try
    OleCheckUTF8(AFolder.EnumObjects(0, grfFlags, EnumIDList));

    while EnumIDList.Next(1, PIDL, NumIDs) = S_OK do
    try
      CheckOperationState;

      aFile:= TShellFileSource.CreateFile(Path);

      AFile.Name:= GetDisplayNameEx(AFolder, PIDL, SHGDN_INFOLDER);
      TFileShellProperty(AFile.LinkProperty).Item:= ILCombine(AParent, PIDL);
      AFile.LinkProperty.LinkTo:= GetDisplayName(AFolder, PIDL, SHGDN_INFOLDER or SHGDN_FORPARSING);

      rgfInOut:= SFGAOF_DEFAULT;

      if Succeeded(AFolder.GetAttributesOf(1, PIDL, rgfInOut)) then
      begin
        if (rgfInOut and SFGAO_STORAGE <> 0) then
        begin
          AFile.Attributes:= FILE_ATTRIBUTE_DEVICE or FILE_ATTRIBUTE_VIRTUAL;
        end;
        if (rgfInOut and SFGAO_FOLDER <> 0) then
        begin
          AFile.Attributes:= AFile.Attributes or FILE_ATTRIBUTE_DIRECTORY;
        end;
        if (rgfInOut and SFGAO_HIDDEN <> 0) then
        begin
          AFile.Attributes:= AFile.Attributes or FILE_ATTRIBUTE_HIDDEN;
        end;
      end;

      AValue:= GetDetails(AFolder, PIDL, SCID_FileSize);
      if VarIsOrdinal(AValue) then
        AFile.Size:= AValue
      else if AFile.IsDirectory then
        AFile.Size:= 0
      else begin
        AFile.SizeProperty.IsValid:= False;
      end;

      AValue:= GetDetails(AFolder, PIDL, SCID_DateModified);
      if AValue <> Unassigned then
        AFile.ModificationTime:= AValue
      else begin
        AFile.ModificationTimeProperty.IsValid:= False;
      end;

      AValue:= GetDetails(AFolder, PIDL, SCID_DateCreated);
      if AValue <> Unassigned then
        AFile.CreationTime:= AValue
      else begin
        AFile.CreationTimeProperty.IsValid:= False;
      end;

      FFiles.Add(AFile);
    finally
      CoTaskMemFree(PIDL);
    end;
  finally
    CoTaskMemFree(AParent);
  end;
end;

procedure TShellListOperation.ListDrives;
const
  SFGAOF_DEFAULT = SFGAO_FILESYSTEM or SFGAO_FOLDER;
var
  AFile: TFile;
  PIDL: PItemIDList;
  rgfInOut: LongWord;
  AValue: OleVariant;
  NumIDs: LongWord = 0;
  AFolder: IShellFolder2;
  EnumIDList: IEnumIDList;
  DrivesPIDL: PItemIDList;
  DesktopFolder: IShellFolder;
begin
  OleCheckUTF8(SHGetDesktopFolder(DesktopFolder));
  OleCheckUTF8(SHGetFolderLocation(0, CSIDL_DRIVES, 0, 0, {%H-}DrivesPIDL));
  try
    OleCheckUTF8(DesktopFolder.BindToObject(DrivesPIDL, nil, IID_IShellFolder2, Pointer(AFolder)));

    OleCheckUTF8(AFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_STORAGE, EnumIDList));

    while EnumIDList.Next(1, PIDL, NumIDs) = S_OK do
    try
      CheckOperationState;

      aFile:= TShellFileSource.CreateFile(Path);

      AFile.Name:= GetDisplayNameEx(AFolder, PIDL, SHGDN_INFOLDER);
      TFileShellProperty(AFile.LinkProperty).Item:= ILCombine(DrivesPIDL, PIDL);
      AFile.LinkProperty.LinkTo:= GetDisplayName(AFolder, PIDL, SHGDN_INFOLDER or SHGDN_FORPARSING);

      rgfInOut:= SFGAOF_DEFAULT;
      AFile.Attributes:= FILE_ATTRIBUTE_DEVICE or FILE_ATTRIBUTE_VIRTUAL;

      if Succeeded(AFolder.GetAttributesOf(1, PIDL, rgfInOut)) then
      begin
        if (SFGAO_FILESYSTEM and rgfInOut) <> 0 then
        begin
          AFile.Attributes:= AFile.Attributes or FILE_ATTRIBUTE_NORMAL;
        end
        else if (rgfInOut and SFGAO_FOLDER <> 0) then
        begin
          AFile.Attributes:= AFile.Attributes or FILE_ATTRIBUTE_DIRECTORY;
        end;
      end;

      AFile.ModificationTimeProperty.IsValid:= False;

      AValue:= GetDetails(AFolder, PIDL, SCID_Capacity);
      if VarIsOrdinal(AValue) then
        AFile.Size:= AValue
      else if AFile.IsDirectory then
        AFile.Size:= 0
      else begin
        AFile.SizeProperty.IsValid:= False;
      end;

      FFiles.Add(AFile);
    finally
      CoTaskMemFree(PIDL);
    end;
  finally
    CoTaskMemFree(DrivesPIDL);
  end;
end;

procedure TShellListOperation.ListDirectory;
var
  AFolder: IShellFolder2;
begin
  if Succeeded(FShellFileSource.FindFolder(ExcludeTrailingBackslash(Path), AFolder)) then
  begin
    ListFolder(AFolder, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN);
  end;
end;

constructor TShellListOperation.Create(aFileSource: IFileSource;
  aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FShellFileSource:= aFileSource as IShellFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TShellListOperation.MainExecute;
begin
  FFiles.Clear;
  try
    if FShellFileSource.IsPathAtRoot(Path) then
      ListDrives
    else begin
      ListDirectory;
    end;
  except
    on E: Exception do msgError(Thread, E.Message);
  end;
end;

end.

