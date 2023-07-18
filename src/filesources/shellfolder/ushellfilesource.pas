unit uShellFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  Windows, ShlObj,
  uFileSourceProperty,
  uVirtualFileSource, uFileProperty, uFileSource,
  uFileSourceOperation, uFile, uFileSourceOperationTypes;

type

  { IShellFileSource }

  IShellFileSource = interface(IVirtualFileSource)
    ['{1E598290-5E66-423C-BB55-333E293106E8}']
    function CreateFolder(AParent: IShellFolder2; const Name: String): HRESULT;
    function FindFolder(const Path: String; out AValue: IShellFolder2): HRESULT;
    function FindObject(const AObject: String; out AValue: PItemIDList): HRESULT;
    function FindObject(AParent: IShellFolder2; const AName: String; out AValue: PItemIDList): HRESULT;
  end;

  { TShellFileSource }

  TShellFileSource = class(TVirtualFileSource, IShellFileSource)
  private
    FRootPath: String;
    FDrives: PItemIDList;
    FRootFolder: IShellFolder2;
    FDesktopFolder: IShellFolder;
  protected
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function IsSupportedPath(const Path: String): Boolean; override;
    class function CreateFile(const APath: String): TFile; override;
    class function GetMainIcon(out Path: String): Boolean; override;

    class function RootName: String;

    function CreateFolder(AParent: IShellFolder2; const Name: String): HRESULT;
    function FindFolder(const Path: String; out AValue: IShellFolder2): HRESULT;
    function FindObject(const AObject: String; out AValue: PItemIDList): HRESULT;
    function FindObject(AParent: IShellFolder2; const AName: String; out AValue: PItemIDList): HRESULT;

    function CreateDirectory(const Path: String): Boolean; override;
    function FileSystemEntryExists(const Path: String): Boolean; override;

    function GetOperationsTypes: TFileSourceOperationTypes; override;
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetRootDir(sPath: String): String; override; overload;
    function GetProperties: TFileSourceProperties; override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;
    function CreateMoveOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; override;
  end;

implementation

uses
  ActiveX, ComObj,DCConvertEncoding,  uShellFolder, uShellListOperation,
  uShellCopyOperation, uShellFileOperation, uShellCreateDirectoryOperation,
  uShellExecuteOperation, uShellSetFilePropertyOperation, uShellFileSourceUtil,
  uShellDeleteOperation, uShellMoveOperation, UShellCalcStatisticsOperation,
  DCStrUtils, uLng, uShlObjAdditional;

{ TShellFileSource }

function TShellFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  Result := True;
end;

constructor TShellFileSource.Create;
begin
  inherited Create;
  OleCheck(SHGetDesktopFolder(FDesktopFolder));
  OleCheck(SHGetFolderLocation(0, CSIDL_DRIVES, 0, 0, {%H-}FDrives));
  OleCheck(FDesktopFolder.BindToObject(FDrives, nil, IID_IShellFolder2, Pointer(FRootFolder)));
  FRootPath := GetDisplayName(FDesktopFolder, FDrives, SHGDN_INFOLDER);
  FOperationsClasses[fsoMove] := TShellMoveOperation.GetOperationClass;
  FOperationsClasses[fsoCopy] := TShellCopyOperation.GetOperationClass;
  FOperationsClasses[fsoCopyIn] := TShellCopyInOperation.GetOperationClass;
  FOperationsClasses[fsoCopyOut] := TShellCopyOutOperation.GetOperationClass;
end;

destructor TShellFileSource.Destroy;
begin
  inherited Destroy;
  CoTaskMemFree(FDrives);
end;

class function TShellFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= StrBegins(Path, PathDelim + PathDelim + PathDelim + RootName);
end;

class function TShellFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);
  with Result do
  begin
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes;
    SizeProperty := TFileSizeProperty.Create;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
    CreationTimeProperty := TFileCreationDateTimeProperty.Create;
    LinkProperty := TFileShellProperty.Create;
    CommentProperty := TFileCommentProperty.Create;
  end;
end;

class function TShellFileSource.GetMainIcon(out Path: String): Boolean;
begin
  Result:= True;
  Path:= '%SystemRoot%\System32\shell32.dll,15';
end;

class function TShellFileSource.RootName: String;
var
  DrivesPIDL: PItemIDList;
  DesktopFolder: IShellFolder;
begin
  OleCheckUTF8(SHGetDesktopFolder(DesktopFolder));
  OleCheckUTF8(SHGetFolderLocation(0, CSIDL_DRIVES, 0, 0, {%H-}DrivesPIDL));
  Result:= GetDisplayName(DesktopFolder, DrivesPIDL, SHGDN_INFOLDER);
end;

function TShellFileSource.FindObject(const AObject: String; out
  AValue: PItemIDList): HRESULT;
var
  APath: String;
  AFolder: IShellFolder2;
  AItemPIDL, AFolderPIDL: PItemIDList;
begin
  APath:= ExtractFileDir(AObject);
  Result:= FindFolder(APath, AFolder);

  if Succeeded(Result) then
  begin
    Result:= FindObject(AFolder, ExtractFileName(AObject), AItemPIDL);

    if Succeeded(Result) then
    begin
      Result:= SHGetIDListFromObject(AFolder, AFolderPIDL);
      if Succeeded(Result) then
      begin
        AValue:= ILCombine(AFolderPIDL, AItemPIDL);
        CoTaskMemFree(AFolderPIDL);
      end;
      CoTaskMemFree(AItemPIDL);
    end;
  end;
end;

function TShellFileSource.FindObject(AParent: IShellFolder2;
  const AName: String; out AValue: PItemIDList): HRESULT;
var
  AItemName: String;
  PIDL: PItemIDList;
  NumIDs: LongWord = 0;
  EnumIDList: IEnumIDList;
begin
  Result:= AParent.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_STORAGE or SHCONTF_INCLUDEHIDDEN, EnumIDList);

  if Succeeded(Result) then
  begin
    while EnumIDList.Next(1, PIDL, NumIDs) = S_OK do
    begin
      AItemName:= GetDisplayNameEx(AParent, PIDL, SHGDN_INFOLDER);
      if AName = AItemName then
      begin
        AValue:= PIDL;
        Exit(S_OK);
      end;
      CoTaskMemFree(PIDL);
    end;
  end;
  Result:= STG_E_FILENOTFOUND;
end;

function TShellFileSource.FindFolder(const Path: String; out
  AValue: IShellFolder2): HRESULT;

  function List(var AFolder: IShellFolder2; const AObject: String): HRESULT;
  var
    AName: String;
    PIDL: PItemIDList;
    NumIDs: LongWord = 0;
    AValue: IShellFolder2;
    EnumIDList: IEnumIDList;
  begin
    Result:= AFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_STORAGE or SHCONTF_INCLUDEHIDDEN, EnumIDList);

    if Succeeded(Result) then
    begin
      while EnumIDList.Next(1, PIDL, NumIDs) = S_OK do
      try
        AName:= GetDisplayNameEx(AFolder, PIDL, SHGDN_INFOLDER);

        if AName = AObject then
        begin
          Result:= AFolder.BindToObject(PIDL, nil, IID_IShellFolder2, Pointer(AValue));
          if Succeeded(Result) then AFolder:= AValue;
          Exit;
        end;
      finally
        CoTaskMemFree(PIDL);
      end;
    end;
    Result:= STG_E_PATHNOTFOUND;
  end;

var
  Index: Integer;
  APath: TStringArray;
begin
  APath:= Path.Split([PathDelim], TStringSplitOptions.ExcludeEmpty);

  if Length(APath) = 0 then
    Result:= STG_E_PATHNOTFOUND
  else begin
    if (APath[0] <> FRootPath) then
      Result:= STG_E_PATHNOTFOUND
    else begin
      AValue:= FRootFolder;
      // Find subdirectory
      for Index:= 1 to High(APath) do
      begin
        Result:= List(AValue, APath[Index]);
        if Failed(Result) then Exit;
      end;
    end;
  end;
end;

function TShellFileSource.CreateFolder(AParent: IShellFolder2;
  const Name: String): HRESULT;
var
  AName: WideString;
  AParentItem: IShellItem;
  AFileOp: IFileOperation;
  AParentPIDL: PItemIDList;
begin
  AName:= CeUtf8ToUtf16(Name);
  Result:= SHGetIDListFromObject(AParent, AParentPIDL);
  if Succeeded(Result) then
  try
    Result:= SHCreateItemFromIDList(AParentPIDL, IShellItem, AParentItem);
    if Succeeded(Result) then
    begin
      AFileOp:= CreateComObject(CLSID_FileOperation) as IFileOperation;
      Result:= AFileOp.NewItem(AParentItem, FILE_ATTRIBUTE_DIRECTORY, PWideChar(AName), nil, nil);
      if Succeeded(Result) then
      begin
        Result:= AFileOp.PerformOperations();
      end;
    end;
  finally
    CoTaskMemFree(AParentPIDL);
  end;
end;

function TShellFileSource.CreateDirectory(const Path: String): Boolean;
var
  AName: String;
  AParent: IShellFolder2;
begin
  AName:= ExtractFileName(Path);
  Result:= Succeeded(FindFolder(ExtractFileDir(Path), AParent));
  if Result then
  begin
    Result:= Succeeded(CreateFolder(AParent, AName));
  end;
end;

function TShellFileSource.FileSystemEntryExists(const Path: String): Boolean;
var
  AObject: PItemIDList;
begin
  Result:= Succeeded(FindObject(Path, AObject));
  if Result then CoTaskMemFree(AObject);
end;

function TShellFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoExecute, fsoDelete, fsoCreateDirectory,
             fsoCopyIn, fsoCopyOut, fsoSetFileProperty,
             fsoCalcStatistics];
end;

function TShellFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties
          + [fpSize,
             fpAttributes,
             fpModificationTime,
             fpCreationTime,
             uFileProperty.fpLink,
             fpComment
            ];
end;

function TShellFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + FRootPath + PathDelim;
end;

function TShellFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspVirtual];
end;

function TShellFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:= TShellListOperation.Create(TargetFileSource, TargetPath);
end;

function TShellFileSource.CreateDeleteOperation(var FilesToDelete: TFiles
  ): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:= TShellDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

function TShellFileSource.CreateCreateDirectoryOperation(BasePath: String;
  DirectoryPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TShellCreateDirectoryOperation.Create(TargetFileSource, BasePath, DirectoryPath);
end;

function TShellFileSource.CreateExecuteOperation(var ExecutableFile: TFile;
  BasePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:= TShellExecuteOperation.Create(TargetFileSource, ExecutableFile, BasePath, Verb);
end;

function TShellFileSource.CreateMoveOperation(var SourceFiles: TFiles;
  TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TShellMoveOperation.Create(TargetFileSource, SourceFiles, TargetPath);
end;

function TShellFileSource.CreateCopyOperation(var SourceFiles: TFiles;
  TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result:= TShellCopyOperation.Create(SourceFileSource, SourceFileSource, SourceFiles, TargetPath);
end;

function TShellFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:= TShellCopyInOperation.Create(SourceFileSource, TargetFileSource, SourceFiles, TargetPath);
end;

function TShellFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result := TShellCopyOutOperation.Create(SourceFileSource, TargetFileSource, SourceFiles, TargetPath);
end;

function TShellFileSource.CreateCalcStatisticsOperation(var theFiles: TFiles
  ): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TShellCalcStatisticsOperation.Create(TargetFileSource, theFiles);
end;

function TShellFileSource.CreateSetFilePropertyOperation(
  var theTargetFiles: TFiles; var theNewProperties: TFileProperties
  ): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TShellSetFilePropertyOperation.Create(TargetFileSource, theTargetFiles, theNewProperties);
end;

end.

