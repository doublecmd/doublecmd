unit uTrashFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Menus, uGLib2, uGio2, uFile, uFileSource, uGioFileSource,
  uFileSourceProperty, uFileProperty, uFileSourceOperationTypes, uFileSourceOperation;

type

  ITrashFileSource = interface(IGioFileSource)
    ['{5EABE432-2310-460B-8A59-32C2D2C28207}']
  end;

  { TTrashFileSource }

  TTrashFileSource = class(TGioFileSource, ITrashFileSource)
  private
    FFiles: TFiles;
    FMenu: TPopupMenu;
    procedure RestoreItem(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetFileSystem: String; override;
    function GetProperties: TFileSourceProperties; override;
    class function GetMainIcon(out Path: String): Boolean; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;
    class function IsSupportedPath(const Path: String): Boolean; override;
    function GetRetrievableFileProperties: TFilePropertiesTypes; override;
    function GetDefaultView(out DefaultView: TFileSourceFields): Boolean; override;
    function QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean; override;
    procedure RetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes; const AVariantProperties: array of String); override;

    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
  end;

implementation

uses
  System.UITypes, Dialogs, DCStrUtils,  uGObject2, uLng, uGio, uFileProcs,
  uGioFileSourceUtil, uTrashDeleteOperation;

const
  G_FILE_ATTRIBUTE_TRASH_ORIG_PATH = 'trash::orig-path';

{ TTrashFileSource }

procedure TTrashFileSource.RestoreItem(Sender: TObject);
var
  AFile: TFile;
  APath: String;
  AIndex: Integer;
  AInfo: PGFileInfo;
  AError: PGError = nil;
  SourceFile, TargetFile: PGFile;
begin
  for AIndex:= 0 to FFiles.Count - 1 do
  begin
    AFile:= FFiles[AIndex];
    SourceFile:= GioNewFile(AFile.FullPath);
    try
      AInfo:= g_file_query_info(SourceFile, G_FILE_ATTRIBUTE_TRASH_ORIG_PATH, G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, nil, nil);
      if Assigned(AInfo) then
      try
        APath:= g_file_info_get_attribute_byte_string(AInfo, G_FILE_ATTRIBUTE_TRASH_ORIG_PATH);
        mbForceDirectory(ExtractFileDir(APath));

        TargetFile:= GioNewFile(APAth);
        try
          if not g_file_move(SourceFile, TargetFile, G_FILE_COPY_NOFOLLOW_SYMLINKS or G_FILE_COPY_ALL_METADATA or G_FILE_COPY_NO_FALLBACK_FOR_MOVE, nil, nil, nil, @AError) then
          begin
            if Assigned(AError) then
            try
              if MessageDlg(AError^.message, mtError, [mbAbort, mbIgnore], 0, mbAbort) = mrAbort then
                Break;
            finally
              FreeAndNil(AError);
            end;
          end;
        finally
          g_object_unref(PGObject(TargetFile));
        end;
      finally
        g_object_unref(AInfo);
      end;
    finally
      g_object_unref(PGObject(SourceFile));
    end;
  end;
  Reload(PathDelim);
end;

constructor TTrashFileSource.Create;
begin
  inherited Create;
  FCurrentAddress:= 'trash://';
  FMenu:= TPopupMenu.Create(nil);
  FFiles:= TFiles.Create(EmptyStr);
end;

destructor TTrashFileSource.Destroy;
begin
  inherited Destroy;
  FFiles.Free;
  FMenu.Free;
end;

function TTrashFileSource.GetFileSystem: String;
begin
  Result:= 'Trash';
end;

function TTrashFileSource.GetProperties: TFileSourceProperties;
begin
  Result:= (inherited GetProperties) + [fspContextMenu, fspDefaultView];
end;

class function TTrashFileSource.GetMainIcon(out Path: String): Boolean;
begin
  Result:= True;
  Path:= 'user-trash';
end;

function TTrashFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result:= [fsoList, fsoCopyOut, fsoDelete, fsoCalcStatistics];
end;

class function TTrashFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= StrBegins(Path, 'trash://');
end;

function TTrashFileSource.GetDefaultView(out DefaultView: TFileSourceFields): Boolean;
begin
  Result:= True;
  SetLength(DefaultView, 3);
  DefaultView[0].Header:= rsColName;
  DefaultView[0].Content:= '[DC().GETFILENAMENOEXT{}]';
  DefaultView[0].Width:= 30;
  DefaultView[1].Header:= rsColExt;
  DefaultView[1].Content:= '[DC().GETFILEEXT{}]';
  DefaultView[1].Width:= 15;
  DefaultView[2].Header:= rsFuncTrashOrigPath;
  DefaultView[2].Content:= '[Plugin(FS).' + G_FILE_ATTRIBUTE_TRASH_ORIG_PATH + '{}]';
  DefaultView[2].Width:= 55;
end;

function TTrashFileSource.QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean;
var
  Index: Integer;
  MenuItem: TMenuItem;
begin
  if AFiles[0].Path = 'trash:///' then
  begin
    FMenu.Assign(AMenu);

    MenuItem:= TMenuItem.Create(FMenu);
    MenuItem.Caption:= '-';
    Index:= FMenu.Items.Count - 2;
    FMenu.Items.Insert(Index, MenuItem);

    MenuItem:= TMenuItem.Create(FMenu);
    MenuItem.Caption:= rsMnuRestore;
    MenuItem.OnClick:= @RestoreItem;
    Index:= FMenu.Items.Count - 2;
    FMenu.Items.Insert(Index, MenuItem);

    FFiles.Clear;
    AFiles.CloneTo(FFiles);

    AMenu:= FMenu;
  end;
  Result:= True;
end;

function TTrashFileSource.GetRetrievableFileProperties: TFilePropertiesTypes;
begin
  Result:= inherited GetRetrievableFileProperties + fpVariantAll;
end;

procedure TTrashFileSource.RetrieveProperties(AFile: TFile;
  PropertiesToSet: TFilePropertiesTypes; const AVariantProperties: array of String);
var
  AGFile: PGFile;
  AIndex: Integer;
  AInfo: PGFileInfo;
  AProp: TFilePropertyType;
  AVariant: TFileVariantProperty;
begin
  PropertiesToSet:= PropertiesToSet * fpVariantAll;
  for AProp in PropertiesToSet do
  begin
    AIndex:= Ord(AProp) - Ord(fpVariant);
    if (AIndex >= 0) and (AIndex <= High(AVariantProperties)) then
    begin
      AVariant:= TFileVariantProperty.Create(AVariantProperties[AIndex]);

      AGFile:= GioNewFile(AFile.FullPath);

      AInfo:= g_file_query_info(AGFile, 'trash::*', G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, nil, nil);
      if Assigned(AInfo) then
      begin
        AVariant.Value:= g_file_info_get_attribute_byte_string(AInfo, G_FILE_ATTRIBUTE_TRASH_ORIG_PATH);
        AFile.Properties[AProp]:= AVariant;
        g_object_unref(AInfo);
      end;
      g_object_unref(PGObject(AGFile));
    end;
  end;
end;

function TTrashFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  FilesToDelete.Path:= FCurrentAddress + FilesToDelete.Path;
  Result := TTrashDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

end.

