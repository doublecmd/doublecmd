unit uArchiveFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  uClassesEx,
  DCStringHashListUtf8,
  DCOSUtils,
  uLocalFileSource,
  uFileSource,
  uFileSourceUtil,
  uFile,
  uFileProperty
  {$IFDEF DARWIN}
  ,uDarwinImage
  {$ENDIF}
  ;

type

  IArchiveFileSource = interface(ILocalFileSource)
    ['{13A8637C-FFDF-46B0-B5B4-E7C6851C157A}']

    function Changed: Boolean;
    function GetPacker: String;
    function GetArcFileList: TThreadObjectList;
    function GetArcFilenameList: TStringHashListUtf8;

    property Packer: String read GetPacker;

    property ArchiveFileName: String read GetCurrentAddress;
    property ArchiveFileList: TThreadObjectList read GetArcFileList;
    property ArchiveFileNameList: TStringHashListUtf8 read GetArcFilenameList;

  end;

  { TArchiveFileSource }

  TArchiveFileSource = class(TLocalFileSource, IArchiveFileSource)
  private
    FAttributeData: TFileAttributeData;

  protected
    // Wcx Header List, iterate in sequence
    FArcFileList : TThreadObjectList;
    // Filename List, index of Filename
    // FArcFileList should be locked before accessing FArcFilenameList
    FArcFilenameList: TStringHashListUtf8;

    function GetPacker: String; virtual; abstract;
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function ReadArchive: Boolean; virtual; abstract;
    procedure DoReload(const {%H-}PathsToReload: TPathsArray); override;

  public
    {en
      Creates an archive file source.

      @param(anArchiveFileSource
             File source that stores the archive.
             Usually it will be direct-access file source, like filesystem.)
      @param(anArchiveFileName
             Full path to the archive on the ArchiveFileSource.)
    }
    constructor Create(anArchiveFileSource: IFileSource;
                       anArchiveFileName: String); virtual reintroduce overload;
    destructor Destroy; override;

    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

    class function CreateFile(const APath: String): TFile; override;

    function GetCustomIcon(const path: String; const iconSize: Integer): TBitmap; override;

    function Changed: Boolean; virtual;

    function GetArcFileList: TThreadObjectList;
    // FArcFileList should be locked before calling GetArcFilenameList()
    function GetArcFilenameList: TStringHashListUtf8;

    property ArchiveFileName: String read GetCurrentAddress;
    property ArchiveFileList: TThreadObjectList read GetArcFileList;
    property ArchiveFileNameList: TStringHashListUtf8 read GetArcFilenameList;
  end;

implementation

constructor TArchiveFileSource.Create(anArchiveFileSource: IFileSource;
                                      anArchiveFileName: String);
begin
  FCurrentAddress := anArchiveFileName;
  inherited Create;
  FArcFileList := TThreadObjectList.Create;
  FArcFilenameList:= TStringHashListUtf8.Create(True);
  ParentFileSource := anArchiveFileSource;
  mbFileGetAttr(anArchiveFileName, FAttributeData);
end;

destructor TArchiveFileSource.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FArcFilenameList);
  FreeAndNil(FArcFileList);
end;

function TArchiveFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  Result := False;
  if NewDir = EmptyStr then
    Exit;
  if NewDir = GetRootDir() then
    Exit(True);
  Result:= DirectoryExists(self, NewDir);
end;

class function TArchiveFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
    SizeProperty := TFileSizeProperty.Create;
    CompressedSizeProperty := TFileCompressedSizeProperty.Create;
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
  end;
end;


function TArchiveFileSource.GetCustomIcon(const path: String;
  const iconSize: Integer): TBitmap;
begin
  Result:= nil;

  {$IFDEF DARWIN}
  if path = PathDelim then
    Result:= darwinImageCacheForExt.copyBitmapForFileExt( FCurrentAddress, iconSize );
  {$ENDIF}
end;

function TArchiveFileSource.Changed: Boolean;
var
  Attr: TFileAttributeData;
begin
  if not mbFileGetAttr(ArchiveFileName, Attr) then begin
    FAttributeData.Size:= 0;
    Result:= (FArcFileList.Count <> 0);
  end else begin
    Result:= (Attr.Size <> FAttributeData.Size) or
             (Attr.LastWriteTime <> FAttributeData.LastWriteTime);
    if Result then FAttributeData:= Attr;
  end;
end;

function TArchiveFileSource.GetArcFileList: TThreadObjectList;
begin
  if self.Changed then
    self.ReadArchive;
  Result := FArcFileList;
end;

function TArchiveFileSource.GetArcFilenameList: TStringHashListUtf8;
begin
  Result:= FArcFilenameList;
end;

function TArchiveFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties
          + [fpSize, fpCompressedSize, fpAttributes, fpModificationTime];
end;

procedure TArchiveFileSource.DoReload(const PathsToReload: TPathsArray);
begin
  // reset FAttributeData (updated timestamp)
  // avoids Changed() still return True after ReadArchive()
  self.Changed;

  self.ReadArchive;
end;

end.

