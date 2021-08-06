unit uRecycleBinFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  uFileSourceProperty,
  uVirtualFileSource, uFileProperty, uFileSource,
  uFileSourceOperation, uFile, uFileSourceOperationTypes;

type

  { IRecycleBinFileSource }

  IRecycleBinFileSource = interface(IVirtualFileSource)
    ['{1E598290-5E66-423C-BB55-333E293106E8}']
  end;

  { TRecycleBinFileSource }

  TRecycleBinFileSource = class(TVirtualFileSource, IRecycleBinFileSource)
  protected
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;
  public
    class function IsSupportedPath(const Path: String): Boolean; override;
    class function CreateFile(const APath: String): TFile; override;

    function GetOperationsTypes: TFileSourceOperationTypes; override;
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetLocalName(var aFile: TFile): Boolean; override;
    function GetRootDir(sPath: String): String; override; overload;
    function GetProperties: TFileSourceProperties; override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
  end;

implementation

uses
  uRecycleBinListOperation, uLng;

{ TRecycleBinFileSource }

function TRecycleBinFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  Result := IsPathAtRoot(NewDir);
end;

class function TRecycleBinFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= SameText(ExcludeTrailingBackslash(Path), PathDelim + PathDelim + PathDelim + rsVfsRecycleBin);
end;

class function TRecycleBinFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);
  with Result do
  begin
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes;
    SizeProperty := TFileSizeProperty.Create;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
    CreationTimeProperty := TFileCreationDateTimeProperty.Create;
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create;
    ChangeTimeProperty:= TFileChangeDateTimeProperty.Create;
    LinkProperty := TFileLinkProperty.Create;
    CommentProperty := TFileCommentProperty.Create;
  end;
end;

function TRecycleBinFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList];
end;

function TRecycleBinFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties
          + [fpSize,
             fpAttributes,
             fpModificationTime,
             fpCreationTime,
             fpLastAccessTime,
             fpChangeTime,
             uFileProperty.fpLink,
             fpComment
            ];
end;

function TRecycleBinFileSource.GetLocalName(var aFile: TFile): Boolean;
begin
  Result:= True;
  aFile.FullPath:= aFile.LinkProperty.LinkTo;
end;

function TRecycleBinFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + rsVfsRecycleBin + PathDelim;
end;

function TRecycleBinFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspDirectAccess, fspVirtual, fspLinksToLocalFiles];
end;

function TRecycleBinFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:= TRecycleBinListOperation.Create(TargetFileSource, TargetPath);
end;

end.

