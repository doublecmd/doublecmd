unit uWfxPluginFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSystemFile,
  uFileProperty,
  ufsplugin,
  uOSUtils;

type

  TWfxPluginFile = class(TFileSystemFile)
  private
    FIsLinkToDirectory: Boolean;

  public
    constructor Create; override;
    constructor Create(FindData: TWin32FindData); overload;

    destructor Destroy; override;

    {en
       Creates an identical copy of the object (as far as object data is concerned).
    }
    function Clone: TWfxPluginFile; override;
    procedure CloneTo(AFile: TFile); override;

    class function GetSupportedProperties: TFilePropertiesTypes; override;

    function IsLinkToDirectory: Boolean; override;

  end;

implementation

uses
  LCLProc, FileUtil;

constructor TWfxPluginFile.Create;
begin
  inherited Create;

  FIsLinkToDirectory := False;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := '';
end;

constructor TWfxPluginFile.Create(FindData: TWin32FindData);
begin
  inherited Create;

  Attributes := FindData.dwFileAttributes;
  Size := (Int64(FindData.nFileSizeHigh) * MAXDWORD) + FindData.nFileSizeLow;
  ModificationTime := FileTimeToDateTime(FindData.ftLastWriteTime);
  FIsLinkToDirectory := FPS_ISLNK(FindData.dwFileAttributes);

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := SysToUTF8(FindData.cFileName);
end;

destructor TWfxPluginFile.Destroy;
begin
  inherited Destroy;
end;

function TWfxPluginFile.Clone: TWfxPluginFile;
begin
  Result := TWfxPluginFile.Create;
  CloneTo(Result);
end;

procedure TWfxPluginFile.CloneTo(AFile: TFile);
begin
  if Assigned(AFile) then
  begin
    inherited CloneTo(AFile);
    // All properties are cloned in base class.

    with AFile as TWfxPluginFile do
    begin
      FIsLinkToDirectory := Self.FIsLinkToDirectory;
    end;
  end;
end;

class function TWfxPluginFile.GetSupportedProperties: TFilePropertiesTypes;
begin
  Result := [{fpName, }fpSize, fpAttributes, fpModificationTime];
end;

function TWfxPluginFile.IsLinkToDirectory: Boolean;
begin
  Result := FIsLinkToDirectory;
end;

end.

