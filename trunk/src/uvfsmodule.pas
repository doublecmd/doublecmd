unit uVfsModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource;

type
  { TFileSourceClass }

  TFileSourceClass = class of TFileSource;

  { TVfsModule }

  TVfsModule = class
    FileSourceClass: TFileSourceClass;
  end;

  { TVfsModuleList }

  TVfsModuleList = class(TStringList)
  private
    function GetVfsModule(const S: String): TVfsModule;
  public
    destructor Destroy; override;
    property VfsModule[const S: String]: TVfsModule read GetVfsModule;
  end;

procedure RegisterVirtualFileSource(AName: UTF8String; AFileSourceClass: TFileSourceClass);

var
  // All of file sources from this list will be displayed
  // in the Virtual File System List. It can be used for example
  // for system specific virtual folders (Control Panel, Desktop, etc.)
  gVfsModuleList: TVfsModuleList;

implementation

procedure RegisterVirtualFileSource(AName: UTF8String; AFileSourceClass: TFileSourceClass);
var
  VfsModule: TVfsModule;
begin
  VfsModule:= TVfsModule.Create;
  VfsModule.FileSourceClass:= AFileSourceClass;
  gVfsModuleList.AddObject(AName, VfsModule);
end;

{ TVfsModuleList }

function TVfsModuleList.GetVfsModule(const S: String): TVfsModule;
var
  I: Integer;
begin
  I:= IndexOf(S);
  if I < 0 then Exit(nil);
  Result:= TVfsModule(Objects[I]);
end;

destructor TVfsModuleList.Destroy;
var
  I: Integer;
begin
  for I:= 0 to Count - 1 do
    TVfsModule(Objects[I]).Free;
  inherited Destroy;
end;

initialization
  gVfsModuleList := TVfsModuleList.Create;

finalization
  FreeAndNil(gVfsModuleList);

end.
