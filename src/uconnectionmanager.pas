unit uConnectionManager;

{$mode delphi}

interface

uses
  Classes, SysUtils; 

procedure AddNetworkConnection(const ConnectionName: UTF8String; Connection: TObject);
procedure RemoveNetworkConnection(const ConnectionName: UTF8String);

var
  WfxConnectionList: TStringList = nil;

implementation

uses
  Menus, fMain, uWfxPluginFileSource;

procedure AddNetworkConnection(const ConnectionName: UTF8String; Connection: TObject);
var
  I: Integer;
  miTemp: TMenuItem;
  CallbackDataClass: TCallbackDataClass absolute Connection;
begin
  WfxConnectionList.AddObject(ConnectionName, Connection);
  with frmMain do
  begin
    miTemp:= TMenuItem.Create(miNetworkDisconnect);
    miTemp.Caption:= ConnectionName;
    miNetworkDisconnect.Add(miTemp);
  end;
end;

procedure RemoveNetworkConnection(const ConnectionName: UTF8String);
var
  I: Integer;
begin
  I:= WfxConnectionList.IndexOf(ConnectionName);
  if I >= 0 then
    with frmMain do
    begin
      WfxConnectionList.Delete(I);
      miNetworkDisconnect.Remove(miNetworkDisconnect.Find(ConnectionName));
    end;
end;

initialization
  WfxConnectionList:= TStringList.Create;
finalization
  if Assigned(WfxConnectionList) then
    FreeAndNil(WfxConnectionList);

end.

