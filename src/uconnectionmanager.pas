unit uConnectionManager;

{$mode delphi}

interface

uses
  Classes, SysUtils, uFileSource;

type
  TFileSourceRecord = record
    FileSource: IFileSource;
  end;
  PFileSourceRecord = ^TFileSourceRecord;

function NewFileSourceRecord(FileSource: IFileSource): PFileSourceRecord;
procedure DisposeFileSourceRecord(FileSourceRecord: PFileSourceRecord);

procedure AddNetworkConnection(const ConnectionName: UTF8String; FileSource: IFileSource);
procedure RemoveNetworkConnection(const ConnectionName: UTF8String);

var
  WfxConnectionList: TStringList = nil;

implementation

uses
  Menus, fMain, uWfxPluginFileSource;

function NewFileSourceRecord(FileSource: IFileSource): PFileSourceRecord;
begin
  New(Result);
  Result^.FileSource:= FileSource;
end;

procedure DisposeFileSourceRecord(FileSourceRecord: PFileSourceRecord);
begin
  FileSourceRecord^.FileSource:= nil;
  Dispose(FileSourceRecord);
end;

procedure AddNetworkConnection(const ConnectionName: UTF8String; FileSource: IFileSource);
var
  I: Integer;
  miTemp: TMenuItem;
begin
  WfxConnectionList.AddObject(ConnectionName, TObject(NewFileSourceRecord(FileSource)));
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

