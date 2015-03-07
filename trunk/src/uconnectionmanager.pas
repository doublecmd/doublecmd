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

procedure AddNetworkConnection(const ConnectionName, Path: String; FileSource: IFileSource);
procedure RemoveNetworkConnection(const ConnectionName: String);

var
  WfxConnectionList: TStringList = nil;

implementation

uses
  Menus, DCStrUtils, fMain, uWfxPluginFileSource, uLog, uGlobs;

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

procedure OnNetworkDisconnect(Self, Sender: TObject);
var
  Index: Integer;
  Connection: PFileSourceRecord;
  FileSource: IWfxPluginFileSource;
  MenuItem: TMenuItem absolute Sender;
begin
  if WfxConnectionList.Find(MenuItem.Caption, Index) then
  begin
    Connection:= PFileSourceRecord(WfxConnectionList.Objects[Index]);
    FileSource:= Connection.FileSource as IWfxPluginFileSource;
    FileSource.WfxModule.WfxDisconnect(MenuItem.Hint);
    with frmMain do
    begin
      if ActiveFrame.FileSource.Equals(FileSource) and
        IsInPath(MenuItem.Hint, ActiveFrame.CurrentPath, True, True) then
      begin
        ActiveFrame.RemoveCurrentFileSource;
      end
      else if NotActiveFrame.FileSource.Equals(FileSource) and
        IsInPath(MenuItem.Hint, NotActiveFrame.CurrentPath, True, True) then
      begin
        NotActiveFrame.RemoveCurrentFileSource
      end;
    end;
  end;
end;

procedure AddNetworkConnection(const ConnectionName, Path: String; FileSource: IFileSource);
var
  Handler: TMethod;
  MenuItem: TMenuItem;
begin
  WfxConnectionList.AddObject(ConnectionName, TObject(NewFileSourceRecord(FileSource)));
  with frmMain do
  begin
    MenuItem:= TMenuItem.Create(miNetworkDisconnect);
    MenuItem.Hint:= Path;
    MenuItem.Caption:= ConnectionName;
    Handler.Data:= MenuItem;
    Handler.Code:= @OnNetworkDisconnect;
    MenuItem.OnClick:= TNotifyEvent(Handler);
    miNetworkDisconnect.Add(MenuItem);
    miNetworkDisconnect.Enabled:= miNetworkDisconnect.Count > 0;
  end;
end;

procedure RemoveNetworkConnection(const ConnectionName: String);
var
  Index: Integer;
begin
  if WfxConnectionList.Find(ConnectionName, Index) then
  with frmMain do
  begin
    DisposeFileSourceRecord(PFileSourceRecord(WfxConnectionList.Objects[Index]));
    WfxConnectionList.Delete(Index);
    miNetworkDisconnect.Remove(miNetworkDisconnect.Find(ConnectionName));
    miNetworkDisconnect.Enabled:= miNetworkDisconnect.Count > 0;
    if WfxConnectionList.Count = 0 then
    begin
      if gLogWindow = False then ShowLogWindow(False);
    end;
  end;
end;

initialization
  WfxConnectionList:= TStringList.Create;

finalization
  FreeAndNil(WfxConnectionList);

end.
