unit uConnectionManager;

{$mode delphi}

interface

uses
  Classes, SysUtils, uFileSource, uDrive, uDrivesList;

type
  TFileSourceRecord = record
    Count: Integer;
    Name, Path: String;
    FileSource: IFileSource;
  end;
  PFileSourceRecord = ^TFileSourceRecord;

function GetNetworkPath(ADrive: PDrive): String;
procedure UpdateDriveList(ADriveList: TDrivesList);
procedure ShowVirtualDriveMenu(ADrive: PDrive; X, Y : Integer; CloseEvent: TNotifyEvent);

procedure AddNetworkConnection(const Name, Path: String; FileSource: IFileSource);
procedure RemoveNetworkConnection(const Name, Path: String);

procedure CloseNetworkConnection();

var
  WfxConnectionList: TStringList = nil;

implementation

uses
  Forms, Menus, StrUtils, DCStrUtils, fMain, uWfxPluginFileSource, uLog,
  uGlobs, uFileSourceUtil, uFileView;

var
  ContextMenu: TPopupMenu = nil;

function GetConnectionName(const Name, Path: String): String; inline;
begin
  Result:= Name + ': ' + Path;
end;

function NewFileSourceRecord(FileSource: IFileSource; const Name, Path: String): PFileSourceRecord;
begin
  New(Result);
  Result^.Count:= 1;
  Result^.Name:= Name;
  Result^.Path:= Path;
  Result^.FileSource:= FileSource;
end;

procedure DisposeFileSourceRecord(FileSourceRecord: PFileSourceRecord);
begin
  FileSourceRecord^.FileSource:= nil;
  Dispose(FileSourceRecord);
end;

procedure CloseConnection(Index: Integer);
var
  Connection: TFileSourceRecord;
  FileSource: IWfxPluginFileSource;
begin
  PFileSourceRecord(WfxConnectionList.Objects[Index]).Count:= 1;
  Connection:= PFileSourceRecord(WfxConnectionList.Objects[Index])^;
  FileSource:= Connection.FileSource as IWfxPluginFileSource;
  if FileSource.WfxModule.WfxDisconnect(Connection.Path) then
  begin
    RemoveNetworkConnection(Connection.Name, Connection.Path);
  end;
  with frmMain do
  begin
    if ActiveFrame.FileSource.Equals(FileSource) and
      IsInPath(Connection.Path, ActiveFrame.CurrentPath, True, True) then
    begin
      ActiveFrame.RemoveCurrentFileSource;
    end
    else if NotActiveFrame.FileSource.Equals(FileSource) and
      IsInPath(Connection.Path, NotActiveFrame.CurrentPath, True, True) then
    begin
      NotActiveFrame.RemoveCurrentFileSource
    end;
  end;
end;

procedure OnNetworkDisconnect(Self, Sender: TObject);
var
  Index: Integer;
  MenuItem: TMenuItem absolute Sender;
begin
  Index:= WfxConnectionList.IndexOf(MenuItem.Hint);
  if Index >= 0 then CloseConnection(Index);
end;

function GetNetworkPath(ADrive: PDrive): String;
begin
  Result:= ADrive^.DeviceId + StringReplace(ADrive^.Path, '\', '/', [rfReplaceAll]);
end;

procedure UpdateDriveList(ADriveList: TDrivesList);
var
  Drive: PDrive;
  Index: Integer;
  FileSourceRecord: PFileSourceRecord;
begin
  for Index:= 0 to WfxConnectionList.Count - 1 do
  begin
    FileSourceRecord:= PFileSourceRecord(WfxConnectionList.Objects[Index]);
    New(Drive);
    Drive^.IsMounted:= True;
    Drive^.DriveType:= dtVirtual;
    Drive^.Path:= FileSourceRecord.Path;
    Drive^.DisplayName:= IntToStr(Index);
    Drive^.DeviceId:= 'wfx://' + FileSourceRecord.Name;
    Drive^.DriveLabel:= GetConnectionName(FileSourceRecord.Name, FileSourceRecord.Path);
    ADriveList.Add(Drive);
  end;
end;

procedure ShowVirtualDriveMenu(ADrive: PDrive; X, Y: Integer; CloseEvent: TNotifyEvent);
var
  Handler: TMethod;
  MenuItem: TMenuItem;
begin
  if not StrBegins(ADrive^.DeviceId, 'wfx://') then
    Exit;

  // Free previous created menu
  FreeAndNil(ContextMenu);

  // Create new context menu
  ContextMenu:= TPopupMenu.Create(nil);
  ContextMenu.OnClose := CloseEvent;

  MenuItem:= TMenuItem.Create(ContextMenu);
  MenuItem.Caption:= ADrive.DriveLabel;
  MenuItem.Enabled:= False;
  ContextMenu.Items.Add(MenuItem);

  MenuItem:= TMenuItem.Create(ContextMenu);
  MenuItem.Caption:= '-';
  ContextMenu.Items.Add(MenuItem);

  MenuItem:= TMenuItem.Create(ContextMenu);
  MenuItem.Caption:= frmMain.actNetworkDisconnect.Caption;
  MenuItem.Hint:= ADrive.DriveLabel;
  Handler.Data:= MenuItem;
  Handler.Code:= @OnNetworkDisconnect;
  MenuItem.OnClick:= TNotifyEvent(Handler);
  ContextMenu.Items.Add(MenuItem);

  // Show context menu
  ContextMenu.PopUp(X, Y);
end;

procedure AddNetworkConnection(const Name, Path: String; FileSource: IFileSource);
var
  Index: Integer;
  ConnectionName: String;
  FileSourceRecord: PFileSourceRecord;
begin
  ConnectionName:= GetConnectionName(Name, Path);
  Index:= WfxConnectionList.IndexOf(ConnectionName);
  if Index >= 0 then begin
    FileSourceRecord:= PFileSourceRecord(WfxConnectionList.Objects[Index]);
    FileSourceRecord.Count:= FileSourceRecord.Count + 1;
  end
  else begin
    FileSourceRecord:= NewFileSourceRecord(FileSource, Name, Path);
    WfxConnectionList.AddObject(ConnectionName, TObject(FileSourceRecord));
    with frmMain do
    begin
      miNetworkDisconnect.Enabled:= WfxConnectionList.Count > 0;
      UpdateDiskCount;
    end;
  end;
end;

procedure RemoveNetworkConnection(const Name, Path: String);
var
  Index: Integer;
  ConnectionName: String;
  FileSourceRecord: PFileSourceRecord;
begin
  ConnectionName:= GetConnectionName(Name, Path);
  Index:= WfxConnectionList.IndexOf(ConnectionName);
  if Index >= 0 then
  with frmMain do
  begin
    FileSourceRecord:= PFileSourceRecord(WfxConnectionList.Objects[Index]);
    FileSourceRecord^.Count:= FileSourceRecord^.Count - 1;
    if FileSourceRecord^.Count > 0 then Exit;
    DisposeFileSourceRecord(PFileSourceRecord(WfxConnectionList.Objects[Index]));
    WfxConnectionList.Delete(Index);
    miNetworkDisconnect.Enabled:= WfxConnectionList.Count > 0;
    if WfxConnectionList.Count = 0 then
    begin
      if gLogWindow = False then
      begin
        ShowLogWindow(PtrInt(False));
      end;
    end;
    UpdateDiskCount;
  end;
end;

procedure CloseNetworkConnection;
var
  Index: Integer;
  ConnectionName: String;
  FileView: TFileView = nil;
begin
  if WfxConnectionList.Count > 0 then
  with frmMain do
  begin
    if ActiveFrame.FileSource.IsInterface(IWfxPluginFileSource) and (ActiveFrame.CurrentPath <> PathDelim) then
      FileView:= ActiveFrame
    else if NotActiveFrame.FileSource.IsInterface(IWfxPluginFileSource) and (NotActiveFrame.CurrentPath <> PathDelim) then begin
      FileView:= NotActiveFrame
    end;
    if Assigned(FileView) then
    begin
      ConnectionName:= ExtractWord(2, FileView.CurrentAddress, ['/']);
      ConnectionName:= GetConnectionName(ConnectionName, PathDelim + ExtractWord(1, FileView.CurrentPath, [PathDelim]));
      Index:= WfxConnectionList.IndexOf(ConnectionName);
      if Index >= 0 then CloseConnection(Index);
    end
    // Close last connection
    else begin
      CloseConnection(WfxConnectionList.Count - 1);
    end;
  end;
end;

initialization
  WfxConnectionList:= TStringList.Create;

finalization
  FreeAndNil(WfxConnectionList);

end.

