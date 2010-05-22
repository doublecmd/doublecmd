unit fConnectionManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls, Buttons, ComCtrls, uFileView;

type

  { TfrmConnectionManager }

  TfrmConnectionManager = class(TForm)
    btnCancel: TBitBtn;
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    btnAdd: TBitBtn;
    btnConnect: TBitBtn;
    gbConnectTo: TGroupBox;
    ImageList: TImageList;
    tvConnections: TTreeView;
    procedure btnAddClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvConnectionsSelectionChanged(Sender: TObject);
  private
    FFileView: TFileView;
  public
    constructor Create(TheOwner: TComponent; FileView: TFileView); reintroduce;
  end; 

function ShowConnectionManager(FileView: TFileView): Boolean;

implementation

{$R *.lfm}

uses
  uGlobs, uDCUtils, uShowMsg, uWfxModule, WfxPlugin, uWfxPluginFileSource, uLng;

function ShowConnectionManager(FileView: TFileView): Boolean;
begin
  with TfrmConnectionManager.Create(Application, FileView) do
  begin
    try
      Result:= (ShowModal = mrOK);
    finally
      Free;
    end;
  end;
end;

{ TfrmConnectionManager }

procedure TfrmConnectionManager.tvConnectionsSelectionChanged(Sender: TObject);
var
  bEnabled: Boolean;
begin
  if not Assigned(tvConnections.Selected) then
    begin
      btnConnect.Enabled:= False;
      btnAdd.Enabled:= False;
      btnEdit.Enabled:= False;
      btnDelete.Enabled:= False;
    end
  else
    begin
      bEnabled:= Assigned(tvConnections.Selected.Data);
      btnConnect.Enabled:= not bEnabled;
      btnAdd.Enabled:= bEnabled;
      btnEdit.Enabled:= not bEnabled;
      btnDelete.Enabled:= not bEnabled;
    end;
end;

procedure TfrmConnectionManager.btnAddClick(Sender: TObject);
var
  WfxPluginFileSource: TWfxPluginFileSource;
  Connection: UTF8String;
begin
  WfxPluginFileSource:= TWfxPluginFileSource(tvConnections.Selected.Data);
  if Assigned(WfxPluginFileSource) then
  begin
    if WfxPluginFileSource.WfxModule.WfxNetworkManageConnection(Connection, FS_NM_ACTION_ADD) then
    begin
      with tvConnections.Items.AddChild(tvConnections.Selected, Connection) do
      StateIndex:= 1;
    end;
  end;
end;

procedure TfrmConnectionManager.btnConnectClick(Sender: TObject);
var
  WfxPluginFileSource: TWfxPluginFileSource;
  Connection,
  RemotePath,
  RootPath: UTF8String;
begin
  WfxPluginFileSource:= TWfxPluginFileSource(tvConnections.Selected.Parent.Data);
  if Assigned(WfxPluginFileSource) then
  begin
    Connection:= tvConnections.Selected.Text;
    if WfxPluginFileSource.WfxModule.WfxNetworkOpenConnection(Connection, RemotePath) then
      begin
        RootPath:= PathDelim + tvConnections.Selected.Text;
        WfxPluginFileSource.SetCurrentAddress(Connection);
        WfxPluginFileSource.SetRootDir(RootPath + PathDelim);
        DoDirSeparators(RemotePath);
        FFileView.AddFileSource(WfxPluginFileSource, RootPath + RemotePath);
        tvConnections.Selected.Parent.Data:= nil;
        Close;
      end
    else
      begin
        msgError(Format(rsMsgErrCanNotConnect, [Connection]));
      end;
  end;
end;

procedure TfrmConnectionManager.btnDeleteClick(Sender: TObject);
var
  WfxPluginFileSource: TWfxPluginFileSource;
  Connection: UTF8String;
begin
  WfxPluginFileSource:= TWfxPluginFileSource(tvConnections.Selected.Parent.Data);
  if Assigned(WfxPluginFileSource) then
  begin
    Connection:= tvConnections.Selected.Text;
    if WfxPluginFileSource.WfxModule.WfxNetworkManageConnection(Connection, FS_NM_ACTION_DELETE) then
    begin
      tvConnections.Items.BeginUpdate;
      tvConnections.Items.Delete(tvConnections.Selected);
      tvConnections.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmConnectionManager.btnEditClick(Sender: TObject);
var
  WfxPluginFileSource: TWfxPluginFileSource;
  Connection: UTF8String;
begin
  WfxPluginFileSource:= TWfxPluginFileSource(tvConnections.Selected.Parent.Data);
  if Assigned(WfxPluginFileSource) then
  begin
    Connection:= tvConnections.Selected.Text;
    if WfxPluginFileSource.WfxModule.WfxNetworkManageConnection(Connection, FS_NM_ACTION_EDIT) then
      tvConnections.Selected.Text:= Connection;
  end;
end;

procedure TfrmConnectionManager.FormDestroy(Sender: TObject);
var
  I: Integer;
  WfxPluginFileSource: TWfxPluginFileSource;
begin
  {
  for I:= 1 to tvConnections.Items.Count - 1 do
  begin
    WfxPluginFileSource:= TWfxPluginFileSource(tvConnections.Items.Item[I].Data);
    if Assigned(WfxPluginFileSource) then
    begin
      WfxPluginFileSource.Free;
    end;
  end;
  }
end;

constructor TfrmConnectionManager.Create(TheOwner: TComponent; FileView: TFileView);
var
  I, J: Integer;
  WfxPluginFileSource: TWfxPluginFileSource = nil;
  sModuleFileName,
  Connection: UTF8String;
  Node, SubNode: TTreeNode;
begin
  FFileView:= FileView;
  inherited Create(TheOwner);
  for I:= 0 to gWfxPlugins.Count - 1 do
  begin
    if gWfxPlugins.Enabled[I] then
    begin
      sModuleFileName:= GetCmdDirFromEnvVar(gWfxPlugins.FileName[I]);
      WfxPluginFileSource:= TWfxPluginFileSource.Create(sModuleFileName, gWfxPlugins.Name[I]);
      try
        if Assigned(WfxPluginFileSource) then
        with WfxPluginFileSource do
        begin
           if WFXmodule.VFSNetworkSupport then
            begin
              Node:= tvConnections.Items.Add(nil, gWfxPlugins.Name[I]);
              Node.Data:= WfxPluginFileSource;
              Node.StateIndex:= 0;
              J:= 0;
              while WfxModule.WfxNetworkGetConnection(J, Connection) do
              begin
                SubNode:= tvConnections.Items.AddChild(Node, Connection);
                SubNode.StateIndex:= 1;
                Inc(J);
              end;
            end
           else
             begin
               WfxPluginFileSource.Free;
             end;
         end;
      except
        WfxPluginFileSource.Free;
      end;
  end;
end;

end;

end.

