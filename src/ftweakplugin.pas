{
   Double commander
   -------------------------------------------------------------------------
   Plugin tweak window

   Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit fTweakPlugin; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, uTypes, uWCXModule;

type

  { TfrmTweakPlugin }

  TfrmTweakPlugin = class(TForm)
    btnAdd: TButton;
    btnCancel: TButton;
    btnChange: TButton;
    btnDefault: TButton;
    btnOK: TButton;
    btnRemove: TButton;
    cbExt: TComboBox;
    cbPK_CAPS_BY_CONTENT: TCheckBox;
    cbPK_CAPS_DELETE: TCheckBox;
    cbPK_CAPS_ENCRYPT: TCheckBox;
    cbPK_CAPS_HIDE: TCheckBox;
    cbPK_CAPS_MEMPACK: TCheckBox;
    cbPK_CAPS_MODIFY: TCheckBox;
    cbPK_CAPS_MULTIPLE: TCheckBox;
    cbPK_CAPS_NEW: TCheckBox;
    cbPK_CAPS_OPTIONS: TCheckBox;
    cbPK_CAPS_SEARCHTEXT: TCheckBox;
    edtDescription: TEdit;
    edtDetectStr: TEdit;
    edtName: TEdit;
    edtPlugin: TEdit;
    grpTweakOther: TGroupBox;
    lblDescription: TLabel;
    lblDetectStr: TLabel;
    lblName: TLabel;
    lblExtension: TLabel;
    lblFlags: TLabel;
    lblFlagsValue: TLabel;
    lblPlugin1: TLabel;
    lblPackerPlugin: TLabel;
    lblPlugin: TLabel;
    edtPlugin1: TEdit;
    nbTweakAll: TNotebook;
    pgTweakPacker: TPage;
    pgTweakOther: TPage;
    grpTweak: TGroupBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnChangeClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure cbExtChange(Sender: TObject);
  private
    FWCXPlugins: TWCXModuleList;
    FPluginFileName: String;
    iPrevIndex: Integer;
    function GetDefaultFlags(PluginFileName: String): PtrInt;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end; 

function ShowTweakPluginDlg(PluginType: TPluginType; PluginIndex: Integer): Boolean;

implementation
uses
  fOptions, WcxPlugin, uDCUtils, uLng;

function ShowTweakPluginDlg(PluginType: TPluginType; PluginIndex: Integer): Boolean;
var
  I, iIndex: Integer;
begin
  with TfrmTweakPlugin.Create(Application) do
  try
    case PluginType of
    ptDSX:
      begin
        nbTweakAll.PageIndex:= 1;
        edtPlugin1.Text:= tmpDSXPlugins.GetDsxModule(PluginIndex).FileName;
        edtDescription.Text:= tmpDSXPlugins.GetDsxModule(PluginIndex).Descr;
        edtName.Text:= tmpDSXPlugins.GetDsxModule(PluginIndex).Name;
        lblDetectStr.Visible:= False;
        edtDetectStr.Visible:= False;
      end;
    ptWCX:
      begin
        nbTweakAll.PageIndex:= 0;
        FWCXPlugins:= TWCXModuleList.Create;
        FWCXPlugins.Assign(tmpWCXPlugins);
        FPluginFileName := FWCXPlugins.FileName[PluginIndex];
        edtPlugin.Text:= FPluginFileName;
        for I:= 0 to FWCXPlugins.Count - 1 do
          if FWCXPlugins.FileName[I] = edtPlugin.Text then
            cbExt.Items.AddObject(FWCXPlugins.Ext[I], TObject(FWCXPlugins.Flags[I]));
        iPrevIndex:= -1;
        cbExt.ItemIndex:= 0;
        cbExtChange(cbExt);
        btnRemove.Enabled:= (cbExt.Items.Count > 1);
      end;
    ptWDX:
      begin
        nbTweakAll.PageIndex:= 1;
        edtPlugin1.Text:= tmpWDXPlugins.GetWdxModule(PluginIndex).FileName;
        edtDetectStr.Text:= tmpWDXPlugins.GetWdxModule(PluginIndex).DetectStr;
        edtName.Text:= tmpWDXPlugins.GetWdxModule(PluginIndex).Name;
        lblDescription.Visible:= False;
        edtDescription.Visible:= False;
      end;
    ptWFX:
      begin
        nbTweakAll.PageIndex:= 1;
        edtPlugin1.Text:= tmpWFXPlugins.FileName[PluginIndex];
        edtName.Text:= tmpWFXPlugins.Name[PluginIndex];
        lblDetectStr.Visible:= False;
        edtDetectStr.Visible:= False;
        lblDescription.Visible:= False;
        edtDescription.Visible:= False;
      end;
    ptWLX:
      begin
        nbTweakAll.PageIndex:= 1;
        edtPlugin1.Text:= tmpWLXPlugins.GetWlxModule(PluginIndex).FileName;
        edtDetectStr.Text:= tmpWLXPlugins.GetWlxModule(PluginIndex).DetectStr;
        edtName.Text:= tmpWLXPlugins.GetWlxModule(PluginIndex).Name;
        lblDescription.Visible:= False;
        edtDescription.Visible:= False;
      end;
    end;
    Result:= (ShowModal = mrOK);
    if Result then
      case PluginType of
      ptDSX:
        begin
          tmpDSXPlugins.GetDsxModule(PluginIndex).FileName:= edtPlugin1.Text;
          tmpDSXPlugins.GetDsxModule(PluginIndex).Descr := edtDescription.Text;
          tmpDSXPlugins.GetDsxModule(PluginIndex).Name:= edtName.Text;
        end;
      ptWCX:
        begin
          for I:= 0 to cbExt.Items.Count - 1 do
            begin
              iIndex:= FWCXPlugins.Find(FPluginFileName, cbExt.Items[I]);
              if iIndex >= 0 then
                begin
                  FWCXPlugins.FileName[iIndex]:= edtPlugin.Text;
                  FWCXPlugins.Flags[iIndex]:= PtrInt(cbExt.Items.Objects[I]);
                end;
            end;
          tmpWCXPlugins.Assign(FWCXPlugins);
        end;
      ptWDX:
        begin
          tmpWDXPlugins.GetWdxModule(PluginIndex).FileName:= edtPlugin1.Text;
          tmpWDXPlugins.GetWdxModule(PluginIndex).DetectStr:= edtDetectStr.Text;
          tmpWDXPlugins.GetWdxModule(PluginIndex).Name:= edtName.Text;
        end;
      ptWFX:
        begin
          tmpWFXPlugins.FileName[PluginIndex]:= edtPlugin1.Text;
          tmpWFXPlugins.Name[PluginIndex]:= edtName.Text;
        end;
      ptWLX:
        begin
          tmpWLXPlugins.GetWlxModule(PluginIndex).FileName:= edtPlugin1.Text;
          tmpWLXPlugins.GetWlxModule(PluginIndex).DetectStr:= edtDetectStr.Text;
          tmpWLXPlugins.GetWlxModule(PluginIndex).Name:= edtName.Text;
        end;
      end;
  finally
    Free;
  end;
end;

{ TfrmTweakPlugin }

constructor TfrmTweakPlugin.Create(TheOwner: TComponent);
begin
  FWCXPlugins := nil;
  iPrevIndex := -1;
  inherited;
end;

destructor TfrmTweakPlugin.Destroy;
begin
  inherited;
  if Assigned(FWCXPlugins) then
    FreeAndNil(FWCXPlugins);
end;

procedure TfrmTweakPlugin.cbExtChange(Sender: TObject);
var
  iFlags: PtrInt;
begin
  if iPrevIndex >= 0 then // save new flags
    begin
      iFlags:= 0;
      if cbPK_CAPS_NEW.Checked then
        iFlags:= iFlags or PK_CAPS_NEW;
      if cbPK_CAPS_MODIFY.Checked then
        iFlags:= iFlags or PK_CAPS_MODIFY;
      if cbPK_CAPS_MULTIPLE.Checked then
        iFlags:= iFlags or PK_CAPS_MULTIPLE;
      if cbPK_CAPS_DELETE.Checked then
        iFlags:= iFlags or PK_CAPS_DELETE;
      if cbPK_CAPS_OPTIONS.Checked then
        iFlags:= iFlags or PK_CAPS_OPTIONS;
      if cbPK_CAPS_MEMPACK.Checked then
        iFlags:= iFlags or PK_CAPS_MEMPACK;
      if cbPK_CAPS_BY_CONTENT.Checked then
        iFlags:= iFlags or PK_CAPS_BY_CONTENT;
      if cbPK_CAPS_SEARCHTEXT.Checked then
        iFlags:= iFlags or PK_CAPS_SEARCHTEXT;
      if cbPK_CAPS_HIDE.Checked then
        iFlags:= iFlags or PK_CAPS_HIDE;
      if cbPK_CAPS_ENCRYPT.Checked then
        iFlags:= iFlags or PK_CAPS_ENCRYPT;
      cbExt.Items.Objects[iPrevIndex]:= TObject(iFlags);
    end;

  iPrevIndex:= cbExt.ItemIndex;
  iFlags:= PtrInt(cbExt.Items.Objects[cbExt.ItemIndex]);
  lblFlagsValue.Caption:= '('+IntToStr(iFlags)+')';

  cbPK_CAPS_NEW.Checked:= Boolean(iFlags and PK_CAPS_NEW);
  cbPK_CAPS_MODIFY.Checked:= Boolean(iFlags and PK_CAPS_MODIFY);
  cbPK_CAPS_MULTIPLE.Checked:= Boolean(iFlags and PK_CAPS_MULTIPLE);
  cbPK_CAPS_DELETE.Checked:= Boolean(iFlags and PK_CAPS_DELETE);
  cbPK_CAPS_OPTIONS.Checked:= Boolean(iFlags and PK_CAPS_OPTIONS);
  cbPK_CAPS_MEMPACK.Checked:= Boolean(iFlags and PK_CAPS_MEMPACK);
  cbPK_CAPS_BY_CONTENT.Checked:= Boolean(iFlags and PK_CAPS_BY_CONTENT);
  cbPK_CAPS_SEARCHTEXT.Checked:= Boolean(iFlags and PK_CAPS_SEARCHTEXT);
  cbPK_CAPS_HIDE.Checked:= Boolean(iFlags and PK_CAPS_HIDE);
  cbPK_CAPS_ENCRYPT.Checked:= Boolean(iFlags and PK_CAPS_ENCRYPT);
end;

procedure TfrmTweakPlugin.btnDefaultClick(Sender: TObject);
begin
  cbExt.Items.Objects[cbExt.ItemIndex]:= TObject(GetDefaultFlags(edtPlugin.Text));
  iPrevIndex:= -1;
  cbExtChange(cbExt);
end;

procedure TfrmTweakPlugin.btnRemoveClick(Sender: TObject);
var
  I, OldIndex: Integer;
begin
  iPrevIndex:= -1;  // Must be before cbExt.Items.Delete, because it may trigger cbExtChange.
  OldIndex := cbExt.ItemIndex;
  I:= FWCXPlugins.Find(FPluginFileName, cbExt.Text);
  if I >= 0 then
    FWCXPlugins.Delete(I);
  cbExt.Items.Delete(cbExt.ItemIndex);
  if OldIndex >= cbExt.Items.Count then
    OldIndex := OldIndex - 1;
  cbExt.ItemIndex := OldIndex;
  if iPrevIndex = -1 then // Call only if not already triggerred.
    cbExtChange(cbExt);
  btnRemove.Enabled:= (cbExt.Items.Count > 1);
end;

procedure TfrmTweakPlugin.btnAddClick(Sender: TObject);
var
  sExt: String;
  iFlags: PtrInt;
begin
  if InputQuery(rsOptEnterExt,Format(rsOptAssocPluginWith, [GetCmdDirFromEnvVar(edtPlugin.Text)]), sExt) then
    begin
      iFlags:= GetDefaultFlags(edtPlugin.Text);
      cbExt.ItemIndex:= cbExt.Items.AddObject(sExt, TObject(iFlags));
      FWCXPlugins.Add(cbExt.Items[cbExt.ItemIndex], iFlags, FPluginFileName);
      iPrevIndex:= -1;
      cbExtChange(cbExt);
    end;
end;

procedure TfrmTweakPlugin.btnChangeClick(Sender: TObject);
var
  I: Integer;
  sExt: String;
begin
  sExt:= cbExt.Items[cbExt.ItemIndex];
  I:= FWCXPlugins.Find(FPluginFileName, sExt);
  if (I >= 0) and
     InputQuery(rsOptEnterExt,Format(rsOptAssocPluginWith, [GetCmdDirFromEnvVar(edtPlugin.Text)]), sExt) then
    begin
      FWCXPlugins.Ext[I]:= sExt;
      cbExt.Items[cbExt.ItemIndex]:= sExt;
    end;
end;

function TfrmTweakPlugin.GetDefaultFlags(PluginFileName: String): PtrInt;
var
  WCXmodule: TWCXmodule;
begin
  Result:= 0;
  WCXmodule := TWCXmodule.Create;
  if WCXmodule.LoadModule(GetCmdDirFromEnvVar(PluginFileName)) then
    begin
      Result:= WCXmodule.GetPluginCapabilities;
      WCXModule.UnloadModule;
    end;
  WCXmodule.Free;
end;

initialization
  {$I ftweakplugin.lrs}

end.
