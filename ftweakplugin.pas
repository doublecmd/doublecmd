unit fTweakPlugin; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, uTypes;

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
    edtDetectStr: TEdit;
    edtName: TEdit;
    edtPlugin: TEdit;
    GroupBox1: TGroupBox;
    lblDetectStr: TLabel;
    lblName: TLabel;
    ledPlugin: TLabeledEdit;
    lblExtension: TLabel;
    lblFlags: TLabel;
    lblFlagsValue: TLabel;
    lblPackerPlugin: TLabel;
    lblPlugin: TLabel;
    nbTweakAll: TNotebook;
    pgTweakPacker: TPage;
    pgTweakOther: TPage;
    pnlTweak: TGroupBox;
    procedure cbExtChange(Sender: TObject);
  private
    iPrevIndex: Integer;
  public
    { public declarations }
  end; 

function ShowTweakPluginDlg(PluginType: TPluginType; PluginIndex: Integer): Boolean;

implementation
uses
  uGlobs, uWCXHead;

function ShowTweakPluginDlg(PluginType: TPluginType; PluginIndex: Integer): Boolean;
var
  I, iIndex: Integer;
begin
  with TfrmTweakPlugin.Create(Application) do
  try
    case PluginType of
    ptWCX:
      begin
        nbTweakAll.PageIndex:= 0;
        edtPlugin.Text:= gWCXPlugins.FileName[PluginIndex];
        for I:= 0 to gWCXPlugins.Count - 1 do
          if gWCXPlugins.FileName[I] = edtPlugin.Text then
            cbExt.Items.AddObject(gWCXPlugins.Ext[I], TObject(gWCXPlugins.Flags[I]));
        iPrevIndex:= -1;
        cbExt.ItemIndex:= 0;
        cbExtChange(cbExt);
      end;
    ptWDX:
      begin
        nbTweakAll.PageIndex:= 1;
        ledPlugin.Text:= WDXPlugins.GetWdxModule(PluginIndex).FileName;
        edtDetectStr.Text:= WDXPlugins.GetWdxModule(PluginIndex).DetectStr;
        edtName.Text:= WDXPlugins.GetWdxModule(PluginIndex).Name;
      end;
    ptWFX:
      begin
        nbTweakAll.PageIndex:= 1;
        ledPlugin.Text:= gWFXPlugins.FileName[PluginIndex];
        lblDetectStr.Visible:= False;
        edtDetectStr.Visible:= False;
        edtName.Text:= gWFXPlugins.Name[PluginIndex];
      end;
    end;
    Result:= (ShowModal = mrOK);
    if Result then
      case PluginType of
      ptWCX:
        begin
          for I:= 0 to cbExt.Items.Count - 1 do
            begin
              iIndex:= gWCXPlugins.IndexOfName(cbExt.Items[I]);
              if iIndex >= 0 then
                begin
                  gWCXPlugins.FileName[iIndex]:= edtPlugin.Text;
                  gWCXPlugins.Flags[iIndex]:= PtrInt(cbExt.Items.Objects[I]);
                end;
            end;
        end;
      ptWDX:
        begin
          WDXPlugins.GetWdxModule(PluginIndex).FileName:= ledPlugin.Text;
          WDXPlugins.GetWdxModule(PluginIndex).DetectStr:= edtDetectStr.Text;
          WDXPlugins.GetWdxModule(PluginIndex).Name:= edtName.Text;
        end;
      ptWFX:
        begin
          gWFXPlugins.FileName[PluginIndex]:= ledPlugin.Text;
          gWFXPlugins.Name[PluginIndex]:= edtName.Text;
        end;
      end;
  finally
    Free;
  end;
end;

{ TfrmTweakPlugin }

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

initialization
  {$I ftweakplugin.lrs}

end.

